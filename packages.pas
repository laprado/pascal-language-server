unit packages;

{$mode objfpc}{$H+}

interface

type
  TPaths = record
    // Search path for units (OtherUnitFiles)
    UnitPath:    String;
    // Search path for includes (IncludeFiles)
    IncludePath: String;
    // Additional sources, not passed to compiler (SrcFiles)
    SrcPath:     String;
  end;

  TDependency = record
    // Name of the package, e.g. 'LCLBase'
    Name: String;
    // Projects may hardcode a path to a package. If a path was hardcoded, Path
    // will contain the expanded path, otherwise will be empty string.
    Path: String;
    // Whether the hardcoded path should take precedence over a global package
    // of the same name. Currently we only use the hardcoded Path if Prefer is
    // true.
    Prefer: Boolean;
  end;

  { TPackage }

  TPackage = class
    // Valid: True if the package was found, False otherwise. If False, this
    // is a dummy object whose only purpose is to prevent us from trying to load
    // a non-existing package multiple times.
    Valid: Boolean;
    // Configured: True if the defines (IncludePath, UnitPath, SrcPath) have
    // been set for this package. This is so we don't initialize multiple times.
    Configured: Boolean;
    // Expanded search paths for this package
    Paths: TPaths;
    // List of dependencies of this package
    Dependencies: array of TDependency;
    constructor Create;
  end;

  // Get package or project information from a file. The file must end in .lpk
  // if it is a package, or .lpi if it is a project.
  // Results are cached. If the file could not be loaded, the Valid member of
  // the result will be set to False.
  function GetPackageOrProject(const FileName: String): TPackage;
  // Get the location of a global package by its name.
  // E.g. 'LCLBase'  -> '/Applications/Lazarus/lcl/lclbase.lpk'
  function LookupGlobalPackage(const Name: String): String;

implementation

uses
  Classes, SysUtils, contnrs, FileUtil, DOM, XMLRead, LazFileUtils, 
  general, udebug;

var
  PkgNameToPath: TFPStringHashTable;
  // Path -> TPackage
  PkgCache: TFPObjectHashTable;

procedure PopulateGlobalPackages;
var
  Files: TStringList;
  FileName, Name: String;
begin
  Files := TStringList.Create;
  try
    FindAllFiles(Files, '/Applications/Lazarus/components', '*.lpk');
    FindAllFiles(Files, '/Applications/Lazarus/lcl', '*.lpk');
    for FileName in Files do
    begin
      Name := ExtractFileNameOnly(FileName);
      PkgNameToPath[UpperCase(Name)] := FileName;
    end;
  finally
    Files.Free;
  end;
end;

procedure LoadPackageOrProject(const FileName: String);
var
  Doc: TXMLDocument;
  Dir: String;
  Root: TDomNode;
  Package: TPackage;

  function GetAdditionalPaths(SearchPaths: TDomNode; const What: String): String;
  var
    Node: TDomNode;
    Segments: TStringArray;
    S, Segment, AbsSegment: String;
  begin
    Result := '';

    Node := SearchPaths.FindNode(What);
    if Assigned(Node) then
      Node := Node.Attributes.GetNamedItem('Value');
    if not Assigned(Node) then
      Exit;

    S := Node.NodeValue;
    Segments := S.Split([';'], TStringSplitOptions.ExcludeEmpty);

    for Segment in Segments do
    begin
      AbsSegment := CreateAbsolutePath(Segment, Dir);
      Result     := Result + ';' + AbsSegment;
    end;
  end;

  procedure LoadPaths;
  var
    CompilerOptions, SearchPaths: TDomNode;
  begin
    CompilerOptions := Root.FindNode('CompilerOptions');
    if not Assigned(CompilerOptions) then
      Exit;

    SearchPaths := CompilerOptions.FindNode('SearchPaths');
    if not Assigned(SearchPaths) then
      Exit;

    Package.Paths.IncludePath := MergePaths([Dir, GetAdditionalPaths(SearchPaths, 'IncludeFiles')]);
    Package.Paths.UnitPath    := MergePaths([Dir, GetAdditionalPaths(SearchPaths, 'OtherUnitFiles')]);
    Package.Paths.SrcPath     := GetAdditionalPaths(SearchPaths, 'SrcPath');
  end;

  procedure LoadDeps;
  var
    Deps, Item, Name, Path, Prefer: TDomNode;
    Dep: TDependency;
    i, DepCount: integer;
  begin
    if UpperCase(ExtractFileExt(FileName)) = '.LPK' then
      Deps := Root.FindNode('RequiredPkgs')
    else
      Deps := Root.FindNode('RequiredPackages');

    if not Assigned(Deps) then
      Exit;

    DepCount := 0;
    SetLength(Package.Dependencies, Deps.ChildNodes.Count);

    for i := 0 to Deps.ChildNodes.Count - 1 do
    begin
      Item := Deps.ChildNodes.Item[i];

      Name := Item.FindNode('PackageName');
      if not Assigned(Name) then
        continue;

      Name := Name.Attributes.GetNamedItem('Value');
      if not Assigned(Name) then
        continue;

      Dep.Name := Name.NodeValue; 
      Dep.Prefer := False;

      Path := Item.FindNode('DefaultFilename');
      if Assigned(Path) then
      begin
        Prefer := Path.Attributes.GetNamedItem('Prefer');
        Path   := Path.Attributes.GetNamedItem('Value');

        Dep.Prefer := Assigned(Prefer) and (Prefer.NodeValue = 'True');
        if Assigned(Path) then
          Dep.Path := CreateAbsolutePath(Path.NodeValue, Dir);
      end;

      Package.Dependencies[DepCount] := Dep;
      Inc(DepCount);
    end;
  end;

begin
  if Assigned(PkgCache[FileName]) then
    Exit;

  Package := TPackage.Create;
  Package.Valid := False;

  PkgCache[FileName] := Package;

  try
    try
      Dir := ExtractFilePath(FileName);

      ReadXMLFile(doc, filename);

      Root := Doc.DocumentElement;
      if Root.NodeName <> 'CONFIG' then
        Exit;

      if UpperCase(ExtractFileExt(FileName)) = '.LPK' then
        Root := Root.FindNode('Package')
      else
        Root := Root.FindNode('ProjectOptions');


      if not Assigned(Root) then
        Exit;

      LoadPaths;
      LoadDeps;

      Package.Valid := True;
    except
      // swallow
      DebugLog('Error loading %s', [FileName]);
    end;
  finally
    if Assigned(doc) then
      FreeAndNil(doc);
  end;
end;

function GetPackageOrProject(const FileName: String): TPackage;
begin
  Result := TPackage(PkgCache[FileName]);
  if not Assigned(Result) then
  begin
    LoadPackageOrProject(FileName);
    Result := TPackage(PkgCache[FileName]);
  end;
end;

function LookupGlobalPackage(const Name: String): String;
begin
  Result := PkgNameToPath[UpperCase(Name)];
end;

{ TPackage }

constructor TPackage.Create;
begin
  Valid      := False;
  Configured := False;
end;

initialization

PkgNameToPath := TFPStringHashTable.Create;
PkgCache      := TFPObjectHashTable.Create;
PopulateGlobalPackages;


end.

