// Pascal Language Server
// Copyright 2020 Arjan Adriaanse
//           2021 Philip Zander

// This file is part of Pascal Language Server.

// Pascal Language Server is free software: you can redistribute it
// and/or modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.

// Pascal Language Server is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied warranty
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Pascal Language Server.  If not, see
// <https://www.gnu.org/licenses/>.

unit uinitialize;

{$mode objfpc}{$H+}

interface

uses
  jsonstream, ujsonrpc;

procedure Initialize(Rpc: TRpcPeer; Request: TRpcRequest);

implementation

uses
  SysUtils, Classes, CodeToolManager, CodeToolsConfig, URIParser, LazUTF8,
  DefineTemplates, FileUtil, LazFileUtils, DOM, XMLRead, udebug, uutils,
  upackages;

procedure ResolveDeps(Pkg: TPackage);
var
  Dep: ^TDependency;
  DepPath: String;
  i: integer;
begin
  if Pkg.DidResolveDeps then
    exit;

  Pkg.DidResolveDeps := True;

  for i := low(Pkg.Dependencies) to high(Pkg.Dependencies) do
  begin
    Dep := @Pkg.Dependencies[i];

    DepPath := LookupGlobalPackage(Dep^.Name);
    if (Dep^.Prefer) or (DepPath = '') then
      DepPath := Dep^.Path;

    if DepPath = '' then
    begin
      DebugLog('* Dependency %s: not found', [Dep^.Name]);
      continue;
    end;

    DebugLog('* Dependency: %s -> %s', [Dep^.Name, DepPath]);

    Dep^.Package := GetPackageOrProject(DepPath);

    // Add ourselves to the RequiredBy list of the dependency.
    SetLength(Dep^.Package.RequiredBy, Length(Dep^.Package.RequiredBy) + 1);
    Dep^.Package.RequiredBy[High(Dep^.Package.RequiredBy)] := Pkg;

    // Recurse
    ResolveDeps(Dep^.Package);
  end;
end;

procedure GuessMissingDependencies(Pkg: TPackage);
var
  Dep: ^TDependency;
  i: integer;

  function GuessDependency(Node: TPackage; DepName: String): TPackage;
  var
    j: integer;
  begin
    Result := nil;

    if Node.Visited then
      exit;

    Node.Visited := True;
    try
      for j := low(Node.Dependencies) to high(Node.Dependencies) do
      begin
        if (UpperCase(DepName) = UpperCase(Node.Dependencies[j].Name)) and
           Assigned(Node.Dependencies[j].Package) then
        begin
          Result := Node.Dependencies[j].Package;
          exit;
        end;
      end;

      // Not found, recurse

      for j := low(Node.RequiredBy) to high(Node.RequiredBy) do
      begin
        Result := GuessDependency(Node.RequiredBy[j], DepName);
        if Assigned(Result) then
          exit;
      end;

    finally
      Node.Visited := False;
    end;
  end;
begin
  for i := low(Pkg.Dependencies) to high(Pkg.Dependencies) do
  begin
    Dep := @Pkg.Dependencies[i];
    if Assigned(Dep^.Package) then
      continue;

    Dep^.Package := GuessDependency(Pkg, Dep^.Name);
  end;
end;

procedure ResolvePaths(Pkg: TPackage);
var
  Dep: TDependency;
begin
  if Pkg.DidResolvePaths then
    exit;

  Pkg.DidResolvePaths := True;

  Pkg.ResolvedPaths := Pkg.Paths;

  for Dep in Pkg.Dependencies do
  begin
    if not Assigned(Dep.Package) then
      continue;

    // Recurse
    ResolvePaths(Dep.Package);

    Pkg.ResolvedPaths.IncludePath := MergePaths([
      Pkg.ResolvedPaths.IncludePath,
      Dep.Package.ResolvedPaths.IncludePath
    ]);
    Pkg.ResolvedPaths.UnitPath := MergePaths([
      Pkg.ResolvedPaths.UnitPath,
      Dep.Package.ResolvedPaths.UnitPath
    ]);
    Pkg.ResolvedPaths.SrcPath := MergePaths([
      Pkg.ResolvedPaths.SrcPath,
      Dep.Package.ResolvedPaths.SrcPath
    ]);
  end;
end;

// Add required search paths to package's directory (and its subdirectories).
// TODO: Should we also add the search paths to all of the other unit
// directories specified in the package? This would probably be the correct way,
// but any sane project structure will have the package/project file in the root
// of its source anyway.
procedure ConfigurePackage(Pkg: TPackage);
var
  DirectoryTemplate,
  IncludeTemplate,
  UnitPathTemplate,
  SrcTemplate:       TDefineTemplate;
  Dep: TDependency;
begin
  if Pkg.Configured then
    exit;
  Pkg.Configured := True;
 
  DirectoryTemplate := TDefineTemplate.Create(
    'Directory', '',
    '', Pkg.Dir,
    da_Directory
  );

  UnitPathTemplate := TDefineTemplate.Create(
    'Add to the UnitPath', '',
    UnitPathMacroName, MergePaths([UnitPathMacro, Pkg.ResolvedPaths.UnitPath]),
    da_DefineRecurse
  );

  IncludeTemplate := TDefineTemplate.Create(
    'Add to the Include path', '',
    IncludePathMacroName, MergePaths([IncludePathMacro, Pkg.ResolvedPaths.IncludePath]),
    da_DefineRecurse
  );

  SrcTemplate := TDefineTemplate.Create(
    'Add to the Src path', '',
    SrcPathMacroName, MergePaths([SrcPathMacro, Pkg.ResolvedPaths.SrcPath]),
    da_DefineRecurse
  );

  DirectoryTemplate.AddChild(UnitPathTemplate);
  DirectoryTemplate.AddChild(IncludeTemplate);
  DirectoryTemplate.AddChild(SrcTemplate);

  CodeToolBoss.DefineTree.Add(DirectoryTemplate);

  // Recurse
  for Dep in Pkg.Dependencies do
  begin
    if not Assigned(Dep.Package) then
      continue;
    ConfigurePackage(Dep.Package);
  end;
end;

function IgnoreDirectory(const Dir: string): Boolean;
var
  DirName: string;
begin
  Dirname := lowercase(ExtractFileName(Dir));
  Result := 
    (DirName = '.git')                              or 
    ((Length(DirName) >= 1) and (DirName[1] = '.')) or
    (DirName = 'backup')                            or 
    (DirName = 'lib')                               or 
    (Pos('.dsym', DirName) > 0)                     or
    (Pos('.app', DirName) > 0);
end;

procedure LoadAllPackagesUnderPath(const Dir: string);
var
  Packages,
  SubDirectories:    TStringList;
  i:                 integer;     
  Pkg:               TPackage;
begin
  if IgnoreDirectory(Dir) then
    Exit;

  try
    Packages := FindAllFiles(
      Dir, '*.lpi;*.lpk', False, faAnyFile and not faDirectory
    );

    for i := 0 to Packages.Count - 1 do
    begin
      Pkg := GetPackageOrProject(Packages[i]);
      ResolveDeps(Pkg);
    end;

    // Recurse into child directories

    SubDirectories := FindAllDirectories(Dir, False);
    for i := 0 to SubDirectories.Count - 1 do
      LoadAllPackagesUnderPath(SubDirectories[i]);

  finally
    if Assigned(Packages) then
      FreeAndNil(Packages);
    if Assigned(Packages) then
      FreeAndNil(SubDirectories);
  end;
end;

procedure GuessMissingDepsForAllPackages(const Dir: string);
var
  Packages,
  SubDirectories:    TStringList;
  i:                 integer;
  Pkg:               TPackage;
begin
  if IgnoreDirectory(Dir) then
    Exit;

  try
    Packages := FindAllFiles(
      Dir, '*.lpi;*.lpk', False, faAnyFile and not faDirectory
    );

    for i := 0 to Packages.Count - 1 do
    begin
      Pkg := GetPackageOrProject(Packages[i]);
      GuessMissingDependencies(Pkg);
    end;

    // Recurse into child directories

    SubDirectories := FindAllDirectories(Dir, False);
    for i := 0 to SubDirectories.Count - 1 do
      GuessMissingDepsForAllPackages(SubDirectories[i]);

  finally
    if Assigned(Packages) then
      FreeAndNil(Packages);
    if Assigned(Packages) then
      FreeAndNil(SubDirectories);
  end;
end;

// Use heuristic to add search paths to the directory 'Dir'.
// If there are any projects (.lpi) or packages (.lpk) in the directory, use
// (only) their search paths. Otherwise, inherit the search paths from the
// parent directory ('ParentPaths').
// TODO: Should we also add the search paths to other UnitPaths mentioned in the
// project/package files? See also comment before ConfigurePackage.
procedure ConfigurePaths(const Dir: string; const ParentPaths: TPaths);
var
  Packages,
  SubDirectories:    TStringList;
  i:                 integer;
  Paths:             TPaths;

  DirectoryTemplate,
  IncludeTemplate,
  UnitPathTemplate,
  SrcTemplate:       TDefineTemplate;
  Pkg:               TPackage;

begin
  if IgnoreDirectory(Dir) then
    Exit;

  Packages          := nil;
  SubDirectories    := nil;

  Paths.IncludePath := Dir;
  Paths.UnitPath    := Dir;
  Paths.SrcPath     := '';

  try
    DebugLog('--- %s ---', [Dir]);

    Packages := FindAllFiles(
      Dir, '*.lpi;*.lpk', False, faAnyFile and not faDirectory
    );

    // 1. Recursively merge search paths
    for i := 0 to Packages.Count - 1 do
    begin
      Pkg := GetPackageOrProject(Packages[i]);
      ResolvePaths(Pkg);
    end;

    // 2. Configure package directories
    for i := 0 to Packages.Count - 1 do
    begin
      Pkg := GetPackageOrProject(Packages[i]);
      ConfigurePackage(Pkg);
    end;

    // 3. Add merged search paths for all packages/projects in directory to
    //    search path of directory.
    for i := 0 to Packages.Count - 1 do
    begin
      Pkg := GetPackageOrProject(Packages[i]);
      Paths.IncludePath := MergePaths([Paths.IncludePath, Pkg.ResolvedPaths.IncludePath]);
      Paths.UnitPath    := MergePaths([Paths.UnitPath,    Pkg.ResolvedPaths.UnitPath]);
      Paths.SrcPath     := MergePaths([Paths.SrcPath,     Pkg.ResolvedPaths.SrcPath]);
    end;

    if Packages.Count = 0 then
    begin
      Paths.IncludePath := MergePaths([Paths.IncludePath, ParentPaths.IncludePath]);
      Paths.UnitPath    := MergePaths([Paths.UnitPath,    ParentPaths.UnitPath]);
      Paths.SrcPath     := MergePaths([Paths.SrcPath,     ParentPaths.SrcPath]);
    end;

    // Inform CodeToolBoss

    DirectoryTemplate := TDefineTemplate.Create(
      'Directory', '',
      '', Dir,
      da_Directory
    );

    UnitPathTemplate := TDefineTemplate.Create(
      'Add to the UnitPath', '',
      UnitPathMacroName, MergePaths([UnitPathMacro, Paths.UnitPath]),
      da_Define
    );

    IncludeTemplate := TDefineTemplate.Create(
      'Add to the Include path', '',
      IncludePathMacroName, MergePaths([IncludePathMacro, Paths.IncludePath]),
      da_Define
    );

    SrcTemplate := TDefineTemplate.Create(
      'Add to the Src path', '',
      SrcPathMacroName, MergePaths([SrcPathMacro, Paths.SrcPath]),
      da_Define
    );

    DirectoryTemplate.AddChild(UnitPathTemplate);
    DirectoryTemplate.AddChild(IncludeTemplate);
    DirectoryTemplate.AddChild(SrcTemplate);

    CodeToolBoss.DefineTree.Add(DirectoryTemplate);

    DebugLog('  UnitPath: %s', [Paths.UnitPath]);

    // Recurse into child directories

    SubDirectories := FindAllDirectories(Dir, False);
    for i := 0 to SubDirectories.Count - 1 do
      ConfigurePaths(SubDirectories[i], Paths);
  finally
    if Assigned(Packages) then
      FreeAndNil(Packages);
    if Assigned(Packages) then
      FreeAndNil(SubDirectories);
  end;
end;

// yuck
var
  _FakeAppName, _FakeVendorName: string;

function GetFakeAppName: string;
begin
  Result := _FakeAppName;
end;

function GetFakeVendorName: string;
begin
  Result := _FakeVendorName;
end;

function MyGetAppConfigDir(AppName, Vendor: string; Global: Boolean): string;
var
  OldGetAppName:     TGetAppNameEvent;
  OldGetVendorName:  TGetVendorNameEvent;
begin
  _FakeAppName     := AppName;
  _FakeVendorName  := Vendor;
  OldGetAppName    := OnGetApplicationName;
  OldGetVendorName := OnGetVendorName;
  try
    OnGetApplicationName := @GetFakeAppName;
    OnGetVendorName      := @GetFakeVendorName;
    Result               := GetAppConfigDir(Global);
  finally
    OnGetApplicationName := OldGetAppName;
    OnGetVendorName      := OldGetVendorName;
  end;
end;

procedure GuessCodeToolConfig(Options: TCodeToolsOptions);
var
  ConfigDirs:         TStringList;
  Dir:                string;
  Doc:                TXMLDocument;
  Root:               TDomNode;
  EnvironmentOptions, FPCConfigs, Item1: TDomNode;
  LazarusDirectory, FPCSourceDirectory, CompilerFilename, OS, CPU: string;

  function LoadLazConfig(Path: string): Boolean;
  begin
    Doc    := nil;
    Root   := nil;
    Result := false;
    try
      ReadXMLFile(Doc, Path);
      Root := Doc.DocumentElement;
      if Root.NodeName = 'CONFIG' then
        Result := true;
    except
      // Swallow
    end;
  end;

  function GetVal(Parent: TDomNode; Ident: string; Attr: string='Value'): string;
  var
    Node, Value: TDomNode;
  begin
    Result := '';
    if Parent = nil then
      exit;
    Node := Parent.FindNode(DOMString(Ident));
    if Node = nil then
      exit;
    Value := Node.Attributes.GetNamedItem(Attr);
    if Value = nil then
      exit;
    Result := string(Value.NodeValue);
  end;
begin
  ConfigDirs := TStringList.Create;
  try
    ConfigDirs.Add(MyGetAppConfigDir('lazarus', '', False));
    ConfigDirs.Add(GetUserDir + DirectorySeparator + '.lazarus');
    ConfigDirs.Add(MyGetAppConfigDir('lazarus', '', True));  ;
    for Dir in ConfigDirs do
    begin
      Doc := nil;
      try
        if LoadLazConfig(Dir + DirectorySeparator + 'environmentoptions.xml') then
        begin
          EnvironmentOptions := Root.FindNode('EnvironmentOptions');
          LazarusDirectory   := GetVal(EnvironmentOptions, 'LazarusDirectory');
          FPCSourceDirectory := GetVal(EnvironmentOptions, 'FPCSourceDirectory');
          CompilerFilename   := GetVal(EnvironmentOptions, 'CompilerFilename');
          if (Options.LazarusSrcDir = '') and (LazarusDirectory <> '') then
            Options.LazarusSrcDir := LazarusDirectory;
          if (Options.FPCSrcDir = '') and (FPCSourceDirectory <> '') then
            Options.FPCSrcDir := FPCSourceDirectory;
          if (Options.FPCPath = '') and (CompilerFilename <> '') then
            Options.FPCPath := CompilerFilename;
        end;
      finally
        FreeAndNil(Doc);
      end;

      Doc := nil;
      try
        if LoadLazConfig(Dir + DirectorySeparator + 'fpcdefines.xml') then
        begin
          FPCConfigs := Root.FindNode('FPCConfigs');
          Item1 := nil;
          if Assigned(FPCConfigs) and (FPCConfigs.ChildNodes.Count > 0) then
            Item1 := FPCConfigs.ChildNodes[0];
          OS  := GetVal(Item1, 'RealCompiler', 'OS');
          CPU := GetVal(Item1, 'RealCompiler', 'CPU');
          if (Options.TargetOS = '') and (OS <> '') then
            Options.TargetOS := OS;
          if (Options.TargetProcessor = '') and (CPU <> '') then
            Options.TargetProcessor := CPU;
        end;
      finally
        FreeAndNil(Doc);
      end;
    end;
  finally
    FreeAndNil(ConfigDirs);
  end;

end;

procedure Initialize(Rpc: TRpcPeer; Request: TRpcRequest);
var
  Options:   TCodeToolsOptions;
  Key:       string;
  s:         string;

  RootUri:   string;
  Directory: string;
  Paths:     TPaths;
  Response:  TRpcResponse;
  Reader:    TJsonReader;
  Writer:    TJsonWriter;
begin
  Options  := nil;
  Response := nil;

  try
    Options := TCodeToolsOptions.Create;
    Options.InitWithEnvironmentVariables;

    {
    with Options do
    begin
      InitWithEnvironmentVariables;
      ProjectDir      := Directory;

      // Could be loaded from .lazarus/fpcdefines.xml ?
      TargetOS        := 'Darwin';
      TargetProcessor := 'x86_64';

      // These could be loaded from .lazarus/environmentoptions.xml:
      FPCSrcDir       := '/usr/local/share/fpcsrc/3.2.0';
      LazarusSrcDir   := '/Applications/Lazarus';
      FPCPath         := '/usr/local/bin/fpc';
      TestPascalFile  := '/tmp/testfile1.pas';
    end;
    }     

    Reader := Request.Reader;
    if Reader.Dict then
      while (Reader.Advance <> jsDictEnd) and Reader.Key(Key) do
      begin
        if Key = 'rootUri' then
          Reader.Str(RootUri)
        else if (Key = 'initializationOptions') and Reader.Dict then
          while (Reader.Advance <> jsDictEnd) and Reader.Key(Key) do
          begin
            if (Key = 'PP') and Reader.Str(s) then
              Options.FPCPath := s
            else if (Key = 'FPCDIR') and Reader.Str(s) then
              Options.FPCSrcDir := s
            else if (Key = 'LAZARUSDIR') and Reader.Str(s) then
              Options.LazarusSrcDir := s
            else if (Key = 'FPCTARGET') and Reader.Str(s) then
              Options.TargetOS := s
            else if (Key = 'FPCTARGETCPU') and Reader.Str(s) then
              Options.TargetProcessor := s;
          end;
      end;

    // Try to fill in missing values from lazarus config
    GuessCodeToolConfig(Options);

    URIToFilename(RootUri, Directory);

    Options.ProjectDir     := Directory;
    Options.TestPascalFile := GetTempFileName;

    with CodeToolBoss do
    begin
      Init(Options);
      IdentifierList.SortForHistory := True;
      IdentifierList.SortForScope   := True;
    end;

    Paths.IncludePath := '';
    Paths.UnitPath    := '';
    Paths.SrcPath     := '';

    LoadAllPackagesUnderPath(Directory);
    GuessMissingDepsForAllPackages(Directory);
    ConfigurePaths(Directory, Paths);

    // Send response & announce our capabilities
    Response := TRpcResponse.Create(Request.Id);
    Writer   := Response.Writer;

    Writer.Dict;
      Writer.Key('serverInfo');
      Writer.Dict;
        Writer.Key('name');
        Writer.Str('Pascal Language Server');
      Writer.DictEnd;

      Writer.Key('capabilities');
      Writer.Dict;
        Writer.Key('textDocumentSync');
        Writer.Dict;
          Writer.Key('openClose');
          Writer.Bool(true);

          Writer.Key('change');
          Writer.Number(1); // 1 = Sync by sending full content, 2 = Incremental
        Writer.DictEnd;

        Writer.Key('completionProvider');
        Writer.Dict;
          Writer.Key('triggerCharacters');
          Writer.Null;

          Writer.Key('allCommitCharacters');
          Writer.Null;

          Writer.Key('resolveProvider');
          Writer.Bool(false);
        Writer.DictEnd;

        Writer.Key('signatureHelpProvider');
        Writer.Dict;
          Writer.Key('triggerCharacters');
          Writer.List;
            Writer.Str('(');
            Writer.Str(',');
          Writer.ListEnd;

          Writer.Key('retriggerCharacters');
          Writer.List;
          Writer.ListEnd;
        Writer.DictEnd;

        Writer.Key('declarationProvider');
        Writer.Bool(true);

        Writer.Key('definitionProvider');
        Writer.Bool(true);
      Writer.DictEnd;

      Writer.Key('workspaceFolders');
      Writer.Bool(true);
    Writer.DictEnd;

    Rpc.Send(Response);
  finally
    FreeAndNil(Options);
    FreeAndNil(Response);
  end;
end;

end.

