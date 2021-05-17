// Pascal Language Server
// Copyright 2020 Arjan Adriaanse

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

unit general;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  capabilities, lsp;

type

  { TVoidParams }

  TVoidParams = class(TPersistent);

  { TInitializeParams }

  TInitializeParams = class(TPersistent)
  private
    //fProcessId: string;
    fRootUri: string;
    fCapabilities: TClientCapabilities;
  public
    destructor Destroy; override;
  published
    //property processId: string read fProcessId write fProcessId;
    property rootUri: string read fRootUri write fRootUri;
    property capabilities: TClientCapabilities read fCapabilities write fCapabilities;
  end;

  { TInitializeResult }

  TInitializeResult = class(TPersistent)
  private
    fCapabilities: TServerCapabilities;
  public
    destructor Destroy; override;
  published
    property capabilities: TServerCapabilities read fCapabilities write fCapabilities;
  end;

  { TInitialize }

  TInitialize = class(specialize TLSPRequest<TInitializeParams, TInitializeResult>)
    function Process(var Params : TInitializeParams): TInitializeResult; override;
  end;

  { TInitialized }

  TInitialized = class(specialize TLSPNotification<TVoidParams>)
    procedure Process(var Params : TVoidParams); override;
  end;

  { TCancelParams }

  TCancelParams = class(TPersistent)
  private
    fId: Integer;
  published
    property id: Integer read fId write fId;
  end;

  { TShutdown }

  TShutdown = class(specialize TLSPRequest<TVoidParams, TPersistent>)
    function Process(var Params : TVoidParams): TPersistent; override;
  end;

  { TExit }

  TExit = class(specialize TLSPNotification<TVoidParams>)
    procedure Process(var Params : TVoidParams); override;
  end;

  { TCancel }

  TCancel = class(specialize TLSPNotification<TCancelParams>)
    procedure Process(var Params : TCancelParams); override;
  end;

  function MergePaths(Paths: array of String): String;

implementation

uses
  CodeToolManager, CodeToolsConfig, URIParser, LazUTF8, DefineTemplates,
  fileutil, DOM, XMLRead, 
  udebug, packages;

{ TInitializeParams }

destructor TInitializeParams.Destroy;
begin
  FreeAndNil(fCapabilities);
  inherited Destroy;
end;

{ TInitializeResult }

destructor TInitializeResult.Destroy;
begin
  FreeAndNil(fCapabilities);
  inherited Destroy;
end;

{ TInitialize }       

function MergePaths(Paths: array of String): String;
var
  i: integer;
begin
  Result := '';
  for i := low(Paths) to high(Paths) do
  begin
    if (Result <> '') and (Paths[i] <> '') then
      Result := Result + ';' + Paths[i]
    else if (Result = '') and (Paths[i] <> '') then
      Result := Paths[i];
  end;
end;

procedure ResolveDeps(Pkg: TPackage);
var
  Dep: ^TDependency;
  DepPath: String;
  i: integer;
begin
  if Pkg.DidResolveDependencies then
    exit;

  Pkg.DidResolveDependencies := True;

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

    Pkg.ResolvedPaths.IncludePath := MergePaths([Pkg.ResolvedPaths.IncludePath, Dep.Package.ResolvedPaths.IncludePath]);
    Pkg.ResolvedPaths.UnitPath    := MergePaths([Pkg.ResolvedPaths.UnitPath,    Dep.Package.ResolvedPaths.UnitPath]);
    Pkg.ResolvedPaths.SrcPath     := MergePaths([Pkg.ResolvedPaths.SrcPath,     Dep.Package.ResolvedPaths.SrcPath]);
  end;
end;

// Add required search paths to package's directory (and its subdirectories).
// TODO: Should we also add the search paths to all of the other unit directories
// specified in the package? This would probably be the correct way, but any
// sane project structure will have the package/project file in the root of its
// source anyway.
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
    Packages := FindAllFiles(Dir, '*.lpi;*.lpk', False, faAnyFile and not faDirectory);

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
    Packages := FindAllFiles(Dir, '*.lpi;*.lpk', False, faAnyFile and not faDirectory);

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

    Packages := FindAllFiles(Dir, '*.lpi;*.lpk', False, faAnyFile and not faDirectory);

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

function TInitialize.Process(var Params : TInitializeParams): TInitializeResult;
var
  CodeToolsOptions: TCodeToolsOptions;

  Directory: String;

  Paths: TPaths;
begin
  CodeToolsOptions := TCodeToolsOptions.Create;

  URIToFilename(Params.rootUri, Directory);

  Paths.IncludePath := '';
  Paths.UnitPath    := '';
  Paths.SrcPath     := '';

  with CodeToolsOptions do
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
  with CodeToolBoss do
  begin
    Init(CodeToolsOptions);
    IdentifierList.SortForHistory := True;
    IdentifierList.SortForScope := True;
  end;

  LoadAllPackagesUnderPath(Directory);
  GuessMissingDepsForAllPackages(Directory);
  ConfigurePaths(Directory, Paths);

  Result := TInitializeResult.Create;
  Result.capabilities := TServerCapabilities.Create;

  FreeAndNil(CodeToolsOptions);
end;

{ TInitialized }

procedure TInitialized.Process(var Params : TVoidParams);
begin
  // do nothing
end;

{ TShutdown }

function TShutdown.Process(var Params : TVoidParams): TPersistent;
begin
  // do nothing
end;

{ TExit }

procedure TExit.Process(var Params : TVoidParams);
begin
  Halt(0);
end;

{ TCancel }

procedure TCancel.Process(var Params : TCancelParams);
begin
  // not supported
end;

initialization
  LSPHandlerManager.RegisterHandler('initialize', TInitialize);
  LSPHandlerManager.RegisterHandler('initialized', TInitialized);
  LSPHandlerManager.RegisterHandler('shutdown', TShutdown);
  LSPHandlerManager.RegisterHandler('exit', TExit);
  LSPHandlerManager.RegisterHandler('$/cancelRequest', TCancel);
end.

