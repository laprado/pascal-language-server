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
  SysUtils, Classes, CodeToolManager, CodeToolsConfig, URIParser, LazUTF8,
  lsp, capabilities, DefineTemplates;

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

implementation

uses
  fileutil, DOM, XMLRead, udebug, packages;

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

procedure AddPathsFromPackage(const FileName: String; var Paths: TPaths);
var
  Pkg: TPackage;
  Dep: TDependency;
  DepPath: String;
begin
  Pkg := GetPackageOrProject(FileName);
  Paths.IncludePath := Paths.IncludePath + ';' + Pkg.Paths.IncludePath;
  Paths.UnitPath    := Paths.UnitPath    + ';' + Pkg.Paths.UnitPath;
  Paths.SrcPath     := Paths.SrcPath     + ';' + Pkg.Paths.SrcPath;

  // Load dependencies
  for Dep in Pkg.Dependencies do
  begin
    if Dep.Prefer then
      DepPath := Dep.Path
    else
      DepPath := LookupGlobalPackage(Dep.Name);

    if DepPath = '' then
    begin
      DebugLog('Package not found: %s', [Dep.Name]);
      continue;
    end;

    // TODO: Recurse properly
    Pkg := GetPackageOrProject(DepPath);
    Paths.IncludePath := Paths.IncludePath + ';' + Pkg.Paths.IncludePath;
    Paths.UnitPath    := Paths.UnitPath    + ';' + Pkg.Paths.UnitPath;
    Paths.SrcPath     := Paths.SrcPath     + ';' + Pkg.Paths.SrcPath;

  end;
end;

procedure SetupPaths(const Dir: string; const ParentPaths: TPaths);
var
  Packages, SubDirectories: TStringList;
  i: integer;
  Paths: TPaths;

  DirectoryTemplate: TDefineTemplate;
  IncludeTemplate: TDefineTemplate;
  UnitPathTemplate: TDefineTemplate;
  SrcTemplate: TDefineTemplate;
begin
  if ExtractFileName(Dir) = '.git' then
    Exit;

  Packages          := nil;
  SubDirectories    := nil;

  Paths.IncludePath := Dir;
  Paths.UnitPath    := Dir;
  Paths.SrcPath     := '';

  try
    Packages := FindAllFiles(Dir, '*.lpi;*.lpk', False, faAnyFile and not faDirectory);
    for i := 0 to Packages.Count - 1 do
      AddPathsFromPackage(Packages[i], Paths);

    if Packages.Count = 0 then
    begin
      Paths.IncludePath := Paths.IncludePath + ';' + ParentPaths.IncludePath;
      Paths.UnitPath    := Paths.UnitPath    + ';' + ParentPaths.UnitPath;
      Paths.SrcPath     := Paths.SrcPath     + ';' + ParentPaths.SrcPath;
    end;

    // Inform CodeToolBoss

    DirectoryTemplate := TDefineTemplate.Create(
      'Directory', '',
      '', Dir,  da_Directory
    );

    UnitPathTemplate := TDefineTemplate.Create(
      'Add to the UnitPath', '',
      UnitPathMacroName, UnitPathMacro+';'+Paths.UnitPath, da_Define
    );

    IncludeTemplate := TDefineTemplate.Create(
      'Add to the Include path', '',
      IncludePathMacroName, IncludePathMacro+';'+Paths.IncludePath, da_Define
    );

    SrcTemplate := TDefineTemplate.Create(
      'Add to the Src path', '',
      SrcPathMacroName, SrcPathMacro+';'+Paths.SrcPath, da_Define
    );

    DirectoryTemplate.AddChild(UnitPathTemplate);
    DirectoryTemplate.AddChild(IncludeTemplate);
    DirectoryTemplate.AddChild(SrcTemplate);

    CodeToolBoss.DefineTree.Add(DirectoryTemplate);

    DebugLog('--- %s ---', [Dir]);
    DebugLog('  UnitPath: %s', [Paths.UnitPath]);

    // Recurse into child directories

    SubDirectories := FindAllDirectories(Dir, False);
    for i := 0 to SubDirectories.Count - 1 do
      SetupPaths(SubDirectories[i], Paths);
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
  DirectoryTemplate, UnitPathTemplate: TDefineTemplate;

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

  SetupPaths(Directory, Paths);

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

