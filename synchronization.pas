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

unit synchronization;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, jsonstream, lsp, basic;

procedure TextDocument_DidOpen(Reader: TJsonReader);
procedure TextDocument_DidChange(Reader: TJsonReader);

implementation

uses
  URIParser, CodeToolManager, CodeCache;

function ParseChangeOrOpen(
  Reader: TJsonReader; out Uri: string; out Content: string; IsChange: Boolean
): Boolean;
var
  Key:                  string;
  HaveUri, HaveContent: Boolean;
begin 
  HaveUri     := false;
  HaveContent := false;
  if Reader.Dict then
    while (Reader.Advance <> jsDictEnd) and Reader.Key(Key) do
    begin
      if (Key = 'textDocument') and Reader.Dict then
        while (Reader.Advance <> jsDictEnd) and Reader.Key(Key) do
        begin
          if (Key = 'uri') and Reader.Str(Uri) then
            HaveUri := true
          else if not IsChange and (Key = 'text') and Reader.Str(Content) then
            HaveContent := true;
        end
      else if IsChange and (Key = 'contentChanges') and Reader.List then
        while Reader.Advance <> jsListEnd do
        begin
          if Reader.Dict then
            while (Reader.Advance <> jsDictEnd) and (Reader.Key(Key)) do
            begin
              if (Key = 'text') and Reader.Str(Content) then
                HaveContent := true;
            end;
        end;
    end;
  Result := HaveUri and HaveContent;
end;

procedure TextDocument_DidOpen(Reader: TJsonReader);
var
  Code:        TCodeBuffer;
  UriStr:      string;
  Uri:         TURI;
  Content:     string;
begin 
  if ParseChangeOrOpen(Reader, UriStr, Content, false) then
  begin
    Uri         := ParseURI(UriStr);
    Code        := CodeToolBoss.LoadFile(URI.Path + URI.Document, false, false);
    Code.Source := Content;
  end;
end;

procedure TextDocument_DidChange(Reader: TJsonReader);
var
  Code:        TCodeBuffer;
  UriStr:      string;
  Uri:         TURI;
  Content:     string;
begin 
  if ParseChangeOrOpen(Reader, UriStr, Content, true) then
  begin
    Uri         := ParseURI(UriStr);
    Code        := CodeToolBoss.FindFile(URI.Path + URI.Document);
    Code.Source := Content;
  end;
end;

initialization
  {
  LSPHandlerManager.RegisterHandler('textDocument/didOpen', TDidOpenTextDocument);
  LSPHandlerManager.RegisterHandler('textDocument/didChange', TDidChangeTextDocument);
  }
end.
