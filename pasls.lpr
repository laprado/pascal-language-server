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

program pasls;

{$mode objfpc}{$H+}

uses
  SysUtils, fpjson, jsonparser, jsonscanner,
  lsp, general, synchronization, completion,
  classes, projects, udebug;

const
  _Request: String = 'Request:'#10;
  _Response: String = 'Response:'#10;
  ContentType: String = 'application/vscode-jsonrpc; charset=utf-8';

var
  Dispatcher: TLSPDispatcher;
  Header, Name, Value, Content: string;
  RequestContent: string;
  s: string;
  I, Length: Integer;
  Request, Response: TJSONData;

function Min(x,y: integer): integer;
begin
  if x < y then
    Result := x
  else
    Result := y;
end;

const
  Mock: array of string =
    (
      '{"id": 1, "jsonrpc": "2.0", "method": "initialize", "params": {"initializationOptions": [], "rootUri": "file:///Users/isopod/dev/pascal-language-server", "capabilities": {"workspace": {"workspaceFolders": true, "applyEdit": true, "symbol": {"symbolKind": {"valueSet": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]}, "dynamicRegistration": false, "hierarchicalWorkspaceSymbolSupport": true}, "configuration": true}, "window": {"showDocument": {"support": false}, "showMessage": {"messageActionItem": {"additionalPropertiesSupport": false}}, "workDoneProgress": true}, "callHierarchy": {"dynamicRegistration": false}, "textDocument": {"implementation": {"linkSupport": true}, "documentSymbol": {"symbolKind": {"valueSet": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]}, "dynamicRegistration": false, "hierarchicalDocumentSymbolSupport": true}, "references": {"dynamicRegistration": false}, "documentHighlight": {"dynamicRegistration": false}, "rename": {"prepareSupport": true, "dynamicRegistration": false}, "codeAction": {"codeActionLiteralSupport": {"codeActionKind": {"valueSet": ["", "Empty", "QuickFix", "Refactor", "RefactorExtract", "RefactorInline", "RefactorRewrite", "Source", "SourceOrganizeImports", "quickfix", "refactor", "refactor.extract", "refactor.inline", "refactor.rewrite", "source", "source.organizeImports"]}}, "dynamicRegistration": false}, "completion": {"completionItem": {"preselectSupport": false, "commitCharactersSupport": false, "snippetSupport": false, "deprecatedSupport": false, "documentationFormat": ["markdown", "plaintext"]}, "contextSupport": false, "dynamicRegistration": false, "completionItemKind": {"valueSet": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]}}, "definition": {"linkSupport": true}, "declaration": {"linkSupport": true}, "signatureHelp": {"signatureInformation": {"documentationFormat": ["markdown", "plaintext"]}, "dynamicRegistration": false}, "hover": {"dynamicRegistration": false, "contentFormat": ["markdown", "plaintext"]}, "publishDiagnostics": {"relatedInformation": true, "tagSupport": {"valueSet": [1, 2]}}, "synchronization": {"dynamicRegistration": false, "willSaveWaitUntil": false, "willSave": false, "didSave": true}, "typeDefinition": {"linkSupport": true}}}, "rootPath": "/Users/isopod/dev/pascal-language-server", "clientInfo": {"version": "0.5.0", "name": "Neovim"}, "processId": 25338, "trace": "off", "workspaceFolders": [{"uri": "file:///Users/isopod/dev/pascal-language-server", "name": "/Users/isopod/dev/pascal-language-server"}]}}',
      '{"id": 3, "jsonrpc": "2.0", "method": "textDocument/completion", "params": {"textDocument": {"uri": "file:///Users/isopod/dev/pascal-language-server/capabilities.pas"}, "position": {"character": 21, "line": 78}}}'
    );
begin
  Dispatcher := TLSPDispatcher.Create(nil);
  TJSONData.CompressedJSON := True;
  SetTextLineEnding(Input, #13#10);
  SetTextLineEnding(Output, #13#10);


  {$IFNDEF foo}
  while not EOF do
  begin
    ReadLn(Header);
    while Header <> '' do
    begin
      I := Pos(':', Header);
      Name := Copy(Header, 1, I - 1);
      Delete(Header, 1, i);
      Value := Trim(Header);
      if Name = 'Content-Length' then Length := StrToInt(Value);
      ReadLn(Header);
    end;

    Content := '';
    SetLength(Content, Length);
    I := 1;
    while I <= Length do
    begin
      Read(Content[I]);
      Inc(I);
    end;

  {$ELSE}
  for s in mock do
  begin
    Content := s;
  {$ENDIF}
    RequestContent := Content;

    Request := TJSONParser.Create(Content, DefaultOptions).Parse;
    Response := Dispatcher.Execute(Request);

    DebugLog(Copy(_Request, 1, 200) + Copy(RequestContent, 1, 200));

    if Assigned(Response) then
    begin
      Content := Response.AsJSON;
      WriteLn('Content-Type: ', ContentType);
      WriteLn('Content-Length: ', Content.Length);
      WriteLn;
      Write(Content);
      Flush(Output);
    end
    else
      Content := '-';

    DebugLog(Copy(_Response, 1, 200) + Copy(Content, 1, 200));

    FreeAndNil(Request);
    FreeAndNil(Response);
  end;

  FreeAndNil(Dispatcher);
end.
