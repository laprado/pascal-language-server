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
  classes, iostream, streamex, packages, udebug, ubufferedreader;

procedure Main(Reader: TBufferedReader);
const
  ContentType: String = 'application/vscode-jsonrpc; charset=utf-8';
var
  Dispatcher: TLSPDispatcher;
  Header, Name, Value, Content: string;
  RequestContent: string;
  I, Len: Integer;
  Request, Response: TJSONData;
  rid: integer;

begin
  Dispatcher := TLSPDispatcher.Create(nil);
  TJSONData.CompressedJSON := True;
  SetTextLineEnding(Output, #13#10);

  rid := 0;

  while True do
  begin
    Header := Reader.ReadLine;
    if Header = '' then
      Exit;

    Len := 0;
    while Header <> '' do
    begin
      I := Pos(':', Header);
      Name := Copy(Header, 1, I - 1);
      Delete(Header, 1, i);
      Value := Trim(Header);
      if Name = 'Content-Length' then
        Len := StrToInt(Value);
      Header := Reader.ReadLine;
    end;

    if Len = 0 then
      continue;

    Content := '';
    SetLength(Content, Len);

    Reader.BlockRead(Content[1], Len);

    RequestContent := Content;

    Request := TJSONParser.Create(Content, DefaultOptions).Parse;
    Response := Dispatcher.Execute(Request);

    DebugLog('> Request (%d): '#10'%s', [rid, Copy(RequestContent, 1, 2000)]);
    Inc(rid);

    if Assigned(Response) then
    begin
      Content := Response.AsJSON;
      WriteLn(Output, 'Content-Type: ', ContentType);
      WriteLn(Output, 'Content-Length: ', Content.Length);
      WriteLn(Output);
      Write(Output, Content);
      Flush(Output);
    end
    else
      Content := '-';

    DebugLog('< Response: '#10'%s', [Copy(Content, 1, 2000)]);

    FreeAndNil(Request);
    FreeAndNil(Response);
  end;

  FreeAndNil(Dispatcher);
end;


var
  InputStream: TStream;
  InputReader: TBufferedReader;

begin
  InputStream := nil;
  InputReader := nil;

  try
    InputStream := TIOStream.Create(iosInput);
    //InputStream := TFileStream.Create('/Users/isopod/dev/pascal-language-server/test.txt', fmOpenRead);
    InputReader := TBufferedReader.Create(InputStream);

    Main(InputReader);
  finally
    if Assigned(InputStream) then
      FreeAndNil(InputStream);
    if Assigned(InputReader) then
      FreeAndNil(InputReader);
  end;

end.
