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
  Header, Name, Value: string;
  RequestBody, ResponseBody: String;
  i, Len: Integer;
  Request, Response: TJSONData;
  rid: integer;
  Parser: TJSONParser;

begin
  Dispatcher := nil;
  try
    Dispatcher := TLSPDispatcher.Create(nil);
    TJSONData.CompressedJSON := True;
    SetTextLineEnding(Output, #13#10);

    rid := 0;

    while True do
    begin
      // Read request headers
      Header := Reader.ReadLine;
      if Header = '' then
        Exit;

      Len := 0;
      while Header <> '' do
      begin
        i := Pos(':', Header);
        Name := Copy(Header, 1, i - 1);
        Delete(Header, 1, i);
        Value := Trim(Header);
        if Name = 'Content-Length' then
          Len := StrToInt(Value);
        Header := Reader.ReadLine;
      end;

      if Len = 0 then
        continue;

      // Read request body
      RequestBody := '';
      SetLength(RequestBody, Len);
      Reader.BlockRead(RequestBody[1], Len);

      // Parse & handle request
      Parser   := nil;
      Request  := nil;
      Response := nil;
      try
        Parser   := TJSONParser.Create(RequestBody, DefaultOptions);
        Request  := Parser.Parse;
        Response := Dispatcher.Execute(Request);

        DebugLog('> Request (%d): '#10'%s', [rid, Copy(RequestBody, 1, 2000)]);
        Inc(rid);

        if Assigned(Response) then
        begin
          ResponseBody := Response.AsJSON;
          WriteLn(Output, 'Content-Type: ', ContentType);
          WriteLn(Output, 'Content-Length: ', ResponseBody.Length);
          WriteLn(Output);
          Write(Output, ResponseBody);
          Flush(Output);
          DebugLog('< Response: '#10'%s', [Copy(ResponseBody, 1, 2000)]);
        end
        else
          DebugLog('< No Response'#10);

      finally
        if Assigned(Parser) then
          FreeAndNil(Parser);
        if Assigned(Request) then
          FreeAndNil(Request);
        if Assigned(Response) then
          FreeAndNil(Response);
      end;
    end;

  finally
    if Assigned(Dispatcher) then
      FreeAndNil(Dispatcher);
  end;
end;


var
  InputStream: TStream;
  Transcript:  TStream;
  Tee:         TStream;
  InputReader: TBufferedReader;

begin
  InputStream := nil;
  InputReader := nil;
  Transcript  := nil;
  Tee         := nil;

  try
    InputStream := TIOStream.Create(iosInput);

    {$IF 1}
    Transcript := TFileStream.Create('/Users/isopod/pasls-transcript.txt', fmCreate or fmOpenWrite);
    Tee := TTeeStream.Create(InputStream, Transcript);
    InputReader := TBufferedReader.Create(Tee);
    {$ELSE}
    InputStream := TFileStream.Create('/Users/isopod/pasls-transcript.txt', fmOpenRead);
    InputReader := TBufferedReader.Create(InputStream);
    {$ENDIF}

    Main(InputReader);
  finally
    if Assigned(InputStream) then
      FreeAndNil(InputStream);
    if Assigned(Transcript) then
      FreeAndNil(Transcript);
    if Assigned(Tee) then
      FreeAndNil(Tee);
    if Assigned(InputReader) then
      FreeAndNil(InputReader);
  end;

end.
