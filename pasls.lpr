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

program pasls;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, iostream, streamex, udebug, ubufferedreader, jsonstream,
  upackages, uerrors, uinitialize, utextdocument, uutils;

type
  TRpcIdKind = (ridString, ridInteger, ridNull);

  TRpcId = record
    Kind: TRpcIdKind;
    Str: string;
    Int: Integer;
  end;

  TRpcRequest = record
    Version: string;
    Method:  string;
    Id:      TRpcId;
  end;

function ReadLspRequest(Reader: TBufferedReader): TStringStream;
var
  Header, HeaderKey, HeaderVal:  string;
  Idx, Len:                      Integer;
begin
  Result := nil;

  Header := Reader.ReadLine;
  if Header = '' then
    exit;

  Len := 0;
  while Header <> '' do
  begin
    Idx := Pos(':', Header);
    HeaderKey := Copy(Header, 1, Idx - 1);
    Delete(Header, 1, Idx);
    HeaderVal := Trim(Header);
    if HeaderKey = 'Content-Length' then
      Len := StrToInt(HeaderVal);
    Header := Reader.ReadLine;
  end;

  if Len = 0 then
    raise EParserError.Create('Invalid request body.');

  Result := TStringStream.Create();
  try
    Result.SetSize(Len);  
    Reader.BlockRead(Result.Bytes[0], Len);
  except
    FreeAndNil(Result);
  end;
end;

procedure WriteLspResponse(var f: TextFile; Body: TStringStream);
const
  ContentType: string = 'application/vscode-jsonrpc; charset=utf-8';
begin
  Write(f, 'Content-Type: ', ContentType, #13#10);
  Write(f, 'Content-Length: ', Body.Size, #13#10);
  Write(f, #13#10);
  Write(f, Body.DataString);
  Flush(f);
end;

function ParseRpcRequest(Stream: TStream): TRpcRequest;
var
  Reader: TJsonReader;
  Key:    string;
begin
  Result.Method  := '';
  Result.Version := '';
  Result.Id.Kind := ridNull;
  Reader         := nil;
  try
    Reader := TJsonReader.Create(Stream);

    if Reader.Dict then
      while (Reader.Advance <> jsDictEnd) and Reader.key(Key) do
      begin
        if Key = 'jsonrpc' then
          Reader.Str(Result.Version)
        else if Key = 'method' then
          Reader.Str(Result.Method)
        else if (Key = 'id') and Reader.Str(Result.Id.Str) then
          Result.Id.Kind := ridString
        else if (Key = 'id') and Reader.Number(Result.Id.Int) then
          Result.Id.Kind := ridInteger
        else if (Key = 'id') and Reader.Null then
          Result.Id.Kind := ridNull;
      end;

    if Reader.LastError <> jeNoError then
      raise EParseError.CreateFmt(
        'Invalid Request. JSON error @%d: %s',
        [Reader.LastErrorPosition, Reader.LastErrorMessage]
      );

    if (Result.Version <> '2.0') then
      raise EInvalidRequest.Create('No or invalid jsonrpc version specified. Must be 2.0.');

    if (Result.Method = '') then
      raise EInvalidRequest.Create('No method specified.');
  finally
    FreeAndNil(Reader);
  end;
end;

function CreateParamReader(Stream: TStream): TJsonReader;
var
  Key: string;
begin
  Result := TJsonReader.Create(Stream);
  if Result.Dict then
    while Result.Advance <> jsDictEnd do
      if Result.Key(Key) and (Key = 'params') then
        exit;
end;

procedure BeginRpcResponse(Writer: TJsonWriter; Id: TRpcId);
begin
  Writer.Dict;

  Writer.Key('jsonrpc');
  Writer.Str('2.0');

  Writer.Key('id'); 
  case Id.Kind of
    ridString:  Writer.Str(Id.Str);
    ridInteger: Writer.Number(Id.Int);
    else        Writer.Null;
  end;
end;

procedure EndRpcResponse(Writer: TJsonWriter);
begin
  Writer.DictEnd;
end;

function DispatchRpc(Method: String; Parameters: TJsonReader; Response: TJsonWriter): Boolean;
begin
  Result := true;

  if Method = 'initialize' then
    Initialize(Parameters, Response)
  else if Method = 'initialized' then
    Result := false
  else if Method = 'shutdown' then
    Result := false
  else if Method = 'textDocument/completion' then
    TextDocument_Completion(Parameters, Response)
  else if Method = 'textDocument/signatureHelp' then
    TextDocument_SignatureHelp(Parameters, Response)
  else if Method = 'textDocument/didOpen' then
  begin
    TextDocument_DidOpen(Parameters);
    Result := false;
  end
  else if Method = 'textDocument/didChange' then
  begin
    TextDocument_DidChange(Parameters);
    Result := false;
  end
  else if Method = 'exit' then
    Result := false
  else if Method = '$/cancelRequest' then
    Result := false
  else
    raise EMethodNotFound.CreateFmt('Method not found: %s', [Method]);
end;

procedure Main(Reader: TBufferedReader{; Output: TextFile});
var
  rid:               Integer;
  Request, Response: TStringStream;
  RpcRequest:        TRpcRequest;
  JsonReader:        TJsonReader;
  JsonWriter:        TJsonWriter;
  HaveResult:        Boolean;
begin
  rid := 0;

  while True do
  begin
    Response   := nil;
    Request    := nil;
    JsonWriter := nil;
    JsonReader := nil;
    HaveResult := false;

    try
      try
        Response   := TStringStream.Create;
        JsonWriter := TJsonWriter.Create(Response);

        Request    := ReadLspRequest(Reader);
        if Request = nil then
        begin  
          DebugLog('** End of stream, exiting **');
          exit;
        end;

        DebugLog('> Request (%d): '#10'%s', [rid, Copy(Request.DataString, 1, 2000)]);
        Inc(rid);

        RpcRequest := ParseRpcRequest(Request);
        Request.Position := 0; // Rewind
        JsonReader := CreateParamReader(Request);

        BeginRpcResponse(JsonWriter, RpcRequest.Id);
        JsonWriter.Key('result');
        HaveResult := DispatchRpc(RpcRequest.Method, JsonReader, JsonWriter);
        if not HaveResult then
          JsonWriter.Null;
        EndRpcResponse(JsonWriter);

      except
        on E: ERpcException do
        begin
          assert(Response <> nil);

          // Throw away any partial responses, restart from scratch
          Response.Clear;
          FreeAndNil(JsonWriter);
          JsonWriter := TJsonWriter.Create(Response);

          // Write error response
          BeginRpcResponse(JsonWriter, RpcRequest.Id);
          JsonWriter.Key('error');
          JsonWriter.Dict;
            JsonWriter.Key('code');
            JsonWriter.Number(E.Code);

            JsonWriter.Key('message');
            JsonWriter.Str(E.Message);
          JsonWriter.DictEnd;
          EndRpcResponse(JsonWriter);

          HaveResult := true;
        end;
      end;

      if HaveResult then
      begin
        WriteLspResponse(Output, Response);
        DebugLog('< Response: '#10'%s', [Copy(Response.DataString, 1, 2000)]);
      end
      else
        DebugLog('< No Response'#10);

    finally
      FreeAndNil(Request);
      FreeAndNil(Response);
      FreeAndNil(JsonWriter);
      FreeAndNil(JsonReader);
    end;
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
    FreeAndNil(InputStream);
    FreeAndNil(Transcript);
    FreeAndNil(Tee);
    FreeAndNil(InputReader);
  end;

end.
