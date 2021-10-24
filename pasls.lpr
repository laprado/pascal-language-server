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
  upackages, ujsonrpc, uinitialize, utextdocument, uutils;

procedure SendError(
  Rpc: TRpcPeer; Id: TRpcId; Code: Integer; const Msg: string
);
var
  Response: TRpcResponse;
begin
  Response := nil;
  try
    Response := TRpcResponse.CreateError(Id, Code, Msg);
    Rpc.Send(Response);
  finally
    FreeAndNil(Response);
  end;
end;

procedure Dispatch(Rpc: TRpcPeer; Request: TRpcRequest);
begin
  if Request.Method = 'initialize' then
    Initialize(Rpc, Request)
  else if Request.Method = 'initialized' then
  else if Request.Method = 'shutdown' then
  else if Request.Method = 'textDocument/didOpen' then
    TextDocument_DidOpen(Rpc, Request)
  else if Request.Method = 'textDocument/didChange' then
    TextDocument_DidChange(Rpc, Request)
  else if Request.Method = 'textDocument/didClose' then
  else if Request.Method = 'textDocument/completion' then
    TextDocument_Completion(Rpc, Request)
  else if Request.Method = 'textDocument/signatureHelp' then
    TextDocument_SignatureHelp(Rpc, Request)
  else if Request.Method = 'textDocument/declaration' then
    TextDocument_Declaration(Rpc, Request)
  else if Request.Method = 'textDocument/definition' then
    TextDocument_Definition(Rpc, Request)
  else if Request.Method = 'exit' then
  else if Request.Method = '$/cancelRequest' then
  else
    raise ERpcError.CreateFmt(
      jsrpcMethodNotFound, 'Method not found: %s', [Request.Method]
    );
end;

procedure Main(Rpc: TRpcPeer);
var
  Request: TRpcRequest;
begin
  while True do
  begin
    Request := nil;
    try
      Request := Rpc.Receive;

      if Request = nil then
      begin  
        DebugLog('** End of stream, exiting **');
        exit;
      end;

      try
        Dispatch(Rpc, Request);
      except
        on E: ERpcError do
          SendError(Rpc, Request.Id, E.Code, E.Message);
      end;
    finally
      FreeAndNil(Request);
    end;
  end;
end;


var
  InputStream:    TStream;
  OutputStream:   TStream;
  Transcript:     TStream;
  Tee:            TStream;

  RpcPeer:        TRpcPeer;

  TranscriptPath: string;
begin
  InputStream    := nil;
  OutputStream   := nil;
  Transcript     := nil;
  Tee            := nil;
  RpcPeer        := nil;

  TranscriptPath := '/Users/isopod/pasls-transcript.txt';

  try
    InputStream  := TIOStream.Create(iosInput);
    OutputStream := TIOStream.Create(iosOutput);

    {$IF 1}
    try
      Transcript := TFileStream.Create(TranscriptPath, fmCreate or fmOpenWrite);
    except           
      FreeAndNil(Transcript);
    end;
    Tee          := TTeeStream.Create(InputStream, Transcript);
    RpcPeer      := TRpcPeer.Create(Tee, OutputStream);
    {$ELSE}
    InputStream  := TFileStream.Create(TranscriptPath, fmOpenRead);
    RpcPeer      := TRpcPeer.Create(InputStream, OutputStream);
    {$ENDIF}

    try
      Main(RpcPeer);
    except
      on E: Exception do
        DebugLog('EXCEPTION: ' + E.Message);
    end;
  finally
    FreeAndNil(InputStream);
    FreeAndNil(OutputStream);
    FreeAndNil(Transcript);
    FreeAndNil(Tee);
    FreeAndNil(RpcPeer);
  end;

end.
