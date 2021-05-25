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

unit lsp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, basic, fgl, jsonstream;

type

  TRpcMethod = procedure(Parameters: TJsonReader; Response: TJsonWriter);

  { ERpcException }

  ERpcException = class(Exception)
  public
    function Code: Integer; virtual; abstract;
  end;

  EServerNotInitialized = class(ERpcException)
  public
    function Code: Integer; override;
  end;

  EParseError = class(ERpcException)
  public
    function Code: Integer; override;
  end;

  EInvalidRequest = class(ERpcException)
  public
    function Code: Integer; override;
  end;

  EMethodNotFound = class(ERpcException)
  public
    function Code: Integer; override;
  end;

  EUnknownErrorCode = class(ERpcException)
  public
    function Code: Integer; override;
  end;

  // Defined by the protocol.
  ERequestCancelled = class(ERpcException)
  public
    function Code: Integer; override;
  end;

  EContentModified = class(ERpcException)
  public
    function Code: Integer; override;
  end;


implementation

function EServerNotInitialized.Code: Integer;
begin
  result := -32002;
end;

function EParseError.Code: Integer;
begin
  Result := -32700;
end;

function ERequestCancelled.Code: Integer;
begin
  result := -32800;
end;

function EContentModified.Code: Integer;
begin
  result := -32801;
end;

function EInvalidRequest.Code: Integer;
begin
  Result := -32600;
end;

function EMethodNotfound.Code: Integer;
begin
  Result := -32601;
end;

function EUnknownErrorCode.Code: Integer;
begin
  result := -32001;
end;


end.

