// Pascal Language Server
// Copyright 2021 Philip Zander

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

unit udebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure DebugLog(const message: String); overload;
procedure DebugLog(const message: String; args: array of const); overload;

implementation

uses
  iostream;

var
  Debug: TStream;

procedure InitDebugLog;
begin
  if not Assigned(Debug) then
  begin
    try
      Debug := TFileStream.Create('/Users/isopod/pasls-log', fmCreate);
    except                  
      //Debug := TFileStream.Create('/Users/isopod/pasls-log-1', fmCreate);
      Debug := TIOStream.Create(iosError);
    end;
  end;
end;

procedure DebugLog(const message: String);
begin
  InitDebugLog;
  if message <> '' then
    Debug.Write(message[1], Length(message));
end;

procedure DebugLog(const message: String; args: array of const);
var
  S: String;
begin
  S := Format(message, args) + #10;
  DebugLog(S);
end;

end.

