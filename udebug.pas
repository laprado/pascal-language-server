unit udebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


procedure DebugLog(const message: String); overload;
procedure DebugLog(const message: String; args: array of const); overload;

implementation

var
  Debug: TStream;

procedure InitDebugLog;
begin
  if not Assigned(Debug) then
    Debug := TFileStream.Create('/Users/isopod/pasls-log', fmCreate);
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

