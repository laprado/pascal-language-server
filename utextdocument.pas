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

unit utextdocument;

{$mode objfpc}{$H+}

interface

uses
  jsonstream, ujsonrpc;

procedure TextDocument_DidOpen(Rpc: TRpcPeer; Request: TRpcRequest);
procedure TextDocument_DidChange(Rpc: TRpcPeer; Request: TRpcRequest);
procedure TextDocument_SignatureHelp(Rpc: TRpcPeer; Request: TRpcRequest);
procedure TextDocument_Completion(Rpc: TRpcPeer; Request: TRpcRequest);
procedure TextDocument_Declaration(Rpc: TRpcPeer; Request: TRpcRequest);

implementation

uses
  Classes, SysUtils, URIParser, CodeToolManager, CodeCache, IdentCompletionTool,
  BasicCodeTools, PascalParserTool, CodeTree, FindDeclarationTool, CustomCodeTool,
  udebug;

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

procedure TextDocument_DidOpen(Rpc: TRpcPeer; Request: TRpcRequest);
var
  Code:    TCodeBuffer;
  UriStr:  string;
  Uri:     TURI;
  Content: string;
begin
  if ParseChangeOrOpen(Request.Reader, UriStr, Content, false) then
  begin
    Uri         := ParseURI(UriStr);
    Code        := CodeToolBoss.LoadFile(URI.Path + URI.Document, false, false);
    Code.Source := Content;
  end;
end;

procedure TextDocument_DidChange(Rpc: TRpcPeer; Request: TRpcRequest);
var
  Code:        TCodeBuffer;
  UriStr:      string;
  Uri:         TURI;
  Content:     string;
begin
  if ParseChangeOrOpen(Request.Reader, UriStr, Content, true) then
  begin
    Uri         := ParseURI(UriStr);
    Code        := CodeToolBoss.FindFile(URI.Path + URI.Document);
    Code.Source := Content;
  end;
end;

type
  TStringSlice = record
    a, b: integer;
  end;

  TCompletionRec = record
    Text:       String;
    Identifier: TStringSlice;
    ResultType: TStringSlice;
    Parameters: array of TStringSlice;
    Desc:       String;
  end;

  TCompletionCallback = procedure(const Rec: TCompletionRec; Writer: TJsonWriter);

procedure GetCompletionRecords(
  Code: TCodeBuffer; X, Y: integer; Prefix: String; Exact: Boolean;
  Callback: TCompletionCallback; Writer: TJsonWriter
);
var
  Identifier:       TIdentifierListItem;
  i, j, Count:      Integer;
  ResultType:       string;
  Segment:          string;
  node, paramsNode: TCodeTreeNode;
  SegmentLen:       integer;
  Rec:              TCompletionRec;

  function AppendString(var S: String; Suffix: String): TStringSlice;
  begin
    Result.a := Length(S) + 1;
    Result.b := Length(S) + Length(Suffix) + 1;
    S := S + Suffix;
  end;

begin
  assert(Code <> nil);

  CodeToolBoss.IdentifierList.Prefix := Prefix;

  if not CodeToolBoss.GatherIdentifiers(Code, X, Y) then
    raise ERpcError.Create(
      jsrpcRequestFailed, 
      Format('Line %d: %s', [
        CodeToolBoss.ErrorLine, 
        CodeToolBoss.ErrorMessage
      ])
    );

  Count := CodeToolBoss.IdentifierList.GetFilteredCount;

  for i := 0 to Count - 1 do
  begin
    Identifier := CodeToolBoss.IdentifierList.FilteredItems[i];

    Rec.Text         := '';
    Rec.Identifier.a := 0;
    Rec.Identifier.b := 0;
    Rec.ResultType.a := 0;
    Rec.ResultType.b := 0;
    Rec.Parameters   := nil;
    Rec.Desc         := '';
    ResultType       := '';

    if (not Exact) or (CompareText(Identifier.Identifier, Prefix) = 0) then
    begin
      paramsNode := Identifier.Tool.GetProcParamList(identifier.Node);
      if Assigned(paramsNode) then
      begin
        ResultType :=
          Identifier.Tool.ExtractProcHead(
            identifier.Node, 
            [
              phpWithoutName, phpWithoutParamList, phpWithoutSemicolon, 
              phpWithResultType, phpWithoutBrackets, phpWithoutGenericParams,
              phpWithoutParamTypes
            ]
          ).Replace(':', '').Trim;

        node := paramsNode.firstChild;

        Rec.Identifier := AppendString(Rec.Text, Identifier.Identifier);
        AppendString(Rec.Text, ' (');

        SetLength(Rec.Parameters, paramsNode.ChildCount);

        for j := 0 to paramsNode.ChildCount - 1 do
        begin
          Segment := Identifier.Tool.ExtractNode(node, []);
          Segment := StringReplace(Segment, ':', ': ', [rfReplaceAll]);
          Segment := StringReplace(Segment, '=', ' = ', [rfReplaceAll]);

          Rec.Parameters[j] := AppendString(Rec.Text, Segment);

          SegmentLen := Pos(':', Segment) - 1;
          if SegmentLen <= 0 then
            SegmentLen := Length(Segment);

          if J <> paramsNode.ChildCount - 1 then
            Rec.Text := Rec.Text + ', ';

          node := node.NextBrother;
        end;

        AppendString(Rec.Text, ')');
      end
      else
        Rec.Identifier := AppendString(Rec.Text, Identifier.Identifier);

      if ResultType <> '' then
      begin
        AppendString(Rec.Text, ': ');
        Rec.ResultType := AppendString(Rec.Text, ResultType);
      end;
      
      Rec.Desc := Identifier.Node.DescAsString;

      Callback(Rec, Writer);
    end;
  end;
end;

type
  TCompletionRequest = record
    X, Y:        Integer;
    Uri:         TURI;
    TriggerKind: Integer;
    TriggerChar: string;
    IsRetrigger: Boolean;
  end;

function ParseCompletionRequest(Reader: TJsonReader): TCompletionRequest;
var
  Key:    string;
  UriStr: string;
begin
  UriStr             := '';
  Result.TriggerKind := -1;
  Result.Y           := -1;
  Result.X           := -1;

  if Reader.Dict then
    while (Reader.Advance <> jsDictEnd) and Reader.Key(Key) do
    begin
      if (Key = 'textDocument') and Reader.Dict then
        while (Reader.Advance <> jsDictEnd) and Reader.Key(Key) do
        begin
          if Key = 'uri' then
            Reader.Str(UriStr);
        end
      else if (Key = 'position') and Reader.Dict then
        while (Reader.Advance <> jsDictEnd) and Reader.Key(Key) do
        begin
          if Key = 'line' then
            Reader.Number(Result.Y)
          else if (Key = 'character') then
            Reader.Number(Result.X);
        end
      else if (Key = 'context') and Reader.Dict then
        while (Reader.Advance <> jsDictEnd) and Reader.Key(Key) do
        begin
          if Key = 'triggerKind' then
            Reader.Number(Result.TriggerKind)
          else if Key = 'triggerCharacter' then
            Reader.Str(Result.TriggerChar)
          else if Key = 'isRetrigger' then
            Reader.Bool(Result.IsRetrigger);
          //else if Key = 'activeSignatureHelp' then

        end;
    end;

  Result.Uri := ParseUri(UriStr);
end;

// Identifier completion

procedure CompletionCallback(const Rec: TCompletionRec; Writer: TJsonWriter);
begin
  Writer.Dict;
    Writer.Key('insertText');
    Writer.Str(
      Copy(Rec.Text, Rec.Identifier.a, Rec.Identifier.b - Rec.Identifier.a)
    );

    Writer.Key('insertTextFormat');
    Writer.Number(1); // 1 = Plain Text

    Writer.Key('label');
    Writer.Str(Rec.Text);

    Writer.Key('detail');
    Writer.Str(Rec.Desc);
  Writer.DictEnd;
end;

function GetPrefix(Code: TCodeBuffer; X, Y: integer): string;
var
  PStart, PEnd: integer;
  Line: String;
begin
  Line := Code.GetLine(Y);
  GetIdentStartEndAtPosition(Line, X + 1, PStart, PEnd);
  Result := Copy(Line, PStart, PEnd - PStart);
end;

procedure TextDocument_Completion(Rpc: TRpcPeer; Request: TRpcRequest);
var
  Req:      TCompletionRequest;
  Code:     TCodeBuffer;
  Prefix:   string;
  Response: TRpcResponse;
  Writer:   TJsonWriter;


begin
  Response := nil;
  try
    try
      Req := ParseCompletionRequest(Request.Reader);

      Code := CodeToolBoss.FindFile(Req.Uri.Path + Req.Uri.Document);

      if Code = nil then
        raise ERpcError.CreateFmt(
          jsrpcInvalidRequest,
          'File not found: %s', [Req.Uri.Path + Req.Uri.Document]
        );

      Prefix := GetPrefix(Code, Req.X, Req.Y);

      DebugLog('Complete: %d, %d, "%s"', [Req.X, Req.Y, Prefix]);

      Response := TRpcResponse.Create(Request.Id);
      Writer   := Response.Writer;
      Writer.Dict;
        Writer.Key('isIncomplete');
        Writer.Bool(false);

        Writer.Key('items');
        Writer.List;
          GetCompletionRecords(Code, Req.X + 1, Req.Y + 1, Prefix, false, @CompletionCallback, Writer);
        Writer.ListEnd;
      Writer.DictEnd;

      Rpc.Send(Response);
    except
      on E: ERpcError do
      begin
        // Unfortunately, there isn't really a good way to report errors to the
        // client. While there are error responses, those aren't shown to the
        // user. There is also the call window/showMessage, but this one is not
        // implemented by NeoVim. So we work around it by showing a fake
        // completion item.
        FreeAndNil(Response);
        Response := TRpcResponse.Create(Request.Id);
        Writer := Response.Writer;
        Writer.Dict;
          Writer.Key('items');
          Writer.List;
            Writer.Dict;
              Writer.Key('label');
              Writer.Str(e.Message);
              Writer.Key('insertText');
              Writer.Str('');
            Writer.DictEnd;
          Writer.ListEnd;

          //Writer.Key('activeParameter');
          //Writer.Key('activeSignature');
        Writer.DictEnd;
        Rpc.Send(Response);
      end;
    end;
  finally
    FreeAndNil(Response);
  end;
end;

// Signature help

procedure SignatureCallback(const Rec: TCompletionRec; Writer: TJsonWriter);
var
  i: integer;
begin
  Writer.Dict;
    Writer.Key('label');
    Writer.Str(Rec.Text);

    Writer.Key('parameters');
    Writer.List;
      for i := low(Rec.Parameters) to high(Rec.Parameters) do
      begin
        Writer.Dict;
          Writer.Key('label');
          Writer.List;
            Writer.Number(Rec.Parameters[i].a);
            Writer.Number(Rec.Parameters[i].b);
          Writer.ListEnd;
          // Writer.Key('documentation');
        Writer.DictEnd;
      end;
    Writer.ListEnd;

    //Writer.Key('documentation');
    //Writer.Key('activeParameter');
  Writer.DictEnd;
end;

procedure TextDocument_SignatureHelp(Rpc: TRpcPeer; Request: TRpcRequest);
var
  Code:     TCodeBuffer;
  ProcName: String;
  Req:      TCompletionRequest;
  Response: TRpcResponse;
  Writer:   TJsonWriter;

const
  NullId: TRpcId = (Kind: ridNull);

  function GetProcName(Code: TCodeBuffer; var X, Y: integer): String;
  var
    CodeContexts: TCodeContextInfo;
    ProcStart: integer;
  begin
    Result := '';

    CodeToolBoss.FindCodeContext(Code, X + 1, Y + 1, CodeContexts);

    if not Assigned(CodeContexts) then
      raise ERpcError.Create(jsrpcRequestFailed, CodeToolBoss.ErrorMessage);

    ProcStart := CodeContexts.StartPos;

    // Find closest opening parenthesis
    while (ProcStart > 1) and (Code.Source[ProcStart] <> '(') do
      Dec(ProcStart);

    // ProcStart point to the parenthesis before the first parameter.
    // But we actually need a position *inside* the procedure identifier.
    // Note that there may be whitespace, even newlines, between the first
    // parenthesis and the procedure.
    while (ProcStart > 1) and (Code.Source[ProcStart] in ['(', ' ', #13, #10, #9]) do
      Dec(ProcStart);

    Code.AbsoluteToLineCol(ProcStart, Y, X);

    Result := CodeContexts.ProcName;
  end;
begin
  Response := nil;
  try
    try
      Req := ParseCompletionRequest(Request.Reader);

      Code := CodeToolBoss.FindFile(Req.Uri.Path + Req.Uri.Document);
      assert(Code <> nil);

      ProcName := GetProcName(Code, Req.X, Req.Y);

      Response := TRpcResponse.Create(Request.Id);
      Writer := Response.Writer;

      Writer.Dict;
        Writer.Key('signatures');
        Writer.List;
          GetCompletionRecords(Code, Req.X, Req.Y, ProcName, true, @SignatureCallback, Writer);
        Writer.ListEnd;

        //Writer.Key('activeParameter');
        //Writer.Key('activeSignature');
      Writer.DictEnd;

      //raise ERpcError.Create(-123, 'This is a test');

      Rpc.Send(Response);
    except
      on E: ERpcError do
      begin
        // Unfortunately, there isn't really a good way to report errors to the
        // client. While there are error responses, those aren't shown to the
        // user. There is also the call window/showMessage, but this one is not
        // implemented by NeoVim. So we work around it by showing a fake
        // completion item.
        FreeAndNil(Response);
        Response := TRpcResponse.Create(Request.Id);
        Writer := Response.Writer;
        Writer.Dict;
          Writer.Key('signatures');
          Writer.List;
            Writer.Dict;
              Writer.key('label');
              Writer.Str(e.Message);
            Writer.DictEnd;
          Writer.ListEnd;

          //Writer.Key('activeParameter');
          //Writer.Key('activeSignature');
        Writer.DictEnd;
        Rpc.Send(Response);
      end;
    end;
  finally
    FreeAndNil(Response);
  end;
end;

// Go to declaration

procedure TextDocument_Declaration(Rpc: TRpcPeer; Request: TRpcRequest);
var
  Req:             TCompletionRequest;
  Code:            TCodeBuffer;
  NewCode:         TCodeBuffer;
  NewX, NewY:      Integer;
  Response:        TRpcResponse;
  Writer:          TJsonWriter;
  Found:           Boolean;
  Prefix:          string;


  CursorPos:       TCodeXYPosition;
  XYPos:           TCodeXYPosition;
  TopLine:         integer;
  CTExprType:      TExpressionType;
begin
  Response := nil;
  NewCode  := nil;
  NewX     := 0;
  NewY     := 0;
  Found    := false;
  try
    Req := ParseCompletionRequest(Request.Reader);

    Code := CodeToolBoss.FindFile(Req.Uri.Path + Req.Uri.Document);

    if Code = nil then
      raise ERpcError.CreateFmt(
        jsrpcInvalidRequest,
        'File not found: %s', [Req.Uri.Path + Req.Uri.Document]
      );

    if not CodeToolBoss.InitCurCodeTool(Code) then
      raise ERpcError.CreateFmt(
        jsrpcRequestFailed,
        'Could not initialize code tool', []
      );

    Prefix := GetPrefix(Code, Req.X, Req.Y);

    DebugLog('Find declaration: %d, %d "%s"', [Req.X, Req.Y, Prefix]);

    CursorPos.Code := Code;
    CursorPos.X    := Req.X + 1;
    CursorPos.Y    := Req.Y + 1;

    try
      Found := CodeToolBoss.CurCodeTool.FindDeclaration(
        CursorPos, DefaultFindSmartHintFlags+[fsfSearchSourceName], CTExprType,
        XYPos, TopLine
      );
    except
      on E: ECodeToolError do ; // Swallow
    end;

    NewCode := XYPos.Code;
    NewX    := XYPos.X;
    NewY    := XYPos.Y;

    Response := TRpcResponse.Create(Request.Id);  
    Writer   := Response.Writer;

    if Found then
    begin
      Dec(NewY);
      Dec(NewX);

      Writer.Dict;
        Writer.Key('uri');
        Writer.Str('file://' + NewCode.Filename);

        Writer.Key('range');
        Writer.Dict;
          Writer.Key('start');
          Writer.Dict;
            Writer.Key('line');
            Writer.Number(NewY);

            Writer.Key('character');
            Writer.Number(NewX);
          Writer.DictEnd;

          Writer.Key('end');
          Writer.Dict;
            Writer.Key('line');
            Writer.Number(NewY);

            Writer.Key('character');
            Writer.Number(NewX);
          Writer.DictEnd;
        Writer.DictEnd;
      Writer.DictEnd;
    end
    else
    begin
      Writer.Null;
    end;

    Rpc.Send(Response);
  finally
    FreeAndNil(Response);
  end;
end;

end.

