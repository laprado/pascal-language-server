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

unit completion;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lsp, basic, jsonstream;

procedure TextDocument_SignatureHelp(Reader: TJsonReader; Writer: TJsonWriter);
procedure TextDocument_Completion(Reader: TJsonReader; Writer: TJsonWriter);

implementation

uses
  URIParser, CodeToolManager, CodeCache, IdentCompletionTool, 
  BasicCodeTools, PascalParserTool, CodeTree,
  udebug;

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
  Text:             string;
  Segment:          string;
  node, paramsNode,
  resultNode:       TCodeTreeNode;
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
    raise EUnknownErrorCode.Create(CodeToolBoss.ErrorMessage);

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

    if (not Exact) or (CompareText(Identifier.Identifier, Prefix) = 0) then
    begin
      resultNode := Identifier.Tool.GetProcResultNode(identifier.Node);
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

//function TSignatureHelp.Process(var Params: TSignatureHelpParams
//  ): TSignatureList;

procedure SignatureCallback(const Rec: TCompletionRec; W: TJsonWriter);
var
  i: integer;
begin
  W.Dict;
    W.Key('label');
    W.Str(Rec.Text);

    W.Key('parameters');
    W.List;
      for i := low(Rec.Parameters) to high(Rec.Parameters) do
      begin
        W.Dict;
          W.Key('label');
          W.List;
            W.Number(Rec.Parameters[i].a);
            W.Number(Rec.Parameters[i].b);
          W.ListEnd;
          // W.Key('documentation');
        W.DictEnd;
      end;
    W.ListEnd;

    //W.Key('documentation');
    //W.Key('activeParameter');
  W.DictEnd;
end;

procedure TextDocument_SignatureHelp(Reader: TJsonReader; Writer: TJsonWriter);
var
  Code:     TCodeBuffer;
  ProcName: String;

  Req:      TCompletionRequest;

  function GetProcName(Code: TCodeBuffer; var X, Y: integer): String;
  var
    CodeContexts: TCodeContextInfo;
    ProcStart: integer;
  begin
    Result := '';

    CodeToolBoss.FindCodeContext(Code, X + 1, Y + 1, CodeContexts);

    if not Assigned(CodeContexts) then
      raise EParseError.Create(CodeToolBoss.ErrorMessage);

    ProcStart := CodeContexts.StartPos;

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
  Req := ParseCompletionRequest(Reader);

  //Code := CodeToolBoss.LoadFile(URI.Path + URI.Document, true, false);
  Code := CodeToolBoss.FindFile(Req.Uri.Path + Req.Uri.Document);
  assert(Code <> nil);

  ProcName := GetProcName(Code, Req.X, Req.Y);

  Writer.Dict;
    Writer.Key('signatures');
    Writer.List;
      GetCompletionRecords(Code, Req.X, Req.Y, ProcName, true, @SignatureCallback, Writer);
    Writer.ListEnd;

    //Writer.Key('activeParameter');
    //Writer.Key('activeSignature');
  Writer.DictEnd;
end;


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

procedure TextDocument_Completion(Reader: TJsonReader; Writer: TJsonWriter);
var
  Req:    TCompletionRequest;
  Code:   TCodeBuffer;
  Prefix: string;

  function GetPrefix(Code: TCodeBuffer; X, Y: integer): string;
  var
    PStart, PEnd: integer;
    Line: String;
  begin
    Line := Code.GetLine(Y);
    GetIdentStartEndAtPosition(Line, X + 1, PStart, PEnd);
    Result := Copy(Line, PStart, PEnd - PStart);
  end;

begin
  Req := ParseCompletionRequest(Reader);

  Code := CodeToolBoss.FindFile(Req.Uri.Path + Req.Uri.Document);
  //Code := CodeToolBoss.LoadFile(URI.Path + URI.Document, true, false);

  if Code = nil then
    raise EUnknownErrorCode.CreateFmt('File not found: %s', [Req.Uri.Path + Req.Uri.Document]);

  Prefix := GetPrefix(Code, Req.X, Req.Y);

  DebugLog('Complete: %d, %d, "%s"', [Req.X, Req.Y, Prefix]);

  Writer.Dict;
    Writer.Key('isIncomplete');
    Writer.Bool(false);

    Writer.Key('items');
    Writer.List;
      GetCompletionRecords(Code, Req.X + 1, Req.Y + 1, Prefix, false, @CompletionCallback, Writer);
    Writer.ListEnd;
  Writer.DictEnd;
end;

initialization
{
  LSPHandlerManager.RegisterHandler('textDocument/completion', TCompletion);
  LSPHandlerManager.RegisterHandler('textDocument/signatureHelp', TSignatureHelp);
}
end.

