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
  SysUtils, Classes, lsp, basic;

type

  { TCompletionTriggerKind }

  // How a completion was triggered
  TCompletionTriggerKind = (
    // Completion was triggered by typing an identifier (24x7 code
    // complete), manual invocation (e.g Ctrl+Space) or via API.
    Invoked = 1,
    // Completion was triggered by a trigger character specified by
    // the `triggerCharacters` properties of the
    // `CompletionRegistrationOptions`.
    TriggerCharacter = 2,
    // Completion was re-triggered as the current completion list is
    // incomplete.
    TriggerForIncompleteCompletions = 3);

  { TCompletionContext }

  // Contains additional information about the context in which a
  // completion request is triggered.
  TCompletionContext = class(TPersistent)
  private
    fTriggerKind: TCompletionTriggerKind;
    fTriggerCharacter: string;
  published
    // How the completion was triggered.
    property triggerKind: TCompletionTriggerKind read fTriggerKind write fTriggerKind;
    // The trigger character (a single character) that has trigger
    // code complete.  Is undefined if `triggerKind !==
    // CompletionTriggerKind.TriggerCharacter`
    property triggerCharacter: string read fTriggerCharacter write fTriggerCharacter;
  end;

  { TCompletionParams }

  TCompletionParams = class(TTextDocumentPositionParams)
  private
    fContext: TCompletionContext;
  public
    destructor Destroy; override;
  published
    // The completion context. This is only available if the client
    // specifies to send this using
    // `ClientCapabilities.textDocument.completion.contextSupport ===
    // true`
    property context: TCompletionContext read fContext write fContext;
  end;

  { TInsertTextFormat }

  // Defines whether the insert text in a completion item should be
  // interpreted as plain text or a snippet.
  TInsertTextFormat = (
    // The primary text to be inserted is treated as a plain string.
    PlainText = 1,
    // The primary text to be inserted is treated as a snippet.
    //
    // A snippet can define tab stops and placeholders with `$1`, `$2`
    // and `${3:foo}`. `$0` defines the final tab stop, it defaults to
    // the end of the snippet. Placeholders with equal identifiers are
    // linked, that is typing in one will update others too.
    Snippet = 2);

  { TCompletionItemTag }

  // Completion item tags are extra annotations that tweak the
  // rendering of a completion item.
  //
  // @since 3.15.0
  TCompletionItemTag = (
    // Render a completion as obsolete, usually using a strike-out.
    Deprecated = 1);

  TCompletionItemTags = set of TCompletionItemTag;

  { TCompletionItemKind }

  // The kind of a completion entry.
  TCompletionItemKind = (
    TextItem = 1,
    MethodItem = 2,
    FunctionItem = 3,
    ConstructorItem = 4,
    FieldItem = 5,
    VariableItem = 6,
    ClassItem = 7,
    InterfaceItem = 8,
    ModuleItem = 9,
    PropertyItem = 10,
    UnitItem = 11,
    ValueItem = 12,
    EnumItem = 13,
    KeywordItem = 14,
    SnippetItem = 15,
    ColorItem = 16,
    FileItem = 17,
    ReferenceItem = 18,
    FolderItem = 19,
    EnumMemberItem = 20,
    ConstantItem = 21,
    StructItem = 22,
    EventItem = 23,
    OperatorItem = 24,
    TypeParameterItem = 25);

  { TCompletionItem }

  TCompletionItem = class(TCollectionItem)
  private
    fLabel: string;
    fKind: TCompletionItemKind;
    fTags: TCompletionItemTags;
    fDetail: string;
    fDocumentation: TMarkupContent;
    fPreselect: Boolean;
    fSortText: string;
    fFilterText: string;
    fInsertText: string;
    fInsertTextFormat: TInsertTextFormat;
    fTextEdit: TTextEdit;
    fAdditionalTextEdits: TTextEdits;
    fCommitCharacters: TStrings;
  public
    destructor Destroy; override;
  published
    // The label of this completion item. By default also the text
    // that is inserted when selecting this completion.
    property &label: string read fLabel write fLabel;
    // The kind of this completion item. Based of the kind an icon is
    // chosen by the editor. The standardized set of available values
    // is defined in `CompletionItemKind`.
    property kind: TCompletionItemKind read fKind write fKind;
    // Tags for this completion item.
    //
    // @since 3.15.0
    property tags: TCompletionItemTags read fTags write fTags;
    // A human-readable string with additional information about this
    // item, like type or symbol information.
    property detail: string read fDetail write fDetail;
    // A human-readable string that represents a doc-comment.
    property documentation: TMarkupContent read fDocumentation write fDocumentation;
    // Select this item when showing.
    //
    // *Note* that only one completion item can be selected and that
    // the tool / client decides which item that is. The rule is that
    // the *first* item of those that match best is selected.
    property preselect: Boolean read fPreselect write fPreselect;
    // A string that should be used when comparing this item
    // with other items. When `falsy` the label is used.
    property sortText: string read fSortText write fSortText;
    // A string that should be used when filtering a set of
    // completion items. When `falsy` the label is used.
    property filterText: string read fFilterText write fFilterText;
    // A string that should be inserted into a document when selecting
    // this completion. When `falsy` the label is used.
    //
    // The `insertText` is subject to interpretation by the client
    // side.  Some tools might not take the string literally. For
    // example VS Code when code complete is requested in this example
    // `con<cursor position>` and a completion item with an
    // `insertText` of `console` is provided it will only insert
    // `sole`. Therefore it is recommended to use `textEdit` instead
    // since it avoids additional client side interpretation.
    property insertText: string read fInsertText write fInsertText;
    // The format of the insert text. The format applies to both the
    // `insertText` property and the `newText` property of a provided
    // `textEdit`. If omitted defaults to
    // `InsertTextFormat.PlainText`.
    property insertTextFormat: TInsertTextFormat read fInsertTextFormat write fInsertTextFormat;
    // An edit which is applied to a document when selecting this
    // completion. When an edit is provided the value of `insertText`
    // is ignored.
    //
    // *Note:* The range of the edit must be a single line range and
    // it must contain the position at which completion has been
    // requested.
    property textEdit: TTextEdit read fTextEdit write fTextEdit;
    // An optional array of additional text edits that are applied
    // when selecting this completion. Edits must not overlap
    // (including the same insert position) with the main edit nor
    // with themselves.
    //
    // Additional text edits should be used to change text unrelated
    // to the current cursor position (for example adding an import
    // statement at the top of the file if the completion item will
    // insert an unqualified type).
    property additionalTextEdits: TTextEdits read fAdditionalTextEdits write fAdditionalTextEdits;
    // An optional set of characters that when pressed while this
    // completion is active will accept it first and then type that
    // character. *Note* that all commit characters should have
    // `length=1` and that superfluous characters will be ignored.
    property commitCharacters: TStrings read fCommitCharacters write fCommitCharacters;
  end;

  TCompletionItems = specialize TGenericCollection<TCompletionItem>;

  { TCompletionList }

  // Represents a collection of [completion items](#CompletionItem) to
  // be presented in the editor.
  TCompletionList = class(TPersistent)
  private
    fIsIncomplete: Boolean;
    fItems: TCompletionItems;
  public
    destructor Destroy; override;
  published
    // This list it not complete. Further typing should result in
    // recomputing this list.
    property isIncomplete: Boolean read fIsIncomplete write fIsIncomplete;
    // The completion items.
    property items: TCompletionItems read fItems write fItems;
  end;

  { TCompletion }

  TCompletion = class(specialize TLSPRequest<TCompletionParams, TCompletionList>)
    function Process(var Params: TCompletionParams): TCompletionList; override;
  end;


  { Forward declaration }

  TSignatureHelp = class;

  { TSignatureHelpTriggerKind }

  TSignatureHelpTriggerKind = (
    //Signature help was invoked manually by the user or by a command.
    shtkInvoked = 1,
    // Signature help was triggered by a trigger character.
    shtkTriggerCharacter = 2,
    // Signature help was triggered by the cursor moving or by the document
    // content changing.
    shtkContentChange = 3
  );

  { TSignatureHelpContext }

  TSignatureHelpContext = class(TPersistent)
  private
    fTriggerKind: TCompletionTriggerKind;
    fTriggerCharacter: string;
    fIsRetrigger: Boolean;
    fActiveSignatureHelp: TSignatureHelp;
  public
    destructor Destroy; override;
  published
    // Action that caused signature help to be triggered.
    property triggerKind: TCompletionTriggerKind read fTriggerKind write fTriggerKind;

    // Character that caused signature help to be triggered.
    //
    // This is undefined when triggerKind !==
    // SignatureHelpTriggerKind.TriggerCharacter
    property triggerCharacter: string read fTriggerCharacter write fTriggerCharacter;

    // `true` if signature help was already showing when it was triggered.
    //
    // Retriggers occur when the signature help is already active and can be
    // caused by actions such as typing a trigger character, a cursor move, or
    // document content changes.
    property isRetrigger: Boolean read fIsRetrigger write fIsRetrigger;

    // The currently active `SignatureHelp`.
    //
    // The `activeSignatureHelp` has its `SignatureHelp.activeSignature` field
    // updated based on the user navigating through available signatures.
    property activeSignatureHelp: TSignatureHelp read fActiveSignatureHelp write fActiveSignatureHelp;
  end;

  { TSignatureHelpParams }

  TSignatureHelpParams = class(TTextDocumentPositionParams)
  private
    fContext: TCompletionContext;
  public
    destructor Destroy; override;
  published
    // The signature help context. This is only available if the client
    // specifies to send this using the client capability
    // `textDocument.signatureHelp.contextSupport === true`
    //
    // @since 3.15.0
    property context: TCompletionContext read fContext write fContext;
  end;

  { TParameterInformation }

  TParameterInformation = class(TCollectionItem)
  private
    fLabel: TIntegerPair;
    fDocumentation: string;
  public
    constructor Create;
    destructor Destroy; override;
  published
    // The label of this parameter information.
    //
    // Either a string or an inclusive start and exclusive end offsets within
    // its containing signature label. (see SignatureInformation.label). The
    // offsets are based on a UTF-16 string representation as `Position` and
    // `Range` does.
    //
    // *Note*: a label of type string should be a substring of its containing
    // signature label. Its intended use case is to highlight the parameter
    // label part in the `SignatureInformation.label`.
    //property &label: string read fLabel write fLabel;// | [uinteger, uinteger];
    property &label: TIntegerPair read fLabel write fLabel;

    // The human-readable doc-comment of this parameter. Will be shown
    // in the UI but can be omitted.
    property documentation: string read fDocumentation write fDocumentation;
  end;

  { TParameterInformationList }

  TParameterInformationList = class(TCollection)
  end;

  { TSignatureInformation }

  TSignatureInformation = class(TCollectionItem)
  private
    fActiveParameter: integer;
    fLabel: string;
    fDocumentation: string;
    fParameters: TParameterInformationList;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    // The label of this signature. Will be shown in
    // the UI.
    property &label: string read fLabel write fLabel;

    // The human-readable doc-comment of this signature. Will be shown
    // in the UI but can be omitted.
    property documentation: string read fDocumentation write fDocumentation;

    // The parameters of this signature.
  	property parameters: TParameterInformationList read fParameters write fParameters;

    // The index of the active parameter.
    //
    // If provided, this is used in place of `SignatureHelp.activeParameter`.
    //
    // @since 3.16.0
    property activeParameter: integer read fActiveParameter write fActiveParameter;
  end;

  { TSignatureInformationList }

  TSignatureInformationList = specialize TGenericCollection<TSignatureInformation>;

  { TSignatureList }

  // Represents a collection of [signaure items](#SignatureItem) to
  // be presented in the editor.
  TSignatureList = class(TPersistent)
  private
    fItems: TSignatureInformationList;
    fActiveParameter: integer;
    fActiveSignature: integer;
  public
    destructor Destroy; override;
  published
    //
    property ActiveParameter: integer read fActiveParameter write fActiveParameter;
    //
    property ActiveSignature: integer read fActiveSignature write fActiveSignature;

    // The signature items.
    property signatures: TSignatureInformationList read fItems write fItems;
  end;

  TSignatureHelp = class(specialize TLSPRequest<TSignatureHelpParams, TSignatureList>)
    function Process(var Params: TSignatureHelpParams): TSignatureList; override;
  end;

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

  TCompletionRecArray = array of TCompletionRec;

function GetCompletionRecords(Code: TCodeBuffer; X, Y: integer; Prefix: String;
  Exact: Boolean): TCompletionRecArray;
var
  Identifier:       TIdentifierListItem;
  i, j, o, Count:   integer;
  ResultType:       String;
  Text:             String;
  Segment:          String;
  node, paramsNode,
  resultNode:       TCodeTreeNode;
  SegmentLen:       integer;

  function AppendString(var S: String; Suffix: String): TStringSlice;
  begin
    Result.a := Length(S) + 1;
    Result.b := Length(S) + Length(Suffix) + 1;
    S := S + Suffix;
  end;

begin
  assert(Assigned(Code));

  CodeToolBoss.IdentifierList.Prefix := Prefix;
  Result := nil;

  if not CodeToolBoss.GatherIdentifiers(Code, X, Y) then
    raise EParseError.Create(CodeToolBoss.ErrorMessage);

  Count := CodeToolBoss.IdentifierList.GetFilteredCount;
  SetLength(Result, Count);

  o := 0;
  for i := 0 to Count - 1 do
  begin
    Identifier := CodeToolBoss.IdentifierList.FilteredItems[i];
    Text := '';
    if (not Exact) or (CompareText(Identifier.Identifier, Prefix) = 0) then
    begin
      resultNode := Identifier.Tool.GetProcResultNode(identifier.Node);
      paramsNode := Identifier.Tool.GetProcParamList(identifier.Node);
      if Assigned(paramsNode) then
      begin
        SetLength(Result[I].Parameters, paramsNode.ChildCount);

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

        Result[o].Identifier := AppendString(Text, Identifier.Identifier);
        AppendString(Text, ' (');

        for j := 0 to paramsNode.ChildCount - 1 do
        begin
          Segment := Identifier.Tool.ExtractNode(node, []);
          Segment := StringReplace(Segment, ':', ': ', [rfReplaceAll]);
          Segment := StringReplace(Segment, '=', ' = ', [rfReplaceAll]);

          Result[o].Parameters[j] := AppendString(Text, Segment);

          SegmentLen := Pos(':', Segment) - 1;
          if SegmentLen <= 0 then
            SegmentLen := Length(Segment);

          if J <> paramsNode.ChildCount - 1 then
            Text := Text + ', ';

          node := node.NextBrother;
        end;

        AppendString(Text, ')');
      end
      else
        Result[o].Identifier := AppendString(Text, Identifier.Identifier);

      if ResultType <> '' then
      begin
        AppendString(Text, ': ');
        Result[o].ResultType := AppendString(Text, ResultType);
      end;

      Result[o].Text := Text;
      Result[o].Desc := Identifier.Node.DescAsString;
    end;
    Inc(o);
  end;

  SetLength(Result, o);
end;


{ TParameterInformation }

constructor TParameterInformation.Create;
begin
  FreeAndNil(fLabel);
end;

destructor TParameterInformation.Destroy;
begin
  inherited Destroy;
end;

{ TCompletionParams }

destructor TCompletionParams.Destroy;
begin
  FreeAndNil(fContext);
  inherited Destroy;
end;

{ TCompletionItem }

destructor TCompletionItem.Destroy;
begin
  FreeAndNil(fAdditionalTextEdits);
  FreeAndNil(fCommitCharacters);
  FreeAndNil(fDocumentation);
  inherited Destroy;
end;

{ TCompletionList }

destructor TCompletionList.Destroy;
begin
  FreeAndNil(fItems);
  inherited Destroy;
end;

{ TSignatureHelpContext }

destructor TSignatureHelpContext.Destroy;
begin
  FreeAndNil(fActiveSignatureHelp);
  inherited Destroy;
end;

{ TSignatureHelpParams }

destructor TSignatureHelpParams.Destroy;
begin
  FreeAndNil(fContext);
  inherited Destroy;
end;

{ TSignatureInformation }

constructor TSignatureInformation.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fParameters := TParameterInformationList.Create(TParameterInformation);
end;

destructor TSignatureInformation.Destroy;
begin
  FreeAndNil(fParameters);
  inherited Destroy;
end;

{ TSignatureList }

destructor TSignatureList.Destroy;
begin
  FreeAndNil(fItems);
  inherited Destroy;
end;

{ TSignatureHelp }

function TSignatureHelp.Process(var Params: TSignatureHelpParams
  ): TSignatureList;
var
  URI: TURI;
  Code: TCodeBuffer;
  Recs: TCompletionRecArray;
  Rec: TCompletionRec;
  Signature: TSignatureInformation;
  Signatures: TSignatureInformationList;    
  ProcName: String;
  Param: TParameterInformation;
  X, Y, i: integer;

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
  URI := ParseURI(Params.textDocument.uri);
  //Code := CodeToolBoss.LoadFile(URI.Path + URI.Document, true, false);
  Code := CodeToolBoss.FindFile(URI.Path + URI.Document);

  assert(Code <> nil);

  X := Params.position.character;
  Y := Params.position.line;
  ProcName := GetProcName(Code, X, Y);

  Recs := GetCompletionRecords(Code, X, Y, ProcName, true);

  Result := TSignatureList.Create;
  Signatures := TSignatureInformationList.Create;
  Result.signatures := Signatures;

  for Rec in Recs do
  begin
    Signature := TSignatureInformation(Signatures.Add);
    Signature.&label := Rec.Text;
    for i := 0 to Length(Rec.Parameters) - 1 do
    begin
      Param := TParameterInformation(Signature.parameters.Add);
      Param.&label := TIntegerPair.Create(
        Rec.Parameters[i].a, Rec.Parameters[i].b
      );
    end;
  end;
end;

{ TCompletion }

function TCompletion.Process(var Params: TCompletionParams): TCompletionList;
var
  Recs: TCompletionRecArray;
  Rec: TCompletionRec;
  URI: TURI;
  Code: TCodeBuffer;
  X, Y: integer;
  Prefix: String;

  Completions: TCompletionItems;
  Completion: TCompletionItem;

  function GetPrefix(Code: TCodeBuffer; X, Y: integer): String;
  var
    PStart, PEnd: integer;
    Line: String;
  begin
    Line := Code.GetLine(Y);
    GetIdentStartEndAtPosition(Line, X + 1, PStart, PEnd);
    Result := Copy(Line, PStart, PEnd - PStart);
  end;
begin
  URI := ParseURI(Params.textDocument.uri);
  Code := CodeToolBoss.FindFile(URI.Path + URI.Document);
  //Code := CodeToolBoss.LoadFile(URI.Path + URI.Document, true, false);
  assert(Assigned(Code));

  X := Params.position.character;
  Y := Params.position.line;
  Prefix := GetPrefix(Code, X, Y);

  DebugLog('Complete: %d, %d, "%s"', [X, Y, Prefix]);

  Recs := GetCompletionRecords(Code, X + 1, Y + 1, Prefix, false);

  Completions := TCompletionItems.Create;
  
  for Rec in Recs do
  begin
    Completion := TCompletionItem(Completions.Add);
    Completion.insertText := Copy(
      Rec.Text, Rec.Identifier.a, Rec.Identifier.b - Rec.Identifier.a
    );
    Completion.&label := Rec.Text;
    Completion.detail := Rec.Desc;
    Completion.insertTextFormat := TInsertTextFormat.PlainText;
  end;

  Result := TCompletionList.Create;
  Result.items := Completions;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/completion', TCompletion);
  LSPHandlerManager.RegisterHandler('textDocument/signatureHelp', TSignatureHelp);
end.

