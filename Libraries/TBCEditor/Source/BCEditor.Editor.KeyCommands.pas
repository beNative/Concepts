unit BCEditor.Editor.KeyCommands;

interface

uses
  System.Classes, System.SysUtils, Vcl.Menus;

const
  ecNone = 0;
  ecEditCommandFirst = 501;
  ecEditCommandLast = 1000;
  { Caret moving }
  ecLeft = 1;
  ecRight = 2;
  ecUp = 3;
  ecDown = 4;
  ecWordLeft = 5;
  ecWordRight = 6;
  ecLineStart = 7;
  ecLineEnd = 8;
  ecPageUp = 9;
  ecPageDown = 10;
  ecPageLeft = 11;
  ecPageRight = 12;
  ecPageTop = 13;
  ecPageBottom = 14;
  ecEditorTop = 15;
  ecEditorBottom = 16;
  ecGotoXY = 17;
  { Selection }
  ecSelection = 100;
  ecSelectionLeft = ecLeft + ecSelection;
  ecSelectionRight = ecRight + ecSelection;
  ecSelectionUp = ecUp + ecSelection;
  ecSelectionDown = ecDown + ecSelection;
  ecSelectionWordLeft = ecWordLeft + ecSelection;
  ecSelectionWordRight = ecWordRight + ecSelection;
  ecSelectionLineStart = ecLineStart + ecSelection;
  ecSelectionLineEnd = ecLineEnd + ecSelection;
  ecSelectionPageUp = ecPageUp + ecSelection;
  ecSelectionPageDown = ecPageDown + ecSelection;
  ecSelectionPageLeft = ecPageLeft + ecSelection;
  ecSelectionPageRight = ecPageRight + ecSelection;
  ecSelectionPageTop = ecPageTop + ecSelection;
  ecSelectionPageBottom = ecPageBottom + ecSelection;
  ecSelectionEditorTop = ecEditorTop + ecSelection;
  ecSelectionEditorBottom = ecEditorBottom + ecSelection;
  ecSelectionGotoXY = ecGotoXY + ecSelection;
  ecSelectionScope = ecSelection + 21;
  ecSelectionWord = ecSelection + 22;
  ecSelectAll = ecSelection + 23;
  { Scrolling }
  ecScrollUp = 211;
  ecScrollDown = 212;
  ecScrollLeft = 213;
  ecScrollRight = 214;
  { Mode }
  ecInsertMode = 221;
  ecOverwriteMode = 222;
  ecToggleMode = 223;
  { Selection modes }
  ecNormalSelect = 231;
  ecColumnSelect = 232;
  ecLineSelect = 233;
  { Bookmark }
  ecGotoBookmark1 = 302;
  ecGotoBookmark2 = 303;
  ecGotoBookmark3 = 304;
  ecGotoBookmark4 = 305;
  ecGotoBookmark5 = 306;
  ecGotoBookmark6 = 307;
  ecGotoBookmark7 = 308;
  ecGotoBookmark8 = 309;
  ecGotoBookmark9 = 310;
  ecSetBookmark1 = 352;
  ecSetBookmark2 = 353;
  ecSetBookmark3 = 354;
  ecSetBookmark4 = 355;
  ecSetBookmark5 = 356;
  ecSetBookmark6 = 357;
  ecSetBookmark7 = 358;
  ecSetBookmark8 = 359;
  ecSetBookmark9 = 360;
  { Focus }
  ecGotFocus = 480;
  ecLostFocus = 481;
  { Help }
  ecContextHelp = 490;
  { Deletion }
  ecBackspace = 501;
  ecDeleteChar = 502;
  ecDeleteWord = 503;
  ecDeleteLastWord = 504;
  ecDeleteBeginningOfLine = 505;
  ecDeleteEndOfLine = 506;
  ecDeleteLine = 507;
  ecClear = 508;
  { Insert }
  ecLineBreak = 509;
  ecInsertLine = 510;
  ecChar = 511;
  ecString = 512;
  ecImeStr = 550;
  { Clipboard }
  ecUndo = 601;
  ecRedo = 602;
  ecCopy = 603;
  ecCut = 604;
  ecPaste = 605;
  { Indent }
  ecBlockIndent = 610;
  ecBlockUnindent = 611;
  ecTab = 612;
  ecShiftTab = 613;
  { Case }
  ecUpperCase = 620;
  ecLowerCase = 621;
  ecAlternatingCase = 622;
  ecSentenceCase = 623;
  ecTitleCase = 624;
  ecUpperCaseBlock = 625;
  ecLowerCaseBlock = 626;
  ecAlternatingCaseBlock = 627;
  { Move }
  ecMoveLineUp      = 701;
  ecMoveLineDown    = 702;
  ecMoveCharLeft    = 703;
  ecMoveCharRight   = 704;
  { Search }
  ecSearchNext = 800;
  ecSearchPrevious = 801;
  { Comments }
  ecLineComment = 900;
  ecBlockComment = 901;

  ecUserFirst = 1001;
  { code folding }
  ecCollapse = ecUserFirst + 100;
  ecUncollapse = ecUserFirst + 101;
  ecCollapseLevel = ecUserFirst + 102;
  ecUncollapseLevel = ecUserFirst + 103;
  ecCollapseAll = ecUserFirst + 104;
  ecUncollapseAll = ecUserFirst + 105;
  ecCollapseCurrent = ecUserFirst + 109;

type
  TBCEditorCommand = type Word;

  TBCEditorHookedCommandEvent = procedure(Sender: TObject; AfterProcessing: Boolean; var AHandled: Boolean;
    var ACommand: TBCEditorCommand; var AChar: Char; Data: Pointer; AHandlerData: Pointer) of object;
  TBCEditorProcessCommandEvent = procedure(Sender: TObject; var ACommand: TBCEditorCommand; var AChar: Char;
    AData: Pointer) of object;

  TBCEditorHookedCommandHandler = class(TObject)
  strict private
    FEvent: TBCEditorHookedCommandEvent;
    FData: Pointer;
  public
    constructor Create(AEvent: TBCEditorHookedCommandEvent; AData: pointer);
    function Equals(AEvent: TBCEditorHookedCommandEvent): Boolean; reintroduce;
    property Data: Pointer read FData write FData;
    property Event: TBCEditorHookedCommandEvent read FEvent write FEvent;
  end;

  TBCEditorKeyCommand = class(TCollectionItem)
  strict private
    FKey: Word;
    FSecondaryKey: Word;
    FShiftState: TShiftState;
    FSecondaryShiftState: TShiftState;
    FCommand: TBCEditorCommand;
    function GetShortCut: TShortCut;
    function GetSecondaryShortCut: TShortCut;
    procedure SetCommand(const AValue: TBCEditorCommand);
    procedure SetKey(const AValue: Word);
    procedure SetSecondaryKey(const AValue: Word);
    procedure SetShiftState(const AValue: TShiftState);
    procedure SetSecondaryShiftState(const AValue: TShiftState);
    procedure SetShortCut(const AValue: TShortCut);
    procedure SetSecondaryShortCut(const AValue: TShortCut);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(ASource: TPersistent); override;
    property Key: Word read FKey write SetKey;
    property SecondaryKey: Word read FSecondaryKey write SetSecondaryKey;
    property ShiftState: TShiftState read FShiftState write SetShiftState;
    property SecondaryShiftState: TShiftState read FSecondaryShiftState write SetSecondaryShiftState;
  published
    property Command: TBCEditorCommand read FCommand write SetCommand;
    property ShortCut: TShortCut read GetShortCut write SetShortCut default 0;
    property SecondaryShortCut: TShortCut read GetSecondaryShortCut write SetSecondaryShortCut default 0;
  end;

  TBCEditorKeyCommands = class(TCollection)
  strict private
    FOwner: TPersistent;
    function GetItem(AIndex: Integer): TBCEditorKeyCommand;
    procedure SetItem(AIndex: Integer; AValue: TBCEditorKeyCommand);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);

    function FindCommand(ACommand: TBCEditorCommand): Integer;
    function FindKeyCode(AKeyCode: Word; AShift: TShiftState): Integer;
    function FindKeyCodes(AKeyCode: Word; AShift: TShiftState; ASecondaryKeycode: Word; ASecondaryShift: TShiftState): Integer;
    function FindShortcut(AShortCut: TShortCut): Integer;
    function FindShortcuts(AShortCut, ASecondaryShortCut: TShortCut): Integer;
    function NewItem: TBCEditorKeyCommand;
    procedure Add(const ACommand: TBCEditorCommand; const AShift: TShiftState; const AKey: Word);
    procedure Assign(ASource: TPersistent); override;
    procedure ResetDefaults;
  public
    property Items[AIndex: Integer]: TBCEditorKeyCommand read GetItem write SetItem; default;
  end;

function IdentToEditorCommand(const AIdent: string; var ACommand: LongInt): Boolean;
function EditorCommandToIdent(ACommand: LongInt; var AIdent: string): Boolean;

implementation

uses
  Winapi.Windows, BCEditor.Language;

type
  TBCEditorCommandString = record
    Value: TBCEditorCommand;
    Name: string;
  end;

const
  EditorCommandStrings: array [0 .. 111] of TBCEditorCommandString = (
    (Value: ecNone; Name: 'ecNone'),
    (Value: ecLeft; Name: 'ecLeft'),
    (Value: ecRight; Name: 'ecRight'),
    (Value: ecUp; Name: 'ecUp'),
    (Value: ecDown; Name: 'ecDown'),
    (Value: ecWordLeft; Name: 'ecWordLeft'),
    (Value: ecWordRight; Name: 'ecWordRight'),
    (Value: ecLineStart; Name: 'ecLineStart'),
    (Value: ecLineEnd; Name: 'ecLineEnd'),
    (Value: ecPageUp; Name: 'ecPageUp'),
    (Value: ecPageDown; Name: 'ecPageDown'),
    (Value: ecPageLeft; Name: 'ecPageLeft'),
    (Value: ecPageRight; Name: 'ecPageRight'),
    (Value: ecPageTop; Name: 'ecPageTop'),
    (Value: ecPageBottom; Name: 'ecPageBottom'),
    (Value: ecEditorTop; Name: 'ecEditorTop'),
    (Value: ecEditorBottom; Name: 'ecEditorBottom'),
    (Value: ecGotoXY; Name: 'ecGotoXY'),
    (Value: ecSelectionLeft; Name: 'ecSelectionLeft'),
    (Value: ecSelectionRight; Name: 'ecSelectionRight'),
    (Value: ecSelectionUp; Name: 'ecSelectionUp'),
    (Value: ecSelectionDown; Name: 'ecSelectionDown'),
    (Value: ecSelectionWordLeft; Name: 'ecSelectionWordLeft'),
    (Value: ecSelectionWordRight; Name: 'ecSelectionWordRight'),
    (Value: ecSelectionLineStart; Name: 'ecSelectionLineStart'),
    (Value: ecSelectionLineEnd; Name: 'ecSelectionLineEnd'),
    (Value: ecSelectionPageUp; Name: 'ecSelectionPageUp'),
    (Value: ecSelectionPageDown; Name: 'ecSelectionPageDown'),
    (Value: ecSelectionPageLeft; Name: 'ecSelectionPageLeft'),
    (Value: ecSelectionPageRight; Name: 'ecSelectionPageRight'),
    (Value: ecSelectionPageTop; Name: 'ecSelectionPageTop'),
    (Value: ecSelectionPageBottom; Name: 'ecSelectionPageBottom'),
    (Value: ecSelectionEditorTop; Name: 'ecSelectionEditorTop'),
    (Value: ecSelectionEditorBottom; Name: 'ecSelectionEditorBottom'),
    (Value: ecSelectionGotoXY; Name: 'ecSelectionGotoXY'),
    (Value: ecSelectionWord; Name: 'ecSelectionWord'),
    (Value: ecSelectAll; Name: 'ecSelectAll'),
    (Value: ecScrollUp; Name: 'ecScrollUp'),
    (Value: ecScrollDown; Name: 'ecScrollDown'),
    (Value: ecScrollLeft; Name: 'ecScrollLeft'),
    (Value: ecScrollRight; Name: 'ecScrollRight'),
    (Value: ecBackspace; Name: 'ecBackspace'),
    (Value: ecDeleteChar; Name: 'ecDeleteChar'),
    (Value: ecDeleteWord; Name: 'ecDeleteWord'),
    (Value: ecDeleteLastWord; Name: 'ecDeleteLastWord'),
    (Value: ecDeleteBeginningOfLine; Name: 'ecDeleteBeginningOfLine'),
    (Value: ecDeleteEndOfLine; Name: 'ecDeleteEndOfLine'),
    (Value: ecDeleteLine; Name: 'ecDeleteLine'),
    (Value: ecClear; Name: 'ecClear'),
    (Value: ecLineBreak; Name: 'ecLineBreak'),
    (Value: ecInsertLine; Name: 'ecInsertLine'),
    (Value: ecChar; Name: 'ecChar'),
    (Value: ecImeStr; Name: 'ecImeStr'),
    (Value: ecUndo; Name: 'ecUndo'),
    (Value: ecRedo; Name: 'ecRedo'),
    (Value: ecCut; Name: 'ecCut'),
    (Value: ecCopy; Name: 'ecCopy'),
    (Value: ecPaste; Name: 'ecPaste'),
    (Value: ecInsertMode; Name: 'ecInsertMode'),
    (Value: ecOverwriteMode; Name: 'ecOverwriteMode'),
    (Value: ecToggleMode; Name: 'ecToggleMode'),
    (Value: ecBlockIndent; Name: 'ecBlockIndent'),
    (Value: ecBlockUnindent; Name: 'ecBlockUnindent'),
    (Value: ecTab; Name: 'ecTab'),
    (Value: ecShiftTab; Name: 'ecShiftTab'),
    (Value: ecNormalSelect; Name: 'ecNormalSelect'),
    (Value: ecColumnSelect; Name: 'ecColumnSelect'),
    (Value: ecLineSelect; Name: 'ecLineSelect'),
    (Value: ecUserFirst; Name: 'ecUserFirst'),
    (Value: ecContextHelp; Name: 'ecContextHelp'),
    (Value: ecGotoBookmark1; Name: 'ecGotoBookmark1'),
    (Value: ecGotoBookmark2; Name: 'ecGotoBookmark2'),
    (Value: ecGotoBookmark3; Name: 'ecGotoBookmark3'),
    (Value: ecGotoBookmark4; Name: 'ecGotoBookmark4'),
    (Value: ecGotoBookmark5; Name: 'ecGotoBookmark5'),
    (Value: ecGotoBookmark6; Name: 'ecGotoBookmark6'),
    (Value: ecGotoBookmark7; Name: 'ecGotoBookmark7'),
    (Value: ecGotoBookmark8; Name: 'ecGotoBookmark8'),
    (Value: ecGotoBookmark9; Name: 'ecGotoBookmark9'),
    (Value: ecSetBookmark1; Name: 'ecSetBookmark1'),
    (Value: ecSetBookmark2; Name: 'ecSetBookmark2'),
    (Value: ecSetBookmark3; Name: 'ecSetBookmark3'),
    (Value: ecSetBookmark4; Name: 'ecSetBookmark4'),
    (Value: ecSetBookmark5; Name: 'ecSetBookmark5'),
    (Value: ecSetBookmark6; Name: 'ecSetBookmark6'),
    (Value: ecSetBookmark7; Name: 'ecSetBookmark7'),
    (Value: ecSetBookmark8; Name: 'ecSetBookmark8'),
    (Value: ecSetBookmark9; Name: 'ecSetBookmark9'),
    (Value: ecString; Name: 'ecString'),
    (Value: ecMoveLineUp; Name: 'ecMoveLineUp'),
    (Value: ecMoveLineDown; Name: 'ecMoveLineDown'),
    (Value: ecMoveCharLeft; Name: 'ecMoveCharLeft'),
    (Value: ecMoveCharRight; Name: 'ecMoveCharRight'),
    (Value: ecUpperCase; Name: 'ecUpperCase'),
    (Value: ecLowerCase; Name: 'ecLowerCase'),
    (Value: ecAlternatingCase; Name: 'ecAlternatingCase'),
    (Value: ecSentenceCase; Name: 'ecSentenceCase'),
    (Value: ecTitleCase; Name: 'ecTitleCase'),
    (Value: ecUpperCaseBlock; Name: 'ecUpperCaseBlock'),
    (Value: ecLowerCaseBlock; Name: 'ecLowerCaseBlock'),
    (Value: ecAlternatingCaseBlock; Name: 'ecAlternatingCaseBlock'),
    (Value: ecCollapse; Name: 'ecCollapse'),
    (Value: ecUncollapse; Name: 'ecUncollapse'),
    (Value: ecCollapseLevel; Name: 'ecCollapseLevel'),
    (Value: ecUncollapseLevel; Name: 'ecUncollapseLevel'),
    (Value: ecCollapseAll; Name: 'ecCollapseAll'),
    (Value: ecUncollapseAll; Name: 'ecUncollapseAll'),
    (Value: ecCollapseCurrent; Name: 'ecCollapseCurrent'),
    (Value: ecSearchNext; Name: 'ecSearchNext'),
    (Value: ecSearchPrevious; Name: 'ecSearchPrevious'),
    (Value: ecLineComment; Name: 'ecLineComment'),
    (Value: ecBlockComment; Name: 'ecBlockComment')
  );

function IdentToEditorCommand(const AIdent: string; var ACommand: LongInt): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := Low(EditorCommandStrings) to High(EditorCommandStrings) do
    if CompareText(EditorCommandStrings[i].Name, AIdent) = 0 then
    begin
      ACommand := EditorCommandStrings[i].Value;
      Exit;
    end;
  Result := False;
end;

function EditorCommandToIdent(ACommand: LongInt; var AIdent: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := Low(EditorCommandStrings) to High(EditorCommandStrings) do
    if EditorCommandStrings[i].Value = ACommand then
    begin
      AIdent := EditorCommandStrings[i].Name;
      Exit;
    end;
  Result := False;
end;

function EditorCommandToCodeString(ACommand: TBCEditorCommand): string;
begin
  if not EditorCommandToIdent(ACommand, Result) then
    Result := IntToStr(ACommand);
end;

{ TBCEditorHookedCommandHandler }

constructor TBCEditorHookedCommandHandler.Create(AEvent: TBCEditorHookedCommandEvent; AData: pointer);
begin
  inherited Create;
  FEvent := AEvent;
  FData := AData;
end;

function TBCEditorHookedCommandHandler.Equals(AEvent: TBCEditorHookedCommandEvent): Boolean;
begin
  Result := (TMethod(FEvent).Code = TMethod(AEvent).Code) and (TMethod(FEvent).Data = TMethod(AEvent).Data);
end;

{ TBCEditorKeyCommand }

procedure TBCEditorKeyCommand.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorKeyCommand) then
  with ASource as TBCEditorKeyCommand do
  begin
    Self.FCommand := FCommand;
    Self.FKey := FKey;
    Self.FSecondaryKey := FSecondaryKey;
    Self.FShiftState := FShiftState;
    Self.FSecondaryShiftState := FSecondaryShiftState;
  end
  else
    inherited Assign(ASource);
end;

function TBCEditorKeyCommand.GetDisplayName: string;
begin
  Result := EditorCommandToCodeString(Command) + ' - ' + ShortCutToText(ShortCut);
  if SecondaryShortCut <> 0 then
    Result := Result + ' ' + ShortCutToText(SecondaryShortCut);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TBCEditorKeyCommand.GetShortCut: TShortCut;
begin
  Result := Vcl.Menus.ShortCut(Key, ShiftState);
end;

procedure TBCEditorKeyCommand.SetCommand(const AValue: TBCEditorCommand);
begin
  if FCommand <> AValue then
    FCommand := AValue;
end;

procedure TBCEditorKeyCommand.SetKey(const AValue: Word);
begin
  if FKey <> AValue then
    FKey := AValue;
end;

procedure TBCEditorKeyCommand.SetShiftState(const AValue: TShiftState);
begin
  if FShiftState <> AValue then
    FShiftState := AValue;
end;

procedure TBCEditorKeyCommand.SetShortCut(const AValue: TShortCut);
var
  LNewKey: Word;
  LNewShiftState: TShiftState;
  LDuplicate: Integer;
begin
  if AValue <> 0 then
  begin
    LDuplicate := TBCEditorKeyCommands(Collection).FindShortcuts(AValue, SecondaryShortCut);
    if (LDuplicate <> -1) and (LDuplicate <> Self.Index) then
      raise Exception.Create(SBCEditorDuplicateShortcut);
  end;

  Vcl.Menus.ShortCutToKey(AValue, LNewKey, LNewShiftState);

  if (LNewKey <> Key) or (LNewShiftState <> ShiftState) then
  begin
    Key := LNewKey;
    ShiftState := LNewShiftState;
  end;
end;

procedure TBCEditorKeyCommand.SetSecondaryKey(const AValue: Word);
begin
  if FSecondaryKey <> AValue then
    FSecondaryKey := AValue;
end;

procedure TBCEditorKeyCommand.SetSecondaryShiftState(const AValue: TShiftState);
begin
  if FSecondaryShiftState <> AValue then
    FSecondaryShiftState := AValue;
end;

procedure TBCEditorKeyCommand.SetSecondaryShortCut(const AValue: TShortCut);
var
  LNewKey: Word;
  LNewShiftState: TShiftState;
  LDuplicate: Integer;
begin
  if AValue <> 0 then
  begin
    LDuplicate := TBCEditorKeyCommands(Collection).FindShortcuts(ShortCut, AValue);
    if (LDuplicate <> -1) and (LDuplicate <> Self.Index) then
      raise Exception.Create(SBCEditOrduplicateShortcut);
  end;

  Vcl.Menus.ShortCutToKey(AValue, LNewKey, LNewShiftState);
  if (LNewKey <> SecondaryKey) or (LNewShiftState <> SecondaryShiftState) then
  begin
    SecondaryKey := LNewKey;
    SecondaryShiftState := LNewShiftState;
  end;
end;

function TBCEditorKeyCommand.GetSecondaryShortCut: TShortCut;
begin
  Result := Vcl.Menus.ShortCut(SecondaryKey, SecondaryShiftState);
end;

{ TBCEditorKeyCommands }

function TBCEditorKeyCommands.NewItem: TBCEditorKeyCommand;
begin
  Result := TBCEditorKeyCommand(inherited Add);
end;

procedure TBCEditorKeyCommands.Add(const ACommand: TBCEditorCommand; const AShift: TShiftState; const AKey: Word);
var
  LNewKeystroke: TBCEditorKeyCommand;
begin
  LNewKeystroke := NewItem;
  LNewKeystroke.Key := AKey;
  LNewKeystroke.ShiftState := AShift;
  LNewKeystroke.Command := ACommand;
end;

procedure TBCEditorKeyCommands.Assign(ASource: TPersistent);
var
  i: Integer;
  LKeyCommands: TBCEditorKeyCommands;
begin
  if Assigned(ASource) and (ASource is TBCEditorKeyCommands) then
  begin
    LKeyCommands := ASource as TBCEditorKeyCommands;
    Self.Clear;
    for i := 0 to LKeyCommands.Count - 1 do
      NewItem.Assign(LKeyCommands[i]);
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorKeyCommands.Create(AOwner: TPersistent);
begin
  inherited Create(TBCEditorKeyCommand);

  FOwner := AOwner;
end;

function TBCEditorKeyCommands.FindCommand(ACommand: TBCEditorCommand): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].Command = ACommand then
    begin
      Result := i;
      Break;
    end;
end;

function TBCEditorKeyCommands.FindKeyCode(AKeycode: Word; AShift: TShiftState): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if (Items[i].Key = AKeyCode) and (Items[i].ShiftState = AShift) and (Items[i].SecondaryKey = 0) then
    begin
      Result := i;
      Break;
    end;
end;

function TBCEditorKeyCommands.FindKeyCodes(AKeyCode: Word; AShift: TShiftState; ASecondaryKeyCode: Word; ASecondaryShift: TShiftState): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if (Items[i].Key = AKeyCode) and (Items[i].ShiftState = AShift) and (Items[i].SecondaryKey = ASecondaryKeyCode) and
      (Items[i].SecondaryShiftState = ASecondaryShift) then
    begin
      Result := i;
      Break;
    end;
end;

function TBCEditorKeyCommands.FindShortcut(AShortCut: TShortCut): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].ShortCut = AShortCut then
    begin
      Result := i;
      Break;
    end;
end;

function TBCEditorKeyCommands.FindShortcuts(AShortCut, ASecondaryShortCut: TShortCut): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if (Items[i].ShortCut = AShortCut) and (Items[i].SecondaryShortCut = ASecondaryShortCut) then
    begin
      Result := i;
      break;
    end;
end;

function TBCEditorKeyCommands.GetItem(AIndex: Integer): TBCEditorKeyCommand;
begin
  Result := TBCEditorKeyCommand(inherited GetItem(AIndex));
end;

function TBCEditorKeyCommands.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TBCEditorKeyCommands.ResetDefaults;
begin
  Clear;

  { Scrolling, caret moving and selection }
  Add(ecUp, [], VK_UP);
  Add(ecSelectionUp, [ssShift], VK_UP);
  Add(ecScrollUp, [ssCtrl], VK_UP);
  Add(ecDown, [], VK_DOWN);
  Add(ecSelectionDown, [ssShift], VK_DOWN);
  Add(ecScrollDown, [ssCtrl], VK_DOWN);
  Add(ecLeft, [], VK_LEFT);
  Add(ecSelectionLeft, [ssShift], VK_LEFT);
  Add(ecWordLeft, [ssCtrl], VK_LEFT);
  Add(ecSelectionWordLeft, [ssShift, ssCtrl], VK_LEFT);
  Add(ecRight, [], VK_RIGHT);
  Add(ecSelectionRight, [ssShift], VK_RIGHT);
  Add(ecWordRight, [ssCtrl], VK_RIGHT);
  Add(ecSelectionWordRight, [ssShift, ssCtrl], VK_RIGHT);
  Add(ecPageDown, [], VK_NEXT);
  Add(ecSelectionPageDown, [ssShift], VK_NEXT);
  Add(ecPageBottom, [ssCtrl], VK_NEXT);
  Add(ecSelectionPageBottom, [ssShift, ssCtrl], VK_NEXT);
  Add(ecPageUp, [], VK_PRIOR);
  Add(ecSelectionPageUp, [ssShift], VK_PRIOR);
  Add(ecPageTop, [ssCtrl], VK_PRIOR);
  Add(ecSelectionPageTop, [ssShift, ssCtrl], VK_PRIOR);
  Add(ecLineStart, [], VK_HOME);
  Add(ecSelectionLineStart, [ssShift], VK_HOME);
  Add(ecEditorTop, [ssCtrl], VK_HOME);
  Add(ecSelectionEditorTop, [ssShift, ssCtrl], VK_HOME);
  Add(ecLineEnd, [], VK_END);
  Add(ecSelectionLineEnd, [ssShift], VK_END);
  Add(ecEditorBottom, [ssCtrl], VK_END);
  Add(ecSelectionEditorBottom, [ssShift, ssCtrl], VK_END);
  { Insert key alone }
  Add(ecToggleMode, [], VK_INSERT);
  { Clipboard }
  Add(ecUndo, [ssAlt], VK_BACK);
  Add(ecRedo, [ssAlt, ssShift], VK_BACK);
  Add(ecCopy, [ssCtrl], VK_INSERT);
  Add(ecCut, [ssShift], VK_DELETE);
  Add(ecPaste, [ssShift], VK_INSERT);
  { Deletion }
  Add(ecDeleteChar, [], VK_DELETE);
  Add(ecBackspace, [], VK_BACK);
  Add(ecBackspace, [ssShift], VK_BACK);
  Add(ecDeleteLastWord, [ssCtrl], VK_BACK);
  { Search }
  Add(ecSearchNext, [], VK_F3);
  Add(ecSearchPrevious, [ssShift], VK_F3);
  { Enter (return) & Tab }
  Add(ecLineBreak, [], VK_RETURN);
  Add(ecLineBreak, [ssShift], VK_RETURN);
  Add(ecTab, [], VK_TAB);
  Add(ecShiftTab, [ssShift], VK_TAB);
  { Help }
  Add(ecContextHelp, [], VK_F1);
  { Standard edit commands }
  Add(ecUndo, [ssCtrl], Ord('Z'));
  Add(ecRedo, [ssCtrl, ssShift], Ord('Z'));
  Add(ecCut, [ssCtrl], Ord('X'));
  Add(ecCopy, [ssCtrl], Ord('C'));
  Add(ecPaste, [ssCtrl], Ord('V'));
  Add(ecSelectAll, [ssCtrl], Ord('A'));
  { Block commands }
  Add(ecBlockIndent, [ssCtrl, ssShift], Ord('I'));
  Add(ecBlockUnindent, [ssCtrl, ssShift], Ord('U'));
  { Fragment deletion }
  Add(ecDeleteWord, [ssCtrl], Ord('T'));
  { Line operations }
  Add(ecInsertLine, [ssCtrl], Ord('M'));
  Add(ecMoveLineUp, [ssCtrl, ssAlt], VK_UP);
  Add(ecMoveLineDown, [ssCtrl, ssAlt], VK_DOWN);
  Add(ecDeleteLine, [ssCtrl], Ord('Y'));
  Add(ecDeleteEndOfLine, [ssCtrl, ssShift], Ord('Y'));
  Add(ecMoveCharLeft, [ssAlt, ssCtrl], VK_LEFT);
  Add(ecMoveCharRight, [ssAlt, ssCtrl], VK_RIGHT);
  { Bookmarks }
  Add(ecGotoBookmark1, [ssCtrl], Ord('1'));
  Add(ecGotoBookmark2, [ssCtrl], Ord('2'));
  Add(ecGotoBookmark3, [ssCtrl], Ord('3'));
  Add(ecGotoBookmark4, [ssCtrl], Ord('4'));
  Add(ecGotoBookmark5, [ssCtrl], Ord('5'));
  Add(ecGotoBookmark6, [ssCtrl], Ord('6'));
  Add(ecGotoBookmark7, [ssCtrl], Ord('7'));
  Add(ecGotoBookmark8, [ssCtrl], Ord('8'));
  Add(ecGotoBookmark9, [ssCtrl], Ord('9'));
  Add(ecSetBookmark1, [ssCtrl, ssShift], Ord('1'));
  Add(ecSetBookmark2, [ssCtrl, ssShift], Ord('2'));
  Add(ecSetBookmark3, [ssCtrl, ssShift], Ord('3'));
  Add(ecSetBookmark4, [ssCtrl, ssShift], Ord('4'));
  Add(ecSetBookmark5, [ssCtrl, ssShift], Ord('5'));
  Add(ecSetBookmark6, [ssCtrl, ssShift], Ord('6'));
  Add(ecSetBookmark7, [ssCtrl, ssShift], Ord('7'));
  Add(ecSetBookmark8, [ssCtrl, ssShift], Ord('8'));
  Add(ecSetBookmark9, [ssCtrl, ssShift], Ord('9'));
  { Selection modes }
  Add(ecNormalSelect, [ssCtrl, ssAlt], Ord('N'));
  Add(ecColumnSelect, [ssCtrl, ssAlt], Ord('C'));
  Add(ecLineSelect, [ssCtrl, ssAlt], Ord('L'));
  { Comments }
  Add(ecLineComment, [ssCtrl], VK_OEM_2);
  Add(ecBlockComment, [ssCtrl, ssShift], VK_OEM_2);
end;

procedure TBCEditorKeyCommands.SetItem(AIndex: Integer; AValue: TBCEditorKeyCommand);
begin
  inherited SetItem(AIndex, AValue);
end;

initialization

  RegisterIntegerConsts(TypeInfo(TBCEditorCommand), IdentToEditorCommand, EditorCommandToIdent);

finalization

  UnregisterIntegerConsts(TypeInfo(TBCEditorCommand), IdentToEditorCommand, EditorCommandToIdent);

end.
