unit BCEditor.Highlighter.Rules;

interface

uses
  Vcl.Graphics, System.Classes, System.SysUtils, BCEditor.Highlighter.Token, BCEditor.Highlighter.Attributes,
  BCEditor.Consts, BCEditor.Types;

type
  TBCEditorRange = class;
  TBCEditorSet = class;

  TBCEditorAbstractParser = class abstract
  public
    function GetToken(ACurrentRule: TBCEditorRange; APLine: PChar; var ARun: Integer; var AToken: TBCEditorToken): Boolean;
      virtual; abstract;
  end;

  TBCEditorParser = class(TBCEditorAbstractParser)
  strict private
    FHeadNode: TBCEditorTokenNode;
    FSets: TList;
  public
    constructor Create(AChar: Char; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType); reintroduce; overload; virtual;
    constructor Create(ASet: TBCEditorSet); reintroduce; overload; virtual;
    destructor Destroy; override;

    function GetToken(ACurrentRange: TBCEditorRange; APLine: PChar; var ARun: Integer; var AToken: TBCEditorToken): Boolean; override;
    procedure AddSet(ASet: TBCEditorSet);
    procedure AddTokenNode(const AString: string; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType);
    property HeadNode: TBCEditorTokenNode read FHeadNode;
    property Sets: TList read FSets;
  end;

  TBCEditorDefaultParser = class(TBCEditorAbstractParser)
  strict private
    FToken: TBCEditorToken;
  public
    constructor Create(AToken: TBCEditorToken); reintroduce; virtual;
    destructor Destroy; override;

    function GetToken(ACurrentRange: TBCEditorRange; APLine: PChar; var ARun: Integer; var AToken: TBCEditorToken): Boolean; override;
    property Token: TBCEditorToken read FToken;
  end;

  TDelimitersParser = class(TBCEditorAbstractParser)
  strict private
    FToken: TBCEditorToken;
  public
    constructor Create(AToken: TBCEditorToken); virtual;
    destructor Destroy; override;

    function GetToken(ACurrentRange: TBCEditorRange; APLine: PChar; var ARun: Integer; var AToken: TBCEditorToken): Boolean; override;
    property Token: TBCEditorToken read FToken;
  end;

  TBCEditorRule = class(TBCEditorAbstractRule)
  private
    FStyle: string;
    FAttribute: TBCEditorHighlighterAttribute;
  protected
    FParent: TBCEditorRange;
  public
    constructor Create;
    destructor Destroy; override;

    property Attribute: TBCEditorHighlighterAttribute read FAttribute;
    property Parent: TBCEditorRange read FParent write FParent;
    property Style: string read FStyle;
  end;

  TBCEditorKeyList = class(TBCEditorRule)
  strict private
    FKeyList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property KeyList: TStringList read FKeyList write FKeyList;
  end;

  TBCEditorSet = class(TBCEditorRule)
  strict private
    FCharSet: TBCEditorCharSet;
  public
    constructor Create(ACharSet: TBCEditorCharSet = []);
    property CharSet: TBCEditorCharSet read FCharSet write FCharSet;
  end;

  TBCEditorAbstractParserArray = array [AnsiChar] of TBCEditorAbstractParser;

  TBCEditorCaseFunction = function(const AChar: Char): Char;
  TBCEditorStringCaseFunction = function(const AString: string): string;

  TBCEditorRange = class(TBCEditorRule)
  strict private
    FAlternativeCloseArray: TBCEditorArrayOfString;
    FAlternativeCloseArrayCount: Integer;
    FCaseFunct: TBCEditorCaseFunction;
    FCaseSensitive: Boolean;
    FCloseOnEndOfLine: Boolean;
    FCloseOnTerm: Boolean;
    FCloseParent: Boolean;
    FCloseToken: TBCEditorMultiToken;
    FClosingToken: TBCEditorToken;
    FDefaultSymbols: TBCEditorDefaultParser;
    FDefaultTermSymbol: TDelimitersParser;
    FDefaultToken: TBCEditorToken;
    FDelimiters: TBCEditorCharSet;
    FKeyList: TList;
    FOpenBeginningOfLine: Boolean;
    FOpenToken: TBCEditorMultiToken;
    FPrepared: Boolean;
    FRanges: TList;
    FSets: TList;
    FSkipWhitespace: Boolean;
    FStringCaseFunct: TBCEditorStringCaseFunction;
    FSymbolList: TBCEditorAbstractParserArray;
    FTokens: TList;
    FUseDelimitersForText: Boolean;
    function GetKeyList(const AIndex: Integer): TBCEditorKeyList;
    function GetKeyListCount: Integer;
    function GetRange(const AIndex: Integer): TBCEditorRange;
    function GetRangeCount: Integer;
    function GetSet(const AIndex: Integer): TBCEditorSet;
    function GetSetCount: Integer;
    function GetToken(const AIndex: Integer): TBCEditorToken;
    procedure SetAlternativeCloseArrayCount(const AValue: Integer);
    procedure SetCaseSensitive(const AValue: Boolean);
  public
    constructor Create(const AOpenToken: string = ''; const ACloseToken: string = ''); virtual;
    destructor Destroy; override;

    function FindToken(const AString: string): TBCEditorToken;
    procedure AddKeyList(NewKeyList: TBCEditorKeyList);
    procedure AddRange(NewRange: TBCEditorRange);
    procedure AddSet(NewSet: TBCEditorSet);
    procedure AddToken(const AToken: TBCEditorToken);
    procedure AddTokenRange(const AOpenToken: string; AOpenTokenBreakType: TBCEditorBreakType; const ACloseToken: string;
      ACloseTokenBreakType: TBCEditorBreakType);
    procedure Clear;
    procedure Prepare(AParent: TBCEditorRange);
    procedure Reset;
    procedure SetDelimiters(const ADelimiters: TBCEditorCharSet);
    property AlternativeCloseArray: TBCEditorArrayOfString read FAlternativeCloseArray write FAlternativeCloseArray;
    property AlternativeCloseArrayCount: Integer read FAlternativeCloseArrayCount write SetAlternativeCloseArrayCount;
    property OpenBeginningOfLine: Boolean read FOpenBeginningOfLine write FOpenBeginningOfLine;
    property CaseFunct: TBCEditorCaseFunction read FCaseFunct;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property CloseOnEndOfLine: Boolean read FCloseOnEndOfLine write FCloseOnEndOfLine;
    property CloseOnTerm: Boolean read FCloseOnTerm write FCloseOnTerm;
    property SkipWhitespace: Boolean read FSkipWhitespace write FSkipWhitespace;
    property CloseParent: Boolean read FCloseParent write FCloseParent;
    property CloseToken: TBCEditorMultiToken read FCloseToken write FCloseToken;
    property ClosingToken: TBCEditorToken read FClosingToken write FClosingToken;
    property DefaultToken: TBCEditorToken read FDefaultToken;
    property Delimiters: TBCEditorCharSet read FDelimiters write FDelimiters;
    property KeyListCount: Integer read GetKeyListCount;
    property KeyList[const AIndex: Integer]: TBCEditorKeyList read GetKeyList;
    property OpenToken: TBCEditorMultiToken read FOpenToken write FOpenToken;
    property Prepared: Boolean read FPrepared;
    property RangeCount: Integer read GetRangeCount;
    property Ranges[const AIndex: Integer]: TBCEditorRange read GetRange;
    property SetCount: Integer read GetSetCount;
    property Sets[const AIndex: Integer]: TBCEditorSet read GetSet;
    property StringCaseFunct: TBCEditorStringCaseFunction read FStringCaseFunct;
    property SymbolList: TBCEditorAbstractParserArray read FSymbolList;
    property Tokens[const AIndex: Integer]: TBCEditorToken read GetToken;
    property UseDelimitersForText: Boolean read FUseDelimitersForText write FUseDelimitersForText;
  end;

implementation

uses
  BCEditor.Utils, System.Types;

{ TBCEditorParser }

constructor TBCEditorParser.Create(AChar: Char; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType);
begin
  inherited Create;

  FHeadNode := TBCEditorTokenNode.Create(AChar, AToken, ABreakType);
  FSets := TList.Create;
end;

constructor TBCEditorParser.Create(ASet: TBCEditorSet);
begin
  inherited Create;

  FSets := TList.Create;
  AddSet(ASet);
end;

destructor TBCEditorParser.Destroy;
begin
  if Assigned(FHeadNode) then
  begin
    FHeadNode.Free;
    FHeadNode := nil;
  end;
  FSets.Clear;
  FSets.Free;
  FSets := nil;
  inherited;
end;

procedure TBCEditorParser.AddTokenNode(const AString: string; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType);
var
  LIndex: Integer;
  LLength: Integer;
  LTokenNode: TBCEditorTokenNode;
  LTokenNodeList: TBCEditorTokenNodeList;
  LChar: Char;
begin
  LTokenNodeList := HeadNode.NextNodes;
  LTokenNode := nil;
  LLength := Length(AString);
  for LIndex := 1 to LLength do
  begin
    LChar := AString[LIndex];
    LTokenNode := LTokenNodeList.FindNode(LChar);
    if not Assigned(LTokenNode) then
    begin
      LTokenNode := TBCEditorTokenNode.Create(LChar);
      LTokenNodeList.AddNode(LTokenNode);
    end;
    LTokenNodeList := LTokenNode.NextNodes;
  end;
  LTokenNode.BreakType := ABreakType;
  LTokenNode.Token := AToken;
end;

procedure TBCEditorParser.AddSet(ASet: TBCEditorSet);
begin
  Sets.Add(ASet);
end;

function TBCEditorParser.GetToken(ACurrentRange: TBCEditorRange; APLine: PChar; var ARun: Integer;
  var AToken: TBCEditorToken): Boolean;
var
  LCurrentTokenNode, LStartTokenNode, LFindTokenNode: TBCEditorTokenNode;
  LIndex, LStartPosition, LNextPosition, LPreviousPosition: Integer;
  LAllowedDelimiters: TBCEditorCharSet;
  LSet: TBCEditorSet;
  LChar: Char;
begin
  Result := False;

  LStartPosition := ARun;
  if Assigned(HeadNode) then
  begin
    LCurrentTokenNode := HeadNode;
    LNextPosition := LStartPosition;
    LStartTokenNode := nil;
    repeat
      if Assigned(LStartTokenNode) then
      begin
        LCurrentTokenNode := LStartTokenNode;
        ARun := LNextPosition;
        LStartTokenNode := nil;
      end;
      if Assigned(LCurrentTokenNode.Token) then
        LFindTokenNode := LCurrentTokenNode
      else
        LFindTokenNode := nil;
      LPreviousPosition := ARun;
      while (LCurrentTokenNode.NextNodes.Count > 0) and (APLine[ARun] <> BCEDITOR_NONE_CHAR) do
      begin
        Inc(ARun);
        LCurrentTokenNode := LCurrentTokenNode.NextNodes.FindNode(ACurrentRange.CaseFunct(APLine[ARun]));
        if not Assigned(LCurrentTokenNode) then
        begin
          Dec(ARun);
          Break;
        end;

        if Assigned(LCurrentTokenNode.Token) then
        begin
          LFindTokenNode := LCurrentTokenNode;
          LPreviousPosition := ARun;
        end;

        if not Assigned(LStartTokenNode) then
          if CharInSet(LCurrentTokenNode.Char, ACurrentRange.Delimiters) then
          begin
            LStartTokenNode := LCurrentTokenNode;
            lNextPosition := ARun;
          end;
      end;

      ARun := LPreviousPosition;

      if not Assigned(LFindTokenNode) or not Assigned(LFindTokenNode.Token) or
        ((LFindTokenNode.Token.Attribute.EscapeChar <> BCEDITOR_NONE_CHAR) and
        (LStartPosition > 0) and (APLine[LStartPosition - 1] = LFindTokenNode.Token.Attribute.EscapeChar)) then
        Continue;

      if APLine[ARun] <> BCEDITOR_NONE_CHAR then
        Inc(ARun);

      if (LFindTokenNode.BreakType = btAny) or (CharInSet(APLine[ARun], ACurrentRange.Delimiters)) then
      begin
        AToken := LFindTokenNode.Token;
        Exit(True);
      end;
    until not Assigned(LStartTokenNode);
  end;

  LAllowedDelimiters := ACurrentRange.Delimiters;
  for LIndex := 0 to Sets.Count - 1 do
    LAllowedDelimiters := LAllowedDelimiters - TBCEditorSet(Sets.List[LIndex]).CharSet;

  for LIndex := 0 to Sets.Count - 1 do
  begin
    ARun := LStartPosition;
    LSet := TBCEditorSet(Sets.List[LIndex]);
    repeat
      Inc(ARun);
      LChar := APLine[ARun];
    until not CharInSet(LChar, LSet.CharSet) or (LChar = BCEDITOR_NONE_CHAR);

    if CharInSet(LChar, LAllowedDelimiters) then
    begin
      AToken := TBCEditorToken.Create(LSet.Attribute);
      AToken.Temporary := True;
      Exit(True);
    end;
  end;
  ARun := LStartPosition + 1;
end;

constructor TBCEditorDefaultParser.Create(AToken: TBCEditorToken);
begin
  FToken := AToken;
end;

destructor TBCEditorDefaultParser.Destroy;
begin
  FToken.Free;
  FToken := nil;
  inherited;
end;

function TBCEditorDefaultParser.GetToken(ACurrentRange: TBCEditorRange; APLine: PChar; var ARun: Integer;
  var AToken: TBCEditorToken): Boolean;
begin
  Inc(ARun);
  Result := False;
end;

constructor TDelimitersParser.Create(AToken: TBCEditorToken);
begin
  inherited Create;
  FToken := AToken;
end;

destructor TDelimitersParser.Destroy;
begin
  FToken.Free;
  FToken := nil;
  inherited;
end;

function TDelimitersParser.GetToken(ACurrentRange: TBCEditorRange; APLine: PChar; var ARun: Integer; var AToken: TBCEditorToken): Boolean;
begin
  if APLine[ARun] <> BCEDITOR_NONE_CHAR then
    Inc(ARun);
  AToken := Self.Token;
  Result := True;
end;

constructor TBCEditorRule.Create;
begin
  inherited;

  FAttribute := TBCEditorHighlighterAttribute.Create('');
end;

destructor TBCEditorRule.Destroy;
begin
  FAttribute.Free;
  FAttribute := nil;

  inherited;
end;

{ TBCEditorRange }

constructor TBCEditorRange.Create(const AOpenToken: string; const ACloseToken: string);
begin
  inherited Create;

  FOpenToken := TBCEditorMultiToken.Create;
  FCloseToken := TBCEditorMultiToken.Create;
  AddTokenRange(AOpenToken, btUnspecified, ACloseToken, btUnspecified);

  SetCaseSensitive(False);

  FAlternativeCloseArrayCount := 0;

  FPrepared := False;

  FRanges := TList.Create;
  FKeyList := TList.Create;
  FSets := TList.Create;
  FTokens := TList.Create;

  FDelimiters := BCEDITOR_DEFAULT_DELIMITERS;

  FAttribute.Foreground := clWindowText;
  FAttribute.Background := clWindow;
end;

destructor TBCEditorRange.Destroy;
begin
  Clear;
  Reset;

  FOpenToken.Free;
  FOpenToken := nil;
  FCloseToken.Free;
  FCloseToken := nil;
  FAttribute.Free;
  FAttribute := nil;
  FKeyList.Free;
  FSets.Free;
  FTokens.Free;
  FTokens := nil;
  FRanges.Free;
  FRanges := nil;

  inherited;
end;

procedure TBCEditorRange.AddToken(const AToken: TBCEditorToken);
var
  LToken: TBCEditorToken;
  LLow, LHigh, LMiddle, LCompare: Integer;
begin
  LLow := 0;
  LHigh := FTokens.Count - 1;

  while LLow <= LHigh do
  begin
    LMiddle := LLow + (LHigh - LLow) shr 1;
    LToken := TBCEditorToken(FTokens.Items[LMiddle]);
    LCompare := CompareStr(LToken.Symbol, AToken.Symbol);

    if LCompare < 0 then
      LLow := LMiddle + 1
    else
    if LCompare > 0 then
      LHigh := LMiddle - 1
    else
      Exit;
  end;

  FTokens.Insert(LLow, AToken);
end;

function TBCEditorRange.FindToken(const AString: string): TBCEditorToken;
var
  LToken: TBCEditorToken;
  LLow, LHigh, LMiddle, LCompare: Integer;
begin
  Result := nil;

  LLow := 0;
  LHigh := FTokens.Count - 1;

  while LLow <= LHigh do
  begin
    LMiddle := LLow + (LHigh - LLow) shr 1;

    LToken := TBCEditorToken(FTokens.Items[LMiddle]);
    LCompare := CompareStr(LToken.Symbol, AString);

    if LCompare = 0 then
      Exit(LToken)
    else
    if LCompare < 0 then
      LLow := LMiddle + 1
    else
      LHigh := LMiddle - 1;
  end;
end;

procedure TBCEditorRange.AddRange(NewRange: TBCEditorRange);
begin
  FRanges.Add(NewRange);
  NewRange.Parent := Self;
end;

procedure TBCEditorRange.AddKeyList(NewKeyList: TBCEditorKeyList);
begin
  FKeyList.Add(NewKeyList);
  NewKeyList.Parent := Self;
end;

procedure TBCEditorRange.AddSet(NewSet: TBCEditorSet);
begin
  FSets.Add(NewSet);
  NewSet.Parent := Self;
end;

function TBCEditorRange.GetRangeCount: Integer;
begin
  Result := FRanges.Count;
end;

function TBCEditorRange.GetKeyListCount: Integer;
begin
  Result := FKeyList.Count;
end;

function TBCEditorRange.GetSetCount: Integer;
begin
  Result := FSets.Count;
end;

function TBCEditorRange.GetToken(const AIndex: Integer): TBCEditorToken;
begin
  Result := TBCEditorToken(FTokens[AIndex]);
end;

function TBCEditorRange.GetRange(const AIndex: Integer): TBCEditorRange;
begin
  Result := TBCEditorRange(FRanges[AIndex]);
end;

function TBCEditorRange.GetKeyList(const AIndex: Integer): TBCEditorKeyList;
begin
  Result := TBCEditorKeyList(FKeyList[AIndex]);
end;

function TBCEditorRange.GetSet(const AIndex: Integer): TBCEditorSet;
begin
  Result := TBCEditorSet(FSets.List[AIndex]);
end;

procedure TBCEditorRange.AddTokenRange(const AOpenToken: string; AOpenTokenBreakType: TBCEditorBreakType; const ACloseToken: string;
  ACloseTokenBreakType: TBCEditorBreakType);
begin
  FOpenToken.AddSymbol(AOpenToken);
  FOpenToken.BreakType := AOpenTokenBreakType;
  FCloseToken.AddSymbol(ACloseToken);
  FCloseToken.BreakType := ACloseTokenBreakType;
end;

procedure TBCEditorRange.SetDelimiters(const ADelimiters: TBCEditorCharSet);
var
  LIndex: Integer;
begin
  Delimiters := ADelimiters;
  for LIndex := 0 to RangeCount - 1 do
    Ranges[LIndex].SetDelimiters(ADelimiters);
end;

procedure TBCEditorRange.SetAlternativeCloseArrayCount(const AValue: Integer);
begin
  FAlternativeCloseArrayCount := AValue;
  SetLength(FAlternativeCloseArray, AValue);
end;

procedure TBCEditorRange.SetCaseSensitive(const AValue: Boolean);
begin
  FCaseSensitive := AValue;
  if not AValue then
  begin
    FCaseFunct := CaseUpper;
    FStringCaseFunct := AnsiUpperCase;
  end
  else
  begin
    FCaseFunct := CaseNone;
    FStringCaseFunct := CaseStringNone;
  end;
end;

procedure TBCEditorRange.Prepare(AParent: TBCEditorRange);
var
  LIndex, LIndex2: Integer;
  LLength: Integer;
  LSymbol: string;
  LFirstChar: Char;
  LBreakType: TBCEditorBreakType;

  function InsertTokenDefault(AToken: TBCEditorToken; ARules: TBCEditorRange; AAttribute: TBCEditorHighlighterAttribute): TBCEditorToken;
  begin
    Result := ARules.FindToken(AToken.Symbol);
    if not Assigned(Result) then
      Result := AToken
    else
      AToken.Free;
    ARules.AddToken(Result);
    if not Assigned(Result.Attribute) then
      Result.Attribute := AAttribute;
  end;

  procedure InsertToken(AToken: TBCEditorToken; ARules: TBCEditorRange);
  var
    LToken: TBCEditorToken;
  begin
    LToken := ARules.FindToken(AToken.Symbol);
    if not Assigned(LToken) then
      ARules.AddToken(AToken)
    else
    begin
      LToken.Attribute := AToken.Attribute;
      AToken.Free;
    end;
  end;

var
  LRange: TBCEditorRange;
  LKeyList: TBCEditorKeyList;
  LToken, LTempToken: TBCEditorToken;
  LAnsiChar: AnsiChar;
  LChar: Char;
  LSet: TBCEditorSet;
begin
  Reset;
  FDefaultToken := TBCEditorToken.Create(Attribute);
  if Assigned(FDefaultTermSymbol) then
  begin
    FDefaultTermSymbol.Free;
    FDefaultTermSymbol := nil;
  end;
  FDefaultTermSymbol := TDelimitersParser.Create(TBCEditorToken.Create(Attribute));
  FDefaultSymbols := TBCEditorDefaultParser.Create(TBCEditorToken.Create(Attribute));

  FDelimiters := FDelimiters + BCEDITOR_ABSOLUTE_DELIMITERS;

  if Assigned(FRanges) then
  for LIndex := 0 to FRanges.Count - 1 do
  begin
    LRange := TBCEditorRange(FRanges[LIndex]);

    for LIndex2 := 0 to LRange.FOpenToken.SymbolCount - 1 do
    begin
      LTempToken := TBCEditorToken.Create(LRange.OpenToken, LIndex2);
      LToken := InsertTokenDefault(LTempToken, Self, LRange.Attribute);
      LToken.OpenRule := LRange;

      LTempToken := TBCEditorToken.Create(LRange.CloseToken, LIndex2);
      LToken.ClosingToken := InsertTokenDefault(LTempToken, LRange, LRange.Attribute);
    end;
    LRange.Prepare(Self);
  end;

  if Assigned(FKeyList) then
  for LIndex := 0 to FKeyList.Count - 1 do
  begin
    LKeyList := TBCEditorKeyList(FKeyList[LIndex]);

    for LIndex2 := 0 to LKeyList.KeyList.Count - 1 do
    begin
      LTempToken := TBCEditorToken.Create(LKeyList.Attribute);
      LTempToken.Symbol := LKeyList.KeyList[LIndex2];
      InsertToken(LTempToken, Self);
    end;
  end;

  if Assigned(FTokens) then
  for LIndex := 0 to FTokens.Count - 1 do
  begin
    LTempToken := TBCEditorToken(FTokens[LIndex]);
    LLength := Length(LTempToken.Symbol);
    if LLength < 1 then
      Continue;

    LSymbol := LTempToken.Symbol;
    LFirstChar := LSymbol[1];

    if CharInSet(LFirstChar, FDelimiters) then
      LBreakType := btAny
    else
    if LTempToken.BreakType <> btUnspecified then
      LBreakType := LTempToken.BreakType
    else
      LBreakType := btTerm;

    LChar := CaseFunct(LFirstChar);
    if Ord(LChar) < 256 then
    begin
      LAnsiChar := AnsiChar(LChar);
      if not Assigned(SymbolList[LAnsiChar]) then
      begin
        if LLength = 1 then
          FSymbolList[LAnsiChar] := TBCEditorParser.Create(LFirstChar, LTempToken, LBreakType)
        else
          FSymbolList[LAnsiChar] := TBCEditorParser.Create(LFirstChar, FDefaultToken, LBreakType);
      end;
      if CharInSet(LSymbol[LLength], FDelimiters) then
        LBreakType := btAny;
      if LLength <> 1 then
        TBCEditorParser(SymbolList[LAnsiChar]).AddTokenNode(StringCaseFunct(Copy(LSymbol, 2, LLength - 1)), LTempToken,
          LBreakType);
    end;
  end;

  if Assigned(FSets) then
    if FSets.Count > 0 then
    for LIndex := 0 to 255 do
    begin
      LAnsiChar := AnsiChar(CaseFunct(Char(LIndex)));
      for LIndex2 := 0 to FSets.Count - 1 do
      begin
        LSet := TBCEditorSet(FSets.List[LIndex2]);
        if CharInSet(LAnsiChar, LSet.CharSet) then
          if not Assigned(SymbolList[LAnsiChar]) then
            FSymbolList[LAnsiChar] := TBCEditorParser.Create(LSet)
          else
            TBCEditorParser(SymbolList[LAnsiChar]).AddSet(LSet);
      end;
    end;

  for LIndex := 0 to 255 do
  begin
    LAnsiChar := AnsiChar(LIndex);
    if not Assigned(SymbolList[LAnsiChar]) then
    begin
      if CharInSet(LAnsiChar, FDelimiters) then
        FSymbolList[LAnsiChar] := FDefaultTermSymbol
      else
        FSymbolList[LAnsiChar] := FDefaultSymbols;
    end;
  end;

  FPrepared := True;
end;

procedure TBCEditorRange.Reset;
var
  LIndex: Integer;
  LAnsiChar: AnsiChar;
begin
  if not FPrepared then
    Exit;

  for LIndex := 0 to 255 do
  begin
    LAnsiChar := AnsiChar(LIndex);
    if Assigned(SymbolList[LAnsiChar]) and (SymbolList[LAnsiChar] <> FDefaultTermSymbol) and (SymbolList[LAnsiChar] <> FDefaultSymbols) then
      FSymbolList[LAnsiChar].Free;
    FSymbolList[LAnsiChar] := nil;
  end;

  FDefaultToken.Free;
  FDefaultToken := nil;
  FDefaultTermSymbol.Free;
  FDefaultTermSymbol := nil;
  FDefaultSymbols.Free;
  FDefaultSymbols := nil;

  if Assigned(FRanges) then
  for LIndex := 0 to FRanges.Count - 1 do
    TBCEditorRange(FRanges[LIndex]).Reset;

  ClearList(FTokens);
  FPrepared := False;
end;

procedure TBCEditorRange.Clear;
var
  LIndex: Integer;
begin
  OpenToken.Clear;
  CloseToken.Clear;
  CloseOnTerm := False;
  CloseOnEndOfLine := False;
  CloseParent := False;
  Reset;

  if Assigned(FRanges) then
  for LIndex := 0 to FRanges.Count - 1 do
    TBCEditorRange(FRanges[LIndex]).Clear;

  ClearList(FRanges);
  ClearList(FTokens);
  ClearList(FKeyList);
  ClearList(FSets);
end;

constructor TBCEditorKeyList.Create;
begin
  inherited;

  FKeyList := TStringList.Create;
  FKeyList.Sorted := True;
  FAttribute.Foreground := clWindowText;
  FAttribute.Background := clWindow;
end;

destructor TBCEditorKeyList.Destroy;
begin
  FKeyList.Free;
  FKeyList := nil;

  inherited;
end;

constructor TBCEditorSet.Create(ACharSet: TBCEditorCharSet = []);
begin
  inherited Create;

  FCharSet := ACharSet;
  FAttribute.Foreground := clWindowText;
  FAttribute.Background := clWindow;
end;

end.
