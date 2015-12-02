unit BCEditor.Highlighter.Rules;

interface

uses
  Vcl.Graphics, System.Classes, System.SysUtils, BCEditor.Highlighter.Token, BCEditor.Highlighter.Attributes,
  BCEditor.Types;

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
    procedure AddTokenNode(AString: string; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType);
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

  TBCEditorCaseFunction = function(AChar: Char): Char;
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
    function GetKeyList(AIndex: Integer): TBCEditorKeyList;
    function GetKeyListCount: Integer;
    function GetRange(AIndex: Integer): TBCEditorRange;
    function GetRangeCount: Integer;
    function GetSet(AIndex: Integer): TBCEditorSet;
    function GetSetCount: Integer;
    function GetToken(AIndex: Integer): TBCEditorToken;
    procedure SetAlternativeCloseArrayCount(const AValue: Integer);
    procedure SetCaseSensitive(const AValue: Boolean);
  public
    constructor Create(AOpenToken: string = ''; ACloseToken: string = ''); virtual;
    destructor Destroy; override;

    function FindToken(AString: string): TBCEditorToken;
    procedure AddKeyList(NewKeyList: TBCEditorKeyList);
    procedure AddRange(NewRange: TBCEditorRange);
    procedure AddSet(NewSet: TBCEditorSet);
    procedure AddToken(AToken: TBCEditorToken);
    procedure AddTokenRange(AOpenToken: string; AOpenTokenBreakType: TBCEditorBreakType; ACloseToken: string;
      ACloseTokenBreakType: TBCEditorBreakType);
    procedure Clear;
    procedure Prepare(AParent: TBCEditorRange);
    procedure Reset;
    procedure SetDelimiters(ADelimiters: TBCEditorCharSet);
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
    property KeyList[AIndex: Integer]: TBCEditorKeyList read GetKeyList;
    property OpenToken: TBCEditorMultiToken read FOpenToken write FOpenToken;
    property Prepared: Boolean read FPrepared;
    property RangeCount: Integer read GetRangeCount;
    property Ranges[AIndex: Integer]: TBCEditorRange read GetRange;
    property SetCount: Integer read GetSetCount;
    property Sets[AIndex: Integer]: TBCEditorSet read GetSet;
    property StringCaseFunct: TBCEditorStringCaseFunction read FStringCaseFunct;
    property SymbolList: TBCEditorAbstractParserArray read FSymbolList;
    property Tokens[AIndex: Integer]: TBCEditorToken read GetToken;
  end;

implementation

uses
  BCEditor.Utils, BCEditor.Consts, System.Types;

function CaseNone(AChar: Char): Char;
begin
  Result := AChar;
end;

function StringCaseNone(const AString: string): string;
begin
  Result := AString;
end;

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

procedure TBCEditorParser.AddTokenNode(AString: string; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType);
var
  i: Integer;
  LLength: Integer;
  TokenNode: TBCEditorTokenNode;
  TokenNodeList: TBCEditorTokenNodeList;
begin
  TokenNodeList := HeadNode.NextNodes;
  TokenNode := nil;
  LLength := Length(AString);
  for i := 1 to LLength do
  begin
    TokenNode := TokenNodeList.FindNode(AString[i]);
    if not Assigned(TokenNode) then
    begin
      TokenNode := TBCEditorTokenNode.Create(AString[i]);
      TokenNodeList.AddNode(TokenNode);
    end;
    TokenNodeList := TokenNode.NextNodes;
  end;
  TokenNode.BreakType := ABreakType;
  TokenNode.Token := AToken;
end;

procedure TBCEditorParser.AddSet(ASet: TBCEditorSet);
begin
  Sets.Add(ASet);
end;

function TBCEditorParser.GetToken(ACurrentRange: TBCEditorRange; APLine: PChar; var ARun: Integer;
  var AToken: TBCEditorToken): Boolean;
var
  CurrentTokenNode, StartTokenNode, FindTokenNode: TBCEditorTokenNode;
  i, StartPosition, NextPosition, PreviousPosition: Integer;
  AllowedDelimiters: TBCEditorCharSet;
begin
  Result := False;
  StartPosition := ARun;
  if Assigned(HeadNode) then
  begin
    CurrentTokenNode := HeadNode;
    NextPosition := StartPosition;
    StartTokenNode := nil;
    repeat
      if Assigned(StartTokenNode) then
      begin
        CurrentTokenNode := StartTokenNode;
        ARun := NextPosition;
        StartTokenNode := nil;
      end;
      if Assigned(CurrentTokenNode.Token) then
        FindTokenNode := CurrentTokenNode
      else
        FindTokenNode := nil;
      PreviousPosition := ARun;
      while (CurrentTokenNode.NextNodes.Count > 0) and (APLine[ARun] <> BCEDITOR_NONE_CHAR) do
      begin
        Inc(ARun);
        CurrentTokenNode := CurrentTokenNode.NextNodes.FindNode(ACurrentRange.CaseFunct(APLine[ARun]));
        if not Assigned(CurrentTokenNode) then
        begin
          Dec(ARun);
          Break;
        end;

        if Assigned(CurrentTokenNode.Token) then
        begin
          FindTokenNode := CurrentTokenNode;
          PreviousPosition := ARun;
        end;

        if not Assigned(StartTokenNode) then
          if CharInSet(CurrentTokenNode.Char, ACurrentRange.Delimiters) then
          begin
            StartTokenNode := CurrentTokenNode;
            NextPosition := ARun;
          end;
      end;

      ARun := PreviousPosition;

      if not Assigned(FindTokenNode) then
        Continue;
      if not Assigned(FindTokenNode.Token) then
        Continue;

      if APLine[ARun] <> BCEDITOR_NONE_CHAR then
        Inc(ARun);

      if FindTokenNode.BreakType = btAny then
      begin
        Result := True;
        AToken := FindTokenNode.Token;
        Exit;
      end;

      if CharInSet(APLine[ARun], ACurrentRange.Delimiters) then
      begin
        Result := True;
        AToken := FindTokenNode.Token;
        Exit;
      end;
    until not Assigned(StartTokenNode);
  end;
  ARun := StartPosition;

  AllowedDelimiters := ACurrentRange.Delimiters;
  for i := 0 to Sets.Count - 1 do
    AllowedDelimiters := AllowedDelimiters - TBCEditorSet(Sets[i]).CharSet;

  for i := 0 to Sets.Count - 1 do
  begin
    ARun := StartPosition;
    repeat
      Inc(ARun);
    until not CharInSet(APLine[ARun], TBCEditorSet(Sets[i]).CharSet) or (APLine[ARun] = BCEDITOR_NONE_CHAR);

    if CharInSet(APLine[ARun], AllowedDelimiters) then
    begin
      Result := True;
      AToken := TBCEditorToken.Create(TBCEditorSet(Sets[i]).Attribute);
      AToken.Temporary := True;
      Exit;
    end;
  end;
  ARun := StartPosition + 1;
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
  while APLine[ARun] = BCEDITOR_FILLER_CHAR do
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
  while APLine[ARun] = BCEDITOR_FILLER_CHAR do
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

constructor TBCEditorRange.Create(AOpenToken: string; ACloseToken: string);
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

procedure TBCEditorRange.AddToken(AToken: TBCEditorToken);
var
  Token: TBCEditorToken;
begin
  Token := FindToken(AToken.Symbol);
  if not Assigned(Token) then
    FTokens.Add(AToken);
end;

function TBCEditorRange.FindToken(AString: string): TBCEditorToken;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FTokens.Count - 1 do
    if TBCEditorToken(FTokens.Items[i]).Symbol = AString then
    begin
      Result := TBCEditorToken(FTokens.Items[i]);
      Break;
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

function TBCEditorRange.GetToken(AIndex: Integer): TBCEditorToken;
begin
  Result := TBCEditorToken(FTokens[AIndex]);
end;

function TBCEditorRange.GetRange(AIndex: Integer): TBCEditorRange;
begin
  Result := TBCEditorRange(FRanges[AIndex]);
end;

function TBCEditorRange.GetKeyList(AIndex: Integer): TBCEditorKeyList;
begin
  Result := TBCEditorKeyList(FKeyList[AIndex]);
end;

function TBCEditorRange.GetSet(AIndex: Integer): TBCEditorSet;
begin
  Result := TBCEditorSet(FSets[AIndex]);
end;

procedure TBCEditorRange.AddTokenRange(AOpenToken: string; AOpenTokenBreakType: TBCEditorBreakType; ACloseToken: string;
  ACloseTokenBreakType: TBCEditorBreakType);
begin
  FOpenToken.AddSymbol(AOpenToken);
  FOpenToken.BreakType := AOpenTokenBreakType;
  FCloseToken.AddSymbol(ACloseToken);
  FCloseToken.BreakType := ACloseTokenBreakType;
end;

procedure TBCEditorRange.SetDelimiters(ADelimiters: TBCEditorCharSet);
var
  i: Integer;
begin
  Delimiters := ADelimiters;
  for i := 0 to RangeCount - 1 do
    Ranges[i].SetDelimiters(ADelimiters);
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
    FCaseFunct := UpCase;
    FStringCaseFunct := AnsiUpperCase;
  end
  else
  begin
    FCaseFunct := CaseNone;
    FStringCaseFunct := StringCaseNone;
  end;
end;

procedure QuickSortTokenList(AList: TList; const ALowerPosition, AUpperPosition: Integer);
var
  i, LMiddlePosition: Integer;
  LPivotValue: string;
begin
  if ALowerPosition < AUpperPosition then
  begin
    LPivotValue := TBCEditorToken(AList[ALowerPosition]).Symbol;
    LMiddlePosition := ALowerPosition;

    for i := ALowerPosition + 1 to AUpperPosition do
    begin
      if TBCEditorToken(AList[i]).Symbol < LPivotValue then
      begin
        Inc(LMiddlePosition);
        AList.Exchange(i, LMiddlePosition);
      end;
    end;
    AList.Exchange(ALowerPosition, LMiddlePosition);

    QuickSortTokenList(AList, ALowerPosition, LMiddlePosition - 1);
    QuickSortTokenList(AList, LMiddlePosition + 1, AUpperPosition);
  end;
end;

procedure TBCEditorRange.Prepare(AParent: TBCEditorRange);
var
  i, j, LLength: Integer;
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
  for i := 0 to FRanges.Count - 1 do
  begin
    LRange := TBCEditorRange(FRanges[i]);

    for j := 0 to LRange.FOpenToken.SymbolCount - 1 do
    begin
      LTempToken := TBCEditorToken.Create(LRange.OpenToken, j);
      LToken := InsertTokenDefault(LTempToken, Self, LRange.Attribute);
      LToken.OpenRule := LRange;

      LTempToken := TBCEditorToken.Create(LRange.CloseToken, j);
      LToken.ClosingToken := InsertTokenDefault(LTempToken, LRange, LRange.Attribute);
    end;
    LRange.Prepare(Self);
  end;

  if Assigned(FKeyList) then
  for i := 0 to FKeyList.Count - 1 do
  begin
    LKeyList := TBCEditorKeyList(FKeyList[i]);

    for j := 0 to LKeyList.KeyList.Count - 1 do
    begin
      LTempToken := TBCEditorToken.Create(LKeyList.Attribute);
      LTempToken.Symbol := LKeyList.KeyList[j];
      InsertToken(LTempToken, Self);
    end;
  end;

  QuickSortTokenList(FTokens, 0, FTokens.Count - 1);

  if Assigned(FTokens) then
  for i := 0 to FTokens.Count - 1 do
  begin
    LTempToken := TBCEditorToken(FTokens[i]);
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
    for i := 0 to 255 do
    begin
      LAnsiChar := AnsiChar(CaseFunct(Char(i)));
      for j := 0 to FSets.Count - 1 do
      begin
        if CharInSet(LAnsiChar, TBCEditorSet(FSets[j]).CharSet) then
          if not Assigned(SymbolList[LAnsiChar]) then
            FSymbolList[LAnsiChar] := TBCEditorParser.Create(TBCEditorSet(FSets[j]))
          else
            TBCEditorParser(SymbolList[LAnsiChar]).AddSet(TBCEditorSet(FSets[j]));
      end;
    end;

  for i := 0 to 255 do
  begin
    LAnsiChar := AnsiChar(i);
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
  i: Integer;
  LAnsiChar: AnsiChar;
begin
  if not FPrepared then
    Exit;
  for i := 0 to 255 do
  begin
    LAnsiChar := AnsiChar(i);
    if Assigned(SymbolList[LAnsiChar]) and (SymbolList[LAnsiChar] <> FDefaultTermSymbol) and (SymbolList[LAnsiChar] <> FDefaultSymbols) then
    begin
      FSymbolList[LAnsiChar].Free;
      FSymbolList[LAnsiChar] := nil;
    end
    else
      FSymbolList[LAnsiChar] := nil;
  end;
  FDefaultToken.Free;
  FDefaultToken := nil;
  FDefaultTermSymbol.Free;
  FDefaultTermSymbol := nil;
  FDefaultSymbols.Free;
  FDefaultSymbols := nil;
  if Assigned(FRanges) then
  for i := 0 to FRanges.Count - 1 do
    TBCEditorRange(FRanges[i]).Reset;
  ClearList(FTokens);
  FPrepared := False;
end;

procedure TBCEditorRange.Clear;
var
  i: Integer;
begin
  OpenToken.Clear;
  CloseToken.Clear;
  CloseOnTerm := False;
  CloseOnEndOfLine := False;
  CloseParent := False;
  Reset;
  if Assigned(FRanges) then
  for i := 0 to FRanges.Count - 1 do
    TBCEditorRange(FRanges[i]).Clear;
  ClearList(FRanges);
  ClearList(FTokens);
  ClearList(FKeyList);
  ClearList(FSets);
end;

constructor TBCEditorKeyList.Create;
begin
  inherited;

  FKeyList := TStringList.Create;
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
