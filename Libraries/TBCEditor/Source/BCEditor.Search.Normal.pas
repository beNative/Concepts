unit BCEditor.Search.Normal;

interface

uses
  System.Classes, System.SysUtils, BCEditor.Search, BCEditor.Lines;

type
  TBCEditorNormalSearch = class(TBCEditorSearchBase)
  strict private
    FCount: Integer;
    FLookAt: Integer;
    FOrigin: PChar;
    FPattern, FCasedPattern: string;
    FPatternLength, FPatternLengthSuccessor: Integer;
    FResults: TList;
    FRun: PChar;
    FShift: array [AnsiChar] of Integer;
    FShiftInitialized: Boolean;
    FTextLength: Integer;
    FTextToSearch: string;
    FTheEnd: PChar;
    function GetFinished: Boolean;
    procedure InitShiftTable;
  protected
    function GetLength(const AIndex: Integer): Integer; override;
    function GetPattern: string; override;
    function GetResult(const AIndex: Integer): Integer; override;
    function GetResultCount: Integer; override;
    function TestWholeWord: Boolean;
    procedure CaseSensitiveChanged; override;
    procedure SetPattern(const AValue: string); override;
  public
    constructor Create;
    destructor Destroy; override;

    function SearchAll(const ALines: TBCEditorLines): Integer; override;
    function FindFirst(const AText: string): Integer;
    function Next: Integer;
    procedure Clear; override;
    property Count: Integer read FCount write FCount;
    property Finished: Boolean read GetFinished;
    property Pattern read FCasedPattern;
  end;

implementation

uses
  System.Character, BCEditor.Consts, BCEditor.Language, Winapi.Windows;

constructor TBCEditorNormalSearch.Create;
begin
  inherited;
  FResults := TList.Create;
end;

function TBCEditorNormalSearch.GetFinished: Boolean;
begin
  Result := (FRun >= FTheEnd) or (FPatternLength >= FTextLength);
end;

function TBCEditorNormalSearch.GetResult(const AIndex: Integer): Integer;
begin
  Result := 0;
  if (AIndex >= 0) and (AIndex < FResults.Count) then
    Result := Integer(FResults[AIndex]);
end;

function TBCEditorNormalSearch.GetResultCount: Integer;
begin
  Result := FResults.Count;
end;

procedure TBCEditorNormalSearch.InitShiftTable;
var
  LAnsiChar: AnsiChar;
  LIndex: Integer;
begin
  FPatternLength := Length(FPattern);
  if FPatternLength = 0 then
    Status := SBCEditorPatternIsEmpty;
  FPatternLengthSuccessor := FPatternLength + 1;
  FLookAt := 1;
  for LAnsiChar := Low(AnsiChar) to High(AnsiChar) do
    FShift[LAnsiChar] := FPatternLengthSuccessor;
  for LIndex := 1 to FPatternLength do
    FShift[AnsiChar(FPattern[LIndex])] := FPatternLengthSuccessor - LIndex;
  while FLookAt < FPatternLength do
  begin
    if FPattern[FPatternLength] = FPattern[FPatternLength - FLookAt] then
      Break;
    Inc(FLookAt);
  end;
  FShiftInitialized := True;
end;

function TBCEditorNormalSearch.TestWholeWord: Boolean;
var
  LPTest: PChar;

  function IsWordBreakChar(AChar: Char): Boolean;
  begin
    if (AChar < BCEDITOR_EXCLAMATION_MARK) or AChar.IsWhiteSpace then
      Result := True
    else
    if AChar = BCEDITOR_LOW_LINE then
      Result := False
    else
      Result := not AChar.IsLetterOrDigit;
  end;

begin
  LPTest := FRun - FPatternLength;

  Result := ((LPTest < FOrigin) or IsWordBreakChar(LPTest[0])) and ((FRun >= FTheEnd) or IsWordBreakChar(FRun[1]));
end;

function TBCEditorNormalSearch.Next: Integer;
var
  LIndex: Integer;
  LPValue: PChar;
begin
  Result := 0;
  Inc(FRun, FPatternLength);
  while FRun < FTheEnd do
  begin
    if FPattern[FPatternLength] <> FRun^ then
      Inc(FRun, FShift[AnsiChar((FRun + 1)^)])
    else
    begin
      LPValue := FRun - FPatternLength + 1;
      LIndex := 1;
      while FPattern[LIndex] = LPValue^ do
      begin
        if LIndex = FPatternLength then
        begin
          if WholeWordsOnly then
            if not TestWholeWord then
              Break;
          Inc(FCount);
          Result := FRun - FOrigin - FPatternLength + 2;
          Exit;
        end;
        Inc(LIndex);
        Inc(LPValue);
      end;
      Inc(FRun, FLookAt);
      if FRun >= FTheEnd then
        Break;
      Inc(FRun, FShift[AnsiChar(FRun^)] - 1);
    end;
  end;
end;

destructor TBCEditorNormalSearch.Destroy;
begin
  FResults.Free;
  inherited Destroy;
end;

procedure TBCEditorNormalSearch.SetPattern(const AValue: string);
var
  LValue: string;
begin
  LValue := AValue;

  LValue := StringReplace(LValue, '\n', sLineBreak, [rfReplaceAll]);

  if FPattern <> LValue then
  begin
    FCasedPattern := LValue;
    if CaseSensitive then
      FPattern := FCasedPattern
    else
      FPattern := LowerCase(FCasedPattern);
    FShiftInitialized := False;
  end;
  FCount := 0;
end;

procedure TBCEditorNormalSearch.CaseSensitiveChanged;
begin
  if CaseSensitive then
    FPattern := FCasedPattern
  else
    FPattern := LowerCase(FCasedPattern);
  FShiftInitialized := False;
end;

procedure TBCEditorNormalSearch.Clear;
begin
  FResults.Count := 0;
end;

function TBCEditorNormalSearch.SearchAll(const ALines: TBCEditorLines): Integer;
var
  Found: Integer;
begin
  Status := '';
  Clear;
  Found := FindFirst(ALines.Text);
  while Found > 0 do
  begin
    FResults.Add(Pointer(Found));
    Found := Next;
  end;
  Result := FResults.Count;
  SetLength(FTextToSearch, 0);
end;

function TBCEditorNormalSearch.FindFirst(const AText: string): Integer;
begin
  if not FShiftInitialized then
    InitShiftTable;
  Result := 0;
  FTextLength := Length(AText);
  if FTextLength >= FPatternLength then
  begin
    FTextToSearch := AText;
    if not CaseSensitive then
      CharLowerBuff(PChar(FTextToSearch), FTextLength);
    FOrigin := PChar(FTextToSearch);
    FTheEnd := FOrigin + FTextLength;
    FRun := FOrigin - 1;
    Result := Next;
  end;
end;

function TBCEditorNormalSearch.GetLength(const AIndex: Integer): Integer;
begin
  Result := FPatternLength;
end;

function TBCEditorNormalSearch.GetPattern: string;
begin
  Result := FCasedPattern;
end;

end.
