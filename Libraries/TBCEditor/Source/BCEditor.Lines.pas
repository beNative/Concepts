unit BCEditor.Lines;

interface

uses
  System.SysUtils, Vcl.Graphics, BCEditor.Utils, System.Classes, BCEditor.Types;

type
  TBCEditorLinesRange = Pointer;

  TBCEditorStringFlag = (sfHasTabs, sfHasNoTabs, sfExpandedLengthUnknown);
  TBCEditorStringFlags = set of TBCEditorStringFlag;

  TBCEditorLineState = (lsNone, lsNormal, lsModified);

  TBCEditorLineAttribute = packed record
    Background: TColor;
    Foreground: TColor;
    LineState: TBCEditorLineState;
  end;
  PBCEditorLineAttribute = ^TBCEditorLineAttribute;

  TBCEditorStringRecord = packed record
    Attribute: PBCEditorLineAttribute;
    Flags: TBCEditorStringFlags;
    ExpandedLength: Integer;
    Range: TBCEditorLinesRange;
    Value: string;
  end;
  PBCEditorStringRecord = ^TBCEditorStringRecord;

const
  CSTRINGRECORDSIZE = SizeOf(TBCEditorStringRecord);
  CMAXSTRINGS = MaxInt div CSTRINGRECORDSIZE;
  CNULLRANGE = TBCEditorLinesRange(-1);

type
  PEditorStringRecordList = ^TBCEditorStringRecordList;
  TBCEditorStringRecordList = array [0 .. CMAXSTRINGS - 1] of TBCEditorStringRecord;

  TStringListChangeEvent = procedure(Sender: TObject; AIndex: Integer; ACount: Integer) of object;

  TBCEditorLines = class(TStrings)
  strict private
    FCapacity: Integer;
    FColumns: Boolean;
    FCount: Integer;
    FIndexOfLongestLine: Integer;
    FLengthOfLongestLine: Integer;
    FList: PEditorStringRecordList;
    FLongestLineNeedsUpdate: Boolean;
    FOnAfterSetText: TNotifyEvent;
    FOnBeforePutted: TStringListChangeEvent;
    FOnBeforeSetText: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOnCleared: TNotifyEvent;
    FOnDeleted: TStringListChangeEvent;
    FOnInserted: TStringListChangeEvent;
    FOnPutted: TStringListChangeEvent;
    FOwner: TObject;
    FStreaming: Boolean;
    FTabConvertProc: TBCEditorTabConvertProc;
    FTabWidth: Integer;
    FUpdateCount: Integer;
    function ExpandString(AIndex: Integer): string;
    function GetAttributes(AIndex: Integer): PBCEditorLineAttribute;
    function GetExpandedString(AIndex: Integer): string;
    function GetExpandedStringLength(AIndex: Integer): Integer;
    function GetRange(AIndex: Integer): TBCEditorLinesRange;
    procedure Grow;
    procedure PutAttributes(AIndex: Integer; const AValue: PBCEditorLineAttribute);
    procedure PutRange(AIndex: Integer; ARange: TBCEditorLinesRange);
  protected
    function Get(AIndex: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetTextStr: string; override;
    procedure Put(AIndex: Integer; const AValue: string); override;
    procedure SetCapacity(AValue: Integer); override;
    procedure SetColumns(AValue: Boolean);
    procedure SetTabWidth(AValue: Integer);
    procedure SetTextStr(const AValue: string); override;
    procedure SetUpdateState(AUpdating: Boolean); override;
    procedure InsertItem(AIndex: Integer; const AValue: string);
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    function StringLength(AIndex: Integer): Integer;
    function Add(const AValue: string): Integer; override;
    function GetLengthOfLongestLine: Integer; overload;
    function GetLineText(ALine: Integer): string;
    procedure Clear; override;
    procedure Delete(AIndex: Integer); override;
    procedure DeleteLines(const AIndex: Integer; ACount: Integer);
    procedure Insert(AIndex: Integer; const AValue: string); override;
    procedure InsertLines(AIndex, ACount: Integer; AStrings: TStrings = nil);
    procedure InsertStrings(AIndex: Integer; AStrings: TStrings);
    procedure InsertText(AIndex: Integer; const AText: string);
    procedure LoadFromStream(AStream: TStream; AEncoding: TEncoding = nil); override;
    procedure SaveToStream(AStream: TStream; AEncoding: TEncoding = nil); override;
    procedure TrimTrailingSpaces(AIndex: Integer);
    property Attributes[AIndex: Integer]: PBCEditorLineAttribute read GetAttributes write PutAttributes;
    property Columns: Boolean read FColumns write SetColumns;
    property Count: Integer read FCount;
    property ExpandedStrings[AIndex: Integer]: string read GetExpandedString;
    property ExpandedStringLengths[AIndex: Integer]: Integer read GetExpandedStringLength;
    property List: PEditorStringRecordList read FList;
    property OnAfterSetText: TNotifyEvent read FOnAfterSetText write FOnAfterSetText;
    property OnBeforePutted: TStringListChangeEvent read FOnBeforePutted write FOnBeforePutted;
    property OnBeforeSetText: TNotifyEvent read FOnBeforeSetText write FOnBeforeSetText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnCleared: TNotifyEvent read FOnCleared write FOnCleared;
    property OnDeleted: TStringListChangeEvent read FOnDeleted write FOnDeleted;
    property OnInserted: TStringListChangeEvent read FOnInserted write FOnInserted;
    property OnPutted: TStringListChangeEvent read FOnPutted write FOnPutted;
    property Owner: TObject read FOwner write FOwner;
    property Ranges[AIndex: Integer]: TBCEditorLinesRange read GetRange write PutRange;
    property Strings[AIndex: Integer]: string read Get write Put; default;
    property Streaming: Boolean read FStreaming;
    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property Text: string read GetTextStr write SetTextStr;
  end;

  EBCEditorLinesException = class(Exception);

implementation

uses
  BCEditor.Consts, BCEditor.Language;

{ TBCEditorLines }

procedure ListIndexOutOfBounds(AIndex: Integer);
begin
  raise EBCEditorLinesException.CreateFmt(SBCEditorListIndexOutOfBounds, [AIndex]);
end;

constructor TBCEditorLines.Create;
begin
  inherited Create;

  FCount := 0;
  FOwner := AOwner;
  FUpdateCount := 0;
  FIndexOfLongestLine := -1;
  FLengthOfLongestLine := 0;
  FLongestLineNeedsUpdate := False;
  TabWidth := 4;
  Add(EmptyStr);
end;

destructor TBCEditorLines.Destroy;
var
  i: Integer;
begin
  FOnChange := nil;
  FOnChanging := nil;
  if FCount > 0 then
  begin
    for i := 0 to FCount - 1 do
      Dispose(FList^[i].Attribute);
    Finalize(FList^[0], FCount);
  end;
  FCount := 0;
  SetCapacity(0);

  inherited;
end;

function TBCEditorLines.Add(const AValue: string): Integer;
begin
  Result := FCount;
  InsertItem(Result, AValue);
  if Assigned(OnInserted) and (FUpdateCount = 0) then
    OnInserted(Self, Result, 1);
end;

function TBCEditorLines.GetLengthOfLongestLine: Integer;
var
  i, LMaxLength: Integer;
  PStringRecord: PBCEditorStringRecord;
begin
  if FIndexOfLongestLine < 0 then
  begin
    LMaxLength := 0;
    if FCount > 0 then
    begin
      PStringRecord := @FList^[0];
      for i := 0 to FCount - 1 do
      begin
        if sfExpandedLengthUnknown in PStringRecord^.Flags then
          ExpandString(i);
        if PStringRecord^.ExpandedLength > LMaxLength then
        begin
          LMaxLength := PStringRecord^.ExpandedLength;
          FIndexOfLongestLine := i;
        end;
        Inc(PStringRecord);
      end;
    end;
  end;
  if (fIndexOfLongestLine >= 0) and (FIndexOfLongestLine < FCount) then
    Result := FList^[FIndexOfLongestLine].ExpandedLength
  else
    Result := 0;
end;

function TBCEditorLines.StringLength(AIndex: Integer): Integer;
begin
  Result := 0;
  if (AIndex < 0) or (AIndex > FCount - 1) then
    Exit;
  Result := Length(FList^[AIndex].Value);
end;

procedure TBCEditorLines.Clear;
var
  i: Integer;
begin
  if FCount <> 0 then
  begin
    for i := 0 to FCount - 1 do
      Dispose(FList^[i].Attribute);
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    if Assigned(FOnCleared) then
      FOnCleared(Self);
  end;
  { Clear information about longest line }
  FIndexOfLongestLine := -1;
  FLengthOfLongestLine := 0;
end;

procedure TBCEditorLines.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex > FCount) then
    ListIndexOutOfBounds(AIndex);
  BeginUpdate;
  try
    Dispose(FList^[AIndex].Attribute);
    Finalize(FList^[AIndex]);
    Dec(FCount);
    if AIndex < FCount then
      System.Move(FList[AIndex + 1], FList[AIndex], (FCount - AIndex) * CSTRINGRECORDSIZE);
  finally
    EndUpdate;
  end;
  FIndexOfLongestLine := -1;
  if Assigned(FOnDeleted) then
    FOnDeleted(Self, AIndex, 1);
end;

procedure TBCEditorLines.DeleteLines(const AIndex: Integer; ACount: Integer);
var
  i, LLinesAfter: Integer;
begin
  if ACount > 0 then
  begin
    if (AIndex < 0) or (AIndex > FCount) then
      ListIndexOutOfBounds(AIndex);
    LLinesAfter := FCount - (AIndex + ACount);
    if LLinesAfter < 0 then
      ACount := FCount - AIndex - 1;
    for i := AIndex to AIndex + ACount - 1 do
      Dispose(FList^[i].Attribute);
    Finalize(FList^[AIndex], ACount);
    if LLinesAfter > 0 then
    begin
      BeginUpdate;
      try
        System.Move(FList[AIndex + ACount], FList[AIndex], LLinesAfter * CSTRINGRECORDSIZE);
      finally
        EndUpdate;
      end;
    end;
    Dec(FCount, ACount);

    FIndexOfLongestLine := -1;
    if Assigned(FOnDeleted) then
      FOnDeleted(Self, AIndex, ACount);
  end;
end;

function TBCEditorLines.GetAttributes(AIndex: Integer): PBCEditorLineAttribute;
begin
  if (AIndex >= 0) and (AIndex < FCount) then
    Result := FList^[AIndex].Attribute
  else
    Result := nil;
end;

procedure TBCEditorLines.PutAttributes(AIndex: Integer; const AValue: PBCEditorLineAttribute);
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    ListIndexOutOfBounds(AIndex);
  BeginUpdate;
  FList^[AIndex].Attribute := AValue;
  EndUpdate;
end;

function TBCEditorLines.ExpandString(AIndex: Integer): string;
var
  LHasTabs: Boolean;
begin
  with FList^[AIndex] do
  begin
    if Value = '' then
    begin
      Result := '';
      Exclude(Flags, sfExpandedLengthUnknown);
      Exclude(Flags, sfHasTabs);
      Include(Flags, sfHasNoTabs);
      ExpandedLength := 0;
    end
    else
    begin
      Result := FTabConvertProc(Value, FTabWidth, LHasTabs);

      ExpandedLength := Length(Result);
      Exclude(Flags, sfExpandedLengthUnknown);
      Exclude(Flags, sfHasTabs);
      Exclude(Flags, sfHasNoTabs);
      if LHasTabs then
        Include(Flags, sfHasTabs)
      else
        Include(Flags, sfHasNoTabs);
    end;
  end;
end;

function TBCEditorLines.Get(AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex < FCount) then
    Result := FList^[AIndex].Value
  else
    Result := '';
end;

function TBCEditorLines.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TBCEditorLines.GetCount: Integer;
begin
  Result := FCount;
end;

function TBCEditorLines.GetExpandedString(AIndex: Integer): string;
begin
  Result := '';
  if (AIndex >= 0) and (AIndex < FCount) then
  begin
    if sfHasNoTabs in FList^[AIndex].Flags then
      Result := Get(AIndex)
    else
      Result := ExpandString(AIndex);
  end
end;

function TBCEditorLines.GetExpandedStringLength(AIndex: Integer): Integer;
begin
  if (AIndex >= 0) and (AIndex < FCount) then
  begin
    if sfExpandedLengthUnknown in FList^[AIndex].Flags then
      Result := Length(ExpandedStrings[AIndex])
    else
      Result := FList^[AIndex].ExpandedLength;
  end
  else
    Result := 0;
end;

function TBCEditorLines.GetRange(AIndex: Integer): TBCEditorLinesRange;
begin
  if (AIndex >= 0) and (AIndex < FCount) then
    Result := FList^[AIndex].Range
  else
    Result := nil;
end;

function TBCEditorLines.GetTextStr: string;
var
  i, LLength, LSize, LCount: Integer;
  LPValue: PChar;
  LValue, LLineBreak: string;
begin
  LCount := GetCount;
  LSize := 0;
  LLineBreak := SLineBreak;
  for i := 0 to LCount - 1 do
    Inc(LSize, Length(Get(i)) + Length(LLineBreak));
  SetString(Result, nil, LSize);
  LPValue := Pointer(Result);
  for i := 0 to LCount - 1 do
  begin
    LValue := Get(i);
    LLength := Length(LValue);
    if LLength <> 0 then
    begin
      System.Move(Pointer(LValue)^, LPValue^, LLength * SizeOf(Char));
      Inc(LPValue, LLength);
    end;
    LLength := Length(LLineBreak);
    if LLength <> 0 then
    begin
      System.Move(Pointer(LLineBreak)^, LPValue^, LLength * SizeOf(Char));
      Inc(LPValue, LLength);
    end;
  end;
end;

procedure TBCEditorLines.Grow;
var
  LDelta: Integer;
begin
  if FCapacity > 64 then
    LDelta := FCapacity div 4
  else
    LDelta := 16;
  SetCapacity(FCapacity + LDelta);
end;

procedure TBCEditorLines.Insert(AIndex: Integer; const AValue: string);
begin
  if (AIndex < 0) or (AIndex > FCount) then
    ListIndexOutOfBounds(AIndex);
  BeginUpdate;
  InsertItem(AIndex, AValue);
  if Assigned(FOnInserted) then
    FOnInserted(Self, AIndex, 1);
  EndUpdate;
end;

procedure TBCEditorLines.InsertItem(AIndex: Integer; const AValue: string);
begin
  if FCount = FCapacity then
    Grow;

  if AIndex < FCount then
    System.Move(FList^[AIndex], FList^[AIndex + 1], (FCount - AIndex) * CSTRINGRECORDSIZE);
  FIndexOfLongestLine := -1;
  with FList^[AIndex] do
  begin
    Pointer(Value) := nil;
    Value := AValue;
    Range := CNULLRANGE;
    ExpandedLength := -1;
    Flags := [sfExpandedLengthUnknown];
    New(Attribute);
    Attribute^.Foreground := clNone;
    Attribute^.Background := clNone;
    Attribute^.LineState := lsNone;
  end;
  Inc(FCount);
end;

procedure TBCEditorLines.InsertLines(AIndex, ACount: Integer; AStrings: TStrings = nil);
var
  i: Integer;
  LLine: Integer;
begin
  if (AIndex < 0) or (AIndex > FCount) then
    ListIndexOutOfBounds(AIndex);
  if ACount > 0 then
  begin
    BeginUpdate;
    try
      SetCapacity(FCount + ACount);
      if AIndex < FCount then
        System.Move(FList^[AIndex], FList^[AIndex + ACount], (FCount - AIndex) * CSTRINGRECORDSIZE);
      i := 0;
      for LLine := AIndex to AIndex + ACount - 1 do
      with FList^[LLine] do
      begin
        Pointer(Value) := nil;
        if Assigned(AStrings) then
          Value := AStrings[i];
        Inc(i);
        Range := CNULLRANGE;
        ExpandedLength := -1;
        Flags := [sfExpandedLengthUnknown];
        New(Attribute);
        Attribute^.Foreground := clNone;
        Attribute^.Background := clNone;
        Attribute^.LineState := lsModified;
      end;
      Inc(FCount, ACount);
    finally
      EndUpdate;
    end;

    if Assigned(OnInserted) then
      OnInserted(Self, AIndex, ACount);
  end;
end;

procedure TBCEditorLines.InsertStrings(AIndex: Integer; AStrings: TStrings);
var
  LCount: Integer;
begin
  LCount := AStrings.Count;
  if LCount = 0 then
    Exit;

  BeginUpdate;
  try
    InsertLines(AIndex, LCount, AStrings);
  finally
    EndUpdate;
  end;
end;

procedure TBCEditorLines.InsertText(AIndex: Integer; const AText: string);
var
  LStringList: TStringList;
begin
  if AText = '' then
    Exit;

  LStringList := TStringList.Create;
  try
    LStringList.Text := AText;
    InsertStrings(AIndex, LStringList);
  finally
    LStringList.Free;
  end;
end;

procedure TBCEditorLines.LoadFromStream(AStream: TStream; AEncoding: TEncoding = nil);
var
  LSize: Integer;
  LBuffer: TBytes;
  LStrBuffer: string;
begin
  FStreaming := True;

  BeginUpdate;
  try
    LSize := AStream.Size - AStream.Position;
    if Assigned(AEncoding) then
    begin
      SetLength(LBuffer, LSize);
      AStream.Read(LBuffer[0], LSize);
      LSize := TEncoding.GetBufferEncoding(LBuffer, AEncoding);
      LStrBuffer := AEncoding.GetString(LBuffer, LSize, Length(LBuffer) - LSize);
    end
    else
    begin
      SetLength(LStrBuffer, LSize shr 1);
      AStream.ReadBuffer(LStrBuffer[1], LSize);
    end;
    SetTextStr(LStrBuffer);
  finally
    EndUpdate;
  end;

  if Assigned(OnInserted) then
    OnInserted(Self, 0, FCount);

  FStreaming := False;
end;

procedure TBCEditorLines.SaveToStream(AStream: TStream; AEncoding: TEncoding);
var
  LBuffer, LPreamble: TBytes;
begin
  FStreaming := True;

  if AEncoding = nil then
    AEncoding := TEncoding.Default;
  LBuffer := AEncoding.GetBytes(GetTextStr);
  LPreamble := AEncoding.GetPreamble;
  if Length(LPreamble) > 0 then
    AStream.WriteBuffer(LPreamble[0], Length(LPreamble));
  AStream.WriteBuffer(LBuffer[0], Length(LBuffer));

  FStreaming := False;
end;

procedure TBCEditorLines.Put(AIndex: Integer; const AValue: string);
begin
  if ((AIndex = 0) and (FCount = 0)) or (FCount = AIndex) then
  begin
    Add(AValue);
    FList^[AIndex].Attribute^.LineState := lsModified;
  end
  else
  begin
    if (AIndex < 0) or (AIndex >= FCount) then
      ListIndexOutOfBounds(AIndex);
    if Assigned(OnBeforePutted) then
      OnBeforePutted(Self, AIndex, 1);
    with FList^[AIndex] do
    begin
      Include(Flags, sfExpandedLengthUnknown);
      Exclude(Flags, sfHasTabs);
      Exclude(Flags, sfHasNoTabs);
      Value := AValue;
      Attribute^.LineState := lsModified;
    end;

    if Assigned(FOnPutted) then
      FOnPutted(Self, AIndex, 1);
  end;
end;

procedure TBCEditorLines.TrimTrailingSpaces(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    ListIndexOutOfBounds(AIndex);
  FList^[AIndex].Value := TrimRight(FList^[AIndex].Value);
end;

procedure TBCEditorLines.PutRange(AIndex: Integer; ARange: TBCEditorLinesRange);
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    ListIndexOutOfBounds(AIndex);
  FList^[AIndex].Range := ARange;
end;

procedure TBCEditorLines.SetCapacity(AValue: Integer);
begin
  if AValue < Count then
    EListError.Create(SBCEditorInvalidCapacity);
  if AValue <> FCapacity then
  begin
    ReallocMem(FList, AValue * CSTRINGRECORDSIZE);
    FCapacity := AValue;
  end;
end;

procedure TBCEditorLines.SetTabWidth(AValue: Integer);
var
  i: Integer;
begin
  if FTabWidth <> AValue then
  begin
    FTabWidth := AValue;
    FIndexOfLongestLine := -1;
    for i := 0 to FCount - 1 do
      with FList^[i] do
      begin
        ExpandedLength := -1;
        Exclude(Flags, sfHasNoTabs);
        Include(Flags, sfExpandedLengthUnknown);
      end;
  end;
end;

procedure TBCEditorLines.SetColumns(AValue: Boolean);
begin
  FTabConvertProc := GetTabConvertProc(AValue);
  FColumns := AValue;
end;

procedure TBCEditorLines.SetTextStr(const AValue: string);
var
  LValue: string;
  LLength: Integer;
  LPValue, LPStartValue, LPLastChar: PChar;
begin
  if Assigned(FOnBeforeSetText) then
    FOnBeforeSetText(Self);
  Clear;
  LPValue := Pointer(AValue);
  if Assigned(LPValue) then
  begin
    LLength := Length(AValue);
    LPLastChar := @AValue[LLength];
    while LPValue <= LPLastChar do
    begin
      LPStartValue := LPValue;
      while (LPValue^ <> BCEDITOR_CARRIAGE_RETURN) and (LPValue^ <> BCEDITOR_LINEFEED) and (LPValue^ <> WideChar($2028))
        and (LPValue <= LPLastChar) do
        Inc(LPValue);
      if LPValue <> LPStartValue then
      begin
        SetString(LValue, LPStartValue, LPValue - LPStartValue);
        InsertItem(FCount, LValue);
      end
      else
        InsertItem(FCount, '');
      if LPValue^ = BCEDITOR_CARRIAGE_RETURN then
        Inc(LPValue);
      if LPValue^ = BCEDITOR_LINEFEED then
        Inc(LPValue);
      if LPValue^ = WideChar($2028) then
        Inc(LPValue);
    end;
  end;

  if (FUpdateCount = 0) and Assigned(FOnInserted) then
    FOnInserted(Self, 0, FCount);
  if Assigned(FOnChange) then
    FOnChange(Self);
  if Assigned(FOnAfterSetText) then
    FOnAfterSetText(Self);
end;

procedure TBCEditorLines.SetUpdateState(AUpdating: Boolean);
begin
  if AUpdating then
  begin
    if Assigned(FOnChanging) then
      FOnChanging(Self);
  end
  else
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TBCEditorLines.GetLineText(ALine: Integer): string;
begin
  if (ALine >= 0) and (ALine < Count) then
    Result := Get(ALine)
  else
    Result := '';
end;

end.
