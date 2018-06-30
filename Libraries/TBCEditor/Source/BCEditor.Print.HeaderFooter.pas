unit BCEditor.Print.HeaderFooter;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, Vcl.Graphics, BCEditor.Print.Types, BCEditor.Print.Margins,
  BCEditor.Utils;

type
  TBCEditorSectionItem = class
  strict private
    FAlignment: TAlignment;
    FFont: TFont;
    FIndex: Integer;
    FLineNumber: Integer;
    FText: string;
    procedure SetFont(const AValue: TFont);
  public
    constructor Create;
    destructor Destroy; override;

    function GetText(ANumberOfPages, APageNumber: Integer; ARoman: Boolean; const ATitle, ATime, ADate: string): string;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  public
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Font: TFont read FFont write SetFont;
    property Index: Integer read FIndex write FIndex;
    property LineNumber: Integer read FLineNumber write FLineNumber;
    property Text: string read FText write FText;
  end;

  TBCEditorSectionType = (stHeader, stFooter);

  TBCEditorLineInfo = class
  public
    LineHeight: Integer;
    MaxBaseDistance: Integer;
  end;

  TBCEditorSection = class(TPersistent)
  strict private
    FDate: string;
    FDefaultFont: TFont;
    FFrameHeight: Integer;
    FFrameTypes: TBCEditorFrameTypes;
    FItems: TList;
    FLineColor: TColor;
    FLineCount: Integer;
    FLineInfo: TList;
    FMargins: TBCEditorPrintMargins;
    FMirrorPosition: Boolean;
    FNumberOfPages: Integer;
    FOldBrush: TBrush;
    FOldFont: TFont;
    FOldPen: TPen;
    FRomanNumbers: Boolean;
    FShadedColor: TColor;
    FTime: string;
    FTitle: string;
    FSectionType: TBCEditorSectionType;
    procedure CalculateHeight(ACanvas: TCanvas);
    procedure DrawFrame(ACanvas: TCanvas);
    procedure RestoreFontPenBrush(ACanvas: TCanvas);
    procedure SaveFontPenBrush(ACanvas: TCanvas);
    procedure SetDefaultFont(const AValue: TFont);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const AText: string; const AFont: TFont; const AAlignment: TAlignment; const ALineNumber: Integer): Integer;
    function Count: Integer;
    function Get(AIndex: Integer): TBCEditorSectionItem;
    procedure Assign(ASource: TPersistent); override;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure FixLines;
    procedure InitPrint(ACanvas: TCanvas; NumberOfPages: Integer; const Title: string; Margins: TBCEditorPrintMargins);
    procedure LoadFromStream(AStream: TStream);
    procedure Print(ACanvas: TCanvas; PageNum: Integer);
    procedure SaveToStream(AStream: TStream);
    procedure SetPixelsPerInch(AValue: Integer);
    property SectionType: TBCEditorSectionType read FSectionType write FSectionType;
    property NumberOfPages: Integer read FNumberOfPages write FNumberOfPages;
  published
    property DefaultFont: TFont read FDefaultFont write SetDefaultFont;
    property FrameTypes: TBCEditorFrameTypes read FFrameTypes write FFrameTypes default [ftLine];
    property LineColor: TColor read FLineColor write FLineColor default clBlack;
    property MirrorPosition: Boolean read FMirrorPosition write FMirrorPosition default False;
    property RomanNumbers: Boolean read FRomanNumbers write FRomanNumbers default False;
    property ShadedColor: TColor read FShadedColor write FShadedColor default clSilver;
  end;

  TBCEditorPrintHeader = class(TBCEditorSection)
  public
    constructor Create;
  end;

  TBCEditorPrintFooter = class(TBCEditorSection)
  public
    constructor Create;
  end;

implementation

uses
  System.Math, System.UITypes, BCEditor.Consts;

{ TBCEditorSectionItem }

constructor TBCEditorSectionItem.Create;
begin
  inherited;
  FFont := TFont.Create;
end;

destructor TBCEditorSectionItem.Destroy;
begin
  inherited;
  FFont.Free;
end;

function TBCEditorSectionItem.GetText(ANumberOfPages, APageNumber: Integer; ARoman: Boolean; const ATitle, ATime, ADate: string): string;
var
  LLength, Start, Run: Integer;
  LString: string;

  procedure DoAppend(AText: string);
  begin
    Result := Result + AText;
  end;
  procedure TryAppend(var First: Integer; After: Integer);
  begin
    if After > First then
    begin
      DoAppend(Copy(LString, First, After - First));
      First := After;
    end;
  end;
  function TryExecuteMacro: Boolean;
  var
    Macro: string;
  begin
    Result := True;
    Macro := AnsiUpperCase(Copy(FText, Start, Run - Start + 1));
    if Macro = '$PAGENUM$' then
    begin
      if ARoman then
        DoAppend(IntToRoman(APageNumber))
      else
        DoAppend(IntToStr(APageNumber));
      Exit;
    end;
    if Macro = '$PAGECOUNT$' then
    begin
      if ARoman then
        DoAppend(IntToRoman(ANumberOfPages))
      else
        DoAppend(IntToStr(ANumberOfPages));
      Exit;
    end;
    if Macro = '$TITLE$' then
    begin
      DoAppend(ATitle);
      Exit;
    end;
    if Macro = '$DATE$' then
    begin
      DoAppend(ADate);
      Exit;
    end;
    if Macro = '$TIME$' then
    begin
      DoAppend(ATime);
      Exit;
    end;
    if Macro = '$DATETIME$' then
    begin
      DoAppend(ADate + ' ' + ATime);
      Exit;
    end;
    if Macro = '$TIMEDATE$' then
    begin
      DoAppend(ATime + ' ' + ADate);
      Exit;
    end;
    Result := False;
  end;

begin
  Result := '';
  LString := FText;
  if Trim(LString) = '' then
    Exit;
  LLength := Length(LString);
  if LLength > 0 then
  begin
    Start := 1;
    Run := 1;
    while Run <= LLength do
    begin
      if LString[Run] = '$' then
      begin
        TryAppend(Start, Run);
        Inc(Run);
        while Run <= LLength do
        begin
          if LString[Run] = '$' then
          begin
            if TryExecuteMacro then
            begin
              Inc(Run);
              Start := Run;
              break;
            end
            else
            begin
              TryAppend(Start, Run);
              Inc(Run);
            end;
          end
          else
            Inc(Run);
        end;
      end
      else
        Inc(Run);
    end;
    TryAppend(Start, Run);
  end;
end;

procedure TBCEditorSectionItem.LoadFromStream(AStream: TStream);
var
  LCharset: TFontCharset;
  LColor: TColor;
  LHeight: Integer;
  LName: TFontName;
  LPitch: TFontPitch;
  LSize: Integer;
  LStyle: TFontStyles;
  LLength, BufferSize: Integer;
  LBuffer: Pointer;
begin
  with AStream do
  begin
    Read(LLength, sizeof(LLength));
    BufferSize := LLength * sizeof(Char);
    GetMem(LBuffer, BufferSize + sizeof(Char));
    try
      Read(LBuffer^, BufferSize);
      PChar(LBuffer)[BufferSize div sizeof(Char)] := BCEDITOR_NONE_CHAR;
      FText := PChar(LBuffer);
    finally
      FreeMem(LBuffer);
    end;
    Read(FLineNumber, SizeOf(FLineNumber));
    Read(LCharset, SizeOf(lCharset));
    Read(LColor, SizeOf(LColor));
    Read(LHeight, SizeOf(LHeight));
    Read(BufferSize, SizeOf(BufferSize));
    GetMem(LBuffer, BufferSize + 1);
    try
      Read(LBuffer^, BufferSize);
      PAnsiChar(LBuffer)[BufferSize div SizeOf(AnsiChar)] := BCEDITOR_NONE_CHAR;
      LName := string(PAnsiChar(LBuffer));
    finally
      FreeMem(LBuffer);
    end;
    Read(LPitch, SizeOf(LPitch));
    Read(LSize, SizeOf(LSize));
    Read(LStyle, SizeOf(LStyle));
    FFont.Charset := LCharset;
    FFont.Color := LColor;
    FFont.Height := LHeight;
    FFont.Name := LName;
    FFont.Pitch := LPitch;
    FFont.Size := LSize;
    FFont.Style := LStyle;
    Read(FAlignment, SizeOf(FAlignment));
  end;
end;

procedure TBCEditorSectionItem.SaveToStream(AStream: TStream);
var
  LCharset: TFontCharset;
  LColor: TColor;
  LHeight: Integer;
  LName: TFontName;
  LPitch: TFontPitch;
  LSize: Integer;
  LStyle: TFontStyles;
  LLength: Integer;
begin
  with AStream do
  begin
    LLength := Length(FText);
    Write(LLength, SizeOf(LLength));
    Write(PChar(FText)^, LLength * SizeOf(Char));
    Write(FLineNumber, SizeOf(FLineNumber));
    lCharset := FFont.Charset;
    LColor := FFont.Color;
    LHeight := FFont.Height;
    LName := FFont.Name;
    LPitch := FFont.Pitch;
    LSize := FFont.Size;
    LStyle := FFont.Style;
    Write(LCharset, SizeOf(LCharset));
    Write(LColor, SizeOf(LColor));
    Write(LHeight, SizeOf(LHeight));
    LLength := Length(LName);
    Write(LLength, SizeOf(LLength));
    Write(PAnsiChar(AnsiString(LName))^, LLength);
    Write(LPitch, SizeOf(LPitch));
    Write(LSize, SizeOf(LSize));
    Write(LStyle, SizeOf(LStyle));
    Write(FAlignment, SizeOf(FAlignment));
  end;
end;

procedure TBCEditorSectionItem.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TBCEditorSection }

constructor TBCEditorSection.Create;
begin
  inherited;
  FFrameTypes := [ftLine];
  FShadedColor := clSilver;
  FLineColor := clBlack;
  FItems := TList.Create;
  FDefaultFont := TFont.Create;
  FOldPen := TPen.Create;
  FOldBrush := TBrush.Create;
  FOldFont := TFont.Create;
  FRomanNumbers := False;
  FMirrorPosition := False;
  FLineInfo := TList.Create;
  with FDefaultFont do
  begin
    Name := 'Courier New';
    Size := 9;
    Color := clBlack;
  end;
end;

destructor TBCEditorSection.Destroy;
var
  LIndex: Integer;
begin
  Clear;
  FItems.Free;
  FDefaultFont.Free;
  FOldPen.Free;
  FOldBrush.Free;
  FOldFont.Free;
  for LIndex := 0 to FLineInfo.Count - 1 do
    TBCEditorLineInfo(FLineInfo[LIndex]).Free;
  FLineInfo.Free;
  inherited;
end;

function TBCEditorSection.Add(const AText: string; const AFont: TFont; const AAlignment: TAlignment; const ALineNumber: Integer): Integer;
var
  LSectionItem: TBCEditorSectionItem;
begin
  LSectionItem := TBCEditorSectionItem.Create;
  if not Assigned(AFont) then
    LSectionItem.Font := FDefaultFont
  else
    LSectionItem.Font := AFont;
  LSectionItem.Alignment := AAlignment;
  LSectionItem.LineNumber := ALineNumber;
  LSectionItem.Index := FItems.Add(LSectionItem);
  LSectionItem.Text := AText;
  Result := LSectionItem.Index;
end;

procedure TBCEditorSection.Delete(AIndex: Integer);
var
  LIndex: Integer;
begin
  for LIndex := 0 to FItems.Count - 1 do
  if TBCEditorSectionItem(FItems[LIndex]).Index = AIndex then
  begin
    FItems.Delete(LIndex);
    Break;
  end;
end;

procedure TBCEditorSection.Clear;
var
  LIndex: Integer;
begin
  for LIndex := 0 to FItems.Count - 1 do
    TBCEditorSectionItem(FItems[LIndex]).Free;
  FItems.Clear;
end;

procedure TBCEditorSection.SetDefaultFont(const AValue: TFont);
begin
  FDefaultFont.Assign(AValue);
end;

procedure TBCEditorSection.FixLines;
var
  i, LCurrentLine: Integer;
  LLineInfo: TBCEditorLineInfo;
begin
  for i := 0 to FLineInfo.Count - 1 do
    TBCEditorLineInfo(FLineInfo[i]).Free;
  FLineInfo.Clear;
  LCurrentLine := 0;
  FLineCount := 0;
  for i := 0 to FItems.Count - 1 do
  begin
    if TBCEditorSectionItem(FItems[i]).LineNumber <> LCurrentLine then
    begin
      LCurrentLine := TBCEditorSectionItem(FItems[i]).LineNumber;
      FLineCount := FLineCount + 1;
      LLineInfo := TBCEditorLineInfo.Create;
      FLineInfo.Add(LLineInfo);
    end;
    TBCEditorSectionItem(FItems[i]).LineNumber := FLineCount;
  end;
end;

procedure TBCEditorSection.CalculateHeight(ACanvas: TCanvas);
var
  i, LCurrentLine: Integer;
  LSectionItem: TBCEditorSectionItem;
  LOrginalHeight: Integer;
  LTextMetric: TTextMetric;
begin
  FFrameHeight := -1;
  if FItems.Count <= 0 then
    Exit;

  LCurrentLine := 1;
  FFrameHeight := 0;
  LOrginalHeight := FFrameHeight;
  for i := 0 to FItems.Count - 1 do
  begin
    LSectionItem := TBCEditorSectionItem(FItems[i]);
    if LSectionItem.LineNumber <> LCurrentLine then
    begin
      LCurrentLine := LSectionItem.LineNumber;
      LOrginalHeight := FFrameHeight;
    end;
    ACanvas.Font.Assign(LSectionItem.Font);
    GetTextMetrics(ACanvas.Handle, LTextMetric);
    with TBCEditorLineInfo(FLineInfo[LCurrentLine - 1]), LTextMetric do
    begin
      LineHeight := Max(LineHeight, TextHeight(ACanvas, 'W'));
      MaxBaseDistance := Max(MaxBaseDistance, tmHeight - tmDescent);
    end;
    FFrameHeight := Max(FFrameHeight, LOrginalHeight + TextHeight(ACanvas, 'W'));
  end;
  FFrameHeight := FFrameHeight + 2 * FMargins.PixelInternalMargin;
end;

function CompareItems(Item1, Item2: Pointer): Integer;
begin
  Result := TBCEditorSectionItem(Item1).LineNumber - TBCEditorSectionItem(Item2).LineNumber;
  if Result = 0 then
    Result := Integer(Item1) - Integer(Item2);
end;

procedure TBCEditorSection.SetPixelsPerInch(AValue: Integer);
var
  i, LTmpSize: Integer;
  LFont: TFont;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    LFont := TBCEditorSectionItem(FItems[i]).Font;
    LTmpSize := LFont.Size;
    LFont.PixelsPerInch := AValue;
    LFont.Size := LTmpSize;
  end;
end;

procedure TBCEditorSection.InitPrint(ACanvas: TCanvas; NumberOfPages: Integer; const Title: string; Margins: TBCEditorPrintMargins);
begin
  SaveFontPenBrush(ACanvas);
  FDate := DateToStr(Now);
  FTime := TimeToStr(Now);
  FNumberOfPages := NumberOfPages;
  FMargins := Margins;
  FTitle := Title;
  FItems.Sort(CompareItems);
  FixLines;
  CalculateHeight(ACanvas);
  RestoreFontPenBrush(ACanvas);
end;

procedure TBCEditorSection.SaveFontPenBrush(ACanvas: TCanvas);
begin
  FOldFont.Assign(ACanvas.Font);
  FOldBrush.Assign(ACanvas.Brush);
  FOldPen.Assign(ACanvas.Pen);
end;

procedure TBCEditorSection.RestoreFontPenBrush(ACanvas: TCanvas);
begin
  ACanvas.Font.Assign(FOldFont);
  ACanvas.Brush.Assign(FOldBrush);
  ACanvas.Pen.Assign(FOldPen);
end;

procedure TBCEditorSection.DrawFrame(ACanvas: TCanvas);
begin
  if FrameTypes = [] then
    Exit;
  with ACanvas, FMargins do
  begin
    Pen.Color := LineColor;
    Brush.Color := ShadedColor;
    if ftShaded in FrameTypes then
      Brush.Style := bsSolid
    else
      Brush.Style := bsClear;
    if ftBox in FrameTypes then
      Pen.Style := psSolid
    else
      Pen.Style := psClear;
    if FrameTypes * [ftBox, ftShaded] <> [] then
    begin
      if FSectionType = stHeader then
        Rectangle(PixelLeft, PixelHeader - FFrameHeight, PixelRight, PixelHeader)
      else
        Rectangle(PixelLeft, PixelFooter, PixelRight, PixelFooter + FFrameHeight);
    end;
    if ftLine in FrameTypes then
    begin
      Pen.Style := psSolid;
      if FSectionType = stHeader then
      begin
        MoveTo(PixelLeft, PixelHeader);
        LineTo(PixelRight, PixelHeader);
      end
      else
      begin
        MoveTo(PixelLeft, PixelFooter);
        LineTo(PixelRight, PixelFooter);
      end
    end;
  end;
end;

procedure TBCEditorSection.Print(ACanvas: TCanvas; PageNum: Integer);
var
  i, X, Y, LCurrentLine: Integer;
  S: string;
  LSectionItem: TBCEditorSectionItem;
  LOldAlign: UINT;
  LAlignment: TAlignment;
begin
  if FFrameHeight <= 0 then
    Exit;
  SaveFontPenBrush(ACanvas);
  DrawFrame(ACanvas);
  ACanvas.Brush.Style := bsClear;
  if FSectionType = stHeader then
    Y := FMargins.PixelHeader - FFrameHeight
  else
    Y := FMargins.PixelFooter;
  Y := Y + FMargins.PixelInternalMargin;

  LCurrentLine := 1;
  for i := 0 to FItems.Count - 1 do
  begin
    LSectionItem := TBCEditorSectionItem(FItems[i]);
    ACanvas.Font := LSectionItem.Font;
    if LSectionItem.LineNumber <> LCurrentLine then
    begin
      Y := Y + TBCEditorLineInfo(FLineInfo[LCurrentLine - 1]).LineHeight;
      LCurrentLine := LSectionItem.LineNumber;
    end;
    S := LSectionItem.GetText(FNumberOfPages, PageNum, FRomanNumbers, FTitle, FTime, FDate);
    LAlignment := LSectionItem.Alignment;
    if MirrorPosition and ((PageNum mod 2) = 0) then
    begin
      case LSectionItem.Alignment of
        taRightJustify:
          LAlignment := taLeftJustify;
        taLeftJustify:
          LAlignment := taRightJustify;
      end;
    end;
    with FMargins do
    begin
      X := PixelLeftTextIndent;
      case LAlignment of
        taRightJustify:
          X := PixelRightTextIndent - TextWidth(ACanvas, S);
        taCenter:
          X := (PixelLeftTextIndent + PixelRightTextIndent - TextWidth(ACanvas, S)) div 2;
      end;
    end;
    LOldAlign := SetTextAlign(ACanvas.Handle, TA_BASELINE);
    Winapi.Windows.ExtTextOut(ACanvas.Handle, X, Y + TBCEditorLineInfo(FLineInfo[LCurrentLine - 1]).MaxBaseDistance, 0, nil, PChar(S),
      Length(S), nil);
    SetTextAlign(ACanvas.Handle, LOldAlign);
  end;
  RestoreFontPenBrush(ACanvas);
end;

procedure TBCEditorSection.Assign(ASource: TPersistent);
var
  LIndex: Integer;
  LSectionItem: TBCEditorSectionItem;
begin
  if Assigned(ASource) and (ASource is TBCEditorSection) then
  with ASource as TBCEditorSection do
  begin
    Clear;
    Self.FSectionType := FSectionType;
    Self.FFrameTypes := FFrameTypes;
    Self.FShadedColor := FShadedColor;
    Self.FLineColor := FLineColor;
    for LIndex := 0 to FItems.Count - 1 do
    begin
      LSectionItem := TBCEditorSectionItem(FItems[LIndex]);
      Self.Add(LSectionItem.Text, LSectionItem.Font, LSectionItem.Alignment, LSectionItem.LineNumber);
    end;
    Self.FDefaultFont.Assign(FDefaultFont);
    Self.FRomanNumbers := FRomanNumbers;
    Self.FMirrorPosition := FMirrorPosition;
  end
  else
    inherited Assign(ASource);
end;

function TBCEditorSection.Count: Integer;
begin
  Result := FItems.Count;
end;

function TBCEditorSection.Get(AIndex: Integer): TBCEditorSectionItem;
begin
  Result := TBCEditorSectionItem(FItems[AIndex]);
end;

procedure TBCEditorSection.LoadFromStream(AStream: TStream);
var
  LCount, LIndex: Integer;
  LCharset: TFontCharset;
  LColor: TColor;
  LHeight: Integer;
  LName: TFontName;
  LPitch: TFontPitch;
  LSize: Integer;
  LStyle: TFontStyles;
  LBufferSize: Integer;
  LBuffer: PAnsiChar;
begin
  with AStream do
  begin
    Read(FFrameTypes, SizeOf(FFrameTypes));
    Read(FShadedColor, SizeOf(FShadedColor));
    Read(FLineColor, SizeOf(FLineColor));
    Read(FRomanNumbers, SizeOf(FRomanNumbers));
    Read(FMirrorPosition, SizeOf(FMirrorPosition));
    Read(LCharset, SizeOf(LCharset));
    Read(LColor, SizeOf(LColor));
    Read(LHeight, SizeOf(LHeight));
    Read(LBufferSize, SizeOf(LBufferSize));
    GetMem(LBuffer, LBufferSize + 1);
    try
      Read(LBuffer^, LBufferSize);
      LBuffer[LBufferSize] := BCEDITOR_NONE_CHAR;
      LName := string(LBuffer);
    finally
      FreeMem(LBuffer);
    end;
    Read(LPitch, SizeOf(LPitch));
    Read(LSize, SizeOf(LSize));
    Read(LStyle, SizeOf(LStyle));
    FDefaultFont.Charset := LCharset;
    FDefaultFont.Color := LColor;
    FDefaultFont.Height := LHeight;
    FDefaultFont.Name := LName;
    FDefaultFont.Pitch := LPitch;
    FDefaultFont.Size := LSize;
    FDefaultFont.Style := LStyle;
    Read(LCount, SizeOf(LCount));
    while LCount > 0 do
    begin
      LIndex := Add('', nil, taLeftJustify, 1);
      Get(LIndex).LoadFromStream(AStream);
      Dec(LCount);
    end;
  end;
end;

procedure TBCEditorSection.SaveToStream(AStream: TStream);
var
  LIndex, LCount: Integer;
  LCharset: TFontCharset;
  LColor: TColor;
  LHeight: Integer;
  LName: TFontName;
  LPitch: TFontPitch;
  LSize: Integer;
  LStyle: TFontStyles;
  LLength: Integer;
begin
  with AStream do
  begin
    Write(FFrameTypes, SizeOf(FFrameTypes));
    Write(FShadedColor, SizeOf(FShadedColor));
    Write(FLineColor, SizeOf(FLineColor));
    Write(FRomanNumbers, SizeOf(FRomanNumbers));
    Write(FMirrorPosition, SizeOf(FMirrorPosition));
    LCharset := FDefaultFont.Charset;
    LColor := FDefaultFont.Color;
    LHeight := FDefaultFont.Height;
    LName := FDefaultFont.Name;
    LPitch := FDefaultFont.Pitch;
    LSize := FDefaultFont.Size;
    LStyle := FDefaultFont.Style;
    Write(LCharset, SizeOf(LCharset));
    Write(LColor, SizeOf(LColor));
    Write(LHeight, SizeOf(LHeight));
    LLength := Length(LName);
    Write(LLength, SizeOf(LLength));
    Write(PAnsiChar(AnsiString(LName))^, Length(LName));
    Write(LPitch, SizeOf(LPitch));
    Write(LSize, SizeOf(LSize));
    Write(LStyle, SizeOf(LStyle));
    LCount := Count;
    Write(LCount, SizeOf(LCount));
    for LIndex := 0 to LCount - 1 do
      Get(LIndex).SaveToStream(AStream);
  end;
end;

{ TBCEditorPrintHeader }

constructor TBCEditorPrintHeader.Create;
begin
  inherited;
  SectionType := stHeader;
end;

{ TBCEditorPrintFooter }

constructor TBCEditorPrintFooter.Create;
begin
  inherited;
  SectionType := stFooter;
end;

end.
