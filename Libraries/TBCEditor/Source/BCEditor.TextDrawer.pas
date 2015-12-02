unit BCEditor.TextDrawer;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, System.Math, System.Types, System.UITypes,
  BCEditor.Utils;

const
  CFontStyleCount = Ord(High(TFontStyle)) + 1;
  CFontStyleCombineCount = 1 shl CFontStyleCount;

type
  TBCEditorStockFontPatterns = 0 .. CFontStyleCombineCount - 1;

  TBCEditorFontData = record
    Style: TFontStyles;
    Handle: HFont;
    CharAdvance: Integer;
    CharHeight: Integer;
  end;
  PBCEditorFontData = ^TBCEditorFontData;

  TBCEditorFontsData = array [TBCEditorStockFontPatterns] of TBCEditorFontData;

  TBCEditorSharedFontsInfo = record
    RefCount: Integer;
    LockCount: Integer;
    BaseFont: TFont;
    BaseLogFont: TLogFont;
    IsTrueType: Boolean;
    FontsData: TBCEditorFontsData;
  end;

  PBCEditorSharedFontsInfo = ^TBCEditorSharedFontsInfo;

  { TBCEditorFontsInfoManager }

  TBCEditorFontsInfoManager = class(TObject)
  strict private
    FFontsInfo: TList;
    function FindFontsInfo(const ALogFont: TLogFont): PBCEditorSharedFontsInfo;
    function CreateFontsInfo(ABaseFont: TFont; const ALogFont: TLogFont): PBCEditorSharedFontsInfo;
    procedure DestroyFontHandles(ASharedFontsInfo: PBCEditorSharedFontsInfo);
    procedure RetrieveLogFontForComparison(ABaseFont: TFont; var ALogFont: TLogFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LockFontsInfo(ASharedFontsInfo: PBCEditorSharedFontsInfo);
    procedure UnLockFontsInfo(ASharedFontsInfo: PBCEditorSharedFontsInfo);
    function GetFontsInfo(ABaseFont: TFont): PBCEditorSharedFontsInfo;
    procedure ReleaseFontsInfo(ASharedFontsInfo: PBCEditorSharedFontsInfo);
  end;

  TBCEditorTextOutOptions = set of (tooOpaque, tooClipped);

  TBCEditorFontStock = class(TObject)
  strict private
    FCurrentFont: HFont;
    FCurrentStyle: TFontStyles;
    FHandle: HDC;
    FHandleRefCount: Integer;
    FPSharedFontsInfo: PBCEditorSharedFontsInfo;
    FUsingFontHandles: Boolean;
    FPCurrentFontData: PBCEditorFontData;
    FBaseLogFont: TLogFont;
    function GetBaseFont: TFont;
    function GetIsTrueType: Boolean;
  protected
    function CalculateFontAdvance(AHandle: HDC; ACharHeight: PInteger): Integer; virtual;
    function GetCharAdvance: Integer; virtual;
    function GetCharHeight: Integer; virtual;
    function GetFontData(AIndex: Integer): PBCEditorFontData; virtual;
    function InternalGetHandle: HDC; virtual;
    function InternalCreateFont(AStyle: TFontStyles): HFont; virtual;
    procedure InternalReleaseDC(AValue: HDC); virtual;
    procedure ReleaseFontsInfo;
    procedure SetBaseFont(AValue: TFont); virtual;
    procedure SetStyle(AValue: TFontStyles); virtual;
    procedure UseFontHandles;
    property FontData[AIndex: Integer]: PBCEditorFontData read GetFontData;
    property FontsInfo: PBCEditorSharedFontsInfo read FPSharedFontsInfo;
  public
    constructor Create(AInitialFont: TFont); virtual;
    destructor Destroy; override;

    procedure ReleaseFontHandles; virtual;
    property BaseFont: TFont read GetBaseFont;
    property Style: TFontStyles read FCurrentStyle write SetStyle;
    property FontHandle: HFont read FCurrentFont;
    property CharAdvance: Integer read GetCharAdvance;
    property CharHeight: Integer read GetCharHeight;
    property IsTrueType: Boolean read GetIsTrueType;
  end;

  { TBCEditorTextDrawer }
  ETextDrawerException = class(Exception);

  TBCEditorTextDrawer = class(TObject)
  strict private
    FBackgroundColor: TColor;
    FBaseCharHeight: Integer;
    FBaseCharWidth: Integer;
    FCalcExtentBaseStyle: TFontStyles;
    FCharABCWidthCache: array [0 .. 127] of TABC;
    FCharExtra: Integer;
    FCharWidthCache: array [0 .. 127] of Integer;
    FColor: TColor;
    FCurrentFont: HFont;
    FDrawingCount: Integer;
    FExtTextOutDistance: PIntegerArray;
    FExtTextOutLength: Integer;
    FFontStock: TBCEditorFontStock;
    FHandle: HDC;
    FSaveHandle: Integer;
    FStockBitmap: TBitmap;
  protected
    function GetCachedABCWidth(AChar: Cardinal; var AABC: TABC): Boolean;
    procedure AfterStyleSet; virtual;
    procedure DoSetCharExtra(AValue: Integer); virtual;
    procedure FlushCharABCWidthCache;
    property BaseCharHeight: Integer read FBaseCharHeight;
    property BaseCharWidth: Integer read FBaseCharWidth;
    property DrawingCount: Integer read FDrawingCount;
    property FontStock: TBCEditorFontStock read FFontStock;
    property StockHandle: HDC read FHandle;
  public
    constructor Create(ACalcExtentBaseStyle: TFontStyles; ABaseFont: TFont); virtual;
    destructor Destroy; override;

    function GetCharCount(AChar: PChar): Integer;
    function GetCharHeight: Integer; virtual;
    function GetCharWidth: Integer; virtual;
    function TextExtent(const Text: string): TSize;
    function TextCharWidth(const AChar: PChar): Integer;
    procedure BeginDrawing(AHandle: HDC); virtual;
    procedure EndDrawing; virtual;
    procedure ExtTextOut(X, Y: Integer; AOptions: TBCEditorTextOutOptions; var ARect: TRect; AText: PChar; ALength: Integer); virtual;
    procedure SetBackgroundColor(AValue: TColor); virtual;
    procedure SetBaseFont(AValue: TFont); virtual;
    procedure SetBaseStyle(const AValue: TFontStyles); virtual;
    procedure SetCharExtra(AValue: Integer); virtual;
    procedure SetForegroundColor(AValue: TColor); virtual;
    procedure SetStyle(AValue: TFontStyles); virtual;
    procedure TextOut(X, Y: Integer; AText: PChar; ALength: Integer); virtual;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property BaseFont: TFont write SetBaseFont;
    property BaseStyle: TFontStyles write SetBaseStyle;
    property CharExtra: Integer read FCharExtra write SetCharExtra;
    property CharHeight: Integer read GetCharHeight;
    property CharWidth: Integer read GetCharWidth;
    property ForegroundColor: TColor write SetForegroundColor;
    property Style: TFontStyles write SetStyle;
  end;

function GetFontsInfoManager: TBCEditorFontsInfoManager;

function UniversalExtTextOut(AHandle: HDC; X, Y: Integer; AOptions: TBCEditorTextOutOptions; ARect: TRect; AStr: PChar;
  ACount: Integer; AExtTextOutDistance: PIntegerArray): Boolean;

implementation

var
  GFontsInfoManager: TBCEditorFontsInfoManager;

  { utility routines }

function GetFontsInfoManager: TBCEditorFontsInfoManager;
begin
  if not Assigned(GFontsInfoManager) then
    GFontsInfoManager := TBCEditorFontsInfoManager.Create;
  Result := GFontsInfoManager;
end;

function UniversalExtTextOut(AHandle: HDC; X, Y: Integer; AOptions: TBCEditorTextOutOptions; ARect: TRect; AStr: PChar;
  ACount: Integer; AExtTextOutDistance: PIntegerArray): Boolean;
var
  LTextOutFlags: DWORD;
begin
  LTextOutFlags := 0;
  if tooOpaque in AOptions then
    LTextOutFlags := LTextOutFlags or ETO_OPAQUE;
  if tooClipped in AOptions then
    LTextOutFlags := LTextOutFlags or ETO_CLIPPED;

  Result := Winapi.Windows.ExtTextOut(AHandle, X, Y, LTextOutFlags, @ARect, AStr, ACount, Pointer(AExtTextOutDistance));
end;

{ TFontsInfoManager }

procedure TBCEditorFontsInfoManager.LockFontsInfo(ASharedFontsInfo: PBCEditorSharedFontsInfo);
begin
  Inc(ASharedFontsInfo^.LockCount);
end;

constructor TBCEditorFontsInfoManager.Create;
begin
  inherited;

  FFontsInfo := TList.Create;
end;

function TBCEditorFontsInfoManager.CreateFontsInfo(ABaseFont: TFont; const ALogFont: TLogFont): PBCEditorSharedFontsInfo;
begin
  New(Result);
  FillChar(Result^, SizeOf(TBCEditorSharedFontsInfo), 0);
  with Result^ do
  try
    BaseFont := TFont.Create;
    BaseFont.Assign(ABaseFont);
    BaseLogFont := ALogFont;
    IsTrueType := (0 <> (TRUETYPE_FONTTYPE and ALogFont.lfPitchAndFamily));
  except
    Result^.BaseFont.Free;
    Dispose(Result);
    raise;
  end;
end;

procedure TBCEditorFontsInfoManager.UnLockFontsInfo(ASharedFontsInfo: PBCEditorSharedFontsInfo);
begin
  with ASharedFontsInfo^ do
  begin
    Dec(LockCount);
    if 0 = LockCount then
      DestroyFontHandles(ASharedFontsInfo);
  end;
end;

destructor TBCEditorFontsInfoManager.Destroy;
begin
  GFontsInfoManager := nil;

  if Assigned(FFontsInfo) then
  begin
    while FFontsInfo.Count > 0 do
    begin
      Assert(1 = PBCEditorSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1])^.RefCount);
      ReleaseFontsInfo(PBCEditorSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1]));
    end;
    FFontsInfo.Free;
  end;

  inherited;
end;

procedure TBCEditorFontsInfoManager.DestroyFontHandles(ASharedFontsInfo: PBCEditorSharedFontsInfo);
var
  i: Integer;
begin
  with ASharedFontsInfo^ do
  for i := Low(TBCEditorStockFontPatterns) to High(TBCEditorStockFontPatterns) do
  with FontsData[i] do
  if Handle <> 0 then
  begin
    DeleteObject(Handle);
    Handle := 0;
  end;
end;

function TBCEditorFontsInfoManager.FindFontsInfo(const ALogFont: TLogFont): PBCEditorSharedFontsInfo;
var
  i: Integer;
begin
  for i := 0 to FFontsInfo.Count - 1 do
  begin
    Result := PBCEditorSharedFontsInfo(FFontsInfo[i]);
    if CompareMem(@(Result^.BaseLogFont), @ALogFont, SizeOf(TLogFont)) then
      Exit;
  end;
  Result := nil;
end;

function TBCEditorFontsInfoManager.GetFontsInfo(ABaseFont: TFont): PBCEditorSharedFontsInfo;
var
  LLogFont: TLogFont;
begin
  Assert(Assigned(ABaseFont));

  RetrieveLogFontForComparison(ABaseFont, LLogFont);
  Result := FindFontsInfo(LLogFont);
  if not Assigned(Result) then
  begin
    Result := CreateFontsInfo(ABaseFont, LLogFont);
    FFontsInfo.Add(Result);
  end;

  if Assigned(Result) then
    Inc(Result^.RefCount);
end;

procedure TBCEditorFontsInfoManager.ReleaseFontsInfo(ASharedFontsInfo: PBCEditorSharedFontsInfo);
begin
  Assert(Assigned(ASharedFontsInfo));

  with ASharedFontsInfo^ do
  begin
    Assert(LockCount < RefCount);
    if RefCount > 1 then
      Dec(RefCount)
    else
    begin
      FFontsInfo.Remove(ASharedFontsInfo);
      BaseFont.Free;
      Dispose(ASharedFontsInfo);
    end;
  end;
end;

procedure TBCEditorFontsInfoManager.RetrieveLogFontForComparison(ABaseFont: TFont; var ALogFont: TLogFont);
var
  LPEnd: PChar;
begin
  GetObject(ABaseFont.Handle, SizeOf(TLogFont), @ALogFont);
  with ALogFont do
  begin
    lfItalic := 0;
    lfUnderline := 0;
    lfStrikeOut := 0;
    LPEnd := StrEnd(lfFaceName);
    FillChar(LPEnd[1], @lfFaceName[high(lfFaceName)] - LPEnd, 0);
  end;
end;

{ TFontStock }

function TBCEditorFontStock.CalculateFontAdvance(AHandle: HDC; ACharHeight: PInteger): Integer;
var
  LTextMetric: TTextMetric;
  LCharInfo: TABC;
  LHasABC: Boolean;
begin
  GetTextMetrics(AHandle, LTextMetric);
  LHasABC := GetCharABCWidths(AHandle, Ord('M'), Ord('M'), LCharInfo);
  if not LHasABC then
  begin
    with LCharInfo do
    begin
      abcA := 0;
      abcB := LTextMetric.tmAveCharWidth;
      abcC := 0;
    end;
    LTextMetric.tmOverhang := 0;
  end;

  with LCharInfo do
    Result := abcA + Integer(abcB) + abcC + LTextMetric.tmOverhang;
  if Assigned(ACharHeight) then
    ACharHeight^ := Abs(LTextMetric.tmHeight)
end;

constructor TBCEditorFontStock.Create(AInitialFont: TFont);
begin
  inherited Create;

  SetBaseFont(AInitialFont);
end;

destructor TBCEditorFontStock.Destroy;
begin
  ReleaseFontsInfo;
  Assert(FHandleRefCount = 0);

  inherited;
end;

function TBCEditorFontStock.GetBaseFont: TFont;
begin
  Result := FPSharedFontsInfo^.BaseFont;
end;

function TBCEditorFontStock.GetCharAdvance: Integer;
begin
  Result := FPCurrentFontData^.CharAdvance;
end;

function TBCEditorFontStock.GetCharHeight: Integer;
begin
  Result := FPCurrentFontData^.CharHeight;
end;

function TBCEditorFontStock.GetFontData(AIndex: Integer): PBCEditorFontData;
begin
  Result := @FPSharedFontsInfo^.FontsData[AIndex];
end;

function TBCEditorFontStock.GetIsTrueType: Boolean;
begin
  Result := FPSharedFontsInfo^.IsTrueType
end;

function TBCEditorFontStock.InternalCreateFont(AStyle: TFontStyles): HFont;
const
  CBolds: array [Boolean] of Integer = (400, 700);
begin
  with FBaseLogFont do
  begin
    lfWeight := CBolds[fsBold in Style];
    lfItalic := Ord(BOOL(fsItalic in Style));
    lfUnderline := Ord(BOOL(fsUnderline in Style));
    lfStrikeOut := Ord(BOOL(fsStrikeOut in Style));
  end;
  Result := CreateFontIndirect(FBaseLogFont);
end;

function TBCEditorFontStock.InternalGetHandle: HDC;
begin
  if FHandleRefCount = 0 then
  begin
    Assert(FHandle = 0);
    FHandle := GetDC(0);
  end;
  Inc(FHandleRefCount);
  Result := FHandle;
end;

procedure TBCEditorFontStock.InternalReleaseDC(AValue: HDC);
begin
  Dec(FHandleRefCount);
  if FHandleRefCount <= 0 then
  begin
    Assert((FHandle <> 0) and (FHandle = AValue));
    ReleaseDC(0, FHandle);
    FHandle := 0;
    Assert(FHandleRefCount = 0);
  end;
end;

procedure TBCEditorFontStock.ReleaseFontHandles;
begin
  if FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      UnLockFontsInfo(FPSharedFontsInfo);
      FUsingFontHandles := False;
    end;
end;

procedure TBCEditorFontStock.ReleaseFontsInfo;
begin
  if Assigned(FPSharedFontsInfo) then
    with GetFontsInfoManager do
    begin
      if FUsingFontHandles then
      begin
        UnLockFontsInfo(FPSharedFontsInfo);
        FUsingFontHandles := False;
      end;
      ReleaseFontsInfo(FPSharedFontsInfo);
      FPSharedFontsInfo := nil;
    end;
end;

procedure TBCEditorFontStock.SetBaseFont(AValue: TFont);
var
  LSharedFontsInfo: PBCEditorSharedFontsInfo;
begin
  if Assigned(AValue) then
  begin
    LSharedFontsInfo := GetFontsInfoManager.GetFontsInfo(AValue);
    if LSharedFontsInfo = FPSharedFontsInfo then
      GetFontsInfoManager.ReleaseFontsInfo(LSharedFontsInfo)
    else
    begin
      ReleaseFontsInfo;
      FPSharedFontsInfo := LSharedFontsInfo;
      FBaseLogFont := FPSharedFontsInfo^.BaseLogFont;
      SetStyle(AValue.Style);
    end;
  end
  else
    raise Exception.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TBCEditorFontStock.SetStyle(AValue: TFontStyles);
var
  LIndex: Integer;
  LHandle: HDC;
  LOldFont: HFont;
  LFontDataPointer: PBCEditorFontData;
begin
  Assert(SizeOf(TFontStyles) = 1);

  LIndex := Byte(AValue);
  Assert(LIndex <= High(TBCEditorStockFontPatterns));

  UseFontHandles;
  LFontDataPointer := FontData[LIndex];
  if FPCurrentFontData = LFontDataPointer then
    Exit;

  FPCurrentFontData := LFontDataPointer;
  with LFontDataPointer^ do
  if Handle <> 0 then
  begin
    FCurrentFont := Handle;
    FCurrentStyle := Style;
    Exit;
  end;

  FCurrentFont := InternalCreateFont(AValue);
  LHandle := InternalGetHandle;
  LOldFont := SelectObject(LHandle, FCurrentFont);

  with FPCurrentFontData^ do
  begin
    Handle := FCurrentFont;
    CharAdvance := CalculateFontAdvance(LHandle, @CharHeight);
  end;

  SelectObject(LHandle, LOldFont);
  InternalReleaseDC(LHandle);
end;

procedure TBCEditorFontStock.UseFontHandles;
begin
  if not FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      LockFontsInfo(FPSharedFontsInfo);
      FUsingFontHandles := True;
    end;
end;

{ TBCEditorTextDrawer }

constructor TBCEditorTextDrawer.Create(ACalcExtentBaseStyle: TFontStyles; ABaseFont: TFont);
begin
  inherited Create;

  FFontStock := TBCEditorFontStock.Create(ABaseFont);
  FStockBitmap := TBitmap.Create;
  FCalcExtentBaseStyle := ACalcExtentBaseStyle;
  SetBaseFont(ABaseFont);
  FColor := clWindowText;
  FBackgroundColor := clWindow;
  FExtTextOutLength := 0;
end;

destructor TBCEditorTextDrawer.Destroy;
begin
  FStockBitmap.Free;
  FFontStock.Free;
  if Assigned(FExtTextOutDistance) then
  begin
    FreeMem(FExtTextOutDistance);
    FExtTextOutDistance := nil;
  end;

  inherited;
end;

procedure TBCEditorTextDrawer.BeginDrawing(AHandle: HDC);
begin
  if FHandle = AHandle then
    Assert(FHandle <> 0)
  else
  begin
    Assert((FHandle = 0) and (AHandle <> 0) and (FDrawingCount = 0));
    FHandle := AHandle;
    FSaveHandle := SaveDC(AHandle);
    SelectObject(AHandle, FCurrentFont);
    Winapi.Windows.SetTextColor(AHandle, ColorToRGB(FColor));
    Winapi.Windows.SetBkColor(AHandle, ColorToRGB(FBackgroundColor));
    DoSetCharExtra(FCharExtra);
  end;
  Inc(FDrawingCount);
end;

procedure TBCEditorTextDrawer.EndDrawing;
begin
  Assert(FDrawingCount >= 1);
  Dec(FDrawingCount);
  if FDrawingCount <= 0 then
  begin
    if FHandle <> 0 then
      RestoreDC(FHandle, FSaveHandle);
    FSaveHandle := 0;
    FHandle := 0;
    FDrawingCount := 0;
  end;
end;

function TBCEditorTextDrawer.GetCharWidth: Integer;
begin
  Result := FBaseCharWidth + FCharExtra;
end;

function TBCEditorTextDrawer.GetCharHeight: Integer;
begin
  Result := FBaseCharHeight;
end;

procedure TBCEditorTextDrawer.SetBaseFont(AValue: TFont);
begin
  if Assigned(AValue) then
  begin
    FlushCharABCWidthCache;
    FStockBitmap.Canvas.Font.Assign(AValue);
    FStockBitmap.Canvas.Font.Style := [];
    with FFontStock do
    begin
      SetBaseFont(AValue);
      Style := FCalcExtentBaseStyle;
      FBaseCharWidth := CharAdvance;
      FBaseCharHeight := CharHeight;
    end;
    SetStyle(AValue.Style);
  end
  else
    raise ETextDrawerException.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TBCEditorTextDrawer.SetBaseStyle(const AValue: TFontStyles);
begin
  if FCalcExtentBaseStyle <> AValue then
  begin
    FlushCharABCWidthCache;
    FCalcExtentBaseStyle := AValue;
    with FFontStock do
    begin
      Style := AValue;
      FBaseCharWidth := CharAdvance;
      FBaseCharHeight := CharHeight;
    end;
  end;
end;

procedure TBCEditorTextDrawer.SetStyle(AValue: TFontStyles);
begin
  with FFontStock do
  begin
    SetStyle(AValue);
    Self.FCurrentFont := FontHandle;
  end;
  AfterStyleSet;
end;

procedure TBCEditorTextDrawer.FlushCharABCWidthCache;
begin
  FillChar(FCharABCWidthCache, SizeOf(TABC) * Length(FCharABCWidthCache), 0);
  FillChar(FCharWidthCache, SizeOf(Integer) * Length(FCharWidthCache), 0);
end;

procedure TBCEditorTextDrawer.AfterStyleSet;
begin
  if FHandle <> 0 then
    SelectObject(FHandle, FCurrentFont);
end;

function TBCEditorTextDrawer.GetCachedABCWidth(AChar: Cardinal; var AABC: TABC): Boolean;
begin
  if AChar > High(FCharABCWidthCache) then
  begin
    Result := GetCharABCWidthsW(FHandle, AChar, AChar, AABC);
    Exit;
  end;
  AABC := FCharABCWidthCache[AChar];
  if (AABC.abcA or Integer(AABC.abcB) or AABC.abcC) = 0 then
  begin
    Result := GetCharABCWidthsW(FHandle, AChar, AChar, AABC);
    if Result then
      FCharABCWidthCache[AChar] := AABC;
  end
  else
    Result := True;
end;

procedure TBCEditorTextDrawer.SetForegroundColor(AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    if FHandle <> 0 then
      SetTextColor(FHandle, ColorToRGB(AValue));
  end;
end;

procedure TBCEditorTextDrawer.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    if FHandle <> 0 then
      Winapi.Windows.SetBkColor(FHandle, ColorToRGB(AValue));
  end;
end;

procedure TBCEditorTextDrawer.SetCharExtra(AValue: Integer);
begin
  if FCharExtra <> AValue then
  begin
    FCharExtra := AValue;
    DoSetCharExtra(FCharExtra);
  end;
end;

procedure TBCEditorTextDrawer.DoSetCharExtra(AValue: Integer);
begin
  if FHandle <> 0 then
    SetTextCharacterExtra(FHandle, AValue);
end;

procedure TBCEditorTextDrawer.TextOut(X, Y: Integer; AText: PChar; ALength: Integer);
var
  LTempRect: TRect;
begin
  LTempRect := Rect(X, Y, X, Y);
  UniversalExtTextOut(FHandle, X, Y, [], LTempRect, AText, ALength, nil);
end;

function TBCEditorTextDrawer.GetCharCount(AChar: PChar): Integer;
begin
  Result := CeilOfIntDiv(TextCharWidth(AChar), CharWidth);
end;

procedure TBCEditorTextDrawer.ExtTextOut(X, Y: Integer; AOptions: TBCEditorTextOutOptions; var ARect: TRect; AText: PChar;
  ALength: Integer);

  procedure InitExtTextOutDistance(ACharWidth: Integer);
  var
    i: Integer;
  begin
    if ALength > FExtTextOutLength then
    begin
      FExtTextOutLength := ALength;
      ReallocMem(FExtTextOutDistance, ALength * SizeOf(Integer));
    end;
    for i := 0 to ALength - 1 do
      FExtTextOutDistance[i] := GetCharCount(@AText[i]) * ACharWidth;
  end;

  { avoid clipping the last pixels of text in italic }
  procedure AdjustLastCharWidthAndRect;
  var
    LLastChar: Cardinal;
    LRealCharWidth, LNormalCharWidth: Integer;
    LCharInfo: TABC;
    LTextMetricA: TTextMetricA;
  begin
    if ALength <= 0 then
      Exit;
    LLastChar := Ord(AText[ALength - 1]);
    if LLastChar = 32 then
      Exit;
    LNormalCharWidth := FExtTextOutDistance[ALength - 1];
    LRealCharWidth := LNormalCharWidth;

    if GetCachedABCWidth(LLastChar, LCharInfo) then
    begin
      LRealCharWidth := LCharInfo.abcA + Integer(LCharInfo.abcB);
      if LCharInfo.abcC >= 0 then
        Inc(LRealCharWidth, LCharInfo.abcC);
    end
    else
    if LLastChar < Ord(High(AnsiChar)) then
    begin
      GetTextMetricsA(FHandle, LTextMetricA);
      LRealCharWidth := LTextMetricA.tmAveCharWidth + LTextMetricA.tmOverhang;
    end;

    if LRealCharWidth > LNormalCharWidth then
      Inc(ARect.Right, LRealCharWidth - LNormalCharWidth);
    FExtTextOutDistance[ALength - 1] := Max(LRealCharWidth, LNormalCharWidth);
  end;

begin
  InitExtTextOutDistance(GetCharWidth);
  AdjustLastCharWidthAndRect;
  UniversalExtTextOut(FHandle, X, Y, AOptions, ARect, AText, ALength, FExtTextOutDistance);
end;

function TBCEditorTextDrawer.TextExtent(const Text: string): TSize;
begin
  Result := BCEditor.Utils.TextExtent(FStockBitmap.Canvas, Text);
end;

function TBCEditorTextDrawer.TextCharWidth(const AChar: PChar): Integer;
var
  LCharCode: Cardinal;
begin
  LCharCode := Ord(AChar^);
  if LCharCode <= High(FCharWidthCache) then
  begin
     Result := FCharWidthCache[LCharCode];
     if Result = 0 then
     begin
        Result := BCEditor.Utils.TextExtent(FStockBitmap.Canvas, AChar^).cX;
        FCharWidthCache[LCharCode] := Result;
     end;
     Exit;
  end;
  Result := BCEditor.Utils.TextExtent(FStockBitmap.Canvas, AChar^).cX;
end;

initialization

finalization

if Assigned(GFontsInfoManager) then
  GFontsInfoManager.Free;

end.
