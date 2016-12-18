unit BCEditor.PaintHelper;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, System.Math, System.Types, System.UITypes,
  BCEditor.Types;

const
  CFontStyleCount = Ord(High(TFontStyle)) + 1;
  CFontStyleCombineCount = 1 shl CFontStyleCount;

type
  TBCEditorStockFontPatterns = 0 .. CFontStyleCombineCount - 1;

  TBCEditorFontData = record
    Style: TFontStyles;
    Handle: HFont;
    CharWidth: Integer;
    CharHeight: Integer;
    FixedSize: Boolean;
  end;
  PBCEditorFontData = ^TBCEditorFontData;

  TBCEditorFontsData = array [TBCEditorStockFontPatterns] of TBCEditorFontData;

  TBCEditorSharedFontsInfo = record
    RefCount: Integer;
    LockCount: Integer;
    BaseFont: TFont;
    BaseLogFont: TLogFont;
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

  TBCEditorFontStock = class(TObject)
  strict private
    FBaseLogFont: TLogFont;
    FCurrentFont: HFont;
    FCurrentStyle: TFontStyles;
    FHandle: HDC;
    FHandleRefCount: Integer;
    FPCurrentFontData: PBCEditorFontData;
    FPSharedFontsInfo: PBCEditorSharedFontsInfo;
    FUsingFontHandles: Boolean;
    function GetBaseFont: TFont;
  protected
    function GetCharWidth: Integer;
    function GetCharHeight: Integer;
    function GetFixedSizeFont: Boolean;
    function GetFontData(AIndex: Integer): PBCEditorFontData;
    function InternalGetHandle: HDC;
    function InternalCreateFont(AStyle: TFontStyles): HFont;
    procedure CalculateFontMetrics(AHandle: HDC; ACharHeight: PInteger; ACharWidth: PInteger);
    procedure InternalReleaseDC(AValue: HDC);
    procedure ReleaseFontsInfo;
    procedure SetBaseFont(AValue: TFont);
    procedure SetStyle(const AValue: TFontStyles);
    procedure UseFontHandles;
    property FontData[AIndex: Integer]: PBCEditorFontData read GetFontData;
    property FontsInfo: PBCEditorSharedFontsInfo read FPSharedFontsInfo;
  public
    constructor Create(AInitialFont: TFont);
    destructor Destroy; override;

    procedure ReleaseFontHandles; virtual;
    property CharWidth: Integer read GetCharWidth;
    property BaseFont: TFont read GetBaseFont;
    property Style: TFontStyles read FCurrentStyle write SetStyle;
    property FontHandle: HFont read FCurrentFont;
  end;

  EBCEditorFontStockException = class(Exception);

  { TBCEditorPaintHelper }

  TBCEditorPaintHelper = class(TObject)
  strict private
    FBackgroundColor: TColor;
    FCharHeight: Integer;
    FCharWidth: Integer;
    FCalcExtentBaseStyle: TFontStyles;
    FColor: TColor;
    FCurrentFont: HFont;
    FDrawingCount: Integer;
    FFixedSizeFont: Boolean;
    FFontStock: TBCEditorFontStock;
    FHandle: HDC;
    FSaveHandle: Integer;
    FStockBitmap: Vcl.Graphics.TBitmap;
  protected
    property DrawingCount: Integer read FDrawingCount;
  public
    constructor Create(ACalcExtentBaseStyle: TFontStyles; ABaseFont: TFont);
    destructor Destroy; override;

    procedure BeginDrawing(AHandle: HDC);
    procedure EndDrawing;
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBaseFont(AValue: TFont);
    procedure SetBaseStyle(const AValue: TFontStyles);
    procedure SetForegroundColor(AValue: TColor);
    procedure SetStyle(const AValue: TFontStyles);
    property BackgroundColor: TColor read FBackgroundColor;
    property CharHeight: Integer read FCharHeight;
    property CharWidth: Integer read FCharWidth;
    property Color: TColor read FColor;
    property FixedSizeFont: Boolean read FFixedSizeFont;
    property FontStock: TBCEditorFontStock read FFontStock;
    property StockBitmap: Vcl.Graphics.TBitmap read FStockBitmap;
  end;

  EBCEditorPaintHelperException = class(Exception);

implementation

uses
  BCEditor.Utils, BCEditor.Consts, BCEditor.Language, System.Character;

var
  GFontsInfoManager: TBCEditorFontsInfoManager;

function GetFontsInfoManager: TBCEditorFontsInfoManager;
begin
  if not Assigned(GFontsInfoManager) then
    GFontsInfoManager := TBCEditorFontsInfoManager.Create;
  Result := GFontsInfoManager;
end;

{ TBCEditorFontsInfoManager }

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
  LFontData: TBCEditorFontData;
begin
  with ASharedFontsInfo^ do
  for i := Low(TBCEditorStockFontPatterns) to High(TBCEditorStockFontPatterns) do
  begin
    LFontData := FontsData[i];
    if LFontData.Handle <> 0 then
    begin
      DeleteObject(LFontData.Handle);
      LFontData.Handle := 0;
    end;
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

{ TBCEditorFontStock }

procedure TBCEditorFontStock.CalculateFontMetrics(AHandle: HDC; ACharHeight: PInteger; ACharWidth: PInteger);
var
  LTextMetric: TTextMetric;
  LCharInfo: TABC;
  LHasABC: Boolean;
begin
  GetTextMetrics(AHandle, LTextMetric);

  LHasABC := GetCharABCWidths(AHandle, Ord(' '), Ord(' '), LCharInfo);
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
    ACharWidth^ := abcA + Integer(abcB) + abcC + LTextMetric.tmOverhang;
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

function TBCEditorFontStock.GetCharWidth: Integer;
begin
  Result := FPCurrentFontData^.CharWidth;
end;

function TBCEditorFontStock.GetCharHeight: Integer;
begin
  Result := FPCurrentFontData^.CharHeight;
end;

function TBCEditorFontStock.GetFixedSizeFont: Boolean;
begin
  Result := FPCurrentFontData^.FixedSize;
end;

function TBCEditorFontStock.GetFontData(AIndex: Integer): PBCEditorFontData;
begin
  Result := @FPSharedFontsInfo^.FontsData[AIndex];
end;

function TBCEditorFontStock.InternalCreateFont(AStyle: TFontStyles): HFont;
const
  CBolds: array [Boolean] of Integer = (400, 700);
begin
  with FBaseLogFont do
  begin
    lfWeight := CBolds[fsBold in AStyle];
    lfItalic := Ord(BOOL(fsItalic in AStyle));
    lfUnderline := Ord(BOOL(fsUnderline in AStyle));
    lfStrikeOut := Ord(BOOL(fsStrikeOut in AStyle));
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
    raise EBCEditorFontStockException.Create(SBCEditorValueMustBeSpecified);
end;

procedure TBCEditorFontStock.SetStyle(const AValue: TFontStyles);
var
  LIndex: Integer;
  LHandle: HDC;
  LOldFont: HFont;
  LFontDataPointer: PBCEditorFontData;
  LSize1, LSize2: TSize;
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

  GetTextExtentPoint32(LHandle, 'W', 1, LSize1);
  GetTextExtentPoint32(LHandle, '!', 1, LSize2);

  with FPCurrentFontData^ do
  begin
    Handle := FCurrentFont;
    CalculateFontMetrics(LHandle, @CharHeight, @CharWidth);
    FixedSize := LSize1.cx = LSize2.cx;
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

{ TBCEditorPaintHelper }

constructor TBCEditorPaintHelper.Create(ACalcExtentBaseStyle: TFontStyles; ABaseFont: TFont);
begin
  inherited Create;

  FFontStock := TBCEditorFontStock.Create(ABaseFont);
  FStockBitmap := Vcl.Graphics.TBitmap.Create;
  FStockBitmap.Canvas.Brush.Color := clWhite;
  FCalcExtentBaseStyle := ACalcExtentBaseStyle;
  SetBaseFont(ABaseFont);
  FColor := clWindowText;
  FBackgroundColor := clWindow;
end;

destructor TBCEditorPaintHelper.Destroy;
begin
  FStockBitmap.Free;
  FFontStock.Free;

  inherited;
end;

procedure TBCEditorPaintHelper.BeginDrawing(AHandle: HDC);
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
  end;
  Inc(FDrawingCount);
end;

procedure TBCEditorPaintHelper.EndDrawing;
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

procedure TBCEditorPaintHelper.SetBaseFont(AValue: TFont);
begin
  if Assigned(AValue) then
  begin
    FStockBitmap.Canvas.Font.Assign(AValue);
    FStockBitmap.Canvas.Font.Style := [];
    with FFontStock do
    begin
      SetBaseFont(AValue);
      Style := FCalcExtentBaseStyle;
      FCharWidth := GetCharWidth;
      FCharHeight := GetCharHeight;
      FFixedSizeFont := GetFixedSizeFont;
    end;
    SetStyle(AValue.Style);
  end
  else
    raise EBCEditorPaintHelperException.Create(SBCEditorValueMustBeSpecified);
end;

procedure TBCEditorPaintHelper.SetBaseStyle(const AValue: TFontStyles);
begin
  if FCalcExtentBaseStyle <> AValue then
  begin
    FCalcExtentBaseStyle := AValue;
    with FFontStock do
    begin
      Style := AValue;
      FCharWidth := GetCharWidth;
      FCharHeight := GetCharHeight;
      FFixedSizeFont := GetFixedSizeFont;
    end;
  end;
end;

procedure TBCEditorPaintHelper.SetStyle(const AValue: TFontStyles);
begin
  with FFontStock do
  begin
    SetStyle(AValue);
    Self.FCurrentFont := FontHandle;
  end;
  FStockBitmap.Canvas.Font.Style := AValue;
  if FHandle <> 0 then
    SelectObject(FHandle, FCurrentFont);
end;

procedure TBCEditorPaintHelper.SetForegroundColor(AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    if FHandle <> 0 then
      SetTextColor(FHandle, ColorToRGB(AValue));
  end;
end;

procedure TBCEditorPaintHelper.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    if FHandle <> 0 then
      Winapi.Windows.SetBkColor(FHandle, ColorToRGB(AValue));
  end;
end;

initialization

finalization

  if Assigned(GFontsInfoManager) then
    GFontsInfoManager.Free;

end.