unit BCEditor.Print.Preview;

{$M+}

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils, Vcl.Controls, Vcl.Graphics, Vcl.Forms,
  BCEditor.Print;

type
  TBCEditorPreviewPageEvent = procedure(Sender: TObject; PageNumber: Integer) of object;
  TBCEditorPreviewScale = (pscWholePage, pscPageWidth, pscUserScaled);

  TBCEditorPrintPreview = class(TCustomControl)
  strict private
    FBorderStyle: TBorderStyle;
    FEditorPrint: TBCEditorPrint;
    FOnPreviewPage: TBCEditorPreviewPageEvent;
    FOnScaleChange: TNotifyEvent;
    FPageBackground: TColor;
    FPageNumber: Integer;
    FPageSize: TPoint;
    FScaleMode: TBCEditorPreviewScale;
    FScalePercent: Integer;
    FScrollPosition: TPoint;
    FShowScrollHint: Boolean;
    FWheelAccumulator: Integer;
    FVirtualOffset: TPoint;
    FVirtualSize: TPoint;
    function GetEditorPrint: TBCEditorPrint;
    function GetPageCount: Integer;
    function GetPageHeight100Percent: Integer;
    function GetPageHeightFromWidth(AWidth: Integer): Integer;
    function GetPageWidth100Percent: Integer;
    function GetPageWidthFromHeight(AHeight: Integer): Integer;
    procedure PaintPaper;
    procedure SetBorderStyle(AValue: TBorderStyle);
    procedure SetEditorPrint(AValue: TBCEditorPrint);
    procedure SetPageBackground(AValue: TColor);
    procedure SetScaleMode(AValue: TBCEditorPreviewScale);
    procedure SetScalePercent(AValue: Integer);
    procedure WMEraseBkgnd(var AMessage: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMHScroll(var AMessage: TWMHScroll); message WM_HSCROLL;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMSize(var AMessage: TWMSize); message WM_SIZE;
    procedure WMVScroll(var AMessage: TWMVScroll); message WM_VSCROLL;
  protected
    procedure CreateParams(var AParams: TCreateParams); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ScrollHorzFor(AValue: Integer);
    procedure ScrollHorzTo(AValue: Integer); virtual;
    procedure ScrollVertFor(AValue: Integer);
    procedure ScrollVertTo(AValue: Integer); virtual;
    procedure SizeChanged; virtual;
    procedure UpdateScrollbars; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure FirstPage;
    procedure LastPage;
    procedure NextPage;
    procedure Paint; override;
    procedure PreviousPage;
    procedure Print;
    procedure UpdatePreview;
    property PageCount: Integer read GetPageCount;
    property PageNumber: Integer read FPageNumber;
  published
    property Align default alClient;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color default clAppWorkspace;
    property Cursor;
    property EditorPrint: TBCEditorPrint read GetEditorPrint write SetEditorPrint;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnPreviewPage: TBCEditorPreviewPageEvent read FOnPreviewPage write FOnPreviewPage;
    property OnScaleChange: TNotifyEvent read FOnScaleChange write FOnScaleChange;
    property PageBackgroundColor: TColor read FPageBackground write SetPageBackground default clWhite;
    property PopupMenu;
    property ScaleMode: TBCEditorPreviewScale read FScaleMode write SetScaleMode default pscUserScaled;
    property ScalePercent: Integer read FScalePercent write SetScalePercent default 100;
    property ShowScrollHint: Boolean read FShowScrollHint write FShowScrollHint default True;
    property Visible default True;
  end;

implementation

uses
  System.Types, BCEditor.Language;

const
  MARGIN_WIDTH_LEFT_AND_RIGHT = 12;
  MARGIN_HEIGHT_TOP_AND_BOTTOM = 12;

{ TBCEditorPrintPreview }

constructor TBCEditorPrintPreview.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csNeedsBorderPaint];
  FBorderStyle := bsSingle;
  FScaleMode := pscUserScaled;
  FScalePercent := 100;
  FPageBackground := clWhite;
  Width := 200;
  Height := 120;
  ParentColor := False;
  Color := clAppWorkspace;
  Visible := True;
  FPageNumber := 1;
  FShowScrollHint := True;
  Align := alClient;
  FWheelAccumulator := 0;
end;

procedure TBCEditorPrintPreview.CreateParams(var AParams: TCreateParams);
const
  BorderStyles: array [TBorderStyle] of DWord = (0, WS_BORDER);
begin
  inherited;
  with AParams do
  begin
    Style := Style or WS_HSCROLL or WS_VSCROLL or BorderStyles[FBorderStyle] or WS_CLIPCHILDREN;
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

function TBCEditorPrintPreview.GetPageHeightFromWidth(AWidth: Integer): Integer;
begin
  if Assigned(FEditorPrint) then
  with FEditorPrint.PrinterInfo do
    Result := MulDiv(AWidth, PhysicalHeight, PhysicalWidth)
  else
    Result := MulDiv(AWidth, 141, 100);
end;

function TBCEditorPrintPreview.GetPageWidthFromHeight(AHeight: Integer): Integer;
begin
  if Assigned(FEditorPrint) then
  with FEditorPrint.PrinterInfo do
    Result := MulDiv(AHeight, PhysicalWidth, PhysicalHeight)
  else
    Result := MulDiv(AHeight, 100, 141);
end;

function TBCEditorPrintPreview.GetPageHeight100Percent: Integer;
var
  LHandle: HDC;
  LScreenDPI: Integer;
begin
  Result := 0;
  LHandle := GetDC(0);
  LScreenDPI := GetDeviceCaps(LHandle, LogPixelsY);
  ReleaseDC(0, LHandle);
  if Assigned(FEditorPrint) then
  with FEditorPrint.PrinterInfo do
    Result := MulDiv(PhysicalHeight, LScreenDPI, YPixPerInch);
end;

function TBCEditorPrintPreview.GetPageWidth100Percent: Integer;
var
  LHandle: HDC;
  LScreenDPI: Integer;
begin
  Result := 0;
  LHandle := GetDC(0);
  LScreenDPI := GetDeviceCaps(LHandle, LogPixelsX);
  ReleaseDC(0, LHandle);
  if Assigned(FEditorPrint) then
  with FEditorPrint.PrinterInfo do
    Result := MulDiv(PhysicalWidth, LScreenDPI, XPixPerInch);
end;

procedure TBCEditorPrintPreview.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FEditorPrint) then
    EditorPrint := nil;
end;

procedure TBCEditorPrintPreview.PaintPaper;
var
  LClipRect, PaperRect: TRect;
  PaperRGN: HRGN;
begin
  with Canvas do
  begin
    LClipRect := ClipRect;
    if IsRectEmpty(LClipRect) then
      Exit;
    Brush.Color := Self.Color;
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Pen.Width := 1;
    Pen.Style := psSolid;
    if (csDesigning in ComponentState) or (not Assigned(FEditorPrint)) then
    begin
      FillRect(LClipRect);
      Brush.Color := FPageBackground;
      Rectangle(MARGIN_WIDTH_LEFT_AND_RIGHT, MARGIN_HEIGHT_TOP_AND_BOTTOM, MARGIN_WIDTH_LEFT_AND_RIGHT + 30,
        MARGIN_HEIGHT_TOP_AND_BOTTOM + 43);
      Exit;
    end;
    with PaperRect do
    begin
      Left := FVirtualOffset.X + FScrollPosition.X;
      if ScaleMode = pscWholePage then
        Top := FVirtualOffset.Y
      else
        Top := FVirtualOffset.Y + FScrollPosition.Y;
      Right := Left + FPageSize.X;
      Bottom := Top + FPageSize.Y;
      PaperRGN := CreateRectRgn(Left, Top, Right + 1, Bottom + 1);
    end;
    if NULLREGION <> ExtSelectClipRgn(Handle, PaperRGN, RGN_DIFF) then
      FillRect(LClipRect);
    SelectClipRgn(Handle, PaperRGN);
    Brush.Color := FPageBackground;
    with PaperRect do
      Rectangle(Left, Top, Right + 1, Bottom + 1);
    DeleteObject(PaperRGN);
  end;
end;

procedure TBCEditorPrintPreview.Paint;
var
  OriginalScreenPoint: TPoint;
begin
  with Canvas do
  begin
    PaintPaper;
    if (csDesigning in ComponentState) or (not Assigned(FEditorPrint)) then
      Exit;
    SetMapMode(Handle, MM_ANISOTROPIC);
    with FEditorPrint.PrinterInfo do
    begin
      SetWindowExtEx(Handle, PhysicalWidth, PhysicalHeight, nil);
      SetViewPortExtEx(Handle, FPageSize.X, FPageSize.Y, nil);
      OriginalScreenPoint.X := MulDiv(LeftMargin, FPageSize.X, PhysicalWidth);
      OriginalScreenPoint.Y := MulDiv(TopMargin, FPageSize.Y, PhysicalHeight);
      Inc(OriginalScreenPoint.X, FVirtualOffset.X + FScrollPosition.X);
      if ScaleMode = pscWholePage then
        Inc(OriginalScreenPoint.Y, FVirtualOffset.Y)
      else
        Inc(OriginalScreenPoint.Y, FVirtualOffset.Y + FScrollPosition.Y);
      SetViewPortOrgEx(Handle, OriginalScreenPoint.X, OriginalScreenPoint.Y, nil);
      IntersectClipRect(Handle, 0, 0, PrintableWidth, PrintableHeight);
    end;
    FEditorPrint.PrintToCanvas(Canvas, FPageNumber);
  end;
end;

procedure TBCEditorPrintPreview.ScrollHorzFor(AValue: Integer);
begin
  ScrollHorzTo(FScrollPosition.X + AValue);
end;

procedure TBCEditorPrintPreview.ScrollHorzTo(AValue: Integer);
var
  LWidth, Position: Integer;
begin
  LWidth := ClientWidth;
  Position := LWidth - FVirtualSize.X;
  if AValue < Position then
    AValue := Position;
  if AValue > 0 then
    AValue := 0;
  if FScrollPosition.X <> AValue then
  begin
    Position := AValue - FScrollPosition.X;
    FScrollPosition.X := AValue;
    UpdateScrollbars;
    if Abs(Position) > LWidth div 2 then
      Invalidate
    else
    begin
      ScrollWindow(Handle, Position, 0, nil, nil);
      Update;
    end;
  end;
end;

procedure TBCEditorPrintPreview.ScrollVertFor(AValue: Integer);
begin
  ScrollVertTo(FScrollPosition.Y + AValue);
end;

procedure TBCEditorPrintPreview.ScrollVertTo(AValue: Integer);
var
  LHeight, Position: Integer;
begin
  LHeight := ClientHeight;
  Position := LHeight - FVirtualSize.Y;
  if AValue < Position then
    AValue := Position;
  if AValue > 0 then
    AValue := 0;
  if FScrollPosition.Y <> AValue then
  begin
    Position := AValue - FScrollPosition.Y;
    FScrollPosition.Y := AValue;
    UpdateScrollbars;
    if Abs(Position) > LHeight div 2 then
      Invalidate
    else
    begin
      ScrollWindow(Handle, 0, Position, nil, nil);
      Update;
    end;
  end;
end;

procedure TBCEditorPrintPreview.SizeChanged;
var
  LWidth: Integer;
begin
  if not (HandleAllocated and Assigned(FEditorPrint)) then
    Exit;
  // compute paper size
  case FScaleMode of
    pscWholePage:
      begin
        FPageSize.X := ClientWidth - 2 * MARGIN_WIDTH_LEFT_AND_RIGHT;
        FPageSize.Y := ClientHeight - 2 * MARGIN_HEIGHT_TOP_AND_BOTTOM;
        LWidth := GetPageWidthFromHeight(FPageSize.Y);
        if LWidth < FPageSize.X then
          FPageSize.X := LWidth
        else
          FPageSize.Y := GetPageHeightFromWidth(FPageSize.X);
      end;
    pscPageWidth:
      begin
        FPageSize.X := ClientWidth - 2 * MARGIN_WIDTH_LEFT_AND_RIGHT;
        FPageSize.Y := GetPageHeightFromWidth(FPageSize.X);
      end;
    pscUserScaled:
      begin
        FPageSize.X := MulDiv(GetPageWidth100Percent, FScalePercent, 100);
        FPageSize.Y := MulDiv(GetPageHeight100Percent, FScalePercent, 100);
      end;
  end;
  FVirtualSize.X := FPageSize.X + 2 * MARGIN_WIDTH_LEFT_AND_RIGHT;
  FVirtualSize.Y := FPageSize.Y + 2 * MARGIN_HEIGHT_TOP_AND_BOTTOM;
  FVirtualOffset.X := MARGIN_WIDTH_LEFT_AND_RIGHT;
  if FVirtualSize.X < ClientWidth then
    Inc(FVirtualOffset.X, (ClientWidth - FVirtualSize.X) div 2);
  FVirtualOffset.Y := MARGIN_HEIGHT_TOP_AND_BOTTOM;
  if FVirtualSize.Y < ClientHeight then
    Inc(FVirtualOffset.Y, (ClientHeight - FVirtualSize.Y) div 2);
  UpdateScrollbars;

  FScrollPosition.X := 0;
  FScrollPosition.Y := 0;
end;

procedure TBCEditorPrintPreview.UpdateScrollbars;
var
  ScrollInfo: TScrollInfo;
begin
  FillChar(ScrollInfo, SizeOf(TScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  case FScaleMode of
    pscWholePage:
      begin
        ShowScrollbar(Handle, SB_HORZ, False);
        ScrollInfo.fMask := ScrollInfo.fMask or SIF_DISABLENOSCROLL;
        ScrollInfo.nMin := 1;
        if Assigned(FEditorPrint) then
        begin
          ScrollInfo.nMax := FEditorPrint.PageCount;
          ScrollInfo.nPos := FPageNumber;
        end
        else
        begin
          ScrollInfo.nMax := 1;
          ScrollInfo.nPos := 1;
        end;
        ScrollInfo.nPage := 1;
        SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
      end;
    pscPageWidth:
      begin
        ShowScrollbar(Handle, SB_HORZ, False);
        ScrollInfo.fMask := ScrollInfo.fMask or SIF_DISABLENOSCROLL;
        ScrollInfo.nMax := FVirtualSize.Y;
        ScrollInfo.nPos := -FScrollPosition.Y;
        ScrollInfo.nPage := ClientHeight;
        SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
      end;
    pscUserScaled:
      begin
        ShowScrollbar(Handle, SB_HORZ, True);
        ShowScrollbar(Handle, SB_VERT, True);
        ScrollInfo.fMask := ScrollInfo.fMask or SIF_DISABLENOSCROLL;
        ScrollInfo.nMax := FVirtualSize.X;
        ScrollInfo.nPos := -FScrollPosition.X;
        ScrollInfo.nPage := ClientWidth;
        SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
        ScrollInfo.nMax := FVirtualSize.Y;
        ScrollInfo.nPos := -FScrollPosition.Y;
        ScrollInfo.nPage := ClientHeight;
        SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
      end;
  end;
end;

procedure TBCEditorPrintPreview.SetBorderStyle(AValue: TBorderStyle);
begin
  if FBorderStyle <> AValue then
  begin
    FBorderStyle := AValue;
    RecreateWnd;
  end;
end;

procedure TBCEditorPrintPreview.SetPageBackground(AValue: TColor);
begin
  if FPageBackground <> AValue then
  begin
    FPageBackground := AValue;
    Invalidate;
  end;
end;

function TBCEditorPrintPreview.GetEditorPrint: TBCEditorPrint;
begin
  if not Assigned(FEditorPrint) then
    FEditorPrint := TBCEditorPrint.Create(Self);
  Result := FEditorPrint
end;

procedure TBCEditorPrintPreview.SetEditorPrint(AValue: TBCEditorPrint);
begin
  if FEditorPrint <> AValue then
  begin
    FEditorPrint := AValue;
    if Assigned(FEditorPrint) then
      FEditorPrint.FreeNotification(Self);
  end;
end;

procedure TBCEditorPrintPreview.SetScaleMode(AValue: TBCEditorPreviewScale);
begin
  if FScaleMode <> AValue then
  begin
    FScaleMode := AValue;
    FScrollPosition := Point(0, 0);
    SizeChanged;
    if Assigned(FOnScaleChange) then
      FOnScaleChange(Self);
    Invalidate;
  end;
end;

procedure TBCEditorPrintPreview.SetScalePercent(AValue: Integer);
begin
  if FScalePercent <> AValue then
  begin
    FScaleMode := pscUserScaled;
    FScrollPosition := Point(0, 0);
    FScalePercent := AValue;
    SizeChanged;
    Invalidate;
  end
  else
    ScaleMode := pscUserScaled;
  if Assigned(FOnScaleChange) then
    FOnScaleChange(Self);
end;

procedure TBCEditorPrintPreview.WMEraseBkgnd(var AMessage: TWMEraseBkgnd);
begin
  AMessage.Result := 1;
end;

procedure TBCEditorPrintPreview.WMHScroll(var AMessage: TWMHScroll);
var
  LWidth: Integer;
begin
  if FScaleMode <> pscWholePage then
  begin
    LWidth := ClientWidth;
    case AMessage.ScrollCode of
      SB_TOP:
        ScrollHorzTo(0);
      SB_BOTTOM:
        ScrollHorzTo(-FVirtualSize.X);
      SB_LINEDOWN:
        ScrollHorzFor(-(LWidth div 10));
      SB_LINEUP:
        ScrollHorzFor(LWidth div 10);
      SB_PAGEDOWN:
        ScrollHorzFor(-(LWidth div 2));
      SB_PAGEUP:
        ScrollHorzFor(LWidth div 2);
      SB_THUMBPOSITION, SB_THUMBTRACK:
        ScrollHorzTo(-AMessage.Pos);
    end;
  end;
end;

procedure TBCEditorPrintPreview.WMSize(var AMessage: TWMSize);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    SizeChanged;
end;

var
  ScrollHintWnd: THintWindow;

function GetScrollHint: THintWindow;
begin
  if not Assigned(ScrollHintWnd) then
  begin
    ScrollHintWnd := HintWindowClass.Create(Application);
    ScrollHintWnd.Visible := False;
  end;
  Result := ScrollHintWnd;
end;

procedure TBCEditorPrintPreview.WMVScroll(var AMessage: TWMVScroll);
var
  LHeight: Integer;
  S: string;
  ScrollHintRect: TRect;
  LPoint: TPoint;
  ScrollHint: THintWindow;
begin
  if (FScaleMode = pscWholePage) then
  begin
    if Assigned(FEditorPrint) then
      case AMessage.ScrollCode of
        SB_TOP:
          FPageNumber := 1;
        SB_BOTTOM:
          FPageNumber := FEditorPrint.PageCount;
        SB_LINEDOWN, SB_PAGEDOWN:
          begin
            FPageNumber := FPageNumber + 1;
            if FPageNumber > FEditorPrint.PageCount then
              FPageNumber := FEditorPrint.PageCount;
          end;
        SB_LINEUP, SB_PAGEUP:
          begin
            FPageNumber := FPageNumber - 1;
            if FPageNumber < 1 then
              FPageNumber := 1;
          end;
        SB_THUMBPOSITION, SB_THUMBTRACK:
          begin
            FPageNumber := AMessage.Pos;
            if FShowScrollHint then
            begin
              ScrollHint := GetScrollHint;
              if not ScrollHint.Visible then
              begin
                ScrollHint.Color := Application.HintColor;
                ScrollHint.Visible := True;
              end;
              S := Format(SBCEditorPreviewScrollHint, [FPageNumber]);
              ScrollHintRect := ScrollHint.CalcHintRect(200, S, nil);
              LPoint := ClientToScreen(Point(ClientWidth - ScrollHintRect.Right - 4, 10));
              OffsetRect(ScrollHintRect, LPoint.X, LPoint.Y);
              ScrollHint.ActivateHint(ScrollHintRect, S);
              SendMessage(ScrollHint.Handle, WM_NCPAINT, 1, 0);
              ScrollHint.Invalidate;
              ScrollHint.Update;
            end;
          end;
        SB_ENDSCROLL:
          begin
            if FShowScrollHint then
            begin
              ScrollHint := GetScrollHint;
              ScrollHint.Visible := False;
              ShowWindow(ScrollHint.Handle, SW_HIDE);
            end;
          end;
      end;
    FScrollPosition.Y := -(FPageNumber - 1);
    UpdateScrollbars;
    if Assigned(FOnPreviewPage) then
      FOnPreviewPage(Self, FPageNumber);
    Invalidate;
  end
  else
  begin
    LHeight := ClientHeight;
    case AMessage.ScrollCode of
      SB_TOP:
        ScrollVertTo(0);
      SB_BOTTOM:
        ScrollVertTo(-FVirtualSize.Y);
      SB_LINEDOWN:
        ScrollVertFor(-(LHeight div 10));
      SB_LINEUP:
        ScrollVertFor(LHeight div 10);
      SB_PAGEDOWN:
        ScrollVertFor(-(LHeight div 2));
      SB_PAGEUP:
        ScrollVertFor(LHeight div 2);
      SB_THUMBPOSITION, SB_THUMBTRACK:
        ScrollVertTo(-AMessage.Pos);
    end;
  end;
end;

procedure TBCEditorPrintPreview.WMMouseWheel(var Message: TWMMouseWheel);
var
  CtrlPressed: Boolean;

  procedure MouseWheelUp;
  begin
    if CtrlPressed and (FPageNumber > 1) then
      PreviousPage
    else
      ScrollVertFor(WHEEL_DELTA);
  end;

  procedure MouseWheelDown;
  begin
    if CtrlPressed and (FPageNumber < PageCount) then
      NextPage
    else
      ScrollVertFor(-WHEEL_DELTA);
  end;

var
  IsNegative: Boolean;
begin
  CtrlPressed := GetKeyState(VK_CONTROL) < 0;

  Inc(FWheelAccumulator, message.WheelDelta);

  while Abs(FWheelAccumulator) >= WHEEL_DELTA do
  begin
    IsNegative := FWheelAccumulator < 0;
    FWheelAccumulator := Abs(FWheelAccumulator) - WHEEL_DELTA;
    if IsNegative then
    begin
      if FWheelAccumulator <> 0 then
        FWheelAccumulator := -FWheelAccumulator;
      MouseWheelDown;
    end
    else
      MouseWheelUp;
  end;
end;

procedure TBCEditorPrintPreview.UpdatePreview;
var
  OldScale: Integer;
  OldMode: TBCEditorPreviewScale;
begin
  OldScale := ScalePercent;
  OldMode := ScaleMode;
  ScalePercent := 100;
  if Assigned(FEditorPrint) then
    FEditorPrint.UpdatePages(Canvas);
  SizeChanged;
  Invalidate;
  ScaleMode := OldMode;
  if ScaleMode = pscUserScaled then
    ScalePercent := OldScale;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
end;

procedure TBCEditorPrintPreview.FirstPage;
begin
  FPageNumber := 1;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
  Invalidate;
end;

procedure TBCEditorPrintPreview.LastPage;
begin
  if Assigned(FEditorPrint) then
    FPageNumber := FEditorPrint.PageCount;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
  Invalidate;
end;

procedure TBCEditorPrintPreview.NextPage;
begin
  FPageNumber := FPageNumber + 1;
  if Assigned(FEditorPrint) and (FPageNumber > FEditorPrint.PageCount) then
    FPageNumber := FEditorPrint.PageCount;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
  Invalidate;
end;

procedure TBCEditorPrintPreview.PreviousPage;
begin
  FPageNumber := FPageNumber - 1;
  if Assigned(FEditorPrint) and (FPageNumber < 1) then
    FPageNumber := 1;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
  Invalidate;
end;

procedure TBCEditorPrintPreview.Print;
begin
  if Assigned(FEditorPrint) then
  begin
    FEditorPrint.Print;
    UpdatePreview;
  end;
end;

function TBCEditorPrintPreview.GetPageCount: Integer;
begin
  Result := EditorPrint.PageCount;
end;

end.
