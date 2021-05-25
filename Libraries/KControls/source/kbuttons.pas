{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kbuttons; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF FPC}
    LCLType, LCLIntf, LMessages, LCLProc, LResources,
  {$ELSE}
    Windows, Messages,
  {$ENDIF}
    SysUtils, Classes, Controls, Forms, Graphics,
    ActnList, StdCtrls, Dialogs, Buttons, ImgList,
    KFunctions, KGraphics, KControls
  {$IFDEF USE_THEMES}
    , Themes
   {$IFNDEF FPC}
    , UxTheme
   {$ENDIF}
  {$ENDIF}
    ;

const
  cGlyphUp = 0;
  cGlyphDisabled = 1;
  cGlyphClicked = 2;
  cGlyphDown = 3;

type
  TKButtonState = (cbsPressed, cbsWasPressed, cbsMouseCapture, cbsFocused, cbsLostFocus, cbsHot, cbsCheck);

  TKButtonStates = set of TKButtonState;

  { Base control for KControls buttons. }

  { TKButtonControl }

  TKButtonControl = class(TKCustomControl)
  private
    FCancel: Boolean;
    FDefault: Boolean;
    FFocusRect: Boolean;
    FHAlign: TKHAlign;
    FModalResult: TModalResult;
    FVAlign: TKVAlign;
    FWordWrap: Boolean;
    procedure WMSetFocus(var Msg: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Msg: TLMKillFocus); message LM_KILLFOCUS;
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMFocusChanged(var Msg: TLMessage); message CM_FOCUSCHANGED;
    procedure CMMouseEnter(var Msg: TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TLMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Msg: TLMessage); message CM_TEXTCHANGED;
    function GetDown: Boolean;
    function GetIsCheck: Boolean;
    procedure SetCancel(AValue: Boolean);
    procedure SetDefault(AValue: Boolean);
    procedure SetDown(const Value: Boolean);
    procedure SetFocusRect(const Value: Boolean);
    procedure SetHAlign(const Value: TKHAlign);
    procedure SetIsCheck(const Value: Boolean);
    procedure SetModalResult(AValue: TModalResult);
    procedure SetVAlign(const Value: TKVAlign);
    procedure SetWordWrap(const Value: Boolean);
  protected
    FStates: TKButtonStates;
    procedure DrawCaption(ACanvas: TCanvas; const ARect: TRect); virtual;
    procedure DrawFocusRect(ACanvas: TCanvas; ARect: TRect); virtual;
    procedure DrawInterior(ACanvas: TCanvas; ARect: TRect); virtual; abstract;
    function GetSkinned: Boolean; virtual;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    procedure UpdateSize; override;
    procedure WndProc(var Msg: TLMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Invalidate; override;
    property Cancel: Boolean read FCancel write SetCancel default false;
    property Default: Boolean read FDefault write SetDefault default false;
    property HAlign: TKHAlign read FHAlign write SetHAlign default halCenter;
    property ModalResult: TModalResult read FModalResult write SetModalResult default mrNone;
    property Skinned: Boolean read GetSkinned;
    property VAlign: TKVAlign read FVAlign write SetVAlign default valCenter;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  published
    property Action;
    property Anchors;
    property Caption;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Down: Boolean read GetDown write SetDown default False;
    property FocusRect: Boolean read FFocusRect write SetFocusRect default True;
    property Enabled;
    property Font;
    property IsCheck: Boolean read GetIsCheck write SetIsCheck default False;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
  {$IFDEF COMPILER9_UP}
    property OnMouseEnter;
    property OnMouseLeave;
  {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{$IFDEF FPC}
  TColorDialogOption = (cdFullOpen, cdPreventFullOpen, cdShowHelp, cdSolidColor, cdAnyColor);

  TColorDialogOptions = set of TColorDialogOption;
{$ENDIF}

  { Custom drawn TBitBtn using TKAlphaBitmap as Glyph. }
  TKBitBtn = class(TKButtonControl)
  private
    FLayout: TButtonLayout;
    FKind: TBitBtnKind;
    FSpacing: Integer;
    FMargin: Integer;
    FMaskGlyph: Boolean;
    FNumGlyphs: Integer;
    FGlyph: TKAlphaBitmap;
    function GetGlyph: TKAlphaBitmap;
    function IsGlyphStored: Boolean;
    procedure SetGlyph(const Value: TKAlphaBitmap);
    procedure SetKind(Value: TBitBtnKind);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetMargin(Value: Integer);
    procedure SetNumGlyphs(Value: Integer);
    procedure SetSpacing(Value: Integer);
    procedure SetMaskGlyph(const Value: Boolean);
  protected
    FGlyphs: array of TKAlphaBitmap;
    FGrayedGlyph: TKAlphaBitmap;
    procedure GlyphChange(Sender: TObject);
    procedure DrawInterior(ACanvas: TCanvas; ARect: TRect); override;
    procedure UpdateGlyphs; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MaskGlyph: Boolean read FMaskGlyph write SetMaskGlyph;
  published
    property Cancel;
    property Default;
    property Glyph: TKAlphaBitmap read GetGlyph write SetGlyph stored IsGlyphStored;
    property Kind: TBitBtnKind read FKind write SetKind default bkCustom;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default 3;
    property ModalResult;
    property NumGlyphs: Integer read FNumGlyphs write SetNumGlyphs default 1;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property WordWrap;
  end;

  { Custom drawn TColorButton using rectangle as color indication. }
  TKColorButton = class(TKButtonControl)
  private
    FDlgColor: TColor;
    FColorDlgOptions: TColorDialogOptions;
    procedure SetDlgColor(Value: TColor);
  protected
    procedure DrawInterior(ACanvas: TCanvas; ARect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  published
    property DlgColor: TColor read FDlgColor write SetDlgColor default clRed;
    property ColorDlgOptions: TColorDialogOptions read FColorDlgOptions write FColorDlgOptions;
  end;

  { Custom drawn TSpeedBtn using TKAlphaBitmap as Glyph. }
  TKSpeedButton = class(TKButtonControl)
  private
    FGlyph: TKAlphaBitmap;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    function GetGlyph: TKAlphaBitmap;
    function IsGlyphStored: Boolean;
    procedure SetGlyph(const Value: TKAlphaBitmap);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageIndex(Value: TImageIndex);
  protected
    procedure GlyphChange(Sender: TObject);
    procedure DrawInterior(ACanvas: TCanvas; ARect: TRect); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Cancel;
    property Default;
    property Glyph: TKAlphaBitmap read GetGlyph write SetGlyph stored IsGlyphStored;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ModalResult;
  end;

implementation

uses
  Math, Types;

{ TKButtonControl }

constructor TKButtonControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  ControlStyle := [csClickEvents, csCaptureMouse];
  Font.Color := clBtnText;
  Width := 70;
  Height := 25;
  TabStop := True;
  FCancel := False;
  FDefault := False;
  FFocusRect := True;
  FHAlign := halCenter;
  FVAlign := valCenter;
  FModalResult := mrNone;
  FStates := [];
  FWordWrap := False;
end;

destructor TKButtonControl.Destroy;
begin
  inherited;
end;

procedure TKButtonControl.AfterConstruction;
begin
  inherited;
end;

procedure TKButtonControl.CMDialogChar(var Msg: TCMDialogChar);
begin
  with Msg do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TKButtonControl.CMDialogKey(var Msg: TCMDialogKey);
begin
  with Msg do
    if (KeyDataToShiftState(KeyData) = []) and CanFocus and
    {$IFDEF FPC}
      (CharCode = VK_RETURN) and (cbsFocused in FStates)
    {$ELSE}
      (((CharCode = VK_RETURN) and (FDefault or (cbsFocused in FStates))) or
      ((CharCode = VK_ESCAPE) and FCancel))
    {$ENDIF}
       then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TKButtonControl.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TKButtonControl.CMFocusChanged(var Msg: TLMessage);
begin
  inherited;
  Exclude(FStates, cbsLostFocus);
end;

procedure TKButtonControl.CMMouseEnter(var Msg: TLMessage);
begin
  inherited;
  if Enabled then
    Include(FStates, cbsHot);
  Invalidate;
end;

procedure TKButtonControl.CMMouseLeave(var Msg: TLMessage);
begin
  inherited;
  if Enabled then
    Exclude(FStates, cbsHot);
  Invalidate;
end;

procedure TKButtonControl.CMTextChanged(var Msg: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TKButtonControl.DrawCaption(ACanvas: TCanvas; const ARect: TRect);
var
  TB: TKTextBox;
begin
  TB := TKTextBox.Create;
  try
    ACanvas.Font := Font;
    if not (Enabled or Skinned) then
      ACanvas.Font.Color := clGrayText;
    TB.HAlign := FHAlign;
    TB.VAlign := FVAlign;
    TB.Attributes := [taTrimWhiteSpaces];
    if FWordWrap then
      TB.Attributes := TB.Attributes + [taWordBreak];
    TB.Text := Caption;
    TB.Draw(ACanvas, ARect);
  finally
    TB.Free;
  end;
end;

procedure TKButtonControl.DrawFocusRect(ACanvas: TCanvas; ARect: TRect);
begin
  if cbsFocused in FStates then with ACanvas do
  begin
    InflateRect(ARect, -3, -3);
    Pen.Color := clWindowFrame;
    Brush.Color := clBtnFace;
    SetBkColor(Handle, $FFFFFF);
    SetTextColor(Handle, 0);
    DrawFocusRect(ARect);
  end;
end;

function TKButtonControl.GetDown: Boolean;
begin
  Result := cbsPressed in FStates;
end;

function TKButtonControl.GetIsCheck: Boolean;
begin
  Result := cbsCheck in FStates;
end;

function TKButtonControl.GetSkinned: Boolean;
begin
  Result := False;
end;

procedure TKButtonControl.Invalidate;
begin
  inherited;
end;

procedure TKButtonControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_RETURN then Click;
end;

procedure TKButtonControl.Loaded;
begin
  inherited;
end;

procedure TKButtonControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Enabled and (Button = mbLeft) then
  begin
    SetFocus;
    Invalidate;
    if cbsPressed in FStates then
      Include(FStates, cbsWasPressed);
    FStates := FStates + [cbsMouseCapture, cbsPressed];
  end;
end;

procedure TKButtonControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  if (cbsMouseCapture in FStates) then
  begin
    P := Point(X, Y);
    if PtInRect(ClientRect, P) then
    begin
      if FStates * [cbsPressed, cbsCheck] = [] then
      begin
        Include(FStates, cbsPressed);
        Invalidate;
      end;
    end else
      if cbsPressed in FStates then
      begin
        Exclude(FStates, cbsPressed);
        Invalidate;
      end;
  end;
end;

procedure TKButtonControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if cbsPressed in FStates then
  begin
    if not (cbsCheck in FStates) or (cbsWasPressed in FStates) then
      Exclude(FStates, cbsPressed);
    Invalidate;
  end;
  Exclude(FStates, cbsWasPressed);
  Exclude(FStates, cbsMouseCapture);
  inherited;
end;

procedure TKButtonControl.PaintToCanvas(ACanvas: TCanvas);
var
  Ofs: Integer;
  R: TRect;
  States: TKButtonDrawStates;
begin
  R := ClientRect;
  if cbsPressed in FStates then
    Ofs := 1
  else
    Ofs := 0;
  States := [];
  if ThemeServices.ThemesEnabled then
    Include(States, bsUseThemes);
  if not Enabled then
    Include(States, bsDisabled);
  if cbsPressed in FStates then
    Include(States, bsPressed);
  if (cbsFocused in FStates) or Default then
    Include(States, bsFocused);
  if FStates * [cbsHot, cbsMouseCapture] <> [] then
    Include(States, bsHot);
  KGraphics.DrawButtonFrame(ACanvas, R, States);
  KFunctions.OffsetRect(R, Ofs, Ofs);
  DrawInterior(ACanvas, R);
  KFunctions.OffsetRect(R, -Ofs, -Ofs);
  if FFocusRect then
    DrawFocusRect(ACanvas, R);
end;

procedure TKButtonControl.SetCancel(AValue: Boolean);
{$IFDEF FPC}
var
  Form: TCustomForm;
{$ENDIF}
begin
  if FCancel = AValue then Exit;
  FCancel := AValue;
{$IFDEF FPC}
  Form := GetParentForm(Self);
  if Assigned(Form) then
  begin
    if AValue then
      Form.CancelControl := Self
    else
      Form.CancelControl := nil;
  end;
{$ENDIF}
end;

procedure TKButtonControl.SetDefault(AValue: Boolean);
{$IFDEF FPC}
var
  Form: TCustomForm;
{$ENDIF}
begin
  if FDefault = AValue then Exit;
  FDefault := AValue;
{$IFDEF FPC}
  Form := GetParentForm(Self);
  if Assigned(Form) then
  begin
    if AValue then
    begin
      Form.DefaultControl := Self;
    end else
    begin
      if Form.DefaultControl = Self then
        Form.DefaultControl := nil;
     end;
  end;
{$ENDIF}
end;

procedure TKButtonControl.SetDown(const Value: Boolean);
begin
  if IsCheck then
  begin
    if Value <> Down then
    begin
      if Value then
        Include(FStates, cbsPressed)
      else
        Exclude(FStates, cbsPressed);
      Invalidate;
    end;
  end;
end;

procedure TKButtonControl.SetFocusRect(const Value: Boolean);
begin
  if Value <> FFocusRect then
  begin
    FFocusRect := Value;
    Invalidate;
  end;
end;

procedure TKButtonControl.SetHAlign(const Value: TKHAlign);
begin
  if Value <> FHAlign then
  begin
    FHAlign := Value;
    Invalidate;
  end;
end;

procedure TKButtonControl.SetIsCheck(const Value: Boolean);
begin
  if Value then
    Include(FStates, cbsCheck)
  else
    Exclude(FStates, cbsCheck);
end;

procedure TKButtonControl.SetModalResult(AValue: TModalResult);
begin
  if AValue = FModalResult then Exit;
  FModalResult := AValue;
end;

procedure TKButtonControl.SetVAlign(const Value: TKVAlign);
begin
  if Value <> FVAlign then
  begin
    FVAlign := Value;
    Invalidate;
  end;
end;

procedure TKButtonControl.SetWordWrap(const Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;

procedure TKButtonControl.UpdateSize;
begin
  Invalidate;
end;

procedure TKButtonControl.WMKillFocus(var Msg: TLMKillFocus);
begin
  inherited;
  Exclude(FStates, cbsFocused);
  Include(FStates, cbsLostFocus);
  Invalidate;
end;

procedure TKButtonControl.WMSetFocus(var Msg: TLMSetFocus);
begin
  inherited;
  Include(FStates, cbsFocused);
  Invalidate;
end;

procedure TKButtonControl.WndProc(var Msg: TLMessage);
begin
  inherited;
end;

{ TKBitBtn }

constructor TKBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph := TKAlphaBitmap.Create;
  FGlyph.OnChange := GlyphChange;
  FGlyphs := nil;
  FGrayedGlyph := nil;
  FKind := bkCustom;
  FLayout := blGlyphLeft;
  FMargin := 3;
  FMaskGlyph := False;
  FNumGlyphs := 1;
  FSpacing := 4;
  UpdateGlyphs;
end;

destructor TKBitBtn.Destroy;
begin
  FNumGlyphs := 0;
  UpdateGlyphs;
  FGlyph.Free;
  FGrayedGlyph.Free;
  inherited;
end;

function TKBitBtn.GetGlyph: TKAlphaBitmap;
begin
  Result := FGlyph;
end;

procedure TKBitBtn.GlyphChange(Sender: TObject);
begin
  UpdateGlyphs;
  Invalidate;
end;

function TKBitBtn.IsGlyphStored: Boolean;
begin
  Result := (Kind = bkCustom) and not FGlyph.Empty and
    (FGlyph.Width > 0) and (FGlyph.Height > 0);
end;

procedure TKBitBtn.DrawInterior(ACanvas: TCanvas; ARect: TRect);
var
  InteriorRect, TextRect, ModRect: TRect;
  GlyphPos, GlyphSize, TextSize, ModSize: TPoint;
  TB: TKTextBox;
  BM: TKAlphaBitmap;
begin
  InteriorRect := ARect;
  if FMargin > 0 then
    InflateRect(InteriorRect, -FMargin, -FMargin);
  if Length(FGlyphs) > 0 then
  begin
    GlyphSize.X := FGlyphs[cGlyphUp].Width;
    GlyphSize.Y := FGlyphs[cGlyphUp].Height;
    if (GlyphSize.X > 0) and (GlyphSize.Y > 0) then
    begin
      TB := TKTextBox.Create;
      try
        ACanvas.Font := Font;
        if not Enabled then
          ACanvas.Font.Color := clGrayText;
        TB.HAlign := HAlign;
        TB.VAlign := VAlign;
        TB.Attributes := [taTrimWhiteSpaces];
        if WordWrap then
          TB.Attributes := TB.Attributes + [taWordBreak];
        TB.Text := Caption;
        TextRect := InteriorRect;
        // first measure text extent
        case FLayout of
          blGlyphLeft: Inc(TextRect.Left, GlyphSize.X + FSpacing);
          blGlyphRight: Dec(TextRect.Right, GlyphSize.X + FSpacing);
        end;
        TB.Measure(ACanvas, TextRect, TextSize.X, TextSize.Y);
        if TextSize.X > TextRect.Right - TextRect.Left then
        begin
          // measure again to get correct height because text is wider than box
          TextRect.Right := TextRect.Left + TextSize.X;
          TB.Measure(ACanvas, TextRect, TextSize.X, TextSize.Y);
        end;
        // then compute glyph position and text rect
        case FLayout of
          blGlyphLeft, blGlyphRight:
          begin
            ModSize.X := GlyphSize.X + FSpacing + TextSize.X;
            ModSize.Y := TextSize.Y;
          end;
          blGlyphTop, blGlyphBottom:
          begin
            ModSize.X := TextSize.X;
            ModSize.Y := GlyphSize.Y + FSpacing + TextSize.Y;
          end;
        end;
        ModRect.Left := HorizontalShapePosition(HAlign, InteriorRect, ModSize);
        ModRect.Top := VerticalShapePosition(VAlign, InteriorRect, ModSize);
        ModRect.Right := ModRect.Left + ModSize.X;
        ModRect.Bottom := ModRect.Top + ModSize.Y;
        TextRect := ModRect;
        case FLayout of
          blGlyphLeft:
          begin
            Inc(TextRect.Left, GlyphSize.X + FSpacing);
            GlyphPos.X := ModRect.Left;
            GlyphPos.Y := VerticalShapePosition(VAlign, ModRect, GlyphSize);
          end;
          blGlyphRight:
          begin
            Dec(TextRect.Right, GlyphSize.X + FSpacing);
            GlyphPos.X := ModRect.Right - GlyphSize.X;
            GlyphPos.Y := VerticalShapePosition(VAlign, ModRect, GlyphSize);
          end;
          blGlyphTop:
          begin
            Inc(TextRect.Top, GlyphSize.Y + FSpacing);
            GlyphPos.X := HorizontalShapePosition(HAlign, ModRect, GlyphSize);
            GlyphPos.Y := ModRect.Top;
          end;
          blGlyphBottom:
          begin
            Dec(TextRect.Bottom, GlyphSize.Y + FSpacing);
            GlyphPos.X := HorizontalShapePosition(HAlign, ModRect, GlyphSize);
            GlyphPos.Y := ModRect.Bottom - GlyphSize.Y;
          end;
        end;
        if Enabled then
        begin
          if (cbsHot in FStates) and (Length(FGlyphs) >= 3) then
            BM := FGlyphs[cGlyphClicked]
          else if (cbsPressed in FStates) and (Length(FGlyphs) >= 4) then
            BM := FGlyphs[cGlyphDown]
          else
            BM := FGlyphs[cGlyphUp]
        end else
        begin
          if Skinned then
            BM := FGlyphs[cGlyphUp]
          else if Length(FGlyphs) >= 2 then
            BM := FGlyphs[cGlyphDisabled]
          else
            BM := FGrayedGlyph;
        end;
        BM.AlphaDrawTo(ACanvas, GlyphPos.X, GlyphPos.Y);
        TB.Draw(ACanvas, TextRect);
      finally
        TB.Free;
      end;
    end else
      DrawCaption(ACanvas, ARect);
  end else
    DrawCaption(ACanvas, ARect);
end;

procedure TKBitBtn.SetGlyph(const Value: TKAlphaBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TKBitBtn.SetKind(Value: TBitBtnKind);
{$IFnDEF FPC}
var
  Bu: TBitBtn;
{$ENDIF}
begin
  if Value <> FKind then
  begin
    FKind := Value;
    if FKind <> bkCustom then
    begin
      // simply steal bitmaps either from Delphi or Lazarus :-)
    {$IFDEF FPC}
      FNumGlyphs := 1;
      FMaskGlyph := False;
      FGlyph.Assign(GetLCLDefaultBtnGlyph(FKind));
    {$ELSE}
      // Delphi is more complicated, we must steal directly from TBitBtn
      Bu := TBitBtn.Create(nil);
      try
        Bu.Kind := FKind;
        FNumGlyphs := 2;
        FMaskGlyph := True;
        FGlyph.Assign(Bu.Glyph);
      finally
        Bu.Free;
      end;
    {$ENDIF}
    end else
    begin
      // reset corresponding properties
      FNumGlyphs := 1;
      FMaskGlyph := False;
    end;
  end;
end;

procedure TKBitBtn.SetLayout(Value: TButtonLayout);
begin
  if Value <> FLayout then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TKBitBtn.SetMargin(Value: Integer);
begin
  if Value <> FMargin then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TKBitBtn.SetMaskGlyph(const Value: Boolean);
begin
  if FMaskGlyph <> Value then
  begin
    FMaskGlyph := Value;
    UpdateGlyphs;
    Invalidate;
  end;
end;

procedure TKBitBtn.SetNumGlyphs(Value: Integer);
begin
  Value := MinMax(Value, 1, 4);
  if Value <> FNumGlyphs then
  begin
    FNumGlyphs := Value;
    UpdateGlyphs;
    Invalidate;
  end;
end;

procedure TKBitBtn.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TKBitBtn.UpdateGlyphs;
var
  I, GlyphWidth: Integer;
begin
  if Length(FGlyphs) <> FNumGlyphs then
  begin
    for I := 0 to Length(FGlyphs) - 1 do
      FreeAndNil(FGlyphs[I]);
    SetLength(FGlyphs, FNumGlyphs);
    for I := 0 to Length(FGlyphs) - 1 do
      FGlyphs[I] := TKAlphaBitmap.Create;
  end;
  if Length(FGlyphs) > 0 then
  begin
    GlyphWidth := FGlyph.Width div Length(FGlyphs);
    for I := 0 to Length(FGlyphs) - 1 do
    begin
      FGlyphs[I].SetSize(GlyphWidth, FGlyph.Height);
      if not FGlyph.Empty then
        FGlyphs[I].CopyFromXY(-I * GlyphWidth, 0, FGlyph);
    end;
    if Length(FGlyphs) = 1 then
    begin
      // create disabled icon as grayscale of the first icon
      if FGrayedGlyph = nil then
        FGrayedGlyph := TKAlphaBitmap.Create;
      FGrayedGlyph.Assign(FGlyphs[cGlyphUp]);
      FGrayedGlyph.GrayScale;
    end else
    begin
      FreeAndNil(FGrayedGlyph);
      if FMaskGlyph then
      begin
        // if source for Glyph was transparent bitmap
        // (old Delphi approach with bottom-leftmost pixel) then unmask it
        for I := 0 to Length(FGlyphs) - 1 do
          FGlyphs[I].AlphaFillOnColorMatch(
            ColorRecToColor(FGlyphs[I].Pixel[0, FGlyphs[I].Height - 1]), 0);
      end;
    end;
  end;
end;

{ TKColorButton }

constructor TKColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDlgColor := clRed;
  FColorDlgOptions := [];
end;

procedure TKColorButton.Click;
var
  Dlg: TColorDialog;
begin
  Exclude(FStates, cbsPressed);
  Dlg := TColorDialog.Create(Self);
  try
    Dlg.Color := FDlgColor;
  {$IFnDEF FPC}
    Dlg.Options := FColorDlgOptions;
  {$ENDIF}
    if Dlg.Execute then
    begin
      FDlgColor := Dlg.Color;
      Invalidate;
    end;
  finally
    Dlg.Free;
  end;
  inherited Click;
end;

procedure TKColorButton.DrawInterior(ACanvas: TCanvas; ARect: TRect);
var
  C: TKColorRec;
  ExOfs: Integer;
begin
  with ACanvas do
  begin
    ExOfs := 0;
    C := ColorToColorRec(FDlgColor);
    if (C.R < 128) and (C.G < 128) and (C.B < 128) then
      Pen.Color := clWhite
    else
      Pen.Color := clBlack;
    Brush.Color := FDlgColor;
    InflateRect(ARect, -10 + ExOfs, -6 + ExOfs);
    Rectangle(ARect);
    if not Enabled then
    begin
      MoveTo(ARect.Left, ARect.Top);
      LineTo(ARect.Right, ARect.Bottom - 1);
      MoveTo(ARect.Right - 1, ARect.Top);
      LineTo(ARect.Left, ARect.Bottom - 1);
    end;
  end;
end;

procedure TKColorButton.SetDlgColor(Value: TColor);
begin
  if Value <> FDlgColor then
  begin
    FDlgColor := Value;
    Invalidate;
  end;
end;

{ TKSpeedButton }

constructor TKSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  Width := Height;
  FGlyph := TKAlphaBitmap.Create;
  FGlyph.OnChange := GlyphChange;
  FImages := nil;
  FImageIndex := -1;
end;

destructor TKSpeedButton.Destroy;
begin
  FGlyph.Free;
  inherited;
end;

function TKSpeedButton.GetGlyph: TKAlphaBitmap;
begin
  Result := FGlyph;
end;

procedure TKSpeedButton.GlyphChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TKSpeedButton.DrawInterior(ACanvas: TCanvas; ARect: TRect);
begin
  if (FImages <> nil) and (FImageIndex >= 0) and (FImageIndex < FImages.Count) then
  begin
    FImages.Draw(ACanvas,
      ARect.Left + (ARect.Right - ARect.Left - FImages.Width) div 2,
      ARect.Top + (ARect.Bottom - ARect.Top - FImages.Height) div 2,
      FImageIndex, Enabled);
  end else
    FGlyph.AlphaDrawTo(ACanvas,
      ARect.Left + (ARect.Right - ARect.Left - FGlyph.Width) div 2,
      ARect.Top + (ARect.Bottom - ARect.Top - FGlyph.Height) div 2);
end;

function TKSpeedButton.IsGlyphStored: Boolean;
begin
  Result := not FGlyph.Empty and
    (FGlyph.Width > 0) and (FGlyph.Height > 0);
end;

procedure TKSpeedButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TKSpeedButton.SetGlyph(const Value: TKAlphaBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TKSpeedButton.SetImageIndex(Value: TImageIndex);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TKSpeedButton.SetImages(const Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

end.
