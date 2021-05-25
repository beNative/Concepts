{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit klabels; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Classes, Controls, Forms, Graphics, StdCtrls, KFunctions, KControls
  {$IFDEF USE_THEMES}
  , Themes
   {$IFNDEF FPC}
  , UxTheme
   {$ENDIF}
  {$ENDIF}
  ;

type
  TKGradientLabel = class(TKCustomControl)
  private
    BM: TBitmap;
    FLeftColor,
    FRightColor,
    FDividerColor: TColor;
    FDividerWidth: Integer;
    FColorStep: Integer;
    FCaptionWidth: Integer;
    procedure SetLeftColor(Value: TColor);
    procedure SetRightColor(Value: TColor);
    procedure SetDividerColor(Value: TColor);
    procedure SetDividerWidth(Value: Integer);
    procedure SetColorStep(Value: Integer);
    procedure SetCaptionWidth(Value: Integer);
    procedure WMEraseBkGnd(var Msg: TLMessage); message LM_ERASEBKGND;
    procedure CMTextChanged(var Msg: TLMessage); message CM_TEXTCHANGED;
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Caption;
    property CaptionWidth: Integer read FCaptionWidth write SetCaptionWidth default 50;
    property ColorStep: Integer read FColorStep write SetColorStep default 50;
    property Constraints;
    property DividerColor: TColor read FDividerColor write SetDividerColor default clBlack;
    property DividerWidth: Integer read FDividerWidth write SetDividerWidth default 2;
    property Font;
    property LeftColor: TColor read FLeftColor write SetLeftColor default clNavy;
    property RightColor: TColor read FRightColor write SetRightColor default clBlue;
  end;

  { TKLinkLabel }

  TKLinkLabel = class(TLabel)
  private
    FHotColor: TColor;
    FLinkColor: TColor;
    FShowURLAsHint: Boolean;
    FURL: string;
    procedure CMMouseEnter(var Message: TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TLMessage); message CM_MOUSELEAVE;
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    procedure SetHotColor(Value: TColor);
    procedure SetLinkColor(const Value: TColor);
  protected
    FActiveColor: TColor;
    FMouseInControl: Boolean;
    procedure Loaded; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  published
    property HotColor: TColor read FHotColor write SetHotColor default clRed;
    property LinkColor: TColor read FLinkColor write SetLinkColor default clBlue;
    property ShowURLAsHint: Boolean read FShowURLAsHint write FShowURLAsHint;
    property URL: string read FURL write FURL;
  end;

implementation

uses
  Math, SysUtils, KGraphics;

{ TKGradientLabel }

constructor TKGradientLabel.Create(AOwner: TComponent);
begin
  inherited;
  BM := TBitmap.Create;
{$IFNDEF FPC}
  BM.IgnorePalette := True;
{$ENDIF}
  Caption := '';
  FLeftColor := clNavy;
  FRightColor := clBlue;
  FDividerColor := clBlack;
  FDividerWidth := 2;
  Font.Color := clWhite;
  Font.Name := 'Arial';
  Font.Height := 20;
  Font.Style := [fsBold];
  FColorStep := 50;
  Width := 50;
  Height := 30;
  FCaptionWidth := 50;
end;

destructor TKGradientLabel.Destroy;
begin
  inherited;
  BM.Free;
end;

procedure TKGradientLabel.Resize;
begin
  FCaptionWidth := Width;
  Invalidate;
  inherited;
end;

procedure TKGradientLabel.SetDividerColor(Value: TColor);
begin
  if Value <> FDividerColor then
  begin
    FDividerColor := Value;
    Invalidate;
  end;
end;

procedure TKGradientLabel.SetDividerWidth(Value: Integer);
begin
  if Value <> FDividerWidth then
  begin
    FDividerWidth := Value;
    Invalidate;
  end;
end;

procedure TKGradientLabel.SetLeftColor(Value: TColor);
begin
  if Value <> FLeftColor then
  begin
    FLeftColor := Value;
    Invalidate;
  end;
end;

procedure TKGradientLabel.SetRightColor(Value: TColor);
begin
  if Value <> FRightColor then
  begin
    FRightColor := Value;
    Invalidate;
  end;
end;

procedure TKGradientLabel.SetCaptionWidth(Value: Integer);
begin
  if Value <> FCaptionWidth then
  begin
    FCaptionWidth := Value;
    Invalidate;
  end;
end;

procedure TKGradientLabel.SetColorStep(Value: Integer);
begin
  Value := Max(Value, 1);
  Value := Min(Value, 255);
  if Value <> FColorStep then
  begin
    FColorStep := Value;
    Invalidate;
  end;
end;

procedure TKGradientLabel.WMEraseBkGnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

procedure TKGradientLabel.Paint;
begin
  if Width > 0 then
  begin
    BM.Width := Width;
    BM.Height := Max(Height - FDividerWidth, 1);
    with BM.Canvas do
    begin
      if FLeftColor <> FRightColor then
      begin
        DrawGradientRect(BM.Canvas, Rect(0, 0, BM.Width, BM.Height), FLeftColor, FRightColor, FColorStep, True);
      end else
      begin
        Brush.Color := FLeftColor;
        FillRect(Rect(0, 0, BM.Width, BM.Height));
      end;
      Font := Self.Font;
      SetBkMode(Handle, TRANSPARENT);
      TextOut(Max((FCaptionWidth - TextWidth(Caption)) div 2, 10),
        (Height - Font.Height) div 2, Caption);
    end;
    with Canvas do
    begin
      Draw(0,0, BM);
      if FDividerWidth > 0 then
      begin
        Pen.Color := FDividerColor;
        Brush.Color := FDividerColor;
        Rectangle(0, Max(Height - FDividerWidth, 0), Width, Height);
      end;
    end;
  end;
end;

procedure TKGradientLabel.CMTextChanged(var Msg: TLMessage);
begin
  inherited;
  Invalidate;
end;

{ TKLinkLabel }

constructor TKLinkLabel.Create(AOwner: TComponent);
begin
  inherited;
  FMouseInControl := False;
  FShowURLAsHint := True;
  ShowHint := True;
  FHotColor := clRed;
  FLinkColor := clBlue;
  FActiveColor := FLinkColor;
  FURL := 'http://example.com';
  Caption := FURL;
  Cursor := crHandPoint;
end;

procedure TKLinkLabel.Paint;
begin
  if csDesigning in ComponentState then
    Font.Color := FLinkColor
  else
    Font.Color := FActiveColor;
  inherited;
end;

procedure TKLinkLabel.Click;
begin
  inherited;
  OpenURLWithShell(FURL);
end;

procedure TKLinkLabel.SetHotColor(Value: TColor);
begin
  if Value <> FHotColor then
  begin
    FHotColor := Value;
    if FMouseInControl then
      Invalidate;
  end;
end;

procedure TKLinkLabel.SetLinkColor(const Value: TColor);
begin
  if Value <> FLinkColor then
  begin
    FLinkColor := Value;
    if not FMouseInControl then
      Invalidate;
  end;
end;

procedure TKLinkLabel.Loaded;
begin
  inherited Loaded;
  FActiveColor := FLinkColor;
end;

procedure TKLinkLabel.CMMouseEnter(var Message: TLMessage);
begin
  inherited;
  { Don't draw a border if DragMode <> dmAutomatic since this button is meant to
    be used as a dock client. }
  if not (csDesigning in ComponentState) and not FMouseInControl
    and Enabled and (DragMode <> dmAutomatic) and (GetCapture = 0) then
  begin
    FMouseInControl := True;
    FActiveColor := FHotColor;
    if FShowURLAsHint then
      Hint := FURL;
    Invalidate;
  end;
end;

procedure TKLinkLabel.CMMouseLeave(var Message: TLMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) and FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    FActiveColor := FLinkColor;
    if FShowURLAsHint then
      Hint := '';
    Invalidate;
  end;
end;

procedure TKLinkLabel.CMFontChanged(var Message: TLMessage);
begin
  Invalidate;
end;

end.
