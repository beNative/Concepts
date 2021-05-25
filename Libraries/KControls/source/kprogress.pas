{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kprogress; // lowercase name because of Lazarus/Linux

{$include KControls.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Classes, Controls, Forms, Graphics, KFunctions, KControls
  {$IFDEF USE_THEMES}
  , Themes
   {$IFNDEF FPC}
  , UxTheme
   {$ENDIF}
  {$ENDIF}
  ;

type
  TKPercentProgressBar = class(TKCustomControl)
  private
    FBarColor: TColor;
    FBorder: Boolean;
    FDrawPercent: Boolean;
    FFrame: Boolean;
    FMax: Integer;
    FMin: Integer;
    FPosition: Integer;
    procedure SetBarColor(const Value: TColor);
    procedure SetBorder(const Value: Boolean);
    procedure SetDrawPercent(Value: Boolean);
    procedure SetFrame(const Value: Boolean);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure WMEraseBkGnd(var Msg: TLMessage); message LM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property BarColor: TColor read FBarColor write SetBarColor default clNavy;
    property Border: Boolean read FBorder write SetBorder default False;
    property Color default clBtnFace;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DrawPercent: Boolean read FDrawPercent write SetDrawPercent default True;
    property Enabled;
    property Font;
    property Frame: Boolean read FFrame write SetFrame default True;
    property Hint;
    property Constraints;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property ParentShowHint;
    property PopupMenu;
    property Position: Integer read FPosition write SetPosition default 0;
    property ShowHint;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
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

implementation

uses
  Math, SysUtils, Types;

{ TKPercentProgressBar }

constructor TKPercentProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  FBarColor := clNavy;
  FBorder := False;
  FDrawPercent := True;
  FFrame := True;
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  Top := 10;
  Left := 10;
  Width := 150;
  Height := 20;
  BorderStyle := bsNone;
  Color := clBtnFace;
  ControlStyle := ControlStyle + [csOpaque];
end;

procedure TKPercentProgressBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TKPercentProgressBar.Paint;
var
  R: TRect;
  S: string;
  I, J, X, Y, Percent: Integer;
begin
  with Canvas do
  begin
    X := 0; Y := 0;
    SetBkMode(Handle, TRANSPARENT);
    R := ClientRect;
    if FMin > FMax then
    begin
      I := FMin;
      FMin := FMax;
      FMax := I;
    end
    else if FMin = FMax then
      Dec(FMin);
    Percent := (FPosition - FMin) * 100 div (FMax - FMin);
    if FFrame then
    begin
      DrawEdge(Handle, R, BDR_SUNKENOUTER, BF_RECT);
      if Color = clNone then
        Brush.Style := bsClear
      else
        Brush.Color := Color;
      InflateRect(R, -1, -1);
      FrameRect(R);
      InflateRect(R, -1, -1);
    end
    else if FBorder then
    begin
      Brush.Style := bsClear;
      Pen.Color := FBarColor;
      Rectangle(R);
      InflateRect(R, -1, -1);
    end;
    I := (R.Right - R.Left) * Percent div 100;
    J := R.Right;
    R.Right := R.Left + I;
    Brush.Color := FBarColor;
    if FDrawPercent then
    begin
      S := IntToStr(Percent) + ' %';
      Font := Self.Font;
      X := (ClientWidth - TextWidth(S)) div 2;
      Y := (ClientHeight - TextHeight(S)) div 2;
      TextRect(R, X, Y, S);
    end else
      FillRect(R);
    if Color = clNone then
      Brush.Style := bsClear
    else
      Brush.Color := Color;
    R.Left := R.Right;
    R.Right := J;
    if FDrawPercent then
    begin
      Font.Color := clWindowText;
      TextRect(R, X, Y, S);
    end else
      FillRect(R);
  end;
end;

procedure TKPercentProgressBar.SetBarColor(const Value: TColor);
begin
  if Value <> FBarColor then
  begin
    FBarColor := Value;
    Invalidate;
  end;
end;

procedure TKPercentProgressBar.SetBorder(const Value: Boolean);
begin
  if Value <> FBorder then
  begin
    FBorder := Value;
    Invalidate;
  end;
end;

procedure TKPercentProgressBar.SetDrawPercent(Value: Boolean);
begin
  if Value <> FDrawPercent then
  begin
    FDrawPercent := Value;
    Invalidate;
  end;
end;

procedure TKPercentProgressBar.SetFrame(const Value: Boolean);
begin
  if Value <> FFrame then
  begin
    FFrame := Value;
    Invalidate;
  end;
end;

procedure TKPercentProgressBar.SetMax(Value: Integer);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    FPosition := Math.Min(FMax, FPosition);
    Invalidate;
  end;
end;

procedure TKPercentProgressBar.SetMin(Value: Integer);
begin
  if Value <> FMin then
  begin
    FMin := Value;
    FPosition := Math.Max(FMin, FPosition);
    Invalidate;
  end;
end;

procedure TKPercentProgressBar.SetPosition(Value: Integer);
begin
  if Value < FMin then Value := FMin
  else if Value > FMax then Value := FMax;
  if Value <> FPosition then
  begin
    FPosition := Value;
    Invalidate;
  end;
end;

procedure TKPercentProgressBar.WMEraseBkGnd(var Msg: TLMessage);
begin
  Msg.Result := -1;
end;

end.
