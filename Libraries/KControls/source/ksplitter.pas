{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit ksplitter; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  Classes, Controls, Graphics, ExtCtrls, Forms, StdCtrls,
  KFunctions, KControls;

type

  { TKSplitter }

{$IFDEF FPC}
  TKSplitter = class(TSplitter);
{$ELSE}
  TKSplitter = class(TGraphicControl)
  private
    FActiveControl: TWinControl;
    FAutoSnap: Boolean;
    FBeveled: Boolean;
    FBrush: TBrush;
    FControl: TControl;
    FDownPos: TPoint;
    FLineDC: HDC;
    FLineVisible: Boolean;
    FMinSize: Integer;
    FMaxSize: Integer;
    FNewSize: Integer;
    FOldKeyDown: TKeyEvent;
    FOldSize: Integer;
    FPrevBrush: HBrush;
    FResizeStyle: TResizeStyle;
    FSplit: Integer;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    procedure AllocateLineDC;
    procedure CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure DrawLine;
    function FindControl: TControl;
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ReleaseLineDC;
    procedure SetBeveled(Value: Boolean);
    procedure SetMinSize(AValue: Integer);
    procedure UpdateControlSize;
    procedure UpdateSize(X, Y: Integer);
  protected
    function CanResize(var NewSize: Integer): Boolean; reintroduce; virtual;
    function DoCanResize(var NewSize: Integer): Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure RequestAlign; override;
    procedure StopSizing; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
    property Align default alLeft;
    property AutoSnap: Boolean read FAutoSnap write FAutoSnap default True;
    property Beveled: Boolean read FBeveled write SetBeveled default False;
    property Color;
    property Cursor default crHSplit;
    property Constraints;
    property MinSize: Integer read FMinSize write SetMinSize default 30;
    property ParentColor;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle default rsUpdate;
    property Visible;
    property Width default 3;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

{$ENDIF}

implementation

uses
  Math;

{ TKSplitter }

{$IFDEF FPC}
{$ELSE}

type
  TWinControlAccess = class(TWinControl);

constructor TKSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSnap := True;
  Height := 100;
  Align := alLeft;
  Width := 3;
  Cursor := crHSplit;
  FMinSize := 30;
  FResizeStyle := rsUpdate;
  FOldSize := -1;
end;

destructor TKSplitter.Destroy;
begin
  FBrush.Free;
  inherited Destroy;
end;

procedure TKSplitter.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
{$IFDEF MSWINDOWS}
      FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
{$ENDIF}
{$IFDEF UNIX}
      FBrush.Color := clGray;
{$ENDIF}
    end;
    FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
end;

procedure TKSplitter.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(Left, Top);
  if Align in [alLeft, alRight] then
    P.X := Left + FSplit else
    P.Y := Top + FSplit;
  with P do PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;

procedure TKSplitter.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;

function TKSplitter.FindControl: TControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  Result := nil;
  P := Point(Left, Top);
  case Align of
    alLeft: Dec(P.X);
    alRight: Inc(P.X, Width);
    alTop: Dec(P.Y);
    alBottom: Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if Result.Visible and Result.Enabled and (Result <> Self) then
    begin
      R := Result.BoundsRect;
      if (R.Right - R.Left) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);
      if PtInRect(R, P) then Exit;
    end;
  end;
  Result := nil;
end;

procedure TKSplitter.RequestAlign;
begin
  inherited RequestAlign;
  if (Cursor <> crVSplit) and (Cursor <> crHSplit) then Exit;
  if Align in [alBottom, alTop] then
    Cursor := crVSplit
  else
    Cursor := crHSplit;
end;

procedure TKSplitter.Paint;
const
  XorColor = $00FFD8CE;
var
  FrameBrush: HBRUSH;
  R: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  if Beveled then
  begin
    if Align in [alLeft, alRight] then
      InflateRect(R, -1, 2) else
      InflateRect(R, 2, -1);
    OffsetRect(R, 1, 1);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
    OffsetRect(R, -2, -2);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
  end;
  if csDesigning in ComponentState then
    { Draw outline }
    with Canvas do
    begin
      Pen.Style := psDot;
      Pen.Mode := pmXor;
      Pen.Color := XorColor;
      Brush.Style := bsClear;
      Rectangle(0, 0, ClientWidth, ClientHeight);
    end;
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

function TKSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := CanResize(NewSize);
  if Result and (NewSize <= MinSize) and FAutoSnap then
    NewSize := 0;
end;

function TKSplitter.CanResize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanResize) then FOnCanResize(Self, NewSize, Result);
end;

procedure TKSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FControl := FindControl;
    FDownPos := Point(X, Y);
    if Assigned(FControl) then
    begin
      if Align in [alLeft, alRight] then
      begin
        FMaxSize := Parent.ClientWidth - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Visible and (Align in [alLeft, alRight]) then Dec(FMaxSize, Width);
        Inc(FMaxSize, FControl.Width);
      end
      else
      begin
        FMaxSize := Parent.ClientHeight - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alTop, alBottom] then Dec(FMaxSize, Height);
        Inc(FMaxSize, FControl.Height);
      end;
      UpdateSize(X, Y);
      AllocateLineDC;
      with ValidParentForm(Self) do
        if ActiveControl <> nil then
        begin
          FActiveControl := ActiveControl;
          FOldKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
          TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
        end;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TKSplitter.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case Align of
      alLeft:
        begin
          FControl.Width := FNewSize;
          FControl.Left := Self.Left - FNewSize;
        end;
      alTop: FControl.Height := FNewSize;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - FNewSize);
            FControl.Width := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - FNewSize);
            FControl.Height := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    Update;
    if Assigned(FOnMoved) then FOnMoved(Self);
    FOldSize := FNewSize;
  end;
end;

procedure TKSplitter.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case Align of
    alLeft: S := FControl.Width + Split;
    alRight: S := FControl.Width - Split;
    alTop: S := FControl.Height + Split;
    alBottom: S := FControl.Height - Split;
  end;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if Align in [alRight, alBottom] then
      S := S - NewSize else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

procedure TKSplitter.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

procedure TKSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSize, Split: Integer;
begin
  inherited;
  if (ssLeft in Shift) and Assigned(FControl) then
  begin
    CalcSplitSize(X, Y, NewSize, Split);
    if DoCanResize(NewSize) then
    begin
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      FNewSize := NewSize;
      FSplit := Split;
      if ResizeStyle = rsUpdate then UpdateControlSize;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TKSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Assigned(FControl) then
  begin
    if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    UpdateControlSize;
    StopSizing;
  end;
end;

procedure TKSplitter.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
  else if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TKSplitter.SetBeveled(Value: Boolean);
begin
  FBeveled := Value;
  Repaint;
end;

procedure TKSplitter.SetMinSize(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if FMinSize=AValue then Exit;
  FMinSize:=AValue;
end;

procedure TKSplitter.StopSizing;
begin
  if Assigned(FControl) then
  begin
    if FLineVisible then DrawLine;
    FControl := nil;
    ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      TWinControlAccess(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

{$ENDIF}

end.
