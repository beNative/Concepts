unit BCEditor.Editor.PopupWindow;

interface

uses
  Winapi.Messages, System.Classes, System.Types, Vcl.Controls;

type
  TBCEditorPopupWindow = class(TCustomControl)
  private
    FEditor: TWinControl;
    procedure WMEraseBkgnd(var AMessage: TMessage); message WM_ERASEBKGND;
    procedure WMMouseActivate(var AMessage: TMessage); message WM_MOUSEACTIVATE;
{$IFDEF USE_VCL_STYLES}
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
{$ENDIF}
  protected
    FActiveControl: TWinControl;
    FIsFocusable: Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Hide; virtual;
    procedure Show(Origin: TPoint); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property ActiveControl: TWinControl read FActiveControl;
    property IsFocusable: Boolean read FIsFocusable;
  end;

implementation

uses
  Winapi.Windows, System.SysUtils{$IFDEF USE_VCL_STYLES}, Vcl.Themes{$ENDIF};

constructor TBCEditorPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEditor := AOwner as TWinControl;
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];

  if not(csDesigning in ComponentState) then
    ControlStyle := ControlStyle + [csAcceptsControls];

  Ctl3D := False;
  ParentCtl3D := False;
  Parent := FEditor;
  Visible := False;
end;

procedure TBCEditorPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  Params.Style := WS_POPUP or WS_BORDER;
end;

procedure TBCEditorPopupWindow.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TBCEditorPopupWindow.Show(Origin: TPoint);
begin
  SetBounds(Origin.X, Origin.Y, Width, Height);
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0, SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;

procedure TBCEditorPopupWindow.WMMouseActivate(var AMessage: TMessage);
begin
  if FIsFocusable then
    inherited
  else
    AMessage.Result := MA_NOACTIVATE;
end;

procedure TBCEditorPopupWindow.WMEraseBkgnd(var AMessage: TMessage);
begin
  AMessage.Result := -1;
end;

{$IFDEF USE_VCL_STYLES}
procedure TBCEditorPopupWindow.WMNCPaint(var Message: TWMNCPaint);
var
  LRect: TRect;
  LExStyle: Integer;
  LTempRgn: HRGN;
  LBorderWidth, LBorderHeight: Integer;
begin
  if StyleServices.Enabled then
  begin
    LExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if (LExStyle and WS_EX_CLIENTEDGE) <> 0 then
    begin
      GetWindowRect(Handle, LRect);
      LBorderWidth := GetSystemMetrics(SM_CXEDGE);
      LBorderHeight := GetSystemMetrics(SM_CYEDGE);
      InflateRect(LRect, -LBorderWidth, -LBorderHeight);
      LTempRgn := CreateRectRgnIndirect(LRect);
      DefWindowProc(Handle, Message.Msg, wParam(LTempRgn), 0);
      DeleteObject(LTempRgn);
    end
    else
      DefaultHandler(Message);
  end
  else
    DefaultHandler(Message);

  if StyleServices.Enabled then
    StyleServices.PaintBorder(Self, False);
end;
{$ENDIF}

end.
