unit BCEditor.Editor.CodeFolding.Hint.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, System.Types, Vcl.Forms, Vcl.Controls, Vcl.Graphics;

type
  TBCEditorCodeFoldingHintForm = class(TCustomForm)
  strict private
    FBackgroundColor: TColor;
    FBufferBitmap: TBitmap;
    FBorderColor: TColor;
    FEffectiveItemHeight: Integer;
    FFont: TFont;
    FFontHeight: Integer;
    FFormWidth: Integer;
    FHeightBuffer: Integer;
    FItemHeight: Integer;
    FItemList: TStrings;
    FMargin: Integer;
    FVisibleLines: Integer;
    procedure AdjustMetrics;
    procedure FontChange(Sender: TObject);
    procedure RecalcItemHeight;
    procedure SetFont(const AValue: TFont);
    procedure SetItemHeight(const AValue: Integer);
    procedure SetItemList(const AValue: TStrings);
    procedure WMEraseBackgrnd(var AMessage: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var AMessage: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure Activate; override;
    procedure CreateParams(var AParams: TCreateParams); override;
    procedure Deactivate; override;
    procedure DoKeyPressW(AKey: Char);
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure KeyPressW(var AKey: Char); virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;

    procedure Execute(const ACurrentString: string; X, Y: Integer);

    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor default clWindow;
    property BorderColor: TColor read FBorderColor write FBorderColor default clBtnFace;
    property Font: TFont read FFont write SetFont;
    property FormWidth: Integer read FFormWidth write FFormWidth; { Don't use the width because it triggers resizing }
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 0;
    property ItemList: TStrings read FItemList write SetItemList;
    property Margin: Integer read FMargin write FMargin default 2;
    property VisibleLines: Integer read FVisibleLines write FVisibleLines;
  end;

implementation

uses
  System.SysUtils, System.UITypes, BCEditor.Editor.Base, BCEditor.Editor.KeyCommands, BCEditor.Utils,
  BCEditor.Consts{$IFDEF USE_ALPHASKINS}, sSkinProvider, sMessages{$ENDIF};

{ TBCEditorCodeFoldingHintForm }

constructor TBCEditorCodeFoldingHintForm.Create(AOwner: TComponent);
{$IFDEF USE_ALPHASKINS}
var
  LSkinProvider: TsSkinProvider;
{$ENDIF}
begin
  CreateNew(AOwner);

  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
  if not (csDesigning in ComponentState) then
    ControlStyle := ControlStyle + [csAcceptsControls];

  FBufferBitmap := Vcl.Graphics.TBitmap.Create;
  Visible := False;

  Color := FBackgroundColor;

  FItemList := TStringList.Create;

  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;

  FBackgroundColor := clWindow;
  FBorderColor := clBtnFace;

  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;

  FItemHeight := 0;
  FMargin := 2;
  FEffectiveItemHeight := 0;
  RecalcItemHeight;

  FHeightBuffer := 0;
  FFont.OnChange := FontChange;

{$IFDEF USE_ALPHASKINS}
  LSkinProvider := TsSkinProvider(SendMessage(Handle, SM_ALPHACMD, MakeWParam(0, AC_GETPROVIDER), 0));
  if Assigned(LSkinProvider) then
  begin
    LSkinProvider.AllowExtBorders := False;
    LSkinProvider.DrawNonClientArea := False;
    LSkinProvider.DrawClientArea := False;
  end;
{$ENDIF}
end;

destructor TBCEditorCodeFoldingHintForm.Destroy;
begin
  FBufferBitmap.Free;
  FItemList.Free;
  FFont.Free;

  inherited Destroy;
end;

procedure TBCEditorCodeFoldingHintForm.CreateParams(var AParams: TCreateParams);
begin
  inherited CreateParams(AParams);

  with AParams do
    if ((Win32Platform and VER_PLATFORM_WIN32_NT) <> 0) and (Win32MajorVersion > 4) and (Win32MinorVersion > 0) then
      WindowClass.Style := WindowClass.Style or CS_DROPSHADOW;
end;

procedure TBCEditorCodeFoldingHintForm.Activate;
begin
  Visible := True;
end;

procedure TBCEditorCodeFoldingHintForm.Deactivate;
begin
  Close;
end;

procedure TBCEditorCodeFoldingHintForm.KeyDown(var AKey: Word; AShift: TShiftState);
var
  LChar: Char;
  LData: Pointer;
  LEditorCommand: TBCEditorCommand;
begin
  with Owner as TBCBaseEditor do
  begin
    LData := nil;
    LChar := BCEDITOR_NONE_CHAR;
    LEditorCommand := TranslateKeyCode(AKey, AShift, LData);
    CommandProcessor(LEditorCommand, LChar, LData);
  end;
  Invalidate;
end;

procedure TBCEditorCodeFoldingHintForm.DoKeyPressW(AKey: Char);
begin
  if AKey <> BCEDITOR_NONE_CHAR then
    KeyPressW(AKey);
end;

procedure TBCEditorCodeFoldingHintForm.KeyPressW(var AKey: Char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, AKey);
  Invalidate;
end;

procedure TBCEditorCodeFoldingHintForm.Paint;

  procedure ResetCanvas;
  begin
    with FBufferBitmap.Canvas do
    begin
      Pen.Color := FBackgroundColor;
      Brush.Color := FBackgroundColor;
      Font.Assign(FFont);
    end;
  end;

const
  TitleMargin = 2;
var
  TmpRect: TRect;
  i: Integer;
begin
  ResetCanvas;
  TmpRect := ClientRect;
  PatBlt(FBufferBitmap.Canvas.Handle, TmpRect.Left, TmpRect.Top, TmpRect.Width, TmpRect.Height, PATCOPY);
  FBufferBitmap.Canvas.Pen.Color := FBorderColor;
  FBufferBitmap.Canvas.Rectangle(TmpRect);

  for i := 0 to FItemList.Count - 1 do
    FBufferBitmap.Canvas.TextOut(FMargin + 1, FEffectiveItemHeight * i + FMargin, FItemList[i]);

  Canvas.Draw(0, 0, FBufferBitmap);
end;

procedure TBCEditorCodeFoldingHintForm.SetItemList(const AValue: TStrings);
begin
  FItemList.Assign(AValue);
end;

procedure TBCEditorCodeFoldingHintForm.SetItemHeight(const AValue: Integer);
begin
  if FItemHeight <> AValue then
  begin
    FItemHeight := AValue;
    RecalcItemHeight;
  end;
end;

procedure TBCEditorCodeFoldingHintForm.RecalcItemHeight;
begin
  Canvas.Font.Assign(FFont);
  FFontHeight := TextHeight(Canvas, 'X');
  if FItemHeight > 0 then
    FEffectiveItemHeight := FItemHeight
  else
    FEffectiveItemHeight := FFontHeight;
end;

procedure TBCEditorCodeFoldingHintForm.WMEraseBackgrnd(var AMessage: TMessage);
begin
  AMessage.Result := 1;
end;

procedure TBCEditorCodeFoldingHintForm.WMGetDlgCode(var AMessage: TWMGetDlgCode);
begin
  inherited;
  AMessage.Result := AMessage.Result or DLGC_WANTTAB;
end;

procedure TBCEditorCodeFoldingHintForm.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
  RecalcItemHeight;
  AdjustMetrics;
end;

procedure TBCEditorCodeFoldingHintForm.FontChange(Sender: TObject);
begin
  RecalcItemHeight;
  AdjustMetrics;
end;

procedure TBCEditorCodeFoldingHintForm.Execute(const ACurrentString: string; X, Y: Integer);

  function GetWorkAreaWidth: Integer;
  begin
    Result := Screen.DesktopWidth;
  end;

  function GetWorkAreaHeight: Integer;
  begin
    Result := Screen.DesktopHeight;
  end;

  procedure RecalcFormPlacement;
  var
    i: Integer;
    LWidth: Integer;
    LHeight: Integer;
    LX: Integer;
    LY: Integer;
    LStr: string;
    LBorderWidth: Integer;
    LNewWidth: Integer;
  begin
    LX := X;
    LY := Y;
    LWidth := 0;

    LBorderWidth := 2;
    LHeight := FEffectiveItemHeight * ItemList.Count + LBorderWidth + 2 * Margin;

    Canvas.Font.Assign(Font);
    for i := 0 to ItemList.Count - 1 do
    begin
      LStr := ItemList[i];
      LNewWidth := Canvas.TextWidth(LStr);
      if LNewWidth > LWidth then
        LWidth := LNewWidth;
    end;

    Inc(LWidth, 2 * Margin + LBorderWidth + 4);

    if LX + LWidth > GetWorkAreaWidth then
    begin
      LX := GetWorkAreaWidth - LWidth - 5;
      if LX < 0 then
        LX := 0;
    end;

    if LY + LHeight > GetWorkAreaHeight then
    begin
      LY := LY - LHeight - (Owner as TBCBaseEditor).LineHeight - 2;
      if LY < 0 then
        LY := 0;
    end;

    SetWindowPos(Handle, HWND_TOP, LX, LY, 0, 0, SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);

    Width := LWidth;
    Height := LHeight;
  end;

begin
  RecalcFormPlacement;
  AdjustMetrics;
  Visible := True;
end;

procedure TBCEditorCodeFoldingHintForm.AdjustMetrics;
begin
  if (ClientWidth > 0) and (ClientHeight > 0) then
  begin
    FBufferBitmap.Width := ClientWidth;
    FBufferBitmap.Height := ClientHeight;
  end;
end;

end.
