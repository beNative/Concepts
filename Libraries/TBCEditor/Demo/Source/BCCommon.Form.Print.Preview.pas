unit BCCommon.Form.Print.Preview;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms, Vcl.Controls, Vcl.ComCtrls, Vcl.ActnList,
  BCEditor.Print.Preview, Vcl.Menus, Vcl.AppEvnts, Vcl.Printers, BCCommon.Images, System.Actions, System.Types,
  BCControl.Panel, BCControl.StatusBar, Vcl.Dialogs, BCControl.SpeedButton, Vcl.Buttons, sSpeedButton, Vcl.ExtCtrls,
  sPanel, sStatusBar, sTrackBar;

type
  TPrintPreviewDialog = class(TForm)
    ActionColors: TAction;
    ActionExit: TAction;
    ActionFirst: TAction;
    ActionHighlight: TAction;
    ActionLast: TAction;
    ActionLineNumbers: TAction;
    ActionList: TActionList;
    ActionNext: TAction;
    ActionPrevious: TAction;
    ActionPrint: TAction;
    ActionWordWrap: TAction;
    ActionZoom: TAction;
    ActionZoomIn: TAction;
    ActionZoomOut: TAction;
    ApplicationEvents: TApplicationEvents;
    MenuItemPercent100: TMenuItem;
    MenuItemPercent125: TMenuItem;
    MenuItemPercent150: TMenuItem;
    MenuItemPercent175: TMenuItem;
    MenuItemPercent200: TMenuItem;
    MenuItemPercent25: TMenuItem;
    MenuItemPercent300: TMenuItem;
    MenuItemPercent400: TMenuItem;
    MenuItemPercent50: TMenuItem;
    MenuItemPercent75: TMenuItem;
    PanelButtons: TBCPanel;
    PanelPrintPreview: TBCPanel;
    PopupMenuZoom: TPopupMenu;
    PrintDialog: TPrintDialog;
    PrintPreview: TBCEditorPrintPreview;
    SpeedButtonColors: TBCSpeedButton;
    SpeedButtonDivider1: TBCSpeedButton;
    SpeedButtonDivider2: TBCSpeedButton;
    SpeedButtonDivider3: TBCSpeedButton;
    SpeedButtonDivider4: TBCSpeedButton;
    SpeedButtonExit: TBCSpeedButton;
    SpeedButtonFirst: TBCSpeedButton;
    SpeedButtonHighlighter: TBCSpeedButton;
    SpeedButtonLast: TBCSpeedButton;
    SpeedButtonLineNumbers: TBCSpeedButton;
    SpeedButtonNext: TBCSpeedButton;
    SpeedButtonPrevious: TBCSpeedButton;
    SpeedButtonPrint: TBCSpeedButton;
    SpeedButtonWordWrap: TBCSpeedButton;
    SpeedButtonZoom: TBCSpeedButton;
    SpeedButtonZoomIn: TBCSpeedButton;
    SpeedButtonZoomOut: TBCSpeedButton;
    StatusBar: TBCStatusBar;
    TrackBarZoom: TsTrackBar;
    procedure ActionColorsExecute(Sender: TObject);
    procedure ActionColorsUpdate(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionFirstExecute(Sender: TObject);
    procedure ActionFirstUpdate(Sender: TObject);
    procedure ActionHighlightExecute(Sender: TObject);
    procedure ActionLastExecute(Sender: TObject);
    procedure ActionLastUpdate(Sender: TObject);
    procedure ActionLineNumbersExecute(Sender: TObject);
    procedure ActionNextExecute(Sender: TObject);
    procedure ActionNextUpdate(Sender: TObject);
    procedure ActionPreviousExecute(Sender: TObject);
    procedure ActionPreviousUpdate(Sender: TObject);
    procedure ActionPrintExecute(Sender: TObject);
    procedure ActionWordWrapExecute(Sender: TObject);
    procedure ActionZoomInExecute(Sender: TObject);
    procedure ActionZoomOutExecute(Sender: TObject);
    procedure ApplicationEventsHint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PercentClick(Sender: TObject);
    procedure PrintPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PrintPreviewPreviewPage(Sender: TObject; PageNumber: Integer);
    procedure TrackBarZoomChange(Sender: TObject);
    procedure PrintPreviewScaleChange(Sender: TObject);
  private
    FLeft: Integer;
    FTop: Integer;
  end;

function PrintPreviewDialog: TPrintPreviewDialog;

implementation

{$R *.DFM}

uses
  UxTheme, BCCommon.Language.Utils, BCCommon.Language.Strings, WinApi.Windows, Math;

const
  STATUSBAR_SCALE = 1;
  STATUSBAR_PAGE = 2;
  STATUSBAR_HINT = 3;

var
  FPrintPreviewDialog: TPrintPreviewDialog;

function PrintPreviewDialog: TPrintPreviewDialog;
begin
  if not Assigned(FPrintPreviewDialog) then
    Application.CreateForm(TPrintPreviewDialog, FPrintPreviewDialog);
  Result := FPrintPreviewDialog;
end;

procedure TPrintPreviewDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TPrintPreviewDialog.FormCreate(Sender: TObject);
begin
  { IDE can lose these properties }
  PopupMenuZoom.Images := ImagesDataModule.ImageListSmall;
end;

procedure TPrintPreviewDialog.FormDestroy(Sender: TObject);
begin
  FPrintPreviewDialog := nil;
end;

procedure TPrintPreviewDialog.FormShow(Sender: TObject);
begin
  PrintPreview.UpdatePreview;
  PrintPreview.FirstPage;
  if Printer.PrinterIndex >= 0 then
    ActionPrint.Hint := Format(LanguageDataModule.GetConstant('PreviewPrintDocument'),
      [Printer.Printers[Printer.PrinterIndex], Printer.Printers[Printer.PrinterIndex]]);
  PrintPreview.ScalePercent := 100;
  ActionLineNumbers.Checked := PrintPreview.EditorPrint.LineNumbers;
  ActionWordWrap.Checked := PrintPreview.EditorPrint.Wrap;
  ActionColors.Checked := PrintPreview.EditorPrint.Colors;
  ActionHighlight.Checked := PrintPreview.EditorPrint.Highlight;

  FLeft := PrintPreview.EditorPrint.Margins.PixelLeft;
  FTop := PrintPreview.EditorPrint.Margins.PixelTop;

  UpdateLanguage(Self);
  if PrintPreview.EditorPrint.Title <> '' then
    Caption := Format('%s - [%s]', [Caption, PrintPreview.EditorPrint.Title]);
end;

procedure TPrintPreviewDialog.ActionHighlightExecute(Sender: TObject);
begin
  if PrintPreview.EditorPrint.Highlight and PrintPreview.EditorPrint.Colors then
    ActionColorsExecute(Sender);
  PrintPreview.EditorPrint.Highlight := not PrintPreview.EditorPrint.Highlight;
  ActionHighlight.Checked := PrintPreview.EditorPrint.Highlight;
  PrintPreview.Refresh;
end;

procedure TPrintPreviewDialog.ActionColorsExecute(Sender: TObject);
begin
  PrintPreview.EditorPrint.Colors := not PrintPreview.EditorPrint.Colors;
  ActionColors.Checked := PrintPreview.EditorPrint.Colors;
  PrintPreview.Refresh;
end;

procedure TPrintPreviewDialog.ActionColorsUpdate(Sender: TObject);
begin
  ActionColors.Enabled := ActionHighlight.Checked;
end;

procedure TPrintPreviewDialog.ActionFirstExecute(Sender: TObject);
begin
  PrintPreview.FirstPage;
end;

procedure TPrintPreviewDialog.ActionFirstUpdate(Sender: TObject);
begin
  ActionFirst.Enabled := PrintPreview.PageNumber > 1;
end;

procedure TPrintPreviewDialog.ActionPreviousExecute(Sender: TObject);
begin
  PrintPreview.PreviousPage;
end;

procedure TPrintPreviewDialog.ActionPreviousUpdate(Sender: TObject);
begin
  ActionPrevious.Enabled := PrintPreview.PageNumber > 1;
end;

procedure TPrintPreviewDialog.ActionNextExecute(Sender: TObject);
begin
  PrintPreview.NextPage;
end;

procedure TPrintPreviewDialog.ActionNextUpdate(Sender: TObject);
begin
  ActionNext.Enabled := PrintPreview.PageNumber < PrintPreview.PageCount;
end;

procedure TPrintPreviewDialog.ActionLastExecute(Sender: TObject);
begin
  PrintPreview.LastPage;
end;

procedure TPrintPreviewDialog.ActionLastUpdate(Sender: TObject);
begin
  ActionLast.Enabled := PrintPreview.PageNumber < PrintPreview.PageCount;
end;

procedure TPrintPreviewDialog.ActionLineNumbersExecute(Sender: TObject);
begin
  PrintPreview.EditorPrint.LineNumbers := not PrintPreview.EditorPrint.LineNumbers;
  PrintPreview.EditorPrint.LineNumbersInMargin := PrintPreview.EditorPrint.LineNumbers;
  ActionLineNumbers.Checked := PrintPreview.EditorPrint.LineNumbers;
  PrintPreview.Refresh;
end;

procedure TPrintPreviewDialog.ActionZoomInExecute(Sender: TObject);
begin
  PrintPreviewMouseDown(Sender, mbLeft, [], 0, 0);
end;

procedure TPrintPreviewDialog.ActionZoomOutExecute(Sender: TObject);
begin
  PrintPreviewMouseDown(Sender, mbRight, [], 0, 0);
end;

procedure TPrintPreviewDialog.ActionPrintExecute(Sender: TObject);
begin
  PrintPreview.Print;
end;

procedure TPrintPreviewDialog.PercentClick(Sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    - 1:
      PrintPreview.ScaleMode := pscPageWidth;
  else
    PrintPreview.ScalePercent := (Sender as TMenuItem).Tag;
  end;
end;

procedure TPrintPreviewDialog.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TPrintPreviewDialog.ApplicationEventsHint(Sender: TObject);
begin
  StatusBar.Panels[STATUSBAR_HINT].Text := Application.Hint;
end;

procedure TPrintPreviewDialog.PrintPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  FScale: Integer;
begin
  FScale := PrintPreview.ScalePercent;
  if Button = mbLeft then
  begin
    if PrintPreview.ScaleMode = pscWholePage then
      PrintPreview.ScalePercent := 100
    else
    begin
      FScale := FScale + 25;
      if FScale > 400 then
        FScale := 400;
      PrintPreview.ScalePercent := FScale;
    end;
  end
  else
  begin
    FScale := FScale - 25;
    if FScale < 25 then
      FScale := 25;
    PrintPreview.ScalePercent := FScale;
  end;
  { fix for scrollbar resize bug }
  SetWindowPos(PrintPreview.Handle, 0, 0, PrintPreview.Top, PrintPreview.Width, PrintPreview.Height, SWP_DRAWFRAME);
end;

procedure TPrintPreviewDialog.PrintPreviewPreviewPage(Sender: TObject; PageNumber: Integer);
begin
  StatusBar.Panels[STATUSBAR_PAGE].Text := Format(LanguageDataModule.GetConstant('PreviewPage'), [PageNumber, PrintPreview.PageCount]);
  StatusBar.Panels[STATUSBAR_PAGE].Width := StatusBar.Canvas.TextWidth(StatusBar.Panels[STATUSBAR_PAGE].Text) + 16;
end;

procedure TPrintPreviewDialog.PrintPreviewScaleChange(Sender: TObject);
begin
  StatusBar.Panels[STATUSBAR_SCALE].Text := Format('%d%%', [PrintPreview.ScalePercent]);
  if TrackBarZoom.Position <> PrintPreview.ScalePercent then
   TrackBarZoom.Position := PrintPreview.ScalePercent;
end;

procedure TPrintPreviewDialog.TrackBarZoomChange(Sender: TObject);
begin
  PrintPreview.ScalePercent := TrackBarZoom.Position;
end;

procedure TPrintPreviewDialog.ActionWordWrapExecute(Sender: TObject);
begin
  PrintPreview.EditorPrint.Wrap := not PrintPreview.EditorPrint.Wrap;
  ActionWordWrap.Checked := PrintPreview.EditorPrint.Wrap;
  Printpreview.UpdatePreview;
  PrintPreview.Refresh;
end;

end.
