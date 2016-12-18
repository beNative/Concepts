unit TBCEditorDemo.Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  BCCommon.Form.Base, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, BCEditor.Editor, BCEditor.Highlighter,
  BCEditor.Editor.Base, Vcl.Buttons, Vcl.AppEvnts, System.Actions, Vcl.ActnList, BCEditor.Print, BCCommon.Images,
  BCComponent.SkinManager, BCControl.Panel, BCControl.StatusBar, BCComponent.TitleBar, Vcl.Menus,
  BCControl.Splitter, sPanel, BCComponent.MultiStringHolder, sSkinManager, sStatusBar, sSplitter, acTitleBar,
  sSkinProvider, sDialogs, Vcl.StdCtrls, System.Diagnostics, BCCommon.Dialog.Popup.Highlighter, BCEditor.Types,
  BCCommon.Dialog.Popup.Encoding, BCCommon.Dialog.Popup.Highlighter.Color, sSpeedButton, BCControl.SpeedButton,
  sComboBox, BCControl.ComboBox, sLabel, BCEditor.MacroRecorder, BCCommon.Dialog.Popup.SearchEngine,
  VirtualTrees, BCControl.ObjectInspector;

const
  BCEDITORDEMO_CAPTION = 'TBCEditor Control Demo';

  TITLE_BAR_CAPTION = 1;
  TITLE_BAR_ENCODING = 2;
  TITLE_BAR_HIGHLIGHTER = 4;
  TITLE_BAR_COLORS = 6;

type
  TMainForm = class(TBCBaseForm)
    ActionCaseSensitive: TAction;
    ActionClose: TAction;
    ActionFileOpen: TAction;
    ActionFindNext: TAction;
    ActionFindPrevious: TAction;
    ActionInSelection: TAction;
    ActionOptions: TAction;
    ActionPreview: TAction;
    ActionSearch: TAction;
    ActionSearchEngine: TAction;
    ActionSkins: TAction;
    ComboBoxSearchText: TBCComboBox;
    Editor: TBCEditor;
    LabelSearchResultCount: TsLabel;
    MenuItemExit: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemPrintPreview: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    MenuItemSeparator2: TMenuItem;
    MenuItemSkins: TMenuItem;
    MultiStringHolderFileTypes: TBCMultiStringHolder;
    ObjectInspector: TBCObjectInspector;
    OpenDialog: TsOpenDialog;
    PanelLeft: TBCPanel;
    PanelProperty: TBCPanel;
    PanelRight: TBCPanel;
    PanelSearch: TBCPanel;
    PopupMenuDummy: TPopupMenu;
    PopupMenuFile: TPopupMenu;
    SpeedButtonCaseSensitive: TBCSpeedButton;
    SpeedButtonClose: TBCSpeedButton;
    SpeedButtonFindNext: TBCSpeedButton;
    SpeedButtonFindPrevious: TBCSpeedButton;
    SpeedButtonInSelection: TBCSpeedButton;
    SpeedButtonOptions: TBCSpeedButton;
    SpeedButtonSearchDivider1: TBCSpeedButton;
    SpeedButtonSearchDivider2: TBCSpeedButton;
    SpeedButtonSearchEngine: TBCSpeedButton;
    Splitter: TBCSplitter;
    SplitterSearch: TBCSplitter;
    procedure ActionCaseSensitiveExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionFindPreviousExecute(Sender: TObject);
    procedure ActionInSelectionExecute(Sender: TObject);
    procedure ActionOptionsExecute(Sender: TObject);
    procedure ActionPreviewExecute(Sender: TObject);
    procedure ActionSearchEngineExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ActionSkinsExecute(Sender: TObject);
    procedure ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure ComboBoxSearchTextChange(Sender: TObject);
    procedure ComboBoxSearchTextKeyPress(Sender: TObject; var Key: Char);
    procedure EditorCaretChanged(Sender: TObject; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SelectedEncodingClick(const AIndex: Integer);
    procedure SelectedHighlighterClick(AHighlighterName: string);
    procedure SelectedHighlighterColorClick(AHighlighterColorName: string);
    procedure SelectedSearchEngineClick(ASearchEngine: TBCEditorSearchEngine);
    procedure TitleBarItems2Click(Sender: TObject);
    procedure TitleBarItems4Click(Sender: TObject);
    procedure TitleBarItems6Click(Sender: TObject);
  private
    FHighlighterColorStrings: TStringList;
    FHighlighterStrings: TStringList;
    FPopupEncodingDialog: TPopupEncodingDialog;
    FPopupHighlighterColorDialog: TPopupHighlighterColorDialog;
    FPopupHighlighterDialog: TPopupHighlighterDialog;
    FPopupSearchEngineDialog: TBCPopupSearchEngineDialog;
    FStopWatch: TStopWatch;
    function GetTitleBarItemLeftBottom(AIndex: Integer): TPoint;
    procedure ClearText;
    procedure DoSearchTextChange;
    procedure InitializeEditorPrint(AEditorPrint: TBCEditorPrint);
    procedure LockFormPaint;
    procedure PrintPreview;
    procedure SetMatchesFound;
    procedure UnlockFormPaint;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  BCCommon.Form.Print.Preview, BCEditor.Print.Types, BCCommon.Dialog.SkinSelect, BCCommon.FileUtils, BCCommon.Utils,
  BCCommon.Dialog.Options.Search, BCCommon.Encoding, BCEditor.Consts, acPopupController, sVclUtils;

procedure TMainForm.ActionSkinsExecute(Sender: TObject);
begin
  inherited;
  TSkinSelectDialog.ClassShowModal(SkinManager);
end;

procedure TMainForm.ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
var
  InfoText: string;
  KeyState: TKeyboardState;
begin
  if PanelSearch.Visible then
    Editor.Margins.Bottom := 0
  else
    Editor.Margins.Bottom := 5;
  if Editor.Modified then
    InfoText := 'Modified'
  else
    InfoText := '';

  if StatusBar.Panels[2].Text <> InfoText then
    StatusBar.Panels[2].Text := InfoText;
  GetKeyboardState(KeyState);
  if KeyState[VK_INSERT] = 0 then
    if StatusBar.Panels[1].Text <> 'Insert' then
      StatusBar.Panels[1].Text := 'Insert';
  if KeyState[VK_INSERT] = 1 then
    if StatusBar.Panels[1].Text <> 'Overwrite' then
      StatusBar.Panels[1].Text := 'Overwrite';
end;

procedure TMainForm.DoSearchTextChange;
begin
  if Assigned(Editor) then
    Editor.Search.SearchText := ComboBoxSearchText.Text;
  SetMatchesFound;
  StatusBar.Panels[3].Text := Editor.SearchStatus
end;

procedure TMainForm.ComboBoxSearchTextChange(Sender: TObject);
begin
  inherited;
  if (soSearchOnTyping in Editor.Search.Options) or (ComboBoxSearchText.Text = '') then
    DoSearchTextChange;
end;

procedure TMainForm.ComboBoxSearchTextKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if (Key = #13) or (Key = #10) then
  begin
    if Assigned(Editor) then
      if Editor.CanFocus then
        Editor.SetFocus;
    if ComboBoxSearchText.Items.IndexOf(ComboBoxSearchText.Text) = -1 then
      ComboBoxSearchText.Items.Add(ComboBoxSearchText.Text);
    Key := #0;

    DoSearchTextChange;
  end;
end;

procedure TMainForm.SetMatchesFound;
var
  LLabel: string;
begin
  LLabel := '';

  if Assigned(Editor) and (Editor.SearchResultCount > 1) then
    LLabel := 'es';
  if Assigned(Editor) and (Editor.SearchResultCount > 0) then
    LLabel := Format('%d match%s found', [Editor.SearchResultCount, LLabel]);

  LabelSearchResultCount.Caption := LLabel;
end;

procedure TMainForm.TitleBarItems2Click(Sender: TObject);
var
  LPoint: TPoint;
begin
  inherited;

  if not Assigned(FPopupEncodingDialog) then
  begin
    FPopupEncodingDialog := TPopupEncodingDialog.Create(Self);
    FPopupEncodingDialog.PopupParent := Self;
    FPopupEncodingDialog.OnSelectEncoding := SelectedEncodingClick;
  end;

  LPoint := GetTitleBarItemLeftBottom(TITLE_BAR_ENCODING);
  FPopupEncodingDialog.Left := LPoint.X;
  FPopupEncodingDialog.Top := LPoint.Y;

  LockFormPaint;
  FPopupEncodingDialog.Execute(TitleBar.Items[TITLE_BAR_ENCODING].Caption);
  UnlockFormPaint;
end;

procedure TMainForm.TitleBarItems4Click(Sender: TObject);
var
  LPoint: TPoint;
begin
  inherited;
  if not Assigned(FPopupHighlighterDialog) then
  begin
    FPopupHighlighterDialog := TPopupHighlighterDialog.Create(Self);
    FPopupHighlighterDialog.PopupParent := Self;
    FPopupHighlighterDialog.OnSelectHighlighter := SelectedHighlighterClick;
  end;

  LPoint := GetTitleBarItemLeftBottom(TITLE_BAR_HIGHLIGHTER);
  FPopupHighlighterDialog.Left := LPoint.X;
  FPopupHighlighterDialog.Top := LPoint.Y;

  LockFormPaint;
  FPopupHighlighterDialog.Execute(FHighlighterStrings, TitleBar.Items[TITLE_BAR_HIGHLIGHTER].Caption);
  UnlockFormPaint;
end;

procedure TMainForm.TitleBarItems6Click(Sender: TObject);
var
  LPoint: TPoint;
begin
  inherited;
  if not Assigned(FPopupHighlighterColorDialog) then
  begin
    FPopupHighlighterColorDialog := TPopupHighlighterColorDialog.Create(Self);
    FPopupHighlighterColorDialog.PopupParent := Self;
    FPopupHighlighterColorDialog.OnSelectHighlighterColor := SelectedHighlighterColorClick;
  end;

  LPoint := GetTitleBarItemLeftBottom(TITLE_BAR_COLORS);
  FPopupHighlighterColorDialog.Left := LPoint.X;
  FPopupHighlighterColorDialog.Top := LPoint.Y;

  LockFormPaint;
  FPopupHighlighterColorDialog.Execute(FHighlighterColorStrings, TitleBar.Items[TITLE_BAR_COLORS].Caption);
  UnlockFormPaint;
end;

procedure TMainForm.ClearText;
begin
  ComboBoxSearchText.Text := '';
  if Assigned(Editor) then
    Editor.Search.SearchText := '';
  SetMatchesFound;
end;

procedure TMainForm.EditorCaretChanged(Sender: TObject; X, Y: Integer);
var
  LInfoText: string;
begin
  inherited;
  LInfoText := Format('%d: %d', [Y, X]);
  if StatusBar.Panels[0].Text <> LInfoText then
    StatusBar.Panels[0].Text := LInfoText;
end;

procedure TMainForm.InitializeEditorPrint(AEditorPrint: TBCEditorPrint);
begin
  with AEditorPrint.Header do
  begin
    Clear;
    FrameTypes := [ftLine];
    Add(Editor.DocumentName, nil, taLeftJustify, 1);
    Add('$DATE$ $TIME$', nil, taRightJustify, 1);
  end;

  with AEditorPrint.Footer do
  begin
    Clear;
    FrameTypes := [ftLine];
    Add(Format('Printed by', [Application.Title]), nil, taLeftJustify, 1);
    Add('Page: $PAGENUM$ of $PAGECOUNT$', nil, taRightJustify, 1);
  end;

  AEditorPrint.LineNumbersInMargin := True;
  AEditorPrint.LineNumbers := True;
  AEditorPrint.Wrap := False;
  AEditorPrint.Colors := True;
  AEditorPrint.Editor := Editor;
  AEditorPrint.Title := Editor.DocumentName;
end;

procedure TMainForm.ActionFindNextExecute(Sender: TObject);
begin
  inherited;
  if Editor.Search.SearchText <> ComboBoxSearchText.Text then
    Editor.Search.SearchText := ComboBoxSearchText.Text
  else
    Editor.FindNext;
end;

procedure TMainForm.ActionFindPreviousExecute(Sender: TObject);
begin
  inherited;
  if Editor.Search.SearchText <> ComboBoxSearchText.Text then
    Editor.Search.SearchText := ComboBoxSearchText.Text
  else
    Editor.FindPrevious;
end;

procedure TMainForm.ActionInSelectionExecute(Sender: TObject);
begin
  inherited;

  ActionInSelection.Checked := not ActionInSelection.Checked;
  Editor.Search.InSelection.Active := ActionInSelection.Checked;
end;

procedure TMainForm.ActionOptionsExecute(Sender: TObject);
begin
  inherited;
  if Assigned(Editor) then
    TSearchOptionsDialog.ClassShowModal(Editor);
end;

procedure TMainForm.ActionPreviewExecute(Sender: TObject);
begin
  PrintPreview
end;

procedure TMainForm.PrintPreview;
begin
  with PrintPreviewDialog do
  begin
    InitializeEditorPrint(PrintPreview.EditorPrint);
    ShowModal;
  end;
end;

procedure TMainForm.ActionCaseSensitiveExecute(Sender: TObject);
begin
  inherited;

  ActionCaseSensitive.Checked := not ActionCaseSensitive.Checked;
  Editor.Search.SetOption(soCaseSensitive, ActionCaseSensitive.Checked);
end;

procedure TMainForm.ActionCloseExecute(Sender: TObject);
begin
  inherited;
  PanelSearch.Visible := False;
  Editor.Search.Enabled := False;
end;

procedure TMainForm.ActionFileOpenExecute(Sender: TObject);
var
  LFileName: string;
begin
  OpenDialog.Title := 'Open';
  if OpenDialog.Execute(Handle) then
  begin
    FStopWatch.Reset;
    FStopWatch.Start;
    LFileName := OpenDialog.Files[0];
    TitleBar.Items[TITLE_BAR_CAPTION].Caption := Format('%s %s - %s', [BCEDITORDEMO_CAPTION, BCEDITOR_VERSION, LFileName]);
    Editor.LoadFromFile(LFileName);
    TitleBar.Items[TITLE_BAR_ENCODING].Caption := EncodingToText(Editor.Encoding);
    FStopWatch.Stop;
    StatusBar.Panels[3].Text := 'Load: ' + FormatDateTime('s.zzz "s"', FStopWatch.ElapsedMilliseconds / MSecsPerDay);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  inherited;

  ObjectInspector.AddUnlistedProperties(['Ctl3D', 'CustomHint', 'Hint', 'HelpContext', 'HelpKeyword', 'HelpType',
    'ImeMode', 'ImeName', 'ParentColor', 'ParentCtl3D', 'ParentCustomHint', 'ParentFont', 'ParentShowHint', 'ShowHint']);
  ObjectInspector.InspectedObject := Editor;
  ObjectInspector.SkinManager := SkinManager;

  TitleBar.Items[TITLE_BAR_CAPTION].Caption := BCEDITORDEMO_CAPTION + ' ' + BCEDITOR_VERSION;
  SkinManager.ExtendedBorders := True;
  { IDE can lose these properties }
  PopupMenuFile.Images := ImagesDataModule.ImageList;
  TitleBar.Images := ImagesDataModule.ImageListSmall;

  FHighlighterStrings := GetHighlighters;
  FHighlighterColorStrings := GetHighlighterColors;

  SelectedHighlighterClick('Object Pascal');
  SelectedHighlighterColorClick('Default');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FHighlighterStrings.Free;
  FHighlighterColorStrings.Free;

  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if Editor.CanFocus then
    Editor.SetFocus;
end;

function TMainForm.GetTitleBarItemLeftBottom(AIndex: Integer): TPoint;
var
  LRect: TRect;
begin
  Result.X := TitleBar.Items[AIndex].Rect.Left;
  Result.Y := TitleBar.Items[AIndex].Rect.Bottom;

  if Assigned(TitleBar.Items[AIndex].ExtForm) then
  begin
    Inc(Result.X, TitleBar.Items[AIndex].ExtForm.Left);
    Inc(Result.Y, TitleBar.Items[AIndex].ExtForm.Top);
  end
  else
  begin
    GetWindowRect(Handle, LRect);
    Inc(Result.Y, LRect.Top);
    Inc(Result.X, LRect.Left);
  end;
end;

procedure TMainForm.ActionSearchEngineExecute(Sender: TObject);
begin
  inherited;
  if not Assigned(FPopupSearchEngineDialog) then
  begin
    FPopupSearchEngineDialog := TBCPopupSearchEngineDialog.Create(Self);
    FPopupSearchEngineDialog.PopupParent := Self;
    FPopupSearchEngineDialog.OnSelectSearchEngine := SelectedSearchEngineClick;
  end;
  LockFormPaint;
  FPopupSearchEngineDialog.Execute(Editor.Search.Engine);
  UnlockFormPaint;
end;

procedure TMainForm.SelectedSearchEngineClick(ASearchEngine: TBCEditorSearchEngine);
begin
  Editor.Search.Engine := ASearchEngine;
end;

procedure TMainForm.ActionSearchExecute(Sender: TObject);
begin
  PanelSearch.Visible := True;
  if Editor.SelectionAvailable and (Editor.SelectionBeginPosition.Line = Editor.SelectionEndPosition.Line) then
    Editor.Search.SearchText := Editor.SelectedText
  else
    Editor.Search.SearchText := Editor.Search.SearchText;
  ComboBoxSearchText.Text := Editor.Search.SearchText;
  ComboBoxSearchText.SetFocus;
  Editor.Search.Enabled := True;
  SetMatchesFound;
end;

procedure TMainForm.SelectedEncodingClick(const AIndex: Integer);
begin
  SetEncoding(Editor, AIndex);
  TitleBar.Items[TITLE_BAR_ENCODING].Caption := EncodingToText(Editor.Encoding);
end;

procedure TMainForm.SelectedHighlighterClick(AHighlighterName: string);
begin
  with Editor do
  begin
    Highlighter.LoadFromFile(Format('%s.json', [AHighlighterName]));
    CodeFolding.Visible := Highlighter.CodeFoldingRangeCount > 0;
  end;
  TitleBar.Items[TITLE_BAR_HIGHLIGHTER].Caption := Editor.Highlighter.Name;
  Editor.Lines.Text := Editor.Highlighter.Info.General.Sample;
  Editor.MoveCaretToBOF;
  StatusBar.Panels[3].Text := '';
  Caption := BCEDITORDEMO_CAPTION;
  ClearText;
end;

procedure TMainForm.SelectedHighlighterColorClick(AHighlighterColorName: string);
begin
  with Editor do
  begin
    Highlighter.Colors.LoadFromFile(Format('%s.json', [AHighlighterColorName]));
    Invalidate;
  end;
  TitleBar.Items[TITLE_BAR_COLORS].Caption := Editor.Highlighter.Colors.Name;
end;

procedure TMainForm.LockFormPaint;
begin
  SkinProvider.SkinData.BeginUpdate;
  SkinProvider.Form.Perform(WM_SETREDRAW, 0, 0);
end;

procedure TMainForm.UnlockFormPaint;
begin
  SkinProvider.SkinData.EndUpdate;
  SkinProvider.Form.Perform(WM_SETREDRAW, 1, 0);
end;

end.
