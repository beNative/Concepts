unit TBCEditorDemo.Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  BCCommon.Form.Base, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, BCEditor.Editor, BCEditor.Highlighter,
  BCEditor.Editor.Base, Vcl.Buttons, Vcl.AppEvnts, System.Actions, Vcl.ActnList, BCEditor.Print,
  BCCommon.Images, BCComponent.SkinProvider, BCComponent.SkinManager, BCControl.Panel, BCControl.StatusBar,
  BCComponent.TitleBar, Vcl.Menus, ToolCtrlsEh, DBGridEhToolCtrls, EhLibVCL, DBAxisGridsEh, ObjectInspectorEh,
  BCControl.Splitter, GridsEh, BCCommon.Frame.Base, sPanel, BCComponent.MultiStringHolder, sSkinManager, sStatusBar,
  sSplitter, acTitleBar, sSkinProvider, System.Win.TaskbarCore, Vcl.Taskbar, sDialogs, Vcl.StdCtrls, sButton,
  BCControl.Button, System.Diagnostics, BCCommon.Dialog.Popup.Highlighter, BCCommon.Dialog.Popup.Highlighter.Color,
  sSpeedButton, BCControl.SpeedButton, sComboBox, BCControl.ComboBox, sLabel, sMemo;

const
  BCEDITORDEMO_CAPTION = 'TBCEditor Control Demo v1.3';
  TITLE_BAR_CAPTION = 1;
  TITLE_BAR_HIGHLIGHTER = 2;
  TITLE_BAR_COLORS = 4;

type
  TMainForm = class(TBCBaseForm)
    ActionFileOpen: TAction;
    ActionPreview: TAction;
    ActionSearch: TAction;
    Editor: TBCEditor;
    MenuItemExit: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemPrintPreview: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    MenuItemSeparator2: TMenuItem;
    MultiStringHolderFileTypes: TBCMultiStringHolder;
    ObjectInspectorEh: TObjectInspectorEh;
    PanelLeft: TBCPanel;
    PanelProperty: TBCPanel;
    PopupMenuFile: TPopupMenu;
    PopupMenuDummy: TPopupMenu;
    Splitter: TBCSplitter;
    OpenDialog: TsOpenDialog;
    MenuItemSkins: TMenuItem;
    ActionSkins: TAction;
    PanelSearch: TBCPanel;
    ComboBoxSearchText: TBCComboBox;
    BCSplitter1: TBCSplitter;
    SpeedButtonFindPrevious: TBCSpeedButton;
    SpeedButtonFindNext: TBCSpeedButton;
    SpeedButtonDivider: TBCSpeedButton;
    SpeedButtonOptions: TBCSpeedButton;
    SpeedButtonClose: TBCSpeedButton;
    PanelRight: TBCPanel;
    ActionFindNext: TAction;
    ActionFindPrevious: TAction;
    ActionOptions: TAction;
    ActionClose: TAction;
    LabelSearchResultCount: TsLabel;
    SplitterUndo: TBCSplitter;
    PanelUndo: TBCPanel;
    MemoRedo: TsMemo;
    BCSplitter3: TBCSplitter;
    MemoUndo: TsMemo;
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionPreviewExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditorCaretChanged(Sender: TObject; X, Y: Integer);
    procedure ActionSkinsExecute(Sender: TObject);
    procedure SelectedHighlighterClick(AHighlighterName: string);
    procedure SelectedHighlighterColorClick(AHighlighterColorName: string);
    procedure TitleBarItems2Click(Sender: TObject);
    procedure TitleBarItems4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionFindPreviousExecute(Sender: TObject);
    procedure ActionOptionsExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ComboBoxSearchTextChange(Sender: TObject);
    procedure ComboBoxSearchTextKeyPress(Sender: TObject; var Key: Char);
  private
    FStopWatch: TStopWatch;
    FPopupHighlighterDialog: TPopupHighlighterDialog;
    FPopupHighlighterColorDialog: TPopupHighlighterColorDialog;
    FHighlighterStrings: TStringList;
    FHighlighterColorStrings: TStringList;
    function GetTitleBarItemLeftBottom(AIndex: Integer): TPoint;
    procedure ClearText;
    procedure InitializeEditorPrint(EditorPrint: TBCEditorPrint);
    procedure LockFormPaint;
    procedure PopulateUndoList(AMemo: TsMemo);
    procedure PrintPreview;
    procedure SetMatchesFound;
    procedure UnlockFormPaint;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  BCCommon.Language.Strings, BCCommon.Form.Print.Preview, BCEditor.Print.Types, BCCommon.StringUtils,
  BCCommon.Dialog.SkinSelect, BCCommon.FileUtils, BCEditor.Types, BCCommon.Dialog.Options.Search,
  BCEditor.Editor.Undo.Item, BCEditor.Editor.Undo.List;

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
  PanelSearch.Visible := Editor.Search.Enabled;
  if PanelSearch.Visible then
    Editor.Margins.Bottom := 0
  else
    Editor.Margins.Bottom := 5;
  if Editor.Modified then
    InfoText := LanguageDataModule.GetConstant('Modified')
  else
    InfoText := '';
  if StatusBar.Panels[2].Text <> InfoText then
    StatusBar.Panels[2].Text := InfoText;
  GetKeyboardState(KeyState);
  if KeyState[VK_INSERT] = 0 then
    if StatusBar.Panels[1].Text <> LanguageDataModule.GetConstant('Insert') then
      StatusBar.Panels[1].Text := LanguageDataModule.GetConstant('Insert');
  if KeyState[VK_INSERT] = 1 then
    if StatusBar.Panels[1].Text <> LanguageDataModule.GetConstant('Overwrite') then
      StatusBar.Panels[1].Text := LanguageDataModule.GetConstant('Overwrite');
end;

procedure TMainForm.ComboBoxSearchTextChange(Sender: TObject);
begin
  inherited;
  if soSearchOnTyping in Editor.Search.Options then
  begin
    if Assigned(Editor) then
      Editor.Search.SearchText := ComboBoxSearchText.Text;
    SetMatchesFound;
  end;
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
  end;
end;

procedure TMainForm.SetMatchesFound;
var
  s: string;
begin
  s := '';

  if Assigned(Editor) and (Editor.SearchResultCount > 1) then
    s := LanguageDataModule.GetConstant('MatchFoundPluralExtension');
  if Assigned(Editor) and (Editor.SearchResultCount > 0) then
    s := Format(LanguageDataModule.GetConstant('MatchFound'), [Editor.SearchResultCount, s]);

  LabelSearchResultCount.Caption := s;
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
  InfoText: string;
begin
  inherited;
  InfoText := Format('%d: %d', [Y, X]);
  if StatusBar.Panels[0].Text <> InfoText then
    StatusBar.Panels[0].Text := InfoText;
  {$IFDEF DEBUG}
  PopulateUndoList(MemoUndo);
  PopulateUndoList(MemoRedo);
  {$ENDIF}
end;

procedure TMainForm.InitializeEditorPrint(EditorPrint: TBCEditorPrint);
var
  Alignment: TAlignment;

  procedure SetHeaderFooter(Option: Integer; Value: string);
  begin
    case Option of
      0, 1:
        with EditorPrint.Footer do
        begin
          case Option of
            0:
              Alignment := taLeftJustify;
            1:
              Alignment := taRightJustify;
          end;
          Add(Value, nil, Alignment, 1);
        end;
      2, 3:
        with EditorPrint.Header do
        begin
          case Option of
            2:
              Alignment := taLeftJustify;
            3:
              Alignment := taRightJustify;
          end;
          Add(Value, nil, Alignment, 1);
        end;
    end;
  end;

begin
  EditorPrint.Header.Clear;
  EditorPrint.Footer.Clear;

  SetHeaderFooter(0, Format(LanguageDataModule.GetConstant('PrintedBy'), [Application.Title]));
  SetHeaderFooter(1, LanguageDataModule.GetConstant('PreviewDocumentPage'));
  SetHeaderFooter(2, Editor.DocumentName);
  SetHeaderFooter(3, '$DATE$ $TIME$');

  EditorPrint.Header.FrameTypes := [ftLine];
  EditorPrint.Footer.FrameTypes := [ftLine];
  EditorPrint.LineNumbersInMargin := True;
  EditorPrint.LineNumbers := True;
  EditorPrint.Wrap := False;
  EditorPrint.Colors := True;

  EditorPrint.Editor := Editor;
  EditorPrint.Title := Editor.DocumentName;
end;

procedure TMainForm.ActionFindNextExecute(Sender: TObject);
begin
  inherited;
  if Assigned(Editor) then
    Editor.FindNext;
end;

procedure TMainForm.ActionFindPreviousExecute(Sender: TObject);
begin
  inherited;
  if Assigned(Editor) then
    Editor.FindPrevious;
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

procedure TMainForm.ActionCloseExecute(Sender: TObject);
begin
  inherited;
  if Assigned(Editor) then
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
    TitleBar.Items[TITLE_BAR_CAPTION].Caption := Format('%s - %s', [BCEDITORDEMO_CAPTION, LFileName]);
    Editor.LoadFromFile(LFileName);
    FStopWatch.Stop;
    StatusBar.Panels[3].Text := 'Load: ' + FormatDateTime('s.zzz "s"', FStopWatch.ElapsedMilliseconds / MSecsPerDay);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  TitleBar.Items[TITLE_BAR_CAPTION].Caption := BCEDITORDEMO_CAPTION;
  SkinManager.ExtendedBorders := True;
  { IDE can lose these properties }
  PopupMenuFile.Images := ImagesDataModule.ImageList;
  TitleBar.Images := ImagesDataModule.ImageListSmall;

  FHighlighterStrings := GetHighlighters;
  FHighlighterColorStrings := GetHighlighterColors;

  SelectedHighlighterClick('Object Pascal');
  SelectedHighlighterColorClick('Default');

  {$IFDEF DEBUG}
  PanelUndo.Visible := True;
  SplitterUndo.Visible := True;
  {$ELSE}
  ClientWidth := 1100;
  ClientHeight := 644;
  {$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FHighlighterStrings.Free;
  FHighlighterColorStrings.Free;

  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ObjectInspectorEh.Component := Editor;
  ObjectInspectorEh.LabelColWidth := 145;
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

procedure TMainForm.TitleBarItems2Click(Sender: TObject);
var
  LPoint: TPoint;
begin
  inherited;

  if Assigned(FPopupHighlighterDialog) then
    FPopupHighlighterDialog := nil
  else
  begin
    FPopupHighlighterDialog := TPopupHighlighterDialog.Create(Self);
    FPopupHighlighterDialog.PopupParent := Self;
    FPopupHighlighterDialog.OnSelectHighlighter := SelectedHighlighterClick;

    LPoint := GetTitleBarItemLeftBottom(TITLE_BAR_HIGHLIGHTER);

    FPopupHighlighterDialog.Left := LPoint.X;
    FPopupHighlighterDialog.Top := LPoint.Y;

    LockFormPaint;

    FPopupHighlighterDialog.Execute(FHighlighterStrings, TitleBar.Items[TITLE_BAR_HIGHLIGHTER].Caption);

    UnlockFormPaint;

    while Assigned(FPopupHighlighterDialog) and FPopupHighlighterDialog.Visible do
      Application.HandleMessage;
    FPopupHighlighterDialog := nil;
  end;
end;

procedure TMainForm.TitleBarItems4Click(Sender: TObject);
var
  LPoint: TPoint;
begin
  inherited;
  if Assigned(FPopupHighlighterColorDialog) then
    FPopupHighlighterColorDialog := nil
  else
  begin
    FPopupHighlighterColorDialog := TPopupHighlighterColorDialog.Create(Self);
    FPopupHighlighterColorDialog.PopupParent := Self;
    FPopupHighlighterColorDialog.OnSelectHighlighterColor := SelectedHighlighterColorClick;

    LPoint := GetTitleBarItemLeftBottom(TITLE_BAR_COLORS);

    FPopupHighlighterColorDialog.Left := LPoint.X;
    FPopupHighlighterColorDialog.Top := LPoint.Y;

    LockFormPaint;

    FPopupHighlighterColorDialog.Execute(FHighlighterColorStrings, TitleBar.Items[TITLE_BAR_COLORS].Caption);

    UnlockFormPaint;

    while Assigned(FPopupHighlighterColorDialog) and FPopupHighlighterColorDialog.Visible do
      Application.HandleMessage;
    FPopupHighlighterColorDialog := nil;
  end;
end;

procedure TMainForm.ActionSearchExecute(Sender: TObject);
begin
  Editor.Search.Enabled := True;
  Application.ProcessMessages; { search frame visible }
  ComboBoxSearchText.SetFocus;
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
  Editor.CaretZero;
  StatusBar.Panels[3].Text := '';
  Caption := BCEDITORDEMO_CAPTION;
  ClearText;

  if Assigned(FPopupHighlighterDialog) then
  begin
    FPopupHighlighterDialog.Visible := False;
    FPopupHighlighterDialog := nil;
  end;
end;

procedure TMainForm.SelectedHighlighterColorClick(AHighlighterColorName: string);
begin
  with Editor do
  begin
    Highlighter.Colors.LoadFromFile(Format('%s.json', [AHighlighterColorName]));
    Invalidate;
  end;
  TitleBar.Items[TITLE_BAR_COLORS].Caption := Editor.Highlighter.Colors.Name;
  if Assigned(FPopupHighlighterColorDialog) then
  begin
    FPopupHighlighterColorDialog.Visible := False;
    FPopupHighlighterColorDialog := nil;
  end;
end;

procedure TMainForm.PopulateUndoList(AMemo: TsMemo);
var
  i: Integer;
  LUndoItem: TBCEditorUndoItem;
  LUndoList: TBCEditorUndoList;
  LMemo: TsMemo;

  function GetReasonText(AReason: TBCEditorChangeReason): string;
  begin
    case AReason of
      crInsert: Result := 'Insert';
      crPaste: Result := 'Paste';
      crDragDropInsert: Result := 'DragDropInsert';
      crDelete: Result := 'Delete';
      crLineBreak: Result := 'LineBreak';
      crIndent: Result := 'Indent';
      crUnindent: Result := 'Unindent';
      crCaret: Result := 'Caret';
      crSelection: Result := 'Selection';
      crNothing: Result := 'Nothing';
      crGroupBreak: Result := 'GroupBreak';
    end;
  end;

begin
  LUndoList := nil;
  LMemo := AMemo;
  if LMemo = MemoUndo then
    LUndoList := Editor.UndoList
  else
  if LMemo = MemoRedo then
    LUndoList := Editor.RedoList;

  if LMemo.Lines.Count = LUndoList.ItemCount then
    Exit;

  LMemo.Lines.BeginUpdate;
  LMemo.Clear;
  if Assigned(LUndoList) then
  for i := 0 to LUndoList.ItemCount - 1 do
  begin
    LUndoItem := LUndoList.Items[i];
    LMemo.Lines.Add(Format('%d: %s c: (%d, %d) b: (%d, %d), e: (%d, %d), s: %s',
      [LUndoItem.ChangeBlockNumber,
      GetReasonText(LUndoItem.ChangeReason),
      LUndoItem.ChangeCaretPosition.Char,
      LUndoItem.ChangeCaretPosition.Line,
      LUndoItem.ChangeBeginPosition.Char,
      LUndoItem.ChangeBeginPosition.Line,
      LUndoItem.ChangeEndPosition.Char,
      LUndoItem.ChangeEndPosition.Line,
      LUndoItem.ChangeString]));
  end;
  LMemo.Lines.EndUpdate;
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
