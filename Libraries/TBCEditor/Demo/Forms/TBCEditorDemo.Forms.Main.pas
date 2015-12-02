unit TBCEditorDemo.Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  BCCommon.Forms.Base, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, BCEditor.Editor, BCEditor.Highlighter,
  BCEditor.Editor.Base, BCCommon.Frames.Search, Vcl.Buttons, Vcl.AppEvnts, System.Actions, Vcl.ActnList, BCEditor.Print,
  BCCommon.Images, BCComponents.SkinProvider, BCComponents.SkinManager, BCControls.Panel, BCControls.StatusBar,
  BCComponents.TitleBar, Vcl.Menus, ToolCtrlsEh, DBGridEhToolCtrls, EhLibVCL, DBAxisGridsEh, ObjectInspectorEh,
  BCControls.Splitter, GridsEh, BCCommon.Frames.Base, sPanel, BCComponents.MultiStringHolder, sSkinManager, sStatusBar,
  sSplitter, acTitleBar, sSkinProvider, System.Win.TaskbarCore, Vcl.Taskbar, sDialogs, Vcl.StdCtrls, sButton,
  BCControls.Button, System.Diagnostics;

const
  BCEDITORDEMO_CAPTION = 'TBCEditor Control Demo v1.0b';

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
    PopupMenuColors: TPopupMenu;
    PopupMenuFile: TPopupMenu;
    PopupMenuHighlighters: TPopupMenu;
    Splitter: TBCSplitter;
    OpenDialog: TsOpenDialog;
    SearchFrame: TBCSearchFrame;
    MenuItemSkins: TMenuItem;
    ActionSkins: TAction;
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionPreviewExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ActionSelectHighlighterColorExecute(Sender: TObject);
    procedure ActionSelectHighlighterExecute(Sender: TObject);
    procedure ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SkinManagerGetMenuExtraLineData(FirstItem: TMenuItem; var SkinSection, Caption: string;
      var Glyph: TBitmap; var LineVisible: Boolean);
    procedure EditorCaretChanged(Sender: TObject; X, Y: Integer);
    procedure ActionSkinsExecute(Sender: TObject);
    procedure TitleBarItems2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FStopWatch: TStopWatch;
    procedure InitializeEditorPrint(EditorPrint: TBCEditorPrint);
    procedure PrintPreview;
    procedure SetHighlighterColors;
    procedure SetHighlighters;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  BCCommon.Language.Strings, BCCommon.Forms.Print.Preview, BCEditor.Print.Types, BCCommon.StringUtils,
  BCCommon.Dialogs.SkinSelect, BCCommon.FileUtils;

procedure TMainForm.ActionSelectHighlighterExecute(Sender: TObject);
begin
  with Editor do
  begin
    Highlighter.LoadFromFile(Format('%s.json', [TAction(Sender).Caption]));
    Lines.Text := Highlighter.Info.General.Sample;
    CaretZero;
    SetFocus;
  end;
  StatusBar.Panels[3].Text := '';
  TitleBar.Items[2].Caption := TAction(Sender).Caption;
  Caption := BCEDITORDEMO_CAPTION;
  SearchFrame.ClearText;
end;

procedure TMainForm.ActionSkinsExecute(Sender: TObject);
begin
  inherited;
  TSkinSelectDialog.ClassShowModal(SkinManager);
end;

procedure TMainForm.ActionSelectHighlighterColorExecute(Sender: TObject);
begin
  Editor.Highlighter.Colors.LoadFromFile(Format('%s.json', [TAction(Sender).Caption]));
  TitleBar.Items[4].Caption := TAction(Sender).Caption;
  Editor.SetFocus;
end;

procedure TMainForm.ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
var
  InfoText: string;
  KeyState: TKeyboardState;
begin
  SearchFrame.Visible := Editor.Search.Enabled;
  if SearchFrame.Visible then
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

procedure TMainForm.EditorCaretChanged(Sender: TObject; X, Y: Integer);
var
  InfoText: string;
begin
  inherited;
  InfoText := Format('%d: %d', [Y, X]);
  if StatusBar.Panels[0].Text <> InfoText then
    StatusBar.Panels[0].Text := InfoText;
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

procedure TMainForm.ActionFileOpenExecute(Sender: TObject);
var
  i: Integer;
  FileName, Ext, ItemString, Token, LFileType: string;
begin
  OpenDialog.Title := 'Open';
  if OpenDialog.Execute(Handle) then
  begin
    FStopWatch.Reset;
    FStopWatch.Start;
    FileName := OpenDialog.Files[0];
    Ext := LowerCase(ExtractFileExt(FileName));

    for i := 0 to MultiStringHolderFileTypes.MultipleStrings.Count - 1 do
    begin
      ItemString := MultiStringHolderFileTypes.MultipleStrings.Items[i].Strings.Text;
      while ItemString <> '' do
      begin
        Token := GetNextToken(';', ItemString);
        ItemString := RemoveTokenFromStart(';', ItemString);
        if Ext = Token then
        begin
          LFileType := MultiStringHolderFileTypes.MultipleStrings.Items[i].Name;
          PopupMenuHighlighters.Items.Find(LFileType[1]).Find(LFileType).Action.Execute;
          Break;
        end;
      end;
    end;
    TitleBar.Items[1].Caption := Format('%s - %s', [BCEDITORDEMO_CAPTION, FileName]);
    Editor.LoadFromFile(FileName);
    FStopWatch.Stop;
    StatusBar.Panels[3].Text := 'Load: ' + FormatDateTime('s.zzz "s"', FStopWatch.ElapsedMilliseconds / MSecsPerDay);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  inherited;

  { IDE can lose these properties }
  SearchFrame.SpeedButtonFindPrevious.Images := ImagesDataModule.ImageListSmall;
  SearchFrame.SpeedButtonFindNext.Images := ImagesDataModule.ImageListSmall;
  SearchFrame.SpeedButtonOptions.Images := ImagesDataModule.ImageListSmall;
  PopupMenuFile.Images := ImagesDataModule.ImageList;
  TitleBar.Images := ImagesDataModule.ImageListSmall;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ObjectInspectorEh.Component := Editor;
  ObjectInspectorEh.LabelColWidth := 145;

  SearchFrame.Editor := Editor;
  SetHighlighters;
  SetHighlighterColors;
end;

procedure TMainForm.SetHighlighters;
var
  LFileName, LName: string;
  LMenuItem, LSubMenuItem: TMenuItem;
  LAction: TAction;
begin
  PopupMenuHighlighters.Items.Clear;

  for LFileName in BCCommon.FileUtils.GetFiles(ExtractFilePath(Application.ExeName) + '\Highlighters\', '*.json', False) do
  begin
    LName := ChangeFileExt(ExtractFileName(LFileName), '');

    LMenuItem := PopupMenuHighlighters.Items.Find(LName[1]);
    if not Assigned(LMenuItem) then
    begin
      LMenuItem := TMenuItem.Create(PopupMenuHighlighters);
      LMenuItem.Caption := LName[1];
      LMenuItem.RadioItem := True;
      PopupMenuHighlighters.Items.Add(LMenuItem);
    end;

    LAction := TAction.Create(Self);
    LAction.Caption := LName;
    LAction.OnExecute := ActionSelectHighlighterExecute;
    LSubMenuItem := TMenuItem.Create(PopupMenuHighlighters);
    LSubMenuItem.Action := LAction;
    LSubMenuItem.RadioItem := True;
    LSubMenuItem.AutoCheck := True;
    LMenuItem.Add(LSubMenuItem);
    if LAction.Caption = 'Object Pascal' then
    begin
      LAction.Checked := True;
      LAction.Execute;
    end;
  end;
end;

procedure TMainForm.SetHighlighterColors;
var
  LFileName, LName: string;
  LMenuItem: TMenuItem;
  LAction: TAction;
begin
  PopupMenuColors.Items.Clear;
  for LFileName in BCCommon.FileUtils.GetFiles(ExtractFilePath(Application.ExeName) + '\Colors\', '*.json', False) do
  begin
    LName := ChangeFileExt(ExtractFileName(LFileName), '');

    LAction := TAction.Create(Self);
    LAction.Caption := LName;
    LAction.OnExecute := ActionSelectHighlighterColorExecute;
    LMenuItem := TMenuItem.Create(PopupMenuColors);
    LMenuItem.Action := LAction;
    LMenuItem.RadioItem := True;
    LMenuItem.AutoCheck := True;
    PopupMenuColors.Items.Add(LMenuItem);
    if LAction.Caption = 'Default' then
    begin
      LAction.Checked := True;
      LAction.Execute;
    end;
  end;
end;

procedure TMainForm.SkinManagerGetMenuExtraLineData(FirstItem: TMenuItem; var SkinSection, Caption: string;
  var Glyph: TBitmap; var LineVisible: Boolean);
begin
  inherited;

  if FirstItem = PopupMenuHighlighters.Items[0] then
  begin
    LineVisible := True;
    Caption := 'Highlighter';
  end
  else if FirstItem = PopupMenuColors.Items[0] then
  begin
    LineVisible := True;
    Caption := 'Color';
  end
  else
    LineVisible := False;
end;

procedure TMainForm.TitleBarItems2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LMenuItem: TMenuItem;
  LTitleCaption: string;
begin
  LTitleCaption := TitleBar.Items[2].Caption;
  LMenuItem := PopupMenuHighlighters.Items.Find(LTitleCaption[1]);
  if Assigned(LMenuItem) then
  begin
    LMenuItem.Checked := True;

    LMenuItem := LMenuItem.Find(LTitleCaption);
    if Assigned(LMenuItem) then
      LMenuItem.Checked := True;
  end;
end;

procedure TMainForm.ActionSearchExecute(Sender: TObject);
begin
  Editor.Search.Enabled := True;
  Application.ProcessMessages; { search frame visible }
  SearchFrame.ComboBoxSearchText.SetFocus;
end;

end.
