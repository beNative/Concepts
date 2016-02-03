unit BCCommon.Dialog.FindInFiles;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  BCControl.ComboBox, Vcl.ActnList, BCCommon.Dialog.Base,
  sComboBox, BCControl.SpeedButton, BCControl.Panel,
  BCControl.GroupBox, sLabel, acSlider, System.Actions, Vcl.Buttons, sSpeedButton, sGroupBox, Vcl.ExtCtrls, sPanel;

type
  TFindInFilesDialog = class(TBCBaseDialog)
    ActionDirectoryButtonClick: TAction;
    ActionList: TActionList;
    ButtonCancel: TButton;
    ButtonFind: TButton;
    ComboBoxDirectory: TBCComboBox;
    GroupBoxSearchDirectoryOptions: TBCGroupBox;
    GroupBoxSearchOptions: TBCGroupBox;
    PanelButtons: TBCPanel;
    PanelDirectoryComboBoxClient: TBCPanel;
    PanelDirectoryComboBoxAndButton: TBCPanel;
    SpeedButtonDirectory: TBCSpeedButton;
    ComboBoxFileMask: TBCComboBox;
    SliderCaseSensitive: TsSlider;
    StickyLabelCaseSensitive: TsStickyLabel;
    PanelIncludeSubdirectories: TBCPanel;
    StickyLabelIncludeSubdirectories: TsStickyLabel;
    SliderIncludeSubDirectories: TsSlider;
    PanelFileMaskComboBoxRight: TBCPanel;
    PanelFileMaskButton: TBCPanel;
    SpeedButtonFileMask: TBCSpeedButton;
    ActionFileMaskItemsButtonClick: TAction;
    ActionDirectoryItemsButtonClick: TAction;
    PanelTextToFind: TBCPanel;
    PanelTextToFindRight: TBCPanel;
    PanelTextToFindButton: TBCPanel;
    SpeedButtonTextToFind: TBCSpeedButton;
    PanelTextToFindClient: TBCPanel;
    ComboBoxTextToFind: TBCComboBox;
    ActionTextToFindItemsButtonClick: TAction;
    procedure ActionDirectoryButtonClickExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionFileMaskItemsButtonClickExecute(Sender: TObject);
    procedure ActionDirectoryItemsButtonClickExecute(Sender: TObject);
    procedure ComboBoxTextToFindChange(Sender: TObject);
    procedure ActionTextToFindItemsButtonClickExecute(Sender: TObject);
  private
    function GetFileTypeText: string;
    function GetFindWhatText: string;
    function GetFolderText: string;
    function GetLookInSubfolders: Boolean;
    function GetSearchCaseSensitive: Boolean;
    procedure SetButtons;
    procedure SetFindWhatText(AValue: string);
    procedure SetFolderText(Value: string);
    procedure ReadIniFile;
    procedure WriteIniFile;
  public
    function GetFileExtensions(AFileExtensions: string): string;
    property FileTypeText: string read GetFileTypeText;
    property FindWhatText: string read GetFindWhatText write SetFindWhatText;
    property FolderText: string read GetFolderText write SetFolderText;
    property LookInSubfolders: Boolean read GetLookInSubfolders;
    property SearchCaseSensitive: Boolean read GetSearchCaseSensitive;
  end;

function FindInFilesDialog: TFindInFilesDialog;

implementation

{$R *.dfm}

uses
  BCCommon.Language.Strings, BCCommon.Utils, System.IniFiles,
  {$WARNINGS OFF}
  Vcl.FileCtrl, { warning: FileCtrl is specific to a platform }
  {$WARNINGS ON}
  BCCommon.FileUtils, BCCommon.Dialog.ItemList;

var
  LFindInFilesDialog: TFindInFilesDialog;

function FindInFilesDialog: TFindInFilesDialog;
begin
  if not Assigned(LFindInFilesDialog) then
    Application.CreateForm(TFindInFilesDialog, LFindInFilesDialog);
  Result := LFindInFilesDialog;
  Result.ReadIniFile;
end;

procedure TFindInFilesDialog.FormDestroy(Sender: TObject);
begin
  LFindInFilesDialog := nil;
end;

procedure TFindInFilesDialog.FormShow(Sender: TObject);
begin
  inherited;
  SetButtons;
  if ComboBoxTextToFind.CanFocus then
    ComboBoxTextToFind.SetFocus;
end;

function TFindInFilesDialog.GetFindWhatText: string;
begin
  Result := ComboBoxTextToFind.Text;
end;

procedure TFindInFilesDialog.SetFindWhatText(AValue: string);
begin
  ComboBoxTextToFind.Text := AValue;
  SetButtons;
end;

function TFindInFilesDialog.GetFileExtensions(AFileExtensions: string): string;
var
  i: Integer;
  s, LFileExtension: string;

  procedure AddFileExtension;
  begin
    if Pos(LFileExtension, Result) = 0 then
      Result := Result + LFileExtension;
  end;

begin
  Result := AFileExtensions;
  for i := 1 to ComboBoxFileMask.Items.Count - 1 do { start from 1 because first is *.* }
  begin
    s := ComboBoxFileMask.Items[i];
    while Pos(';', s) <> 0 do
    begin
      LFileExtension := Copy(s, 1, Pos(';', s));
      AddFileExtension;
      s := Copy(s, Pos(';', s) + 1, Length(s));
    end;
    LFileExtension := s + ';';
    AddFileExtension;
  end;
end;

function TFindInFilesDialog.GetFileTypeText: string;
begin
  Result := ComboBoxFileMask.Text;
end;

function TFindInFilesDialog.GetFolderText: string;
begin
  Result := Trim(ComboBoxDirectory.Text);
  {$WARNINGS OFF} { IncludeTrailingBackslash is specific to a platform }
  if Result <> '' then
    Result := IncludeTrailingBackslash(Result);
  {$WARNINGS ON}
end;

procedure TFindInFilesDialog.SetFolderText(Value: string);
begin
  ComboBoxDirectory.Text := Value;
end;

function TFindInFilesDialog.GetSearchCaseSensitive: Boolean;
begin
  Result := SliderCaseSensitive.SliderOn;
end;

function TFindInFilesDialog.GetLookInSubfolders: Boolean;
begin
  Result := SliderIncludeSubdirectories.SliderOn;
end;

procedure TFindInFilesDialog.SetButtons;
begin
  ButtonFind.Enabled := Trim(ComboBoxTextToFind.Text) <> '';
end;

procedure TFindInFilesDialog.ActionDirectoryItemsButtonClickExecute(Sender: TObject);
begin
  with TItemListDialog.Create(Self) do
  try
    Caption := LanguageDataModule.GetConstant('DirectoryItems');
    ListBox.Items.Assign(ComboBoxDirectory.Items);
    if ShowModal = mrOk then
      ComboBoxDirectory.Items.Assign(ListBox.Items);
  finally
    Free;
  end;
end;

procedure TFindInFilesDialog.ActionFileMaskItemsButtonClickExecute(Sender: TObject);
begin
  with TItemListDialog.Create(Self) do
  try
    Caption := LanguageDataModule.GetConstant('FileMaskItems');
    ListBox.Items.Assign(ComboBoxFileMask.Items);
    if ShowModal = mrOk then
      ComboBoxFileMask.Items.Assign(ListBox.Items);
  finally
    Free;
  end;
end;

procedure TFindInFilesDialog.ActionTextToFindItemsButtonClickExecute(Sender: TObject);
begin
  with TItemListDialog.Create(Self) do
  try
    Caption := LanguageDataModule.GetConstant('TextToFindItems');
    ListBox.Items.Assign(ComboBoxTextToFind.Items);
    if ShowModal = mrOk then
      ComboBoxTextToFind.Items.Assign(ListBox.Items);
  finally
    Free;
  end;
end;

procedure TFindInFilesDialog.ComboBoxTextToFindChange(Sender: TObject);
begin
  SetButtons;
end;

procedure TFindInFilesDialog.ActionDirectoryButtonClickExecute(Sender: TObject);
var
  Dir: string;
begin
  Dir := ComboBoxDirectory.Text;
  if Vcl.FileCtrl.SelectDirectory(LanguageDataModule.GetConstant('SelectRootDirectory'), '', Dir, [sdNewFolder,
    sdShowShares, sdNewUI, sdValidateDir], Self) then
    ComboBoxDirectory.Text := Dir;
end;

procedure TFindInFilesDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOK then
    WriteIniFile;
end;

procedure TFindInFilesDialog.ReadIniFile;
var
  LItems: TStrings;
begin
  LItems := TStringList.Create;
  with TIniFile.Create(GetIniFilename) do
  try
    SliderCaseSensitive.SliderOn := ReadBool('FindInFilesOptions', 'CaseSensitive', False);
    SliderIncludeSubDirectories.SliderOn := ReadBool('FindInFilesOptions', 'IncludeSubDirectories', True);

    ReadSectionValues('TextToFindItems', LItems);
    InsertItemsToComboBox(LItems, ComboBoxTextToFind);
    ReadSectionValues('FindInFilesFileMasks', LItems);
    InsertItemsToComboBox(LItems, ComboBoxFileMask);
    ReadSectionValues('FindInFilesDirectories', LItems);
    InsertItemsToComboBox(LItems, ComboBoxDirectory);

    ComboBoxTextToFind.ItemIndex := ComboBoxTextToFind.Items.IndexOf(ReadString('FindInFilesOptions', 'TextToFind', ''));
    ComboBoxFileMask.ItemIndex := ComboBoxFileMask.Items.IndexOf(ReadString('FindInFilesOptions', 'FileMask', '*.*'));
    ComboBoxDirectory.ItemIndex := ComboBoxDirectory.Items.IndexOf(ReadString('FindInFilesOptions', 'Directory', ''));
  finally
    LItems.Free;
    Free;
  end;
end;

procedure TFindInFilesDialog.WriteIniFile;
var
  i: Integer;
begin
  with TIniFile.Create(GetIniFilename) do
  try
    WriteBool('FindInFilesOptions', 'CaseSensitive', SliderCaseSensitive.SliderOn);
    WriteBool('FindInFilesOptions', 'IncludeSubDirectories', SliderIncludeSubDirectories.SliderOn);

    i := InsertTextToCombo(ComboBoxTextToFind);
    if i <> -1 then
      WriteString('TextToFindItems', IntToStr(i), ComboBoxTextToFind.Items[0]);

    i := InsertTextToCombo(ComboBoxFileMask);
    if i <> -1 then
      WriteString('FindInFilesFileMasks', IntToStr(i), ComboBoxFileMask.Items[0]);

    i := InsertTextToCombo(ComboBoxDirectory);
    if i <> -1 then
      WriteString('FindInFilesDirectories', IntToStr(i), IncludeTrailingPathDelimiter(ComboBoxDirectory.Items[0]));

    WriteString('FindInFilesOptions', 'TextToFind', ComboBoxTextToFind.Text);
    WriteString('FindInFilesOptions', 'FileMask', ComboBoxFileMask.Text);
    WriteString('FindInFilesOptions', 'Directory', IncludeTrailingPathDelimiter(ComboBoxDirectory.Text));
  finally
    Free;
  end;
end;

end.
