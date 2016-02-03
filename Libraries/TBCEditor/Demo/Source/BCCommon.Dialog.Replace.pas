unit BCCommon.Dialog.Replace;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, BCCommon.Dialog.Base,
  BCControl.ComboBox, Vcl.StdCtrls, BCControl.Panel,
  BCControl.RadioButton, BCEditor.Editor, BCEditor.Types, BCControl.Button, sComboBox,
  sRadioButton, BCControl.GroupBox, sLabel, acSlider, sButton, Vcl.ExtCtrls, sPanel, sGroupBox;

type
  TReplaceDialog = class(TBCBaseDialog)
    ButtonCancel: TBCButton;
    ButtonOK: TBCButton;
    ButtonReplaceAll: TBCButton;
    ComboBoxReplaceWith: TBCComboBox;
    ComboBoxSearchFor: TBCComboBox;
    GroupBoxOptions: TBCGroupBox;
    GroupBoxReplaceIn: TBCGroupBox;
    PanelButtons: TBCPanel;
    PanelReplaceWith: TBCPanel;
    PanelReplaceWithComboBox: TBCPanel;
    PanelSearchForComboBox: TBCPanel;
    RadioButtonAllOpenFiles: TBCRadioButton;
    RadioButtonReplaceWith: TBCRadioButton;
    RadioButtonWholeFile: TBCRadioButton;
    SliderCaseSensitive: TsSlider;
    StickyLabelCaseSensitive: TsStickyLabel;
    StickyLabelPromptOnReplace: TsStickyLabel;
    SliderPromptOnReplace: TsSlider;
    SliderRegularExpression: TsSlider;
    StickyLabelRegularExpression: TsStickyLabel;
    SliderSelectedOnly: TsSlider;
    StickyLabelSelectedOnly: TsStickyLabel;
    StickyLabelWholeWordsOnly: TsStickyLabel;
    SliderWholeWordsOnly: TsSlider;
    StickyLabelWildCard: TsStickyLabel;
    SliderWildCard: TsSlider;
    PanelDeleteLine: TBCPanel;
    RadioButtonDeleteLine: TBCRadioButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RadioButtonDeleteLineClick(Sender: TObject);
    procedure RadioButtonReplaceWithClick(Sender: TObject);
    procedure SliderRegularExpressionClick(Sender: TObject);
    procedure SliderWildCardClick(Sender: TObject);
    procedure ComboBoxSearchForChange(Sender: TObject);
  private
    function GetReplaceInWholeFile: Boolean;
    function GetReplaceWith: string;
    function GetSearchFor: string;
    procedure ReadIniFile;
    procedure SetButtons;
    procedure SetReplaceWith(AValue: string);
    procedure SetSearchFor(AValue: string);
    procedure WriteIniFile;
  public
    procedure GetOptions(Editor: TBCEditor);
    property SearchFor: string read GetSearchFor write SetSearchFor;
    property ReplaceWith: string read GetReplaceWith write SetReplaceWith;
    property ReplaceInWholeFile: Boolean read GetReplaceInWholeFile;
  end;

function ReplaceDialog: TReplaceDialog;

implementation

{$R *.DFM}

uses
  System.IniFiles, BCCommon.FileUtils, BCControl.Utils, BCCommon.Utils;

var
  FReplaceDialog: TReplaceDialog;

function ReplaceDialog: TReplaceDialog;
begin
  if not Assigned(FReplaceDialog) then
    Application.CreateForm(TReplaceDialog, FReplaceDialog);
  Result := FReplaceDialog;
end;

procedure TReplaceDialog.GetOptions(Editor: TBCEditor);

  procedure SetOption(Enabled: Boolean; Option: TBCEditorReplaceOption);
  begin
    if Enabled then
      Editor.Replace.Options := Editor.Replace.Options + [Option]
    else
      Editor.Replace.Options := Editor.Replace.Options - [Option];
  end;

begin
  SetOption(SliderCaseSensitive.SliderOn, roCaseSensitive);
  SetOption(SliderPromptOnReplace.SliderOn, roPrompt);
  SetOption(SliderSelectedOnly.SliderOn, roSelectedOnly);
  SetOption(SliderWholeWordsOnly.SliderOn, roWholeWordsOnly);
  SetOption(ModalResult = mrYes, roReplaceAll);
  if RadioButtonReplaceWith.Checked then
    Editor.Replace.Action := eraReplace
  else
    Editor.Replace.Action := eraDeleteLine;
  if SliderRegularExpression.SliderOn then
    Editor.Replace.Engine := seRegularExpression
  else
  if SliderWildCard.SliderOn then
    Editor.Replace.Engine := seWildCard
  else
    Editor.Replace.Engine := seNormal;
end;

procedure TReplaceDialog.FormShow(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  if ComboBoxSearchFor.CanFocus then
    ComboBoxSearchFor.SetFocus;
  AlignSliders(GroupBoxOptions);
end;

procedure TReplaceDialog.SetButtons;
begin
  ButtonOK.Enabled := ComboBoxSearchFor.Text <> '';
  ButtonReplaceAll.Enabled := ButtonOK.Enabled;
end;

procedure TReplaceDialog.ComboBoxSearchForChange(Sender: TObject);
begin
  inherited;
  SetButtons;
end;

procedure TReplaceDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  WriteIniFile;
  if ModalResult in [mrOK, mrYes] then
  begin
    InsertTextToCombo(ComboBoxSearchFor);
    InsertTextToCombo(ComboBoxReplaceWith);
  end;
end;

procedure TReplaceDialog.FormDestroy(Sender: TObject);
begin
  FReplaceDialog := nil;
end;

procedure TReplaceDialog.RadioButtonDeleteLineClick(Sender: TObject);
begin
  RadioButtonReplaceWith.Checked := False;
  ComboBoxReplaceWith.Text := '';
  ComboBoxReplaceWith.Enabled := False;
end;

procedure TReplaceDialog.RadioButtonReplaceWithClick(Sender: TObject);
begin
  RadioButtonDeleteLine.Checked := False;
  ComboBoxReplaceWith.Enabled := True;
end;

procedure TReplaceDialog.ReadIniFile;
begin
  with TIniFile.Create(GetIniFilename) do
  try
    RadioButtonReplaceWith.Checked := ReadBool('ReplaceOptions', 'ReplaceWith', True);
    RadioButtonDeleteLine.Checked := ReadBool('ReplaceOptions', 'DeleteLine', False);
    SliderCaseSensitive.SliderOn := ReadBool('ReplaceOptions', 'CaseSensitive', False);
    SliderPromptOnReplace.SliderOn := ReadBool('ReplaceOptions', 'PromptOnReplace', True);
    SliderRegularExpression.SliderOn := ReadBool('ReplaceOptions', 'RegularExpressions', False);
    SliderSelectedOnly.SliderOn := ReadBool('ReplaceOptions', 'SelectedOnly', False);
    SliderWholeWordsOnly.SliderOn := ReadBool('ReplaceOptions', 'WholeWordsOnly', False);
    SliderWildCard.SliderOn := ReadBool('ReplaceOptions', 'WildCard', False);
    RadioButtonWholeFile.Checked := ReadBool('ReplaceOptions', 'WholeFile', True);
    RadioButtonAllOpenFiles.Checked := ReadBool('ReplaceOptions', 'AllOpenFiles', False);
  finally
    Free;
  end;
end;

procedure TReplaceDialog.SliderRegularExpressionClick(Sender: TObject);
begin
  SliderWildCard.SliderOn := False
end;

procedure TReplaceDialog.SliderWildCardClick(Sender: TObject);
begin
  SliderRegularExpression.SliderOn := False;
end;

procedure TReplaceDialog.WriteIniFile;
begin
  with TIniFile.Create(GetIniFilename) do
  try
    WriteBool('ReplaceOptions', 'ReplaceWith', RadioButtonReplaceWith.Checked);
    WriteBool('ReplaceOptions', 'DeleteLine', RadioButtonDeleteLine.Checked);
    WriteBool('ReplaceOptions', 'CaseSensitive', SliderCaseSensitive.SliderOn);
    WriteBool('ReplaceOptions', 'PromptOnReplace', SliderPromptOnReplace.SliderOn);
    WriteBool('ReplaceOptions', 'RegularExpressions', SliderRegularExpression.SliderOn);
    WriteBool('ReplaceOptions', 'SelectedOnly', SliderSelectedOnly.SliderOn);
    WriteBool('ReplaceOptions', 'WholeWordsOnly', SliderWholeWordsOnly.SliderOn);
    WriteBool('ReplaceOptions', 'WildCard', SliderWildCard.SliderOn);
    WriteBool('ReplaceOptions', 'WholeFile', RadioButtonWholeFile.Checked);
    WriteBool('ReplaceOptions', 'AllOpenFiles', RadioButtonAllOpenFiles.Checked);
  finally
    Free;
  end;
end;

function TReplaceDialog.GetReplaceInWholeFile: Boolean;
begin
  Result := RadioButtonWholeFile.Checked;
end;

function TReplaceDialog.GetReplaceWith: string;
begin
  Result := ComboBoxReplaceWith.Text;
end;

function TReplaceDialog.GetSearchFor: string;
begin
  Result := ComboBoxSearchFor.Text;
end;

procedure TReplaceDialog.SetReplaceWith(AValue: string);
begin
  ComboBoxReplaceWith.Text := AValue;
end;

procedure TReplaceDialog.SetSearchFor(AValue: string);
begin
  ComboBoxSearchFor.Text := AValue;
  SetButtons;
end;

end.

