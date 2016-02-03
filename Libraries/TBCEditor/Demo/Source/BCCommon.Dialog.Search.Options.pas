unit BCCommon.Dialog.Search.Options;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, BCEditor.Types, BCEditor.Editor,
  BCCommon.Dialog.Base, BCControl.Panel, BCControl.CheckBox, BCControl.Button, sButton, sPanel, sCheckBox;

type
  TSearchOptionsDialog = class(TBCBaseDialog)
    ButtonCancel: TBCButton;
    ButtonOK: TBCButton;
    CheckBoxBeepIfSearchStringNotFound: TBCCheckBox;
    CheckBoxCaseSensitive: TBCCheckBox;
    CheckBoxEntireScope: TBCCheckBox;
    CheckBoxHighlightResult: TBCCheckBox;
    CheckBoxRegularExpression: TBCCheckBox;
    CheckBoxSearchOnTyping: TBCCheckBox;
    CheckBoxSelectedOnly: TBCCheckBox;
    CheckBoxShowSearchStringNotFound: TBCCheckBox;
    CheckBoxWholeWordsOnly: TBCCheckBox;
    CheckBoxWildCard: TBCCheckBox;
    PanelButton: TBCPanel;
  private
    { Private declarations }
    procedure SetOptions(Editor: TBCEditor);
    procedure GetOptions(Editor: TBCEditor);
  public
    { Public declarations }
    class procedure ClassShowModal(Editor: TBCEditor);
  end;

implementation

{$R *.dfm}

class procedure TSearchOptionsDialog.ClassShowModal(Editor: TBCEditor);
var
  FSearchOptionsDialog: TSearchOptionsDialog;
begin
  Application.CreateForm(TSearchOptionsDialog, FSearchOptionsDialog);

  FSearchOptionsDialog.SetOptions(Editor);
  if FSearchOptionsDialog.ShowModal = mrOk then
    FSearchOptionsDialog.GetOptions(Editor);

  FSearchOptionsDialog.Free;
  FSearchOptionsDialog := nil;
end;

procedure TSearchOptionsDialog.SetOptions(Editor: TBCEditor);
begin
  CheckBoxCaseSensitive.Checked := soCaseSensitive in Editor.Search.Options;
  CheckBoxWholeWordsOnly.Checked := soWholeWordsOnly in Editor.Search.Options;
  CheckBoxRegularExpression.Checked := Editor.Search.Engine = seRegularExpression;
  CheckBoxWildCard.Checked := Editor.Search.Engine = seWildCard;
  CheckBoxBeepIfSearchStringNotFound.Checked := soBeepIfStringNotFound in Editor.Search.Options;
  CheckBoxSearchOnTyping.Checked := soSearchOnTyping in Editor.Search.Options;
  CheckBoxEntireScope.Checked := soEntireScope in Editor.Search.Options;
  CheckBoxHighlightResult.Checked := soHighlightResults in Editor.Search.Options;
  CheckBoxSelectedOnly.Checked := soSelectedOnly in Editor.Search.Options;
  CheckBoxShowSearchStringNotFound.Checked := soShowStringNotFound in Editor.Search.Options;
end;

procedure TSearchOptionsDialog.GetOptions(Editor: TBCEditor);

  procedure SetOption(Enabled: Boolean; Option: TBCEditorSearchOption);
  begin
    if Enabled then
      Editor.Search.Options := Editor.Search.Options + [Option]
    else
      Editor.Search.Options := Editor.Search.Options - [Option];
  end;

begin
  SetOption(CheckBoxCaseSensitive.Checked, soCaseSensitive);
  SetOption(CheckBoxWholeWordsOnly.Checked, soWholeWordsOnly);
  if CheckBoxRegularExpression.Checked then
    Editor.Search.Engine := seRegularExpression
  else
  if CheckBoxWildCard.Checked then
    Editor.Search.Engine := seWildCard
  else
    Editor.Search.Engine := seNormal;
  SetOption(CheckBoxBeepIfSearchStringNotFound.Checked, soBeepIfStringNotFound);
  SetOption(CheckBoxSearchOnTyping.Checked, soSearchOnTyping);
  SetOption(CheckBoxEntireScope.Checked, soEntireScope);
  SetOption(CheckBoxHighlightResult.Checked, soHighlightResults);
  SetOption(CheckBoxSelectedOnly.Checked, soSelectedOnly);
  SetOption(CheckBoxShowSearchStringNotFound.Checked, soShowStringNotFound);
end;

end.
