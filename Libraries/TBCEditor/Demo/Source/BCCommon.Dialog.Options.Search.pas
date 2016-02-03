unit BCCommon.Dialog.Options.Search;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BCEditor.Types, BCEditor.Editor,
  BCCommon.Dialog.Base, BCControl.Panel, BCControl.Button, acSlider,
  sLabel, Vcl.StdCtrls, sButton, Vcl.ExtCtrls, sPanel;

type
  TSearchOptionsDialog = class(TBCBaseDialog)
    ButtonCancel: TBCButton;
    ButtonOK: TBCButton;
    PanelButton: TBCPanel;
    StickyLabelBeepIfSearchStringNotFound: TsStickyLabel;
    SliderBeepIfSearchStringNotFound: TsSlider;
    SliderCaseSensitive: TsSlider;
    StickyLabelEntireScope: TsStickyLabel;
    StickyLabelHighlightResult: TsStickyLabel;
    StickyLabelRegularExpression: TsStickyLabel;
    StickyLabelSearchOnTyping: TsStickyLabel;
    StickyLabelSelectedOnly: TsStickyLabel;
    StickyLabelShowSearchStringNotFound: TsStickyLabel;
    StickyLabelWildCard: TsStickyLabel;
    StickyLabelWholeWordsOnly: TsStickyLabel;
    StickyLabelCaseSensitive: TsStickyLabel;
    SliderEntireScope: TsSlider;
    SliderHighlightResult: TsSlider;
    SliderRegularExpression: TsSlider;
    SliderSearchOnTyping: TsSlider;
    SliderSelectedOnly: TsSlider;
    SliderShowSearchStringNotFound: TsSlider;
    SliderWildCard: TsSlider;
    SliderWholeWordsOnly: TsSlider;
    Panel: TBCPanel;
    SliderShowSearchMatchNotFound: TsSlider;
    StickyLabelShowSearchMatchNotFound: TsStickyLabel;
    SliderWrapAround: TsSlider;
    StickyLabelWrapAround: TsStickyLabel;
    procedure FormShow(Sender: TObject);
  private
    procedure SetOptions(Editor: TBCEditor);
    procedure GetOptions(Editor: TBCEditor);
    procedure WriteIniFile;
  public
    class procedure ClassShowModal(AEditor: TBCEditor);
  end;

implementation

{$R *.dfm}

uses
  BigIni, BCCommon.Utils, BCCommon.FileUtils;

class procedure TSearchOptionsDialog.ClassShowModal(AEditor: TBCEditor);
var
  FSearchOptionsDialog: TSearchOptionsDialog;
begin
  Application.CreateForm(TSearchOptionsDialog, FSearchOptionsDialog);

  with FSearchOptionsDialog do
  begin
    SetOptions(AEditor);
    if ShowModal = mrOk then
    begin
      GetOptions(AEditor);
      WriteIniFile;
    end;
    Free;
  end;
  FSearchOptionsDialog := nil;
end;

procedure TSearchOptionsDialog.WriteIniFile;
begin
  with TBigIniFile.Create(GetIniFilename) do
  try
    WriteBool('Options', 'SearchBeepIfSearchStringNotFound', SliderBeepIfSearchStringNotFound.SliderOn);
    WriteBool('Options', 'SearchCaseSensitive', SliderCaseSensitive.SliderOn);
    WriteBool('Options', 'SearchEntireScope', SliderEntireScope.SliderOn);
    WriteBool('Options', 'SearchHighlightResult', SliderHighlightResult.SliderOn);
    WriteBool('Options', 'SearchRegularExpression', SliderRegularExpression.SliderOn);
    WriteBool('Options', 'SearchOnTyping', SliderSearchOnTyping.SliderOn);
    WriteBool('Options', 'SearchSelectedOnly', SliderSelectedOnly.SliderOn);
    WriteBool('Options', 'SearchShowSearchMatchNotFound', SliderShowSearchMatchNotFound.SliderOn);
    WriteBool('Options', 'SearchShowSearchStringNotFound', SliderShowSearchStringNotFound.SliderOn);
    WriteBool('Options', 'SearchWildCard', SliderWildCard.SliderOn);
    WriteBool('Options', 'SearchWholeWordsOnly', SliderWholeWordsOnly.SliderOn);
    WriteBool('Options', 'SearchWrapAround', SliderWrapAround.SliderOn);
  finally
    Free;
  end;
end;

procedure TSearchOptionsDialog.SetOptions(Editor: TBCEditor);
begin
  SliderBeepIfSearchStringNotFound.SliderOn := soBeepIfStringNotFound in Editor.Search.Options;
  SliderCaseSensitive.SliderOn := soCaseSensitive in Editor.Search.Options;
  SliderEntireScope.SliderOn := soEntireScope in Editor.Search.Options;
  SliderHighlightResult.SliderOn := soHighlightResults in Editor.Search.Options;
  SliderRegularExpression.SliderOn := Editor.Search.Engine = seRegularExpression;
  SliderSearchOnTyping.SliderOn := soSearchOnTyping in Editor.Search.Options;
  SliderSelectedOnly.SliderOn := soSelectedOnly in Editor.Search.Options;
  SliderShowSearchMatchNotFound.SliderOn := soShowSearchMatchNotFound in Editor.Search.Options;
  SliderShowSearchStringNotFound.SliderOn := soShowStringNotFound in Editor.Search.Options;
  SliderWildCard.SliderOn := Editor.Search.Engine = seWildCard;
  SliderWholeWordsOnly.SliderOn := soWholeWordsOnly in Editor.Search.Options;
  SliderWrapAround.SliderOn := soWrapAround in Editor.Search.Options;
end;

procedure TSearchOptionsDialog.FormShow(Sender: TObject);
begin
  inherited;
  AlignSliders(Panel);
  Width := SliderBeepIfSearchStringNotFound.Left + SliderBeepIfSearchStringNotFound.Width + 12;
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
  SetOption(SliderBeepIfSearchStringNotFound.SliderOn, soBeepIfStringNotFound);
  SetOption(SliderCaseSensitive.SliderOn, soCaseSensitive);
  SetOption(SliderEntireScope.SliderOn, soEntireScope);
  SetOption(SliderHighlightResult.SliderOn, soHighlightResults);
  if SliderRegularExpression.SliderOn then
    Editor.Search.Engine := seRegularExpression
  else
  if SliderWildCard.SliderOn then
    Editor.Search.Engine := seWildCard
  else
    Editor.Search.Engine := seNormal;
  SetOption(SliderSearchOnTyping.SliderOn, soSearchOnTyping);
  SetOption(SliderSelectedOnly.SliderOn, soSelectedOnly);
  SetOption(SliderShowSearchMatchNotFound.SliderOn, soShowSearchMatchNotFound);
  SetOption(SliderShowSearchStringNotFound.SliderOn, soShowStringNotFound);
  SetOption(SliderWholeWordsOnly.SliderOn, soWholeWordsOnly);
  SetOption(SliderWrapAround.SliderOn, soWrapAround);
end;

end.
