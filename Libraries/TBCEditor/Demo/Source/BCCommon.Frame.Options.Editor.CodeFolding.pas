unit BCCommon.Frame.Options.Editor.CodeFolding;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  BCControl.Panel, sComboBox, BCControl.ComboBox, BCCommon.Frame.Options.Base, acSlider,
  sLabel, BCControl.Edit, sEdit, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsEditorCodeFoldingFrame = class(TBCOptionsBaseFrame)
    ComboBoxMarkStyle: TBCComboBox;
    Panel: TBCPanel;
    StickyLabelVisible: TsStickyLabel;
    SliderVisible: TsSlider;
    StickyLabelFoldMultilineComments: TsStickyLabel;
    SliderFoldMultilineComments: TsSlider;
    StickyLabelHighlightIndentGuides: TsStickyLabel;
    SliderHighlightIndentGuides: TsSlider;
    StickyLabelHighlightMatchingPair: TsStickyLabel;
    SliderHighlightMatchingPair: TsSlider;
    StickyLabelShowCollapsedCodeHint: TsStickyLabel;
    SliderShowCollapsedCodeHint: TsSlider;
    StickyLabelShowCollapsedLine: TsStickyLabel;
    SliderShowCollapsedLine: TsSlider;
    StickyLabelShowIndentGuides: TsStickyLabel;
    SliderShowIndentGuides: TsSlider;
    StickyLabelUncollapseByHintClick: TsStickyLabel;
    SliderUncollapseByHintClick: TsSlider;
    EditHintRowCount: TBCEdit;
  protected
    procedure Init; override;
    procedure GetData; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsEditorCodeFoldingFrame(AOwner: TComponent): TOptionsEditorCodeFoldingFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Options.Container, BCCommon.Language.Strings, BCCommon.Utils;

var
  FOptionsEditorCodeFoldingFrame: TOptionsEditorCodeFoldingFrame;

function OptionsEditorCodeFoldingFrame(AOwner: TComponent): TOptionsEditorCodeFoldingFrame;
begin
  if not Assigned(FOptionsEditorCodeFoldingFrame) then
    FOptionsEditorCodeFoldingFrame := TOptionsEditorCodeFoldingFrame.Create(AOwner);
  Result := FOptionsEditorCodeFoldingFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsEditorCodeFoldingFrame.Destroy;
begin
  inherited;
  FOptionsEditorCodeFoldingFrame := nil;
end;

procedure TOptionsEditorCodeFoldingFrame.Init;
begin
  inherited;
  with ComboBoxMarkStyle.Items do
  begin
    Clear;
    Add(LanguageDatamodule.GetConstant('Square'));
    Add(LanguageDatamodule.GetConstant('Circle'));
  end;
end;

procedure TOptionsEditorCodeFoldingFrame.PutData;
begin
  OptionsContainer.ShowCodeFolding := SliderVisible.SliderOn;
  OptionsContainer.FoldMultilineComments := SliderFoldMultilineComments.SliderOn;
  OptionsContainer.HighlightIndentGuides := SliderHighlightIndentGuides.SliderOn;
  OptionsContainer.HighlightMatchingPair := SliderHighlightMatchingPair.SliderOn;
  OptionsContainer.ShowCollapsedCodeHint := SliderShowCollapsedCodeHint.SliderOn;
  OptionsContainer.ShowCollapsedLine := SliderShowCollapsedLine.SliderOn;
  OptionsContainer.ShowIndentGuides := SliderShowIndentGuides.SliderOn;
  OptionsContainer.UncollapseByHintClick := SliderUncollapseByHintClick.SliderOn;
  OptionsContainer.CodeFoldingMarkStyle := ComboBoxMarkStyle.ItemIndex;
  OptionsContainer.CodeFoldingHintRowCount := StrToIntDef(EditHintRowCount.Text, 40);
end;

procedure TOptionsEditorCodeFoldingFrame.GetData;
begin
  SliderVisible.SliderOn := OptionsContainer.ShowCodeFolding;
  SliderFoldMultilineComments.SliderOn := OptionsContainer.FoldMultilineComments;
  SliderHighlightIndentGuides.SliderOn := OptionsContainer.HighlightIndentGuides;
  SliderHighlightMatchingPair.SliderOn := OptionsContainer.HighlightMatchingPair;
  SliderShowCollapsedCodeHint.SliderOn := OptionsContainer.ShowCollapsedCodeHint;
  SliderShowCollapsedLine.SliderOn := OptionsContainer.ShowCollapsedLine;
  SliderShowIndentGuides.SliderOn := OptionsContainer.ShowIndentGuides;
  SliderUncollapseByHintClick.SliderOn := OptionsContainer.UncollapseByHintClick;
  ComboBoxMarkStyle.ItemIndex := OptionsContainer.CodeFoldingMarkStyle;
  EditHintRowCount.Text := IntToStr(OptionsContainer.CodeFoldingHintRowCount);
end;

end.
