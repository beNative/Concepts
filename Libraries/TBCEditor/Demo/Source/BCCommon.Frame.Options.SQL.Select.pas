unit BCCommon.Frame.Options.SQL.Select;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, BCCommon.Options.Container.SQL.Formatter,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  BCControl.ComboBox, Vcl.ComCtrls, BCControl.PageControl, BCControl.Panel,
  BCCommon.Frame.Options.Base, sComboBox, sPageControl, acSlider, sLabel, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsSQLSelectFrame = class(TBCOptionsBaseFrame)
    ComboBoxAndOrLineBreak: TBCComboBox;
    ComboBoxColumnListLineBreak: TBCComboBox;
    ComboBoxColumnListStyle: TBCComboBox;
    ComboBoxFromClauseStyle: TBCComboBox;
    ComboBoxGroupByClauseStyle: TBCComboBox;
    ComboBoxOrderByClauseStyle: TBCComboBox;
    PageControl: TBCPageControl;
    Panel: TBCPanel;
    TabSheetAndOrKeyword: TsTabSheet;
    TabSheetColumnList: TsTabSheet;
    TabSheetFromJoinClause: TsTabSheet;
    TabSheetGroupByClause: TsTabSheet;
    TabSheetHavingClause: TsTabSheet;
    TabSheetIntoClause: TsTabSheet;
    TabSheetOrderByClause: TsTabSheet;
    TabSheetSubquery: TsTabSheet;
    StickyLabelAlignAlias: TsStickyLabel;
    SliderAlignAlias: TsSlider;
    StickyLabelColumnInNewLine: TsStickyLabel;
    SliderColumnInNewLine: TsSlider;
    StickyLabelTreatDistinctAsVirtualColumn: TsStickyLabel;
    SliderTreatDistinctAsVirtualColumn: TsSlider;
    StickyLabelNewLineAfterIn: TsStickyLabel;
    SliderNewLineAfterIn: TsSlider;
    StickyLabelNewLineAfterExists: TsStickyLabel;
    SliderNewLineAfterExists: TsSlider;
    StickyLabelNewlineAfterComparisonOperator: TsStickyLabel;
    SliderNewlineAfterComparisonOperator: TsSlider;
    StickyLabelNewlineBeforeComparisonOperator: TsStickyLabel;
    SliderNewlineBeforeComparisonOperator: TsSlider;
    StickyLabelIntoClauseInNewLine: TsStickyLabel;
    SliderIntoClauseInNewLine: TsSlider;
    StickyLabelFromClauseInNewLine: TsStickyLabel;
    SliderFromClauseInNewLine: TsSlider;
    SliderJoinClauseInNewLine: TsSlider;
    StickyLabelJoinClauseInNewLine: TsStickyLabel;
    SliderAlignJoinWithFromKeyword: TsSlider;
    StickyLabelAlignJoinWithFromKeyword: TsStickyLabel;
    SliderAlignAndOrWithOnInJoinClause: TsSlider;
    StickyLabelAlignAndOrWithOnInJoinClause: TsStickyLabel;
    SliderAlignAliasInFromClause: TsSlider;
    StickyLabelAlignAliasInFromClause: TsStickyLabel;
    StickyLabelAndOrUnderWhere: TsStickyLabel;
    SliderAndOrUnderWhere: TsSlider;
    StickyLabelWhereClauseInNewLine: TsStickyLabel;
    SliderWhereClauseInNewLine: TsSlider;
    StickyLabelWhereClauseAlignExpr: TsStickyLabel;
    SliderWhereClauseAlignExpr: TsSlider;
    StickyLabelGroupByClauseInNewLine: TsStickyLabel;
    SliderGroupByClauseInNewLine: TsSlider;
    StickyLabelHavingClauseInNewLine: TsStickyLabel;
    SliderHavingClauseInNewLine: TsSlider;
    StickyLabelOrderByClauseInNewLine: TsStickyLabel;
    SliderOrderByClauseInNewLine: TsSlider;
  protected
    procedure GetData; override;
    procedure Init; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsSQLSelectFrame(AOwner: TComponent): TOptionsSQLSelectFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Language.Strings, BCCommon.Utils;

var
  FOptionsSQLSelectFrame: TOptionsSQLSelectFrame;

function OptionsSQLSelectFrame(AOwner: TComponent): TOptionsSQLSelectFrame;
begin
  if not Assigned(FOptionsSQLSelectFrame) then
    FOptionsSQLSelectFrame := TOptionsSQLSelectFrame.Create(AOwner);
  Result := FOptionsSQLSelectFrame;
  AlignSliders(Result.TabSheetColumnList);
  AlignSliders(Result.TabSheetSubquery);
  AlignSliders(Result.TabSheetFromJoinClause);
  AlignSliders(Result.TabSheetAndOrKeyword);
end;

destructor TOptionsSQLSelectFrame.Destroy;
begin
  inherited;
  FOptionsSQLSelectFrame := nil;
end;

procedure TOptionsSQLSelectFrame.Init;
begin
  with ComboBoxColumnListStyle.Items do
  begin
    Add(LanguageDatamodule.GetSQLFormatter('Stacked'));
    Add(LanguageDatamodule.GetSQLFormatter('Wrapped'));
  end;
  with ComboBoxColumnListLineBreak.Items do
  begin
    Add(LanguageDatamodule.GetSQLFormatter('AfterComma'));
    Add(LanguageDatamodule.GetSQLFormatter('BeforeComma'));
    Add(LanguageDatamodule.GetSQLFormatter('BeforeCommaWithSpace'));
    Add(LanguageDatamodule.GetSQLFormatter('NoLineBreak'));
  end;
  with ComboBoxFromClauseStyle.Items do
  begin
    Add(LanguageDatamodule.GetSQLFormatter('Stacked'));
    Add(LanguageDatamodule.GetSQLFormatter('Wrapped'));
  end;
  with ComboBoxAndOrLineBreak.Items do
  begin
    Add(LanguageDatamodule.GetSQLFormatter('BeforeAndor'));
    Add(LanguageDatamodule.GetSQLFormatter('AfterAndOr'));
    Add(LanguageDatamodule.GetSQLFormatter('NoLineBreak'));
  end;
  with ComboBoxGroupByClauseStyle.Items do
  begin
    Add(LanguageDatamodule.GetSQLFormatter('Stacked'));
    Add(LanguageDatamodule.GetSQLFormatter('Wrapped'));
  end;
  with ComboBoxOrderByClauseStyle.Items do
  begin
    Add(LanguageDatamodule.GetSQLFormatter('Stacked'));
    Add(LanguageDatamodule.GetSQLFormatter('Wrapped'));
  end;
  { Set default page }
  PageControl.TabIndex := 0;
end;

procedure TOptionsSQLSelectFrame.GetData;
begin
  { Column List }
  ComboBoxColumnListStyle.ItemIndex := SQLFormatterOptionsContainer.SelectColumnListStyle;
  ComboBoxColumnListLineBreak.ItemIndex := SQLFormatterOptionsContainer.SelectColumnListLineBreak;
  SliderColumnInNewLine.SliderOn := SQLFormatterOptionsContainer.SelectColumnListColumnInNewLine;
  SliderAlignAlias.SliderOn := SQLFormatterOptionsContainer.SelectColumnListAlignAlias;
  SliderTreatDistinctAsVirtualColumn.SliderOn := SQLFormatterOptionsContainer.SelectColumnListTreatDistinctAsVirtualColumn;
  { Subquery }
  SliderNewLineAfterIn.SliderOn := SQLFormatterOptionsContainer.SelectSubqueryNewLineAfterIn;
  SliderNewLineAfterExists.SliderOn := SQLFormatterOptionsContainer.SelectSubqueryNewLineAfterExists;
  SliderNewlineAfterComparisonOperator.SliderOn := SQLFormatterOptionsContainer.SelectSubqueryNewLineAfterComparisonOperator;
  SliderNewlineBeforeComparisonOperator.SliderOn := SQLFormatterOptionsContainer.SelectSubqueryNewLineBeforeComparisonOperator;
  { Into Clause }
  SliderIntoClauseInNewLine.SliderOn := SQLFormatterOptionsContainer.SelectIntoClauseInNewLine;
  { Select From/Join Clause }
  ComboBoxFromClauseStyle.ItemIndex := SQLFormatterOptionsContainer.SelectFromClauseStyle;
  SliderFromClauseInNewLine.SliderOn := SQLFormatterOptionsContainer.SelectFromClauseInNewLine;
  SliderJoinClauseInNewLine.SliderOn := SQLFormatterOptionsContainer.SelectJoinClauseInNewLine;
  SliderAlignJoinWithFromKeyword.SliderOn := SQLFormatterOptionsContainer.SelectAlignJoinWithFromKeyword;
  SliderAlignAndOrWithOnInJoinClause.SliderOn := SQLFormatterOptionsContainer.SelectAlignAndOrWithOnInJoinClause;
  SliderAlignAliasInFromClause.SliderOn := SQLFormatterOptionsContainer.SelectAlignAliasInFromClause;
  { And/Or Keyword }
  ComboBoxAndOrLineBreak.ItemIndex := SQLFormatterOptionsContainer.SelectAndOrLineBreak;
  SliderAndOrUnderWhere.SliderOn := SQLFormatterOptionsContainer.SelectAndOrUnderWhere;
  SliderWhereClauseInNewline.SliderOn := SQLFormatterOptionsContainer.SelectWhereClauseInNewline;
  SliderWhereClauseAlignExpr.SliderOn := SQLFormatterOptionsContainer.SelectWhereClauseAlignExpr;
  { Group By Clause }
  ComboBoxGroupByClauseStyle.ItemIndex := SQLFormatterOptionsContainer.SelectGroupByClauseStyle;
  SliderGroupByClauseInNewLine.SliderOn := SQLFormatterOptionsContainer.SelectGroupByClauseInNewLine;
  { Having Clause }
  SliderHavingClauseInNewLine.SliderOn := SQLFormatterOptionsContainer.SelectHavingClauseInNewLine;
  { Order By Clause }
  ComboBoxOrderByClauseStyle.ItemIndex := SQLFormatterOptionsContainer.SelectOrderByClauseStyle;
  SliderOrderByClauseInNewLine.SliderOn := SQLFormatterOptionsContainer.SelectOrderByClauseInNewLine;
end;

procedure TOptionsSQLSelectFrame.PutData;
begin
  { Column List }
  SQLFormatterOptionsContainer.SelectColumnListStyle := ComboBoxColumnListStyle.ItemIndex;
  SQLFormatterOptionsContainer.SelectColumnListLineBreak := ComboBoxColumnListLineBreak.ItemIndex;
  SQLFormatterOptionsContainer.SelectColumnListColumnInNewLine := SliderColumnInNewLine.SliderOn;
  SQLFormatterOptionsContainer.SelectColumnListAlignAlias := SliderAlignAlias.SliderOn;
  SQLFormatterOptionsContainer.SelectColumnListTreatDistinctAsVirtualColumn := SliderTreatDistinctAsVirtualColumn.SliderOn;
  { Subquery }
  SQLFormatterOptionsContainer.SelectSubqueryNewLineAfterIn := SliderNewLineAfterIn.SliderOn;
  SQLFormatterOptionsContainer.SelectSubqueryNewLineAfterExists := SliderNewLineAfterExists.SliderOn;
  SQLFormatterOptionsContainer.SelectSubqueryNewLineAfterComparisonOperator := SliderNewlineAfterComparisonOperator.SliderOn;
  SQLFormatterOptionsContainer.SelectSubqueryNewLineBeforeComparisonOperator := SliderNewlineBeforeComparisonOperator.SliderOn;
  { Into Clause }
  SQLFormatterOptionsContainer.SelectIntoClauseInNewLine := SliderIntoClauseInNewLine.SliderOn;
  { Select From/Join Clause }
  SQLFormatterOptionsContainer.SelectFromClauseStyle := ComboBoxFromClauseStyle.ItemIndex;
  SQLFormatterOptionsContainer.SelectFromClauseInNewLine := SliderFromClauseInNewLine.SliderOn;
  SQLFormatterOptionsContainer.SelectJoinClauseInNewLine := SliderJoinClauseInNewLine.SliderOn;
  SQLFormatterOptionsContainer.SelectAlignJoinWithFromKeyword := SliderAlignJoinWithFromKeyword.SliderOn;
  SQLFormatterOptionsContainer.SelectAlignAndOrWithOnInJoinClause := SliderAlignAndOrWithOnInJoinClause.SliderOn;
  SQLFormatterOptionsContainer.SelectAlignAliasInFromClause := SliderAlignAliasInFromClause.SliderOn;
  { And/Or Keyword }
  SQLFormatterOptionsContainer.SelectAndOrLineBreak := ComboBoxAndOrLineBreak.ItemIndex;
  SQLFormatterOptionsContainer.SelectAndOrUnderWhere := SliderAndOrUnderWhere.SliderOn;
  SQLFormatterOptionsContainer.SelectWhereClauseInNewline := SliderWhereClauseInNewline.SliderOn;
  SQLFormatterOptionsContainer.SelectWhereClauseAlignExpr := SliderWhereClauseAlignExpr.SliderOn;
  { Group By Clause }
  SQLFormatterOptionsContainer.SelectGroupByClauseStyle := ComboBoxGroupByClauseStyle.ItemIndex;
  SQLFormatterOptionsContainer.SelectGroupByClauseInNewLine := SliderGroupByClauseInNewLine.SliderOn;
  { Having Clause }
  SQLFormatterOptionsContainer.SelectHavingClauseInNewLine := SliderHavingClauseInNewLine.SliderOn;
  { Order By Clause }
  SQLFormatterOptionsContainer.SelectOrderByClauseStyle := ComboBoxOrderByClauseStyle.ItemIndex;
  SQLFormatterOptionsContainer.SelectOrderByClauseInNewLine := SliderOrderByClauseInNewLine.SliderOn;
end;

end.
