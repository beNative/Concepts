unit BCCommon.Frame.Options.SQL.Alignments;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, BCCommon.Options.Container.SQL.Formatter,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  BCControl.ComboBox, BCCommon.Frame.Options.Base, sComboBox, BCControl.Panel,
  acSlider, sLabel, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsSQLAlignmentsFrame = class(TBCOptionsBaseFrame)
    ComboBoxKeywordAlign: TBCComboBox;
    Panel: TBCPanel;
    StickyLabelKeywordAlignmentLeftJustify: TsStickyLabel;
    SliderKeywordAlignmentLeftJustify: TsSlider;
  protected
    procedure GetData; override;
    procedure Init; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsSQLAlignmentsFrame(AOwner: TComponent): TOptionsSQLAlignmentsFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Language.Strings;

var
  FOptionsSQLAlignmentsFrame: TOptionsSQLAlignmentsFrame;

function OptionsSQLAlignmentsFrame(AOwner: TComponent): TOptionsSQLAlignmentsFrame;
begin
  if not Assigned(FOptionsSQLAlignmentsFrame) then
    FOptionsSQLAlignmentsFrame := TOptionsSQLAlignmentsFrame.Create(AOwner);
  Result := FOptionsSQLAlignmentsFrame;
end;

destructor TOptionsSQLAlignmentsFrame.Destroy;
begin
  inherited;
  FOptionsSQLAlignmentsFrame := nil;
end;

procedure TOptionsSQLAlignmentsFrame.Init;
begin
  with ComboBoxKeywordAlign.Items do
  begin
    Add(LanguageDatamodule.GetSQLFormatter('Left'));
    Add(LanguageDatamodule.GetSQLFormatter('Right'));
    Add(LanguageDatamodule.GetSQLFormatter('None'));
  end;
end;

procedure TOptionsSQLAlignmentsFrame.GetData;
begin
  ComboBoxKeywordAlign.ItemIndex := SQLFormatterOptionsContainer.KeywordAlign;
  SliderKeywordAlignmentLeftJustify.SliderOn := SQLFormatterOptionsContainer.KeywordAlignmentLeftJustify;
end;

procedure TOptionsSQLAlignmentsFrame.PutData;
begin
  SQLFormatterOptionsContainer.KeywordAlign := ComboBoxKeywordAlign.ItemIndex;
  SQLFormatterOptionsContainer.KeywordAlignmentLeftJustify := SliderKeywordAlignmentLeftJustify.SliderOn;
end;

end.
