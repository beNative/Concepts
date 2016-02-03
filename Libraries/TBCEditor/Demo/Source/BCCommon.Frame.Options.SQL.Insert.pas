unit BCCommon.Frame.Options.SQL.Insert;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, BCCommon.Options.Container.SQL.Formatter,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BCCommon.Frame.Options.Base, Vcl.StdCtrls, BCControl.ComboBox,
  BCControl.Edit, sComboBox, BCControl.Panel, acSlider, sLabel, sEdit, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsSQLInsertFrame = class(TBCOptionsBaseFrame)
    ComboBoxColumnListStyle: TBCComboBox;
    ComboBoxValueListStyle: TBCComboBox;
    EditInsertColumnsPerLine: TBCEdit;
    Panel: TBCPanel;
    StickyLabelParenthesisInSeparateLines: TsStickyLabel;
    SliderParenthesisInSeparateLines: TsSlider;
  protected
    procedure GetData; override;
    procedure Init; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsSQLInsertFrame(AOwner: TComponent): TOptionsSQLInsertFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Language.Strings;

var
  FOptionsSQLInsertFrame: TOptionsSQLInsertFrame;

function OptionsSQLInsertFrame(AOwner: TComponent): TOptionsSQLInsertFrame;
begin
  if not Assigned(FOptionsSQLInsertFrame) then
    FOptionsSQLInsertFrame := TOptionsSQLInsertFrame.Create(AOwner);
  Result := FOptionsSQLInsertFrame;
end;

destructor TOptionsSQLInsertFrame.Destroy;
begin
  inherited;
  FOptionsSQLInsertFrame := nil;
end;

procedure TOptionsSQLInsertFrame.Init;
begin
  with ComboBoxColumnListStyle.Items do
  begin
    Add(LanguageDatamodule.GetSQLFormatter('Stacked'));
    Add(LanguageDatamodule.GetSQLFormatter('Wrapped'));
  end;
  with ComboBoxValueListStyle.Items do
  begin
    Add(LanguageDatamodule.GetSQLFormatter('Stacked'));
    Add(LanguageDatamodule.GetSQLFormatter('Wrapped'));
  end;
end;

procedure TOptionsSQLInsertFrame.GetData;
begin
  ComboBoxColumnListStyle.ItemIndex := SQLFormatterOptionsContainer.InsertColumnListStyle;
  ComboBoxValueListStyle.ItemIndex := SQLFormatterOptionsContainer.InsertValueListStyle;
  EditInsertColumnsPerLine.Text := IntToStr(SQLFormatterOptionsContainer.InsertColumnsPerLine);
  SliderParenthesisInSeparateLines.SliderOn := SQLFormatterOptionsContainer.InsertParenthesisInSeparateLine;
end;

procedure TOptionsSQLInsertFrame.PutData;
begin
  SQLFormatterOptionsContainer.InsertColumnListStyle := ComboBoxColumnListStyle.ItemIndex;
  SQLFormatterOptionsContainer.InsertValueListStyle := ComboBoxValueListStyle.ItemIndex;
  SQLFormatterOptionsContainer.InsertColumnsPerLine := StrToIntDef(EditInsertColumnsPerLine.Text, 0);
  SQLFormatterOptionsContainer.InsertParenthesisInSeparateLine := SliderParenthesisInSeparateLines.SliderOn;
end;


end.
