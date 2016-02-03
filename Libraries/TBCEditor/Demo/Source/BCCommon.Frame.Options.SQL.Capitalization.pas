unit BCCommon.Frame.Options.SQL.Capitalization;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, BCCommon.Options.Container.SQL.Formatter,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BCCommon.Frame.Options.Base, Vcl.StdCtrls, BCControl.ComboBox,
  sComboBox, BCControl.Panel, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsSQLCapitalizationFrame = class(TBCOptionsBaseFrame)
    ComboBoxAliasName: TBCComboBox;
    ComboBoxColumnName: TBCComboBox;
    ComboBoxDataType: TBCComboBox;
    ComboBoxFunctionName: TBCComboBox;
    ComboBoxIdentifier: TBCComboBox;
    ComboBoxKeywords: TBCComboBox;
    ComboBoxQuotedIdentifier: TBCComboBox;
    ComboBoxTableName: TBCComboBox;
    ComboBoxVariableName: TBCComboBox;
    Panel: TBCPanel;
  protected
    procedure GetData; override;
    procedure Init; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsSQLCapitalizationFrame(AOwner: TComponent): TOptionsSQLCapitalizationFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Language.Strings;

var
  FOptionsSQLCapitalizationFrame: TOptionsSQLCapitalizationFrame;

function OptionsSQLCapitalizationFrame(AOwner: TComponent): TOptionsSQLCapitalizationFrame;
begin
  if not Assigned(FOptionsSQLCapitalizationFrame) then
    FOptionsSQLCapitalizationFrame := TOptionsSQLCapitalizationFrame.Create(AOwner);
  Result := FOptionsSQLCapitalizationFrame;
end;

destructor TOptionsSQLCapitalizationFrame.Destroy;
begin
  inherited;
  FOptionsSQLCapitalizationFrame := nil;
end;

procedure TOptionsSQLCapitalizationFrame.Init;

  procedure AddCaseItems(ComboBox: TBCComboBox);
  begin
    with ComboBox.Items do
    begin
      Add(LanguageDatamodule.GetSQLFormatter('Uppercase'));
      Add(LanguageDatamodule.GetSQLFormatter('Lowercase'));
      Add(LanguageDatamodule.GetSQLFormatter('SentenceCase'));
      Add(LanguageDatamodule.GetSQLFormatter('NoChange'));
    end;
  end;

begin
  AddCaseItems(ComboBoxKeywords);
  AddCaseItems(ComboBoxIdentifier);
  AddCaseItems(ComboBoxQuotedIdentifier);
  AddCaseItems(ComboBoxTableName);
  AddCaseItems(ComboBoxColumnName);
  AddCaseItems(ComboBoxAliasName);
  AddCaseItems(ComboBoxVariableName);
  AddCaseItems(ComboBoxFunctionName);
  AddCaseItems(ComboBoxDataType);
end;

procedure TOptionsSQLCapitalizationFrame.GetData;
begin
  ComboBoxKeywords.ItemIndex := SQLFormatterOptionsContainer.CapitalizationKeywords;
  ComboBoxIdentifier.ItemIndex := SQLFormatterOptionsContainer.CapitalizationIdentifier;
  ComboBoxQuotedIdentifier.ItemIndex := SQLFormatterOptionsContainer.CapitalizationQuotedIdentifier;
  ComboBoxTableName.ItemIndex := SQLFormatterOptionsContainer.CapitalizationTableName;
  ComboBoxColumnName.ItemIndex := SQLFormatterOptionsContainer.CapitalizationColumnName;
  ComboBoxAliasName.ItemIndex := SQLFormatterOptionsContainer.CapitalizationAliasName;
  ComboBoxVariableName.ItemIndex := SQLFormatterOptionsContainer.CapitalizationVariableName;
  ComboBoxFunctionName.ItemIndex := SQLFormatterOptionsContainer.CapitalizationFuncname;
  ComboBoxDataType.ItemIndex := SQLFormatterOptionsContainer.CapitalizationDatatype;
end;

procedure TOptionsSQLCapitalizationFrame.PutData;
begin
  SQLFormatterOptionsContainer.CapitalizationKeywords := ComboBoxKeywords.ItemIndex;
  SQLFormatterOptionsContainer.CapitalizationIdentifier := ComboBoxIdentifier.ItemIndex;
  SQLFormatterOptionsContainer.CapitalizationQuotedIdentifier := ComboBoxQuotedIdentifier.ItemIndex;
  SQLFormatterOptionsContainer.CapitalizationTableName := ComboBoxTableName.ItemIndex;
  SQLFormatterOptionsContainer.CapitalizationColumnName := ComboBoxColumnName.ItemIndex;
  SQLFormatterOptionsContainer.CapitalizationAliasName := ComboBoxAliasName.ItemIndex;
  SQLFormatterOptionsContainer.CapitalizationVariableName := ComboBoxVariableName.ItemIndex;
  SQLFormatterOptionsContainer.CapitalizationFuncname := ComboBoxFunctionName.ItemIndex;
  SQLFormatterOptionsContainer.CapitalizationDatatype := ComboBoxDataType.ItemIndex;
end;

end.
