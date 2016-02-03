unit BCCommon.Frame.Options.Print;

interface

uses
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, BCControl.ComboBox,
  BCCommon.Options.Container, BCCommon.Frame.Options.Base, sComboBox, BCControl.Panel,
  acSlider, sLabel, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsPrintFrame = class(TBCOptionsBaseFrame)
    ComboBoxDateTime: TBCComboBox;
    ComboBoxDocumentName: TBCComboBox;
    ComboBoxPageNumber: TBCComboBox;
    ComboBoxPrintedBy: TBCComboBox;
    Panel: TBCPanel;
    StickyLabelShowHeaderLine: TsStickyLabel;
    SliderShowHeaderLine: TsSlider;
    StickyLabelShowFooterLine: TsStickyLabel;
    SliderShowFooterLine: TsSlider;
    StickyLabelShowLineNumbers: TsStickyLabel;
    SliderShowLineNumbers: TsSlider;
    StickyLabelWordWrap: TsStickyLabel;
    SliderWordWrap: TsSlider;
  protected
    procedure Init; override;
    procedure GetData; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsPrintFrame(AOwner: TComponent): TOptionsPrintFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Language.Strings, BCCommon.Utils;

var
  FOptionsPrintFrame: TOptionsPrintFrame;

function OptionsPrintFrame(AOwner: TComponent): TOptionsPrintFrame;
begin
  if not Assigned(FOptionsPrintFrame) then
    FOptionsPrintFrame := TOptionsPrintFrame.Create(AOwner);
  Result := FOptionsPrintFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsPrintFrame.Destroy;
begin
  inherited;
  FOptionsPrintFrame := nil;
end;

procedure TOptionsPrintFrame.Init;
begin
  with ComboBoxDocumentName.Items do
  begin
    Add(LanguageDatamodule.GetConstant('FooterLeft'));
    Add(LanguageDatamodule.GetConstant('FooterRight'));
    Add(LanguageDatamodule.GetConstant('HeaderLeft'));
    Add(LanguageDatamodule.GetConstant('HeaderRight'));
    Add(LanguageDatamodule.GetConstant('Hide'));
  end;
  ComboBoxPageNumber.Items.Text := ComboBoxDocumentName.Items.Text;
  ComboBoxPrintedBy.Items.Text := ComboBoxDocumentName.Items.Text;
  ComboBoxDateTime.Items.Text := ComboBoxDocumentName.Items.Text;
end;

procedure TOptionsPrintFrame.PutData;
begin
  OptionsContainer.PrintDocumentName := ComboBoxDocumentName.ItemIndex;
  OptionsContainer.PrintPageNumber := ComboBoxPageNumber.ItemIndex;
  OptionsContainer.PrintPrintedBy := ComboBoxPrintedBy.ItemIndex;
  OptionsContainer.PrintDateTime := ComboBoxDateTime.ItemIndex;
  OptionsContainer.PrintShowHeaderLine := SliderShowHeaderLine.SliderOn;
  OptionsContainer.PrintShowFooterLine := SliderShowFooterLine.SliderOn;
  OptionsContainer.PrintShowLineNumbers := SliderShowLineNumbers.SliderOn;
  OptionsContainer.PrintWordWrapLine := SliderWordWrap.SliderOn;
end;

procedure TOptionsPrintFrame.GetData;
begin
  ComboBoxDocumentName.ItemIndex := OptionsContainer.PrintDocumentName;
  ComboBoxPageNumber.ItemIndex := OptionsContainer.PrintPageNumber;
  ComboBoxPrintedBy.ItemIndex := OptionsContainer.PrintPrintedBy;
  ComboBoxDateTime.ItemIndex := OptionsContainer.PrintDateTime;
  SliderShowHeaderLine.SliderOn := OptionsContainer.PrintShowHeaderLine;
  SliderShowFooterLine.SliderOn := OptionsContainer.PrintShowFooterLine;
  SliderShowLineNumbers.SliderOn := OptionsContainer.PrintShowLineNumbers;
  SliderWordWrap.SliderOn := OptionsContainer.PrintWordWrapLine;
end;

end.
