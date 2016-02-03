unit BCCommon.Frame.Options.SQL.Whitespace;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, BCCommon.Options.Container.SQL.Formatter,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  BCCommon.Frame.Options.Base, BCControl.Panel, acSlider, sLabel, Vcl.StdCtrls, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsSQLWhitespaceFrame = class(TBCOptionsBaseFrame)
    Panel: TBCPanel;
    StickyLabelSpaceAroundOperator: TsStickyLabel;
    SliderSpaceAroundOperator: TsSlider;
    StickyLabelSpaceInsideCreate: TsStickyLabel;
    SliderSpaceInsideCreate: TsSlider;
    StickyLabelSpaceInsideExpression: TsStickyLabel;
    SliderSpaceInsideExpression: TsSlider;
    StickyLabelSpaceInsideSubquery: TsStickyLabel;
    SliderSpaceInsideSubquery: TsSlider;
    StickyLabelSpaceInsideFunction: TsStickyLabel;
    SliderSpaceInsideFunction: TsSlider;
    StickyLabelSpaceInsideTypename: TsStickyLabel;
    SliderSpaceInsideTypename: TsSlider;
  protected
    procedure GetData; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsSQLWhitespaceFrame(AOwner: TComponent): TOptionsSQLWhitespaceFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Utils;

var
  FOptionsSQLWhitespaceFrame: TOptionsSQLWhitespaceFrame;

function OptionsSQLWhitespaceFrame(AOwner: TComponent): TOptionsSQLWhitespaceFrame;
begin
  if not Assigned(FOptionsSQLWhitespaceFrame) then
    FOptionsSQLWhitespaceFrame := TOptionsSQLWhitespaceFrame.Create(AOwner);
  Result := FOptionsSQLWhitespaceFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsSQLWhitespaceFrame.Destroy;
begin
  inherited;
  FOptionsSQLWhitespaceFrame := nil;
end;

procedure TOptionsSQLWhitespaceFrame.GetData;
begin
  SliderSpaceAroundOperator.SliderOn := SQLFormatterOptionsContainer.WhitespaceSpaceAroundOperator;
  SliderSpaceInsideCreate.SliderOn := SQLFormatterOptionsContainer.WhitespaceSpaceInsideCreate;
  SliderSpaceInsideExpression.SliderOn := SQLFormatterOptionsContainer.WhitespaceSpaceInsideExpression;
  SliderSpaceInsideSubquery.SliderOn := SQLFormatterOptionsContainer.WhitespaceSpaceInsideSubquery;
  SliderSpaceInsideFunction.SliderOn := SQLFormatterOptionsContainer.WhitespaceSpaceInsideFunction;
  SliderSpaceInsideTypename.SliderOn := SQLFormatterOptionsContainer.WhitespaceSpaceInsideTypename;
end;

procedure TOptionsSQLWhitespaceFrame.PutData;
begin
  SQLFormatterOptionsContainer.WhitespaceSpaceAroundOperator := SliderSpaceAroundOperator.SliderOn;
  SQLFormatterOptionsContainer.WhitespaceSpaceInsideCreate := SliderSpaceInsideCreate.SliderOn;
  SQLFormatterOptionsContainer.WhitespaceSpaceInsideExpression := SliderSpaceInsideExpression.SliderOn;
  SQLFormatterOptionsContainer.WhitespaceSpaceInsideSubquery := SliderSpaceInsideSubquery.SliderOn;
  SQLFormatterOptionsContainer.WhitespaceSpaceInsideFunction := SliderSpaceInsideFunction.SliderOn;
  SQLFormatterOptionsContainer.WhitespaceSpaceInsideTypename := SliderSpaceInsideTypename.SliderOn;
end;

end.
