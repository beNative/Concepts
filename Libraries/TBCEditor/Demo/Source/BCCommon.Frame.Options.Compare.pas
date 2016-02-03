unit BCCommon.Frame.Options.Compare;

interface

uses
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  BCCommon.Options.Container, BCCommon.Frame.Options.Base, BCControl.Panel,
  acSlider, sLabel, Vcl.StdCtrls, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsCompareFrame = class(TBCOptionsBaseFrame)
    Panel: TBCPanel;
    StickyLabelIgnoreCase: TsStickyLabel;
    SliderIgnoreCase: TsSlider;
    SliderIgnoreBlanks: TsSlider;
    StickyLabelIgnoreBlanks: TsStickyLabel;
  protected
    procedure GetData; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsCompareFrame(AOwner: TComponent): TOptionsCompareFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Utils;

var
  FOptionsCompareFrame: TOptionsCompareFrame;

function OptionsCompareFrame(AOwner: TComponent): TOptionsCompareFrame;
begin
  if not Assigned(FOptionsCompareFrame) then
    FOptionsCompareFrame := TOptionsCompareFrame.Create(AOwner);
  Result := FOptionsCompareFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsCompareFrame.Destroy;
begin
  inherited;
  FOptionsCompareFrame := nil;
end;

procedure TOptionsCompareFrame.PutData;
begin
  OptionsContainer.CompareIgnoreCase := SliderIgnoreCase.SliderOn;
  OptionsContainer.CompareIgnoreBlanks := SliderIgnoreBlanks.SliderOn;
end;

procedure TOptionsCompareFrame.GetData;
begin
  SliderIgnoreCase.SliderOn := OptionsContainer.CompareIgnoreCase;
  SliderIgnoreBlanks.SliderOn := OptionsContainer.CompareIgnoreBlanks;
end;

end.
