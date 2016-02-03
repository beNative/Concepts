unit BCCommon.Frame.Options.Editor.RightMargin;

interface

uses
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  BCControl.Edit, BCCommon.Frame.Options.Base,
  BCControl.Panel, acSlider, sLabel, sEdit, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsEditorRightMarginFrame = class(TBCOptionsBaseFrame)
    EditPosition: TBCEdit;
    Panel: TBCPanel;
    StickyLabelVisible: TsStickyLabel;
    SliderVisible: TsSlider;
    StickyLabelMouseMove: TsStickyLabel;
    SliderMouseMove: TsSlider;
    SliderShowMovingHint: TsSlider;
    StickyLabelShowMovingHint: TsStickyLabel;
  protected
    procedure GetData; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsEditorRightMarginFrame(AOwner: TComponent): TOptionsEditorRightMarginFrame;

implementation

{$R *.dfm}

uses
  System.SysUtils, BCCommon.Utils, BCCommon.Options.Container;

var
  FOptionsEditorRightMarginFrame: TOptionsEditorRightMarginFrame;

function OptionsEditorRightMarginFrame(AOwner: TComponent): TOptionsEditorRightMarginFrame;
begin
  if not Assigned(FOptionsEditorRightMarginFrame) then
    FOptionsEditorRightMarginFrame := TOptionsEditorRightMarginFrame.Create(AOwner);
  Result := FOptionsEditorRightMarginFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsEditorRightMarginFrame.Destroy;
begin
  inherited;
  FOptionsEditorRightMarginFrame := nil;
end;

procedure TOptionsEditorRightMarginFrame.PutData;
begin
  OptionsContainer.RightMarginVisible := SliderVisible.SliderOn;
  OptionsContainer.RightMarginMouseMove := SliderMouseMove.SliderOn;
  OptionsContainer.RightMarginShowMovingHint := SliderShowMovingHint.SliderOn;
  OptionsContainer.RightMarginPosition := StrToIntDef(EditPosition.Text, 80);
end;

procedure TOptionsEditorRightMarginFrame.GetData;
begin
  SliderVisible.SliderOn := OptionsContainer.RightMarginVisible;
  SliderMouseMove.SliderOn := OptionsContainer.RightMarginMouseMove;
  SliderShowMovingHint.SliderOn := OptionsContainer.RightMarginShowMovingHint;
  EditPosition.Text := IntToStr(OptionsContainer.RightMarginPosition);
end;

end.
