unit BCCommon.Frame.Options.Editor.Scroll;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, BCControl.Panel,
  BCCommon.Frame.Options.Base, acSlider, sLabel, Vcl.StdCtrls, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsEditorScrollFrame = class(TBCOptionsBaseFrame)
    Panel: TBCPanel;
    StickyLabelAutosizeMaxWidth: TsStickyLabel;
    SliderAutosizeMaxWidth: TsSlider;
    StickyLabelHalfPage: TsStickyLabel;
    SliderHalfPage: TsSlider;
    StickyLabelHintFollows: TsStickyLabel;
    SliderHintFollows: TsSlider;
    StickyLabelPastEndOfFile: TsStickyLabel;
    SliderPastEndOfFile: TsSlider;
    StickyLabelPastEndOfLineMarker: TsStickyLabel;
    SliderPastEndOfLineMarker: TsSlider;
    StickyLabelShowHint: TsStickyLabel;
    SliderShowHint: TsSlider;
  protected
    procedure GetData; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsEditorScrollFrame(AOwner: TComponent): TOptionsEditorScrollFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Options.Container, BCCommon.Utils;

var
  FOptionsEditorScrollFrame: TOptionsEditorScrollFrame;

function OptionsEditorScrollFrame(AOwner: TComponent): TOptionsEditorScrollFrame;
begin
  if not Assigned(FOptionsEditorScrollFrame) then
    FOptionsEditorScrollFrame := TOptionsEditorScrollFrame.Create(AOwner);
  Result := FOptionsEditorScrollFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsEditorScrollFrame.Destroy;
begin
  inherited;
  FOptionsEditorScrollFrame := nil;
end;

procedure TOptionsEditorScrollFrame.PutData;
begin
  OptionsContainer.ScrollAutosizeMaxWidth := SliderAutosizeMaxWidth.SliderOn;
  OptionsContainer.ScrollHalfPage := SliderHalfPage.SliderOn;
  OptionsContainer.ScrollHintFollows := SliderHintFollows.SliderOn;
  OptionsContainer.ScrollPastEndOfFile := SliderPastEndOfFile.SliderOn;
  OptionsContainer.ScrollPastEndOfLineMarker := SliderPastEndOfLineMarker.SliderOn;
  OptionsContainer.ScrollShowHint := SliderShowHint.SliderOn;
end;

procedure TOptionsEditorScrollFrame.GetData;
begin
  SliderAutosizeMaxWidth.SliderOn := OptionsContainer.ScrollAutosizeMaxWidth;
  SliderHalfPage.SliderOn := OptionsContainer.ScrollHalfPage;
  SliderHintFollows.SliderOn := OptionsContainer.ScrollHintFollows;
  SliderPastEndOfFile.SliderOn := OptionsContainer.ScrollPastEndOfFile;
  SliderPastEndOfLineMarker.SliderOn := OptionsContainer.ScrollPastEndOfLineMarker;
  SliderShowHint.SliderOn := OptionsContainer.ScrollShowHint;
end;

end.
