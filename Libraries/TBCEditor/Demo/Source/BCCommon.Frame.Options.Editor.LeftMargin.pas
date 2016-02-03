unit BCCommon.Frame.Options.Editor.LeftMargin;

interface

uses
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, BCControl.Edit,
  BCCommon.Frame.Options.Base, BCControl.Panel,
  BCControl.GroupBox, acSlider, sLabel, sGroupBox, sEdit, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsEditorLeftMarginFrame = class(TBCOptionsBaseFrame)
    EditBookmarkPanelWidth: TBCEdit;
    EditWidth: TBCEdit;
    GroupBoxLineNumbers: TBCGroupBox;
    Panel: TBCPanel;
    StickyLabelVisible: TsStickyLabel;
    SliderVisible: TsSlider;
    StickyLabelAutosize: TsStickyLabel;
    SliderAutosize: TsSlider;
    StickyLabelShowBookmarks: TsStickyLabel;
    SliderShowBookmarks: TsSlider;
    StickyLabelShowBookmarkPanel: TsStickyLabel;
    SliderShowBookmarkPanel: TsSlider;
    StickyLabelShowLineState: TsStickyLabel;
    SliderShowLineState: TsSlider;
    StickyLabelShowInTens: TsStickyLabel;
    SliderShowInTens: TsSlider;
    StickyLabelShowLeadingZeros: TsStickyLabel;
    SliderShowLeadingZeros: TsSlider;
    StickyLabelShowAfterLastLine: TsStickyLabel;
    SliderShowAfterLastLine: TsSlider;
    EditLineNumbersStartFrom: TBCEdit;
  protected
    procedure GetData; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsEditorLeftMarginFrame(AOwner: TComponent): TOptionsEditorLeftMarginFrame;

implementation

{$R *.dfm}

uses
  System.SysUtils, BCCommon.Options.Container, BCCommon.Utils;

var
  FOptionsEditorLeftMarginFrame: TOptionsEditorLeftMarginFrame;

function OptionsEditorLeftMarginFrame(AOwner: TComponent): TOptionsEditorLeftMarginFrame;
begin
  if not Assigned(FOptionsEditorLeftMarginFrame) then
    FOptionsEditorLeftMarginFrame := TOptionsEditorLeftMarginFrame.Create(AOwner);
  Result := FOptionsEditorLeftMarginFrame;
  AlignSliders(Result.Panel);
  AlignSliders(Result.GroupBoxLineNumbers);
end;

destructor TOptionsEditorLeftMarginFrame.Destroy;
begin
  inherited;
  FOptionsEditorLeftMarginFrame := nil;
end;

procedure TOptionsEditorLeftMarginFrame.PutData;
begin
  OptionsContainer.LeftMarginVisible := SliderVisible.SliderOn;
  OptionsContainer.LeftMarginAutosize := SliderAutosize.SliderOn;
  OptionsContainer.LeftMarginShowBookmarks := SliderShowBookmarks.SliderOn;
  OptionsContainer.LeftMarginShowBookmarkPanel := SliderShowBookmarkPanel.SliderOn;
  OptionsContainer.LeftMarginShowLineState := SliderShowLineState.SliderOn;
  OptionsContainer.LeftMarginLineNumbersShowInTens := SliderShowInTens.SliderOn;
  OptionsContainer.LeftMarginLineNumbersShowLeadingZeros := SliderShowLeadingZeros.SliderOn;
  OptionsContainer.LeftMarginLineNumbersShowAfterLastLine := SliderShowAfterLastLine.SliderOn;
  OptionsContainer.LeftMarginLineNumbersStartFrom := StrToIntDef(EditLineNumbersStartFrom.Text, 1);
  OptionsContainer.LeftMarginWidth := StrToIntDef(EditWidth.Text, 57);
  OptionsContainer.LeftMarginBookmarkPanelWidth := StrToIntDef(EditBookmarkPanelWidth.Text, 20);
end;

procedure TOptionsEditorLeftMarginFrame.GetData;
begin
  SliderVisible.SliderOn := OptionsContainer.LeftMarginVisible;
  SliderAutosize.SliderOn := OptionsContainer.LeftMarginAutosize;
  SliderShowBookmarks.SliderOn := OptionsContainer.LeftMarginShowBookmarks;
  SliderShowBookmarkPanel.SliderOn := OptionsContainer.LeftMarginShowBookmarkPanel;
  SliderShowLineState.SliderOn := OptionsContainer.LeftMarginShowLineState;
  SliderShowInTens.SliderOn := OptionsContainer.LeftMarginLineNumbersShowInTens;
  SliderShowLeadingZeros.SliderOn := OptionsContainer.LeftMarginLineNumbersShowLeadingZeros;
  SliderShowAfterLastLine.SliderOn := OptionsContainer.LeftMarginLineNumbersShowAfterLastLine;
  EditLineNumbersStartFrom.Text := IntToStr(OptionsContainer.LeftMarginLineNumbersStartFrom);
  EditWidth.Text := IntToStr(OptionsContainer.LeftMarginWidth);
  EditBookmarkPanelWidth.Text := IntToStr(OptionsContainer.LeftMarginBookmarkPanelWidth);
end;

end.
