unit BCCommon.Frame.Options.Editor.Search;

interface

uses
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  BCCommon.Frame.Options.Base, BCControl.Panel, acSlider, sLabel, Vcl.StdCtrls, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsEditorSearchFrame = class(TBCOptionsBaseFrame)
    Panel: TBCPanel;
    StickyLabelDocumentSpecificSearch: TsStickyLabel;
    SliderDocumentSpecificSearch: TsSlider;
    StickyLabelShowSearchMap: TsStickyLabel;
    SliderShowSearchMap: TsSlider;
    StickyLabelVisible: TsStickyLabel;
    SliderVisible: TsSlider;
  protected
    procedure GetData; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsEditorSearchFrame(AOwner: TComponent): TOptionsEditorSearchFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Options.Container, BCCommon.Utils;

var
  FOptionsEditorSearchFrame: TOptionsEditorSearchFrame;

function OptionsEditorSearchFrame(AOwner: TComponent): TOptionsEditorSearchFrame;
begin
  if not Assigned(FOptionsEditorSearchFrame) then
    FOptionsEditorSearchFrame := TOptionsEditorSearchFrame.Create(AOwner);
  Result := FOptionsEditorSearchFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsEditorSearchFrame.Destroy;
begin
  inherited;
  FOptionsEditorSearchFrame := nil;
end;

procedure TOptionsEditorSearchFrame.PutData;
begin
  OptionsContainer.SearchVisible := SliderVisible.SliderOn;
  OptionsContainer.DocumentSpecificSearch := SliderDocumentSpecificSearch.SliderOn;
  OptionsContainer.ShowSearchMap := SliderShowSearchMap.SliderOn;
end;

procedure TOptionsEditorSearchFrame.GetData;
begin
  SliderVisible.SliderOn := OptionsContainer.SearchVisible;
  SliderDocumentSpecificSearch.SliderOn := OptionsContainer.DocumentSpecificSearch;
  SliderShowSearchMap.SliderOn := OptionsContainer.ShowSearchMap;
end;

end.
