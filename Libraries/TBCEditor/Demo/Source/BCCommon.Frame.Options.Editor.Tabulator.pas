unit BCCommon.Frame.Options.Editor.Tabulator;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, BCControl.Edit,
  BCControl.Panel, BCCommon.Frame.Options.Base, acSlider, sLabel, sEdit, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsEditorTabulatorFrame = class(TBCOptionsBaseFrame)
    EditWidth: TBCEdit;
    Panel: TBCPanel;
    StickyLabelSelectedBlockIndent: TsStickyLabel;
    SliderSelectedBlockIndent: TsSlider;
    StickyLabelTabsToSpaces: TsStickyLabel;
    SliderTabsToSpaces: TsSlider;
  protected
    procedure GetData; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsEditorTabulatorFrame(AOwner: TComponent): TOptionsEditorTabulatorFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Options.Container, BCCommon.Utils;

var
  FOptionsEditorTabulatorFrame: TOptionsEditorTabulatorFrame;

function OptionsEditorTabulatorFrame(AOwner: TComponent): TOptionsEditorTabulatorFrame;
begin
  if not Assigned(FOptionsEditorTabulatorFrame) then
    FOptionsEditorTabulatorFrame := TOptionsEditorTabulatorFrame.Create(AOwner);
  Result := FOptionsEditorTabulatorFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsEditorTabulatorFrame.Destroy;
begin
  inherited;
  FOptionsEditorTabulatorFrame := nil;
end;

procedure TOptionsEditorTabulatorFrame.PutData;
begin
  OptionsContainer.SelectedBlockIndent := SliderSelectedBlockIndent.SliderOn;
  OptionsContainer.TabsToSpaces := SliderTabsToSpaces.SliderOn;
  OptionsContainer.TabWidth := StrToIntDef(EditWidth.Text, 2);
end;

procedure TOptionsEditorTabulatorFrame.GetData;
begin
  SliderSelectedBlockIndent.SliderOn := OptionsContainer.SelectedBlockIndent;
  SliderTabsToSpaces.SliderOn := OptionsContainer.TabsToSpaces;
  EditWidth.Text := IntToStr(OptionsContainer.LeftMarginWidth);
end;

end.
