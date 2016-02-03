unit BCCommon.Frame.Options.Editor.Minimap;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, BCControl.Edit,
  BCControl.Panel, BCCommon.Frame.Options.Base, acSlider, sLabel, sEdit, Vcl.ExtCtrls, sPanel, sFrameAdapter,
  sComboBox, BCControl.ComboBox;

type
  TOptionsEditorMinimapFrame = class(TBCOptionsBaseFrame)
    EditWidth: TBCEdit;
    Panel: TBCPanel;
    StickyLabelVisible: TsStickyLabel;
    SliderVisible: TsSlider;
    StickyLabelShowIndentGuides: TsStickyLabel;
    SliderShowIndentGuides: TsSlider;
    SliderShowBookmarks: TsSlider;
    StickyLabelShowBookmarks: TsStickyLabel;
    ComboBoxAlign: TBCComboBox;
  protected
    procedure GetData; override;
    procedure Init; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsEditorMinimapFrame(AOwner: TComponent): TOptionsEditorMinimapFrame;

implementation

{$R *.dfm}

uses
  System.SysUtils, BCCommon.Options.Container, BCCommon.Utils, BCCommon.Language.Strings;

var
  FOptionsEditorMinimapFrame: TOptionsEditorMinimapFrame;

function OptionsEditorMinimapFrame(AOwner: TComponent): TOptionsEditorMinimapFrame;
begin
  if not Assigned(FOptionsEditorMinimapFrame) then
    FOptionsEditorMinimapFrame := TOptionsEditorMinimapFrame.Create(AOwner);
  Result := FOptionsEditorMinimapFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsEditorMinimapFrame.Destroy;
begin
  inherited;
  FOptionsEditorMinimapFrame := nil;
end;

procedure TOptionsEditorMinimapFrame.Init;
begin
  with ComboBoxAlign.Items do
  begin
    Add(LanguageDatamodule.GetSQLFormatter('Left'));
    Add(LanguageDatamodule.GetSQLFormatter('Right'));
  end;
end;

procedure TOptionsEditorMinimapFrame.PutData;
begin
  with OptionsContainer do
  begin
    MinimapVisible := SliderVisible.SliderOn;
    MinimapShowBookmarks := SliderShowBookmarks.SliderOn;
    MinimapShowIndentGuides := SliderShowIndentGuides.SliderOn;
    MinimapWidth := StrToIntDef(EditWidth.Text, 100);
    MinimapAlign := ComboBoxAlign.ItemIndex;
  end;
end;

procedure TOptionsEditorMinimapFrame.GetData;
begin
  with OptionsContainer do
  begin
    SliderVisible.SliderOn := MinimapVisible;
    SliderShowBookmarks.SliderOn := MinimapShowBookmarks;
    SliderShowIndentGuides.SliderOn := MinimapShowIndentGuides;
    EditWidth.Text := IntToStr(MinimapWidth);
    ComboBoxAlign.ItemIndex := MinimapAlign;
  end;
end;

end.
