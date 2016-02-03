unit BCCommon.Frame.Options.StatusBar;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  BCCommon.Options.Container,
  BCCommon.Frame.Options.Base, BCControl.Panel, BCControl.Edit,
  sFontCtrls, BCControl.ComboBox, acSlider, sLabel, Vcl.StdCtrls, sEdit, sComboBox, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsStatusBarFrame = class(TBCOptionsBaseFrame)
    EditFontSize: TBCEdit;
    FontComboBoxFont: TBCFontComboBox;
    Panel: TBCPanel;
    StickyLabelUseSystemFont: TsStickyLabel;
    SliderUseSystemFont: TsSlider;
    SliderShowMacro: TsSlider;
    StickyLabelShowMacro: TsStickyLabel;
    SliderShowCaretPosition: TsSlider;
    StickyLabelShowCaretPosition: TsStickyLabel;
    SliderShowKeyState: TsSlider;
    StickyLabelShowKeyState: TsStickyLabel;
    SliderShowModified: TsSlider;
    StickyLabelShowModified: TsStickyLabel;
  protected
    procedure GetData; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsStatusBarFrame(AOwner: TComponent): TOptionsStatusBarFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Utils;

var
  FOptionsStatusBarFrame: TOptionsStatusBarFrame;

function OptionsStatusBarFrame(AOwner: TComponent): TOptionsStatusBarFrame;
begin
  if not Assigned(FOptionsStatusBarFrame) then
    FOptionsStatusBarFrame := TOptionsStatusBarFrame.Create(AOwner);
  Result := FOptionsStatusBarFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsStatusBarFrame.Destroy;
begin
  inherited;
  FOptionsStatusBarFrame := nil;
end;

procedure TOptionsStatusBarFrame.PutData;
begin
  OptionsContainer.StatusBarUseSystemFont := SliderUseSystemFont.SliderOn;
  OptionsContainer.StatusBarFontName := FontComboBoxFont.Text;
  OptionsContainer.StatusBarFontSize := EditFontSize.ValueInt;
  OptionsContainer.StatusBarShowMacro := SliderShowMacro.SliderOn;
  OptionsContainer.StatusBarShowCaretPosition := SliderShowCaretPosition.SliderOn;
  OptionsContainer.StatusBarShowKeyState := SliderShowKeyState.SliderOn;
  OptionsContainer.StatusBarShowModified := SliderShowModified.SliderOn;
end;

procedure TOptionsStatusBarFrame.GetData;
begin
  SliderUseSystemFont.SliderOn := OptionsContainer.StatusBarUseSystemFont;
  FontComboBoxFont.ItemIndex := FontComboBoxFont.Items.IndexOf(OptionsContainer.StatusBarFontName);
  EditFontSize.ValueInt := OptionsContainer.StatusBarFontSize;
  SliderShowMacro.SliderOn := OptionsContainer.StatusBarShowMacro;
  SliderShowCaretPosition.SliderOn := OptionsContainer.StatusBarShowCaretPosition;
  SliderShowKeyState.SliderOn := OptionsContainer.StatusBarShowKeyState;
  SliderShowModified.SliderOn := OptionsContainer.StatusBarShowModified;
end;

end.
