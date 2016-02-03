unit BCCommon.Frame.Options.MainMenu;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  BCControl.ComboBox, BCControl.Edit,
  BCCommon.Options.Container, BCCommon.Frame.Options.Base, BCControl.Panel,
  sFontCtrls, acSlider, sLabel, Vcl.StdCtrls, sEdit, sComboBox, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsMainMenuFrame = class(TBCOptionsBaseFrame)
    Panel: TBCPanel;
    StickyLabelUseSystemFont: TsStickyLabel;
    SliderUseSystemFont: TsSlider;
    FontComboBoxFont: TBCFontComboBox;
    EditFontSize: TBCEdit;
  protected
    procedure GetData; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsMainMenuFrame(AOwner: TComponent): TOptionsMainMenuFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Utils;

var
  FOptionsMainMenuFrame: TOptionsMainMenuFrame;

function OptionsMainMenuFrame(AOwner: TComponent): TOptionsMainMenuFrame;
begin
  if not Assigned(FOptionsMainMenuFrame) then
    FOptionsMainMenuFrame := TOptionsMainMenuFrame.Create(AOwner);
  Result := FOptionsMainMenuFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsMainMenuFrame.Destroy;
begin
  inherited;
  FOptionsMainMenuFrame := nil;
end;

procedure TOptionsMainMenuFrame.PutData;
begin
  OptionsContainer.MainMenuUseSystemFont := SliderUseSystemFont.SliderOn;
  OptionsContainer.MainMenuFontName := FontComboBoxFont.Text;
  OptionsContainer.MainMenuFontSize := EditFontSize.ValueInt;
end;

procedure TOptionsMainMenuFrame.GetData;
begin
  SliderUseSystemFont.SliderOn := OptionsContainer.MainMenuUseSystemFont;
  FontComboBoxFont.ItemIndex := FontComboBoxFont.Items.IndexOf(OptionsContainer.MainMenuFontName);
  EditFontSize.ValueInt := OptionsContainer.MainMenuFontSize;
end;

end.
