unit BCCommon.Frame.Options.Editor.Caret;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BCControl.ComboBox, Vcl.StdCtrls,
  sComboBox, BCControl.Panel, BCControl.GroupBox, BCCommon.Frame.Options.Base,
  acSlider, sLabel, Vcl.ComCtrls, sComboBoxes, sGroupBox, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsEditorCaretFrame = class(TBCOptionsBaseFrame)
    ColorBoxNonblinkingCaretBackground: TBCColorComboBox;
    ColorBoxNonblinkingCaretForeground: TBCColorComboBox;
    ComboBoxStylesInsertCaret: TBCComboBox;
    ComboBoxStylesOverwriteCaret: TBCComboBox;
    GroupBoxNonBlinkingCaret: TBCGroupBox;
    GroupBoxStyles: TBCGroupBox;
    Panel: TBCPanel;
    StickyLabelVisible: TsStickyLabel;
    SliderVisible: TsSlider;
    StickyLabelRightMouseClickMovesCaret: TsStickyLabel;
    SliderRightMouseClickMovesCaret: TsSlider;
    StickyLabelNonblinkingCaretEnabled: TsStickyLabel;
    SliderNonblinkingCaretEnabled: TsSlider;
  public
    destructor Destroy; override;
    procedure Init; override;
    procedure GetData; override;
    procedure PutData; override;
  end;

function OptionsEditorCaretFrame(AOwner: TComponent): TOptionsEditorCaretFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Options.Container, BCCommon.Language.Strings, BCCommon.Utils;

var
  FOptionsEditorCaretFrame: TOptionsEditorCaretFrame;

function OptionsEditorCaretFrame(AOwner: TComponent): TOptionsEditorCaretFrame;
begin
  if not Assigned(FOptionsEditorCaretFrame) then
    FOptionsEditorCaretFrame := TOptionsEditorCaretFrame.Create(AOwner);
  Result := FOptionsEditorCaretFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsEditorCaretFrame.Destroy;
begin
  inherited;
  FOptionsEditorCaretFrame := nil;
end;

procedure TOptionsEditorCaretFrame.Init;

  procedure AddComboItems(AComboBox: TBCComboBox);
  begin
    with AComboBox.Items do
    begin
      Clear;
      Add(LanguageDatamodule.GetConstant('VerticalLine'));
      Add(LanguageDatamodule.GetConstant('ThinVerticalLine'));
      Add(LanguageDatamodule.GetConstant('HorizontalLine'));
      Add(LanguageDatamodule.GetConstant('ThinHorizontalLine'));
      Add(LanguageDatamodule.GetConstant('HalfBlock'));
      Add(LanguageDatamodule.GetConstant('Block'));
    end;
  end;

begin
  inherited;

  AddComboItems(ComboBoxStylesInsertCaret);
  AddComboItems(ComboBoxStylesOverwriteCaret);
end;

procedure TOptionsEditorCaretFrame.PutData;
begin
  OptionsContainer.ShowCaret := SliderVisible.SliderOn;
  OptionsContainer.RightMouseClickMovesCaret := SliderRightMouseClickMovesCaret.SliderOn;
  OptionsContainer.ShowNonblinkingCaret := SliderNonblinkingCaretEnabled.SliderOn;
  OptionsContainer.NonblinkingCaretBackgroundColor := ColorBoxNonblinkingCaretBackground.ColorText;
  OptionsContainer.NonblinkingCaretForegroundColor := ColorBoxNonblinkingCaretForeground.ColorText;
  OptionsContainer.InsertCaret := ComboBoxStylesInsertCaret.ItemIndex;
  OptionsContainer.OverwriteCaret := ComboBoxStylesOverwriteCaret.ItemIndex;
end;

procedure TOptionsEditorCaretFrame.GetData;
begin
  SliderVisible.SliderOn := OptionsContainer.ShowCaret;
  SliderRightMouseClickMovesCaret.SliderOn := OptionsContainer.RightMouseClickMovesCaret;
  SliderNonblinkingCaretEnabled.SliderOn := OptionsContainer.ShowNonblinkingCaret;
  ColorBoxNonblinkingCaretBackground.ColorText := OptionsContainer.NonblinkingCaretBackgroundColor;
  ColorBoxNonblinkingCaretForeground.ColorText := OptionsContainer.NonblinkingCaretForegroundColor;
  ComboBoxStylesInsertCaret.ItemIndex := OptionsContainer.InsertCaret;
  ComboBoxStylesOverwriteCaret.ItemIndex := OptionsContainer.OverwriteCaret;
end;

end.
