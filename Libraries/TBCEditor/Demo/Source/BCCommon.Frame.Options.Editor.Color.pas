unit BCCommon.Frame.Options.Editor.Color;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, BCEditor.JsonDataObjects,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, sComboBox, BCControl.ComboBox,
  sComboBoxes, BCControl.Panel,
  BCEditor.Editor.Base, BCEditor.Editor, BCControl.SpeedButton,
  BCControl.GroupBox, BCCommon.Frame.Options.Base, sFrameAdapter, Vcl.ActnList, BCControl.ScrollBox,
  sDialogs, BCComponent.MultiStringHolder, sPageControl, BCControl.PageControl, sSplitter,
  BCControl.Edit, sLabel, BCControl.Labels,
  acSlider, System.Actions, sEdit, sGroupBox, Vcl.ComCtrls, Vcl.Buttons, sSpeedButton, Vcl.ExtCtrls, sPanel;

type
  TOptionsEditorColorFrame = class(TBCOptionsBaseFrame)
    ActionAddColor: TAction;
    ActionList: TActionList;
    ColorComboBoxEditorColor: TBCColorComboBox;
    ColorComboBoxElementsBackground: TBCColorComboBox;
    ColorComboBoxElementsForeground: TBCColorComboBox;
    ComboBoxColor: TBCComboBox;
    ComboBoxEditorElement: TBCComboBox;
    ComboBoxElementsName: TBCComboBox;
    ComboBoxHighlighter: TBCComboBox;
    DateEditDate: TBCEdit;
    EditEmail: TBCEdit;
    EditName: TBCEdit;
    Editor: TBCEditor;
    EditVersion: TBCEdit;
    GroupBoxAttributes: TBCGroupBox;
    MultiStringHolder: TBCMultiStringHolder;
    PageControl: TBCPageControl;
    Panel: TBCPanel;
    SaveDialog: TsSaveDialog;
    SpeedButtonColor: TBCSpeedButton;
    Splitter: TsSplitter;
    TabSheetAuthor: TsTabSheet;
    TabSheetEditor: TsTabSheet;
    TabSheetElements: TsTabSheet;
    TabSheetGeneral: TsTabSheet;
    TabSheetUseSkinColor: TsTabSheet;
    StickyLabelElementsAttributesBold: TsStickyLabel;
    SliderElementsAttributesBold: TsSlider;
    StickyLabelElementsAttributesItalic: TsStickyLabel;
    SliderElementsAttributesItalic: TsSlider;
    StickyLabelElementsAttributesUnderline: TsStickyLabel;
    SliderElementsAttributesUnderline: TsSlider;
    PanelUseSkinColorLeft: TBCPanel;
    SliderSkinActiveLineBackground: TsSlider;
    StickyLabelSkinActiveLineBackground: TsStickyLabel;
    StickyLabelSkinForeground: TsStickyLabel;
    SliderSkinForeground: TsSlider;
    StickyLabelSkinSelectionForeground: TsStickyLabel;
    SliderSkinSelectionForeground: TsSlider;
    StickyLabelSkinLeftMarginBackground: TsStickyLabel;
    SliderSkinLeftMarginBackground: TsSlider;
    StickyLabelSkinCodeFoldingBackground: TsStickyLabel;
    SliderSkinCodeFoldingBackground: TsSlider;
    StickyLabelSkinCompletionProposalBackground: TsStickyLabel;
    SliderSkinCompletionProposalBackground: TsSlider;
    PanelUseSkinColorRight: TBCPanel;
    StickyLabelSkinBackground: TsStickyLabel;
    SliderSkinBackground: TsSlider;
    StickyLabelSkinSelectionBackground: TsStickyLabel;
    SliderSkinSelectionBackground: TsSlider;
    StickyLabelSkinBookmarkPanelBackground: TsStickyLabel;
    SliderSkinBookmarkPanelBackground: TsSlider;
    StickyLabelSkinCodeFoldingHintBackground: TsStickyLabel;
    SliderSkinCodeFoldingHintBackground: TsSlider;
    StickyLabelSkinCompletionProposalSelectionBackground: TsStickyLabel;
    SliderSkinCompletionProposalSelectionBackground: TsSlider;
    procedure ActionAddColorExecute(Sender: TObject);
    procedure ComboBoxColorChange(Sender: TObject);
    procedure ColorComboBoxEditorColorChange(Sender: TObject);
    procedure ColorComboBoxElementsForegroundChange(Sender: TObject);
    procedure ColorComboBoxElementsBackgroundChange(Sender: TObject);
    procedure SliderElementsAttributesClick(Sender: TObject);
    procedure ComboBoxHighlighterChange(Sender: TObject);
    procedure ComboBoxEditorElementChange(Sender: TObject);
    procedure ComboBoxElementsNameChange(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure SliderSkinValueClick(Sender: TObject);
  private
    FFileName: string;
    FJSONObject: TJsonObject;
    FModified: Boolean;
    function GetColorFileName: string;
    function GetElementDataValue: PJsonDataValue;
    procedure CreateJSONObject;
    procedure FreeJSONObject;
    procedure LoadColors;
    procedure SaveColor(ADoChange: Boolean = True);
    procedure SetSkinColors;
  protected
    procedure Init; override;
    procedure PutData; override;
    procedure GetData; override;
  public
    destructor Destroy; override;
  end;

function OptionsEditorColorFrame(AOwner: TComponent): TOptionsEditorColorFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Options.Container, BCCommon.Language.Strings, BCCommon.StringUtils, BCEditor.Highlighter.Colors,
  BCCommon.Utils;

var
  FOptionsEditorColorFrame: TOptionsEditorColorFrame;

function OptionsEditorColorFrame(AOwner: TComponent): TOptionsEditorColorFrame;
begin
  if not Assigned(FOptionsEditorColorFrame) then
    FOptionsEditorColorFrame := TOptionsEditorColorFrame.Create(AOwner);
  Result := FOptionsEditorColorFrame;
  AlignSliders(Result.GroupBoxAttributes);
  AlignSliders(Result.PanelUseSkinColorLeft);
  AlignSliders(Result.PanelUseSkinColorRight);
end;

procedure TOptionsEditorColorFrame.FreeJSONObject;
begin
  if Assigned(FJSONObject) then
  begin
    FJSONObject.Free;
    FJSONObject := nil;
  end;
end;

destructor TOptionsEditorColorFrame.Destroy;
begin
  inherited;
  FreeJSONObject;
  FOptionsEditorColorFrame := nil;
end;

procedure TOptionsEditorColorFrame.EditChange(Sender: TObject);
begin
  inherited;
  FModified := True;
end;

procedure TOptionsEditorColorFrame.PutData;
begin
  OptionsContainer.SkinActiveLineBackground := SliderSkinActiveLineBackground.SliderOn;
  OptionsContainer.SkinBackground := SliderSkinBackground.SliderOn;
  OptionsContainer.SkinBookmarkPanelBackground := SliderSkinBookmarkPanelBackground.SliderOn;
  OptionsContainer.SkinCodeFoldingBackground := SliderSkinCodeFoldingBackground.SliderOn;
  OptionsContainer.SkinCodeFoldingHintBackground := SliderSkinCodeFoldingHintBackground.SliderOn;
  OptionsContainer.SkinCompletionProposalBackground := SliderSkinCompletionProposalBackground.SliderOn;
  OptionsContainer.SkinCompletionProposalSelectionBackground := SliderSkinCompletionProposalSelectionBackground.SliderOn;
  OptionsContainer.SkinForeground := SliderSkinForeground.SliderOn;
  OptionsContainer.SkinLeftMarginBackground := SliderSkinLeftMarginBackground.SliderOn;
  OptionsContainer.SkinSelectionBackground := SliderSkinSelectionBackground.SliderOn;
  OptionsContainer.SkinSelectionForeground := SliderSkinSelectionForeground.SliderOn;
  SaveColor(False);
end;

procedure TOptionsEditorColorFrame.SaveColor(ADoChange: Boolean = True);
begin
  if Assigned(FJSONObject) then
  begin
    FJSONObject['Colors']['Info']['General']['Version'] := EditVersion.Text;
    FJSONObject['Colors']['Info']['General']['Date'] := DateEditDate.Text;
    FJSONObject['Colors']['Info']['Author']['Name'] := EditName.Text;
    FJSONObject['Colors']['Info']['Author']['Email'] := EditEmail.Text;

    JsonSerializationConfig.IndentChar := '    ';
    FJSONObject.SaveToFile(GetColorFileName, False);
    if ADoChange then
      ComboBoxColorChange(Self);
  end;
end;

procedure TOptionsEditorColorFrame.SliderElementsAttributesClick(Sender: TObject);
var
  LStyle: string;
  LElementDataValue: PJsonDataValue;
begin
  LElementDataValue := GetElementDataValue;
  if Assigned(LElementDataValue) then
  begin
    FModified := True;
    LStyle := '';
    if SliderElementsAttributesBold.SliderOn then
      LStyle := 'Bold';
    if SliderElementsAttributesItalic.SliderOn then
    begin
      if LStyle <> '' then
        LStyle := LStyle + ';';
      LStyle := LStyle + 'Italic';
    end;
    if SliderElementsAttributesUnderline.SliderOn then
    begin
      if LStyle <> '' then
        LStyle := LStyle + ';';
      LStyle := LStyle + 'Underline';
    end;
    LElementDataValue.ObjectValue['Style'] := LStyle;
    SaveColor;
  end;
end;

procedure TOptionsEditorColorFrame.SliderSkinValueClick(Sender: TObject);
begin
  inherited;
  LoadColors;
  SetSkinColors;
end;

procedure TOptionsEditorColorFrame.SetSkinColors;
var
  i: Integer;
  LColor: TColor;
begin
  LColor := FrameAdapter.SkinData.SkinManager.GetActiveEditColor;
  if SliderSkinActiveLineBackground.SliderOn then
    Editor.ActiveLine.Color := FrameAdapter.SkinData.SkinManager.GetHighLightColor(False);
  if SliderSkinBackground.SliderOn then
    Editor.BackgroundColor := LColor;
  if SliderSkinCodeFoldingBackground.SliderOn then
    Editor.CodeFolding.Colors.Background := LColor;
  if SliderSkinCodeFoldingHintBackground.SliderOn then
    Editor.CodeFolding.Hint.Colors.Background := LColor;
  if SliderSkinCompletionProposalBackground.SliderOn then
    Editor.CompletionProposal.Colors.Background := LColor;
  if SliderSkinCompletionProposalSelectionBackground.SliderOn then
    Editor.CompletionProposal.Colors.SelectedBackground := LColor;
  if SliderSkinLeftMarginBackground.SliderOn then
    Editor.LeftMargin.Colors.Background := LColor;
  if SliderSkinBookmarkPanelBackground.SliderOn then
    Editor.LeftMargin.Colors.BookmarkPanelBackground := LColor;
  if SliderSkinSelectionForeground.SliderOn then
    Editor.Selection.Colors.Foreground := FrameAdapter.SkinData.SkinManager.GetHighLightFontColor;
  if SliderSkinSelectionBackground.SliderOn then
    Editor.Selection.Colors.Background := FrameAdapter.SkinData.SkinManager.GetHighLightColor;
  for i := 0 to Editor.Highlighter.Colors.Styles.Count - 1 do
  if PBCEditorHighlighterElement(Editor.Highlighter.Colors.Styles.Items[i])^.Name = 'Editor' then
  begin
    if SliderSkinForeground.SliderOn then
      PBCEditorHighlighterElement(Editor.Highlighter.Colors.Styles.Items[i])^.ForeGround := FrameAdapter.SkinData.SkinManager.GetActiveEditFontColor;
    if SliderSkinBackground.SliderOn then
      PBCEditorHighlighterElement(Editor.Highlighter.Colors.Styles.Items[i])^.Background := LColor;
    Break;
  end;
  Editor.Highlighter.UpdateColors;
end;

procedure TOptionsEditorColorFrame.GetData;
begin
  SliderSkinActiveLineBackground.SliderOn := OptionsContainer.SkinActiveLineBackground;
  SliderSkinBackground.SliderOn := OptionsContainer.SkinBackground;
  SliderSkinBookmarkPanelBackground.SliderOn := OptionsContainer.SkinBookmarkPanelBackground;
  SliderSkinCodeFoldingBackground.SliderOn := OptionsContainer.SkinCodeFoldingBackground;
  SliderSkinCodeFoldingHintBackground.SliderOn := OptionsContainer.SkinCodeFoldingHintBackground;
  SliderSkinCompletionProposalBackground.SliderOn := OptionsContainer.SkinCompletionProposalBackground;
  SliderSkinCompletionProposalSelectionBackground.SliderOn := OptionsContainer.SkinCompletionProposalSelectionBackground;
  SliderSkinForeground.SliderOn := OptionsContainer.SkinForeground;
  SliderSkinLeftMarginBackground.SliderOn := OptionsContainer.SkinLeftMarginBackground;
  SliderSkinSelectionBackground.SliderOn := OptionsContainer.SkinSelectionBackground;
  SliderSkinSelectionForeground.SliderOn := OptionsContainer.SkinSelectionForeground;

  LoadColors;
  SetSkinColors;
end;

procedure TOptionsEditorColorFrame.ColorComboBoxEditorColorChange(Sender: TObject);
begin
  FModified := True;
  FJSONObject['Colors']['Editor']['Colors'][CapitalizeText(ComboBoxEditorElement.Text)] := ColorToString(ColorComboBoxEditorColor.Selected);
  SaveColor;
end;

procedure TOptionsEditorColorFrame.ColorComboBoxElementsBackgroundChange(Sender: TObject);
var
  LElementDataValue: PJsonDataValue;
begin
  LElementDataValue := GetElementDataValue;
  if Assigned(LElementDataValue) then
  begin
    FModified := True;
    LElementDataValue.ObjectValue['Background'] := ColorComboBoxElementsBackground.ColorText;
    SaveColor;
  end;
end;

procedure TOptionsEditorColorFrame.ColorComboBoxElementsForegroundChange(Sender: TObject);
var
  LElementDataValue: PJsonDataValue;
begin
  LElementDataValue := GetElementDataValue;
  if Assigned(LElementDataValue) then
  begin
    FModified := True;
    LElementDataValue.ObjectValue['Foreground'] := ColorComboBoxElementsForeground.ColorText;
    SaveColor;
  end;
end;

function TOptionsEditorColorFrame.GetColorFileName: string;
begin
  Result := Format('%sColors\%s.json', [ExtractFilePath(Application.ExeName), FFileName]);
end;

procedure TOptionsEditorColorFrame.LoadColors;
var
  LFileName: string;
begin
  LFileName := GetColorFileName;
  with Editor do
  begin
    Highlighter.Colors.LoadFromFile(LFileName);
    Invalidate;
  end;
end;

procedure TOptionsEditorColorFrame.CreateJSONObject;
var
  LFileName: string;
begin
  LFileName := GetColorFileName;
  FreeJSONObject;
  FJSONObject := TJsonObject.ParseFromFile(LFileName) as TJsonObject;
end;

procedure TOptionsEditorColorFrame.ComboBoxColorChange(Sender: TObject);
begin
  if FModified then
    SaveColor(False);

  FFileName := ComboBoxColor.Text;
  LoadColors;
  CreateJSONObject;
  FModified := False;

  ComboBoxEditorElementChange(Self);
  ComboBoxElementsNameChange(Self);
  EditVersion.Text := FJSONObject['Colors']['Info']['General']['Version'];
  DateEditDate.Text := FJSONObject['Colors']['Info']['General']['Date'];
  EditName.Text := FJSONObject['Colors']['Info']['Author']['Name'];
  EditEmail.Text := FJSONObject['Colors']['Info']['Author']['Email'];
end;

procedure TOptionsEditorColorFrame.ComboBoxEditorElementChange(Sender: TObject);
begin
  ColorComboBoxEditorColor.Selected := clNone;
  if ComboBoxEditorElement.Text <> '' then
    ColorComboBoxEditorColor.Selected := StringToColor(FJSONObject['Colors']['Editor']['Colors'][CapitalizeText(ComboBoxEditorElement.Text)]);
end;

function TOptionsEditorColorFrame.GetElementDataValue: PJsonDataValue;
var
  i: Integer;
  LElement: string;
  LElementArray: TJsonArray;
begin
  Result := nil;
  LElement := CapitalizeText(ComboBoxElementsName.Text);
  LElementArray := FJSONObject['Colors']['Elements'].ArrayValue;
  for i := 0 to LElementArray.Count - 1 do
    if LElementArray.Items[i].ObjectValue['Name'] = LElement then
      Exit(LElementArray.Items[i])
end;

procedure TOptionsEditorColorFrame.ComboBoxElementsNameChange(Sender: TObject);
var
  LStyle: string;
  LElementDataValue: PJsonDataValue;
begin
  LElementDataValue := GetElementDataValue;
  if Assigned(LElementDataValue) then
  begin
    ColorComboBoxElementsForeground.Selected := clNone;
    ColorComboBoxElementsForeground.Selected := StringToColor(LElementDataValue.ObjectValue['Foreground']);
    ColorComboBoxElementsBackground.Selected := clNone;
    ColorComboBoxElementsBackground.Selected := StringToColor(LElementDataValue.ObjectValue['Background']);
    LStyle := LElementDataValue.ObjectValue['Style'];
    SliderElementsAttributesBold.SliderOn := Pos('Bold', LStyle) <> 0;
    SliderElementsAttributesItalic.SliderOn := Pos('Italic', LStyle) <> 0;
    SliderElementsAttributesUnderline.SliderOn := Pos('Underline', LStyle) <> 0;
    GroupBoxAttributes.Invalidate;
  end;
end;

procedure TOptionsEditorColorFrame.ComboBoxHighlighterChange(Sender: TObject);
begin
  with Editor do
  begin
    Highlighter.LoadFromFile(Format('%s.json', [ComboBoxHighlighter.Text]));
    Lines.Text := Highlighter.Info.General.Sample;
    if Highlighter.CodeFoldingRangeCount > 0 then // Highlighter.CodeFoldingRegions.Count > 0 then
      CodeFolding.Visible := OptionsContainer.ShowCodeFolding;
    Invalidate;
  end;
end;

procedure TOptionsEditorColorFrame.Init;
begin
  PageControl.ActivePage := TabSheetEditor;
  FModified := False;
  ComboBoxHighlighter.Items := OptionsContainer.HighlighterStrings;
  ComboBoxHighlighter.ItemIndex := ComboBoxHighlighter.Items.IndexOf(OptionsContainer.DefaultHighlighter);
  ComboBoxColor.Items := OptionsContainer.HighlighterColorStrings;
  ComboBoxColor.ItemIndex := ComboBoxColor.Items.IndexOf(OptionsContainer.DefaultColor);
  ComboBoxHighlighterChange(nil);
  FFileName := ComboBoxColor.Text;
  ComboBoxColorChange(Self);
  ComboBoxEditorElementChange(Self);
  ComboBoxElementsNameChange(Self);
end;

procedure TOptionsEditorColorFrame.ActionAddColorExecute(Sender: TObject);
var
  LColorName: string;
begin
  SaveDialog.Filter := Trim(StringReplace(LanguageDataModule.GetFileTypes('JSON')
        , '|', #0, [rfReplaceAll])) + #0#0;
  SaveDialog.Title := LanguageDataModule.GetConstant('SaveAs');
  SaveDialog.InitialDir := Format('%sColors\', [ExtractFilePath(Application.ExeName)]);
  SaveDialog.FileName := '';
  if SaveDialog.Execute(Handle) then
  begin
    LColorName :=  ChangeFileExt(ExtractFileName(SaveDialog.FileName), '');
    ComboBoxColor.Items.Add(LColorName);
    ComboBoxColor.ItemIndex := ComboBoxColor.IndexOf(LColorName);
    FreeJSONObject;
    FJSONObject := TJsonObject.Parse(MultiStringHolder.StringsByName['DefaultJSON'].Text) as TJsonObject;
    SaveColor;
    ComboBoxColorChange(Self);
  end;
end;

end.
