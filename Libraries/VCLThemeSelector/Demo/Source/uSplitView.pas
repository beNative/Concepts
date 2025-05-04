{******************************************************************************}
{  ModernAppDemo by Carlo Barazzetta                                           }
{  A full example of an HighDPI - VCL Themed enabled application               }
{  See how to select the application Theme using VCLThemeSelector Form         }
{                                                                              }
{       Copyright (c) 2020-2023 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       https://github.com/EtheaDev/VCLThemeSelector                           }
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
//-----------------------------------------------------------------------------
// Original software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.
//-----------------------------------------------------------------------------

unit uSplitView;

{$I ..\..\Source\VCLThemeSelector.inc}

interface

uses
  Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , System.Variants
  , System.Classes
  , System.ImageList
  , System.Actions
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.ExtCtrls
  , Vcl.WinXCtrls
  , Vcl.StdCtrls
  , Vcl.CategoryButtons
  , Vcl.Buttons
  , Vcl.ImgList
  , Vcl.Imaging.PngImage
  , Vcl.ComCtrls
  , Vcl.ActnList
  , Vcl.Menus
  , Vcl.Mask
  , Data.DB
  , Datasnap.DBClient
  , Vcl.WinXCalendars
  , Vcl.CheckLst
  , Vcl.DBCtrls
  , Vcl.ColorGrd
  , Vcl.Samples.Spin
  , Vcl.Grids
  , Vcl.DBGrids
  , Vcl.ToolWin
  , Vcl.GraphUtil
  ,  EditForm
  , IconFontsImageList //uses IconFontsImageList - download free at: https://github.com/EtheaDev/IconFontsImageList
  , SVGIconImageList //uses SVGIconImageList - download free at: https://github.com/EtheaDev/SVGIconImageList
  , IconFontsUtils
  , FVCLThemeSelector
  , SVGIconImageListBase
  , IconFontsImageListBase
  , SVGIconVirtualImageList
  , IconFontsVirtualImageList
  , DImageCollections
  , SVGIconImage
  , IconFontsImage
{$IFDEF VCLSTYLEUTILS}
  //VCLStyles support:
  //to compile you need to donwload VCLStyleUtils+DDetours from:
  //https://github.com/RRUZ/vcl-styles-utils
  //https://github.com/MahdiSafsafi/DDetours
  , Vcl.PlatformVclStylesActnCtrls
  , Vcl.Styles.ColorTabs
  , Vcl.Styles.ControlColor
  , Vcl.Styles.DbGrid
  , Vcl.Styles.DPIAware
  , Vcl.Styles.Fixes
  , Vcl.Styles.Ext
  , Vcl.Styles.FontAwesome
  , Vcl.Styles.FormStyleHooks
  , Vcl.Styles.NC
  , Vcl.Styles.OwnerDrawFix
  , Vcl.Styles.Utils.ScreenTips
  , Vcl.Styles.Utils.SysStyleHook
  , Vcl.Styles.WebBrowser
  , Vcl.Styles.Utils
  , Vcl.Styles.Utils.Menus
  , Vcl.Styles.Utils.Misc
  , Vcl.Styles.Utils.SystemMenu
  , Vcl.Styles.Utils.Graphics
  , Vcl.Styles.Utils.SysControls
  , Vcl.Styles.UxTheme
  , Vcl.Styles.Hooks
  , Vcl.Styles.Utils.Forms
  , Vcl.Styles.Utils.ComCtrls
  , Vcl.Styles.Utils.StdCtrls, Vcl.VirtualImageList
{$ENDIF}

{$IFDEF D10_2+}
  , Vcl.WinXPickers
{$ENDIF}
  {$IFDEF D10_4+}
  , Vcl.NumberBox
{$ENDIF}
  {$IFDEF STYLEDCOMPONENTS}
  , Vcl.StyledComponentsHooks
  , Vcl.StyledCategoryButtons
  , Vcl.ButtonStylesAttributes
  {$ENDIF}
  ;

const
  COMPANY_NAME = 'Ethea';

type
  TFormMain = class(TForm)
    panlTop: TPanel;
    SV: TSplitView;
    catMenuItems: TCategoryButtons;
    catSettings: TCategoryButtons;
    catMenuSettings: TCategoryButtons;
    catPanelSettings: TCategoryButtons;
    IconFontsImageList: TIconFontsVirtualImageList;
    ActionList: TActionList;
    actHome: TAction;
    actChangeTheme: TAction;
    actShowChildForm: TAction;
    actMenu: TAction;
    lblTitle: TLabel;
    splSplit: TSplitter;
    svSettings: TSplitView;
    IconFontsImageListColored: TIconFontsVirtualImageList;
    splSettings: TSplitter;
    actSettings: TAction;
    actViewOptions: TAction;
    pnlSettings: TPanel;
    pcSettings: TPageControl;
    tsStyle: TTabSheet;
    grpDisplayMode: TRadioGroup;
    grpCloseStyle: TRadioGroup;
    grpPlacement: TRadioGroup;
    tsAnimation: TTabSheet;
    tsLog: TTabSheet;
    lstLog: TListBox;
    actBack: TAction;
    actAnimate: TAction;
    actLog: TAction;
    PageControl: TPageControl;
    tsDatabase: TTabSheet;
    ClientDataSet: TClientDataSet;
    DBNavigator: TDBNavigator;
    DbGrid: TDBGrid;
    tsWindows10: TTabSheet;
    CalendarView: TCalendarView;
    CalendarPicker: TCalendarPicker;
    tsStandard: TTabSheet;
    Edit: TEdit;
    HomeButton: TButton;
    LogButton: TButton;
    CheckListBox: TCheckListBox;
    RichEdit: TRichEdit;
    SpinEdit: TSpinEdit;
    ComboBox: TComboBox;
    ListBox: TListBox;
    Memo: TMemo;
    ColorBox: TColorBox;
    MaskEdit: TMaskEdit;
    RadioButton: TRadioButton;
    RadioGroup: TRadioGroup;
    DBRichEdit: TDBRichEdit;
    Label1: TLabel;
    MenuButtonToolbar: TToolBar;
    MenuToolButton: TToolButton;
    ToolBar: TToolBar;
    HomeToolButton: TToolButton;
    ThemeToolButton: TToolButton;
    SettingsToolButton: TToolButton;
    DateTimePicker: TDateTimePicker;
    ClientDataSetSpeciesNo: TFloatField;
    ClientDataSetCategory: TStringField;
    ClientDataSetCommon_Name: TStringField;
    ClientDataSetSpeciesName: TStringField;
    ClientDataSetLengthcm: TFloatField;
    ClientDataSetLength_In: TFloatField;
    ClientDataSetNotes: TMemoField;
    ClientDataSetGraphic: TGraphicField;
    DataSource: TDataSource;
    tsFont: TTabSheet;
    Label2: TLabel;
    FontTrackBar: TTrackBar;
    acFont: TAction;
    FontComboBox: TComboBox;
    FontSizeLabel: TLabel;
    acApplyFont: TAction;
    SaveFontButton: TButton;
    tsIconFonts: TTabSheet;
    Label3: TLabel;
    IconFontsSizeLabel: TLabel;
    IconFontsTrackBar: TTrackBar;
    acIconFonts: TAction;
    paEdit: TPanel;
    Label4: TLabel;
    DBEdit1: TDBEdit;
    Label5: TLabel;
    DBEdit2: TDBEdit;
    Label6: TLabel;
    DBEdit3: TDBEdit;
    Label7: TLabel;
    DBEdit4: TDBEdit;
    Label8: TLabel;
    DBEdit5: TDBEdit;
    Label9: TLabel;
    DBEdit6: TDBEdit;
    Label10: TLabel;
    DBMemo1: TDBMemo;
    Label11: TLabel;
    DBImage: TDBImage;
    FileOpenDialog: TFileOpenDialog;
    ButtonEdit: TSearchBox;
    ButtonEditDate: TSearchBox;
    acErrorMessage: TAction;
    acWarningMessage: TAction;
    acInfoMessage: TAction;
    acConfirmMessage: TAction;
    acAbout: TAction;
    SVGIconImageList: TSVGIconVirtualImageList;
    IconsToggleSwitch: TToggleSwitch;
    tswAnimation: TToggleSwitch;
    lblAnimationDelay: TLabel;
    trkAnimationDelay: TTrackBar;
    lblAnimationStep: TLabel;
    trkAnimationStep: TTrackBar;
    tsvDisplayMode: TToggleSwitch;
    ttsCloseStyle: TToggleSwitch;
    ttsCloseSplitView: TToggleSwitch;
    acExit: TAction;
    SVGIconImage: TSVGIconImage;
    IconFontImage: TIconFontImage;
    IconFontImageLabel: TLabel;
    SVGIconImageLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure grpDisplayModeClick(Sender: TObject);
    procedure grpPlacementClick(Sender: TObject);
    procedure grpCloseStyleClick(Sender: TObject);
    procedure SVClosed(Sender: TObject);
    procedure SVOpened(Sender: TObject);
    procedure SVOpening(Sender: TObject);
    procedure trkAnimationDelayChange(Sender: TObject);
    procedure trkAnimationStepChange(Sender: TObject);
    procedure actHomeExecute(Sender: TObject);
    procedure actChangeThemeExecute(Sender: TObject);
    procedure actShowChildFormExecute(Sender: TObject);
    procedure actMenuExecute(Sender: TObject);
    procedure CatPreventCollapase(Sender: TObject;
      const Category: TButtonCategory);
    procedure SVResize(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure svSettingsClosed(Sender: TObject);
    procedure svSettingsOpened(Sender: TObject);
    procedure SVClosing(Sender: TObject);
    procedure tswAnimationClick(Sender: TObject);
    procedure actBackExecute(Sender: TObject);
    procedure actViewOptionsExecute(Sender: TObject);
    procedure actAnimateExecute(Sender: TObject);
    procedure actLogExecute(Sender: TObject);
    procedure svSettingsClosing(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tsvDisplayModeClick(Sender: TObject);
    procedure ttsCloseStyleClick(Sender: TObject);
    procedure FontTrackBarChange(Sender: TObject);
    procedure acFontExecute(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
    procedure FontComboBoxSelect(Sender: TObject);
    procedure IconFontsTrackBarChange(Sender: TObject);
    procedure acIconFontsExecute(Sender: TObject);
    procedure acApplyFontExecute(Sender: TObject);
    procedure acApplyFontUpdate(Sender: TObject);
    procedure DBImageDblClick(Sender: TObject);
    procedure ClientDataSetAfterPost(DataSet: TDataSet);
    procedure PageControlChange(Sender: TObject);
    procedure IconFontsImageListFontMissing(const AFontName: TFontName);
    procedure acMessageExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure IconsToggleSwitchClick(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acExitExecute(Sender: TObject);
    procedure IconImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormBeforeMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
  private
    FActiveFont: TFont;
    FActiveStyleName: string;
    {$IFDEF D10_2+}
    TimePicker: TTimePicker;
    DatePicker: TDatePicker;
    {$ENDIF}
    {$IFNDEF D10_3+}
    FScaleFactor: Single;
    {$ENDIF}
    {$IFDEF D10_4+}
    NumberBox: TNumberBox;
    {$ENDIF}
    procedure UpdateButtons;
    procedure CreateComponents;
    procedure FixFontComponents;
    procedure Log(const Msg: string);
    procedure AfterMenuClick;
    procedure ShowSettingPage(TabSheet: TTabSheet; AutoOpen: Boolean = False);
    procedure FontTrackBarUpdate;
    procedure IconFontsTrackBarUpdate;
    procedure SetActiveStyleName(const Value: string);
    procedure AdjustCatSettings;
    procedure UpdateDefaultAndSystemFonts;
    {$IFNDEF D10_4+}
    procedure FixSplitViewResize(ASplitView: TSplitView; const OldDPI, NewDPI: Integer);
    {$ENDIF}
    procedure UpdateIconsByType;
    function GetFontHeightAt96Dpi: Integer;
    procedure SetFontHeightAt96Dpi(const Value: Integer);
  protected
    procedure Loaded; override;
    {$IFNDEF D10_3+}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
  public
    procedure ScaleForPPI(NewPPI: Integer); override;
    destructor Destroy; override;
    property ActiveStyleName: string read FActiveStyleName write SetActiveStyleName;
    property FontHeightAt96Dpi: Integer read GetFontHeightAt96Dpi write SetFontHeightAt96Dpi;
  end;

var
  FormMain: TFormMain;

implementation

uses
  Vcl.Themes
  , System.UITypes
  , DemoAbout
  ;

{$R *.dfm}

procedure ApplyStyleElements(ARootControl: TControl;
  AStyleElements: TStyleElements);
var
  I: Integer;
  LControl: TControl;
begin
  ARootControl.StyleElements := AStyleElements;
  if ARootControl is TWinControl then
  For I := 0 to TWinControl(ARootControl).ControlCount -1 do
  begin
    LControl := TWinControl(ARootControl).Controls[I];
    ApplyStyleElements(TWinControl(LControl), AStyleElements);
  end;
  ARootControl.Invalidate;
end;

{ TFormMain }

procedure TFormMain.IconFontsImageListFontMissing(const AFontName: TFontName);
var
  LFontFileName: string;
begin
  inherited;
  //The "material design web-font is not installed into system: load and install now from disk
  LFontFileName := ExtractFilePath(Application.ExeName)+'..\Fonts\Material Design Icons Desktop.ttf';
  if FileExists(LFontFileName) then
  begin
    {$IFNDEF D2010+}
    AddFontResource(PChar(LFontFileName));
    {$ELSE}
    AddFontResource(PWideChar(LFontFileName));
    {$ENDIF}
    SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
  end
  else
  begin
    //If the font file is not available
    MessageDlg(Format('Warning: "%s" font is not present in your system!'+sLineBreak+
      'Please download from https://github.com/Templarian/MaterialDesign-Font and install it, because this demo is based on this font!',
        [AFontName]), mtError, [mbOK], 0);
  end;
end;

procedure TFormMain.FontTrackBarUpdate;
begin
  FontSizeLabel.Caption := IntToStr(FontTrackBar.Position);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  FontTrackBarUpdate;
  IconFontsTrackBarUpdate;
  UpdateButtons;
end;

function TFormMain.GetFontHeightAt96Dpi: Integer;
begin
  Result := MulDiv(-Font.Height, 96, Font.PixelsPerInch);
end;

procedure TFormMain.SetFontHeightAt96Dpi(const Value: Integer);
begin
  Font.Height := - MulDiv(Abs(Value), Font.PixelsPerInch, 96);
  FontTrackBar.Position := Abs(Value);
end;

procedure TFormMain.FixFontComponents;
begin
  CalendarView.ParentFont := True;
  CalendarView.HeaderInfo.Font.Assign(Font);
  CalendarView.HeaderInfo.Font.Height := Round(CalendarView.HeaderInfo.Font.Height*1.2);
  CalendarView.HeaderInfo.DaysOfWeekFont.Assign(Font);
  CalendarPicker.ParentFont := True;

{$IFDEF D10_2+}
  TimePicker.Font.Name := Font.Name;
  DatePicker.Font.Name := Font.Name;
{$ENDIF}
end;

procedure TFormMain.CreateComponents;
begin
{$IFDEF D10_2+}
  TimePicker := TTimePicker.Create(Self);
  TimePicker.Left := 2;
  TimePicker.Top := 367;
  TimePicker.Hint := 'TimePicker';
  TimePicker.Parent := tsWindows10;

  DatePicker := TDatePicker.Create(Self);
  DatePicker.Left := 2;
  DatePicker.Top := 423;
  DatePicker.Hint := 'DatePicker';
  DatePicker.Parent := tsWindows10;
{$ENDIF}

{$IFDEF D10_4+}
  NumberBox := TNumberBox.Create(Self);
  NumberBox.Left := 2;
  NumberBox.Width := 200;
  NumberBox.Top := 470;
  NumberBox.Hint := 'NumberBox';
  NumberBox.SpinButtonOptions.Placement := nbspInline;
  NumberBox.NegativeValueColor := clRed;
  NumberBox.parent := tsWindows10;
  NumberBox.ParentFont := True;
{$ENDIF}
end;

procedure TFormMain.FontComboBoxSelect(Sender: TObject);
begin
  Font.Name := FontComboBox.Text;
  FixFontComponents;
  UpdateDefaultAndSystemFonts;
end;

procedure TFormMain.FontTrackBarChange(Sender: TObject);
begin
  FontHeightAt96Dpi := FontTrackBar.Position;
  FontTrackBarUpdate;
  UpdateDefaultAndSystemFonts;
end;

procedure TFormMain.IconFontsTrackBarChange(Sender: TObject);
begin
  IconFontsImageList.Size := IconFontsTrackBar.Position;
  IconFontsImageListColored.Size := IconFontsTrackBar.Position;
  SVGIconImageList.Size := IconFontsTrackBar.Position;
  SV.CompactWidth := IconFontsTrackBar.Position * 2;
  SV.OpenedWidth := IconFontsTrackBar.Position * 8;
  IconFontsTrackBarUpdate;
end;

procedure TFormMain.IconFontsTrackBarUpdate;
var
  LContainerSize: Integer;

  procedure UpdateCategoryButtonSize(ACatButton: TCategoryButtons);
  begin
    ACatButton.ButtonHeight := LContainerSize;
    ACatButton.ButtonWidth := LContainerSize;
  end;

begin
  IconFontsTrackBar.Position := IconFontsImageList.Size;
  IconFontsSizeLabel.Caption := IntToStr(IconFontsTrackBar.Position);

  LContainerSize := IconFontsTrackBar.Position + 10;
  panlTop.Height := LContainerSize + 10;
  SV.Top := panlTop.Top+panlTop.Height;

  svSettings.Top := SV.Top;
  ToolBar.ButtonHeight := LContainerSize;
  MenuButtonToolbar.ButtonHeight := LContainerSize;
  UpdateCategoryButtonSize(catMenuItems);
  UpdateCategoryButtonSize(catMenuSettings);
  UpdateCategoryButtonSize(catSettings);
  UpdateCategoryButtonSize(catPanelSettings);
end;

procedure TFormMain.IconImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LIndex: Integer;
begin
  LIndex := IconFontImage.ImageIndex;
  if (Button = mbRight) and (Shift = [ssRight]) then
  begin
    if IconFontImage.ImageIndex = 0 then
      LIndex := IconFontsImageList.Count -1
    else
      Dec(LIndex);
  end
  else if (Button = mbLeft) and (Shift = [ssLeft]) then
  begin
    if IconFontImage.ImageIndex = IconFontsImageList.Count -1 then
      LIndex := 0
    else
      Inc(LIndex);
  end;
  IconFontImage.ImageIndex := LIndex;
  SVGIconImage.ImageIndex := LIndex;
end;

procedure TFormMain.IconsToggleSwitchClick(Sender: TObject);
begin
  if IconsToggleSwitch.State = tssOn then
    ImageCollectionDataModule.IconsType := itSVGIcons
  else
    ImageCollectionDataModule.IconsType := itIconFonts;
  UpdateIconsByType;
end;

{$IFNDEF D10_4+}
procedure TFormMain.FixSplitViewResize(ASplitView: TSplitView;
  const OldDPI, NewDPI: Integer);
begin
  ASplitView.CompactWidth := MulDiv(ASplitView.CompactWidth, NewDPI, OldDPI);
  ASplitView.OpenedWidth := MulDiv(ASplitView.OpenedWidth, NewDPI, OldDPI);
end;
{$ENDIF}

procedure TFormMain.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  LockWindowUpdate(0);
  {$IFNDEF D10_3}
  IconFontsImageList.DPIChanged(Self, OldDPI, NewDPI);
  IconFontsImageListColored.DPIChanged(Self, OldDPI, NewDPI);
  {$ENDIF}

  FontTrackBarUpdate;
  IconFontsTrackBarUpdate;

  {$IFNDEF D10_4+}
  AdjustCatSettings;

  //Fix for SplitViews
  FixSplitViewResize(SV, OldDPI, NewDPI);
  FixSplitViewResize(svSettings, OldDPI, NewDPI);
  {$ENDIF}

  UpdateDefaultAndSystemFonts;
end;

procedure TFormMain.FormBeforeMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  LockWindowUpdate(Handle);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Caption := Application.Title + ' - Copyright (c) Ethea S.r.l.';

  {$IFDEF STYLEDCOMPONENTS}
  (catMenuItems as TStyledCategoryButtons).StyleRoundedCorners :=
    [rcTopRight, rcBottomRight];
  {$ENDIF}

  //Hide Tabs
  for I := 0 to pcSettings.PageCount-1 do
    pcSettings.Pages[I].TabVisible := False;

  {$IFDEF D10_4+}
  //Assign Windows10 Style to Settings Menu
  svSettings.StyleName := 'Windows10';
  {$ELSE}
  //Disable any StyleElements of Settings Menu only for Windows10 Style
  if FActiveStyleName = 'Windows10' then
    ApplyStyleElements(svSettings, [seFont]);
  {$ENDIF}

  //Assign Fonts to Combobox
  FontComboBox.Items.Assign(Screen.Fonts);
  FontComboBox.ItemIndex := FontComboBox.Items.IndexOf(Font.Name);

  //Title label
  lblTitle.Caption := Application.Title;
  lblTitle.Font.Color := clHighlightText;
  lblTitle.Font.Height := lblTitle.Font.Height - 4;
  lblTitle.Font.Style := lblTitle.Font.Style + [fsBold];

  //Assign file for ClientDataSet
  {$IFDEF D10_2+}
  ClientDataSet.FileName := ExtractFilePath(Application.ExeName)+'..\Data\biolife_png.xml';
  {$ELSE}
  //Delphi 10.1 cannot load png blob field automatically
  ClientDataSet.FileName := ExtractFilePath(Application.ExeName)+'..\Data\biolife.xml';
  {$ENDIF}
end;

procedure TFormMain.CatPreventCollapase(Sender: TObject;
  const Category: TButtonCategory);
begin
  // Prevent the catMenuButton Category group from being collapsed
  (Sender as TCategoryButtons).Categories[0].Collapsed := False;
end;

{$IFNDEF D10_3+}
procedure TFormMain.ChangeScale(M, D: Integer);
begin
  inherited;
  FScaleFactor := FScaleFactor * M / D;
  IconFontsImageList.DPIChanged(Self, M, D);
  IconFontsImageListColored.DPIChanged(Self, M, D);
end;
{$ENDIF}

procedure TFormMain.ScaleForPPI(NewPPI: Integer);
begin
  inherited;
{$IFNDEF D10_3+}
  FScaleFactor := NewPPI / PixelsPerInch;
  IconFontsImageList.DPIChanged(Self, Self.PixelsPerInch, NewPPI);
  IconFontsImageListColored.DPIChanged(Self, Self.PixelsPerInch, NewPPI);
{$ENDIF}
{$IFNDEF D10_4+}
  FixSplitViewResize(SV, Self.PixelsPerInch, NewPPI);
  FixSplitViewResize(svSettings, Self.PixelsPerInch, NewPPI);
{$ENDIF}
end;

procedure TFormMain.ClientDataSetAfterPost(DataSet: TDataSet);
begin
  //Save content to File
  ClientDataSet.SaveToFile(ClientDataSet.FileName, dfXMLUTF8);
end;

procedure TFormMain.DBImageDblClick(Sender: TObject);
begin
  ClientDataSet.Edit;
  FileOpenDialog.DefaultFolder := ExtractFilePath(ClientDataSet.FileName);
  if FileOpenDialog.Execute then
    ClientDataSetGraphic.LoadFromFile(FileOpenDialog.FileName);
end;

destructor TFormMain.Destroy;
begin
  inherited;
  FActiveFont.Free;
end;

procedure TFormMain.grpDisplayModeClick(Sender: TObject);
begin
  SV.DisplayMode := TSplitViewDisplayMode(grpDisplayMode.ItemIndex);
end;

procedure TFormMain.grpCloseStyleClick(Sender: TObject);
begin
  SV.CloseStyle := TSplitViewCloseStyle(grpCloseStyle.ItemIndex);
end;

procedure TFormMain.grpPlacementClick(Sender: TObject);
begin
  SV.Placement := TSplitViewPlacement(grpPlacement.ItemIndex);
end;

procedure TFormMain.SetActiveStyleName(const Value: string);
var
  I: Integer;
  LFontColor, LColoredColor: TColor;
  LMaskColor: TColor;
begin
  if Value <> '' then
  begin
    try
      TStyleManager.SetStyle(Value);
    except
      WriteAppStyleToReg(COMPANY_NAME,
        ExtractFileName(Application.ExeName), 'Windows');
    end;
    WriteAppStyleToReg(COMPANY_NAME, ExtractFileName(Application.ExeName), Value);
    FActiveStyleName := Value;
    if FActiveStyleName = 'Windows' then
    begin
      catMenuItems.Color := clHighlight;
      catMenuItems.Font.Color := clHighlightText;
      for I := 0 to catMenuItems.Categories.Count -1 do
        catMenuItems.Categories[I].Color := clNone;

      DBImage.Color := clAqua;
      //Default non-style "Windows": use White icons over Highlight Color
      LFontColor := clWhite;
      LColoredColor := clBlack;
      LMaskColor := clHighlight;
    end
    else
    begin
      if FActiveStyleName = 'Windows10' then
      begin
        //For "Windows10" style: use "Windows 10 blue" color for the icons
        LFontColor := RGB(0, 120, 215);
        LColoredColor := LFontColor;
        LMaskColor := clBtnFace;
      end
      else
      begin
        //Request color from Style
        LFontColor := TStyleManager.ActiveStyle.GetStyleFontColor(sfButtonTextNormal);
        LMaskColor := TStyleManager.ActiveStyle.GetStyleFontColor(sfButtonTextDisabled);
        LColoredColor := clBlack;
      end;
      catMenuItems.Font.Color := IconFontsImageList.FontColor;
      //Color for Bitmap Background
      DBImage.Color := TStyleManager.ActiveStyle.GetStyleColor(scGenericBackground);
    end;
    //Update IconFonts Colors
    IconFontsImageList.UpdateIconsAttributes(LFontColor, LMaskColor, True);
    IconFontsImageListColored.UpdateIconsAttributes(LColoredColor, LMaskColor, False);
    IconFontImage.FontColor := LColoredColor;
    IconFontImage.MaskColor := LMaskColor;

    catMenuItems.BackgroundGradientDirection := gdVertical;
    catMenuItems.RegularButtonColor := clNone;
    catMenuItems.SelectedButtonColor := clNone;
  end;
end;

procedure TFormMain.UpdateIconsByType;
begin
  case ImageCollectionDataModule.IconsType of
    itIconFonts:
    begin
      MenuButtonToolbar.Images := IconFontsImageList;
      ToolBar.Images := IconFontsImageList;
      catMenuItems.Images := IconFontsImageList;
      catSettings.Images := IconFontsImageList;
      catMenuSettings.Images := IconFontsImageListColored; //Colored icons
      ActionList.Images := IconFontsImageList;
    end;
    itSVGIcons:
    begin
      MenuButtonToolbar.Images := SVGIconImageList;
      ToolBar.Images := SVGIconImageList;
      catMenuItems.Images := SVGIconImageList;
      catSettings.Images := SVGIconImageList;
      catMenuSettings.Images := SVGIconImageList;
      ActionList.Images := SVGIconImageList;
    end;
  end;
  UpdateButtons;
end;

procedure TFormMain.ShowSettingPage(TabSheet: TTabSheet;
  AutoOpen: Boolean = False);
begin
  if Assigned(TabSheet) then
  begin
    pcSettings.ActivePage := TabSheet;
    pnlSettings.Visible := True;
    catMenuSettings.Visible := False;
    actBack.Caption := Tabsheet.Caption;
    if AutoOpen then
      svSettings.Open;
  end
  else
  begin
    pnlSettings.Visible := False;
    catMenuSettings.Visible := True;
    actBack.Caption := 'Back';
  end;
end;

procedure TFormMain.SVClosed(Sender: TObject);
begin
  // When TSplitView is closed, adjust ButtonOptions and Width
  catMenuItems.ButtonOptions := catMenuItems.ButtonOptions - [boShowCaptions];
  actMenu.Hint := 'Expand';
  splSplit.Visible := False;
end;

procedure TFormMain.SVClosing(Sender: TObject);
begin
  SV.OpenedWidth := SV.Width;
end;

procedure TFormMain.SVOpened(Sender: TObject);
begin
  // When not animating, change size of catMenuItems when TSplitView is opened
  catMenuItems.ButtonOptions := catMenuItems.ButtonOptions + [boShowCaptions];
  actMenu.Hint := 'Collapse';
  splSplit.Visible := True;
  splSplit.Left := SV.Left + SV.Width;
end;

procedure TFormMain.SVOpening(Sender: TObject);
begin
  // When animating, change size of catMenuItems at the beginning of open
  catMenuItems.ButtonOptions := catMenuItems.ButtonOptions + [boShowCaptions];
end;

procedure TFormMain.AdjustCatSettings;
var
  LButtonWidth, LNumButtons, LButtonsFit: Integer;
  LCaptionOffset: Integer;
begin
  catSettings.Realign;
  LButtonWidth := catSettings.ButtonWidth;
  LNumButtons := catSettings.Categories[0].Items.Count;
  LButtonsFit := (SV.Width) div (LButtonWidth+2);
  LCaptionOffset := Round(-Font.Height+14);
  if (LButtonsFit <> 0) and (LButtonsFit < LNumButtons) then
    catSettings.Height := (((LNumButtons div LButtonsFit)+1) * catSettings.ButtonHeight) + LCaptionOffset
  else
  begin
    catSettings.Height := catSettings.ButtonHeight + LCaptionOffset;
  end;
end;

procedure TFormMain.SVResize(Sender: TObject);
begin
  AdjustCatSettings;
end;

procedure TFormMain.svSettingsClosed(Sender: TObject);
begin
  splSettings.Visible := False;
  ShowSettingPage(nil);
end;

procedure TFormMain.svSettingsClosing(Sender: TObject);
begin
  if svSettings.Width <> 0 then
    svSettings.OpenedWidth := svSettings.Width;
end;

procedure TFormMain.svSettingsOpened(Sender: TObject);
begin
  splSettings.Visible := True;
  splSettings.Left := svSettings.Left -1;
end;

procedure TFormMain.trkAnimationDelayChange(Sender: TObject);
begin
  SV.AnimationDelay := trkAnimationDelay.Position * 5;
  lblAnimationDelay.Caption := Format('Animation Delay (%d)', [SV.AnimationDelay]);
end;

procedure TFormMain.trkAnimationStepChange(Sender: TObject);
begin
  SV.AnimationStep := trkAnimationStep.Position * 5;
  lblAnimationStep.Caption := Format('Animation Step (%d)', [SV.AnimationStep]);
end;

procedure TFormMain.ttsCloseStyleClick(Sender: TObject);
begin
  if ttsCloseStyle.State = tssOff then
    SV.CloseStyle := svcCompact
  else
    SV.CloseStyle := svcCollapse;
end;

procedure TFormMain.tsvDisplayModeClick(Sender: TObject);
begin
  if tsvDisplayMode.State = tssOff then
    SV.DisplayMode := svmDocked
  else
    SV.DisplayMode := svmOverlay;
end;

procedure TFormMain.tswAnimationClick(Sender: TObject);
begin
  SV.UseAnimation := tswAnimation.State = tssOn;
  lblAnimationDelay.Enabled := SV.UseAnimation;
  trkAnimationDelay.Enabled := SV.UseAnimation;
  lblAnimationStep.Enabled := SV.UseAnimation;
  trkAnimationStep.Enabled := SV.UseAnimation;
end;

procedure TFormMain.acAboutExecute(Sender: TObject);
begin
  ShowAboutForm(Application.Title);
end;

procedure TFormMain.acApplyFontExecute(Sender: TObject);
begin
  FActiveFont.Name := FontComboBox.Text;
  FActiveFont.Height := -FontTrackBar.Position;
  WriteAppStyleAndFontToReg(COMPANY_NAME, ExtractFileName(Application.ExeName),
    FActiveStyleName, FActiveFont);

  //Update Child Forms
  if Assigned(FmEdit) then
    FmEdit.Font.Assign(Font);

  UpdateDefaultAndSystemFonts;

  AfterMenuClick;
end;

procedure TFormMain.acApplyFontUpdate(Sender: TObject);
begin
  acApplyFont.Enabled := (FActiveFont.Name <> FontComboBox.Text) or
    (FActiveFont.Height <> FontTrackBar.Position);
end;

procedure TFormMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.acMessageExecute(Sender: TObject);
begin
  if Sender = acErrorMessage then
    MessageDlg('Error Message...', mtError, [mbOK, mbHelp], 1000)
  else if Sender = acWarningMessage then
    MessageDlg('Warning Message...', mtWarning, [mbOK, mbHelp], 1000)
  else if Sender = acInfoMessage then
    MessageDlg('Information Message...', mtInformation, [mbOK, mbHelp], 1000)
  else if Sender = acConfirmMessage then
    MessageDlg('Do you want to confirm?', mtConfirmation, [mbYes, mbNo, mbHelp], 1000);
  Log((Sender as TAction).Caption + ' Clicked');
end;

procedure TFormMain.acFontExecute(Sender: TObject);
begin
  ShowSettingPage(tsFont);
end;

procedure TFormMain.acIconFontsExecute(Sender: TObject);
begin
  ShowSettingPage(tsIconFonts);
end;

procedure TFormMain.actAnimateExecute(Sender: TObject);
begin
  ShowSettingPage(tsAnimation);
end;

procedure TFormMain.actBackExecute(Sender: TObject);
begin
  ShowSettingPage(nil);
end;

procedure TFormMain.actHomeExecute(Sender: TObject);
begin
  PageControl.ActivePageIndex := 0;
  Log(actHome.Caption + ' Clicked');
  AfterMenuClick;
end;

procedure TFormMain.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actHome.Enabled := not svSettings.Opened;
end;

procedure TFormMain.actChangeThemeExecute(Sender: TObject);
begin
  //Show Theme selector
  if ShowVCLThemeSelector(FActiveStyleName,
    False, 3, 4) then
    ActiveStyleName := FActiveStyleName;
end;

procedure TFormMain.actLogExecute(Sender: TObject);
begin
  if not svSettings.Opened then
    svSettings.Open;
  ShowSettingPage(tsLog);
end;

procedure TFormMain.actMenuExecute(Sender: TObject);
begin
  if SV.Opened then
    SV.Close
  else
    SV.Open;
end;

procedure TFormMain.actShowChildFormExecute(Sender: TObject);
begin
  if not Assigned(FmEdit) then
    FmEdit := TFmEdit.Create(Self);
  FmEdit.Show;
  Log(actShowChildForm.Caption + ' Clicked');
  AfterMenuClick;
end;

procedure TFormMain.actSettingsExecute(Sender: TObject);
begin
  if svSettings.Opened then
    svSettings.Close
  else
    svSettings.Open;
end;

procedure TFormMain.actViewOptionsExecute(Sender: TObject);
begin
  ShowSettingPage(tsStyle);
end;

procedure TFormMain.AfterMenuClick;
begin
  if SV.Opened and (ttsCloseSplitView.State = tssOn) then
    SV.Close;
  if svSettings.Opened and (ttsCloseSplitView.State = tssOn) then
    svSettings.Close;
end;

procedure TFormMain.UpdateButtons;
begin
  //Buttons with Actions must be reassigned to refresh Icon
  HomeButton.Images := ActionList.Images;
  LogButton.Images := ActionList.Images;
  SaveFontButton.Images := ActionList.Images;
  //HomeButton.Action := actHome;
  //LogButton.Action := actLog;
  //SaveFontButton.Action := acApplyFont;
end;

procedure TFormMain.UpdateDefaultAndSystemFonts;
var
  LHeight: Integer;
begin
  //Update Application.DefaultFont for Childforms with ParentFont = True
  Application.DefaultFont.Assign(Font);
  //Update system fonts as user preferences
  LHeight := Muldiv(Font.Height, Screen.PixelsPerInch, Monitor.PixelsPerInch);
  Screen.IconFont.Name := Font.Name;
  Screen.IconFont.Height := LHeight;
  Screen.MenuFont.Name := Font.Name;
  Screen.MenuFont.Height := LHeight;
  Screen.MessageFont.Name := Font.Name;
  Screen.MessageFont.Height := LHeight;
  Screen.HintFont.Name := Font.Name;
  Screen.HintFont.Height := LHeight;
  Screen.CaptionFont.Name := Font.Name;
  Screen.CaptionFont.Height := LHeight;

  catMenuItems.Font.Assign(Font);
end;

procedure TFormMain.Loaded;
begin
  {$IFNDEF D10_3+}
  FScaleFactor := 1;
  {$ENDIF}

  FActiveFont := TFont.Create;
  //Acquire system font and size (eg. for windows 10 Segoe UI and 14 at 96 DPI)
  //but without using Assign!
  Font.Name := Screen.IconFont.Name;
  //If you want to use system font Height:
  Font.Height := Muldiv(Screen.IconFont.Height, 96, Screen.IconFont.PixelsPerInch);
  FActiveFont.Assign(Font);

  inherited; //Cause changing Font Height

  //Check for Font stored into Registry (user preferences)
  ReadAppStyleAndFontFromReg(COMPANY_NAME,
    ExtractFileName(Application.ExeName), FActiveStyleName, FActiveFont);
  //Create and Fix components for ParentFont
  CreateComponents;
  FixFontComponents;

  //Reassign Font values reads from Registry (stored as 96 Dpi)
  FontHeightAt96Dpi := - FActiveFont.Height;

  //For ParentFont on Child Forms
  UpdateDefaultAndSystemFonts;

  //GUI default
  ActiveStyleName := FActiveStyleName;
  svSettings.Opened := False;
  PageControl.ActivePageIndex := 0;
end;

procedure TFormMain.Log(const Msg: string);
var
  Idx: Integer;
begin
  Idx := lstLog.Items.Add(Msg);
  lstLog.TopIndex := Idx;
  ShowSettingPage(tsLog, True);
end;

procedure TFormMain.PageControlChange(Sender: TObject);
begin
  if (PageControl.ActivePage = tsDatabase) then
  begin
    if not ClientDataSet.Active then
    begin
      Screen.Cursor := crHourGlass;
      try
        ClientDataSet.Open;
        ClientDataSet.LogChanges := False;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

end.
