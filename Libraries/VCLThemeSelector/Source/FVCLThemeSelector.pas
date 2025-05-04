{******************************************************************************}
{                                                                              }
{       VCLThemeSelector: Form for Preview and Selection of VCL Style          }
{                                                                              }
{       Copyright (c) 2020-2024 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributor: Nicola Tambascia                                          }
{                                                                              }
{       https://github.com/EtheaDev/VCLThemeSelector                           }
{                                                                              }
{******************************************************************************}
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

unit FVCLThemeSelector;

interface

uses
  Winapi.Windows
  , System.Actions
  , System.Classes
  , Vcl.ActnList
  , Vcl.ExtCtrls
  , Vcl.Forms
  , Vcl.StdCtrls
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.DBCtrls;

const
  VCLThemeSelectorVersion = '1.3.2';
  DEFAULT_MAXROWS = 3;
  DEFAULT_MAXCOLUMNS = 4;

resourcestring
{$IFDEF LANGUAGE_ITA}
  SELECT_THEME = 'Seleziona il tema chiaro o scuro';
  APPLY_THEME = 'Applica';
  CANCEL_THEME = 'Annulla';
  LIGHT_THEMES = 'Temi chiari';
  DARK_THEMES = 'Temi scuri';

  PREVIEW_THEME = 'Anteprima';
  THEME_SELECTED = 'Nuovo tema selezionato: %s';
  THEME_PREVIEW_VALUES =
    'File'+sLineBreak+
    'Modifica'+sLineBreak+
    'Visualizza'+sLineBreak+
    'Help'+sLineBreak+
    'Editor di testo'+sLineBreak+
    'Normale'+sLineBreak+
    'Caldo'+sLineBreak+
    'Premuto'+sLineBreak+
    'Disabilitato'+sLineBreak+
    'Obbligatorio'+sLineBreak+
    'Sola lettura'+sLineBreak+
    'Spunta'+sLineBreak+
    'Pag. 1'+sLineBreak+
    'Pag. 2'+sLineBreak+
    'Pag. 3'+sLineBreak;
{$ELSE}
  SELECT_THEME = 'Select Light or Dark theme';
  APPLY_THEME = 'Apply';
  CANCEL_THEME = 'Cancel';
  LIGHT_THEMES = 'Light themes';
  DARK_THEMES = 'Dark themes';

  PREVIEW_THEME = 'Preview';
  THEME_SELECTED = 'New Selected theme: %s';
  THEME_PREVIEW_VALUES =
    'File'+sLineBreak+
    'Edit'+sLineBreak+
    'View'+sLineBreak+
    'Help'+sLineBreak+
    'Text editor'+sLineBreak+
    'Normal'+sLineBreak+
    'Hot'+sLineBreak+
    'Pressed'+sLineBreak+
    'Disabled'+sLineBreak+
    'Required'+sLineBreak+
    'Readonly'+sLineBreak+
    'Check'+sLineBreak+
    'Page 1'+sLineBreak+
    'Page 2'+sLineBreak+
    'Page 3'+sLineBreak;
{$ENDIF}

type
  TVCLThemeSelectorForm = class(TForm)
    LeftScrollBox: TScrollBox;
    paButtons: TPanel;
    ActionListAppereance: TActionList;
    acApplyStyle: TAction;
    acCancel: TAction;
    LeftFlowPanel: TFlowPanel;
    StyleLabel: TPanel;
    paRight: TPanel;
    btApply: TButton;
    btCancel: TButton;
    RightScrollBox: TScrollBox;
    RightFlowPanel: TFlowPanel;
    TopPanel: TPanel;
    LightPanel: TPanel;
    DarkPanel: TPanel;
    procedure SelectionClick(Sender: TObject);
    procedure acApplyStyleExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure acApplyStyleUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
  private
    FPreviousStyleName : string;
    FStyleName : string;
    FExcludeWindows: Boolean;
    FMaxRows: Integer;
    FMaxColumns: Integer;
    procedure UpdateLabel;
    procedure UpdateButtons;
    procedure BuildPreviewPanels;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreatePreview(AOwner: TComponent;
      const AExcludeWindows: Boolean;
      const AMaxRows: Integer = DEFAULT_MAXROWS;
      const AMaxColumns: Integer = DEFAULT_MAXCOLUMNS);
  end;

  //Class to register Theme attributes (like dark or light)
  TThemeType = (ttLight, ttDark);
  TThemeAttribute = class
    StyleName: String;
    ThemeType: TThemeType;
    EditRequiredColor: TColor;
    EditReadonlyColor: TColor;
  end;

//function to get Theme Attributes
function GetStyleAttributes(const AStyleName: string;
  out AThemeAttribute: TThemeAttribute): Boolean;

//function to launch form
function ShowVCLThemeSelector(var AStyleName: string;
  const AExcludeWindows: Boolean = False;
  const AMaxRows: Integer = DEFAULT_MAXROWS;
  const AMaxColumns: Integer = DEFAULT_MAXCOLUMNS): boolean;

//Utilities to read/write application preferences from/to Registry
function ReadAppStyleFromReg(const CompanyName, ApplicationName: string) : string;
procedure WriteAppStyleToReg(const CompanyName, ApplicationName, AppStyle : string);
procedure ReadAppStyleAndFontFromReg(const CompanyName, ApplicationName: string;
  out AAppStyle: string; const AFont: TFont);
procedure WriteAppStyleAndFontToReg(const CompanyName, ApplicationName: string;
  const AAppStyle: string; const AFont: TFont);
procedure SetEditorStyleAttributes(const AEditControl: TDBEdit);
procedure SetRequiredEditStyleAttributes(const AEdit: TCustomEdit);

//To add new styles used from your application that are not managed by default
procedure RegisterThemeAttributes(const AVCLStyleName: string;
  const AThemeType: TThemeType; const AEditRequiredColor: TColor;
  const AEditReadonlyColor: TColor);

implementation

{$R *.dfm}

uses
  Vcl.Themes
  , System.TypInfo
  {$IF CompilerVersion > 33}
  , CBVCLStylePreviewForm
  {$IFEND}
  , CBVCLStylePreview
  , Winapi.Messages
  , System.UITypes
  , System.SysUtils
  , System.Win.Registry
  , System.Math
  , System.Generics.Collections;

var
  ThemeAttributes: TList<TThemeAttribute>;

function GetStyleAttributes(const AStyleName: string;
  out AThemeAttribute: TThemeAttribute): Boolean;
var
  LThemeAttribute: TThemeAttribute;
begin
  for LThemeAttribute in ThemeAttributes do
  begin
    if SameText(AStyleName, LThemeAttribute.StyleName) then
    begin
      AThemeAttribute := LThemeAttribute;
      Exit(True);
    end;
  end;
  AThemeAttribute := nil;
  raise Exception.CreateFmt('Attributes for Style "%s" not found!'+sLineBreak+
    'please call RegisterThemeAttributes in an initialization section to add your custom style',[AStyleName]);
end;

procedure FreeThemesAttributes;
var
  LThemeAttribute: TThemeAttribute;
begin
  for LThemeAttribute in ThemeAttributes do
    LThemeAttribute.Free;
  ThemeAttributes.Free;
end;

procedure RegisterThemeAttributes(
  const AVCLStyleName: string;
  const AThemeType: TThemeType;
  const AEditRequiredColor: TColor;
  const AEditReadonlyColor: TColor);
var
  LThemeAttribute: TThemeAttribute;

  procedure UpdateThemeAttributes;
  begin
    LThemeAttribute.StyleName := AVCLStyleName;
    LThemeAttribute.ThemeType := AThemeType;
    LThemeAttribute.EditRequiredColor := StyleServices.GetSystemColor(AEditRequiredColor);
    LThemeAttribute.EditReadonlyColor := StyleServices.GetSystemColor(AEditReadonlyColor);
  end;

begin
  for LThemeAttribute in ThemeAttributes do
  begin
    if SameText(LThemeAttribute.StyleName, AVCLStyleName) then
    begin
      UpdateThemeAttributes;
      Exit; //Found: exit
    end;
  end;
  //not found
  LThemeAttribute := TThemeAttribute.Create;
  ThemeAttributes.Add(LThemeAttribute);
  UpdateThemeAttributes;
end;

procedure InitDefaultThemesAttributes;
begin
  ThemeAttributes := TList<TThemeAttribute>.Create;

  if StyleServices.Enabled then
  begin
    //Non themed Windows Style
    RegisterThemeAttributes('Windows'                 ,ttLight, clInfoBk           , clWebLightgrey);

    //High-DPI Themes (from Delphi 10.4)
    RegisterThemeAttributes('Aqua Light Slate'        ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Copper'                  ,ttLight, clWebLightCoral    , clWebLightgrey);
    RegisterThemeAttributes('CopperDark'              ,ttDark , clWebDarkSlategray , clDkGray      );
    RegisterThemeAttributes('Coral'                   ,ttLight, clWebLightCoral    , clWebLightgrey);
    RegisterThemeAttributes('Diamond'                 ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Emerald'                 ,ttLight, clWebLightGreen    , clWebLightgrey);
    RegisterThemeAttributes('Glow'                    ,ttDark , clWebDarkSlategray , clWebDarkGray );
    RegisterThemeAttributes('Iceberg Classico'        ,ttLight, clWebLightSkyBlue  , clWebLightgrey);
    RegisterThemeAttributes('Lavender Classico'       ,ttLight, clWebLightSteelBlue, clWebLightgrey);
    RegisterThemeAttributes('Sky'                     ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Slate Classico'          ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Sterling'                ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('TabletDark'              ,ttDark , clWebDarkSlategray , clWebDarkGray );
    RegisterThemeAttributes('Tablet Dark'             ,ttDark , clWebDarkSlategray , clWebDarkGray );
    RegisterThemeAttributes('Tablet Light'            ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Windows10'               ,ttLight, clWebLightYellow   , clWebAliceBlue);
    RegisterThemeAttributes('Windows10 Blue'          ,ttDark,  clWebLightSkyBlue  , clWebLightgrey);
    RegisterThemeAttributes('Windows10 Dark'          ,ttDark,  clWebDarkBlue      , clWebDarkGray );
    RegisterThemeAttributes('Windows10 Green'         ,ttDark,  clWebLightGreen    , clWebLightgrey);
    RegisterThemeAttributes('Windows10 Purple'        ,ttDark,  clWebLightPink     , clWebLightgrey);
    RegisterThemeAttributes('Windows10 SlateGray'     ,ttDark,  clWebDarkSlategray , clDkGray      );
    RegisterThemeAttributes('Glossy'                  ,ttDark,  clWebDarkSlategray , clDkGray      );
    RegisterThemeAttributes('Windows10 BlackPearl'    ,ttDark,  clWebFirebrick     , clDkGray      );
    RegisterThemeAttributes('Windows10 Blue Whale'    ,ttDark,  clWebDarkSlategray , clDkGray      );
    RegisterThemeAttributes('Windows10 Clear Day'     ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Windows10 Malibu'        ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Windows10 Charcoal'      ,ttDark,  clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Flat UI Light'           ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Windows11 Modern Light'  ,ttLight, clWebLightYellow   , clWebAliceBlue);
    RegisterThemeAttributes('Windows11 Modern Dark'   ,ttDark,  clWebDarkBlue      , clWebDarkGray );
    RegisterThemeAttributes('Windows11 Impressive Dark',ttDark,  clWebDarkBlue      , clWebDarkGray );
    RegisterThemeAttributes('Windows11 Impressive Dark SE',ttDark,  clWebDarkBlue      , clWebDarkGray );
    RegisterThemeAttributes('Windows11 Impressive Light',ttLight, clWebLightYellow   , clWebAliceBlue);
    RegisterThemeAttributes('Windows11 Impressive Light SE',ttLight, clWebLightYellow   , clWebAliceBlue);
    RegisterThemeAttributes('Windows11 MineShaft'     ,ttDark,  clWebDarkBlue      , clWebDarkGray );
    RegisterThemeAttributes('Windows11 Polar Dark'    ,ttDark,  clWebDarkBlue      , clWebDarkGray );
    RegisterThemeAttributes('Windows11 Polar Light'   ,ttLight, clWebLightYellow   , clWebAliceBlue);
    RegisterThemeAttributes('Windows11 White Smoke'   ,ttLight, clWebLightYellow   , clWebAliceBlue);
    RegisterThemeAttributes('Material Oxford Blue'    ,ttDark,  clWebLightSkyBlue  , clWebLightgrey);
    RegisterThemeAttributes('Material Oxford Blue SE' ,ttDark,  clWebDarkBlue      , clWebDarkGray );
    RegisterThemeAttributes('Puerto Rico'             ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Wedgewood Light'         ,ttLight, clWebLightYellow   , clWebAliceBlue);
    RegisterThemeAttributes('Calypso'                 ,ttDark,  clWebLightSkyBlue  , clWebAqua     );
    RegisterThemeAttributes('Calypso SE'              ,ttDark,  clWebLightSkyBlue  , clWebAqua     );
    RegisterThemeAttributes('Calypso LE'              ,ttDark,  clWebLightSkyBlue  , clWebAqua     );
    RegisterThemeAttributes('Calypso SLE'             ,ttDark,  clWebLightSkyBlue  , clWebAqua     );
    RegisterThemeAttributes('Jet'                     ,ttDark,  clWebDarkSlategray , clDkGray      );;
    RegisterThemeAttributes('Lucky Point'             ,ttDark,  clWebDarkBlue      , clWebDarkGray );
    RegisterThemeAttributes('Material Pattens Blue'   ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Stellar'                 ,ttDark,  clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Stellar Dark'            ,ttDark , clWebDarkBlue      , clWebDarkGray );
    RegisterThemeAttributes('Vapor'                   ,ttDark , clWebDarkSlategray , clWebDarkGray );
    RegisterThemeAttributes('Zircon'                  ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Zircon SE'               ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Radiant'                 ,ttLight, clWebLightGreen    , clWebLightgrey);

    //Non High DPI Themes
    RegisterThemeAttributes('Aqua Graphite'           ,ttDark , clWebDarkSlategray , clDkGray      );
    RegisterThemeAttributes('Cobalt XEMedia'          ,ttDark , clWebDarkBlue      , clWebDarkGray );
    RegisterThemeAttributes('Emerald Light Slate'     ,ttLight, clWebLightGreen    , clWebLightgrey);
    RegisterThemeAttributes('Golden Graphite'         ,ttDark , clWebDarkOrange    , clWebDarkSalmon);
    RegisterThemeAttributes('Cyan Night'              ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Light'                   ,ttLight, clWebLightSteelBlue, clWebLightgrey);
    RegisterThemeAttributes('Obsidian'                ,ttDark , clWebYellow        , clWebDarkKhaki);
    RegisterThemeAttributes('Silver'                  ,ttLight, clWebYellow        , clWebDarkKhaki);
    RegisterThemeAttributes('Amakrits'                ,ttDark , clWebDarkSlategray , clDkGray      );
    RegisterThemeAttributes('Amethyst Kamri'          ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Auric'                   ,ttDark , clWebDarkSlategray , clDkGray      );
    RegisterThemeAttributes('Carbon'                  ,ttDark , clWebDarkSlategray , clDkGray      );
    RegisterThemeAttributes('Cyan Dusk'               ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Charcoal Dark Slate'     ,ttDark , clWebDarkSlategray , clDkGray      );
    RegisterThemeAttributes('Luna'                    ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Material Oxford Blue'    ,ttDark,  clWebDarkSlategray , clDkGray      );
    RegisterThemeAttributes('Onyx Blue'               ,ttDark , clWebDarkSlategray , clDkGray      );
    RegisterThemeAttributes('Ruby Graphite'           ,ttDark , clWebDarkRed       , clDkGray      );
    RegisterThemeAttributes('Sapphire Kamri'          ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Smokey Quartz Kamri'     ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Turquoise Gray'          ,ttLight, clWebLightYellow   , clWebLightgrey);
    RegisterThemeAttributes('Windows10 Blue Whale LE' ,ttDark,  clWebLightYellow   , clDkGray      );
    RegisterThemeAttributes('Metropolis UI Black'     ,ttDark,  clWebLightYellow   , clDkGray      );
    RegisterThemeAttributes('Metropolis UI Blue'      ,ttDark , clWebDarkSlategray , clDkGray      );
    RegisterThemeAttributes('Metropolis UI Dark'      ,ttDark,  clWebLightYellow   , clDkGray      );
    RegisterThemeAttributes('Metropolis UI Green'     ,ttDark,  clWebLightGreen    , clWebLightgrey);
  end;
end;

procedure ReadAppStyleAndFontFromReg(const CompanyName, ApplicationName: string;
  out AAppStyle: string; const AFont: TFont);
var
  FRegistry : TRegistry;
  RegistryKey : string;
begin
  FRegistry := TRegistry.Create(KEY_ALL_ACCESS);
  try
    FRegistry.RootKey := HKEY_CURRENT_USER;
    RegistryKey := Format('\Software\%s\%s',[CompanyName, ApplicationName]);
    FRegistry.OpenKey(RegistryKey, True);
    //Read Application Style
    AAppStyle := FRegistry.ReadString('AppStyle');
    if AAppStyle = '' then
      AAppStyle := 'Windows';
    //Read font attributes
    if Assigned(AFont) then
    begin
      if FRegistry.ValueExists('FontName') then
        AFont.Name := FRegistry.ReadString('FontName');
      if FRegistry.ValueExists('FontHeight') then
        AFont.Height := FRegistry.ReadInteger('FontHeight');
      if FRegistry.ValueExists('FontColor') then
        AFont.Color := TColor(FRegistry.ReadInteger('FontColor'));
    end;
  finally
    FRegistry.Free;
  end;
end;

procedure WriteAppStyleAndFontToReg(const CompanyName, ApplicationName: string;
  const AAppStyle: string; const AFont: TFont);
var
  FRegistry : TRegistry;
  RegistryKey : string;
begin
  FRegistry := TRegistry.Create(KEY_ALL_ACCESS);
  try
    RegistryKey := Format('\Software\%s\%s',[CompanyName, ApplicationName]);
    FRegistry.RootKey := HKEY_CURRENT_USER;
    FRegistry.OpenKey(RegistryKey, True);
    FRegistry.WriteString('AppStyle',AAppStyle);
    if Assigned(AFont) then
    begin
      FRegistry.WriteString('FontName',AFont.Name);
      FRegistry.WriteInteger('FontHeight',AFont.Height);
      FRegistry.WriteInteger('FontColor',AFont.Color);
      FRegistry.WriteBool('FontBold',fsBold in AFont.Style);
      FRegistry.WriteBool('FontItalic',fsItalic in AFont.Style);
      FRegistry.WriteBool('FontUnderline',fsUnderline in AFont.Style);
      FRegistry.WriteBool('FontStrikeOut',fsStrikeOut in AFont.Style);
    end;
  finally
    FRegistry.Free;
  end;
end;

function ReadAppStyleFromReg(const CompanyName, ApplicationName: string) : string;
var
  LFont: TFont;
begin
  LFont := nil;
  ReadAppStyleAndFontFromReg(CompanyName, ApplicationName, Result, LFont);
end;

procedure WriteAppStyleToReg(const CompanyName, ApplicationName, AppStyle: string);
begin
  WriteAppStyleAndFontToReg(CompanyName, ApplicationName, AppStyle, nil);
end;

function ShowVCLThemeSelector(var AStyleName: string;
  const AExcludeWindows: Boolean = False;
  const AMaxRows: Integer = DEFAULT_MAXROWS;
  const AMaxColumns: Integer = DEFAULT_MAXCOLUMNS): boolean;
var
  fmVCLStyleSelector: TVCLThemeSelectorForm;
begin
  Screen.Cursor := crHourGlass;
  Try
    fmVCLStyleSelector := TVCLThemeSelectorForm.CreatePreview(nil,
      AExcludeWindows, AMaxRows, AMaxColumns);
    try
      Screen.Cursor := crDefault;
      Result := (fmVCLStyleSelector.ShowModal = mrOk);
      if Result then
        AStyleName := fmVCLStyleSelector.FStyleName;
    finally
      fmVCLStyleSelector.Free;
    end;
  Finally
    Screen.Cursor := crDefault;
  End;
end;

procedure SetEditorStyleAttributes(const AEditControl: TDBEdit);
var
  LActiveStyleName: string;
  LThemeAttribute: TThemeAttribute;
  LDetails: TThemedElementDetails;
  LFontColor: TColor;
begin
  AEditControl.StyleElements := [seBorder];

  //Retrieve info from my LThemeAttribute structure
  LActiveStyleName := TStyleManager.ActiveStyle.Name;
  if GetStyleAttributes(LActiveStyleName, LThemeAttribute) then
  begin
    if AEditControl.ReadOnly then
      AEditControl.Color := LThemeAttribute.EditReadonlyColor
    else
      AEditControl.Color := LThemeAttribute.EditRequiredColor;
  end
  else
  begin
    AEditControl.Color := clWindow;
  end;

  if StyleServices.Enabled then
  begin
    LDetails := StyleServices.GetElementDetails(teEditTextNormal);
    StyleServices.GetElementColor(LDetails, ecTextColor, LFontColor);
  end
  else
    LFontColor := clDefault;

  AEditControl.Font.Color := LFontColor;
end;

procedure SetRequiredEditStyleAttributes(const AEdit: TCustomEdit);
var
  LActiveStyleName: string;
  LThemeAttribute: TThemeAttribute;
  LDetails: TThemedElementDetails;
  LFontColor: TColor;
  LFont: TFont;
  LReadOnly: Boolean;
begin
  AEdit.StyleElements := [seBorder];

  //Retrieve info for Style from my structure LThemeAttribute
  LActiveStyleName := TStyleManager.ActiveStyle.Name;
  if IsPublishedProp(AEdit, 'Color') then
  begin
    if GetStyleAttributes(LActiveStyleName, LThemeAttribute) then
    begin
      if IsPublishedProp(AEdit, 'ReadOnly') then
        LReadOnly := Boolean(GetOrdProp(AEdit, 'ReadOnly'))
      else
        LReadOnly := AEdit.ReadOnly;

      //Set the Edit Component Color using RTTI
      if LReadOnly then
        SetOrdProp(AEdit, 'Color', LThemeAttribute.EditReadonlyColor)
      else
        SetOrdProp(AEdit, 'Color', LThemeAttribute.EditRequiredColor);
    end
    else
      SetOrdProp(AEdit, 'Color', clWindow);
  end;

  if StyleServices.Enabled and (LActiveStyleName <> 'Windows') then
  begin
    LDetails := StyleServices.GetElementDetails(teEditTextNormal);
    StyleServices.GetElementColor(LDetails, ecTextColor, LFontColor);
  end
  else
    LFontColor := clWindowText;

  if IsPublishedProp(AEdit, 'Font') then
  begin
    LFont := GetObjectProp(AEdit, 'Font', TFont) as TFont;
    LFont.Color := LFontColor;
  end;
end;

{ TVCLThemeSelectorForm }

procedure TVCLThemeSelectorForm.acApplyStyleExecute(Sender: TObject);
begin
  inherited;
  ModalResult := mrOk;
end;

procedure TVCLThemeSelectorForm.acApplyStyleUpdate(Sender: TObject);
begin
  inherited;
  acApplyStyle.Enabled := (FPreviousStyleName <> FStyleName);
end;

procedure TVCLThemeSelectorForm.acCancelExecute(Sender: TObject);
begin
  inherited;
  ModalResult := mrCancel;
end;

constructor TVCLThemeSelectorForm.Create(AOwner: TComponent);
begin
  inherited;
  FMaxRows := DEFAULT_MAXROWS;
  FMaxColumns := DEFAULT_MAXCOLUMNS;
end;

constructor TVCLThemeSelectorForm.CreatePreview(
  AOwner: TComponent; const AExcludeWindows: Boolean;
  const AMaxRows: Integer = DEFAULT_MAXROWS;
  const AMaxColumns: Integer = DEFAULT_MAXCOLUMNS);
begin
  FExcludeWindows := AExcludeWindows;
  FMaxRows := AMaxRows;
  FMaxColumns := AMaxColumns;
  inherited Create(AOwner);
end;

procedure TVCLThemeSelectorForm.BuildPreviewPanels;
var
  i : Integer;
  LStyleName, LActiveStyleName: string;
  LStyleNames: TStringList;
  LpnPreview: TPanel;
  LpnButton: TButton;
  {$IF CompilerVersion >= 34}
  LVCLPreviewForm: TCBVCLPreviewForm;
  {$IFEND}
  LVCLPreview: TCBVclStylesPreview;
  LCountStyle, LCountLight, LCountDark: Integer;
  LNumRows : integer;
  LCalcHeight, LCalcWidth : Integer;
  LHeight, LWidth : Integer;
  LMonitor: TMonitor;
  LMonitorMargin: Integer;
  LScrollBarWidth: Integer;
  LThemeAttribute: TThemeAttribute;
  LIsLight: Boolean;
const
  MARGIN = 4;
begin
  LActiveStyleName := TStyleManager.ActiveStyle.Name;
  LStyleNames := TStringList.Create;
  LCountStyle := 0;
  LCountLight := 0;
  LCountDark := 0;
  LpnPreview := nil;
  try
    for i := 0 to High(TStyleManager.StyleNames) do
      LStyleNames.Add(TStyleManager.StyleNames[i]);
    LStyleNames.Sort;
    for i := 0 to LStyleNames.Count -1 do
    begin
      LStyleName := LStyleNames.Strings[i];

      GetStyleAttributes(LStyleName, LThemeAttribute);
      if Assigned(LThemeAttribute) then
        LIsLight := LThemeAttribute.ThemeType = ttLight
      else
        LIsLight := True;
      if LIsLight then
      begin
        LpnPreview := TPanel.Create(LeftFlowPanel);
        Inc(LCountLight);
      end
      else
      begin
        LpnPreview := TPanel.Create(RightFlowPanel);
        Inc(LCountDark);
      end;
      Inc(LCountStyle);

      //Jump Windows Style if requested
      if FExcludeWindows and (LStyleName = 'Windows') then
        Continue;

      //First assign size
      LpnPreview.Height := PREVIEW_HEIGHT;
      LpnPreview.Width := PREVIEW_WIDTH;
      LpnPreview.Margins.Left := MARGIN;
      LpnPreview.Margins.Top := MARGIN;
      LpnPreview.Margins.Bottom := MARGIN;
      LpnPreview.Margins.Right := MARGIN;
      //Then parent the control, so it is scaled correctly
      if LIsLight then
        LpnPreview.Parent := LeftFlowPanel
      else
        LpnPreview.Parent := RightFlowPanel;
      LpnPreview.Align := alLeft;
      LpnPreview.AlignWithMargins := True;

      LpnButton := TButton.Create(LpnPreview);
      LpnButton.Parent := LpnPreview;
      LpnButton.Align := alTop;
      LpnButton.OnClick := SelectionClick;
      LpnButton.Caption :=  LStyleName;
      LpnButton.Cursor := crHandPoint;

      {$IF CompilerVersion >= 34}
      if TStyleManager.ActiveStyle.Name = 'Windows' then
      begin
        //If the application Style is "Windows" cannot use per-control styles
        LVCLPreviewForm := nil;
        LVCLPreview := TCBVclStylesPreview.Create(LpnPreview);
      end
      else
      begin
        //Use per-control styles
        LVCLPreview := nil;
        LVCLPreviewForm := TCBVCLPreviewForm.Create(LpnPreview);
        LVCLPreviewForm.Caption := PREVIEW_THEME;
        LVCLPreviewForm.SetCaptions(THEME_PREVIEW_VALUES);
        LVCLPreviewForm.CustomStyle := TStyleManager.Style[LStyleName];
        if Assigned(LThemeAttribute) then
        begin
          LVCLPreviewForm.FRequiredTextEdit.StyleElements := [seBorder];
          LVCLPreviewForm.FRequiredTextEdit.Color := LThemeAttribute.EditRequiredColor;
          LVCLPreviewForm.FRequiredTextEdit.Font.Color := TStyleManager.Style[LStyleName].GetSystemColor(clWindowText);
          LVCLPreviewForm.FReadonlyTextEdit.StyleElements := [seBorder];
          LVCLPreviewForm.FReadonlyTextEdit.Color := LThemeAttribute.EditReadonlyColor;
          LVCLPreviewForm.FReadonlyTextEdit.Font.Color := TStyleManager.Style[LStyleName].GetSystemColor(clWindowText);
        end;
        LVCLPreviewForm.Parent := LpnPreview;
        LVCLPreviewForm.Align := alClient;
        if LStyleName = 'Windows' then
        begin
          LVCLPreviewForm.StyleElements := [];
          LVCLPreviewForm.TabControl.StyleElements := [];
        end;
      end;
      {$ELSE}
      //Before 10.4 cannot use per-control styles
      LVCLPreview := TCBVclStylesPreview.Create(LpnPreview);
      {$IFEND}
      if Assigned(LVCLPreview) then
      begin
        LVCLPreview.Caption := PREVIEW_THEME;
        LVCLPreview.SetEditColors(LThemeAttribute.EditRequiredColor,
          LThemeAttribute.EditReadonlyColor);
        LVCLPreview.SetCaptions(THEME_PREVIEW_VALUES);
        LVCLPreview.Parent := LpnPreview;
        LVCLPreview.CustomStyle := TStyleManager.Style[LStyleName];
        LVCLPreview.Align := alClient;
      end;

      if SameText(LStyleName, LActiveStyleName)  then
      begin
        FPreviousStyleName := LStyleName;
        FStyleName := LStyleName;
        LpnButton.Font.Style := [fsBold];
      end;

      {$IF CompilerVersion >= 34}
      if Assigned(LVCLPreviewForm) then
        LVCLPreviewForm.FormShow(LVCLPreview);
      {$IFEND}
    end;

    if LCountLight > LCountDark then
    begin
      RightScrollBox.Align := alRight;
      LeftScrollBox.Align := alClient;
    end;

    if (LCountStyle mod FMaxColumns) <> 0 then
      LNumRows := (LCountStyle div FMaxColumns) + 1
    else
      LNumRows := (LCountStyle div FMaxColumns);

    Self.Constraints.MaxHeight := Screen.Height;
    LHeight := (LpnPreview.Height+(LpnPreview.Margins.Top+LpnPreview.Margins.Bottom*2));
    LWidth := (LpnPreview.Width+(LpnPreview.Margins.Left+LpnPreview.Margins.Right*2));
    LCalcHeight := (LHeight*Min(LNumRows, FMaxRows))+paButtons.Height + TopPanel.Height;
    LCalcWidth := (LWidth*FMaxColumns);

    LMonitor := Screen.MonitorFromWindow(Self.Handle);
    LMonitorMargin := 100;
    //Check Max height available
    if (LCalcHeight > LMonitor.Height - LMonitorMargin) or
      (LNumRows > FMaxRows) or
      (LCalcWidth > LMonitor.Width - LMonitorMargin) then
    begin
      //Show scrollbar
      LScrollBarWidth := GetSystemMetrics(SM_CXVSCROLL);
      LCalcHeight := Min(LHeight * FMaxRows + paButtons.Height + TopPanel.Height,
        LMonitor.Height - LMonitorMargin);
      Self.Constraints.MinHeight := LCalcHeight;
      Self.ClientHeight := LCalcHeight;
      LeftScrollBox.VertScrollBar.Visible := True;
      RightScrollBox.VertScrollBar.Visible := True;
      i := FMaxColumns;
      while True do
      begin
        LCalcWidth := LWidth * i + (LScrollBarWidth * 2) + 6;
        if LCalcWidth <= LMonitor.Width - LMonitorMargin then
          break
        else
          Dec(i);
      end;
      Self.Constraints.MinWidth := LCalcWidth;
      Self.ClientWidth := LCalcWidth;
    end
    else
    begin
      LeftScrollBox.VertScrollBar.Visible := False;
      RightScrollBox.VertScrollBar.Visible := False;
      //Self.BorderStyle := bsDialog;
      Self.Constraints.MinHeight := LCalcHeight;
      Self.ClientHeight := LCalcHeight;
      if LCountStyle < FMaxColumns then
      begin
        Self.Constraints.MinWidth := (LWidth*LCountStyle);
        Self.ClientWidth := (LWidth*LCountStyle);
      end
      else
      begin
        Self.Constraints.MinWidth := LWidth*FMaxColumns;
        Self.ClientWidth := LWidth*FMaxColumns;
      end;
    end;

  finally
    LStyleNames.Free;
  end;
end;

procedure TVCLThemeSelectorForm.FormAfterMonitorDpiChanged(Sender: TObject;
  OldDPI, NewDPI: Integer);
begin
  if ParentFont and (Application.MainForm.Monitor.Handle <> Self.Monitor.Handle) then
    Font.Height := MulDiv(Font.Height, NewDPI, OldDPI);
end;

procedure TVCLThemeSelectorForm.FormCreate(Sender: TObject);
begin
  Caption := SELECT_THEME;
  acApplyStyle.Caption := APPLY_THEME;
  acCancel.Caption := CANCEL_THEME;
  LightPanel.Caption := LIGHT_THEMES;
  DarkPanel.Caption := DARK_THEMES;

  UpdateButtons;
  UpdateLabel;
end;

procedure TVCLThemeSelectorForm.UpdateButtons;
begin
  btApply.Action := acApplyStyle;
  btCancel.Action := acCancel;
end;

procedure TVCLThemeSelectorForm.UpdateLabel;
begin
  if FPreviousStyleName <> FStyleName then
  begin
    StyleLabel.Caption := Format(THEME_SELECTED,[FStyleName]);
    StyleLabel.Visible := True;
  end
  else
    StyleLabel.Visible := False;
end;

procedure TVCLThemeSelectorForm.FormResize(Sender: TObject);
begin
  inherited;
  LeftFlowPanel.Height := ClientRect.Bottom-ClientRect.Top-paButtons.Height;
  RightFlowPanel.Height := LeftFlowPanel.Height;
  LightPanel.Width := ClientWidth div 2 -1;
  if RightScrollBox.Align = alClient then
  begin
    LeftScrollBox.Width := ClientWidth div 2;
    LeftScrollBox.VertScrollBar.Range := RightScrollBox.VertScrollBar.Range;
  end
  else
  begin
    RightScrollBox.Width := ClientWidth div 2;
    RightScrollBox.VertScrollBar.Range := LeftScrollBox.VertScrollBar.Range;
  end;
end;

procedure TVCLThemeSelectorForm.Loaded;
begin
  ParentFont := False;
  //Note: the form uses Screen.IconFont by default
  Font.Name := Screen.IconFont.Name;
  Font.Height := Muldiv(Screen.IconFont.Height, 96, Screen.IconFont.PixelsPerInch);
  {$IFDEF D10_1+}
  OnAfterMonitorDpiChanged := FormAfterMonitorDpiChanged;
  {$ENDIF}

  inherited;
  //Build Preview panels
  BuildPreviewPanels;
end;

procedure TVCLThemeSelectorForm.SelectionClick(Sender: TObject);
var
  LButton: TButton;
  LPanel: TPanel;
  I, J : integer;
begin
  if (Sender is TButton) then
  begin
    LButton := TButton(Sender);
    FStyleName := LButton.Caption;
    LButton.Font.Style := [fsBold];
    for I := 0 to LeftFlowPanel.ComponentCount-1 do
    begin
      if (LeftFlowPanel.Components[I] is TPanel) then
      begin
        LPanel := TPanel(LeftFlowPanel.Components[I]);
        for J := 0 to LPanel.ComponentCount-1 do
        begin
          if (LPanel.Components[J] is TButton) and (LButton <> LPanel.Components[J]) then
            TButton(LPanel.Components[J]).Font.Style := [];
        end;
      end;
    end;
    UpdateLabel;
    if btApply.CanFocus then
      btApply.SetFocus;
  end;
end;

procedure TVCLThemeSelectorForm.ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
Var
  msg: Cardinal;
  code: Cardinal;
  i, n: Integer;
begin
  Handled := true;
  If ssShift In Shift Then
    msg := WM_HSCROLL
  Else
    msg := WM_VSCROLL;

  If WheelDelta > 0 Then
    code := SB_LINEUP
  Else
    code := SB_LINEDOWN;

  n := Mouse.WheelScrollLines * 4; //Speed Up scrolling
  For i:= 1 to n Do
  begin
    LeftScrollBox.Perform( msg, code, 0 );
    RightScrollBox.Perform( msg, code, 0 );
  end;
  LeftScrollBox.Perform( msg, SB_ENDSCROLL, 0 );
  RightScrollBox.Perform( msg, SB_ENDSCROLL, 0 );
end;

initialization
  InitDefaultThemesAttributes;

finalization
  FreeThemesAttributes;
end.
