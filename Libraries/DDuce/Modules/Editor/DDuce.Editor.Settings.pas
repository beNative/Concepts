{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit DDuce.Editor.Settings;

interface

{$REGION 'Documentation'}
{
  TEditorSettings is a component that implements IEditorSettings and holds
  all settings that can be persisted.

  All published properties are streamed from/to a XML file in a similar way as
  components are stored in a DFM/LFM file so there is no special code needed
  that handles the storage and retrieval of properties.

  Reading and writing of the settings is handled completely by these methods:
    procedure Load;
    procedure Save;

  To support another persistence mechanism (like e.g. LFM or JSON) you only
  need to override these two methods.

  Some remarks:
  - Don't use TPersistent objects with published TComponent properties as these
    will not be persisted. If you call SetSubComponent(True) on the nested
    component they will be stored to XML, but they will not be read if you
    try to load it back from XML.
  - Collections can be stored and loaded from XML without problems, but keep in
    mind that you don't wrap TComponent properties in your collection items (as
    these descend from TPersistent).
  - If you need to store a list of components the best way is to rely on the
    ownership mechanism of TComponent to store sub components.
    Both TToolSettings and THighlighters are components that are used as a
    container to host other components. Every subcomponent that they own will
    be persisted automatically. The property that references the container
    component does not need to be published.
    All the component types you want to store this way need to be registered
    with the RegisterClass procedure found in the Classes unit.
}
{$ENDREGION}

uses
  System.Classes, System.SysUtils,
  Vcl.Graphics, Vcl.ActnList,

  Spring,

  DDuce.Settings.Form,
  DDuce.Editor.Interfaces, DDuce.Editor.Highlighters,
  DDuce.Editor.Tools.Settings, DDuce.Editor.Colors.Settings,
  DDuce.Editor.Options.Settings, DDuce.Editor.CodeTags,
  DDuce.Logger;

const
  DEFAULT_AUTO_GUESS_HIGHLIGHTER_TYPE = True;
  DEFAULT_AUTO_FORMAT_XML             = True;
  DEFAULT_RIGHT_EDGE                  = 80;
  DEFAULT_DIM_ACTIVE_VIEW             = True;
  DEFAULT_SINGLE_INSTANCE             = False;
  DEFAULT_LANGUAGE_CODE               = 'en';
  DEFAULT_FONT_NAME                   = 'Consolas';
  DEFAULT_SETTINGS_FILE               = 'settings.json';

type
  TEditorSettings = class(TComponent, IEditorSettings)
    procedure FEditorOptionsChanged(Sender: TObject);

  private
    FAutoFormatXML            : Boolean;
    FReadOnly                 : Boolean;
    FHighlighterType          : string;
    FAutoGuessHighlighterType : Boolean;
    FCloseWithEsc             : Boolean;
    FDebugMode                : Boolean;
    FSingleInstance           : Boolean;
    FFileName                 : string;
    FLanguageCode             : string;
    FHighlighters             : THighLighters;
    FDimInactiveView          : Boolean;
    FFormSettings             : TFormSettings;
    FEditorFont               : TFont;
    FColors                   : TEditorColorSettings;
    FToolSettings             : TEditorToolSettings;
    FEditorOptions            : TEditorOptionsSettings;
    FOnChanged                : Event<TNotifyEvent>;

    procedure FFormSettingsChanged(Sender: TObject);
    procedure FColorsChanged(Sender: TObject);

    {$REGION 'property access methods'}
    function GetAutoFormatXML: Boolean;
    function GetAutoGuessHighlighterType: Boolean;
    function GetCloseWithESC: Boolean;
    function GetColors: TEditorColorSettings;
    function GetDebugMode: Boolean;
    function GetDimInactiveView: Boolean;
    function GetEditorFont: TFont;
    function GetEditorOptions: TEditorOptionsSettings;
    function GetFileName: string;
    function GetFormSettings: TFormSettings;
    function GetHighlighters: THighlighters;
    function GetHighlighterType: string;
    function GetLanguageCode: string;
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetReadOnly: Boolean;
    function GetSingleInstance: Boolean;
    function GetToolSettings: TEditorToolSettings;
    procedure SetAutoFormatXML(const AValue: Boolean);
    procedure SetAutoGuessHighlighterType(const AValue: Boolean);
    procedure SetCloseWithESC(const AValue: Boolean);
    procedure SetColors(AValue: TEditorColorSettings);
    procedure SetDebugMode(AValue: Boolean);
    procedure SetDimInactiveView(const AValue: Boolean);
    procedure SetEditorFont(AValue: TFont);
    procedure SetEditorOptions(AValue: TEditorOptionsSettings);
    procedure SetFileName(const AValue: string);
    procedure SetFormSettings(const AValue: TFormSettings);
    procedure SetHighlighters(const AValue: THighlighters);
    procedure SetHighlighterType(const AValue: string);
    procedure SetLanguageCode(AValue: string);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetSingleInstance(AValue: Boolean);
    procedure SetToolSettings(AValue: TEditorToolSettings);
    {$ENDREGION}

  protected
    procedure AssignDefaultColors;
    procedure InitializeHighlighterAttributes;
    procedure Changed;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure Apply; // to manually force a notification
    procedure Load; virtual;
    procedure Save; virtual;

    property FileName: string
      read GetFileName write SetFileName;

    property Highlighters: THighlighters
      read GetHighlighters write SetHighlighters;

    property ToolSettings: TEditorToolSettings
      read GetToolSettings write SetToolSettings;

  published
    property Colors: TEditorColorSettings
      read GetColors write SetColors;

    property EditorOptions: TEditorOptionsSettings
      read GetEditorOptions write SetEditorOptions;

    { Default highlighter type to use. }
    property HighlighterType: string
      read GetHighlighterType write SetHighlighterType;

    property LanguageCode: string
      read GetLanguageCode write SetLanguageCode;

    property ReadOnly: Boolean
      read GetReadOnly write SetReadOnly default False;

    property DimInactiveView: Boolean
      read GetDimInactiveView write SetDimInactiveView
      default DEFAULT_DIM_ACTIVE_VIEW;

    property AutoFormatXML: Boolean
      read GetAutoFormatXML write SetAutoFormatXML
      default DEFAULT_AUTO_FORMAT_XML;

    property AutoGuessHighlighterType: Boolean
      read GetAutoGuessHighlighterType write SetAutoGuessHighlighterType
      default DEFAULT_AUTO_GUESS_HIGHLIGHTER_TYPE;

    { Determines if the application can be closed with the ESCAPE key. }
    property CloseWithESC: Boolean
      read GetCloseWithESC write SetCloseWithESC default False;

    property FormSettings: TFormSettings
      read GetFormSettings write SetFormSettings;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    property DebugMode: Boolean
      read GetDebugMode write SetDebugMode default False;

    property SingleInstance: Boolean
      read GetSingleInstance write SetSingleInstance
      default DEFAULT_SINGLE_INSTANCE;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;
  end;

implementation

uses
  Vcl.Dialogs, Vcl.Forms,

  JsonDataObjects,

  DDuce.Editor.Resources;

{$REGION 'construction and destruction'}
procedure TEditorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FFormSettings := TFormSettings.Create;
  FFormSettings.OnChanged.Add(FFormSettingsChanged);
  FColors := TEditorColorSettings.Create;
  FColors.OnChanged := FColorsChanged;
  FEditorOptions := TEditorOptionsSettings.Create;
  FEditorOptions.OnChanged := FEditorOptionsChanged;

  FToolSettings := TEditorToolSettings.Create(Self);
  FToolSettings.Name := 'ToolSettings';

  FHighlighters := THighLighters.Create(Self);
  FHighlighters.Name := 'Highlighters';

  FFileName                      := DEFAULT_SETTINGS_FILE;
  FHighlighterType               := HL_TXT;
  FAutoFormatXML                 := DEFAULT_AUTO_FORMAT_XML;
  FAutoGuessHighlighterType      := DEFAULT_AUTO_GUESS_HIGHLIGHTER_TYPE;
  FSingleInstance                := DEFAULT_SINGLE_INSTANCE;
  FEditorFont                    := TFont.Create;
  FEditorFont.Name               := DEFAULT_FONT_NAME;
  FEditorFont.Size               := 11;

  FDimInactiveView               := DEFAULT_DIM_ACTIVE_VIEW;
  FLanguageCode                  := DEFAULT_LANGUAGE_CODE;
  FOnChanged.UseFreeNotification := False; // test TS

  AssignDefaultColors;
end;

destructor TEditorSettings.Destroy;
begin
  FFormSettings.OnChanged.Remove(FFormSettingsChanged);
  FToolSettings.Free;
  FColors.Free;
  FEditorOptions.Free;
  FFormSettings.Free;
  FHighlighters.Free;
  FEditorFont.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TEditorSettings.FColorsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TEditorSettings.FEditorOptionsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TEditorSettings.FFormSettingsChanged(Sender: TObject);
begin
  Changed;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TEditorSettings.GetAutoFormatXML: Boolean;
begin
  Result := FAutoFormatXML;
end;

procedure TEditorSettings.SetAutoFormatXML(const AValue: Boolean);
begin
  if AValue <> AutoFormatXML then
  begin
    FAutoFormatXML := AValue;
  end;
end;

function TEditorSettings.GetAutoGuessHighlighterType: Boolean;
begin
  Result := FAutoGuessHighlighterType;
end;

procedure TEditorSettings.SetAutoGuessHighlighterType(const AValue: Boolean);
begin
  if AValue <> AutoGuessHighlighterType then
  begin
    FAutoGuessHighlighterType := AValue;
  end;
end;

function TEditorSettings.GetCloseWithESC: Boolean;
begin
  Result := FCloseWithESC;
end;

procedure TEditorSettings.SetCloseWithESC(const AValue: Boolean);
begin
  if AValue <> CloseWithESC then
  begin
    FCloseWithESC := AValue;
  end;
end;

function TEditorSettings.GetColors: TEditorColorSettings;
begin
  Result := FColors;
end;

procedure TEditorSettings.SetColors(AValue: TEditorColorSettings);
begin
  FColors.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetDebugMode: Boolean;
begin
  Result := FDebugMode;
end;

procedure TEditorSettings.SetDebugMode(AValue: Boolean);
begin
  if AValue <> DebugMode then
  begin
    FDebugMode := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetDimInactiveView: Boolean;
begin
  Result := FDimInactiveView;
end;

procedure TEditorSettings.SetDimInactiveView(const AValue: Boolean);
begin
  if AValue <> DimInactiveView then
  begin
    FDimInactiveView := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetEditorFont: TFont;
begin
  Result := FEditorFont;
end;

procedure TEditorSettings.SetEditorFont(AValue: TFont);
begin
  FEditorFont.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TEditorSettings.SetFileName(const AValue: string);
begin
  if AValue <> FileName then
  begin
    FFileName := AValue;
  end;
end;

function TEditorSettings.GetFormSettings: TFormSettings;
begin
  Result := FFormSettings;
end;

procedure TEditorSettings.SetFormSettings(const AValue: TFormSettings);
begin
  FFormSettings.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetHighlighters: THighlighters;
begin
  Result := FHighlighters;
end;

procedure TEditorSettings.SetHighlighters(const AValue: THighlighters);
begin
  FHighlighters.Assign(AValue);
end;

function TEditorSettings.GetHighlighterType: string;
begin
  Result := FHighlighterType;
end;

procedure TEditorSettings.SetHighlighterType(const AValue: string);
begin
  if AValue <> HighlighterType then
  begin
    FHighlighterType := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetLanguageCode: string;
begin
  Result := FLanguageCode;
end;

function TEditorSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

procedure TEditorSettings.SetLanguageCode(AValue: string);
begin
  if AValue <> LanguageCode then
  begin
    FLanguageCode := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetEditorOptions: TEditorOptionsSettings;
begin
  Result := FEditorOptions;
end;

procedure TEditorSettings.SetEditorOptions(AValue: TEditorOptionsSettings);
begin
  FEditorOptions.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TEditorSettings.SetReadOnly(const AValue: Boolean);
begin
  if AValue <> ReadOnly then
  begin
    FReadOnly := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetSingleInstance: Boolean;
begin
  Result := FSingleInstance;
end;

procedure TEditorSettings.SetSingleInstance(AValue: Boolean);
begin
  if AValue <> SingleInstance then
  begin
    FSingleInstance := AValue;
    // we need to save here to make sure that any other instance runs with the
    // same configuration.
    Save;
    Changed;
  end;
end;

function TEditorSettings.GetToolSettings: TEditorToolSettings;
begin
  Result := FToolSettings;
end;

procedure TEditorSettings.SetToolSettings(AValue: TEditorToolSettings);
begin
  FToolSettings.Assign(AValue);
  Changed;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TEditorSettings.AssignDefaultColors;
begin
//  Colors.BracketMatchColor.Background := clAqua;
//  Colors.BracketMatchColor.Foreground := clNone;
//  Colors.BracketMatchColor.FrameColor := clBlue;
//
//  Colors.SelectedColor.Background := clMedGray;
//  Colors.SelectedColor.BackAlpha  := 128;
//  Colors.SelectedColor.Foreground := clNone;
//
//  Colors.IncrementColor.Background := clMedGray;
//  Colors.IncrementColor.BackAlpha  := 128;
//  Colors.IncrementColor.Foreground := clNone;
//
//  Colors.HighlightAllColor.Background := $000080FF; // orange
//  Colors.HighlightAllColor.BackAlpha  := 128;
//  Colors.HighlightAllColor.Foreground := clNone;
//  Colors.HighlightAllColor.FrameColor := $00006BD7; // dark orange
//
//  Colors.LineHighlightColor.Background := clYellow;
//  Colors.LineHighlightColor.BackAlpha  := 128;
//  Colors.LineHighlightColor.Foreground := clNone;
//  Colors.LineHighlightColor.FrameColor := clOlive;
//  Colors.LineHighlightColor.FrameAlpha := 64;
//  Colors.LineHighlightColor.FrameStyle := slsDashed;
//
//  Colors.FoldedCodeColor.Background := clSilver;
//  Colors.FoldedCodeColor.BackAlpha  := 50;
//  Colors.FoldedCodeColor.Foreground := clMedGray;
//  Colors.FoldedCodeColor.FrameColor := clMedGray;
end;

procedure TEditorSettings.Changed;
begin
  OnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TEditorSettings.Load;
var
  JO : TJsonObject;
begin
  Logger.Track(Self, 'Load');
  if FileExists(FFileName) then
  begin
    JO := TJsonObject.Create;
    try
      JO.LoadFromFile(FFileName);
      JO['FormSettings'].ObjectValue.ToSimpleObject(FFormSettings);
      JO['Colors'].ObjectValue.ToSimpleObject(FColors);
      JO['EditorOptions'].ObjectValue.ToSimpleObject(FEditorOptions);
      JO['EditorFont'].ObjectValue.ToSimpleObject(FEditorFont);
      JO.ToSimpleObject(Self);
    finally
      JO.Free;
    end;
  end;
end;

procedure TEditorSettings.Save;
var
  JO : TJsonObject;
begin
  Logger.Track(Self, 'Save');
  JO := TJsonObject.Create;
  try
    JO.FromSimpleObject(Self);
    JO['FormSettings'].ObjectValue.FromSimpleObject(FFormSettings);
    JO['Colors'].ObjectValue.FromSimpleObject(FColors);
    JO['EditorOptions'].ObjectValue.FromSimpleObject(FEditorOptions);
    JO['EditorFont'].ObjectValue.FromSimpleObject(FEditorFont);
    JO.SaveToFile(FFileName, False);
  finally
    JO.Free;
  end;
end;

procedure TEditorSettings.Apply;
begin
  Changed;
end;

{ TODO -oTS : Refactor this }

{ Registers common highlighter attributes to share settings for multiple
  highlighters. }

procedure TEditorSettings.InitializeHighlighterAttributes;
begin
//
end;
{$ENDREGION}

end.

