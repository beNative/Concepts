{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I DDuce.inc}

unit DDuce.Components.IniTree;

interface

uses
  System.IniFiles, System.Classes, System.Generics.Collections, System.Rtti,
  Vcl.Graphics,

  Spring,

  VirtualTrees.Types, VirtualTrees.Header, VirtualTrees, VirtualTrees.BaseTree,

  DDuce.DynamicRecord, DDuce.Settings.TextFormat, DDuce.Components.SectionTree,
  DDuce.Components.VirtualTrees.Node;

type
  TIniData = TPair<string, TValue>;
  TIniNode = TVTNode<TIniData>;

type
  TIniTree = class(TSectionTree)
  private type
    TColorSettings = class(TPersistent)
    private
      FSection   : TTextFormatSettings;
      FName      : TTextFormatSettings;
      FValue     : TTextFormatSettings;
      FNullValue : TTextFormatSettings;
      FOnChanged : Event<TNotifyEvent>;

    protected
      function GetOnChanged: IEvent<TNotifyEvent>;

      procedure InitializeObjects;
      procedure Changed;
      procedure FormatSettingsChanged(Sender: TObject);

    public
      procedure AfterConstruction; override;
      destructor Destroy; override;

      property NullValue: TTextFormatSettings
        read FNullValue;

      property Section: TTextFormatSettings
        read FSection;

      property Name: TTextFormatSettings
        read FName;

      property Value: TTextFormatSettings
        read FValue;

      property OnChanged: IEvent<TNotifyEvent>
        read GetOnChanged;

    end;

  private
    FIniFile       : TMemIniFile;
    FIniStream     : TStringStream;
    FIniString     : string;
    FColorSettings : TColorSettings;

  protected
    {$REGION 'property access methods'}
    function GetFocusedIniNode: TIniNode;
    function GetFocusedValue: string;
    function GetIniString: string;
    procedure SetIniString(const Value: string);
    {$ENDREGION}

    {$REGION 'event dispatch methods'}
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;
    procedure DoNewText(
      Node       : PVirtualNode;
      Column     : TColumnIndex;
      const Text : string
    ); override;
    procedure DoNodeDblClick(const HitInfo: THitInfo); override;
    procedure DoPaintText(
      Node         : PVirtualNode;
      const Canvas : TCanvas;
      Column       : TColumnIndex;
      TextType     : TVSTTextType
    ); override;
    {$ENDREGION}

    procedure BuildTree; override;
    function GetNode(const AVNode: PVirtualNode): TIniNode;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property FocusedIniNode: TIniNode
      read GetFocusedIniNode;

    { Return value in selected cell. }
    property FocusedValue: string
      read GetFocusedValue;

    property IniString: string
      read GetIniString write SetIniString;
  end;

implementation

uses
  System.SysUtils,

  DDuce.Logger, DDuce.Logger.Interfaces;

{$REGION 'construction and destruction'}
procedure TIniTree.AfterConstruction;
begin
  inherited AfterConstruction;
  FColorSettings := TColorSettings.Create;
  FIniStream := TStringStream.Create;
  Header.Options := Header.Options + [hoAutoResize];
  with Header.Columns.Add do
  begin
    Color    := clWhite;
    MaxWidth := 1200;
    MinWidth := 100;
    Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
      coParentColor, coResizable, coShowDropMark, coVisible, coSmartResize,
      coAllowFocus, coFixed{, coEditable}];
    Position := 0;
    Width    := 400;
    Text     := 'Name';
  end;
  with Header.Columns.Add do
  begin
    MaxWidth := 1200;
    MinWidth := 100;
    Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
      coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
      coSmartResize, coAllowFocus{, coEditable}];
    Position := 1;
    Width    := 400;
    Text     := 'Value';
  end;
  Header.AutoSizeIndex := 1;
  TreeOptions.MiscOptions := [
    toCheckSupport, toInitOnSave, toWheelPanning, toVariableNodeHeight,
    {toEditable,} toEditOnDblClick, toGridExtensions
  ];
  TreeOptions.EditOptions := toVerticalEdit;
end;

destructor TIniTree.Destroy;
begin
  FreeAndNil(FColorSettings);
  FreeAndNil(FIniStream);
  FreeAndNil(FIniFile);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TIniTree.GetFocusedIniNode: TIniNode;
begin
  Result := GetNode(FocusedNode);
end;

function TIniTree.GetFocusedValue: string;
begin
  if Assigned(FocusedIniNode) then
  begin
    if FocusedColumn = 0 then
      Result := FocusedIniNode.Data.Key
    else
      Result := FocusedIniNode.Data.Value.ToString;
  end;
end;

function TIniTree.GetIniString: string;
begin
  Result := FIniString;
end;

procedure TIniTree.SetIniString(const Value: string);
begin
   if Value <> IniString then
   begin
     Clear;
     FreeAndNil(FIniFile);
     FIniString := Value;
     FIniStream.Clear;
     FIniStream.WriteString(Value);
     FIniFile := TMemIniFile.Create(FIniStream);
     BuildTree;
   end;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TIniTree.DoFreeNode(Node: PVirtualNode);
begin
  GetNode(Node).Free;
  inherited DoFreeNode(Node);
end;

procedure TIniTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
var
  LNode  : TIniNode;
  LValue : TIniData;
  S      : string;
begin
  with pEventArgs do
  begin
    LNode  := GetNode(Node);
    LValue := LNode.Data;
    if Column = 0 then
    begin
      S := LValue.Key;
    end
    else
    begin
      S := LValue.Value.ToString;
    end;
    CellText := S;
  end;
end;

{ Gets called after text has been edited. Entered new text is assigned to Text
  parameter. }

procedure TIniTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const Text: string);
var
  LNode    : TIniNode;
  LStrings : IShared<TStringList>;
begin
  LNode := GetNode(Node);
  if LNode.HasParent then
  begin
    FIniFile.WriteString(LNode.ParentData.Key, LNode.Data.Key, Text);
  end;
  FIniFile.UpdateFile;
  LStrings := Shared.Make(TStringList.Create);
  FIniFile.GetStrings(LStrings);
  //Logger.SendStrings('FIniFile', LStrings);
  //InitNode(Node);
  Clear;
  BuildTree;
  inherited DoNewText(Node, Column, Text);
end;

procedure TIniTree.DoNodeDblClick(const HitInfo: THitInfo);
var
  LNode : TIniNode;
begin
  LNode := GetNode(HitInfo.HitNode);
  LNode.Expanded := not LNode.Expanded;
  inherited DoNodeDblClick(HitInfo);
end;

procedure TIniTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
  Column: TColumnIndex; TextType: TVSTTextType);
begin

  inherited DoPaintText(Node, Canvas, Column, TextType);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TIniTree.BuildTree;
var
  LSections      : IShared<TStringList>;
  LSection       : string;
  LSectionValues : IShared<TStringList>;
  LSectionNode   : TIniNode;
begin
  BeginUpdate;
  try
    LSections := Shared.Make(TStringList.Create);
    FIniFile.ReadSections(LSections);
    for LSection in LSections do
    begin
      LSectionNode := TIniNode.Create(Self, TIniData.Create(LSection, ''), False);
      LSectionValues := Shared.Make(TStringList.Create);
      FIniFile.ReadSectionValues(LSection, LSectionValues);
      for var I := 0 to LSectionValues.Count - 1 do
      begin
        LSectionNode.Add(TIniData.Create(
          LSectionValues.Names[I], LSectionValues.ValueFromIndex[I])
        );
      end;
    end;
    Header.AutoFitColumns;
  finally
    EndUpdate;
  end;
end;

function TIniTree.GetNode(const AVNode: PVirtualNode): TIniNode;
begin
  Result := GetNodeData<TIniNode>(AVNode);
end;
{$ENDREGION}

{$REGION 'TIniTree.TColorSettings'}
{$REGION 'construction and destruction'}
procedure TIniTree.TColorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  InitializeObjects;
end;

destructor TIniTree.TColorSettings.Destroy;
begin
  FreeAndNil(FNullValue);
  FreeAndNil(FSection);
  FreeAndNil(FName);
  FreeAndNil(FValue);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TIniTree.TColorSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TIniTree.TColorSettings.FormatSettingsChanged(Sender: TObject);
begin
  Changed;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TIniTree.TColorSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;

procedure TIniTree.TColorSettings.InitializeObjects;
begin
  FSection := TTextFormatSettings.Create;
  FSection.OnChanged.Add(FormatSettingsChanged);
  FSection.FontName  := 'Consolas';
  FSection.FontColor := clMaroon;
  FSection.FontStyle := [fsBold];

  FName := TTextFormatSettings.Create;
  FName.OnChanged.Add(FormatSettingsChanged);
  FName.FontName  := 'Consolas';
  FName.FontColor := clBlue;
  FName.FontStyle := [fsBold];

  FValue := TTextFormatSettings.Create;
  FValue.OnChanged.Add(FormatSettingsChanged);
  FValue.FontName  := 'Consolas';
  FValue.FontColor := clGreen;
  FValue.FontStyle := [fsBold];

  FNullValue := TTextFormatSettings.Create;
  FNullValue.OnChanged.Add(FormatSettingsChanged);
end;
{$ENDREGION}
{$ENDREGION}

end.
