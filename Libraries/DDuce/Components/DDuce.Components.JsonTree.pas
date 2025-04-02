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

unit DDuce.Components.JsonTree;

interface

uses
  System.JSON, System.Classes,
  Vcl.Graphics,

  Spring,

  VirtualTrees.Types, VirtualTrees.Header, VirtualTrees, VirtualTrees.BaseTree,

  DDuce.Components.SectionTree, DDuce.Components.VirtualTrees.Node,
  DDuce.Settings.TextFormat;

type
  TJsonNode = TVTNode<TJSONAncestor>;

type
  TJsonTree = class(TSectionTree)
  private type
    TColorSettings = class(TPersistent)
    private
      FObjectName   : TTextFormatSettings;
      FStringValue  : TTextFormatSettings;
      FBooleanValue : TTextFormatSettings;
      FNumberValue  : TTextFormatSettings;
      FNullValue    : TTextFormatSettings;
      FOnChanged    : Event<TNotifyEvent>;      

    protected
      function GetOnChanged: IEvent<TNotifyEvent>;

      procedure InitializeObjects;
      procedure Changed;
      procedure FormatSettingsChanged(Sender: TObject);

    public
      procedure AfterConstruction; override;
      destructor Destroy; override;

      property ObjectName: TTextFormatSettings
        read FObjectName;

      property StringValue: TTextFormatSettings
        read FStringValue;

      property BooleanValue: TTextFormatSettings
        read FBooleanValue;

      property NumberValue: TTextFormatSettings
        read FNumberValue;

      property NullValue: TTextFormatSettings
        read FNullValue;

      property OnChanged: IEvent<TNotifyEvent>
        read GetOnChanged;      

    end;

  private
    FRootNode      : TJsonNode;
    FJson          : TJSONValue;
    FColorSettings : TColorSettings;

  protected
    procedure BuildTree; override;
    procedure ParseNode(ANode: TJsonNode);
    function GetNode(const AVNode: PVirtualNode): TJsonNode;

    {$REGION 'property access methods'}
    function GetFocusedJsonNode: TJsonNode;
    function GetFocusedValue: string;
    function GetJsonString: string;
    procedure SetJsonString(const Value: string);
    {$ENDREGION}

    {$REGION 'event dispatch methods'}
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;
    procedure DoNewText(
      Node       : PVirtualNode;
      Column     : TColumnIndex;
      const Text : string
    ); override;
    procedure DoGetBackColor(
      ANode          : PVirtualNode;
      var ABackColor : TColor
    ); override;
    procedure DoNodeDblClick(const HitInfo: THitInfo); override;
    procedure DoPaintText(
      Node         : PVirtualNode; 
      const Canvas : TCanvas; 
      Column       : TColumnIndex; 
      TextType     : TVSTTextType
    ); override;
    {$ENDREGION}

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property ColorSettings: TColorSettings
      read FColorSettings;

    property FocusedJsonNode: TJsonNode
      read GetFocusedJsonNode;

    { Return value in selected cell. }
    property FocusedValue: string
      read GetFocusedValue;

    property JsonString: string
      read GetJsonString write SetJsonString;
  end;

implementation

uses
  System.SysUtils,

  DDuce.Logger;

{$REGION 'TJsonTree'}
{$REGION 'construction and destruction'}
procedure TJsonTree.AfterConstruction;
begin
  inherited AfterConstruction;
  FColorSettings := TColorSettings.Create;
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
    Width    := 200;
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
    {toEditable, toEditOnDblClick,} toGridExtensions
  ];
  // After VK_RETURN switch to next column.
  TreeOptions.EditOptions := toVerticalEdit;
end;

destructor TJsonTree.Destroy;
begin
  FreeAndNil(FJson);
  FreeAndNil(FColorSettings);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TJsonTree.DoFreeNode(Node: PVirtualNode);
begin
  GetNode(Node).Free;
  inherited DoFreeNode(Node);
end;

procedure TJsonTree.DoGetBackColor(ANode: PVirtualNode; var ABackColor: TColor);
begin
  var LNode := GetNode(ANode);
  if Assigned(LNode) then
  begin
    if LNode.HasChildren then
      ABackColor := $00EBEBEB
    else
      ABackColor := $00F8F8F8;
  end;
  inherited DoGetBackColor(ANode, ABackColor);
end;

procedure TJsonTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
var
  LNode : TJsonNode;
  S     : string;
begin
  with pEventArgs do
  begin
    LNode := GetNode(Node);
    if LNode.Data is TJSONPair then
    begin
      var LValue := LNode.Data as TJSONPair;
      if Column = 0 then
      begin
        S := LValue.JsonString.Value;
        CellText := S;
      end
      else if Column = 1 then
      begin
        if LNode.HasChildren then
        begin
          if LValue.JsonValue is TJSONArray then
          begin
            CellText := Format('[%d]', [LNode.ChildCount]);
          end
          else if LValue.JsonValue is TJSONObject then
          begin
            CellText := Format('{%d}', [LNode.ChildCount]);
          end;
        end
        else
          CellText := LValue.JsonValue.Value;
      end;
    end
    else
    begin
      S := LNode.Data.Value;
      if Column = 0 then
      begin
        if LNode.HasChildren then
        begin
          if S.IsEmpty then
            S := Format('(%d)', [LNode.Index]);
          CellText := S;
      //    CellText := Format('%s {%d}', [S, LNode.ChildCount]);
        end
        else
        begin
          CellText := Format('(%d)', [LNode.Index]);
        end;
      end
      else if Column = 1 then
      begin
        if LNode.HasChildren then
        begin
          CellText := Format('{%d}', [LNode.ChildCount]);
        end
        else
          CellText := S;
      end;
    end;
  end;
  inherited DoGetText(pEventArgs);
end;

{ Called after node text has been edited. }

procedure TJsonTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const Text: string);
var
  LNode : TJsonNode;
begin
  LNode := GetNode(Node);
  if LNode.Data is TJSONPair then
  begin
    if Column = 0 then
      (LNode.Data as TJSONPair).JsonString := TJSONString.Create(Text);
  end;
  inherited DoNewText(Node, Column, Text);
end;

{ Expand/collapse on double click. }

procedure TJsonTree.DoNodeDblClick(const HitInfo: THitInfo);
var
  LNode : TJsonNode;
begin
  LNode := GetNode(HitInfo.HitNode);
  LNode.Expanded := not LNode.Expanded;
  inherited DoNodeDblClick(HitInfo);
end;

{ Custom text painting using ColorSettings. }

procedure TJsonTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  if TextType = ttNormal then
  begin
    var LNode := GetNode(Node);
    if Column = 0 then
    begin
      if LNode.Data is TJSONPair then
        Canvas.Font.Assign(ColorSettings.ObjectName.Font)
      else
        Canvas.Font.Assign(ColorSettings.NullValue.Font);
    end
    else if Column = 1 then
    begin
      if LNode.Data is TJSONPair then
      begin
        var LValue := (LNode.Data as TJSONPair).JsonValue;
        if LValue is TJSONNumber then
          Canvas.Font.Assign(ColorSettings.NumberValue.Font)
        else if LValue is TJSONBool then
          Canvas.Font.Assign(ColorSettings.BooleanValue.Font)
        else if LValue is TJSONString then
          Canvas.Font.Assign(ColorSettings.StringValue.Font)
        else
          Canvas.Font.Assign(ColorSettings.NullValue.Font);
      end
      else
      begin
        if LNode.Data is TJSONNumber then
          Canvas.Font.Assign(ColorSettings.NumberValue.Font)
        else if LNode.Data is TJSONBool then
          Canvas.Font.Assign(ColorSettings.BooleanValue.Font)
        else if LNode.Data is TJSONString then
          Canvas.Font.Assign(ColorSettings.StringValue.Font)
        else
          Canvas.Font.Assign(ColorSettings.NullValue.Font);
      end;
    end;
  end;
  inherited DoPaintText(Node, Canvas, Column, TextType);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TJsonTree.GetFocusedJsonNode: TJsonNode;
begin
  Result := GetNode(FocusedNode);
end;

function TJsonTree.GetFocusedValue: string;
begin
  if Assigned(FocusedJsonNode) and (FocusedJsonNode.Data is TJSONPair) then
  begin
    var LPair := FocusedJsonNode.Data as TJSONPair;
    if FocusedColumn = 0 then
    begin
      Result := LPair.JsonString.Value;
    end
    else if FocusedColumn = 1 then
    begin
      Result := LPair.JsonValue.Value;
    end;
  end;
end;

function TJsonTree.GetJsonString: string;
begin
  if Assigned(FJson) then
    Result := FJson.Format(2)
  else
    Result := '';
end;

procedure TJsonTree.SetJsonString(const Value: string);
begin
  if Assigned(FJson) then
  begin
    FreeAndNil(FJson);
    Clear;
    FRootNode := nil;
  end;
  FJson := TJSONObject.ParseJSONValue(Value);
  if Assigned(FJson) then
    BuildTree;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TJsonTree.BuildTree;
begin
  BeginUpdate;
  try
    FRootNode := TJsonNode.Create(Self, FJson, False);
    ParseNode(FRootNode);
    FRootNode.Expand;
    Header.AutoFitColumns;
  finally
    EndUpdate;
  end;
end;

function TJsonTree.GetNode(const AVNode: PVirtualNode): TJsonNode;
begin
  Result := GetNodeData<TJsonNode>(AVNode);
end;

{ Parses a JSON object and its children to instrument the tree structure. }

procedure TJsonTree.ParseNode(ANode: TJsonNode);
var
  LValue : TJSONAncestor;
  LNode  : TJsonNode;
  LData  : TJSONAncestor;
begin
  LData := ANode.Data;
  if LData is TJSONPair then
  begin
    LValue := (LData as TJSONPair).JsonValue;
  end
  else
    LValue := ANode.Data;
  if LValue is TJSONObject then
  begin
    var LObject := LValue as TJSONObject;
    for var LPair in LObject do
    begin
      LNode := ANode.Add(LPair, False);
      if (LPair.JsonValue is TJSONArray) or (LPair.JsonValue is TJSONObject) then
      begin
        ParseNode(LNode);
      end
    end;
  end
  else if LValue is TJSONArray then
  begin
    var LArray := LValue as TJSONArray;
    for var LItem in LArray do
    begin
      LNode := ANode.Add(LItem, False);
      if (LItem is TJSONArray) or (LItem is TJSONObject) then
      begin
        ParseNode(LNode);
      end
    end;
  end;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TJsonTree.TColorSettings'}
{$REGION 'construction and destruction'}
procedure TJsonTree.TColorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  InitializeObjects;
end;

destructor TJsonTree.TColorSettings.Destroy;
begin
  FreeAndNil(FObjectName);
  FreeAndNil(FStringValue);
  FreeAndNil(FBooleanValue);
  FreeAndNil(FNumberValue);
  FreeAndNil(FNullValue);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TJsonTree.TColorSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TJsonTree.TColorSettings.FormatSettingsChanged(Sender: TObject);
begin
  Changed;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TJsonTree.TColorSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;

procedure TJsonTree.TColorSettings.InitializeObjects;
begin
  FObjectName   := TTextFormatSettings.Create;
  FObjectName.OnChanged.Add(FormatSettingsChanged);  
  FObjectName.FontName  := 'Consolas';
  FObjectName.FontSize  := 10;
  FObjectName.FontColor := clMaroon;
  FObjectName.FontStyle := [fsBold];

  FStringValue  := TTextFormatSettings.Create;
  FStringValue.OnChanged.Add(FormatSettingsChanged);
  FStringValue.FontColor := clGreen;
  FStringValue.FontStyle := [fsBold];
  FStringValue.FontSize  := 10;
  FStringValue.FontName  := 'Consolas';

  FBooleanValue := TTextFormatSettings.Create;
  FBooleanValue.OnChanged.Add(FormatSettingsChanged);
  FBooleanValue.FontColor := clBlue;
  FBooleanValue.FontStyle := [fsBold];
  FBooleanValue.FontSize  := 10;
  FBooleanValue.FontName  := 'Consolas';

  FNumberValue  := TTextFormatSettings.Create;
  FNumberValue.OnChanged.Add(FormatSettingsChanged);
  FNumberValue.FontColor := clRed;
  FNumberValue.FontStyle := [fsBold];
  FNumberValue.FontSize  := 10;
  FNumberValue.FontName  := 'Consolas';

  FNullValue  := TTextFormatSettings.Create;
  FNullValue.FontColor := clGray;
  FNullValue.FontStyle := [];
  FNullValue.FontSize  := 10;
  FNullValue.FontName  := 'Consolas';
  FNullValue.OnChanged.Add(FormatSettingsChanged);
end;
{$ENDREGION}
{$ENDREGION}

end.
