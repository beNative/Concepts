{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Components.ValueList;

interface

uses
  System.Classes, System.Types,
  Vcl.Graphics,

  VirtualTrees,

  Spring, Spring.Collections,

  DDuce.Factories.VirtualTrees, DDuce.DynamicRecord,
  DDuce.Components.ValueList.Node;

type
  TValueList = class(TCustomVirtualStringTree)
  private
    FData  : IDynamicRecord;
    FNodes : IList<TValueListNode>;

  protected
    {$REGION 'property access methods'}
    procedure SetNameColumn(const Value: TVirtualTreeColumn);
    procedure SetValueColumn(const Value: TVirtualTreeColumn);
    function GetEditable: Boolean;
    procedure SetEditable(const Value: Boolean);
    function GetNameColumn: TVirtualTreeColumn;
    function GetValueColumn: TVirtualTreeColumn;
    function GetShowHeader: Boolean;
    procedure SetShowHeader(const Value: Boolean);
    function GetData: IDynamicRecord;
    procedure SetData(const Value: IDynamicRecord);
    {$ENDREGION}

    procedure Initialize;

    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;
    procedure DoInitNode(
      Parent         : PVirtualNode;
      Node           : PVirtualNode;
      var InitStates : TVirtualNodeInitStates
    ); override;
    function DoInitChildren(
      Node           : PVirtualNode;
      var ChildCount : Cardinal
    ) : Boolean; override;
    procedure DoBeforeCellPaint(
      Canvas          : TCanvas;
      Node            : PVirtualNode;
      Column          : TColumnIndex;
      CellPaintMode   : TVTCellPaintMode;
      CellRect        : TRect;
      var ContentRect : TRect
    ); override;
    procedure DoNewText(
      Node       : PVirtualNode;
      Column     : TColumnIndex;
      const Text : string
    ); override;

  public
    procedure AfterConstruction; override;

    property Data: IDynamicRecord
      read GetData write SetData;

  published
    property Font;

    property ShowHeader: Boolean
      read GetShowHeader write SetShowHeader;

    property Editable: Boolean
      read GetEditable write SetEditable;

    property NameColumn: TVirtualTreeColumn
      read GetNameColumn write SetNameColumn;

    property ValueColumn: TVirtualTreeColumn
      read GetValueColumn write SetValueColumn;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TValueList.AfterConstruction;
begin
  inherited AfterConstruction;
  FNodes := TCollections.CreateObjectList<TValueListNode>;
  Initialize;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TValueList.GetData: IDynamicRecord;
begin
  Result := FData;
end;

procedure TValueList.SetData(const Value: IDynamicRecord);
begin
  if Value <> Data then
  begin
    FData := Value;
    if Assigned(FData) then
    begin
      NodeDataSize  := SizeOf(TValueListNode);
      RootNodeCount := FData.Count;
      Header.AutoFitColumns;
    end;
  end;
end;

function TValueList.GetEditable: Boolean;
begin
  Result := toEditable in TreeOptions.MiscOptions;
end;

procedure TValueList.SetEditable(const Value: Boolean);
begin
  if Value <> Editable then
  begin
    if Value then
      TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toEditable]
    else
      TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toEditable];
  end;
end;

function TValueList.GetNameColumn: TVirtualTreeColumn;
begin
  Result := Header.Columns.Items[0];
end;

procedure TValueList.SetNameColumn(const Value: TVirtualTreeColumn);
begin
  Header.Columns.Items[0].Assign(Value);
end;

function TValueList.GetValueColumn: TVirtualTreeColumn;
begin
  Result := Header.Columns.Items[1];
end;

procedure TValueList.SetValueColumn(const Value: TVirtualTreeColumn);
begin
  Header.Columns.Items[1].Assign(Value);
end;

function TValueList.GetShowHeader: Boolean;
begin
  Result := hoVisible in Header.Options;
end;

procedure TValueList.SetShowHeader(const Value: Boolean);
begin
  if Value <> ShowHeader then
  begin
    if Value then
      Header.Options := Header.Options + [hoVisible]
    else
      Header.Options := Header.Options - [hoVisible];
  end;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TValueList.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  N : TValueListNode;
begin
  N := GetNodeData<TValueListNode>(Node);
  ContentRect.Offset(2, 0);
  if Column = 0 then
  begin
    Canvas.Brush.Color := clCream;
    if Assigned(N.Field) then
    begin
      Canvas.FillRect(Rect(0, 0, 16, CellRect.Height));
    end
    else
    begin
      Canvas.FillRect(Rect(0, 0, 24, CellRect.Height));
    end;
  end;
  inherited DoBeforeCellPaint(
    Canvas, Node, Column, CellPaintMode, CellRect, ContentRect
  );
end;

procedure TValueList.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
var
  N : TValueListNode;
begin
  inherited DoGetText(pEventArgs);
  N := GetNodeData<TValueListNode>(pEventArgs.Node);
  if pEventArgs.Column = 0 then
  begin
    pEventArgs.CellText := N.Name
  end
  else if pEventArgs.Column = 1 then
  begin
    pEventArgs.CellText := N.Value.ToString;
  end;
end;

function TValueList.DoInitChildren(Node: PVirtualNode;
  var ChildCount: Cardinal): Boolean;
var
  N : TValueListNode;
  SN : TValueListNode;
begin
  N := GetNodeData<TValueListNode>(Node);
  ChildCount := N.Count;
  if N.Count > 0 then
  begin
    for SN in N.Nodes do
    begin
      SN.VTNode := AddChild(Node, SN);
    end;
  end;
  Result := inherited DoInitChildren(Node, ChildCount);
end;

procedure TValueList.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  N  : TValueListNode;
begin
  N := TValueListNode.Create;
  FNodes.Add(N);
  N.VTNode := Node;
  N.Field := FData.Items[Node.Index];
  if (Parent = nil) and (N.Count > 0) then
    InitStates := InitStates + [ivsHasChildren];
  Node.SetData(N);
  inherited DoInitNode(Parent, Node, InitStates);
end;

{ Gets called after text has been edited. }

procedure TValueList.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const Text: string);
var
  N : TValueListNode;
begin
  N := GetNodeData<TValueListNode>(Node);
  N.Value := Text;
  inherited DoNewText(Node, Column, Text);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TValueList.Initialize;
begin
  Header.Options := [
    hoAutoResize, hoColumnResize, hoDblClickResize, hoRestrictDrag,
    hoShowHint, hoShowImages, hoShowSortGlyphs, hoAutoSpring,
    hoDisableAnimatedResize, hoVisible
  ];
  TreeOptions.PaintOptions := [
    toHideFocusRect, toHideSelection, toHotTrack, toPopupMode,
    toShowBackground, toShowButtons, toShowDropmark, toStaticBackground,
    toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages,
    toUseBlendedSelection, toStaticBackground, toUseExplorerTheme
  ];
  TreeOptions.AnimationOptions := [];
  TreeOptions.AutoOptions := [
    toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoSort,
    toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale,
    toDisableAutoscrollOnEdit, toAutoBidiColumnOrdering
  ];
  TreeOptions.SelectionOptions := [toExtendedFocus, toFullRowSelect];
  TreeOptions.MiscOptions := [
    toCheckSupport, toInitOnSave, toWheelPanning, toVariableNodeHeight,
    toGridExtensions, toEditable, toEditOnDblClick
  ];
  TreeOptions.EditOptions := toVerticalEdit;

  LineStyle                    := lsSolid;
  LineMode                     := lmNormal;
  DrawSelectionMode            := smBlendedRectangle;
  HintMode                     := hmTooltip;
  Colors.SelectionRectangleBlendColor := clGray;
  Colors.SelectionTextColor           := clBlack;
  Colors.GridLineColor                := clSilver;

  with Header.Columns.Add do
  begin
    Color    := clWhite;
    MaxWidth := 400;
    MinWidth := 80;
    Options  := [coFixed, coAllowClick, coEnabled, coParentBidiMode, coResizable,
      coVisible, coSmartResize];
    Position := 0;
    Width    := 200;
    Text     := 'Name';
  end;
  with Header.Columns.Add do
  begin
    MaxWidth    := 800;
    MinWidth    := 50;
    Options     := [coAllowClick, coEnabled, coParentBidiMode, coResizable,
      coVisible, coAutoSpring, coSmartResize, coAllowFocus, coEditable];
    Position    := 1;
    Width       := 100;
    Text        := 'Value';
    EditOptions := toVerticalEdit;
  end;
  Indent               := 8;
  Header.AutoSizeIndex := 1;
  Header.MainColumn    := 0;
  LineMode             := lmBands;
end;
{$ENDREGION}

end.
