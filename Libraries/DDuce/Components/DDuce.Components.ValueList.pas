{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

{ TCustomVirtualStringTree descendant to display key-value pairs. }

interface

uses
  System.Classes, System.Types,
  Vcl.Graphics,

  VirtualTrees,

  Spring, Spring.Collections,

  DDuce.Components.VirtualTrees.Node, DDuce.Factories.VirtualTrees,
  DDuce.DynamicRecord;

type
  TValueListNode = TVTNode<IDynamicField>;

type
  TValueList = class(TCustomVirtualStringTree)
  private
    FData        : IDynamicRecord;
    FInitialized : Boolean;
    FShowGutter  : Boolean;

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
    function GetMultiSelect: Boolean;
    procedure SetMultiSelect(const Value: Boolean);
    function GetShowGutter: Boolean;
    procedure SetShowGutter(const Value: Boolean);
    function GetFocusedField: IDynamicField;
    procedure SetFocusedField(const Value: IDynamicField);
    {$ENDREGION}

    procedure Initialize;
    procedure BuildTree;

    function FindNode(
      AIdx        : Integer;
      AParentNode : PVirtualNode
    ): PVirtualNode;
    procedure SelectNode(ANode: PVirtualNode); overload;

    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;
    procedure DoNewText(
      Node       : PVirtualNode;
      Column     : TColumnIndex;
      const Text : string
    ); override;
    procedure DoTextDrawing(
      var PaintInfo : TVTPaintInfo;
      const Text    : string;
      CellRect      : TRect;
      DrawFormat    : Cardinal
    ); override;
    procedure DoFreeNode(Node: PVirtualNode); override;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure UpdateColumns;
    procedure Repaint; override;
    procedure Refresh; virtual;
    procedure SetFocusedFieldByName(const AName: string);
    procedure SelectNode(
      AIdx        : Integer;
      AParentNode : PVirtualNode = nil
    ); overload;

    property Data: IDynamicRecord
      read GetData write SetData;

  published
    property Align;
    property Alignment;
    property Anchors;

    property Background;
    property BackgroundOffsetX;
    property BackgroundOffsetY;

    property BiDiMode;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderStyle;
    property BottomSpace;
    property ButtonFillMode;
    property ButtonStyle;
    property BorderWidth;
    property ChangeDelay;
    property ClipboardFormats;
    property Color;
    property Colors;

    property Constraints;
    property Ctl3D;
    property CustomCheckImages;
    property DefaultNodeHeight;
    property DefaultPasteMode;
    property DragCursor;
    property DragHeight;
    property DragKind;
    property DragImageKind;
    property DragMode;
    property DragOperations;
    property DragType;
    property DragWidth;
    property DrawSelectionMode;
    property EditDelay;
    property Enabled;
    property Font;

    property HintMode;
    property HotCursor;
    property Images;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineMode;
    property LineStyle;
    property Margin;

    property OnAddToSelection;
    property OnAdvancedHeaderDraw;
    property OnAfterAutoFitColumn;
    property OnAfterAutoFitColumns;
    property OnAfterCellPaint;
    property OnAfterColumnExport;
    property OnAfterColumnWidthTracking;
    property OnAfterGetMaxColumnWidth;
    property OnAfterHeaderExport;
    property OnAfterHeaderHeightTracking;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterNodeExport;
    property OnAfterPaint;
    property OnAfterTreeExport;
    property OnBeforeAutoFitColumn;
    property OnBeforeAutoFitColumns;
    property OnBeforeCellPaint;
    property OnBeforeColumnExport;
    property OnBeforeColumnWidthTracking;
    property OnBeforeDrawTreeLine;
    property OnBeforeGetMaxColumnWidth;
    property OnBeforeHeaderExport;
    property OnBeforeHeaderHeightTracking;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforeNodeExport;
    property OnBeforePaint;
    property OnBeforeTreeExport;
    property OnCanSplitterResizeColumn;
    property OnCanSplitterResizeHeader;
    property OnCanSplitterResizeNode;
    property OnChange;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnClick;
    property OnColumnDblClick;
    property OnColumnExport;
    property OnColumnResize;
    property OnColumnVisibilityChanged;
    property OnColumnWidthDblClickResize;
    property OnColumnWidthTracking;
    property OnCompareNodes;
    property OnContextPopup;
    property OnCreateDataObject;
    property OnCreateDragManager;
    property OnCreateEditor;
    property OnDblClick;
    property OnDragAllowed;
    property OnDragOver;
    property OnDragDrop;
    property OnDrawHint;
    property OnDrawText;
    property OnEditCancelled;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEndOperation;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnGetCellText;
    property OnGetCellIsEmpty;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnGetText;
    property OnPaintText;
    property OnGetHelpContext;
    property OnGetHintKind;
    property OnGetHintSize;
    property OnGetImageIndex;
    property OnGetImageIndexEx;
    property OnGetImageText;
    property OnGetHint;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
    property OnGetUserClipboardFormats;
    property OnHeaderAddPopupItem;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDragged;
    property OnHeaderDraggedOut;
    property OnHeaderDragging;
    property OnHeaderDraw;
    property OnHeaderDrawQueryElements;
    property OnHeaderHeightDblClickResize;
    property OnHeaderHeightTracking;
    property OnHeaderMouseDown;
    property OnHeaderMouseMove;
    property OnHeaderMouseUp;
    property OnHotChange;
    property OnIncrementalSearch;
    property OnInitChildren;
    property OnInitNode;
    property OnKeyAction;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoadNode;
    property OnLoadTree;
    property OnMeasureItem;
    property OnMeasureTextWidth;
    property OnMeasureTextHeight;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnNewText;
    property OnNodeClick;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeDblClick;
    property OnNodeExport;
    property OnNodeHeightDblClickResize;
    property OnNodeHeightTracking;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnPaintBackground;
    property OnPrepareButtonBitmaps;
    property OnRemoveFromSelection;
    property OnRenderOLEData;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnSaveTree;
    property OnScroll;
    property OnShortenString;
    property OnShowScrollBar;
    property OnBeforeGetCheckState;
    property OnStartDock;
    property OnStartDrag;
    property OnStartOperation;
    property OnStateChange;
    property OnStructureChange;
    property OnUpdating;
    property OnCanResize;
    property OnGesture;
    property Touch;

    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;

    property ScrollBarOptions;
    property SelectionBlendFactor;
    property SelectionCurveRadius;
    property ShowHint;

    property TreeOptions;

    property StateImages;
    property StyleElements;
    property TabOrder;
    property TabStop default True;
    property TextMargin;

    property Visible;
    property WantTabs;

    property ShowHeader: Boolean
      read GetShowHeader write SetShowHeader;

    property ShowGutter: Boolean
      read GetShowGutter write SetShowGutter;

    property Editable: Boolean
      read GetEditable write SetEditable;

    property FocusedField: IDynamicField
      read GetFocusedField write SetFocusedField;

    property MultiSelect: Boolean
      read GetMultiSelect write SetMultiSelect;

    property NameColumn: TVirtualTreeColumn
      read GetNameColumn write SetNameColumn;

    property ValueColumn: TVirtualTreeColumn
      read GetValueColumn write SetValueColumn;
  end;

implementation

uses
  System.SysUtils,
  Vcl.Forms, Vcl.Controls;

const
  GUTTER_COLUMN = 0;
  NAME_COLUMN   = 1;
  VALUE_COLUMN  = 2;

{$REGION 'construction and destruction'}
procedure TValueList.AfterConstruction;
begin
  inherited AfterConstruction;
//  Initialize;
end;

destructor TValueList.Destroy;
begin
  FData := nil;
  inherited Destroy;
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
      if not FInitialized then
        Initialize;
      NodeDataSize := SizeOf(TValueListNode);
      BuildTree;
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

function TValueList.GetMultiSelect: Boolean;
begin
  Result := toMultiSelect in TreeOptions.SelectionOptions;
end;

procedure TValueList.SetMultiSelect(const Value: Boolean);
begin
  if Value <> MultiSelect then
  begin
    if Value then
      TreeOptions.SelectionOptions :=
        TreeOptions.SelectionOptions + [toMultiSelect]
    else
      TreeOptions.SelectionOptions :=
        TreeOptions.SelectionOptions - [toMultiSelect];
  end;
end;

function TValueList.GetNameColumn: TVirtualTreeColumn;
begin
  Result := Header.Columns.Items[NAME_COLUMN];
end;

procedure TValueList.SetNameColumn(const Value: TVirtualTreeColumn);
begin
  Header.Columns.Items[NAME_COLUMN].Assign(Value);
end;

function TValueList.GetValueColumn: TVirtualTreeColumn;
begin
  Result := Header.Columns.Items[VALUE_COLUMN];
end;

procedure TValueList.SetValueColumn(const Value: TVirtualTreeColumn);
begin
  Header.Columns.Items[VALUE_COLUMN].Assign(Value);
end;

function TValueList.GetFocusedField: IDynamicField;
var
  N : TValueListNode;
begin
  N := GetNodeData<TValueListNode>(FocusedNode);
  if Assigned(N) then
    Result := N.Data
  else
    Result := nil;
end;

procedure TValueList.SetFocusedField(const Value: IDynamicField);
var
  VN : PVirtualNode;
  N  : TValueListNode;
begin
  BeginUpdate;
  for VN in Nodes do
  begin
    N := GetNodeData<TValueListNode>(VN);
    if Assigned(N) and (N.Data = Value) then
    begin
      ClearSelection;
      FocusedNode := VN;
      Selected[VN] := True;
      ScrollIntoView(VN, False);
      Break;
    end;
  end;
  EndUpdate;
end;

function TValueList.GetShowGutter: Boolean;
begin
  Result := coVisible in Header.Columns.Items[GUTTER_COLUMN].Options;
end;

procedure TValueList.SetShowGutter(const Value: Boolean);
begin
  if Value <> ShowGutter then
  begin
    FShowGutter := Value;
    UpdateColumns;
  end;
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
procedure TValueList.DoFreeNode(Node: PVirtualNode);
var
  N : TValueListNode;
begin
  N := GetNodeData<TValueListNode>(Node);
  N.Free;
  inherited DoFreeNode(Node);
end;

procedure TValueList.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
var
  N : TValueListNode;
begin
  inherited DoGetText(pEventArgs);
  N := GetNodeData<TValueListNode>(pEventArgs.Node);
  if Assigned(N) then
  begin
    if pEventArgs.Column = NAME_COLUMN then
    begin
      pEventArgs.CellText := N.Data.Name
    end
    else if pEventArgs.Column = VALUE_COLUMN then
    begin
      pEventArgs.CellText := N.Data.Value.ToString;
    end;
  end;
end;

{ Gets called after text has been edited. }

procedure TValueList.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const Text: string);
var
  N : TValueListNode;
begin
  N := GetNodeData<TValueListNode>(Node);
  if Column = NAME_COLUMN then
    N.Data.Name := Text
  else if Column = VALUE_COLUMN then
    N.Data.Value := Text;
  inherited DoNewText(Node, Column, Text);
end;

procedure TValueList.DoTextDrawing(var PaintInfo: TVTPaintInfo;
  const Text: string; CellRect: TRect; DrawFormat: Cardinal);
begin
(*
  { DrawText() Format Flags }
  DT_TOP                  = 0;
  DT_LEFT                 = 0;
  DT_CENTER               = 1;
  DT_RIGHT                = 2;
  DT_VCENTER              = 4;
  DT_BOTTOM               = 8;
  DT_WORDBREAK            = $10;
  DT_SINGLELINE           = $20;
  DT_EXPANDTABS           = $40;
  DT_TABSTOP              = $80;
  DT_NOCLIP               = $100;
  DT_EXTERNALLEADING      = $200;
  DT_CALCRECT             = $400;
  DT_NOPREFIX             = $800;
  DT_INTERNAL             = $1000;
  DT_EDITCONTROL          = $2000;
  DT_PATH_ELLIPSIS        = $4000;
  DT_END_ELLIPSIS         = $8000;
  DT_MODIFYSTRING         = $10000;
  DT_RTLREADING           = $20000;
  DT_WORD_ELLIPSIS        = $40000;
  DT_NOFULLWIDTHCHARBREAK = $0080000;
  DT_HIDEPREFIX           = $00100000;
  DT_PREFIXONLY           = $00200000;
*)
  inherited DoTextDrawing(PaintInfo, Text, CellRect, DrawFormat);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TValueList.BuildTree;
var
  LField : IDynamicField;
begin
  BeginUpdate;
  Clear;
  for LField in FData do
  begin
    TValueListNode.Create(Self, LField);
  end;
  FullExpand;
  EndUpdate;
end;

function TValueList.FindNode(AIdx: Integer;
  AParentNode: PVirtualNode): PVirtualNode;
var
  LNode: PVirtualNode;
begin
  // Helper to find a Node by its index
  Result := nil;
  if Assigned(AParentNode) then
    LNode := GetFirstChild(AParentNode)
  else
    LNode := GetFirst;
  while Assigned(LNode) do
  begin
    if LNode.Index = Cardinal(AIdx) then
    begin
      Result := LNode;
      Break;
    end;
    LNode := GetNextSibling(LNode);
  end;
end;

procedure TValueList.Initialize;
begin
  Header.Options := [
    hoAutoResize, hoColumnResize, hoDblClickResize, hoRestrictDrag,
    hoShowHint, hoShowImages, hoShowSortGlyphs, hoAutoSpring, hoVisible,
    hoDisableAnimatedResize
  ];
  TreeOptions.PaintOptions := [
    toHideFocusRect, toHotTrack, toPopupMode, toShowBackground, toShowButtons,
    toShowDropmark, toStaticBackground, toShowRoot, toShowVertGridLines,
    toThemeAware, toUseBlendedImages, toUseBlendedSelection, toStaticBackground,
    toUseExplorerTheme
  ];
  TreeOptions.AnimationOptions := [];
  TreeOptions.AutoOptions := [
    toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoSort,
    toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale,
    toDisableAutoscrollOnEdit, toAutoBidiColumnOrdering
  ];
  TreeOptions.SelectionOptions := [toExtendedFocus, toFullRowSelect,
    toAlwaysSelectNode];
  // toGridExtensions causes the inline editor to have the full size of the cell
  TreeOptions.MiscOptions := [
    toCheckSupport, toInitOnSave, toWheelPanning, toVariableNodeHeight,
    toEditable, toEditOnDblClick, toGridExtensions
  ];
  TreeOptions.EditOptions := toVerticalEdit;
  with Header.Columns.Add do
  begin
    Color    := clCream;
    MaxWidth := 14;
    MinWidth := 14;
    Options  := [coFixed, coAllowClick, coEnabled, coParentBidiMode, coVisible];
    Position := GUTTER_COLUMN;
    Width    := 14;
    Text     := '';
  end;
  with Header.Columns.Add do
  begin
    Color    := clWhite;
    MaxWidth := 400;
    MinWidth := 80;
    // needs to be coFixed to allow column resizing when no header is shown.
    Options  := [coFixed, coAllowClick, coEnabled, coParentBidiMode, coResizable,
      coVisible, coAutoSpring, coSmartResize, coAllowFocus, coEditable];
    Position := NAME_COLUMN;
    Width    := 100;
    Text     := 'Name';
  end;
  with Header.Columns.Add do
  begin
    MaxWidth    := 800;
    MinWidth    := 50;
    Options     := [coAllowClick, coEnabled, coParentBidiMode, coResizable,
      coVisible, coAutoSpring, coAllowFocus, coEditable];
    Position    := VALUE_COLUMN;
    Width       := 100;
    Text        := 'Value';
    EditOptions := toVerticalEdit;
  end;
  Header.MainColumn    := GUTTER_COLUMN;
  Header.AutoSizeIndex := VALUE_COLUMN;
  Indent               := 0; // pixels between node levels
  Margin               := 0;
  LineMode             := lmBands;
  LineStyle            := lsSolid;
  DrawSelectionMode    := smBlendedRectangle;
  HintMode             := hmTooltip;

  Colors.SelectionRectangleBlendColor := clGray;
  Colors.SelectionTextColor           := clBlack;
  Colors.GridLineColor                := clSilver;
  FInitialized := True;
end;

procedure TValueList.Refresh;
var
  I : Integer;
begin
  if Assigned(FocusedNode) then
    I := FocusedNode.Index
  else
    I := -1;
  Repaint;
  if I >= 0 then
    SelectNode(I);
end;

procedure TValueList.Repaint;
begin
  BuildTree;
  inherited Repaint;
end;

procedure TValueList.SelectNode(ANode: PVirtualNode);
begin
  ClearSelection;
  FocusedNode := ANode;
  Selected[ANode] := True;
  ScrollIntoView(ANode, False);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TValueList.SetFocusedFieldByName(const AName: string);
begin
  FocusedField := Data.Fields[AName];
end;

procedure TValueList.UpdateColumns;
begin
  if Header.Columns.Count > GUTTER_COLUMN then
  begin
    if FShowGutter then
      Header.Columns.Items[GUTTER_COLUMN].Options :=
        Header.Columns.Items[GUTTER_COLUMN].Options + [coVisible]
    else
      Header.Columns.Items[GUTTER_COLUMN].Options :=
        Header.Columns.Items[GUTTER_COLUMN].Options - [coVisible];
  end;
end;

procedure TValueList.SelectNode(AIdx: Integer; AParentNode: PVirtualNode);
var
  LNode : PVirtualNode;
begin
  // Helper to focus and highlight a node by its index
  LNode := FindNode(AIdx, AParentNode);
  if Assigned(LNode) then
    SelectNode(LNode);
end;
{$ENDREGION}

end.

