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

{$I DDuce.inc}

unit DDuce.Components.SectionTree;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList,

  VirtualTrees, VirtualTrees.Types, VirtualTrees.Header;

//const
//  // Helper message to decouple node change handling from edit handling.
//  WM_STARTEDITING = 1000 + 778;
type
  TSectionTree = class(TCustomVirtualStringTree)
  type
    TGetBackColorEvent = procedure(
      ASender        : TBaseVirtualTree;
      AParentNode    : PVirtualNode;
      var ABackColor : TColor
    ) of object;
  const
    {$REGION 'default VST options'}
    DEFAULT_VST_SELECTIONOPTIONS = [
      { Prevent user from selecting with the selection rectangle in multiselect
        mode. }
  //    toDisableDrawSelection,
      { Entries other than in the main column can be selected, edited etc. }
      toExtendedFocus
      { Hit test as well as selection highlight are not constrained to the text
        of a node. }
  //    toFullRowSelect,
      { Constrain selection to the same level as the selection anchor. }
  //    toLevelSelectConstraint,
      { Allow selection, dragging etc. with the middle mouse button. This and
        toWheelPanning are mutual exclusive. }
  //    toMiddleClickSelect,
      { Allow more than one node to be selected. }
  //    toMultiSelect,
      { Allow selection, dragging etc. with the right mouse button. }
  //    toRightClickSelect,
      { Constrain selection to nodes with same parent. }
  //    toSiblingSelectConstraint,
      { Center nodes vertically in the client area when scrolling into view. }
  //    toCenterScrollIntoView,
      { Simplifies draw selection, so a node's caption does not need to intersect
        with the selection rectangle. }
  //    toSimpleDrawSelection
    ];
    DEFAULT_VST_MISCOPTIONS = [
      { Register tree as OLE accepting drop target }
  //    toAcceptOLEDrop,
      { Show checkboxes/radio buttons. }
  //    toCheckSupport,
      { Node captions can be edited. }
  //    toEditable,
      { Fully invalidate the tree when its window is resized (CS_HREDRAW/CS_VREDRAW).}
  //    toFullRepaintOnResize,
      { Use some special enhancements to simulate and support grid behavior. }
      toGridExtensions,
      { Initialize nodes when saving a tree to a stream. }
      toInitOnSave,
      { Tree behaves like TListView in report mode. }
      toReportMode,
      { Toggle node expansion state when it is double clicked. }
      toToggleOnDblClick,
      { Support for mouse panning (wheel mice only). This option and
        toMiddleClickSelect are mutal exclusive, where panning has precedence. }
      toWheelPanning,
      { The tree does not allow to be modified in any way. No action is executed
        and node editing is not possible. }
  //    toReadOnly,
      { When set then GetNodeHeight will trigger OnMeasureItem to allow variable
        node heights. }
      toVariableNodeHeight
      { Start node dragging by clicking anywhere in it instead only on the caption
        or image. Must be used together with toDisableDrawSelection. }
  //    toFullRowDrag,
      { Allows changing a node's height via mouse. }
  //    toNodeHeightResize,
      { Allows to reset a node's height to FDefaultNodeHeight via a double click. }
  //    toNodeHeightDblClickResize,
      { Editing mode can be entered with a single click }
  //    toEditOnClick,
      { Editing mode can be entered with a double click }
  //    toEditOnDblClick
    ];
    DEFAULT_VST_PAINTOPTIONS = [
      { Avoid drawing the dotted rectangle around the currently focused node. }
      toHideFocusRect,
      { Paint tree as would it always have the focus }
  //    toPopupMode,
      { Display collapse/expand buttons left to a node. }
      toShowButtons,
      { Show the dropmark during drag'n drop operations. }
      toShowDropmark,
      { Display horizontal lines to simulate a grid. }
      toShowHorzGridLines,
      { Use the background image if there's one. }
      toShowBackground,
      { Show static background instead of a tiled one. }
      toStaticBackground,
      { Show lines also at top level (does not show the hidden/internal root
        node). }
      toShowRoot,
      { Display tree lines to show hierarchy of nodes. }
      toShowTreeLines,
      { Display vertical lines (depending on columns) to simulate a grid. }
      toShowVertGridLines,
      { Draw UI elements (header, tree buttons etc.) according to the current
        theme if enabled (Windows XP+ only, application must be themed). }
      toThemeAware,
      { Enable alpha blending for ghosted nodes or those which are being
        cut/copied. }
      toUseBlendedImages,
      { Enable alpha blending for node selections. }
      toUseBlendedSelection
    ];
    DEFAULT_VST_HEADEROPTIONS = [
      { Adjust a column so that the header never exceeds the client width of the
        owner control. }
  //    hoAutoResize,
      { Resizing columns with the mouse is allowed. }
      hoColumnResize,
      { Allows a column to resize itself to its largest entry. }
      hoDblClickResize,
      { Dragging columns is allowed. }
  //    hoDrag,
      { Header captions are highlighted when mouse is over a particular column. }
  //    hoHotTrack,
      { Header items with the owner draw style can be drawn by the application
        via event. }
  //    hoOwnerDraw,
      { Header can only be dragged horizontally. }
  //    hoRestrictDrag,
      { Show application defined header hint. }
      hoShowHint,
      { Show header images. }
      hoShowImages,
      { Allow visible sort glyphs. }
  //    hoShowSortGlyphs,
      { Distribute size changes of the header to all columns, which are sizable
        and have the coAutoSpring option enabled. hoAutoResize must be enabled
        too. }
  //    hoAutoSpring,
      { Fully invalidate the header (instead of subsequent columns only) when a
        column is resized. }
      hoFullRepaintOnResize,
      { Disable animated resize for all columns. }
      hoDisableAnimatedResize,
      { Allow resizing header height via mouse. }
  //    hoHeightResize,
      { Allow the header to resize itself to its default height. }
  //    hoHeightDblClickResize
      { Header is visible. }
      hoVisible
    ];
    DEFAULT_VST_STRINGOPTIONS = [
      { If set then the caption is automatically saved with the tree node,
        regardless of what is saved in the user data. }
  //    toSaveCaptions,
      { Show static text in a caption which can be differently formatted than the
        caption but cannot be edited. }
  //    toShowStaticText,
      { Automatically accept changes during edit if the user finishes editing
        other then VK_RETURN or ESC. If not set then changes are cancelled. }
  //    toAutoAcceptEditChange
    ];
    DEFAULT_VST_ANIMATIONOPTIONS = [
      { Expanding and collapsing a node is animated (quick window scroll). }
  //    toAnimatedToggle,
      { Do some advanced animation effects when toggling a node. }
  //    toAdvancedAnimatedToggle
    ];
    DEFAULT_VST_AUTOOPTIONS = [
      { Expand node if it is the drop target for more than a certain time. }
      toAutoDropExpand,
      { Nodes are expanded (collapsed) when getting (losing) the focus. }
  //    toAutoExpand,
      { Scroll if mouse is near the border while dragging or selecting. }
      toAutoScroll,
      { Scroll as many child nodes in view as possible after expanding a node. }
      toAutoScrollOnExpand,
      { Sort tree when Header.SortColumn or Header.SortDirection change or sort
        node if child nodes are added. }
  //    toAutoSort,
      { Large entries continue into next column(s) if there's no text in them
        (no clipping). }
      toAutoSpanColumns,
      { Checkstates are automatically propagated for tri state check boxes. }
      toAutoTristateTracking,
      { Node buttons are hidden when there are child nodes, but all are invisible.}
  //    toAutoHideButtons,
      { Delete nodes which where moved in a drag operation (if not directed
        otherwise). }
      toAutoDeleteMovedNodes,
      { Disable scrolling a node or column into view if it gets focused. }
  //    toDisableAutoscrollOnFocus,
      { Change default node height automatically if the system's font scale is
        set to big fonts. }
      toAutoChangeScale,
      { Frees any child node after a node has been collapsed (HasChildren flag
        stays there). }
  //    toAutoFreeOnCollapse,
      { Do not center a node horizontally when it is edited. }
      toDisableAutoscrollOnEdit,
      { When set then columns (if any exist) will be reordered from lowest index
        to highest index and vice versa when the tree's bidi mode is changed. }
      toAutoBidiColumnOrdering
    ];
  {$ENDREGION}
  private
    FOnGetBackColor: TGetBackColorEvent;

    {$REGION 'property access methods'}
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(const AValue: TStringTreeOptions);
    {$ENDREGION}

    //procedure WMChar(var Message: TWMChar); message WM_CHAR;

  protected
    procedure BuildTree; virtual;
    {$REGION 'TVirtualStringTree overrides'}
    function GetOptionsClass: TTreeOptionsClass; override;
    procedure DoInitNode(
      Parent, ANode  : PVirtualNode;
      var InitStates : TVirtualNodeInitStates
    ); override;
//    function DoCreateEditor(
//      Node   : PVirtualNode;
//      Column : TColumnIndex
//    ): IVTEditLink; override;

//    procedure KeyDown(
//      var Key : Word;
//      Shift   : TShiftState
//    ); override;
    procedure DoBeforeCellPaint(
      Canvas          : TCanvas;
      ANode           : PVirtualNode;
      Column          : TColumnIndex;
      CellPaintMode   : TVTCellPaintMode;
      CellRect        : TRect;
      var ContentRect : TRect
    ); override;
    procedure DoAfterCellPaint(
      Canvas   : TCanvas;
      Node     : PVirtualNode;
      Column   : TColumnIndex;
      CellRect : TRect
    ); override;
    procedure DoMeasureItem(
      TargetCanvas   : TCanvas;
      Node           : PVirtualNode;
      var NodeHeight : Integer
    ); override;
    {$ENDREGION}

    {$REGION 'event dispatch methods'}
    procedure DoGetBackColor(
      ANode          : PVirtualNode;
      var ABackColor : TColor
    ); virtual;
    {$ENDREGION}

    // message handlers
//    procedure WMStartEditing(var AMessage: TMessage); message WM_STARTEDITING;

  public
    procedure AfterConstruction; override;

  published
    {$REGION 'published properties'}
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AnimationDuration;
    property AutoExpandDelay;
    property AutoScrollDelay;
    property AutoScrollInterval;
    property Background;
    property BackgroundOffsetX;
    property BackgroundOffsetY;
    property BiDiMode;
    property BorderStyle;
    property ButtonFillMode;
    property ButtonStyle;
    property BorderWidth;
    property ChangeDelay;
    property CheckImageKind;
    property ClipboardFormats;
    property Color;
    property Colors;
    property Constraints;
    property CustomCheckImages;
    property DefaultPasteMode;
    property DefaultText;
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
    property Header;
    property HintMode;
    property HotCursor;
    property Images;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    //property LineMode;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property NodeDataSize;
    property OperationCanceled;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RootNodeCount;
    property ScrollBarOptions;
    property SelectionBlendFactor;
    property SelectionCurveRadius;
    property ShowHint;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    property Visible;
    property WantTabs;

    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property Ctl3D;
    property ParentCtl3D;

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
    property OnBeforeGetMaxColumnWidth;
    property OnBeforeHeaderExport;
    property OnBeforeHeaderHeightTracking;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforeNodeExport;
    property OnBeforePaint;
    property OnBeforeTreeExport;
    property OnCanSplitterResizeColumn;
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
    property OnDrawText;
    property OnEditCancelled;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnGetCellIsEmpty;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnGetText;
    property OnPaintText;
    property OnGetHelpContext;
    property OnGetImageIndex;
    property OnGetImageIndexEx;
    property OnGetImageText;
    property OnGetHint;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
    property OnGetUserClipboardFormats;
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
    property OnMeasureItem;
    property OnMeasureTextWidth;
    property OnMeasureTextHeight;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnNewText;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeExport;
    property OnNodeHeightDblClickResize;
    property OnNodeHeightTracking;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnPaintBackground;
    property OnRenderOLEData;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnScroll;
    property OnShortenString;
    property OnShowScrollbar;
    property OnStartDock;
    property OnStartDrag;
    property OnStateChange;
    property OnStructureChange;
    property OnUpdating;

    property OnGetBackColor: TGetBackColorEvent
      read FOnGetBackColor write FOnGetBackColor;

    property TreeOptions : TStringTreeOptions
      read GetOptions write SetOptions;
    {$ENDREGION}
  end;

implementation

//type
//  TVKSet = set of Byte;
//
//var
//  VK_EDIT_KEYS : TVKSet = [
//    Ord('0')..Ord('Z'),
//    VK_OEM_1..VK_OEM_102,
//    VK_MULTIPLY..VK_DIVIDE
//  ];

{$REGION 'construction and destruction'}
procedure TSectionTree.AfterConstruction;
begin
  inherited AfterConstruction;
  Color                        := clWhite;
  Header.Height                := 18;
  DefaultNodeHeight            := 18;
  Indent                       := 18;
  LineStyle                    := lsSolid;
  LineMode                     := lmBands;
  DragType                     := dtVCL; // dtOLE not supported yet
  DragOperations               := [doMove];
  Margin                       := 0;
  DrawSelectionMode            := smBlendedRectangle;
  HintMode                     := hmHintAndDefault;
  WantTabs                     := True;
  DefaultPasteMode             := amInsertAfter;
  EditDelay                    := 0;
  IncrementalSearch            := isNone;
  Colors.GridLineColor         := clSilver;

  Header.Options               := DEFAULT_VST_HEADEROPTIONS;
  TreeOptions.SelectionOptions := DEFAULT_VST_SELECTIONOPTIONS;
  TreeOptions.MiscOptions      := DEFAULT_VST_MISCOPTIONS;
  TreeOptions.PaintOptions     := DEFAULT_VST_PAINTOPTIONS;
  TreeOptions.StringOptions    := DEFAULT_VST_STRINGOPTIONS;
  TreeOptions.AnimationOptions := DEFAULT_VST_ANIMATIONOPTIONS;
  TreeOptions.AutoOptions      := DEFAULT_VST_AUTOOPTIONS;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TSectionTree.GetOptions: TStringTreeOptions;
begin
  Result := inherited TreeOptions as TStringTreeOptions;
end;

procedure TSectionTree.SetOptions(const AValue: TStringTreeOptions);
begin
  inherited TreeOptions.Assign(AValue);
end;
{$ENDREGION}

{$REGION 'message handlers'}
{ This message was posted by ourselves from the node change handler above to
  decouple that change event and our intention to start editing a node. This
  is necessary to avoid interferences between nodes editors potentially created
  for an old edit action and the new one we start here. }

//procedure TSectionTree.WMStartEditing(var AMessage: TMessage);
//var
//  Node: PVirtualNode;
//begin
//  Node := Pointer(AMessage.WParam);
//  { Note: the test whether a node can really be edited is done in the
//    OnEditing event. }
//  EditNode(Node, 1);
//end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TSectionTree.DoInitNode(Parent, ANode: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
begin
  Include(ANode.States, vsInitialized);
  Include(ANode.States, vsMultiline);
  Include(ANode.States, vsHeightMeasured);
  if not Assigned(Parent) then
    Include(InitStates, ivsExpanded);

  inherited DoInitNode(Parent, ANode, InitStates);
end;

procedure TSectionTree.DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
  var NodeHeight: Integer);
var
  N : Cardinal;
begin
  inherited DoMeasureItem(TargetCanvas, Node, NodeHeight);
  N := ComputeNodeHeight(TargetCanvas, Node, 0);
  if N > (DefaultNodeHeight + 5) then
  begin
    NodeHeight := N;
  end;
end;

//function TSectionTree.DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex)
//  : IVTEditLink;
//begin
//end;

procedure TSectionTree.DoGetBackColor(ANode: PVirtualNode; var ABackColor: TColor);
begin
  if Assigned(FOnGetBackColor) then
    FOnGetBackColor(Self, ANode, ABackColor);
end;

//procedure TSectionTree.KeyDown(var Key: Word; Shift: TShiftState);
//var
//  M : TMessage;
//begin
//  inherited KeyDown(Key, Shift);
//  if not (tsEditing in TreeStates) and (Shift = []) and (Key in VK_EDIT_KEYS) then
//  begin
//    SendMessage(Self.Handle, WM_STARTEDITING, NativeUint(FocusedNode), 0);
//    M.Result := 0;
//    M.msg := WM_KEYDOWN;
//    M.wParam := Key;
//    M.lParam := 0;
//    EditLink.ProcessMessage(M);
//  end;
//end;

procedure TSectionTree.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);
var
  LColor  : TColor;
  LIndent : Integer;
begin
//  if vsSelected in Node.States then
//  begin
//    LColor := clYellow;
//    if LineMode = lmBands then
//    begin
//      if Column = Header.MainColumn then
//      begin
//        LIndent := GetNodeLevel(Node) * Indent;
//        Inc(CellRect.Left, LIndent);
//        LIndent := -Integer(Indent);
//      end
//      else
//      begin
//        LIndent := 0;
//      end;
//
//      if LColor <> Color then
//      begin // fill cell
//        Canvas.Brush.Color := LColor;
//        Canvas.FillRect(CellRect);
//      end;
//
//      if Column = Header.MainColumn then
//      begin
//        CellRect.Right := CellRect.Left + Integer(Indent);
//        Inc(CellRect.Bottom);
//        repeat
//          if LColor <> Color then
//          begin // fill vertical band
//            Canvas.Brush.Color := LColor;
//            Canvas.FillRect(CellRect);
//          end;
//
//          Node := Node.Parent;
//          if not Assigned(Node) or (Node = RootNode) then
//            Break;
//
//          Inc(CellRect.Left, LIndent);
//          Inc(CellRect.Right, LIndent);
//        until False;
//      end;
//    end;
//
//
//  end;
  inherited;
end;

procedure TSectionTree.DoBeforeCellPaint(Canvas: TCanvas; ANode: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  LColor  : TColor;
  LIndent : Integer;
begin
  LColor := clWhite;
  if LineMode = lmBands then
  begin
    if Column = Header.MainColumn then
    begin
      LIndent := GetNodeLevel(ANode) * Indent;
      Inc(CellRect.Left, LIndent);
      LIndent := -Integer(Indent);
    end
    else
    begin
      LIndent := 0;
    end;

    DoGetBackColor(ANode, LColor);

      if vsSelected in ANode.States then
      begin
        LColor := clYellow;
      end;

    if LColor <> Color then
    begin // fill cell
      Canvas.Brush.Color := LColor;
      Canvas.FillRect(CellRect);
    end;

    if Column = Header.MainColumn then
    begin
      CellRect.Right := CellRect.Left + Integer(Indent);
      Inc(CellRect.Bottom);
      repeat
        if vsSelected in ANode.States then
        begin
          LColor := clYellow;
        end;
        if LColor <> Color then
        begin // fill vertical band
          Canvas.Brush.Color := LColor;
          Canvas.FillRect(CellRect);
        end;

        ANode := ANode.Parent;
        if not Assigned(ANode) or (ANode = RootNode) then
          Break;

        Inc(CellRect.Left, LIndent);
        Inc(CellRect.Right, LIndent);
        DoGetBackColor(ANode, LColor);
      until False;
    end;
  end;
  inherited;
end;
{$ENDREGION}

{$REGION 'private methods'}
//procedure TSectionTree.WMChar(var Message: TWMChar);
//begin
//  with Message do
//    if (CharCode in [Ord(^H), 32..255] -
//        [VK_HOME, VK_END, VK_PRIOR, VK_NEXT, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_BACK, VK_TAB,
//         VK_ADD, VK_SUBTRACT, VK_MULTIPLY, VK_DIVIDE, VK_ESCAPE, VK_SPACE, Ord('+'), Ord('-'), Ord('*'), Ord('/')])
//        and not Assigned(EditLink) then
//      if Assigned(FocusedNode) and EditNode(FocusedNode, FocusedColumn) and Assigned(EditLink) then
//      begin
//        EditLink.ProcessMessage(TMessage(Message));
//        Message.CharCode := 0;
//      end;
//  inherited;
//end;
{$ENDREGION}

{$REGION 'protected methods'}
function TSectionTree.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TStringTreeOptions;
end;

procedure TSectionTree.BuildTree;
begin
// intended to be overridden in descendants.
end;
{$ENDREGION}

end.
