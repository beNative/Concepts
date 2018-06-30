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

unit DDuce.Factories.VirtualTrees;

interface

uses
  System.Classes, System.UITypes,
  Vcl.Controls,

  Spring,

  VirtualTrees;

{ TVSTOptions is a settings container which holds the most commonly adjusted
  properties of the TVirtualStringTree component. }

type
  TVSTOptions = class(TPersistent)
  strict private
    FHeaderOptions                : TVTHeaderOptions;
    FPaintOptions                 : TVTPaintOptions;
    FAnimationOptions             : TVTAnimationOptions;
    FAutoOptions                  : TVTAutoOptions;
    FStringOptions                : TVTStringOptions;
    FSelectionOptions             : TVTSelectionOptions;
    FMiscOptions                  : TVTMiscOptions;
    FColumnOptions                : TVTColumnOptions;
    FLineStyle                    : TVTLineStyle;     // style of the tree lines
    FLineMode                     : TVTLineMode;      // tree lines or bands etc.
    FDrawSelectionMode            : TVTDrawSelectionMode;
    FHintMode                     : TVTHintMode;
    FSelectionTextColor           : TColor;
    FSelectionRectangleBlendColor : TColor;
    FGridLineColor                : TColor;

  public
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    property HeaderOptions: TVTHeaderOptions
      read FHeaderOptions write FHeaderOptions;

    property PaintOptions: TVTPaintOptions
      read FPaintOptions write FPaintOptions;

    property AnimationOptions: TVTAnimationOptions
      read FAnimationOptions write FAnimationOptions;

    property AutoOptions: TVTAutoOptions
      read FAutoOptions write FAutoOptions;

    property StringOptions: TVTStringOptions
      read FStringOptions write FStringOptions;

    property SelectionOptions: TVTSelectionOptions
      read FSelectionOptions write FSelectionOptions;

    property MiscOptions: TVTMiscOptions
      read FMiscOptions write FMiscOptions;

    property ColumnOptions: TVTColumnOptions // TS: todo default column options
      read FColumnOptions write FColumnOptions;

    property LineStyle: TVTLineStyle
      read FLineStyle write FLineStyle;

    property LineMode: TVTLineMode
      read FLineMode write FLineMode;

    property DrawSelectionMode: TVTDrawSelectionMode
      read FDrawSelectionMode write FDrawSelectionMode;

    property HintMode: TVTHintMode
      read FHintMode write FHintMode;

    { background color for selection. }
    property SelectionRectangleBlendColor: TColor
      read FSelectionRectangleBlendColor write FSelectionRectangleBlendColor;

    { font color for text in selection. }
    property SelectionTextColor: TColor
      read FSelectionTextColor write FSelectionTextColor;

    property GridLineColor: TColor
      read FGridLineColor write FGridLineColor;
  end;

type
  TVirtualStringTreeFactory = class sealed
  private class var
    FDefaultTreeOptions     : Lazy<TVSTOptions>;
    FDefaultGridOptions     : Lazy<TVSTOptions>;
    FDefaultListOptions     : Lazy<TVSTOptions>;
    FDefaultTreeGridOptions : Lazy<TVSTOptions>;
    FDefaultTreeListOptions : Lazy<TVSTOptions>;

    class function GetDefaultGridOptions: TVSTOptions; static;
    class function GetDefaultListOptions: TVSTOptions; static;
    class function GetDefaultTreeGridOptions: TVSTOptions; static;
    class function GetDefaultTreeListOptions: TVSTOptions; static;
    class function GetDefaultTreeOptions: TVSTOptions; static;

  public
    class constructor Create;

    class function Create(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TVirtualStringTree;

    class function CreateTree(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TVirtualStringTree;

    class function CreateList(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TVirtualStringTree;

    class function CreateGrid(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TVirtualStringTree;

    class function CreateTreeGrid(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TVirtualStringTree;

    class function CreateTreeList(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TVirtualStringTree;

    class property DefaultTreeOptions: TVSTOptions
      read GetDefaultTreeOptions;

    class property DefaultGridOptions: TVSTOptions
      read GetDefaultGridOptions;

    class property DefaultListOptions: TVSTOptions
      read GetDefaultListOptions;

    class property DefaultTreeGridOptions: TVSTOptions
      read GetDefaultTreeGridOptions;

    class property DefaultTreeListOptions: TVSTOptions
      read GetDefaultTreeListOptions;
  end;

implementation

uses
  System.SysUtils,
  Vcl.Graphics;

type
  TCustomVirtualStringTreeAccess = class(TCustomVirtualStringTree) end;

{$REGION 'TVSTOptions'}
procedure TVSTOptions.Assign(Source: TPersistent);
var
  LOptions : TVSTOptions;
begin
  if Source is TVSTOptions then
  begin
    LOptions := TVSTOptions(Source);
    AnimationOptions             := LOptions.AnimationOptions;
    AutoOptions                  := LOptions.AutoOptions;
    MiscOptions                  := LOptions.MiscOptions;
    PaintOptions                 := LOptions.PaintOptions;
    SelectionOptions             := LOptions.SelectionOptions;
    HeaderOptions                := LOptions.HeaderOptions;
    LineStyle                    := LOptions.LineStyle;
    LineMode                     := LOptions.LineMode;
    DrawSelectionMode            := LOptions.DrawSelectionMode;
    HintMode                     := LOptions.HintMode;
    SelectionTextColor           := LOptions.SelectionTextColor;
    GridLineColor                := LOptions.GridLineColor;
    SelectionRectangleBlendColor := LOptions.SelectionRectangleBlendColor;
    StringOptions                := LOptions.StringOptions;
  end
  else
    inherited Assign(Source);
end;

procedure TVSTOptions.AssignTo(Dest: TPersistent);
var
  LTree    : TCustomVirtualStringTreeAccess;
  LOptions : TVSTOptions;
begin
  if Dest is TCustomVirtualStringTree then
  begin
    LTree := TCustomVirtualStringTreeAccess(Dest);
    LTree.TreeOptions.AnimationOptions        := AnimationOptions;
    LTree.TreeOptions.AutoOptions             := AutoOptions;
    LTree.TreeOptions.MiscOptions             := MiscOptions;
    LTree.TreeOptions.PaintOptions            := PaintOptions;
    LTree.TreeOptions.SelectionOptions        := SelectionOptions;
    LTree.Header.Options                      := HeaderOptions;
    LTree.LineStyle                           := LineStyle;
    LTree.LineMode                            := LineMode;
    LTree.DrawSelectionMode                   := DrawSelectionMode;
    LTree.HintMode                            := HintMode;
    LTree.Colors.SelectionTextColor           := SelectionTextColor;
    LTree.Colors.GridLineColor                := GridLineColor;
    LTree.Colors.SelectionRectangleBlendColor := SelectionRectangleBlendColor;
    TStringTreeOptions(LTree.TreeOptions).StringOptions := StringOptions;
  end
  else if Dest is TVSTOptions then
  begin
    LOptions                              := TVSTOptions(Dest);
    LOptions.AnimationOptions             := AnimationOptions;
    LOptions.AutoOptions                  := AutoOptions;
    LOptions.MiscOptions                  := MiscOptions;
    LOptions.PaintOptions                 := PaintOptions;
    LOptions.SelectionOptions             := SelectionOptions;
    LOptions.HeaderOptions                := HeaderOptions;
    LOptions.LineStyle                    := LineStyle;
    LOptions.LineMode                     := LineMode;
    LOptions.DrawSelectionMode            := DrawSelectionMode;
    LOptions.HintMode                     := HintMode;
    LOptions.SelectionTextColor           := SelectionTextColor;
    LOptions.GridLineColor                := GridLineColor;
    LOptions.SelectionRectangleBlendColor := SelectionRectangleBlendColor;
    LOptions.StringOptions                := StringOptions;
  end
  else
    inherited AssignTo(Dest);
end;
{$ENDREGION}

{$REGION 'TVirtualStringTree settings'}
{
  DefaultPaintOptions = [
    toShowButtons,
    toShowDropmark,
    toShowTreeLines,
    toShowRoot,
    toThemeAware,
    toUseBlendedImages
  ];
  DefaultAnimationOptions = [
  ];
  DefaultAutoOptions      = [
    toAutoDropExpand,
    toAutoTristateTracking,
    toAutoScrollOnExpand,
    toAutoDeleteMovedNodes,
    toAutoChangeScale,
    toAutoSort
  ];
  DefaultSelectionOptions = [
  ];
  DefaultMiscOptions = [
    toAcceptOLEDrop,
    toFullRepaintOnResize,
    toInitOnSave,
    toToggleOnDblClick,
    toWheelPanning,
    toEditOnClick
  ];
  DefaultColumnOptions = [
    coAllowClick,
    coDraggable,
    coEnabled,
    coParentColor,
    coParentBidiMode,
    coResizable,
    coShowDropmark,
    coVisible,
    coAllowFocus,
    coEditable
  ];
 }

//const
//  DEFAULT_VST_SELECTIONOPTIONS = [
    { Prevent user from selecting with the selection rectangle in multiselect
      mode. }
//    toDisableDrawSelection,
    {  Entries other than in the main column can be selected, edited etc. }
//    toExtendedFocus,
    { Hit test as well as selection highlight are not constrained to the text
      of a node. }
//    toFullRowSelect
    { Constrain selection to the same level as the selection anchor. }
//    toLevelSelectConstraint,
    { Allow selection, dragging etc. with the middle mouse button. This and
      toWheelPanning are mutual exclusive. }
//    toMiddleClickSelect,
    { Allow more than one node to be selected. }
//    toMultiSelect,
    {  Allow selection, dragging etc. with the right mouse button. }
//    toRightClickSelect,
    { Constrain selection to nodes with same parent. }
//    toSiblingSelectConstraint,
    { Center nodes vertically in the client area when scrolling into view. }
//    toCenterScrollIntoView
    { Simplifies draw selection, so a node's caption does not need to intersect
      with the selection rectangle. }
//    toSimpleDrawSelection
//  ];
//  DEFAULT_VST_MISCOPTIONS = [
    { Register tree as OLE accepting drop target }
//    toAcceptOLEDrop,
    { Show checkboxes/radio buttons. }
//    toCheckSupport,
    { Node captions can be edited. }
//    toEditable,
    { Fully invalidate the tree when its window is resized
      (CS_HREDRAW/CS_VREDRAW). }
//    toFullRepaintOnResize,
    { Use some special enhancements to simulate and support grid behavior. }
    { This also changes how the selection is drawn. }
//    toGridExtensions,
    { Initialize nodes when saving a tree to a stream. }
//  toInitOnSave,
    { Tree behaves like TListView in report mode. }
//    toReportMode,
    { Toggle node expansion state when it is double clicked. }
//  toToggleOnDblClick,
    { Support for mouse panning (wheel mice only). This option and
      toMiddleClickSelect are mutal exclusive, where panning has precedence. }
//  toWheelPanning,
    { The tree does not allow to be modified in any way. No action is executed
      and node editing is not possible. }
//    toReadOnly,
    { When set then GetNodeHeight will trigger OnMeasureItem to allow variable
      node heights. }
//  toVariableNodeHeight
    { Start node dragging by clicking anywhere in it instead only on the
      caption or image. Must be used together with toDisableDrawSelection. }
//    toFullRowDrag,
    { Allows changing a node's height via mouse. }
//    toNodeHeightResize,
    { Allows to reset a node's height to FDefaultNodeHeight via a double click. }
//    toNodeHeightDblClickResize,
    { Editing mode can be entered with a single click }
//    toEditOnClick,
    { Editing mode can be entered with a double click }
//    toEditOnDblClick
//  ];
//  DEFAULT_VST_PAINTOPTIONS = [
    { Avoid drawing the dotted rectangle around the currently focused node. }
//  toHideFocusRect,
    { Selected nodes are drawn as unselected nodes if the tree is unfocused. }
//  toHideSelection,
    { Track which node is under the mouse cursor. Assigns a background color
      if used in combination with toUseExplorerTheme. }
//    toHotTrack,
    { Paint tree as would it always have the focus }
//  toPopupMode,
    { Use the background image if there's one. }
//  toShowBackground,
    { Display collapse/expand buttons left to a node. }
//  toShowButtons,
    { Show the dropmark during drag'n drop operations. }
//  toShowDropmark,
    { Display horizontal lines to simulate a grid. }
//    toShowHorzGridLines,
    { Show static background instead of a tiled one. }
//  toStaticBackground,
    { Show lines also at top level (does not show the hidden/internal root
      node). }
//  toShowRoot,
    { Display tree lines to show hierarchy of nodes. }
//    toShowTreeLines,
    { Display vertical lines (depending on columns) to simulate a grid. }
//  toShowVertGridLines,
    { Draw UI elements (header, tree buttons etc.) according to the current
      theme if enabled (Windows XP+ only, application must be themed). }
//  toThemeAware,
    { Enable alpha blending for ghosted nodes or those which are being
      cut/copied. }
//  toUseBlendedImages,
    { Ghosted images are still shown as ghosted if unfocused (otherwise they
      become non-ghosted images). }
//    toGhostedIfUnfocused,
    { Display vertical lines over the full client area, not only the space
      occupied by nodes. This option only has an effect if toShowVertGridLines
      is enabled too. }
//    toFullVertGridLines,
    { Do not draw node selection, regardless of focused state. }
//    toAlwaysHideSelection,
    { Enable alpha blending for node selections. }
//  toUseBlendedSelection,
    { Show simple static background instead of a tiled one. }
//  toStaticBackground,
    { Display child nodes above their parent. }
//    toChildrenAbove,
    { Draw the tree with a fixed indent. }
//    toFixedIndent,
    { Use the explorer theme if run under Windows Vista (or above). }
//    toUseExplorerTheme
    { Do not show tree lines if theming is used. }
//    toHideTreeLinesIfThemed
    { Draw nodes even if they are filtered out. }
//    toShowFilteredNodes
//  ];
//  DEFAULT_VST_HEADEROPTIONS = [
    { Adjust a column so that the header never exceeds the client width of the
      owner control. }
//  hoAutoResize,
    { Resizing columns with the mouse is allowed. }
//  hoColumnResize,
    { Allows a column to resize itself to its largest entry. }
//  hoDblClickResize,
    { Dragging columns is allowed. }
//    hoDrag,
    { Header captions are highlighted when mouse is over a particular column. }
//    hoHotTrack,
    { Header items with the owner draw style can be drawn by the application
      via event. }
//    hoOwnerDraw,
    { Header can only be dragged horizontally. }
//  hoRestrictDrag,
    { Show application defined header hint. }
//  hoShowHint,
    { Show header images. }
//  hoShowImages,
    { Allow visible sort glyphs. }
//  hoShowSortGlyphs,
    { Distribute size changes of the header to all columns, which are sizable
      and have the coAutoSpring option enabled. hoAutoResize must be enabled
      too. }
//  hoAutoSpring,
    { Fully invalidate the header (instead of subsequent columns only) when a
      column is resized. }
//    hoFullRepaintOnResize,
    { Disable animated resize for all columns. }
//  hoDisableAnimatedResize,
    { Allow resizing header height via mouse. }
//    hoHeightResize,
    { Allow the header to resize itself to its default height. }
//    hoHeightDblClickResize
    { Header is visible. }
//  hoVisible
//  ];
//  DEFAULT_VST_STRINGOPTIONS = [
    { If set then the caption is automatically saved with the tree node,
      regardless of what is saved in the user data. }
    //toSaveCaptions,
    { Show static text in a caption which can be differently formatted than the
      caption but cannot be edited. }
    //toShowStaticText,
    { Automatically accept changes during edit if the user finishes editing
      other then VK_RETURN or ESC. If not set then changes are cancelled. }
//    toAutoAcceptEditChange
//  ];
//  DEFAULT_VST_ANIMATIONOPTIONS = [
    { Expanding and collapsing a node is animated (quick window scroll). }
//    toAnimatedToggle,
    { Do some advanced animation effects when toggling a node. }
//    toAdvancedAnimatedToggle
//  ];
//  DEFAULT_VST_AUTOOPTIONS = [
    { Expand node if it is the drop target for more than a certain time. }
//  toAutoDropExpand,
    { Nodes are expanded (collapsed) when getting (losing) the focus. }
//    toAutoExpand,
    { Scroll if mouse is near the border while dragging or selecting. }
//  toAutoScroll,
    { Scroll as many child nodes in view as possible after expanding a node. }
//  toAutoScrollOnExpand,
    { Sort tree when Header.SortColumn or Header.SortDirection change or sort
      node if child nodes are added. }
//  toAutoSort,
    { Large entries continue into next column(s) if there's no text in them
      (no clipping). }
//    toAutoSpanColumns,
    { Checkstates are automatically propagated for tri state check boxes. }
//  toAutoTristateTracking,
    { Node buttons are hidden when there are child nodes, but all are invisible.}
//    toAutoHideButtons,
    { Delete nodes which where moved in a drag operation (if not directed
      otherwise). }
//  toAutoDeleteMovedNodes,
    { Disable scrolling a node or column into view if it gets focused. }
//    toDisableAutoscrollOnFocus,
    { Change default node height automatically if the system's font scale is
      set to big fonts. }
//  toAutoChangeScale,
    { Frees any child node after a node has been collapsed (HasChildren flag
      stays there). }
//    toAutoFreeOnCollapse,
    { Do not center a node horizontally when it is edited. }
//  toDisableAutoscrollOnEdit,
    { When set then columns (if any exist) will be reordered from lowest index
      to highest index and vice versa when the tree's bidi mode is changed. }
//    toAutoBidiColumnOrdering
//  ];
{$ENDREGION}

{$REGION 'construction and destruction'}
class constructor TVirtualStringTreeFactory.Create;
begin
  FDefaultTreeOptions.Create(
    function: TVSTOptions
    begin
      Result := TVSTOptions.Create;
      with Result do
      begin
        HeaderOptions := [
          hoAutoResize, hoRestrictDrag, hoShowHint, hoShowImages, hoShowSortGlyphs,
          hoHeightResize, hoHeightDblClickResize, hoVisible
        ];
        PaintOptions := [
          toHideFocusRect, toHideSelection, toPopupMode, toShowBackground,
          toShowButtons, toShowDropmark, toShowRoot, toThemeAware,
          toUseBlendedImages, toUseBlendedSelection, toStaticBackground
        ];
        AnimationOptions := [];
        AutoOptions := [
          toAutoScroll, toAutoSort, toAutoDeleteMovedNodes, toAutoChangeScale,
          toDisableAutoscrollOnEdit, toAutoBidiColumnOrdering
        ];
        StringOptions := [toAutoAcceptEditChange];
        SelectionOptions := [toExtendedFocus];
        MiscOptions := [
          toInitOnSave, toToggleOnDblClick, toWheelPanning, toVariableNodeHeight,
          toCheckSupport
        ];
        ColumnOptions := [];

        LineStyle                    := lsDotted;
        LineMode                     := lmNormal;
        DrawSelectionMode            := smBlendedRectangle;
        HintMode                     := hmTooltip;
        SelectionRectangleBlendColor := clGray;
        SelectionTextColor           := clBlack;
        GridLineColor                := clSilver;
      end;
    end,
    True
  );

  FDefaultGridOptions.Create(
    function: TVSTOptions
    begin
      Result := TVSTOptions.Create;
      with Result do
      begin
        HeaderOptions := [
          hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoRestrictDrag,
          hoShowHint, hoShowImages, hoShowSortGlyphs, hoAutoSpring,
          hoDisableAnimatedResize, hoVisible
        ];
        PaintOptions := [
          toHideFocusRect, toHideSelection, toHotTrack, toPopupMode,
          toShowBackground, toShowButtons, toShowDropmark, toShowRoot,
          toShowVertGridLines, toShowHorzGridLines, toThemeAware, toUseBlendedImages,
          toUseBlendedSelection, toStaticBackground, toUseExplorerTheme
        ];
        AnimationOptions := [];
        AutoOptions := [
          toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoSort,
          toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale,
          toDisableAutoscrollOnEdit, toAutoBidiColumnOrdering
        ];
        StringOptions := [toAutoAcceptEditChange];
        SelectionOptions := [toExtendedFocus, toFullRowSelect];
        MiscOptions := [
          toCheckSupport, toInitOnSave, toToggleOnDblClick, toWheelPanning,
          toVariableNodeHeight{, toGridExtensions}
        ];
        ColumnOptions := [];

        LineStyle                    := lsSolid;
        LineMode                     := lmBands;
        DrawSelectionMode            := smBlendedRectangle;
        HintMode                     := hmTooltip;
        SelectionRectangleBlendColor := clGray;
        SelectionTextColor           := clBlack;
        GridLineColor                := clSilver;
      end;
    end,
    True
  );

  FDefaultListOptions.Create(
    function: TVSTOptions
    begin
      Result := TVSTOptions.Create;
      with Result do
      begin
        HeaderOptions := [
          hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoRestrictDrag,
          hoShowHint, hoShowImages, hoShowSortGlyphs, hoAutoSpring,
          hoDisableAnimatedResize, hoVisible
        ];
        PaintOptions := [
          toHideFocusRect, toHotTrack, toPopupMode, toShowBackground,
          toShowDropmark, toThemeAware, toUseBlendedImages, toUseBlendedSelection,
          toUseExplorerTheme
        ];
        AnimationOptions := [];
        AutoOptions := [
          toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoSort,
          toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale,
          toDisableAutoscrollOnEdit, toAutoBidiColumnOrdering
        ];
        StringOptions := [toAutoAcceptEditChange];
        SelectionOptions := [toExtendedFocus, toFullRowSelect];
        MiscOptions := [
          toCheckSupport, toInitOnSave, toToggleOnDblClick, toWheelPanning,
          toVariableNodeHeight
        ];
        ColumnOptions := [];
        LineStyle                    := lsSolid;
        LineMode                     := lmNormal;
        DrawSelectionMode            := smBlendedRectangle;
        HintMode                     := hmTooltip;
        SelectionRectangleBlendColor := clGray;
        SelectionTextColor           := clBlack;
        GridLineColor                := clSilver;
      end;
    end,
    True
  );

  FDefaultTreeGridOptions.Create(
    function: TVSTOptions
    begin
      Result := TVSTOptions.Create;
      with Result do
      begin
        HeaderOptions := [
          hoAutoResize, hoColumnResize, hoDblClickResize, hoRestrictDrag,
          hoShowHint, hoShowImages, hoShowSortGlyphs, hoAutoSpring,
          hoDisableAnimatedResize, hoVisible
        ];
        PaintOptions := [
          toHideFocusRect, toHideSelection, toHotTrack, toPopupMode,
          toShowBackground, toShowButtons, toShowDropmark, toStaticBackground,
          toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages,
          toUseBlendedSelection, toStaticBackground, toUseExplorerTheme
        ];
        AnimationOptions := [];
        AutoOptions := [
          toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoSort,
          toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale,
          toDisableAutoscrollOnEdit, toAutoBidiColumnOrdering
        ];
        StringOptions := [toAutoAcceptEditChange];
        SelectionOptions := [toExtendedFocus, toFullRowSelect];
        MiscOptions := [
          toCheckSupport, toInitOnSave, toToggleOnDblClick, toWheelPanning,
          toVariableNodeHeight{, toGridExtensions}
        ];
        ColumnOptions := [];

        LineStyle                    := lsSolid;
        LineMode                     := lmNormal;
        DrawSelectionMode            := smBlendedRectangle;
        HintMode                     := hmTooltip;
        SelectionRectangleBlendColor := clGray;
        SelectionTextColor           := clBlack;
        GridLineColor                := clSilver;
      end;
    end,
    True
  );

  FDefaultTreeListOptions.Create(
    function: TVSTOptions
    begin
      Result := TVSTOptions.Create;
      with Result do
      begin
        HeaderOptions := [
          hoAutoResize, hoColumnResize, hoDblClickResize, hoRestrictDrag,
          hoShowHint, hoShowImages, hoShowSortGlyphs, hoAutoSpring,
          hoDisableAnimatedResize, hoVisible
        ];
        PaintOptions := [
          toHideFocusRect, toHideSelection, toHotTrack, toPopupMode,
          toShowBackground, toShowButtons, toShowDropmark, toStaticBackground,
          toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages,
          toUseBlendedSelection, toStaticBackground, toUseExplorerTheme
        ];
        AnimationOptions := [];
        AutoOptions := [
          toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoSort,
          toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale,
          toDisableAutoscrollOnEdit, toAutoBidiColumnOrdering
        ];
        StringOptions := [toAutoAcceptEditChange];
        SelectionOptions := [toExtendedFocus, toFullRowSelect];
        MiscOptions := [
          toCheckSupport, toInitOnSave, toToggleOnDblClick, toWheelPanning,
          toVariableNodeHeight
        ];
        ColumnOptions := [];

        LineStyle                    := lsSolid;
        LineMode                     := lmNormal;
        DrawSelectionMode            := smBlendedRectangle;
        HintMode                     := hmTooltip;
        SelectionRectangleBlendColor := clGray;
        SelectionTextColor           := clBlack;
        GridLineColor                := clSilver;
      end;
    end,
    True
  );
end;
{$ENDREGION}

{$REGION 'property access methods'}
class function TVirtualStringTreeFactory.GetDefaultGridOptions: TVSTOptions;
begin
  Result := FDefaultGridOptions;
end;

class function TVirtualStringTreeFactory.GetDefaultListOptions: TVSTOptions;
begin
  Result := FDefaultListOptions;
end;

class function TVirtualStringTreeFactory.GetDefaultTreeGridOptions: TVSTOptions;
begin
  Result := FDefaultTreeGridOptions;
end;

class function TVirtualStringTreeFactory.GetDefaultTreeListOptions: TVSTOptions;
begin
  Result := FDefaultTreeListOptions;
end;

class function TVirtualStringTreeFactory.GetDefaultTreeOptions: TVSTOptions;
begin
  Result := FDefaultTreeOptions;
end;
{$ENDREGION}

{$REGION 'public methods'}
class function TVirtualStringTreeFactory.Create(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  VST := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent           := AParent;
  VST.Align            := alClient;
  VST.Header.Height    := 18;
  Result := VST;
end;

{ Creates a TVirtualStringTree that will be used as a grid control. }

class function TVirtualStringTreeFactory.CreateGrid(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  VST := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent           := AParent;
  VST.Align            := alClient;
  VST.Header.Height    := 18;
  DefaultGridOptions.AssignTo(VST);
  VST.Indent := 2; // show first column as a normal grid column
  Result := VST;
end;

{ Creates a TVirtualStringTree that mimics a list view. }

class function TVirtualStringTreeFactory.CreateList(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  VST := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent           := AParent;
  VST.Align            := alClient;
  VST.Header.Height    := 18;
  DefaultListOptions.AssignTo(VST);
  VST.Indent := 2; // show first column as a normal grid column
  Result := VST;
end;

{ Creates a TVirtualStringTree that will be used as a tree control. }

class function TVirtualStringTreeFactory.CreateTree(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  VST := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent           := AParent;
  VST.Align            := alClient;
  VST.Header.Height    := 18;
  DefaultTreeOptions.AssignTo(VST);
  Result := VST;
end;

class function TVirtualStringTreeFactory.CreateTreeGrid(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  VST := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent           := AParent;
  VST.Align            := alClient;
  VST.Header.Height    := 18;
  DefaultTreeGridOptions.AssignTo(VST);
  Result := VST;
end;

class function TVirtualStringTreeFactory.CreateTreeList(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  VST := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent           := AParent;
  VST.Align            := alClient;
  VST.Header.Height    := 18;
  DefaultTreeListOptions.AssignTo(VST);
  Result := VST;
end;
{$ENDREGION}
end.
