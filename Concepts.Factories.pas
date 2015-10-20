{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.Factories;

{ Factories for some commonly used components and objects. }

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.DBGrids,
  Data.DB,

  VirtualTrees,

  Spring, Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Bindings.Collections,
  DSharp.Core.DataTemplates,

  DDuce.Components.PropertyInspector, DDuce.Components.LogTree,
  DDuce.Components.GridView, DDuce.Components.DBGridView;

type
  TConceptFactories = record
  strict private
    class procedure InitializeTVP(
      ATVP      : TTreeViewPresenter;
      AVST      : TVirtualStringTree = nil;
      ASource   : IObjectList = nil;
      ATemplate : IDataTemplate = nil;
      AFilter   : TFilterEvent = nil
    ); static;

  public
    class procedure FillListWithContacts(
      AList  : IList;
      ACount : Integer
    ); static;

    class function CreateLogTree(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TLogTree; static;

    class function CreateInspector(
      AOwner  : TComponent;
      AParent : TWinControl;
      AObject : TPersistent = nil
    ): TPropertyInspector; static;

    class function CreateVST(
            AOwner  : TComponent;
            AParent : TWinControl;
      const AName   : string = ''
    ): TVirtualStringTree; static;

    class function CreateTVP(
            AOwner    : TComponent;
            AVST      : TVirtualStringTree = nil;
            ASource   : IObjectList = nil;
            ATemplate : IDataTemplate = nil;
            AFilter   : TFilterEvent = nil;
      const AName     : string = ''
    ): TTreeViewPresenter; static;

    class function CreateDBGridView(
            AOwner      : TComponent;
            AParent     : TWinControl;
            ADataSource : TDataSource = nil;
      const AName       : string = ''
    ): TDBGridView; static;

    { Create standard VCL DB Grid. }
    class function CreateDBGrid(
            AOwner      : TComponent;
            AParent     : TWinControl;
            ADataSource : TDataSource = nil;
      const AName       : string = ''
    ): TDBGrid; static;

  end;

implementation

uses
  System.Rtti,
  Vcl.Forms,

  DSharp.Windows.ColumnDefinitions.ControlTemplate;

{$REGION 'TVirtualStringTree settings'}
const
  DEFAULT_VST_SELECTIONOPTIONS = [
    { Prevent user from selecting with the selection rectangle in multiselect
      mode. }
//    toDisableDrawSelection,
    {  Entries other than in the main column can be selected, edited etc. }
    toExtendedFocus,
    { Hit test as well as selection highlight are not constrained to the text
      of a node. }
    toFullRowSelect,
    { Constrain selection to the same level as the selection anchor. }
//    toLevelSelectConstraint,
    { Allow selection, dragging etc. with the middle mouse button. This and
      toWheelPanning are mutual exclusive. }
//    toMiddleClickSelect,
    { Allow more than one node to be selected. }
    toMultiSelect
    {  Allow selection, dragging etc. with the right mouse button. }
//    toRightClickSelect,
    { Constrain selection to nodes with same parent. }
//    toSiblingSelectConstraint,
    { Center nodes vertically in the client area when scrolling into view. }
//    toCenterScrollIntoView
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
    { Fully invalidate the tree when its window is resized
      (CS_HREDRAW/CS_VREDRAW). }
//    toFullRepaintOnResize,
    { Use some special enhancements to simulate and support grid behavior. }
//    toGridExtensions,
    { Initialize nodes when saving a tree to a stream. }
    toInitOnSave,
    { Tree behaves like TListView in report mode. }
//    toReportMode,
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
    { Start node dragging by clicking anywhere in it instead only on the
      caption or image. Must be used together with toDisableDrawSelection. }
//    toFullRowDrag,
    { Allows changing a node's height via mouse. }
//    toNodeHeightResize,
    { Allows to reset a node's height to FDefaultNodeHeight via a double click. }
//    toNodeHeightDblClickResize
    { Editing mode can be entered with a single click }
//    toEditOnClick,
    { Editing mode can be entered with a double click }
//    toEditOnDblClick
  ];
  DEFAULT_VST_PAINTOPTIONS = [
    { Avoid drawing the dotted rectangle around the currently focused node. }
    toHideFocusRect,
    { Selected nodes are drawn as unselected nodes if the tree is unfocused. }
//    toHideSelection,
    { Track which node is under the mouse cursor. }
//    toHotTrack,
    { Paint tree as would it always have the focus }
    toPopupMode,
    { Use the background image if there's one. }
    toShowBackground,
    { Display collapse/expand buttons left to a node. }
    toShowButtons,
    { Show the dropmark during drag'n drop operations. }
    toShowDropmark,
    { Display horizontal lines to simulate a grid. }
    toShowHorzGridLines,
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
    toUseBlendedSelection,
    { Show simple static background instead of a tiled one. }
    toStaticBackground,
    { Display child nodes above their parent. }
//    toChildrenAbove,
    { Draw the tree with a fixed indent. }
//    toFixedIndent,
    { Use the explorer theme if run under Windows Vista (or above). }
    toUseExplorerTheme
    { Do not show tree lines if theming is used. }
//    toHideTreeLinesIfThemed,
    { Draw nodes even if they are filtered out. }
//    toShowFilteredNodes
  ];
  DEFAULT_VST_HEADEROPTIONS = [
    { Adjust a column so that the header never exceeds the client width of the
      owner control. }
    hoAutoResize,
    { Resizing columns with the mouse is allowed. }
    hoColumnResize,
    { Allows a column to resize itself to its largest entry. }
    hoDblClickResize,
    { Dragging columns is allowed. }
    hoDrag,
    { Header captions are highlighted when mouse is over a particular column. }
//    hoHotTrack,
    { Header items with the owner draw style can be drawn by the application
      via event. }
//    hoOwnerDraw,
    { Header can only be dragged horizontally. }
    hoRestrictDrag,
    { Show application defined header hint. }
    hoShowHint,
    { Show header images. }
    hoShowImages,
    { Allow visible sort glyphs. }
    hoShowSortGlyphs,
    { Distribute size changes of the header to all columns, which are sizable
      and have the coAutoSpring option enabled. hoAutoResize must be enabled
      too. }
    hoAutoSpring,
    { Fully invalidate the header (instead of subsequent columns only) when a
      column is resized. }
//    hoFullRepaintOnResize,
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
    toAutoAcceptEditChange
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
    toAutoExpand,
    { Scroll if mouse is near the border while dragging or selecting. }
    toAutoScroll,
    { Scroll as many child nodes in view as possible after expanding a node. }
    toAutoScrollOnExpand,
    { Sort tree when Header.SortColumn or Header.SortDirection change or sort
      node if child nodes are added. }
    toAutoSort,
    { Large entries continue into next column(s) if there's no text in them
      (no clipping). }
//    toAutoSpanColumns,
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

{$REGION 'TConceptFactories'}
class function TConceptFactories.CreateDBGrid(AOwner: TComponent;
  AParent: TWinControl; ADataSource: TDataSource; const AName: string): TDBGrid;
var
  DBG: TDBGrid;
begin
  DBG                  := TDBGrid.Create(AOwner);
  DBG.AlignWithMargins := True;
  DBG.Parent           := AParent;
  DBG.Align            := alClient;
  DBG.DataSource       := ADataSource;
  Result := DBG;
end;

class function TConceptFactories.CreateDBGridView(AOwner: TComponent;
  AParent: TWinControl; ADataSource: TDataSource;
  const AName: string): TDBGridView;
var
  GV: TDBGridView;
begin
  GV                  := TDBGridView.Create(AOwner);
  GV.Header.Flat      := False;
  GV.AlignWithMargins := True;
  GV.Parent           := AParent;
  GV.Align            := alClient;
  GV.CursorKeys       := GV.CursorKeys + [gkReturn];
  GV.GridStyle        := GV.GridStyle + [gsDotLines];
  GV.ColumnsFullDrag  := True;
  GV.DoubleBuffered   := True;
  GV.CheckBoxes       := True;
  GV.ShowFocusRect    := False;
  GV.CheckStyle       := csFlat;
  GV.ColumnClick      := True;
  GV.ShowIndicator    := False;
  GV.DataSource       := ADataSource;
  GV.AutoSizeCols;
  Result := GV;
end;

class function TConceptFactories.CreateInspector(AOwner: TComponent;
  AParent: TWinControl; AObject: TPersistent): TPropertyInspector;
var
  PI : TPropertyInspector;
begin
  PI                  := TPropertyInspector.Create(AOwner);
  PI.AlignWithMargins := True;
  PI.Parent           := AParent;
  PI.BorderStyle      := bsSingle;
  PI.PropKinds        := PI.PropKinds + [pkReadOnly];
  PI.Align            := alClient;
  PI.Splitter         := PI.Width div 2;
  if Assigned(AObject) then
  begin
    PI.Add(AObject);
    PI.UpdateItems;
  end;
  Result := PI;
end;

class function TConceptFactories.CreateLogTree(AOwner: TComponent;
  AParent: TWinControl): TLogTree;
var
  VLT : TLogTree;
begin
  VLT                    := TLogTree.Create(AOwner);
  VLT.AlignWithMargins   := True;
  VLT.BorderStyle        := bsNone;
  VLT.Parent             := AParent;
  VLT.Align              := alClient;
  VLT.ShowImages         := True;
  VLT.Header.Options     := VLT.Header.Options + [hoAutoSpring];
  Result := VLT;
end;

class function TConceptFactories.CreateTVP(AOwner: TComponent;
  AVST: TVirtualStringTree; ASource: IObjectList; ATemplate: IDataTemplate;
  AFilter: TFilterEvent; const AName: string): TTreeViewPresenter;
var
  TVP: TTreeViewPresenter;
begin
  TVP := TTreeViewPresenter.Create(AOwner);
  InitializeTVP(TVP, AVST, ASource, ATemplate, AFilter);
  Result := TVP;
end;

class function TConceptFactories.CreateVST(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  VST          := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent   := AParent;
  VST.HintMode := hmTooltip;
  VST.Align    := alClient;
  VST.DrawSelectionMode := smBlendedRectangle;
  VST.Header.Height := 18;
  VST.Header.Options               := DEFAULT_VST_HEADEROPTIONS;
  VST.TreeOptions.SelectionOptions := DEFAULT_VST_SELECTIONOPTIONS;
  VST.TreeOptions.MiscOptions      := DEFAULT_VST_MISCOPTIONS;
  VST.TreeOptions.PaintOptions     := DEFAULT_VST_PAINTOPTIONS;
  VST.TreeOptions.StringOptions    := DEFAULT_VST_STRINGOPTIONS;
  VST.TreeOptions.AnimationOptions := DEFAULT_VST_ANIMATIONOPTIONS;
  VST.TreeOptions.AutoOptions      := DEFAULT_VST_AUTOOPTIONS;
  Result := VST;
end;

class procedure TConceptFactories.FillListWithContacts(AList: IList;
  ACount: Integer);
var
  I : Integer;
begin
  if Assigned(AList) then
  begin
    AList.Clear;
    for I := 0 to ACount - 1 do
    begin

      //AList.Add(TConcept CreateRandomContact);
    end;
  end;
end;

class procedure TConceptFactories.InitializeTVP(ATVP: TTreeViewPresenter;
  AVST: TVirtualStringTree; ASource: IObjectList; ATemplate: IDataTemplate;
  AFilter: TFilterEvent);
var
  P : TRttiProperty;
  C : TRttiContext;
begin
  if Assigned(ASource) then // auto create column definitions
  begin
    for P in C.GetType(ASource.ElementType).GetProperties do
    begin
      with ATVP.ColumnDefinitions.Add(P.Name) do
        ValuePropertyName := P.Name;
    end;
  end;
  ATVP.TreeView := AVST;
  ATVP.SyncMode := False;
  ATVP.UseColumnDefinitions := True;
  ATVP.ListMode             := True;
  ATVP.View.ItemsSource     := ASource;
  if Assigned(ATemplate) then
    ATVP.View.ItemTemplate :=
      TColumnDefinitionsControlTemplate.Create(ATVP.ColumnDefinitions);
  if Assigned(AFilter) then
    ATVP.View.Filter.Add(AFilter);
end;
{$ENDREGION}

end.
