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

{$I Concepts.inc}

unit Concepts.Factories;

{ Factories for some commonly used components and objects. }

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.StdCtrls, Vcl.DBGrids,
  Data.DB, Data.Bind.Components,

  VirtualTrees, VirtualTrees.Header, VirtualTrees.Types,

  Spring, Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Bindings.Collections,
  DSharp.Core.DataTemplates, DSharp.Windows.CustomPresenter,
  DSharp.Windows.ColumnDefinitions,
  {$IFDEF DEVEXPRESS}
  DSharp.DevExpress.GridViewPresenter,
  DSharp.DevExpress.TreeListPresenter,
  {$ENDIF}

  {$IFDEF DEVEXPRESS}
  cxGridCustomView, cxTLData,
  {$ENDIF}

  {$IFDEF BCEDITOR}
  BCEditor.Editor.Base, BCEditor.Editor,
  {$ENDIF}

  Concepts.Types.Contact;

type
  TConceptFactories = record
  strict private
    class procedure InitializePresenter(
      APresenter  : TCustomPresenter;
      ASource     : IObjectList = nil;
      ATemplate   : IDataTemplate = nil;
      AFilter     : TFilterEvent = nil;
      ACustomDraw : TCustomDrawEvent = nil
    ); static;

  public
    class function CreateContactList(
      const ACount: Integer = 0
    ): IList<TContact>; static;

    class procedure FillListWithContacts(
      AList  : IObjectList;
      ACount : Integer
    ); static;

    class function CreateVirtualStringTree(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TVirtualStringTree; static;

    class function CreateTreeViewPresenter(
      AOwner            : TComponent;
      AVST              : TVirtualStringTree = nil;
      ASource           : IObjectList = nil;
      ATemplate         : IDataTemplate = nil;
      AFilter           : TFilterEvent = nil;
      ACustomDraw       : TCustomDrawEvent = nil;
      const AName       : string = ''
    ): TTreeViewPresenter; static;

    {$IFDEF DEVEXPRESS}
    class function CreateGridViewPresenter(
      AOwner      : TComponent;
      AGridView   : TcxCustomGridView = nil;
      ASource     : IObjectList = nil;
      ATemplate   : IDataTemplate = nil;
      AFilter     : TFilterEvent = nil;
      const AName : string = ''
    ): TGridViewPresenter; static;

    class function CreateTreeListPresenter(
      AOwner      : TComponent;
      ATreeList   : TcxVirtualTreeList = nil;
      ASource     : IObjectList = nil;
      ATemplate   : IDataTemplate = nil;
      AFilter     : TFilterEvent = nil;
      const AName : string = ''
    ): TTreeListPresenter; static;
    {$ENDIF}

    class function CreateDBGrid(
      AOwner      : TComponent;
      AParent     : TWinControl;
      ADataSource : TDataSource = nil;
      const AName : string = ''
    ): TDBGrid; static;

    class function CreateRandomContact(
      ASpecial: Boolean = False
    ): TContact; static;

    {$IFDEF BCEDITOR}
    class function CreateBCEditor(
      AOwner             : TComponent;
      AParent            : TWinControl;
      const AFileName    : string = '';
      const AHighlighter : string = '';
      const AColorMap    : string = ''
    ): TBCEditor; static;
    {$ENDIF}
  end;

type
  TBindingsFactory = record
    class function CreateBindScope(
      ASource       : TObject;
      AOwner        : TComponent;
      AAutoActivate : Boolean = True
    ): TBindScope; static;

    class function CreateBindExpression(
      ASourceComponent         : TComponent;  // bindscope
      const ASourceExpression  : string;
      AControlComponent        : TComponent;
      const AControlExpression : string;
      ADirection               : TExpressionDirection = dirBidirectional;
      AOwner                   : TComponent = nil // take sourcecomponent as owner
    ): TBindExpression; static;

    class function CreateEditBinding(
      ASourceComponent        : TComponent;
      const ASourceExpression : string;
      AEdit                   : TCustomEdit;
      ADirection              : TExpressionDirection = dirBidirectional;
      AOwner                  : TComponent = nil
    ): TBindExpression; static;
  end;

implementation

uses
  System.Rtti, System.TypInfo,
  Vcl.Forms, Vcl.Graphics,

  BCEditor.Types,

  DDuce.RandomData,

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
//  toShowHorzGridLines,
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
  Guard.CheckNotNull(AParent, 'AParent');
  DBG                  := TDBGrid.Create(AOwner);
  DBG.AlignWithMargins := True;
  DBG.Parent           := AParent;
  DBG.Align            := alClient;
  DBG.DataSource       := ADataSource;
  Result := DBG;
end;

{$IFDEF DEVEXPRESS}
class function TConceptFactories.CreateGridViewPresenter(AOwner: TComponent;
  AGridView: TcxCustomGridView; ASource: IObjectList; ATemplate: IDataTemplate;
  AFilter: TFilterEvent; const AName: string): TGridViewPresenter;
var
  GVP: TGridViewPresenter;
begin
  GVP := TGridViewPresenter.Create(AOwner);
  GVP.GridView := AGridView;
  InitializePresenter(GVP, ASource, ATemplate, AFilter);
  Result := GVP;
end;

class function TConceptFactories.CreateTreeListPresenter(AOwner: TComponent;
  ATreeList: TcxVirtualTreeList; ASource: IObjectList; ATemplate: IDataTemplate;
  AFilter: TFilterEvent; const AName: string): TTreeListPresenter;
var
  TLP: TTreeListPresenter;
begin
  TLP := TTreeListPresenter.Create(AOwner);
  TLP.TreeList := ATreeList;
  InitializePresenter(TLP, ASource, ATemplate, AFilter);
  Result := TLP;
end;
{$ENDIF}

class function TConceptFactories.CreateTreeViewPresenter(
  AOwner: TComponent; AVST: TVirtualStringTree; ASource: IObjectList;
  ATemplate: IDataTemplate; AFilter: TFilterEvent; ACustomDraw: TCustomDrawEvent;
  const AName : string): TTreeViewPresenter;
var
  TVP: TTreeViewPresenter;
begin
  TVP := TTreeViewPresenter.Create(AOwner);
  TVP.TreeView := AVST;
  TVP.SyncMode := True;
  TVP.ListMode := False;
  InitializePresenter(TVP, ASource, ATemplate, AFilter, ACustomDraw);
  Result := TVP;
end;

class function TConceptFactories.CreateVirtualStringTree(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  Guard.CheckNotNull(AParent, 'AParent');
  VST                              := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins             := True;
  VST.Parent                       := AParent;
  VST.HintMode                     := hmTooltip;
  VST.Align                        := alClient;
  VST.BorderStyle                  := bsNone;
  VST.DrawSelectionMode            := smBlendedRectangle;
  VST.Indent                       := 2;
  VST.Header.Height                := 18;
  VST.Header.Options               := DEFAULT_VST_HEADEROPTIONS;
  VST.TreeOptions.SelectionOptions := DEFAULT_VST_SELECTIONOPTIONS;
  VST.TreeOptions.MiscOptions      := DEFAULT_VST_MISCOPTIONS;
  VST.TreeOptions.PaintOptions     := DEFAULT_VST_PAINTOPTIONS;
  VST.TreeOptions.StringOptions    := DEFAULT_VST_STRINGOPTIONS;
  VST.TreeOptions.AnimationOptions := DEFAULT_VST_ANIMATIONOPTIONS;
  VST.TreeOptions.AutoOptions      := DEFAULT_VST_AUTOOPTIONS;
  Result := VST;
end;

{$IFDEF BCEDITOR}
class function TConceptFactories.CreateBCEditor(AOwner: TComponent;
  AParent: TWinControl; const AFileName : string; const AHighlighter: string;
  const AColorMap: string): TBCEditor;
var
  BCE : TBCEditor;
begin
  BCE := TBCEditor.Create(AOwner);
  BCE.Parent := AParent;
  BCE.Align := alClient;
  BCE.AlignWithMargins := True;

  if AFileName <> '' then
    BCE.LoadFromFile(AFileName);
  if AHighlighter <> '' then
    BCE.Highlighter.LoadFromFile(AHighlighter + '.json');
  if AColorMap <> '' then
    BCE.Highlighter.Colors.LoadFromFile(AColorMap + '.json');

  BCE.CodeFolding.Options := BCE.CodeFolding.Options + [cfoFoldMultilineComments];

  BCE.Tabs.Options := BCE.Tabs.Options + [toPreviousLineIndent];
  BCE.Selection.Options := BCE.Selection.Options + [soALTSetsColumnMode];

  BCE.Font.Name := 'Consolas';
  BCE.CodeFolding.Visible := True;
  BCE.URIOpener := True;

  Result := BCE;
end;
{$ENDIF}

class function TConceptFactories.CreateContactList(
  const ACount: Integer): IList<TContact>;
begin
  Result := TCollections.CreateObjectList<TContact>;
  if ACount > 0 then
    FillListWithContacts(Result as IObjectList, ACount);
end;

class procedure TConceptFactories.FillListWithContacts(AList: IObjectList;
  ACount: Integer);
var
  I : Integer;
  B : Boolean;
begin
  Guard.CheckNotNull(AList, 'AList');
  AList.Clear;
  B := ACount < 40000;
  for I := 0 to ACount - 1 do
  begin
    AList.Add(CreateRandomContact(B));
  end;
end;

class function TConceptFactories.CreateRandomContact(ASpecial: Boolean)
  : TContact;
var
  C: TContact;
begin
  C := TContact.Create;
  with C do
  begin
    FirstName   := RandomData.FirstName;
    LastName    := RandomData.LastName;
    if ASpecial then
      CompanyName := RandomData.AlliteratedCompanyName
    else
      CompanyName := RandomData.CompanyName;
    Email       := RandomData.Email(FirstName, LastName);
    Address     := RandomData.Address;
    Number      := RandomData.Number(100);
    BirthDate   := RandomData.BirthDate(1928, 1987);
    Active      := RandomData.Bool;
  end;
  Result := C;
end;

class procedure TConceptFactories.InitializePresenter(
  APresenter: TCustomPresenter; ASource: IObjectList; ATemplate: IDataTemplate;
  AFilter: TFilterEvent; ACustomDraw: TCustomDrawEvent);
var
  P : TRttiProperty;
  C : TRttiContext;
begin
  Guard.CheckNotNull(APresenter, 'APresenter');
  if Assigned(ASource) then // auto create column definitions
  begin
    for P in C.GetType(ASource.ElementType).GetProperties do
    begin
      with APresenter.ColumnDefinitions.Add(P.Name) do
      begin
        ValuePropertyName := P.Name;
        HintPropertyName  := P.Name;
        OnCustomDraw := ACustomDraw;
      end;
    end;
  end;
  APresenter.UseColumnDefinitions := True;
  APresenter.View.ItemsSource     := ASource;
  if not Assigned(ATemplate) then
    APresenter.View.ItemTemplate :=
      TColumnDefinitionsControlTemplate.Create(APresenter.ColumnDefinitions)
  else
    APresenter.View.ItemTemplate := ATemplate;
  if Assigned(AFilter) then
    APresenter.View.Filter.Add(AFilter);
end;
{$ENDREGION}

{$REGION 'TBindingsFactory'}
{ Wraps an object in a TBindScope component. }

class function TBindingsFactory.CreateBindScope(ASource: TObject; AOwner: TComponent;
  AAutoActivate: Boolean): TBindScope;
var
  BS : TBindScope;
begin
  BS := TBindScope.Create(AOwner);
  BS.AutoActivate := AAutoActivate;
  BS.DataObject   := ASource;
  Result := BS;
end;

class function TBindingsFactory.CreateBindExpression(
  ASourceComponent: TComponent; const ASourceExpression: string;
  AControlComponent: TComponent; const AControlExpression: string;
  ADirection: TExpressionDirection; AOwner: TComponent): TBindExpression;
var
  BE : TBindExpression;
begin
  if not Assigned(AOwner) then
    AOwner := ASourceComponent;
  BE := TBindExpression.Create(AOwner);
  BE.NotifyOutputs     := True;
  BE.SourceComponent   := ASourceComponent;
  BE.SourceExpression  := ASourceExpression;
  BE.ControlComponent  := AControlComponent;
  BE.ControlExpression := AControlExpression;
  BE.Direction         := ADirection;
  Result := BE;
end;

class function TBindingsFactory.CreateEditBinding(ASourceComponent: TComponent;
  const ASourceExpression: string; AEdit: TCustomEdit; ADirection:
  TExpressionDirection; AOwner: TComponent): TBindExpression;
begin
  Result := TBindingsFactory.CreateBindExpression(
    ASourceComponent,
    ASourceExpression,
    AEdit,
    'Text',
    ADirection,
    AOwner
  );
end;
{$ENDREGION}

end.
