(*
  Copyright (c) 2011-2013, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.Windows.TreeViewPresenter;

interface

uses
  ActiveX,
  Classes,
  ComCtrls,
  Controls,
  DSharp.Bindings.Collections,
  DSharp.Bindings.Notifications,
  DSharp.Core.DataTemplates,
  DSharp.Windows.ColumnDefinitions,
  DSharp.Windows.CustomPresenter,
  DSharp.Windows.CustomPresenter.Types,
  Menus,
  Spring,
  Spring.Collections,
  SysUtils,
  Types,
  VirtualTrees;

{$I DSharp.Windows.CustomPresenter.Types.inc}

type
  TTreeViewPresenter = class(TCustomPresenter)
  private
    FAllowClearSelection: Boolean;
    FCheckedItems: IList<TObject>;
    FChecking: Boolean;
    FCurrentNode: PVirtualNode;
    FFilterDirection: TFilterDirection;
    FExpandedItems: IList<TObject>;
    FHitInfo: THitInfo;
    FListMode: Boolean;
    FOnCollapsed: TCollapsedEvent;
    FOnCompare: TCompareEvent;
    FOnExpanded: TExpandedEvent;
    FOnKeyAction: TKeyEvent;
    FProgressBar: TProgressBar;
    FSelectedItems: IList<TObject>;
    FSelectedItemPath: IList<TObject>;
    FSorting: Boolean;
    FSyncing: Boolean;
    FSyncMode: Boolean;
    FTreeView: TVirtualStringTree;

    procedure DoAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure DoBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure DoChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure DoDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure DoDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: VirtualTrees.TDropMode);
    procedure DoDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint;
      Mode: VirtualTrees.TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure DoEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure DoEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure DoExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoFilterNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure DoFocusChanging(Sender: TBaseVirtualTree;
      OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure DoGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure DoGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure DoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure DoHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure DoHeaderDblClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure DoIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: string; var Result: Integer);
    procedure DoInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure DoNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

    procedure DrawCheckBox(TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect; Value: Boolean);
    procedure DrawProgressBar(TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect; Value: Integer);
    procedure DrawImage(TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect; Value: Integer);

    procedure ExpandNode(Node: PVirtualNode);
    function GetCheckedItem: TObject;
    function GetCheckedItems: IList<TObject>;
    function GetExpandedItems: IList<TObject>;
    procedure GetItemNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    procedure GetItemsNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    function GetNodeItem(Tree: TBaseVirtualTree; Node: PVirtualNode): TObject;
    function GetNodeItems(Tree: TBaseVirtualTree; Node: PVirtualNode): IObjectList;
    function GetParentItem(const Level: Integer): TObject;
    function GetSelectedItem: TObject;
    function GetSelectedItems: IList<TObject>;

    function CalcCheckBoxRect(const Rect: TRect): TRect;
    function CalcImageRect(const Rect: TRect): TRect;
    function IsMouseInCheckBox(Node: PVirtualNode; Column: TColumnIndex): Boolean;
    function IsMouseInToggleIcon(HitInfo: THitInfo): Boolean;
    function ToggleCheckBox(Node: PVirtualNode; Column: TColumnIndex): Boolean;
    procedure ToggleIcon(Node: PVirtualNode; Column: TColumnIndex);

    procedure ReadMultiSelect(Reader: TReader);
    procedure ResetRootNodeCount;

    procedure SetCheckedItem(const Value: TObject);
    procedure SetCheckedItems(const Value: IList<TObject>);
    procedure SetExpandedItems(const Value: IList<TObject>);
    procedure SetListMode(const Value: Boolean);
    procedure SetNodeItem(Tree: TBaseVirtualTree; Node: PVirtualNode; Item: TObject);
    procedure SetNodeItems(Tree: TBaseVirtualTree; Node: PVirtualNode; Items: IObjectList);
    procedure SetSelectedItem(const Value: TObject);
    procedure SetSelectedItems(const Value: IList<TObject>);
    procedure SetSorting(const Value: Boolean);
    procedure SetTreeView(const Value: TVirtualStringTree);

    procedure UpdateCheckedItems;
    procedure UpdateExpandedItems;
    procedure UpdateSelectedItems;
    procedure UpdateSelectedItemPath;
  protected
    procedure ApplyAllowMove; override;
    procedure ApplyCheckSupport; override;
    procedure ApplySelectionMode; override;
    procedure ApplyShowHeader; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoCheckedItemsChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
    procedure DoDblClick(Sender: TObject); override;
    procedure DoExpandedItemsChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
    procedure DoFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoSelectedItemsChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
    procedure DoSourceCollectionChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction); override;
    function GetCanMoveCurrentToNext: Boolean; override;
    function GetCanMoveCurrentToPrevious: Boolean; override;
    function GetCurrentItem: TObject; override;
    procedure SetCurrentItem(const Value: TObject); override;
    procedure InitColumns; override;
    procedure InitControl; override;
    procedure InitEvents; override;
    procedure InitProperties; override;
    procedure MoveCurrentToFirst; override;
    procedure MoveCurrentToLast; override;
    procedure MoveCurrentToNext; override;
    procedure MoveCurrentToPrevious; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyFilter; override;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    procedure DeleteItems(Items: IList<TObject>); deprecated 'use View.ItemsSource.RemoveRange instead';
    procedure FullCollapse;
    procedure FullExpand;

    procedure Refresh; override;

    property CheckedItem: TObject read GetCheckedItem write SetCheckedItem;
    property CheckedItems: IList<TObject> read GetCheckedItems write SetCheckedItems;
    property ExpandedItems: IList<TObject> read GetExpandedItems write SetExpandedItems;
    property ParentItem[const Level: Integer]: TObject read GetParentItem;
    property SelectedItem: TObject read GetSelectedItem write SetSelectedItem;
    property SelectedItems: IList<TObject> read GetSelectedItems write SetSelectedItems;
    property SelectedItemPath: IList<TObject> read FSelectedItemPath;
  published
    property AllowClearSelection: Boolean
      read FAllowClearSelection write FAllowClearSelection default True;
    property FilterDirection: TFilterDirection read FFilterDirection write FFilterDirection default fdRootToLeafs;
    property ListMode: Boolean read FListMode write SetListMode default False;
    property OnCollapsed: TCollapsedEvent read FOnCollapsed write FOnCollapsed;
    property OnCompare: TCompareEvent read FOnCompare write FOnCompare;
    property OnExpanded: TExpandedEvent read FOnExpanded write FOnExpanded;
    property OnKeyAction: TKeyEvent read FOnKeyAction write FOnKeyAction;
    property Sorting: Boolean read FSorting write SetSorting default True;
    property SyncMode: Boolean read FSyncMode write FSyncMode default False;
    property TreeView: TVirtualStringTree read FTreeView write SetTreeView;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.Windows.ColumnDefinitions.ControlTemplate,
  DSharp.Windows.ControlTemplates,
  Forms,
  Graphics,
  Math,
  Rtti,
  Spring.Collections.Base,
  Spring.Collections.Lists,
  Themes,
  TypInfo,
  UxTheme,
  Windows;

const
  CDefaultCellRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

type
  PNodeData = ^TNodeData;
  TNodeData = record
    Item: TObject;
    Items: IObjectList;
  end;

var
  CheckBoxSize: Byte;

procedure UpdateList(const Target, Source: IList<TObject>);
var
  i: Integer;
begin
  i := 0;
  while i < Target.Count do
  begin
    if Source.Contains(Target[i]) then
    begin
      Inc(i);
    end
    else
    begin
      Target.Delete(i);
    end;
  end;
  for i := 0 to Pred(Source.Count) do
  begin
    if not Target.Contains(Source[i]) then
    begin
      Target.Add(Source[i]);
    end;
  end;
end;

{$IF CompilerVersion < 23}
type
  TThemeServicesHelper = class helper for TThemeServices
    function Enabled: Boolean;
  end;

function TThemeServicesHelper.Enabled: Boolean;
begin
  Result := ThemesEnabled;
end;

function StyleServices: TThemeServices;
begin
  Result := ThemeServices;
end;
{$IFEND}

{ TTreeViewPresenter }

constructor TTreeViewPresenter.Create(AOwner: TComponent);
begin
  FCheckedItems := TList<TObject>.Create();
  FCheckedItems.OnChanged.Add(DoCheckedItemsChanged);
  FExpandedItems := TList<TObject>.Create();
  FExpandedItems.OnChanged.Add(DoExpandedItemsChanged);
  FSelectedItems := TList<TObject>.Create();
  FSelectedItems.OnChanged.Add(DoSelectedItemsChanged);
  FSelectedItemPath := TList<TObject>.Create();
  inherited;
  FAllowClearSelection := True;
  FSorting := True;
  FProgressBar := TProgressBar.Create(Self);
  FProgressBar.Smooth := True;
  FProgressBar.Visible := False;
end;

destructor TTreeViewPresenter.Destroy;
begin
  FSelectedItemPath := nil;
  FCheckedItems.OnChanged.Remove(DoCheckedItemsChanged);
  FExpandedItems.OnChanged.Remove(DoExpandedItemsChanged);
  FSelectedItems.OnChanged.Remove(DoSelectedItemsChanged);
  inherited;
end;

procedure TTreeViewPresenter.ApplyAllowMove;
begin
  inherited;
  InitProperties();
end;

procedure TTreeViewPresenter.ApplyCheckSupport;
begin
  inherited;
  InitProperties();
end;

procedure TTreeViewPresenter.ApplyFilter;
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState) then
  begin
    FTreeView.BeginUpdate;
    try
      case FFilterDirection of
        fdRootToLeafs:
        begin
          LNode := FTreeView.GetFirst();
          while Assigned(LNode) do
          begin
            DoFilterNode(FTreeView, LNode);
            LNode := FTreeView.GetNext(LNode);
          end;
        end;
        fdLeafsToRoot:
        begin
          LNode := FTreeView.GetLast();
          while Assigned(LNode) do
          begin
            DoFilterNode(FTreeView, LNode);
            LNode := FTreeView.GetPrevious(LNode);
          end;
        end;
      end;
    finally
      FTreeView.EndUpdate;
    end;
  end;
end;

procedure TTreeViewPresenter.ApplySelectionMode;
begin
  inherited;
  InitProperties();
end;

procedure TTreeViewPresenter.ApplyShowHeader;
begin
  inherited;
  InitProperties();
end;

procedure TTreeViewPresenter.BeginUpdate;
begin
  inherited;
  FTreeView.BeginUpdate();
end;

function TTreeViewPresenter.CalcCheckBoxRect(const Rect: TRect): TRect;
begin
  Result.Left := Rect.Left + (RectWidth(Rect) - CheckBoxSize) div 2;
  Result.Top := Rect.Top + (RectHeight(Rect) - CheckBoxSize) div 2;
  Result.Right := Result.Left + CheckBoxSize;
  Result.Bottom := Result.Top + CheckBoxSize;
end;

function TTreeViewPresenter.CalcImageRect(const Rect: TRect): TRect;
begin
  Result.Left := Rect.Left + (RectWidth(Rect) - ImageList.Width) div 2;
  Result.Top := Rect.Top + (RectHeight(Rect) - ImageList.Height) div 2;
  Result.Right := Result.Left + ImageList.Width;
  Result.Bottom := Result.Top + ImageList.Height;
end;

procedure TTreeViewPresenter.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('MultiSelect', ReadMultiSelect, nil, False);
end;

procedure TTreeViewPresenter.DeleteItems(Items: IList<TObject>);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(Items) then
  begin
    LNode := FTreeView.GetFirst();
    while Assigned(LNode) do
    begin
      LItem := GetNodeItem(FTreeView, LNode);
      if Assigned(LItem) and Items.Contains(LItem) then
      begin
        FTreeView.DeleteNode(LNode);
      end;
      LNode := FTreeView.GetNext(LNode);
    end;
  end;
end;

procedure TTreeViewPresenter.DoAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  LItem: TObject;
  LDataTemplate: IDataTemplate;
  LControlTemplate: IControlTemplate;
  LValue: TValue;
begin
  LItem := GetNodeItem(Sender, Node);
  LDataTemplate := GetItemTemplate(LItem);
  if Supports(LDataTemplate, IControlTemplate, LControlTemplate) then
  begin
    LControlTemplate.CustomDraw(LItem, Column, TargetCanvas, CellRect, ImageList,
      dmAfterCellPaint, Sender.Selected[Node]);
  end;

  if Assigned(LDataTemplate) and Assigned(ColumnDefinitions) and (Column > -1)
    and (Column < ColumnDefinitions.Count) then
  begin
    LValue := LDataTemplate.GetValue(LItem, Column);
    if not LValue.IsEmpty then
    begin
      case ColumnDefinitions[Column].ColumnType of
        TColumnType.ctCheckBox:
          DrawCheckBox(TargetCanvas, Node, Column, CellRect, LValue.AsBoolean);
        TColumnType.ctProgressBar:
          DrawProgressBar(TargetCanvas, Node, Column, CellRect, LValue.AsOrdinal);
        TColumnType.ctImage:
          DrawImage(TargetCanvas, Node, Column, CellRect, LValue.AsOrdinal +
            ColumnDefinitions[Column].ImageIndexOffset);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  LItem: TObject;
  LItemTemplate: IControlTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  if Supports(GetItemTemplate(LItem), IControlTemplate, LItemTemplate) then
  begin
    LItemTemplate.CustomDraw(LItem, Column, TargetCanvas, CellRect, ImageList,
      dmBeforeCellPaint, Sender.Selected[Node]);
  end;
end;

procedure TTreeViewPresenter.DoChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if SelectionMode <> smNone then
  begin
    UpdateSelectedItems();
    UpdateSelectedItemPath();

    DoPropertyChanged('View');
    DoPropertyChanged('SelectedItem');
    DoPropertyChanged('SelectedItems');
    DoPropertyChanged('SelectedItemPath');


    if Assigned(OnSelectionChanged)
     and Assigned(Node) // added TS

    then
    begin
      OnSelectionChanged(Self);
    end;
  end;
end;

procedure TTreeViewPresenter.DoChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  UpdateCheckedItems();

  DoPropertyChanged('CheckedItem');
  DoPropertyChanged('CheckedItems');
end;

procedure TTreeViewPresenter.DoCheckedItemsChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState) then
  begin
    if FCollectionUpdateLock = 0 then
    begin
      LNode := FTreeView.GetFirst();
      while Assigned(LNode) do
      begin
        if GetNodeItem(FTreeView, LNode) = Item then
        begin
          case Action of
            caAdded: FTreeView.CheckState[LNode] := csCheckedNormal;
            caRemoved: FTreeView.CheckState[LNode] := csUncheckedNormal;
          end;
        end;
        LNode := FTreeView.GetNext(LNode);
      end;
    end;

    if FSyncMode and not FSyncing then
    try
      FSyncing := True;
      SetSelectedItems(FCheckedItems);
    finally
      FSyncing := False;
    end;
  end;
end;

procedure TTreeViewPresenter.DoCollapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LItem: TObject;
begin
  if Assigned(FOnCollapsed) then
  begin
    LItem := GetNodeItem(Sender, Node);
    FOnCollapsed(Sender, LItem);
  end;

  UpdateExpandedItems();

  DoPropertyChanged('ExpandedItems');
end;

procedure TTreeViewPresenter.DoCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  LItem1, LItem2: TObject;
  LItemTemplate: IDataTemplate;
begin
  if not Assigned(ColumnDefinitions) or (Column > -1) then
  begin
    if not (vsInitialized in Node1.States) then
      Sender.ReinitNode(Node1, False);
    LItem1 := GetNodeItem(Sender, Node1);
    if not (vsInitialized in Node2.States) then
      Sender.ReinitNode(Node2, False);
    LItem2 := GetNodeItem(Sender, Node2);

    if Assigned(FOnCompare) then
    begin
      FOnCompare(Self, LItem1, LItem2, Column, Result);
    end
    else
    begin
      LItemTemplate := GetItemTemplate(LItem1);
      if not Assigned(LItemTemplate) then
      begin
        LItemTemplate := View.ItemTemplate;
      end;
      Result := LItemTemplate.CompareItems(LItem1, LItem2, Column);
    end;
  end;
end;

procedure TTreeViewPresenter.DoDblClick(Sender: TObject);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
  LCursorPos: TPoint;
  LHitInfo: THitInfo;
begin
  LCursorPos := FTreeView.ScreenToClient(Mouse.CursorPos);
  FTreeView.GetHitTestInfoAt(LCursorPos.X, LCursorPos.Y, True, LHitInfo);

  if not IsMouseInToggleIcon(LHitInfo) then
  begin
    if Assigned(LHitInfo.HitNode)
      and (LHitInfo.HitColumn < 1)
      and not (hiOnItemButtonExact in LHitInfo.HitPositions)
      and ((hiOnNormalIcon in LHitInfo.HitPositions)
      or (not Assigned(OnDoubleClick) and not Assigned(Action))) then
    begin
      LItem := GetNodeItem(FTreeView, LHitInfo.HitNode);
      LItemTemplate := GetItemTemplate(LItem);
      if Assigned(LItemTemplate) and Assigned(LItemTemplate.Action) then
        LItemTemplate.Action.Execute
      else
        FTreeView.ToggleNode(LHitInfo.HitNode);
    end
    else
    begin
      if ([hiOnItemButton..hiOnItemCheckbox] * LHitInfo.HitPositions = [])
        and Assigned(LHitInfo.HitNode)
        and not IsMouseInCheckBox(LHitInfo.HitNode, LHitInfo.HitColumn) then
      begin
        if FTreeView.FocusedNode = LHitInfo.HitNode then
        begin
          inherited;
        end;
      end;
    end;
  end
  else
  begin
    if Assigned(ColumnDefinitions) and (LHitInfo.HitColumn > -1)
      and (LHitInfo.HitColumn < ColumnDefinitions.Count)
      and (ColumnDefinitions[LHitInfo.HitColumn].ToggleMode = tmDoubleClick) then
    begin
      ToggleIcon(LHitInfo.HitNode, LHitInfo.HitColumn);
    end;
  end;
end;

procedure TTreeViewPresenter.DoDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  LItem: TObject;
begin
  Allowed := AllowMove;
  LItem := GetNodeItem(Sender, Node);

  if Assigned(OnDragBegin) then
  begin
    OnDragBegin(Self, LItem, Allowed);
  end;
end;

procedure TTreeViewPresenter.DoDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer;
  Mode: VirtualTrees.TDropMode);
var
  i: Integer;
  LItem: TObject;
  LNode: PVirtualNode;
  LSelectedNodes: TNodeArray;
  LHandled: Boolean;
begin
  LNode := Sender.DropTargetNode;
  LItem := GetNodeItem(Sender, LNode);
  LHandled := False;

  LSelectedNodes := Sender.GetSortedSelection(False);
  if ssCtrl in Shift then
  begin
    if Assigned(OnDragDrop) then
    begin
      OnDragDrop(Sender, Source, LItem, doCopy, TDropMode(Mode), LHandled);
    end;
    Sender.ReinitNode(LNode, True);
  end
  else
  begin
    if Assigned(OnDragDrop) then
    begin
      OnDragDrop(Sender, Source, LItem, doMove, TDropMode(Mode), LHandled);
    end;

    if not LHandled and (Sender = Source) then
    begin
      Inc(FUpdateCount);
      try
        for i := Low(LSelectedNodes) to High(LSelectedNodes) do
        begin
          FCurrentNode := LSelectedNodes[i].Parent;
          case TDropMode(Mode) of
            dmNowhere: FTreeView.MoveTo(LSelectedNodes[i], nil, amAddChildLast, False);
            dmAbove: FTreeView.MoveTo(LSelectedNodes[i], LNode, amInsertBefore, False);
            dmOnNode:
            begin
              if FCurrentNode <> LNode then
              begin
                if FListMode then
                  FTreeView.MoveTo(LSelectedNodes[i], LNode, amInsertAfter, False)
                else
                begin
                  FTreeView.MoveTo(LSelectedNodes[i], LNode, amAddChildLast, False);
                  FTreeView.Sort(LNode, FTreeView.Header.SortColumn, FTreeView.Header.SortDirection);
                  ExpandNode(LNode);
                end;
              end;
            end;
            dmBelow: FTreeView.MoveTo(LSelectedNodes[i], LNode, amInsertAfter, False);
          end;
        end;
      finally
        Dec(FUpdateCount);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: VirtualTrees.TDropMode; var Effect: Integer; var Accept: Boolean);
var
  LItem: TObject;
  LNode: PVirtualNode;
  LItemNode: PVirtualNode;
begin
  if Pt.Y > -1 then
  begin
    LNode := Sender.GetNodeAt(Pt.X, Pt.Y);
    LItem := GetNodeItem(Sender, LNode);
    case TDropMode(Mode) of
      dmAbove, dmBelow: Accept := AllowMove;
    end;
    if Assigned(OnDragOver) then
    begin
      OnDragOver(Sender, Source, LItem, Accept);
    end
    else
    begin
      if Sender = Source then
      begin
        if not Assigned(LNode) then
        begin
          LNode := Sender.RootNode;
        end;
        Accept := Assigned(GetNodeItems(Sender, LNode));
        if Accept then
        begin
          for LItemNode in Sender.SelectedNodes do
          begin
            if (LItemNode = LNode) or (LItemNode.Parent = LNode) or
              Sender.HasAsParent(LNode, LItemNode) then
            begin
              Accept := False;
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if FTreeView.Header.SortColumn = Column then
  begin
    FTreeView.Sort(Node.Parent, FTreeView.Header.SortColumn, FTreeView.Header.SortDirection);
  end;
end;

procedure TTreeViewPresenter.DoEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Assigned(ColumnDefinitions) and (Column > -1)
    and (Column < ColumnDefinitions.Count)
    and (ColumnDefinitions[Column].ColumnType = ctText)
    and ColumnDefinitions[Column].AllowEdit;
end;

procedure TTreeViewPresenter.DoExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LItem: TObject;
begin
  if Assigned(FOnExpanded) then
  begin
    LItem := GetNodeItem(Sender, Node);
    FOnExpanded(Sender, LItem);
  end;

  UpdateExpandedItems();

  DoPropertyChanged('ExpandedItems');
end;

procedure TTreeViewPresenter.DoExpandedItemsChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState)
    and (FCollectionUpdateLock = 0) then
  begin
    LNode := FTreeView.GetFirst();
    while Assigned(LNode) do
    begin
      if GetNodeItem(FTreeView, LNode) = Item then
      begin
        case Action of
          caAdded: ExpandNode(LNode);
          caRemoved: FTreeView.Expanded[LNode] := False;
        end;
      end;
      LNode := FTreeView.GetNext(LNode);
    end;
  end;
end;

procedure TTreeViewPresenter.DoFilterNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LItem: TObject;
  LAccepted: Boolean;

  function IsLeaf(Node: PVirtualNode): Boolean;
  begin
    Result := Node.ChildCount = 0;
  end;

  function IsNodeWithNonFilteredChildren(Node: PVirtualNode): Boolean;
  begin
    Result := False;
    Sender.ValidateChildren(Node, False);
    Node := Sender.GetFirstChild(Node);
    while Assigned(Node) do
    begin
      if not (vsFiltered in Node.States) then
      begin
        Result := True;
        Break;
      end;
      Node := Sender.GetNextSibling(Node);
    end;
  end;

begin
  LItem := GetNodeItem(Sender, Node);
  case FFilterDirection of
    fdRootToLeafs: LAccepted := True;
    fdLeafsToRoot: LAccepted := IsLeaf(Node) or IsNodeWithNonFilteredChildren(Node);
  end;

  DoFilterItem(LItem, LAccepted);
  Sender.IsFiltered[Node] := not LAccepted;

  if Sender.IsFiltered[Node] and Sender.Selected[Node] then
  begin
    Sender.Selected[Node] := False;
  end;
end;

procedure TTreeViewPresenter.DoFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LItem: TObject;
begin
  LItem := GetNodeItem(Sender, Node);
  if Assigned(LItem) then
  begin
    // nothing to do here yet
  end;
end;

procedure TTreeViewPresenter.DoFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
var
  LItem: TObject;
begin
  if OldNode <> NewNode then
  begin
    LItem := GetNodeItem(Sender, NewNode);
    if Assigned(OnSelectionChanging) then
    begin
      OnSelectionChanging(Sender, LItem, Allowed);
      if not Allowed and Assigned(OldNode) then
      begin
        FTreeView.Selected[OldNode] := True;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  SetNodeItems(Sender, Node, nil);
  if FHitInfo.HitNode = Node then
  begin
    FHitInfo.HitNode := nil;
  end;
end;

procedure TTreeViewPresenter.DoGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  FCurrentNode := Node;
  DoPropertyChanged('ParentItem');

  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    HintText := LItemTemplate.GetHint(LItem, Column);
  end;
end;

procedure TTreeViewPresenter.DoGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  if Kind in [ikNormal, ikSelected] then
  begin
    LItem := GetNodeItem(Sender, Node);
    LItemTemplate := GetItemTemplate(LItem);
    if Assigned(LItemTemplate) then
    begin
      ImageIndex := LItemTemplate.GetImageIndex(LItem, Column);
    end;
  end;
end;

procedure TTreeViewPresenter.DoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  FCurrentNode := Node;
  DoPropertyChanged('ParentItem');

  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    if Assigned(ColumnDefinitions) and (Column > -1)
      and (Column < ColumnDefinitions.Count)
      and (ColumnDefinitions[Column].ColumnType <> TColumnType.ctText) then
    begin
      CellText := '';
    end
    else
    begin
      CellText := LItemTemplate.GetText(LItem, Column);
    end;
  end;
end;

procedure TTreeViewPresenter.DoHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
var
  LCursor: TCursor;
begin
  if FSorting and (HitInfo.Button = mbLeft) and (HitInfo.Column > -1)
    and (HitInfo.Column < ColumnDefinitions.Count)
    and (coSortable in ColumnDefinitions[HitInfo.Column].ColumnOptions) then
  begin
    LCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      if Sender.SortColumn <> HitInfo.Column then
      begin
        Sender.SortColumn := HitInfo.Column;
      end
      else
      begin
        if Sender.SortDirection = sdAscending then
        begin
          Sender.SortDirection := sdDescending;
        end
        else
        begin
          Sender.SortDirection := sdAscending;
        end;
      end;

      if Sender.SortColumn = -1 then
      begin
        Refresh();
      end;
    finally
      Screen.Cursor := LCursor;
    end;
  end;
end;

procedure TTreeViewPresenter.DoHeaderDblClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
var
  LCursor: TCursor;
begin
  if FSorting and (HitInfo.Button = mbLeft) and (HitInfo.Column > -1)
    and (HitInfo.Column < ColumnDefinitions.Count)
    and (coSortable in ColumnDefinitions[HitInfo.Column].ColumnOptions) then
  begin
    LCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      if Sender.SortColumn = HitInfo.Column then
      begin
        Sender.SortColumn := -1;
        Refresh();
      end;
    finally
      Screen.Cursor := LCursor;
    end;
  end;
end;

procedure TTreeViewPresenter.DoIncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: string; var Result: Integer);
var
  LCellText: string;
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  FCurrentNode := Node;
  DoPropertyChanged('ParentItem');

  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    LCellText := LItemTemplate.GetText(LItem, ColumnDefinitions.MainColumnIndex);
  end
  else
  begin
    LCellText := '';
  end;

  Result := StrLIComp(PChar(SearchText), PChar(LCellText),
    Min(Length(SearchText), Length(LCellText)));
end;

procedure TTreeViewPresenter.DoInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  LItem: TObject;
  LItems: IObjectList;
  LItemTemplate: IDataTemplate;
  LParentItem: TObject;
  LParentItems: IObjectList;
begin
  FCurrentNode := Node;
  DoPropertyChanged('ParentItem');

  case CheckSupport of
    csTriState: Node.CheckType := ctTriStateCheckBox;
    csRadio: Node.CheckType := ctRadioButton;
  else
    Node.CheckType := ctCheckBox;
  end;

  if Assigned(ParentNode) then
  begin
    LParentItems := GetNodeItems(Sender, ParentNode);
    if Assigned(LParentItems) then
    begin
      LItem := LParentItems[Node.Index];
    end
    else
    begin
      LParentItem := GetNodeItem(Sender, ParentNode);
      LItemTemplate := GetItemTemplate(LParentItem);
      LItem := LItemTemplate.GetItem(LParentItem, Node.Index);
    end;
  end
  else
  begin
    LItem := View.ItemTemplate.GetItem(View.ItemsSource.AsObject, Node.Index);
  end;

  SetNodeItem(Sender, Node, LItem);

  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    LItems := LItemTemplate.GetItems(LItem);
    SetNodeItems(Sender, Node, LItems);
    if Assigned(LItems) then
    begin
      Sender.ChildCount[Node] := LItems.Count;
    end
    else
    begin
      Sender.ChildCount[Node] := LItemTemplate.GetItemCount(LItem);
    end;

    if not LItemTemplate.IsCheckBoxVisible(LItem) then
    begin
      Node.CheckType := ctNone;
    end;
  end
  else
  begin
    Sender.ChildCount[Node] := 0;
  end;

  while Assigned(Node) do
  begin
    DoFilterNode(Sender, Node);
    Node := Sender.NodeParent[Node];
  end;
end;

procedure TTreeViewPresenter.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  LItemTemplate: IDataTemplate;
  i: Integer;
  LAllowed: Boolean;
  LNodes: TNodeArray;
begin
  if Shift = [] then
  begin
    case Key of
      VK_RETURN:
      begin
        if FTreeView.SelectedCount > 0 then
        begin
          if Assigned(Action) then
            Action.Execute
          else
          begin
            LItemTemplate := GetItemTemplate(View.CurrentItem);
            if Assigned(LItemTemplate) and Assigned(LItemTemplate.Action) then
              LItemTemplate.Action.Execute;
          end;
        end;
      end;
      VK_SPACE:
      begin
        if ToggleCheckBox(FTreeView.FocusedNode, FTreeView.FocusedColumn) then
          Key := 0;
      end;
    end;
  end;

  // moving elements with Ctrl+Up and Ctrl+Down only when sorting is off
  if AllowMove and (ssCtrl in Shift) and (FTreeView.Header.SortColumn = -1) then
  begin
    case Key of
      VK_UP:
      begin
        LAllowed := True;
        LNodes := FTreeView.GetSortedSelection(False);
        for i := Low(LNodes) to High(LNodes) do
        begin
          if LNodes[i].PrevSibling = nil then
          begin
            LAllowed := False;
            Break;
          end;
        end;
        if LAllowed then
        begin
          Inc(FUpdateCount);
          try
            for i := Low(LNodes) to High(LNodes) do
            begin
              FCurrentNode := LNodes[i].Parent;
              FTreeView.MoveTo(LNodes[i], LNodes[i].PrevSibling, amInsertBefore, False);
            end;
          finally
            Dec(FUpdateCount);
          end;
        end;
      end;
      VK_DOWN:
      begin
        LAllowed := True;
        LNodes := FTreeView.GetSortedSelection(False);
        for i := High(LNodes) downto Low(LNodes) do
        begin
          if LNodes[i].NextSibling = nil then
          begin
            LAllowed := False;
            Break;
          end;
        end;
        if LAllowed then
        begin
          Inc(FUpdateCount);
          try
            for i := High(LNodes) downto Low(LNodes) do
            begin
              FCurrentNode := LNodes[i].Parent;
              FTreeView.MoveTo(LNodes[i], LNodes[i].NextSibling, amInsertAfter, False);
            end;
          finally
            Dec(FUpdateCount);
          end;
        end;
      end;
    end;
  end;

  if Assigned(FOnKeyAction) then
  begin
    FOnKeyAction(Self, Key, Shift);
  end;
end;

procedure TTreeViewPresenter.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LHitInfo: THitInfo;
begin
  FChecking := False;
  if not (ssDouble in Shift)
    and not (tsVCLDragPending in FTreeView.TreeStates) then
  begin
    FTreeView.GetHitTestInfoAt(X, Y, True, LHitInfo);
    if not Assigned(LHitInfo.HitNode) then
    begin
      if FAllowClearSelection then
      begin
        FTreeView.FocusedNode := nil;
        if FTreeView.FocusedNode = nil then
          FTreeView.ClearSelection()
        else
          Abort;
      end
      else
      begin
        Abort;
      end;
    end
    else
    begin
      if IsMouseInCheckBox(LHitInfo.HitNode, LHitInfo.HitColumn) then
      begin
        FChecking := True;
        FTreeView.FocusedColumn := LHitInfo.HitColumn;
        FTreeView.RepaintNode(LHitInfo.HitNode);
        Abort;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  LHitInfo: THitInfo;
begin
  if GetAsyncKeyState(VK_LBUTTON) = 0 then
  begin
    FTreeView.GetHitTestInfoAt(X, Y, True, LHitInfo);

    if Assigned(LHitInfo.HitNode) and Assigned(FHitInfo.HitNode)
      and (FHitInfo.HitNode <> LHitInfo.HitNode) then
    begin
      FTreeView.RepaintNode(FHitInfo.HitNode);
    end;

    if Assigned(LHitInfo.HitNode) then
    begin
      FTreeView.RepaintNode(LHitInfo.HitNode);
    end;

    FHitInfo := LHitInfo;

    if IsMouseInCheckBox(LHitInfo.HitNode, LHitInfo.HitColumn) then
      FHitInfo.HitPositions := [hiOnItem, hiOnItemCheckbox];

    if IsMouseInToggleIcon(LHitInfo) then
      FHitInfo.HitPositions := [hiOnItem, hiOnNormalIcon];
  end;
end;

procedure TTreeViewPresenter.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LHitInfo: THitInfo;
  LItem: TObject;
  LItemTemplate: IDataTemplate;
  LColumnDefinition: TColumnDefinition;
begin
  if Assigned(FHitInfo.HitNode)
    and not (tsVCLDragPending in FTreeView.TreeStates) then
  begin
    FTreeView.GetHitTestInfoAt(X, Y, True, LHitInfo);
    if (FHitInfo.HitNode = LHitInfo.HitNode)
      and (FHitInfo.HitColumn = LHitInfo.HitColumn)
      and Assigned(ColumnDefinitions) and (LHitInfo.HitColumn > -1)
      and (LHitInfo.HitColumn < ColumnDefinitions.Count) then
    begin
      LItem := GetNodeItem(FTreeView, LHitInfo.HitNode);
      LItemTemplate := GetItemTemplate(LItem);
      LColumnDefinition := ColumnDefinitions[LHitInfo.HitColumn];

      if Assigned(LItemTemplate) then
      begin
        if IsMouseInCheckBox(LHitInfo.HitNode, LHitInfo.HitColumn)
          and FChecking and LColumnDefinition.AllowEdit then
        begin
          LItemTemplate.SetValue(LItem, LHitInfo.HitColumn,
            not LItemTemplate.GetValue(LItem, LHitInfo.HitColumn).AsBoolean);
        end;

        if (hiOnNormalIcon in FHitInfo.HitPositions)
          and IsMouseInToggleIcon(LHitInfo)
          and (LColumnDefinition.ToggleMode = tmClick) then
        begin
          ToggleIcon(LHitInfo.HitNode, LHitInfo.HitColumn);
        end;
      end;
    end;

    FTreeView.RepaintNode(FHitInfo.HitNode);
    if LHitInfo.HitNode <> FHitInfo.HitNode then
    begin
      FTreeView.RepaintNode(LHitInfo.HitNode);
    end;
  end;
end;

procedure TTreeViewPresenter.DoNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    LItemTemplate.SetText(LItem, Column, NewText);
  end;
end;

procedure TTreeViewPresenter.DoNodeMoved(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LItem: TObject;
  LItems: IObjectList;
begin
  LItem := GetNodeItem(Sender, Node);

  if Node.Parent <> FCurrentNode then
  begin
    LItems := GetNodeItems(Sender, FCurrentNode);
    if Assigned(LItems) then
    begin
      LItems.Extract(LItem);
    end;
  end;

  if Sender.GetNodeLevel(Node) = 0 then
  begin
    LItems := View.ItemsSource;
  end
  else
  begin
    LItems := GetNodeItems(Sender, Node.Parent);
  end;

  if Assigned(LItems) then
  begin
    if Node.Parent <> FCurrentNode then
    begin
      LItems.Insert(Node.Index, LItem);
    end
    else
    begin
      LItems.Move(LItems.IndexOf(LItem), Node.Index);
    end;
  end;
end;

procedure TTreeViewPresenter.DoPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  LItem: TObject;
  LItemTemplate: IControlTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  if Supports(GetItemTemplate(LItem), IControlTemplate, LItemTemplate) then
  begin
    LItemTemplate.CustomDraw(LItem, Column, TargetCanvas, CDefaultCellRect,
      ImageList, dmPaintText, Sender.Selected[Node]);
  end;
end;

procedure TTreeViewPresenter.DoSelectedItemsChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState)
    and (SelectionMode <> smNone) then
  begin
    if FCollectionUpdateLock = 0 then
    begin
      LNode := FTreeView.GetFirst();
      while Assigned(LNode) do
      begin
        if GetNodeItem(FTreeView, LNode) = Item then
        begin
          case Action of
            caAdded:
              begin
                FTreeView.Selected[LNode] := True;
                ExpandNode(LNode);
                if SelectionMode = smSingle then
                  FTreeView.ScrollIntoView(LNode, True);
              end;
            caRemoved: FTreeView.Selected[LNode] := False;
          end;
        end;
        LNode := FTreeView.GetNext(LNode);
      end;
    end;

    if FSyncMode and not FSyncing then
    try
      FSyncing := True;
      SetCheckedItems(FSelectedItems);
    finally
      FSyncing := False;
    end;
  end;
end;

procedure TTreeViewPresenter.DoSourceCollectionChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LParentNode, LNode: PVirtualNode;
  LSelectedNode: PVirtualNode;
  LItems: IObjectList;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState)
    and (FUpdateCount = 0) then
  begin
    case Action of
      caAdded:
      begin
        LNode := FTreeView.IterateSubtree(nil, GetItemsNode, Pointer(Sender));
        if Assigned(LNode) then
          FTreeView.ReinitNode(LNode, False)
        else
          Refresh;
      end;

      caRemoved:
      begin
        LParentNode := FTreeView.IterateSubtree(nil, GetItemsNode, Pointer(Sender));
        LNode := FTreeView.IterateSubtree(LParentNode, GetItemNode, Pointer(Item));
        if Assigned(LNode) then
        begin
          // find node to select after deleting current node
          if FTreeView.Selected[LNode] and (SelectionMode = smSingle) then
          begin
            LSelectedNode := FTreeView.GetNextVisibleSibling(LNode);

            if not Assigned(LSelectedNode) then
              LSelectedNode := FTreeView.GetPreviousVisibleSibling(LNode);
            if not Assigned(LSelectedNode) then
              LSelectedNode := LNode.Parent;
            if Assigned(LSelectedNode) then
            begin
              FTreeView.Selected[LSelectedNode] := True;
              FTreeView.FocusedNode := LSelectedNode;
            end;
          end;

          FTreeView.DeleteNode(LNode);
        end
        else
        begin
          LItems := GetNodeItems(FTreeView, LParentNode);
          if Assigned(LItems) then
            FTreeView.ChildCount[LParentNode] := LItems.Count
          else
            FTreeView.ChildCount[LParentNode] := 0;
        end;
      end;

      caReplaced:
      begin
        LNode := FTreeView.IterateSubtree(nil, GetItemsNode, Pointer(Sender));
        FTreeView.Sort(LNode, FTreeView.Header.SortColumn, FTreeView.Header.SortDirection);
      end;

      caMoved:
      begin
        LNode := FTreeView.IterateSubtree(nil, GetItemsNode, Pointer(Sender));
        if Assigned(LNode) then
        begin
          FTreeView.ReinitChildren(LNode, False);
          FTreeView.InvalidateChildren(LNode, True);
        end
        else
          Refresh;
      end;

      caReseted:
      begin
        ResetRootNodeCount;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DrawCheckBox(TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect; Value: Boolean);
var
  LThemedButton: TThemedButton;
  LCheckBoxRect: TRect;
  LDetails: TThemedElementDetails;
  LState: Cardinal;
begin
  LCheckBoxRect := CalcCheckBoxRect(CellRect);

  if (Column > -1) and (Column < ColumnDefinitions.Count)
    and ColumnDefinitions[Column].AllowEdit then
  begin
    if Value then
    begin
      LThemedButton := tbCheckBoxCheckedNormal;
    end
    else
    begin
       LThemedButton := tbCheckBoxUncheckedNormal;
    end;

    if IsMouseInCheckBox(Node, Column) then
    begin
      Inc(LThemedButton);
    end;
  end
  else
  begin
    if Value then
    begin
      LThemedButton := tbCheckBoxCheckedDisabled;
    end
    else
    begin
      LThemedButton := tbCheckBoxUncheckedDisabled;
    end;
  end;

  if (FHitInfo.HitNode = Node) and (FHitInfo.HitColumn = Column)
    and (hiOnItemCheckbox in FHitInfo.HitPositions)
    and (GetAsyncKeyState(VK_LBUTTON) <> 0)
    and ColumnDefinitions[FHitInfo.HitColumn].AllowEdit then
  begin
    if Value then
    begin
      LThemedButton := tbCheckBoxCheckedPressed;
    end
    else
    begin
      LThemedButton := tbCheckBoxUncheckedPressed;
    end;
  end;

  if StyleServices.Enabled and
    (toThemeAware in FTreeView.TreeOptions.PaintOptions) then
  begin
    LDetails := StyleServices.GetElementDetails(LThemedButton);
    StyleServices.DrawElement(TargetCanvas.Handle, LDetails, LCheckBoxRect);
  end
  else
  begin
    LState := DFCS_BUTTONCHECK;
    if LThemedButton in [tbCheckBoxCheckedNormal..tbCheckBoxCheckedDisabled] then
    begin
      LState := LState or DFCS_CHECKED;
    end;

    if LThemedButton in [tbCheckBoxUncheckedDisabled, tbCheckBoxCheckedDisabled] then
    begin
      LState := LState or DFCS_INACTIVE;
    end;

    DrawFrameControl(TargetCanvas.Handle, LCheckBoxRect, DFC_BUTTON, LState);
  end;
end;

procedure TTreeViewPresenter.DrawImage(TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect; Value: Integer);
var
  LRect: TRect;
begin
  if Assigned(ImageList) then
  begin
    LRect := CalcImageRect(CellRect);
    ImageList.Draw(TargetCanvas, LRect.Left, LRect.Top, Value);
  end;
end;

procedure TTreeViewPresenter.DrawProgressBar(TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect; Value: Integer);
var
  LDetails: TThemedElementDetails;
begin
  if StyleServices.Enabled and
    (toThemeAware in FTreeView.TreeOptions.PaintOptions) then
  begin
    InflateRect(CellRect, -1, -1);
    LDetails := StyleServices.GetElementDetails(tpBar);
    StyleServices.DrawElement(TargetCanvas.Handle, LDetails, CellRect, nil);
    InflateRect(CellRect, -2, -2);
    CellRect.Right := CellRect.Left + Trunc(RectWidth(CellRect) * Value / 100);
    LDetails := StyleServices.GetElementDetails(tpChunk);
    StyleServices.DrawElement(TargetCanvas.Handle, LDetails, CellRect, nil);
  end
  else
  begin
    InflateRect(CellRect, -1, -1);
    FProgressBar.Position := Value;
    FProgressBar.Height := RectHeight(CellRect);
    FProgressBar.Width := RectWidth(CellRect);
    FProgressBar.PaintTo(TargetCanvas, CellRect.Left, 1);
  end;
end;

procedure TTreeViewPresenter.EndUpdate;
begin
  inherited;
  FTreeView.EndUpdate();
end;

procedure TTreeViewPresenter.ExpandNode(Node: PVirtualNode);
begin
  if Assigned(FTreeView) then
  begin
    while Assigned(Node) do
    begin
      if [vsChecking..vsExpanded] * Node.States = [] then
      begin
        FTreeView.Expanded[Node] := True;
      end;
      Node := FTreeView.NodeParent[Node];
    end;
  end;
end;

procedure TTreeViewPresenter.FullCollapse;
begin
  if Assigned(FTreeView) then
  begin
    FTreeView.FullCollapse();
  end;
end;

procedure TTreeViewPresenter.FullExpand;
begin
  if Assigned(FTreeView) then
  begin
    FTreeView.FullExpand();
  end;
end;

function TTreeViewPresenter.GetCanMoveCurrentToNext: Boolean;
var
  LNode: PVirtualNode;
begin
  Result := False;

  if Assigned(FTreeView) then
  begin
    LNode := FTreeView.GetFirstSelected();
    LNode := FTreeView.GetNextVisible(LNode);
    Result := Assigned(LNode);
  end;
end;

function TTreeViewPresenter.GetCanMoveCurrentToPrevious: Boolean;
var
  LNode: PVirtualNode;
begin
  Result := False;

  if Assigned(FTreeView) then
  begin
    LNode := FTreeView.GetFirstSelected();
    LNode := FTreeView.GetPreviousVisible(LNode);
    Result := Assigned(LNode);
  end;
end;

function TTreeViewPresenter.GetCheckedItem: TObject;
begin
  if FCheckedItems.Count > 0 then
  begin
    Result := FCheckedItems[0];
  end
  else
  begin
    Result := nil;
  end;
end;

function TTreeViewPresenter.GetCheckedItems: IList<TObject>;
begin
  UpdateCheckedItems();
  Result := FCheckedItems;
end;

function TTreeViewPresenter.GetCurrentItem: TObject;
begin
  Result := GetSelectedItem();
end;

function TTreeViewPresenter.GetExpandedItems: IList<TObject>;
begin
  UpdateExpandedItems();
  Result := FExpandedItems;
end;

procedure TTreeViewPresenter.GetItemNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  Abort := GetNodeItem(Sender, Node) = TObject(Data);
end;

procedure TTreeViewPresenter.GetItemsNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Sender.GetNodeData(Node));
  Abort := Assigned(LNodeData) and Assigned(LNodeData.Items)
    and (LNodeData.Items.AsObject = TObject(Data));
end;

function TTreeViewPresenter.GetNodeItem(Tree: TBaseVirtualTree;
  Node: PVirtualNode): TObject;
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    Result := LNodeData.Item;
  end
  else
  begin
    Result := nil;
  end;
end;

function TTreeViewPresenter.GetNodeItems(Tree: TBaseVirtualTree;
  Node: PVirtualNode): IObjectList;
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    Result := LNodeData.Items;
  end
  else
  begin
    if Node = Tree.RootNode then
    begin
      Result := View.ItemsSource;
    end
    else
    begin
      Result := nil;
    end;
  end;
end;

function TTreeViewPresenter.GetParentItem(const Level: Integer): TObject;
var
  LLevel: Integer;
  LNode: PVirtualNode;
begin
  Result := GetNodeItem(FTreeView, FCurrentNode);

  LLevel := 0;
  LNode := FCurrentNode;
  while Assigned(LNode) and (LLevel < Level) do
  begin
    LNode := LNode.Parent;
    Result := GetNodeItem(FTreeView, LNode);
    Inc(LLevel);
  end;
end;

function TTreeViewPresenter.GetSelectedItem: TObject;
begin
  if (FSelectedItems.Count > 0) and (SelectionMode <> smNone) then
  begin
    Result := FSelectedItems[0];
  end
  else
  begin
    Result := nil;
  end;
end;

function TTreeViewPresenter.GetSelectedItems: IList<TObject>;
begin
  if (FSelectedItems.Count > 0) and (SelectionMode = smNone) then
  begin
    FSelectedItems.Clear;
  end;
  Result := FSelectedItems;
end;

procedure TTreeViewPresenter.InitColumns;
var
  i: Integer;
begin
  if Assigned(FTreeView) and UseColumnDefinitions then
  begin
    FTreeView.Header.Options := FTreeView.Header.Options - [hoAutoResize];
    FTreeView.Header.Columns.Clear;
    if Assigned(ColumnDefinitions) then
    begin
      for i := 0 to Pred(ColumnDefinitions.Count) do
      begin
        with FTreeView.Header.Columns.Add do
        begin
          Alignment := ColumnDefinitions[i].Alignment;
          MinWidth := ColumnDefinitions[i].MinWidth;
          Text := ColumnDefinitions[i].Caption;
          Width := ColumnDefinitions[i].Width;
          Options := Options + [coUseCaptionAlignment, coSmartResize];
          if not ColumnDefinitions[i].Visible then
          begin
            Options := Options - [coVisible];
          end;
          if not (TColumnOption.coResizable in ColumnDefinitions[i].ColumnOptions) then
          begin
            Options := Options - [coResizable];
          end;
          if not (TColumnOption.coDraggable in ColumnDefinitions[i].ColumnOptions) then
          begin
            Options := Options - [coDraggable];
          end;
        end;
      end;
      FTreeView.Header.MainColumn := ColumnDefinitions.MainColumnIndex;
      FTreeView.Header.AutoSizeIndex := ColumnDefinitions.AutoSizeIndex;
      if FTreeView.Header.AutoSizeIndex = -1 then
      begin
        FTreeView.Header.Options := FTreeView.Header.Options - [hoAutoResize];
      end
      else
      begin
        FTreeView.Header.Options := FTreeView.Header.Options + [hoAutoResize];
      end;
      if FSorting then
      begin
        FTreeView.Header.SortColumn := ColumnDefinitions.SortColumnIndex;
        if FTreeView.Header.SortColumn > -1 then
        begin
          FTreeView.Header.SortDirection := TSortDirection(
            Ord(ColumnDefinitions[FTreeView.Header.SortColumn].SortingDirection) - 1);
        end;
      end
      else
      begin
        FTreeView.Header.SortColumn := -1;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.InitControl;
begin
  if Assigned(FTreeView) and ([csDesigning, csDestroying] * ComponentState = []) then
  begin
    FTreeView.Images := ImageList;
    FTreeView.NodeDataSize := SizeOf(TNodeData);
    FTreeView.PopupMenu := PopupMenu;

    InitColumns();
    InitProperties();
    InitEvents();
    ResetRootNodeCount();
  end;
end;

procedure TTreeViewPresenter.InitEvents;
begin
  if Assigned(FTreeView) and not (csDesigning in ComponentState) then
  begin
    FTreeView.OnAfterCellPaint := DoAfterCellPaint;
    FTreeView.OnBeforeCellPaint := DoBeforeCellPaint;
    FTreeView.OnChange := DoChange;
    FTreeView.OnChecked := DoChecked;
    FTreeView.OnCollapsed := DoCollapsed;
    FTreeView.OnCompareNodes := DoCompareNodes;
    FTreeView.OnDblClick := DoDblClick;
    FTreeView.OnDragAllowed := DoDragAllowed;
    FTreeView.OnDragDrop := DoDragDrop;
    FTreeView.OnDragOver := DoDragOver;
    FTreeView.OnEdited := DoEdited;
    FTreeView.OnEditing := DoEditing;
    FTreeView.OnExpanded := DoExpanded;
    FTreeView.OnFocusChanged := DoFocusChanged;
    FTreeView.OnFocusChanging := DoFocusChanging;
    FTreeView.OnFreeNode := DoFreeNode;
    FTreeView.OnGetHint := DoGetHint;
    FTreeView.OnGetImageIndex := DoGetImageIndex;
    FTreeView.OnGetText := DoGetText;
    FTreeView.OnHeaderClick := DoHeaderClick;
    FTreeView.OnHeaderDblClick := DoHeaderDblClick;
    FTreeView.OnIncrementalSearch := DoIncrementalSearch;
    FTreeView.OnInitNode := DoInitNode;
    FTreeView.OnKeyDown := DoKeyDown;
    FTreeView.OnMouseDown := DoMouseDown;
    FTreeView.OnMouseMove := DoMouseMove;
    FTreeView.OnMouseUp := DoMouseUp;
    FTreeView.OnNewText := DoNewText;
    FTreeView.OnNodeMoved := DoNodeMoved;
    FTreeView.OnPaintText := DoPaintText;
  end;
end;

procedure TTreeViewPresenter.InitProperties;
begin
  if Assigned(FTreeView) and not (csDesigning in ComponentState) then
  begin
    if AllowMove then
    begin
      FTreeView.DragMode := dmAutomatic;
    end
    else
    begin
      FTreeView.DragMode := dmManual;
    end;

    if CheckSupport <> csNone then
    begin
      FTreeView.TreeOptions.MiscOptions :=
        FTreeView.TreeOptions.MiscOptions + [toCheckSupport];
    end
    else
    begin
      FTreeView.TreeOptions.MiscOptions :=
        FTreeView.TreeOptions.MiscOptions - [toCheckSupport];
    end;

    if Assigned(ImageList) then
    begin
      FTreeView.DefaultNodeHeight := ImageList.Height + 2;
    end;

    if FListMode then
    begin
      FTreeView.TreeOptions.PaintOptions :=
        FTreeView.TreeOptions.PaintOptions - [toShowButtons, toShowRoot, toShowTreeLines];
    end
    else
    begin
      FTreeView.TreeOptions.PaintOptions :=
        FTreeView.TreeOptions.PaintOptions + [toShowButtons, toShowRoot, toShowTreeLines];
    end;

    if SelectionMode = smSingle then
    begin
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions - [toMultiSelect];
    end
    else
    begin
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions + [toMultiSelect];
    end;

    if SelectionMode = smLevel then
    begin
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions + [toLevelSelectConstraint];
    end
    else
    begin
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions - [toLevelSelectConstraint];
    end;

    if SelectionMode = smNone then
    begin
      FTreeView.TreeOptions.PaintOptions :=
        FTreeView.TreeOptions.PaintOptions + [toHideSelection, toAlwaysHideSelection];
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions + [toDisableDrawSelection];
    end
    else
    begin
      FTreeView.TreeOptions.PaintOptions :=
        FTreeView.TreeOptions.PaintOptions - [toHideSelection, toAlwaysHideSelection];
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions - [toDisableDrawSelection];
    end;

    if ShowHeader then
    begin
      FTreeView.Header.Options := FTreeView.Header.Options + [hoVisible];
    end
    else
    begin
      FTreeView.Header.Options := FTreeView.Header.Options - [hoVisible];
    end;

    if FSorting then
    begin
      FTreeView.TreeOptions.AutoOptions :=
        FTreeView.TreeOptions.AutoOptions - [toAutoDeleteMovedNodes] + [toAutoSort];
    end
    else
    begin
      FTreeView.TreeOptions.AutoOptions :=
        FTreeView.TreeOptions.AutoOptions - [toAutoDeleteMovedNodes] - [toAutoSort];
    end;

    FTreeView.Header.Options := FTreeView.Header.Options + [hoDblClickResize{, hoHeaderClickAutoSort}];

    FTreeView.HintMode := hmHintAndDefault;
    FTreeView.IncrementalSearch := isAll;
    FTreeView.ShowHint := True;

    FTreeView.TreeOptions.AutoOptions :=
      FTreeView.TreeOptions.AutoOptions + [toAutoHideButtons];
    FTreeView.TreeOptions.MiscOptions :=
      FTreeView.TreeOptions.MiscOptions - [toToggleOnDblClick] + [toFullRowDrag];
    FTreeView.TreeOptions.PaintOptions :=
      FTreeView.TreeOptions.PaintOptions + [toHotTrack, toUseExplorerTheme, toHideTreeLinesIfThemed];
    FTreeView.TreeOptions.SelectionOptions :=
      FTreeView.TreeOptions.SelectionOptions + [toExtendedFocus, toFullRowSelect, toRightClickSelect, toSimpleDrawSelection];
  end;
end;

function TTreeViewPresenter.IsMouseInCheckBox(Node: PVirtualNode;
  Column: TColumnIndex): Boolean;
var
  LCursorPos: TPoint;
  LHitInfo: THitInfo;
  LRect: TRect;
begin
  if Assigned(Node) and Assigned(ColumnDefinitions) and (Column > -1)
    and (Column < ColumnDefinitions.Count)
    and (ColumnDefinitions[Column].ColumnType = TColumnType.ctCheckBox) then
  begin
    LCursorPos := FTreeView.ScreenToClient(Mouse.CursorPos);
    FTreeView.GetHitTestInfoAt(LCursorPos.X, LCursorPos.Y, True, LHitInfo);
    LRect := FTreeView.GetDisplayRect(Node, Column, False);
    LRect := CalcCheckBoxRect(LRect);
    Result := PtInRect(LRect, LCursorPos);
  end
  else
  begin
    Result := False;
  end;
end;

function TTreeViewPresenter.IsMouseInToggleIcon(HitInfo: THitInfo): Boolean;
var
  LCursorPos: TPoint;
  LRect: TRect;
begin
  if Assigned(ColumnDefinitions) and (HitInfo.HitColumn > -1)
    and (HitInfo.HitColumn < ColumnDefinitions.Count) then
  begin
    if ColumnDefinitions[HitInfo.HitColumn].ColumnType = ctImage then
    begin
      LCursorPos := FTreeView.ScreenToClient(Mouse.CursorPos);
      LRect := FTreeView.GetDisplayRect(HitInfo.HitNode, HitInfo.HitColumn, False);
      LRect := CalcImageRect(LRect);
      if PtInRect(LRect, LCursorPos) then
      begin
        Include(HitInfo.HitPositions, hiOnNormalIcon);
      end;
    end;

    Result := (hiOnNormalIcon in HitInfo.HitPositions)
      and (ColumnDefinitions[HitInfo.HitColumn].ToggleMode <> tmNone);
  end
  else
  begin
    Result := False;
  end;
end;

procedure TTreeViewPresenter.MoveCurrentToFirst;
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    LNode := FTreeView.GetFirstVisible();
    if Assigned(LNode) then
    begin
      FTreeView.Selected[LNode] := True;
      if FTreeView.HandleAllocated then
      begin
        FTreeView.ScrollIntoView(LNode, True, True);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.MoveCurrentToLast;
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    LNode := FTreeView.GetLastVisible();
    if Assigned(LNode) then
    begin
      FTreeView.Selected[LNode] := True;
      if FTreeView.HandleAllocated then
      begin
        FTreeView.ScrollIntoView(LNode, True, True);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.MoveCurrentToNext;
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    LNode := FTreeView.GetFirstSelected();
    LNode := FTreeView.GetNextVisibleSibling(LNode);
    if Assigned(LNode) then
    begin
      FTreeView.Selected[LNode] := True;
      if FTreeView.HandleAllocated then
      begin
        FTreeView.ScrollIntoView(LNode, True, True);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.MoveCurrentToPrevious;
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    LNode := FTreeView.GetFirstSelected();
    LNode := FTreeView.GetPreviousVisibleSibling(LNode);
    if Assigned(LNode) then
    begin
      FTreeView.Selected[LNode] := True;
      if FTreeView.HandleAllocated then
      begin
        FTreeView.ScrollIntoView(LNode, True, True);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FTreeView then
      FTreeView := nil;
  end;
end;

procedure TTreeViewPresenter.ReadMultiSelect(Reader: TReader);
begin
  if Reader.ReadBoolean then
  begin
    SelectionMode := smMulti;
  end
  else
  begin
    SelectionMode := smSingle;
  end;
end;

procedure TTreeViewPresenter.Refresh;
var
  LCheckedItems: IList<TObject>;
  LExpandedItems: IList<TObject>;
  LSelectedItems: IList<TObject>;
begin
  if Assigned(FTreeView) and not (csDesigning in ComponentState) then
  begin
    LCheckedItems := TList<TObject>.Create();
    LCheckedItems.AddRange(CheckedItems);
    LExpandedItems := TList<TObject>.Create();
    LExpandedItems.AddRange(ExpandedItems);
    LSelectedItems := TList<TObject>.Create();
    LSelectedItems.AddRange(SelectedItems);
    ResetRootNodeCount();
    CheckedItems := LCheckedItems;
    ExpandedItems := LExpandedItems;
    SelectedItems := LSelectedItems;
  end;
end;

procedure TTreeViewPresenter.ResetRootNodeCount;
begin
  if Assigned(FTreeView) then
  begin
    if Assigned(View.ItemsSource) and Assigned(View.ItemTemplate) then
    begin
      FTreeView.Clear;
      FTreeView.RootNodeCount := View.ItemTemplate.GetItemCount(View.ItemsSource.AsObject);
    end
    else
    begin
      FTreeView.RootNodeCount := 0;
    end;
  end;
end;

procedure TTreeViewPresenter.SetCheckedItem(const Value: TObject);
begin
  FCheckedItems.Clear();
  if Assigned(Value) then
  begin
    FCheckedItems.Add(Value);
  end;
  SetCheckedItems(FCheckedItems);
end;

procedure TTreeViewPresenter.SetCheckedItems(const Value: IList<TObject>);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(Value) then
  begin
    LNode := FTreeView.GetFirstInitialized();
    while Assigned(LNode) do
    begin
      LItem := GetNodeItem(FTreeView, LNode);
      if Assigned(LItem) and Value.Contains(LItem) then
      begin
        FTreeView.CheckState[LNode] := csCheckedNormal;
      end
      else
      begin
        FTreeView.CheckState[LNode] := csUncheckedNormal;
      end;
      LNode := FTreeView.GetNextInitialized(LNode);
    end;
  end;
end;

procedure TTreeViewPresenter.SetCurrentItem(const Value: TObject);
begin
  SetSelectedItem(Value);
end;

procedure TTreeViewPresenter.SetExpandedItems(const Value: IList<TObject>);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    FTreeView.FullCollapse();
    if Assigned(Value) and (Value.Count > 0) then
    begin
      LNode := FTreeView.GetFirst();
      while Assigned(LNode) do
      begin
        LItem := GetNodeItem(FTreeView, LNode);
        if Assigned(LItem) and Value.Contains(LItem) then
        begin
          ExpandNode(LNode);
        end;
        LNode := FTreeView.GetNext(LNode);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.SetListMode(const Value: Boolean);
begin
  FListMode := Value;
  InitProperties();
end;

procedure TTreeViewPresenter.SetNodeItem(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Item: TObject);
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    LNodeData.Item := Item;
  end;
end;

procedure TTreeViewPresenter.SetNodeItems(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Items: IObjectList);
var
  LNodeData: PNodeData;
  LCollectionChanged: IEvent<TCollectionChangedEvent>;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    if Assigned(LNodeData.Items) then
    begin
      LCollectionChanged := IEvent<TCollectionChangedEvent>(LNodeData.Items.OnChanged);
      LCollectionChanged.Remove(DoSourceCollectionChanged);
    end;
    LNodeData.Items := Items;
    if Assigned(LNodeData.Items) then
    begin
      LNodeData.Items := Items;
      LCollectionChanged := IEvent<TCollectionChangedEvent>(LNodeData.Items.OnChanged);
      LCollectionChanged.Add(DoSourceCollectionChanged);
    end;
  end;
end;

procedure TTreeViewPresenter.SetSelectedItem(const Value: TObject);
begin
  if ((Value <> SelectedItem) or (SelectedItems.Count > 1))
    and (SelectionMode <> smNone) then
  begin
    FSelectedItems.Clear();
    if Assigned(Value) then
    begin
      FSelectedItems.Add(Value);
    end;
    SetSelectedItems(FSelectedItems);
  end;
end;

procedure TTreeViewPresenter.SetSelectedItems(const Value: IList<TObject>);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in FTreeView.ComponentState)
    and (SelectionMode <> smNone) then
  begin
    FTreeView.BeginUpdate();
    FTreeView.ClearSelection();
    if Assigned(Value) and (Value.Count > 0) then
    begin
      LNode := FTreeView.GetFirst();
      while Assigned(LNode) do
      begin
        LItem := GetNodeItem(FTreeView, LNode);
        if Assigned(LItem) and Value.Contains(LItem) then
        begin
          FTreeView.Selected[LNode] := True;
        end;
        LNode := FTreeView.GetNext(LNode);
      end;
      LNode := FTreeView.GetFirstSelected();
      FTreeView.FocusedNode := LNode;
      if Assigned(LNode) and (FCollectionUpdateLock = 0)
        and FTreeView.HandleAllocated then
      begin
        FTreeView.ScrollIntoView(LNode, True, True);
      end;
    end;
    FTreeView.EndUpdate();
  end;
end;

procedure TTreeViewPresenter.SetSorting(const Value: Boolean);
begin
  FSorting := Value;
  if Assigned(FTreeView) and not FSorting then
  begin
    FTreeView.Header.SortColumn := -1;
    Refresh();
  end;
  InitProperties();
end;

procedure TTreeViewPresenter.SetTreeView(const Value: TVirtualStringTree);
begin
  if FTreeView <> Value then
  begin
    if Assigned(FTreeView) then
    begin
      FTreeView.RemoveFreeNotification(Self);
    end;

    FTreeView := Value;

    if Assigned(FTreeView) then
    begin
      FTreeView.FreeNotification(Self);
    end;

    if not (csDesigning in ComponentState) then
    begin
      FProgressBar.Parent := FTreeView;
    end;
    InitControl();
  end;
end;

function TTreeViewPresenter.ToggleCheckBox(Node: PVirtualNode;
  Column: TColumnIndex): Boolean;
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
  LColumnDefinition: TColumnDefinition;
begin
  Result := False;
  if Assigned(ColumnDefinitions) and (FTreeView.FocusedColumn > -1)
    and (FTreeView.FocusedColumn < ColumnDefinitions.Count) then
  begin
    LItem := GetNodeItem(FTreeView, FTreeView.FocusedNode);
    LItemTemplate := GetItemTemplate(LItem);
    LColumnDefinition := ColumnDefinitions[FTreeView.FocusedColumn];

    if Assigned(LItemTemplate) and LColumnDefinition.AllowEdit
      and (LColumnDefinition.ColumnType = TColumnType.ctCheckBox) then
    begin
      LItemTemplate.SetValue(LItem, FTreeView.FocusedColumn,
        not LItemTemplate.GetValue(LItem, FTreeView.FocusedColumn).AsBoolean);
      Result := True;
    end;
  end;
end;

procedure TTreeViewPresenter.ToggleIcon(Node: PVirtualNode;
  Column: TColumnIndex);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
  LColumnDefinition: TColumnDefinition;
  LValue: TValue;

  procedure ToggleValue;
  begin
    if LValue.AsOrdinal < LValue.TypeData.MaxValue then
    begin
      TValue.Make(LValue.AsOrdinal + 1, LValue.TypeInfo, LValue);
    end
    else
    begin
      TValue.Make(LValue.TypeData.MinValue, LValue.TypeInfo, LValue);
    end;
  end;

begin
  LItem := GetNodeItem(FTreeView, Node);
  LColumnDefinition := ColumnDefinitions[Column];
  if LColumnDefinition.ColumnType = ctImage then
  begin
    LItemTemplate := GetItemTemplate(LItem);
    if Assigned(LItemTemplate) then
    begin
      LValue := LItemTemplate.GetValue(LItem, Column);
      if LValue.IsOrdinal then
      begin
        ToggleValue;
        LItemTemplate.SetValue(LItem, Column, LValue);
      end;
    end;
  end
  else
  begin
    if Assigned(LColumnDefinition.ImageIndexPropertyExpression) then
    begin
      LColumnDefinition.ImageIndexPropertyExpression.Instance := LItem;
      LValue := LColumnDefinition.ImageIndexPropertyExpression.Value;
      if LValue.IsOrdinal then
      begin
        ToggleValue;
        LColumnDefinition.ImageIndexPropertyExpression.Value := LValue;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.UpdateCheckedItems;
var
  LItem: TObject;
  LCheckedItems: IList<TObject>;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    Inc(FCollectionUpdateLock);
    LCheckedItems := TList<TObject>.Create();
    try
      LNode := FTreeView.GetFirstChecked();
      while Assigned(LNode) do
      begin
        LItem := GetNodeItem(FTreeView, LNode);
        if Assigned(LItem) then
        begin
          LCheckedItems.Add(LItem);
        end;
        LNode := FTreeView.GetNextChecked(LNode);
      end;

      UpdateList(FCheckedItems, LCheckedItems);
    finally
      Dec(FCollectionUpdateLock);
    end;
  end;
end;

procedure TTreeViewPresenter.UpdateExpandedItems;
var
  LItem: TObject;
  LExpandedItems: IList<TObject>;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    Inc(FCollectionUpdateLock);
    LExpandedItems := TList<TObject>.Create();
    try
      LNode := FTreeView.GetFirstInitialized();
      while Assigned(LNode) do
      begin
        LItem := GetNodeItem(FTreeView, LNode);
        if Assigned(LItem) and FTreeView.Expanded[LNode] then
        begin
          LExpandedItems.Add(LItem);
        end;
        LNode := FTreeView.GetNextInitialized(LNode);
      end;

      UpdateList(FExpandedItems, LExpandedItems);
    finally
      Dec(FCollectionUpdateLock);
    end;
  end;
end;

procedure TTreeViewPresenter.UpdateSelectedItemPath;
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and (SelectionMode <> smNone) then
  begin
    FSelectedItemPath.Clear;

    LItem := SelectedItem;
    if Assigned(LItem) then
    begin
      FSelectedItemPath.Add(LItem);
      LNode := FTreeView.IterateSubtree(nil, GetItemNode, Pointer(LItem));

      if Assigned(LNode) then
      begin
        LNode := FTreeView.NodeParent[LNode];
        while Assigned(LNode) do
        begin
          LItem := GetNodeItem(FTreeView, LNode);
          if Assigned(LItem) then
          begin
            FSelectedItemPath.Insert(0, LItem);
          end;
          LNode := FTreeView.NodeParent[LNode];
        end;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.UpdateSelectedItems;
var
  i: Integer;
  LItem: TObject;
  LSelectedItems: IList<TObject>;
  LSelectedNodes: TNodeArray;
begin
  if Assigned(FTreeView) and (SelectionMode <> smNone) then
  begin
    Inc(FCollectionUpdateLock);
    LSelectedItems := TList<TObject>.Create();
    try
      LSelectedNodes := FTreeView.GetSortedSelection(False);

      for i := Low(LSelectedNodes) to High(LSelectedNodes) do
      begin
        LItem := GetNodeItem(FTreeView, LSelectedNodes[i]);
        if Assigned(LItem) then
        begin
          LSelectedItems.Add(LItem);
        end;
      end;

      UpdateList(FSelectedItems, LSelectedItems);
    finally
      Dec(FCollectionUpdateLock);
    end;
  end;
end;

initialization
  CheckBoxSize := GetSystemMetrics(SM_CYMENUCHECK);

end.
