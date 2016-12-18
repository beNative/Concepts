(*
  Copyright (c) 2011, Stefan Glienke
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

unit DSharp.Bindings.CollectionView.VCLAdapters;

interface

uses
  Classes,
  ComCtrls,
  DSharp.Bindings.CollectionView.Adapters,
  DSharp.Bindings.VCLControls,
  Grids;

type
  TCollectionViewListItemsAdapter = class(TCollectionViewAdapter)
  private
    FItems: TListItems;
    FColumns: TListColumns;
  protected
    function AddDisplayItem: NativeInt; override;
    procedure ClearDisplayItems; override;
    function GetDisplayItemsCount: NativeInt; override;
    function FindDisplayItem(AItem: TObject): NativeInt; override;
    procedure RemoveDisplayItem(AIndex: NativeInt); override;
    procedure UpdateDisplayItem(AIndex: NativeInt; AItem: TObject); override;

    function GetCurrentItem: TObject; override;
    procedure SetItemIndex(const Value: NativeInt); override;
    procedure UpdateItems(AClearItems: Boolean = False); override;
  public
    constructor Create(AOwner: TPersistent; AItems: TListItems;
      AColumns: TListColumns);
    destructor Destroy; override;
  end;

  TCollectionViewPageControlAdapter = class(TCollectionViewAdapter)
  private
    FPageControl: TPageControl;
  protected
    function AddDisplayItem: NativeInt; override;
    procedure ClearDisplayItems; override;
    function GetDisplayItemsCount: NativeInt; override;
    function FindDisplayItem(AItem: TObject): NativeInt; override;
    procedure RemoveDisplayItem(AIndex: NativeInt); override;
    procedure UpdateDisplayItem(AIndex: NativeInt; AItem: TObject); override;

    function GetCurrentItem: TObject; override;
    procedure SetItemIndex(const Value: NativeInt); override;
    procedure UpdateItems(AClearItems: Boolean = False); override;
  public
    constructor Create(AOwner: TPersistent; APageControl: TPageControl);
    destructor Destroy; override;
  end;

  TCollectionViewStringGridAdapter = class(TCollectionViewAdapter)
  private
    FEmpty: Boolean;
    FGrid: TStringGrid;
  protected
    procedure ClearRow(AIndex: NativeInt); virtual;
    procedure DeleteRow(AIndex: NativeInt); virtual;

    function AddDisplayItem: NativeInt; override;
    procedure ClearDisplayItems; override;
    function GetDisplayItemsCount: NativeInt; override;
    function FindDisplayItem(AItem: TObject): NativeInt; override;
    procedure RemoveDisplayItem(AIndex: NativeInt); override;
    procedure UpdateDisplayItem(AIndex: NativeInt; AItem: TObject); override;

    function GetCurrentItem: TObject; override;
    procedure SetItemIndex(const Value: NativeInt); override;
    procedure UpdateItems(AClearItems: Boolean = False); override;

    property Grid: TStringGrid read FGrid;
  public
    constructor Create(AOwner: TPersistent; AGrid: TStringGrid);
    destructor Destroy; override;
  end;

  TCollectionViewTreeNodesAdapter = class(TCollectionViewAdapter)
  private
    FItems: TTreeNodes;
  protected
    function AddDisplayItem: NativeInt; override;
    procedure ClearDisplayItems; override;
    function GetDisplayItemsCount: NativeInt; override;
    function FindDisplayItem(AItem: TObject): NativeInt; override;
    procedure RemoveDisplayItem(AIndex: NativeInt); override;
    procedure UpdateDisplayItem(AIndex: NativeInt; AItem: TObject); override;

    function GetCurrentItem: TObject; override;
    procedure SetItemIndex(const Value: NativeInt); override;
    procedure UpdateItems(AClearItems: Boolean = False); override;
  public
    constructor Create(AOwner: TPersistent; AItems: TTreeNodes);
    destructor Destroy; override;
  end;

implementation

uses
  DSharp.Bindings.Notifications,
  DSharp.Core.DataTemplates,
  DSharp.Core.Reflection,
  Rtti;

type
  TStringGridAccess = class(TStringGrid);

{ TCollectionViewListItemsAdapter }

constructor TCollectionViewListItemsAdapter.Create(AOwner: TPersistent;
  AItems: TListItems; AColumns: TListColumns);
begin
  inherited Create(AOwner);
  FItems := AItems;
  FColumns := AColumns;
end;

destructor TCollectionViewListItemsAdapter.Destroy;
begin
  FColumns := nil;
  FItems := nil;
  inherited;
end;

procedure TCollectionViewListItemsAdapter.ClearDisplayItems;
begin
  FItems.Clear();
end;

function TCollectionViewListItemsAdapter.AddDisplayItem: NativeInt;
var
  LListItem: TListItem;
begin
  LListItem := FItems.Add;
  Result := LListItem.Index;
end;

function TCollectionViewListItemsAdapter.GetCurrentItem: TObject;
begin
  if Assigned(FItems) and(ItemIndex > -1) then
  begin
    Result := FItems[ItemIndex].Data;
  end
  else
  begin
    Result := nil;
  end;
end;

function TCollectionViewListItemsAdapter.GetDisplayItemsCount: NativeInt;
begin
  Result := FItems.Count;
end;

function TCollectionViewListItemsAdapter.FindDisplayItem(AItem: TObject): NativeInt;
var
  i: NativeInt;
begin
  Result := -1;
  for i := 0 to Pred(FItems.Count) do
  begin
    if FItems[i].Data = AItem then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TCollectionViewListItemsAdapter.RemoveDisplayItem(AIndex: NativeInt);
begin
  if AIndex = ItemIndex then
  begin
    FItems.Delete(AIndex);
    UpdateItemIndex(nil);
    NotifyPropertyChanged(FOwner, Self, 'View');
  end
  else
  begin
    FItems.Delete(AIndex);
  end;
end;

procedure TCollectionViewListItemsAdapter.UpdateDisplayItem(AIndex: NativeInt;
  AItem: TObject);
var
  i: NativeInt;
  LListItem: TListItem;
begin
  LListItem := FItems[AIndex];
  LListItem.Data := AItem;
  LListItem.SubItems.Clear();
  for i := 0 to Pred(FColumns.Count) do
  begin
    if i = 0 then
    begin
      LListItem.Caption := ItemTemplate.GetText(AItem, i);
    end
    else
    begin
      LListItem.SubItems.Add(ItemTemplate.GetText(AItem, i));
    end;
  end;
end;

procedure TCollectionViewListItemsAdapter.SetItemIndex(const Value: NativeInt);
var
  LProperty: TRttiProperty;
begin
  inherited;

  if FOwner.TryGetProperty('ItemIndex', LProperty) then
  begin
    LProperty.SetValue(FOwner, Value);
  end;

  NotifyPropertyChanged(FOwner, Self, 'View');
end;

procedure TCollectionViewListItemsAdapter.UpdateItems(AClearItems: Boolean);
begin
  if Assigned(FItems) then
  begin
    inherited;
  end;
end;

{ TCollectionViewPageControlAdapter }

constructor TCollectionViewPageControlAdapter.Create(AOwner: TPersistent;
  APageControl: TPageControl);
begin
  inherited Create(AOwner);
  FPageControl := APageControl;
end;

destructor TCollectionViewPageControlAdapter.Destroy;
begin
  FPageControl := nil;
  inherited;
end;

function TCollectionViewPageControlAdapter.AddDisplayItem: NativeInt;
var
  LTabSheet: TTabSheet;
begin
  LTabSheet := TTabSheet.Create(FPageControl);
  LTabSheet.PageControl := FPageControl;
  Result := LTabSheet.TabIndex;
end;

procedure TCollectionViewPageControlAdapter.ClearDisplayItems;
begin
  while FPageControl.PageCount > 0 do
  begin
    RemoveDisplayItem(FPageControl.PageCount - 1);
  end;
end;

function TCollectionViewPageControlAdapter.FindDisplayItem(
  AItem: TObject): NativeInt;
var
  i: NativeInt;
begin
  Result := -1;
  for i := 0 to Pred(FPageControl.PageCount) do
  begin
    if TObject(FPageControl.Pages[i].Tag) = AItem then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TCollectionViewPageControlAdapter.GetCurrentItem: TObject;
begin
  if Assigned(FPageControl) and (ItemIndex > -1) then
  begin
    Result := TObject(FPageControl.Pages[ItemIndex].Tag);
  end
  else
  begin
    Result := nil;
  end;
end;

function TCollectionViewPageControlAdapter.GetDisplayItemsCount: NativeInt;
begin
  Result := FPageControl.PageCount;
end;

procedure TCollectionViewPageControlAdapter.RemoveDisplayItem(
  AIndex: NativeInt);
begin
  if not (csDestroying in FPageControl.Pages[AIndex].ComponentState) then
  begin
    FPageControl.Pages[AIndex].Free;
  end;
end;

procedure TCollectionViewPageControlAdapter.SetItemIndex(
  const Value: NativeInt);
var
  LProperty: TRttiProperty;
begin
  inherited;

  if FOwner.TryGetProperty('TabIndex', LProperty) then
  begin
    LProperty.SetValue(FOwner, Value);
  end;

  NotifyPropertyChanged(FOwner, Self, 'View');
end;

procedure TCollectionViewPageControlAdapter.UpdateDisplayItem(AIndex: NativeInt;
  AItem: TObject);
begin
  FPageControl.Pages[AIndex].Tag := NativeInt(AItem);
end;

procedure TCollectionViewPageControlAdapter.UpdateItems(AClearItems: Boolean);
begin
  if Assigned(FPageControl) then
  begin
    inherited;
  end;
end;

{ TCollectionViewStringGridAdapter }

constructor TCollectionViewStringGridAdapter.Create(AOwner: TPersistent;
  AGrid: TStringGrid);
begin
  inherited Create(AOwner);
  FGrid := AGrid;
end;

destructor TCollectionViewStringGridAdapter.Destroy;
begin
  FGrid := nil;
  inherited;
end;

function TCollectionViewStringGridAdapter.AddDisplayItem: NativeInt;
begin
  if FEmpty then
  begin
    FEmpty := False;
    Result := 0;
  end
  else
  begin
    FGrid.RowCount := FGrid.RowCount + 1;
    Result := FGrid.RowCount - FGrid.FixedRows - 1;
  end;
end;

procedure TCollectionViewStringGridAdapter.ClearDisplayItems;
begin
  FGrid.RowCount := FGrid.FixedRows + 1;
  ClearRow(FGrid.RowCount - 1);
  FEmpty := True;
end;

procedure TCollectionViewStringGridAdapter.ClearRow(AIndex: NativeInt);
begin
  with FGrid.Rows[AIndex + FGrid.FixedRows] do
  begin
    Text := '';
    Objects[0] := nil;
  end;
end;

procedure TCollectionViewStringGridAdapter.DeleteRow(AIndex: NativeInt);
begin
  if FGrid.RowCount > FGrid.FixedRows + 1 then
  begin
    TStringGridAccess(FGrid).DeleteRow(AIndex + FGrid.FixedRows);
  end
  else
  begin
    ClearRow(AIndex);
  end;
end;

function TCollectionViewStringGridAdapter.FindDisplayItem(
  AItem: TObject): NativeInt;
begin
  Result := FGrid.Cols[0].IndexOfObject(AItem);
  if Result > -1 then
  begin
    Result := Result - FGrid.FixedRows;
  end;
end;

function TCollectionViewStringGridAdapter.GetCurrentItem: TObject;
begin
  if Assigned(FGrid) and (FItemIndex > -1) then
  begin
    Result := FGrid.Cols[0].Objects[FItemIndex + FGrid.FixedRows];
  end
  else
  begin
    Result := nil;
  end;
end;

function TCollectionViewStringGridAdapter.GetDisplayItemsCount: NativeInt;
begin
  Result := FGrid.RowCount - FGrid.FixedRows;
end;

procedure TCollectionViewStringGridAdapter.RemoveDisplayItem(AIndex: NativeInt);
begin
  if AIndex = ItemIndex then
  begin
    DeleteRow(AIndex);
    UpdateItemIndex(nil);
    NotifyPropertyChanged(FOwner, Self, 'View');
  end
  else
  begin
    DeleteRow(AIndex);
  end;
end;

procedure TCollectionViewStringGridAdapter.SetItemIndex(const Value: NativeInt);
var
  LProperty: TRttiProperty;
begin
  if FItemIndex <> Value then
  begin
    inherited;

    if FOwner.TryGetProperty('Row', LProperty) then
    begin
      if Value > -1 then
        LProperty.SetValue(FOwner, Value + FGrid.FixedRows)
      else
        LProperty.SetValue(FOwner, FGrid.FixedRows);
    end;

    NotifyPropertyChanged(FOwner, Self, 'View');
  end;
end;

procedure TCollectionViewStringGridAdapter.UpdateDisplayItem(AIndex: NativeInt;
  AItem: TObject);
var
  i: Integer;
begin
  FGrid.Cols[0].Objects[AIndex + FGrid.FixedRows] := AItem;
  for i := FGrid.FixedCols to FGrid.ColCount - 1 do
  begin
    FGrid.Cells[i, AIndex + FGrid.FixedRows] := ItemTemplate.GetText(AItem, i - FGrid.FixedCols);
  end;
end;

procedure TCollectionViewStringGridAdapter.UpdateItems(AClearItems: Boolean);
begin
  if Assigned(FGrid) then
  begin
    inherited;
  end;
end;

{ TCollectionViewTreeNodesAdapter }

constructor TCollectionViewTreeNodesAdapter.Create(AOwner: TPersistent;
  AItems: TTreeNodes);
begin
  inherited Create(AOwner);
  FItems := AItems;
end;

destructor TCollectionViewTreeNodesAdapter.Destroy;
begin
  FItems := nil;
  inherited;
end;

function TCollectionViewTreeNodesAdapter.AddDisplayItem: NativeInt;
begin
  Result := NativeInt(FItems.AddObject(nil, '', nil));
end;

procedure TCollectionViewTreeNodesAdapter.ClearDisplayItems;
begin
  FItems.Clear();
end;

function TCollectionViewTreeNodesAdapter.FindDisplayItem(
  AItem: TObject): NativeInt;
var
  i: NativeInt;
begin
  Result := -1;
  for i := 0 to Pred(FItems.Count) do
  begin
    if FItems[i].Data = AItem then
    begin
      Result := NativeInt(FItems[i]);
      Break;
    end;
  end;
end;

function TCollectionViewTreeNodesAdapter.GetCurrentItem: TObject;
begin
  if Assigned(FItems) and (FItemIndex > 0) then
  begin
    Result := TTreeNode(FItemIndex).Data;
  end
  else
  begin
    Result := nil;
  end;
end;

function TCollectionViewTreeNodesAdapter.GetDisplayItemsCount: NativeInt;
begin
  Result := FItems.Count;
end;

procedure TCollectionViewTreeNodesAdapter.RemoveDisplayItem(AIndex: NativeInt);
begin
  if AIndex = ItemIndex then
  begin
    ItemIndex := NativeInt(TTreeNode(AIndex).GetPrev());
    FItems.Delete(TTreeNode(AIndex));
    NotifyPropertyChanged(FOwner, Self, 'View');
  end
  else
  begin
    FItems.Delete(TTreeNode(AIndex));
  end;
end;

procedure TCollectionViewTreeNodesAdapter.SetItemIndex(const Value: NativeInt);
var
  LProperty: TRttiProperty;
begin
  inherited;

  if FOwner.TryGetProperty('Selected', LProperty) then
  begin
    if Value > 0 then
      LProperty.SetValue(FOwner, TTreeNode(Value))
    else
      LProperty.SetValue(FOwner, nil);
  end;

  NotifyPropertyChanged(FOwner, Self, 'View');
end;

procedure TCollectionViewTreeNodesAdapter.UpdateDisplayItem(AIndex: NativeInt;
  AItem: TObject);

  procedure CreateNodes(ANode: TTreeNode; AItemTemplate: IDataTemplate);
  var
    i: Integer;
    LItem: TObject;
    LItemTemplate: IDataTemplate;
    LTreeNode: TTreeNode;
  begin
    if Assigned(AItemTemplate) then
    begin
      for i := 0 to Pred(AItemTemplate.GetItemCount(ANode.Data)) do
      begin
        LItem := AItemTemplate.GetItem(ANode.Data, i);
        LItemTemplate := AItemTemplate.GetItemTemplate(LItem);
        LTreeNode := FItems.AddChildObject(ANode, LItemTemplate.GetText(LItem, -1), LItem);
        CreateNodes(LTreeNode, LItemTemplate);
      end;
    end;
  end;

var
  LItemTemplate: IDataTemplate;
  LTreeNode: TTreeNode;
begin
  LTreeNode := TTreeNode(AIndex);
  LItemTemplate := ItemTemplate.GetItemTemplate(AItem);
  LTreeNode.Text := LItemTemplate.GetText(AItem, -1);
  LTreeNode.Data := AItem;
  LTreeNode.DeleteChildren();
  CreateNodes(LTreeNode, LItemTemplate);
end;

procedure TCollectionViewTreeNodesAdapter.UpdateItems(AClearItems: Boolean);
begin
  if Assigned(FItems) then
  begin
    inherited;
  end;
end;

end.
