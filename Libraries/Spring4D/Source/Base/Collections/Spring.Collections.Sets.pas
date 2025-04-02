{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2024 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Collections.Sets;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Trees,
  Spring.HashTable;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  /// <summary>
  ///   The abstract base class for all set implementations.
  /// </summary>
  TSetBase<T> = class abstract(TCollectionBase<T>)
  protected
    function CreateSet: ISet<T>; virtual; abstract;
  public
    procedure ExceptWith(const other: IEnumerable<T>);
    procedure IntersectWith(const other: IEnumerable<T>);
    procedure UnionWith(const other: IEnumerable<T>);
    function IsSubsetOf(const other: IEnumerable<T>): Boolean;
    function IsSupersetOf(const other: IEnumerable<T>): Boolean;
    function SetEquals(const other: IEnumerable<T>): Boolean;
    function Overlaps(const other: IEnumerable<T>): Boolean;
  end;

  THashSetItem<T> = packed record
  public
    HashCode: Integer;
    Item: T;
  end;

  /// <summary>
  ///   Represents a set of values.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of elements in the hash set.
  /// </typeparam>
  THashSet<T> = class(TSetBase<T>, IInterface, IEnumerable<T>,
    IReadOnlyCollection<T>, ICollection<T>, ISet<T>, IOrderedSet<T>)
  private type
  {$REGION 'Nested Types'}
    TItem = THashSetItem<T>;
    TItems = TArray<TItem>;
    PItem = ^TItem;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: THashSet<T>;
      fIndex: Integer;
      fVersion: Integer;
      fItem: PItem;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fHashTable: THashTable;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; inline;
    function GetCount: Integer;
    function GetItemByIndex(index: Integer): T;
    function GetNonEnumeratedCount: Integer;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}
  protected
    function CreateSet: ISet<T>; override;
    function TryGetElementAt(var item: T; index: Integer): Boolean;
    property Capacity: Integer read GetCapacity;
  public
    constructor Create(capacity: Integer; const comparer: IEqualityComparer<T>);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const item: T): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    function Add(const item: T): Boolean;
    procedure AddRange(const values: array of T); overload;
    function Remove(const item: T): Boolean;
    function Extract(const item: T): T;
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements ISet<T>'}
    procedure TrimExcess;
  {$ENDREGION}

  {$REGION 'Implements IOrderedSet<T>'}
    function IndexOf(const key: T): Integer;
  {$ENDREGION}
  end;

  TSortedSet<T> = class(TSetBase<T>, IEnumerable<T>,
    IReadOnlyCollection<T>, ICollection<T>, ISet<T>)
  private type
  {$REGION 'Nested Types'}
    PNode = ^TNode;
    TNode = packed record // same layout as TRedBlackTreeBase<T>.TNode
      Parent, Right, Left: PNode;
      Key: T;
    end;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: TSortedSet<T>;
      fNode: PBinaryTreeNode;
      fVersion: Integer;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fTree: TRedBlackTreeBase<T>;
    fVersion: Integer;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetNonEnumeratedCount: Integer;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}
  protected
    function CreateSet: ISet<T>; override;
  public
    constructor Create(const comparer: IComparer<T>);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const item: T): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    function Add(const item: T): Boolean;
    function Remove(const item: T): Boolean;
    function Extract(const item: T): T;
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements ISet<T>'}
    procedure TrimExcess;
  {$ENDREGION}
  end;

  TFoldedHashSet<T> = class(THashSet<T>)
  private
    fElementType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(elementType: PTypeInfo;
      capacity: Integer;
      const comparer: IEqualityComparer<T>);
  end;

implementation

uses
  TypInfo,
  Spring.Comparers,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TSetBase<T>'}

procedure TSetBase<T>.ExceptWith(const other: IEnumerable<T>);
begin
  ICollection<T>(this).RemoveRange(other);
end;

procedure TSetBase<T>.IntersectWith(const other: IEnumerable<T>);
var
  count, capacity: NativeInt;
  enumerator: IEnumerator<T>;
  items: TArray<T>;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  count := 0;
  capacity := 0;
  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    if count >= capacity then
      capacity := DynArrayGrow(Pointer(items), TypeInfo(TArray<T>), capacity);
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(items[count])
    else
    {$ENDIF}
    items[count] := enumerator.Current;
    Inc(count, Ord(not other.Contains(items[count])));
  end;
  if count > 0 then
  begin
    SetLength(items, count);
    ICollection<T>(this).RemoveRange(items);
  end;
end;

function TSetBase<T>.IsSubsetOf(const other: IEnumerable<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    if not other.Contains(item) then
      Exit(False);
  end;

  Result := True;
end;

function TSetBase<T>.IsSupersetOf(const other: IEnumerable<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  enumerator := other.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    if not IEnumerable<T>(this).Contains(item) then
      Exit(False);
  end;

  Result := True;
end;

function TSetBase<T>.Overlaps(const other: IEnumerable<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  enumerator := other.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    if IEnumerable<T>(this).Contains(item) then
      Exit(True);
  end;

  Result := False;
end;

function TSetBase<T>.SetEquals(const other: IEnumerable<T>): Boolean;
var
  localSet: ISet<T>;
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  if other = IEnumerable<T>(this) then
    Exit(True);

  localSet := CreateSet;

  enumerator := other.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    localSet.Add(item);
    if not IEnumerable<T>(this).Contains(item) then
      Exit(False);
  end;

  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    if not localSet.Contains(item) then
      Exit(False);
  end;

  Result := True;
end;

procedure TSetBase<T>.UnionWith(const other: IEnumerable<T>);
begin
  ICollection<T>(this).AddRange(other);
end;

{$ENDREGION}


{$REGION 'THashSet<T>'}

constructor THashSet<T>.Create(capacity: Integer; const comparer: IEqualityComparer<T>);
begin
  fHashTable.Comparer := comparer;
  fHashTable.ItemsInfo := TypeInfo(TItems);
  SetCapacity(capacity);
end;

procedure THashSet<T>.AfterConstruction;
var
  elementType: PTypeInfo;
begin
  inherited AfterConstruction;

  elementType := GetElementType;
  fHashTable.Initialize(@TComparerThunks<T>.Equals, @TComparerThunks<T>.GetHashCode, elementType);
  {$IFDEF DELPHIXE7_UP}
  if fHashTable.DefaultComparer then
    fHashTable.Find := @THashTable<T>.FindWithoutComparer
  else
  {$ENDIF}
    fHashTable.Find := @THashTable<T>.FindWithComparer;
end;

procedure THashSet<T>.BeforeDestruction;
begin
  Clear;
  inherited BeforeDestruction;
end;

function THashSet<T>.CreateSet: ISet<T>;
begin
  Result := THashSet<T>.Create(0, IEqualityComparer<T>(fHashTable.Comparer));
end;

procedure THashSet<T>.SetCapacity(value: Integer);
begin
  fHashTable.Capacity := value;
end;

procedure THashSet<T>.TrimExcess;
begin
  fHashTable.Capacity := fHashTable.Count;
end;

function THashSet<T>.TryGetElementAt(var item: T; index: Integer): Boolean;
begin
  if Cardinal(index) < Cardinal(fHashTable.Count) then
  begin
    fHashTable.EnsureCompact;
    item := TItems(fHashTable.Items)[index].Item;
    Exit(True);
  end;
  item := Default(T);
  Result := False;
end;

function THashSet<T>.Add(const item: T): Boolean;
var
  entry: PItem;
begin
  entry := IHashTable<T>(@fHashTable).Find(item, IgnoreExisting or InsertNonExisting);
  if not Assigned(entry) then Exit(Boolean(Pointer(entry)));
  entry.Item := item;
  DoNotify(item, caAdded);
  Result := True;
end;

procedure THashSet<T>.AddRange(const values: array of T);
var
  i: NativeInt;
  entry: PItem;
begin
  fHashTable.Capacity := fHashTable.Count + Length(values);
  for i := 0 to High(values) do
  begin
    entry := IHashTable<T>(@fHashTable).Find(values[i], IgnoreExisting or InsertNonExisting);
    if not Assigned(entry) then Continue;
    entry.Item := values[i];
    if Assigned(Notify) then
      Notify(Self, entry.Item, caAdded);
  end;
end;

procedure THashSet<T>.Clear;
var
  item: PItem;
  i: Integer;
begin
  if Assigned(Notify) then
  begin
    fHashTable.ClearCount;
    item := PItem(fHashTable.Items);
    for i := 1 to fHashTable.ItemCount do //FI:W528
      if item.HashCode >= 0 then
        Notify(Self, item.Item, caRemoved);
  end;

  fHashTable.Clear;
end;

function THashSet<T>.Contains(const item: T): Boolean;
var
  entry: PItem;
begin
  entry := IHashTable<T>(@fHashTable).Find(item);
  Result := Assigned(entry);
end;

function THashSet<T>.Extract(const item: T): T;
var
  entry: PItem;
begin
  entry := IHashTable<T>(@fHashTable).Find(item, DeleteExisting);
  if Assigned(entry) then
  begin
    if Assigned(Notify) then
      Notify(Self, entry.Item, caExtracted);
    Result := entry.Item;
    entry.Item := Default(T);
  end
  else
    Result := Default(T);
end;

function THashSet<T>.GetEnumerator: IEnumerator<T>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fVersion := Self.fHashTable.Version;
  end;
end;

function THashSet<T>.GetCapacity: Integer;
begin
  Result := fHashTable.Capacity;
end;

function THashSet<T>.GetCount: Integer;
begin
  Result := fHashTable.Count;
end;

function THashSet<T>.GetItemByIndex(index: Integer): T; //FI:W521
begin
  if Cardinal(index) < Cardinal(fHashTable.Count) then
  begin
    fHashTable.EnsureCompact;
    Exit(TItems(fHashTable.Items)[index].Item);
  end;
  RaiseHelper.ArgumentOutOfRange_Index;
  __SuppressWarning(Result);
end;

function THashSet<T>.GetNonEnumeratedCount: Integer;
begin
  Result := fHashTable.Count;
end;

function THashSet<T>.IndexOf(const key: T): Integer;
var
  entry: THashTableEntry;
begin
  entry.HashCode := IEqualityComparer<T>(fHashTable.Comparer).GetHashCode(key);
  fHashTable.EnsureCompact;
  if fHashTable.FindEntry(key, entry) then
    Exit(entry.ItemIndex);
  Result := -1;
end;

function THashSet<T>.Remove(const item: T): Boolean;
var
  temp: Pointer;
  entry: PItem;
begin
  temp := IHashTable<T>(@fHashTable).Find(item, DeleteExisting);
  if not Assigned(temp) then Exit(Boolean(Pointer(temp)));
  entry := temp;
  if Assigned(Notify) then
    Notify(Self, entry.Item, caRemoved);
  entry.Item := Default(T);
  Result := True;
end;

function THashSet<T>.ToArray: TArray<T>;
var
  target: ^T;
  source: PItem;
  i: Integer;
begin
  SetLength(Result, fHashTable.Count);
  target := Pointer(Result);
  if Assigned(target) then
  begin
    source := Pointer(fHashTable.Items);
    for i := 1 to fHashTable.ItemCount do //FI:W528
    begin
      if source.HashCode >= 0 then
      begin
        target^ := source.Item;
        Inc(target);
      end;
      Inc(source);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'THashSet<T>.TEnumerator'}

function THashSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fItem.Item;
end;

function THashSet<T>.TEnumerator.MoveNext: Boolean;
var
  hashTable: PHashTable;
  item: PItem;
begin
  hashTable := @fSource.fHashTable;
  if fVersion = hashTable.Version then
  begin
    repeat
      if fIndex >= hashTable.ItemCount then
        Break;

      item := @TItems(hashTable.Items)[fIndex];
      Inc(fIndex);
      if item.HashCode >= 0 then
      begin
        fItem := item;
        Exit(True);
      end;
    until False;
    Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TSortedSet<T>'}

constructor TSortedSet<T>.Create(const comparer: IComparer<T>);
begin
  fComparer := comparer;
end;

procedure TSortedSet<T>.AfterConstruction;
begin
  inherited AfterConstruction;

  fTree := TRedBlackTreeBase<T>.Create(fComparer);
end;

procedure TSortedSet<T>.BeforeDestruction;
begin
  Clear;
  fTree.Free;
  inherited BeforeDestruction;
end;

function TSortedSet<T>.CreateSet: ISet<T>;
begin
  Result := TSortedSet<T>.Create(fComparer);
end;

function TSortedSet<T>.Add(const item: T): Boolean;
var
  node: Pointer;
begin
  node := fTree.AddNode(item);
  if not Assigned(node) then Exit(Boolean(node));
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if Assigned(Notify) then
    Notify(Self, PNode(node).Key, caAdded);
  Result := True;
end;

procedure TSortedSet<T>.Clear;
var
  node: Pointer;
begin
  if fTree.Count = 0 then
    Exit;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if Assigned(Notify) then // optimization: if no notification needs to be send the entire tree traversal won't be done
  begin
    node := fTree.Root.LeftMost;
    while Assigned(node) do
      Notify(Self, PNode(node).Key, caRemoved);
  end;

  fTree.Clear;
end;

function TSortedSet<T>.Contains(const item: T): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(item);
  Result := Assigned(node);
end;

function TSortedSet<T>.Extract(const item: T): T;
var
  node: Pointer;
begin
  node := fTree.FindNode(item);
  if Assigned(node) then
  begin
    Result := PNode(node).Key;
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    fTree.DeleteNode(node);
    DoNotify(Result, caExtracted);
  end
  else
    Result := Default(T);
end;

function TSortedSet<T>.GetCapacity: Integer;
begin
  Result := fTree.Capacity;
end;

function TSortedSet<T>.GetCount: Integer;
begin
  Result := fTree.Count;
end;

function TSortedSet<T>.GetEnumerator: IEnumerator<T>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fVersion := Self.fVersion;
  end;
end;

function TSortedSet<T>.GetNonEnumeratedCount: Integer;
begin
  Result := fTree.Count;
end;

function TSortedSet<T>.Remove(const item: T): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(item);
  if not Assigned(node) then Exit(Boolean(node));
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fTree.DeleteNode(node);
  DoNotify(item, caRemoved);
  Result := True;
end;

function TSortedSet<T>.ToArray: TArray<T>;
var
  tree: TBinaryTree;
  i: Integer;
  node: PBinaryTreeNode;
begin
  tree := fTree;
  SetLength(Result, tree.Count);
  i := 0;
  node := tree.Root.LeftMost;
  while Assigned(node) do
  begin
    Result[i] := PNode(node).Key;
    node := node.Next;
    Inc(i);
  end;
end;

{$ENDREGION}


{$REGION 'TSortedSet<T>.TEnumerator'}

function TSortedSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := PNode(fNode).Key;
end;

function TSortedSet<T>.TEnumerator.MoveNext: Boolean;
var
  node: Pointer;
begin
  if fVersion = fSource.fVersion then
  begin
    if fNode <> Pointer(1) then
    begin
      if Assigned(fNode) then
        node := fNode.Next
      else
        node := fSource.fTree.Root.LeftMost;
      if Assigned(node) then
      begin
        fNode := node;
        Exit(True);
      end;
      fNode := Pointer(1);
    end;
    Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

procedure TSortedSet<T>.SetCapacity(value: Integer);
begin
  fTree.Capacity := value;
end;

procedure TSortedSet<T>.TrimExcess;
begin
  fTree.TrimExcess;
end;

{$ENDREGION}


{$REGION 'TFoldedHashSet<T>'}

constructor TFoldedHashSet<T>.Create(elementType: PTypeInfo; capacity: Integer;
  const comparer: IEqualityComparer<T>);
begin
  fHashTable.Comparer := comparer;
  fHashTable.ItemsInfo := TypeInfo(TItems);
  SetCapacity(capacity);
  fElementType := elementType;
end;

function TFoldedHashSet<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


end.
