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

unit Spring.Collections.MultiMaps;

interface

uses
  Classes,
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Lists,
  Spring.Collections.Sets,
  Spring.Collections.Trees,
  Spring.Events.Base,
  Spring.HashTable;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  IGroupingInternal<TKey, TElement> = interface(IGrouping<TKey, TElement>)
    // IMPORTANT NOTICE:
    // keep this in sync with ICollection<T> in Spring.Collections
    // we are using some hack to keep their IMT indexes compatible
    // GetOnChanged is replaced by GetKey in IGrouping
    function Add(const item: TElement): Boolean;
    procedure AddRange(const values: array of TElement); overload;
    procedure AddRange(const values: IEnumerable<TElement>); overload;
    function Extract(const item: TElement): TElement;
    procedure Clear;
    function MoveTo(const collection: ICollection<TElement>): Integer; overload;
    function MoveTo(const collection: ICollection<TElement>;
      const predicate: Predicate<TElement>): Integer; overload;
    function Remove(const item: TElement): Boolean;
  end;

  TCollectionFactory = procedure (const key; const comparer: IInterface; elementType: PTypeInfo; var result);

  // same layout as TCollectionWrapper<T>
  TCollectionWrapper = class sealed(TEnumerableBase)
  private type
  {$REGION 'Nested Types'}
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      // same layout as TCollectionWrapper<T>.TEnumerator
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TCollectionWrapper;
      fCollection: IEnumerable;
      fEnumerator: IEnumerator;
      procedure ValidateEnumerator;
      function MoveNext: Boolean;
    end;
  {$ENDREGION}
  protected
    // TEnumerableBase<T>
    fComparer: IInterface;
    fCollection: IEnumerable;
    fWrappers: TList;
    fUpdateValues: TNotifyEvent;
    procedure RefreshIfEmpty;
    procedure HandleDestroy(Sender: TObject);
    function GetCount: Integer;
    function GetElementType: PTypeInfo;
    procedure GetEnumerator(enumerator: PPointer; vtable: PEnumeratorVtable;
      typeInfo, getCurrent: Pointer);
    function GetNonEnumeratedCount: Integer;
  public
    class procedure Create(classType: TClass; const collection: IEnumerable;
      wrappers: TList; updateValues: TNotifyEvent;
      collectionFactory: TCollectionFactory; const key;
      const valueComparer: IInterface; elementType: PTypeInfo; var result); static;

    procedure BeforeDestruction; reintroduce;
  end;

  // same layout as TCollectionWrapper
  TCollectionWrapper<T> = class(TEnumerableBase<T>, IEnumerable<T>, IReadOnlyCollection<T>)
  private type
  {$REGION 'Nested Types'}
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      // same layout as TCollectionWrapper.TEnumerator
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TCollectionWrapper;
      fCollection: IReadOnlyCollection<T>;
      fEnumerator: IEnumerator<T>;
      function GetCurrent: T;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  protected
    fCollection: IReadOnlyCollection<T>;
    fWrappers: TList;
    fUpdateValues: TNotifyEvent;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetElementType: PTypeInfo; override;
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}
  public
    procedure BeforeDestruction; override;

    function Contains(const value: T;
      const comparer: IEqualityComparer<T>): Boolean; overload;
    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;
  end;

  TValueList<TKey, T> = class(TAbstractArrayList<T>, IInterface, IEnumerable<T>,
    IReadOnlyCollection<T>, IGrouping<TKey, T>, IGroupingInternal<TKey, T>)
  private
    fKey: TKey;
    fElementType: PTypeInfo;
    function GetElementType: PTypeInfo; reintroduce;
    function GetKey: TKey;
    function Add(const item: T): Boolean;
  public
    constructor Create(const key: TKey; elementType: PTypeInfo);
  end;

  TValueHashSet<TKey, T> = class(THashSet<T>, IGrouping<TKey, T>, IGroupingInternal<TKey, T>)
  private
    fKey: TKey;
    fElementType: PTypeInfo;
    function GetElementType: PTypeInfo; reintroduce;
    function GetKey: TKey;
  public
    constructor Create(const key: TKey; elementType: PTypeInfo;
      const comparer: IEqualityComparer<T>);
  end;

  TValueTreeSet<TKey, T> = class(TSortedSet<T>, IGrouping<TKey, T>, IGroupingInternal<TKey, T>)
  private
    fKey: TKey;
    fElementType: PTypeInfo;
    function GetElementType: PTypeInfo; reintroduce;
    function GetKey: TKey;
  public
    constructor Create(const key: TKey; elementType: PTypeInfo;
      const comparer: IComparer<T>);
  end;

  TMultiMap<TKey, TValue> = class abstract(TMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  private type
  {$REGION 'Nested Types'}
    TKeyValuePair = TPair<TKey, TValue>;
    TItem = packed record
      HashCode: Integer;
      Key: TKey;
      Values: IGroupingInternal<TKey, TValue>;
    end;
    TItems = TArray<TItem>;
    PItem = ^TItem;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      // same layout as THashMapInnerCollection.TEnumerator
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fHashTable: PHashTable;
      fOffset: Integer;
      fIndex: Integer;
      fVersion: Integer;
      fItem: PItem;
      fEnumerator: IEnumerator<TValue>;
      function GetCurrent: TKeyValuePair;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fHashTable: THashTable;
    fKeys: THashMapInnerCollection;
    fValues: THashMapInnerCollection;
    fGroups: THashMapInnerCollection;
    fCount: Integer;
    fWrappers: TList;
    fValueComparer: IInterface;
    fCollectionFactory: TCollectionFactory;
    function CreateWrappedCollection(const key: TKey; item: PItem): IReadOnlyCollection<TValue>;
    procedure UpdateValues(collection: TObject);
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetGroups: IEnumerable<IGrouping<TKey, TValue>>;
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetNonEnumeratedCount: Integer;
    function GetValues: IReadOnlyCollection<TValue>;
  {$ENDREGION}
    procedure DoRemove(const entry: THashTableEntry;
      action: TCollectionChangedAction; const extractTarget: ICollection<TValue>);
    procedure DoRemoveValues(item: PItem; action: TCollectionChangedAction);
  public
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IInterface;
      collectionFactory: TCollectionFactory;
      ownerships: TDictionaryOwnerships);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    function TryAdd(const key: TKey; const value: TValue): Boolean;
    function Remove(const key: TKey): Boolean; overload;
    function Remove(const key: TKey; const value: TValue): Boolean; overload;
    function Extract(const key: TKey; const value: TValue): TKeyValuePair; overload;
    function Contains(const key: TKey; const value: TValue): Boolean; overload;
    function ContainsKey(const key: TKey): Boolean;
    function ContainsValue(const value: TValue): Boolean;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IMultiMap<TKey, TValue>'}
    function Add(const key: TKey; const value: TValue): Boolean; overload;
    procedure AddRange(const key: TKey; const values: array of TValue); overload;
    procedure AddRange(const key: TKey; const values: IEnumerable<TValue>); overload;
    function Extract(const key: TKey): ICollection<TValue>; overload;
    function TryGetValues(const key: TKey; var values: IReadOnlyCollection<TValue>): Boolean;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  {$ENDREGION}
  end;

  TSortedMultiMap<TKey, TValue> = class abstract(TMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  private type
  {$REGION 'Nested Types'}
    TKeyValuePair = TPair<TKey, TValue>;
    PNode = ^TNode;
    TNode = packed record // same layout as TRedBlackTreeBase<TKey, IInterface>.TNode
      Parent, Right, Left: Pointer;
      Key: TKey;
      Values: IGroupingInternal<TKey, TValue>;
    end;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      // same layout as TTreeMapInnerCollection.TEnumerator
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fTree: TBinaryTree;
      fOffset: Integer;
      fSourceVersion: PInteger;
      fNode: PNode;
      fVersion: Integer;
      fItem: Pointer;
      fEnumerator: IEnumerator<TValue>;
      function GetCurrent: TKeyValuePair;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fTree: TRedBlackTreeBase<TKey, IInterface>;
    fKeys: TTreeMapInnerCollection;
    fValues: TTreeMapInnerCollection;
    fGroups: TTreeMapInnerCollection;
    fVersion: Integer;
    fCount: Integer;
    fWrappers: TList;
    fKeyComparer: IComparer<TKey>;
    fValueComparer: IInterface;
    fCollectionFactory: TCollectionFactory;
    fOwnerships: TDictionaryOwnerships;
    function CreateWrappedCollection(const key: TKey; node: PNode): IReadOnlyCollection<TValue>;
    procedure UpdateValues(collection: TObject);
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetGroups: IEnumerable<IGrouping<TKey, TValue>>;
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetNonEnumeratedCount: Integer;
    function GetValues: IReadOnlyCollection<TValue>;
  {$ENDREGION}
    procedure DoRemove(const node: PNode; action: TCollectionChangedAction;
      const extractTarget: ICollection<TValue>; deleteNode: Boolean = True);
    procedure DoRemoveValues(node: PNode; action: TCollectionChangedAction);
  public
    constructor Create(const keyComparer: IComparer<TKey>;
      const valueComparer: IInterface;
      collectionFactory: TCollectionFactory;
      ownerships: TDictionaryOwnerships);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    function TryAdd(const key: TKey; const value: TValue): Boolean;
    function Remove(const key: TKey): Boolean; overload;
    function Remove(const key: TKey; const value: TValue): Boolean; overload;
    function Extract(const key: TKey; const value: TValue): TKeyValuePair; overload;
    function Contains(const key: TKey; const value: TValue): Boolean; overload;
    function ContainsKey(const key: TKey): Boolean;
    function ContainsValue(const value: TValue): Boolean;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IMultiMap<TKey, TValue>'}
    function Add(const key: TKey; const value: TValue): Boolean; overload;
    procedure AddRange(const key: TKey; const values: array of TValue); overload;
    procedure AddRange(const key: TKey; const values: IEnumerable<TValue>); overload;
    function Extract(const key: TKey): ICollection<TValue>; overload;
    function TryGetValues(const key: TKey; var values: IReadOnlyCollection<TValue>): Boolean;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  {$ENDREGION}
  end;

  TFoldedMultiMap<TKey, TValue> = class(TMultiMap<TKey, TValue>)
  private
    fElementType: PTypeInfo;
    fKeyType: PTypeInfo;
    fValueType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
    function GetKeyType: PTypeInfo; override;
    function GetValueType: PTypeInfo; override;
  public
    constructor Create(keyType, valueType, elementType: PTypeInfo;
      const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IInterface;
      collectionFactory: TCollectionFactory;
      ownerships: TDictionaryOwnerships);
  end;

  TFoldedSortedMultiMap<TKey, TValue> = class(TSortedMultiMap<TKey, TValue>)
  private
    fElementType: PTypeInfo;
    fKeyType: PTypeInfo;
    fValueType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
    function GetKeyType: PTypeInfo; override;
    function GetValueType: PTypeInfo; override;
  public
    constructor Create(keyType, valueType, elementType: PTypeInfo;
      const keyComparer: IComparer<TKey>;
      const valueComparer: IInterface;
      collectionFactory: TCollectionFactory;
      ownerships: TDictionaryOwnerships);
  end;

  TCollectionsHelper = class(Spring.Collections.TCollections);

  TCollectionWrapper_Object = class(TCollectionWrapper<TObject>);
  TCollectionWrapper_Interface = class(TCollectionWrapper<IInterface>);

implementation

uses
  Types,
  TypInfo,
  Spring.Comparers,
  Spring.ResourceStrings;


{$REGION 'TCollectionWrapper'}

procedure TCollectionWrapper.BeforeDestruction;
begin
  if Assigned(fWrappers) then
  begin
    TMonitor.Enter(fWrappers);
    try
      fWrappers.Remove(Self);
    finally
      TMonitor.Exit(fWrappers);
    end;
  end;
  inherited;
end;

class procedure TCollectionWrapper.Create(classType: TClass;
  const collection: IEnumerable; wrappers: TList;
  updateValues: TNotifyEvent; collectionFactory: TCollectionFactory; const key;
  const valueComparer: IInterface; elementType: PTypeInfo; var result);
var
  instance: TCollectionWrapper;
begin
  instance := Pointer(classType.NewInstance);
  instance.fCollection := collection;
  instance.fWrappers := wrappers;
  TMonitor.Enter(instance.fWrappers);
  try
    instance.fWrappers.Add(instance);
  finally
    TMonitor.Exit(instance.fWrappers);
  end;
  instance.fUpdateValues := updateValues;
  if collection = nil then
    collectionFactory(key, valueComparer, elementType, instance.fCollection);
  TObject(instance).AfterConstruction;
  AssignComparer(instance.fComparer, instance.fCollection);
  IInterface(result) := nil;
  instance.GetInterface(IReadOnlyCollectionOfTGuid, result);
end;

function TCollectionWrapper.GetCount: Integer;
begin
  RefreshIfEmpty;
  Result := fCollection.Count;
end;

function TCollectionWrapper.GetElementType: PTypeInfo;
begin
  Result := fCollection.ElementType;
end;

procedure TCollectionWrapper.GetEnumerator(enumerator: PPointer;
  vtable: PEnumeratorVtable; typeInfo, getCurrent: Pointer);
begin
  _AddRef;
  RefreshIfEmpty;
  with PEnumerator(TEnumeratorBlock.Create(enumerator, vtable,
    typeInfo, getCurrent, @TEnumerator.MoveNext))^ do
  begin
    Parent := Self;
    fCollection := Self.fCollection;
    {$IFDEF MSWINDOWS}
    IEnumerableInternal(fCollection).GetEnumerator(fEnumerator);
    {$ELSE}
    fEnumerator := fCollection.GetEnumerator;
    {$ENDIF}
  end;
end;

function TCollectionWrapper.GetNonEnumeratedCount: Integer;
begin
  RefreshIfEmpty;
  Result := fCollection.GetNonEnumeratedCount;
end;

procedure TCollectionWrapper.HandleDestroy(Sender: TObject);
begin
  fWrappers := nil;
  fUpdateValues := nil;
end;

procedure TCollectionWrapper.RefreshIfEmpty;
begin
  if fCollection.IsEmpty and Assigned(fUpdateValues) then
    fUpdateValues(Self);
end;

{$ENDREGION}


{$REGION 'TCollectionWrapper.TEnumerator'}

function TCollectionWrapper.TEnumerator.MoveNext: Boolean;
begin
  ValidateEnumerator;
  Result := fEnumerator.MoveNext;
end;

procedure TCollectionWrapper.TEnumerator.ValidateEnumerator;
begin
  Parent.RefreshIfEmpty;
  if Parent.fCollection <> fCollection then
    RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TCollectionWrapper<T>'}

procedure TCollectionWrapper<T>.BeforeDestruction;
begin
  TCollectionWrapper(Self).BeforeDestruction;
end;

function TCollectionWrapper<T>.Contains(
  const value: T; const comparer: IEqualityComparer<T>): Boolean;
begin
  TCollectionWrapper(Self).RefreshIfEmpty;
  Result := fCollection.Contains(value, comparer);
end;

function TCollectionWrapper<T>.GetCount: Integer;
begin
  Result := TCollectionWrapper(Self).GetCount;
end;

function TCollectionWrapper<T>.GetElementType: PTypeInfo;
begin
  Result := TCollectionWrapper(Self).GetElementType;
end;

function TCollectionWrapper<T>.GetEnumerator: IEnumerator<T>; //FI:W521
begin
  TCollectionWrapper(Self).GetEnumerator(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent);
end;

function TCollectionWrapper<T>.GetNonEnumeratedCount: Integer;
begin
  Result := TCollectionWrapper(Self).GetNonEnumeratedCount;
end;

function TCollectionWrapper<T>.ToArray: TArray<T>;
begin
  TCollectionWrapper(Self).RefreshIfEmpty;
  Result := fCollection.ToArray;
end;

{$ENDREGION}


{$REGION 'TCollectionWrapper<T>.TEnumerator'}

function TCollectionWrapper<T>.TEnumerator.GetCurrent: T;
begin
  TCollectionWrapper.TEnumerator(Self).ValidateEnumerator;
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(fEnumerator).GetCurrent(Result)
  else
  {$ENDIF}
  Result := fEnumerator.Current;
end;

{$ENDREGION}


{$REGION 'TMultiMap<TKey, TValue>'}

constructor TMultiMap<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>; const valueComparer: IInterface;
  collectionFactory: TCollectionFactory; ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fHashTable.Comparer := keyComparer;
  fHashTable.Ownerships := ownerships;
  fValueComparer := valueComparer;
  fCollectionFactory := collectionFactory;
end;

procedure TMultiMap<TKey, TValue>.AfterConstruction;
var
  keyType, valueType: PTypeInfo;
begin
  inherited AfterConstruction;

  keyType := GetKeyType;
  valueType := GetValueType;
  fHashTable.ItemsInfo := TypeInfo(TItems);
  fHashTable.Initialize(@TComparerThunks<TKey>.Equals, @TComparerThunks<TKey>.GetHashCode, keyType);
  {$IFDEF DELPHIXE7_UP}
  if fHashTable.DefaultComparer then
    fHashTable.Find := @THashTable<TKey>.FindWithoutComparer
  else
  {$ENDIF}
    fHashTable.Find := @THashTable<TKey>.FindWithComparer;

  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TKey) of
    tkClass: fKeys := THashMapInnerCollection.Create_Object(
      Self, @fHashTable, nil, keyType, 0);
    tkInterface: fKeys := THashMapInnerCollection.Create_Interface(
      Self, @fHashTable, nil, keyType, 0);
  else{$ELSE}begin{$ENDIF}
    fKeys := THashMapInnerCollection.Create(THashMapInnerCollection<TKey>,
      Self, @fHashTable, nil, keyType, 0);
  end;
  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: fValues := THashMapInnerCollection.Create_Object(
      Self, @fHashTable, nil, valueType, SizeOf(TKey), @fCount, TCollectionThunks<TValue>.Contains);
    tkInterface: fValues := THashMapInnerCollection.Create_Interface(
      Self, @fHashTable, nil, valueType, SizeOf(TKey), @fCount, TCollectionThunks<TValue>.Contains);
  else{$ELSE}begin{$ENDIF}
    fValues := THashMapInnerCollection.Create(THashMapInnerCollection<TValue>,
      Self, @fHashTable, nil, valueType, SizeOf(TKey), @fCount, TCollectionThunks<TValue>.Contains);
  end;
  fGroups := THashMapInnerCollection.Create_Interface(Self,
    @fHashTable, nil, TypeInfo(IGrouping<TKey, TValue>), SizeOf(TKey));
  fWrappers := TList.Create;
end;

procedure TMultiMap<TKey, TValue>.BeforeDestruction;
var
  list: Pointer;
  i: Integer;
begin
  list := fWrappers.List;
  for i := 1 to fWrappers.Count do
    TCollectionWrapper({$IFDEF DELPHIXE}PPointerList{$ELSE}TPointerList{$ENDIF}(list)[i-1]).HandleDestroy(Self);
  fWrappers.Free;
  Clear;
  fKeys.Free;
  fValues.Free;
  fGroups.Free;
  inherited BeforeDestruction;
end;

function TMultiMap<TKey, TValue>.CreateWrappedCollection(
  const key: TKey; item: PItem): IReadOnlyCollection<TValue>;
var
  values: Pointer;
begin
  if not Assigned(item) then
    values := item
  else
    values := Pointer(item.Values);

  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: TCollectionWrapper.Create(TCollectionWrapper_Object, IEnumerable(values), fWrappers,
      UpdateValues, fCollectionFactory, key, fValueComparer, GetValueType, Result);
    tkInterface: TCollectionWrapper.Create(TCollectionWrapper_Interface, IEnumerable(values), fWrappers,
      UpdateValues, fCollectionFactory, key, fValueComparer, GetValueType, Result);
  else{$ELSE}begin{$ENDIF}
    TCollectionWrapper.Create(TCollectionWrapper<TValue>, IEnumerable(values), fWrappers,
      UpdateValues, fCollectionFactory, key, fValueComparer, GetValueType, Result);
  end;
end;

function TMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

function TMultiMap<TKey, TValue>.Add(const key: TKey;
  const value: TValue): Boolean;
begin
  Result := TryAdd(key, value);
end;

procedure TMultiMap<TKey, TValue>.AddRange(const key: TKey;
  const values: array of TValue);
var
  i: Integer;
begin
  for i := 0 to High(values) do
    TryAdd(key, values[i]);
end;

procedure TMultiMap<TKey, TValue>.AddRange(const key: TKey;
  const values: IEnumerable<TValue>);
var
  enumerator: IEnumerator<TValue>;
  item: TValue;
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(TValue) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    TryAdd(key, item);
  end;
end;

procedure TMultiMap<TKey, TValue>.Clear;
var
  i: NativeInt;
begin
  for i := 0 to fHashTable.ItemCount - 1 do
    if TItems(fHashTable.Items)[i].HashCode >= 0 then
      DoRemoveValues(@TItems(fHashTable.Items)[i], caRemoved);
  fHashTable.Clear;
  fCount := 0;
end;

function TMultiMap<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if not Assigned(item) then Exit(Boolean(Pointer(item)));
  Result := item.Values.Contains(value);
end;

function TMultiMap<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  Result := Assigned(item);
end;

function TMultiMap<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  i: Integer;
begin
  for i := 0 to fHashTable.ItemCount - 1 do
    if TItems(fHashTable.Items)[i].HashCode >= 0 then
      if TItems(fHashTable.Items)[i].Values.Contains(value) then
        Exit(True);
  Result := False;
end;

procedure TMultiMap<TKey, TValue>.DoRemove(const entry: THashTableEntry;
  action: TCollectionChangedAction; const extractTarget: ICollection<TValue>);
var
  item: PItem;
begin
  item := fHashTable.DeleteEntry(entry);
  Dec(fCount, item.Values.Count);
  DoRemoveValues(item, action);
  if action = caExtracted then
    item.Values.MoveTo(extractTarget);
  item.HashCode := RemovedFlag;
  item.Key := Default(TKey);
  item.Values := nil;
end;

procedure TMultiMap<TKey, TValue>.DoRemoveValues(item: PItem;
  action: TCollectionChangedAction);
var
  enumerator: IEnumerator<TValue>;
  value: TValue;
begin
  enumerator := item.values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(TValue) then
      IEnumeratorInternal(enumerator).GetCurrent(value)
    else
    {$ENDIF}
    value := enumerator.Current;
    if Assigned(Notify) then
      DoNotify(item.key, value, action);
    with fOnValueChanged do if CanInvoke then
      Invoke(Self, value, action);
    {$IFDEF DELPHIXE7_UP}
    if GetTypeKind(TValue) = tkClass then
    {$ENDIF}
    if (action = caRemoved) and (doOwnsValues in fHashTable.Ownerships) then
      PObject(@value).Free;
  end;
  with fOnKeyChanged do if CanInvoke then
    Invoke(Self, item.key, action);
  if action = caRemoved then
  begin
    {$IFDEF DELPHIXE7_UP}
    if GetTypeKind(TKey) = tkClass then
    {$ENDIF}
    if doOwnsKeys in fHashTable.Ownerships then
      PObject(@item.key).Free;
    item.values.Clear;
  end;
end;

function TMultiMap<TKey, TValue>.Extract(const key: TKey): ICollection<TValue>;
var
  valueType: PTypeInfo;
  comparer: Pointer;
  entry: THashTableEntry;
begin
  valueType := GetValueType;
  comparer := _LookupVtableInfo(giComparer, valueType, SizeOf(TValue));
  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: TCollectionsHelper.CreateList_Object(comparer, False, Result, valueType);
    tkInterface: TCollectionsHelper.CreateList_Interface(comparer, Result, valueType);
    tkUString: TCollectionsHelper.CreateList_String(comparer, Result, valueType);
    tkMethod: TCollectionsHelper.CreateList_Method(comparer, Result, valueType);
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(TValue) of
        1: TCollectionsHelper.CreateList_Int8(comparer, Result, valueType);
        2: TCollectionsHelper.CreateList_Int16(comparer, Result, valueType);
        4: TCollectionsHelper.CreateList_Int32(comparer, Result, valueType);
        8: TCollectionsHelper.CreateList_Int64(comparer, Result, valueType);
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TList<TValue>.Create(IComparer<TValue>(comparer));
  end;

  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  if fHashTable.FindEntry(key, entry) then
    DoRemove(entry, caExtracted, Result);
end;

function TMultiMap<TKey, TValue>.Extract(const key: TKey; const value: TValue): TKeyValuePair;
var
  item: PItem;
  count, newCount: Integer;
begin
  Result.Key := key;
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if Assigned(item) then
  begin
    count := item.Values.Count;
    {$IFDEF RSP31615}
    if IsManagedType(TValue) then
      TCollectionThunks<TValue>.ICollectionInternal(item.Values).Extract(
        {$IFDEF CPUX64}Result.Value, {$ENDIF}value{$IFDEF CPUX86}, Result.Value{$ENDIF})
    else
    {$ENDIF}
    Result.Value := item.Values.Extract(value);
    newCount := item.Values.Count;
    if newCount < count then
    begin
      Dec(fCount);
      if Assigned(Notify) then
        DoNotify(key, value, caExtracted);
      with fOnValueChanged do if CanInvoke then
        Invoke(Self, Result.Value, caExtracted);
      if newCount = 0 then
        with fOnKeyChanged do if CanInvoke then
          Invoke(Self, item.Key, caExtracted);
    end;
  end
  else
    Result.Value := Default(TValue);
end;

function TMultiMap<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

function TMultiMap<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent,
    @THashMapInnerCollection.TEnumerator.MoveNext_MultiMap))^ do
  begin
    Parent := Self;
    fHashTable := @Self.fHashTable;
    fOffset := KeyOffset + SizeOf(TKey);
    fVersion := Self.fHashTable.Version;
  end;
end;

function TMultiMap<TKey, TValue>.GetGroups: IEnumerable<IGrouping<TKey, TValue>>;
begin
  Result := IEnumerable<IGrouping<TKey, TValue>>(fGroups._this);
end;

function TMultiMap<TKey, TValue>.GetItems(
  const key: TKey): IReadOnlyCollection<TValue>;
begin
  Result := CreateWrappedCollection(key, IHashTable<TKey>(@fHashTable).Find(key));
end;

function TMultiMap<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := IReadOnlyCollection<TKey>(fKeys._this);
end;

function TMultiMap<TKey, TValue>.GetNonEnumeratedCount: Integer;
begin
  Result := fCount;
end;

function TMultiMap<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := IReadOnlyCollection<TValue>(fValues._this);
end;

function TMultiMap<TKey, TValue>.Remove(const key: TKey; const value: TValue): Boolean;
var
  entry: THashTableEntry;
  item: PItem;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  Result := fHashTable.FindEntry(key, entry);
  if not Result then Exit;
  item := @TItems(fHashTable.Items)[entry.ItemIndex];
  Result := item.Values.Remove(value);
  if Result then
  begin
    Dec(fCount);
    if Assigned(Notify) then
      DoNotify(item.Key, value, caRemoved);
    with fOnValueChanged do if CanInvoke then
      Invoke(Self, value, caRemoved);
    {$IFDEF DELPHIXE7_UP}
    if GetTypeKind(TValue) = tkClass then
    {$ENDIF}
    if doOwnsValues in fHashTable.Ownerships then
      PObject(@value).Free;
    if item.Values.Any then
      {$Q-}
      Inc(PInteger(@fHashTable.Version)^)
      {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    else
      DoRemove(entry, caRemoved, nil);
    Result := True;
  end;
end;

function TMultiMap<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  Result := fHashTable.FindEntry(key, entry);
  if Result then
  begin
    DoRemove(entry, caRemoved, nil);
    Result := True;
  end;
end;

function TMultiMap<TKey, TValue>.TryAdd(const key: TKey; const value: TValue): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key, InsertNonExisting);
  if not Assigned(item.Values) then
  begin
    item.Key := key;
    fCollectionFactory(key, fValueComparer, GetValueType, item.Values);
    with fOnKeyChanged do if CanInvoke then
      Invoke(Self, item.Key, caAdded);
  end;

  Result := item.Values.Add(value);
  if Result then
  begin
    Inc(fCount);
    {$Q-}
    Inc(PInteger(@fHashTable.Version)^);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    with fOnValueChanged do if CanInvoke then
      Invoke(Self, value, caAdded);
    if Assigned(Notify) then
    begin
      DoNotify(item.Key, value, caAdded);
      Result := True;
    end;
  end;
end;

procedure TMultiMap<TKey, TValue>.UpdateValues(collection: TObject);
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(
    IGrouping<TKey, TValue>(TCollectionWrapper(collection).fCollection).Key);
  if Assigned(item) then
    TCollectionWrapper(collection).fCollection := IEnumerable(item.Values);
end;

function TMultiMap<TKey, TValue>.TryGetValues(const key: TKey;
  var values: IReadOnlyCollection<TValue>): Boolean;
var
  temp: Pointer;
  item: PItem;
begin
  temp := IHashTable<TKey>(@fHashTable).Find(key);
  if not Assigned(temp) then Exit(Boolean(temp));
  item := temp;
  values := item.Values as IReadOnlyCollection<TValue>;
  Result := True;
end;

{$ENDREGION}


{$REGION 'TMultiMap<TKey, TValue>.TEnumerator'}

function TMultiMap<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
  Result.Key := fItem.Key;
  {$IFDEF RSP31615}
  if IsManagedType(TValue) then
    IEnumeratorInternal(fEnumerator).GetCurrent(Result.Value)
  else
  {$ENDIF}
  Result.Value := fEnumerator.Current;
end;

{$ENDREGION}


{$REGION 'TSortedMultiMap<TKey, TValue>'}

constructor TSortedMultiMap<TKey, TValue>.Create(
  const keyComparer: IComparer<TKey>; const valueComparer: IInterface;
  collectionFactory: TCollectionFactory; ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fKeyComparer := keyComparer;
  fValueComparer := valueComparer;
  fCollectionFactory := collectionFactory;
  fOwnerships := ownerships;
end;

procedure TSortedMultiMap<TKey, TValue>.AfterConstruction;
var
  keyType, valueType: PTypeInfo;
begin
  inherited AfterConstruction;

  keyType := GetKeyType;
  valueType := GetValueType;
  fTree := TRedBlackTreeBase<TKey,IInterface>.Create(fKeyComparer);

  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TKey) of
    tkClass: fKeys := TTreeMapInnerCollection.Create_Object(
      Self, fTree, @fVersion, nil, keyType, 0);
    tkInterface: fKeys := TTreeMapInnerCollection.Create_Interface(
      Self, fTree, @fVersion, nil, keyType, 0);
  else{$ELSE}begin{$ENDIF}
    fKeys := TTreeMapInnerCollection.Create(TTreeMapInnerCollection<TKey>,
      Self, fTree, @fVersion, nil, keyType, 0);
  end;
  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: fValues := TTreeMapInnerCollection.Create_Object(
      Self, fTree, @fVersion, nil, valueType, SizeOf(TKey), @fCount, TCollectionThunks<TObject>.Contains);
    tkInterface: fValues := TTreeMapInnerCollection.Create_Interface(
      Self, fTree, @fVersion, nil, valueType, SizeOf(TKey), @fCount, TCollectionThunks<IInterface>.Contains);
  else{$ELSE}begin{$ENDIF}
    fValues := TTreeMapInnerCollection.Create(TTreeMapInnerCollection<TValue>,
      Self, fTree, @fVersion, nil, valueType, SizeOf(TKey), @fCount, TCollectionThunks<TValue>.Contains);
  end;
  fGroups := TTreeMapInnerCollection.Create_Interface(
    Self, fTree, @fVersion, nil, TypeInfo(IGrouping<TKey, TValue>), 0);
  fWrappers := TList.Create;
end;

procedure TSortedMultiMap<TKey, TValue>.BeforeDestruction;
var
  i: Integer;
begin
  for i := 0 to fWrappers.Count-1 do
    TCollectionWrapper(fWrappers.List[i]).HandleDestroy(Self);
  fWrappers.Free;
  Clear;
  fTree.Free;
  fKeys.Free;
  fValues.Free;
  fGroups.Free;
  inherited BeforeDestruction;
end;

function TSortedMultiMap<TKey, TValue>.CreateWrappedCollection(
  const key: TKey; node: PNode): IReadOnlyCollection<TValue>;
var
  values: Pointer;
begin
  if not Assigned(node) then
    values := node
  else
    values := Pointer(node.Values);

  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: TCollectionWrapper.Create(TCollectionWrapper_Object, IEnumerable(values), fWrappers,
      UpdateValues, fCollectionFactory, key, fValueComparer, GetValueType, Result);
    tkInterface: TCollectionWrapper.Create(TCollectionWrapper_Interface, IEnumerable(values), fWrappers,
      UpdateValues, fCollectionFactory, key, fValueComparer, GetValueType, Result);
  else{$ELSE}begin{$ENDIF}
    TCollectionWrapper.Create(TCollectionWrapper<TValue>, IEnumerable(values), fWrappers,
      UpdateValues, fCollectionFactory, key, fValueComparer, GetValueType, Result);
  end;
end;

function TSortedMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

function TSortedMultiMap<TKey, TValue>.Add(const key: TKey;
  const value: TValue): Boolean;
begin
  Result := TryAdd(key, value);
end;

procedure TSortedMultiMap<TKey, TValue>.AddRange(const key: TKey;
  const values: array of TValue);
var
  i: Integer;
begin
  for i := 0 to High(values) do
    TryAdd(key, values[i]);
end;

procedure TSortedMultiMap<TKey, TValue>.AddRange(const key: TKey;
  const values: IEnumerable<TValue>);
var
  enumerator: IEnumerator<TValue>;
  item: TValue;
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(TValue) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    TryAdd(key, item);
  end;
end;

procedure TSortedMultiMap<TKey, TValue>.Clear;
var
  node: PBinaryTreeNode;
begin
  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    DoRemove(PNode(node), caRemoved, nil, False);
    node := node.Next;
  end;
  fTree.Clear;
  fCount := 0;
end;

function TSortedMultiMap<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  if not Assigned(node) then Exit(Boolean(node));
  Result := PNode(node).Values.Contains(value);
end;

function TSortedMultiMap<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  Result := Assigned(node);
end;

function TSortedMultiMap<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  node: PBinaryTreeNode;
begin
  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    if PNode(node).Values.Contains(value) then Break;
    node := node.Next;
  end;
  Result := Assigned(node);
end;

procedure TSortedMultiMap<TKey, TValue>.DoRemove(const node: PNode;
  action: TCollectionChangedAction; const extractTarget: ICollection<TValue>;
  deleteNode: Boolean);
begin
  Dec(fCount, node.Values.Count);
  DoRemoveValues(node, action);
  if action = caExtracted then
    node.Values.MoveTo(extractTarget);
  if deleteNode then
    fTree.DeleteNode(Pointer(node));
end;

procedure TSortedMultiMap<TKey, TValue>.DoRemoveValues(node: PNode;
  action: TCollectionChangedAction);
var
  enumerator: IEnumerator<TValue>;
  value: TValue;
begin
  enumerator := node.values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(TValue) then
      IEnumeratorInternal(enumerator).GetCurrent(value)
    else
    {$ENDIF}
    value := enumerator.Current;
    if Assigned(Notify) then
      DoNotify(node.key, value, action);
    with fOnValueChanged do if CanInvoke then
      Invoke(Self, value, action);
    {$IFDEF DELPHIXE7_UP}
    if GetTypeKind(TValue) = tkClass then
    {$ENDIF}
    if (action = caRemoved) and (doOwnsValues in fOwnerships) then
      PObject(@value).Free;
  end;
  with fOnKeyChanged do if CanInvoke then
    Invoke(Self, node.key, action);
  if action = caRemoved then
  begin
    {$IFDEF DELPHIXE7_UP}
    if GetTypeKind(TKey) = tkClass then
    {$ENDIF}
    if doOwnsKeys in fOwnerships then
      PObject(@node.key).Free;
    node.values.Clear;
    node.values := nil;
  end;
end;

function TSortedMultiMap<TKey, TValue>.Extract(const key: TKey): ICollection<TValue>;
var
  valueType: PTypeInfo;
  comparer: Pointer;
  node: Pointer;
begin
  valueType := GetValueType;
  comparer := _LookupVtableInfo(giComparer, valueType, SizeOf(TValue));
  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: TCollectionsHelper.CreateList_Object(comparer, False, Result, valueType);
    tkInterface: TCollectionsHelper.CreateList_Interface(comparer, Result, valueType);
    tkUString: TCollectionsHelper.CreateList_String(comparer, Result, valueType);
    tkMethod: TCollectionsHelper.CreateList_Method(comparer, Result, valueType);
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(TValue) of
        1: TCollectionsHelper.CreateList_Int8(comparer, Result, valueType);
        2: TCollectionsHelper.CreateList_Int16(comparer, Result, valueType);
        4: TCollectionsHelper.CreateList_Int32(comparer, Result, valueType);
        8: TCollectionsHelper.CreateList_Int64(comparer, Result, valueType);
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TList<TValue>.Create(IComparer<TValue>(comparer));
  end;

  node := fTree.FindNode(key);
  if Assigned(node) then
    DoRemove(node, caExtracted, Result);
end;

function TSortedMultiMap<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  node: PNode;
  count, newCount: Integer;
begin
  Result.Key := key;
  node := Pointer(fTree.FindNode(key));
  if Assigned(node) then
  begin
    count := node.Values.Count;
    {$IFDEF RSP31615}
    if IsManagedType(TValue) then
      TCollectionThunks<TValue>.ICollectionInternal(node.Values).Extract(
        {$IFDEF CPUX64}Result.Value, {$ENDIF}value{$IFDEF CPUX86}, Result.Value{$ENDIF})
    else
    {$ENDIF}
    Result.Value := node.Values.Extract(value);
    newCount := node.Values.Count;
    if newCount < count then
    begin
      Dec(fCount);
      if Assigned(Notify) then
        DoNotify(node.Key, value, caExtracted);
      with fOnValueChanged do if CanInvoke then
        Invoke(Self, Result.Value, caExtracted);
      if newCount = 0 then
      begin
        with fOnKeyChanged do if CanInvoke then
          Invoke(Self, node.Key, caExtracted);
        fTree.DeleteNode(Pointer(node));
      end;
    end;
  end
  else
    Result.Value := Default(TValue);
end;

function TSortedMultiMap<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

function TSortedMultiMap<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent,
    @TTreeMapInnerCollection.TEnumerator.MoveNext_MultiMap))^ do
  begin
    Parent := Self;
    fTree := Self.fTree;
    fOffset := SizeOf(TKey);
    fSourceVersion := @Self.fVersion;
    fVersion := Self.fVersion;
  end;
end;

function TSortedMultiMap<TKey, TValue>.GetGroups: IEnumerable<IGrouping<TKey, TValue>>;
begin
  Result := IEnumerable<IGrouping<TKey, TValue>>(fGroups._this);
end;

function TSortedMultiMap<TKey, TValue>.GetItems(
  const key: TKey): IReadOnlyCollection<TValue>;
begin
  Result := CreateWrappedCollection(key, PNode(fTree.FindNode(key)));
end;

function TSortedMultiMap<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := IReadOnlyCollection<TKey>(fKeys._this);
end;

function TSortedMultiMap<TKey, TValue>.GetNonEnumeratedCount: Integer;
begin
  Result := fCount;
end;

function TSortedMultiMap<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := IReadOnlyCollection<TValue>(fValues._this);
end;

function TSortedMultiMap<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  if not Assigned(node) then Exit(Boolean(node));
  DoRemove(node, caRemoved, nil);
  Result := True;
end;

function TSortedMultiMap<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  temp: Pointer;
  node: PNode;
begin
  temp := fTree.FindNode(key);
  if not Assigned(temp) then Exit(Boolean(temp));
  node := temp;
  Result := node.Values.Remove(value);
  if Result then
  begin
    Dec(fCount);
    if Assigned(Notify) then
      DoNotify(node.Key, value, caRemoved);
    with fOnValueChanged do if CanInvoke then
      Invoke(Self, value, caRemoved);
    {$IFDEF DELPHIXE7_UP}
    if GetTypeKind(TValue) = tkClass then
    {$ENDIF}
    if doOwnsValues in fOwnerships then
      PObject(@value).Free;
    if not node.Values.Any then
      DoRemove(node, caRemoved, nil);
    Result := True;
  end;
end;

function TSortedMultiMap<TKey, TValue>.TryAdd(const key: TKey;
  const value: TValue): Boolean;
var
  node: PNode;
begin
  node := Pointer(fTree.AddNode(key, True));
  node := Pointer(IntPtr(node) and not 1);
  if not Assigned(node.Values) then
  begin
    fCollectionFactory(key, fValueComparer, GetValueType, node.Values);
    with fOnKeyChanged do if CanInvoke then
      Invoke(Self, node.Key, caAdded);
  end;
  Result := node.Values.Add(value);
  if Result then
  begin
    Inc(fCount);
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    with fOnValueChanged do if CanInvoke then
      Invoke(Self, value, caAdded);
    if Assigned(Notify) then
    begin
      DoNotify(node.Key, value, caAdded);
      Result := True;
    end;
  end;
end;

function TSortedMultiMap<TKey, TValue>.TryGetValues(const key: TKey;
  var values: IReadOnlyCollection<TValue>): Boolean;
var
  temp: Pointer;
  node: PNode;
begin
  temp := fTree.FindNode(key);
  if not Assigned(temp) then Exit(Boolean(temp));
  node := temp;
  values := node.Values as IReadOnlyCollection<TValue>;
  Result := True;
end;

procedure TSortedMultiMap<TKey, TValue>.UpdateValues(collection: TObject);
var
  node: PNode;
begin
  node := Pointer(fTree.FindNode(
    IGrouping<TKey, TValue>(TCollectionWrapper(collection).fCollection).Key));
  if Assigned(node) then
    TCollectionWrapper(collection).fCollection := IEnumerable(node.Values);
end;

{$ENDREGION}


{$REGION 'TSortedMultiMap<TKey, TValue>.TEnumerator'}

function TSortedMultiMap<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
  Result.Key := fNode.Key;
  {$IFDEF RSP31615}
  if IsManagedType(TValue) then
    IEnumeratorInternal(fEnumerator).GetCurrent(Result.Value)
  else
  {$ENDIF}
  Result.Value := fEnumerator.Current;
end;

{$ENDREGION}


{$REGION 'TFoldedMultiMap<TKey, TValue>'}

constructor TFoldedMultiMap<TKey, TValue>.Create(
  keyType, valueType, elementType: PTypeInfo;
  const keyComparer: IEqualityComparer<TKey>; const valueComparer: IInterface;
  collectionFactory: TCollectionFactory; ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(keyType);

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(valueType);

  fHashTable.Comparer := keyComparer;
  fHashTable.Ownerships := ownerships;
  fValueComparer := valueComparer;
  fCollectionFactory := collectionFactory;

  fElementType := elementType;
  fKeyType := keyType;
  fValueType := valueType;
end;

function TFoldedMultiMap<TKey, TValue>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TFoldedMultiMap<TKey, TValue>.GetKeyType: PTypeInfo;
begin
  Result := fKeyType;
end;

function TFoldedMultiMap<TKey, TValue>.GetValueType: PTypeInfo;
begin
  Result := fValueType;
end;

{$ENDREGION}


{$REGION 'TFoldedSortedMultiMap<TKey, TValue>'}

constructor TFoldedSortedMultiMap<TKey, TValue>.Create(
  keyType, valueType, elementType: PTypeInfo;
  const keyComparer: IComparer<TKey>; const valueComparer: IInterface;
  collectionFactory: TCollectionFactory; ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(keyType);

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(valueType);

  fKeyComparer := keyComparer;
  fOwnerships := ownerships;
  fValueComparer := valueComparer;
  fCollectionFactory := collectionFactory;

  fElementType := elementType;
  fKeyType := keyType;
  fValueType := valueType;
end;

function TFoldedSortedMultiMap<TKey, TValue>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TFoldedSortedMultiMap<TKey, TValue>.GetKeyType: PTypeInfo;
begin
  Result := fKeyType;
end;

function TFoldedSortedMultiMap<TKey, TValue>.GetValueType: PTypeInfo;
begin
  Result := fValueType;
end;

{$ENDREGION}


{$REGION 'TValueList<TKey, T>'}

constructor TValueList<TKey, T>.Create(const key: TKey; elementType: PTypeInfo);
begin
  fKey := key;
  fElementType := elementType;
end;

function TValueList<TKey, T>.Add(const item: T): Boolean;
begin
  inherited Add(item);
  Result := True;
end;

function TValueList<TKey, T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TValueList<TKey, T>.GetKey: TKey;
begin
  Result := fKey;
end;

{$ENDREGION}


{$REGION 'TValueHashSet<TKey, T>'}

constructor TValueHashSet<TKey, T>.Create(const key: TKey;
  elementType: PTypeInfo; const comparer: IEqualityComparer<T>);
begin
  fKey := key;
  fElementType := elementType;
  inherited Create(0, comparer);
end;

function TValueHashSet<TKey, T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TValueHashSet<TKey, T>.GetKey: TKey;
begin
  Result := fKey;
end;

{$ENDREGION}


{$REGION 'TValueTreeSet<TKey, T>'}

constructor TValueTreeSet<TKey, T>.Create(const key: TKey;
  elementType: PTypeInfo; const comparer: IComparer<T>);
begin
  fKey := key;
  fElementType := elementType;
  inherited Create(comparer);
end;

function TValueTreeSet<TKey, T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TValueTreeSet<TKey, T>.GetKey: TKey;
begin
  Result := fKey;
end;

{$ENDREGION}


end.
