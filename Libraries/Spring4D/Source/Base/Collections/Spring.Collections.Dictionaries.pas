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

unit Spring.Collections.Dictionaries;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Trees,
  Spring.Events.Base,
  Spring.HashTable;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  TDictionaryItem<TKey, TValue> = packed record
  public
    HashCode: Integer;
    Key: TKey;
    Value: TValue;
  end;

  TDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyDictionary<TKey, TValue>, IReadOnlyOrderedDictionary<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IDictionary<TKey, TValue>, IOrderedDictionary<TKey, TValue>)
  private type
  {$REGION 'Nested Types'}
    TKeyValuePair = TPair<TKey, TValue>;
    PKeyValuePair = ^TKeyValuePair;
    TItem = TDictionaryItem<TKey, TValue>;
    TItems = TArray<TItem>;
    PItem = ^TItem;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      // needs to be same layout as
      // THashMapInnerCollection<T>.TEnumerator
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TDictionary<TKey, TValue>;
      fHashTable: PHashTable;
      fOffset: Integer;
      fIndex: Integer;
      fVersion: Integer;
      fItem: PKeyValuePair;
      function GetCurrent: TKeyValuePair;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fHashTable: THashTable;
    fValueComparer: IEqualityComparer<TValue>;
    fKeys: THashMapInnerCollection;
    fValues: THashMapInnerCollection;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; inline;
    function GetCount: Integer;
    function GetItem(const key: TKey): TValue;
    function GetItemByIndex(index: Integer): TPair<TKey, TValue>;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetNonEnumeratedCount: Integer;
    function GetValues: IReadOnlyCollection<TValue>;
    procedure SetCapacity(value: Integer);
    procedure SetItem(const key: TKey; const value: TValue);
  {$ENDREGION}
    function TryInsert(const key: TKey; const value: TValue; behavior: Byte): Boolean;
    function DoRemove(const entry: THashTableEntry; action: TCollectionChangedAction): Boolean;
  public
    constructor Create(capacity: Integer;
      const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
    function Contains(const value: TKeyValuePair;
      const comparer: IEqualityComparer<TKeyValuePair>): Boolean; overload;
    function ToArray: TArray<TKeyValuePair>;
    function TryGetElementAt(var item: TKeyValuePair; index: Integer): Boolean;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    procedure Add(const key: TKey; const value: TValue); overload;
    function TryAdd(const key: TKey; const value: TValue): Boolean;
    function Remove(const key: TKey): Boolean; overload;
    function Remove(const key: TKey; const value: TValue): Boolean; overload;
    function Extract(const key: TKey; const value: TValue): TKeyValuePair; overload;
    function Contains(const key: TKey; const value: TValue): Boolean; overload;
    function ContainsKey(const key: TKey): Boolean;

    function ContainsValue(const value: TValue): Boolean;
  {$ENDREGION}

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function Extract(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;
    function TryExtract(const key: TKey; var value: TValue): Boolean;
    function TryGetValue(const key: TKey; var value: TValue): Boolean;
    function TryUpdateValue(const key: TKey; const newValue: TValue; var oldValue: TValue): Boolean;
    procedure TrimExcess;
  {$ENDREGION}

  {$REGION 'Implements IOrderedDictionary<TKey, TValue>'}
    function IndexOf(const key: TKey): Integer;
    function AsReadOnly: IReadOnlyOrderedDictionary<TKey, TValue>; overload;
  {$ENDREGION}
  end;

  TBidiDictionaryItem<TKey, TValue> = record
  public
    KeyHashCode: Integer;
    ValueHashCode: Integer;
    Key: TKey;
    Value: TValue;
  end;

  TBidiDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyDictionary<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>,
    IDictionary<TKey, TValue>, IBidiDictionary<TKey, TValue>)
  protected type
  {$REGION 'Nested Types'}
    TKeyValuePair = TPair<TKey, TValue>;
    TValueKeyPair = TPair<TValue, TKey>;
    TValueKeyPairComparer = TPairComparer<TValue, TKey>;
    TItem = TBidiDictionaryItem<TKey, TValue>;
    {$POINTERMATH ON}
    PItem = ^TItem;
    {$POINTERMATH OFF}

    TInverse = class(TCollectionBase<TValueKeyPair>,
      IEnumerable<TValueKeyPair>, IReadOnlyCollection<TValueKeyPair>,
      IReadOnlyMap<TValue, TKey>, IReadOnlyDictionary<TValue, TKey>,
      ICollection<TValueKeyPair>, IMap<TValue, TKey>,
      IDictionary<TValue, TKey>, IBidiDictionary<TValue, TKey>)
    private
      fSource: TBidiDictionary<TKey, TValue>;
    {$REGION 'Property Accessors'}
      function GetCapacity: Integer;
      function GetCount: Integer;
      function GetInverse: IBidiDictionary<TKey, TValue>;
      function GetItem(const value: TValue): TKey;
      function GetKeys: IReadOnlyCollection<TValue>;
      function GetKeyType: PTypeInfo;
      function GetNonEnumeratedCount: Integer;
      function GetOnKeyChanged: ICollectionChangedEvent<TValue>;
      function GetOnValueChanged: ICollectionChangedEvent<TKey>;
      function GetValues: IReadOnlyCollection<TKey>;
      function GetValueType: PTypeInfo;
      procedure SetCapacity(value: Integer);
      procedure SetItem(const value: TValue; const key: TKey);
    {$ENDREGION}
    protected
      procedure Changed(const item: TValueKeyPair; action: TCollectionChangedAction); override;
    public
    {$REGION 'Implements IInterface'}
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
    {$ENDREGION}

    {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
      function GetEnumerator: IEnumerator<TValueKeyPair>;
      function Contains(const value: TValueKeyPair): Boolean; overload;
      function Contains(const value: TValueKeyPair;
        const comparer: IEqualityComparer<TValueKeyPair>): Boolean; overload;
      function ToArray: TArray<TValueKeyPair>;
      function TryGetElementAt(var item: TValueKeyPair; index: Integer): Boolean;
    {$ENDREGION}

    {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
      function Add(const item: TValueKeyPair): Boolean; overload;
      function Remove(const item: TValueKeyPair): Boolean; overload;
      function Extract(const item: TValueKeyPair): TValueKeyPair; overload;
      procedure Clear;
    {$ENDREGION}

    {$REGION 'Implements IMap<TValue, TKey>'}
      procedure Add(const value: TValue; const key: TKey); overload;
      function TryAdd(const value: TValue; const key: TKey): Boolean;
      function Remove(const value: TValue): Boolean; overload;
      function Remove(const value: TValue; const key: TKey): Boolean; overload;
      function RemoveRange(const values: array of TValue): Integer; overload;
      function RemoveRange(const values: IEnumerable<TValue>): Integer; overload;
      function Extract(const value: TValue; const key: TKey): TValueKeyPair; overload;
      function Contains(const value: TValue; const key: TKey): Boolean; overload;
      function ContainsKey(const value: TValue): Boolean;
      function ContainsValue(const key: TKey): Boolean;
      property Keys: IReadOnlyCollection<TValue> read GetKeys;
      property Values: IReadOnlyCollection<TKey> read GetValues;
    {$ENDREGION}

    {$REGION 'Implements IDictionary<TValue, TKey>'}
      procedure AddOrSetValue(const value: TValue; const key: TKey);
      function Extract(const value: TValue): TKey; overload;
      function GetValueOrDefault(const value: TValue): TKey; overload;
      function GetValueOrDefault(const value: TValue; const defaultKey: TKey): TKey; overload;
      function TryExtract(const value: TValue; var key: TKey): Boolean;
      function TryGetValue(const value: TValue; var key: TKey): Boolean;
      function TryUpdateValue(const value: TValue; const newKey: TKey; var oldKey: TKey): Boolean;
      procedure TrimExcess;
      function AsReadOnly: IReadOnlyDictionary<TValue, TKey>;
    {$ENDREGION}
    end;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: TBidiDictionary<TKey, TValue>;
      fItemIndex: Integer;
      fVersion: Integer;
      fItem: PItem;
      function GetCurrent: TKeyValuePair;
      function GetCurrentInverse: TValueKeyPair;
      function GetCurrentKey: TKey;
      function GetCurrentValue: TValue;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
      class var InverseEnumerator_Vtable: TEnumeratorVtable;
      class var KeysEnumerator_Vtable: TEnumeratorVtable;
      class var ValuesEnumerator_Vtable: TEnumeratorVtable;
    end;

    TKeyCollection = class(TEnumerableBase<TKey>,
      IEnumerable<TKey>, IReadOnlyCollection<TKey>)
    private
      fSource: TBidiDictionary<TKey, TValue>;
    {$REGION 'Property Accessors'}
      function GetCount: Integer;
      function GetNonEnumeratedCount: Integer;
    {$ENDREGION}
    public
      constructor Create(const source: TBidiDictionary<TKey, TValue>);

    {$REGION 'Implements IInterface'}
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
    {$ENDREGION}

    {$REGION 'Implements IEnumerable<TKey>'}
      function GetEnumerator: IEnumerator<TKey>;
      function Contains(const value: TKey): Boolean; overload;
      function ToArray: TArray<TKey>;
      function TryGetElementAt(var key: TKey; index: Integer): Boolean;
    {$ENDREGION}
    end;

    TValueCollection = class(TEnumerableBase<TValue>,
      IEnumerable<TValue>, IReadOnlyCollection<TValue>)
    private
      fSource: TBidiDictionary<TKey, TValue>;
    {$REGION 'Property Accessors'}
      function GetCount: Integer;
      function GetNonEnumeratedCount: Integer;
    {$ENDREGION}
    public
      constructor Create(const source: TBidiDictionary<TKey, TValue>);

    {$REGION 'Implements IInterface'}
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
    {$ENDREGION}

    {$REGION 'Implements IEnumerable<TValue>'}
      function GetEnumerator: IEnumerator<TValue>;
      function Contains(const value: TValue): Boolean; overload;
      function ToArray: TArray<TValue>;
      function TryGetElementAt(var value: TValue; index: Integer): Boolean;
    {$ENDREGION}
    end;
  {$ENDREGION}
  private
    fKeyBuckets: TArray<Integer>;
    fValueBuckets: TArray<Integer>;
    fItems: TArray<TItem>;
    fCount: Integer;
    fItemCount: Integer;
    fVersion: Integer;
    fKeyComparer: IEqualityComparer<TKey>;
    fValueComparer: IEqualityComparer<TValue>;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
    fInverse: TInverse;
    fOwnerships: TDictionaryOwnerships;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; inline;
    function GetCount: Integer;
    function GetInverse: IBidiDictionary<TValue, TKey>;
    function GetItem(const key: TKey): TValue;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetNonEnumeratedCount: Integer;
    function GetValues: IReadOnlyCollection<TValue>;
    procedure SetCapacity(value: Integer);
    procedure SetItem(const key: TKey; const value: TValue);
  {$ENDREGION}
    procedure Rehash(newCapacity: Integer);
    procedure EnsureCompact;
    procedure Grow;
    function FindKey(const key: TKey; hashCode: Integer;
      out bucketIndex, itemIndex: Integer): Boolean;
    function FindValue(const value: TValue; hashCode: Integer;
      out bucketIndex, itemIndex: Integer): Boolean;
    function KeyHash(const key: TKey): Integer; inline;
    function ValueHash(const value: TValue): Integer; inline;
    procedure DoAdd(keyHashCode, keyBucketIndex, valueHashCode, valueBucketIndex,
      itemIndex: Integer; const key: TKey; const value: TValue);
    procedure DoRemove(keyBucketIndex, valueBucketIndex, itemIndex: Integer;
      action: TCollectionChangedAction);
    procedure DoSetKey(valueBucketIndex, itemIndex, keyHashCode: Integer;
      const key: TKey);
    procedure DoSetValue(keyBucketIndex, itemIndex, valueHashCode: Integer;
      const value: TValue);
    procedure AddOrSetKey(const value: TValue; const key: TKey);
  protected
    procedure Changed(const item: TPair<TKey, TValue>; action: TCollectionChangedAction); override;
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction);
    procedure ValueChanged(const item: TValue; action: TCollectionChangedAction);
    property Capacity: Integer read GetCapacity;
  public
    constructor Create(capacity: Integer;
      const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
    function Contains(const value: TKeyValuePair;
      const comparer: IEqualityComparer<TKeyValuePair>): Boolean; overload;
    function ToArray: TArray<TKeyValuePair>;
    function TryGetElementAt(var item: TKeyValuePair; index: Integer): Boolean;
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

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function Extract(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;
    function TryExtract(const key: TKey; var value: TValue): Boolean;
    function TryGetValue(const key: TKey; var value: TValue): Boolean;
    function TryUpdateValue(const key: TKey; const newValue: TValue; var oldValue: TValue): Boolean;
    procedure TrimExcess;
    function AsReadOnly: IReadOnlyDictionary<TKey, TValue>;

    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
  {$ENDREGION}
  end;

  TSortedDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyDictionary<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IDictionary<TKey, TValue>)
  private type
  {$REGION 'Nested Types'}
    TKeyValuePair = TPair<TKey, TValue>;
    PKeyValuePair = ^TKeyValuePair;
    PNode = ^TNode;
    TNode = packed record // same layout as TRedBlackTreeBase<TKey, TValue>.TNode
      Parent, Right, Left: Pointer;
      Key: TKey;
      Value: TValue;
    end;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      // needs to be same layout as
      // TTreeMapInnerCollection<T>.TEnumerator
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fTree: TBinaryTree;
      fOffset: Integer;
      fSourceVersion: PInteger;
      fNode: PBinaryTreeNode;
      fVersion: Integer;
      fItem: PKeyValuePair;
      function GetCurrent: TKeyValuePair;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fTree: TRedBlackTreeBase<TKey,TValue>;
    fKeyComparer: IComparer<TKey>;
    fValueComparer: IEqualityComparer<TValue>;
    fVersion: Integer;
    fKeys: TTreeMapInnerCollection;
    fValues: TTreeMapInnerCollection;
    fOwnerships: TDictionaryOwnerships;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItem(const key: TKey): TValue;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetNonEnumeratedCount: Integer;
    function GetValues: IReadOnlyCollection<TValue>;
    procedure SetCapacity(value: Integer);
    procedure SetItem(const key: TKey; const value: TValue);
  {$ENDREGION}
    function DoRemove(const node: PNode; action: TCollectionChangedAction): Boolean;
  public
    constructor Create(const keyComparer: IComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
    function Contains(const value: TKeyValuePair;
      const comparer: IEqualityComparer<TKeyValuePair>): Boolean; overload;
    function ToArray: TArray<TKeyValuePair>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    procedure Add(const key: TKey; const value: TValue); overload;
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

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function Extract(const key: TKey): TValue; overload;
    function TryExtract(const key: TKey; var value: TValue): Boolean;
    function TryGetValue(const key: TKey; var value: TValue): Boolean;
    function TryUpdateValue(const key: TKey; const newValue: TValue; var oldValue: TValue): Boolean;
    procedure TrimExcess;
    function AsReadOnly: IReadOnlyDictionary<TKey, TValue>;

    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
  {$ENDREGION}

  {$REGION 'Implements IReadOnlyDictionary<TKey, TValue>'}
    function GetValueOrDefault(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;
  {$ENDREGION}
  end;

  TFoldedDictionary<TKey, TValue> = class(TDictionary<TKey, TValue>)
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
      capacity: Integer;
      const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships);
  end;

  TFoldedBidiDictionary<TKey, TValue> = class(TBidiDictionary<TKey, TValue>)
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
      capacity: Integer;
      const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships);
  end;

procedure ValidateParams(keyType, valueType: PTypeInfo; capacity: Integer; ownerships: TDictionaryOwnerships);

implementation

uses
  Types,
  TypInfo,
  Spring.Comparers,
  Spring.Hash,
  Spring.ResourceStrings;

procedure ValidateParams(keyType, valueType: PTypeInfo; capacity: Integer; ownerships: TDictionaryOwnerships);
begin
  if capacity < 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.capacity, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);

  if keyType.Kind <> tkClass then
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(keyType);

  if valueType.Kind <> tkClass then
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(valueType);
end;


{$REGION 'TDictionary<TKey, TValue>'}

constructor TDictionary<TKey, TValue>.Create(capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  ValidateParams(TypeInfo(TKey), TypeInfo(TValue), capacity, ownerships);

  fHashTable.Comparer := keyComparer;
  fHashTable.Ownerships := ownerships;
  fValueComparer := valueComparer;

  fHashTable.ItemsInfo := TypeInfo(TItems);
  SetCapacity(capacity);
end;

procedure TDictionary<TKey, TValue>.AfterConstruction;
var
  keyType, valueType: PTypeInfo;
begin
  inherited AfterConstruction;

  keyType := GetKeyType;
  valueType := GetValueType;
  if not Assigned(fValueComparer) then
    fValueComparer := IEqualityComparer<TValue>(_LookupVtableInfo(giEqualityComparer, valueType, SizeOf(TValue)));
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
      Self, @fHashTable, fValueComparer, valueType, SizeOf(TKey));
    tkInterface: fValues := THashMapInnerCollection.Create_Interface(
      Self, @fHashTable, fValueComparer, valueType, SizeOf(TKey));
  else{$ELSE}begin{$ENDIF}
    fValues := THashMapInnerCollection.Create(THashMapInnerCollection<TValue>,
      Self, @fHashTable, fValueComparer, valueType, SizeOf(TKey));
  end;
end;

procedure TDictionary<TKey, TValue>.BeforeDestruction;
begin
  Clear;
  fKeys.Free;
  fValues.Free;
  inherited BeforeDestruction;
end;

function TDictionary<TKey, TValue>.GetCapacity: Integer;
begin
  Result := fHashTable.Capacity;
end;

procedure TDictionary<TKey, TValue>.SetCapacity(value: Integer);
begin
  fHashTable.Capacity := value;
end;

function TDictionary<TKey, TValue>.DoRemove(const entry: THashTableEntry;
  action: TCollectionChangedAction): Boolean;
var
  item: PItem;
begin
  item := fHashTable.DeleteEntry(entry);

  if Assigned(Notify) then
    Notify(Self, PKeyValuePair(@item.Key)^, action);
  with fOnKeyChanged do if CanInvoke then
    Invoke(Self, item.Key, action);
{$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) = tkClass then
{$ENDIF}
  if (action = caRemoved) and (doOwnsKeys in fHashTable.Ownerships) then
    PObject(@item.Key).Free;
  with fOnValueChanged do if CanInvoke then
    Invoke(Self, item.Value, action);
{$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) = tkClass then
{$ENDIF}
  if (action = caRemoved) and (doOwnsValues in fHashTable.Ownerships) then
    PObject(@item.Value).Free;

  item.Key := Default(TKey);
  item.Value := Default(TValue);
  Result := True;
end;

function TDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent,
    @THashMapInnerCollection.TEnumerator.MoveNext))^ do
  begin
    Parent := Self;
    fHashTable := @Self.fHashTable;
    fOffset := KeyOffset;
    fVersion := Self.fHashTable.Version;
  end;
end;

procedure TDictionary<TKey, TValue>.Clear;
var
  item: PItem;
  i: Integer;
begin
  fHashTable.ClearCount;

  item := PItem(fHashTable.Items);
  for i := 1 to fHashTable.ItemCount do //FI:W528
  begin
    if item.HashCode >= 0 then
    begin
      if Assigned(Notify) then
        Notify(Self, PKeyValuePair(@item.Key)^, caRemoved);
      with fOnKeyChanged do if CanInvoke then
        Invoke(Self, item.Key, caRemoved);
    {$IFDEF DELPHIXE7_UP}
      if GetTypeKind(TKey) = tkClass then
    {$ENDIF}
      if doOwnsKeys in fHashTable.Ownerships then
        PObject(@item.Key).Free;
      with fOnValueChanged do if CanInvoke then
        Invoke(Self, item.Value, caRemoved);
    {$IFDEF DELPHIXE7_UP}
      if GetTypeKind(TValue) = tkClass then
    {$ENDIF}
      if doOwnsValues in fHashTable.Ownerships then
        PObject(@item.Value).Free;
    end;
    Inc(item);
  end;

  fHashTable.Clear;
end;

function TDictionary<TKey, TValue>.Contains(const value: TKeyValuePair;
  const comparer: IEqualityComparer<TKeyValuePair>): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(value.Key);
  if not Assigned(item) then Exit(Boolean(Pointer(item)));
  Result := comparer.Equals(PKeyValuePair(@item.Key)^, value);
end;

function TDictionary<TKey, TValue>.ToArray: TArray<TKeyValuePair>;
var
  target: PKeyValuePair;
  source: PItem;
  i: Integer;
begin
  SetLength(Result, fHashTable.Count);
  target := Pointer(Result);
  if Assigned(target) then
  begin
    source := PItem(fHashTable.Items);
    for i := 1 to fHashTable.ItemCount do //FI:W528
    begin
      if source.HashCode >= 0 then
      begin
        target.Key := source.Key;
        target.Value := source.Value;
        Inc(target);
      end;
      Inc(source);
    end;
  end;
end;

function TDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fHashTable.Count;
end;

function TDictionary<TKey, TValue>.GetNonEnumeratedCount: Integer;
begin
  Result := fHashTable.Count;
end;

procedure TDictionary<TKey, TValue>.Add(const key: TKey; const value: TValue);
begin
  TryInsert(key, value, RaiseOnExisting or InsertNonExisting);
end;

function TDictionary<TKey, TValue>.TryInsert(
  const key: TKey; const value: TValue; behavior: Byte): Boolean;
var
  temp: Pointer;
  item: PItem;
begin
  temp := IHashTable<TKey>(@fHashTable).Find(key, behavior);
  if not Assigned(temp) then Exit(Boolean(temp));

  item := temp;
  if item.HashCode < 0 then
  begin
    if Assigned(Notify) then
      Notify(Self, PKeyValuePair(@item.Key)^, caRemoved);
    with fOnValueChanged do if CanInvoke then
      Invoke(Self, item.Value, caRemoved);
  {$IFDEF DELPHIXE7_UP}
    if GetTypeKind(TValue) = tkClass then
  {$ENDIF}
    if doOwnsValues in fHashTable.Ownerships then
      PObject(@item.Value).Free;
  end;

  item.Key := key;
  item.Value := value;

  if Assigned(Notify) then
    Notify(Self, PKeyValuePair(@item.Key)^, caAdded);
  if item.HashCode >= 0 then
    with fOnKeyChanged do if CanInvoke then
      Invoke(Self, item.Key, caAdded);
  with fOnValueChanged do if CanInvoke then
    Invoke(Self, item.Value, caAdded);

  item.HashCode := item.HashCode and not RemovedFlag;

  Result := True;
end;

procedure TDictionary<TKey, TValue>.AddOrSetValue(const key: TKey; const value: TValue);
begin
  SetItem(key, value);
end;

function TDictionary<TKey, TValue>.AsReadOnly: IReadOnlyOrderedDictionary<TKey, TValue>;
begin
  Result := Self;
end;

function TDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
var
  item: Pointer;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  Result := Assigned(item);
end;

function TDictionary<TKey, TValue>.Contains(const key: TKey; const value: TValue): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if not Assigned(item) then Exit(Boolean(Pointer(item)));
  Result := fValueComparer.Equals(item.Value, value);
end;

function TDictionary<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
begin
  Result := IEnumerable<TValue>(fValues._this).Contains(value);
end;

function TDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
begin
  TryExtract(key, Result);
end;

function TDictionary<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  entry: THashTableEntry;
  item: PItem;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  if fHashTable.FindEntry(key, entry) then
  begin
    item := @TItems(fHashTable.Items)[entry.ItemIndex];
    if fValueComparer.Equals(item.Value, value) then
    begin
      Result.Key := item.Key;
      Result.Value := item.Value;
      DoRemove(entry, caExtracted);
      Exit;
    end;
  end;
  Result.Key := Default(TKey);
  Result.Value := Default(TValue);
end;

procedure TDictionary<TKey, TValue>.TrimExcess;
begin
  fHashTable.Capacity := fHashTable.Count;
end;

function TDictionary<TKey, TValue>.TryAdd(const key: TKey; const value: TValue): Boolean;
begin
  Result := TryInsert(key, value, IgnoreExisting or InsertNonExisting);
end;

function TDictionary<TKey, TValue>.TryExtract(const key: TKey; var value: TValue): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  if fHashTable.FindEntry(key, entry) then
  begin
    value := TItems(fHashTable.Items)[entry.ItemIndex].Value;
    Result := DoRemove(entry, caExtracted);
    Exit;
  end;
  value := Default(TValue);
  Result := False;
end;

function TDictionary<TKey, TValue>.TryGetElementAt(var item: TKeyValuePair; index: Integer): Boolean;
begin
  if Cardinal(index) < Cardinal(fHashTable.Count) then
  begin
    fHashTable.EnsureCompact;
    with TItems(fHashTable.Items)[index] do
    begin
      item.Key := Key;
      item.Value := Value;
    end;
    Exit(True);
  end;
  item.Key := Default(TKey);
  item.Value := Default(TValue);
  Result := False;
end;

function TDictionary<TKey, TValue>.TryGetValue(const key: TKey; var value: TValue): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if Assigned(item) then
  begin
    value := item.Value;
    Exit(True);
  end;
  value := Default(TValue);
  Result := False;
end;

function TDictionary<TKey, TValue>.TryUpdateValue(const key: TKey;
  const newValue: TValue; var oldValue: TValue): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if Assigned(item) then
  begin
    {$Q-}
    Inc(PInteger(@fHashTable.Version)^);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

    if Assigned(Notify) then
      Notify(Self, PKeyValuePair(@item.Key)^, caRemoved);
    with fOnValueChanged do if CanInvoke then
      Invoke(Self, item.Value, caRemoved);

    oldValue := item.Value;
    item.Value := newValue;

    if Assigned(Notify) then
      Notify(Self, PKeyValuePair(@item.Key)^, caAdded);
    with fOnValueChanged do if CanInvoke then
      Invoke(Self, item.Value, caAdded);

    Exit(True);
  end;
  oldValue := Default(TValue);
  Result := False;
end;

function TDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  Result := fHashTable.FindEntry(key, entry);
  if not Result then Exit;
  Result := DoRemove(entry, caRemoved);
end;

function TDictionary<TKey, TValue>.Remove(const key: TKey; const value: TValue): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  Result := fHashTable.FindEntry(key, entry);
  if not Result then Exit;
  Result := fValueComparer.Equals(TItems(fHashTable.Items)[entry.ItemIndex].Value, value);
  if not Result then Exit;
  Result := DoRemove(entry, caRemoved);
end;

function TDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := IReadOnlyCollection<TKey>(fKeys._this);
end;

function TDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey): TValue;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if Assigned(item) then
    Result := item.Value
  else
    Result := Default(TValue);
end;

function TDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey;
  const defaultValue: TValue): TValue;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if Assigned(item) then
    Result := item.Value
  else
    Result := defaultValue;
end;

function TDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := IReadOnlyCollection<TValue>(fValues._this);
end;

function TDictionary<TKey, TValue>.IndexOf(const key: TKey): Integer;
var
  entry: THashTableEntry;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  fHashTable.EnsureCompact;
  if fHashTable.FindEntry(key, entry) then
    Exit(entry.ItemIndex);
  Result := -1;
end;

function TDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key, RaiseOnNonExisting);
  Result := item.Value;
end;

function TDictionary<TKey, TValue>.GetItemByIndex(index: Integer): TPair<TKey, TValue>; //FI:W521
begin
  if Cardinal(index) < Cardinal(fHashTable.Count) then
  begin
    fHashTable.EnsureCompact;
    Exit(PKeyValuePair(@TItems(fHashTable.Items)[index].Key)^);
  end;
  RaiseHelper.ArgumentOutOfRange_Index;
end;

procedure TDictionary<TKey, TValue>.SetItem(const key: TKey; const value: TValue);
begin
  TryInsert(key, value, OverwriteExisting or InsertNonExisting);
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TEnumerator'}

function TDictionary<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
var
  item: PKeyValuePair;
begin
  item := fItem;
  Result.Key := item.Key;
  Result.Value := item.Value;
end;

{$ENDREGION}


{$REGION 'TBidiDictionary<TKey, TValue>'}

constructor TBidiDictionary<TKey, TValue>.Create(capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  ValidateParams(TypeInfo(TKey), TypeInfo(TValue), capacity, ownerships);

  fOwnerships := ownerships;
  fKeyComparer := keyComparer;
  fValueComparer := valueComparer;

  SetCapacity(capacity);
end;

procedure TBidiDictionary<TKey, TValue>.AfterConstruction;
var
  keyType, valueType: PTypeInfo;
begin
  inherited AfterConstruction;

  keyType := GetKeyType;
  valueType := GetValueType;
  if not Assigned(fKeyComparer) then
    fKeyComparer := IEqualityComparer<TKey>(_LookupVtableInfo(giEqualityComparer, keyType, SizeOf(TKey)));
  if not Assigned(fValueComparer) then
    fValueComparer := IEqualityComparer<TValue>(_LookupVtableInfo(giEqualityComparer, valueType, SizeOf(TValue)));

  fKeys := TKeyCollection.Create(Self);
  fValues := TValueCollection.Create(Self);
  fInverse := TInverse.Create;
  fInverse.fSource := Self;

  TPairComparer.Create(@fInverse.fComparer,
    @TValueKeyPairComparer.Comparer_Vtable,
    @TValueKeyPairComparer.Compare,
    valueType, keyType);
end;

procedure TBidiDictionary<TKey, TValue>.BeforeDestruction;
begin
  Clear;
  fInverse.Free;
  fKeys.Free;
  fValues.Free;
  inherited BeforeDestruction;
end;

procedure TBidiDictionary<TKey, TValue>.Changed(const item: TPair<TKey, TValue>;
  action: TCollectionChangedAction);
var
  inverseItem: TValueKeyPair;
begin
  inherited Changed(item, action);
  inverseItem.Key := item.Value;
  inverseItem.Value := item.Key;
  fInverse.Changed(inverseItem, action);
end;

procedure TBidiDictionary<TKey, TValue>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  with fOnKeyChanged do if CanInvoke then
    Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsKeys in fOwnerships) then
    PObject(@item).Free;
end;

procedure TBidiDictionary<TKey, TValue>.ValueChanged(const item: TValue;
  action: TCollectionChangedAction);
begin
  with fOnValueChanged do if CanInvoke then
    Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsValues in fOwnerships) then
    PObject(@item).Free;
end;

function TBidiDictionary<TKey, TValue>.GetCapacity: Integer;
begin
  Result := DynArrayLength(fItems);
end;

procedure TBidiDictionary<TKey, TValue>.SetCapacity(value: Integer);
var
  newCapacity: Integer;
begin
  if value = 0 then
    newCapacity := 0
  else
  begin
    newCapacity := value;
    if newCapacity < MinCapacity then
      newCapacity := MinCapacity;
  end;
  if newCapacity <> Capacity then
    Rehash(newCapacity);
end;

procedure TBidiDictionary<TKey, TValue>.Rehash(newCapacity: Integer);
var
  newBucketCount: Integer;
  bucketIndex, itemIndex: Integer;
  sourceItem, targetItem: PItem;
  i: Integer;
begin
  if newCapacity = 0 then
  begin
    Assert(fCount = 0);
    Assert(fItemCount = 0);
    Assert(not Assigned(fKeyBuckets));
    Assert(not Assigned(fValueBuckets));
    Assert(not Assigned(fItems));
    Exit;
  end;

  Assert(newCapacity >= fCount);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

  newBucketCount := NextPowerOf2(newCapacity * 4 div 3 - 1); // 75% load factor

  // compact the items array, if necessary
  if fItemCount > fCount then
  begin
    sourceItem := PItem(fItems);
    targetItem := PItem(fItems);
    for i := 0 to fItemCount - 1 do //FI:W528
    begin
      if PInteger(sourceItem)^ >= 0 then // not removed
      begin
        if targetItem < sourceItem then
          targetItem^ := sourceItem^;
        Inc(targetItem);
      end;
      Inc(sourceItem);
    end;
    FinalizeArray(targetItem, TypeInfo(TItem), Cardinal(fItemCount - fCount));
  end;

  // resize the items array, safe now that we have compacted it
  SetLength(fItems, newBucketCount * 3 div 4);
  Assert(Capacity >= fCount);

  // repopulate the bucket array
  Assert(IsPowerOf2(newBucketCount));
  SetLength(fKeyBuckets, newBucketCount);
  for bucketIndex := 0 to newBucketCount - 1 do
    fKeyBuckets[bucketIndex] := EmptyBucket;
  SetLength(fValueBuckets, newBucketCount);
  for bucketIndex := 0 to newBucketCount - 1 do
    fValueBuckets[bucketIndex] := EmptyBucket;
  fItemCount := 0;
  while fItemCount < fCount do
  begin
    FindKey(fItems[fItemCount].Key, fItems[fItemCount].KeyHashCode, bucketIndex, itemIndex);
    Assert(itemIndex = fItemCount);
    fKeyBuckets[bucketIndex] := itemIndex;

    FindValue(fItems[fItemCount].Value, fItems[fItemCount].ValueHashCode, bucketIndex, itemIndex);
    Assert(itemIndex = fItemCount);
    fValueBuckets[bucketIndex] := itemIndex;

    Inc(fItemCount);
  end;
end;

procedure TBidiDictionary<TKey, TValue>.Grow;
var
  newCapacity: Integer;
begin
  newCapacity := Capacity;
  if newCapacity = 0 then
    newCapacity := MinCapacity
  else if 2 * fCount >= DynArrayLength(fKeyBuckets) then
    // only grow if load factor is greater than 0.5
    newCapacity := newCapacity * 2;
  Rehash(newCapacity);
end;

function TBidiDictionary<TKey, TValue>.FindKey(const key: TKey; hashCode: Integer; //FI:W521
  out bucketIndex, itemIndex: Integer): Boolean;
var
  bucketValue, mask, perturb: Integer;
begin
  if fItems = nil then
  begin
    bucketIndex := EmptyBucket;
    itemIndex := -1;
    Exit(False);
  end;

  mask := DynArrayLength(fKeyBuckets) - 1;
  perturb := hashCode;
  bucketIndex := hashCode and mask;
  while True do
  begin
    bucketValue := fKeyBuckets[bucketIndex];

    if bucketValue >= 0 then
    begin
      itemIndex := bucketValue;
      if fKeyComparer.Equals(fItems[itemIndex].Key, key) then
        Exit(True);
    end else if bucketValue = EmptyBucket then
    begin
      itemIndex := fItemCount;
      Exit(False);
    end;

    perturb := perturb shr PerturbShift;
    bucketIndex := (5 * bucketIndex + 1 + Integer(perturb)) and mask;
  end;
end;

function TBidiDictionary<TKey, TValue>.FindValue(const value: TValue; hashCode: Integer; //FI:W521
  out bucketIndex, itemIndex: Integer): Boolean;
var
  bucketValue, mask, perturb: Integer;
begin
  if fItems = nil then
  begin
    bucketIndex := EmptyBucket;
    itemIndex := -1;
    Exit(False);
  end;

  mask := DynArrayLength(fValueBuckets) - 1;
  perturb := hashCode;
  bucketIndex := hashCode and mask;
  while True do
  begin
    bucketValue := fValueBuckets[bucketIndex];

    if bucketValue = EmptyBucket then
    begin
      itemIndex := fItemCount;
      Exit(False);
    end;

    if bucketValue >= 0 then
    begin
      itemIndex := bucketValue;
      if fValueComparer.Equals(fItems[itemIndex].Value, value) then
        Exit(True);
    end
    else if bucketValue = EmptyBucket then
    begin
      itemIndex := fItemCount;
      Exit(False);
    end;

    perturb := perturb shr PerturbShift;
    bucketIndex := (5 * bucketIndex + 1 + Integer(perturb)) and mask;
  end;
end;

function TBidiDictionary<TKey, TValue>.KeyHash(const key: TKey): Integer;
begin
  Result := fKeyComparer.GetHashCode(key) and not RemovedFlag;
end;

function TBidiDictionary<TKey, TValue>.ValueHash(const value: TValue): Integer;
begin
  Result := fValueComparer.GetHashCode(value) and not RemovedFlag;
end;

procedure TBidiDictionary<TKey, TValue>.DoAdd(keyHashCode, keyBucketIndex, valueHashCode,
  valueBucketIndex, itemIndex: Integer; const key: TKey; const value: TValue);
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fKeyBuckets[keyBucketIndex] := itemIndex;
  fValueBuckets[valueBucketIndex] := itemIndex;
  fItems[itemIndex].KeyHashCode := keyHashCode;
  fItems[itemIndex].ValueHashCode := valueHashCode;
  fItems[itemIndex].Key := key;
  fItems[itemIndex].Value := value;
  Inc(fCount);
  Inc(fItemCount);

  if Assigned(Notify) then
    DoNotify(key, value, caAdded);
  KeyChanged(key, caAdded);
  ValueChanged(value, caAdded);
end;

procedure TBidiDictionary<TKey, TValue>.DoRemove(keyBucketIndex, valueBucketIndex,
  itemIndex: Integer; action: TCollectionChangedAction);
var
  oldKey: TKey;
  oldValue: TValue;
begin
  oldKey := fItems[itemIndex].Key;
  oldValue := fItems[itemIndex].Value;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fKeyBuckets[keyBucketIndex] := UsedBucket;
  fValueBuckets[valueBucketIndex] := UsedBucket;
  fItems[itemIndex].Key := Default(TKey);
  fItems[itemIndex].Value := Default(TValue);
  fItems[itemIndex].KeyHashCode := RemovedFlag;
  fItems[itemIndex].ValueHashCode := RemovedFlag;
  Dec(fCount);

  if Assigned(Notify) then
    DoNotify(oldKey, oldValue, action);
  KeyChanged(oldKey, action);
  ValueChanged(oldValue, action);
end;

procedure TBidiDictionary<TKey, TValue>.DoSetKey(valueBucketIndex, itemIndex,
  keyHashCode: Integer; const key: TKey);
var
  oldKey: TKey;
  oldValue: TValue;
  oldKeyHashCode, valueHashCode, oldKeyBucketIndex, oldKeyItemIndex, keyBucketIndex: Integer;
begin
  oldKey := fItems[itemIndex].Key;
  oldValue := fItems[itemIndex].Value;
  oldKeyHashCode := fItems[itemIndex].KeyHashCode;
  valueHashCode := fItems[itemIndex].ValueHashCode;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if fItemCount = Capacity then
  begin
    Grow;
    FindValue(oldValue, valueHashCode, valueBucketIndex, itemIndex);
  end;
  FindKey(oldKey, oldKeyHashCode, oldKeyBucketIndex, oldKeyItemIndex);
  Assert(oldKeyItemIndex = itemIndex);
  fKeyBuckets[oldKeyBucketIndex] := UsedBucket;
  FindKey(key, keyHashCode, keyBucketIndex, itemIndex);
  Assert(itemIndex = fItemCount);

  fKeyBuckets[keyBucketIndex] := oldKeyItemIndex;
  fValueBuckets[valueBucketIndex] := oldKeyItemIndex;

  fItems[itemIndex].Key := Default(TKey);
  fItems[itemIndex].Value := Default(TValue);
  fItems[itemIndex].KeyHashCode := RemovedFlag;
  fItems[itemIndex].ValueHashCode := RemovedFlag;

  fItems[oldKeyItemIndex].KeyHashCode := keyHashCode;
  Assert(fItems[oldKeyItemIndex].ValueHashCode = valueHashCode);
  fItems[oldKeyItemIndex].Key := key;
  Assert(fValueComparer.Equals(fItems[oldKeyItemIndex].Value, oldValue));

  Inc(fItemCount);

  if Assigned(Notify) then
    DoNotify(oldKey, oldValue, caRemoved);
  KeyChanged(oldKey, caRemoved);
  if Assigned(Notify) then
    DoNotify(key, oldValue, caAdded);
  KeyChanged(key, caAdded);
end;

procedure TBidiDictionary<TKey, TValue>.DoSetValue(keyBucketIndex, itemIndex,
  valueHashCode: Integer; const value: TValue);
var
  oldKey: TKey;
  oldValue: TValue;
  keyHashCode, oldValueHashCode, oldValueBucketIndex, oldValueItemIndex, valueBucketIndex: Integer;
begin
  oldKey := fItems[itemIndex].Key;
  oldValue := fItems[itemIndex].Value;
  keyHashCode := fItems[itemIndex].KeyHashCode;
  oldValueHashCode := fItems[itemIndex].ValueHashCode;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if fItemCount = Capacity then
  begin
    Grow;
    FindKey(oldKey, keyHashCode, keyBucketIndex, itemIndex);
  end;
  FindValue(oldValue, oldValueHashCode, oldValueBucketIndex, oldValueItemIndex);
  Assert(oldValueItemIndex = itemIndex);
  fValueBuckets[oldValueBucketIndex] := UsedBucket;
  FindValue(value, valueHashCode, valueBucketIndex, itemIndex);
  Assert(itemIndex = fItemCount);

  fKeyBuckets[keyBucketIndex] := oldValueItemIndex;
  fValueBuckets[valueBucketIndex] := oldValueItemIndex;

  fItems[itemIndex].Key := Default(TKey);
  fItems[itemIndex].Value := Default(TValue);
  fItems[itemIndex].KeyHashCode := RemovedFlag;
  fItems[itemIndex].ValueHashCode := RemovedFlag;

  Assert(fItems[oldValueItemIndex].KeyHashCode = keyHashCode);
  fItems[oldValueItemIndex].ValueHashCode := valueHashCode;
  Assert(fKeyComparer.Equals(fItems[oldValueItemIndex].Key, oldKey));
  fItems[oldValueItemIndex].Value := value;

  Inc(fItemCount);

  if Assigned(Notify) then
    DoNotify(oldKey, oldValue, caRemoved);
  ValueChanged(oldValue, caRemoved);
  if Assigned(Notify) then
    DoNotify(oldKey, value, caAdded);
  ValueChanged(value, caAdded);
end;

function TBidiDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fVersion := Self.fVersion;
  end;
end;

procedure TBidiDictionary<TKey, TValue>.Clear;
var
  oldItemCount, i: Integer;
  oldItems: TArray<TItem>;
  item: PItem;
begin
  oldItemCount := fItemCount;
  oldItems := fItems;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fCount := 0;
  fItemCount := 0;
  fKeyBuckets := nil;
  fValueBuckets := nil;
  fItems := nil;
  SetCapacity(0);

  item := Pointer(oldItems);
  for i := 1 to oldItemCount do
  begin
    if item.KeyHashCode >= 0 then
    begin
      if Assigned(Notify) then
        DoNotify(item.Key, item.Value, caRemoved);
      KeyChanged(item.Key, caRemoved);
      ValueChanged(item.Value, caRemoved);
    end;
    Inc(item);
  end;
end;

function TBidiDictionary<TKey, TValue>.Contains(const value: TKeyValuePair;
  const comparer: IEqualityComparer<TKeyValuePair>): Boolean;
var
  pair: TKeyValuePair;
begin
  pair.Key := value.Key;
  Result := TryGetValue(value.Key, pair.Value) and comparer.Equals(pair, value);
end;

function TBidiDictionary<TKey, TValue>.ToArray: TArray<TKeyValuePair>;
var
  target: ^TKeyValuePair;
  source: PItem;
  i: Integer;
begin
  SetLength(Result, fCount);
  target := Pointer(Result);
  if Assigned(target) then
  begin
    source := Pointer(fItems);
    for i := 1 to fItemCount do
    begin
      if source.KeyHashCode >= 0 then
      begin
        target.Key := source.Key;
        target.Value := source.Value;
        Inc(target);
      end;
      Inc(source);
    end;
  end;
end;

function TBidiDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

function TBidiDictionary<TKey, TValue>.GetNonEnumeratedCount: Integer;
begin
  Result := fCount;
end;

procedure TBidiDictionary<TKey, TValue>.AddOrSetKey(const value: TValue; const key: TKey);
var
  keyHashCode, keyBucketIndex, valueHashCode, valueBucketIndex, keyItemIndex, valueItemIndex: Integer;
  keyFound, valueFound: Boolean;
begin
  valueHashCode := ValueHash(value);
  valueFound := FindValue(value, valueHashCode, valueBucketIndex, valueItemIndex);
  keyHashCode := KeyHash(key);
  keyFound := FindKey(key, keyHashCode, keyBucketIndex, keyItemIndex);

  if keyFound then
  begin
    if valueFound and (keyItemIndex = valueItemIndex) then
      Exit; // this key/value pair are already mapped to each other
    RaiseHelper.DuplicateKey;
  end
  else if valueFound then
    // value found, but key not found, this is a replace value operation
    DoSetKey(valueBucketIndex, valueItemIndex, keyHashCode, key)
  else
  begin
    // neither value nor key found, this is an add operation
    if fItemCount = Capacity then
    begin
      Grow;
      // rehash invalidates the indices
      FindKey(key, keyHashCode, keyBucketIndex, keyItemIndex);
      FindValue(value, valueHashCode, valueBucketIndex, valueItemIndex);
    end;
    Assert(keyItemIndex = valueItemIndex);
    DoAdd(keyHashCode, keyBucketIndex, valueHashCode, valueBucketIndex, keyItemIndex, key, value);
  end;
end;

procedure TBidiDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
begin
  SetItem(key, value);
end;

function TBidiDictionary<TKey, TValue>.AsReadOnly: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

function TBidiDictionary<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  item: TValue;
begin
  Result := TryGetValue(key, item) and fValueComparer.Equals(item, value);
end;

function TBidiDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
var
  bucketIndex, itemIndex: Integer;
begin
  Result := FindKey(key, KeyHash(key), bucketIndex, itemIndex);
end;

function TBidiDictionary<TKey, TValue>.ContainsValue(
  const value: TValue): Boolean;
var
  bucketIndex, itemIndex: Integer;
begin
  Result := FindValue(value, ValueHash(value), bucketIndex, itemIndex);
end;

procedure TBidiDictionary<TKey, TValue>.EnsureCompact;
begin
  if fCount <> fItemCount then
    Rehash(Capacity);
end;

function TBidiDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
begin
  TryExtract(key, Result);
end;

function TBidiDictionary<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
  foundItem: PItem;
begin
  if FindKey(key, KeyHash(key), keyBucketIndex, keyItemIndex)
    and FindValue(value, ValueHash(value), valueBucketIndex, valueItemIndex)
    and (keyItemIndex = valueItemIndex) then
  begin
    foundItem := @fItems[keyItemIndex];
    Result.Key := foundItem.Key;
    Result.Value := foundItem.Value;
    DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caExtracted);
    Exit;
  end;
  Result.Key := Default(TKey);
  Result.Value := Default(TValue);
end;

procedure TBidiDictionary<TKey, TValue>.TrimExcess;
begin
  SetCapacity(fCount);
end;

function TBidiDictionary<TKey, TValue>.TryAdd(const key: TKey;
  const value: TValue): Boolean;
var
  keyHashCode, keyBucketIndex, valueHashCode, valueBucketIndex, keyItemIndex, valueItemIndex: Integer;
begin
  keyHashCode := KeyHash(key);
  if FindKey(key, keyHashCode, keyBucketIndex, keyItemIndex) then
    Exit(False);
  valueHashCode := ValueHash(value);
  if FindValue(value, valueHashCode, valueBucketIndex, valueItemIndex) then
    Exit(False);
  if fItemCount = Capacity then
  begin
    Grow;
    // rehash invalidates the indices
    FindKey(key, keyHashCode, keyBucketIndex, keyItemIndex);
    FindValue(value, valueHashCode, valueBucketIndex, valueItemIndex);
  end;
  Assert(keyItemIndex = valueItemIndex);
  DoAdd(keyHashCode, keyBucketIndex, valueHashCode, valueBucketIndex, keyItemIndex, key, value);
  Result := True;
end;

function TBidiDictionary<TKey, TValue>.TryExtract(const key: TKey;
  var value: TValue): Boolean;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
begin
  Result := FindKey(key, KeyHash(key), keyBucketIndex, keyItemIndex);
  if Result then
  begin
    value := fItems[keyItemIndex].Value;
    FindValue(value, fItems[keyItemIndex].ValueHashCode, valueBucketIndex, valueItemIndex);
    Assert(keyItemIndex = valueItemIndex);
    DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caExtracted);
  end
  else
    value := Default(TValue);
end;

function TBidiDictionary<TKey, TValue>.TryGetElementAt(var item: TKeyValuePair;
  index: Integer): Boolean;
begin
  Result := Cardinal(index) < Cardinal(fCount);
  if Result then
  begin
    EnsureCompact;
    item.Key := fItems[index].Key;
    item.Value := fItems[index].Value;
  end;
end;

function TBidiDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  var value: TValue): Boolean;
var
  bucketIndex, itemIndex: Integer;
begin
  Result := FindKey(key, KeyHash(key), bucketIndex, itemIndex);
  if Result then
    value := fItems[itemIndex].Value
  else
    value := Default(TValue);
end;

function TBidiDictionary<TKey, TValue>.TryUpdateValue(const key: TKey;
  const newValue: TValue; var oldValue: TValue): Boolean;
begin
  Result := TryGetValue(key, oldValue);
  if Result then
    SetItem(key, newValue);
end;

function TBidiDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
begin
  Result := FindKey(key, KeyHash(key), keyBucketIndex, keyItemIndex);
  if Result then
  begin
    FindValue(fItems[keyItemIndex].Value, fItems[keyItemIndex].ValueHashCode, valueBucketIndex, valueItemIndex);
    Assert(keyItemIndex = valueItemIndex);
    DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caRemoved);
  end;
end;

function TBidiDictionary<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
begin
  Result := FindKey(key, KeyHash(key), keyBucketIndex, keyItemIndex)
    and fValueComparer.Equals(fItems[keyItemIndex].Value, value);
  if Result then
  begin
    FindValue(value, fItems[keyItemIndex].ValueHashCode, valueBucketIndex, valueItemIndex);
    Assert(keyItemIndex = valueItemIndex);
    DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caRemoved);
  end;
end;

function TBidiDictionary<TKey, TValue>.GetInverse: IBidiDictionary<TValue, TKey>;
begin
  Result := fInverse;
end;

function TBidiDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fKeys;
end;

function TBidiDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey): TValue;
begin
  TryGetValue(key, Result);
end;

function TBidiDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey;
  const defaultValue: TValue): TValue;
begin
  if not TryGetValue(key, Result) then
    Result := defaultValue;
end;

function TBidiDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TBidiDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
var
  keyBucketIndex, keyItemIndex: Integer;
begin
  if not FindKey(key, KeyHash(key), keyBucketIndex, keyItemIndex) then
    RaiseHelper.KeyNotFound;
  Result := fItems[keyItemIndex].Value;
end;

procedure TBidiDictionary<TKey, TValue>.SetItem(const key: TKey; const value: TValue);
var
  keyHashCode, keyBucketIndex, keyItemIndex: Integer;
  valueHashCode, valueBucketIndex, valueItemIndex: Integer;
  keyFound, valueFound: Boolean;
begin
  keyHashCode := KeyHash(key);
  keyFound := FindKey(key, keyHashCode, keyBucketIndex, keyItemIndex);
  valueHashCode := ValueHash(value);
  valueFound := FindValue(value, valueHashCode, valueBucketIndex, valueItemIndex);

  if valueFound then
  begin
    if keyFound and (keyItemIndex = valueItemIndex) then
      Exit; // this key/value pair are already mapped to each other
    RaiseHelper.DuplicateKey;
  end
  else if keyFound then
    // key found, but value not found, this is a replace value operation
    DoSetValue(keyBucketIndex, keyItemIndex, valueHashCode, value)
  else
  begin
    // neither key nor value found, this is an add operation
    if fItemCount = Capacity then
    begin
      Grow;
      // rehash invalidates the indices
      FindKey(key, keyHashCode, keyBucketIndex, keyItemIndex);
      FindValue(value, valueHashCode, valueBucketIndex, valueItemIndex);
    end;
    Assert(keyItemIndex = valueItemIndex);
    DoAdd(keyHashCode, keyBucketIndex, valueHashCode, valueBucketIndex, keyItemIndex, key, value);
  end;
end;

{$ENDREGION}


{$REGION 'TBidiDictionary<TKey, TValue>.TInverse'}

procedure TBidiDictionary<TKey, TValue>.TInverse.Add(const value: TValue;
  const key: TKey);
begin
  fSource.Add(key, value);
end;

procedure TBidiDictionary<TKey, TValue>.TInverse.AddOrSetValue(
  const value: TValue; const key: TKey);
begin
  SetItem(value, key);
end;

function TBidiDictionary<TKey, TValue>.TInverse.Add(const item: TValueKeyPair): Boolean;
begin
  Result := fSource.TryAdd(item.Value, item.Key);
end;

function TBidiDictionary<TKey, TValue>.TInverse.AsReadOnly: IReadOnlyDictionary<TValue, TKey>;
begin
  Result := Self;
end;

procedure TBidiDictionary<TKey, TValue>.TInverse.Changed(
  const item: TValueKeyPair; action: TCollectionChangedAction);
begin
  if Assigned(OnChanged) and OnChanged.CanInvoke then
    OnChanged.Invoke(fSource, item, action);
end;

procedure TBidiDictionary<TKey, TValue>.TInverse.Clear;
begin
  fSource.Clear;
end;

function TBidiDictionary<TKey, TValue>.TInverse.Contains(const value: TValue;
  const key: TKey): Boolean;
begin
  Result := fSource.Contains(key, value);
end;

function TBidiDictionary<TKey, TValue>.TInverse.Contains(
  const value: TValueKeyPair): Boolean;
begin
  Result := fSource.Contains(value.Value, value.Key);
end;

function TBidiDictionary<TKey, TValue>.TInverse.Contains(
  const value: TValueKeyPair;
  const comparer: IEqualityComparer<TValueKeyPair>): Boolean;
var
  pair: TValueKeyPair;
begin
  pair.Key := value.Key;
  Result := TryGetValue(value.Key, pair.Value) and comparer.Equals(pair, value);
end;

function TBidiDictionary<TKey, TValue>.TInverse.ContainsKey(
  const value: TValue): Boolean;
begin
  Result := fSource.ContainsValue(value);
end;

function TBidiDictionary<TKey, TValue>.TInverse.ContainsValue(
  const key: TKey): Boolean;
begin
  Result := fSource.ContainsKey(key);
end;

function TBidiDictionary<TKey, TValue>.TInverse.Extract(
  const item: TValueKeyPair): TValueKeyPair;
begin
  Result := Extract(item.Key, item.Value);
end;

function TBidiDictionary<TKey, TValue>.TInverse.Extract(
  const value: TValue): TKey;
begin
  TryExtract(value, Result);
end;

function TBidiDictionary<TKey, TValue>.TInverse.Extract(const value: TValue;
  const key: TKey): TValueKeyPair;
var
  pair: TKeyValuePair;
begin
  pair := fSource.Extract(key, value);
  Result.Key := pair.Value;
  Result.Value := pair.Key;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetCapacity: Integer;
begin
  Result := fSource.Capacity;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetCount: Integer;
begin
  Result := fSource.fCount;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetEnumerator: IEnumerator<TValueKeyPair>; //FI:W521
begin
  fSource._AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.InverseEnumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrentInverse, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self.fSource;
    fVersion := fSource.fVersion;
  end;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetInverse: IBidiDictionary<TKey, TValue>;
begin
  Result := fSource;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetItem(const value: TValue): TKey;
var
  valueBucketIndex, valueItemIndex: Integer;
begin
  if not fSource.FindValue(value, fSource.ValueHash(value), valueBucketIndex, valueItemIndex) then
    RaiseHelper.KeyNotFound;
  Result := fSource.fItems[valueItemIndex].Key;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetKeys: IReadOnlyCollection<TValue>;
begin
  Result := fSource.fValues;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetKeyType: PTypeInfo;
begin
  Result := fSource.GetValueType;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetNonEnumeratedCount: Integer;
begin
  Result := fSource.fCount;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetOnKeyChanged: ICollectionChangedEvent<TValue>;
begin
  Result := fSource.fOnValueChanged;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetOnValueChanged: ICollectionChangedEvent<TKey>;
begin
  Result := fSource.fOnKeyChanged;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetValueOrDefault(
  const value: TValue): TKey;
begin
  TryGetValue(value, Result);
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetValueOrDefault(
  const value: TValue; const defaultKey: TKey): TKey;
begin
  if not TryGetValue(value, Result) then
    Result := defaultKey;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetValues: IReadOnlyCollection<TKey>;
begin
  Result := fSource.fKeys;
end;

function TBidiDictionary<TKey, TValue>.TInverse.GetValueType: PTypeInfo;
begin
  Result := fSource.GetKeyType;
end;

function TBidiDictionary<TKey, TValue>.TInverse.Remove(
  const value: TValue): Boolean;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
begin
  Result := fSource.FindValue(value, fSource.ValueHash(value), valueBucketIndex, valueItemIndex);
  if Result then
  begin
    fSource.FindKey(fSource.fItems[valueItemIndex].Key,
      fSource.fItems[valueItemIndex].KeyHashCode, keyBucketIndex, keyItemIndex);
    Assert(keyItemIndex = valueItemIndex);
    fSource.DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caRemoved);
  end;
end;

function TBidiDictionary<TKey, TValue>.TInverse.Remove(const value: TValue;
  const key: TKey): Boolean;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
begin
  Result := fSource.FindValue(value, fSource.ValueHash(value), valueBucketIndex, valueItemIndex)
    and fSource.fKeyComparer.Equals(fSource.fItems[valueItemIndex].Key, key);
  if Result then
  begin
    fSource.FindKey(key, fSource.fItems[valueItemIndex].KeyHashCode, keyBucketIndex, keyItemIndex);
    Assert(keyItemIndex = valueItemIndex);
    fSource.DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caRemoved);
  end;
end;

function TBidiDictionary<TKey, TValue>.TInverse.Remove(
  const item: TValueKeyPair): Boolean;
begin
  Result := Remove(item.Key, item.Value);
end;

function TBidiDictionary<TKey, TValue>.TInverse.RemoveRange(
  const values: array of TValue): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(values) do
    Inc(Result, Integer(Remove(values[i])));
end;

function TBidiDictionary<TKey, TValue>.TInverse.RemoveRange(
  const values: IEnumerable<TValue>): Integer;
var
  value: TValue;
begin
  Result := 0;
  for value in values do
    Inc(Result, Integer(Remove(value)));
end;

procedure TBidiDictionary<TKey, TValue>.TInverse.SetCapacity(value: Integer);
begin
  fSource.SetCapacity(value);
end;

procedure TBidiDictionary<TKey, TValue>.TInverse.SetItem(const value: TValue;
  const key: TKey);
begin
  fSource.AddOrSetKey(value, key);
end;

function TBidiDictionary<TKey, TValue>.TInverse.ToArray: TArray<TValueKeyPair>;
var
  dict: TBidiDictionary<TKey, TValue>;
  target: ^TValueKeyPair;
  source: PItem;
  i: Integer;
begin
  dict := fSource;
  SetLength(Result, dict.fCount);
  target := Pointer(Result);
  if Assigned(target) then
  begin
    source := Pointer(dict.fItems);
    for i := 1 to dict.fItemCount do
    begin
      if source.KeyHashCode >= 0 then
      begin
        target.Key := source.Value;
        target.Value := source.Key;
        Inc(target);
      end;
      Inc(source);
    end;
  end;
end;

procedure TBidiDictionary<TKey, TValue>.TInverse.TrimExcess;
begin
  fSource.TrimExcess;
end;

function TBidiDictionary<TKey, TValue>.TInverse.TryAdd(const value: TValue;
  const key: TKey): Boolean;
begin
  Result := fSource.TryAdd(key, value);
end;

function TBidiDictionary<TKey, TValue>.TInverse.TryExtract(const value: TValue;
  var key: TKey): Boolean;
var
  keyBucketIndex, keyItemIndex, valueBucketIndex, valueItemIndex: Integer;
begin
  Result := fSource.FindValue(value, fSource.ValueHash(value), valueBucketIndex, valueItemIndex);
  if Result then
  begin
    key := fSource.fItems[valueItemIndex].Key;
    fSource.FindKey(key, fSource.fItems[valueItemIndex].KeyHashCode, keyBucketIndex, keyItemIndex);
    Assert(keyItemIndex = valueItemIndex);
    fSource.DoRemove(keyBucketIndex, valueBucketIndex, keyItemIndex, caExtracted);
  end
  else
    key := Default(TKey);
end;

function TBidiDictionary<TKey, TValue>.TInverse.TryGetElementAt(
  var item: TValueKeyPair; index: Integer): Boolean;
var
  pair: TKeyValuePair;
begin
  Result := fSource.TryGetElementAt(pair, index);
  if Result then
  begin
   item.Key := pair.Value;
   item.Value := pair.Key;
 end;
end;

function TBidiDictionary<TKey, TValue>.TInverse.TryGetValue(const value: TValue;
  var key: TKey): Boolean;
var
  bucketIndex, itemIndex: Integer;
begin
  Result := fSource.FindValue(value, fSource.ValueHash(value), bucketIndex, itemIndex);
  if Result then
    key := fSource.fItems[itemIndex].Key
  else
    key := Default(TKey);
end;

function TBidiDictionary<TKey, TValue>.TInverse.TryUpdateValue(const value: TValue;
  const newKey: TKey; var oldKey: TKey): Boolean;
begin
  Result := TryGetValue(value, oldKey);
  if Result then
    SetItem(value, newKey);
end;

function TBidiDictionary<TKey, TValue>.TInverse._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TBidiDictionary<TKey, TValue>.TInverse._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TBidiDictionary<TKey, TValue>.TEnumerator' }

function TBidiDictionary<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
var
  item: PItem;
begin
  item := fItem;
  Result.Key := item.Key;
  Result.Value := item.Value;
end;

function TBidiDictionary<TKey, TValue>.TEnumerator.GetCurrentInverse: TValueKeyPair;
var
  item: PItem;
begin
  item := fItem;
  Result.Key := item.Value;
  Result.Value := item.Key;
end;

function TBidiDictionary<TKey, TValue>.TEnumerator.GetCurrentKey: TKey;
begin
  Result := fItem.Key;
end;

function TBidiDictionary<TKey, TValue>.TEnumerator.GetCurrentValue: TValue;
begin
  Result := fItem.Value;
end;

function TBidiDictionary<TKey, TValue>.TEnumerator.MoveNext: Boolean;
var
  source: TBidiDictionary<TKey, TValue>;
  item: PItem;
begin
  source := fSource;
  if fVersion = source.fVersion then
  begin
    repeat
      if fItemIndex >= source.fItemCount then
        Break;

      item := @source.fItems[fItemIndex];
      Inc(fItemIndex);
      if item.KeyHashCode >= 0 then
      begin
        fItem := item;
        Exit(True);
      end;
    until False;
    Exit(False);
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TBidiDictionary<TKey, TValue>.TKeyCollection'}

constructor TBidiDictionary<TKey, TValue>.TKeyCollection.Create(
  const source: TBidiDictionary<TKey, TValue>);
begin
  fSource := source;
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection.Contains(const value: TKey): Boolean;
begin
  Result := fSource.ContainsKey(value);
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := fSource.fCount;
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>; //FI:W521
begin
  fSource._AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.KeysEnumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrentKey, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self.fSource;
    fVersion := fSource.fVersion;
  end;
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection.GetNonEnumeratedCount: Integer;
begin
  Result := fSource.fCount;
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
var
  dict: TBidiDictionary<TKey, TValue>;
  target: ^TKey;
  source: PItem;
  i: Integer;
begin
  dict := fSource;
  SetLength(Result, dict.fCount);
  target := Pointer(Result);
  if Assigned(target) then
  begin
    source := Pointer(dict.fItems);
    for i := 1 to dict.fItemCount do
    begin
      if source.KeyHashCode >= 0 then
      begin
        target^ := source.Key;
        Inc(target);
      end;
      Inc(source);
    end;
  end;
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection.TryGetElementAt(
  var key: TKey; index: Integer): Boolean;
begin
  Result := Cardinal(index) < Cardinal(fSource.fCount);
  if Result then
  begin
    fSource.EnsureCompact;
    key := fSource.fItems[index].Key;
  end;
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TBidiDictionary<TKey, TValue>.TKeyCollection._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TBidiDictionary<TKey, TValue>.TValueCollection'}

constructor TBidiDictionary<TKey, TValue>.TValueCollection.Create(
  const source: TBidiDictionary<TKey, TValue>);
begin
  fSource := source;
end;

function TBidiDictionary<TKey, TValue>.TValueCollection.Contains(const value: TValue): Boolean;
begin
  Result := fSource.ContainsValue(value);
end;

function TBidiDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fSource.fCount;
end;

function TBidiDictionary<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>; //FI:W521
begin
  fSource._AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.ValuesEnumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrentValue, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self.fSource;
    fVersion := fSource.fVersion;
  end;
end;

function TBidiDictionary<TKey, TValue>.TValueCollection.GetNonEnumeratedCount: Integer;
begin
  Result := fSource.fCount;
end;

function TBidiDictionary<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  dict: TBidiDictionary<TKey, TValue>;
  target: ^TValue;
  source: PItem;
  i: Integer;
begin
  dict := fSource;
  SetLength(Result, dict.fCount);
  target := Pointer(Result);
  if Assigned(target) then
  begin
    source := Pointer(dict.fItems);
    for i := 1 to dict.fItemCount do
    begin
      if source.KeyHashCode >= 0 then
      begin
        target^ := source.Value;
        Inc(target);
      end;
      Inc(source);
    end;
  end;
end;

function TBidiDictionary<TKey, TValue>.TValueCollection.TryGetElementAt(
  var value: TValue; index: Integer): Boolean;
begin
  Result := Cardinal(index) < Cardinal(fSource.fCount);
  if Result then
  begin
    fSource.EnsureCompact;
    value := fSource.fItems[index].Value;
  end;
end;

function TBidiDictionary<TKey, TValue>.TValueCollection._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TBidiDictionary<TKey, TValue>.TValueCollection._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TSortedDictionary<TKey, TValue>'}

constructor TSortedDictionary<TKey, TValue>.Create(
  const keyComparer: IComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  ValidateParams(TypeInfo(TKey), TypeInfo(TValue), 0, ownerships);

  fKeyComparer := keyComparer;
  fValueComparer := valueComparer;
  fOwnerships := ownerships;
end;

procedure TSortedDictionary<TKey, TValue>.AfterConstruction;
var
  keyType, valueType: PTypeInfo;
begin
  inherited AfterConstruction;

  keyType := GetKeyType;
  valueType := GetValueType;
  if not Assigned(fKeyComparer) then
    fKeyComparer := IComparer<TKey>(_LookupVtableInfo(giComparer, keyType, SizeOf(TKey)));
  if not Assigned(fValueComparer) then
    fValueComparer := IEqualityComparer<TValue>(_LookupVtableInfo(giEqualityComparer, valueType, SizeOf(TValue)));
  PPairComparer(fComparer).KeyComparer := fKeyComparer;

  fTree := TRedBlackTreeBase<TKey,TValue>.Create(fKeyComparer);

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
      Self, fTree, @fVersion, fValueComparer, valueType, SizeOf(TKey));
    tkInterface: fValues := TTreeMapInnerCollection.Create_Interface(
      Self, fTree, @fVersion, fValueComparer, valueType, SizeOf(TKey));
  else{$ELSE}begin{$ENDIF}
    fValues := TTreeMapInnerCollection.Create(TTreeMapInnerCollection<TValue>,
      Self, fTree, @fVersion, fValueComparer, valueType, SizeOf(TKey));
  end;
end;

procedure TSortedDictionary<TKey, TValue>.BeforeDestruction;
begin
  Clear;
  fTree.Free;
  fKeys.Free;
  fValues.Free;
  inherited BeforeDestruction;
end;

procedure TSortedDictionary<TKey, TValue>.Add(const key: TKey; const value: TValue);
var
  node: Pointer;
begin
  node := fTree.AddNode(key);
  if Assigned(node) then
  begin
    PNode(node).Value := value;
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    if Assigned(Notify) then
      Notify(Self, PKeyValuePair(@PNode(node).Key)^, caAdded);
    with fOnKeyChanged do if CanInvoke then
      Invoke(Self, PNode(node).Key, caAdded);
    with fOnValueChanged do if CanInvoke then
      Invoke(Self, PNode(node).Value, caAdded);
  end
  else
    RaiseHelper.DuplicateKey;
end;

procedure TSortedDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
begin
  SetItem(key, value);
end;

function TSortedDictionary<TKey, TValue>.AsReadOnly: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

procedure TSortedDictionary<TKey, TValue>.Clear;
var
  node, next: PBinaryTreeNode;
begin
  if fTree.Count = 0 then
    Exit;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

  next := fTree.Root.LeftMost;
  if Assigned(next) then
    repeat
      node := next;
      if Assigned(Notify) then
        Notify(Self, PKeyValuePair(@PNode(node).Key)^, caRemoved);
      with fOnKeyChanged do if CanInvoke then
        Invoke(Self, PNode(node).Key, caRemoved);
    {$IFDEF DELPHIXE7_UP}
      if GetTypeKind(TKey) = tkClass then
    {$ENDIF}
      if doOwnsKeys in fOwnerships then
        PObject(@PNode(node).Key).Free;
      with fOnValueChanged do if CanInvoke then
        Invoke(Self, PNode(node).Value, caRemoved);
    {$IFDEF DELPHIXE7_UP}
      if GetTypeKind(TValue) = tkClass then
    {$ENDIF}
      if doOwnsValues in fOwnerships then
        PObject(@PNode(node).Value).Free;
      next := node.Next;
    until not Assigned(next);

  fTree.Clear;
end;

function TSortedDictionary<TKey, TValue>.Contains(const value: TKeyValuePair;
  const comparer: IEqualityComparer<TKeyValuePair>): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(value.Key);
  if not Assigned(node) then Exit(Boolean(node));
  Result := comparer.Equals(PKeyValuePair(@PNode(node).Key)^, value);
end;

function TSortedDictionary<TKey, TValue>.Contains(const key: TKey; const value: TValue): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  if not Assigned(node) then Exit(Boolean(node));
  Result := fValueComparer.Equals(PNode(node).Value, value);
end;

function TSortedDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  Result := Assigned(node);
end;

function TSortedDictionary<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  temp: Pointer;
  node: PBinaryTreeNode;
begin
  temp := fTree.Root.LeftMost;
  if not Assigned(temp) then Exit(Boolean(temp));
  repeat
    node := temp;
    Result := fValueComparer.Equals(PNode(node).Value, value);
    if Result then
      Exit;
    temp := node.Next;
  until not Assigned(temp);
  Result := Boolean(temp);
end;

function TSortedDictionary<TKey, TValue>.DoRemove(const node: PNode;
  action: TCollectionChangedAction): Boolean;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if Assigned(Notify) then
    Notify(Self, PKeyValuePair(@node.Key)^, action);
  with fOnKeyChanged do if CanInvoke then
    Invoke(Self, node.Key, action);
{$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) = tkClass then
{$ENDIF}
  if (action = caRemoved) and (doOwnsKeys in fOwnerships) then
    PObject(@node.Key).Free;
  with fOnValueChanged do if CanInvoke then
    Invoke(Self, node.Value, action);
{$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) = tkClass then
{$ENDIF}
  if (action = caRemoved) and (doOwnsValues in fOwnerships) then
    PObject(@node.Value).Free;

  fTree.DeleteNode(Pointer(node));
  Result := True;
end;

function TSortedDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
begin
  TryExtract(key, Result);
end;

function TSortedDictionary<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  if Assigned(node) and fValueComparer.Equals(PNode(node).Value, value) then
  begin
    Result.Key := PNode(node).Key;
    Result.Value := PNode(node).Value;
    DoRemove(node, caExtracted);
    Exit;
  end;
  Result.Key := Default(TKey);
  Result.Value := Default(TValue);
end;

function TSortedDictionary<TKey, TValue>.GetCapacity: Integer;
begin
  Result := fTree.Capacity;
end;

function TSortedDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fTree.Count;
end;

function TSortedDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent,
    @TTreeMapInnerCollection.TEnumerator.MoveNext))^ do
  begin
    Parent := Self;
    fTree := Self.fTree;
    fOffset := 0;
    fSourceVersion := @Self.fVersion;
    fVersion := Self.fVersion;
  end;
end;

function TSortedDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
begin
  if not TryGetValue(key, Result) then
    RaiseHelper.KeyNotFound;
end;

function TSortedDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := IReadOnlyCollection<TKey>(fKeys._this);
end;

function TSortedDictionary<TKey, TValue>.GetNonEnumeratedCount: Integer;
begin
  Result := fTree.Count;
end;

function TSortedDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey): TValue;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  if Assigned(node) then
    Result := PNode(node).Value
  else
    Result := Default(TValue);
end;

function TSortedDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey;
  const defaultValue: TValue): TValue;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  if Assigned(node) then
    Result := PNode(node).Value
  else
    Result := defaultValue;
end;

function TSortedDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := IReadOnlyCollection<TValue>(fValues._this);
end;

function TSortedDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  if not Assigned(node) then Exit(Boolean(node));
  Result := DoRemove(node, caRemoved);
end;

function TSortedDictionary<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  temp, node: Pointer;
begin
  temp := fTree.FindNode(key);
  if not Assigned(temp) then Exit(Boolean(temp));
  node := temp;
  Result := fValueComparer.Equals(value, PNode(node).Value);
  if not Result then Exit;
  Result := DoRemove(node, caRemoved);
end;

procedure TSortedDictionary<TKey, TValue>.SetCapacity(value: Integer);
begin
  fTree.Capacity := value;
end;

procedure TSortedDictionary<TKey, TValue>.SetItem(const key: TKey; const value: TValue);
var
  temp: Pointer;
  node: PNode;
  existingNode: Boolean;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  temp := fTree.AddNode(key, True);
  node := temp;
  node := Pointer(IntPtr(node) and not 1); // clear lowest bit

  existingNode := Odd(IntPtr(temp)); // an existing node
  if existingNode then
  begin
    if Assigned(Notify) then
      Notify(Self, PKeyValuePair(@node.Key)^, caRemoved);
    with fOnValueChanged do if CanInvoke then
      Invoke(Self, node.Value, caRemoved);
  end;

  node.Value := value;

  if Assigned(Notify) then
    Notify(Self, PKeyValuePair(@node.Key)^, caAdded);
  if not existingNode then
    with fOnKeyChanged do if CanInvoke then
      Invoke(Self, node.Key, caAdded);
  with fOnValueChanged do if CanInvoke then
    Invoke(Self, node.Value, caAdded);
end;

function TSortedDictionary<TKey, TValue>.ToArray: TArray<TKeyValuePair>;
var
  node, next: PBinaryTreeNode;
  target: PKeyValuePair;
begin
  SetLength(Result, fTree.Count);
  next := fTree.Root.LeftMost;
  if Assigned(next) then
  begin
    target := Pointer(Result);
    repeat
      node := next;
      target.Key := PNode(node).Key;
      target.Value := PNode(node).Value;
      Inc(target);
      next := node.Next;
    until not Assigned(next);
  end;
end;

procedure TSortedDictionary<TKey, TValue>.TrimExcess;
begin
  fTree.TrimExcess;
end;

function TSortedDictionary<TKey, TValue>.TryAdd(const key: TKey; const value: TValue): Boolean;
var
  temp: Pointer;
  node: PNode;
begin
  temp := fTree.AddNode(key);
  if not Assigned(temp) then Exit(Boolean(temp));

  node := temp;
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  node.Value := value;
  if Assigned(Notify) then
    Notify(Self, PKeyValuePair(@node.Key)^, caAdded);
  with fOnKeyChanged do if CanInvoke then
    Invoke(Self, node.Key, caAdded);
  with fOnValueChanged do if CanInvoke then
    Invoke(Self, node.Value, caAdded);
  Result := True;
end;

function TSortedDictionary<TKey, TValue>.TryExtract(const key: TKey; var value: TValue): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  if Assigned(node) then
  begin
    value := PNode(node).Value;
    Result := DoRemove(node, caExtracted);
    Exit;
  end;
  value := Default(TValue);
  Result := False;
end;

function TSortedDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  var value: TValue): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  if Assigned(node) then
  begin
    value := PNode(node).Value;
    Exit(True);
  end;
  value := Default(TValue);
  Result := False;
end;

function TSortedDictionary<TKey, TValue>.TryUpdateValue(const key: TKey;
  const newValue: TValue; var oldValue: TValue): Boolean;
var
  temp: Pointer;
  node: PNode;
begin
  if fTree.fCount = 0 then
  begin
    oldValue := Default(TValue);
    Exit(False);
  end;

  temp := fTree.FindNode(key);
  Result := Assigned(temp);
  if Result then
  begin
    node := temp;
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

    if Assigned(Notify) then
      Notify(Self, PKeyValuePair(@node.Key)^, caRemoved);
    with fOnValueChanged do if CanInvoke then
      Invoke(Self, node.Value, caRemoved);

    oldValue := node.Value;
    node.Value := newValue;

    if Assigned(Notify) then
      Notify(Self, PKeyValuePair(@node.Key)^, caAdded);
    with fOnValueChanged do if CanInvoke then
      Invoke(Self, node.Value, caAdded);
  end
  else
    oldValue := Default(TValue);
end;

{$ENDREGION}


{$REGION 'TSortedDictionary<TKey, TValue>.TEnumerator'}

function TSortedDictionary<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
var
  item: PKeyValuePair;
begin
  item := fItem;
  Result.Key := item.Key;
  Result.Value := item.Value;
end;

{$ENDREGION}


{$REGION 'TFoldedDictionary<TKey, TValue>'}

constructor TFoldedDictionary<TKey, TValue>.Create(keyType,
  valueType, elementType: PTypeInfo; capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  ValidateParams(keyType, valueType, capacity, ownerships);

  fHashTable.Comparer := keyComparer;
  fHashTable.Ownerships := ownerships;
  fValueComparer := valueComparer;

  fHashTable.ItemsInfo := TypeInfo(TItems);
  fHashTable.Capacity := capacity;

  fElementType := elementType;
  fKeyType := keyType;
  fValueType := valueType;
end;

function TFoldedDictionary<TKey, TValue>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TFoldedDictionary<TKey, TValue>.GetKeyType: PTypeInfo;
begin
  Result := fKeyType;
end;

function TFoldedDictionary<TKey, TValue>.GetValueType: PTypeInfo;
begin
  Result := fValueType;
end;

{$ENDREGION}


{$REGION 'TFoldedBidiDictionary<TKey, TValue>'}

constructor TFoldedBidiDictionary<TKey, TValue>.Create(keyType, valueType,
  elementType: PTypeInfo; capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  ValidateParams(TypeInfo(TKey), TypeInfo(TValue), capacity, ownerships);

  fOwnerships := ownerships;
  fKeyComparer := keyComparer;
  fValueComparer := valueComparer;

  SetCapacity(capacity);

  fElementType := elementType;
  fKeyType := keyType;
  fValueType := valueType;
end;

function TFoldedBidiDictionary<TKey, TValue>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TFoldedBidiDictionary<TKey, TValue>.GetKeyType: PTypeInfo;
begin
  Result := fKeyType;
end;

function TFoldedBidiDictionary<TKey, TValue>.GetValueType: PTypeInfo;
begin
  Result := fValueType;
end;

{$ENDREGION}


end.
