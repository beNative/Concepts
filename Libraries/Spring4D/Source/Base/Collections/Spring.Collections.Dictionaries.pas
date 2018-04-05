{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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
  Classes,
  Generics.Collections,
  Generics.Defaults,
  Spring,
  Spring.Collections,
{$IFNDEF DELPHI2010}
  Spring.Collections.Trees,
{$ENDIF}
  Spring.Collections.Base;

type
  TDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>,
    IDictionary<TKey, TValue>, IReadOnlyDictionary<TKey, TValue>)
  protected
  {$REGION 'Nested Types'}
    type
      TRTLDictionary = Generics.Collections.TDictionary<TKey, TValue>;
      TKeyValuePair = Generics.Collections.TPair<TKey, TValue>;
      TKeyCollectionBase = TContainedReadOnlyCollection<TKey>;
      TValueCollectionBase = TContainedReadOnlyCollection<TValue>;

      TKeyCollection = class(TKeyCollectionBase)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fDictionary: TRTLDictionary;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const controller: IInterface;
          const dictionary: TRTLDictionary);

      {$REGION 'Implements IEnumerable<TKey>'}
        function GetEnumerator: IEnumerator<TKey>; override;
        function Contains(const value: TKey;
          const comparer: IEqualityComparer<TKey>): Boolean; override;
        function ToArray: TArray<TKey>; override;
      {$ENDREGION}
      end;

      TValueCollection = class(TValueCollectionBase)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fDictionary: TRTLDictionary;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const controller: IInterface;
          const dictionary: TRTLDictionary);

      {$REGION 'Implements IEnumerable<TValue>'}
        function GetEnumerator: IEnumerator<TValue>; override;
        function Contains(const value: TValue;
          const comparer: IEqualityComparer<TValue>): Boolean; override;
        function ToArray: TArray<TValue>; override;
      {$ENDREGION}
      end;

      TOrderedEnumerable = class(TIterator<TKeyValuePair>)
      private
        fSource: TDictionary<TKey, TValue>;
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fDictionary: TRTLDictionary;
        fSortedKeys: TArray<TKey>;
        fIndex: Integer;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
        procedure Dispose; override;
        procedure Start; override;
        function TryMoveNext(var current: TKeyValuePair): Boolean; override;
      public
        constructor Create(const source: TDictionary<TKey, TValue>);
        destructor Destroy; override;
        function Clone: TIterator<TKeyValuePair>; override;
      end;

{$IFDEF DELPHI2010}
      TKeyCollectionHelper = class(TRTLDictionary.TKeyCollection)
      public
        function ToArray: TArray<TKey>;
      end;
{$ENDIF}
  {$ENDREGION}
  private
    fDictionary: TRTLDictionary;
    fOwnership: TOwnershipType;
    fKeys: TKeyCollectionBase;
    fValues: TValueCollectionBase;
    fOnKeyNotify: TCollectionNotifyEvent<TKey>;
    fOnValueNotify: TCollectionNotifyEvent<TValue>;
    procedure DoKeyNotify(Sender: TObject; const Item: TKey;
      Action: TCollectionNotification);
    procedure DoValueNotify(Sender: TObject; const Item: TValue;
      Action: TCollectionNotification);
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(const key: TKey): TValue; virtual;
    function GetKeys: IReadOnlyCollection<TKey>; override;
    function GetValues: IReadOnlyCollection<TValue>; override;
    procedure SetItem(const key: TKey; const value: TValue); virtual;
  {$ENDREGION}
    function CreateKeyCollection: TKeyCollectionBase; virtual;
    function CreateValueCollection: TValueCollectionBase; virtual;
  public
    constructor Create; overload; override;
    constructor Create(capacity: Integer); overload;
    constructor Create(const comparer: IEqualityComparer<TKey>); overload;
    constructor Create(capacity: Integer;
      const comparer: IEqualityComparer<TKey>); overload;
    constructor Create(dictionary: TRTLDictionary;
      ownership: TOwnershipType); overload;

    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>; override;
    function Contains(const value: TKeyValuePair;
      const comparer: IEqualityComparer<TKeyValuePair>): Boolean; override;
    function Ordered: IEnumerable<TKeyValuePair>; override;
    function ToArray: TArray<TKeyValuePair>; override;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear; override;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    procedure Add(const key: TKey; const value: TValue); override;
    function Remove(const key: TKey): Boolean; override;
    function Remove(const key: TKey; const value: TValue): Boolean; override;
    function Extract(const key: TKey; const value: TValue): TKeyValuePair; override;
    function Contains(const key: TKey; const value: TValue): Boolean; override;
    function ContainsKey(const key: TKey): Boolean; override;
    function ContainsValue(const value: TValue): Boolean; override;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function Extract(const key: TKey): TValue; overload;
    function ExtractPair(const key: TKey): TKeyValuePair;
    function GetValueOrDefault(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
    function AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;

    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
  {$ENDREGION}
  end;

  TContainedDictionary<TKey, TValue> = class(TDictionary<TKey, TValue>)
  private
    fController: Pointer;
    function GetController: IInterface;
  protected
  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; override;
    function _Release: Integer; override;
  {$ENDREGION}
  public
    constructor Create(const controller: IInterface;
      const comparer: IEqualityComparer<TKey>);
    property Controller: IInterface read GetController;
  end;

  TBidiDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>,
    IReadOnlyDictionary<TKey, TValue>, IDictionary<TKey, TValue>,
    IBidiDictionary<TKey, TValue>)
  private
    type
      TKeyValuePair = Generics.Collections.TPair<TKey, TValue>;
  private
    fValuesByKey: IDictionary<TKey, TValue>;
    fKeysByValue: IDictionary<TValue, TKey>;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(const key: TKey): TValue; inline;
    function GetKey(const value: TValue): TKey;
    function GetKeys: IReadOnlyCollection<TKey>; override;
    function GetValue(const key: TKey): TValue;
    function GetValues: IReadOnlyCollection<TValue>; override;
    procedure SetItem(const key: TKey; const value: TValue); inline;
    procedure SetKey(const value: TValue; const key: TKey);
    procedure SetValue(const key: TKey; const value: TValue);
  {$ENDREGION}
  public
    constructor Create; overload; override;
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>); overload;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>; override;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear; override;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    procedure Add(const key: TKey; const value: TValue); override;
    function Remove(const key: TKey): Boolean; reintroduce; overload;
    function Remove(const key: TKey; const value: TValue): Boolean; override;
    function Extract(const key: TKey; const value: TValue): TKeyValuePair; override;
    function Contains(const key: TKey; const value: TValue): Boolean; override;
    function ContainsKey(const key: TKey): Boolean; override;
    function ContainsValue(const value: TValue): Boolean; override;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function Extract(const key: TKey): TValue; reintroduce; overload;
    function ExtractPair(const key: TKey): TKeyValuePair; reintroduce; overload;
    function AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
  {$ENDREGION}

  {$REGION 'Implements IBidiDictionary<TKey, TValue>'}
    function ExtractKey(const value: TValue): TKey;
    function ExtractValue(const key: TKey): TValue;
    function GetKeyOrDefault(const value: TValue): TKey; overload;
    function GetKeyOrDefault(const value: TValue; const defaultValue: TKey): TKey; overload;
    function GetValueOrDefault(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;
    function RemoveKey(const key: TKey): Boolean;
    function RemoveValue(const value: TValue): Boolean;
    function TryGetKey(const value: TValue; out key: TKey): Boolean;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
  {$ENDREGION}
  end;

  TOrderedDictionary<TKey, TValue> = class(TDictionary<TKey, TValue>,
    IOrderedDictionary<TKey, TValue>)
  protected
  {$REGION 'Nested Types'}
    type
      TKeyValuePair = TDictionary<TKey, TValue>.TKeyValuePair;
      TKeyCollectionBase = TDictionary<TKey, TValue>.TKeyCollectionBase;
      TValueCollectionBase = TDictionary<TKey, TValue>.TValueCollectionBase;

      TEnumerator = class(TEnumeratorBase<TKeyValuePair>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TOrderedDictionary<TKey,TValue>;
        fIndex: Integer;
      protected
        function GetCurrent: TKeyValuePair; override;
      public
        constructor Create(const source: TOrderedDictionary<TKey,TValue>);
        destructor Destroy; override;

        function MoveNext: Boolean; override;
      end;

      TKeyCollection = class(TKeyCollectionBase)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TOrderedDictionary<TKey,TValue>;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const source: TOrderedDictionary<TKey,TValue>);

      {$REGION 'Implements IEnumerable<TKey>'}
        function GetEnumerator: IEnumerator<TKey>; override;
        function Contains(const value: TKey;
          const comparer: IEqualityComparer<TKey>): Boolean; override;
        function ToArray: TArray<TKey>; override;
      {$ENDREGION}
      end;

      TKeyEnumerator = class(TEnumeratorBase<TKey>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TOrderedDictionary<TKey,TValue>;
        fIndex: Integer;
      protected
        function GetCurrent: TKey; override;
      public
        constructor Create(const source: TOrderedDictionary<TKey,TValue>);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
      end;

      TValueCollection = class(TValueCollectionBase)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TOrderedDictionary<TKey,TValue>;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const source: TOrderedDictionary<TKey,TValue>);

      {$REGION 'Implements IEnumerable<TValue>'}
        function GetEnumerator: IEnumerator<TValue>; override;
        function Contains(const value: TValue;
          const comparer: IEqualityComparer<TValue>): Boolean; override;
        function ToArray: TArray<TValue>; override;
      {$ENDREGION}
      end;

      TValueEnumerator = class(TEnumeratorBase<TValue>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TOrderedDictionary<TKey,TValue>;
        fIndex: Integer;
      protected
        function GetCurrent: TValue; override;
      public
        constructor Create(const source: TOrderedDictionary<TKey,TValue>);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
      end;
  {$ENDREGION}
  private
    fKeys: IList<TKey>;
  {$REGION 'Property Accessors'}
    function GetItem(index: Integer): TKeyValuePair; reintroduce; overload;
  {$ENDREGION}
  protected
    function CreateKeyCollection: TKeyCollectionBase; override;
    function CreateValueCollection: TValueCollectionBase; override;
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction); override;
  public
    constructor Create; override;
    constructor Create(const comparer: IEqualityComparer<TKey>); overload;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>; override;
    function ToArray: TArray<TKeyValuePair>; override;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear; override;
  {$ENDREGION}

  {$REGION 'Implements IOrderedDictionary<TPair<TKey, TList>>'}
    function IndexOf(const key: TKey): Integer;
  {$ENDREGION}
  end;

{$IFNDEF DELPHI2010}
  TSortedDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>, IDictionary<TKey, TValue>,
    IReadOnlyDictionary<TKey, TValue>)
  private
    {$REGION 'Private types'}
    type
      TKeyValue = Generics.Collections.TPair<TKey, TValue>;
      PNode = TRedBlackTreeNodeHelper<TKey, TValue>.PNode;
    {$ENDREGION}
  private
    fTree: IRedBlackTree<TKey,TValue>;
    fKeyComparer: IComparer<TKey>;
    fValueComparer: IComparer<TValue>;
    fKeyValueComparerByKey: IComparer<TKeyValue>;
    fKeys: IList<TKey>;
    fValues: IList<TValue>;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(const key: TKey): TValue; virtual;
    function GetKeys: IReadOnlyCollection<TKey>; override;
    function GetValues: IReadOnlyCollection<TValue>; override;
    procedure SetItem(const key: TKey; const value: TValue); virtual;
  {$ENDREGION}
  public
    constructor Create; override;
    constructor Create(const keyComparer: IComparer<TKey>; const valueComparer: IComparer<TValue>); overload;
    destructor Destroy; override;

    {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
      function GetEnumerator: IEnumerator<TKeyValue>; override;
      function Contains(const value: TKeyValue;
        const comparer: IEqualityComparer<TKeyValue>): Boolean; override;
      function Ordered: IEnumerable<TKeyValue>; override;
      function ToArray: TArray<TKeyValue>; override;
    {$ENDREGION}

    {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
      procedure Clear; override;
    {$ENDREGION}

    {$REGION 'Implements IMap<TKey, TValue>'}
      procedure Add(const key: TKey; const value: TValue); overload; override;
      function Remove(const key: TKey): Boolean; overload; override;
      function Remove(const key: TKey; const value: TValue): Boolean; override;
      function Extract(const key: TKey; const value: TValue): TKeyValue; overload; override;
      function Contains(const key: TKey; const value: TValue): Boolean; override;
      function ContainsKey(const key: TKey): Boolean; override;
      function ContainsValue(const value: TValue): Boolean; override;
      property Keys: IReadOnlyCollection<TKey> read GetKeys;
      property Values: IReadOnlyCollection<TValue> read GetValues;
    {$ENDREGION}

    {$REGION 'Implements IDictionary<TKey, TValue>'}
      procedure AddOrSetValue(const key: TKey; const value: TValue);
      function Extract(const key: TKey): TValue; reintroduce; overload;
      function ExtractPair(const key: TKey): TKeyValue; reintroduce; overload;
      function TryGetValue(const key: TKey; out value: TValue): Boolean;
      function AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;

      property Items[const key: TKey]: TValue read GetItem write SetItem; default;
    {$ENDREGION}

    {$REGION 'Implements IReadOnlyDictionary<TKey, TValue>'}
      function GetValueOrDefault(const key: TKey): TValue; overload;
      function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;
    {$ENDREGION}
  end;
{$ENDIF}

implementation

uses
  RTLConsts,
  SysUtils,
  Types,
  Spring.Collections.Extensions,
  Spring.Collections.Lists;


{$REGION 'TDictionary<TKey, TValue>'}

constructor TDictionary<TKey, TValue>.Create(dictionary: TRTLDictionary;
  ownership: TOwnershipType);
begin
  inherited Create;
  fDictionary := dictionary;
  fKeys := CreateKeyCollection;
  fValues := CreateValueCollection;
  fOwnership := ownership;
  fOnKeyNotify := fDictionary.OnKeyNotify;
  fOnValueNotify := fDictionary.OnValueNotify;
  fDictionary.OnKeyNotify := DoKeyNotify;
  fDictionary.OnValueNotify := DoValueNotify;
end;

constructor TDictionary<TKey, TValue>.Create;
var
  dictionary: TRTLDictionary;
begin
  dictionary := TRTLDictionary.Create;
  Create(dictionary, otOwned);
end;

constructor TDictionary<TKey, TValue>.Create(capacity: Integer);
var
  dictionary: TRTLDictionary;
begin
  dictionary := TRTLDictionary.Create(capacity);
  Create(dictionary, otOwned);
end;

constructor TDictionary<TKey, TValue>.Create(
  const comparer: IEqualityComparer<TKey>);
var
  dictionary: TRTLDictionary;
begin
  dictionary := TRTLDictionary.Create(comparer);
  Create(dictionary, otOwned);
end;

constructor TDictionary<TKey, TValue>.Create(capacity: Integer;
  const comparer: IEqualityComparer<TKey>);
var
  dictionary: TRTLDictionary;
begin
  dictionary := TRTLDictionary.Create(capacity, comparer);
  Create(dictionary, otOwned);
end;

function TDictionary<TKey, TValue>.CreateKeyCollection: TKeyCollectionBase;
begin
  Result := TKeyCollection.Create(Self, fDictionary);
end;

function TDictionary<TKey, TValue>.CreateValueCollection: TValueCollectionBase;
begin
  Result := TValueCollection.Create(Self, fDictionary);
end;

destructor TDictionary<TKey, TValue>.Destroy;
begin
  fKeys.Free;
  fValues.Free;
  if fOwnership = otOwned then
    fDictionary.Free
  else
  begin
    fDictionary.OnKeyNotify := fOnKeyNotify;
    fDictionary.OnValueNotify := fOnValueNotify;
  end;

  inherited Destroy;
end;

procedure TDictionary<TKey, TValue>.DoKeyNotify(Sender: TObject;
  const Item: TKey; Action: TCollectionNotification);
begin
  if Assigned(fOnKeyNotify) then
    fOnKeyNotify(Sender, Item, Action);
  KeyChanged(Item, TCollectionChangedAction(action));
end;

procedure TDictionary<TKey, TValue>.DoValueNotify(Sender: TObject;
  const Item: TValue; Action: TCollectionNotification);
begin
  if Assigned(fOnValueNotify) then
    fOnValueNotify(Sender, Item, Action);
  ValueChanged(Item, TCollectionChangedAction(Action));
end;

function TDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>;
var
  dictionary: TEnumerable<TKeyValuePair>;
begin
  dictionary := TEnumerable<TKeyValuePair>(fDictionary);
  Result := TEnumeratorAdapter<TKeyValuePair>.Create(dictionary);
end;

procedure TDictionary<TKey, TValue>.Clear;
begin
  fDictionary.Clear;
end;

function TDictionary<TKey, TValue>.Contains(const value: TKeyValuePair;
  const comparer: IEqualityComparer<TKeyValuePair>): Boolean;
var
  item: TValue;
begin
  Result := fDictionary.TryGetValue(value.Key, item);
  if Result then
    Result := comparer.Equals(TKeyValuePair.Create(value.Key, item), value);
end;

function TDictionary<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  found: Boolean;
  foundValue: TValue;
  comparer: IEqualityComparer<TValue>;
begin
  found := fDictionary.TryGetValue(key, foundValue);
  if found then
  begin
    comparer := TEqualityComparer<TValue>.Default;
    found := comparer.Equals(foundValue, value);
    if found then
{$IFDEF DELPHIXE2_UP}
      Result := fDictionary.ExtractPair(key);
{$ELSE}
    begin
      Result.Key := key;
      Result.Value := value;
      fDictionary.ExtractPair(key);
    end;
{$ENDIF}
  end;
  if not found then
  begin
    Result.Key := Default(TKey);
    Result.Value := Default(TValue);
  end;
end;

function TDictionary<TKey, TValue>.ToArray: TArray<TKeyValuePair>;
{$IFDEF DELPHI2010}
var
  pair: TKeyValuePair;
  index: Integer;
begin
  SetLength(Result, fDictionary.Count);
  index := 0;
  for pair in fDictionary do
  begin
    Result[index] := pair;
    Inc(index);
  end;
{$ELSE}
begin
  Result := fDictionary.ToArray;
{$ENDIF}
end;

function TDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

procedure TDictionary<TKey, TValue>.Add(const key: TKey;
  const value: TValue);
begin
  fDictionary.Add(key, value);
end;

procedure TDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
begin
  fDictionary.AddOrSetValue(key, value);
end;

function TDictionary<TKey, TValue>.AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

function TDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(key);
end;

function TDictionary<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  item: TValue;
begin
  Result := fDictionary.TryGetValue(key, item)
    and TEqualityComparer<TValue>.Default.Equals(item, value);
end;

function TDictionary<TKey, TValue>.ContainsValue(
  const value: TValue): Boolean;
begin
  Result := fDictionary.ContainsValue(value);
end;

function TDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
begin
  Result := ExtractPair(key).Value;
end;

function TDictionary<TKey, TValue>.ExtractPair(
  const key: TKey): TKeyValuePair;
begin
{$IFDEF DELPHIXE2_UP}
  Result := fDictionary.ExtractPair(key);
{$ELSE}
  if fDictionary.TryGetValue(key, Result.Value) then
  begin
    Result.Key := key;
    fDictionary.ExtractPair(key);
  end
  else
    Result := fDictionary.ExtractPair(key);
{$ENDIF}
end;

function TDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
begin
  Result := fDictionary.TryGetValue(key, value);
end;

function TDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(key);
  if Result then
    fDictionary.Remove(key);
end;

function TDictionary<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  comparer: IEqualityComparer<TValue>;
begin
  Result := fDictionary.ContainsKey(key);
  if Result then
  begin
    comparer := TEqualityComparer<TValue>.Default;
    Result := comparer.Equals(fDictionary[key], value);
    if Result then
      fDictionary.Remove(key);
  end;
end;

function TDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fKeys;
end;

function TDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey): TValue;
begin
  if not fDictionary.TryGetValue(key, Result) then
    Result := Default(TValue);
end;

function TDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey;
  const defaultValue: TValue): TValue;
begin
  if not fDictionary.TryGetValue(key, Result) then
    Result := defaultValue;
end;

function TDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
begin
  Result := fDictionary[key];
end;

function TDictionary<TKey, TValue>.Ordered: IEnumerable<TKeyValuePair>;
begin
  Result := TOrderedEnumerable.Create(Self);
end;

procedure TDictionary<TKey, TValue>.SetItem(const key: TKey;
  const value: TValue);
begin
  fDictionary.AddOrSetValue(key, value);
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TKeyCollection'}

constructor TDictionary<TKey, TValue>.TKeyCollection.Create(
  const controller: IInterface; const dictionary: TRTLDictionary);
begin
  inherited Create(controller);
  fDictionary := dictionary;
end;

function TDictionary<TKey, TValue>.TKeyCollection.Contains(const value: TKey;
  const comparer: IEqualityComparer<TKey>): Boolean;
begin
  Result := fDictionary.ContainsKey(value);
end;

function TDictionary<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
var
  key: TKey;
  index: Integer;
begin
  index := 0;
  SetLength(Result, fDictionary.Count);
  for key in fDictionary.Keys do
  begin
    Result[index] := key;
    Inc(index);
  end;
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TEnumeratorAdapter<TKey>.Create(fDictionary.Keys);
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TValueCollection'}

constructor TDictionary<TKey, TValue>.TValueCollection.Create(
  const controller: IInterface; const dictionary: TRTLDictionary);
begin
  inherited Create(controller);
  fDictionary := dictionary;
end;

function TDictionary<TKey, TValue>.TValueCollection.Contains(const value: TValue;
  const comparer: IEqualityComparer<TValue>): Boolean;
begin
  Result := fDictionary.ContainsValue(value);
end;

function TDictionary<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  value: TValue;
  index: Integer;
begin
  index := 0;
  SetLength(Result, fDictionary.Count);
  for value in fDictionary.Values do
  begin
    Result[index] := value;
    Inc(index);
  end;
end;

function TDictionary<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TEnumeratorAdapter<TValue>.Create(fDictionary.Values);
end;

function TDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

{$ENDREGION}


{$REGION 'TContainedDictionary<TKey, TValue>'}

constructor TContainedDictionary<TKey, TValue>.Create(
  const controller: IInterface; const comparer: IEqualityComparer<TKey>);
begin
  inherited Create(comparer);
  fController := Pointer(controller);
end;

function TContainedDictionary<TKey, TValue>.GetController: IInterface;
begin
  Result := IInterface(fController);
end;

function TContainedDictionary<TKey, TValue>._AddRef: Integer;
begin
  Result := IInterface(FController)._AddRef;
end;

function TContainedDictionary<TKey, TValue>._Release: Integer;
begin
  Result := IInterface(FController)._Release;
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TOrderedEnumerable'}

constructor TDictionary<TKey, TValue>.TOrderedEnumerable.Create(
  const source: TDictionary<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
{$IFNDEF AUTOREFCOUNT}
  fSource._AddRef;
{$ENDIF}
  fDictionary := fSource.fDictionary;
end;

destructor TDictionary<TKey, TValue>.TOrderedEnumerable.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  fSource._Release;
{$ENDIF}
  inherited Destroy;
end;

procedure TDictionary<TKey, TValue>.TOrderedEnumerable.Dispose;
begin
  fSortedKeys := nil;
end;

function TDictionary<TKey, TValue>.TOrderedEnumerable.Clone: TIterator<TKeyValuePair>;
begin
  Result := TOrderedEnumerable.Create(fSource);
end;

function TDictionary<TKey, TValue>.TOrderedEnumerable.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

procedure TDictionary<TKey, TValue>.TOrderedEnumerable.Start;
begin
{$IFDEF DELPHI2010}
  fSortedKeys := TKeyCollectionHelper(fDictionary.Keys).ToArray;
{$ELSE}
  fSortedKeys := fDictionary.Keys.ToArray;
{$ENDIF}
  TArray.Sort<TKey>(fSortedKeys);
end;

function TDictionary<TKey, TValue>.TOrderedEnumerable.TryMoveNext(var current: TKeyValuePair): Boolean;
begin
  if fIndex < Length(fSortedKeys) then
  begin
    current.Key := fSortedKeys[fIndex];
    current.Value := fDictionary[fSortedKeys[fIndex]];
    Inc(fIndex);
    Exit(True);
  end;
  Result := False;
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TKeyCollectionHelper'}

{$IFDEF DELPHI2010}
function TDictionary<TKey, TValue>.TKeyCollectionHelper.ToArray: TArray<TKey>;
var
  item: TKey;
  i: Integer;
begin
  SetLength(Result, Count);
  i := 0;
  for item in Self do
  begin
    Result[i] := item;
    Inc(i);
  end;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TBidiDictionary<TKey, TValue>'}

constructor TBidiDictionary<TKey, TValue>.Create;
begin
  Create(nil, nil);
end;

constructor TBidiDictionary<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>);
begin
  inherited Create;
  fKeysByValue := TDictionary<TValue, TKey>.Create(valueComparer);
  fValuesByKey := TDictionary<TKey, TValue>.Create(keyComparer);
end;

procedure TBidiDictionary<TKey, TValue>.Add(const key: TKey;
  const value: TValue);
begin
  if fValuesByKey.ContainsKey(key) then
    raise EInvalidOperationException.Create('key');
  if fKeysByValue.ContainsKey(value) then
    raise EInvalidOperationException.Create('value');
  fValuesByKey.Add(key, value);
  fKeysByValue.Add(value, key);
end;

procedure TBidiDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
var
  oldValue: TValue;
begin
  RemoveValue(value);
  if fValuesByKey.TryGetValue(key, oldValue) then
    fKeysByValue.Remove(oldValue);
  fKeysByValue.Add(value, key);
  fValuesByKey[key] := value;
end;

function TBidiDictionary<TKey, TValue>.AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

procedure TBidiDictionary<TKey, TValue>.Clear;
begin
  fValuesByKey.Clear;
  fKeysByValue.Clear;
end;

function TBidiDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fValuesByKey.ContainsKey(key);
end;

function TBidiDictionary<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  item: TValue;
begin
  Result := fValuesByKey.TryGetValue(key, item)
    and TEqualityComparer<TValue>.Default.Equals(value, item);
end;

function TBidiDictionary<TKey, TValue>.ContainsValue(
  const value: TValue): Boolean;
begin
  Result := fKeysByValue.ContainsKey(value);
end;

function TBidiDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
begin
  Result := ExtractValue(key);
end;

function TBidiDictionary<TKey, TValue>.ExtractKey(const value: TValue): TKey;
begin
  if fKeysByValue.TryGetValue(value, Result) then
  begin
    fKeysByValue.Extract(value);
    fValuesByKey.Extract(Result);
  end
  else
    Result := Default(TKey);
end;

function TBidiDictionary<TKey, TValue>.ExtractPair(
  const key: TKey): TKeyValuePair;
begin
  raise ENotImplementedException.Create('ExtractPair');
end;

function TBidiDictionary<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
begin
  Result := fValuesByKey.Extract(key, value);
  fKeysByValue.Extract(value, key);
end;

function TBidiDictionary<TKey, TValue>.ExtractValue(const key: TKey): TValue;
begin
  if fValuesByKey.TryGetValue(key, Result) then
  begin
    fKeysByValue.Extract(Result);
    fValuesByKey.Extract(key);
  end
  else
    Result := Default(TValue);
end;

function TBidiDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fValuesByKey.Count;
end;

function TBidiDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>;
begin
  Result := fValuesByKey.GetEnumerator();
end;

function TBidiDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
begin
  Result := GetValue(key);
end;

function TBidiDictionary<TKey, TValue>.GetKey(const value: TValue): TKey;
begin
  Result := fKeysByValue[value];
end;

function TBidiDictionary<TKey, TValue>.GetKeyOrDefault(
  const value: TValue): TKey;
begin
  if not fKeysByValue.TryGetValue(value, Result) then
    Result := Default(TKey);
end;

function TBidiDictionary<TKey, TValue>.GetKeyOrDefault(const value: TValue;
  const defaultValue: TKey): TKey;
begin
  if not fKeysByValue.TryGetValue(value, Result) then
    Result := defaultValue;
end;

function TBidiDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fValuesByKey.Keys;
end;

function TBidiDictionary<TKey, TValue>.GetValue(const key: TKey): TValue;
begin
  Result := fValuesByKey[key];
end;

function TBidiDictionary<TKey, TValue>.GetValueOrDefault(
  const key: TKey): TValue;
begin
  if not fValuesByKey.TryGetValue(key, Result) then
    Result := Default(TValue);
end;

function TBidiDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey;
  const defaultValue: TValue): TValue;
begin
  if not fValuesByKey.TryGetValue(key, Result) then
    Result := defaultValue;
end;

function TBidiDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fKeysByValue.Keys;
end;

function TBidiDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
begin
  Result := RemoveKey(key);
end;

function TBidiDictionary<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  item: TValue;
begin
  Result := fValuesByKey.TryGetValue(key, item)
    and TEqualityComparer<TValue>.Default.Equals(value, item);
  if Result then
  begin
    fValuesByKey.Remove(key);
    fKeysByValue.Remove(value);
  end;
end;

function TBidiDictionary<TKey, TValue>.RemoveKey(const key: TKey): Boolean;
var
  value: TValue;
begin
  Result := fValuesByKey.TryGetValue(key, value);
  if Result then
  begin
    fValuesByKey.Remove(key);
    fKeysByValue.Remove(value);

    // notify
  end;
end;

function TBidiDictionary<TKey, TValue>.RemoveValue(const value: TValue): Boolean;
var
  key: TKey;
begin
  Result := fKeysByValue.TryGetValue(value, key);
  if Result then
  begin
    fValuesByKey.Remove(key);
    fKeysByValue.Remove(value);

    // notify
  end;
end;

procedure TBidiDictionary<TKey, TValue>.SetItem(const key: TKey;
  const value: TValue);
begin
  SetValue(key, value);
end;

procedure TBidiDictionary<TKey, TValue>.SetKey(const value: TValue;
  const key: TKey);
var
  oldKey: TKey;
begin
  if fValuesByKey.ContainsKey(key) then
    raise EInvalidOperationException.Create('key');
  if fKeysByValue.TryGetValue(value, oldKey) then
    fValuesByKey.Remove(oldKey);
  fValuesByKey.Add(key, value);
  fKeysByValue[value] := key;
end;

procedure TBidiDictionary<TKey, TValue>.SetValue(const key: TKey;
  const value: TValue);
var
  oldValue: TValue;
begin
  if fKeysByValue.ContainsKey(value) then
    raise EInvalidOperationException.Create('value');
  if fValuesByKey.TryGetValue(key, oldValue) then
    fKeysByValue.Remove(oldValue);
  fKeysByValue.Add(value, key);
  fValuesByKey[key] := value;
end;

function TBidiDictionary<TKey, TValue>.TryGetKey(const value: TValue;
  out key: TKey): Boolean;
begin
  Result := fKeysByValue.TryGetValue(value, key);
end;

function TBidiDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
begin
  Result := fValuesByKey.TryGetValue(key, value);
end;

{$ENDREGION}


{$REGION 'TOrderedDictionary<TKey, TValue>'}

constructor TOrderedDictionary<TKey, TValue>.Create;
begin
  inherited Create;
  fKeys := TKeyList<TKey>.Create(nil);
end;

constructor TOrderedDictionary<TKey, TValue>.Create(
  const comparer: IEqualityComparer<TKey>);
begin
  inherited Create(comparer);
  fKeys := TKeyList<TKey>.Create(comparer);
end;

procedure TOrderedDictionary<TKey, TValue>.Clear;
begin
  fKeys.Clear;
  inherited Clear;
end;

function TOrderedDictionary<TKey, TValue>.CreateKeyCollection: TKeyCollectionBase;
begin
  Result := TKeyCollection.Create(Self);
end;

function TOrderedDictionary<TKey, TValue>.CreateValueCollection: TValueCollectionBase;
begin
  Result := TValueCollection.Create(Self);
end;

function TOrderedDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>;
begin
  Result := TEnumerator.Create(Self);
end;

function TOrderedDictionary<TKey, TValue>.GetItem(index: Integer): TKeyValuePair;
var
  key: TKey;
  value: TValue;
begin
  key := fKeys[index];
  value := Self[key];
  Result.Key := key;
  Result.Value := value;
end;

function TOrderedDictionary<TKey, TValue>.IndexOf(const key: TKey): Integer;
begin
  Result := fKeys.IndexOf(key);
end;

procedure TOrderedDictionary<TKey, TValue>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  inherited KeyChanged(item, action);
  case action of
    caAdded: fKeys.Add(item);
    caRemoved, caExtracted: fKeys.Remove(item);
  end;
end;

function TOrderedDictionary<TKey, TValue>.ToArray: TArray<TKeyValuePair>;
var
  i: Integer;
  key: TKey;
  value: TValue;
begin
  SetLength(Result, Count);
  for i := 0 to Length(Result) - 1 do
  begin
    key := fKeys[i];
    value := Self[key];
    Result[i].Key := key;
    Result[i].Value := value;
  end;
end;

{$ENDREGION}


{$REGION 'TOrderedDictionary<TKey, TValue>.TEnumerator'}

constructor TOrderedDictionary<TKey, TValue>.TEnumerator.Create(
  const source: TOrderedDictionary<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fIndex := -1;
end;

destructor TOrderedDictionary<TKey, TValue>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TOrderedDictionary<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
var
  key: TKey;
  value: TValue;
begin
  key := fSource.fKeys[fIndex];
  value := fSource[key];
  Result.Key := key;
  Result.Value := value;
end;

function TOrderedDictionary<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
   if fIndex >= fSource.Count then
    Exit(False);
  Inc(fIndex);
  Result := fIndex < fSource.Count;
end;

{$ENDREGION}


{$REGION 'TOrderedDictionary<TKey, TValue>.TKeyCollection'}

constructor TOrderedDictionary<TKey, TValue>.TKeyCollection.Create(
  const source: TOrderedDictionary<TKey, TValue>);
begin
  inherited Create(source);
  fSource := source;
end;

function TOrderedDictionary<TKey, TValue>.TKeyCollection.Contains(
  const value: TKey; const comparer: IEqualityComparer<TKey>): Boolean;
begin
  Result := fSource.ContainsKey(value);
end;

function TOrderedDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := fSource.Count;
end;

function TOrderedDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TKeyEnumerator.Create(fSource);
end;

function TOrderedDictionary<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
var
  i: Integer;
begin
  SetLength(Result, fSource.Count);
  for i := 0 to Length(Result) - 1 do
    Result[i] := fSource.fKeys[i];
end;

{$ENDREGION}


{$REGION 'TOrderedDictionary<TKey, TValue>.TKeyEnumerator'}

constructor TOrderedDictionary<TKey, TValue>.TKeyEnumerator.Create(
  const source: TOrderedDictionary<TKey,TValue>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fIndex := -1;
end;

destructor TOrderedDictionary<TKey, TValue>.TKeyEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TOrderedDictionary<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result := fSource.fKeys[fIndex];
end;

function TOrderedDictionary<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  if fIndex >= fSource.Count then
    Exit(False);
  Inc(fIndex);
  Result := fIndex < fSource.Count;
end;

{$ENDREGION}


{$REGION 'TOrderedDictionary<TKey, TValue>.TValueCollection'}

constructor TOrderedDictionary<TKey, TValue>.TValueCollection.Create(
  const source: TOrderedDictionary<TKey, TValue>);
begin
  inherited Create(source);
  fSource := source;
end;

function TOrderedDictionary<TKey, TValue>.TValueCollection.Contains(
  const value: TValue; const comparer: IEqualityComparer<TValue>): Boolean;
begin
  Result := fSource.ContainsValue(value);
end;

function TOrderedDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fSource.Count;
end;

function TOrderedDictionary<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(fSource);
end;

function TOrderedDictionary<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  i: Integer;
begin
  SetLength(Result, fSource.Count);
  for i := 0 to Length(Result) - 1 do
    Result[i] := fSource[fSource.fKeys[i]];
end;

{$ENDREGION}


{$REGION 'TOrderedDictionary<TKey, TValue>.TValueEnumerator'}

constructor TOrderedDictionary<TKey, TValue>.TValueEnumerator.Create(
  const source: TOrderedDictionary<TKey,TValue>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fIndex := -1;
end;

destructor TOrderedDictionary<TKey, TValue>.TValueEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TOrderedDictionary<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := fSource[fSource.fKeys[fIndex]];
end;

function TOrderedDictionary<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  if fIndex >= fSource.Count then
    Exit(False);
  Inc(fIndex);
  Result := fIndex < fSource.Count;
end;

{$ENDREGION}


{$REGION 'TSortedDictionary<TKey, TValue>'}

{$IFNDEF DELPHI2010}
constructor TSortedDictionary<TKey, TValue>.Create;
begin
  Create(nil, nil);
end;

constructor TSortedDictionary<TKey, TValue>.Create(
  const keyComparer: IComparer<TKey>; const valueComparer: IComparer<TValue>);
begin
  inherited Create;

  fKeyComparer := keyComparer;
  if not Assigned(fkeyComparer) then
    fKeyComparer := TComparer<TKey>.Default;
  fValueComparer := valueComparer;
  if not Assigned(fValueComparer) then
    fValueComparer := TComparer<TValue>.Default;
  fTree := TRedBlackTree<TKey,TValue>.Create(keyComparer);

{$IFDEF DELPHI2010}
  fKeys := Spring.Collections.Lists.TList<TKey>.Create;
  fValues := Spring.Collections.Lists.TList<TValue>.Create;
{$ELSE}
  fKeys := TCollections.CreateList<TKey>;
  fValues := TCollections.CreateList<TValue>;
{$ENDIF}
end;

destructor TSortedDictionary<TKey, TValue>.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSortedDictionary<TKey, TValue>.Add(const key: TKey; const value: TValue);
begin
  if not fTree.Add(key, value) then
    raise EListError.CreateRes(@SGenericDuplicateItem);
  KeyChanged(key, caAdded);
  ValueChanged(value, caAdded);
end;

procedure TSortedDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  if Assigned(node) then
  begin
    ValueChanged(node.Value, caRemoved);
    node.Value := value;
    ValueChanged(value, caAdded);
  end
  else
  begin
    fTree.Add(key, value);
    KeyChanged(key, caAdded);
    ValueChanged(value, caAdded);
  end;
end;

function TSortedDictionary<TKey, TValue>.AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

procedure TSortedDictionary<TKey, TValue>.Clear;
var
  node: PBinaryTreeNode;
begin
  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    KeyChanged(PNode(node).Key, caRemoved);
    ValueChanged(PNode(node).Value, caRemoved);
    node := node.Next;
  end;

  fTree.Clear;
end;

function TSortedDictionary<TKey, TValue>.Contains(const value: TKeyValue;
  const comparer: IEqualityComparer<TKeyValue>): Boolean;
var
  found: TValue;
begin
  Result := fTree.Find(value.Key, found)
    and comparer.Equals(value, TKeyValue.Create(value.Key, found));
end;

function TSortedDictionary<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  found: TValue;
begin
  Result := fTree.Find(key, found)
    and (fValueComparer.Compare(value, found) = EqualsValue);
end;

function TSortedDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fTree.Exists(key);
end;

function TSortedDictionary<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  found: TKeyValue;
begin
  for found in fTree do
    if fValueComparer.Compare(value, found.Value) = EqualsValue then
      Exit(True);
  Result := False;
end;

function TSortedDictionary<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValue;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  if Assigned(node)
    and (fValueComparer.Compare(value, node.Value) = EqualsValue) then
  begin
    Result.Key := node.Key;
    Result.Value := node.Value;
    fTree.DeleteNode(node);
    KeyChanged(Result.Key, caExtracted);
    ValueChanged(Result.Value, caExtracted);
  end
  else
    Result := Default(TKeyValue);
end;

function TSortedDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  if Assigned(node) then
  begin
    Result := node.Value;
    fTree.DeleteNode(node);
    KeyChanged(key, caExtracted);
    ValueChanged(Result, caExtracted);
  end
  else
    Result := Default(TValue);
end;

function TSortedDictionary<TKey, TValue>.ExtractPair(const key: TKey): TKeyValue;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  if Assigned(node) then
  begin
    Result.Key := node.Key;
    Result.Value := node.Value;
    fTree.DeleteNode(node);
    KeyChanged(Result.Key, caExtracted);
    ValueChanged(Result.Value, caExtracted);
  end
  else
    Result := Default(TKeyValue);
end;

function TSortedDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fTree.Count;
end;

function TSortedDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValue>;
begin
  Result := fTree.GetEnumerator;
end;

function TSortedDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
begin
  if not TryGetValue(key, Result) then
    Result := Default(TValue);
end;

function TSortedDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
var
  item: TKeyValue;
begin
  // TODO: implement this properly to always provide up to date information
  fKeys.Clear;
  for item in Self do
    fKeys.Add(item.Key);
  Result := fKeys.AsReadOnlyList;
end;

function TSortedDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey): TValue;
begin
  if not fTree.Find(key, Result) then
    Result := Default(TValue);
end;

function TSortedDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey;
  const defaultValue: TValue): TValue;
begin
  if not fTree.Find(key, Result) then
    Result := defaultValue;
end;

function TSortedDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
var
  item: TKeyValue;
begin
  // TODO: implement this properly to always provide up to date information
  fValues.Clear;
  for item in Self do
    fValues.Add(item.Value);
  Result := fValues.AsReadOnlyList;
end;

function TSortedDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  Result := Assigned(node);
  if Result then
  begin
    KeyChanged(node.Key, caRemoved);
    ValueChanged(node.Value, caRemoved);
    fTree.DeleteNode(node);
  end;
end;

function TSortedDictionary<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  Result := Assigned(node)
    and (fValueComparer.Compare(value, node.Value) = EqualsValue);
  if Result then
  begin
    KeyChanged(node.Key, caRemoved);
    ValueChanged(node.Value, caRemoved);
    fTree.DeleteNode(node);
  end;
end;

procedure TSortedDictionary<TKey, TValue>.SetItem(const key: TKey; const value: TValue);
begin
  AddOrSetValue(key, value);
end;

function TSortedDictionary<TKey, TValue>.Ordered: IEnumerable<TKeyValue>;
begin
  Result := TOrderedIterator<TKeyValue>.Create(Self, fKeyValueComparerByKey);
end;

function TSortedDictionary<TKey, TValue>.ToArray: TArray<TKeyValue>;
var
  i: Integer;
  item: TKeyValue;
begin
  // TODO: consider adding ToArray to IBinaryTree
  SetLength(Result, Count);
  i := 0;
  for item in Self do
  begin
    Result[i] := item;
    Inc(i);
  end;
end;

function TSortedDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
begin
  Result := fTree.Find(key, value);
end;
{$ENDIF}

{$ENDREGION}


end.
