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

unit Spring.Collections.MultiMaps;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Dictionaries;

type
  TMultiMapBase<TKey, TValue> = class abstract(TMapBase<TKey, TValue>,
    IMultiMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>)
  private
    type
      TGenericPair = Generics.Collections.TPair<TKey, TValue>;

      TEnumerator = class(TEnumeratorBase<TGenericPair>)
      private
        fSource: TMultiMapBase<TKey, TValue>;
        fDictionaryEnumerator: IEnumerator<TPair<TKey, IList<TValue>>>;
        fCollectionEnumerator: IEnumerator<TValue>;
      protected
        function GetCurrent: TGenericPair; override;
      public
        constructor Create(const source: TMultiMapBase<TKey, TValue>);
        function MoveNext: Boolean; override;
      end;

      TValueEnumerator = class(TEnumeratorBase<TValue>)
      private
        fSource: TMultiMapBase<TKey, TValue>;
        fSourceEnumerator: IEnumerator<TGenericPair>;
      protected
        function GetCurrent: TValue; override;
      public
        constructor Create(const source: TMultiMapBase<TKey, TValue>);
        function MoveNext: Boolean; override;
      end;

      TValueCollection = class(TContainedReadOnlyCollection<TValue>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fOwner: TMultiMapBase<TKey, TValue>;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const owner: TMultiMapBase<TKey, TValue>);

      {$REGION 'Implements IEnumerable<TValue>'}
        function GetEnumerator: IEnumerator<TValue>; override;
        function Contains(const value: TValue;
          const comparer: IEqualityComparer<TValue>): Boolean; override;
        function ToArray: TArray<TValue>; override;
      {$ENDREGION}
      end;
  private
    fDictionary: TDictionary<TKey, IList<TValue>>;
    fCount: Integer;
    fEmpty: IList<TValue>;
    fValues: TValueCollection;
    function AsReadOnlyMultiMap: IReadOnlyMultiMap<TKey,TValue>;
  protected
    fValueComparer: IComparer<TValue>;
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItems(const key: TKey): IReadOnlyList<TValue>;
    function GetKeys: IReadOnlyCollection<TKey>; override;
    function GetValues: IReadOnlyCollection<TValue>; override;
  {$ENDREGION}
    function CreateCollection(const comparer: IComparer<TValue>): IList<TValue>; virtual; abstract;
    function CreateDictionary(const comparer: IEqualityComparer<TKey>): TDictionary<TKey, IList<TValue>>; virtual; abstract;
  public
    constructor Create; override;
    constructor Create(const keyComparer: IEqualityComparer<TKey>); overload;
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IComparer<TValue>); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TGenericPair>; override;
    function Contains(const value: TGenericPair;
      const comparer: IEqualityComparer<TGenericPair>): Boolean; override;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear; override;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    procedure Add(const key: TKey; const value: TValue); override;
    function Remove(const key: TKey): Boolean; reintroduce; overload;
    function Remove(const key: TKey; const value: TValue): Boolean; overload; override;
    function Extract(const key: TKey; const value: TValue): TGenericPair; override;
    function Contains(const key: TKey; const value: TValue): Boolean; override;
    function ContainsKey(const key: TKey): Boolean; override;
    function ContainsValue(const value: TValue): Boolean; override;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IMultiMap<TKey, TValue>'}
    procedure AddRange(const key: TKey; const values: array of TValue); overload;
    procedure AddRange(const key: TKey; const collection: IEnumerable<TValue>); overload;
    function ExtractValues(const key: TKey): IList<TValue>;
    function TryGetValues(const key: TKey; out values: IReadOnlyList<TValue>): Boolean;
    property Items[const key: TKey]: IReadOnlyList<TValue> read GetItems; default;
  {$ENDREGION}
  end;

  TMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>)
  private
    procedure DoKeyChanged(Sender: TObject; const Item: TKey;
      Action: TCollectionChangedAction);
    procedure DoValueChanged(Sender: TObject; const Item: TValue;
      Action: TCollectionChangedAction);
    procedure DoValuesChanged(Sender: TObject; const Item: IList<TValue>;
      Action: TCollectionChangedAction);
  protected
    function CreateCollection(const comparer: IComparer<TValue>): IList<TValue>; override;
    function CreateDictionary(const comparer: IEqualityComparer<TKey>): TDictionary<TKey, IList<TValue>>; override;
  end;

  TObjectMultiMap<TKey, TValue> = class(TMultiMap<TKey, TValue>)
  private
    fOwnerships: TDictionaryOwnerships;
  protected
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction); override;
    procedure ValueChanged(const item: TValue; action: TCollectionChangedAction); override;
  public
    constructor Create(ownerships: TDictionaryOwnerships); overload;
    constructor Create(ownerships: TDictionaryOwnerships;
      const keyComparer: IEqualityComparer<TKey>); overload;
    constructor Create(ownerships: TDictionaryOwnerships;
      const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IComparer<TValue>); overload;
  end;

implementation

uses
  Classes,
  RTLConsts,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Collections.Lists,
  Spring.ResourceStrings;


{$REGION 'TMultiMapBase<TKey, TValue>'}

constructor TMultiMapBase<TKey, TValue>.Create;
begin
  Create(nil, nil);
end;

constructor TMultiMapBase<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>);
begin
  Create(keyComparer, nil);
end;

constructor TMultiMapBase<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IComparer<TValue>);
begin
  fValueComparer := valueComparer;
  inherited Create;
  fDictionary := CreateDictionary(keyComparer);
{$IFNDEF DELPHI2010}
  fEmpty := TCollections.CreateList<TValue>;
{$ELSE}
  fEmpty := TList<TValue>.Create;
{$ENDIF}
  fValues := TValueCollection.Create(Self);
end;

destructor TMultiMapBase<TKey, TValue>.Destroy;
begin
  fValues.Free;
  fDictionary.Free;
  inherited Destroy;
end;

procedure TMultiMapBase<TKey, TValue>.Add(const key: TKey; const value: TValue);
var
  list: IList<TValue>;
begin
  if not fDictionary.TryGetValue(key, list) then
  begin
    list := CreateCollection(fValueComparer);
    fDictionary[key] := list;
  end;

  list.Add(value);
  Inc(fCount);
end;

procedure TMultiMapBase<TKey, TValue>.AddRange(const key: TKey;
  const values: array of TValue);
var
  i: Integer;
begin
  for i := Low(values) to High(values) do
    Add(key, values[i]);
end;

procedure TMultiMapBase<TKey, TValue>.AddRange(const key: TKey;
  const collection: IEnumerable<TValue>);
var
  item: TValue;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(collection), 'collection');
{$ENDIF}

  for item in collection do
    Add(key, item);
end;

function TMultiMapBase<TKey, TValue>.AsReadOnlyMultiMap: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

procedure TMultiMapBase<TKey, TValue>.Clear;
begin
  fDictionary.Clear;
  fCount := 0;
end;

function TMultiMapBase<TKey, TValue>.Contains(const value: TGenericPair;
  const comparer: IEqualityComparer<TGenericPair>): Boolean;
var
  list: IList<TValue>;
begin
  Result := fDictionary.TryGetValue(value.key, list)
    and list.Contains(value.Value);
end;

function TMultiMapBase<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  values: IReadOnlyList<TValue>;
begin
  Result := TryGetValues(key, values) and values.Contains(value);
end;

function TMultiMapBase<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(key);
end;

function TMultiMapBase<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  list: ICollection<TValue>;
begin
  for list in fDictionary.Values do
    if list.Contains(value) then
      Exit(True);
  Result := False;
end;

function TMultiMapBase<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TGenericPair;
var
  list: IList<TValue>;
begin
  if fDictionary.TryGetValue(key, list) then
  begin
    Result.Key := key;
    Result.Value := list.Extract(value);
  end
  else
    Result := Default(TGenericPair);
end;

function TMultiMapBase<TKey, TValue>.ExtractValues(
  const key: TKey): IList<TValue>;
begin
  if not fDictionary.TryGetValue(key, Result) then
    raise EListError.CreateRes(@SGenericItemNotFound);

  Dec(fCount, Result.Count);
  fDictionary.Extract(key);
end;

function TMultiMapBase<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

function TMultiMapBase<TKey, TValue>.GetEnumerator: IEnumerator<TGenericPair>;
begin
  Result := TEnumerator.Create(Self);
end;

function TMultiMapBase<TKey, TValue>.GetItems(
  const key: TKey): IReadOnlyList<TValue>;
var
  list: IList<TValue>;
begin
  if not fDictionary.TryGetValue(key, list) then
    list := fEmpty;
  Result := list as IReadOnlyList<TValue>;
end;

function TMultiMapBase<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fDictionary.Keys;
end;

function TMultiMapBase<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  list: IList<TValue>;
begin
  Result := fDictionary.TryGetValue(key, list) and list.Remove(value);
  if Result then
  begin
    Dec(fCount);
    if not list.Any then
      fDictionary.Remove(key);
  end;
end;

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  list: IList<TValue>;
begin
  Result := fDictionary.TryGetValue(key, list);
  if Result then
  begin
    Dec(fCount, list.Count);
    fDictionary.Remove(key);
  end;
end;

function TMultiMapBase<TKey, TValue>.TryGetValues(const key: TKey;
  out values: IReadOnlyList<TValue>): Boolean;
var
  list: IList<TValue>;
begin
  Result := fDictionary.TryGetValue(key, list);
  if Result then
    values := list as IReadOnlyList<TValue>;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TEnumerator'}

constructor TMultiMapBase<TKey, TValue>.TEnumerator.Create(
  const source: TMultiMapBase<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
end;

function TMultiMapBase<TKey, TValue>.TEnumerator.GetCurrent: TGenericPair;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(fDictionaryEnumerator, 'dictionaryEnumerator');
  Guard.CheckNotNull(fCollectionEnumerator, 'collectionEnumerator');
{$ENDIF}

  Result.Key := fDictionaryEnumerator.Current.Key;
  Result.Value := fCollectionEnumerator.Current;
end;

function TMultiMapBase<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  if not Assigned(fDictionaryEnumerator) then
    fDictionaryEnumerator := fSource.fDictionary.GetEnumerator;

  repeat
    if Assigned(fCollectionEnumerator) and fCollectionEnumerator.MoveNext then
      Exit(True)
    else
    begin
      Result := fDictionaryEnumerator.MoveNext;
      if Result then
        fCollectionEnumerator := fDictionaryEnumerator.Current.Value.GetEnumerator;
    end;
  until not Result;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TValueEnumerator'}

constructor TMultiMapBase<TKey, TValue>.TValueEnumerator.Create(
  const source: TMultiMapBase<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
end;

function TMultiMapBase<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := fSourceEnumerator.Current.Value;
end;

function TMultiMapBase<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  if not Assigned(fSourceEnumerator) then
    fSourceEnumerator := fSource.GetEnumerator;
  Result := fSourceEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TValueCollection'}

constructor TMultiMapBase<TKey, TValue>.TValueCollection.Create(
  const owner: TMultiMapBase<TKey, TValue>);
begin
  inherited Create(owner);
  fOwner := owner;
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.Contains(const value: TValue;
  const comparer: IEqualityComparer<TValue>): Boolean;
begin
  Result := fOwner.ContainsValue(value);
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fOwner.Count;
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(fOwner);
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  list: ICollection<TValue>;
  i: Integer;
begin
  SetLength(Result, fOwner.Count);
  i := 0;
  for list in fOwner.fDictionary.Values do
  begin
    list.CopyTo(Result, i);
    Inc(i, list.Count);
  end;
end;

{$ENDREGION}


{$REGION 'TMultiMap<TKey, TValue>'}

function TMultiMap<TKey, TValue>.CreateCollection(
  const comparer: IComparer<TValue>): IList<TValue>;
begin
{$IFNDEF DELPHI2010}
  Result := TCollections.CreateList<TValue>(comparer);
{$ELSE}
  Result := TList<TValue>.Create(comparer);
{$ENDIF}
end;

function TMultiMap<TKey, TValue>.CreateDictionary(
  const comparer: IEqualityComparer<TKey>): TDictionary<TKey, IList<TValue>>;
begin
  Result := TContainedDictionary<TKey, IList<TValue>>.Create(Self, comparer);
  Result.OnKeyChanged.Add(DoKeyChanged);
  Result.OnValueChanged.Add(DoValuesChanged);
end;

procedure TMultiMap<TKey, TValue>.DoKeyChanged(Sender: TObject;
  const Item: TKey; Action: TCollectionChangedAction);
begin
  KeyChanged(Item, Action);
end;

procedure TMultiMap<TKey, TValue>.DoValueChanged(Sender: TObject;
  const Item: TValue; Action: TCollectionChangedAction);
begin
  ValueChanged(Item, Action);
end;

procedure TMultiMap<TKey, TValue>.DoValuesChanged(Sender: TObject;
  const Item: IList<TValue>; Action: TCollectionChangedAction);
begin
  case Action of
    caAdded: Item.OnChanged.Add(DoValueChanged);
    caRemoved:
    begin
      Item.Clear;
      Item.OnChanged.Remove(DoValueChanged);
    end;
    caExtracted: Item.OnChanged.Remove(DoValueChanged);
  end;
end;

{$ENDREGION}


{$REGION 'TObjectMultiMap<TKey, TValue>'}

constructor TObjectMultiMap<TKey, TValue>.Create(
  ownerships: TDictionaryOwnerships);
begin
  Create(ownerships, nil, nil);
end;

constructor TObjectMultiMap<TKey, TValue>.Create(
  ownerships: TDictionaryOwnerships; const keyComparer: IEqualityComparer<TKey>);
begin
  Create(ownerships, keyComparer, nil);
end;

constructor TObjectMultiMap<TKey, TValue>.Create(
  ownerships: TDictionaryOwnerships; const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IComparer<TValue>);
begin
  if (doOwnsKeys in ownerships) and (KeyType.Kind <> tkClass) then
    raise ENotSupportedException.CreateResFmt(@SNotClassType, [KeyType.TypeName]);
  if (doOwnsValues in ownerships) and (ValueType.Kind <> tkClass) then
    raise ENotSupportedException.CreateResFmt(@SNotClassType, [ValueType.TypeName]);
  inherited Create(keyComparer, valueComparer);
  fOwnerships := ownerships;
end;

procedure TObjectMultiMap<TKey, TValue>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  inherited KeyChanged(item, action);
  if (action = caRemoved) and (doOwnsKeys in fOwnerships) then
{$IFNDEF AUTOREFCOUNT}
    PObject(@item).Free;
{$ELSE}
    PObject(@item).DisposeOf;
{$ENDIF}
end;

procedure TObjectMultiMap<TKey, TValue>.ValueChanged(const item: TValue;
  action: TCollectionChangedAction);
begin
  inherited ValueChanged(item, action);
  if (action = caRemoved) and (doOwnsValues in fOwnerships) then
{$IFNDEF AUTOREFCOUNT}
    PObject(@item).Free;
{$ELSE}
    PObject(@item).DisposeOf;
{$ENDIF}
end;

{$ENDREGION}


end.
