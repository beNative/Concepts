{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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

unit Spring.Collections.Lists;

interface

uses
  Classes,
  Generics.Defaults,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

type
  /// <summary>
  ///   Represents a strongly typed list of elements that can be accessed by
  ///   index. Provides methods to search, sort, and manipulate lists.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of elements in the list.
  /// </typeparam>
  TList<T> = class(TListBase<T>, IArrayAccess<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fList: TList<T>;
        fIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const list: TList<T>);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
        procedure Reset; override;
      end;
      TArrayManager = TArrayManager<T>;
  private
    fItems: TArray<T>;
    fCount: Integer;
    fVersion: Integer;
    procedure DeleteInternal(index: Integer; notification: TCollectionChangedAction);
    procedure DeleteAllInternal(const predicate: TPredicate<T>;
      notification: TCollectionChangedAction);
    procedure IncreaseVersion; inline;
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function GetItem(index: Integer): T; override;
    function GetItems: TArray<T>;
    procedure SetCapacity(value: Integer); override;
    procedure SetCount(value: Integer); override;
    procedure SetItem(index: Integer; const value: T); override;
  {$ENDREGION}

    procedure EnsureCapacity(capacity: Integer); inline;
    procedure Grow(capacity: Integer);
  public
    constructor Create(const values: array of T); override;
    constructor Create(const collection: IEnumerable<T>); override;

    function GetEnumerator: IEnumerator<T>; override;

    procedure Clear; override;

    function Contains(const value: T; const comparer: IEqualityComparer<T>): Boolean; override;
    function IndexOf(const item: T; index, count: Integer): Integer; override;

    procedure Insert(index: Integer; const item: T); override;
    procedure InsertRange(index: Integer; const values: array of T); override;
    procedure InsertRange(index: Integer; const collection: IEnumerable<T>); override;

    procedure Delete(index: Integer); override;
    procedure DeleteRange(index, count: Integer); override;

    procedure RemoveAll(const predicate: TPredicate<T>); override;

    function Extract(const item: T): T; override;
    procedure ExtractAll(const predicate: TPredicate<T>); override;
    function ExtractAt(index: Integer): T; override;

    function GetRange(index, count: Integer): IList<T>; override;

    procedure Exchange(index1, index2: Integer); override;
    procedure Move(currentIndex, newIndex: Integer); override;

    procedure Reverse(index, count: Integer); override;
    procedure Sort(const comparer: IComparer<T>; index, count: Integer); override;

    procedure CopyTo(var values: TArray<T>; index: Integer); override;
    function MoveTo(const collection: ICollection<T>;
      const predicate: TPredicate<T>): Integer; override;
    function ToArray: TArray<T>; override;
  end;

  TObjectList<T: class> = class(TList<T>, ICollectionOwnership)
  private
    fOwnsObjects: Boolean;
  {$REGION 'Property Accessors'}
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}
  protected
    procedure Changed(const item: T; action: TCollectionChangedAction); override;
  public
    constructor Create; override;
    constructor Create(ownsObjects: Boolean); overload;
    constructor Create(const comparer: IComparer<T>; ownsObjects: Boolean = True); overload;

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  TSortedList<T> = class(TList<T>)
  protected
    procedure SetItem(index: Integer; const value: T); override;
  public
    function Add(const item: T): Integer; override;
    procedure Insert(index: Integer; const item: T); override;

    procedure AddRange(const values: array of T); override;
    procedure AddRange(const collection: IEnumerable<T>); override;

    function Contains(const value: T): Boolean; override;
    function IndexOf(const item: T; index, count: Integer): Integer; override;
    function LastIndexOf(const item: T; index, count: Integer): Integer; override;

    procedure Exchange(index1, index2: Integer); override;
    procedure Move(currentIndex, newIndex: Integer); override;
  end;

  TSortedObjectList<T: class> = class(TSortedList<T>, ICollectionOwnership)
  private
    fOwnsObjects: Boolean;
  {$REGION 'Property Accessors'}
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}
  protected
    procedure Changed(const item: T; action: TCollectionChangedAction); override;
  public
    constructor Create; override;
    constructor Create(ownsObjects: Boolean); overload;
    constructor Create(const comparer: IComparer<T>; ownsObjects: Boolean = True); overload;

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  TCollectionList<T: TCollectionItem> = class(TListBase<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fList: TCollectionList<T>;
        fIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const list: TCollectionList<T>);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
        procedure Reset; override;
      end;
  private
    fCollection: TCollection;
    fVersion: Integer;
    procedure DeleteInternal(index: Integer; notification: TCollectionChangedAction);
    procedure IncreaseVersion; inline;
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetElementType: PTypeInfo; override;
    function GetItem(index: Integer): T; override;
    procedure SetCapacity(value: Integer); override;
    procedure SetItem(index: Integer; const value: T); override;
  {$ENDREGION}
  public
    constructor Create(const collection: TCollection);
    destructor Destroy; override;

    function GetEnumerator: IEnumerator<T>; override;

    procedure Insert(index: Integer; const item: T); override;

    procedure Delete(index: Integer); override;
    procedure DeleteRange(index, count: Integer); override;

    function Extract(const item: T): T; override;
    function ExtractAt(index: Integer): T; override;

    procedure Exchange(index1, index2: Integer); override;
    procedure Move(currentIndex, newIndex: Integer); override;
  end;

  TAnonymousReadOnlyList<T> = class(TEnumerableBase<T>, IReadOnlyList<T>)
  private
    fCount: TFunc<Integer>;
    fItems: TFunc<Integer, T>;
    fIterator: IEnumerable<T>;
  protected
  {$REGION 'Property Accessors'}
    function GetItem(index: Integer): T;
  {$ENDREGION}
  public
    constructor Create(const count: TFunc<Integer>;
      const items: TFunc<Integer, T>;
      const iterator: IEnumerable<T>{$IFDEF DELPHIXE3_UP} = nil{$ENDIF});

    function GetEnumerator: IEnumerator<T>; override;

    function GetRange(index, count: Integer): IList<T>;

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;
  end;

{$IFNDEF DELPHI2010}
  TFoldedObjectList<T{: class}> = class(TObjectList<TObject>)
  protected
    function GetElementType: PTypeInfo; override;
  end;

  TFoldedInterfaceList<T{: IInterface}> = class(TList<IInterface>)
  protected
    function GetElementType: PTypeInfo; override;
  end;

  TFoldedSortedObjectList<T{: class}> = class(TSortedObjectList<TObject>)
  protected
    function GetElementType: PTypeInfo; override;
  end;

  TFoldedSortedInterfaceList<T{: IInterface}> = class(TSortedList<IInterface>)
  protected
    function GetElementType: PTypeInfo; override;
  end;
{$ENDIF}

  TObservableList<T: class> = class(
    {$IFNDEF DELPHI2010}TFoldedObjectList<T>{$ELSE}TObjectList<T>{$ENDIF},
    INotifyPropertyChanged)
  private
    fOnPropertyChanged: IEvent<TPropertyChangedEvent>;
    function GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
  protected
    procedure DoItemPropertyChanged(sender: TObject;
      const eventArgs: IPropertyChangedEventArgs);
    procedure DoPropertyChanged(const propertyName: string);
    procedure Changed(const value: {$IFNDEF DELPHI2010}TObject{$ELSE}T{$ENDIF};
      action: TCollectionChangedAction); override;
  public
    constructor Create; override;

    property OnPropertyChanged: IEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

implementation

uses
{$IFDEF DELPHIXE4}
  Rtti, // suppress hint about inlining
{$ENDIF}
  TypInfo,
  Spring.Collections.Extensions,
  Spring.Events,
  Spring.ResourceStrings;


{$REGION 'TList<T>'}

constructor TList<T>.Create(const values: array of T);
var
  i: Integer;
begin
  Create;
  fCount := Length(values);
  if fCount > 0 then
  begin
    SetLength(fItems, fCount);
    for i := Low(values) to High(values) do
      fItems[i] := values[i];
  end;
end;

constructor TList<T>.Create(const collection: IEnumerable<T>);
var
  c: ICollection<T>;
begin
  if Supports(collection, ICollection<T>, c) then
  begin
    Create;
    fCount := c.Count;
    if fCount > 0 then
    begin
      SetLength(fItems, fCount);
      c.CopyTo(fItems, 0);
    end;
  end
  else
    inherited Create(collection);
end;

function TList<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TList<T>.GetEnumerator: IEnumerator<T>;
begin
{$IFNDEF DELPHI2010}
  if TType.Kind<T> = tkClass then
    IEnumerator<TObject>(Result) := TList<TObject>.TEnumerator.Create(TList<TObject>(Self))
  else
    Result := TEnumerator.Create(Self);
{$ELSE}
  Result := TEnumerator.Create(Self);
{$ENDIF}
end;

function TList<T>.GetIsEmpty: Boolean;
begin
  Result := fCount = 0;
end;

function TList<T>.GetItem(index: Integer): T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
{$ENDIF}

  Result := fItems[index];
end;

function TList<T>.GetItems: TArray<T>;
begin
  Result := fItems;
end;

function TList<T>.GetRange(index, count: Integer): IList<T>;
var
  list: TList<T>;
{$IFNDEF DELPHIXE2_UP}
  i: Integer;
{$ENDIF}
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCount - index), 'count');
{$ENDIF}

  list := TList<T>.Create;
  list.fCount := count;
{$IFDEF DELPHIXE2_UP}
  list.fItems := Copy(fItems, index, count);
{$ELSE}
  // the compiler passes wrong typeinfo for the generated call
  // to _DynArrayCopyRange up to XE
  SetLength(list.fItems, count);
  for i := 0 to count - 1 do
  begin
    list.fItems[i] := fItems[index];
    Inc(index);
  end;
{$ENDIF}
  Result := list;
end;

procedure TList<T>.Grow(capacity: Integer);
var
  newCapacity: Integer;
begin
  newCapacity := Length(fItems);
  if newCapacity = 0 then
    newCapacity := capacity
  else
    repeat
      newCapacity := newCapacity * 2;
      if newCapacity < 0 then
        OutOfMemoryError;
    until newCapacity >= capacity;
  SetCapacity(newCapacity);
end;

{$IFOPT Q+}{$DEFINE OVERFLOW_CHECKS_ON}{$Q-}{$ENDIF}
procedure TList<T>.IncreaseVersion;
begin
  Inc(fVersion);
end;
{$IFDEF OVERFLOW_CHECKS_ON}{$Q+}{$ENDIF}

function TList<T>.IndexOf(const item: T; index, count: Integer): Integer;
{$IFDEF DELPHI2010}
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fCount), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCount - index), 'count');
{$ENDIF}

  comparer := EqualityComparer;
  for i := index to index + count - 1 do
    if comparer.Equals(fItems[i], item) then
      Exit(i);
  Result := -1;
{$ELSE}
begin
  Result := TArray.IndexOf<T>(fItems, item, index, count, EqualityComparer);
{$ENDIF}
end;

procedure TList<T>.SetItem(index: Integer; const value: T);
var
  oldItem: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
{$ENDIF}

  oldItem := fItems[index];
  fItems[index] := value;
  IncreaseVersion;

  Changed(oldItem, caRemoved);
  Changed(value, caAdded);
end;

procedure TList<T>.Insert(index: Integer; const item: T);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fCount), 'index');
{$ENDIF}

  EnsureCapacity(fCount + 1);
  if index <> fCount then
  begin
    TArrayManager.Move(fItems, index, index + 1, fCount - index);
    TArrayManager.Finalize(fItems, index, 1);
  end;
  fItems[index] := item;
  Inc(fCount);
  IncreaseVersion;

  Changed(item, caAdded);
end;

procedure TList<T>.InsertRange(index: Integer; const values: array of T);
var
  count: Integer;
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fCount), 'index');
{$ENDIF}

  count := Length(values);
  if count = 0 then
    Exit;

  EnsureCapacity(fCount + count);
  if index <> fCount then
  begin
    TArrayManager.Move(fItems, index, index + count, fCount - index);
    TArrayManager.Finalize(fItems, index, count);
  end;

  if not TType.IsManaged<T>{$IFDEF WEAKREF} and not TType.HasWeakRef<T>{$ENDIF} then
    System.Move(values[0], fItems[index], count * SizeOf(T))
  else
    for i := Low(values) to High(values) do
      fItems[index + i] := values[i];

  Inc(fCount, count);
  IncreaseVersion;

  for i := Low(values) to High(values) do
    Changed(values[i], caAdded);
end;

procedure TList<T>.InsertRange(index: Integer;
  const collection: IEnumerable<T>);
var
  list: TList<T>;
  i: Integer;
begin
  if collection.AsObject is TList<T> then
  begin
{$IFDEF SPRING_ENABLE_GUARD}
    Guard.CheckRange((index >= 0) and (index <= fCount), 'index');
{$ENDIF}

    list := TList<T>(collection.AsObject);
    if list.fCount = 0 then
      Exit;

    EnsureCapacity(fCount + list.fCount);
    if index <> fCount then
    begin
      TArrayManager.Move(fItems, index, index + list.fCount, fCount - index);
      TArrayManager.Finalize(fItems, index, list.fCount);
    end;

    if not TType.IsManaged<T>{$IFDEF WEAKREF} and not TType.HasWeakRef<T>{$ENDIF} then
      System.Move(list.fItems[0], fItems[index], list.fCount * SizeOf(T))
    else
      for i := Low(list.fItems) to list.fCount - 1 do
        fItems[index + i] := list.fItems[i];

    Inc(fCount, list.fCount);
    IncreaseVersion;

    for i := 0 to list.fCount - 1 do
      Changed(list.fItems[i], caAdded);
  end
  else
    inherited InsertRange(index, collection);
end;

procedure TList<T>.DeleteInternal(index: Integer;
  notification: TCollectionChangedAction);
var
  oldItem: T;
begin
  oldItem := fItems[index];
  fItems[index] := Default(T);
  Dec(fCount);
  if index <> fCount then
  begin
    TArrayManager.Move(fItems, index + 1, index, fCount - index);
    TArrayManager.Finalize(fItems, fCount, 1);
  end;
  IncreaseVersion;

  Changed(oldItem, notification);
end;

procedure TList<T>.DeleteRange(index, count: Integer);
var
  oldItems: TArray<T>;
  tailCount,
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCount - index), 'count');
{$ENDIF}

  if count = 0 then
    Exit;

  SetLength(oldItems, count);
  TArrayManager.Move(fItems, oldItems, index, 0, count);

  tailCount := fCount - (index + count);
  if tailCount > 0 then
  begin
    TArrayManager.Move(fItems, index + count, index, tailCount);
    TArrayManager.Finalize(fItems, fCount - count, count);
  end
  else
    TArrayManager.Finalize(fItems, index, count);

  Dec(fCount, count);
  IncreaseVersion;

  for i := Low(oldItems) to High(oldItems) do
    Changed(oldItems[i], caRemoved);
end;

procedure TList<T>.Sort(const comparer: IComparer<T>; index, count: Integer);
begin
  TArray.Sort<T>(fItems, comparer, index, count);
  IncreaseVersion;

  Changed(Default(T), caReseted);
end;

procedure TList<T>.Move(currentIndex, newIndex: Integer);
var
  temp: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((currentIndex >= 0) and (currentIndex < fCount), 'currentIndex');
  Guard.CheckRange((newIndex >= 0) and (newIndex < fCount), 'newIndex');
{$ENDIF}

  temp := fItems[currentIndex];
  fItems[currentIndex] := Default(T);
  if currentIndex < newIndex then
    TArrayManager.Move(fItems, currentIndex + 1, currentIndex, newIndex - currentIndex)
  else
    TArrayManager.Move(fItems, newIndex, newIndex + 1, currentIndex - newIndex);

  TArrayManager.Finalize(fItems, newIndex, 1);
  fItems[newIndex] := temp;
  IncreaseVersion;

  Changed(temp, caMoved);
end;

function TList<T>.MoveTo(const collection: ICollection<T>;
  const predicate: TPredicate<T>): Integer;
var
  i: Integer;
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(collection), 'collection');
{$ENDIF}

  Result := 0;
  i := 0;
  while i < fCount do
    if not Assigned(predicate) or predicate(fItems[i]) then
    begin
      item := fItems[i];
      DeleteInternal(i, caExtracted);
      collection.Add(item);
      Inc(Result);
    end
    else
      Inc(i);
end;

procedure TList<T>.Clear;
begin
  inherited Clear;
  Capacity := 0;
end;

procedure TList<T>.EnsureCapacity(capacity: Integer);
begin
  if capacity > Length(fItems) then
    Grow(capacity)
  else if capacity < 0 then
    OutOfMemoryError;
end;

procedure TList<T>.Exchange(index1, index2: Integer);
var
  temp: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index1 >= 0) and (index1 < fCount), 'index1');
  Guard.CheckRange((index2 >= 0) and (index2 < fCount), 'index2');
{$ENDIF}

  temp := fItems[index1];
  fItems[index1] := fItems[index2];
  fItems[index2] := temp;
  IncreaseVersion;

  Changed(fItems[index2], caMoved);
  Changed(fItems[index1], caMoved);
end;

function TList<T>.GetCapacity: Integer;
begin
  Result := Length(fItems);
end;

procedure TList<T>.RemoveAll(const predicate: TPredicate<T>);
begin
  DeleteAllInternal(predicate, caRemoved);
end;

procedure TList<T>.Reverse(index, count: Integer);
var
  temp: T;
  index1, index2: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(index >= 0, 'index');
  Guard.CheckRange((count >= 0) and (count <= fCount - index), 'count');
{$ENDIF}

  index1 := index;
  index2 := index + count - 1;
  while index1 < index2 do
  begin
    temp := fItems[index1];
    fItems[index1] := fItems[index2];
    fItems[index2] := temp;
    Inc(index1);
    Dec(index2);
  end;
  IncreaseVersion;

  Changed(Default(T), caReseted);
end;

procedure TList<T>.SetCapacity(value: Integer);
begin
  if value < fCount then
    DeleteRange(value, fCount - value);
  SetLength(fItems, value);
end;

procedure TList<T>.SetCount(value: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(count >= 0, 'count');
{$ENDIF}

  if value > Capacity then
    SetCapacity(value);
  if value < fCount then
    DeleteRange(value, fCount - value);
  fCount := value;
end;

procedure TList<T>.Delete(index: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
{$ENDIF}

  DeleteInternal(index, caRemoved);
end;

procedure TList<T>.DeleteAllInternal(const predicate: TPredicate<T>;
  notification: TCollectionChangedAction);
var
  index: Integer;
begin
  index := 0;
  while index < fCount do
    if predicate(fItems[index]) then
      DeleteInternal(index, notification)
    else
      Inc(index);
end;

function TList<T>.Extract(const item: T): T;
var
  index: Integer;
begin
  index := IndexOf(item);
  if index < 0 then
    Result := Default(T)
  else
  begin
    Result := fItems[index];
    DeleteInternal(index, caExtracted);
  end;
end;

function TList<T>.ExtractAt(index: Integer): T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
{$ENDIF}

  Result := fItems[index];
  DeleteInternal(index, caExtracted);
end;

procedure TList<T>.ExtractAll(const predicate: TPredicate<T>);
begin
  DeleteAllInternal(predicate, caExtracted);
end;

function TList<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
    if comparer.Equals(value, fItems[i]) then
      Exit(True);
  Result := False;
end;

procedure TList<T>.CopyTo(var values: TArray<T>; index: Integer);
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(Length(values), index, fCount);
{$ENDIF}

  for i := 0 to fCount - 1 do
  begin
    values[index] := fItems[i];
    Inc(index);
  end;
end;

function TList<T>.ToArray: TArray<T>;
begin
  Result := fItems;
  SetLength(Result, fCount);
end;

{$ENDREGION}


{$REGION 'TList<T>.TEnumerator'}

constructor TList<T>.TEnumerator.Create(const list: TList<T>);
begin
  inherited Create;
  fList := list;
  fList._AddRef;
  fVersion := fList.fVersion;
end;

destructor TList<T>.TEnumerator.Destroy;
begin
  fList._Release;
  inherited Destroy;
end;

function TList<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if fVersion <> fList.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  if fIndex < fList.fCount then
  begin
    fCurrent := fList.fItems[fIndex];
    Inc(fIndex);
    Result := True;
  end
  else
    fCurrent := Default(T);
end;

function TList<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

procedure TList<T>.TEnumerator.Reset;
begin
  if fVersion <> fList.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  fIndex := 0;
  fCurrent := Default(T);
end;

{$ENDREGION}


{$REGION 'TObjectList<T>'}

constructor TObjectList<T>.Create;
begin
  inherited Create;
  fOwnsObjects := True;
end;

constructor TObjectList<T>.Create(ownsObjects: Boolean);
begin
  Create;
  fOwnsObjects := ownsObjects;
end;

constructor TObjectList<T>.Create(const comparer: IComparer<T>;
  ownsObjects: Boolean);
begin
  inherited Create(comparer);
  fOwnsObjects := ownsObjects;
end;

procedure TObjectList<T>.Changed(const item: T; action: TCollectionChangedAction);
begin
  inherited Changed(item, action);
  if OwnsObjects and (action = caRemoved) then
{$IFNDEF AUTOREFCOUNT}
    item.Free;
{$ELSE}
    item.DisposeOf;
{$ENDIF}
end;

function TObjectList<T>.GetOwnsObjects: Boolean;
begin
  Result := fOwnsObjects;
end;

procedure TObjectList<T>.SetOwnsObjects(const value: Boolean);
begin
  fOwnsObjects := value;
end;

{$ENDREGION}


{$REGION 'TSortedList<T>'}

function TSortedList<T>.Add(const item: T): Integer;
begin
  Result := fCount;
  if Result > 0 then
  begin
    // If the new item is smaller than the last one in the list ...
    if fComparer.Compare(item, fItems[Result - 1]) < 0 then
      // ... search for the correct insertion point
      TArray.BinarySearch<T>(fItems, item, Result, fComparer, 0, fCount);
  end;
  inherited Insert(Result, item);
end;

procedure TSortedList<T>.AddRange(const collection: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(collection), 'collection');
{$ENDIF}

  for item in collection do
    Add(item);
end;

procedure TSortedList<T>.AddRange(const values: array of T);
var
  i: Integer;
begin
  for i := Low(values) to High(values) do
    Add(values[i]);
end;

function TSortedList<T>.Contains(const value: T): Boolean;
var
  index: Integer;
begin
  Result := TArray.BinarySearch<T>(fItems, value, index, fComparer, 0, fCount);
end;

procedure TSortedList<T>.Exchange(index1, index2: Integer);
begin
  raise EInvalidOperationException.Create('Exchange');
end;

function TSortedList<T>.IndexOf(const item: T; index, count: Integer): Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fCount), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCount - index), 'count');
{$ENDIF}

  if not TArray.BinarySearch<T>(fItems, item, Result, fComparer, index, count) then
    Result := -1;
end;

procedure TSortedList<T>.Insert(index: Integer; const item: T);
begin
  raise EInvalidOperationException.Create('Insert');
end;

function TSortedList<T>.LastIndexOf(const item: T; index,
  count: Integer): Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= index + 1), 'count');
{$ENDIF}

  if not TArray.BinarySearchUpperBound<T>(
    fItems, item, Result, fComparer, index, count) then
    Result := -1;
end;

procedure TSortedList<T>.Move(currentIndex, newIndex: Integer);
begin
  raise EInvalidOperationException.Create('Move');
end;

procedure TSortedList<T>.SetItem(index: Integer; const value: T);
begin
  raise EInvalidOperationException.Create('SetItem');
end;

{$ENDREGION}


{$REGION 'TSortedObjectList<T>'}

constructor TSortedObjectList<T>.Create;
begin
  Create(True);
end;

constructor TSortedObjectList<T>.Create(ownsObjects: Boolean);
begin
  inherited Create;
  fOwnsObjects := ownsObjects;
end;

constructor TSortedObjectList<T>.Create(const comparer: IComparer<T>;
  ownsObjects: Boolean);
begin
  inherited Create(comparer);
  fOwnsObjects := ownsObjects;
end;

procedure TSortedObjectList<T>.Changed(const item: T;
  action: TCollectionChangedAction);
begin
  inherited Changed(item, action);
  if OwnsObjects and (action = caRemoved) then
{$IFNDEF AUTOREFCOUNT}
    item.Free;
{$ELSE}
    item.DisposeOf;
{$ENDIF}
end;

function TSortedObjectList<T>.GetOwnsObjects: Boolean;
begin
  Result := fOwnsObjects;
end;

procedure TSortedObjectList<T>.SetOwnsObjects(const value: Boolean);
begin
  fOwnsObjects := value;
end;

{$ENDREGION}


{$REGION 'TCollectionList<T>' }

constructor TCollectionList<T>.Create(const collection: TCollection);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(collection, 'collection');
  Guard.CheckInheritsFrom(collection.ItemClass, TClass(T), 'collection.ItemClass');
{$ENDIF}

  inherited Create;
  fCollection := collection;
end;

destructor TCollectionList<T>.Destroy; //FI:W504
begin
  // not calling inherited because we don't want to call Clear
end;

procedure TCollectionList<T>.Delete(index: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  DeleteInternal(index, caRemoved);
end;

procedure TCollectionList<T>.DeleteInternal(index: Integer;
  notification: TCollectionChangedAction);
var
  oldItem: T;
begin
  oldItem := T(fCollection.Items[index]);
  oldItem.Collection := nil;
  IncreaseVersion;

  Changed(oldItem, notification);
  if notification = caRemoved then
    oldItem.Free;
end;

procedure TCollectionList<T>.DeleteRange(index, count: Integer);
var
  oldItems: array of T;
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  if count = 0 then
    Exit;

  SetLength(oldItems, count);

  for i := count downto 1 do
  begin
    oldItems[count - i] := T(fCollection.Items[index]);
    fCollection.Items[index].Collection := nil;
  end;
  IncreaseVersion;

  for i := Low(oldItems) to High(oldItems) do
  begin
    Changed(oldItems[i], caRemoved);
    oldItems[i].Free;
  end;
end;

procedure TCollectionList<T>.Exchange(index1, index2: Integer);
var
  temp: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index1 >= 0) and (index1 < Count), 'index1');
  Guard.CheckRange((index2 >= 0) and (index2 < Count), 'index2');
{$ENDIF}

  temp := T(fCollection.Items[index1]);
  fCollection.Items[index2].Index := index1;
  temp.Index := index2;
  IncreaseVersion;

  Changed(fCollection.Items[index2], caMoved);
  Changed(fCollection.Items[index1], caMoved);
end;

function TCollectionList<T>.Extract(const item: T): T;
var
  index: Integer;
begin
  index := IndexOf(item);
  if index < 0 then
    Result := Default(T)
  else
  begin
    Result := T(fCollection.Items[index]);
    DeleteInternal(index, caExtracted);
  end;
end;

function TCollectionList<T>.ExtractAt(index: Integer): T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  Result := T(fCollection.Items[index]);
  DeleteInternal(index, caExtracted);
end;

function TCollectionList<T>.GetCapacity: Integer;
begin
  Result := fCollection.Capacity;
end;

function TCollectionList<T>.GetCount: Integer;
begin
  Result := fCollection.Count;
end;

function TCollectionList<T>.GetElementType: PTypeInfo;
begin
  Result := fCollection.ItemClass.ClassInfo;
end;

function TCollectionList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TCollectionList<T>.GetItem(index: Integer): T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  Result := T(fCollection.Items[index]);
end;

{$IFOPT Q+}{$DEFINE OVERFLOW_CHECKS_ON}{$Q-}{$ENDIF}
procedure TCollectionList<T>.IncreaseVersion;
begin
  Inc(fVersion);
end;
{$IFDEF OVERFLOW_CHECKS_ON}{$Q+}{$ENDIF}

procedure TCollectionList<T>.Insert(index: Integer; const item: T);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');
{$ENDIF}

  item.Collection := fCollection;
  item.Index := index;
  IncreaseVersion;

  Changed(item, caAdded);
end;

procedure TCollectionList<T>.Move(currentIndex, newIndex: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((currentIndex >= 0) and (currentIndex < Count), 'currentIndex');
  Guard.CheckRange((newIndex >= 0) and (newIndex < Count), 'newIndex');
{$ENDIF}

  fCollection.Items[currentIndex].Index := newIndex;
  IncreaseVersion;

  Changed(fCollection.Items[newIndex], caMoved);
end;

procedure TCollectionList<T>.SetCapacity(value: Integer);
begin
  fCollection.Capacity := value;
end;

procedure TCollectionList<T>.SetItem(index: Integer; const value: T);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  fCollection.Items[index] := value;
end;

{$ENDREGION}


{$REGION 'TCollectionList<T>.TEnumerator'}

constructor TCollectionList<T>.TEnumerator.Create(const list: TCollectionList<T>);
begin
  inherited Create;
  fList := list;
  fList._AddRef;
  fVersion := fList.fVersion;
end;

destructor TCollectionList<T>.TEnumerator.Destroy;
begin
  fList._Release;
  inherited Destroy;
end;

function TCollectionList<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if fVersion <> fList.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  if fIndex < fList.Count then
  begin
    fCurrent := fList.Items[fIndex];
    Inc(fIndex);
    Result := True;
  end
  else
    fCurrent := Default(T);
end;

function TCollectionList<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

procedure TCollectionList<T>.TEnumerator.Reset;
begin
  if fVersion <> fList.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  fIndex := 0;
  fCurrent := Default(T);
end;

{$ENDREGION}


{$REGION 'TAnonymousReadOnlyList<T>'}

constructor TAnonymousReadOnlyList<T>.Create(const count: TFunc<Integer>;
  const items: TFunc<Integer, T>; const iterator: IEnumerable<T>);
begin
  inherited Create;
  fCount := count;
  fItems := items;
  fIterator := iterator;
  if not Assigned(fIterator) then
    fIterator := TAnonymousIterator<T>.Create(fCount, fItems);
end;

function TAnonymousReadOnlyList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := fIterator.GetEnumerator;
end;

function TAnonymousReadOnlyList<T>.GetItem(index: Integer): T;
begin
  Result := fItems(index);
end;

function TAnonymousReadOnlyList<T>.GetRange(index, count: Integer): IList<T>;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

{$IFNDEF DELPHI2010}
  Result := TCollections.CreateList<T>;
{$ELSE}
  Result := TList<T>.Create;
{$ENDIF}
  Result.Count := count;
  for i := 0 to count - 1 do
  begin
    Result[i] := fItems(index);
    Inc(index);
  end;
end;

function TAnonymousReadOnlyList<T>.IndexOf(const item: T): Integer;
begin
  Result := IndexOf(item, 0, Count)
end;

function TAnonymousReadOnlyList<T>.IndexOf(const item: T;
  index: Integer): Integer;
begin
  Result := IndexOf(item, index, Count - index);
end;

function TAnonymousReadOnlyList<T>.IndexOf(const item: T; index,
  count: Integer): Integer;
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
  comparer := EqualityComparer;
  for i := index to index + count - 1 do
    if comparer.Equals(fItems(i), item) then
      Exit(i);
  Result := -1;
end;

{$ENDREGION}


{$REGION 'TFoldedObjectList<T>'}

{$IFNDEF DELPHI2010}
function TFoldedObjectList<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TFoldedInterfaceList<T>'}

{$IFNDEF DELPHI2010}
function TFoldedInterfaceList<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TFoldedSortedObjectList<T>'}

{$IFNDEF DELPHI2010}
function TFoldedSortedObjectList<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TFoldedSortedInterfaceList<T>'}

{$IFNDEF DELPHI2010}
function TFoldedSortedInterfaceList<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TObservableList<T> }

constructor TObservableList<T>.Create;
begin
  inherited Create;
  fOnPropertyChanged := TPropertyChangedEventImpl.Create;
end;

procedure TObservableList<T>.DoItemPropertyChanged(sender: TObject;
  const eventArgs: IPropertyChangedEventArgs);
begin
  inherited Changed(T(sender), caChanged);
end;

procedure TObservableList<T>.DoPropertyChanged(const propertyName: string);
begin
  if Assigned(fOnPropertyChanged) and fOnPropertyChanged.CanInvoke then
    fOnPropertyChanged.Invoke(Self,
      TPropertyChangedEventArgs.Create(propertyName) as IPropertyChangedEventArgs);
end;

function TObservableList<T>.GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  Result := fOnPropertyChanged;
end;

procedure TObservableList<T>.Changed(
  const value: {$IFNDEF DELPHI2010}TObject{$ELSE}T{$ENDIF};
  action: TCollectionChangedAction);
var
  notifyPropertyChanged: INotifyPropertyChanged;
  propertyChanged: IEvent<TPropertyChangedEvent>;
begin
  if Supports({$IFNDEF DELPHI2010}value{$ELSE}PObject(@value)^{$ENDIF},
    INotifyPropertyChanged, notifyPropertyChanged) then
  begin
    propertyChanged := notifyPropertyChanged.OnPropertyChanged;
    case Action of
      caAdded: propertyChanged.Add(DoItemPropertyChanged);
      caRemoved, caExtracted: propertyChanged.Remove(DoItemPropertyChanged);
    end;
  end;

  inherited Changed(value, action);
  DoPropertyChanged('Count');
end;

{$ENDREGION}


end.
