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

unit Spring.Collections.Base;

interface

uses
  Classes,
  Generics.Defaults,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.Collections.Events,
  Spring.Collections.Trees,
  Spring.Events.Base,
  Spring.HashTable;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  {$IFDEF MSWINDOWS}
  IEnumerableInternal = interface //FI:W523
    procedure GetEnumerator(var result);
    function GetCount: Integer;
    function GetElementType: PTypeInfo;
    function GetIsEmpty: Boolean;
    function GetNonEnumeratedCount: Integer;
    function AsObject: TObject;
    procedure ToArray(var result);
  end;
  IEnumeratorInternal = interface //FI:W523
    procedure GetCurrent(var result);
  end;
  {$ENDIF}

  PEnumeratorVtable = ^TEnumeratorVtable;
  TEnumeratorVtable = array[0..4] of Pointer;

  PEnumeratorBlock = ^TEnumeratorBlock;
  TEnumeratorBlock = record
    Vtable: Pointer;
    RefCount: Integer;
    TypeInfo: PTypeInfo;
    Parent: TRefCountedObject;
    function _Release: Integer; stdcall;
    class function Create(enumerator: PPointer; vtable: PEnumeratorVtable;
      typeInfo, getCurrent, moveNext: Pointer): Pointer; overload; static;
    class function Create(enumerator: PPointer; vtable: PEnumeratorVtable;
      typeInfo: Pointer): Pointer; overload; static;
  end;

  PComparerVtable = ^TComparerVtable;
  TComparerVtable = array[0..3] of Pointer;
  PPairComparer = ^TPairComparer;
  TPairComparer = record
    Vtable: Pointer;
    RefCount: Integer;
    KeyComparer, ValueComparer: IInterface;
    function _Release: Integer; stdcall;
    class function Create(comparer: PPointer; vtable: PComparerVtable;
      compare: Pointer; keyType, valueType: PTypeInfo): Pointer; static;
  end;

  TPairComparer<TKey, TValue> = record
    Vtable: Pointer;
    RefCount: Integer;
    KeyComparer: IComparer<TKey>;
    ValueComparer: IComparer<TValue>;
    class var Comparer_Vtable: TComparerVtable;
    function Compare(const left, right: TPair<TKey, TValue>): Integer;
    class function Default: IComparer<TPair<TKey, TValue>>; static;
  end;

  TComparerThunks<T> = record
    class function Compare(instance: Pointer; const left, right): Integer; static;
    class function Equals(instance: Pointer; const left, right): Boolean; static;
    class function GetHashCode(instance: Pointer; const value): Integer; static;
  end;

  TActionCall = procedure(const enumerator, action: IInterface);
  TAssign = procedure(var target; const source);
  TCompare = function(self: Pointer; const left, right): Integer;
  TContains = function(const collection: IInterface; const value; const comparer: IInterface): Boolean;
  TEqualsCurrentWithOtherEnumerator = function(const enumerator1, enumerator2, comparer: IInterface): Boolean;
  TEqualsValue = function(const enumerator, comparer: IInterface; const value): Boolean;
  TEqualsArray = function(const enumerator: IInterface; comparer, values: Pointer; index: NativeInt): Boolean;
  TGetCurrent = procedure(const enumerator: IInterface; var value);
  TGetCurrentWithPredicate = function(const enumerator, predicate: IInterface; var value): Boolean;
  TGetCurrentWithSelector = procedure(const enumerator, comparer: IInterface; var value);
  TGetCurrentWithSelector<T> = function(const enumerator, selector: IInterface): T;
  TGetDefault = procedure(var value);
  TCollectionOperation = function(const enumerator, collection: IInterface): Boolean;
  TAddToCollection = function(const collection: IInterface; const value): Boolean;

  TEnumerableBase = class abstract(TRefCountedObject)
  private
    // Win64 compilers of XE2 and XE3 generate some bad code when using nil directly
    const emptyComparer{$IF defined(WIN64) and not defined(DELPHIXE4_UP)}: Pointer{$IFEND} = nil;
    function MemoizeCanReturnThis(iteratorClass: TClass): Boolean;
  protected
    this: Pointer;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}
    function QueryInterface(const IID: TGUID; out obj; getCurrent: TGetCurrent; add: TAddToCollection): HResult; overload; stdcall;

    function All(const predicate: IInterface; getCurrent: TGetCurrentWithSelector<Boolean>): Boolean;
    function Any(const predicate: IInterface; getCurrent: TGetCurrentWithSelector<Boolean>): Boolean; overload;

    procedure Aggregate(const func: IInterface; var result; getCurrent: TGetCurrent; getNext: TGetCurrentWithSelector);

    function Average(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Integer>): Double; overload;
    function Average(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Int64>): Double; overload;
    function Average(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Single>): Double; overload;
    function Average(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Double>): Double; overload;
    function Average(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Currency>): Double; overload;

    procedure ForEach(const action: IInterface; actionCall: TActionCall); overload;
    function ForEach(const values: IInterface; operation: TCollectionOperation): Integer; overload;

    procedure MaxMin(const comparer: IInterface; var result; getCurrent: TGetCurrent; getValue: TGetCurrentWithSelector);

    function Max(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Integer>): Integer; overload;
    function Max(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Int64>): Int64; overload;
    function Max(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Single>): Single; overload;
    function Max(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Double>): Double; overload;
    function Max(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Currency>): Currency; overload;

    function Min(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Integer>): Integer; overload;
    function Min(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Int64>): Int64; overload;
    function Min(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Single>): Single; overload;
    function Min(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Double>): Double; overload;
    function Min(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Currency>): Currency; overload;

    function MoveTo(const collection: IInterface; typeInfo: PTypeInfo): Integer; overload;
    function MoveTo(const collection, predicate: IInterface; typeInfo: PTypeInfo): Integer; overload;

    procedure ExtractAll(const match: IInterface; var result);
    function RemoveAll(const match: IInterface; typeInfo: PTypeInfo): Integer;

    function Sum(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Integer>): Integer; overload;
    function Sum(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Int64>): Int64; overload;
    function Sum(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Single>): Single; overload;
    function Sum(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Double>): Double; overload;
    function Sum(const selector: IInterface; getCurrent: TGetCurrentWithSelector<Currency>): Currency; overload;

    function Contains(const value; comparer: Pointer; equals: TEqualsValue): Boolean; overload;
    function EqualsTo(const values: IInterface; comparer: Pointer; equals: TEqualsCurrentWithOtherEnumerator): Boolean; overload;
    function EqualsTo(const values: Pointer; high: Integer; equals: TEqualsArray): Boolean; overload;

    function TryGetElementAt(var value; index: Integer; getCurrent: TGetCurrent; default: TGetDefault): Boolean;
    function TryGetFirst(var value; getCurrent: TGetCurrent; default: TGetDefault): Boolean; overload;
    function TryGetFirst(var value; const predicate: IInterface; getCurrent: TGetCurrentWithPredicate; default: TGetDefault): Boolean; overload;
    function TryGetLast(var value; getCurrent: TGetCurrent; default: TGetDefault): Boolean; overload;
    function TryGetLast(var value; const predicate: IInterface; getCurrent: TGetCurrentWithPredicate; default: TGetDefault): Boolean; overload;
    function TryGetSingle(var value; getCurrent: TGetCurrent; default: TGetDefault): Byte; overload;
    function TryGetSingle(var value; const predicate: IInterface; getCurrent: TGetCurrentWithPredicate; default: TGetDefault): Byte; overload;
    function TryGetSingleOrDefault(var value; getCurrent: TGetCurrent): Boolean; overload;
    function TryGetSingleOrDefault(var value; const predicate: IInterface; getCurrent: TGetCurrentWithPredicate): Boolean; overload;

    function CopyTo(var values: Pointer; index, size: NativeInt; getCurrent: TGetCurrent): Integer;
    procedure ToArray(var result: Pointer; typeInfo: Pointer; getCurrent: TGetCurrent);

    procedure Concat(const second: IInterface; var result; classType: TClass);
    procedure DefaultIfEmpty(defaultValue: Pointer; var result; typeInfo: Pointer; classType: TClass);
    procedure Distinct(comparer: Pointer; var result; classType: TClass);
    procedure Exclude(const second: IInterface; comparer: Pointer; var result; classType: TClass);
    procedure Intersect(const second: IInterface; comparer: Pointer; var result; classType: TClass);
    procedure Memoize(var result; classType: TClass);
    procedure Ordered(const comparer: IInterface; var result; classType: TClass);
    procedure Reversed(var result; classType: TClass);
    procedure Shuffled(var result; classType: TClass);
    procedure Skip(count: Integer; var result; classType: TClass);
    procedure SkipLast(count: Integer; var result; classType: TClass);
    procedure SkipWhile(const predicate: IInterface; var result; classType: TClass);
    procedure SkipWhileIndex(const predicate: IInterface; var result; classType: TClass);
    procedure Take(count: Integer; var result; classType: TClass);
    procedure TakeLast(count: Integer; var result; classType: TClass);
    procedure TakeWhile(const predicate: IInterface; var result; classType: TClass);
    procedure TakeWhileIndex(const predicate: IInterface; var result; classType: TClass);
    procedure Union(const second: IInterface; comparer: Pointer; var result; classType: TClass);
    procedure Where(const predicate: IInterface; var result; classType: TClass);
    procedure WhereIndex(const predicate: IInterface; var result; classType: TClass);
  public
    class function NewInstance: TObject; override;

    function Any: Boolean; overload;

    function AtLeast(count: Integer): Boolean;
    function AtMost(count: Integer): Boolean;
    function Between(min, max: Integer): Boolean;
    function Exactly(count: Integer): Boolean;
  end;

  TEnumerableBase<T> = class abstract(TEnumerableBase)
  protected
    fComparer: IComparer<T>;
  {$REGION 'Property Accessors'}
    function GetComparer: IComparer<T>;
    function GetElementType: PTypeInfo; virtual;
  {$ENDREGION}
    function QueryInterface(const IID: TGUID; out obj): HResult; stdcall;
    function CopyTo(var values: TArray<T>; index: Integer): Integer;
    function TryGetElementAt(var value: T; index: Integer): Boolean;
    function TryGetFirst(var value: T): Boolean; overload;
    function TryGetFirst(var value: T; const predicate: Predicate<T>): Boolean; overload;
    function TryGetLast(var value: T): Boolean; overload;
    function TryGetLast(var value: T; const predicate: Predicate<T>): Boolean; overload;
    function TryGetSingle(var value: T): Boolean; overload;
    function TryGetSingle(var value: T; const predicate: Predicate<T>): Boolean; overload;

    {$IFDEF DELPHIXE7_UP}
    class procedure __SuppressWarning(var value); static; inline;
    {$ENDIF}
  public
    procedure AfterConstruction; override;

    function Aggregate(const func: Func<T, T, T>): T;

    function All(const predicate: Predicate<T>): Boolean;

    function Any(const predicate: Predicate<T>): Boolean; overload;

    function Average: Double; overload;
    function Average(const selector: Func<T, Integer>): Double; overload;
    function Average(const selector: Func<T, Int64>): Double; overload;
    function Average(const selector: Func<T, Single>): Double; overload;
    function Average(const selector: Func<T, Double>): Double; overload;
    function Average(const selector: Func<T, Currency>): Double; overload;

    function Concat(const second: IEnumerable<T>): IEnumerable<T>;

    function Contains(const value: T): Boolean; overload;
    function Contains(const value: T; const comparer: IEqualityComparer<T>): Boolean; overload;
    function Contains(const value: T; const comparer: TEqualityComparison<T>): Boolean; overload;

    function DefaultIfEmpty: IEnumerable<T>; overload;
    function DefaultIfEmpty(const defaultValue: T): IEnumerable<T>; overload;

    function Distinct: IEnumerable<T>; overload;
    function Distinct(const comparer: IEqualityComparer<T>): IEnumerable<T>; overload;

    function ElementAt(index: Integer): T;
    function ElementAtOrDefault(index: Integer): T; overload;
    function ElementAtOrDefault(index: Integer; const defaultValue: T): T; overload;

    function EqualsTo(const values: array of T): Boolean; overload;
    function EqualsTo(const values: IEnumerable<T>): Boolean; overload;
    function EqualsTo(const values: IEnumerable<T>; const comparer: IEqualityComparer<T>): Boolean; overload;

    function Exclude(const second: IEnumerable<T>): IEnumerable<T>; overload;
    function Exclude(const second: IEnumerable<T>; const comparer: IEqualityComparer<T>): IEnumerable<T>; overload;

    function First: T; overload;
    function First(const predicate: Predicate<T>): T; overload;
    function FirstOrDefault: T; overload;
    function FirstOrDefault(const defaultValue: T): T; overload;
    function FirstOrDefault(const predicate: Predicate<T>): T; overload;
    function FirstOrDefault(const predicate: Predicate<T>; const defaultValue: T): T; overload;

    procedure ForEach(const action: Action<T>); overload;

    function Intersect(const second: IEnumerable<T>): IEnumerable<T>; overload;
    function Intersect(const second: IEnumerable<T>; const comparer: IEqualityComparer<T>): IEnumerable<T>; overload;

    function Last: T; overload;
    function Last(const predicate: Predicate<T>): T; overload;
    function LastOrDefault: T; overload;
    function LastOrDefault(const defaultValue: T): T; overload;
    function LastOrDefault(const predicate: Predicate<T>): T; overload;
    function LastOrDefault(const predicate: Predicate<T>; const defaultValue: T): T; overload;

    function Max: T; overload;
    function Max(const comparer: IComparer<T>): T; overload;
    function Max(const comparer: TComparison<T>): T; overload;
    function Max(const selector: Func<T, Integer>): Integer; overload;
    function Max(const selector: Func<T, Int64>): Int64; overload;
    function Max(const selector: Func<T, Single>): Single; overload;
    function Max(const selector: Func<T, Double>): Double; overload;
    function Max(const selector: Func<T, Currency>): Currency; overload;
    function Min: T; overload;
    function Min(const comparer: IComparer<T>): T; overload;
    function Min(const comparer: TComparison<T>): T; overload;
    function Min(const selector: Func<T, Integer>): Integer; overload;
    function Min(const selector: Func<T, Int64>): Int64; overload;
    function Min(const selector: Func<T, Single>): Single; overload;
    function Min(const selector: Func<T, Double>): Double; overload;
    function Min(const selector: Func<T, Currency>): Currency; overload;

    function Memoize: IEnumerable<T>;

    function Ordered: IEnumerable<T>; overload;
    function Ordered(const comparer: IComparer<T>): IEnumerable<T>; overload;
    function Ordered(const comparer: TComparison<T>): IEnumerable<T>; overload;

    function Reversed: IEnumerable<T>;

    function Shuffled: IEnumerable<T>;

    function Single: T; overload;
    function Single(const predicate: Predicate<T>): T; overload;
    function SingleOrDefault: T; overload;
    function SingleOrDefault(const defaultValue: T): T; overload;
    function SingleOrDefault(const predicate: Predicate<T>): T; overload;
    function SingleOrDefault(const predicate: Predicate<T>; const defaultValue: T): T; overload;

    function Skip(count: Integer): IEnumerable<T>;
    function SkipLast(count: Integer): IEnumerable<T>;
    function SkipWhile(const predicate: Predicate<T>): IEnumerable<T>; overload;
    function SkipWhile(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; overload;

    function Sum: T; overload;
    function Sum(const selector: Func<T, Integer>): Integer; overload;
    function Sum(const selector: Func<T, Int64>): Int64; overload;
    function Sum(const selector: Func<T, Single>): Single; overload;
    function Sum(const selector: Func<T, Double>): Double; overload;
    function Sum(const selector: Func<T, Currency>): Currency; overload;

    function Take(count: Integer): IEnumerable<T>;
    function TakeLast(count: Integer): IEnumerable<T>;
    function TakeWhile(const predicate: Predicate<T>): IEnumerable<T>; overload;
    function TakeWhile(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; overload;

    function Union(const second: IEnumerable<T>): IEnumerable<T>; overload;
    function Union(const second: IEnumerable<T>; const comparer: IEqualityComparer<T>): IEnumerable<T>; overload;

    function Where(const predicate: Predicate<T>): IEnumerable<T>; overload;
    function Where(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; overload;

    function ToArray: TArray<T>;
  end;

  TEnumerableWrapper = class(TRefCountedObject, IInterface, IEnumerable)
  private type
    TEnumerator = class(TRefCountedObject, IEnumerator)
    private
      fSource: IEnumerator;
      fElementType: PTypeInfo;
      fGetCurrent: TGetCurrent;
      function GetCurrent: Spring.TValue;
    public
      constructor Create(const source: IEnumerator; elementType: PTypeInfo; getCurrent: TGetCurrent);
      function MoveNext: Boolean;
    end;
  private
    fSource: IEnumerable;
    fElementType: PTypeInfo;
    fGetCurrent: TGetCurrent;
    function GetCount: Integer;
    function GetElementType: PTypeInfo;
    function GetIsEmpty: Boolean;
    function GetNonEnumeratedCount: Integer;
  protected
    function QueryInterface(const IID: TGUID; out obj): HResult; stdcall;
  public
    constructor Create(const source: IEnumerable; getCurrent: TGetCurrent);
    function AsObject: TObject;
    function GetEnumerator: IEnumerator;
  end;

  TCollectionWrapper = class(TEnumerableWrapper, ICollection)
  private
    fAdd: TAddToCollection;
  public
    constructor Create(const source: IEnumerable;
      getCurrent: TGetCurrent; add: TAddToCollection);
    function Add(const item: Spring.TValue): Boolean;
    procedure Clear;
  end;

  TExtensionKind = (
    Empty, DefaultIfEmpty, Partition, PartitionFromEnd,
    Distinct, Exclude, Intersect, Union,
    Concat, Memoize, Ordered, Reversed, Shuffled,
    SkipWhile, SkipWhileIndex,
    TakeWhile, TakeWhileIndex,
    Where, WhereIndex, Select);

  TIteratorMethods = packed record
    MoveNext: function(self: Pointer): Boolean;
    Finalize: function(self: Pointer): Boolean;
  end;

  TEnumerableExtension = class sealed(TEnumerableBase)
    // field layout has to match with TEnumerableExtension<T> below
    // TEnumerableBase<T>
    fComparer: IInterface;
    // TEnumerableExtension<T>
    fSource: IEnumerable;
    fPredicate: IInterface;
    fItems: Pointer;
    fIndex, fCount: Integer;
    fEqualityComparer: IInterface;
    fKind: TExtensionKind;
    IMT: Pointer;

    class function Create(classType: TClass; const source: IEnumerable;
      kind: TExtensionKind): TEnumerableExtension; static;

    function GetNonEnumeratedCount: Integer;
    procedure MemoizeToArray(var values: Pointer; typeInfo: PTypeInfo);
    function PartitionToArray(var values: Pointer; typeInfo: PTypeInfo): Boolean;
    procedure Skip(count: Integer; var result; classType: TClass);
    procedure Take(count: Integer; var result; classType: TClass);
    function TryGetElementAt(var value; index: Integer; getCurrent: TGetCurrent; default: TGetDefault): Boolean;
    function TryGetFirst(var value; getCurrent: TGetCurrent; default: TGetDefault): Boolean;
    function TryGetLast(var value; getCurrent: TGetCurrent; default: TGetDefault): Boolean;
  end;

  TEnumerableExtension<T> = class sealed(TEnumerableBase<T>, IInterface, IEnumerable<T>)
  protected
    // field layout has to match with TEnumerableExtension above
    fSource: IEnumerable<T>;
    fPredicate: IInterface;
    fItems: TArray<T>;
    fIndex, fCount: Integer;
    fEqualityComparer: IEqualityComparer<T>;
    fKind: TExtensionKind;

    function GetElementType: PTypeInfo; override;
    function GetNonEnumeratedCount: Integer;
  public
    function GetEnumerator: IEnumerator<T>;
    function Skip(count: Integer): IEnumerable<T>;
    function Take(count: Integer): IEnumerable<T>;
    function ToArray: TArray<T>;
    function TryGetElementAt(var value: T; index: Integer): Boolean;
    function TryGetFirst(var value: T): Boolean; overload;
    function TryGetLast(var value: T): Boolean; overload;
  end;

  PIteratorBlock = ^TIteratorBlock;
  TIteratorBlock = packed record
    // field layout has to match with TIteratorBlock<T> record below
    Vtable: Pointer;
    RefCount: Integer;

    Parent: TRefCountedObject;

    Methods: TIteratorMethods;
    DoMoveNext: function(self: Pointer): Boolean;
    Enumerator: IEnumerator;

    Source: IEnumerable;
    Predicate: IInterface;
    Items: Pointer;
    Index, Count: Integer;
    Kind: TExtensionKind;

    Current: record end;

    procedure InitHashTable(const equals, getHashCode: Pointer;
      itemsInfo: PTypeInfo; const comparer: IInterface);

    function Finalize(typeInfo: PTypeInfo): Boolean;

    function MoveNextEmpty: Boolean;

    function GetEnumerator: Boolean;
    function GetEnumeratorAndSkip: Boolean;
    function GetEnumeratorDefaultIfEmpty: Boolean;
    function GetEnumeratorHashTable(getCurrent: TGetCurrent; assign: TAssign): Boolean;
    function GetEnumeratorMemoize: Boolean;
    function GetEnumeratorPartitionFromEnd: Boolean;
    function GetEnumeratorSkipLast(getCurrent: TGetCurrent; typeInfo: PTypeInfo): Boolean;
    function GetEnumeratorTakeLast(getCurrent: TGetCurrent; typeInfo: PTypeInfo): Boolean;
    function MoveNextMemoize(getCurrent: TGetCurrent; assign: TAssign; typeInfo: PTypeInfo): Boolean;

    function _Release: Integer; stdcall;
    function MoveNext: Boolean;

    class procedure Create(enumerator: PPointer; extension: TEnumerableExtension;
      size: NativeInt; finalize, initialize: Pointer); static;
  end;

  TIteratorBlock<T> = packed record
    // field layout has to match with TIteratorBlock record above
    Vtable: Pointer;
    RefCount: Integer;

    Parent: TRefCountedObject;

    Methods: TIteratorMethods;
    DoMoveNext: function(self: Pointer): Boolean;
    Enumerator: IEnumerator<T>;

    Source: IEnumerable<T>;
    Predicate: IInterface;
    Items: TArray<T>;
    Index, Count: Integer;
    Kind: TExtensionKind;

    Current: T;

    class var Enumerator_Vtable: array[0..4] of Pointer;
    procedure Initialize(extension: TEnumerableExtension);

    function Finalize: Boolean;

    function GetCurrent: T;

    function GetEnumeratorHashTable: Boolean;
    function GetEnumeratorSkipLast: Boolean;
    function GetEnumeratorTakeLast: Boolean;

    function MoveNextHashTable: Boolean;

    function MoveNextArray: Boolean;
    function MoveNextConcat: Boolean;
    function MoveNextMemoize: Boolean;
    function MoveNextSkipLast: Boolean;
    function MoveNextSkipWhile: Boolean;
    function MoveNextSkipWhileIndex: Boolean;
    function MoveNextTakeWhile: Boolean;
    function MoveNextTakeWhileIndex: Boolean;
    function MoveNextWhere: Boolean;
    function MoveNextWhereIndex: Boolean;

    function MoveNextEnumerator: Boolean;
    function MoveNextEnumeratorCounted: Boolean;
    function MoveNextIndexed: Boolean;

    function ToArray: Boolean;
  private type
    TItem = packed record
    public
      HashCode: Integer;
      Item: T;
    end;
    TItems = TArray<TItem>;
    PItem = ^TItem;
  end;

  TCollectionThunks<T> = record
  public type
    {$IFDEF RSP31615}
    FuncInternal = reference to procedure({$IFDEF CPUX64}var result;{$ENDIF}const arg1, arg2: T{$IFDEF CPUX86}; var result{$ENDIF});
    {$ENDIF}
    ICollectionInternal = interface(IReadOnlyCollection<T>)
      // IMPORTANT NOTICE:
      // keep this in sync with ICollection<T> in Spring.Collections
      function GetOnChanged: ICollectionChangedEvent<T>;
      function Add(const item: T): Boolean;
      // internal helper type to solve compiler issue with using Slice on the
      // overloaded AddRange method in older Delphi versions
      procedure AddRange(const values: array of T);
      {$IFDEF RSP31615}overload;
      procedure AddRange(const values: IEnumerable<T>); overload;
      procedure Extract({$IFDEF CPUX64}var result; {$ENDIF}const item: T{$IFDEF CPUX86}; var result{$ENDIF});
      {$ENDIF}
    end;
  public
    class procedure AggregateCurrentWithValue(const enumerator, func: IInterface; var result); static;
    class procedure Assign(var target; const source); static;
    class procedure CallActionOnCurrent(const enumerator, action: IInterface); static;
    class function Contains(const collection: IInterface; const value; const comparer: IInterface): Boolean; static;
    class function EqualsCurrentWithOtherEnumerator(const enumerator1, enumerator2, comparer: IInterface): Boolean; static;
    class function EqualsCurrentWithArrayElement(const enumerator: IInterface; comparer, values: Pointer; index: NativeInt): Boolean; static;
    class function EqualsCurrentWithValue(const enumerator, comparer: IInterface; const value): Boolean; static;
    class procedure GetCurrent(const enumerator: IInterface; var value); static;
    class procedure GetCurrentIfGreaterThan(const enumerator, comparer: IInterface; var result); static;
    class procedure GetCurrentIfLessThan(const enumerator, comparer: IInterface; var result); static;
    class function GetCurrentWithPredicate(const enumerator, predicate: IInterface; var value): Boolean; static;
    class function GetCurrentWithSelector(const enumerator, selector: IInterface): T; static;
    class procedure GetDefault(var value); static;
    class function AddToCollection(const collection: IInterface; const value): Boolean; static;
    class function AddCurrentToCollection(const enumerator, collection: IInterface): Boolean; static;
    class function ExtractCurrentFromCollection(const enumerator, collection: IInterface): Boolean; static;
    class function RemoveCurrentFromCollection(const enumerator, collection: IInterface): Boolean; static;
    class procedure ProcessArray(kind: TExtensionKind; var values: TArray<T>; const comparer: IComparer<T>); static;
  end;

  TCollectionThunks<T1, T2> = record
    class function GetCurrentWithSelector(const enumerator, selector: IInterface): T2; static;
  end;

  TArrayIterator<T> = class(TEnumerableBase<T>, IInterface,
    IEnumerable<T>, IReadOnlyCollection<T>, IReadOnlyList<T>)
  private
  {$REGION 'Nested Types'}
    type
      PEnumerator = ^TEnumerator;
      TEnumerator = record
        Vtable: Pointer;
        RefCount: Integer;
        TypeInfo: PTypeInfo;
        Parent: TRefCountedObject;
        fItems: TArray<T>;
        fIndex, fCount: Integer;
        function GetCurrent: T;
        function MoveNext: Boolean;
        class var Enumerator_Vtable: TEnumeratorVtable;
      end;
  {$ENDREGION}
  private
    fItems: TArray<T>;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetItem(index: Integer): T;
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}
  public
    class function Create(const values: TArray<T>): IReadOnlyList<T>; overload; static;
    class function Create(const values: array of T): IReadOnlyList<T>; overload; static;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements IReadOnlyCollection<T>'}
    function CopyTo(var values: TArray<T>; index: Integer): Integer;
  {$ENDREGION}

  {$REGION 'Implements IReadOnlyList<T>'}
    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;
  {$ENDREGION}
  end;

  TFoldedArrayIterator<T> = class(TArrayIterator<T>)
  private
    fElementType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
  public
    class function Create(const values: TArray<T>; elementType: PTypeInfo): IReadOnlyList<T>; overload; static;
    class function Create(values: PPointer; count: Integer; elementType: PTypeInfo): IReadOnlyList<T>; overload; static;
  end;

  TIterator<T> = class abstract(TEnumerableBase<T>, IInterface, IEnumerator<T>)
  private
    fCurrent: T;
    fThreadId: TThreadID;
    fState: Integer;
    fTryMoveNext: function (self: TObject; var current: T): Boolean;
    const
      STATE_INITIAL    = -2; // initial state, before GetEnumerator
      STATE_FINISHED   = -1; // end of enumerator
      STATE_ENUMERATOR = 0;  // before calling MoveNext
      STATE_RUNNING    = 1;  // enumeration is running
    function GetCurrent: T;
  protected
    function Clone: TIterator<T>; virtual; abstract;
    procedure Dispose; virtual;
    procedure Start; virtual;
    function TryMoveNext(var current: T): Boolean; virtual; abstract;
  public
    procedure AfterConstruction; override;
    function GetEnumerator: IEnumerator<T>;
    function MoveNext: Boolean;
  end;

  TSourceIterator<T> = class abstract(TIterator<T>, IEnumerable<T>)
  strict private
    fSource: IEnumerable<T>;
  protected
    function GetElementType: PTypeInfo; override;
    property Source: IEnumerable<T> read fSource;
  public
    constructor Create(const source: IEnumerable<T>);
  end;

  /// <summary>
  ///   Provides an abstract implementation for the <see cref="Spring.Collections|ICollection&lt;T&gt;" />
  ///    interface.
  /// </summary>
  /// <remarks>
  ///   The Add/Remove/Extract/Clear methods are abstract. IsReadOnly returns <c>
  ///   False</c> by default.
  /// </remarks>
  TCollectionBase<T> = class abstract(TEnumerableBase<T>)
  private type
    TNotify = procedure(Self: TObject; const item: T; action: TCollectionChangedAction);
  private
    fOnChanged: TCollectionChangedEventImpl<T>;
    fNotify: TNotify;
    procedure EventChanged(Sender: TObject);
  protected
  {$REGION 'Property Accessors'}
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}
    procedure Changed(const item: T; action: TCollectionChangedAction); virtual;
    procedure DoNotify(const item: T; action: TCollectionChangedAction);
      // there are errors in the XE4 Win64 compiler when this method is inline
      {$IF not (Defined(DELPHIXE4) and Defined(WIN64))}inline;{$IFEND}
    procedure Reset;
    property OnChanged: TCollectionChangedEventImpl<T> read fOnChanged;
    property Notify: TNotify read fNotify;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function Add(const item: T): Boolean;
    procedure AddRange(const values: array of T); overload;
    procedure AddRange(const values: IEnumerable<T>); overload;

    function RemoveAll(const match: Predicate<T>): Integer;
    function RemoveRange(const values: array of T): Integer; overload;
    function RemoveRange(const values: IEnumerable<T>): Integer; overload;

    function ExtractAll(const match: Predicate<T>): TArray<T>;
    procedure ExtractRange(const values: array of T); overload;
    procedure ExtractRange(const values: IEnumerable<T>); overload;

    function MoveTo(const collection: ICollection<T>): Integer; overload;
    function MoveTo(const collection: ICollection<T>;
      const predicate: Predicate<T>): Integer; overload;
  end;

  THashMapInnerCollection = class(TEnumerableBase)
  public type
  {$REGION 'Nested Types'}
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      // same layout as THashMapInnerCollection<T>.TEnumerator
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fHashTable: PHashTable;
      fOffset: Integer;
      fIndex: Integer;
      fVersion: Integer;
      fItem: Pointer;
      fEnumerator: IEnumerator;
      procedure Assign(source: THashMapInnerCollection);
      function MoveNext: Boolean;
      function MoveNext_MultiMap: Boolean;
    end;
  {$ENDREGION}
  protected
    // TEnumerableBase<T>
    fComparer: IInterface;
    fSource: TRefCountedObject;
    fElementType: PTypeInfo;
    fHashTable: PHashTable;
    fValueComparer: IInterface;
    fOffset: Integer;
    fCount: PInteger;
    fContains: TContains;
    function GetCount: Integer;
    function Contains(const value; comparer: Pointer; equals: TEqualsMethod): Boolean;
    procedure ToArray(var result: Pointer; typeInfo: Pointer; assign: TAssign);
    function TryGetElementAt(var value; index: Integer;
      assign: TAssign; default: TGetDefault; getCurrent: TGetCurrent): Boolean;
  public
    class function Create(classType: TClass; source: TRefCountedObject;
      hashTable: PHashTable; const valueComparer: IInterface;
      elementType: PTypeInfo; offset: Integer = 0; count: PInteger = nil;
      contains: TContains = nil): THashMapInnerCollection; static;
    class function Create_Object(source: TRefCountedObject;
      hashTable: PHashTable; const valueComparer: IInterface;
      elementType: PTypeInfo; offset: Integer = 0; count: PInteger = nil;
      contains: TContains = nil): THashMapInnerCollection; static;
    class function Create_Interface(source: TRefCountedObject;
      hashTable: PHashTable; const valueComparer: IInterface;
      elementType: PTypeInfo; offset: Integer = 0; count: PInteger = nil;
      contains: TContains = nil): THashMapInnerCollection; static;
    property _this: Pointer read this;
  end;

  THashMapInnerCollection<T> = class(TEnumerableBase<T>,
    IEnumerable<T>, IReadOnlyCollection<T>)
  private type
  {$REGION 'Nested Types'}
    PT = ^T;

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
      fItem: ^T;
      fEnumerator: IEnumerator<T>;
      function GetCurrent: T;
      function GetCurrent_Values: T;
      class var Enumerator_Vtable: TEnumeratorVtable;
      class var Enumerator_Vtable_Values: TEnumeratorVtable;
    end;
  {$ENDREGION}
  protected
    fSource: TRefCountedObject;
    fElementType: PTypeInfo;
    fHashTable: PHashTable;
    fValueComparer: IEqualityComparer<T>;
    fOffset: Integer;
    fCount: PInteger;
    fContains: TContains;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}
  protected
    function GetElementType: PTypeInfo; override;
  public
  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  {$ENDREGION}

  {$REGION 'Implements IEnumerable<T>'}
    function Contains(const value: T; const comparer: IEqualityComparer<T>): Boolean; overload;
    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;
    function TryGetElementAt(var value: T; index: Integer): Boolean;
  {$ENDREGION}
  end;

  TCompareMethod = function(self: Pointer; const left, right): Integer;
  TTreeMapInnerCollection = class(TEnumerableBase)
  public type
  {$REGION 'Nested Types'}
    PNode = ^TNode;
    TNode = record
      Parent: PNode;
      Childs: array[0..1] of PNode;
      Key: record end;
    end;

    TEnumerator = record
      // same layout as TTreeMapInnerCollection<T>.TEnumerator
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fTree: TBinaryTree;
      fOffset: Integer;
      fSourceVersion: PInteger;
      fNode: PBinaryTreeNode;
      fVersion: Integer;
      fItem: Pointer;
      fEnumerator: IEnumerator;
      function MoveNext: Boolean;
      function MoveNext_MultiMap: Boolean;
    end;
  {$ENDREGION}
  protected
    // TEnumerableBase<T>
    fComparer: IInterface;
    fSource: TRefCountedObject;
    fElementType: PTypeInfo;
    fTree: TBinaryTree;
    fVersion: PInteger;
    fValueComparer: IInterface;
    fOffset: Integer;
    fCount: PInteger;
    fContains: TContains;
    function GetCount: Integer;
    function Contains(const value; comparer: Pointer;
      compare: TCompareMethod; equals: TEqualsMethod): Boolean;
    procedure ToArray(var result: Pointer; typeInfo: Pointer; assign: TAssign);
  public
    class function Create(classType: TClass; source: TRefCountedObject;
      tree: TBinaryTree; version: PInteger; const valueComparer: IInterface;
      elementType: PTypeInfo; offset: Integer; count: PInteger = nil;
      contains: TContains = nil): TTreeMapInnerCollection; static;
    class function Create_Object(source: TRefCountedObject;
      tree: TBinaryTree; version: PInteger; const valueComparer: IInterface;
      elementType: PTypeInfo; offset: Integer; count: PInteger = nil;
      contains: TContains = nil): TTreeMapInnerCollection; static;
    class function Create_Interface(source: TRefCountedObject;
      tree: TBinaryTree; version: PInteger; const valueComparer: IInterface;
      elementType: PTypeInfo; offset: Integer; count: PInteger = nil;
      contains: TContains = nil): TTreeMapInnerCollection; static;
    property _this: Pointer read this;
  end;

  TTreeMapInnerCollection<T> = class(TEnumerableBase<T>,
    IEnumerable<T>, IReadOnlyCollection<T>)
  private type
  {$REGION 'Nested Types'}
    PNode = ^TNode;
    TNode = packed record // same layout as TRedBlackTreeBase<TKey>.TNode
      Parent: Pointer;
      Childs: array[0..1] of Pointer;
      Key: T;
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
      fNode: PBinaryTreeNode;
      fVersion: Integer;
      fItem: ^T;
      fEnumerator: IEnumerator<T>;
      function GetCurrent: T;
      function GetCurrent_Values: T;
      class var Enumerator_Vtable: TEnumeratorVtable;
      class var Enumerator_Vtable_Values: TEnumeratorVtable;
    end;
  {$ENDREGION}
  protected
    fSource: TRefCountedObject;
    fElementType: PTypeInfo;
    fTree: TBinaryTree;
    fVersion: PInteger;
    fValueComparer: IEqualityComparer<T>;
    fOffset: Integer;
    fCount: PInteger;
    fContains: TContains;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}
  protected
    function GetElementType: PTypeInfo; override;
  public
  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  {$ENDREGION}

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const value: T; const comparer: IEqualityComparer<T>): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}
  end;

  TCircularArrayBuffer<T> = class(TEnumerableBase<T>)
  private
  {$REGION 'Nested Types'}
    type
      PEnumerator = ^TEnumerator;
      TEnumerator = record
        Vtable: Pointer;
        RefCount: Integer;
        TypeInfo: PTypeInfo;
        fSource: TCircularArrayBuffer<T>;
        fIndex, fCount: Integer;
        fVersion: Integer;
        function GetCurrent: T;
        function MoveNext: Boolean;
        class var Enumerator_Vtable: TEnumeratorVtable;
      end;
      ItemType = TTypeInfo<T>;
      PT = ^T;
  {$ENDREGION}
  strict private
    fOnChanged: TCollectionChangedEventImpl<T>;
    fItems: TArray<T>;
    fCapacity: Integer;
    fCount: Integer;
    fVersion: Integer;
    fHead: Integer;
    fTail: Integer;
    function GetTail: Integer; inline;
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
    function GetNonEnumeratedCount: Integer;
    function GetOnChanged: ICollectionChangedEvent<T>;
    function GetOwnsObjects: Boolean; inline;
    procedure SetCapacity(value: Integer);
    procedure SetOwnsObjects(value: Boolean);
  {$ENDREGION}

    procedure AddToHead(const item: T); inline;
    procedure AddToTail(const item: T); inline;
    procedure DeleteFromHead(action: TCollectionChangedAction); inline;
    procedure DeleteFromTail(action: TCollectionChangedAction); inline;

    property Capacity: Integer read GetCapacity;
    property Count: Integer read GetCount;
    property Items: TArray<T> read fItems;
    property Head: Integer read fHead;
    property Tail: Integer read GetTail;
    property OwnsObjects: Boolean read GetOwnsObjects;
  public
    constructor Create(capacity: Integer = 0; ownsObjects: Boolean = False);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;

    function First: T; overload;
    function FirstOrDefault: T; overload;
    function Single: T; overload;
    function SingleOrDefault(const defaultValue: T): T; overload;
    function TryGetFirst(var value: T): Boolean; overload;
    function TryGetLast(var value: T): Boolean; overload;
  {$ENDREGION}

    procedure Clear;
    procedure TrimExcess;
  end;

  TMapBase<TKey, TValue> = class abstract(TCollectionBase<TPair<TKey, TValue>>)
  private
    type
      TKeyValuePair = TPair<TKey, TValue>;
      TKeyValuePairComparer = TPairComparer<TKey, TValue>;
    class function RemoveCurrentFromCollection(const enumerator: IEnumerator<TKey>; const collection: IMap<TKey, TValue>): Boolean; static;
  protected
    fOnKeyChanged: TCollectionChangedEventImpl<TKey>;
    fOnValueChanged: TCollectionChangedEventImpl<TValue>;
  {$REGION 'Property Accessors'}
    function GetOnKeyChanged: ICollectionChangedEvent<TKey>;
    function GetOnValueChanged: ICollectionChangedEvent<TValue>;
    function GetKeyType: PTypeInfo; virtual;
    function GetValueType: PTypeInfo; virtual;
  {$ENDREGION}
    procedure DoNotify(const key: TKey; const value: TValue; action: TCollectionChangedAction); overload;
    function AsReadOnly: IReadOnlyDictionary<TKey, TValue>;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function Add(const item: TKeyValuePair): Boolean; overload;
    procedure Add(const key: TKey; const value: TValue); overload;

    function Remove(const item: TKeyValuePair): Boolean;
    function RemoveRange(const keys: array of TKey): Integer; overload;
    function RemoveRange(const keys: IEnumerable<TKey>): Integer; overload;

    function Extract(const item: TKeyValuePair): TKeyValuePair;

    function Contains(const item: TKeyValuePair): Boolean; overload;
  end;

const
  OwnsObjectsBitIndex = 31;
  OwnsObjectsMask     = 1 shl OwnsObjectsBitIndex;
  CountMask           = not OwnsObjectsMask;

  IEnumerableGuid: TGUID = '{6BC97F33-C0A8-4770-8E1C-C2017527B7E7}';
  ICollectionGuid: TGUID = '{4E749779-0873-498E-9597-FCF2A42C3F7B}';
  IObjectListGuid: TGUID = '{78A32DC5-1A5B-4191-9CA5-006CD85CF1AA}';
  IInterfaceListGuid: TGUID = '{B6BF9A6E-797C-4982-8D0D-B935E43D917E}';

  IEnumerableOfTGuid: TGUID = '{A6B46D30-5B0F-495F-B7EC-46FBC5A75D24}';
  ICollectionOfTGuid: TGUID = '{9BFD9B06-45CD-4C80-B145-01B09D432CF0}';
  IListOfTGuid: TGUID = '{B6B4E1E1-0D29-40E1-854C-A93DEA8D1AA5}';

  IReadOnlyCollectionOfTGuid: TGUID = '{E1368FD5-02AE-4481-A9DC-96329DFF606C}';
  IReadOnlyListOfTGuid: TGUID = '{82A74ABB-509E-4AC0-9268-A993E7DC3AB3}';
  IReadOnlyDictionaryGuid: TGUID = '{39F7C68B-373E-4758-808C-705D3978E38F}';

  IPartitionOfTGuid: TGUID = '{ACFB79AB-F593-4F2B-9720-E6CE984F6844}';

procedure AssignComparer(var comparer; const source: IInterface);
procedure EnsureEventInstance(var event: TEventBase; var result;
  eventClass: TEventBaseClass; eventChanged: TNotifyEvent);
function SupportsIndexedAccess(const source: IInterface): Boolean;
procedure UpdateNotify(instance: TObject; baseClass: TClass; var notify);
function DynArrayGrow(var items: Pointer; typeInfo: PTypeInfo; capacity: NativeInt): NativeInt;

implementation

uses
  Rtti,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Pthread,
{$ENDIF}
  Spring.Collections.Lists,
  Spring.Comparers,
  Spring.ResourceStrings;

procedure AssignComparer(var comparer; const source: IInterface);
begin
  IComparer<Integer>(comparer) := IEnumerable<Integer>(source).Comparer;
end;

type
  TEventImpl = class(TEventBase, IEvent);

procedure EnsureEventInstance(var event: TEventBase; var result;
  eventClass: TEventBaseClass; eventChanged: TNotifyEvent);

  procedure CreateEvent(var event: TEventBase; var result;
    eventClass: TEventBaseClass; eventChanged: TNotifyEvent);
  var
    newEvent: TEventBase;
  begin
    newEvent := eventClass.Create;
    if AtomicCmpExchange(Pointer(event), Pointer(newEvent), nil) <> nil then
      newEvent.Free
    else
    begin
      event.OnChanged := eventChanged;
      eventChanged(event);
    end;
    IEvent(result) := TEventImpl(event);
  end;

begin
  if Assigned(event) then
    IntfAssign(IEvent(TEventImpl(event)), IInterface(result))
  else
    CreateEvent(event, result, eventClass, eventChanged);
end;

function SupportsIndexedAccess(const source: IInterface): Boolean;
var
  obj: TObject;
  entry: PInterfaceEntry;
begin
  if source = nil then Exit(Boolean(NativeInt(source)));
  obj := IEnumerable(source).AsObject;
  Result := obj.GetInterfaceEntry(IReadOnlyListOfTGuid) <> nil;
  if not Result then
  begin
    entry := obj.GetInterfaceEntry(IPartitionOfTGuid);
    Result := Assigned(entry) and (IEnumerable(PByte(obj) + entry.IOffset).GetNonEnumeratedCount >= 0);
  end;
end;

procedure UpdateNotify(instance: TObject; baseClass: TClass; var notify);
type
  TNotifyRec = record
    event: TEventBase;
    code: Pointer;
  end;
const
  ChangedVirtualIndex = 1;
var
  baseAddress, actualAddress: Pointer;
begin
  baseAddress := PVTable(baseClass)[ChangedVirtualIndex];
  actualAddress := PPVTable(instance)^[ChangedVirtualIndex];
  with TNotifyRec(notify) do
    if (Assigned(event) and event.CanInvoke) or (actualAddress <> baseAddress) then
      code := actualAddress
    else
      code := nil;
end;

procedure DynArrayUnwrap(var items: Pointer; typeInfo: PTypeInfo; index, count: NativeInt);
var
  p: PDynArrayTypeInfo;
  capacity: NativeInt;
begin
  p := PDynArrayTypeInfo(PByte(typeInfo) + Byte(PDynArrayTypeInfo(typeInfo).name));
  {$POINTERMATH ON}
  capacity := PNativeInt(items)[-1];
  {$POINTERMATH OFF}
  if capacity < index + count then
  begin
    capacity := index + count;
    DynArraySetLength(items, typeInfo, 1, @capacity);
  end;
  {$IFDEF WEAKREF}
  if Assigned(p.elType) and HasWeakRef(PPointer(p.elType)^) then
  begin
    MoveManaged(items, @PByte(items)[count*p.elSize], p.elType^, index);
    MoveManaged(@PByte(items)[index*p.elSize], items, p.elType^, count);
  end
  else
  {$ENDIF}
  begin
    Move(items^, PByte(items)[count*p.elSize], p.elSize * index);
    Move(PByte(items)[index*p.elSize], items^, p.elSize * count);
    if Assigned(p.elType) then
      FillChar(PByte(items)[count*p.elSize], p.elSize * index, 0);
  end;
  DynArraySetLength(items, typeInfo, 1, @count);
end;

function DynArrayGrow(var items: Pointer; typeInfo: PTypeInfo; capacity: NativeInt): NativeInt;
begin
  Result := GrowCapacity(capacity);
  DynArraySetLength(items, typeInfo, 1, @Result);
end;


{$REGION 'TEnumeratorBlock'}

class function TEnumeratorBlock.Create(enumerator: PPointer; vtable: PEnumeratorVtable;
  typeInfo, getCurrent, moveNext: Pointer): Pointer;

  function GetEnumeratorBlockSize(typeInfo: Pointer): Integer; inline;
  var
    p: PByte;
  begin
    p := typeInfo;
    Result := PTypeData(@p[p[1]+2]).RecSize;
  end;

begin
  IInterface(enumerator^) := nil;
  Result := AllocMem(GetEnumeratorBlockSize(typeInfo));
  PEnumeratorBlock(Result).Vtable := vtable;
  PEnumeratorBlock(Result).RefCount := 1;
  PEnumeratorBlock(Result).TypeInfo := typeInfo;
  enumerator^ := Result;

  if not Assigned(vtable[0]) then
  begin
    vtable[0] := @NopQueryInterface;
    vtable[1] := @RecAddRef;
    vtable[2] := @TEnumeratorBlock._Release;
    vtable[3] := getCurrent;
    vtable[4] := moveNext;
  end;
end;

class function TEnumeratorBlock.Create(enumerator: PPointer;
  vtable: PEnumeratorVtable; typeInfo: Pointer): Pointer;

  function GetEnumeratorBlockSize(typeInfo: Pointer): Integer; inline;
  var
    p: PByte;
  begin
    p := typeInfo;
    Result := PTypeData(@p[p[1]+2]).RecSize;
  end;

begin
  IInterface(enumerator^) := nil;
  Result := AllocMem(GetEnumeratorBlockSize(typeInfo));
  PEnumeratorBlock(Result).Vtable := vtable;
  PEnumeratorBlock(Result).RefCount := 1;
  PEnumeratorBlock(Result).TypeInfo := typeInfo;
  enumerator^ := Result;

  if not Assigned(vtable[0]) then
  begin
    vtable[0] := @NopQueryInterface;
    vtable[1] := @RecAddRef;
    vtable[2] := @TEnumeratorBlock._Release;
  end;
end;

function TEnumeratorBlock._Release: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
  begin
    if Assigned(Parent) then
      Parent._Release;
    FinalizeRecord(@Self, TypeInfo);
    FreeMem(@Self);
  end;
end;

{$ENDREGION}


{$REGION 'TPairComparer'}

class function TPairComparer.Create(comparer: PPointer; vtable: PComparerVtable;
  compare: Pointer; keyType, valueType: PTypeInfo): Pointer;
begin
  vtable[0] := @NopQueryInterface;
  vtable[1] := @RecAddRef;
  vtable[2] := @TPairComparer._Release;
  vtable[3] := compare;

  IInterface(comparer^) := nil;
  Result := AllocMem(SizeOf(TPairComparer));
  PPairComparer(Result).Vtable := vtable;
  PPairComparer(Result).RefCount := 1;
  PPairComparer(Result).KeyComparer := IInterface(_LookupVtableInfo(giComparer, keyType, GetTypeSize(keyType)));
  PPairComparer(Result).ValueComparer := IInterface(_LookupVtableInfo(giComparer, valueType, GetTypeSize(valueType)));
  comparer^ := Result;
end;

function TPairComparer._Release: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
  begin
    KeyComparer := nil;
    ValueComparer := nil;
    FreeMem(@Self);
  end;
end;

{$ENDREGION}


{$REGION 'TPairComparer<TKey, TValue>' }

function TPairComparer<TKey, TValue>.Compare(const left, right: TPair<TKey, TValue>): Integer;
begin
  Result := KeyComparer.Compare(left.Key, right.Key);
  if Result = 0 then
    Result := ValueComparer.Compare(left.Value, right.Value);
end;

class function TPairComparer<TKey, TValue>.Default: IComparer<TPair<TKey, TValue>>;
begin
  TPairComparer.Create(@Result, @Comparer_Vtable,
    @TPairComparer<TKey, TValue>.Compare, TypeInfo(TKey), TypeInfo(TValue));
end;

{$ENDREGION}


{$REGION 'TComparerThunks<T>'}

class function TComparerThunks<T>.Compare(instance: Pointer; const left, right): Integer;
begin
  Result := IComparer<T>(instance).Compare(T(left), T(right));
end;

class function TComparerThunks<T>.Equals(instance: Pointer; const left, right): Boolean;
begin
  Result := IEqualityComparer<T>(instance).Equals(T(left), T(right));
end;

class function TComparerThunks<T>.GetHashCode(instance: Pointer; const value): Integer;
begin
  Result := IEqualityComparer<T>(instance).GetHashCode(T(value));
end;

{$ENDREGION}


{$REGION 'TEnumerableBase'}

class function TEnumerableBase.NewInstance: TObject;
begin
  Result := inherited NewInstance;

  // child classes must implement IEnumerable<T>
  TEnumerableBase(Result).this := Pointer(PByte(Result) + GetInterfaceEntry(IEnumerableOfTGuid).IOffset);
end;

function TEnumerableBase.QueryInterface(const IID: TGUID; out obj;
  getCurrent: TGetCurrent; add: TAddToCollection): HResult;
begin
  if IID = IEnumerableGuid then
  begin
    IEnumerable(obj) := TEnumerableWrapper.Create(IEnumerable(this), getCurrent);
    Result := S_OK;
  end
  else if IID = ICollectionGuid then
  begin
    ICollection(obj) := TCollectionWrapper.Create(IEnumerable(this), getCurrent, add);
    Result := S_OK;
  end
  else
    Result := inherited QueryInterface(IID, obj);
end;

function IsCountInRange(const this: IEnumerable; min, max, limit: Integer): Boolean;

  function SkipAndCountSlow(const this: IEnumerable; limit: Integer): Integer;
  var
    enumerator: IEnumerator;
  begin
    Result := 0;
    enumerator := this.GetEnumerator;
    while (Result < limit) and enumerator.MoveNext do
      Inc(Result);
  end;

var
  count: Integer;
begin
  count := this.GetNonEnumeratedCount;
  if count < 0 then
    count := SkipAndCountSlow(this, limit);
  Result := {$B+}(count >= min) and (count <= max);{$B-}
end;

function HasAnyItems(const this: IEnumerable): Boolean;
var
  enumerator: IEnumerator;
begin
  enumerator := this.GetEnumerator;
  Result := enumerator.MoveNext;
end;

procedure TEnumerableBase.Aggregate(const func: IInterface; var result;
  getCurrent: TGetCurrent; getNext: TGetCurrentWithSelector);
var
  enumerator: IEnumerator;
begin
  if not Assigned(func) then RaiseHelper.ArgumentNil(ExceptionArgument.func);

  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  getCurrent(enumerator, result);
  while enumerator.MoveNext do
    getNext(enumerator, func, result);
end;

function TEnumerableBase.All(const predicate: IInterface;
  getCurrent: TGetCurrentWithSelector<Boolean>): Boolean;
var
  enumerator: IEnumerator;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  enumerator := IEnumerable(this).GetEnumerator;
  while enumerator.MoveNext do
    if not getCurrent(enumerator, predicate) then
      Exit(False);
  Result := True;
end;

function TEnumerableBase.Any: Boolean;
var
  count: Integer;
begin
  count := IEnumerable(this).GetNonEnumeratedCount;
  if count >= 0 then
    Result := count > 0
  else
    Result := HasAnyItems(IEnumerable(this));
end;

function TEnumerableBase.Any(const predicate: IInterface;
  getCurrent: TGetCurrentWithSelector<Boolean>): Boolean;
var
  enumerator: IEnumerator;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  enumerator := IEnumerable(this).GetEnumerator;
  while enumerator.MoveNext do
    if getCurrent(enumerator, predicate) then
      Exit(True);
  Result := False;
end;

function TEnumerableBase.AtLeast(count: Integer): Boolean;
begin
  if count >= 0 then
    Result := IsCountInRange(IEnumerable(this), count, MaxInt, count)
  else
    Result := Boolean(RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum));
end;

function TEnumerableBase.AtMost(count: Integer): Boolean;
begin
  if count >= 0 then
    Result := IsCountInRange(IEnumerable(this), 0, count, count + 1)
  else
    Result := Boolean(RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum));
end;

function TEnumerableBase.Average(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Integer>): Double;
var
  enumerator: IEnumerator;
  sum, count: Int64; //FI:W517
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  sum := getCurrent(enumerator, selector);
  count := 1;
  while enumerator.MoveNext do
  begin
    {$Q+}
    sum := sum + getCurrent(enumerator, selector);
    {$IFNDEF OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
    Inc(count);
  end;
  Result := sum / count;
end;

function TEnumerableBase.Average(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Int64>): Double;
var
  enumerator: IEnumerator;
  sum, count: Int64; //FI:W517
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  sum := getCurrent(enumerator, selector);
  count := 1;
  while enumerator.MoveNext do
  begin
    {$Q+}
    sum := sum + getCurrent(enumerator, selector);
    {$IFNDEF OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
    Inc(count);
  end;
  Result := sum / count;
end;

function TEnumerableBase.Average(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Single>): Double;
var
  enumerator: IEnumerator;
  sum: Double; //FI:W517
  count: Int64;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  sum := getCurrent(enumerator, selector);
  count := 1;
  while enumerator.MoveNext do
  begin
    sum := sum + getCurrent(enumerator, selector);
    Inc(count);
  end;
  Result := sum / count;
end;

function TEnumerableBase.Average(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Double>): Double;
var
  enumerator: IEnumerator;
  sum: Double; //FI:W517
  count: Int64;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  sum := getCurrent(enumerator, selector);
  count := 1;
  while enumerator.MoveNext do
  begin
    sum := sum + getCurrent(enumerator, selector);
    Inc(count);
  end;
  Result := sum / count;
end;

function TEnumerableBase.Average(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Currency>): Double;
var
  enumerator: IEnumerator;
  sum: Double; //FI:W517
  count: Int64;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  sum := getCurrent(enumerator, selector);
  count := 1;
  while enumerator.MoveNext do
  begin
    sum := sum + getCurrent(enumerator, selector);
    Inc(count);
  end;
  Result := sum / count;
end;

function TEnumerableBase.Between(min, max: Integer): Boolean;
begin
  if min >= 0 then
    if max >= min then
      Result := IsCountInRange(IEnumerable(this), min, max, max + 1)
    else
      Result := Boolean(RaiseHelper.ArgumentOutOfRange(ExceptionArgument.max))
  else
    Result := Boolean(RaiseHelper.ArgumentOutOfRange(ExceptionArgument.min, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum));
end;

procedure TEnumerableBase.Concat(const second: IInterface; var result; classType: TClass);
begin
  if not Assigned(second) then RaiseHelper.ArgumentNil(ExceptionArgument.second);

  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.Concat) do
  begin
    fPredicate := second;
    IInterface(result) := IInterface(@IMT);
  end;
end;

function TEnumerableBase.Contains(const value; comparer: Pointer;
  equals: TEqualsValue): Boolean;
var
  elementType: PTypeInfo;
  enumerator: IEnumerator;
begin
  if not Assigned(comparer) then
  begin
    elementType := IEnumerable(this).GetElementType;
    comparer := _LookupVtableInfo(giEqualityComparer, elementType, GetTypeSize(elementType));
  end;

  enumerator := IEnumerable(this).GetEnumerator;
  while enumerator.MoveNext do
    if equals(enumerator, IInterface(comparer), value) then
      Exit(True);
  Result := False;
end;

function TEnumerableBase.CopyTo(var values: Pointer; index, size: NativeInt;
  getCurrent: TGetCurrent): Integer;
var
  enumerator: IEnumerator;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  Result := 0;
  while enumerator.MoveNext do
  begin
    getCurrent(enumerator, PByte(values)[index*size]);
    Inc(index);
    Inc(Result);
  end;
end;

procedure TEnumerableBase.DefaultIfEmpty(defaultValue: Pointer; var result; typeInfo: Pointer; classType: TClass);
const
  Len: NativeInt = 1;
var
  p: PDynArrayTypeInfo;
begin
  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.DefaultIfEmpty) do
  begin
    DynArraySetLength(fItems, typeInfo, 1, @Len);
    p := PDynArrayTypeInfo(PByte(typeInfo) + Byte(PDynArrayTypeInfo(typeInfo).name));
    IInterface(result) := IInterface(@IMT);
    if defaultValue <> nil then
      if p.elType <> nil then
        MoveManaged(defaultValue, fItems, p.elType^, 1)
      else
        Move(defaultValue^, fItems^, p.elSize);
  end;
end;

procedure TEnumerableBase.Distinct(comparer: Pointer; var result; classType: TClass);
var
  elementType: PTypeInfo;
begin
  if not Assigned(comparer) then
  begin
    elementType := IEnumerable(this).GetElementType;
    comparer := _LookupVtableInfo(giEqualityComparer, elementType, GetTypeSize(elementType));
  end;

  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.Distinct) do
  begin
    fEqualityComparer := IInterface(comparer);
    IInterface(result) := IInterface(@IMT);
  end;
end;

function TEnumerableBase.EqualsTo(const values: IInterface; comparer: Pointer;
  equals: TEqualsCurrentWithOtherEnumerator): Boolean;
var
  elementType: PTypeInfo;
  e1, e2: IEnumerator;
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  if IInterface(this) = values then Exit(True);

  if not Assigned(comparer) then
  begin
    elementType := IEnumerable(this).GetElementType;
    comparer := _LookupVtableInfo(giEqualityComparer, elementType, GetTypeSize(elementType));
  end;

  e1 := IEnumerable(this).GetEnumerator;
  e2 := IEnumerable(values).GetEnumerator;

  while e1.MoveNext do
    if not e2.MoveNext or not equals(e1, e2, IInterface(comparer)) then
      Exit(False);
  Result := not e2.MoveNext;
end;

function TEnumerableBase.EqualsTo(const values: Pointer; high: Integer; equals: TEqualsArray): Boolean;
var
  elementType: PTypeInfo;
  comparer: Pointer;
  enumerator: IEnumerator;
  i: NativeInt;
begin
  elementType := IEnumerable(this).GetElementType;
  comparer := _LookupVtableInfo(giEqualityComparer, elementType, GetTypeSize(elementType));
  enumerator := IEnumerable(this).GetEnumerator;
  for i := 0 to high do
    if not enumerator.MoveNext or not equals(enumerator, comparer, values, i) then
      Exit(False);
  Result := not enumerator.MoveNext;
end;

function TEnumerableBase.Exactly(count: Integer): Boolean;
begin
  if count >= 0 then
    Result := IsCountInRange(IEnumerable(this), count, count, count + 1)
  else
    Result := Boolean(RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum));
end;

procedure TEnumerableBase.Exclude(const second: IInterface; comparer: Pointer; var result; classType: TClass);
var
  elementType: PTypeInfo;
begin
  if not Assigned(second) then RaiseHelper.ArgumentNil(ExceptionArgument.second);

  if not Assigned(comparer) then
  begin
    elementType := IEnumerable(this).GetElementType;
    comparer := _LookupVtableInfo(giEqualityComparer, elementType, GetTypeSize(elementType));
  end;

  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.Exclude) do
  begin
    fPredicate := second;
    fEqualityComparer := IInterface(comparer);
    IInterface(result) := IInterface(@IMT);
  end;
end;

procedure TEnumerableBase.ExtractAll(const match: IInterface; var result);
begin
  if not Assigned(match) then RaiseHelper.ArgumentNil(ExceptionArgument.match);

  // little hack - we can hardcast here since we are just passing along a predicate
  {$IFDEF MSWINDOWS}
  IEnumerableInternal(IEnumerable<Pointer>(this).Where(Predicate<Pointer>(match))).ToArray(result);
  {$ELSE}
  TArray<Pointer>(result) := IEnumerable<Pointer>(this).Where(Predicate<Pointer>(match)).ToArray;
  {$ENDIF}
  // little hack - we can hardcast here since we are just passing along an open array
  ICollection<Pointer>(this).ExtractRange(TArray<Pointer>(result));
end;

procedure TEnumerableBase.ForEach(const action: IInterface; actionCall: TActionCall);
var
  enumerator: IEnumerator;
begin
  if not Assigned(action) then RaiseHelper.ArgumentNil(ExceptionArgument.action);

  enumerator := IEnumerable(this).GetEnumerator;
  while enumerator.MoveNext do
    actionCall(enumerator, action);
end;

function TEnumerableBase.ForEach(const values: IInterface; operation: TCollectionOperation): Integer;
var
  enumerator: IEnumerator;
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  Result := 0;
  enumerator := IEnumerable(values).GetEnumerator;
  while enumerator.MoveNext do
    Inc(Result, Byte(operation(enumerator, IInterface(this))));
end;

function TEnumerableBase.GetCount: Integer;

  function GetEnumeratedCount(const this: IEnumerable): Integer;
  var
    enumerator: IEnumerator;
  begin
    Result := 0;
    enumerator := this.GetEnumerator;
    while enumerator.MoveNext do
      Inc(Result);
  end;

begin
  Result := IEnumerable(this).GetNonEnumeratedCount;
  if Result < 0 then
    Result := GetEnumeratedCount(IEnumerable(this));
end;

function TEnumerableBase.GetIsEmpty: Boolean;
var
  count: Integer;
begin
  count := IEnumerable(this).GetNonEnumeratedCount;
  if count >= 0 then
    Result := count = 0
  else
    Result := not HasAnyItems(IEnumerable(this));
end;

function TEnumerableBase.GetNonEnumeratedCount: Integer;
begin
  // implementing IReadOnlyCollection is an indicator for having its own count
  if GetInterfaceEntry(IReadOnlyCollectionOfTGuid) <> nil then
    Result := IEnumerable(this).Count
  else
    Result := -1;
end;

procedure TEnumerableBase.Intersect(const second: IInterface; comparer: Pointer; var result; classType: TClass);
var
  elementType: PTypeInfo;
begin
  if not Assigned(second) then RaiseHelper.ArgumentNil(ExceptionArgument.second);

  if not Assigned(comparer) then
  begin
    elementType := IEnumerable(this).GetElementType;
    comparer := _LookupVtableInfo(giEqualityComparer, elementType, GetTypeSize(elementType));
  end;

  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.Intersect) do
  begin
    fPredicate := second;
    fEqualityComparer := IInterface(comparer);
    IInterface(result) := IInterface(@IMT);
  end;
end;

function TEnumerableBase.MemoizeCanReturnThis(iteratorClass: TClass): Boolean;
begin
  // TODO: optimize by iterating class hierarchy only once
  Result := (GetInterfaceEntry(ICollectionOfTGuid) <> nil)
    or (GetInterfaceEntry(IReadOnlyCollectionOfTGuid) <> nil)
    or (InheritsFrom(iteratorClass)
    and (TEnumerableExtension(Self).fKind = TExtensionKind.Memoize));
end;

procedure TEnumerableBase.MaxMin(const comparer: IInterface; var result;
  getCurrent: TGetCurrent; getValue: TGetCurrentWithSelector);
var
  enumerator: IEnumerator;
begin
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);

  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;

  getCurrent(enumerator, result);
  while enumerator.MoveNext do
    getValue(enumerator, comparer, result);
end;

function TEnumerableBase.Max(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Integer>): Integer;
var
  enumerator: IEnumerator;
  value: Integer;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  Result := getCurrent(enumerator, selector);
  while enumerator.MoveNext do
  begin
    value := getCurrent(enumerator, selector);
    if value > Result then
      Result := value;
  end;
end;

function TEnumerableBase.Max(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Int64>): Int64;
var
  enumerator: IEnumerator;
  value: Int64;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  Result := getCurrent(enumerator, selector);
  while enumerator.MoveNext do
  begin
    value := getCurrent(enumerator, selector);
    if value > Result then
      Result := value;
  end;
end;

function TEnumerableBase.Max(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Single>): Single;
var
  enumerator: IEnumerator;
  value: Single;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  Result := getCurrent(enumerator, selector);
  while enumerator.MoveNext do
  begin
    value := getCurrent(enumerator, selector);
    if value > Result then
      Result := value;
  end;
end;

function TEnumerableBase.Max(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Double>): Double;
var
  enumerator: IEnumerator;
  value: Double;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  Result := getCurrent(enumerator, selector);
  while enumerator.MoveNext do
  begin
    value := getCurrent(enumerator, selector);
    if value > Result then
      Result := value;
  end;
end;

function TEnumerableBase.Max(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Currency>): Currency;
var
  enumerator: IEnumerator;
  value: Currency;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  Result := getCurrent(enumerator, selector);
  while enumerator.MoveNext do
  begin
    value := getCurrent(enumerator, selector);
    if value > Result then
      Result := value;
  end;
end;

procedure TEnumerableBase.Memoize(var result; classType: TClass);
begin
  if MemoizeCanReturnThis(classType) then
    IInterface(Result) := IInterface(this)
  else
    with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.Memoize) do
      IInterface(result) := IInterface(@IMT);
end;

function TEnumerableBase.Min(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Integer>): Integer;
var
  enumerator: IEnumerator;
  value: Integer;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  Result := getCurrent(enumerator, selector);
  while enumerator.MoveNext do
  begin
    value := getCurrent(enumerator, selector);
    if value < Result then
      Result := value;
  end;
end;

function TEnumerableBase.Min(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Int64>): Int64;
var
  enumerator: IEnumerator;
  value: Int64;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  Result := getCurrent(enumerator, selector);
  while enumerator.MoveNext do
  begin
    value := getCurrent(enumerator, selector);
    if value < Result then
      Result := value;
  end;
end;

function TEnumerableBase.Min(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Single>): Single;
var
  enumerator: IEnumerator;
  value: Single;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  Result := getCurrent(enumerator, selector);
  while enumerator.MoveNext do
  begin
    value := getCurrent(enumerator, selector);
    if value < Result then
      Result := value;
  end;
end;

function TEnumerableBase.Min(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Double>): Double;
var
  enumerator: IEnumerator;
  value: Double;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  Result := getCurrent(enumerator, selector);
  while enumerator.MoveNext do
  begin
    value := getCurrent(enumerator, selector);
    if value < Result then
      Result := value;
  end;
end;

function TEnumerableBase.Min(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Currency>): Currency;
var
  enumerator: IEnumerator;
  value: Currency;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  Result := getCurrent(enumerator, selector);
  while enumerator.MoveNext do
  begin
    value := getCurrent(enumerator, selector);
    if value < Result then
      Result := value;
  end;
end;

function TEnumerableBase.MoveTo(const collection: IInterface; typeInfo: PTypeInfo): Integer;
var
  values: Pointer;
begin
  if not Assigned(collection) then RaiseHelper.ArgumentNil(ExceptionArgument.collection);

  values := nil;
  {$IFDEF MSWINDOWS}
  IEnumerableInternal(this).ToArray(values);
  {$ELSE}
  values := Pointer(IEnumerable<Pointer>(this).ToArray);
  {$ENDIF}
  try
    // little hack - we can hardcast here since we are just passing along an open array
    ICollection<Pointer>(this).ExtractRange(TArray<Pointer>(values));
    ICollection<Pointer>(collection).AddRange(TArray<Pointer>(values));
    Result := DynArrayLength(values);
  finally
    DynArrayClear(values, typeInfo);
  end;
end;

function TEnumerableBase.MoveTo(const collection, predicate: IInterface; typeInfo: PTypeInfo): Integer;
var
  values: Pointer;
begin
  if not Assigned(collection) then RaiseHelper.ArgumentNil(ExceptionArgument.collection);
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  values := nil;
  // little hack - we can hardcast here since we are just passing along a predicate
  {$IFDEF MSWINDOWS}
  IEnumerableInternal(IEnumerable<Pointer>(this).Where(Predicate<Pointer>(predicate))).ToArray(values);
  {$ELSE}
  values := Pointer(IEnumerable<Pointer>(this).Where(Predicate<Pointer>(predicate)).ToArray);
  {$ENDIF}
  try
    // little hack - we can hardcast here since we are just passing along an open array
    ICollection<Pointer>(this).ExtractRange(TArray<Pointer>(values));
    ICollection<Pointer>(collection).AddRange(TArray<Pointer>(values));
    Result := DynArrayLength(values);
  finally
    DynArrayClear(values, typeInfo);
  end;
end;

procedure TEnumerableBase.Ordered(const comparer: IInterface; var result; classType: TClass);
begin
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);

  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.Ordered) do
  begin
    fPredicate := comparer;
    IInterface(result) := IInterface(@IMT);
  end;
end;

function TEnumerableBase.RemoveAll(const match: IInterface; typeInfo: PTypeInfo): Integer;
var
  values: Pointer;
begin
  if not Assigned(match) then RaiseHelper.ArgumentNil(ExceptionArgument.match);

  values := nil;
  // little hack - we can hardcast here since we are just passing along a predicate
  {$IFDEF MSWINDOWS}
  IEnumerableInternal(IEnumerable<Pointer>(this).Where(Predicate<Pointer>(match))).ToArray(values);
  {$ELSE}
  values := Pointer(IEnumerable<Pointer>(this).Where(Predicate<Pointer>(match)).ToArray);
  {$ENDIF}
  try
    // little hack - we can hardcast here since we are just passing along an open array
    Result := ICollection<Pointer>(this).RemoveRange(TArray<Pointer>(values));
  finally
    DynArrayClear(values, typeInfo);
  end;
end;

procedure TEnumerableBase.Reversed(var result; classType: TClass);
begin
  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.Reversed) do
    IInterface(result) := IInterface(@IMT);
end;

procedure TEnumerableBase.Shuffled(var result; classType: TClass);
begin
  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.Shuffled) do
    IInterface(result) := IInterface(@IMT);
end;

procedure TEnumerableBase.Skip(count: Integer; var result; classType: TClass);
var
  maxCount: Integer;
begin
  if count <= 0 then
    IInterface(result) := IInterface(this)
  else
    with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.Partition) do
    begin
      fIndex := count;
      maxCount := IEnumerable(Self.this).GetNonEnumeratedCount;
      if maxCount >= 0 then
        maxCount := MaxInt - count;
      fCount := maxCount;
      IInterface(result) := IInterface(@IMT);
    end;
end;

procedure TEnumerableBase.SkipLast(count: Integer; var result; classType: TClass);
begin
  if count <= 0 then
    IInterface(result) := IInterface(this)
  else
    with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.PartitionFromEnd) do
    begin
      fCount := count;
      IInterface(result) := IInterface(@IMT);
    end;
end;

procedure TEnumerableBase.SkipWhile(const predicate: IInterface; var result; classType: TClass);
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.SkipWhile) do
  begin
    fPredicate := predicate;
    IInterface(result) := IInterface(@IMT);
  end;
end;

procedure TEnumerableBase.SkipWhileIndex(const predicate: IInterface; var result; classType: TClass);
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.SkipWhileIndex) do
  begin
    fPredicate := predicate;
    IInterface(result) := IInterface(@IMT);
  end;
end;

function TEnumerableBase.Sum(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Integer>): Integer;
var
  enumerator: IEnumerator;
begin
  Result := 0;
  enumerator := IEnumerable(this).GetEnumerator;
  while enumerator.MoveNext do
    {$Q+}
    Result := Result + getCurrent(enumerator, selector);
    {$IFNDEF OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
end;

function TEnumerableBase.Sum(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Int64>): Int64;
var
  enumerator: IEnumerator;
begin
  Result := 0;
  enumerator := IEnumerable(this).GetEnumerator;
  while enumerator.MoveNext do
    {$Q+}
    Result := Result + getCurrent(enumerator, selector);
    {$IFNDEF OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
end;

function TEnumerableBase.Sum(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Single>): Single;
var
  enumerator: IEnumerator;
begin
  Result := 0;
  enumerator := IEnumerable(this).GetEnumerator;
  while enumerator.MoveNext do
    Result := Result + getCurrent(enumerator, selector);
end;

function TEnumerableBase.Sum(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Double>): Double;
var
  enumerator: IEnumerator;
begin
  Result := 0;
  enumerator := IEnumerable(this).GetEnumerator;
  while enumerator.MoveNext do
    Result := Result + getCurrent(enumerator, selector);
end;

function TEnumerableBase.Sum(const selector: IInterface;
  getCurrent: TGetCurrentWithSelector<Currency>): Currency;
var
  enumerator: IEnumerator;
begin
  Result := 0;
  enumerator := IEnumerable(this).GetEnumerator;
  while enumerator.MoveNext do
    Result := Result + getCurrent(enumerator, selector);
end;

procedure TEnumerableBase.Take(count: Integer; var result; classType: TClass);
begin
  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.Partition) do
  begin
    if count < 0 then
      count := 0;
    fCount := count;
    IInterface(result) := IInterface(@IMT);
  end;
end;

procedure TEnumerableBase.TakeLast(count: Integer; var result; classType: TClass);
begin
  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.PartitionFromEnd) do
  begin
    fIndex := -1;
    if count <= 0 then
      count := 0;
    fCount := count;
    IInterface(result) := IInterface(@IMT);
  end;
end;

procedure TEnumerableBase.TakeWhile(const predicate: IInterface; var result; classType: TClass);
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.TakeWhile) do
  begin
    fPredicate := predicate;
    IInterface(result) := IInterface(@IMT);
  end;
end;

procedure TEnumerableBase.TakeWhileIndex(const predicate: IInterface; var result; classType: TClass);
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.TakeWhileIndex) do
  begin
    fPredicate := predicate;
    IInterface(result) := IInterface(@IMT);
  end;
end;

procedure TEnumerableBase.ToArray(var result: Pointer; typeInfo: Pointer; getCurrent: TGetCurrent);
var
  count, capacity, size: NativeInt;
  enumerator: IEnumerator;
begin
  count := 0;
  capacity := 0;
  size := PDynArrayTypeInfo(PByte(typeInfo) + Byte(PDynArrayTypeInfo(typeInfo).name)).elSize;
  enumerator := IEnumerable(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    if count >= capacity then
      capacity := DynArrayGrow(result, typeInfo, capacity);
    getCurrent(enumerator, PByte(result)[count*size]);
    Inc(count);
  end;
  DynArraySetLength(result, typeInfo, 1, @count);
end;

function TEnumerableBase.TryGetFirst(var value;
  getCurrent: TGetCurrent; default: TGetDefault): Boolean;
var
  enumerator: IEnumerator;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if enumerator.MoveNext then
  begin
    getCurrent(enumerator, value);
    Exit(True);
  end;
  default(value);
  Result := False;
end;

function TEnumerableBase.TryGetElementAt(var value; index: Integer;
  getCurrent: TGetCurrent; default: TGetDefault): Boolean;
var
  enumerator: IEnumerator;
begin
  if index >= 0 then
  begin
    enumerator := IEnumerable(this).GetEnumerator;
    while True do
    begin
      if not enumerator.MoveNext then
        Break;
      Dec(index);
      if index >= 0 then
        Continue;

      getCurrent(enumerator, value);
      Exit(True);
    end;
  end;
  default(value);
  Result := False;
end;

function TEnumerableBase.TryGetFirst(var value; const predicate: IInterface;
  getCurrent: TGetCurrentWithPredicate; default: TGetDefault): Boolean;
var
  enumerator: IEnumerator;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  enumerator := IEnumerable(this).GetEnumerator;
  while enumerator.MoveNext do
    if getCurrent(enumerator, predicate, value) then
      Exit(True);
  default(value);
  Result := False;
end;

function TEnumerableBase.TryGetLast(var value; getCurrent: TGetCurrent; default: TGetDefault): Boolean;
var
  enumerator: IEnumerator;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  if enumerator.MoveNext then
  begin
    repeat
      getCurrent(enumerator, value)
    until not enumerator.MoveNext;
    Exit(True);
  end;
  default(value);
  Result := False;
end;

function TEnumerableBase.TryGetLast(var value; const predicate: IInterface;
  getCurrent: TGetCurrentWithPredicate; default: TGetDefault): Boolean;
var
  enumerator: IEnumerator;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  enumerator := IEnumerable(this).GetEnumerator;
  Result := False;
  while enumerator.MoveNext do
    if getCurrent(enumerator, predicate, value) then
      Result := True;
  if not Result then
    default(value);
end;

function TEnumerableBase.TryGetSingle(var value; getCurrent: TGetCurrent; default: TGetDefault): Byte;
var
  enumerator: IEnumerator;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  Result := Ord(enumerator.MoveNext);
  if Result > 0 then
  begin
    getCurrent(enumerator, value);
    Inc(Result, Ord(enumerator.MoveNext));
  end;
  if Result <> 1 then
    default(value);
end;

function TEnumerableBase.TryGetSingle(var value; const predicate: IInterface;
  getCurrent: TGetCurrentWithPredicate; default: TGetDefault): Byte;
var
  enumerator: IEnumerator;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  enumerator := IEnumerable(this).GetEnumerator;
  Result := 0;
  if enumerator.MoveNext then
  begin
    repeat
      if getCurrent(enumerator, predicate, value) then
      begin
        Inc(Result);
        if Result > 1 then
          Break;
      end;
    until not enumerator.MoveNext;
  end;
  if Result <> 1 then
    default(value);
end;

function TEnumerableBase.TryGetSingleOrDefault(var value; getCurrent: TGetCurrent): Boolean;
var
  enumerator: IEnumerator;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  Result := enumerator.MoveNext;
  if Result then
  begin
    getCurrent(enumerator, value);
    if enumerator.MoveNext then
      RaiseHelper.MoreThanOneElement;
  end;
end;

function TEnumerableBase.TryGetSingleOrDefault(var value; const predicate: IInterface;
  getCurrent: TGetCurrentWithPredicate): Boolean;
var
  enumerator: IEnumerator;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  enumerator := IEnumerable(this).GetEnumerator;
  Result := False;
  while enumerator.MoveNext do
    if getCurrent(enumerator, predicate, value) then
    begin
      Result := not Result;
      if not Result then
        RaiseHelper.MoreThanOneMatch;
    end;
end;

procedure TEnumerableBase.Union(const second: IInterface; comparer: Pointer; var result; classType: TClass);
var
  elementType: PTypeInfo;
begin
  if not Assigned(second) then RaiseHelper.ArgumentNil(ExceptionArgument.second);

  if not Assigned(comparer) then
  begin
    elementType := IEnumerable(this).GetElementType;
    comparer := _LookupVtableInfo(giEqualityComparer, elementType, GetTypeSize(elementType));
  end;

  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.Union) do
  begin
    fPredicate := second;
    fEqualityComparer := IInterface(comparer);
    IInterface(result) := IInterface(@IMT);
  end;
end;

procedure TEnumerableBase.Where(const predicate: IInterface; var result; classType: TClass);
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.Where) do
  begin
    fPredicate := predicate;
    IInterface(result) := IInterface(@IMT);
  end;
end;

procedure TEnumerableBase.WhereIndex(const predicate: IInterface; var result; classType: TClass);
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  with TEnumerableExtension.Create(classType, IEnumerable(this), TExtensionKind.WhereIndex) do
  begin
    fPredicate := predicate;
    IInterface(result) := IInterface(@IMT);
  end;
end;

{$ENDREGION}


{$REGION 'TEnumerableBase<T>'}

{$IFDEF DELPHIXE7_UP}
class procedure TEnumerableBase<T>.__SuppressWarning(var value);
begin
  {$IFDEF CPUX86}{$IFDEF OPTIMIZATION_ON}
  // cause the compiler to omit push instruction for types that return in eax
  if GetTypeKind(T) in [tkInteger, tkChar, tkEnumeration, tkClass, tkWChar, tkClassRef, tkPointer, tkProcedure] then
    T(value) := Default(T);
  {$ENDIF}{$ENDIF}
end;
{$ENDIF}

procedure TEnumerableBase<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  if not Assigned(fComparer) then
    fComparer := IComparer<T>(_LookupVtableInfo(giComparer, GetElementType, SizeOf(T)));
end;

function TEnumerableBase<T>.Aggregate(const func: Func<T, T, T>): T; //FI:W521
begin
  inherited Aggregate(PInterface(@func)^, Result,
    TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.AggregateCurrentWithValue);
end;

function TEnumerableBase<T>.All(const predicate: Predicate<T>): Boolean;
begin
  Result := inherited All(PInterface(@predicate)^, TCollectionThunks<T, Boolean>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Any(const predicate: Predicate<T>): Boolean;
begin
  Result := Any(PInterface(@predicate)^, TCollectionThunks<T, Boolean>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Average: Double; //FI:W521
begin
  if TypeInfo(T) = TypeInfo(Integer) then
    Result := Average(nil, TCollectionThunks<Integer>.GetCurrentWithSelector)
  else if TypeInfo(T) = TypeInfo(Int64) then
    Result := Average(nil, TCollectionThunks<Int64>.GetCurrentWithSelector)
  else if TypeInfo(T) = TypeInfo(NativeInt) then
    {$IFDEF CPU32BITS}
    Result := Average(nil, TCollectionThunks<Integer>.GetCurrentWithSelector)
    {$ELSE}
    Result := Average(nil, TCollectionThunks<Int64>.GetCurrentWithSelector)
    {$ENDIF}
  else if TypeInfo(T) = TypeInfo(System.Single) then
    Result := Average(nil, TCollectionThunks<System.Single>.GetCurrentWithSelector)
  else if TypeInfo(T) = TypeInfo(Double) then
    Result := Average(nil, TCollectionThunks<Double>.GetCurrentWithSelector)
  else if TypeInfo(T) = TypeInfo(Currency) then
    Result := Average(nil, TCollectionThunks<Currency>.GetCurrentWithSelector)
  else
  begin
    // if T is not of any of the above types this way of calling Average is not supported
    // consider using Average with a selector function to turn the values
    // to any of the supported types for calculating the average
    RaiseHelper.NotSupported;
    __SuppressWarning(Result);
  end;
end;

function TEnumerableBase<T>.Average(const selector: Func<T, Integer>): Double;
begin
  Result := Average(PInterface(@selector)^, TCollectionThunks<T, Integer>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Average(const selector: Func<T, Int64>): Double;
begin
  Result := Average(PInterface(@selector)^, TCollectionThunks<T, Int64>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Average(const selector: Func<T, Single>): Double;
begin
  Result := Average(PInterface(@selector)^, TCollectionThunks<T, System.Single>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Average(const selector: Func<T, Double>): Double;
begin
  Result := Average(PInterface(@selector)^, TCollectionThunks<T, Double>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Average(const selector: Func<T, Currency>): Double;
begin
  Result := Average(PInterface(@selector)^, TCollectionThunks<T, Currency>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Concat(const second: IEnumerable<T>): IEnumerable<T>; //FI:W521
begin
  inherited Concat(second, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Contains(const value: T): Boolean;
begin
  Result := IEnumerable<T>(this).Contains(value, IEqualityComparer<T>(emptyComparer));
end;

function TEnumerableBase<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
begin
  Result := Contains(value, Pointer(comparer), TCollectionThunks<T>.EqualsCurrentWithValue);
end;

function TEnumerableBase<T>.Contains(const value: T;
  const comparer: TEqualityComparison<T>): Boolean;
begin
  Result := IEnumerable<T>(this).Contains(value, IEqualityComparer<T>(PPointer(@comparer)^));
end;

function TEnumerableBase<T>.CopyTo(var values: TArray<T>; index: Integer): Integer;
begin
  Result := inherited CopyTo(Pointer(values), index, SizeOf(T), TCollectionThunks<T>.GetCurrent);
end;

function TEnumerableBase<T>.DefaultIfEmpty: IEnumerable<T>; //FI:W521
begin
  inherited DefaultIfEmpty(nil, Result, TypeInfo(TArray<T>), TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.DefaultIfEmpty(const defaultValue: T): IEnumerable<T>; //FI:W521
begin
  inherited DefaultIfEmpty(@defaultValue, Result, TypeInfo(TArray<T>), TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Distinct: IEnumerable<T>; //FI:W521
begin
  inherited Distinct(nil, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Distinct(const comparer: IEqualityComparer<T>): IEnumerable<T>; //FI:W521
begin
  inherited Distinct(Pointer(comparer), Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.ElementAt(index: Integer): T;
begin
  if not IEnumerable<T>(this).TryGetElementAt(Result, index) then
    RaiseHelper.ArgumentOutOfRange_Index;
end;

function TEnumerableBase<T>.ElementAtOrDefault(index: Integer): T;
begin
  IEnumerable<T>(this).TryGetElementAt(Result, index);
end;

function TEnumerableBase<T>.ElementAtOrDefault(index: Integer;
  const defaultValue: T): T;
begin
  if not IEnumerable<T>(this).TryGetElementAt(Result, index) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.EqualsTo(const values: array of T): Boolean;
begin
  {$R-}
  Result := inherited EqualsTo(@values[0], High(values), TCollectionThunks<T>.EqualsCurrentWithArrayElement);
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

function TEnumerableBase<T>.EqualsTo(const values: IEnumerable<T>): Boolean;
begin
  Result := inherited EqualsTo(values, nil, TCollectionThunks<T>.EqualsCurrentWithOtherEnumerator);
end;

function TEnumerableBase<T>.EqualsTo(const values: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): Boolean;
begin
  Result := inherited EqualsTo(values, Pointer(comparer), TCollectionThunks<T>.EqualsCurrentWithOtherEnumerator);
end;

function TEnumerableBase<T>.Exclude(const second: IEnumerable<T>): IEnumerable<T>; //FI:W521
begin
  inherited Exclude(second, nil, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Exclude(const second: IEnumerable<T>; //FI:W521
  const comparer: IEqualityComparer<T>): IEnumerable<T>;
begin
  inherited Exclude(second, Pointer(comparer), Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.First: T;
begin
  if not IEnumerable<T>(this).TryGetFirst(Result) then
    RaiseHelper.NoElements;
end;

function TEnumerableBase<T>.First(const predicate: Predicate<T>): T;
begin
  if not IEnumerable<T>(this).TryGetFirst(Result, predicate) then
    RaiseHelper.NoMatch;
end;

function TEnumerableBase<T>.FirstOrDefault: T;
begin
  IEnumerable<T>(this).TryGetFirst(Result);
end;

function TEnumerableBase<T>.FirstOrDefault(const defaultValue: T): T;
begin
  if not IEnumerable<T>(this).TryGetFirst(Result) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.FirstOrDefault(const predicate: Predicate<T>): T;
begin
  IEnumerable<T>(this).TryGetFirst(Result, predicate);
end;

function TEnumerableBase<T>.FirstOrDefault(const predicate: Predicate<T>;
  const defaultValue: T): T;
begin
  if not IEnumerable<T>(this).TryGetFirst(Result, predicate) then
    Result := defaultValue;
end;

procedure TEnumerableBase<T>.ForEach(const action: Action<T>);
begin
  inherited ForEach(PInterface(@action)^, TCollectionThunks<T>.CallActionOnCurrent);
end;

function TEnumerableBase<T>.GetComparer: IComparer<T>;
begin
  Result := fComparer;
end;

function TEnumerableBase<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

function TEnumerableBase<T>.Intersect(const second: IEnumerable<T>): IEnumerable<T>; //FI:W521
begin
  inherited Intersect(second, nil, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Intersect(const second: IEnumerable<T>; //FI:W521
  const comparer: IEqualityComparer<T>): IEnumerable<T>;
begin
  inherited Intersect(second, Pointer(comparer), Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Last: T;
begin
  if not IEnumerable<T>(this).TryGetLast(Result) then
    RaiseHelper.NoElements;
end;

function TEnumerableBase<T>.Last(const predicate: Predicate<T>): T;
begin
  if not IEnumerable<T>(this).TryGetLast(Result, predicate) then
    RaiseHelper.NoMatch;
end;

function TEnumerableBase<T>.LastOrDefault: T;
begin
  IEnumerable<T>(this).TryGetLast(Result);
end;

function TEnumerableBase<T>.LastOrDefault(const defaultValue: T): T;
begin
  if not IEnumerable<T>(this).TryGetLast(Result) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.LastOrDefault(const predicate: Predicate<T>): T;
begin
  IEnumerable<T>(this).TryGetLast(Result, predicate);
end;

function TEnumerableBase<T>.LastOrDefault(const predicate: Predicate<T>;
  const defaultValue: T): T;
begin
  if not IEnumerable<T>(this).TryGetLast(Result, predicate) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.Max: T;
begin
  MaxMin(fComparer, Result, TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.GetCurrentIfGreaterThan);
end;

function TEnumerableBase<T>.Max(const comparer: IComparer<T>): T;
begin
  MaxMin(comparer, Result, TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.GetCurrentIfGreaterThan);
end;

function TEnumerableBase<T>.Max(const comparer: TComparison<T>): T;
begin
  MaxMin(PInterface(@comparer)^, Result, TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.GetCurrentIfGreaterThan);
end;

function TEnumerableBase<T>.Max(const selector: Func<T, Integer>): Integer;
begin
  Result := Max(PInterface(@selector)^, TCollectionThunks<T, Integer>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Max(const selector: Func<T, Int64>): Int64;
begin
  Result := Max(PInterface(@selector)^, TCollectionThunks<T, Int64>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Max(const selector: Func<T, Single>): Single;
begin
  Result := Max(PInterface(@selector)^, TCollectionThunks<T, System.Single>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Max(const selector: Func<T, Double>): Double;
begin
  Result := Max(PInterface(@selector)^, TCollectionThunks<T, Double>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Max(const selector: Func<T, Currency>): Currency;
begin
  Result := Max(PInterface(@selector)^, TCollectionThunks<T, Currency>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Min: T;
begin
  MaxMin(fComparer, Result, TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.GetCurrentIfLessThan);
end;

function TEnumerableBase<T>.Min(const comparer: IComparer<T>): T;
begin
  MaxMin(comparer, Result, TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.GetCurrentIfLessThan);
end;

function TEnumerableBase<T>.Min(const comparer: TComparison<T>): T;
begin
  MaxMin(PInterface(@comparer)^, Result, TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.GetCurrentIfLessThan);
end;

function TEnumerableBase<T>.Min(const selector: Func<T, Integer>): Integer;
begin
  Result := Min(PInterface(@selector)^, TCollectionThunks<T, Integer>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Min(const selector: Func<T, Int64>): Int64;
begin
  Result := Min(PInterface(@selector)^, TCollectionThunks<T, Int64>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Min(const selector: Func<T, Single>): Single;
begin
  Result := Min(PInterface(@selector)^, TCollectionThunks<T, System.Single>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Min(const selector: Func<T, Double>): Double;
begin
  Result := Min(PInterface(@selector)^, TCollectionThunks<T, Double>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Min(const selector: Func<T, Currency>): Currency;
begin
  Result := Min(PInterface(@selector)^, TCollectionThunks<T, Currency>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Memoize: IEnumerable<T>; //FI:W521
begin
  inherited Memoize(Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Ordered: IEnumerable<T>; //FI:W521
begin
  inherited Ordered(fComparer, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Ordered(const comparer: IComparer<T>): IEnumerable<T>; //FI:W521
begin
  inherited Ordered(comparer, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Ordered(const comparer: TComparison<T>): IEnumerable<T>; //FI:W521
begin
  inherited Ordered(PInterface(@comparer)^, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.QueryInterface(const IID: TGUID; out obj): HResult;
begin
  Result := inherited QueryInterface(IID, obj,
    TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.AddToCollection);
end;

function TEnumerableBase<T>.Reversed: IEnumerable<T>; //FI:W521
begin
  inherited Reversed(Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Shuffled: IEnumerable<T>; //FI:W521
begin
  inherited Shuffled(Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Single: T;
begin
  if not TryGetSingleOrDefault(Result, TCollectionThunks<T>.GetCurrent) then
    RaiseHelper.NoElements;
end;

function TEnumerableBase<T>.Single(const predicate: Predicate<T>): T;
begin
  if not TryGetSingleOrDefault(Result, PInterface(@predicate)^, TCollectionThunks<T>.GetCurrentWithPredicate) then
    RaiseHelper.NoMatch;
end;

function TEnumerableBase<T>.SingleOrDefault: T;
begin
  if not TryGetSingleOrDefault(Result, TCollectionThunks<T>.GetCurrent) then
    Result := Default(T);
end;

function TEnumerableBase<T>.SingleOrDefault(const defaultValue: T): T;
begin
  if not TryGetSingleOrDefault(Result, TCollectionThunks<T>.GetCurrent) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.SingleOrDefault(const predicate: Predicate<T>): T;
begin
  if not TryGetSingleOrDefault(Result, PInterface(@predicate)^, TCollectionThunks<T>.GetCurrentWithPredicate) then
    Result := Default(T);
end;

function TEnumerableBase<T>.SingleOrDefault(const predicate: Predicate<T>; const defaultValue: T): T;
begin
  if not TryGetSingleOrDefault(Result, PInterface(@predicate)^, TCollectionThunks<T>.GetCurrentWithPredicate) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.Skip(count: Integer): IEnumerable<T>; //FI:W521
begin
  inherited Skip(count, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.SkipLast(count: Integer): IEnumerable<T>; //FI:W521
begin
  inherited SkipLast(count, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.SkipWhile(const predicate: Predicate<T>): IEnumerable<T>; //FI:W521
begin
  inherited SkipWhile(PInterface(@predicate)^, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.SkipWhile(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; //FI:W521
begin
  inherited SkipWhileIndex(PInterface(@predicate)^, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Sum: T; //FI:W521
begin
  if TypeInfo(T) = TypeInfo(Integer) then
    PInteger(@Result)^ := Sum(nil, TCollectionThunks<Integer>.GetCurrentWithSelector)
  else if TypeInfo(T) = TypeInfo(Int64) then
    PInt64(@Result)^ := Sum(nil, TCollectionThunks<Int64>.GetCurrentWithSelector)
  else if TypeInfo(T) = TypeInfo(NativeInt) then
    {$IFDEF CPU32BITS}
    PInteger(@Result)^ := Sum(nil, TCollectionThunks<Integer>.GetCurrentWithSelector)
    {$ELSE}
    PInt64(@Result)^ := Sum(nil, TCollectionThunks<Int64>.GetCurrentWithSelector)
    {$ENDIF}
  else if TypeInfo(T) = TypeInfo(System.Single) then
    PSingle(@Result)^ := Sum(nil, TCollectionThunks<System.Single>.GetCurrentWithSelector)
  else if TypeInfo(T) = TypeInfo(Double) then
    PDouble(@Result)^ := Sum(nil, TCollectionThunks<Double>.GetCurrentWithSelector)
  else if TypeInfo(T) = TypeInfo(Currency) then
    PCurrency(@Result)^ := Sum(nil, TCollectionThunks<Currency>.GetCurrentWithSelector)
  else
    // if T is not of any of the above types this way of calling Sum is not supported
    // consider using Sum with a selector function to turn the values
    // to any of the supported types for calculating the sum
    RaiseHelper.NotSupported;
end;

function TEnumerableBase<T>.Sum(const selector: Func<T, Integer>): Integer;
begin
  Result := Sum(PInterface(@selector)^, TCollectionThunks<T, Integer>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Sum(const selector: Func<T, Int64>): Int64;
begin
  Result := Sum(PInterface(@selector)^, TCollectionThunks<T, Int64>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Sum(const selector: Func<T, Single>): Single;
begin
  Result := Sum(PInterface(@selector)^, TCollectionThunks<T, System.Single>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Sum(const selector: Func<T, Double>): Double;
begin
  Result := Sum(PInterface(@selector)^, TCollectionThunks<T, Double>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Sum(const selector: Func<T, Currency>): Currency;
begin
  Result := Sum(PInterface(@selector)^, TCollectionThunks<T, Currency>.GetCurrentWithSelector);
end;

function TEnumerableBase<T>.Take(count: Integer): IEnumerable<T>; //FI:W521
begin
  inherited Take(count, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.TakeLast(count: Integer): IEnumerable<T>; //FI:W521
begin
  inherited TakeLast(count, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.TakeWhile(const predicate: Predicate<T>): IEnumerable<T>; //FI:W521
begin
  inherited TakeWhile(PInterface(@predicate)^, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.TakeWhile(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; //FI:W521
begin
  inherited TakeWhileIndex(PInterface(@predicate)^, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.ToArray: TArray<T>; //FI:W521
begin
  inherited ToArray(Pointer(Result), TypeInfo(TArray<T>), TCollectionThunks<T>.GetCurrent);
end;

function TEnumerableBase<T>.TryGetElementAt(var value: T; index: Integer): Boolean;
begin
  Result := inherited TryGetElementAt(value, index, TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.GetDefault);
end;

function TEnumerableBase<T>.TryGetFirst(var value: T): Boolean;
begin
  Result := TryGetFirst(value, TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.GetDefault);
end;

function TEnumerableBase<T>.TryGetFirst(var value: T; const predicate: Predicate<T>): Boolean;
begin
  Result := TryGetFirst(value, PInterface(@predicate)^, TCollectionThunks<T>.GetCurrentWithPredicate, TCollectionThunks<T>.GetDefault);
end;

function TEnumerableBase<T>.TryGetLast(var value: T): Boolean;
begin
  Result := TryGetLast(value, TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.GetDefault);
end;

function TEnumerableBase<T>.TryGetLast(var value: T; const predicate: Predicate<T>): Boolean;
begin
  Result := TryGetLast(value, PInterface(@predicate)^, TCollectionThunks<T>.GetCurrentWithPredicate, TCollectionThunks<T>.GetDefault);
end;

function TEnumerableBase<T>.TryGetSingle(var value: T): Boolean;
begin
  Result := TryGetSingle(value, TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.GetDefault) = 1;
end;

function TEnumerableBase<T>.TryGetSingle(var value: T;
  const predicate: Predicate<T>): Boolean;
begin
  Result := TryGetSingle(value, PInterface(@predicate)^, TCollectionThunks<T>.GetCurrentWithPredicate, TCollectionThunks<T>.GetDefault) = 1;
end;

function TEnumerableBase<T>.Union(const second: IEnumerable<T>): IEnumerable<T>; //FI:W521
begin
  inherited Union(second, nil, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Union(const second: IEnumerable<T>; //FI:W521
  const comparer: IEqualityComparer<T>): IEnumerable<T>;
begin
  inherited Union(second, Pointer(comparer), Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Where(const predicate: Predicate<T>): IEnumerable<T>;
begin
  Where(PInterface(@predicate)^, Result, TEnumerableExtension<T>);
end;

function TEnumerableBase<T>.Where(
  const predicate: Func<T, Integer, Boolean>): IEnumerable<T>;
begin
  WhereIndex(PInterface(@predicate)^, Result, TEnumerableExtension<T>);
end;

{$ENDREGION}


{$REGION 'TEnumerableWrapper'}

constructor TEnumerableWrapper.Create(const source: IEnumerable; //FI:W525
  getCurrent: TGetCurrent);
begin
  fSource := source;
  fElementType := source.ElementType;
  fGetCurrent := getCurrent;
end;

function TEnumerableWrapper.AsObject: TObject;
begin
  Result := fSource.AsObject;
end;

function TEnumerableWrapper.GetCount: Integer;
begin
  Result := fSource.Count;
end;

function TEnumerableWrapper.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TEnumerableWrapper.GetEnumerator: IEnumerator;
begin
  Result := TEnumerator.Create(fSource.GetEnumerator, fElementType, fGetCurrent);
end;

function TEnumerableWrapper.GetIsEmpty: Boolean;
begin
  Result := fSource.IsEmpty;
end;

function TEnumerableWrapper.GetNonEnumeratedCount: Integer;
begin
  Result := fSource.GetNonEnumeratedCount;
end;

function TEnumerableWrapper.QueryInterface(const IID: TGUID; out obj): HResult;
begin
  Result := inherited QueryInterface(IID, obj);
  if Result <> S_OK then
    Result := fSource.QueryInterface(IID, obj);
end;

{$ENDREGION}


{$REGION 'TEnumerableWrapper.TEnumerator'}

constructor TEnumerableWrapper.TEnumerator.Create(const source: IEnumerator; //FI:W525
  elementType: PTypeInfo; getCurrent: TGetCurrent);
begin
  fSource := source;
  fElementType := elementType;
  fGetCurrent := getCurrent;
end;

function TEnumerableWrapper.TEnumerator.GetCurrent: Spring.TValue;
begin
  Spring.TValue.Make(nil, fElementType, Result);
  fGetCurrent(fSource, Result.GetReferenceToRawData^);
end;

function TEnumerableWrapper.TEnumerator.MoveNext: Boolean;
begin
  Result := fSource.MoveNext;
end;

{$ENDREGION}


{$REGION 'TCollectionWrapper'}

constructor TCollectionWrapper.Create(const source: IEnumerable; //FI:W525
  getCurrent: TGetCurrent; add: TAddToCollection);
begin
  fSource := source;
  fElementType := source.ElementType;
  fGetCurrent := getCurrent;
  fAdd := add;
end;

function TCollectionWrapper.Add(const item: Spring.TValue): Boolean;
var
  value: Spring.TValue;
begin
  if item.TypeInfo = fElementType then
    Result := fAdd(fSource, item.GetReferenceToRawData^)
  else if item.TryConvert(fElementType, value) then
    Result := fAdd(fSource, value.GetReferenceToRawData^)
  else
    Result := Guard.RaiseInvalidTypeCast(value.TypeInfo, fElementType);
end;

procedure TCollectionWrapper.Clear;
begin
  // Clear does not have any generic parameter so we can hardcast fSource
  // which is an ICollection<T> to access it
  ICollection<Integer>(fSource).Clear;
end;

{$ENDREGION}


{$REGION 'TIterator<T>'}

procedure TIterator<T>.AfterConstruction;
var
  method: function (var current: T): Boolean of object;
begin
  inherited AfterConstruction;
  fState := STATE_INITIAL;
  fThreadId := GetCurrentThreadId;
  method := TryMoveNext;
  fTryMoveNext := TMethod(method).Code;
end;

procedure TIterator<T>.Dispose;
begin
end;

function TIterator<T>.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TIterator<T>.GetEnumerator: IEnumerator<T>;
var
  enumerator: TIterator<T>;
begin
  if (fState = STATE_INITIAL) and (fThreadId = GetCurrentThreadId) then
    enumerator := Self
  else
    enumerator := Clone;
  enumerator.fState := STATE_ENUMERATOR;
  Result := enumerator;
end;

function TIterator<T>.MoveNext: Boolean;
label
  _STATE_RUNNING;
begin
  repeat
    case fState of
      STATE_RUNNING:
      _STATE_RUNNING:
      begin
        Result := fTryMoveNext(Self, fCurrent);
        if Result then
          Exit;

        Dispose;
        fCurrent := Default(T);
        fState := STATE_FINISHED;
      end;
      STATE_ENUMERATOR:
      begin
        Start;
        fState := STATE_RUNNING;
        Continue;
      end;
    end;
    Exit(False);
  until False;
end;

procedure TIterator<T>.Start;
begin
end;

{$ENDREGION}


{$REGION 'TSourceIterator<T>'}

constructor TSourceIterator<T>.Create(const source: IEnumerable<T>); //FI:W525
begin
  AssignComparer(fComparer, source);
  fSource := source;
end;

function TSourceIterator<T>.GetElementType: PTypeInfo;
begin
  Result := fSource.ElementType;
end;

{$ENDREGION}


{$REGION 'TCollectionBase<T>'}

procedure TCollectionBase<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
  fOnChanged.OnChanged := EventChanged;
  UpdateNotify(Self, TCollectionBase<T>, fOnChanged);
end;

procedure TCollectionBase<T>.BeforeDestruction;
begin
  fOnChanged.Free;
  inherited BeforeDestruction;
end;

procedure TCollectionBase<T>.EventChanged(Sender: TObject);
begin
  UpdateNotify(Self, TCollectionBase<T>, fOnChanged);
end;

procedure TCollectionBase<T>.DoNotify(const item: T;
  action: TCollectionChangedAction);
begin
  if Assigned(Notify) then
    Notify(Self, item, action);
end;

function TCollectionBase<T>.Add(const item: T): Boolean;
begin
  // only for usage with implementing IList<T>
  IList<T>(this).Add(item);
  Result := True;
end;

procedure TCollectionBase<T>.AddRange(const values: array of T);
var
  i: Integer;
begin
  for i := 0 to High(values) do
    ICollection<T>(this).Add(values[i]);
end;

procedure TCollectionBase<T>.AddRange(const values: IEnumerable<T>);
begin
  inherited ForEach(values, TCollectionThunks<T>.AddCurrentToCollection);
end;

procedure TCollectionBase<T>.Changed(const item: T; action: TCollectionChangedAction);
begin
  if Assigned(fOnChanged) and fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, action);
end;

function TCollectionBase<T>.ExtractAll(const match: Predicate<T>): TArray<T>; //FI:W521
begin
  inherited ExtractAll(PInterface(@match)^, Result);
end;

procedure TCollectionBase<T>.ExtractRange(const values: array of T);
var
  i: Integer;
begin
  for i := 0 to High(values) do
    ICollection<T>(this).Extract(values[i]);
end;

procedure TCollectionBase<T>.ExtractRange(const values: IEnumerable<T>);
begin
  inherited ForEach(values, TCollectionThunks<T>.ExtractCurrentFromCollection);
end;

function TCollectionBase<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

function TCollectionBase<T>.MoveTo(const collection: ICollection<T>): Integer;
begin
  Result := inherited MoveTo(collection, TypeInfo(TArray<T>));
end;

function TCollectionBase<T>.MoveTo(const collection: ICollection<T>;
  const predicate: Predicate<T>): Integer;
begin
  Result := inherited MoveTo(collection, PInterface(@predicate)^, TypeInfo(TArray<T>));
end;

function TCollectionBase<T>.RemoveAll(const match: Predicate<T>): Integer;
begin
  Result := inherited RemoveAll(PInterface(@match)^, TypeInfo(TArray<T>));
end;

function TCollectionBase<T>.RemoveRange(const values: array of T): Integer;
var
  this: Pointer;
  i: Integer;
begin
  this := Self.this;
  Result := 0;
  for i := 0 to High(values) do
    Inc(Result, Byte(ICollection<T>(this).Remove(values[i])));
end;

function TCollectionBase<T>.RemoveRange(const values: IEnumerable<T>): Integer;
begin
  Result := inherited ForEach(values, TCollectionThunks<T>.RemoveCurrentFromCollection);
end;

procedure TCollectionBase<T>.Reset;
var
  defaultValue: T;
begin
  if Assigned(Notify) then
  begin
    defaultValue := Default(T);
    Notify(Self, defaultValue, caReset);
  end;
end;

{$ENDREGION}


{$REGION 'THashMapInnerCollection'}

class function THashMapInnerCollection.Create(classType: TClass;
  source: TRefCountedObject; hashTable: PHashTable;
  const valueComparer: IInterface; elementType: PTypeInfo;
  offset: Integer; count: PInteger; contains: TContains): THashMapInnerCollection;
begin
  Result := THashMapInnerCollection(classType.NewInstance);
  Result.fSource := source;
  Result.fElementType := elementType;
  Result.fHashTable := hashTable;
  Result.fValueComparer := valueComparer;
  Result.fOffset := KeyOffset + offset;
  Result.fCount := count;
  Result.fContains := contains;
  Result.AfterConstruction;
end;

class function THashMapInnerCollection.Create_Interface(
  source: TRefCountedObject; hashTable: PHashTable;
  const valueComparer: IInterface; elementType: PTypeInfo; offset: Integer;
  count: PInteger; contains: TContains): THashMapInnerCollection;
begin
  Result := Create(THashMapInnerCollection<IInterface>,
    source, hashTable, valueComparer, elementType, offset, count, contains);
end;

class function THashMapInnerCollection.Create_Object(source: TRefCountedObject;
  hashTable: PHashTable; const valueComparer: IInterface;
  elementType: PTypeInfo; offset: Integer; count: PInteger;
  contains: TContains): THashMapInnerCollection;
begin
  Result := Create(THashMapInnerCollection<TObject>,
    source, hashTable, valueComparer, elementType, offset, count, contains);
end;

function THashMapInnerCollection.Contains(const value; comparer: Pointer; equals: TEqualsMethod): Boolean;
var
  hashTable: PHashTable;
  item: PByte;
  itemCount, itemSize, offset: NativeInt;
begin
  if fOffset = KeyOffset then // means this is for the key
  begin
    item := fHashTable.FindItem(value);
    Result := Assigned(item);
  end
  else if not Assigned(fContains) then
  begin
    offset := fOffset;
    hashTable := fHashTable;
    item := hashTable.Items;
    itemCount := hashTable.ItemCount;
    itemSize := hashTable.ItemSize;
    if comparer = nil then
      comparer := Pointer(fValueComparer);
    while itemCount > 0 do
    begin
      if PInteger(item)^ >= 0 then
        if equals(comparer, item[offset], value) then
          Exit(True);
      Inc(item, itemSize);
      Dec(itemCount);
    end;
    Result := False;
  end
  else
  begin
    hashTable := fHashTable;
    item := hashTable.Items;
    itemSize := hashTable.ItemSize;
    itemCount := hashTable.ItemCount;
    while itemCount > 0 do
    begin
      if PInteger(item)^ >= 0 then
        if fContains(PInterface(@item[itemSize - SizeOf(Pointer)])^, value, IInterface(comparer)) then
          Exit(True);
      Inc(item, itemSize);
      Dec(itemCount);
    end;
    Result := False;
  end;
end;

function THashMapInnerCollection.GetCount: Integer;
begin
  if Assigned(fCount) then
    Result := fCount^
  else
    Result := fHashTable.Count;
end;

procedure THashMapInnerCollection.ToArray(var result: Pointer; typeInfo: Pointer; assign: TAssign);
var
  hashTable: PHashTable;
  source, target: PByte;
  i, itemSize, targetIndex, offset, count, elSize: NativeInt;
  collection: Pointer;
begin
  if fCount = nil then
  begin
    offset := fOffset;
    hashTable := fHashTable;
    count := hashTable.Count;
    elSize := PDynArrayTypeInfo(PByte(typeInfo) + Byte(PDynArrayTypeInfo(typeInfo).name)).elSize;
    DynArraySetLength(result, typeInfo, 1, @count);
    target := result;
    if target = nil then Exit;
    source := hashTable.Items;
    itemSize := hashTable.ItemSize;
    for i := 1 to hashTable.ItemCount do
    begin
      if PInteger(source)^ >= 0 then
      begin
        assign(target^, source[offset]);
        Inc(target, elSize);
      end;
      Inc(source, itemSize);
    end;
  end
  else
  begin
    hashTable := fHashTable;
    count := fCount^;
    DynArraySetLength(result, typeInfo, 1, @count);
    if result = nil then Exit;
    source := hashTable.Items;
    itemSize := hashTable.ItemSize;
    targetIndex := 0;
    for i := 1 to hashTable.ItemCount do
    begin
      if PInteger(source)^ >= 0 then
      begin
        collection := PPointer(source + itemSize - SizeOf(Pointer))^;
        // little hack - we can hardcast here since we are just passing along a dynamic array
        Inc(targetIndex, ICollection<Pointer>(collection).CopyTo(TArray<Pointer>(result), targetIndex));
      end;
      Inc(source, itemSize);
    end;
  end;
end;

function THashMapInnerCollection.TryGetElementAt(var value; index: Integer;
  assign: TAssign; default: TGetDefault; getCurrent: TGetCurrent): Boolean;
begin
  if fCount = nil then
  begin
    if Cardinal(index) < Cardinal(fHashTable.Count) then
    begin
      fHashTable.EnsureCompact;
      assign(value, fHashTable.Items[fHashTable.ItemSize * index + fOffset]);
      Result := True;
    end
    else
    begin
      default(value);
      Result := False;
    end;
  end
  else
    Result := TEnumerableBase(Self).TryGetElementAt(value, index, getCurrent, default);
end;

{$ENDREGION}


{$REGION 'THashMapInnerCollection.TEnumerator'}

procedure THashMapInnerCollection.TEnumerator.Assign(
  source: THashMapInnerCollection);
begin
  source.fSource._AddRef;
  Parent := source.fSource;
  fHashTable := source.fHashTable;
  fOffset := source.fOffset;
  fVersion := source.fHashTable.Version;
end;

function THashMapInnerCollection.TEnumerator.MoveNext: Boolean;
var
  hashTable: PHashTable;
  item: PByte;
  offset: NativeInt;
begin
  offset := fOffset;
  hashTable := fHashTable;
  if fVersion = hashTable.Version then
  begin
    repeat
      if fIndex >= hashTable.ItemCount then
        Break;

      item := @hashTable.Items[fIndex * hashTable.ItemSize];
      Inc(fIndex);
      if PInteger(item)^ >= 0 then
      begin
        fItem := @item[offset];
        Exit(True);
      end;
    until False;
    Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

function THashMapInnerCollection.TEnumerator.MoveNext_MultiMap: Boolean; //FI:W521
var
  hashTable: PHashTable;
  item: PByte;
  offset: NativeInt;
  values: Pointer;
begin
  offset := fOffset;
  hashTable := fHashTable;
  if fVersion = hashTable.Version then
  begin
    repeat
      if Assigned(fEnumerator) then
      begin
        Result := fEnumerator.MoveNext;
        if Result then Exit;
      end;

      repeat
        if fIndex < hashTable.ItemCount then
        begin
          item := @hashTable.Items[fIndex * hashTable.ItemSize];
          Inc(fIndex);
          if PInteger(item)^ >= 0 then
          begin
            fItem := item;
            values := PPointer(@item[offset])^;
            {$IFDEF MSWINDOWS}
            IEnumerableInternal(values).GetEnumerator(fEnumerator);
            {$ELSE}
            fEnumerator := IEnumerable(values).GetEnumerator;
            {$ENDIF}
            Break;
          end;
        end
        else
        begin
          fEnumerator := nil;
          Exit(False);
        end;
      until False;
    until False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'THashMapInnerCollection<T>'}

function THashMapInnerCollection<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
begin
  Result := THashMapInnerCollection(Self).Contains(value, Pointer(comparer), TComparerThunks<T>.Equals);
end;

function THashMapInnerCollection<T>.GetCount: Integer;
begin
  Result := THashMapInnerCollection(Self).GetCount;
end;

function THashMapInnerCollection<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function THashMapInnerCollection<T>.GetEnumerator: IEnumerator<T>; //FI:W521
begin
  if Assigned(fContains) then
    THashMapInnerCollection.PEnumerator(TEnumeratorBlock.Create(@Result,
      @TEnumerator.Enumerator_Vtable_Values, TypeInfo(TEnumerator),
      @TEnumerator.GetCurrent_Values,
      @THashMapInnerCollection.TEnumerator.MoveNext_MultiMap)).Assign(THashMapInnerCollection(Self))
  else
    THashMapInnerCollection.PEnumerator(TEnumeratorBlock.Create(@Result,
      @TEnumerator.Enumerator_Vtable, TypeInfo(TEnumerator),
      @TEnumerator.GetCurrent,
      @THashMapInnerCollection.TEnumerator.MoveNext)).Assign(THashMapInnerCollection(Self));
end;

function THashMapInnerCollection<T>.GetNonEnumeratedCount: Integer;
begin
  Result := THashMapInnerCollection(Self).GetCount;
end;

function THashMapInnerCollection<T>.ToArray: TArray<T>;
begin
  THashMapInnerCollection(Self).ToArray(Pointer(Result), TypeInfo(TArray<T>), TCollectionThunks<T>.Assign);
end;

function THashMapInnerCollection<T>.TryGetElementAt(var value: T; index: Integer): Boolean;
begin
  Result := THashMapInnerCollection(Self).TryGetElementAt(value, index,
    TCollectionThunks<T>.Assign, TCollectionThunks<T>.GetDefault, TCollectionThunks<T>.GetCurrent);
end;

function THashMapInnerCollection<T>._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function THashMapInnerCollection<T>._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'THashMapInnerCollection<T>.TEnumerator'}

function THashMapInnerCollection<T>.TEnumerator.GetCurrent: T;
begin
  Result := fItem^;
end;

function THashMapInnerCollection<T>.TEnumerator.GetCurrent_Values: T;
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(fEnumerator).GetCurrent(Result)
  else
  {$ENDIF}
  Result := fEnumerator.Current;
end;

{$ENDREGION}


{$REGION 'TTreeMapInnerCollection'}

class function TTreeMapInnerCollection.Create(classType: TClass;
  source: TRefCountedObject; tree: TBinaryTree; version: PInteger;
  const valueComparer: IInterface; elementType: PTypeInfo;
  offset: Integer; count: PInteger; contains: TContains): TTreeMapInnerCollection;
begin
  Result := TTreeMapInnerCollection(classType.NewInstance);
  Result.fComparer := tree.Comparer;
  Result.fSource := source;
  Result.fElementType := elementType;
  Result.fTree := tree;
  Result.fVersion := version;
  Result.fValueComparer := valueComparer;
  Result.fOffset := offset;
  Result.fCount := count;
  Result.fContains := contains;
  Result.AfterConstruction;
end;

class function TTreeMapInnerCollection.Create_Interface(
  source: TRefCountedObject; tree: TBinaryTree; version: PInteger;
  const valueComparer: IInterface; elementType: PTypeInfo; offset: Integer;
  count: PInteger; contains: TContains): TTreeMapInnerCollection;
begin
  Result := Create(TTreeMapInnerCollection<IInterface>,
    source, tree, version, valueComparer, elementType, offset, count, contains);
end;

class function TTreeMapInnerCollection.Create_Object(source: TRefCountedObject;
  tree: TBinaryTree; version: PInteger; const valueComparer: IInterface;
  elementType: PTypeInfo; offset: Integer; count: PInteger;
  contains: TContains): TTreeMapInnerCollection;
begin
  Result := Create(TTreeMapInnerCollection<TObject>,
    source, tree, version, valueComparer, elementType, offset, count, contains);
end;

function TTreeMapInnerCollection.Contains(const value; comparer: Pointer;
  compare: TCompareMethod; equals: TEqualsMethod): Boolean;
var
  root, temp, next: Pointer;
  node: PNode;
  compareResult: Integer;
  i, offset: NativeInt;
begin
  if (fOffset = 0) and (comparer = nil) then
  begin
    comparer := Pointer(fTree.Comparer);
    root := fTree.Root;
    if not Assigned(root) then Exit(Boolean(root));
    node := root;
    repeat
      compareResult := compare(comparer, PNode(node).Key, value);
      if compareResult = 0 then Break;
      i := compareResult shr 31; // left -> 0, right -> 1
      node := PNode(node).Childs[i];
    until not Assigned(node);
    Result := Assigned(node);
  end
  else if fCount = nil then
  begin
    if comparer = nil then
      comparer := Pointer(fValueComparer);
    temp := fTree.Root.LeftMost;
    if not Assigned(temp) then Exit(Boolean(temp));
    repeat
      node := temp;
      Result := equals(comparer, PByte(@node.Key)[fOffset], value);
      if Result then
        Exit;
      temp := PBinaryTreeNode(node).Next;
    until not Assigned(temp);
    Result := Boolean(temp);
  end
  else
  begin
    offset := fOffset;
    next := fTree.Root.LeftMost;
    if not Assigned(next) then Exit(Boolean(next));
    repeat
      node := next;
      Result := fContains(PInterface(@PByte(@node.Key)[offset])^, value, IInterface(comparer));
      if Result then Exit;
      next := PBinaryTreeNode(node).Next
    until not Assigned(next);
    Result := Boolean(next);
  end;
end;

function TTreeMapInnerCollection.GetCount: Integer;
begin
  if Assigned(fCount) then
    Result := fCount^
  else
    Result := fTree.Count;
end;

procedure TTreeMapInnerCollection.ToArray(var result: Pointer; typeInfo: Pointer; assign: TAssign);
var
  tree: TBinaryTree;
  node: PNode;
  count, elSize, i, offset: NativeInt;
  next, collection: Pointer;
begin
  if fCount = nil then
  begin
    offset := fOffset;
    tree := fTree;
    count := tree.Count;
    elSize := PDynArrayTypeInfo(PByte(typeInfo) + Byte(PDynArrayTypeInfo(typeInfo).name)).elSize;
    DynArraySetLength(result, typeInfo, 1, @count);
    next := tree.Root.LeftMost;
    if Assigned(next) then
    begin
      i := 0;
      repeat
        node := next;
        assign(PByte(result)[i * elSize], PByte(@node.Key)[offset]);
        Inc(i);
        next := PBinaryTreeNode(node).Next;
      until not Assigned(next);
    end;
  end
  else
  begin
    count := fCount^;
    DynArraySetLength(result, typeInfo, 1, @count);
    offset := fOffset;
    next := fTree.Root.LeftMost;
    if Assigned(next) then
    begin
      i := 0;
      repeat
        node := next;
        collection := PPointer(@PByte(@node.Key)[offset])^;
        {$R-}
        Inc(i, ICollection<Pointer>(collection).CopyTo(TArray<Pointer>(result), i));
        {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
        next := PBinaryTreeNode(node).Next;
      until not Assigned(next);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TTreeMapInnerCollection.TEnumerator'}

function TTreeMapInnerCollection.TEnumerator.MoveNext: Boolean;
var
  tree: TBinaryTree;
  node: PBinaryTreeNode;
  offset: NativeInt;
begin
  offset := fOffset;
  tree := fTree;
  if fVersion = fSourceVersion^ then
  begin
    if (tree.Count > 0) and (fNode <> Pointer(1)) then
    begin
      if Assigned(fNode) then
        node := fNode.Next
      else
        node := tree.Root.LeftMost;
      if Assigned(node) then
      begin
        fNode := node;
        fItem := @PByte(@PNode(node).Key)[offset];
        Exit(True);
      end;
    end;

    fNode := Pointer(1);
    Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

function TTreeMapInnerCollection.TEnumerator.MoveNext_MultiMap: Boolean;
var
  tree: TBinaryTree;
  node: PBinaryTreeNode;
  offset: NativeInt;
  values: Pointer;
begin
  offset := fOffset;
  tree := fTree;
  if fVersion = fSourceVersion^ then
  begin
    repeat
      if Assigned(fEnumerator) then
      begin
        Result := fEnumerator.MoveNext;
        if Result then Exit;
      end;

      if (tree.Count > 0) and (fNode <> Pointer(1)) then
      begin
        if Assigned(fNode) then
          node := fNode.Next
        else
          node := tree.Root.LeftMost;
        if Assigned(node) then
        begin
          fNode := node;
          values := PPointer(@PByte(@PNode(node).Key)[offset])^;
          {$IFDEF MSWINDOWS}
          IEnumerableInternal(values).GetEnumerator(fEnumerator);
          {$ELSE}
          fEnumerator := IEnumerable(values).GetEnumerator;
          {$ENDIF}
          Continue;
        end;
      end;

      fNode := Pointer(1);
      fEnumerator := nil;
      Exit(False);
    until False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;


{$ENDREGION}


{$REGION 'TTreeMapInnerCollection<T>'}

function TTreeMapInnerCollection<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
begin
  Result := TTreeMapInnerCollection(Self).Contains(value, Pointer(comparer),
    TComparerThunks<T>.Compare, TComparerThunks<T>.Equals);
end;

function TTreeMapInnerCollection<T>.GetCount: Integer;
begin
  Result := TTreeMapInnerCollection(Self).GetCount;
end;

function TTreeMapInnerCollection<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TTreeMapInnerCollection<T>.GetEnumerator: IEnumerator<T>; //FI:W521
var
  enumerator: PEnumerator;
begin
  _AddRef;
  if Assigned(fContains) then
    enumerator := TEnumeratorBlock.Create(@Result,
      @TEnumerator.Enumerator_Vtable_Values, TypeInfo(TEnumerator),
      @TEnumerator.GetCurrent_Values,
      @TTreeMapInnerCollection.TEnumerator.MoveNext_MultiMap)
  else
    enumerator := TEnumeratorBlock.Create(@Result,
      @TEnumerator.Enumerator_Vtable, TypeInfo(TEnumerator),
      @TEnumerator.GetCurrent,
      @TTreeMapInnerCollection.TEnumerator.MoveNext);

  with enumerator^ do
  begin
    Parent := Self.fSource;
    fTree := Self.fTree;
    fOffset := Self.fOffset;
    fSourceVersion := Self.fVersion;
    fVersion := Self.fVersion^;
  end;
end;

function TTreeMapInnerCollection<T>.GetNonEnumeratedCount: Integer;
begin
  Result := TTreeMapInnerCollection(Self).GetCount;
end;

function TTreeMapInnerCollection<T>.ToArray: TArray<T>;
begin
  TTreeMapInnerCollection(Self).ToArray(Pointer(Result), TypeInfo(TArray<T>), TCollectionThunks<T>.Assign);
end;

function TTreeMapInnerCollection<T>._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TTreeMapInnerCollection<T>._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TTreeMapInnerCollection<T>.TEnumerator'}

function TTreeMapInnerCollection<T>.TEnumerator.GetCurrent: T;
begin
  Result := fItem^;
end;

function TTreeMapInnerCollection<T>.TEnumerator.GetCurrent_Values: T;
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(fEnumerator).GetCurrent(Result)
  else
  {$ENDIF}
  Result := fEnumerator.Current;
end;


{$ENDREGION}


{$REGION 'TCircularArrayBuffer<T>'}

constructor TCircularArrayBuffer<T>.Create(capacity: Integer; ownsObjects: Boolean); //FI:W525
begin
  SetCapacity(capacity);
  SetOwnsObjects(ownsObjects);
end;

procedure TCircularArrayBuffer<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
end;

procedure TCircularArrayBuffer<T>.BeforeDestruction;
begin
  Clear;
  fOnChanged.Free;
end;

procedure TCircularArrayBuffer<T>.Clear;
var
  i: Integer;
begin
  for i := Count downto 1 do //FI:W528 // TODO: evaluate possible optimization
    DeleteFromHead(caRemoved); //FI:O805
end;

function TCircularArrayBuffer<T>.GetCapacity: Integer;
begin
  Result := fCapacity;
end;

function TCircularArrayBuffer<T>.GetCount: Integer;
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) <> tkClass then
    Result := fCount
  else
  {$ENDIF}
  Result := fCount and CountMask;
end;

function TCircularArrayBuffer<T>.GetEnumerator: IEnumerator<T>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fCount := Self.Count;
    fIndex := Self.fHead - 1;
    fVersion := Self.fVersion;
  end;
end;

function TCircularArrayBuffer<T>.GetNonEnumeratedCount: Integer;
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) <> tkClass then
    Result := fCount
  else
  {$ENDIF}
  Result := fCount and CountMask;
end;

function TCircularArrayBuffer<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

function TCircularArrayBuffer<T>.GetOwnsObjects: Boolean;
begin
  Result := {$IFDEF DELPHIXE7_UP}(GetTypeKind(T) = tkClass) and {$ENDIF}(fCount < 0);
end;

function TCircularArrayBuffer<T>.GetTail: Integer;
var
  index: Integer;
begin
  index := fTail;
  if index = 0 then
    index := fCapacity;
  Result := index - 1;
end;

procedure TCircularArrayBuffer<T>.AddToHead(const item: T);
var
  index: Integer;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Inc(fCount);

  index := fHead;
  if index = 0 then
    index := fCapacity;
  Dec(index);
  fItems[index] := item;
  fHead := index;

  if Assigned(fOnChanged) and fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, caAdded);
end;

procedure TCircularArrayBuffer<T>.AddToTail(const item: T);
var
  index: Integer;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Inc(fCount);

  index := fTail;
  fItems[index] := item;
  Inc(index);
  if index = fCapacity then
    index := 0;
  fTail := index;

  if Assigned(fOnChanged) and fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, caAdded);
end;

procedure TCircularArrayBuffer<T>.DeleteFromHead(action: TCollectionChangedAction);
var
  index: Integer;
  item: PT;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);

  index := fHead;
  item := @fItems[index];
  Inc(index);
  if index = fCapacity then
    index := 0;
  fHead := index;

  if Assigned(fOnChanged) and fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item^, action);
  if OwnsObjects and (action = caRemoved) then
    PObject(item).Free;
  item^ := Default(T);
end;

procedure TCircularArrayBuffer<T>.DeleteFromTail(action: TCollectionChangedAction);
var
  index: Integer;
  item: PT;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);

  index := fTail;
  if index = 0 then
    index := fCapacity;
  Dec(index);
  item := @fItems[index];
  fTail := index;

  if Assigned(fOnChanged) and fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item^, action);
  if OwnsObjects and (action = caRemoved) then
    PObject(item).Free;
  item^ := Default(T);
end;

function TCircularArrayBuffer<T>.First: T; //FI:W521
begin
  if Count > 0 then
    Exit(fItems[fHead]);
  RaiseHelper.NoElements;
  __SuppressWarning(Result);
end;

function TCircularArrayBuffer<T>.FirstOrDefault: T;
begin
  if Count > 0 then
    Result := fItems[fHead]
  else
    Result := Default(T);
end;

function TCircularArrayBuffer<T>.Single: T; //FI:W521
begin
  case Count of
    1: Exit(fItems[fHead]);
    0: RaiseHelper.NoElements;
  end;
  RaiseHelper.MoreThanOneElement;
  __SuppressWarning(Result);
end;

function TCircularArrayBuffer<T>.SingleOrDefault(const defaultValue: T): T; //FI:W521
begin
  case Count of
    1: Result := fItems[fHead];
    0: Result := defaultValue;
  else
    RaiseHelper.MoreThanOneElement;
    __SuppressWarning(Result);
  end;
end;

procedure TCircularArrayBuffer<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

function TCircularArrayBuffer<T>.TryGetFirst(var value: T): Boolean;
begin
  if Count > 0 then
  begin
    value := fItems[fHead];
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

function TCircularArrayBuffer<T>.TryGetLast(var value: T): Boolean;
begin
  if Count > 0 then
  begin
    value := fItems[Tail];
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

procedure TCircularArrayBuffer<T>.SetCapacity(value: Integer);
var
  itemCount, oldCapacity, offset: Integer;
begin
  itemCount := Count;
  if value < itemCount then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.value, ExceptionResource.ArgumentOutOfRange_Capacity);

  if itemCount = 0 then
  begin
    fHead := 0;
    fTail := 0;
    fCapacity := value;
    SetLength(fItems, value);
    Exit;
  end;

  oldCapacity := DynArrayLength(fItems);
  offset := value - oldCapacity;
  if offset = 0 then
    Exit;

  if offset > 0 then
  begin
    fCapacity := value;
    SetLength(fItems, value);
  end;
  if fTail <= fHead then
  begin
    if ItemType.HasWeakRef then
      MoveManaged(@fItems[fHead], @fItems[fHead + offset], TypeInfo(T), oldCapacity - fHead)
    else
      System.Move(fItems[fHead], fItems[fHead + offset], SizeOf(T) * (oldCapacity - fHead));
    if offset > 0 then
    begin
      if ItemType.HasWeakRef then
        System.Finalize(fItems[fHead], offset);
      System.FillChar(fItems[fHead], SizeOf(T) * offset, 0);
    end
    else
    begin
      if ItemType.HasWeakRef then
        System.Finalize(fItems[itemCount], -offset);
      System.FillChar(fItems[itemCount], SizeOf(T) * -offset, 0)
    end;
    Inc(fHead, offset);
  end
  else
  begin
    if fHead + itemCount > value then
    begin
      if ItemType.HasWeakRef then
      begin
        MoveManaged(@fItems[fHead], @fItems[0], TypeInfo(T), itemCount);
        System.Finalize(fItems[itemCount], fHead);
      end
      else
        System.Move(fItems[fHead], fItems[0], SizeOf(T) * itemCount);
      System.FillChar(fItems[itemCount], SizeOf(T) * fHead, 0);
      fHead := 0;
    end;
    fTail := itemCount;
  end;
  if offset < 0 then
  begin
    fCapacity := value;
    SetLength(fItems, value);
  end;
end;

procedure TCircularArrayBuffer<T>.SetOwnsObjects(value: Boolean);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) = tkClass then
  {$ELSE}
  if TType.Kind<T> = tkClass then
  {$ENDIF}
    fCount := (fCount and CountMask) or (Ord(value) shl OwnsObjectsBitIndex);
end;

{$ENDREGION}


{$REGION 'TCircularArrayBuffer<T>.TEnumerator'}

function TCircularArrayBuffer<T>.TEnumerator.GetCurrent: T;
begin
  Result := fSource.fItems[fIndex];
end;

function TCircularArrayBuffer<T>.TEnumerator.MoveNext: Boolean;
var
  source: TCircularArrayBuffer<T>;
  capacity: Integer;
begin
  source := fSource;
  if fVersion = source.fVersion then
  begin
    if fCount > 0 then
    begin
      capacity := source.fCapacity;
      Dec(fCount);
      Inc(fIndex);
      if fIndex >= capacity then
        Dec(fIndex, capacity);
      Result := True;
    end
    else
      Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TMapBase<TKey, TValue>'}

procedure TMapBase<TKey, TValue>.AfterConstruction;
begin
  TPairComparer.Create(@fComparer,
    @TKeyValuePairComparer.Comparer_Vtable,
    @TKeyValuePairComparer.Compare,
    GetKeyType, GetValueType);
  inherited AfterConstruction;
  fOnKeyChanged := TCollectionChangedEventImpl<TKey>.Create;
  fOnValueChanged := TCollectionChangedEventImpl<TValue>.Create;
end;

function TMapBase<TKey, TValue>.AsReadOnly: IReadOnlyDictionary<TKey, TValue>;
begin
  GetInterface(IReadOnlyDictionaryGuid, Result);
end;

procedure TMapBase<TKey, TValue>.BeforeDestruction;
begin
  fOnValueChanged.Free;
  fOnKeyChanged.Free;
  inherited BeforeDestruction;
end;

procedure TMapBase<TKey, TValue>.DoNotify(const key: TKey; const value: TValue;
  action: TCollectionChangedAction);
var
  pair: TKeyValuePair;
begin
  pair.Key := key;
  pair.Value := value;
  Notify(Self, pair, action);
end;

function TMapBase<TKey, TValue>.Add(const item: TKeyValuePair): Boolean;
begin
  Result := IMap<TKey, TValue>(this).TryAdd(item.Key, item.Value);
end;

procedure TMapBase<TKey, TValue>.Add(const key: TKey; const value: TValue);
begin
  if not IMap<TKey, TValue>(this).TryAdd(key, value) then
    RaiseHelper.DuplicateKey;
end;

function TMapBase<TKey, TValue>.Contains(const item: TKeyValuePair): Boolean;
begin
  Result := IMap<TKey, TValue>(this).Contains(item.Key, item.Value);
end;

function TMapBase<TKey, TValue>.Extract(const item: TKeyValuePair): TKeyValuePair;
begin
  Result := IMap<TKey, TValue>(this).Extract(item.Key, item.Value);
end;

function TMapBase<TKey, TValue>.GetKeyType: PTypeInfo;
begin
  Result := TypeInfo(TKey);
end;

function TMapBase<TKey, TValue>.GetOnKeyChanged: ICollectionChangedEvent<TKey>;
begin
  Result := fOnKeyChanged;
end;

function TMapBase<TKey, TValue>.GetOnValueChanged: ICollectionChangedEvent<TValue>;
begin
  Result := fOnValueChanged;
end;

function TMapBase<TKey, TValue>.GetValueType: PTypeInfo;
begin
  Result := TypeInfo(TValue);
end;

function TMapBase<TKey, TValue>.Remove(const item: TKeyValuePair): Boolean;
begin
  Result := IMap<TKey, TValue>(this).Remove(item.Key, item.Value);
end;

class function TMapBase<TKey, TValue>.RemoveCurrentFromCollection(
  const enumerator: IEnumerator<TKey>; const collection: IMap<TKey, TValue>): Boolean;
{$IFDEF RSP31615}
var
  key: TKey;
{$ENDIF}
begin
  {$IFDEF RSP31615}
  if IsManagedType(TKey) then
  begin
    IEnumeratorInternal(enumerator).GetCurrent(key);
    Result := IMap<TKey, TValue>(collection).Remove(key);
  end
  else
  {$ENDIF}
  Result := IMap<TKey, TValue>(collection).Remove(IEnumerator<TKey>(enumerator).Current);
end;

function TMapBase<TKey, TValue>.RemoveRange(const keys: array of TKey): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(keys) do
    Inc(Result, Integer(IMap<TKey, TValue>(this).Remove(keys[i])));
end;

function TMapBase<TKey, TValue>.RemoveRange(const keys: IEnumerable<TKey>): Integer;
begin
  Result := inherited ForEach(keys, TCollectionOperation(@RemoveCurrentFromCollection));
end;

{$ENDREGION}


{$REGION 'TIteratorBlock'}

class procedure TIteratorBlock.Create(enumerator: PPointer;
  extension: TEnumerableExtension; size: NativeInt;
  finalize, initialize: Pointer);
const
  EmptyEnumerator: record
    VTable: Pointer;
    QueryInterface,
    AddRef,
    Release,
    GetCurrent,
    MoveNext: Pointer;
  end = (
    VTable: @EmptyEnumerator.QueryInterface;
    QueryInterface: @NopQueryInterface;
    AddRef: @NopRef;
    Release: @NopRef;
    GetCurrent: @RaiseHelper.NoElements;
    MoveNext: @TIteratorBlock.MoveNextEmpty
  );
type
  TInit = procedure(self: Pointer; extension: TEnumerableExtension);
var
  Result: PIteratorBlock;
begin
  if extension.fKind <> Empty then
  begin
    extension._AddRef;
    IInterface(enumerator^) := nil;
    Result := AllocMem(size);
    enumerator^ := Result;

    Result.RefCount := 1;
    Result.Source := extension.fSource;
    if extension.fKind = Memoize then
      Pointer(Result.Predicate) := extension
    else
    begin
      Result.Predicate := extension.fPredicate;
      Result.Items := extension.fItems;
    end;
    Result.Index := extension.fIndex;
    Result.Count := extension.fCount;
    Result.Kind := extension.fKind;
    Result.Parent := extension;
    Result.Methods.Finalize := finalize;
    TInit(initialize)(Result, extension);
  end
  else
    IInterface(enumerator^) := IInterface(@EmptyEnumerator);
end;

function TIteratorBlock.Finalize(typeInfo: PTypeInfo): Boolean;
var
  p: PDynArrayTypeInfo;
begin
  Enumerator := nil;
  Source := nil;
  if Kind <> Memoize then
    Predicate := nil
  else
    Pointer(Predicate) := nil;
  if Kind in [Distinct..Union] then
  begin
    if Items <> nil then
    begin
      PHashTable(Items).Clear;
      PHashTable(Items).Comparer := nil;
      FreeMem(Items);
      Items := nil;
    end;
  end
  else if Kind <> DefaultIfEmpty then
    DynArrayClear(Items, typeInfo);
  p := PDynArrayTypeInfo(PByte(typeInfo) + Byte(PDynArrayTypeInfo(typeInfo).name));
  if p.elType <> nil then
    System.FinalizeArray(@Current, p.elType^, 1);
  Result := False;
end;

function TIteratorBlock.MoveNext: Boolean;
{$IFDEF CPUX86}
asm
  push eax
  call TIteratorBlock(eax).DoMoveNext
  test al,al
  jz @@ExitFalse
  pop edx
  ret
@@ExitFalse:
  pop eax
  mov TIteratorBlock(eax).DoMoveNext, offset [TIteratorBlock.MoveNextEmpty]
  call TIteratorBlock(eax).Methods.Finalize
end;
{$ELSE}
begin
  Result := DoMoveNext(@Self);
  if Result then
    Exit;

  DoMoveNext := @TIteratorBlock.MoveNextEmpty;
  Result := Methods.Finalize(@Self);
end;
{$ENDIF}

function TIteratorBlock._Release: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
  begin
    Parent._Release;
    Methods.Finalize(@Self);
    FreeMem(@Self);
  end;
end;

function TIteratorBlock.GetEnumerator: Boolean;
begin
{$IFDEF MSWINDOWS}
  IEnumerableInternal(Source).GetEnumerator(Enumerator);
{$ELSE}
  Enumerator := Source.GetEnumerator;
{$ENDIF}
  DoMoveNext := Methods.MoveNext;
  Result := Methods.MoveNext(@Self);
end;

function TIteratorBlock.GetEnumeratorAndSkip: Boolean;
begin
{$IFDEF MSWINDOWS}
  IEnumerableInternal(Source).GetEnumerator(Enumerator);
{$ELSE}
  Enumerator := Source.GetEnumerator;
{$ENDIF}
  while (Index > 0) and Enumerator.MoveNext do
    Dec(Index);
  Result := Index = 0;
  if Result then
  begin
    DoMoveNext := Methods.MoveNext;
    Result := Methods.MoveNext(@Self);
  end;
end;

function TIteratorBlock.GetEnumeratorDefaultIfEmpty: Boolean;
begin
{$IFDEF MSWINDOWS}
  IEnumerableInternal(Source).GetEnumerator(Enumerator);
{$ELSE}
  Enumerator := Source.GetEnumerator;
{$ENDIF}
  if Methods.MoveNext(@Self) then
    DoMoveNext := Methods.MoveNext
  else
    DoMoveNext := @TIteratorBlock.MoveNextEmpty;
  Result := True;
end;

function TIteratorBlock.GetEnumeratorHashTable(
  getCurrent: TGetCurrent; assign: TAssign): Boolean;
var
  entry: PByte;
begin
{$IFDEF MSWINDOWS}
  IEnumerableInternal(Predicate).GetEnumerator(Enumerator);
{$ELSE}
  Enumerator := IEnumerable(Predicate).GetEnumerator;
{$ENDIF}

  while Enumerator.MoveNext do
  begin
    getCurrent(Enumerator, Current);
    entry := PHashTable(Items).FindItem(Current, IgnoreExisting or InsertNonExisting);
    if not Assigned(entry) then Continue;
    assign(entry[KeyOffset], Current);
  end;
  Result := GetEnumerator;
end;

function TIteratorBlock.GetEnumeratorMemoize: Boolean;
begin
  if TEnumerableExtension(Pointer(Predicate)).fCount = 0 then
  begin
  {$IFDEF MSWINDOWS}
    IEnumerableInternal(Source).GetEnumerator(IEnumerator(TEnumerableExtension(Pointer(Predicate)).fPredicate));
  {$ELSE}
    IEnumerator(TEnumerableExtension(Pointer(Predicate)).fPredicate) := Source.GetEnumerator;
  {$ENDIF}
    TEnumerableExtension(Pointer(Predicate)).fCount := TEnumerableExtension(Pointer(Predicate)).fCount or not CountMask;
  end;
  DoMoveNext := Methods.MoveNext;
  Result := Methods.MoveNext(@Self);
end;

function TIteratorBlock.GetEnumeratorPartitionFromEnd: Boolean;
var
  count, index: Integer;
begin
  count := Source.GetNonEnumeratedCount;

  if Self.Index < 0 then
  begin
    index := count - Self.Count;
    if index < 0 then
    begin
      Self.Count := count;
      index := 0;
    end;
    Self.Index := index;
  end
  else
  begin
    count := count - Self.Count;
    if count < 0 then
      Exit(False);
    Self.Count := count;
  end;
  DoMoveNext := Methods.MoveNext;
  Result := Methods.MoveNext(@Self);
end;

function TIteratorBlock.GetEnumeratorSkipLast(getCurrent: TGetCurrent; typeInfo: PTypeInfo): Boolean;
var
  i, capacity, elSize: NativeInt;
begin
{$IFDEF MSWINDOWS}
  IEnumerableInternal(Source).GetEnumerator(Enumerator);
{$ELSE}
  Enumerator := Source.GetEnumerator;
{$ENDIF}

  i := 0;
  capacity := 0;
  elSize := PDynArrayTypeInfo(PByte(typeInfo) + Byte(PDynArrayTypeInfo(typeInfo).name)).elSize;
  repeat
    if not Enumerator.MoveNext then Exit(False);

    if i >= capacity then
      capacity := DynArrayGrow(Pointer(Items), typeInfo, capacity);
    getCurrent(Enumerator, PByte(Items)[i*elSize]);
    Inc(i);
  until i >= Count;
  DoMoveNext := Methods.MoveNext;
  Result := Methods.MoveNext(@Self);
end;

function TIteratorBlock.GetEnumeratorTakeLast(getCurrent: TGetCurrent; typeInfo: PTypeInfo): Boolean;
var
  i, capacity, elSize: NativeInt;
  wrapAround: Boolean;
begin
  if Self.Count = 0 then Exit(Boolean(Self.Count));

{$IFDEF MSWINDOWS}
  IEnumerableInternal(Source).GetEnumerator(Enumerator);
{$ELSE}
  Enumerator := Source.GetEnumerator;
{$ENDIF}
  Result := Enumerator.MoveNext;
  if not Result then Exit;

  i := 0;
  capacity := 0;
  wrapAround := False;
  elSize := PDynArrayTypeInfo(PByte(typeInfo) + Byte(PDynArrayTypeInfo(typeInfo).name)).elSize;
  repeat
    if i >= capacity then
      capacity := DynArrayGrow(Pointer(Items), typeInfo, capacity);
    getCurrent(Enumerator, PByte(Items)[i*elSize]);
    Inc(i);
    if i = Self.Count then
    begin
      i := 0;
      wrapAround := True;
    end;
  until not Enumerator.MoveNext;
  Enumerator := nil;

  if not wrapAround then
    Self.Count := i
  else if i > 0 then
    DynArrayUnwrap(Items, typeInfo, i, Count);
  Index := 0;
  DoMoveNext := Methods.MoveNext;
  Result := Methods.MoveNext(@Self);
end;

procedure TIteratorBlock.InitHashTable(const equals, getHashCode: Pointer;
  itemsInfo: PTypeInfo; const comparer: IInterface);
begin
  PHashTable(Items) := AllocMem(SizeOf(THashTable));
  PHashTable(Items).Comparer := comparer;
  PHashTable(Items).ItemsInfo := itemsInfo;
  PHashTable(Items).Initialize(equals, getHashCode, Source.ElementType);
end;

function TIteratorBlock.MoveNextEmpty: Boolean;
begin
  Result := False;
end;

function TIteratorBlock.MoveNextMemoize(getCurrent: TGetCurrent; assign: TAssign; typeInfo: PTypeInfo): Boolean;
var
  iterator: TEnumerableExtension;
  count, capacity, elSize: NativeInt;
begin
  iterator := Pointer(Predicate);
  count := iterator.fCount and CountMask;
  elSize := PDynArrayTypeInfo(PByte(typeInfo) + Byte(PDynArrayTypeInfo(typeInfo).name)).elSize;
  if Index >= count then
  begin
    if iterator.fPredicate = nil then
      Exit(False);

    if not IEnumerator(iterator.fPredicate).MoveNext then
    begin
      iterator.fPredicate := nil;
      Exit(False);
    end;

    capacity := DynArrayLength(iterator.fItems);
    if count >= capacity then
      DynArrayGrow(Pointer(iterator.fItems), typeInfo, capacity);
    getCurrent(IEnumerator(iterator.fPredicate), PByte(iterator.fItems)[count*elSize]);
    Inc(iterator.fCount);
  end;

  assign(Current, PByte(iterator.fItems)[Index*elSize]);
  Inc(Index);
  Result := True;
end;

{$ENDREGION}


{$REGION 'TIteratorBlock<T>'}

procedure TIteratorBlock<T>.Initialize(extension: TEnumerableExtension);
begin
  DoMoveNext := @TIteratorBlock.GetEnumerator;
  case Kind of //FI:W535
    TExtensionKind.DefaultIfEmpty:
    begin
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextEnumerator;
      DoMoveNext := @TIteratorBlock.GetEnumeratorDefaultIfEmpty;
      Current := Items[0];
    end;
    TExtensionKind.Partition:
      if Assigned(Source) then
        if SupportsIndexedAccess(Source) then
          DoMoveNext := @TIteratorBlock<T>.MoveNextIndexed
        else
        begin
          if Count >= 0 then
            Methods.MoveNext := @TIteratorBlock<T>.MoveNextEnumeratorCounted
          else
            Methods.MoveNext := @TIteratorBlock<T>.MoveNextEnumerator;
          DoMoveNext := @TIteratorBlock.GetEnumeratorAndSkip;
        end
      else
        DoMoveNext := @TIteratorBlock.MoveNextEmpty;
    TExtensionKind.PartitionFromEnd:
      if Assigned(Source) then
        if SupportsIndexedAccess(Source) then
        begin
          Methods.MoveNext := @TIteratorBlock<T>.MoveNextIndexed;
          DoMoveNext := @TIteratorBlock.GetEnumeratorPartitionFromEnd;
        end
        else
          if Index = 0 then
          begin
            Methods.MoveNext := @TIteratorBlock<T>.MoveNextSkipLast;
            DoMoveNext := @TIteratorBlock<T>.GetEnumeratorSkipLast;
          end
          else
          begin
            Methods.MoveNext := @TIteratorBlock<T>.MoveNextArray;
            DoMoveNext := @TIteratorBlock<T>.GetEnumeratorTakeLast;
          end
      else
        DoMoveNext := @TIteratorBlock.MoveNextEmpty;
    TExtensionKind.Distinct,
    TExtensionKind.Exclude,
    TExtensionKind.Intersect:
    begin
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextHashTable;
      if Kind <> TExtensionKind.Distinct then
        DoMoveNext := @TIteratorBlock<T>.GetEnumeratorHashTable;
    end;
    TExtensionKind.Union,
    TExtensionKind.Concat:
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextConcat;
    TExtensionKind.Memoize:
    begin
      if Count = 0 then
      begin
        Methods.MoveNext := @TIteratorBlock<T>.MoveNextMemoize;
        DoMoveNext := @TIteratorBlock.GetEnumeratorMemoize;
      end
      else
        DoMoveNext := @TIteratorBlock<T>.MoveNextMemoize;
    end;
    TExtensionKind.Ordered,
    TExtensionKind.Reversed,
    TExtensionKind.Shuffled:
    begin
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextArray;
      DoMoveNext := @TIteratorBlock<T>.ToArray;
    end;
    TExtensionKind.SkipWhile:
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextSkipWhile;
    TExtensionKind.SkipWhileIndex:
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextSkipWhileIndex;
    TExtensionKind.TakeWhile:
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextTakeWhile;
    TExtensionKind.TakeWhileIndex:
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextTakeWhileIndex;
    TExtensionKind.Where:
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextWhere;
    TExtensionKind.WhereIndex:
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextWhereIndex;
  else
    DoMoveNext := nil;
  end;

  if Kind in [Distinct..Union] then
    PIteratorBlock(@Self).InitHashTable(@TComparerThunks<T>.Equals,
      @TComparerThunks<T>.GetHashCode, TypeInfo(TItems), extension.fEqualityComparer);

  if Assigned(Enumerator_Vtable[0]) then
  begin
    Vtable := @Enumerator_Vtable;
    Exit;
  end;

  Enumerator_Vtable[0] := @NopQueryInterface;
  Enumerator_Vtable[1] := @RecAddRef;
  Enumerator_Vtable[2] := @TIteratorBlock._Release;
  Enumerator_Vtable[3] := @TIteratorBlock<T>.GetCurrent;
  Enumerator_Vtable[4] := @TIteratorBlock.MoveNext;
  Vtable := @Enumerator_Vtable;
end;

function TIteratorBlock<T>.Finalize: Boolean;
begin
  Result := PIteratorBlock(@Self).Finalize(TypeInfo(TArray<T>));
end;

function TIteratorBlock<T>.GetCurrent: T;
begin
  Result := Current;
end;

function TIteratorBlock<T>.GetEnumeratorHashTable: Boolean;
begin
  Result := PIteratorBlock(@Self).GetEnumeratorHashTable(TCollectionThunks<T>.GetCurrent, @TCollectionThunks<T>.Assign);
end;

function TIteratorBlock<T>.GetEnumeratorSkipLast: Boolean;
begin
  Result := PIteratorBlock(@Self).GetEnumeratorSkipLast(TCollectionThunks<T>.GetCurrent, TypeInfo(TArray<T>));
end;

function TIteratorBlock<T>.GetEnumeratorTakeLast: Boolean;
begin
  Result := PIteratorBlock(@Self).GetEnumeratorTakeLast(TCollectionThunks<T>.GetCurrent, TypeInfo(TArray<T>));
end;

function TIteratorBlock<T>.MoveNextArray: Boolean;
begin
  if Count > 0 then
  begin
    Current := Items[Index];
    Inc(Index, -Integer(Kind = reversed) or 1);
    Dec(Count);
    Result := True;
  end
  else
    Result := False;
end;

function TIteratorBlock<T>.MoveNextConcat: Boolean;
var
  entry: PItem;
  p: Pointer;
begin
  repeat
    repeat
      if not Enumerator.MoveNext then Break;
      {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
      if IsManagedType(T) then
        IEnumeratorInternal(Enumerator).GetCurrent(Current)
      else
      {$IFEND}
      Current := Enumerator.Current;
      if Kind <> Concat then
      begin
        {$IF defined(DELPHIXE6) or defined(DELPHIXE7)}
        entry := HashTableFindItem(PHashTable(Items), Current, IgnoreExisting or InsertNonExisting);
        {$ELSE}
        entry := PHashTable(Items).FindItem(Current, IgnoreExisting or InsertNonExisting);
        {$IFEND}
        if not Assigned(entry) then Continue;
        entry.Item := Current;
      end;
      Exit(True);
    until False;

    p := Pointer(Predicate);
    if p = nil then
      Exit(Boolean(p));

    {$IFDEF MSWINDOWS}
    IEnumerableInternal(Predicate).GetEnumerator(Enumerator);
    {$ELSE}
    Enumerator := IEnumerable<T>(Predicate).GetEnumerator;
    {$ENDIF}
    Predicate := nil;
  until False;
end;

function TIteratorBlock<T>.MoveNextEnumerator: Boolean;
begin
  Result := Enumerator.MoveNext;
  if Result then
  begin
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := True;
  end;
end;

function TIteratorBlock<T>.MoveNextEnumeratorCounted: Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    Result := Enumerator.MoveNext;
    if Result then
    begin
      {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
      if IsManagedType(T) then
        IEnumeratorInternal(Enumerator).GetCurrent(Current)
      else
      {$IFEND}
      Current := Enumerator.Current;
      Dec(Count);
      Result := True;
    end;
  end;
end;

function TIteratorBlock<T>.MoveNextHashTable: Boolean;
var
  option: NativeInt;
  entry: PItem;
begin
  repeat
    Result := Enumerator.MoveNext;
    if not Result then Break;
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    if Kind <> Intersect then
      option := IgnoreExisting or InsertNonExisting
    else
      option := DeleteExisting;
    {$IF defined(DELPHIXE6) or defined(DELPHIXE7)}
    entry := HashTableFindItem(PHashTable(Items), Current, option);
    {$ELSE}
    entry := PHashTable(Items).FindItem(Current, option);
    {$IFEND}
    if not Assigned(entry) then Continue;
    if Kind <> Intersect then
      entry.Item := Current
    else
      System.Finalize(entry.Item);
    Result := True;
    Break;
  until False;
end;

function TIteratorBlock<T>.MoveNextIndexed: Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    Result := Source.TryGetElementAt(Current, Index);
    if Result then
    begin
      Inc(Index);
      Dec(Count);
    end;
  end;
end;

function TIteratorBlock<T>.MoveNextMemoize: Boolean;
begin
  Result := PIteratorBlock(@Self).MoveNextMemoize(
    TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.Assign, TypeInfo(TArray<T>));
end;

function TIteratorBlock<T>.MoveNextSkipLast: Boolean;
var
  i: NativeInt;
begin
  Result := Enumerator.MoveNext;
  if Result then
  begin
    i := Index;
    if i >= Count then
      i := 0;
    Index := i + 1;
    Current := Items[i];
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Items[i])
    else
    {$ENDIF}
    Items[i] := Enumerator.Current;
    Result := True;
  end;
end;

function TIteratorBlock<T>.MoveNextSkipWhile: Boolean;
begin
  repeat
    Result := Enumerator.MoveNext;
    if not Result then
      Break;
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := Predicate<T>(Predicate)(Current);
    if Result then
      Continue;
    DoMoveNext := @TIteratorBlock<T>.MoveNextEnumerator;
    Exit(True);
  until False;
end;

function TIteratorBlock<T>.MoveNextSkipWhileIndex: Boolean;
begin
  repeat
    Result := Enumerator.MoveNext;
    if not Result then
      Break;
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := Func<T, Integer, Boolean>(Predicate)(Current, Index);
    Inc(Index, Ord(Result));
    if Result then
      Continue;
    DoMoveNext := @TIteratorBlock<T>.MoveNextEnumerator;
    Exit(True);
  until False;
end;

function TIteratorBlock<T>.MoveNextTakeWhile: Boolean;
begin
  Result := Enumerator.MoveNext;
  if Result then
  begin
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := Predicate<T>(Predicate)(Current);
  end;
end;

function TIteratorBlock<T>.MoveNextTakeWhileIndex: Boolean;
begin
  Result := Enumerator.MoveNext;
  if Result then
  begin
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := Func<T, Integer, Boolean>(Predicate)(Current, Index);
    Inc(Index);
  end;
end;

function TIteratorBlock<T>.MoveNextWhere: Boolean;
begin
  repeat
    Result := Enumerator.MoveNext;
    if not Result then
      Break;
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := Predicate<T>(Predicate)(Current);
  until Result;
end;

function TIteratorBlock<T>.MoveNextWhereIndex: Boolean;
begin
  repeat
    Result := Enumerator.MoveNext;
    if not Result then
      Break;
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := Func<T,Integer,Boolean>(Predicate)(Current, Index);
    Inc(Index);
  until Result;
end;

function TIteratorBlock<T>.ToArray: Boolean;
begin
  {$IFDEF MSWINDOWS}
  IEnumerableInternal(Source).ToArray(Items);
  {$ELSE}
  Items := Source.ToArray;
  {$ENDIF}
  Count := DynArrayLength(Items);
  case Kind of //FI:W535
    TExtensionKind.Ordered, TExtensionKind.Shuffled:
      TCollectionThunks<T>.ProcessArray(Kind, Items, IComparer<T>(Predicate));
    TExtensionKind.Reversed:
      Index := Count - 1;
  end;
  DoMoveNext := Methods.MoveNext;
  Result := Methods.MoveNext(@Self);
end;

{$ENDREGION}


{$REGION 'TCollectionThunks<T>'}

class function TCollectionThunks<T>.AddCurrentToCollection(
  const enumerator, collection: IInterface): Boolean;
{$IFDEF RSP31615}
var
  item: T;
{$ENDIF}
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
  begin
    IEnumeratorInternal(enumerator).GetCurrent(item);
    Result := ICollection<T>(collection).Add(item);
  end
  else
  {$ENDIF}
  Result := ICollection<T>(collection).Add(IEnumerator<T>(enumerator).Current);
end;

class function TCollectionThunks<T>.AddToCollection(
  const collection: IInterface; const value): Boolean;
begin
  Result := ICollection<T>(collection).Add(T(value));
end;

class procedure TCollectionThunks<T>.AggregateCurrentWithValue(
  const enumerator, func: IInterface; var result);
{$IFDEF RSP31615}
var
  item, res: T;
{$ENDIF}
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
  begin
    IEnumeratorInternal(enumerator).GetCurrent(item);
    FuncInternal(func)({$IFDEF CPUX64}res, {$ENDIF}T(result), item{$IFDEF CPUX86}, res{$ENDIF});
    T(result) := res;
  end
  else
  {$ENDIF}
  T(result) := Func<T,T,T>(func)(T(result), IEnumerator<T>(enumerator).Current);
end;

class procedure TCollectionThunks<T>.Assign(var target; const source);
begin
  T(target) := T(source);
end;

class procedure TCollectionThunks<T>.CallActionOnCurrent(const enumerator, action: IInterface);
{$IFDEF RSP31615}
var
  item: T;
{$ENDIF}
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
  begin
    IEnumeratorInternal(enumerator).GetCurrent(item);
    Action<T>(action)(item);
  end
  else
  {$ENDIF}
  Action<T>(action)(IEnumerator<T>(enumerator).Current);
end;

class function TCollectionThunks<T>.Contains(const collection: IInterface;
  const value; const comparer: IInterface): Boolean;
begin
  Result := IEnumerable<T>(collection).Contains(T(value), IEqualityComparer<T>(comparer));
end;

class function TCollectionThunks<T>.EqualsCurrentWithOtherEnumerator(
  const enumerator1, enumerator2, comparer: IInterface): Boolean;
{$IFDEF RSP31615}
var
  item1, item2: T;
{$ENDIF}
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
  begin
    IEnumeratorInternal(enumerator1).GetCurrent(item1);
    IEnumeratorInternal(enumerator2).GetCurrent(item2);
    Result := IEqualityComparer<T>(comparer).Equals(item1, item2);
  end
  else
  {$ENDIF}
  Result := IEqualityComparer<T>(comparer).Equals(
    IEnumerator<T>(enumerator1).Current, IEnumerator<T>(enumerator2).Current);
end;

class function TCollectionThunks<T>.EqualsCurrentWithArrayElement(
  const enumerator: IInterface; comparer, values: Pointer; index: NativeInt): Boolean;
{$IFDEF RSP31615}
var
  item: T;
{$ENDIF}
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
  begin
    IEnumeratorInternal(enumerator).GetCurrent(item);
    {$R-}
    Result := IEqualityComparer<T>(comparer).Equals(item, TSlice<T>(values^)[index]);
    {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
  end
  else
  {$ENDIF}
  {$R-}
  Result := IEqualityComparer<T>(comparer).Equals(
    IEnumerator<T>(enumerator).Current, TSlice<T>(values^)[index]);
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class function TCollectionThunks<T>.EqualsCurrentWithValue(
  const enumerator, comparer: IInterface; const value): Boolean;
{$IFDEF RSP31615}
var
  item: T;
{$ENDIF}
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
  begin
    IEnumeratorInternal(enumerator).GetCurrent(item);
    {$R-}
    Result := IEqualityComparer<T>(comparer).Equals(item, T(value));
    {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
  end
  else
  {$ENDIF}
  {$R-}
  Result := IEqualityComparer<T>(comparer).Equals(
    IEnumerator<T>(enumerator).Current, T(value));
  {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
end;

class function TCollectionThunks<T>.ExtractCurrentFromCollection(
  const enumerator, collection: IInterface): Boolean;
{$IFDEF RSP31615}
var
  item, res: T;
{$ENDIF}
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
  begin
    IEnumeratorInternal(enumerator).GetCurrent(item);
    ICollectionInternal(collection).Extract({$IFDEF CPUX64}res, {$ENDIF}item{$IFDEF CPUX86}, res{$ENDIF});
  end
  else
  {$ENDIF}
  ICollection<T>(collection).Extract(IEnumerator<T>(enumerator).Current);
  Result := True;
end;

class procedure TCollectionThunks<T>.GetCurrent(const enumerator: IInterface; var value);
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(enumerator).GetCurrent(value)
  else
  {$ENDIF}
  T(value) := IEnumerator<T>(enumerator).Current;
end;

class procedure TCollectionThunks<T>.GetCurrentIfGreaterThan(const enumerator,
  comparer: IInterface; var result);
var
  item: T;
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(enumerator).GetCurrent(item)
  else
  {$ENDIF}
  item := IEnumerator<T>(enumerator).Current;
  if IComparer<T>(comparer).Compare(item, T(result)) > 0 then
    T(result) := item;
end;

class procedure TCollectionThunks<T>.GetCurrentIfLessThan(const enumerator,
  comparer: IInterface; var result);
var
  item: T;
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(enumerator).GetCurrent(item)
  else
  {$ENDIF}
  item := IEnumerator<T>(enumerator).Current;
  if IComparer<T>(comparer).Compare(item, T(result)) < 0 then
    T(result) := item;
end;

class function TCollectionThunks<T>.GetCurrentWithPredicate(const enumerator, predicate: IInterface; var value): Boolean;
var
  item: T;
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(enumerator).GetCurrent(item)
  else
  {$ENDIF}
  item := IEnumerator<T>(enumerator).Current;
  Result := Predicate<T>(predicate)(item);
  if Result then
    T(value) := item;
end;

class function TCollectionThunks<T>.GetCurrentWithSelector(const enumerator, selector: IInterface): T;
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(enumerator).GetCurrent(Result)
  else
  {$ENDIF}
  Result := IEnumerator<T>(enumerator).Current;
end;

class procedure TCollectionThunks<T>.GetDefault(var value);
begin
  T(value) := Default(T);
end;

class procedure TCollectionThunks<T>.ProcessArray(kind: TExtensionKind;
  var values: TArray<T>; const comparer: IComparer<T>);
begin
  case kind of //FI:W535
    TExtensionKind.Ordered:
      TArray.Sort<T>(values, comparer);
    TExtensionKind.Reversed:
      TArray.Reverse<T>(values);
    TExtensionKind.Shuffled:
      TArray.Shuffle<T>(values);
  end;
end;

class function TCollectionThunks<T>.RemoveCurrentFromCollection(
  const enumerator, collection: IInterface): Boolean;
{$IFDEF RSP31615}
var
  item: T;
{$ENDIF}
begin
  {$IFDEF RSP31615}
  if IsManagedType(T) then
  begin
    IEnumeratorInternal(enumerator).GetCurrent(item);
    Result := ICollection<T>(collection).Remove(item);
  end
  else
  {$ENDIF}
  Result := ICollection<T>(collection).Remove(IEnumerator<T>(enumerator).Current);
end;

{$ENDREGION}


{$REGION 'TCollectionThunks<TSource, TResult>'}

class function TCollectionThunks<T1, T2>.GetCurrentWithSelector(
  const enumerator, selector: IInterface): T2;
{$IFDEF RSP31615}
var
  item: T1;
{$ENDIF}
begin
  {$IFDEF RSP31615}
  if IsManagedType(T1) then
  begin
    IEnumeratorInternal(enumerator).GetCurrent(item);
    Result := Func<T1, T2>(selector)(item);
  end
  else
  {$ENDIF}
  Result := Func<T1, T2>(selector)(IEnumerator<T1>(enumerator).Current);
end;

{$ENDREGION}


{$REGION 'TEnumerableExtension'}

class function TEnumerableExtension.Create(classType: TClass;
  const source: IEnumerable; kind: TExtensionKind): TEnumerableExtension;
begin
  Result := Pointer(classType.NewInstance);
  TObject(Result).AfterConstruction;
  Result.fSource := source;
  if Assigned(source) then
    AssignComparer(Result.fComparer, source);
  Result.fKind := kind;
end;

function TEnumerableExtension.GetNonEnumeratedCount: Integer;
var
  count: Integer;
begin
  case fKind of //FI:W535
    TExtensionKind.Empty: Exit(0);
    TExtensionKind.Concat:
    begin
      Result := fSource.GetNonEnumeratedCount;
      if Result >= 0 then
      begin
        count := Result;
        Result := IEnumerable(fPredicate).GetNonEnumeratedCount;
        if Result >= 0 then
          {$Q+}
          Inc(Result, count);
          {$IFNDEF OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
      end;
      Exit;
    end;
    TExtensionKind.Memoize:
    begin
      Result := fCount;
      if (Result <> 0) and (fPredicate = nil) then
        Exit(Result and CountMask)
    end;
    TExtensionKind.Ordered, TExtensionKind.Reversed, TExtensionKind.Shuffled:
      Exit(fSource.GetNonEnumeratedCount);
    TExtensionKind.Partition:
    begin
      Result := fCount;
      if Result > 0 then
      begin
        Result := fSource.GetNonEnumeratedCount;
        if Result > 0 then
        begin
          Dec(Result, fIndex);
          if Result < 0 then
            Result := 0
          else if Cardinal(Result) > Cardinal(fCount) then
            Result := fCount;
        end;
      end;
      Exit;
    end;
    TExtensionKind.PartitionFromEnd:
    begin
      if fIndex = 0 then
      begin
        Result := fSource.GetNonEnumeratedCount;
        if Result > 0 then
        begin
          Dec(Result, fCount);
          if Result < 0 then
            Result := 0;
        end;
      end
      else
      begin
        Result := fCount;
        if Result > 0 then
        begin
          Result := fSource.GetNonEnumeratedCount;
          if Result > fCount then
            Result := fCount;
        end;
      end;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TEnumerableExtension.MemoizeToArray(var values: Pointer; typeInfo: PTypeInfo);
var
  count: Integer;
begin
  count := IEnumerable(this).Count;
  DynArrayCopyRange(values, fItems, typeInfo, 0, count);
end;

function TEnumerableExtension.PartitionToArray(var values: Pointer; typeInfo: PTypeInfo): Boolean;
var
  count, index, i, elSize: NativeInt;
  source: Pointer;
begin
  if Assigned(fSource) then
  begin
    Result := SupportsIndexedAccess(fSource);
    if Result then
    begin
      index := fIndex;
      count := fSource.Count - index;
      if count > fCount then
        count := fCount
      else if count < 0 then
        count := 0;
      DynArraySetLength(values, typeInfo, 1, @count);
      source := Pointer(fSource);
      elSize := PDynArrayTypeInfo(PByte(typeInfo) + Byte(PDynArrayTypeInfo(typeInfo).name)).elSize;
      for i := 0 to count - 1 do
      begin
        // little hack - we can hardcast here since we are just passing along a by ref param
        IEnumerable<Pointer>(source).TryGetElementAt(PPointer(@PByte(values)[i*elSize])^, index);
        Inc(index);
      end;
      Result := True;
    end;
  end
  else
  begin
    DynArrayClear(values, typeInfo);
    Result := True;
  end;
end;

procedure TEnumerableExtension.Skip(count: Integer; var result; classType: TClass);
var
  minIndex: Integer;
  source: Pointer;
begin
  if fKind <> TExtensionKind.Partition then
  begin
    inherited;
    Exit;
  end;

  if count < 0 then
    count := 0;
  {$Q-}
  minIndex := fIndex + count;
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if fCount = -1 then
  begin
    if minIndex < 0 then
    begin
      // If we don't know our max count and minIndex can no longer fit in a positive int,
      // then we will need to wrap ourselves in another iterator.
      // This can happen, for example, during e.Skip(MaxInt).Skip(MaxInt).
      source := Pointer(this);
      minIndex := count;
      count := -1;
    end
    else
    begin
      source := Pointer(fSource);
      count := -1;
    end;
  end
  else if count >= fCount then
  begin
    source := nil;
    minIndex := 0;
    count := 0;
  end
  else
  begin
    source := Pointer(fSource);
    count := fCount - count;
  end;

  with TEnumerableExtension.Create(classType, IEnumerable(source), TExtensionKind.Partition) do
  begin
    fIndex := minIndex;
    fCount := count;
    IInterface(result) := IInterface(@IMT);
  end;
end;

procedure TEnumerableExtension.Take(count: Integer; var result; classType: TClass);
var
  maxIndex: Integer;
  source: Pointer;
begin
  if fKind <> TExtensionKind.Partition then
  begin
    inherited;
    Exit;
  end;

  {$Q-}
  maxIndex := fIndex + count - 1;
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if fCount = -1 then
  begin
    if maxIndex < 0 then
    begin
      // If we don't know our max count and maxIndex can no longer fit in a positive int,
      // then we will need to wrap ourselves in another iterator.
      // Note that although maxIndex may be too large, the difference between it and
      // fIterator.Index (which is count - 1) must fit in an int.
      // Example: e.Skip(50).Take(MaxInt).
      source := Pointer(this);
      maxIndex := 0;
    end
    else
    begin
      source := Pointer(fSource);
      maxIndex := fIndex;
    end;
  end
  else if count >= fCount then
  begin
    IInterface(result) := IInterface(this);
    Exit;
  end
  else
  begin
    source := Pointer(fSource);
    maxIndex := fIndex;
  end;

  with TEnumerableExtension.Create(classType, IEnumerable(source), TExtensionKind.Partition) do
  begin
    fIndex := maxIndex;
    fCount := count;
    IInterface(result) := IInterface(@IMT);
  end;
end;

function TEnumerableExtension.TryGetElementAt(var value; index: Integer;
  getCurrent: TGetCurrent; default: TGetDefault): Boolean;
begin
  if fKind <> TExtensionKind.Partition then
    Exit(inherited);
  if Cardinal(index) < Cardinal(fCount) then
    Exit(IEnumerable<Pointer>(fSource).TryGetElementAt(Pointer(value), fIndex + index));
  default(value);
  Result := False;
end;

function TEnumerableExtension.TryGetFirst(var value; getCurrent: TGetCurrent; default: TGetDefault): Boolean;
begin
  if fKind <> TExtensionKind.Partition then
    Exit(inherited);
  if fCount <> 0 then
    // little hack - we can hardcast here since we are just passing along a by ref param
    Exit(IEnumerable<Pointer>(fSource).TryGetElementAt(Pointer(value), fIndex));
  default(value);
  Result := False;
end;

function TEnumerableExtension.TryGetLast(var value; getCurrent: TGetCurrent; default: TGetDefault): Boolean;
var
  count, lastIndex: NativeInt;
  enumerator: IEnumerator;
begin
  if fKind <> TExtensionKind.Partition then
    Exit(inherited);
  if fCount <> 0 then
  begin
    if SupportsIndexedAccess(fSource) then
    begin
      count := fSource.GetNonEnumeratedCount;
      if fIndex < count then
      begin
        {$Q-}
        lastIndex := fIndex + fCount - 1;
        {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
        if lastIndex > count - 1 then
          lastIndex := count - 1;
        // little hack - we can hardcast here since we are just passing along a by ref param
        Exit(IEnumerable<Pointer>(fSource).TryGetElementAt(Pointer(value), lastIndex));
      end;
    end
    else
    begin
      enumerator := fSource.GetEnumerator;
      count := 0;
      while (count < fIndex) and enumerator.MoveNext do
        Inc(count);
      if (count = fIndex) and enumerator.MoveNext then
      begin
        count := fCount;
        repeat
          getCurrent(enumerator, value);
          Dec(count);
        until (count = 0) or not enumerator.MoveNext;
        Exit(True);
      end;
    end;
  end;
  default(value);
  Result := False;
end;

{$ENDREGION}


{$REGION 'TEnumerableExtension<T>'}

function TEnumerableExtension<T>.GetElementType: PTypeInfo;
begin
  if Assigned(fSource) then
    Result := fSource.ElementType
  else
    Result := inherited GetElementType;
end;

function TEnumerableExtension<T>.GetEnumerator: IEnumerator<T>; //FI:W521
begin
  TIteratorBlock.Create(@Result, TEnumerableExtension(Self), SizeOf(TIteratorBlock<T>),
    @TIteratorBlock<T>.Finalize, @TIteratorBlock<T>.Initialize);
end;

function TEnumerableExtension<T>.GetNonEnumeratedCount: Integer;
begin
  Result := TEnumerableExtension(Self).GetNonEnumeratedCount;
end;

function TEnumerableExtension<T>.Skip(count: Integer): IEnumerable<T>;
begin
  TEnumerableExtension(Self).Skip(count, Result, TEnumerableExtension<T>);
end;

function TEnumerableExtension<T>.Take(count: Integer): IEnumerable<T>;
begin
  TEnumerableExtension(Self).Take(count, Result, TEnumerableExtension<T>);
end;

function TEnumerableExtension<T>.ToArray: TArray<T>;
begin
  case fKind of //FI:W535
    TExtensionKind.Partition:
      if TEnumerableExtension(Self).PartitionToArray(Pointer(Result), TypeInfo(TArray<T>)) then Exit;
    TExtensionKind.Memoize:
    begin
      TEnumerableExtension(Self).MemoizeToArray(Pointer(Result), TypeInfo(TArray<T>));
      Exit;
    end;
    TExtensionKind.Ordered..TExtensionKind.Shuffled:
    begin
      Result := fSource.ToArray;
      TCollectionThunks<T>.ProcessArray(fKind, Result, IComparer<T>(fPredicate));
      Exit;
    end;
  end;
  Result := inherited ToArray;
end;

function TEnumerableExtension<T>.TryGetElementAt(var value: T; index: Integer): Boolean;
begin
  Result := TEnumerableExtension(Self).TryGetElementAt(value, index,
    TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.GetDefault);
end;

function TEnumerableExtension<T>.TryGetFirst(var value: T): Boolean;
begin
  Result := TEnumerableExtension(Self).TryGetFirst(value,
    TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.GetDefault);
end;

function TEnumerableExtension<T>.TryGetLast(var value: T): Boolean;
begin
  Result := TEnumerableExtension(Self).TryGetLast(value,
    TCollectionThunks<T>.GetCurrent, TCollectionThunks<T>.GetDefault);
end;

{$ENDREGION}


{$REGION 'TArrayIterator<T>'}

class function TArrayIterator<T>.Create(const values: TArray<T>): IReadOnlyList<T>;
var
  iterator: TArrayIterator<T>;
begin
  iterator := TArrayIterator<T>.Create;
  iterator.fItems := values;
  Result := iterator;
end;

class function TArrayIterator<T>.Create(const values: array of T): IReadOnlyList<T>;
var
  iterator: TArrayIterator<T>;
  count: Integer;
begin
  iterator := TArrayIterator<T>.Create;
  count := Length(values);
  if count > 0 then
  begin
    SetLength(iterator.fItems, count);
    if TType.IsManaged<T> then
      MoveManaged(@values[0], @iterator.fItems[0], TypeInfo(T), count)
    else
      System.Move(values[0], iterator.fItems[0], SizeOf(T) * count);
  end;
  Result := iterator;
end;

function TArrayIterator<T>.CopyTo(var values: TArray<T>; index: Integer): Integer;
begin
  Result := DynArrayLength(fItems);
  if Result > 0 then
    if TType.IsManaged<T> then
      MoveManaged(@fItems[0], @values[index], TypeInfo(T), Result)
    else
      System.Move(fItems[0], values[index], SizeOf(T) * Result);
end;

function TArrayIterator<T>.GetCount: Integer;
begin
  Result := DynArrayLength(fItems);
end;

function TArrayIterator<T>.GetEnumerator: IEnumerator<T>; //FI:W521
var
  items: Pointer;
begin
  items := Pointer(fItems);
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fItems := TArray<T>(items);
    fCount := DynArrayLength(items);
  end;
end;

function TArrayIterator<T>.GetItem(index: Integer): T;
begin
  CheckIndex(index, DynArrayLength(fItems));

  Result := fItems[index];
end;

function TArrayIterator<T>.GetNonEnumeratedCount: Integer;
begin
  Result := DynArrayLength(fItems);
end;

function TArrayIterator<T>.IndexOf(const item: T): Integer;
begin
  Result := IndexOf(item, 0, DynArrayLength(fItems));
end;

function TArrayIterator<T>.IndexOf(const item: T; index: Integer): Integer;
begin
  Result := IndexOf(item, index, DynArrayLength(fItems) - index);
end;

function TArrayIterator<T>.IndexOf(const item: T; index, count: Integer): Integer;
var
  comparer: Pointer;
begin
  CheckRange(index, count, DynArrayLength(fItems));

  comparer := _LookupVtableInfo(giEqualityComparer, GetElementType, SizeOf(T));
  for Result := index to index + count - 1 do
    if IEqualityComparer<T>(comparer).Equals(fItems[Result], item) then
      Exit;
  Result := -1;
end;

function TArrayIterator<T>.ToArray: TArray<T>;
begin
  Result := Copy(fItems);
end;

{$ENDREGION}


{$REGION 'TArrayIterator<T>.TEnumerator'}

function TArrayIterator<T>.TEnumerator.GetCurrent: T;
begin
  Result := fItems[fIndex - 1];
end;

function TArrayIterator<T>.TEnumerator.MoveNext: Boolean;
begin
  Inc(fIndex);
  Result := fIndex <= fCount;
end;

{$ENDREGION}


{$REGION 'TFoldedArrayIterator<T>'}

class function TFoldedArrayIterator<T>.Create(const values: TArray<T>;
  elementType: PTypeInfo): IReadOnlyList<T>;
var
  iterator: TFoldedArrayIterator<T>;
begin
  iterator := TFoldedArrayIterator<T>.Create;
  iterator.fItems := values;
  iterator.fElementType := elementType;
  Result := iterator;
end;

class function TFoldedArrayIterator<T>.Create(values: PPointer; count: Integer;
  elementType: PTypeInfo): IReadOnlyList<T>;
var
  iterator: TFoldedArrayIterator<T>;
begin
  iterator := TFoldedArrayIterator<T>.Create;
  iterator.fElementType := elementType;
  if count > 0 then
  begin
    SetLength(iterator.fItems, count);
    if TType.IsManaged<T> then
      MoveManaged(values, @iterator.fItems[0], TypeInfo(T), count)
    else
      System.Move(values^, iterator.fItems[0], SizeOf(T) * count);
  end;
  Result := iterator;
end;

function TFoldedArrayIterator<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


end.
