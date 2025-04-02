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

unit Spring.Collections.Extensions;

interface

uses
  Classes,
  Generics.Defaults,
  Types,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Lists;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  TEmptyEnumerable<T> = class sealed(TEnumerableBase<T>, IEnumerator<T>,
    IEnumerable<T>, IReadOnlyCollection<T>, IReadOnlyList<T>)
  private
    class var fInstance: IReadOnlyList<T>;
    function GetCurrent: T;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetItem(index: Integer): T;
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}
  public
    class constructor Create;
    class destructor Destroy;

  {$REGION 'Implements IEnumerator<T>'}
    function MoveNext: Boolean;
  {$ENDREGION}

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Skip(count: Integer): IEnumerable<T>; overload;
    function Take(count: Integer): IEnumerable<T>; overload;
    function ToArray: TArray<T>;
    function TryGetElementAt(var value: T; index: Integer): Boolean;
    function TryGetFirst(var value: T): Boolean; overload;
    function TryGetLast(var value: T): Boolean; overload;
  {$ENDREGION}

  {$REGION 'Implements IReadOnlyCollection<T>'}
    function CopyTo(var values: TArray<T>; index: Integer): Integer;
  {$ENDREGION}

  {$REGION 'Implements IReadOnlyList<T>'}
    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;
  {$ENDREGION}

    class property Instance: IReadOnlyList<T> read fInstance;
  end;

  TWhereIterator<T> = class(TSourceIterator<T>)
  private
    fPredicate: Predicate<T>;
    fEnumerator: IEnumerator<T>;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>;
      const predicate: Predicate<T>);
  end deprecated;

  TWhereIndexIterator<T> = class(TSourceIterator<T>)
  private
    fPredicate: Func<T, Integer, Boolean>;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>;
      const predicate: Func<T, Integer, Boolean>);
  end;

  TSkipIterator<T> = class(TSourceIterator<T>)
  private
    fCount: Integer;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>; count: Integer);
  end deprecated;

  TSkipWhileIterator<T> = class(TSourceIterator<T>)
  private
    fPredicate: Predicate<T>;
    fPredicateIndex: Func<T, Integer, Boolean>;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
    fYielding: Boolean;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: Predicate<T>); overload;
    constructor Create(const source: IEnumerable<T>; const predicate: Func<T, Integer, Boolean>); overload;
  end deprecated;

  TTakeIterator<T> = class(TSourceIterator<T>)
  private
    fCount: Integer;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>; count: Integer);
  end deprecated;

  TTakeWhileIterator<T> = class(TSourceIterator<T>)
  private
    fPredicate: Predicate<T>;
    fPredicateIndex: Func<T, Integer, Boolean>;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: Predicate<T>); overload;
    constructor Create(const source: IEnumerable<T>; const predicate: Func<T, Integer, Boolean>); overload;
  end deprecated;

  TConcatIterator<T> = class(TSourceIterator<T>)
  private
    fSecond: IEnumerable<T>;
    fEnumerator: IEnumerator<T>;
    fFlag: Boolean;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const first, second: IEnumerable<T>);
  end deprecated;

  TReversedIterator<T> = class(TSourceIterator<T>)
  private
    fBuffer: TArray<T>;
    fIndex: Integer;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>);
  end deprecated;

  TDistinctIterator<T> = class(TSourceIterator<T>)
  private
    fComparer: IEqualityComparer<T>;
    fSet: ISet<T>;
    fEnumerator: IEnumerator<T>;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>; const comparer: IEqualityComparer<T>);
  end;

  TDistinctByIterator<T,TKey> = class(TSourceIterator<T>)
  private
    fKeySelector: Func<T, TKey>;
    fComparer: IEqualityComparer<TKey>;
    fSet: ISet<TKey>;
    fEnumerator: IEnumerator<T>;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
      const comparer: IEqualityComparer<TKey>);
  end;

  TRangeIterator = class(TEnumerableBase<Integer>,
    IEnumerable<Integer>, IReadOnlyCollection<Integer>, IReadOnlyList<Integer>)
  private type
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      fState, fCurrent, fEnd: Integer;
      function GetCurrent: Integer;
      function MoveNext: Boolean;
      function _Release: Integer; stdcall;
      class function Create(start, count: Integer): IEnumerator<Integer>; static;
    end;
  const
    Enumerator_Vtable: TEnumeratorVtable = (
      @NopQueryInterface,
      @RecAddRef,
      @TEnumerator._Release,
      @TEnumerator.GetCurrent,
      @TEnumerator.MoveNext
    );
  private
    fStart, fCount: Integer;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetItem(index: Integer): Integer;
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}
  public
    constructor Create(start, count: Integer);

  {$REGION 'Implements IEnumerable<Integer>'}
    function GetEnumerator: IEnumerator<Integer>;
    function Ordered: IEnumerable<Integer>; overload;
    function Skip(count: Integer): IEnumerable<Integer>;
    function SkipLast(count: Integer): IEnumerable<Integer>;
    function Take(count: Integer): IEnumerable<Integer>;
    function TakeLast(count: Integer): IEnumerable<Integer>;
    function ToArray: TArray<Integer>;
    function TryGetElementAt(var value: Integer; index: Integer): Boolean;
    function TryGetFirst(var value: Integer): Boolean; overload;
    function TryGetLast(var value: Integer): Boolean; overload;
  {$ENDREGION}

  {$REGION 'Implements IReadOnlyCollection<Integer>'}
    function CopyTo(var values: TArray<Integer>; index: Integer): Integer;
  {$ENDREGION}

  {$REGION 'Implements IReadOnlyList<Integer>'}
    function IndexOf(const item: Integer): Integer; overload;
    function IndexOf(const item: Integer; index: Integer): Integer; overload;
    function IndexOf(const item: Integer; index, count: Integer): Integer; overload;
  {$ENDREGION}
  end;

  TExceptIterator<T> = class(TSourceIterator<T>)
  private
    fSecond: IEnumerable<T>;
    fComparer: IEqualityComparer<T>;
    fSet: ISet<T>;
    fEnumerator: IEnumerator<T>;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const first, second: IEnumerable<T>); overload;
    constructor Create(const first, second: IEnumerable<T>; const comparer: IEqualityComparer<T>); overload;
  end;

  TIntersectIterator<T> = class(TSourceIterator<T>)
  private
    fSecond: IEnumerable<T>;
    fComparer: IEqualityComparer<T>;
    fSet: ISet<T>;
    fEnumerator: IEnumerator<T>;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const first, second: IEnumerable<T>); overload;
    constructor Create(const first, second: IEnumerable<T>; const comparer: IEqualityComparer<T>); overload;
  end;

  TUnionIterator<T> = class(TSourceIterator<T>)
  private
    fSecond: IEnumerable<T>;
    fComparer: IEqualityComparer<T>;
    fSet: ISet<T>;
    fEnumerator: IEnumerator<T>;
    fFlag: Boolean;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const first, second: IEnumerable<T>); overload;
    constructor Create(const first, second: IEnumerable<T>; const comparer: IEqualityComparer<T>); overload;
  end;

  TSelectIterator<TSource, TResult> = class(TIterator<TResult>,
    IEnumerable<TResult>, IPartition<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fSelector: Func<TSource, TResult>;
    fEnumerator: IEnumerator<TSource>;
    function GetNonEnumeratedCount: Integer;
  protected
    function Clone: TIterator<TResult>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TResult): Boolean; override;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const selector: Func<TSource, TResult>);
    function TryGetElementAt(var value: TResult; index: Integer): Boolean;
    function TryGetFirst(var value: TResult): Boolean;
    function TryGetLast(var value: TResult): Boolean;
  end;

  TSelectIndexIterator<TSource, TResult> = class(TIterator<TResult>, IEnumerable<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fSelector: Func<TSource, Integer, TResult>;
    fEnumerator: IEnumerator<TSource>;
    fIndex: Integer;
  protected
    function Clone: TIterator<TResult>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TResult): Boolean; override;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const selector: Func<TSource, Integer, TResult>);
  end;

  // binary compatible interface to Spring.Collections.ILookup<TKey,TValue> but foldable
  ILookupInternal<TKey, TElement> = interface(IReadOnlyCollection<IInterface>) //FI:W524
    ['{B2380533-F2B1-465B-84B2-97FA79A6EE09}'] //FI:W530
    function GetItem(const key: TKey): IReadOnlyCollection<TElement>;
    function Contains(const key: TKey): Boolean;
  end;

  TGroupedEnumerable<TSource, TKey, TElement> = class(
    TEnumerableBase<IInterface>,
    IEnumerable<IInterface>)
  private
    type
      TEnumerator = class(TRefCountedObject, IEnumerator<IInterface>)
      private
        fSource: IEnumerable<TSource>;
        fKeySelector: Func<TSource, TKey>;
        fElementSelector: Func<TSource, TElement>;
        fComparer: IEqualityComparer<TKey>;
        fLookup: ILookupInternal<TKey, TElement>;
        fEnumerator: IEnumerator<IInterface>;
        function GetCurrent: IInterface;
        procedure Start;
      public
        constructor Create(const source: IEnumerable<TSource>;
          const keySelector: Func<TSource, TKey>;
          const elementSelector: Func<TSource, TElement>;
          const comparer: IEqualityComparer<TKey>);
        function MoveNext: Boolean;
      end;
  private
    fSource: IEnumerable<TSource>;
    fKeySelector: Func<TSource, TKey>;
    fElementSelector: Func<TSource, TElement>;
    fComparer: IEqualityComparer<TKey>;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>;
      const elementSelector: Func<TSource, TElement>); overload;
    constructor Create(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>;
      const elementSelector: Func<TSource, TElement>;
      const comparer: IEqualityComparer<TKey>); overload;
    function GetEnumerator: IEnumerator<IInterface>;
  end;

  TGroupedEnumerable<TSource, TKey, TElement, TResult> = class(
    TEnumerableBase<TResult>, IEnumerable<TResult>)
  private
    type
      TEnumerator = class(TRefCountedObject, IEnumerator<TResult>)
      private
        fSource: IEnumerator<IInterface>;
        fResultSelector: Func<TKey, IEnumerable<TElement>, TResult>;
        function GetCurrent: TResult;
      public
        constructor Create(const source: IEnumerator<IInterface>;
          const resultSelector: Func<TKey, IEnumerable<TElement>, TResult>);
        function MoveNext: Boolean;
      end;
  private
    fSource: IEnumerable<TSource>;
    fKeySelector: Func<TSource, TKey>;
    fElementSelector: Func<TSource, TElement>;
    fComparer: IEqualityComparer<TKey>;
    fResultSelector: Func<TKey, IEnumerable<TElement>, TResult>;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>;
      const elementSelector: Func<TSource, TElement>;
      const resultSelector: Func<TKey, IEnumerable<TElement>, TResult>); overload;
    constructor Create(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>;
      const elementSelector: Func<TSource, TElement>;
      const resultSelector: Func<TKey, IEnumerable<TElement>, TResult>;
      const comparer: IEqualityComparer<TKey>); overload;
    function GetEnumerator: IEnumerator<TResult>;
  end;

  TGroupings = class(TList<TObject>)
  protected
    procedure Changed(const item: TObject;
      action: TCollectionChangedAction); override;
  end;

  TLookup<TKey, TElement> = class(TEnumerableBase<IInterface>,
    IEnumerable<IInterface>, IReadOnlyCollection<IInterface>, ILookupInternal<TKey, TElement>)
  private
    type
      TGrouping = class(TEnumerableBase<TElement>,
        IEnumerable<TElement>, IReadOnlyCollection<TElement>, IGrouping<TKey, TElement>)
      private
        fKey: TKey;
        fElements: IList<TElement>;
      {$REGION 'Property Accessors'}
        function GetCount: Integer;
        function GetKey: TKey;
        function GetNonEnumeratedCount: Integer;
      {$ENDREGION}
        procedure Add(const item: TElement);
      public
        constructor Create(const key: TKey);
        function GetEnumerator: IEnumerator<TElement>;
        property Key: TKey read GetKey;
      end;

      TEnumerator = class(TRefCountedObject, IEnumerator<IInterface>)
      private
        fSource: TLookup<TKey, TElement>;
        fIndex: Integer;
        function GetCurrent: IInterface;
      public
        constructor Create(const source: TLookup<TKey, TElement>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
  private
    fComparer: IEqualityComparer<TKey>;
    fGroupings: IList<TObject>;
    fGroupingKeys: IDictionary<TKey, TGrouping>;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetGrouping(const key: TKey; create: Boolean): TGrouping;
    function GetItem(const key: TKey): IReadOnlyCollection<TElement>;
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}
  public
    constructor Create; reintroduce; overload;
    constructor Create(const comparer: IEqualityComparer<TKey>); overload;
    class function Create<TSource>(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>;
      const elementSelector: Func<TSource, TElement>): TLookup<TKey, TElement>; overload; static;
    class function Create<TSource>(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>;
      const elementSelector: Func<TSource, TElement>;
      const comparer: IEqualityComparer<TKey>): TLookup<TKey, TElement>; overload; static;
    class function CreateForJoin(const source: IEnumerable<TElement>;
      const keySelector: Func<TElement, TKey>;
      const comparer: IEqualityComparer<TKey>): TLookup<TKey, TElement>; static;

    function Contains(const key: TKey): Boolean; overload;
    function GetEnumerator: IEnumerator<IInterface>;
    property Items[const key: TKey]: IReadOnlyCollection<TElement> read GetItem; default;
  end;

  TJoinIterator<TOuter, TInner, TKey, TResult> = class(TIterator<TResult>, IEnumerable<TResult>)
  private
    fOuter: IEnumerable<TOuter>;
    fInner: IEnumerable<TInner>;
    fOuterKeySelector: Func<TOuter, TKey>;
    fInnerKeySelector: Func<TInner, TKey>;
    fResultSelector: Func<TOuter, TInner, TResult>;
    fComparer: IEqualityComparer<TKey>;
    fLookup: TLookup<TKey, TInner>;
    fEnumerator: IEnumerator<TOuter>;
    fFlag: Boolean;
    fGrouping: TLookup<TKey, TInner>.TGrouping;
    fIndex: Integer;
  protected
    function Clone: TIterator<TResult>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TResult): Boolean; override;
  public
    constructor Create(const outer: IEnumerable<TOuter>;
      const inner: IEnumerable<TInner>;
      const outerKeySelector: Func<TOuter, TKey>;
      const innerKeySelector: Func<TInner, TKey>;
      const resultSelector: Func<TOuter, TInner, TResult>); overload;
    constructor Create(const outer: IEnumerable<TOuter>;
      const inner: IEnumerable<TInner>;
      const outerKeySelector: Func<TOuter, TKey>;
      const innerKeySelector: Func<TInner, TKey>;
      const resultSelector: Func<TOuter, TInner, TResult>;
      const comparer: IEqualityComparer<TKey>); overload;
    destructor Destroy; override;
  end;

  TGroupJoinIterator<TOuter, TInner, TKey, TResult> = class(TIterator<TResult>, IEnumerable<TResult>)
  private
    fOuter: IEnumerable<TOuter>;
    fInner: IEnumerable<TInner>;
    fOuterKeySelector: Func<TOuter, TKey>;
    fInnerKeySelector: Func<TInner, TKey>;
    fResultSelector: Func<TOuter, IEnumerable<TInner>, TResult>;
    fComparer: IEqualityComparer<TKey>;
    fLookup: TLookup<TKey, TInner>;
    fEnumerator: IEnumerator<TOuter>;
  protected
    function Clone: TIterator<TResult>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TResult): Boolean; override;
  public
    constructor Create(const outer: IEnumerable<TOuter>;
      const inner: IEnumerable<TInner>;
      const outerKeySelector: Func<TOuter, TKey>;
      const innerKeySelector: Func<TInner, TKey>;
      const resultSelector: Func<TOuter, IEnumerable<TInner>, TResult>); overload;
    constructor Create(const outer: IEnumerable<TOuter>;
      const inner: IEnumerable<TInner>;
      const outerKeySelector: Func<TOuter, TKey>;
      const innerKeySelector: Func<TInner, TKey>;
      const resultSelector: Func<TOuter, IEnumerable<TInner>, TResult>;
      const comparer: IEqualityComparer<TKey>); overload;
    destructor Destroy; override;
  end;

  TSelectManyIterator<TSource, TResult> = class(TIterator<TResult>, IEnumerable<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fSelector: Func<TSource, IEnumerable<TResult>>;
    fEnumerator: IEnumerator<TSource>;
    fFlag: Boolean;
    fEnumerator2: IEnumerator<TResult>;
  protected
    function Clone: TIterator<TResult>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TResult): Boolean; override;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const selector: Func<TSource, IEnumerable<TResult>>);
  end;

  TSelectManyIndexIterator<TSource, TResult> = class(TIterator<TResult>, IEnumerable<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fSelector: Func<TSource, Integer, IEnumerable<TResult>>;
    fEnumerator: IEnumerator<TSource>;
    fFlag: Boolean;
    fIndex: Integer;
    fEnumerator2: IEnumerator<TResult>;
  protected
    function Clone: TIterator<TResult>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TResult): Boolean; override;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const selector: Func<TSource, Integer, IEnumerable<TResult>>);
  end;

  TSelectManyIterator<TSource, TCollection, TResult> = class(TIterator<TResult>, IEnumerable<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fCollectionSelector: Func<TSource, IEnumerable<TCollection>>;
    fResultSelector: Func<TSource, TCollection, TResult>;
    fEnumerator: IEnumerator<TSource>;
    fFlag: Boolean;
    fEnumerator2: IEnumerator<TCollection>;
    fItem: TSource;
  protected
    function Clone: TIterator<TResult>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TResult): Boolean; override;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const collectionSelector: Func<TSource, IEnumerable<TCollection>>;
      const resultSelector: Func<TSource, TCollection, TResult>);
  end;

  TSelectManyIndexIterator<TSource, TCollection, TResult> = class(TIterator<TResult>, IEnumerable<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fCollectionSelector: Func<TSource, Integer, IEnumerable<TCollection>>;
    fResultSelector: Func<TSource, TCollection, TResult>;
    fEnumerator: IEnumerator<TSource>;
    fFlag: Boolean;
    fIndex: Integer;
    fEnumerator2: IEnumerator<TCollection>;
    fItem: TSource;
  protected
    function Clone: TIterator<TResult>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TResult): Boolean; override;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const collectionSelector: Func<TSource, Integer, IEnumerable<TCollection>>;
      const resultSelector: Func<TSource, TCollection, TResult>);
  end;

  IEnumerableSorter<T> = interface
    procedure ComputeKeys(const elements: TArray<T>; count: Integer);
    function CompareKeys(index1, index2: Integer): Integer;
    function Sort(var elements: TArray<T>; count: Integer): TIntegerDynArray;
  end;

  TEnumerableSorter<T> = class(TRefCountedObject, IEnumerableSorter<T>)
  protected
    procedure ComputeKeys(const elements: TArray<T>; count: Integer); virtual; abstract;
    function CompareKeys(index1, index2: Integer): Integer; virtual; abstract;
    function Sort(var elements: TArray<T>; count: Integer): TIntegerDynArray;
  end;

  TEnumerableSorter<TElement, TKey> = class(TEnumerableSorter<TElement>)
  private
    fKeySelector: Func<TElement, TKey>;
    fComparer: IComparer<TKey>;
    fDescending: Boolean;
    fNext: IEnumerableSorter<TElement>;
    fKeys: TArray<TKey>;
  protected
    procedure ComputeKeys(const elements: TArray<TElement>; count: Integer); override;
    function CompareKeys(index1, index2: Integer): Integer; override;
  public
    constructor Create(const keySelector: Func<TElement, TKey>;
      const comparer: IComparer<TKey>; descending: Boolean;
      const next: IEnumerableSorter<TElement>);
  end;

  TOrderedEnumerable<T> = class(TEnumerableBase<T>, IEnumerable<T>)
  private
    type
      TEnumerator = class(TRefCountedObject, IEnumerator<T>)
      private
        fBuffer: TArray<T>;
        fMap: TIntegerDynArray;
        fIndex: Integer;
        function GetCurrent: T;
      public
        constructor Create(const source: IEnumerable<T>;
          const sorter: IEnumerableSorter<T>);
        function MoveNext: Boolean;
      end;
  private
    fSource: IEnumerable<T>;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}
  protected
    function GetElementType: PTypeInfo; override;
    function GetEnumerableSorter(
      const next: IEnumerableSorter<T>): IEnumerableSorter<T>; virtual; abstract;
  public
    function GetEnumerator: IEnumerator<T>;
  end;

  TOrderedEnumerable<TElement, TKey> = class(TOrderedEnumerable<TElement>)
  private
    fParent: TOrderedEnumerable<TElement>;
    fKeySelector: Func<TElement, TKey>;
    fComparer: IComparer<TKey>;
    fDescending: Boolean;
  protected
    function GetEnumerableSorter(
      const next: IEnumerableSorter<TElement>): IEnumerableSorter<TElement>; override;
  public
    constructor Create(const source: IEnumerable<TElement>;
      const keySelector: Func<TElement, TKey>); overload;
    constructor Create(const source: IEnumerable<TElement>;
      const keySelector: Func<TElement, TKey>; const comparer: IComparer<TKey>;
      descending: Boolean = False); overload;
  end;

  TOrderedIterator<T> = class(TSourceIterator<T>)
  private
    fComparer: IComparer<T>;
    fValues: TArray<T>;
    fIndex: Integer;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>;
      const comparer: IComparer<T>);// descending: Boolean);
  end deprecated;

  TZipIterator<TFirst, TSecond, TResult> = class(TIterator<TResult>, IEnumerable<TResult>)
  private
    fFirst: IEnumerable<TFirst>;
    fSecond: IEnumerable<TSecond>;
    fResultSelector: Func<TFirst, TSecond, TResult>;
    fEnumerator1: IEnumerator<TFirst>;
    fEnumerator2: IEnumerator<TSecond>;
  protected
    function Clone: TIterator<TResult>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TResult): Boolean; override;
  public
    constructor Create(const first: IEnumerable<TFirst>;
      const second: IEnumerable<TSecond>;
      const resultSelector: Func<TFirst, TSecond, TResult>);
  end;

  TDefaultIfEmptyIterator<T> = class(TSourceIterator<T>)
  private
    fDefaultValue: T;
    fEnumerator: IEnumerator<T>;
    fFlag: Boolean;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>; const defaultValue: T);
  end;

  TExtremaByIterator<T, TKey> = class(TSourceIterator<T>)
  private
    fKeySelector: Func<T, TKey>;
    fComparer: TComparison<TKey>;
    fResult: IList<T>;
    fEnumerator: IEnumerator<T>;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const comparer: TComparison<TKey>);
  end;

  TCastIterator<T, TResult> = class(TIterator<TResult>, IEnumerable<TResult>)
  private
    fSource: IEnumerable<T>;
    fEnumerator: IEnumerator<T>;
  protected
    function Clone: TIterator<TResult>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TResult): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>);
  end;

  TOfTypeIterator<T, TResult> = class(TIterator<TResult>, IEnumerable<TResult>)
  private
    fSource: IEnumerable<T>;
    fEnumerator: IEnumerator<T>;
  protected
    function Clone: TIterator<TResult>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TResult): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>);
  end;

  TRepeatIterator<T> = class(TIterator<T>, IEnumerable<T>)
  private
    fElement: T;
    fCount: Integer;
    fIndex: Integer;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}
  protected
    function Clone: TIterator<T>; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const element: T; count: Integer);
  end;

  TChunkIterator<T> = class(TIterator<TArray<T>>, IEnumerable<TArray<T>>)
  private
    fSource: IEnumerable<T>;
    fEnumerator: IEnumerator<T>;
    fSize, fCapacity: Integer;
  protected
    function Clone: TIterator<TArray<T>>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TArray<T>): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>; size: Integer);
  end;

  TAnonymousIterator<T> = class(TIterator<T>, IEnumerable<T>)
  private
    fCount: Func<Integer>;
    fItems: Func<Integer, T>;
    fIndex: Integer;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}
  protected
    function Clone: TIterator<T>; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const count: Func<Integer>; const items: Func<Integer, T>);
  end;

  // in this unit and not Spring.Collections.Lists because of some
  // compiler glitch on older versions related to units referencing
  // each other in a particular order
  TStringsAdapter = class(TCollectionBase<string>, IInterface,
    IEnumerable<string>, IReadOnlyCollection<string>, IReadOnlyList<string>,
    ICollection<string>, IList<string>)
  private type
  {$REGION 'Nested Types'}
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: TStringsAdapter;
      fIndex, fCount: Integer;
      fVersion: Integer;
      function GetCurrent: string;
      function GetCurrentStringList: string;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fStrings: TStrings;
    fVersion: Integer;
    fIsStringList: Boolean;
    fOwnsObject: Boolean;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItem(index: Integer): string;
    function GetNonEnumeratedCount: Integer;
    function GetOwnsObjects: Boolean;
    procedure SetCapacity(value: Integer);
    procedure SetCount(value: Integer);
    procedure SetItem(index: Integer; const value: string);
    procedure SetOwnsObjects(value: Boolean);
  {$ENDREGION}
    procedure DeleteRangeInternal(index, count: Integer; doClear: Boolean);
    function AsReadOnly: IReadOnlyList<string>;
    function TryGet(var value: string; index: Integer): Boolean;
  public
    constructor Create(const strings: TStrings; ownsObject: Boolean);
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<string>'}
    function GetEnumerator: IEnumerator<string>;

    function ToArray: TArray<string>;

    function TryGetElementAt(var value: string; index: Integer): Boolean;
    function TryGetFirst(var value: string): Boolean; overload;
    function TryGetLast(var value: string): Boolean; overload;
    function TryGetSingle(var value: string): Boolean; overload;
  {$ENDREGION}

  {$REGION 'Implements ICollections<string>'}
    function Remove(const item: string): Boolean;
    function Extract(const item: string): string;

    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IList<string>'}
    function Add(const item: string): Integer; overload;

    procedure Insert(index: Integer; const item: string);
    procedure InsertRange(index: Integer; const values: array of string); overload;
    procedure InsertRange(index: Integer; const values: IEnumerable<string>); overload;

    procedure Delete(index: Integer);
    procedure DeleteRange(index, count: Integer);

    function ExtractAt(index: Integer): string;
    function ExtractRange(index, count: Integer): TArray<string>; overload;

    function GetRange(index, count: Integer): IList<string>;

    procedure Exchange(index1, index2: Integer);
    procedure Move(sourceIndex, targetIndex: Integer);

    function IndexOf(const item: string): Integer; overload;
    function IndexOf(const item: string; index: Integer): Integer; overload;
    function IndexOf(const item: string; index, count: Integer): Integer; overload;

    function LastIndexOf(const item: string): Integer; overload;
    function LastIndexOf(const item: string; index: Integer): Integer; overload;
    function LastIndexOf(const item: string; index, count: Integer): Integer; overload;

    procedure Reverse; overload;
    procedure Reverse(index, count: Integer); overload;

    procedure Sort; overload;
    procedure Sort(const comparer: IComparer<string>); overload;
    procedure Sort(const comparer: TComparison<string>); overload;
    procedure Sort(const comparer: TComparison<string>; index, count: Integer); overload;
    procedure Sort(const comparer: IComparer<string>; index, count: Integer); overload;

    procedure TrimExcess;
  {$ENDREGION}
  end;

implementation

uses
  Rtti,
  SysUtils,
  TypInfo,
  Spring.Comparers,
  Spring.ResourceStrings;


{$REGION 'TEmptyEnumerable<T>'}

class constructor TEmptyEnumerable<T>.Create;
begin
  fInstance := TEmptyEnumerable<T>.Create;
end;

class destructor TEmptyEnumerable<T>.Destroy;
begin
  fInstance := nil;
end;

function TEmptyEnumerable<T>.CopyTo(var values: TArray<T>; index: Integer): Integer;
begin
  Result := 0;
end;

function TEmptyEnumerable<T>.GetCount: Integer;
begin
  Result := 0;
end;

function TEmptyEnumerable<T>.GetCurrent: T; //FI:W521
begin
  RaiseHelper.NoElements;
  __SuppressWarning(Result);
end;

function TEmptyEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := Self;
end;

function TEmptyEnumerable<T>.GetItem(index: Integer): T; //FI:W521
begin
  RaiseHelper.ArgumentOutOfRange_Index;
  __SuppressWarning(Result);
end;

function TEmptyEnumerable<T>.GetNonEnumeratedCount: Integer;
begin
  Result := 0;
end;

function TEmptyEnumerable<T>.MoveNext: Boolean;
begin
  Result := False;
end;

function TEmptyEnumerable<T>.IndexOf(const item: T): Integer;
begin
  Result := -1;
end;

function TEmptyEnumerable<T>.IndexOf(const item: T; index: Integer): Integer;
begin
  Result := -1;
end;

function TEmptyEnumerable<T>.IndexOf(const item: T; index,
  count: Integer): Integer;
begin
  Result := -1;
end;

function TEmptyEnumerable<T>.Skip(count: Integer): IEnumerable<T>;
begin
  Result := Self;
end;

function TEmptyEnumerable<T>.Take(count: Integer): IEnumerable<T>;
begin
  Result := Self;
end;

function TEmptyEnumerable<T>.ToArray: TArray<T>;
begin
  Result := nil;
end;

function TEmptyEnumerable<T>.TryGetElementAt(var value: T; index: Integer): Boolean;
begin
  value := Default(T);
  Result := False;
end;

function TEmptyEnumerable<T>.TryGetFirst(var value: T): Boolean;
begin
  value := Default(T);
  Result := False;
end;

function TEmptyEnumerable<T>.TryGetLast(var value: T): Boolean;
begin
  value := Default(T);
  Result := False;
end;

{$ENDREGION}


{$REGION 'TWhereIterator<T>'}

constructor TWhereIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: Predicate<T>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  inherited Create(source);
  fPredicate := predicate;
end;

function TWhereIterator<T>.Clone: TIterator<T>;
begin
  Result := TWhereIterator<T>.Create(Source, fPredicate);
end;

procedure TWhereIterator<T>.Dispose;
begin
  fEnumerator := nil;
end;

function TWhereIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  while fEnumerator.MoveNext do
  begin
    current := fEnumerator.Current;
    if fPredicate(current) then
      Exit(True);
  end;
  Result := False;
end;

procedure TWhereIterator<T>.Start;
begin
  fEnumerator := Source.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TWhereIndexIterator<T>'}

constructor TWhereIndexIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: Func<T, Integer, Boolean>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  inherited Create(source);
  fPredicate := predicate;
end;

function TWhereIndexIterator<T>.Clone: TIterator<T>;
begin
  Result := TWhereIndexIterator<T>.Create(Source, fPredicate);
end;

procedure TWhereIndexIterator<T>.Dispose;
begin
  fEnumerator := nil;
end;

function TWhereIndexIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  while fEnumerator.MoveNext do
  begin
    current := fEnumerator.Current;
    Inc(fIndex);
    if fPredicate(current, fIndex) then
      Exit(True);
  end;
  Result := False;
end;

procedure TWhereIndexIterator<T>.Start;
begin
  fIndex := -1;
  fEnumerator := Source.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TSkipIterator<T>'}

constructor TSkipIterator<T>.Create(const source: IEnumerable<T>;
  count: Integer);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);

  inherited Create(source);
  fCount := count;
end;

function TSkipIterator<T>.Clone: TIterator<T>;
begin
  Result := TSkipIterator<T>.Create(Source, fCount);
end;

procedure TSkipIterator<T>.Dispose;
begin
  fEnumerator := nil;
end;

function TSkipIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  while (fIndex > 0) and fEnumerator.MoveNext do
    Dec(fIndex);
  Result := fEnumerator.MoveNext;
  if Result then
    current := fEnumerator.Current;
end;

procedure TSkipIterator<T>.Start;
begin
  fEnumerator := Source.GetEnumerator;
  fIndex := fCount;
end;

{$ENDREGION}


{$REGION 'TSkipWhileIterator<T>'}

constructor TSkipWhileIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: Predicate<T>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  inherited Create(source);
  fPredicate := predicate;
end;

constructor TSkipWhileIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: Func<T, Integer, Boolean>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  inherited Create(source);
  fPredicateIndex := predicate;
end;

function TSkipWhileIterator<T>.Clone: TIterator<T>;
begin
  if Assigned(fPredicate) then
    Result := TSkipWhileIterator<T>.Create(Source, fPredicate)
  else
    Result := TSkipWhileIterator<T>.Create(Source, fPredicateIndex);
end;

procedure TSkipWhileIterator<T>.Dispose;
begin
  fEnumerator := nil;
end;

function TSkipWhileIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  while fEnumerator.MoveNext do
  begin
    current := fEnumerator.Current;

    if not fYielding then
      if Assigned(fPredicate) then
      begin
        if not fPredicate(current) then
          fYielding := True;
      end
      else
        if not fPredicateIndex(current, fIndex) then
          fYielding := True;

    Inc(fIndex);
    if fYielding then
      Exit(True);
  end;
  Result := False;
end;

procedure TSkipWhileIterator<T>.Start;
begin
  fEnumerator := Source.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TTakeIterator<T>'}

constructor TTakeIterator<T>.Create(const source: IEnumerable<T>;
  count: Integer);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);

  inherited Create(source);
  fCount := count;
end;

function TTakeIterator<T>.Clone: TIterator<T>;
begin
  Result := TTakeIterator<T>.Create(Source, fCount);
end;

procedure TTakeIterator<T>.Dispose;
begin
  fEnumerator := nil;
end;

function TTakeIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  Result := (fIndex < fCount) and fEnumerator.MoveNext;
  if Result then
  begin
    current := fEnumerator.Current;
    Inc(fIndex);
  end;
end;

procedure TTakeIterator<T>.Start;
begin
  fEnumerator := Source.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TTakeWhileIterator<T>'}

constructor TTakeWhileIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: Predicate<T>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  inherited Create(source);
  fPredicate := predicate;
end;

constructor TTakeWhileIterator<T>.Create(
  const source: IEnumerable<T>;
  const predicate: Func<T, Integer, Boolean>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  inherited Create(source);
  fPredicateIndex := predicate;
end;

function TTakeWhileIterator<T>.Clone: TIterator<T>;
begin
  if Assigned(fPredicate) then
    Result := TTakeWhileIterator<T>.Create(Source, fPredicate)
  else
    Result := TTakeWhileIterator<T>.Create(Source, fPredicateIndex);
end;

procedure TTakeWhileIterator<T>.Dispose;
begin
  fEnumerator := nil;
end;

function TTakeWhileIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  Result := fEnumerator.MoveNext;
  if Result then
  begin
    current := fEnumerator.Current;
    if Assigned(fPredicate) then
      Result := fPredicate(current)
    else
      Result := fPredicateIndex(current, fIndex);
    Inc(fIndex);
  end;
end;

procedure TTakeWhileIterator<T>.Start;
begin
  fEnumerator := Source.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TConcatIterator<T>'}

constructor TConcatIterator<T>.Create(const first, second: IEnumerable<T>);
begin
  if not Assigned(first) then RaiseHelper.ArgumentNil(ExceptionArgument.first);
  if not Assigned(second) then RaiseHelper.ArgumentNil(ExceptionArgument.second);

  inherited Create(first);
  fSecond := second;
end;

function TConcatIterator<T>.Clone: TIterator<T>;
begin
  Result := TConcatIterator<T>.Create(Source, fSecond);
end;

procedure TConcatIterator<T>.Dispose;
begin
  fEnumerator := nil;
end;

function TConcatIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  while Assigned(fEnumerator) do
  begin
    if fEnumerator.MoveNext then
    begin
      current := fEnumerator.Current;
      Exit(True);
    end;

    if fFlag then
      Break;

    fFlag := True;
    Start;
  end;
  Result := False;
end;

procedure TConcatIterator<T>.Start;
begin
  if not fFlag then
    fEnumerator := Source.GetEnumerator
  else
    fEnumerator := fSecond.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TReversedIterator<T>'}

constructor TReversedIterator<T>.Create(const source: IEnumerable<T>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);

  inherited Create(source);
end;

function TReversedIterator<T>.Clone: TIterator<T>;
begin
  Result := TReversedIterator<T>.Create(Source);
end;

procedure TReversedIterator<T>.Dispose;
begin
  fBuffer := nil;
end;

function TReversedIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  Result := fIndex > 0;
  if Result then
  begin
    Dec(fIndex);
    current := fBuffer[fIndex];
  end;
end;

procedure TReversedIterator<T>.Start;
begin
  fBuffer := Source.ToArray;
  fIndex := DynArrayLength(fBuffer);
end;

{$ENDREGION}


{$REGION 'TDistinctIterator<T>'}

constructor TDistinctIterator<T>.Create(const source: IEnumerable<T>;
  const comparer: IEqualityComparer<T>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);

  inherited Create(source);
  fComparer := comparer;
end;

function TDistinctIterator<T>.Clone: TIterator<T>;
begin
  Result := TDistinctIterator<T>.Create(Source, fComparer);
end;

procedure TDistinctIterator<T>.Dispose;
begin
  fSet := nil;
  fEnumerator := nil;
end;

function TDistinctIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  repeat
    Result := fEnumerator.MoveNext;
    if not Result then Break;
    current := fEnumerator.Current;
    Result := fSet.Add(current);
  until Result;
end;

procedure TDistinctIterator<T>.Start;
begin
  fSet := TCollections.CreateSet<T>(fComparer);
  fEnumerator := Source.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TDistinctByIterator<T, TKey>'}

constructor TDistinctByIterator<T, TKey>.Create(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>; const comparer: IEqualityComparer<TKey>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);

  inherited Create(source);
  fKeySelector := keySelector;
  fComparer := comparer;
end;

function TDistinctByIterator<T, TKey>.Clone: TIterator<T>;
begin
  Result := TDistinctByIterator<T, TKey>.Create(Source, fKeySelector, fComparer);
end;

procedure TDistinctByIterator<T, TKey>.Dispose;
begin
  fSet := nil;
  fEnumerator := nil;
end;

function TDistinctByIterator<T, TKey>.TryMoveNext(var current: T): Boolean;
begin
  while fEnumerator.MoveNext do
  begin
    current := fEnumerator.Current;
    if fSet.Add(fKeySelector(current)) then
      Exit(True);
  end;
  Result := False;
end;

procedure TDistinctByIterator<T, TKey>.Start;
begin
  fSet := TCollections.CreateSet<TKey>(fComparer);
  fEnumerator := Source.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TRangeIterator'}

constructor TRangeIterator.Create(start, count: Integer);
begin
  if (count < 0) or ((Int64(start) + count - 1) > MaxInt) then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count);

  fStart := start;
  fCount := count;
end;

function TRangeIterator.CopyTo(var values: TArray<Integer>; index: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
  begin
    values[index] := fStart + i;
    Inc(index);
  end;
  Result := fCount;
end;

function TRangeIterator.GetCount: Integer;
begin
  Result := fCount;
end;

function TRangeIterator.GetEnumerator: IEnumerator<Integer>;
begin
  Result := TEnumerator.Create(fStart, fCount);
end;

function TRangeIterator.GetItem(index: Integer): Integer;
begin
  CheckIndex(index, fCount);

  Result := fStart + index;
end;

function TRangeIterator.GetNonEnumeratedCount: Integer;
begin
  Result := fCount;
end;

function TRangeIterator.IndexOf(const item: Integer): Integer;
begin
  if fCount > 0 then
    Result := IndexOf(item, 0, fCount)
  else
    Result := -1;
end;

function TRangeIterator.IndexOf(const item: Integer; index: Integer): Integer;
begin
  if fCount > 0 then
    Result := IndexOf(item, index, fCount - index)
  else
    Result := -1;
end;

function TRangeIterator.IndexOf(const item: Integer; index,
  count: Integer): Integer;
begin
  CheckRange(index, count, fCount);

  if (item >= fStart) and (item <= fStart + fCount) then
    Result := item - fStart
  else
    Result := -1;
end;

function TRangeIterator.Ordered: IEnumerable<Integer>;
begin
  Result := Self;
end;

function TRangeIterator.Skip(count: Integer): IEnumerable<Integer>;
begin
  if count < 0 then
    count := 0;
  if count >= fCount then
    Result := TEnumerable.Empty<Integer>
  else
    Result := TRangeIterator.Create(fStart + count, fCount - count);
end;

function TRangeIterator.SkipLast(count: Integer): IEnumerable<Integer>;
begin
  if count < 0 then
    count := 0;
  if count >= fCount then
    Result := TEnumerable.Empty<Integer>
  else
    Result := TRangeIterator.Create(fStart, fCount - count);
end;

function TRangeIterator.Take(count: Integer): IEnumerable<Integer>;
begin
  if count <= 0 then
    Result := TEnumerable.Empty<Integer>
  else if count >= fCount then
    Result := IEnumerable<Integer>(this)
  else
    Result := TRangeIterator.Create(fStart, count);
end;

function TRangeIterator.TakeLast(count: Integer): IEnumerable<Integer>;
begin
  if count <= 0 then
    Result := TEnumerable.Empty<Integer>
  else if count >= fCount then
    Result := IEnumerable<Integer>(this)
  else
    Result := TRangeIterator.Create(fStart + fCount - count, count);
end;

function TRangeIterator.ToArray: TArray<Integer>;
var
  i, current: Integer;
  values: PInteger;
begin
  SetLength(Result, fCount);
  values := Pointer(Result);
  current := fStart;
  for i := 0 to fCount - 1 do
  begin
    {$POINTERMATH ON}
    values[i] := current;
    {$POINTERMATH OFF}
    Inc(current);
  end;
end;

function TRangeIterator.TryGetElementAt(var value: Integer; index: Integer): Boolean;
begin
  if Cardinal(index) < Cardinal(fCount) then
  begin
    value := fStart + index;
    Result := True;
  end
  else
  begin
    value := 0;
    Result := False;
  end;
end;

function TRangeIterator.TryGetFirst(var value: Integer): Boolean;
begin
  // assume that there exists no instance with fCount = 0 - it would be TEmptyEnumerable<Integer> then
  value := fStart;
  Result := True;
end;

function TRangeIterator.TryGetLast(var value: Integer): Boolean;
begin
  // assume that there exists no instance with fCount = 0 - it would be TEmptyEnumerable<Integer> then
  value := fStart + fCount - 1;
  Result := True;
end;

{$ENDREGION}


{$REGION 'TRangeIterator.TEnumerator'}

class function TRangeIterator.TEnumerator.Create(start, count: Integer): IEnumerator<Integer>;
begin
  Result := nil;
  GetMem(Pointer(Result), SizeOf(TEnumerator));
  with PEnumerator(Result)^ do
  begin
    Vtable := @Enumerator_Vtable;
    RefCount := 1;
    fState := 0;
    {$Q-}
    fCurrent := start - 1;
    fEnd := start + count;
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  end;
end;

function TRangeIterator.TEnumerator.GetCurrent: Integer;
begin
  Result := fCurrent;
end;

function TRangeIterator.TEnumerator.MoveNext: Boolean;
var
  current: Integer;
begin
  if fState = 0 then
  begin
    current := fCurrent;
    {$Q-}
    Inc(current);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    fCurrent := current;
    if current <> fEnd then
      Exit(True);
  end;
  fState := -1;
  Result := False;
end;

function TRangeIterator.TEnumerator._Release: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
    FreeMem(@Self);
end;

{$ENDREGION}


{$REGION 'TExceptIterator<T>'}

constructor TExceptIterator<T>.Create(const first, second: IEnumerable<T>);
begin
  Create(first, second, nil);
end;

constructor TExceptIterator<T>.Create(const first, second: IEnumerable<T>;
  const comparer: IEqualityComparer<T>);
begin
  if not Assigned(first) then RaiseHelper.ArgumentNil(ExceptionArgument.first);
  if not Assigned(second) then RaiseHelper.ArgumentNil(ExceptionArgument.second);

  inherited Create(first);
  fSecond := second;
  fComparer := comparer;
end;

function TExceptIterator<T>.Clone: TIterator<T>;
begin
  Result := TExceptIterator<T>.Create(Source, fSecond, fComparer);
end;

procedure TExceptIterator<T>.Dispose;
begin
  fSet := nil;
  fEnumerator := nil;
end;

function TExceptIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  while fEnumerator.MoveNext do
  begin
    current := fEnumerator.Current;
    if fSet.Add(current) then
      Exit(True);
  end;
  Result := False;
end;

procedure TExceptIterator<T>.Start;
begin
  fSet := TCollections.CreateSet<T>(fComparer);
  fSet.AddRange(fSecond);
  fEnumerator := Source.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TIntersectIterator<T>'}

constructor TIntersectIterator<T>.Create(const first, second: IEnumerable<T>);
begin
  Create(first, second, nil);
end;

constructor TIntersectIterator<T>.Create(const first, second: IEnumerable<T>;
  const comparer: IEqualityComparer<T>);
begin
  if not Assigned(first) then RaiseHelper.ArgumentNil(ExceptionArgument.first);
  if not Assigned(second) then RaiseHelper.ArgumentNil(ExceptionArgument.second);

  inherited Create(first);
  fSecond := second;
  fComparer := comparer;
end;

function TIntersectIterator<T>.Clone: TIterator<T>;
begin
  Result := TIntersectIterator<T>.Create(Source, fSecond, fComparer);
end;

procedure TIntersectIterator<T>.Dispose;
begin
  fSet := nil;
  fEnumerator := nil;
end;

function TIntersectIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  while fEnumerator.MoveNext do
  begin
    current := fEnumerator.Current;
    if fSet.Remove(current) then
      Exit(True);
  end;
  Result := False;
end;

procedure TIntersectIterator<T>.Start;
begin
  fSet := TCollections.CreateSet<T>(fComparer);
  fSet.AddRange(fSecond);
  fEnumerator := Source.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TUnionIterator<T>'}

constructor TUnionIterator<T>.Create(const first, second: IEnumerable<T>);
begin
  Create(first, second, nil);
end;

constructor TUnionIterator<T>.Create(const first, second: IEnumerable<T>;
  const comparer: IEqualityComparer<T>);
begin
  if not Assigned(first) then RaiseHelper.ArgumentNil(ExceptionArgument.first);
  if not Assigned(second) then RaiseHelper.ArgumentNil(ExceptionArgument.second);

  inherited Create(first);
  fSecond := second;
  fComparer := comparer;
end;

function TUnionIterator<T>.Clone: TIterator<T>;
begin
  Result := TUnionIterator<T>.Create(Source, fSecond, fComparer);
end;

procedure TUnionIterator<T>.Dispose;
begin
  fSet := nil;
  fEnumerator := nil;
end;

function TUnionIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  while Assigned(fEnumerator) do
  begin
    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      if fSet.Add(current) then
        Exit(True);
    end;

    if fFlag then
      Break;

    fFlag := True;
    Start;
  end;
  Result := False;
end;

procedure TUnionIterator<T>.Start;
begin
  if not fFlag then
  begin
    fSet := TCollections.CreateSet<T>(fComparer);
    fEnumerator := Source.GetEnumerator;
  end
  else
    fEnumerator := fSecond.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TSelectIterator<TSource, TResult>'}

constructor TSelectIterator<TSource, TResult>.Create(
  const source: IEnumerable<TSource>; const selector: Func<TSource, TResult>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(selector) then RaiseHelper.ArgumentNil(ExceptionArgument.selector);

  fSource := source;
  fSelector := selector;
end;

function TSelectIterator<TSource, TResult>.Clone: TIterator<TResult>;
begin
  Result := TSelectIterator<TSource, TResult>.Create(fSource, fSelector)
end;

procedure TSelectIterator<TSource, TResult>.Dispose;
begin
  fEnumerator := nil;
end;

function TSelectIterator<TSource, TResult>.GetNonEnumeratedCount: Integer;
begin
  Result := fSource.GetNonEnumeratedCount;
end;

function TSelectIterator<TSource, TResult>.TryGetElementAt(var value: TResult;
  index: Integer): Boolean;
var
  sourceValue: TSource;
begin
  Result := fSource.TryGetElementAt(sourceValue, index);
  if Result then
  begin
    value := fSelector(sourceValue);
    Result := True;
  end;
end;

function TSelectIterator<TSource, TResult>.TryGetFirst(var value: TResult): Boolean;
var
  sourceValue: TSource;
begin
  Result := fSource.TryGetFirst(sourceValue);
  if Result then
  begin
    value := fSelector(sourceValue);
    Result := True;
  end;
end;

function TSelectIterator<TSource, TResult>.TryGetLast(var value: TResult): Boolean;
var
  sourceValue: TSource;
begin
  Result := fSource.TryGetLast(sourceValue);
  if Result then
  begin
    value := fSelector(sourceValue);
    Result := True;
  end;
end;

function TSelectIterator<TSource, TResult>.TryMoveNext(var current: TResult): Boolean;
begin
  Result := fEnumerator.MoveNext;
  if Result then
  begin
    current := fSelector(fEnumerator.Current);
    Result := True;
  end;
end;

procedure TSelectIterator<TSource, TResult>.Start;
begin
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TSelectIndexIterator<TSource, TResult>'}

constructor TSelectIndexIterator<TSource, TResult>.Create(
  const source: IEnumerable<TSource>;
  const selector: Func<TSource, Integer, TResult>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(selector) then RaiseHelper.ArgumentNil(ExceptionArgument.selector);

  fSource := source;
  fSelector := selector;
end;

function TSelectIndexIterator<TSource, TResult>.Clone: TIterator<TResult>;
begin
  Result := TSelectIndexIterator<TSource, TResult>.Create(fSource, fSelector);
end;

procedure TSelectIndexIterator<TSource, TResult>.Dispose;
begin
  fEnumerator := nil;
end;

function TSelectIndexIterator<TSource, TResult>.TryMoveNext(var current: TResult): Boolean;
var
  item: TSource;
begin
  Result := fEnumerator.MoveNext;
  if Result then
  begin
    item := fEnumerator.Current;
    current := fSelector(item, fIndex);
    Inc(fIndex);
  end;
end;

procedure TSelectIndexIterator<TSource, TResult>.Start;
begin
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TGroupedEnumerable<TSource, TKey, TElement>'}

constructor TGroupedEnumerable<TSource, TKey, TElement>.Create(
  const source: IEnumerable<TSource>; const keySelector: Func<TSource, TKey>;
  const elementSelector: Func<TSource, TElement>);
begin
  Create(source, keySelector, elementSelector, nil);
end;

constructor TGroupedEnumerable<TSource, TKey, TElement>.Create(
  const source: IEnumerable<TSource>; const keySelector: Func<TSource, TKey>;
  const elementSelector: Func<TSource, TElement>;
  const comparer: IEqualityComparer<TKey>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(keySelector) then RaiseHelper.ArgumentNil(ExceptionArgument.keySelector);
  if not Assigned(elementSelector) then RaiseHelper.ArgumentNil(ExceptionArgument.elementSelector);

  fSource := source;
  fKeySelector := keySelector;
  fElementSelector := elementSelector;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := IEqualityComparer<TKey>(_LookupVtableInfo(giEqualityComparer, TypeInfo(TKey), SizeOf(TKey)));
end;

function TGroupedEnumerable<TSource, TKey, TElement>.GetEnumerator: IEnumerator<IInterface>;
begin
  Result := TEnumerator.Create(fSource, fKeySelector, fElementSelector, fComparer);
end;

{$ENDREGION}


{$REGION 'TGroupedEnumerable<TSource, TKey, TElement>.TEnumerator'}

constructor TGroupedEnumerable<TSource, TKey, TElement>.TEnumerator.Create(
  const source: IEnumerable<TSource>; const keySelector: Func<TSource, TKey>;
  const elementSelector: Func<TSource, TElement>;
  const comparer: IEqualityComparer<TKey>);
begin
  fSource := source;
  fKeySelector := keySelector;
  fElementSelector := elementSelector;
  fComparer := comparer;
end;

function TGroupedEnumerable<TSource, TKey, TElement>.TEnumerator.GetCurrent: IInterface;
begin
  Result := fEnumerator.Current;
end;

function TGroupedEnumerable<TSource, TKey, TElement>.TEnumerator.MoveNext: Boolean;
begin
  if not Assigned(fEnumerator) then
    Start;

  Result := fEnumerator.MoveNext;
end;

procedure TGroupedEnumerable<TSource, TKey, TElement>.TEnumerator.Start;
var
  lookup: TLookup<TKey, TElement>;
  item: TSource;
begin
  lookup := TLookup<TKey, TElement>.Create(fComparer);
  fLookup := lookup;
  for item in fSource do
    lookup.GetGrouping(fKeySelector(item), True).Add(fElementSelector(item));
  fEnumerator := fLookup.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TGroupedEnumerable<TSource, TKey, TElement, TResult>'}

constructor TGroupedEnumerable<TSource, TKey, TElement, TResult>.Create(
  const source: IEnumerable<TSource>; const keySelector: Func<TSource, TKey>;
  const elementSelector: Func<TSource, TElement>;
  const resultSelector: Func<TKey, IEnumerable<TElement>, TResult>);
begin
  Create(source, keySelector, elementSelector, resultSelector, nil);
end;

constructor TGroupedEnumerable<TSource, TKey, TElement, TResult>.Create(
  const source: IEnumerable<TSource>; const keySelector: Func<TSource, TKey>;
  const elementSelector: Func<TSource, TElement>;
  const resultSelector: Func<TKey, IEnumerable<TElement>, TResult>;
  const comparer: IEqualityComparer<TKey>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(keySelector) then RaiseHelper.ArgumentNil(ExceptionArgument.keySelector);
  if not Assigned(elementSelector) then RaiseHelper.ArgumentNil(ExceptionArgument.elementSelector);
  if not Assigned(resultSelector) then RaiseHelper.ArgumentNil(ExceptionArgument.resultSelector);

  fSource := source;
  fKeySelector := keySelector;
  fElementSelector := elementSelector;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := IEqualityComparer<TKey>(_LookupVtableInfo(giEqualityComparer, TypeInfo(TKey), SizeOf(TKey)));
  fResultSelector := resultSelector;
end;

function TGroupedEnumerable<TSource, TKey, TElement, TResult>.GetEnumerator: IEnumerator<TResult>;
var
  lookup: TLookup<TKey, TElement>;
  item: TSource;
begin
  // TODO: deferred execution ?
  lookup := TLookup<TKey, TElement>.Create(fComparer);
  for item in fSource do
    lookup.GetGrouping(fKeySelector(item), True).Add(fElementSelector(item));
  Result := TEnumerator.Create(lookup.GetEnumerator, fResultSelector);
end;

{$ENDREGION}


{$REGION 'TGroupedEnumerable<TSource, TKey, TElement, TResult>.TEnumerator'}

constructor TGroupedEnumerable<TSource, TKey, TElement, TResult>.TEnumerator.Create(
  const source: IEnumerator<IInterface>;
  const resultSelector: Func<TKey, IEnumerable<TElement>, TResult>);
begin
  fSource := source;
  fResultSelector := resultSelector;
end;

function TGroupedEnumerable<TSource, TKey, TElement, TResult>.TEnumerator.GetCurrent: TResult;
var
  g: IGrouping<TKey, TElement>;
begin
  IInterface(g) := fSource.Current;
  Result := fResultSelector(g.Key, g);
end;

function TGroupedEnumerable<TSource, TKey, TElement, TResult>.TEnumerator.MoveNext: Boolean;
begin
  Result := fSource.MoveNext;
end;

{$ENDREGION}


{$REGION 'TLookup<TKey, TElement>'}

constructor TLookup<TKey, TElement>.Create;
begin
  Create(nil);
end;

constructor TLookup<TKey, TElement>.Create(const comparer: IEqualityComparer<TKey>);
begin
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := IEqualityComparer<TKey>(_LookupVtableInfo(giEqualityComparer, TypeInfo(TKey), SizeOf(TKey)));
  fGroupings := TGroupings.Create;
  fGroupingKeys := TCollections.CreateDictionary<TKey, TGrouping>(fComparer);
end;

class function TLookup<TKey, TElement>.Create<TSource>(
  const source: IEnumerable<TSource>; const keySelector: Func<TSource, TKey>;
  const elementSelector: Func<TSource, TElement>): TLookup<TKey, TElement>;
begin
  Result := Create<TSource>(source, keySelector, elementSelector, nil);
end;

class function TLookup<TKey, TElement>.Create<TSource>(
  const source: IEnumerable<TSource>; const keySelector: Func<TSource, TKey>;
  const elementSelector: Func<TSource, TElement>;
  const comparer: IEqualityComparer<TKey>): TLookup<TKey, TElement>;
var
  item: TSource;
begin
  Result := TLookup<TKey, TElement>.Create(comparer);
  try
    for item in source do
      Result.GetGrouping(keySelector(item), True).Add(elementSelector(item));
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class function TLookup<TKey, TElement>.CreateForJoin(
  const source: IEnumerable<TElement>; const keySelector: Func<TElement, TKey>;
  const comparer: IEqualityComparer<TKey>): TLookup<TKey, TElement>;
var
  element: TElement;
  key: TKey;
begin
  Result := TLookup<TKey, TElement>.Create(comparer);
  try
    for element in source do
    begin
      key := keySelector(element);
      Result.GetGrouping(key, True).Add(element);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TLookup<TKey, TElement>.Contains(const key: TKey): Boolean;
begin
  Result := fGroupingKeys.ContainsKey(key);
end;

function TLookup<TKey, TElement>.GetCount: Integer;
begin
  Result := fGroupings.Count;
end;

function TLookup<TKey, TElement>.GetEnumerator: IEnumerator<IInterface>;
begin
  Result := TEnumerator.Create(Self);
end;

function TLookup<TKey, TElement>.GetGrouping(const key: TKey;
  create: Boolean): TGrouping;
var
  grouping: TGrouping;
begin
  if not fGroupingKeys.TryGetValue(key, Result) and create then
  begin
    grouping := TGrouping.Create(key);
    fGroupings.Add(grouping);
    fGroupingKeys.Add(key, grouping);
    Result := grouping;
  end;
end;

function TLookup<TKey, TElement>.GetItem(
  const key: TKey): IReadOnlyCollection<TElement>;
begin
  Result := GetGrouping(key, False);
  if not Assigned(Result) then
    Result := TEnumerable.Empty<TElement>;
end;

function TLookup<TKey, TElement>.GetNonEnumeratedCount: Integer;
begin
  Result := fGroupings.Count;
end;

{$ENDREGION}


{$REGION 'TLookup<TKey, TElement>.TGrouping'}

constructor TLookup<TKey, TElement>.TGrouping.Create(const key: TKey);
begin
  fKey := key;
  fElements := TCollections.CreateList<TElement>;
end;

procedure TLookup<TKey, TElement>.TGrouping.Add(const item: TElement);
begin
  fElements.Add(item);
end;

function TLookup<TKey, TElement>.TGrouping.GetCount: Integer;
begin
  Result := fElements.Count;
end;

function TLookup<TKey, TElement>.TGrouping.GetEnumerator: IEnumerator<TElement>;
begin
  Result := fElements.GetEnumerator;
end;

function TLookup<TKey, TElement>.TGrouping.GetKey: TKey;
begin
  Result := fKey;
end;

function TLookup<TKey, TElement>.TGrouping.GetNonEnumeratedCount: Integer;
begin
  Result := fElements.Count;
end;

{$ENDREGION}


{$REGION 'TGroupings'}

procedure TGroupings.Changed(const item: TObject;
  action: TCollectionChangedAction);
begin
  inherited Changed(item, action);
  case action of //FI:W535
    caAdded: TRefCountedObject(item)._AddRef;
    caRemoved: TRefCountedObject(item)._Release;
  end;
end;

{$ENDREGION}


{$REGION 'TLookup<TKey, TElement>.TEnumerator'}

constructor TLookup<TKey, TElement>.TEnumerator.Create(
  const source: TLookup<TKey, TElement>);
begin
  fSource := source;
  fSource._AddRef;
  fIndex := -1;
end;

destructor TLookup<TKey, TElement>.TEnumerator.Destroy; //FI:W504
begin
  fSource._Release;
end;

function TLookup<TKey, TElement>.TEnumerator.GetCurrent: IInterface;
begin
  IGrouping<TKey, TElement>(Result) := TGrouping(fSource.fGroupings[fIndex]);
end;

function TLookup<TKey, TElement>.TEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < fSource.fGroupings.Count - 1;
  if Result then
    Inc(fIndex);
end;

{$ENDREGION}


{$REGION 'TJoinIterator<TOuter, TInner, TKey, TResult>'}

constructor TJoinIterator<TOuter, TInner, TKey, TResult>.Create(
  const outer: IEnumerable<TOuter>; const inner: IEnumerable<TInner>;
  const outerKeySelector: Func<TOuter, TKey>;
  const innerKeySelector: Func<TInner, TKey>;
  const resultSelector: Func<TOuter, TInner, TResult>);
begin
  Create(outer, inner, outerKeySelector, innerKeySelector, resultSelector, nil);
end;

constructor TJoinIterator<TOuter, TInner, TKey, TResult>.Create(
  const outer: IEnumerable<TOuter>; const inner: IEnumerable<TInner>;
  const outerKeySelector: Func<TOuter, TKey>;
  const innerKeySelector: Func<TInner, TKey>;
  const resultSelector: Func<TOuter, TInner, TResult>;
  const comparer: IEqualityComparer<TKey>);
begin
  if not Assigned(outer) then RaiseHelper.ArgumentNil(ExceptionArgument.outer);
  if not Assigned(inner) then RaiseHelper.ArgumentNil(ExceptionArgument.inner);
  if not Assigned(outerKeySelector) then RaiseHelper.ArgumentNil(ExceptionArgument.outerKeySelector);
  if not Assigned(innerKeySelector) then RaiseHelper.ArgumentNil(ExceptionArgument.innerKeySelector);
  if not Assigned(resultSelector) then RaiseHelper.ArgumentNil(ExceptionArgument.resultSelector);

  fOuter := outer;
  fInner := inner;
  fOuterKeySelector := outerKeySelector;
  fInnerKeySelector := innerKeySelector;
  fResultSelector := resultSelector;
  fComparer := comparer;
end;

destructor TJoinIterator<TOuter, TInner, TKey, TResult>.Destroy; //FI:W504
begin
  fLookup.Free;
end;

function TJoinIterator<TOuter, TInner, TKey, TResult>.Clone: TIterator<TResult>;
begin
  Result := TJoinIterator<TOuter, TInner, TKey, TResult>.Create(fOuter, fInner,
    fOuterKeySelector, fInnerKeySelector, fResultSelector, fComparer);
end;

procedure TJoinIterator<TOuter, TInner, TKey, TResult>.Dispose;
begin
  fEnumerator := nil;
  FreeAndNil(fLookup);
end;

function TJoinIterator<TOuter, TInner, TKey, TResult>.TryMoveNext(var current: TResult): Boolean;
var
  item: TOuter;
begin
  while fFlag or fEnumerator.MoveNext do
  begin
    item := fEnumerator.Current;
    if not fFlag then
    begin
      fGrouping := fLookup.GetGrouping(fOuterKeySelector(item), False);
      if not Assigned(fGrouping) then
        Continue;
      fFlag := True;
      fIndex := 0;
    end;

    if fIndex < fGrouping.fElements.Count then
    begin
      current := fResultSelector(item, fGrouping.fElements[fIndex]);
      Inc(fIndex);
      Exit(True);
    end
    else
      fFlag := False;
  end;
  Result := False;
end;

procedure TJoinIterator<TOuter, TInner, TKey, TResult>.Start;
begin
  fLookup := TLookup<TKey, TInner>.CreateForJoin(fInner, fInnerKeySelector, fComparer);
  fEnumerator := fOuter.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TGroupJoinIterator<TOuter, TInner, TKey, TResult>'}

constructor TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Create(
  const outer: IEnumerable<TOuter>; const inner: IEnumerable<TInner>;
  const outerKeySelector: Func<TOuter, TKey>;
  const innerKeySelector: Func<TInner, TKey>;
  const resultSelector: Func<TOuter, IEnumerable<TInner>, TResult>);
begin
  Create(outer, inner, outerKeySelector, innerKeySelector, resultSelector, nil);
end;

constructor TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Create(
  const outer: IEnumerable<TOuter>; const inner: IEnumerable<TInner>;
  const outerKeySelector: Func<TOuter, TKey>;
  const innerKeySelector: Func<TInner, TKey>;
  const resultSelector: Func<TOuter, IEnumerable<TInner>, TResult>;
  const comparer: IEqualityComparer<TKey>);
begin
  if not Assigned(outer) then RaiseHelper.ArgumentNil(ExceptionArgument.outer);
  if not Assigned(inner) then RaiseHelper.ArgumentNil(ExceptionArgument.inner);
  if not Assigned(outerKeySelector) then RaiseHelper.ArgumentNil(ExceptionArgument.outerKeySelector);
  if not Assigned(innerKeySelector) then RaiseHelper.ArgumentNil(ExceptionArgument.innerKeySelector);
  if not Assigned(resultSelector) then RaiseHelper.ArgumentNil(ExceptionArgument.resultSelector);

  fOuter := outer;
  fInner := inner;
  fOuterKeySelector := outerKeySelector;
  fInnerKeySelector := innerKeySelector;
  fResultSelector := resultSelector;
  fComparer := comparer;
end;

destructor TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Destroy; //FI:W504
begin
  fLookup.Free;
end;

function TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Clone: TIterator<TResult>;
begin
  Result := TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Create(fOuter,
    fInner, fOuterKeySelector, fInnerKeySelector, fResultSelector, fComparer);
end;

procedure TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Dispose;
begin
  fEnumerator := nil;
end;

function TGroupJoinIterator<TOuter, TInner, TKey, TResult>.TryMoveNext(var current: TResult): Boolean;
var
  item: TOuter;
begin
  Result := fEnumerator.MoveNext;
  if Result then
  begin
    item := fEnumerator.Current;
    current := fResultSelector(item, fLookup[fOuterKeySelector(item)]);
  end;
end;

procedure TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Start;
begin
  fLookup := TLookup<TKey, TInner>.CreateForJoin(fInner, fInnerKeySelector, fComparer);
  fEnumerator := fOuter.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TSelectManyIterator<TSource, TResult>'}

constructor TSelectManyIterator<TSource, TResult>.Create(
  const source: IEnumerable<TSource>;
  const selector: Func<TSource, IEnumerable<TResult>>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(selector) then RaiseHelper.ArgumentNil(ExceptionArgument.selector);

  fSource := source;
  fSelector := selector;
end;

function TSelectManyIterator<TSource, TResult>.Clone: TIterator<TResult>;
begin
  Result := TSelectManyIterator<TSource, TResult>.Create(fSource, fSelector);
end;

procedure TSelectManyIterator<TSource, TResult>.Dispose;
begin
  fEnumerator := nil;
end;

function TSelectManyIterator<TSource, TResult>.TryMoveNext(var current: TResult): Boolean;
begin
  while not fFlag or fEnumerator.MoveNext do
  begin
    if fFlag then
      Start;

    if fEnumerator2.MoveNext then
    begin
      current := fEnumerator2.Current;
      Exit(True);
    end
    else
      fFlag := True;
  end;
  Result := False;
end;

procedure TSelectManyIterator<TSource, TResult>.Start;
var
  current: TSource;
  collection: IEnumerable<TResult>;
begin
  if not fFlag then
  begin
    fEnumerator := fSource.GetEnumerator;
    fFlag := True;
  end
  else
  begin
    current := fEnumerator.Current;
    collection := fSelector(current);
    fEnumerator2 := collection.GetEnumerator;
    fFlag := False;
  end;
end;

{$ENDREGION}


{$REGION 'TSelectManyIndexIterator<TSource, TResult>'}

constructor TSelectManyIndexIterator<TSource, TResult>.Create(
  const source: IEnumerable<TSource>;
  const selector: Func<TSource, Integer, IEnumerable<TResult>>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(selector) then RaiseHelper.ArgumentNil(ExceptionArgument.selector);

  fSource := source;
  fSelector := selector;
end;

function TSelectManyIndexIterator<TSource, TResult>.Clone: TIterator<TResult>;
begin
  Result := TSelectManyIndexIterator<TSource, TResult>.Create(fSource, fSelector);
end;

procedure TSelectManyIndexIterator<TSource, TResult>.Dispose;
begin
  fEnumerator := nil;
  fEnumerator2 := nil;
end;

function TSelectManyIndexIterator<TSource, TResult>.TryMoveNext(var current: TResult): Boolean;
begin
  while not fFlag or fEnumerator.MoveNext do
  begin
    if fFlag then
      Start;

    if fEnumerator2.MoveNext then
    begin
      current := fEnumerator2.Current;
      Exit(True);
    end
    else
      fFlag := True;
  end;
  Result := False;
end;

procedure TSelectManyIndexIterator<TSource, TResult>.Start;
var
  current: TSource;
  collection: IEnumerable<TResult>;
begin
  if not fFlag then
  begin
    fIndex := -1;
    fEnumerator := fSource.GetEnumerator;
    fFlag := True;
  end
  else
  begin
    current := fEnumerator.Current;
    Inc(fIndex);
    collection := fSelector(current, fIndex);
    fEnumerator2 := collection.GetEnumerator;
    fFlag := False;
  end;
end;

{$ENDREGION}


{$REGION 'TSelectManyIterator<TSource, TCollection, TResult>'}

constructor TSelectManyIterator<TSource, TCollection, TResult>.Create(
  const source: IEnumerable<TSource>;
  const collectionSelector: Func<TSource, IEnumerable<TCollection>>;
  const resultSelector: Func<TSource, TCollection, TResult>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(collectionSelector) then RaiseHelper.ArgumentNil(ExceptionArgument.collectionSelector);
  if not Assigned(resultSelector) then RaiseHelper.ArgumentNil(ExceptionArgument.resultSelector);

  fSource := source;
  fCollectionSelector := collectionSelector;
  fResultSelector := resultSelector;
end;

function TSelectManyIterator<TSource, TCollection, TResult>.Clone: TIterator<TResult>;
begin
  Result := TSelectManyIterator<TSource, TCollection, TResult>.Create(
    fSource, fCollectionSelector, fResultSelector);
end;

procedure TSelectManyIterator<TSource, TCollection, TResult>.Dispose;
begin
  fItem := Default(TSource);
  fEnumerator2 := nil;
  fEnumerator := nil;
end;

function TSelectManyIterator<TSource, TCollection, TResult>.TryMoveNext(var current: TResult): Boolean;
var
  subItem: TCollection;
begin
  while not fFlag or fEnumerator.MoveNext do
  begin
    if fFlag then
      Start;

    if fEnumerator2.MoveNext then
    begin
      subItem := fEnumerator2.Current;
      current := fResultSelector(fItem, subItem);
      Exit(True);
    end
    else
      fFlag := True;
  end;
  Result := False;
end;

procedure TSelectManyIterator<TSource, TCollection, TResult>.Start;
var
  collection: IEnumerable<TCollection>;
begin
  if not fFlag then
  begin
    fEnumerator := fSource.GetEnumerator;
    fFlag := True;
  end
  else
  begin
    fItem := fEnumerator.Current;
    collection := fCollectionSelector(fItem);
    fEnumerator2 := collection.GetEnumerator;
    fFlag := False;
  end;
end;

{$ENDREGION}


{$REGION 'TSelectManyIndexIterator<TSource, TCollection, TResult>'}

constructor TSelectManyIndexIterator<TSource, TCollection, TResult>.Create(
  const source: IEnumerable<TSource>;
  const collectionSelector: Func<TSource, Integer, IEnumerable<TCollection>>;
  const resultSelector: Func<TSource, TCollection, TResult>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(collectionSelector) then RaiseHelper.ArgumentNil(ExceptionArgument.collectionSelector);
  if not Assigned(resultSelector) then RaiseHelper.ArgumentNil(ExceptionArgument.resultSelector);

  fSource := source;
  fCollectionSelector := collectionSelector;
  fResultSelector := resultSelector;
end;

function TSelectManyIndexIterator<TSource, TCollection, TResult>.Clone: TIterator<TResult>;
begin
  Result := TSelectManyIndexIterator<TSource, TCollection, TResult>.Create(
    fSource, fCollectionSelector, fResultSelector);
end;

procedure TSelectManyIndexIterator<TSource, TCollection, TResult>.Dispose;
begin
  fItem := Default(TSource);
  fEnumerator2 := nil;
  fEnumerator := nil;
end;

function TSelectManyIndexIterator<TSource, TCollection, TResult>.TryMoveNext(var current: TResult): Boolean;
var
  subItem: TCollection;
begin
  while not fFlag or fEnumerator.MoveNext do
  begin
    if fFlag then
      Start;

    if fEnumerator2.MoveNext then
    begin
      subItem := fEnumerator2.Current;
      current := fResultSelector(fItem, subItem);
      Exit(True);
    end
    else
      fFlag := True;
  end;
  Result := False;
end;

procedure TSelectManyIndexIterator<TSource, TCollection, TResult>.Start;
var
  collection: IEnumerable<TCollection>;
begin
  if not fFlag then
  begin
    fIndex := -1;
    fEnumerator := fSource.GetEnumerator;
    fFlag := True;
  end
  else
  begin
    fItem := fEnumerator.Current;
    Inc(fIndex);
    collection := fCollectionSelector(fItem, fIndex);
    fEnumerator2 := collection.GetEnumerator;
    fFlag := False;
  end;
end;

{$ENDREGION}


{$REGION 'TEnumerableSorter<T>'}

function TEnumerableSorter<T>.Sort(var elements: TArray<T>;
  count: Integer): TIntegerDynArray;
var
  index: Integer;
  comparer: TComparison<Integer>;
begin
  ComputeKeys(elements, count);
  SetLength(Result, count);
  for index := 0 to count - 1 do
    Result[index] := index;
  comparer :=
    function(const Left, Right: Integer): Integer
    begin
      Result := CompareKeys(Left, Right);
    end;
  TArray.Sort<Integer>(Result, IComparer<Integer>(PPointer(@comparer)^));
end;

{$ENDREGION}


{$REGION 'TEnumerableSorter<TElement, TKey>'}

constructor TEnumerableSorter<TElement, TKey>.Create(
  const keySelector: Func<TElement, TKey>; const comparer: IComparer<TKey>;
  descending: Boolean; const next: IEnumerableSorter<TElement>);
begin
  if not Assigned(keySelector) then RaiseHelper.ArgumentNil(ExceptionArgument.keySelector);

  fKeySelector := keySelector;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := IComparer<TKey>(_LookupVtableInfo(giComparer, TypeInfo(TKey), SizeOf(TKey)));
  fDescending := descending;
  fNext := next;
end;

function TEnumerableSorter<TElement, TKey>.CompareKeys(index1,
  index2: Integer): Integer;
const
  MinInt = Low(Integer);
begin
  Result := fComparer.Compare(fKeys[index1], fKeys[index2]);
  if Result = 0 then
  begin
    if fNext = nil then
      Result := index1 - index2
    else
      Result := fNext.CompareKeys(index1, index2);
  end
  else if fDescending then
    if Result = MinInt then
      Result := 1
    else
      Result := -Result;
end;

procedure TEnumerableSorter<TElement, TKey>.ComputeKeys(
  const elements: TArray<TElement>; count: Integer);
var
  index: Integer;
begin
  SetLength(fKeys, count);
  for index := 0 to count - 1 do
    fKeys[index] := fKeySelector(elements[index]);
  if Assigned(fNext) then
    fNext.ComputeKeys(elements, count);
end;

{$ENDREGION}


{$REGION 'TOrderedEnumerable<T>'}

function TOrderedEnumerable<T>.GetCount: Integer;
begin
  Result := fSource.Count;
end;

function TOrderedEnumerable<T>.GetElementType: PTypeInfo;
begin
  Result := fSource.ElementType;
end;

function TOrderedEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(fSource, GetEnumerableSorter(nil));
end;

function TOrderedEnumerable<T>.GetNonEnumeratedCount: Integer;
begin
  Result := fSource.GetNonEnumeratedCount;
end;

{$ENDREGION}


{$REGION 'TOrderedEnumerable<T>.TEnumerator'}

constructor TOrderedEnumerable<T>.TEnumerator.Create(
  const source: IEnumerable<T>; const sorter: IEnumerableSorter<T>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(sorter) then RaiseHelper.ArgumentNil(ExceptionArgument.sorter);

  fBuffer := source.ToArray;
  fMap := sorter.Sort(fBuffer, DynArrayLength(fBuffer));
  fIndex := -1;
end;

function TOrderedEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  Result := fBuffer[fMap[fIndex]];
end;

function TOrderedEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < DynArrayHigh(fBuffer);
  if Result then
    Inc(fIndex);
end;

{$ENDREGION}


{$REGION 'TOrderedEnumerable<TElement, TKey>'}

constructor TOrderedEnumerable<TElement, TKey>.Create(
  const source: IEnumerable<TElement>;
  const keySelector: Func<TElement, TKey>);
begin
  Create(source, keySelector, nil);
end;

constructor TOrderedEnumerable<TElement, TKey>.Create(
  const source: IEnumerable<TElement>; const keySelector: Func<TElement, TKey>;
  const comparer: IComparer<TKey>; descending: Boolean);
var
  obj: TObject;
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(keySelector) then RaiseHelper.ArgumentNil(ExceptionArgument.keySelector);

  obj := source.AsObject;
  if obj is TOrderedEnumerable<TElement> then
  begin
    fParent := TOrderedEnumerable<TElement>(obj);
    fSource := TOrderedEnumerable<TElement>(obj).fSource;
  end
  else
    fSource := source;
  fKeySelector := keySelector;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := IComparer<TKey>(_LookupVtableInfo(giComparer, TypeInfo(TKey), SizeOf(TKey)));
  fDescending := descending;
end;

function TOrderedEnumerable<TElement, TKey>.GetEnumerableSorter(
  const next: IEnumerableSorter<TElement>): IEnumerableSorter<TElement>;
begin
  Result := TEnumerableSorter<TElement, TKey>.Create(
    fKeySelector, fComparer, fDescending, next);
  if Assigned(fParent) then
    Result := fParent.GetEnumerableSorter(Result);
end;

{$ENDREGION}


{$REGION 'TOrderedIterator<T>'}

constructor TOrderedIterator<T>.Create(const source: IEnumerable<T>;
  const comparer: IComparer<T>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);

  inherited Create(source);
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := Source.Comparer;
end;

function TOrderedIterator<T>.Clone: TIterator<T>;
begin
  Result := TOrderedIterator<T>.Create(Source, fComparer);
end;

procedure TOrderedIterator<T>.Dispose;
begin
  fValues := nil;
end;

function TOrderedIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  Result := fIndex < DynArrayLength(fValues);
  if Result then
  begin
    current := fValues[fIndex];
    Inc(fIndex);
  end;
end;

procedure TOrderedIterator<T>.Start;
begin
  fValues := Source.ToArray;
  TArray.Sort<T>(fValues, fComparer);
end;

{$ENDREGION}


{$REGION 'TZipIterator<TFirst, TSecond, TResult>'}

constructor TZipIterator<TFirst, TSecond, TResult>.Create(
  const first: IEnumerable<TFirst>; const second: IEnumerable<TSecond>;
  const resultSelector: Func<TFirst, TSecond, TResult>);
begin
  if not Assigned(first) then RaiseHelper.ArgumentNil(ExceptionArgument.first);
  if not Assigned(second) then RaiseHelper.ArgumentNil(ExceptionArgument.second);
  if not Assigned(resultSelector) then RaiseHelper.ArgumentNil(ExceptionArgument.resultSelector);

  fFirst := first;
  fSecond := second;
  fResultSelector := resultSelector;
end;

function TZipIterator<TFirst, TSecond, TResult>.Clone: TIterator<TResult>;
begin
  Result := TZipIterator<TFirst, TSecond, TResult>.Create(fFirst, fSecond, fResultSelector);
end;

procedure TZipIterator<TFirst, TSecond, TResult>.Dispose;
begin
  fEnumerator2 := nil;
  fEnumerator1 := nil;
end;

function TZipIterator<TFirst, TSecond, TResult>.TryMoveNext(var current: TResult): Boolean;
begin
  Result := fEnumerator1.MoveNext and fEnumerator2.MoveNext;
  if Result then
    current := fResultSelector(fEnumerator1.Current, fEnumerator2.Current);
end;

procedure TZipIterator<TFirst, TSecond, TResult>.Start;
begin
  fEnumerator1 := fFirst.GetEnumerator;
  fEnumerator2 := fSecond.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TDefaultIfEmptyIterator<T>'}

constructor TDefaultIfEmptyIterator<T>.Create(const source: IEnumerable<T>;
  const defaultValue: T);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);

  inherited Create(source);
  fDefaultValue := defaultValue;
end;

function TDefaultIfEmptyIterator<T>.Clone: TIterator<T>;
begin
  Result := TDefaultIfEmptyIterator<T>.Create(Source, fDefaultValue);
end;

procedure TDefaultIfEmptyIterator<T>.Dispose;
begin
  fEnumerator := nil;
end;

function TDefaultIfEmptyIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  if fEnumerator.MoveNext then
  begin
    current := fEnumerator.Current;
    fFlag := True;
    Exit(True);
  end
  else
    if not fFlag then
    begin
      current := fDefaultValue;
      fFlag := True;
      Exit(True);
    end;
  Result := False;
end;

procedure TDefaultIfEmptyIterator<T>.Start;
begin
  fEnumerator := Source.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TExtremaByIterator<T, TKey>'}

constructor TExtremaByIterator<T, TKey>.Create(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>; const comparer: TComparison<TKey>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(keySelector) then RaiseHelper.ArgumentNil(ExceptionArgument.keySelector);
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);

  inherited Create(source);
  fKeySelector := keySelector;
  fComparer := comparer;
end;

function TExtremaByIterator<T, TKey>.Clone: TIterator<T>;
begin
  Result := TExtremaByIterator<T, TKey>.Create(Source, fkeySelector, fComparer);
end;

procedure TExtremaByIterator<T, TKey>.Dispose;
begin
  fEnumerator := nil;
  fResult := nil;
end;

function TExtremaByIterator<T, TKey>.TryMoveNext(var current: T): Boolean;
begin
  Result := fEnumerator.MoveNext;
  if Result then
    current := fEnumerator.Current;
end;

procedure TExtremaByIterator<T, TKey>.Start;
var
  current: T;
  resultKey: TKey;
  key: TKey;
  compareResult: Integer;
begin
  fResult := TCollections.CreateList<T>;
  fEnumerator := Source.GetEnumerator;
  if not fEnumerator.MoveNext then
    RaiseHelper.NoElements;

  current := fEnumerator.Current;
  resultKey := fKeySelector(current);
  fResult.Add(current);

  while fEnumerator.MoveNext do
  begin
    current := fEnumerator.Current;
    key := fKeySelector(current);
    compareResult := fComparer(key, resultKey);
    if compareResult = 0 then
      fResult.Add(current)
    else if compareResult > 0 then
    begin
      fResult.Clear;
      fResult.Add(current);
      resultKey := key;
    end;
  end;

  fEnumerator := fResult.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TCastIterator<T, TResult>'}

constructor TCastIterator<T, TResult>.Create(const source: IEnumerable<T>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);

  fSource := source;
end;

procedure TCastIterator<T, TResult>.Dispose;
begin
  fEnumerator := nil;
end;

function TCastIterator<T, TResult>.Clone: TIterator<TResult>;
begin
  Result := TCastIterator<T, TResult>.Create(fSource);
end;

function TCastIterator<T, TResult>.TryMoveNext(var current: TResult): Boolean;
var
  item: T;
  value: TValue;
begin
  Result := fEnumerator.MoveNext;
  if Result then
  begin
    item := fEnumerator.Current;
    value := TValue.From(item, TypeInfo(T));
    value.AsType(TypeInfo(TResult), current);
  end;
end;

procedure TCastIterator<T, TResult>.Start;
begin
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TOfTypeIterator<T, TResult>'}

constructor TOfTypeIterator<T, TResult>.Create(const source: IEnumerable<T>);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);

  fSource := source;
end;

procedure TOfTypeIterator<T, TResult>.Dispose;
begin
  fEnumerator := nil;
end;

function TOfTypeIterator<T, TResult>.Clone: TIterator<TResult>;
begin
  Result := TOfTypeIterator<T, TResult>.Create(fSource);
end;

function TOfTypeIterator<T, TResult>.TryMoveNext(var current: TResult): Boolean;
var
  item: T;
  value: TValue;
begin
  while fEnumerator.MoveNext do
  begin
    item := fEnumerator.Current;
    value := TValue.From(item, TypeInfo(T));
    if value.TryAsType(TypeInfo(TResult), current) then
      Exit(True);
  end;
  Result := False;
end;

procedure TOfTypeIterator<T, TResult>.Start;
begin
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TRepeatIterator<T>'}

constructor TRepeatIterator<T>.Create(const element: T; count: Integer); //FI:W525
begin
  fElement := element;
  fCount := count;
end;

function TRepeatIterator<T>.Clone: TIterator<T>;
begin
  Result := TRepeatIterator<T>.Create(fElement, fCount);
end;

function TRepeatIterator<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TRepeatIterator<T>.GetNonEnumeratedCount: Integer;
begin
  Result := fCount;
end;

function TRepeatIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  Result := fIndex < fCount;
  begin
    Inc(fIndex);
    current := fElement;
  end;
end;

{$ENDREGION}


{$REGION 'TChunkIterator<T>'}

constructor TChunkIterator<T>.Create(const source: IEnumerable<T>; size: Integer);
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if size <= 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.size);

  fSource := source;
  fSize := size;
  fCapacity := 4;
  if fCapacity > fSize then
    fCapacity := fSize;
end;


function TChunkIterator<T>.Clone: TIterator<TArray<T>>;
begin
  Result := TChunkIterator<T>.Create(fSource, fSize);
end;

procedure TChunkIterator<T>.Dispose;
begin
  fEnumerator := nil;
end;

procedure TChunkIterator<T>.Start;
begin
  fEnumerator := fSource.GetEnumerator;
end;

function TChunkIterator<T>.TryMoveNext(var current: TArray<T>): Boolean;
var
  i: Integer;
begin
  if Assigned(fEnumerator) and fEnumerator.MoveNext then
  begin
    SetLength(current, fCapacity);

    i := 0;
    repeat
      current[i] := fEnumerator.Current;
      Inc(i);
      if i >= fSize then Break
      else if fEnumerator.MoveNext then
      begin
        if i >= fCapacity then
        begin
          fCapacity := GrowCapacity(fCapacity);
          if fCapacity > fSize then
            fCapacity := fSize;
          SetLength(current, fCapacity);
        end;
      end
      else
      begin
        SetLength(current, i);
        Break;
      end;
    until False;
    Result := True;
  end
  else
    Result := False;
end;

{$ENDREGION}


{$REGION 'TAnonymousIterator<T>'}

constructor TAnonymousIterator<T>.Create(const count: Func<Integer>; //FI:W525
  const items: Func<Integer, T>);
begin
  if not Assigned(count) then RaiseHelper.ArgumentNil(ExceptionArgument.count);
  if not Assigned(items) then RaiseHelper.ArgumentNil(ExceptionArgument.items);

  fCount := count;
  fItems := items;
end;

function TAnonymousIterator<T>.Clone: TIterator<T>;
begin
  Result := TAnonymousIterator<T>.Create(fCount, fItems);
end;

function TAnonymousIterator<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TAnonymousIterator<T>.GetNonEnumeratedCount: Integer;
begin
  Result := fCount;
end;

function TAnonymousIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  Result := fIndex < fCount;
  if Result then
  begin
    current := fItems(fIndex);
    Inc(fIndex);
  end;
end;

{$ENDREGION}


{$REGION 'TStringsAdapter'}

type
  TStringsAccess = class(TStrings);
  TStringListAccess = class(TStringList);
  TStringListAccess2 = class(TStrings)
  private
    FList: PStringItem;
  end;

function HasStringListGet(const strings: TStrings): Boolean;
var
  get: function (Index: Integer): string of object;
begin
  get := TStringsAccess(strings).Get;
  Result := TMethod(get).Code = @TStringListAccess.Get;
end;

constructor TStringsAdapter.Create(const strings: TStrings; ownsObject: Boolean);
begin
  if not Assigned(strings) then RaiseHelper.ArgumentNil(ExceptionArgument.source);

  fStrings := strings;
  fIsStringList := HasStringListGet(strings);
  fOwnsObject := ownsObject;
end;

function TStringsAdapter.Add(const item: string): Integer;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

  Result := fStrings.Add(item);

  DoNotify(item, caAdded);
end;

function TStringsAdapter.AsReadOnly: IReadOnlyList<string>;
begin
  Result := Self;
end;

procedure TStringsAdapter.BeforeDestruction;
begin
  if fOwnsObject then
    fStrings.Free;
  inherited BeforeDestruction;
end;

procedure TStringsAdapter.Clear;
begin
  fStrings.Clear;
end;

procedure TStringsAdapter.Delete(index: Integer);
var
  oldItem: string;
begin
  CheckIndex(index, fStrings.Count);

  oldItem := fStrings[index];

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fStrings.Delete(index);

  DoNotify(oldItem, caRemoved);
end;

procedure TStringsAdapter.DeleteRange(index, count: Integer);
begin
  DeleteRangeInternal(index, count, False);
end;

procedure TStringsAdapter.DeleteRangeInternal(index, count: Integer; doClear: Boolean);
var
  oldItems: TArray<string>;
  i: Integer;
begin
  CheckRange(index, count, fStrings.Count);

  if count = 0 then
    Exit;

  SetLength(oldItems, count);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

  for i := count downto 1 do
  begin
    oldItems[count - i] := fStrings[index];
    fStrings.Delete(index);
  end;

  if doClear then
    Reset;

  if Assigned(Notify) then
    for i := 0 to DynArrayHigh(oldItems) do
      Notify(Self, oldItems[i], caRemoved);
end;

procedure TStringsAdapter.Exchange(index1, index2: Integer);
begin
  if Cardinal(index1) >= Cardinal(fStrings.Count) then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.index1);
  if Cardinal(index2) >= Cardinal(fStrings.Count) then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.index2);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fStrings.Exchange(index1, index2);

  if Assigned(Notify) then
  begin
    Notify(Self, fStrings[index2], caMoved);
    Notify(Self, fStrings[index1], caMoved);
  end;
end;

function TStringsAdapter.Extract(const item: string): string;
var
  index: Integer;
begin
  index := IndexOf(item, 0, fStrings.Count);
  if index >= 0 then
    Result := ExtractAt(index)
  else
    Result := ''
end;

function TStringsAdapter.ExtractAt(index: Integer): string;
begin
  CheckIndex(index, fStrings.Count);

  Result := fStrings[index];

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fStrings.Delete(index);

  DoNotify(Result, caExtracted);
end;

function TStringsAdapter.ExtractRange(index, count: Integer): TArray<string>;
var
  i: Integer;
begin
  CheckRange(index, count, fStrings.Count);

  SetLength(Result, count);
  if count > 0 then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    i := 0;

    repeat
      Result[i] := fStrings[index];
      fStrings.Delete(index);
      DoNotify(Result[i], caExtracted);
      Inc(i);
      Dec(count);
    until count = 0;
  end;
end;

function TStringsAdapter.GetCapacity: Integer;
begin
  Result := fStrings.Capacity;
end;

function TStringsAdapter.GetCount: Integer;
begin
  Result := fStrings.Count;
end;

function TStringsAdapter.GetEnumerator: IEnumerator<string>; //FI:W521
var
  getCurrent: Pointer;
begin
  _AddRef;
  if fIsStringList then
    getCurrent := @TEnumerator.GetCurrentStringList
  else
    getCurrent := @TEnumerator.GetCurrent;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), getCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fCount := fStrings.Count;
    fVersion := Self.fVersion;
  end;
end;

function TStringsAdapter.GetItem(index: Integer): string;
begin
  CheckIndex(index, fStrings.Count);

  if fIsStringList then
    {$POINTERMATH ON}
    Result := TStringListAccess2(fStrings).FList[index].FString
    {$POINTERMATH OFF}
  else
    Result := fStrings[index];
end;

function TStringsAdapter.GetNonEnumeratedCount: Integer;
begin
  Result := fStrings.Count;
end;

function TStringsAdapter.GetOwnsObjects: Boolean;
begin
  Result := False;
end;

function TStringsAdapter.GetRange(index, count: Integer): IList<string>;
var
  i: Integer;
begin
  CheckRange(index, count, fStrings.Count);

  Result := TCollections.CreateList<string>;
  Result.Count := count;
  for i := 0 to count - 1 do
  begin
    Result[i] := fStrings[index];
    Inc(index);
  end;
end;

function TStringsAdapter.IndexOf(const item: string): Integer;
begin
  Result := fStrings.IndexOf(item);
end;

function TStringsAdapter.IndexOf(const item: string; index: Integer): Integer;
begin
  Result := IndexOf(item, index, fStrings.Count - index);
end;

function TStringsAdapter.IndexOf(const item: string; index, count: Integer): Integer;
var
  i: Integer;
begin
  CheckRange(index, count, fStrings.Count);

  for i := index to index + count - 1 do
    if TStringsAccess(fStrings).CompareStrings(fStrings[i], item) = 0 then
      Exit(i);
  Result := -1;
end;

procedure TStringsAdapter.Insert(index: Integer; const item: string);
begin
  CheckIndex(index, fStrings.Count + 1);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fStrings.Insert(index, item);

  DoNotify(item, caAdded);
end;

procedure TStringsAdapter.InsertRange(index: Integer; const values: array of string);
var
  i: Integer;
begin
  if Cardinal(index) > Cardinal(fStrings.Count) then RaiseHelper.ArgumentOutOfRange_Index;

  if High(values) >= 0 then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

    for i := 0 to High(values) do
    begin
      fStrings.Insert(index, values[i]);
      DoNotify(values[i], caAdded);
      Inc(index);
    end;
  end;
end;

procedure TStringsAdapter.InsertRange(index: Integer;
  const values: IEnumerable<string>);
var
  enumerator: IEnumerator<string>;
  item: string;
begin
  if Cardinal(index) > Cardinal(fStrings.Count) then RaiseHelper.ArgumentOutOfRange_Index;
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  enumerator := values.GetEnumerator;
  if enumerator.MoveNext then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

    repeat
      item := enumerator.Current;
      fStrings.Insert(index, item);
      DoNotify(item, caAdded);
      Inc(index);
    until not enumerator.MoveNext;
  end;
end;

function TStringsAdapter.LastIndexOf(const item: string): Integer;
var
  listCount: Integer;
begin
  listCount := fStrings.Count;
  if listCount > 0 then
    Result := LastIndexOf(item, listCount - 1, listCount)
  else
    Result := -1;
end;

function TStringsAdapter.LastIndexOf(const item: string; index: Integer): Integer;
begin
  CheckIndex(index, fStrings.Count);

  Result := LastIndexOf(item, index, index + 1);
end;

function TStringsAdapter.LastIndexOf(const item: string; index, count: Integer): Integer;
var
  i: Integer;
begin
  CheckIndex(index, fStrings.Count);
  if Cardinal(count) > Cardinal(index + 1) then RaiseHelper.ArgumentOutOfRange_Count;

  for i := index downto index - count + 1 do
    if TStringsAccess(fStrings).CompareStrings(fStrings[i], item) = 0 then
      Exit(i);
  Result := -1;
end;

procedure TStringsAdapter.Move(sourceIndex, targetIndex: Integer);
begin
  if Cardinal(sourceIndex) >= Cardinal(fStrings.Count) then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.sourceIndex);
  if Cardinal(targetIndex) >= Cardinal(fStrings.Count) then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.targetIndex);

  if sourceIndex = targetIndex then
    Exit;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fStrings.Move(sourceIndex, targetIndex);

  DoNotify(fStrings[targetIndex], caMoved);
end;

function TStringsAdapter.Remove(const item: string): Boolean;
var
  index: Integer;
begin
  index := fStrings.IndexOf(item);
  if index >= 0 then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

    fStrings.Delete(index);
    DoNotify(item, caRemoved);
    Exit(True);
  end;
  Result := False;
end;

procedure TStringsAdapter.Reverse;
begin
  RaiseHelper.NotSupported;
end;

procedure TStringsAdapter.Reverse(index, count: Integer); //FI:O804
begin
  RaiseHelper.NotSupported;
end;

procedure TStringsAdapter.SetCapacity(value: Integer);
begin
  fStrings.Capacity := value;
end;

procedure TStringsAdapter.SetCount(value: Integer);
begin //FI:W519
end;

procedure TStringsAdapter.SetItem(index: Integer; const value: string);
begin
  CheckIndex(index, fStrings.Count);

  fStrings[index] := value;
end;

procedure TStringsAdapter.SetOwnsObjects(value: Boolean);
begin //FI:W519
end;

procedure TStringsAdapter.Sort;
begin
  if (fStrings is TStringList) then
    TStringList(fStrings).Sort;
end;

procedure TStringsAdapter.Sort(const comparer: IComparer<string>); //FI:O804
begin
  RaiseHelper.NotSupported;
end;

procedure TStringsAdapter.Sort(const comparer: IComparer<string>; index, count: Integer); //FI:O804
begin
  RaiseHelper.NotSupported;
end;

procedure TStringsAdapter.Sort(const comparer: TComparison<string>); //FI:O804
begin
  RaiseHelper.NotSupported;
end;

procedure TStringsAdapter.Sort(const comparer: TComparison<string>; index, count: Integer); //FI:O804
begin
  RaiseHelper.NotSupported;
end;

function TStringsAdapter.ToArray: TArray<string>;
begin
  Result := fStrings.ToStringArray;
end;

procedure TStringsAdapter.TrimExcess;
begin
  fStrings.Capacity := fStrings.Count;
end;

function TStringsAdapter.TryGet(var value: string; index: Integer): Boolean;
begin
  value := fStrings[index];
  Result := True;
end;

function TStringsAdapter.TryGetElementAt(var value: string; index: Integer): Boolean;
begin
  if Cardinal(index) < Cardinal(fStrings.Count) then
  begin
    if fIsStringList then
    begin
      {$POINTERMATH ON}
      value := TStringListAccess2(fStrings).FList[index].FString;
      {$POINTERMATH OFF}
      Exit(True);
    end
    else
      Exit(TryGet(value, index));
  end;
  value := '';
  Result := False;
end;

function TStringsAdapter.TryGetFirst(var value: string): Boolean;
begin
  if fStrings.Count > 0 then
  begin
    value := fStrings[0];
    Exit(True);
  end;
  value := '';
  Result := False;
end;

function TStringsAdapter.TryGetLast(var value: string): Boolean;
var
  index: Integer;
begin
  index := fStrings.Count - 1;
  if index >= 0 then
  begin
    value := fStrings[index];
    Exit(True);
  end;
  value := '';
  Result := False;
end;

function TStringsAdapter.TryGetSingle(var value: string): Boolean;
begin
  if fStrings.Count = 1 then
  begin
    value := fStrings[0];
    Exit(True);
  end;
  value := '';
  Result := False;
end;

{$ENDREGION}


{$REGION 'TStringsAdapter.TEnumerator'}

function TStringsAdapter.TEnumerator.GetCurrent: string;
begin
  Result := fSource.fStrings[fIndex - 1];
end;

function TStringsAdapter.TEnumerator.GetCurrentStringList: string;
var
  items: PStringItem;
  index: Integer;
begin
  index := fIndex;
  items := TStringListAccess2(fSource.fStrings).FList;
  {$POINTERMATH ON}
  Result := items[index - 1].FString;
  {$POINTERMATH OFF}
end;

function TStringsAdapter.TEnumerator.MoveNext: Boolean;
var
  index: Integer;
begin
  if fVersion = fSource.fVersion then
  begin
    index := fIndex;
    fIndex := index + 1;
    Result := index < fCount;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


end.
