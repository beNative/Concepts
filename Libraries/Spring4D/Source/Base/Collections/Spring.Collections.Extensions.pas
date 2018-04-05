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

unit Spring.Collections.Extensions;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  SysUtils,
  Types,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Lists;

type
  TEmptyEnumerable<T> = class(TEnumerableBase<T>, IReadOnlyList<T>)
  private
    class var fInstance: IReadOnlyList<T>;
    class function GetInstance: IReadOnlyList<T>; static;
    constructor Create; reintroduce;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(index: Integer): T;
  {$ENDREGION}
  public
    class destructor Destroy;

    function GetRange(index, count: Integer): IList<T>;

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    function ToArray: TArray<T>; override;

    class property Instance: IReadOnlyList<T> read GetInstance;
  end;

  TArrayIterator<T> = class(TIterator<T>, IReadOnlyList<T>, IArrayAccess<T>)
  private
    fValues: TArray<T>;
    fIndex: Integer;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(index: Integer): T;
    function GetItems: TArray<T>;
  {$ENDREGION}
    function Clone: TIterator<T>; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const values: array of T); overload;
    constructor Create(const values: TArray<T>); overload;

    function GetRange(index, count: Integer): IList<T>;

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    function ToArray: TArray<T>; override;
  end;

  /// <summary>
  ///   The adapter implementation for <see cref="Spring.Collections|IEnumerator&lt;T&gt;" />
  ///    .
  /// </summary>
  TEnumeratorAdapter<T> = class(TEnumeratorBase<T>)
  private
    type
      TGenericEnumerable = Generics.Collections.TEnumerable<T>;
      TGenericEnumerator = Generics.Collections.TEnumerator<T>;
  private
    fSource: TGenericEnumerable;
    fEnumerator: TGenericEnumerator;
    procedure Start;
  protected
    function GetCurrent: T; override;
  public
    constructor Create(const source: TGenericEnumerable);
    destructor Destroy; override;
    function MoveNext: Boolean; override;
    property Current: T read GetCurrent;
  end;

  /// <summary>
  ///   The adapter implementation for <see cref="Spring.Collections|IEnumerable&lt;T&gt;" />
  ///    .
  /// </summary>
  TEnumerableAdapter<T> = class(TEnumerableBase<T>)
  private
    type
      TGenericEnumerable = Generics.Collections.TEnumerable<T>;
  private
    fSource: TGenericEnumerable;
  public
    constructor Create(const source: TGenericEnumerable);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TWhereIterator<T> = class(TSourceIterator<T>)
  private
    fPredicate: TPredicate<T>;
    fEnumerator: IEnumerator<T>;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>;
      const predicate: TPredicate<T>);

{$IFNDEF DELPHI2010}
    function Where(const predicate: TPredicate<T>): IEnumerable<T>; override;
{$ENDIF}
  end;

  TWhereIndexIterator<T> = class(TSourceIterator<T>)
  private
    fPredicate: TFunc<T, Integer, Boolean>;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>;
      const predicate: TFunc<T, Integer, Boolean>);
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
  end;

  TSkipWhileIterator<T> = class(TSourceIterator<T>)
  private
    fPredicate: TPredicate<T>;
    fEnumerator: IEnumerator<T>;
    fYielding: Boolean;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: TPredicate<T>);
  end;

  TSkipWhileIndexIterator<T> = class(TSourceIterator<T>)
  private
    fPredicate: TFunc<T, Integer, Boolean>;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
    fYielding: Boolean;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: TFunc<T, Integer, Boolean>);
  end;

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
  end;

  TTakeWhileIterator<T> = class(TSourceIterator<T>)
  private
    fPredicate: TPredicate<T>;
    fEnumerator: IEnumerator<T>;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: TPredicate<T>);
  end;

  TTakeWhileIndexIterator<T> = class(TSourceIterator<T>)
  private
    fPredicate: TFunc<T, Integer, Boolean>;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
    fStopped: Boolean;
  protected
    function Clone: TIterator<T>; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: TFunc<T, Integer, Boolean>);
  end;

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
  end;

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
  end;

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
    fKeySelector: TFunc<T, TKey>;
    fComparer: IEqualityComparer<TKey>;
    fSet: ISet<TKey>;
    fEnumerator: IEnumerator<T>;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>; const keySelector: TFunc<T, TKey>;
      const comparer: IEqualityComparer<TKey>);
  end;

{$IFDEF DELPHI2010}
  TRangeIterator = Spring.Collections.Base.TRangeIterator;
{$ELSE}
  TRangeIterator = class(TIterator<Integer>)
  private
    fStart: Integer;
    fCount: Integer;
    fIndex: Integer;
  protected
    function Clone: TIterator<Integer>; override;
    function GetCount: Integer; override;
    function TryMoveNext(var current: Integer): Boolean; override;
  public
    constructor Create(start, count: Integer);

    function ToArray: TArray<Integer>; override;
  end;
{$ENDIF}

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

  TSelectIterator<TSource, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fSelector: TFunc<TSource, TResult>;
    fEnumerator: IEnumerator<TSource>;
  protected
    function Clone: TIterator<TResult>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TResult): Boolean; override;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const selector: TFunc<TSource, TResult>);
  end;

  TSelectIndexIterator<TSource, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fSelector: TFunc<TSource, Integer, TResult>;
    fEnumerator: IEnumerator<TSource>;
    fIndex: Integer;
  protected
    function Clone: TIterator<TResult>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: TResult): Boolean; override;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const selector: TFunc<TSource, Integer, TResult>);
  end;

  TGroupedEnumerable<TSource, TKey, TElement> = class(TEnumerableBase<IGrouping<TKey, TElement>>)
  private
    type
      TEnumerator = class(TEnumeratorBase<IGrouping<TKey, TElement>>)
      private
        fSource: IEnumerable<TSource>;
        fKeySelector: TFunc<TSource, TKey>;
        fElementSelector: TFunc<TSource, TElement>;
        fComparer: IEqualityComparer<TKey>;
        fLookup: ILookup<TKey, TElement>;
        fEnumerator: IEnumerator<IGrouping<TKey, TElement>>;
        procedure Start;
      protected
        function GetCurrent: IGrouping<TKey, TElement>; override;
      public
        constructor Create(const source: IEnumerable<TSource>;
          const keySelector: TFunc<TSource, TKey>;
          const elementSelector: TFunc<TSource, TElement>;
          const comparer: IEqualityComparer<TKey>);
        function MoveNext: Boolean; override;
      end;
  private
    fSource: IEnumerable<TSource>;
    fKeySelector: TFunc<TSource, TKey>;
    fElementSelector: TFunc<TSource, TElement>;
    fComparer: IEqualityComparer<TKey>;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>); overload;
    constructor Create(const source: IEnumerable<TSource>;
      const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>;
      const comparer: IEqualityComparer<TKey>); overload;
    function GetEnumerator: IEnumerator<IGrouping<TKey, TElement>>; override;
  end;

  TGroupedEnumerable<TSource, TKey, TElement, TResult> = class(TEnumerableBase<TResult>)
  private
    type
      TEnumerator = class(TEnumeratorBase<TResult>)
      private
        fSource: IEnumerator<IGrouping<TKey, TElement>>;
        fResultSelector: TFunc<TKey, Ienumerable<TElement>, TResult>;
      protected
        function GetCurrent: TResult; override;
      public
        constructor Create(const source: IEnumerator<IGrouping<TKey, TElement>>;
          const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>);
        function MoveNext: Boolean; override;
      end;
  private
    fSource: IEnumerable<TSource>;
    fKeySelector: TFunc<TSource, TKey>;
    fElementSelector: TFunc<TSource, TElement>;
    fComparer: IEqualityComparer<TKey>;
    fResultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>;
      const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>); overload;
    constructor Create(const source: IEnumerable<TSource>;
      const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>;
      const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>;
      const comparer: IEqualityComparer<TKey>); overload;
    function GetEnumerator: IEnumerator<TResult>; override;
  end;

  TLookup<TKey, TElement> = class(TEnumerableBase<IGrouping<TKey, TElement>>, ILookup<TKey, TElement>)
  private
    type
      TGrouping = class(TEnumerableBase<TElement>, IGrouping<TKey, TElement>)
      private
        fKey: TKey;
        fElements: IList<TElement>;
        function GetKey: TKey;
        procedure Add(const item: TElement);
      protected
        function GetCount: Integer; override;
      public
        constructor Create(const key: TKey);
        function GetEnumerator: IEnumerator<TElement>; override;
        property Key: TKey read GetKey;
      end;

      TGroupings = class(TObjectList<TGrouping>)
      protected
        procedure Changed(const item: TGrouping;
          action: TCollectionChangedAction); override;
      public
        constructor Create; override;
      end;

      TEnumerator = class(TEnumeratorBase<IGrouping<TKey, TElement>>)
      private
        fSource: IList<TGrouping>;
        fIndex: Integer;
        fLookup: IInterface;
      protected
        function GetCurrent: IGrouping<TKey, TElement>; override;
      public
        constructor Create(const source: TLookup<TKey, TElement>);
        function MoveNext: Boolean; override;
      end;
  private
    fComparer: IEqualityComparer<TKey>;
    fGroupings: IList<TGrouping>;
    fGroupingKeys: TDictionary<TKey, TGrouping>;
    function GetGrouping(const key: TKey; create: Boolean): TGrouping;
    function GetItem(const key: TKey): IEnumerable<TElement>;
  protected
    function GetCount: Integer; override;
  public
    constructor Create; reintroduce; overload;
    constructor Create(const comparer: IEqualityComparer<TKey>); overload;
    class function Create<TSource>(const source: IEnumerable<TSource>;
      const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>): TLookup<TKey, TElement>; overload; static;
    class function Create<TSource>(const source: IEnumerable<TSource>;
      const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>;
      const comparer: IEqualityComparer<TKey>): TLookup<TKey, TElement>; overload; static;
    class function CreateForJoin(const source: IEnumerable<TElement>;
      const keySelector: TFunc<TElement, TKey>;
      const comparer: IEqualityComparer<TKey>): TLookup<TKey, TElement>; static;
    destructor Destroy; override;

    function Contains(const key: TKey): Boolean;
    function GetEnumerator: IEnumerator<IGrouping<TKey, TElement>>; override;
    property Item[const key: TKey]: IEnumerable<TElement> read GetItem; default;
  end;

  TJoinIterator<TOuter, TInner, TKey, TResult> = class(TIterator<TResult>)
  private
    fOuter: IEnumerable<TOuter>;
    fInner: IEnumerable<TInner>;
    fOuterKeySelector: TFunc<TOuter, TKey>;
    fInnerKeySelector: TFunc<TInner, TKey>;
    fResultSelector: TFunc<TOuter, TInner, TResult>;
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
      const outerKeySelector: TFunc<TOuter, TKey>;
      const innerKeySelector: TFunc<TInner, TKey>;
      const resultSelector: TFunc<TOuter, TInner, TResult>); overload;
    constructor Create(const outer: IEnumerable<TOuter>;
      const inner: IEnumerable<TInner>;
      const outerKeySelector: TFunc<TOuter, TKey>;
      const innerKeySelector: TFunc<TInner, TKey>;
      const resultSelector: TFunc<TOuter, TInner, TResult>;
      const comparer: IEqualityComparer<TKey>); overload;
    destructor Destroy; override;
  end;

  TGroupJoinIterator<TOuter, TInner, TKey, TResult> = class(TIterator<TResult>)
  private
    fOuter: IEnumerable<TOuter>;
    fInner: IEnumerable<TInner>;
    fOuterKeySelector: TFunc<TOuter, TKey>;
    fInnerKeySelector: TFunc<TInner, TKey>;
    fResultSelector: TFunc<TOuter, IEnumerable<TInner>, TResult>;
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
      const outerKeySelector: TFunc<TOuter, TKey>;
      const innerKeySelector: TFunc<TInner, TKey>;
      const resultSelector: TFunc<TOuter, IEnumerable<TInner>, TResult>); overload;
    constructor Create(const outer: IEnumerable<TOuter>;
      const inner: IEnumerable<TInner>;
      const outerKeySelector: TFunc<TOuter, TKey>;
      const innerKeySelector: TFunc<TInner, TKey>;
      const resultSelector: TFunc<TOuter, IEnumerable<TInner>, TResult>;
      const comparer: IEqualityComparer<TKey>); overload;
    destructor Destroy; override;
  end;

  TSelectManyIterator<TSource, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fSelector: TFunc<TSource, IEnumerable<TResult>>;
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
      const selector: TFunc<TSource, IEnumerable<TResult>>);
  end;

  TSelectManyIndexIterator<TSource, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fSelector: TFunc<TSource, Integer, IEnumerable<TResult>>;
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
      const selector: TFunc<TSource, Integer, IEnumerable<TResult>>);
  end;

  TSelectManyIterator<TSource, TCollection, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fCollectionSelector: TFunc<TSource, IEnumerable<TCollection>>;
    fResultSelector: TFunc<TSource, TCollection, TResult>;
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
      const collectionSelector: TFunc<TSource, IEnumerable<TCollection>>;
      const resultSelector: TFunc<TSource, TCollection, TResult>);
  end;

  TSelectManyIndexIterator<TSource, TCollection, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fCollectionSelector: TFunc<TSource, Integer, IEnumerable<TCollection>>;
    fResultSelector: TFunc<TSource, TCollection, TResult>;
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
      const collectionSelector: TFunc<TSource, Integer, IEnumerable<TCollection>>;
      const resultSelector: TFunc<TSource, TCollection, TResult>);
  end;

  IEnumerableSorter<T> = interface
    procedure ComputeKeys(const elements: TArray<T>; count: Integer);
    function CompareKeys(index1, index2: Integer): Integer;
    function Sort(var elements: TArray<T>; count: Integer): TIntegerDynArray;
  end;

  TEnumerableSorter<T> = class(TInterfacedObject, IEnumerableSorter<T>)
  protected
    procedure ComputeKeys(const elements: TArray<T>; count: Integer); virtual; abstract;
    function CompareKeys(index1, index2: Integer): Integer; virtual; abstract;
    function Sort(var elements: TArray<T>; count: Integer): TIntegerDynArray;
  end;

  TEnumerableSorter<TElement, TKey> = class(TEnumerableSorter<TElement>)
  private
    fKeySelector: TFunc<TElement, TKey>;
    fComparer: IComparer<TKey>;
    fDescending: Boolean;
    fNext: IEnumerableSorter<TElement>;
    fKeys: TArray<TKey>;
  protected
    procedure ComputeKeys(const elements: TArray<TElement>; count: Integer); override;
    function CompareKeys(index1, index2: Integer): Integer; override;
  public
    constructor Create(const keySelector: TFunc<TElement, TKey>;
      const comparer: IComparer<TKey>; descending: Boolean;
      const next: IEnumerableSorter<TElement>);
  end;

  TOrderedEnumerable<T> = class(TEnumerableBase<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fBuffer: TArray<T>;
        fMap: TIntegerDynArray;
        fIndex: Integer;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const source: IEnumerable<T>;
          const sorter: IEnumerableSorter<T>);
        function MoveNext: Boolean; override;
      end;
  private
    fSource: IEnumerable<T>;
  protected
    function GetCount: Integer; override;
    function GetElementType: PTypeInfo; override;
    function GetEnumerableSorter(
      const next: IEnumerableSorter<T>): IEnumerableSorter<T>; virtual; abstract;
  public
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TOrderedEnumerable<TElement, TKey> = class(TOrderedEnumerable<TElement>)
  private
    fParent: TOrderedEnumerable<TElement>;
    fKeySelector: TFunc<TElement, TKey>;
    fComparer: IComparer<TKey>;
    fDescending: Boolean;
  protected
    function GetEnumerableSorter(
      const next: IEnumerableSorter<TElement>): IEnumerableSorter<TElement>; override;
  public
    constructor Create(const source: IEnumerable<TElement>;
      const keySelector: TFunc<TElement, TKey>); overload;
    constructor Create(const source: IEnumerable<TElement>;
      const keySelector: TFunc<TElement, TKey>; const comparer: IComparer<TKey>;
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
  end;

  TZipIterator<TFirst, TSecond, TResult> = class(TIterator<TResult>)
  private
    fFirst: IEnumerable<TFirst>;
    fSecond: IEnumerable<TSecond>;
    fResultSelector: TFunc<TFirst, TSecond, TResult>;
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
      const resultSelector: TFunc<TFirst, TSecond, TResult>);
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
    fKeySelector: TFunc<T, TKey>;
    fCompare: TFunc<TKey, TKey, Integer>;
    fResult: IList<T>;
    fEnumerator: IEnumerator<T>;
  protected
    function Clone: TIterator<T>; override;
    procedure Dispose; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const source: IEnumerable<T>;
      const keySelector: TFunc<T, TKey>;
      const compare: TFunc<TKey, TKey, Integer>);
  end;

  TCastIterator<T, TResult> = class(TIterator<TResult>)
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

  TOfTypeIterator<T, TResult> = class(TIterator<TResult>)
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

  TRepeatIterator<T> = class(TIterator<T>)
  private
    fElement: T;
    fCount: Integer;
    fIndex: Integer;
  protected
    function Clone: TIterator<T>; override;
    function GetCount: Integer; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const element: T; count: Integer);
  end;

  TAnonymousIterator<T> = class(TIterator<T>)
  private
    fCount: TFunc<Integer>;
    fItems: TFunc<Integer, T>;
    fIndex: Integer;
  protected
    function Clone: TIterator<T>; override;
    function GetCount: Integer; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const count: TFunc<Integer>; const items: TFunc<Integer, T>);
  end;

implementation

uses
{$IFDEF DELPHI2010}
  Spring.Collections.Sets,
{$ENDIF}
  Spring.ResourceStrings;


{$REGION 'TEmptyEnumerable<T>'}

constructor TEmptyEnumerable<T>.Create;
begin
  inherited Create;
end;

class destructor TEmptyEnumerable<T>.Destroy;
begin
  fInstance := nil;
end;

function TEmptyEnumerable<T>.GetCount: Integer;
begin
  Result := 0;
end;

class function TEmptyEnumerable<T>.GetInstance: IReadOnlyList<T>;
begin
  if fInstance = nil then
    fInstance := TEmptyEnumerable<T>.Create;
  Result := fInstance;
end;

function TEmptyEnumerable<T>.GetItem(index: Integer): T;
begin
  Guard.RaiseArgumentOutOfRangeException('index');
end;

function TEmptyEnumerable<T>.GetRange(index, count: Integer): IList<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(index = 0, 'index');
  Guard.CheckRange(count = 0, 'count');
{$ENDIF}

{$IFDEF DELPHIXE_UP}
  Result := TCollections.CreateList<T>;
{$ELSE}
  Result := TList<T>.Create;
{$ENDIF}
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

function TEmptyEnumerable<T>.ToArray: TArray<T>;
begin
  Result := nil;
end;

{$ENDREGION}


{$REGION 'TArrayIterator<T>'}

constructor TArrayIterator<T>.Create(const values: array of T);
var
  i: Integer;
begin
  inherited Create;
  fValues := TArray.Copy<T>(values);
end;

constructor TArrayIterator<T>.Create(const values: TArray<T>);
begin
  inherited Create;
  fValues := values;
end;

function TArrayIterator<T>.GetCount: Integer;
begin
  Result := Length(fValues);
end;

function TArrayIterator<T>.GetItem(index: Integer): T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  Result := fValues[index];
end;

function TArrayIterator<T>.GetItems: TArray<T>;
begin
  Result := fValues;
end;

function TArrayIterator<T>.GetRange(index, count: Integer): IList<T>;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Length(fValues)), 'index');
  Guard.CheckRange((count >= 0) and (count <= Length(fValues) - index), 'count');
{$ENDIF}

{$IFNDEF DELPHI2010}
  Result := TCollections.CreateList<T>;
{$ELSE}
  Result := TList<T>.Create;
{$ENDIF}
  Result.Count := count;
  for i := 0 to count - 1 do
  begin
    Result[i] := fValues[index];
    Inc(index);
  end;
end;

function TArrayIterator<T>.IndexOf(const item: T): Integer;
begin
  Result := IndexOf(item, 0, Count);
end;

function TArrayIterator<T>.IndexOf(const item: T; index: Integer): Integer;
begin
  Result := IndexOf(item, index, Count - index);
end;

function TArrayIterator<T>.IndexOf(const item: T; index,
  count: Integer): Integer;
{$IFDEF DELPHI2010}
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Length(fValues)), 'index');
  Guard.CheckRange((count >= 0) and (count <= Length(fValues) - index), 'count');
{$ENDIF}

  for i := index to index + count - 1 do
    if Equals(fValues[i], item) then
      Exit(i);
  Result := -1;
{$ELSE}
begin
  Result := TArray.IndexOf<T>(fValues, item, index, count, Self);
{$ENDIF}
end;

function TArrayIterator<T>.Clone: TIterator<T>;
begin
  Result := TArrayIterator<T>.Create(fValues);
end;

function TArrayIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  Result := fIndex < Length(fValues);
  if Result then
  begin
    current := fValues[fIndex];
    Inc(fIndex);
  end;
end;

function TArrayIterator<T>.ToArray: TArray<T>;
begin
  Result := fValues;
  SetLength(Result, Length(Result));
end;

{$ENDREGION}


{$REGION 'TEnumeratorAdapter<T>'}

constructor TEnumeratorAdapter<T>.Create(const source: TGenericEnumerable);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
  fSource := source;
end;

destructor TEnumeratorAdapter<T>.Destroy;
begin
  fEnumerator.Free;
  inherited Destroy;
end;

function TEnumeratorAdapter<T>.GetCurrent: T;
begin
  Result := fEnumerator.Current;
end;

function TEnumeratorAdapter<T>.MoveNext: Boolean;
begin
  if not Assigned(fEnumerator) then
    Start;
  Result := fEnumerator.MoveNext;
end;

procedure TEnumeratorAdapter<T>.Start;
begin
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TEnumerableAdapter<T>'}

constructor TEnumerableAdapter<T>.Create(const source: TGenericEnumerable);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
  fSource := source;
end;

function TEnumerableAdapter<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(fSource);
end;

{$ENDREGION}


{$REGION 'TWhereIterator<T>'}

constructor TWhereIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TPredicate<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TWhereIterator<T>.Clone: TIterator<T>;
begin
  Result := TWhereIterator<T>.Create(fSource, fPredicate);
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
  fEnumerator := fSource.GetEnumerator;
end;

{$IFNDEF DELPHI2010}
function TWhereIterator<T>.Where(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
  Result := TWhereIterator<T>.Create(fSource,
    TEnumerable.CombinePredicates<T>(fPredicate, predicate));
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TWhereIndexIterator<T>'}

constructor TWhereIndexIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TFunc<T, Integer, Boolean>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TWhereIndexIterator<T>.Clone: TIterator<T>;
begin
  Result := TWhereIndexIterator<T>.Create(fSource, fPredicate);
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
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TSkipIterator<T>'}

constructor TSkipIterator<T>.Create(const source: IEnumerable<T>;
  count: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fCount := count;
end;

function TSkipIterator<T>.Clone: TIterator<T>;
begin
  Result := TSkipIterator<T>.Create(fSource, fCount);
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
  fEnumerator := fSource.GetEnumerator;
  fIndex := fCount;
end;

{$ENDREGION}


{$REGION 'TSkipWhileIterator<T>'}

constructor TSkipWhileIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TPredicate<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TSkipWhileIterator<T>.Clone: TIterator<T>;
begin
  Result := TSkipWhileIterator<T>.Create(fSource, fPredicate);
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
    if not fYielding and not fPredicate(current) then
      fYielding := True;
    if fYielding then
      Exit(True);
  end;
  Result := False;
end;

procedure TSkipWhileIterator<T>.Start;
begin
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TSkipWhileIndexIterator<T>'}

constructor TSkipWhileIndexIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TFunc<T, Integer, Boolean>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TSkipWhileIndexIterator<T>.Clone: TIterator<T>;
begin
  Result := TSkipWhileIndexIterator<T>.Create(fSource, fPredicate);
end;

procedure TSkipWhileIndexIterator<T>.Dispose;
begin
  fEnumerator := nil;
end;

function TSkipWhileIndexIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  while fEnumerator.MoveNext do
  begin
    current := fEnumerator.Current;
    Inc(fIndex);
    if not fYielding and not fPredicate(current, fIndex) then
      fYielding := True;
    if fYielding then
      Exit(True);
  end;
  Result := False;
end;

procedure TSkipWhileIndexIterator<T>.Start;
begin
  fEnumerator := fSource.GetEnumerator;
  fIndex := -1;
end;

{$ENDREGION}


{$REGION 'TTakeIterator<T>'}

constructor TTakeIterator<T>.Create(const source: IEnumerable<T>;
  count: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fCount := count;
end;

function TTakeIterator<T>.Clone: TIterator<T>;
begin
  Result := TTakeIterator<T>.Create(fSource, fCount);
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
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TTakeWhileIterator<T>'}

constructor TTakeWhileIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TPredicate<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

procedure TTakeWhileIterator<T>.Dispose;
begin
  fEnumerator := nil;
end;

function TTakeWhileIterator<T>.Clone: TIterator<T>;
begin
  Result := TTakeWhileIterator<T>.Create(fSource, fPredicate);
end;

function TTakeWhileIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  Result := fEnumerator.MoveNext;
  if Result then
  begin
    current := fEnumerator.Current;
    Result := fPredicate(current);
  end;
end;

procedure TTakeWhileIterator<T>.Start;
begin
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TTakeWhileIndexIterator<T>'}

constructor TTakeWhileIndexIterator<T>.Create(
  const source: IEnumerable<T>;
  const predicate: TFunc<T, Integer, Boolean>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TTakeWhileIndexIterator<T>.Clone: TIterator<T>;
begin
  Result := TTakeWhileIndexIterator<T>.Create(fSource, fPredicate);
end;

function TTakeWhileIndexIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  while not fStopped and fEnumerator.MoveNext do
  begin
    current := fEnumerator.Current;
    Inc(fIndex);
    if fPredicate(current, findex) then
      Exit(True)
    else
      fStopped := True;
  end;
  Result := False;
end;

procedure TTakeWhileIndexIterator<T>.Start;
begin
  fIndex := -1;
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TConcatIterator<T>'}

constructor TConcatIterator<T>.Create(const first, second: IEnumerable<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(first), 'first');
  Guard.CheckNotNull(Assigned(second), 'second');
{$ENDIF}

  inherited Create;
  fSource := first;
  fSecond := second;
end;

function TConcatIterator<T>.Clone: TIterator<T>;
begin
  Result := TConcatIterator<T>.Create(fSource, fSecond);
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
    fEnumerator := fSource.GetEnumerator
  else
    fEnumerator := fSecond.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TReversedIterator<T>'}

constructor TReversedIterator<T>.Create(const source: IEnumerable<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
end;

function TReversedIterator<T>.Clone: TIterator<T>;
begin
  Result := TReversedIterator<T>.Create(fSource);
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
  fBuffer := fSource.ToArray;
  fIndex := Length(fBuffer);
end;

{$ENDREGION}


{$REGION 'TDistinctIterator<T>'}

constructor TDistinctIterator<T>.Create(const source: IEnumerable<T>;
  const comparer: IEqualityComparer<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fComparer := comparer;
end;

function TDistinctIterator<T>.Clone: TIterator<T>;
begin
  Result := TDistinctIterator<T>.Create(fSource, fComparer);
end;

procedure TDistinctIterator<T>.Dispose;
begin
  fSet := nil;
  fEnumerator := nil;
end;

function TDistinctIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  while fEnumerator.MoveNext do
  begin
    current := fEnumerator.Current;
    if fSet.Add(current) then
      Exit(True);
  end;
  Result := False;
end;

procedure TDistinctIterator<T>.Start;
begin
{$IFNDEF DELPHI2010}
  fSet := TCollections.CreateSet<T>(fComparer);
{$ELSE}
  fSet := THashSet<T>.Create(fComparer);
{$ENDIF}
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TDistinctByIterator<T, TKey>'}

constructor TDistinctByIterator<T, TKey>.Create(const source: IEnumerable<T>;
  const keySelector: TFunc<T, TKey>; const comparer: IEqualityComparer<TKey>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fKeySelector := keySelector;
  fComparer := comparer;
end;

function TDistinctByIterator<T, TKey>.Clone: TIterator<T>;
begin
  Result := TDistinctByIterator<T, TKey>.Create(fSource, fKeySelector, fComparer);
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
{$IFNDEF DELPHI2010}
  fSet := TCollections.CreateSet<TKey>(fComparer);
{$ELSE}
  fSet := THashSet<TKey>.Create(fComparer);
{$ENDIF}
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TRangeIterator'}

{$IFNDEF DELPHI2010}
constructor TRangeIterator.Create(start, count: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(count >= 0, 'count');
  Guard.CheckRange(Int64(start) + Int64(count) - 1 <= Int64(MaxInt), 'count');
{$ENDIF}

  inherited Create;
  fStart := start;
  fCount := count;
end;

function TRangeIterator.Clone: TIterator<Integer>;
begin
  Result := TRangeIterator.Create(fStart, fCount);
end;

function TRangeIterator.GetCount: Integer;
begin
  Result := fCount;
end;

function TRangeIterator.TryMoveNext(var current: Integer): Boolean;
begin
  Result := fIndex < fCount;
  if Result then
  begin
    current := fStart + fIndex;
    Inc(fIndex);
  end;
end;

function TRangeIterator.ToArray: TArray<Integer>;
var
  i: Integer;
begin
  SetLength(Result, fCount);
  for i := 0 to fCount - 1 do
    Result[i] := fStart + i;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TExceptIterator<T>'}

constructor TExceptIterator<T>.Create(const first, second: IEnumerable<T>);
begin
  Create(first, second, nil);
end;

constructor TExceptIterator<T>.Create(const first, second: IEnumerable<T>;
  const comparer: IEqualityComparer<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(first), 'first');
  Guard.CheckNotNull(Assigned(second), 'second');
{$ENDIF}

  inherited Create;
  fSource := first;
  fSecond := second;
  fComparer := comparer;
end;

function TExceptIterator<T>.Clone: TIterator<T>;
begin
  Result := TExceptIterator<T>.Create(fSource, fSecond, fComparer);
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
{$IFNDEF DELPHI2010}
  fSet := TCollections.CreateSet<T>(fComparer);
{$ELSE}
  fSet := THashSet<T>.Create(fComparer);
{$ENDIF}
  fSet.AddRange(fSecond);
  fEnumerator := fSource.GetEnumerator;
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
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(first), 'first');
  Guard.CheckNotNull(Assigned(second), 'second');
{$ENDIF}

  inherited Create;
  fSource := first;
  fSecond := second;
  fComparer := comparer;
end;

function TIntersectIterator<T>.Clone: TIterator<T>;
begin
  Result := TIntersectIterator<T>.Create(fSource, fSecond, fComparer);
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
{$IFNDEF DELPHI2010}
  fSet := TCollections.CreateSet<T>(fComparer);
{$ELSE}
  fSet := THashSet<T>.Create(fComparer);
{$ENDIF}
  fSet.AddRange(fSecond);
  fEnumerator := fSource.GetEnumerator;
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
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(first), 'first');
  Guard.CheckNotNull(Assigned(second), 'second');
{$ENDIF}

  inherited Create;
  fSource := first;
  fSecond := second;
  fComparer := comparer;
end;

function TUnionIterator<T>.Clone: TIterator<T>;
begin
  Result := TUnionIterator<T>.Create(fSource, fSecond, fComparer);
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
{$IFNDEF DELPHI2010}
    fSet := TCollections.CreateSet<T>(fComparer);
{$ELSE}
    fSet := THashSet<T>.Create(fComparer);
{$ENDIF}
    fEnumerator := fSource.GetEnumerator;
  end
  else
    fEnumerator := fSecond.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TSelectIterator<TSource, TResult>'}

constructor TSelectIterator<TSource, TResult>.Create(
  const source: IEnumerable<TSource>; const selector: TFunc<TSource, TResult>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');
{$ENDIF}

  inherited Create;
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

function TSelectIterator<TSource, TResult>.TryMoveNext(var current: TResult): Boolean;
begin
  Result := fEnumerator.MoveNext;
  if Result then
    current := fSelector(fEnumerator.Current);
end;

procedure TSelectIterator<TSource, TResult>.Start;
begin
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TSelectIndexIterator<TSource, TResult>'}

constructor TSelectIndexIterator<TSource, TResult>.Create(
  const source: IEnumerable<TSource>;
  const selector: TFunc<TSource, Integer, TResult>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');
{$ENDIF}

  inherited Create;
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
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>);
begin
  Create(source, keySelector, elementSelector, TEqualityComparer<TKey>.Default);
end;

constructor TGroupedEnumerable<TSource, TKey, TElement>.Create(
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>;
  const comparer: IEqualityComparer<TKey>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
  Guard.CheckNotNull(Assigned(elementSelector), 'elementSelector');
{$ENDIF}

  inherited Create;
  fSource := source;
  fKeySelector := keySelector;
  fElementSelector := elementSelector;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := TEqualityComparer<TKey>.Default;
end;

function TGroupedEnumerable<TSource, TKey, TElement>.GetEnumerator: IEnumerator<IGrouping<TKey, TElement>>;
begin
  Result := TEnumerator.Create(fSource, fKeySelector, fElementSelector, fComparer);
end;

{$ENDREGION}


{$REGION 'TGroupedEnumerable<TSource, TKey, TElement>.TEnumerator'}

constructor TGroupedEnumerable<TSource, TKey, TElement>.TEnumerator.Create(
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>;
  const comparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  fSource := source;
  fKeySelector := keySelector;
  fElementSelector := elementSelector;
  fComparer := comparer;
end;

function TGroupedEnumerable<TSource, TKey, TElement>.TEnumerator.GetCurrent: IGrouping<TKey, TElement>;
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
begin
  fLookup := TLookup<TKey, TElement>.Create<TSource>(
    fSource, fKeySelector, fElementSelector, fComparer);
  fEnumerator := fLookup.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TGroupedEnumerable<TSource, TKey, TElement, TResult>'}

constructor TGroupedEnumerable<TSource, TKey, TElement, TResult>.Create(
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>;
  const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>);
begin
  Create(source, keySelector, elementSelector, resultSelector,
    TEqualityComparer<TKey>.Default);
end;

constructor TGroupedEnumerable<TSource, TKey, TElement, TResult>.Create(
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>;
  const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>;
  const comparer: IEqualityComparer<TKey>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
  Guard.CheckNotNull(Assigned(elementSelector), 'elementSelector');
  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');
{$ENDIF}

  inherited Create;
  fSource := source;
  fKeySelector := keySelector;
  fElementSelector := elementSelector;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := TEqualityComparer<TKey>.Default;
  fResultSelector := resultSelector;
end;

function TGroupedEnumerable<TSource, TKey, TElement, TResult>.GetEnumerator: IEnumerator<TResult>;
begin
  // TODO: deferred execution ?
  Result := TEnumerator.Create(TLookup<TKey, TElement>.Create<TSource>(
    fSource, fKeySelector, fElementSelector, fComparer).GetEnumerator,
    fResultSelector);
end;

{$ENDREGION}


{$REGION 'TGroupedEnumerable<TSource, TKey, TElement, TResult>.TEnumerator'}

constructor TGroupedEnumerable<TSource, TKey, TElement, TResult>.TEnumerator.Create(
  const source: IEnumerator<IGrouping<TKey, TElement>>;
  const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>);
begin
  inherited Create;
  fSource := source;
  fResultSelector := resultSelector;
end;

function TGroupedEnumerable<TSource, TKey, TElement, TResult>.TEnumerator.GetCurrent: TResult;
var
  g: IGrouping<TKey, TElement>;
begin
  g := fSource.Current;
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
  Create(TEqualityComparer<TKey>.Default);
end;

constructor TLookup<TKey, TElement>.Create(const comparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := TEqualityComparer<TKey>.Default;
  fGroupings := TGroupings.Create as IList<TGrouping>;
  fGroupingKeys := TDictionary<TKey, TGrouping>.Create(fComparer);
end;

class function TLookup<TKey, TElement>.Create<TSource>(
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>): TLookup<TKey, TElement>;
begin
  Result := Create<TSource>(source, keySelector, elementSelector, TEqualityComparer<TKey>.Default);
end;

class function TLookup<TKey, TElement>.Create<TSource>(
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>;
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
  const source: IEnumerable<TElement>; const keySelector: TFunc<TElement, TKey>;
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

destructor TLookup<TKey, TElement>.Destroy;
begin
  fGroupingKeys.Free;
  inherited Destroy;
end;

function TLookup<TKey, TElement>.Contains(const key: TKey): Boolean;
begin
  Result := Assigned(GetGrouping(key, False));
end;

function TLookup<TKey, TElement>.GetCount: Integer;
begin
  Result := fGroupings.Count;
end;

function TLookup<TKey, TElement>.GetEnumerator: IEnumerator<IGrouping<TKey, TElement>>;
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
  const key: TKey): IEnumerable<TElement>;
var
  index: Integer;
begin
  Result := GetGrouping(key, False);
  if not Assigned(Result) then
    Result := TEmptyEnumerable<TElement>.Instance;
end;

{$ENDREGION}


{$REGION 'TLookup<TKey, TElement>.TGrouping'}

constructor TLookup<TKey, TElement>.TGrouping.Create(const key: TKey);
begin
  inherited Create;
  fKey := key;
{$IFNDEF DELPHI2010}
  fElements := TCollections.CreateList<TElement>;
{$ELSE}
  fElements := TList<TElement>.Create;
{$ENDIF}
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

{$ENDREGION}


{$REGION 'TLookup<TKey, TElement>.TGroupings'}

constructor TLookup<TKey, TElement>.TGroupings.Create;
begin
  inherited Create;
  OwnsObjects := False;
end;

procedure TLookup<TKey, TElement>.TGroupings.Changed(const item: TGrouping;
  action: TCollectionChangedAction);
begin
  inherited Changed(item, action);
  case action of
    caAdded: TGrouping(item)._AddRef;
    caRemoved: TGrouping(item)._Release;
  end;
end;

{$ENDREGION}


{$REGION 'TLookup<TKey, TElement>.TEnumerator'}

constructor TLookup<TKey, TElement>.TEnumerator.Create(
  const source: TLookup<TKey, TElement>);
begin
  inherited Create;
  fSource := source.fGroupings;
  fIndex := -1;
  fLookup := source;
end;

function TLookup<TKey, TElement>.TEnumerator.GetCurrent: IGrouping<TKey, TElement>;
begin
  Result := fSource[fIndex];
end;

function TLookup<TKey, TElement>.TEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < fSource.Count - 1;
  if Result then
    Inc(fIndex);
end;

{$ENDREGION}


{$REGION 'TJoinIterator<TOuter, TInner, TKey, TResult>'}

constructor TJoinIterator<TOuter, TInner, TKey, TResult>.Create(
  const outer: IEnumerable<TOuter>; const inner: IEnumerable<TInner>;
  const outerKeySelector: TFunc<TOuter, TKey>;
  const innerKeySelector: TFunc<TInner, TKey>;
  const resultSelector: TFunc<TOuter, TInner, TResult>);
begin
  Create(outer, inner, outerKeySelector, innerKeySelector, resultSelector,
    TEqualityComparer<TKey>.Default);
end;

constructor TJoinIterator<TOuter, TInner, TKey, TResult>.Create(
  const outer: IEnumerable<TOuter>; const inner: IEnumerable<TInner>;
  const outerKeySelector: TFunc<TOuter, TKey>;
  const innerKeySelector: TFunc<TInner, TKey>;
  const resultSelector: TFunc<TOuter, TInner, TResult>;
  const comparer: IEqualityComparer<TKey>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(outer), 'outer');
  Guard.CheckNotNull(Assigned(inner), 'inner');
  Guard.CheckNotNull(Assigned(outerKeySelector), 'outerKeySelector');
  Guard.CheckNotNull(Assigned(innerKeySelector), 'innerKeySelector');
  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');
{$ENDIF}

  inherited Create;
  fOuter := outer;
  fInner := inner;
  fOuterKeySelector := outerKeySelector;
  fInnerKeySelector := innerKeySelector;
  fResultSelector := resultSelector;
  fComparer := comparer;
end;

destructor TJoinIterator<TOuter, TInner, TKey, TResult>.Destroy;
begin
  fLookup.Free;
  inherited Destroy;
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

    if fIndex < fGrouping.Count then
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
  const outerKeySelector: TFunc<TOuter, TKey>;
  const innerKeySelector: TFunc<TInner, TKey>;
  const resultSelector: TFunc<TOuter, IEnumerable<TInner>, TResult>);
begin
  Create(outer, inner, outerKeySelector, innerKeySelector, resultSelector,
    TEqualityComparer<TKey>.Default);
end;

constructor TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Create(
  const outer: IEnumerable<TOuter>; const inner: IEnumerable<TInner>;
  const outerKeySelector: TFunc<TOuter, TKey>;
  const innerKeySelector: TFunc<TInner, TKey>;
  const resultSelector: TFunc<TOuter, IEnumerable<TInner>, TResult>;
  const comparer: IEqualityComparer<TKey>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(outer), 'outer');
  Guard.CheckNotNull(Assigned(inner), 'inner');
  Guard.CheckNotNull(Assigned(outerKeySelector), 'outerKeySelector');
  Guard.CheckNotNull(Assigned(innerKeySelector), 'innerKeySelector');
  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');
{$ENDIF}

  inherited Create;
  fOuter := outer;
  fInner := inner;
  fOuterKeySelector := outerKeySelector;
  fInnerKeySelector := innerKeySelector;
  fResultSelector := resultSelector;
  fComparer := comparer;
end;

destructor TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Destroy;
begin
  fLookup.Free;
  inherited Destroy;
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
  const selector: TFunc<TSource, IEnumerable<TResult>>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');
{$ENDIF}

  inherited Create;
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
  const selector: TFunc<TSource, Integer, IEnumerable<TResult>>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');
{$ENDIF}

  inherited Create;
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
  const collectionSelector: TFunc<TSource, IEnumerable<TCollection>>;
  const resultSelector: TFunc<TSource, TCollection, TResult>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(collectionSelector), 'collectionSelector');
  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');
{$ENDIF}

  inherited Create;
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
  const collectionSelector: TFunc<TSource, Integer, IEnumerable<TCollection>>;
  const resultSelector: TFunc<TSource, TCollection, TResult>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(collectionSelector), 'collectionSelector');
  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');
{$ENDIF}

  inherited Create;
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
  const keySelector: TFunc<TElement, TKey>; const comparer: IComparer<TKey>;
  descending: Boolean; const next: IEnumerableSorter<TElement>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
{$ENDIF}

  inherited Create;
  fKeySelector := keySelector;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := TComparer<TKey>.Default;
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

{$ENDREGION}


{$REGION 'TOrderedEnumerable<T>.TEnumerator'}

constructor TOrderedEnumerable<T>.TEnumerator.Create(
  const source: IEnumerable<T>; const sorter: IEnumerableSorter<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(sorter), 'sorter');
{$ENDIF}

  inherited Create;
  fBuffer := source.ToArray;
  fMap := sorter.Sort(fBuffer, Length(fBuffer));
  fIndex := -1;
end;

function TOrderedEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  Result := fBuffer[fMap[fIndex]];
end;

function TOrderedEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < High(fBuffer);
  if Result then
    Inc(fIndex);
end;

{$ENDREGION}


{$REGION 'TOrderedEnumerable<TElement, TKey>'}

constructor TOrderedEnumerable<TElement, TKey>.Create(
  const source: IEnumerable<TElement>;
  const keySelector: TFunc<TElement, TKey>);
begin
  Create(source, keySelector, TComparer<TKey>.Default);
end;

constructor TOrderedEnumerable<TElement, TKey>.Create(
  const source: IEnumerable<TElement>; const keySelector: TFunc<TElement, TKey>;
  const comparer: IComparer<TKey>; descending: Boolean);
var
  obj: TObject;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
{$ENDIF}

  inherited Create;
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
    fComparer := TComparer<TKey>.Default;
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
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
  fSource := source;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := fSource.Comparer;
end;

function TOrderedIterator<T>.Clone: TIterator<T>;
begin
  Result := TOrderedIterator<T>.Create(fSource, fComparer);
end;

procedure TOrderedIterator<T>.Dispose;
begin
  fValues := nil;
end;

function TOrderedIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  Result := fIndex < Length(fValues);
  if Result then
  begin
    current := fValues[fIndex];
    Inc(fIndex);
  end;
end;

procedure TOrderedIterator<T>.Start;
begin
  fValues := fSource.ToArray;
  TArray.Sort<T>(fValues, fComparer);
end;

{$ENDREGION}


{$REGION 'TZipIterator<TFirst, TSecond, TResult>'}

constructor TZipIterator<TFirst, TSecond, TResult>.Create(
  const first: IEnumerable<TFirst>; const second: IEnumerable<TSecond>;
  const resultSelector: TFunc<TFirst, TSecond, TResult>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(first), 'first');
  Guard.CheckNotNull(Assigned(second), 'second');
  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');
{$ENDIF}

  inherited Create;
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
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
  fSource := source;
  fDefaultValue := defaultValue;
end;

function TDefaultIfEmptyIterator<T>.Clone: TIterator<T>;
begin
  Result := TDefaultIfEmptyIterator<T>.Create(fSource, fDefaultValue);
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
  fEnumerator := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TExtremaByIterator<T, TKey>'}

constructor TExtremaByIterator<T, TKey>.Create(const source: IEnumerable<T>;
  const keySelector: TFunc<T, TKey>; const compare: TFunc<TKey, TKey, Integer>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
  Guard.CheckNotNull(Assigned(compare), 'compare');
{$ENDIF}

  inherited Create;
  fSource := source;
  fKeySelector := keySelector;
  fCompare := compare;
end;

function TExtremaByIterator<T, TKey>.Clone: TIterator<T>;
begin
  Result := TExtremaByIterator<T, TKey>.Create(fSource, fkeySelector, fCompare);
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
{$IFNDEF DELPHI2010}
  fResult := TCollections.CreateList<T>;
{$ELSE}
  fResult := TList<T>.Create;
{$ENDIF}
  fEnumerator := fSource.GetEnumerator;
  if not fEnumerator.MoveNext then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);

  current := fEnumerator.Current;
  resultKey := fKeySelector(current);
  fResult.Add(current);

  while fEnumerator.MoveNext do
  begin
    current := fEnumerator.Current;
    key := fKeySelector(current);
    compareResult := fCompare(key, resultKey);
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
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
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
    value := TValue.From<T>(item);
    current := value.AsType<TResult>;
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
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
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
    value := TValue.From<T>(item);
    if value.TryAsType<TResult>(current) then
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

constructor TRepeatIterator<T>.Create(const element: T; count: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(count >= 0, 'count');
{$ENDIF}

  inherited Create;
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

function TRepeatIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  Result := fIndex < fCount;
  begin
    Inc(fIndex);
    current := fElement;
  end;
end;

{$ENDREGION}


{$REGION 'TAnonymousIterator<T>'}

constructor TAnonymousIterator<T>.Create(const count: TFunc<Integer>;
  const items: TFunc<Integer, T>);
begin
  inherited Create;
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

function TAnonymousIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  Result :=  fIndex < fCount;
  if Result then
  begin
    current := fItems(fIndex);
    Inc(fIndex);
  end;
end;

{$ENDREGION}


end.
