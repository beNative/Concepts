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

unit Spring.Tests.Collections;

{$I Spring.inc}

interface

uses
  Classes,
  Generics.Defaults,
  TestFramework,
  Spring.Testing,
  Spring.TestUtils,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.LinkedLists,
  Spring.Collections.Lists,
  Spring.Collections.Trees;

type
  TTestCollectionChangedEventBase = class(TTestCase)
  protected
    type
      TEvent<T> = record
        sender: TObject;
        item: T;
        action: TCollectionChangedAction;
      end;
  protected
    fChangedEvents: IList<TEvent<Integer>>;
    Sender: TObject;
    procedure Changed(Sender: TObject; const Item: Integer; Action: TCollectionChangedAction);
    procedure CheckChanged(index: Integer; item: Integer; action: TCollectionChangedAction);

    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestMapChangedEventBase = class(TTestCase)
  protected
    type
      TEvent<T> = record
        sender: TObject;
        item: T;
        action: TCollectionChangedAction;
      end;
      TKeyValuePair = TPair<Integer, string>;
  protected
    fChangedEvents: IList<TEvent<TKeyValuePair>>;
    fKeyChangedEvents: IList<TEvent<Integer>>;
    fValueChangedEvents: IList<TEvent<string>>;
    Sender: TObject;
    procedure Changed(Sender: TObject; const Item: TKeyValuePair; Action: TCollectionChangedAction);
    procedure KeyChanged(Sender: TObject; const Item: Integer; Action: TCollectionChangedAction);
    procedure ValueChanged(Sender: TObject; const Item: string; Action: TCollectionChangedAction);
    procedure CheckChanged(index: Integer; key: Integer; const value: string; action: TCollectionChangedAction);
    procedure CheckKeyChanged(index: Integer; key: Integer; action: TCollectionChangedAction);
    procedure CheckValueChanged(index: Integer; const value: string; action: TCollectionChangedAction);

    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestEmptyHashSet = class(TTestCase)
  private
    fSet: ISet<Integer>;
    fEmpty: ISet<Integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmpty;
    procedure TestAddDuplications;
    procedure TestExceptWith;
    procedure TestIntersectWith;
    procedure TestUnionWith;
    procedure TestSetEquals;
  end;

  TTestNormalHashSet = class(TTestCase)
  private
    fSet1: ISet<Integer>;
    fSet2: ISet<Integer>;
    fChangeCount: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CheckSet(const collection: ISet<Integer>; const values: array of Integer);
    procedure NotifyChange(Sender: TObject; const item: Integer; action: TCollectionChangedAction);
  published
    procedure TestExceptWith;
    procedure TestIntersectWith;
    procedure TestIntersectWithList;
    procedure TestIntersectWithSameNoChange;
    procedure TestUnionWith;
    procedure TestSetEquals;
    procedure TestSetEqualsList;
    procedure TestIsSubsetOf;
    procedure TestIsSupersetOf;
    procedure TestOverlaps;
    procedure TestExtract;
    procedure Issue368;
  end;

  TTestIntegerList = class(TTestCase)
  private
    SUT: IList<Integer>;
    ChangeCount: Integer;
    procedure SimpleFillList;
    procedure HandleChange(Sender: TObject; const item: Integer; action: TCollectionChangedAction);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TestListRemove;  // Empty
  published
    procedure TestListIsInitializedEmpty;
    procedure TestListCountWithAdd;
    procedure TestListCountWithInsert;
    procedure TestListInsertBetween;
    procedure TestListInsertBeginning;
    procedure TestListInsertEnd;
    procedure TestListInsertRangeArray;
    procedure TestListInsertRangeIEnumerable;
    procedure TestListInsertRangeIEnumerableSelf;
    procedure TestListInsertRangeIEnumerableWithExtraCapacity;
    procedure TestListSimpleDelete;
    procedure TestListMultipleDelete;
    procedure TestListExtractRange;
    procedure TestListRemoveRange;
    procedure TestListRemoveAll;
    procedure TestListSimpleExchange;
    procedure TestListReverse;
    procedure TestListReverseEmpty;
    procedure TestListSort;
    procedure TestListIndexOf;
    procedure TestLastIndexOf;
    procedure TestListMove;
    procedure TestListMoveSameIndexes;
    procedure TestListClear;
    procedure TestListLargeDelete;
    procedure TestQueryInterface;
    procedure TestIssue67;
    procedure TestCopyTo;
    procedure TestIssue53;

    procedure GetCapacity;
    procedure SetCapacity;

    procedure TestGetRange_AllItems;
    procedure TestGetRange_FirstItems;
    procedure TestGetRange_LastItems;
    procedure TestGetRange_NoItems;

    procedure TestExtract_ItemNotInList;
    procedure TestExtractAll_OneItemInList;
    procedure TestExtractAll_MultipleItemsInList_RemoveSome;

    procedure TestEnumeratorMoveNext_VersionMismatch;

    procedure TestRemoveAll;

    procedure TestAddRange_EmptySource;

    procedure TestExtractAt;
    procedure TestExtractRange;

    procedure TestTryMethodsReturnDefaultWhenFalse;

    procedure TestDeleteRange;
    procedure TestDeleteRangeEmptyList;

    procedure EqualsToArray;
    procedure EqualsToEnumerable;
  end;

  TTestStringList = class(TTestCase)
  private
    SUT: IList<string>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure FillList;
  published
    procedure TestCaseInsensitive;
    procedure TestAdd;
    procedure TestDelete;
    procedure TestExtractRange;
    procedure TestListInsertRangeIEnumerableSelf;
    procedure TestGetRange_SomeItems;
  end;

  TTestSortedList = class(TTestCase)
  private const
    SortedPrimes: array[0..6] of Integer = (2, 3, 5, 7, 11, 13, 17);
    NotSortedPrimes: array[0..6] of Integer = (13, 5, 11, 7, 3, 17, 2);
  private
    SUT: IList<Integer>;
    procedure CheckAddRange;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Will actually test Add as well
    procedure TestAddRange_Sorted;
    procedure TestAddRange_NotSorted;
    procedure SetItemRaisesNotSupported;

    procedure TestReturnsMinusOneWhenNotFound;
  end;

  TTestEmptyStackofStrings = class(TTestCase)
  private
    SUT: IStack<string>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStackInitializesEmpty;
    procedure TestEmptyPopPeek;
  end;

  TTestStackOfInteger = class(TTestCase)
  private
    const MaxStackItems = 1000;
  private
    SUT: IStack<Integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure FillStack;
  published
    procedure TestStackCreate;
    procedure TestStackInitializesEmpty;
    procedure TestStackPopPushBalances;
    procedure TestStackClear;
    procedure TestStackPeek;
    procedure TestStackPeekOrDefault;
    procedure TestStackTryPeek;
    procedure TestStackTryPop;
    procedure TestStackTrimExcess;

    procedure TestBoundedStack;
  end;

  TTestStackOfTBytes = class(TTestCase)
  private
    SUT: IStack<TBytes>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStackPush;
  end;

  TTestStackOfIntegerChangedEvent = class(TTestCase)
  private
    SUT: IStack<Integer>;
    fAInvoked, fBInvoked: Boolean;
    fAItem, fBItem: Integer;
    fAAction, fBAction: TCollectionChangedAction;
    procedure HandlerA(Sender: TObject; const Item: Integer; Action: TCollectionChangedAction);
    procedure HandlerB(Sender: TObject; const Item: Integer; Action: TCollectionChangedAction);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmpty;
    procedure TestOneHandler;
    procedure TestTwoHandlers;
    procedure TestNonGenericChangedEvent;
  end;

  TTestEmptyQueueOfInteger = class(TTestCase)
  private
    SUT: IQueue<Integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyQueueIsEmpty;
    procedure TestClearOnEmptyQueue;
    procedure TestPeekRaisesException;
    procedure TestDequeueRaisesException;
  end;

  TTestEmptyDequeOfInteger = class(TTestCase)
  private
    SUT: IDeque<Integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyDequeIsEmpty;
    procedure TestClearOnEmptyDeque;
    procedure TestFirstRaisesException;
    procedure TestLastRaisesException;
    procedure TestRemoveFirstRaisesException;
    procedure TestRemoveLastRaisesException;
  end;

  TTestQueueOfInteger = class(TTestCase)
  private
    SUT: IQueue<Integer>;
    procedure FillQueue;  // Will test Enqueue method
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestQueueCreate;
    procedure TestQueueClear;
    procedure TestQueueDequeue;
    procedure TestQueuePeek;
    procedure TestQueuePeekOrDefault;
    procedure TestQueueTryDequeue;
    procedure TestQueueTryPeek;
    procedure TestQueueTrimExcess;
    procedure TestEnumerator;
  end;

  TTestDequeOfInteger = class(TTestCase)
  private
    SUT: IDeque<Integer>;
    procedure FillDequeFirst;  // Will test AddFirst
    procedure FillDequeLast;  // Will test AddLast
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDequeCreate;
    procedure TestDequeClear;
    procedure TestDequeFirst;
    procedure TestDequeFirstOrDefault;
    procedure TestDequeGrowth;
    procedure TestDequeLast;
    procedure TestDequeLastOrDefault;
    procedure TestDequeRemoveFirst;
    procedure TestDequeRemoveLast;
    procedure TestDequeSingle;
    procedure TestDequeSingleOrDefault;
    procedure TestDequeTrimExcess;
    procedure TestDequeTryGetFirst;
    procedure TestDequeTryGetLast;
    procedure TestDequeTryRemoveFirst;
    procedure TestDequeTryRemoveLast;
  end;

  TTestBoundedDeque = class(TTestCase)
  private
    SUT: IDeque<Integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
  end;

  TTestEvictingDeque = class(TTestCase)
  private
    SUT: IDeque<Integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
  end;

  TTestQueueOfTBytes = class(TTestCase)
  private
    SUT: IQueue<TBytes>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestQueueEnqueue;
  end;

  TTestQueueOfIntegerChangedEvent = class(TTestCase)
  private
    SUT: IQueue<Integer>;
    fAInvoked, fBInvoked: Boolean;
    fAItem, fBItem: Integer;
    fAAction, fBAction: TCollectionChangedAction;
    procedure HandlerA(Sender: TObject; const Item: Integer; Action: TCollectionChangedAction);
    procedure HandlerB(Sender: TObject; const Item: Integer; Action: TCollectionChangedAction);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmpty;
    procedure TestOneHandler;
    procedure TestTwoHandlers;
    procedure TestNonGenericChangedEvent;
  end;

  TTestDequeOfIntegerChangedEvent = class(TTestCase)
  private
    SUT: IDeque<Integer>;
    fAInvoked, fBInvoked: Boolean;
    fAItem, fBItem: Integer;
    fAAction, fBAction: TCollectionChangedAction;
    procedure HandlerA(Sender: TObject; const Item: Integer; Action: TCollectionChangedAction);
    procedure HandlerB(Sender: TObject; const Item: Integer; Action: TCollectionChangedAction);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmpty;
    procedure TestOneHandler;
    procedure TestTwoHandlers;
    procedure TestNonGenericChangedEvent;
  end;

  TTestListOfIntegerAsIEnumerable = class(TTestCase)
  private
    InternalList: IList<Integer>;
    SUT: IEnumerable<Integer>;
    procedure FillList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEnumerableIsEmpty;
    procedure TestEnumerableHasCorrectCountAfterFill;
    procedure TestEnumerableFirst;
    procedure TestEnumerableLast;
    procedure TestSingle;
    procedure TestMin;
    procedure TestMax;
    procedure TestContains;
    procedure TestCheckSingleRaisedExceptionWhenHasMultipleItems;
    procedure TestCheckSingleRaisedExceptionWhenEmpty;
    procedure TestElementAt;
    procedure TestToArray;
  end;

  TTestLinkedList = class(TTestCase)
  private
    SUT: ILinkedList<Integer>;
    fItem: Integer;
    fAction: TCollectionChangedAction;
  protected
    procedure ListChanged(Sender: TObject; const Item: Integer;
      Action: TCollectionChangedAction);
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckCount(expectedCount: Integer);
    procedure CheckEvent(expectedItem: Integer;
      expectedAction: TCollectionChangedAction);
    procedure CheckNode(node: TLinkedListNode<Integer>;
      expectedValue: Integer;
      expectedNext: TLinkedListNode<Integer>;
      expectedPrevious: TLinkedListNode<Integer>);
  published
    procedure TestAddFirstNode_EmptyList;
    procedure TestAddFirstValue_EmptyList;

    procedure TestAddFirstNode_ListContainsTwoItems;
    procedure TestAddFirstValue_ListContainsTwoItems;

    procedure TestAddLastNode_EmptyList;
    procedure TestAddLastValue_EmptyList;

    procedure TestAddLastNode_ListContainsTwoItems;
    procedure TestAddLastValue_ListContainsTwoItems;
  end;

  TTestObjectList = class(TTestCase)
  private
    SUT: IList<TPersistent>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestQueryInterface;
    procedure TestObjectListCreate;
    procedure TestGetElementType;
    procedure TestExtractAt;
    procedure TestGetRangeElementType;
    procedure TestIndexOf;
    procedure TestExtractRange;
  end;

  TTestInterfaceList = class(TTestCase)
  private
    SUT: IList<IInvokable>;
    procedure FillList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInterfaceListCreate;
    procedure TestGetElementType;
    procedure TestCopyTo;
    procedure TestDelete;
    procedure TestDeleteRangeFront;
  end;

  TMyCollectionItem = class(TCollectionItem);
  TMyOtherCollectionItem = class(TCollectionItem);

  TTestCollectionList = class(TTestCase)
  private
    SUT: IList<TCollectionItem>;
    Coll: TCollection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestElementType;
    procedure TestAdd;
    procedure TestDelete;
    procedure TestDeleteRange;
    procedure TestExtract;
    procedure TestExtract_ItemNotInList;

    procedure TestExchange;
    procedure TestMove;

    procedure TestEnumeratorMoveNext_VersionMismatch;
  end;

  TTestEnumerable = class(TTestCase)
  published
    procedure TestAggregate;
    procedure TestContains_Nil;
    procedure TestToArray;

    procedure TestTryMethodsReturnDefaultWhenFalse;
  end;

  TTestMultiMapBase = class(TTestCase)
  private
    SUT: IMultiMap<Integer, Integer>;
    ValueAddedCount, ValueRemovedCount, ValueExtractedCount: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure FillData;

    procedure ValueChanged(Sender: TObject; const Item: Integer;
      Action: TCollectionChangedAction);
    procedure ValueChangedObj(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
  published
    procedure TestAddPair;

    procedure TestAddStringPair;

    procedure TestContains;
    procedure TestEnumerator;

    procedure TestKeys; virtual;

    procedure TestInternalEventHandlersDetached;
    procedure TestValueChangedCalledProperly;
    procedure TestValues; virtual;
    procedure TestValuesOrdered;
    procedure TestValuesToArray; virtual;
    procedure TestExtractValues;
    procedure TestExtractKeyValue;

    procedure WrappedCollection;
    procedure WrappedCollectionEnumerator;
  end;

  TTestListMultiMap = class(TTestMultiMapBase)
  protected
    procedure SetUp; override;
  end;

  TTestSetMultiMapBase = class(TTestMultiMapBase)
  published
    procedure AddDuplicates;
  end;

  TTestHashMultiMap = class(TTestSetMultiMapBase)
  protected
    procedure SetUp; override;
  end;

  TTestTreeMultiMap = class(TTestSetMultiMapBase)
  protected
    procedure SetUp; override;
  published
    procedure TestValues; override;
    procedure TestValuesToArray; override;
  end;

  TTestSortedMultiMapBase = class(TTestMultiMapBase)
  published
    procedure TestKeys; override;
    procedure TestValues; override;
    procedure TestValuesToArray; override;
  end;

  TTestSortedListMultiMap = class(TTestSortedMultiMapBase)
  protected
    procedure SetUp; override;
  end;

  TTestSortedSetMultiMapBase = class(TTestSortedMultiMapBase)
  published
    procedure AddDuplicates;
  end;

  TTestSortedHashMultiMap = class(TTestSortedSetMultiMapBase)
  protected
    procedure SetUp; override;
  end;

  TTestSortedTreeMultiMap = class(TTestSortedSetMultiMapBase)
  protected
    procedure SetUp; override;
  published
    procedure TestValues; override;
    procedure TestValuesToArray; override;
  end;

  TTestObjectStack = class(TTestCase)
  private
    SUT: IStack<TObject>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure PopDestroysItemAndReturnsNil;
    procedure ExtractDoesNotDestroysItemButReturnsIt;
  end;

  TTestObjectQueue = class(TTestCase)
  private
    SUT: IQueue<TObject>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure DequeueDestroysItemAndReturnsNil;
    procedure ExtractDoesNotDestroysItemButReturnsIt;
  end;

  TTestObjectDeque = class(TTestCase)
  private
    SUT: IDeque<TObject>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RemoveFirstDestroysItemAndReturnsNil;
    procedure RemoveLastDestroysItemAndReturnsNil;
    procedure ExtractFirstDoesNotDestroysItemButReturnsIt;
    procedure ExtractLastDoesNotDestroysItemButReturnsIt;
  end;

  TTestSet = class(TTestCase)
  protected
    SUT: ISet<string>;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CheckCount(expected: Integer);
  published
    procedure TestEnumeratorMoveNext_VersionMismatch;
    procedure TestEnumeratorKeepsSourceAlive;
    procedure TestEnumerate;

    procedure TestAdd;
    procedure TestRemove;
    procedure TestExtract;
  end;

  TTestSortedSet = class(TTestSet)
  protected
    procedure SetUp; override;
  published
    procedure TestToArray;
  end;

  TTestMultiSetBase = class(TTestCase)
  private
    procedure CheckCount(expected: Integer);
  protected
    SUT: IMultiSet<string>;
    procedure TearDown; override;
    function IsSorted: Boolean; virtual;
  published
    procedure TestAdd;
    procedure TestRemove;
    procedure TestEnumerate;

    procedure TestOrderedByCount;
    procedure TestSetEquals;
  end;

  TTestHashMultiSet = class(TTestMultiSetBase)
  protected
    procedure SetUp; override;
  published
    procedure TestElements;
    procedure TestEntries;
    procedure TestToArray;
  end;

  TTestTreeMultiSet = class(TTestMultiSetBase)
  protected
    procedure SetUp; override;
    function IsSorted: Boolean; override;
  published
    procedure TestItems;
    procedure TestEntries;
    procedure TestToArray;
  end;

  TTestRedBlackTreeInteger = class(TTestCase)
  private
    SUT: IBinaryTree<Integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDelete;
    procedure TestDuplicates;
    procedure TestInsert;
    procedure FuzzyTesting;
  end;

  TTestRedBlackTreeIntegerString = class(TTestCase)
  private
    type
      TKeyValuePair = TPair<Integer, string>;
  private
    SUT: IBinaryTree<Integer, string>;
    function Add(key: Integer): Boolean;
    procedure CheckKeyValuePair(expectedKey: Integer; const pair: TKeyValuePair; const msg: string = '');
    procedure CheckKeyValuePairs(const expectedKeys: IEnumerable<Integer>; const msg: string = '');
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDelete;
    procedure TestDuplicates;
    procedure TestInsert;
    procedure FuzzyTesting;
  end;

  TTestMultiMapChangedEventBase = class(TTestMapChangedEventBase)
  private
    procedure AddEventHandlers;
  protected
    SUT: IMultiMap<Integer, string>;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestClear;
    procedure TestDestroy;
    procedure TestExtract;
    procedure TestExtractPair;
    procedure TestRemove;
    procedure TestRemovePair;
  end;

  TTestMultiSetChangedEventBase = class(TTestCollectionChangedEventBase)
  private
    procedure AddEventHandlers;
  protected
    SUT: IMultiSet<Integer>;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestClear;
    procedure TestDestroy;
    procedure TestRemove;
  end;

  TTestListMultiMapChangedEvent = class(TTestMultiMapChangedEventBase)
  protected
    procedure SetUp; override;
  end;

  TTestHashMultiMapChangedEvent = class(TTestMultiMapChangedEventBase)
  protected
    procedure SetUp; override;
  end;

  TTestTreeMultiMapChangedEvent = class(TTestMultiMapChangedEventBase)
  protected
    procedure SetUp; override;
  end;

  TTestSortedListMultiMapChangedEvent = class(TTestMultiMapChangedEventBase)
  protected
    procedure SetUp; override;
  end;

  TTestSortedHashMultiMapChangedEvent = class(TTestMultiMapChangedEventBase)
  protected
    procedure SetUp; override;
  end;

  TTestSortedTreeMultiMapChangedEvent = class(TTestMultiMapChangedEventBase)
  protected
    procedure SetUp; override;
  end;

  TTestHashMultiSetChangedEvent = class(TTestMultiSetChangedEventBase)
  protected
    procedure SetUp; override;
  end;

  TTestTreeMultiSetChangedEvent = class(TTestMultiSetChangedEventBase)
  protected
    procedure SetUp; override;
  end;

  TEnumerableTestCase = class(TTestCase)
  private type
    TBreakingSequence<T> = class(TEnumerableBase<T>, IEnumerable<T>)
      function GetEnumerator: IEnumerator<T>;
    end;
    TBreakingReadOnlyCollection<T> = class(TBreakingSequence<T>,
      IEnumerable<T>, IReadOnlyCollection<T>)
    private
      fCollection: IReadOnlyCollection<T>;
      function GetCount: Integer;
    public
      constructor Create(const collection: IReadOnlyCollection<T>);
    end;
    TFastInfiniteEnumerator<T> = class(TIterator<T>, IEnumerable<T>)
    protected
      function Clone: TIterator<T>; override;
      function TryMoveNext(var current: T): Boolean; override;
    end;
    TNumberRangeIterator = class(TIterator<Integer>, IEnumerable<Integer>)
    private
      fNum, fCount, fIndex: Integer;
    protected
      function Clone: TIterator<Integer>; override;
      function TryMoveNext(var current: Integer): Boolean; override;
    public
      constructor Create(num, count: Integer);
    end;
    TRunOnceEnumerable<T> = class(TEnumerableBase<T>, IEnumerable<T>)
    private
      fSource: IEnumerable<T>;
      fCalled: Boolean;
    public
      constructor Create(const source: IEnumerable<T>);
      function GetEnumerator: IEnumerator<T>;
    end;
    TNotCollectionIterator<T> = class(TSourceIterator<T>, IEnumerable<T>)
    private
      fEnumerator: IEnumerator<T>;
    protected
      function Clone: TIterator<T>; override;
      procedure Start; override;
      function TryMoveNext(var current: T): Boolean; override;
    end;
    TDelegateIterator<T> = class(TIterator<T>, IEnumerable<T>)
    private
      fMoveNext: Func<Boolean>;
      fCurrent: Func<T>;
      fDispose: Action;
    protected
      function Clone: TIterator<T>; override;
      procedure Dispose; override;
      function TryMoveNext(var current: T): Boolean; override;
    public
      constructor Create(const moveNext: Func<Boolean>;
        const current: Func<T>; const dispose: Action);
    end;
  protected
    class function BreakingSequence<T>: IEnumerable<T>; static;
    class function BreakingReadOnlyCollection<T>(const collection: IReadOnlyCollection<T>): IReadOnlyCollection<T>; static;
    class function FastInfiniteEnumerator<T>: IEnumerable<T>; static;
    class function ForceNotCollection<T>(const source: array of T): IEnumerable<T>; overload; static;
    class function ForceNotCollection<T>(const source: IEnumerable<T>): IEnumerable<T>; overload; static;
    class function NumberRangeGuaranteedNotCollectionType(num, count: Integer): IEnumerable<Integer>; static;
    class function GuaranteedRunOnce<T>(const source: IEnumerable<T>): IEnumerable<T>; static;
    procedure CheckEmpty(const value: IInterface); overload;
    procedure CheckEmpty(const values: array of Integer); overload;
    procedure CheckEquals(const expected, actual: IInterface); overload;
    procedure CheckEquals(const expected, actual: array of Integer); overload;
    procedure CheckEquals(const expected: array of Integer; const actual: IEnumerable<Integer>); overload;
  end;

  TSkipTests = class(TEnumerableTestCase)
  published
    procedure SkipSome;
    procedure SkipSomeIList;
    procedure RunOnce;
    procedure SkipNone;
    procedure SkipNoneIList;
    procedure SkipExcessive;
    procedure SkipExcessiveIList;
    procedure SkipAllExactly;
    procedure SkipAllExactlyIList;
    procedure SkipOnEmpty;
    procedure SkipOnEmptyIList;
    procedure SkipNegative;
    procedure SkipNegativeIList;
    procedure SameResultsRepeatCallsIntQuery;
    procedure SameResultsRepeatCallsIntQueryIList;
    procedure SkipOne;
    procedure SkipOneNotIList;
    procedure SkipAllButOne;
    procedure SkipAllButOneNotIList;
    procedure SkipOneMoreThanAll;
    procedure SkipOneMoreThanAllNotIList;
    procedure ForcedToEnumeratorDoesntEnumerate;
    procedure ForcedToEnumeratorDoesntEnumerateIList;
    procedure Count;
    procedure FollowWithTake;
    procedure FollowWithTakeNotIList;
    procedure FollowWithTakeThenMassiveTake;
    procedure FollowWithTakeThenMassiveTakeNotIList;
    procedure FollowWithSkip;
    procedure FollowWithSkipNotIList;
    procedure ElementAt;
    procedure ElementAtNotIList;
    procedure ElementAtOrDefault;
    procedure ElementAtOrDefaultNotIList;
    procedure First;
    procedure FirstNotIList;
    procedure FirstOrDefault;
    procedure FirstOrDefaultNotIList;
    procedure Last;
    procedure LastNotIList;
    procedure LastOrDefault;
    procedure LastOrDefaultNotIList;
    procedure ToArray;
    procedure ToArrayNotIList;
    procedure RepeatEnumerating;
    procedure RepeatEnumeratingNotIList;
    procedure LazySkipMoreThan32Bits;
  end;

  TSkipTakeData = class
  public
    class function EnumerableData: TArray<TArray<TValue>>; static;
    class function EvaluationBehaviorData: TArray<Integer>; static;
  end;

  TSkipLastTests = class(TEnumerableTestCase)
  published
    [TestCaseSource(TSkipTakeData, 'EnumerableData')]
    procedure SkipLast(const source: IEnumerable<Integer>; count: Integer);
    [TestCaseSource(TSkipTakeData, 'EvaluationBehaviorData')]
    procedure EvaluationBehavior(count: Integer);
    [TestCaseSource(TSkipTakeData, 'EnumerableData')]
    procedure RunOnce(const source: IEnumerable<Integer>; count: Integer);
    procedure List_ChangesAfterSkipLast_ChangesReflectedInResults;
    procedure List_Skip_ChangesAfterSkipLast_ChangesReflectedInResults;
  end;

  TTakeTests = class(TEnumerableTestCase)
  published
    procedure SameResultsRepeatCallsIntQuery;
    procedure SameResultsRepeatCallsIntQueryIList;
    procedure SourceEmptyCountPositive;
    procedure SourceEmptyCountPositiveNotIList;
    procedure SourceNonEmptyCountNegative;
    procedure SourceNonEmptyCountNegativeNotIList;
    procedure SourceNonEmptyCountZero;
    procedure SourceNonEmptyCountZeroNotIList;
    procedure SourceNonEmptyCountOne;
    procedure SourceNonEmptyCountOneNotIList;
    procedure SourceNonEmptyTakeAllExactly;
    procedure SourceNonEmptyTakeAllExactlyNotIList;
    procedure SourceNonEmptyTakeAllButOne;
    procedure RunOnce;
    procedure SourceNonEmptyTakeAllButOneNotIList;
    procedure SourceNonEmptyTakeExcessive;
    procedure SourceNonEmptyTakeExcessiveNotIList;
    procedure ForcedToEnumeratorDoesntEnumerate;
    procedure ForcedToEnumeratorDoesntEnumerateIList;
    procedure Count;
    procedure FollowWithTake;
    procedure FollowWithTakeNotIList;
    procedure FollowWithSkip;
    procedure FollowWithSkipNotIList;
    procedure ElementAt;
    procedure ElementAtNotIList;
    procedure ElementAtOrDefault;
    procedure ElementAtOrDefaultNotIList;
    procedure First;
    procedure FirstNotIList;
    procedure FirstOrDefault;
    procedure FirstOrDefaultNotIList;
    procedure Last;
    procedure LastNotIList;
    procedure LastOrDefault;
    procedure LastOrDefaultNotIList;
    procedure ToArray;
    procedure ToArrayNotIList;
    procedure TakeCanOnlyBeOneList;
    procedure TakeCanOnlyBeOneNotIList;
    procedure RepeatEnumerating;
    procedure RepeatEnumeratingNotIList;

    [TestCase('1000')]
    [TestCase('1000000')]
    [TestCase('maxint')]
    procedure LazySkipAllTakenForLargeNumbers(largeNumber: Integer);
    procedure LazyOverflowRegression;

    [TestCase('0, 0, 0')]
    [TestCase('1, 1, 1')]
    [TestCase('0, maxint, 100')]
    [TestCase('maxint, 0, 0')]
    [TestCase('0xffff, 1, 0')]
    [TestCase('1, 0xffff, 99')]
    [TestCase('maxint, maxint, 0')]
    [TestCase('1, maxint, 99')]
    [TestCase('0, 100, 100')]
    [TestCase('10, 100, 90')]
    procedure CountOfLazySkipTakeChain(skip, take, expected: Integer);

    [TestCase('[1, 2, 3, 4], 1, 3, 2, 4')]
    [TestCase('[1], 0, 1, 1, 1')]
    [TestCase('[1, 2, 3, 5, 8, 13], 1, maxint, 2, 13')]
    [TestCase('[1, 2, 3, 5, 8, 13], 2, maxint, 3, 13')]
    [TestCase('[1, 2, 3, 5, 8, 13], 0, 2, 1, 2')]
    [TestCase('[1, 2, 3, 5, 8, 13], 500, 2, 0, 0')]
    [TestCase('[], 10, 8, 0, 0')]
    procedure FirstAndLastOfLazySkipTakeChain(const source: TArray<Integer>; skip, take, first, last: Integer);

    [TestCase('[1, 2, 3, 4, 5], 1, 3, [-1, 0, 1, 2], [0, 2, 3, 4]')]
    [TestCase('[0xfefe, 7000, 123], 0, 3, [-1, 0, 1, 2], [0, $fefe, 7000, 123]')]
    [TestCase('[0xfefe], 100, 100, [-1, 0, 1, 2], [0, 0, 0, 0]')]
    [TestCase('[0xfefe, 123, 456, 7890, 5555, 55], 1, 10, [-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], [0, 123, 456, 7890, 5555, 55, 0, 0, 0, 0, 0, 0, 0]')]
    procedure ElementAtOfLazySkipTakeChain(const source: TArray<Integer>; skip, take: Integer; indices, expectedValues: TArray<Integer>);
  end;

  TTakeLastTests = class(TEnumerableTestCase)
  published
    [TestCaseSource(TSkipTakeData, 'EnumerableData')]
    procedure TakeLast(const source: IEnumerable<Integer>; count: Integer);
    [TestCaseSource(TSkipTakeData, 'EvaluationBehaviorData')]
    procedure EvaluationBehavior(count: Integer);
    procedure RunOnce(const source: IEnumerable<Integer>; count: Integer);
    procedure List_ChangesAfterTakeLast_ChangesReflectedInResults;
    procedure List_Skip_ChangesAfterTakeLast_ChangesReflectedInResults;
  end;

  TAtLeastTests = class(TEnumerableTestCase)
  published
    procedure NegativeCount;
    procedure EmptySequenceHasAtLeastZeroElements;
    procedure EmptySequenceHasAtLeastOneElement;
    procedure EmptySequenceHasAtLeastManyElements;
    procedure SingleElementHasAtLeastZeroElements;
    procedure SingleElementHasAtLeastOneElement;
    procedure SingleElementHasAtLeastManyElements;
    procedure ManyElementsHasAtLeastZeroElements;
    procedure ManyElementsHasAtLeastOneElement;
    procedure ManyElementsHasAtLeastManyElements;
    procedure DoesNotIterateUnnecessaryElements;
  end;

  TAtMostTests = class(TEnumerableTestCase)
  published
    procedure NegativeCount;
    procedure EmptySequenceHasAtMostZeroElements;
    procedure EmptySequenceHasAtMostOneElement;
    procedure SingleElementHasAtMostZeroElements;
    procedure SingleElementHasAtMostOneElement;
    procedure SingleElementHasAtMostManyElements;
    procedure ManyElementsHasAtMostOneElement;
    procedure DoesNotIterateUnnecessaryElements;
  end;

  TBetweenTests = class(TEnumerableTestCase)
  published
    procedure NegativeMin;
    procedure NegativeMax;
    procedure MaxLesserThanMin;
    procedure MaxEqualsMin;
    [TestCase('1, 2, 4, false')]
    [TestCase('2, 2, 4, true')]
    [TestCase('3, 2, 4, true')]
    [TestCase('4, 2, 4, true')]
    [TestCase('5, 2, 4, false')]
    procedure RangeTests(count, min, max: Integer; expecting: Boolean);
    procedure DoesNotIterateUnnecessaryElements;
  end;

  TExactlyTests = class(TEnumerableTestCase)
  published
    procedure NegativeCount;
    procedure EmptySequenceHasExactlyZeroElements;
    procedure EmptySequenceHasExactlyOneElement;
    procedure SingleElementHasExactlyOneElement;
    procedure ManyElementsHasExactlyOneElement;
    procedure DoesNotIterateUnnecessaryElements;
  end;

  TSourceKind = (skBreakingCollection, skBreakingReadOnlyCollection);

  TMemoizeTests = class(TEnumerableTestCase)
  published
    procedure MemoizeWithPartialIterationBeforeCompleteIteration;
    procedure MemoizeIsLazy;
    procedure MemoizeWithInMemoryCollection;
    procedure MemoizeEnumeratesOnlyOnce;
    procedure MemoizeWithMemoizedSourceReturnsSame;
  end;

  TTestChunk = class(TEnumerableTestCase)
  published
    [TestCase('0')]
    [TestCase('-1')]
    procedure RaisesWhenSizeIsNonPositive(size: Integer);
    procedure ChunkSourceLazily;
    procedure ChunkSourceRepeatCalls;
    procedure ChunkSourceEvenly;
    procedure ChunkSourceUnevenly;
    procedure ChunkSourceSmallerThanMaxSize;
    procedure EmptySourceYieldsNoChunks;
    procedure RemovingFromSourceBeforeIterating;
    procedure AddingToSourceBeforeIterating;
    procedure DoesNotPrematurelyAllocateHugeArray;
  end;

implementation

uses
  Spring.Collections.MultiMaps,
  Spring.Collections.MultiSets,
  Spring.Collections.Queues,
  Math,
  Rtti, // H2443
  StrUtils,
  SysUtils,
  TypInfo;

type
  TMultiMap = TMultiMap<Integer,Integer>;
  THashMultiSet = THashMultiSet<string>;
  TTreeMultiSet = TTreeMultiSet<string>;
  TSortedMultiMap = TSortedMultiMap<Integer,Integer>;
  TIntQueue = TQueue<Integer>;

const
  MinInt = Low(Integer);
  MaxItems = 1000;
  ListCountLimit = 1000;//0000;


{$REGION 'TTestCollectionChangedEventBase'}

procedure TTestCollectionChangedEventBase.SetUp;
begin
  inherited;
  fChangedEvents := TCollections.CreateList<TEvent<Integer>>;
end;

procedure TTestCollectionChangedEventBase.TearDown;
begin
  fChangedEvents := nil;
  inherited;
end;

procedure TTestCollectionChangedEventBase.Changed(Sender: TObject;
  const Item: Integer; Action: TCollectionChangedAction);
var
  event: TEvent<Integer>;
begin
  event.sender := Sender;
  event.item := Item;
  event.action := Action;
  fChangedEvents.Add(event);
end;

procedure TTestCollectionChangedEventBase.CheckChanged(index, item: Integer;
  action: TCollectionChangedAction);
begin
  Check(Sender = fChangedEvents[index].sender);
  Check(item = fChangedEvents[index].item);
  Check(action = fChangedEvents[index].action);
end;

{$ENDREGION}


{$REGION 'TTestMapChangedEventBase'}

procedure TTestMapChangedEventBase.SetUp;
begin
  inherited;
  fChangedEvents := TCollections.CreateList<TEvent<TKeyValuePair>>;
  fKeyChangedEvents := TCollections.CreateList<TEvent<Integer>>;
  fValueChangedEvents := TCollections.CreateList<TEvent<string>>;
end;

procedure TTestMapChangedEventBase.TearDown;
begin
  fValueChangedEvents := nil;
  fKeyChangedEvents := nil;
  fChangedEvents := nil;
  inherited;
end;

procedure TTestMapChangedEventBase.Changed(Sender: TObject;
  const Item: TKeyValuePair; Action: TCollectionChangedAction);
var
  event: TEvent<TKeyValuePair>;
begin
  event.sender := Sender;
  event.item := Item;
  event.action := Action;
  fChangedEvents.Add(event);
end;

procedure TTestMapChangedEventBase.CheckChanged(index, key: Integer;
  const value: string; action: TCollectionChangedAction);
begin
  Check(Sender = fChangedEvents[index].sender);
  Check(key = fChangedEvents[index].item.Key);
  Check(value = fChangedEvents[index].item.Value);
  Check(action = fChangedEvents[index].action);
end;

procedure TTestMapChangedEventBase.CheckKeyChanged(index, key: Integer;
  action: TCollectionChangedAction);
begin
  Check(Sender = fKeyChangedEvents[index].sender);
  Check(key = fKeyChangedEvents[index].item);
  Check(action = fKeyChangedEvents[index].action);
end;

procedure TTestMapChangedEventBase.CheckValueChanged(index: Integer;
  const value: string; action: TCollectionChangedAction);
begin
  Check(Sender = fValueChangedEvents[index].sender);
  Check(value = fValueChangedEvents[index].item);
  Check(action = fValueChangedEvents[index].action);
end;

procedure TTestMapChangedEventBase.KeyChanged(Sender: TObject;
  const Item: Integer; Action: TCollectionChangedAction);
var
  event: TEvent<Integer>;
begin
  event.sender := Sender;
  event.item := Item;
  event.action := Action;
  fKeyChangedEvents.Add(event);
end;

procedure TTestMapChangedEventBase.ValueChanged(Sender: TObject;
  const Item: string; Action: TCollectionChangedAction);
var
  event: TEvent<string>;
begin
  event.sender := Sender;
  event.item := Item;
  event.action := Action;
  fValueChangedEvents.Add(event);
end;

{$ENDREGION}


{$REGION 'TTestEmptyHashSet'}

procedure TTestEmptyHashSet.SetUp;
begin
  inherited;
  fSet := TCollections.CreateSet<Integer>(10);
  fEmpty := TCollections.CreateSet<Integer>(10);
end;

procedure TTestEmptyHashSet.TearDown;
begin
  inherited;
  fSet := nil;
  fEmpty := nil;
end;

procedure TTestEmptyHashSet.TestEmpty;
begin
  CheckEquals(0, fSet.Count);
  CheckFalse(fSet.Any);
end;

procedure TTestEmptyHashSet.TestExceptWith;
begin
  fSet.ExceptWith(fEmpty);
  CheckEquals(0, fSet.Count);
end;

procedure TTestEmptyHashSet.TestIntersectWith;
begin
  fSet.IntersectWith(fEmpty);
  CheckEquals(0, fSet.Count);
end;

procedure TTestEmptyHashSet.TestUnionWith;
begin
  fSet.UnionWith(fEmpty);
  CheckEquals(0, fSet.Count);
end;

procedure TTestEmptyHashSet.TestAddDuplications;
begin
  CheckTrue(fSet.Add(2));
  CheckEquals(1, fSet.Count);

  CheckFalse(fSet.Add(2));
  CheckEquals(1, fSet.Count);
end;

procedure TTestEmptyHashSet.TestSetEquals;
begin
  CheckTrue(fSet.SetEquals(fEmpty));
end;

{$ENDREGION}


{$REGION 'TTestNormalHashSet'}

procedure TTestNormalHashSet.CheckSet(const collection: ISet<Integer>; const values: array of Integer);
var
  value: Integer;
begin
  CheckEquals(Length(values), collection.Count);
  for value in values do
    CheckTrue(collection.Contains(value));
end;

procedure TTestNormalHashSet.Issue368;
begin
  fSet1 := Tcollections.CreateSet<Integer>;

  CheckTrue(fSet1.Add(0));
  CheckEquals(1, fSet1.Count);
  CheckTrue(fSet1.Remove(0));
  CheckEquals(0, fSet1.Count);
  CheckTrue(fSet1.Add(0));
  CheckEquals(1, fSet1.Count);
  CheckTrue(fSet1.Remove(0));
  CheckEquals(0, fSet1.Count);
  CheckTrue(fSet1.Add(8));

  CheckTrue(fSet1.Add(1));
  CheckTrue(fSet1.Add(2));
  CheckTrue(fSet1.Add(3));
  CheckEquals(4, fSet1.Count);
  CheckTrue(fSet1.Remove(3));
  CheckEquals(3, fSet1.Count);
  CheckTrue(fSet1.Add(4));
  CheckTrue(fSet1.Add(5));
  CheckTrue(fSet1.Add(6));
  CheckEquals(6, fSet1.Capacity);

  CheckFalse(fSet1.Contains(7));
end;

procedure TTestNormalHashSet.NotifyChange(Sender: TObject; const item: Integer;
  action: TCollectionChangedAction);
begin
  Inc(fChangeCount);
end;

procedure TTestNormalHashSet.SetUp;
begin
  inherited;
  fSet1 := TCollections.CreateSet<Integer>(10);
  fSet2 := TCollections.CreateSet<Integer>(10);
  fSet1.AddRange([1, 2, 3]);
  fSet2.AddRange([3, 1, 4, 5]);
  fChangeCount := 0;
end;

procedure TTestNormalHashSet.TearDown;
begin
  inherited;
  fSet1 := nil;
  fSet2 := nil;
end;

procedure TTestNormalHashSet.TestExceptWith;
begin
  fSet1.ExceptWith(fSet2);
  CheckSet(fSet1, [2]);
end;

procedure TTestNormalHashSet.TestExtract;
begin
  CheckEquals(3, fSet1.Extract(3));
  CheckEquals(0, fSet1.Extract(6));
  fSet2.Clear;
  fSet2.AddRange([1, 2]);
  CheckTrue(fSet1.SetEquals(fSet2));
end;

procedure TTestNormalHashSet.TestIntersectWith;
begin
  fSet1.IntersectWith(fSet2);
  CheckSet(fSet1, [1, 3]);
end;

procedure TTestNormalHashSet.TestIntersectWithList;
var
  list: IList<Integer>;
begin
  list := TCollections.CreateList<Integer>;
  list.AddRange([3, 1, 4, 5]);
  fSet1.IntersectWith(list);
  CheckSet(fSet1, [1, 3]);
end;

procedure TTestNormalHashSet.TestIntersectWithSameNoChange;
begin
  fSet1.OnChanged.Add(NotifyChange);
  fSet2.Clear;
  fSet2.AddRange(fSet1);
  fSet1.IntersectWith(fSet2);
  CheckEquals(0, fChangeCount);
end;

procedure TTestNormalHashSet.TestIsSubsetOf;
begin
  CheckFalse(fSet1.IsSubsetOf(fSet2));
  fSet2.Add(2);
  CheckTrue(fSet1.IsSubsetOf(fSet2));
end;

procedure TTestNormalHashSet.TestIsSupersetOf;
begin
  CheckFalse(fSet2.IsSupersetOf(fSet1));
  fSet2.Add(2);
  CheckTrue(fSet2.IsSupersetOf(fSet1));
end;

procedure TTestNormalHashSet.TestOverlaps;
begin
  CheckTrue(fSet1.Overlaps(fSet2));
  fSet2.Clear;
  CheckFalse(fSet1.Overlaps(fSet2));
  fSet2.AddRange([4, 5]);
  CheckFalse(fSet1.Overlaps(fSet2));
end;

procedure TTestNormalHashSet.TestUnionWith;
begin
  fSet1.UnionWith(fSet2);
  CheckSet(fSet1, [1, 2, 3, 4, 5]);
end;

procedure TTestNormalHashSet.TestSetEquals;
begin
  CheckFalse(fSet1.SetEquals(fSet2));
  CheckTrue(fSet1.SetEquals(fSet1));
  CheckTrue(fSet2.SetEquals(fSet2));
end;

procedure TTestNormalHashSet.TestSetEqualsList;
var
  list: IList<Integer>;
begin
  list := TCollections.CreateList<Integer>;
  list.AddRange([3, 2, 1]);
  CheckTrue(fSet1.SetEquals(list));
  CheckFalse(fSet2.SetEquals(list));
end;

{$ENDREGION}


{$REGION 'TTestIntegerList'}

procedure TTestIntegerList.EqualsToArray;
begin
  SimpleFillList;
  CheckFalse(SUT.EqualsTo([1, 2]));
  CheckTrue(SUT.EqualsTo([1, 2, 3]));
  CheckFalse(SUT.EqualsTo([1, 2, 3, 4]));
end;

procedure TTestIntegerList.EqualsToEnumerable;
begin
  SimpleFillList;
  CheckFalse(SUT.EqualsTo(TEnumerable.From<Integer>([1, 2])));
  CheckFalse(SUT.EqualsTo(TEnumerable.From<Integer>([2, 3, 4])));
  CheckTrue(SUT.EqualsTo(TEnumerable.From<Integer>([1, 2, 3])));
  CheckFalse(SUT.EqualsTo(TEnumerable.From<Integer>([1, 2, 3, 4])));
end;

procedure TTestIntegerList.GetCapacity;
begin
  SimpleFillList;
  CheckEquals(4, SUT.Capacity);
end;

procedure TTestIntegerList.HandleChange(Sender: TObject; const item: Integer;
  action: TCollectionChangedAction);
begin
  Inc(ChangeCount);
end;

procedure TTestIntegerList.SetCapacity;
begin
  SimpleFillList;
  SUT.Capacity := 2;
  CheckTrue(SUT.EqualsTo([1, 2]));
end;

procedure TTestIntegerList.SetUp;
begin
  inherited;
  SUT := TCollections.CreateList<Integer>;
  ChangeCount := 0;
end;

procedure TTestIntegerList.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestIntegerList.TestAddRange_EmptySource;
var
  list: IList<Integer>;
begin
  list := TCollections.CreateList<Integer>;
  SUT.AddRange(list);
  Pass;
end;


procedure TTestIntegerList.TestCopyTo;
var
  values: TArray<Integer>;
  i: Integer;
begin
  for i := 0 to MaxItems - 1 do
    SUT.Add(i);
  SetLength(values, MaxItems);
  SUT.CopyTo(values, 0);
  CheckEquals(MaxItems, Length(values));
  CheckEquals(SUT.First, values[0]);
  CheckEquals(SUT.Last, values[MaxItems-1]);
  SUT[0] := MaxItems;
  CheckNotEquals(SUT.First, values[0]);
end;

procedure TTestIntegerList.TestDeleteRange;
begin
  SUT.Add(1);
  SUT.DeleteRange(1, 0);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      SUT.DeleteRange(1, 1);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      SUT.DeleteRange(2, 0);
    end);
end;

procedure TTestIntegerList.TestDeleteRangeEmptyList;
begin
  SUT.DeleteRange(0, 0);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      SUT.DeleteRange(0, 1);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      SUT.DeleteRange(1, 0);
    end);
end;

procedure TTestIntegerList.TestEnumeratorMoveNext_VersionMismatch;
var
  e: IEnumerator<Integer>;
begin
  SimpleFillList;
  ExpectedException := EInvalidOperationException;
  e := SUT.GetEnumerator;
  while e.MoveNext do
    SUT.Add(4);
  ExpectedException := nil;
end;

procedure TTestIntegerList.TestExtractAll_MultipleItemsInList_RemoveSome;
var
  callCount: Integer;
  items: TArray<Integer>;
begin
  callCount := 0;
  SUT.AddRange([1, 2, 3, 4, 5]);
  items := SUT.ExtractAll(
    function(const i: Integer): Boolean
    begin
      Result := Odd(i);
      Inc(callCount);
    end);
  CheckEquals(3, Length(items));
  CheckEquals(1, items[0]);
  CheckEquals(3, items[1]);
  CheckEquals(5, items[2]);
  CheckEquals(5, callCount);
  CheckEquals(2, SUT.Count);
  CheckEquals(2, SUT[0]);
  CheckEquals(4, SUT[1]);
end;

procedure TTestIntegerList.TestExtractAll_OneItemInList;
var
  callCount: Integer;
  items: TArray<Integer>;
begin
  callCount := 0;
  SUT.Add(1);
  items := SUT.ExtractAll(
    function(const i: Integer): Boolean
    begin
      Result := True;
      Inc(callCount);
    end);
  CheckEquals(1, Length(items));
  CheckEquals(1, items[0]);
  CheckEquals(1, callCount);
  CheckEquals(0, SUT.Count);
end;

procedure TTestIntegerList.TestExtractAt;
begin
  SimpleFillList;
  CheckEquals(2, SUT.ExtractAt(1));
  CheckEquals(2, SUT.Count);
  CheckEquals(1, SUT[0]);
  CheckEquals(3, SUT[1]);
end;

procedure TTestIntegerList.TestExtractRange;
var
  values: TArray<Integer>;
begin
  SimpleFillList;
  values := SUT.ExtractRange(0, 3);
  CheckEquals(0, SUT.Count);
  CheckEquals(3, Length(values));
  CheckEquals(1, values[0]);
  CheckEquals(2, values[1]);
  CheckEquals(3, values[2]);
end;

procedure TTestIntegerList.TestExtract_ItemNotInList;
begin
  SimpleFillList;
  CheckEquals(0, SUT.Extract(4));
end;

procedure TTestIntegerList.TestGetRange_AllItems;
var
  values: IList<Integer>;
begin
  SimpleFillList;
  values := SUT.GetRange(0, 3);
  CheckEquals(3, values.Count);
  CheckEquals(SUT[0], values[0]);
  CheckEquals(SUT[1], values[1]);
  CheckEquals(SUT[2], values[2]);
end;

procedure TTestIntegerList.TestGetRange_FirstItems;
var
  values: IList<Integer>;
begin
  SimpleFillList;
  values := SUT.GetRange(0, 2);
  CheckEquals(2, values.Count);
  CheckEquals(SUT[0], values[0]);
  CheckEquals(SUT[1], values[1]);
end;

procedure TTestIntegerList.TestGetRange_LastItems;
var
  values: IList<Integer>;
begin
  SimpleFillList;
  values := SUT.GetRange(1, 2);
  CheckEquals(2, values.Count);
  CheckEquals(SUT[1], values[0]);
  CheckEquals(SUT[2], values[1]);
end;

procedure TTestIntegerList.TestGetRange_NoItems;
var
  values: IList<Integer>;
begin
  SimpleFillList;
  values := SUT.GetRange(2, 0);
  CheckEquals(0, values.Count);
end;

type
  TIntegerList = class(TList<Integer>)
  public
    destructor Destroy; override;
  end;

destructor TIntegerList.Destroy;
var
  i: Integer;
begin
  for i in Self do
    if i = 0 then;
  inherited;
end;

procedure TTestIntegerList.TestIssue53;
begin
  SUT := TIntegerList.Create;
  Pass;
end;

procedure TTestIntegerList.TestIssue67;
var
  i: Integer;
begin
  SUT := TCollections.CreateList<Integer>(TComparer<Integer>.Construct(
    function(const left, right: Integer): Integer
    begin
      Result := right - left; // decending
    end));
  SUT.AddRange([1, 3, 5, 7, 9, 2, 4, 6, 8]);
  i := SUT.Where(
    function(const i: Integer): Boolean
    begin
      Result := Odd(i);
    end)
    .Max;
  CheckEquals(1, i);
end;

procedure TTestIntegerList.TestLastIndexOf;
begin
  CheckEquals(-1, SUT.IndexOf(1));
  SUT.Add(1);
  SUT.Add(1);
  SUT.Add(1);
  SUT.Add(2);
  SUT.Add(3);

  CheckEquals(1, SUT.LastIndexOf(1, 1));
  CheckEquals(2, SUT.LastIndexOf(1));
  CheckEquals(-1, SUT.LastIndexOf(4));
end;

procedure TTestIntegerList.TestListClear;
var
  i: Integer;
begin
  for i := 0 to ListCountLimit do
    SUT.Add(i);

  SUT.Clear;

  CheckEquals(0, SUT.Count, 'List not empty after call to Clear');
end;

procedure TTestIntegerList.TestListCountWithAdd;
var
  i: Integer;
begin
  SUT.Capacity := ListCountLimit;
  for i := 1 to ListCountLimit do
  begin
    SUT.Add(i);
    CheckEquals(i, SUT.Count);
  end;
end;

procedure TTestIntegerList.TestListCountWithInsert;
var
  i: Integer;
begin
  for i := 1 to ListCountLimit do
  begin
    SUT.Insert(0, i);
    CheckEquals(i, SUT.Count);
  end;
end;

procedure TTestIntegerList.TestListRemoveAll;
begin
  SUT.AddRange(TEnumerable.Range(1, 9));
  CheckEquals(8, SUT.RemoveAll(function(const x: Integer): Boolean begin Result := x <> 4 end));
  CheckTrue(SUT.EqualsTo([4]));

  SUT.Clear;

  SUT.AddRange(TEnumerable.Range(1, 9));
  CheckEquals(1, SUT.RemoveAll(function(const x: Integer): Boolean begin Result := x = 4 end));
  CheckTrue(SUT.EqualsTo([1, 2, 3, 5, 6, 7, 8, 9]));

  SUT.Clear;

  SUT.AddRange(TEnumerable.Range(1, 9));
  CheckEquals(7, SUT.RemoveAll(function(const x: Integer): Boolean begin Result := (x <> 4) and (x <> 9) end));
  CheckTrue(SUT.EqualsTo([4, 9]));
end;

procedure TTestIntegerList.TestListExtractRange;
begin
  SUT.AddRange(TEnumerable.Range(1, 9));
  SUT.ExtractRange(TEnumerable.Range(3, 5));
  CheckTrue(SUT.EqualsTo([1, 2, 8, 9]));

  SUT.Clear;

  SUT.AddRange(TEnumerable.Range(1, 9));
  SUT.ExtractRange(2, 5);
  CheckTrue(SUT.EqualsTo([1, 2, 8, 9]));

  SUT.Clear;

  SUT.AddRange(TEnumerable.Range(1, 9));
  SUT.ExtractAll(function(const x: Integer): Boolean begin Result := (x >= 3) and (x <= 7) end);
  CheckTrue(SUT.EqualsTo([1, 2, 8, 9]));
end;

procedure TTestIntegerList.TestListRemoveRange;
begin
  SUT.AddRange(TEnumerable.Range(1, 9));
  SUT.RemoveRange(TEnumerable.Range(3, 5));
  CheckTrue(SUT.EqualsTo([1, 2, 8, 9]));
end;

procedure TTestIntegerList.TestListSimpleDelete;
begin
  SUT.Add(1);
  CheckEquals(1, SUT.Count);
  SUT.Delete(0);
  CheckEquals(0, SUT.Count);
end;

procedure TTestIntegerList.TestListReverse;
var
  i: Integer;
begin
  for i := 0 to ListCountLimit do
    SUT.Add(i);
  CheckEquals(ListCountLimit + 1, SUT.Count, 'TestReverse: List count incorrect after initial adds');

  SUT.Reverse;

  for i := ListCountLimit downto 0 do
    CheckEquals(i, SUT[ListCountLimit - i]);

  ExpectedException := EArgumentOutOfRangeException;
  SUT.Reverse(SUT.Count - 1, 2);
end;

procedure TTestIntegerList.TestListReverseEmpty;
begin
  SUT.Reverse;
  Pass;
end;

procedure TTestIntegerList.TestListSimpleExchange;
begin
  SUT.Add(0);
  SUT.Add(1);
  CheckEquals(2, SUT.Count);
  SUT.Exchange(0, 1);
  CheckEquals(2, SUT.Count, 'Count wrong after exchange');
  CheckEquals(1, SUT[0]);
  CheckEquals(0, SUT[1]);
end;

procedure TTestIntegerList.TestListSort;
var
  i: Integer;
begin
  SUT.Add(6);
  SUT.Add(0);
  SUT.Add(2);
  SUT.Add(5);
  SUT.Add(7);
  SUT.Add(1);
  SUT.Add(8);
  SUT.Add(3);
  SUT.Add(4);
  SUT.Add(9);
  CheckEquals(10, SUT.Count, 'Test');
  SUT.Sort;
  for i := 0 to 9 do
    CheckEquals(i, SUT[i], Format('%s: Items not properly sorted at Index %d', ['TestlistSort', i]));
end;

procedure TTestIntegerList.TestQueryInterface;
var
  list: IObjectList;
begin
  CheckException(EIntfCastError,
    procedure
    begin
      list := SUT as IObjectList;
    end);
end;

procedure TTestIntegerList.TestRemoveAll;
var
  i: Integer;
begin
  for i := 1 to 9 do
    SUT.Add(i);
  SUT.RemoveAll(
    function(const x: Integer): Boolean
    begin
      Result := not Odd(x);
    end);
  Check(SUT.EqualsTo([1, 3, 5, 7, 9]));
end;

procedure TTestIntegerList.TestTryMethodsReturnDefaultWhenFalse;
var
  i: Integer;
begin
  i := -1;
  CheckFalse(SUT.TryGetFirst(i));
  CheckEquals(0, i);

  i := -1;
  CheckFalse(SUT.TryGetLast(i));
  CheckEquals(0, i);

  i := -1;
  CheckFalse(SUT.TryGetSingle(i));
  CheckEquals(0, i);
end;

procedure TTestIntegerList.TestListIndexOf;
var
  i: Integer;
begin
  CheckEquals(-1, SUT.IndexOf(1));
  for i := 0 to ListCountLimit - 1 do
    SUT.Add(i);
  CheckEquals(ListCountLimit, SUT.Count, 'TestLimitIndexOf: List count not correct after adding items.');

  for i := 0 to ListCountLimit - 1 do
    CheckEquals(i, SUT.IndexOf(i));

  CheckEquals(-1, SUT.IndexOf(ListCountLimit + 100), 'Index of item not in list was not -1');
end;

procedure TTestIntegerList.TestListInsertBeginning;
begin
  SUT.Add(0);
  SUT.Add(1);
  SUT.Insert(0, 42);
  CheckEquals(3, SUT.Count);
  CheckEquals(42, SUT[0]);
  CheckEquals(0, SUT[1]);
  CheckEquals(1, SUT[2]);
end;

procedure TTestIntegerList.TestListInsertBetween;
begin
  SUT.Add(0);
  SUT.Add(1);
  SUT.Insert(1, 42);
  CheckEquals(3, SUT.Count);
  CheckEquals(0, SUT[0]);
  CheckEquals(42, SUT[1]);
  CheckEquals(1, SUT[2]);
end;

procedure TTestIntegerList.TestListInsertEnd;
begin
  SUT.Add(0);
  SUT.Add(1);
  SUT.Insert(2, 42);
  CheckEquals(3, SUT.Count);
  CheckEquals(0, SUT[0]);
  CheckEquals(1, SUT[1]);
  CheckEquals(42, SUT[2]);
end;

procedure TTestIntegerList.TestListInsertRangeArray;
begin
  SUT.Add(0);
  SUT.Add(1);
  SUT.InsertRange(1, [3, 4]);
  CheckTrue(SUT.EqualsTo([0, 3, 4, 1]));
end;

procedure TTestIntegerList.TestListInsertRangeIEnumerable;
begin
  SUT.Add(0);
  SUT.Add(1);
  SUT.InsertRange(1, TEnumerable.Range(3, 2));
  CheckTrue(SUT.EqualsTo([0, 3, 4, 1]));
end;

procedure TTestIntegerList.TestListInsertRangeIEnumerableSelf;
begin
  SUT.Add(0);
  SUT.Add(1);
  SUT.InsertRange(1, SUT);
  CheckTrue(SUT.EqualsTo([0, 0, 1, 1]));
end;

procedure TTestIntegerList.TestListInsertRangeIEnumerableWithExtraCapacity;
var
  InsertedList: IList<Integer>;
begin
  SUT.Add(0);
  SUT.Add(1);

  InsertedList := TCollections.CreateList<Integer>([3, 4]);
  InsertedList.Capacity := 10;
  SUT.InsertRange(1, InsertedList);

  CheckTrue(SUT.EqualsTo([0, 3, 4, 1]));
end;

procedure TTestIntegerList.TestListIsInitializedEmpty;
begin
  CheckEquals(SUT.Count, 0);
end;

procedure TTestIntegerList.TestListLargeDelete;
var
  i: Integer;
begin
  for i := 0 to ListCountLimit do
    SUT.Add(i);

  for i := 0 to ListCountLimit do
    SUT.Delete(0);

  CheckEquals(0, SUT.Count, 'Not all items properly deleted from large delete');
end;

procedure TTestIntegerList.TestListMove;
begin
  SimpleFillList;
  CheckEquals(3, SUT.Count);

  SUT.Move(0, 2);
  CheckEquals(3, SUT.Count, 'List count is wrong after call to Move');

  CheckEquals(2, SUT[0]);
  CheckEquals(3, SUT[1]);
  CheckEquals(1, SUT[2]);
end;

procedure TTestIntegerList.TestListMoveSameIndexes;
begin
  SimpleFillList;
  SUT.OnChanged.Add(HandleChange);

  SUT.Move(1, 1);
  CheckEquals(0, ChangeCount);
end;

procedure TTestIntegerList.TestListMultipleDelete;
begin
  SimpleFillList;
  CheckEquals(3, SUT.Count);
  SUT.Delete(0);
  CheckEquals(2, SUT.Count);
  SUT.Delete(0);
  CheckEquals(1, SUT.Count);
  SUT.Delete(0);
  CheckEquals(0, SUT.Count);
end;

procedure TTestIntegerList.TestListRemove;
begin

end;

procedure TTestIntegerList.SimpleFillList;
begin
  CheckNotNull(SUT, 'SUT is nil');
  SUT.Add(1);
  SUT.Add(2);
  SUT.Add(3);
end;

{$ENDREGION}


{$REGION 'TTestStringList'}

procedure TTestStringList.FillList;
var
  i: Integer;
begin
  for i := 0 to 9 do
    SUT.Add(IntToStr(i));
end;

procedure TTestStringList.SetUp;
begin
  inherited;
  SUT := TCollections.CreateList<string>;
end;

procedure TTestStringList.TearDown;
begin
  SUT := nil;
end;

procedure TTestStringList.TestAdd;
var
  i: Integer;
begin
  FillList;
  CheckEquals(10, SUT.Count);
  for i := 0 to 9 do
    CheckEquals(IntToStr(i), SUT[i]);
end;

procedure TTestStringList.TestCaseInsensitive;
begin
  SUT := TCollections.CreateList<string>(TStringComparer.OrdinalIgnoreCase);
  SUT.AddRange(['AAA', 'BBB', 'CCC']);
  CheckTrue(SUT.Contains('aaa'));
end;

procedure TTestStringList.TestDelete;
var
  i: Integer;
begin
  FillList;

  // delete last
  SUT.Delete(9);
  CheckEquals(9, SUT.Count);
  for i := 0 to 8 do
    CheckEquals(IntToStr(i), SUT[i]);

  // delete one before last
  SUT.Delete(7);
  CheckEquals(8, SUT.Count);
  for i := 0 to 6 do
    CheckEquals(IntToStr(i), SUT[i]);
  CheckEquals('8', SUT[7]);

  // delete first
  SUT.Delete(0);
  CheckEquals(7, SUT.Count);
  for i := 0 to 5 do
    CheckEquals(IntToStr(i + 1), SUT[i]);
  CheckEquals('8', SUT[6]);

end;

procedure TTestStringList.TestExtractRange;
var
  values: TArray<string>;
begin
  FillList;
  values := SUT.ExtractRange(0, 3);
  CheckEquals(7, SUT.Count);
  CheckEquals(3, Length(values));
  CheckEquals('0', values[0]);
  CheckEquals('1', values[1]);
  CheckEquals('2', values[2]);
end;

procedure TTestStringList.TestGetRange_SomeItems;
var
  values: IList<string>;
begin
  FillList;
  values := SUT.GetRange(0, 3);
  CheckEquals(3, values.Count);
  CheckEquals(SUT[0], values[0]);
  CheckEquals(SUT[1], values[1]);
  CheckEquals(SUT[2], values[2]);
end;

procedure TTestStringList.TestListInsertRangeIEnumerableSelf;
begin
  SUT.Add('1');
  SUT.Add('2');
  SUT.InsertRange(1, SUT);
  CheckTrue(SUT.EqualsTo(['1', '1', '2', '2']));
  SUT.Delete(1);
  SUT.Delete(0);
end;

{$ENDREGION}


{$REGION 'TTestSortedList'}

procedure TTestSortedList.CheckAddRange;
var
  i: Integer;
begin
  CheckEquals(Length(SortedPrimes), SUT.Count);
  for i := 0 to High(SortedPrimes) do
    CheckEquals(SortedPrimes[i], SUT[i]);
end;

procedure TTestSortedList.SetItemRaisesNotSupported;
begin
  SUT.Add(1);
  CheckException(ENotSupportedException, procedure begin SUT[0] := 1 end);
end;

procedure TTestSortedList.SetUp;
begin
  inherited;
  SUT := TSortedList<Integer>.Create;
end;

procedure TTestSortedList.TearDown;
begin
  SUT := nil;
  inherited;
end;

procedure TTestSortedList.TestAddRange_NotSorted;
begin
  SUT.AddRange(NotSortedPrimes);
  CheckAddRange;

  SUT.Clear;
  SUT.AddRange(TCollections.CreateList<Integer>(NotSortedPrimes));
  CheckAddRange;
end;

procedure TTestSortedList.TestAddRange_Sorted;
begin
  SUT.AddRange(SortedPrimes);
  CheckAddRange;

  SUT.Clear;
  SUT.AddRange(TCollections.CreateList<Integer>(SortedPrimes));
  CheckAddRange;
end;

procedure TTestSortedList.TestReturnsMinusOneWhenNotFound;
var
  Result: Integer;
begin
  // Empty
  Result := SUT.IndexOf(42);
  CheckEquals(-1, Result);

  SUT.AddRange([2, 3, 5]);

  // At the end
  Result := SUT.IndexOf(42);
  CheckEquals(-1, Result);

  // At the beginning
  Result := SUT.IndexOf(0);
  CheckEquals(-1, Result);

  // In the middle
  Result := SUT.IndexOf(4);
  CheckEquals(-1, Result);
end;

{$ENDREGION}


{$REGION 'TTestEmptyStackofStrings'}

procedure TTestEmptyStackofStrings.SetUp;
begin
  inherited;
  SUT := TCollections.CreateStack<string>;
end;

procedure TTestEmptyStackofStrings.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestEmptyStackofStrings.TestEmptyPopPeek;
begin
  CheckException(EInvalidOpException, procedure begin SUT.Pop end, 'EListError not raised');
  CheckException(EInvalidOpException, procedure begin SUT.Peek end, 'EListError not raised');
end;

procedure TTestEmptyStackofStrings.TestStackInitializesEmpty;
begin
  CheckEquals(0, SUT.Count);
end;

{$ENDREGION}


{$REGION 'TTestStackOfInteger'}

procedure TTestStackOfInteger.FillStack;
var
  i: Integer;
begin
  Check(SUT <> nil);
  for i := 0 to MaxStackItems do
    SUT.Push(i);
end;

procedure TTestStackOfInteger.SetUp;
begin
  inherited;
  SUT := TCollections.CreateStack<Integer>;
end;

procedure TTestStackOfInteger.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestStackOfInteger.TestBoundedStack;
var
  i: Integer;
begin
  SUT := TCollections.CreateBoundedStack<Integer>(10);
  for i := 1 to 10 do
    CheckTrue(SUT.Push(i));
  CheckFalse(SUT.Push(11));
  CheckEquals(10, SUT.Pop);
  CheckTrue(SUT.Push(11));
  CheckEquals(10, SUT.Count);
end;

procedure TTestStackOfInteger.TestStackClear;
begin
  FillStack;
  SUT.Clear;
  CheckEquals(0, SUT.Count, 'Stack failed to empty after call to Clear');
end;

procedure TTestStackOfInteger.TestStackCreate;
const
  values: array[0..9] of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  expected: array[0..9] of Integer = (9, 8, 7, 6, 5, 4, 3, 2, 1, 0);
begin
  SUT := TCollections.CreateStack<Integer>(values);
  CheckTrue(SUT.EqualsTo(expected));
  SUT := TCollections.CreateStack<Integer>(TEnumerable.Range(0, 10));
  CheckTrue(SUT.EqualsTo(expected));
end;

procedure TTestStackOfInteger.TestStackInitializesEmpty;
begin
  CheckEquals(0, SUT.Count);
end;

procedure TTestStackOfInteger.TestStackPeek;
begin
  FillStack;
  CheckEquals(MaxStackItems, SUT.Peek, 'Stack.Peek failed');
end;

procedure TTestStackOfInteger.TestStackPeekOrDefault;
begin
  FillStack;
  CheckEquals(MaxStackItems, SUT.PeekOrDefault, 'Stack.Peek failed');

  SUT.Clear;
  CheckEquals(Default(Integer), SUT.PeekOrDefault, 'Stack.Peek failed');
end;

procedure TTestStackOfInteger.TestStackPopPushBalances;
var
  i: Integer;
begin
  FillStack;

  for i := 0 to MaxStackItems do
    SUT.Pop;

  // Should be empty
  CheckEquals(0, SUT.Count);
end;

procedure TTestStackOfInteger.TestStackTrimExcess;
begin
  CheckEquals(0, SUT.Capacity);
  SUT.Capacity := MaxItems;
  CheckEquals(MaxItems, SUT.Capacity);
  SUT.TrimExcess;
  CheckEquals(0, SUT.Capacity);
end;

procedure TTestStackOfInteger.TestStackTryPeek;
var
  value: Integer;
begin
  CheckFalse(SUT.TryPeek(value));
  CheckEquals(0, value);
  SUT.Push(MaxItems);
  CheckTrue(SUT.TryPeek(value));
  CheckEquals(MaxItems, value);
end;

procedure TTestStackOfInteger.TestStackTryPop;
var
  i, value: Integer;
begin
  CheckFalse(SUT.TryPop(value));
  CheckEquals(0, value);
  for i := 1 to MaxItems do
    SUT.Push(i);
  for i := MaxItems downto 1 do
  begin
    CheckTrue(SUT.TryPop(value));
    CheckEquals(i, value);
  end;
  CheckFalse(SUT.TryPop(value));
  CheckEquals(0, value);
  CheckTrue(SUT.IsEmpty);
end;

{$ENDREGION}


{$REGION 'TTestStackOfTBytes'}

procedure TTestStackOfTBytes.SetUp;
begin
  SUT := TCollections.CreateStack<TBytes>;
end;

procedure TTestStackOfTBytes.TearDown;
begin
  SUT := nil;
end;

procedure TTestStackOfTBytes.TestStackPush;
var
  b: TBytes;
begin
  b := TBytes.Create(0);
  SUT.Push(b);
  CheckEquals(1, SUT.Count);
  Check(b = SUT.Peek);
end;

{$ENDREGION}


{$REGION 'TTestStackOfIntegerChangedEvent'}

procedure TTestStackOfIntegerChangedEvent.SetUp;
begin
  inherited;
  SUT := TCollections.CreateStack<Integer>;
end;

procedure TTestStackOfIntegerChangedEvent.TearDown;
begin
  inherited;
  SUT := nil;
  fAInvoked := False;
  fBInvoked := False;
end;

procedure TTestStackOfIntegerChangedEvent.HandlerA(Sender: TObject;
  const Item: Integer; Action: TCollectionChangedAction);
begin
  fAItem := Item;
  fAAction := Action;
  fAInvoked := True;
end;

procedure TTestStackOfIntegerChangedEvent.HandlerB(Sender: TObject;
  const Item: Integer; Action: TCollectionChangedAction);
begin
  fBitem := Item;
  fBAction := Action;
  fBInvoked := True;
end;

procedure TTestStackOfIntegerChangedEvent.TestEmpty;
begin
  SUT.Push(0);

  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);
end;

procedure TTestStackOfIntegerChangedEvent.TestOneHandler;
begin
  SUT.OnChanged.Add(HandlerA);

  SUT.Push(0);

  CheckTrue(fAInvoked, 'handler A not invoked');
  CheckTrue(fAAction = caAdded, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');

  CheckFalse(fBInvoked, 'handler B not registered as callback');

  SUT.Pop;

  CheckTrue(fAAction = caRemoved, 'different collection notifications');

  SUT.OnChanged.Remove(HandlerA);
end;

procedure TTestStackOfIntegerChangedEvent.TestTwoHandlers;
begin
  SUT.OnChanged.Add(HandlerA);
  SUT.OnChanged.Add(HandlerB);

  SUT.Push(0);

  CheckTrue(fAInvoked, 'handler A not invoked');
  CheckTrue(fAAction = caAdded, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');
  CheckTrue(fBInvoked, 'handler B not invoked');
  CheckTrue(fBAction = caAdded, 'handler B: different collection notifications');
  CheckEquals(0, fBItem, 'handler B: different item');

  SUT.Pop;

  CheckTrue(fAAction = caRemoved, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');
  CheckTrue(fBAction = caRemoved, 'handler B: different collection notifications');
  CheckEquals(0, fBItem, 'handler B: different item');

  SUT.OnChanged.Remove(HandlerA);
  SUT.OnChanged.Remove(HandlerB);
end;

procedure TTestStackOfIntegerChangedEvent.TestNonGenericChangedEvent;
var
  event: IEvent;
  method: TMethod;
begin
  event := SUT.OnChanged;

  CheckTrue(event.Enabled);

  method.Code := @TTestStackOfIntegerChangedEvent.HandlerA;
  method.Data := Pointer(Self);

  event.Add(method);

  SUT.Push(0);

  CheckTrue(fAInvoked, 'handler A not invoked');
  CheckTrue(fAAction = caAdded, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');
end;

{$ENDREGION}


{$REGION 'TTestEmptyQueueOfInteger'}

procedure TTestEmptyQueueOfInteger.SetUp;
begin
  inherited;
  SUT := TCollections.CreateQueue<Integer>
end;

procedure TTestEmptyQueueOfInteger.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestEmptyQueueOfInteger.TestClearOnEmptyQueue;
begin
  CheckEquals(0, SUT.Count, 'Queue not empty before call to clear');
  SUT.Clear;
  CheckEquals(0, SUT.Count, 'Queue not empty after call to clear');
end;

procedure TTestEmptyQueueOfInteger.TestEmptyQueueIsEmpty;
begin
  CheckEquals(0, SUT.Count);
end;

procedure TTestEmptyQueueOfInteger.TestPeekRaisesException;
begin
  CheckException(EInvalidOpException, procedure begin SUT.Peek end, 'EListError was not raised on Peek call with empty Queue');
end;

procedure TTestEmptyQueueOfInteger.TestDequeueRaisesException;
begin
  CheckException(EInvalidOpException, procedure begin SUT.Dequeue end, 'EListError was not raised on Peek call with empty Queue');
end;

{$ENDREGION}


{$REGION 'TTestEmptyDequeOfInteger'}

procedure TTestEmptyDequeOfInteger.SetUp;
begin
  inherited;
  SUT := TCollections.CreateDeque<Integer>
end;

procedure TTestEmptyDequeOfInteger.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestEmptyDequeOfInteger.TestClearOnEmptyDeque;
begin
  CheckEquals(0, SUT.Count, 'Deque not empty before call to clear');
  SUT.Clear;
  CheckEquals(0, SUT.Count, 'Deque not empty after call to clear');
end;

procedure TTestEmptyDequeOfInteger.TestEmptyDequeIsEmpty;
begin
  CheckEquals(0, SUT.Count);
end;

procedure TTestEmptyDequeOfInteger.TestFirstRaisesException;
begin
  CheckException(EInvalidOperationException, procedure begin SUT.First end, 'EInvalidOperationException was not raised on First call with empty Deque');
end;

procedure TTestEmptyDequeOfInteger.TestLastRaisesException;
begin
  CheckException(EInvalidOperationException, procedure begin SUT.Last end, 'EInvalidOperationException was not raised on Last call with empty Deque');
end;

procedure TTestEmptyDequeOfInteger.TestRemoveFirstRaisesException;
begin
  CheckException(EInvalidOperationException, procedure begin SUT.RemoveFirst end, 'EInvalidOperationException was not raised on RemoveFirst call with empty Deque');
end;

procedure TTestEmptyDequeOfInteger.TestRemoveLastRaisesException;
begin
  CheckException(EInvalidOperationException, procedure begin SUT.RemoveLast end, 'EInvalidOperationException was not raised on RemoveLast call with empty Deque');
end;

{$ENDREGION}


{$REGION 'TTestQueueOfInteger'}

procedure TTestQueueOfInteger.FillQueue;
var
  i: Integer;
begin
  Check(SUT <> nil);
  for i := 0 to MaxItems - 1 do
    SUT.Enqueue(i);
  CheckEquals(MaxItems, SUT.Count, 'Call to FillQueue did not properly fill the queue');
end;

procedure TTestQueueOfInteger.SetUp;
begin
  inherited;
  SUT := TCollections.CreateQueue<Integer>;
end;

procedure TTestQueueOfInteger.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestQueueOfInteger.TestQueueClear;
begin
  FillQueue;
  SUT.Clear;
  CheckEquals(0, SUT.Count, 'Clear call failed to empty the queue');
end;

procedure TTestQueueOfInteger.TestQueueCreate;
const
  values: array[0..9] of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
begin
  SUT := TCollections.CreateQueue<Integer>(values);
  CheckTrue(SUT.EqualsTo(values));
  SUT := TCollections.CreateQueue<Integer>(TEnumerable.Range(0, 10));
  CheckTrue(SUT.EqualsTo(values));
end;

procedure TTestQueueOfInteger.TestQueueDequeue;
var
  i: Integer;
begin
  FillQueue;
  for i := 1 to MaxItems do
    SUT.Dequeue;

  CheckEquals(0, SUT.Count, 'Dequeue did not remove all the items');
end;

procedure TTestQueueOfInteger.TestQueuePeek;
begin
  FillQueue;
  CheckEquals(0, SUT.Peek);
  ExpectedException := EInvalidOpException;
  SUT.Clear;
  SUT.Peek;
  ExpectedException := nil;
end;

procedure TTestQueueOfInteger.TestQueuePeekOrDefault;
begin
  CheckEquals(0, SUT.PeekOrDefault);
  SUT.Enqueue(MaxItems);
  CheckEquals(MaxItems, SUT.PeekOrDefault);
end;

procedure TTestQueueOfInteger.TestQueueTrimExcess;
begin
  CheckEquals(0, SUT.Capacity);
  SUT.Capacity := MaxItems;
  CheckEquals(MaxItems, SUT.Capacity);
  SUT.TrimExcess;
  CheckEquals(0, SUT.Capacity);
end;

procedure TTestQueueOfInteger.TestQueueTryDequeue;
var
  i, value: Integer;
begin
  CheckFalse(SUT.TryDequeue(value));
  CheckEquals(0, value);
  for i := 1 to MaxItems do
    SUT.Enqueue(i);
  for i := 1 to MaxItems do
  begin
    CheckTrue(SUT.TryDequeue(value));
    CheckEquals(i, value);
  end;
  CheckFalse(SUT.TryDequeue(value));
  CheckEquals(0, value);
  CheckTrue(SUT.IsEmpty);
end;

procedure TTestQueueOfInteger.TestQueueTryPeek;
var
  value: Integer;
begin
  CheckFalse(SUT.TryPeek(value));
  CheckEquals(0, value);
  SUT.Enqueue(MaxItems);
  CheckTrue(SUT.TryPeek(value));
  CheckEquals(MaxItems, value);
end;

procedure TTestQueueOfInteger.TestEnumerator;
var
  value, i: Integer;
begin
  SUT.Enqueue(3);
  SUT.Enqueue(4);
  SUT.Enqueue(1);
  SUT.Enqueue(2);
  SUT.Enqueue(SUT.Dequeue);
  SUT.Enqueue(SUT.Dequeue);
  i := 1;
  for value in SUT do
  begin
    CheckEquals(i, value);
    Inc(i);
  end;
  CheckEquals(5, i);
end;

{$ENDREGION}


{$REGION 'TTestDequeOfInteger'}

procedure TTestDequeOfInteger.FillDequeFirst;
var
  i: Integer;
begin
  Check(SUT <> nil);
  for i := 0 to MaxItems - 1 do
    SUT.AddFirst(i);
  CheckEquals(MaxItems, SUT.Count, 'Call to FillDequeFirst did not properly fill the deque');
end;

procedure TTestDequeOfInteger.FillDequeLast;
var
  i: Integer;
begin
  Check(SUT <> nil);
  for i := 0 to MaxItems - 1 do
    SUT.AddLast(i);
  CheckEquals(MaxItems, SUT.Count, 'Call to FillDequeLast did not properly fill the deque');
end;

procedure TTestDequeOfInteger.SetUp;
begin
  inherited;
  SUT := TCollections.CreateDeque<Integer>;
end;

procedure TTestDequeOfInteger.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestDequeOfInteger.TestDequeCreate;
const
  values: array[0..9] of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
begin
  SUT := TCollections.CreateDeque<Integer>(values);
  CheckTrue(SUT.EqualsTo(values));
  SUT := TCollections.CreateDeque<Integer>(TEnumerable.Range(0, 10));
  CheckTrue(SUT.EqualsTo(values));
end;

procedure TTestDequeOfInteger.TestDequeClear;
begin
  FillDequeFirst;
  SUT.Clear;
  CheckEquals(0, SUT.Count, 'Clear call failed to empty the deque');

  FillDequeLast;
  SUT.Clear;
  CheckEquals(0, SUT.Count, 'Clear call failed to empty the deque');
end;

procedure TTestDequeOfInteger.TestDequeFirst;
begin
  FillDequeLast;
  CheckEquals(0, SUT.First);
  ExpectedException := EInvalidOpException;
  SUT.Clear;
  SUT.First;
  ExpectedException := nil;
end;

procedure TTestDequeOfInteger.TestDequeFirstOrDefault;
begin
  CheckEquals(0, SUT.FirstOrDefault);
  SUT.AddLast(MaxItems);
  CheckEquals(MaxItems, SUT.FirstOrDefault);
end;

procedure TTestDequeOfInteger.TestDequeGrowth;
var
  i, value, expected: Integer;
begin
  value := 1;

  for i := 1 to MaxItems do
  begin
    SUT.AddFirst(value);
    SUT.AddFirst(value + 1);
    expected := value + 1;
    CheckEquals(expected, SUT.RemoveFirst);
    Inc(value, 2);
  end;

  for i := 1 to MaxItems do
  begin
    SUT.AddLast(value);
    SUT.AddLast(value + 1);
    expected := value + 1;
    CheckEquals(expected, SUT.RemoveLast);
    Inc(value, 2);
  end;

  expected := value - 2;
  for i := 1 to MaxItems do
  begin
    SUT.AddFirst(value);
    SUT.AddFirst(value + 1);
    CheckEquals(expected, SUT.RemoveLast);
    Dec(expected, 2);
    Inc(value, 2);
  end;

  expected := value - 1;
  for i := 0 to MaxItems - 1 do
  begin
    SUT.AddLast(value);
    SUT.AddLast(value + 1);
    CheckEquals(expected, SUT.RemoveFirst);
    Dec(expected);
    Inc(value, 2);
  end;

  SUT.Clear;
  for i := 0 to 31 do
    SUT.AddLast(i);
  expected := 0;
  while SUT.Count > 0 do
  begin
    CheckEquals(expected, SUT.RemoveFirst);
    Inc(expected);
    SUT.TrimExcess;
  end;

  SUT.Clear;
  for i := 0 to 31 do
    SUT.AddFirst(i);
  expected := 0;
  while SUT.Count > 0 do
  begin
    CheckEquals(expected, SUT.RemoveLast);
    Inc(expected);
    SUT.TrimExcess;
  end;

  SUT.Clear;
  SUT.AddLast(0);
  SUT.AddLast(1);
  SUT.AddLast(2);
  SUT.AddLast(3);
  CheckEquals(0, SUT.RemoveFirst);
  CheckEquals(1, SUT.RemoveFirst);
  SUT.AddLast(0);
  SUT.TrimExcess;
end;

procedure TTestDequeOfInteger.TestDequeLast;
begin
  FillDequeFirst;
  CheckEquals(0, SUT.Last);
  ExpectedException := EInvalidOpException;
  SUT.Clear;
  SUT.Last;
  ExpectedException := nil;
end;

procedure TTestDequeOfInteger.TestDequeLastOrDefault;
begin
  CheckEquals(0, SUT.LastOrDefault);
  SUT.AddFirst(MaxItems);
  CheckEquals(MaxItems, SUT.LastOrDefault);
end;

procedure TTestDequeOfInteger.TestDequeRemoveFirst;
var
  i: Integer;
begin
  FillDequeFirst;
  for i := 1 to MaxItems do
    SUT.RemoveFirst;

  CheckEquals(0, SUT.Count, 'RemoveFirst did not remove all the items');
end;

procedure TTestDequeOfInteger.TestDequeRemoveLast;
var
  i: Integer;
begin
  FillDequeLast;
  for i := 1 to MaxItems do
    SUT.RemoveLast;

  CheckEquals(0, SUT.Count, 'RemoveLast did not remove all the items');
end;

procedure TTestDequeOfInteger.TestDequeSingle;
begin
  CheckException(EInvalidOperationException,
    procedure
    begin
      SUT.Single;
    end);
  SUT.AddFirst(-1);
  CheckEquals(-1, SUT.Single);
  SUT.AddFirst(-2);
  CheckException(EInvalidOperationException,
    procedure
    begin
      SUT.Single;
    end);
end;

procedure TTestDequeOfInteger.TestDequeSingleOrDefault;
begin
  CheckEquals(Default(Integer), SUT.SingleOrDefault);
  CheckEquals(42, SUT.SingleOrDefault(42));
  SUT.AddFirst(-1);
  CheckEquals(-1, SUT.SingleOrDefault);
  CheckEquals(-1, SUT.SingleOrDefault(42));
  SUT.AddFirst(-2);
  CheckException(EInvalidOperationException,
    procedure
    begin
      SUT.SingleOrDefault;
    end);
  CheckException(EInvalidOperationException,
    procedure
    begin
      SUT.SingleOrDefault(42);
    end);
end;

procedure TTestDequeOfInteger.TestDequeTrimExcess;
begin
  CheckEquals(0, SUT.Capacity);
  SUT.Capacity := MaxItems;
  CheckEquals(MaxItems, SUT.Capacity);
  SUT.TrimExcess;
  CheckEquals(0, SUT.Capacity);
end;

procedure TTestDequeOfInteger.TestDequeTryGetFirst;
var
  value: Integer;
begin
  SUT.AddLast(1);
  SUT.AddLast(2);
  SUT.AddLast(3);
  CheckTrue(SUT.TryGetFirst(value));
  CheckEquals(1, value);
  SUT.Clear;
  CheckFalse(SUT.TryGetFirst(value));
  CheckEquals(Default(Integer), value);
end;

procedure TTestDequeOfInteger.TestDequeTryGetLast;
var
  value: Integer;
begin
  SUT.AddFirst(1);
  SUT.AddFirst(2);
  SUT.AddFirst(3);
  CheckTrue(SUT.TryGetLast(value));
  CheckEquals(1, value);
  SUT.Clear;
  CheckFalse(SUT.TryGetLast(value));
  CheckEquals(Default(Integer), value);
end;

procedure TTestDequeOfInteger.TestDequeTryRemoveFirst;
var
  i, value: Integer;
begin
  CheckFalse(SUT.TryRemoveFirst(value));
  CheckEquals(0, value);
  for i := 1 to MaxItems do
    SUT.AddLast(i);
  for i := 1 to MaxItems do
  begin
    CheckTrue(SUT.TryRemoveFirst(value));
    CheckEquals(i, value);
  end;
  CheckFalse(SUT.TryRemoveFirst(value));
  CheckEquals(0, value);
  CheckTrue(SUT.IsEmpty);
end;

procedure TTestDequeOfInteger.TestDequeTryRemoveLast;
var
  i, value: Integer;
begin
  CheckFalse(SUT.TryRemoveLast(value));
  CheckEquals(0, value);
  for i := 1 to MaxItems do
    SUT.AddFirst(i);
  for i := 1 to MaxItems do
  begin
    CheckTrue(SUT.TryRemoveLast(value));
    CheckEquals(i, value);
  end;
  CheckFalse(SUT.TryRemoveLast(value));
  CheckEquals(0, value);
  CheckTrue(SUT.IsEmpty);
end;

{$ENDREGION}


{$REGION 'TTestBoundedDeque'}

procedure TTestBoundedDeque.SetUp;
begin
  inherited;
  SUT := TCollections.CreateBoundedDeque<Integer>(4);
end;

procedure TTestBoundedDeque.TearDown;
begin
  SUT := nil;
  inherited;
end;

procedure TTestBoundedDeque.TestAdd;
var
  i: Integer;
begin
  for i := 1 to 4 do
    CheckTrue(SUT.AddLast(i));
  CheckFalse(SUT.AddLast(5));
  CheckEquals(4, SUT.Count);
  CheckEquals(1, SUT.RemoveFirst);
  CheckTrue(SUT.AddLast(5));
end;

{$ENDREGION}


{$REGION 'TTestEvictingDeque'}

procedure TTestEvictingDeque.SetUp;
begin
  inherited;
  SUT := TCollections.CreateEvictingDeque<Integer>(4);
end;

procedure TTestEvictingDeque.TearDown;
begin
  SUT := nil;
  inherited;
end;

procedure TTestEvictingDeque.TestAdd;
var
  i: Integer;
begin
  for i := 1 to 4 do
    CheckTrue(SUT.AddLast(i));
  CheckTrue(SUT.AddLast(5));
  CheckEquals(4, SUT.Count);
  CheckEquals(2, SUT.RemoveFirst);
  CheckTrue(SUT.AddLast(5));
end;

{$ENDREGION}


{$REGION 'TTestQueueOfTBytes'}

procedure TTestQueueOfTBytes.SetUp;
begin
  SUT := TCollections.CreateQueue<TBytes>;
end;

procedure TTestQueueOfTBytes.TearDown;
begin
  SUT := nil;
end;

procedure TTestQueueOfTBytes.TestQueueEnqueue;
var
  b: TBytes;
begin
  b := TBytes.Create(0);
  SUT.Enqueue(b);
  CheckEquals(1, SUT.Count);
  Check(b = SUT.Peek);
end;

{$ENDREGION}


{$REGION 'TTestQueueOfIntegerChangedEvent'}

procedure TTestQueueOfIntegerChangedEvent.SetUp;
begin
  inherited;
  SUT := TCollections.CreateQueue<Integer>;
end;

procedure TTestQueueOfIntegerChangedEvent.TearDown;
begin
  inherited;
  SUT := nil;
  fAInvoked := False;
  fBInvoked := False;
end;

procedure TTestQueueOfIntegerChangedEvent.HandlerA(Sender: TObject;
  const Item: Integer; Action: TCollectionChangedAction);
begin
  fAItem := Item;
  fAAction := Action;
  fAInvoked := True;
end;

procedure TTestQueueOfIntegerChangedEvent.HandlerB(Sender: TObject;
  const Item: Integer; Action: TCollectionChangedAction);
begin
  fBitem := Item;
  fBAction := Action;
  fBInvoked := True;
end;

procedure TTestQueueOfIntegerChangedEvent.TestEmpty;
begin
  SUT.Enqueue(0);

  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);
end;

procedure TTestQueueOfIntegerChangedEvent.TestOneHandler;
begin
  SUT.OnChanged.Add(HandlerA);

  SUT.Enqueue(0);

  CheckTrue(fAInvoked, 'handler A not invoked');
  CheckTrue(fAAction = caAdded, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');

  CheckFalse(fBInvoked, 'handler B not registered as callback');

  SUT.Dequeue;

  CheckTrue(fAAction = caRemoved, 'different collection notifications');

  SUT.OnChanged.Remove(HandlerA);
end;

procedure TTestQueueOfIntegerChangedEvent.TestTwoHandlers;
begin
  SUT.OnChanged.Add(HandlerA);
  SUT.OnChanged.Add(HandlerB);

  SUT.Enqueue(0);

  CheckTrue(fAInvoked, 'handler A not invoked');
  CheckTrue(fAAction = caAdded, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');
  CheckTrue(fBInvoked, 'handler B not invoked');
  CheckTrue(fBAction = caAdded, 'handler B: different collection notifications');
  CheckEquals(0, fBItem, 'handler B: different item');

  SUT.Dequeue;

  CheckTrue(fAAction = caRemoved, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');
  CheckTrue(fBAction = caRemoved, 'handler B: different collection notifications');
  CheckEquals(0, fBItem, 'handler B: different item');

  SUT.OnChanged.Remove(HandlerA);
  SUT.OnChanged.Remove(HandlerB);
end;

procedure TTestQueueOfIntegerChangedEvent.TestNonGenericChangedEvent;
var
  event: IEvent;
  method: TMethod;
begin
  event := SUT.OnChanged;

  CheckTrue(event.Enabled);

  method.Code := @TTestStackOfIntegerChangedEvent.HandlerA;
  method.Data := Pointer(Self);

  event.Add(method);

  SUT.Enqueue(0);

  CheckTrue(fAInvoked, 'handler A not invoked');
  CheckTrue(fAAction = caAdded, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');
end;

{$ENDREGION}


{$REGION 'TTestDequeOfIntegerChangedEvent'}

procedure TTestDequeOfIntegerChangedEvent.SetUp;
begin
  inherited;
  SUT := TCollections.CreateDeque<Integer>;
end;

procedure TTestDequeOfIntegerChangedEvent.TearDown;
begin
  inherited;
  SUT := nil;
  fAInvoked := False;
  fBInvoked := False;
end;

procedure TTestDequeOfIntegerChangedEvent.HandlerA(Sender: TObject;
  const Item: Integer; Action: TCollectionChangedAction);
begin
  fAItem := Item;
  fAAction := Action;
  fAInvoked := True;
end;

procedure TTestDequeOfIntegerChangedEvent.HandlerB(Sender: TObject;
  const Item: Integer; Action: TCollectionChangedAction);
begin
  fBitem := Item;
  fBAction := Action;
  fBInvoked := True;
end;

procedure TTestDequeOfIntegerChangedEvent.TestEmpty;
begin
  SUT.AddFirst(0);

  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);

  SUT.AddLast(0);

  CheckFalse(fAInvoked);
  CheckFalse(fBInvoked);
end;

procedure TTestDequeOfIntegerChangedEvent.TestOneHandler;
begin
  SUT.OnChanged.Add(HandlerA);

  SUT.AddFirst(0);

  CheckTrue(fAInvoked, 'handler A not invoked');
  CheckTrue(fAAction = caAdded, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');

  CheckFalse(fBInvoked, 'handler B not registered as callback');

  SUT.RemoveFirst;

  CheckTrue(fAAction = caRemoved, 'different collection notifications');

  SUT.AddLast(0);

  CheckTrue(fAInvoked, 'handler A not invoked');
  CheckTrue(fAAction = caAdded, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');

  CheckFalse(fBInvoked, 'handler B not registered as callback');

  SUT.RemoveLast;

  CheckTrue(fAAction = caRemoved, 'different collection notifications');

  SUT.OnChanged.Remove(HandlerA);
end;

procedure TTestDequeOfIntegerChangedEvent.TestTwoHandlers;
begin
  SUT.OnChanged.Add(HandlerA);
  SUT.OnChanged.Add(HandlerB);

  SUT.AddFirst(0);

  CheckTrue(fAInvoked, 'handler A not invoked');
  CheckTrue(fAAction = caAdded, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');
  CheckTrue(fBInvoked, 'handler B not invoked');
  CheckTrue(fBAction = caAdded, 'handler B: different collection notifications');
  CheckEquals(0, fBItem, 'handler B: different item');

  SUT.RemoveFirst;

  CheckTrue(fAAction = caRemoved, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');
  CheckTrue(fBAction = caRemoved, 'handler B: different collection notifications');
  CheckEquals(0, fBItem, 'handler B: different item');

  SUT.AddLast(0);

  CheckTrue(fAInvoked, 'handler A not invoked');
  CheckTrue(fAAction = caAdded, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');
  CheckTrue(fBInvoked, 'handler B not invoked');
  CheckTrue(fBAction = caAdded, 'handler B: different collection notifications');
  CheckEquals(0, fBItem, 'handler B: different item');

  SUT.RemoveLast;

  CheckTrue(fAAction = caRemoved, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');
  CheckTrue(fBAction = caRemoved, 'handler B: different collection notifications');
  CheckEquals(0, fBItem, 'handler B: different item');

  SUT.OnChanged.Remove(HandlerA);
  SUT.OnChanged.Remove(HandlerB);
end;

procedure TTestDequeOfIntegerChangedEvent.TestNonGenericChangedEvent;
var
  event: IEvent;
  method: TMethod;
begin
  event := SUT.OnChanged;

  CheckTrue(event.Enabled);

  method.Code := @TTestStackOfIntegerChangedEvent.HandlerA;
  method.Data := Pointer(Self);

  event.Add(method);

  SUT.AddFirst(0);

  CheckTrue(fAInvoked, 'handler A not invoked');
  CheckTrue(fAAction = caAdded, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');

  SUT.AddLast(0);

  CheckTrue(fAInvoked, 'handler A not invoked');
  CheckTrue(fAAction = caAdded, 'handler A: different collection notifications');
  CheckEquals(0, fAItem, 'handler A: different item');
end;

{$ENDREGION}


{$REGION 'TTestListOfIntegerAsIEnumerable'}

procedure TTestListOfIntegerAsIEnumerable.FillList;
var
  i: Integer;
begin
  for i := 0 to MaxItems - 1 do
    InternalList.Add(i);
end;

procedure TTestListOfIntegerAsIEnumerable.SetUp;
begin
  inherited;
  InternalList := TCollections.CreateList<Integer>;
  SUT := InternalList;
end;

procedure TTestListOfIntegerAsIEnumerable.TearDown;
begin
  inherited;
  SUT := nil;
  InternalList := nil;
end;

procedure TTestListOfIntegerAsIEnumerable.TestEnumerableIsEmpty;
begin
  CheckEquals(0, SUT.Count);
  CheckFalse(SUT.Any);
end;

procedure TTestListOfIntegerAsIEnumerable.TestEnumerableLast;
begin
  FillList;
  CheckEquals(MaxItems - 1, SUT.Last);
end;

procedure TTestListOfIntegerAsIEnumerable.TestMax;
begin
  FillList;
  CheckEquals(MaxItems - 1, SUT.Max);
end;

procedure TTestListOfIntegerAsIEnumerable.TestMin;
begin
  FillList;
  CheckEquals(0, SUT.Min);
end;

procedure TTestListOfIntegerAsIEnumerable.TestSingle;
begin
  InternalList.Add(1);
  CheckEquals(1, SUT.Single);
end;

procedure TTestListOfIntegerAsIEnumerable.TestToArray;
var
  values: TArray<Integer>;
begin
  FillList;
  values := SUT.ToArray;
  CheckEquals(MaxItems, Length(values));
  CheckEquals(InternalList.First, values[0]);
  CheckEquals(InternalList.Last, values[MaxItems-1]);
  InternalList[0] := MaxItems;
  CheckNotEquals(InternalList.First, values[0]);
end;

procedure TTestListOfIntegerAsIEnumerable.TestCheckSingleRaisedExceptionWhenEmpty;
begin
  CheckException(EInvalidOperationException, procedure begin SUT.Single(function(const i: Integer): Boolean begin Result := i = 2 end) end,
    'SUT is empty, but failed to raise the EInvalidOperationException when the Single method was called');
end;

procedure TTestListOfIntegerAsIEnumerable.TestCheckSingleRaisedExceptionWhenHasMultipleItems;
begin
  FillList;
  CheckException(EInvalidOperationException, procedure begin SUT.Single end,
    'SUT has more thann one item, but failed to raise the EInvalidOperationException when the Single method was called.');
end;

procedure TTestListOfIntegerAsIEnumerable.TestContains;
begin
  FillList;
  CheckTrue(SUT.Contains(50));
  CheckFalse(SUT.Contains(MaxItems + 50));
end;

procedure TTestListOfIntegerAsIEnumerable.TestElementAt;
var
  i: Integer;
begin
  FillList;
  for i := 0 to MaxItems - 1 do
    CheckEquals(i, SUT.ElementAt(i));
end;

procedure TTestListOfIntegerAsIEnumerable.TestEnumerableFirst;
begin
  FillList;
  CheckEquals(0, SUT.First);
end;

procedure TTestListOfIntegerAsIEnumerable.TestEnumerableHasCorrectCountAfterFill;
begin
  FillList;
  CheckEquals(MaxItems, SUT.Count);
end;

{$ENDREGION}


{$REGION 'TTestLinkedList'}

procedure TTestLinkedList.CheckEvent(expectedItem: Integer;
  expectedAction: TCollectionChangedAction);
begin
  CheckEquals(expectedItem, fItem, 'expectedItem');
  CheckTrue(expectedAction = fAction, 'expectedAction');
end;

procedure TTestLinkedList.CheckCount(expectedCount: Integer);
begin
  CheckEquals(expectedCount, SUT.Count, 'expectedCount');
end;

procedure TTestLinkedList.CheckNode(node: TLinkedListNode<Integer>;
  expectedValue: Integer; expectedNext,
  expectedPrevious: TLinkedListNode<Integer>);
begin
  CheckNotNull(node, 'node');
  CheckEquals(expectedValue, node.Value, 'node.Value');
  CheckSame(SUT, node.List, 'node.List');
  CheckSame(expectedNext, node.Next, 'node.Next');
  CheckSame(expectedPrevious, node.Previous, 'node.Previous');
end;

procedure TTestLinkedList.ListChanged(Sender: TObject; const Item: Integer;
  Action: TCollectionChangedAction);
begin
  fItem := Item;
  fAction := Action;
end;

procedure TTestLinkedList.SetUp;
begin
  SUT := TLinkedList<Integer>.Create;
  SUT.OnChanged.Add(ListChanged);
  fItem := 0;
  fAction := caChanged;
end;

procedure TTestLinkedList.TearDown;
begin
  SUT := nil;
end;

procedure TTestLinkedList.TestAddFirstNode_EmptyList;
var
  node: TLinkedListNode<Integer>;
begin
  node := TLinkedListNode<Integer>.Create(1);
  SUT.AddFirst(node);

  CheckCount(1);
  CheckEvent(1, caAdded);
  CheckNode(node, 1, nil, nil);
end;

procedure TTestLinkedList.TestAddFirstNode_ListContainsTwoItems;
var
  node, nextNode: TLinkedListNode<Integer>;
begin
  nextNode := SUT.AddFirst(1);
  SUT.Add(2);

  node := TLinkedListNode<Integer>.Create(3);
  SUT.AddFirst(node);

  CheckCount(3);
  CheckEvent(3, caAdded);
  CheckNode(node, 3, nextNode, nil);
end;

procedure TTestLinkedList.TestAddFirstValue_EmptyList;
var
  node: TLinkedListNode<Integer>;
begin
  node := SUT.AddFirst(1);

  CheckCount(1);
  CheckEvent(1, caAdded);
  CheckNode(node, 1, nil, nil);
end;

procedure TTestLinkedList.TestAddFirstValue_ListContainsTwoItems;
var
  node, nextNode: TLinkedListNode<Integer>;
begin
  nextNode := SUT.AddFirst(1);
  SUT.Add(2);

  node := SUT.AddFirst(3);

  CheckCount(3);
  CheckEvent(3, caAdded);
  CheckNode(node, 3, nextNode, nil);
end;

procedure TTestLinkedList.TestAddLastNode_EmptyList;
var
  node: TLinkedListNode<Integer>;
begin
  node := TLinkedListNode<Integer>.Create(1);
  SUT.AddLast(node);

  CheckCount(1);
  CheckEvent(1, caAdded);
  CheckNode(node, 1, nil, nil);
end;

procedure TTestLinkedList.TestAddLastNode_ListContainsTwoItems;
var
  node, prevNode: TLinkedListNode<Integer>;
begin
  SUT.Add(1);
  prevNode := SUT.AddLast(2);

  node := TLinkedListNode<Integer>.Create(3);
  SUT.AddLast(node);

  CheckCount(3);
  CheckEvent(3, caAdded);
  CheckNode(node, 3, nil, prevNode);
end;

procedure TTestLinkedList.TestAddLastValue_EmptyList;
var
  node: TLinkedListNode<Integer>;
begin
  node := SUT.AddLast(1);

  CheckCount(1);
  CheckEvent(1, caAdded);
  CheckNode(node, 1, nil, nil);
end;

procedure TTestLinkedList.TestAddLastValue_ListContainsTwoItems;
var
  node, prevNode: TLinkedListNode<Integer>;
begin
  SUT.Add(1);
  prevNode := SUT.AddLast(2);

  node := SUT.AddLast(3);

  CheckCount(3);
  CheckEvent(3, caAdded);
  CheckNode(node, 3, nil, prevNode);
end;

{$ENDREGION}


{$REGION 'TTestObjectList'}

procedure TTestObjectList.SetUp;
begin
  SUT := TCollections.CreateObjectList<TPersistent>;
end;

procedure TTestObjectList.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestObjectList.TestGetRangeElementType;
begin
  SUT := TCollections.CreateObjectList<TPersistent>;
  SUT.Add(TPersistent.Create);
  CheckEquals(TPersistent, SUT.GetRange(0, 1).ElementType.TypeData.ClassType);
end;

procedure TTestObjectList.TestIndexOf;
var
  obj1, obj2: TPersistent;
begin
  obj1 := TPersistent.Create;
  obj2 := TPersistent.Create;
  SUT.Add(obj1);
  SUT.Add(obj2);
  CheckEquals(0, SUT.IndexOf(obj1));
  CheckTrue(SUT.Contains(obj1));
  CheckEquals(1, SUT.IndexOf(obj2));
  CheckTrue(SUT.Contains(obj2));
end;

procedure TTestObjectList.TestExtractAt;
var
  obj1, obj2, obj3: TPersistent;
begin
  obj1 := TPersistent.Create;
  obj2 := TPersistent.Create;
  SUT.AddRange([obj1, obj2]);
  obj3 := SUT.ExtractAt(1);
  CheckEquals(1, SUT.Count);
  CheckSame(obj2, obj3);
  obj3.Free;
end;

procedure TTestObjectList.TestExtractRange;
var
  obj1, obj2: TPersistent;
  objs: TArray<TPersistent>;
begin
  obj1 := TPersistent.Create;
  obj2 := TPersistent.Create;
  SUT.AddRange([obj1, obj2]);
  objs := SUT.ExtractRange(0, 2);
  objs[0].Free;
  objs[1].Free;
  Pass;
end;

procedure TTestObjectList.TestGetElementType;
begin
  Check(TypeInfo(TPersistent) = SUT.ElementType);
end;

procedure TTestObjectList.TestObjectListCreate;
begin
  SUT := TCollections.CreateObjectList<TPersistent>(IComparer<TPersistent>(nil));
  CheckNotNull(SUT.Comparer);
end;

procedure TTestObjectList.TestQueryInterface;
var
  list: IObjectList;
  obj: TObject;
begin
  SUT.Add(TPersistent.Create);
  SUT.Add(TPersistent.Create);
  SUT.Add(TPersistent.Create);
  list := SUT as IObjectList;
  CheckEquals(3, list.Count);
  list.Delete(1);
  CheckEquals(2, list.Count);
  list.Add(TPersistent.Create);
  CheckEquals(3, list.Count);
  for obj in list do
    CheckIs(obj, TPersistent);
  CheckTrue(list.ElementType = TPersistent.ClassInfo);
end;


{$ENDREGION}


{$REGION 'TTestInterfaceList'}

type
  TInvokable = class(TInterfacedObject, IInvokable);

procedure TTestInterfaceList.FillList;
var
  i: Integer;
begin
  for i := 1 to 4 do
    SUT.Add(IInvokable(TInvokable.Create));
end;

procedure TTestInterfaceList.SetUp;
begin
  SUT := TCollections.CreateList<IInvokable>;
end;

procedure TTestInterfaceList.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestInterfaceList.TestCopyTo;
var
  values: TArray<IInvokable>;
  i: Integer;
begin
  for i := 0 to MaxItems - 1 do
    SUT.Add(IInvokable(TInvokable.Create));
  SetLength(values, MaxItems);
  SUT.CopyTo(values, 0);
  CheckEquals(MaxItems, Length(values));
  CheckSame(SUT.First, values[0]);
  CheckSame(SUT.Last, values[MaxItems-1]);
end;

procedure TTestInterfaceList.TestDelete;
begin
  FillList;
  SUT.Delete(3);
  SUT.Delete(1);
  Pass;
end;

procedure TTestInterfaceList.TestDeleteRangeFront;
var
  i: Integer;
begin
  FillList;
  for i := 1 to 2 do
    SUT.DeleteRange(0, 2);
  for i := 1 to 4 do
    SUT.Add(IInvokable(TInvokable.Create));
  SUT.Clear;
  Pass;
end;

procedure TTestInterfaceList.TestGetElementType;
begin
  Check(TypeInfo(IInvokable) = SUT.ElementType);
end;

procedure TTestInterfaceList.TestInterfaceListCreate;
begin
  SUT := TCollections.CreateList<IInvokable>;
  CheckNotNull(SUT.Comparer);
end;

{$ENDREGION}


{$REGION 'TTestCollectionList'}

procedure TTestCollectionList.SetUp;
begin
  Coll := TCollection.Create(TMyCollectionItem);
  SUT := Coll.AsList;
end;

procedure TTestCollectionList.TearDown;
begin
  SUT := nil;
  Coll.Free;
end;

procedure TTestCollectionList.TestAdd;
begin
  SUT.Add(TMyCollectionItem.Create(nil));
  TMyCollectionItem.Create(Coll);
  CheckEquals(2, SUT.Count);
  CheckException(Exception,
    procedure
    var
      item: TCollectionItem;
    begin
      item := TMyOtherCollectionItem.Create(nil);
      try
        SUT.Add(item);
      except
        item.Free;
        raise;
      end;
    end);
end;

procedure TTestCollectionList.TestDelete;
var
  item: TMyCollectionItem;
begin
  TMyCollectionItem.Create(Coll);
  item := TMyCollectionItem.Create(Coll);
  CheckEquals(2, SUT.Count);
  SUT.Delete(0);
  CheckEquals(1, SUT.Count);
  CheckSame(item, SUT[0]);
end;

procedure TTestCollectionList.TestDeleteRange;
var
  item: TMyCollectionItem;
begin
  item := TMyCollectionItem.Create(Coll);
  TMyCollectionItem.Create(Coll);
  TMyCollectionItem.Create(Coll);
  CheckEquals(3, SUT.Count);
  SUT.DeleteRange(1, 2);
  CheckEquals(1, SUT.Count);
  CheckSame(item, SUT[0]);
end;

procedure TTestCollectionList.TestElementType;
var
  list: IList<TMyCollectionItem>;
begin
  list := Coll.AsList<TMyCollectionItem>;
  CheckTrue(SUT.ElementType = TMyCollectionItem.ClassInfo);
  CheckTrue(list.ElementType = TMyCollectionItem.ClassInfo);
  CheckException(EArgumentException,
    procedure
    begin
      Coll.AsList<TMyOtherCollectionItem>;
    end);
end;

procedure TTestCollectionList.TestEnumeratorMoveNext_VersionMismatch;
var
  e: IEnumerator<TCollectionItem>;
begin
  TMyCollectionItem.Create(Coll);
  TMyCollectionItem.Create(Coll);
  TMyCollectionItem.Create(Coll);
  ExpectedException := EInvalidOperationException;
  e := SUT.GetEnumerator;
  while e.MoveNext do
    SUT.Add(TMyCollectionItem.Create(nil));
  ExpectedException := nil;
end;

procedure TTestCollectionList.TestExchange;
var
  item1, item2: TMyCollectionItem;
begin
  item1 := TMyCollectionItem.Create(Coll);
  TMyCollectionItem.Create(Coll);
  item2 := TMyCollectionItem.Create(Coll);
  SUT.Exchange(0, 2);
  CheckSame(item1, SUT[2]);
  CheckSame(item2, SUT[0]);
end;

procedure TTestCollectionList.TestExtract;
var
  item: TMyCollectionItem;
begin
  TMyCollectionItem.Create(Coll);
  item := TMyCollectionItem.Create(Coll);
  CheckEquals(2, SUT.Count);
  SUT.Extract(item);
  CheckEquals(1, SUT.Count);
  CheckNull(item.Collection);
  item.Free;
end;

procedure TTestCollectionList.TestExtract_ItemNotInList;
var
  item1: TMyCollectionItem;
  item2: TCollectionItem;
begin
  item1 := TMyCollectionItem.Create(nil);
  item2 := SUT.Extract(item1);
  CheckNull(item2);
  item1.Free;
end;

procedure TTestCollectionList.TestMove;
var
  item1, item2, item3: TMyCollectionItem;
begin
  item1 := TMyCollectionItem.Create(Coll);
  item2 := TMyCollectionItem.Create(Coll);
  item3 := TMyCollectionItem.Create(Coll);
  SUT.Move(0, 2);
  CheckSame(item1, SUT[2]);
  CheckSame(item2, SUT[0]);
  CheckSame(item3, SUT[1]);
end;

{$ENDREGION}


{$REGION 'TTestEnumerable'}

procedure TTestEnumerable.TestAggregate;
var
  sentence, reversed: string;
  words: IEnumerable<string>;
begin
  sentence := 'the quick brown fox jumps over the lazy dog';
  words := TEnumerable.From<string>(TArray<string>(SplitString(sentence, ' ')));
  reversed := words.Aggregate(
    function(const workingSentence, next: string): string
    begin
      Result := next + ' ' + workingSentence;
    end);
  CheckEquals('dog lazy the over jumps fox brown quick the', reversed);
end;

procedure TTestEnumerable.TestContains_Nil;
var
  sut: IEnumerable<TObject>;
begin
  sut := TEnumerable.Repeated<TObject>(nil, 1);
  CheckTrue(sut.Contains(nil));
end;

procedure TTestEnumerable.TestToArray;
var
  sut: IEnumerable<Integer>;
  values: TArray<Integer>;
  i: Integer;
begin
  sut := TEnumerable.Range(0, MaxItems);
  values := sut.ToArray;
  CheckEquals(MaxItems, Length(values));
  for i in sut do
    CheckEquals(i, values[i]);
end;

procedure TTestEnumerable.TestTryMethodsReturnDefaultWhenFalse;
var
  i: Integer;
  SUT: IEnumerable<Integer>;
begin
  SUT := TEnumerable.Empty<Integer>;

  i := -1;
  CheckFalse(SUT.TryGetFirst(i));
  CheckEquals(0, i);

  i := -1;
  CheckFalse(SUT.TryGetLast(i));
  CheckEquals(0, i);

  i := -1;
  CheckFalse(SUT.TryGetSingle(i));
  CheckEquals(0, i);

  SUT := TEnumerable.From<Integer>([1, 2, 3]);

  i := -1;
  CheckFalse(SUT.TryGetFirst(i, function(const n: Integer): Boolean begin Result := n > 3 end));
  CheckEquals(0, i);

  i := -1;
  CheckFalse(SUT.TryGetLast(i, function(const n: Integer): Boolean begin Result := n > 3 end));
  CheckEquals(0, i);

  i := -1;
  CheckFalse(SUT.TryGetSingle(i, function(const n: Integer): Boolean begin Result := n > 0 end));
  CheckEquals(0, i);
end;

{$ENDREGION}


{$REGION 'TTestMultiMapBase'}

procedure TTestMultiMapBase.FillData;
begin
  SUT.Add(3, 4);
  SUT.Add(3, 1);
  SUT.Add(1, 2);
  SUT.Add(1, 7);
  SUT.Add(4, 6);
  SUT.Add(4, 3);
  SUT.Add(2, 8);
  SUT.Add(2, 5);
end;

procedure TTestMultiMapBase.SetUp;
begin
  ValueAddedCount := 0;
  ValueRemovedCount := 0;
  ValueExtractedCount := 0;
end;

procedure TTestMultiMapBase.TearDown;
begin
  SUT := nil;
end;

procedure TTestMultiMapBase.TestAddPair;
begin
  (SUT as ICollection<TPair<Integer, Integer>>).Add(TPair<Integer, Integer>.Create(1,1));
  CheckEquals(1, SUT.Count);
  CheckEquals(1, SUT[1].First);
end;

procedure TTestMultiMapBase.TestAddStringPair;
var
  map: IMultiMap<string,TPair<string, string>>;
  pair: TPair<string, string>;
begin
  map := TCollections.CreateMultiMap<string, TPair<string, string>>;
  pair.Key := 'Hello';
  pair.Value := 'World';
  map.Add('Test', pair);
  CheckEquals(1, map.Count);
end;

procedure TTestMultiMapBase.TestContains;
begin
  SUT.Add(1, 1);
  SUT.Add(1, 2);
  SUT.Add(2, 3);

  CheckTrue(SUT.ContainsKey(1));
  CheckFalse(SUT.ContainsKey(3));
  CheckTrue(SUT.Contains(1, 1));
  CheckFalse(SUT.Contains(1, 3));
end;

procedure TTestMultiMapBase.TestEnumerator;
var
  enumerator: IEnumerator<TPair<Integer,Integer>>;
begin
  SUT.Add(1, 1);
  SUT.Add(1, 2);
  SUT.Add(2, 3);

  enumerator := SUT.GetEnumerator;
  CheckTrue(enumerator.MoveNext);
  CheckEquals(1, enumerator.Current.Key);
  CheckEquals(1, enumerator.Current.Value);
  CheckTrue(enumerator.MoveNext);
  CheckEquals(1, enumerator.Current.Key);
  CheckEquals(2, enumerator.Current.Value);
  CheckTrue(enumerator.MoveNext);
  CheckEquals(2, enumerator.Current.Key);
  CheckEquals(3, enumerator.Current.Value);
  CheckFalse(enumerator.MoveNext);
  CheckFalse(enumerator.MoveNext);
end;

procedure TTestMultiMapBase.TestExtractKeyValue;
begin
  SUT.Add(1, 1);
  SUT.Add(1, 2);
  SUT.Add(1, 3);

  CheckEquals(2, SUT.Extract(1, 2).Value);
  CheckEquals(2, SUT.Count);
end;

procedure TTestMultiMapBase.TestExtractValues;
var
  map: IMultiMap<Integer, TObject>;
  values: IReadOnlyCollection<TObject>;
  extractedValues, emptyValues: ICollection<TObject>;
  obj: TObject;
begin
  map := TCollections.CreateMultiMap<Integer, TObject>([doOwnsValues]);
  map.OnValueChanged.Add(ValueChangedObj);
  map.Add(1, TObject.Create);
  values := map[1];
  CheckEquals(1, ValueAddedCount);
  extractedValues := map.Extract(1);
  CheckEquals(1, ValueExtractedCount);
  CheckEquals(0, map.Count);
  CheckEquals(1, extractedValues.Count);
  CheckEquals(0, values.Count);
  emptyValues := map.Extract(2);
  CheckEquals(0, emptyValues.Count);
  map := nil;
  obj := extractedValues.First;
  extractedValues := nil;
  obj.Free;
end;

procedure TTestMultiMapBase.TestInternalEventHandlersDetached;
var
  items: IReadOnlyCollection<Integer>;
begin
  SUT.Add(1, 1);
  items := SUT[1];
  CheckEquals(1, items.Count);
  SUT := nil;
  CheckEquals(0, items.Count);
  // this raised an AV under LEAKCHECK when the internal change handlers
  // of the multimap where not detached from the value lists upon their removal
  items := nil;
end;

procedure TTestMultiMapBase.TestKeys;
begin
  FillData;

  CheckTrue(SUT.Keys.EqualsTo([3, 1, 4, 2]));
end;

procedure TTestMultiMapBase.TestValueChangedCalledProperly;
begin
  SUT.OnValueChanged.Add(ValueChanged);
  SUT.Add(1, 1);
  SUT.Add(1, 2);
  CheckEquals(2, ValueAddedCount);
  SUT.Remove(1, 1);
  CheckEquals(1, ValueRemovedCount);

  SUT.Add(1, 3);
  CheckEquals(3, ValueAddedCount);
  SUT := nil;
  CheckEquals(3, ValueRemovedCount);
end;

procedure TTestMultiMapBase.TestValues;
begin
  FillData;

  CheckTrue(SUT.Values.EqualsTo([4, 1, 2, 7, 6, 3, 8, 5]));
end;

procedure TTestMultiMapBase.TestValuesOrdered;
begin
  FillData;

  CheckTrue(SUT.Values.Ordered.EqualsTo([1, 2, 3, 4, 5, 6, 7, 8]));
end;

procedure TTestMultiMapBase.TestValuesToArray;
var
  values: TArray<Integer>;
begin
  FillData;

  values := SUT.Values.ToArray;
  CheckEquals(4, values[0]);
  CheckEquals(1, values[1]);
  CheckEquals(2, values[2]);
  CheckEquals(7, values[3]);
  CheckEquals(6, values[4]);
  CheckEquals(3, values[5]);
  CheckEquals(8, values[6]);
  CheckEquals(5, values[7]);
end;

procedure TTestMultiMapBase.ValueChanged(Sender: TObject; const Item: Integer;
  Action: TCollectionChangedAction);
begin
  case Action of
    caAdded: Inc(ValueAddedCount);
    caRemoved: Inc(ValueRemovedCount);
  end;
end;

procedure TTestMultiMapBase.ValueChangedObj(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
begin
  case Action of
    caAdded: Inc(ValueAddedCount);
    caRemoved: Inc(ValueRemovedCount);
    caExtracted: Inc(ValueExtractedCount);
  end;
end;

procedure TTestMultiMapBase.WrappedCollection;
var
  values: IReadOnlyCollection<Integer>;
  pair: TPair<Integer,Integer>;
begin
  SUT.Add(1, 1);
  values := SUT[1];
  CheckTrue(values.Contains(1));
  Check(values.EqualsTo([1]));
  SUT.Remove(1);
  CheckFalse(values.Contains(1));
  Check(values.EqualsTo([]));
  SUT.Add(1, 1);
  CheckTrue(values.Contains(1));
  Check(values.EqualsTo([1]));
  SUT.Clear;

  values := SUT[1];
  Check(values.EqualsTo([]));
  SUT.Add(1, 1);
  Check(values.EqualsTo([1]));
  SUT.Add(1, 2);
  Check(values.EqualsTo([1, 2]));
  SUT.Remove(1, 2);
  Check(values.EqualsTo([1]));
  SUT.Remove(1);
  SUT.Add(1, 1);
  SUT.Add(1, 2);
  Check(values.EqualsTo([1, 2]));
  SUT.Extract(1);
  Check(values.EqualsTo([]));
  SUT.Add(1, 2);
  Check(values.EqualsTo([2]));
  pair := SUT.Extract(1, 2);
  CheckEquals(1, pair.Key);
  CheckEquals(2, pair.Value);
  SUT.Add(1, 1);

  SUT.Clear;
  Check(values.EqualsTo([]));
  SUT.Add(1, 1);
  Check(values.EqualsTo([1]));

  SUT := nil;
  CheckEquals(0, values.Count);
end;

procedure TTestMultiMapBase.WrappedCollectionEnumerator;
var
  values: IReadOnlyCollection<Integer>;
  e: IEnumerator<Integer>;
begin
  values := SUT[1];
  SUT.Add(1, 1);

  CheckException(EInvalidOperationException,
    procedure
    var
      i: Integer;
    begin
      for i in SUT[1] do
      begin
        SUT.Remove(1);
        SUT.Add(1, i);
      end;
    end);

  e := values.GetEnumerator;
  CheckTrue(e.MoveNext);
  SUT.Add(1, 2);
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);
  e := values.GetEnumerator;
  SUT.Remove(1);
  CheckException(EInvalidOperationException, procedure begin e.MoveNext end);
  e := values.GetEnumerator;
  CheckFalse(e.MoveNext);
  SUT := nil;
  CheckFalse(e.MoveNext);
end;

{$ENDREGION}


{$REGION 'TTestListMultiMap'}

procedure TTestListMultiMap.SetUp;
begin
  inherited;
  SUT := TCollections.CreateMultiMap<Integer,Integer>;
end;

{$ENDREGION}


{$REGION 'TTestSetMultiMapBase'}

procedure TTestSetMultiMapBase.AddDuplicates;
begin
  SUT.Add(1, 1);
  SUT.Add(1, 2);
  CheckEquals(2, SUT.Count);
  CheckEquals(2, SUT[1].Count);
  CheckFalse(SUT.Add(1, 1));
  CheckEquals(2, SUT.Count);
  CheckEquals(2, SUT[1].Count);

  CheckFalse(SUT.TryAdd(1, 1));
end;

{$ENDREGION}


{$REGION 'TTestHashMultiMap'}

procedure TTestHashMultiMap.SetUp;
begin
  inherited;
  SUT := TCollections.CreateHashMultiMap<Integer,Integer>;
end;

{$ENDREGION}


{$REGION 'TTestTreeMultiMap'}

procedure TTestTreeMultiMap.SetUp;
begin
  inherited;
  SUT := TCollections.CreateTreeMultiMap<Integer,Integer>;
end;

procedure TTestTreeMultiMap.TestValues;
begin
  FillData;

  // items in each value collection are sorted due to them being rbtrees
  CheckTrue(SUT.Values.EqualsTo([1, 4, 2, 7, 3, 6, 5, 8]));
end;

procedure TTestTreeMultiMap.TestValuesToArray;
var
  values: TArray<Integer>;
begin
  FillData;

  // items in each value collection are sorted due to them being rbtrees
  values := SUT.Values.ToArray;
  CheckEquals(1, values[0]);
  CheckEquals(4, values[1]);
  CheckEquals(2, values[2]);
  CheckEquals(7, values[3]);
  CheckEquals(3, values[4]);
  CheckEquals(6, values[5]);
  CheckEquals(5, values[6]);
  CheckEquals(8, values[7]);
end;

{$ENDREGION}


{$REGION 'TTestSortedMultiMapBase'}

procedure TTestSortedMultiMapBase.TestKeys;
begin
  FillData;

  CheckTrue(SUT.Keys.EqualsTo([1, 2, 3, 4]));
end;

procedure TTestSortedMultiMapBase.TestValues;
begin
  FillData;

  CheckTrue(SUT.Values.EqualsTo([2, 7, 8, 5, 4, 1, 6, 3]));
end;

procedure TTestSortedMultiMapBase.TestValuesToArray;
var
  values: TArray<Integer>;
begin
  FillData;

  values := SUT.Values.ToArray;
  CheckEquals(2, values[0]);
  CheckEquals(7, values[1]);
  CheckEquals(8, values[2]);
  CheckEquals(5, values[3]);
  CheckEquals(4, values[4]);
  CheckEquals(1, values[5]);
  CheckEquals(6, values[6]);
  CheckEquals(3, values[7]);
end;

{$ENDREGION}


{$REGION 'TTestSortedListMultiMap'}

procedure TTestSortedListMultiMap.SetUp;
begin
  inherited;
  SUT := TCollections.CreateSortedMultiMap<Integer,Integer>;
end;

{$ENDREGION}


{$REGION 'TTestSortedSetMultiMapBase'}

procedure TTestSortedSetMultiMapBase.AddDuplicates;
begin
  SUT.Add(1, 1);
  SUT.Add(1, 2);
  CheckEquals(2, SUT.Count);
  CheckEquals(2, SUT[1].Count);
  CheckFalse(SUT.Add(1, 1));
  CheckEquals(2, SUT.Count);
  CheckEquals(2, SUT[1].Count);

  CheckFalse(SUT.TryAdd(1, 1));
end;

{$ENDREGION}


{$REGION 'TTestSortedHashMultiMap'}

procedure TTestSortedHashMultiMap.SetUp;
begin
  inherited;
  SUT := TCollections.CreateSortedHashMultiMap<Integer,Integer>;
end;

{$ENDREGION}


{$REGION 'TTestSortedTreeMultiMap'}

procedure TTestSortedTreeMultiMap.SetUp;
begin
  inherited;
  SUT := TCollections.CreateSortedTreeMultiMap<Integer,Integer>;
end;

procedure TTestSortedTreeMultiMap.TestValues;
begin
  FillData;

  // items in each value collection are sorted due to them being rbtrees
  CheckTrue(SUT.Values.EqualsTo([2, 7, 5, 8, 1, 4, 3, 6]));
end;

procedure TTestSortedTreeMultiMap.TestValuesToArray;
var
  values: TArray<Integer>;
begin
  FillData;

  // items in each value collection are sorted due to them being rbtrees
  values := SUT.Values.ToArray;
  CheckEquals(2, values[0]);
  CheckEquals(7, values[1]);
  CheckEquals(5, values[2]);
  CheckEquals(8, values[3]);
  CheckEquals(1, values[4]);
  CheckEquals(4, values[5]);
  CheckEquals(3, values[6]);
  CheckEquals(6, values[7]);
end;

{$ENDREGION}


{$REGION 'TTestObjectStack'}

procedure TTestObjectStack.ExtractDoesNotDestroysItemButReturnsIt;
var
  obj1, obj2, obj: TObject;
begin
  obj1 := TObject.Create;
  obj2 := TObject.Create;

  // stack -> LIFO
  SUT.Push(obj1);
  SUT.Push(obj2);
  CheckSame(obj2, SUT.Extract);
  CheckTrue(SUT.TryExtract(obj));
  CheckSame(obj1, obj);

  obj2.Free;
  obj1.Free;
end;

procedure TTestObjectStack.PopDestroysItemAndReturnsNil;
var
  obj: TObject;
begin
  SUT.Push(TObject.Create);
  SUT.Push(TObject.Create);
  CheckNull(SUT.Pop);
  CheckTrue(SUT.TryPop(obj));
  CheckNull(obj);
end;

procedure TTestObjectStack.SetUp;
begin
  inherited;
  SUT := TCollections.CreateStack<TObject>(True);
end;

procedure TTestObjectStack.TearDown;
begin
  SUT := nil;
  inherited;
end;

{$ENDREGION}


{$REGION 'TTestObjectQueue'}

procedure TTestObjectQueue.DequeueDestroysItemAndReturnsNil;
begin
  SUT.Enqueue(TObject.Create);
  SUT.Enqueue(TObject.Create);
  CheckNull(SUT.Dequeue);
  CheckNull(SUT.Dequeue);
end;

procedure TTestObjectQueue.ExtractDoesNotDestroysItemButReturnsIt;
var
  obj1, obj2, obj: TObject;
begin
  obj1 := TObject.Create;
  obj2 := TObject.Create;

  // queue -> FIFO
  SUT.Enqueue(obj1);
  SUT.Enqueue(obj2);
  CheckSame(obj1, SUT.Extract);
  CheckTrue(SUT.TryExtract(obj));
  CheckSame(obj2, obj);

  obj2.Free;
  obj1.Free;
end;

procedure TTestObjectQueue.SetUp;
begin
  inherited;
  SUT := TCollections.CreateQueue<TObject>(True);
end;

procedure TTestObjectQueue.TearDown;
begin
  SUT := nil;
  inherited;
end;

{$ENDREGION}


{$REGION 'TTestObjectDeque'}

procedure TTestObjectDeque.RemoveFirstDestroysItemAndReturnsNil;
begin
  SUT.AddFirst(TObject.Create);
  CheckNull(SUT.RemoveFirst);
end;

procedure TTestObjectDeque.RemoveLastDestroysItemAndReturnsNil;
begin
  SUT.AddLast(TObject.Create);
  CheckNull(SUT.RemoveLast);
end;

procedure TTestObjectDeque.ExtractFirstDoesNotDestroysItemButReturnsIt;
var
  obj1, obj2, obj: TObject;
begin
  obj1 := TObject.Create;
  obj2 := TObject.Create;

  SUT.AddLast(obj1);
  SUT.AddLast(obj2);
  CheckSame(obj1, SUT.ExtractFirst);
  CheckTrue(SUT.TryExtractFirst(obj));
  CheckSame(obj2, obj);
  CheckFalse(SUT.TryExtractFirst(obj));

  obj2.Free;
  obj1.Free;
end;

procedure TTestObjectDeque.ExtractLastDoesNotDestroysItemButReturnsIt;
var
  obj1, obj2, obj: TObject;
begin
  obj1 := TObject.Create;
  obj2 := TObject.Create;

  SUT.AddFirst(obj1);
  SUT.AddFirst(obj2);
  CheckSame(obj1, SUT.ExtractLast);
  CheckTrue(SUT.TryExtractLast(obj));
  CheckSame(obj2, obj);
  CheckFalse(SUT.TryExtractLast(obj));

  obj2.Free;
  obj1.Free;
end;

procedure TTestObjectDeque.SetUp;
begin
  inherited;
  SUT := TCollections.CreateDeque<TObject>(True);
end;

procedure TTestObjectDeque.TearDown;
begin
  SUT := nil;
  inherited;
end;

{$ENDREGION}


{$REGION 'TTestRedBlackTreeInteger'}

procedure TTestRedBlackTreeInteger.SetUp;
begin
  SUT := TRedBlackTree<Integer>.Create;
end;

procedure TTestRedBlackTreeInteger.TearDown;
begin
  SUT := nil;
end;

procedure TTestRedBlackTreeInteger.TestDelete;
var
  expected: IList<Integer>;
begin
  SUT.Add(4);
  SUT.Add(2);
  SUT.Add(6);
  SUT.Add(1);
  SUT.Add(3);
  SUT.Add(5);
  SUT.Add(7);
  expected := TCollections.CreateList<Integer>([1, 2, 3, 4, 5, 6, 7]);
  Check(expected.EqualsTo(SUT.ToArray));

  SUT.Delete(4);
  expected.Remove(4);
  Check(expected.EqualsTo(SUT.ToArray));

  SUT.Delete(7);
  expected.Remove(7);
  Check(expected.EqualsTo(SUT.ToArray));

  SUT.Delete(2);
  expected.Remove(2);
  Check(expected.EqualsTo(SUT.ToArray));

  SUT.Delete(1);
  expected.Remove(1);
  Check(expected.EqualsTo(SUT.ToArray));

  SUT.Delete(6);
  expected.Remove(6);
  Check(expected.EqualsTo(SUT.ToArray));

  SUT.Delete(3);
  expected.Remove(3);
  Check(expected.EqualsTo(SUT.ToArray));

  SUT.Delete(5);
  expected.Remove(5);
  Check(expected.EqualsTo(SUT.ToArray));
end;

procedure TTestRedBlackTreeInteger.TestDuplicates;
begin
  CheckTrue(SUT.Add(1));
  CheckFalse(SUT.Add(1));
  CheckTrue(SUT.Add(2));
  CheckTrue(SUT.Add(3));
  CheckFalse(SUT.Add(1));
  CheckEquals(3, SUT.Count);
end;

procedure TTestRedBlackTreeInteger.TestInsert;
var
  arr: TArray<Integer>;
begin
  SUT.Add(2);
  SUT.Add(1);
  SUT.Add(3);
  SUT.Add(4);
  SUT.Add(5);
  arr := SUT.ToArray;
  CheckEquals(5, Length(arr));
  CheckEquals(1, arr[0]);
  CheckEquals(2, arr[1]);
  CheckEquals(3, arr[2]);
  CheckEquals(4, arr[3]);
  CheckEquals(5, arr[4]);

  SUT.Clear;
  SUT.Add(26);
  SUT.Add(56);
  SUT.Add(34);
  SUT.Add(98);
  SUT.Add(21);
  SUT.Add(14);
  SUT.Add(28);
  SUT.Add(92);
  SUT.Add(12);
  SUT.Add(45);
  arr := SUT.ToArray;
  CheckEquals(10, Length(arr));
  CheckEquals(12, arr[0]);
  CheckEquals(14, arr[1]);
  CheckEquals(21, arr[2]);
  CheckEquals(26, arr[3]);
  CheckEquals(28, arr[4]);
  CheckEquals(34, arr[5]);
  CheckEquals(45, arr[6]);
  CheckEquals(56, arr[7]);
  CheckEquals(92, arr[8]);
  CheckEquals(98, arr[9]);
end;

procedure TTestRedBlackTreeInteger.FuzzyTesting;

  function ArrayToString(const values: TArray<Integer>): string;
  var
    i: Integer;
  begin
    Result := '[';
    for i := 0 to Length(values) - 1 do
    begin
      if i > 0 then
        Result := Result + ', ';
      Result := Result + IntToStr(values[i]);
    end;
    Result := Result + ']';
  end;

  procedure Test(const input: TArray<Integer>);
  var
    i: Integer;
    inputSortedArray: TArray<Integer>;
    inputSorted: ISet<Integer>;
    inputString: string;
  begin
    inputString := ArrayToString(input);

    for i in input do
      SUT.Add(i);
    CheckEquals(Length(input), SUT.Count, inputString);

    inputSortedArray := Copy(input);
    TArray.Sort<Integer>(inputSortedArray);
    inputSorted := TCollections.CreateSet<Integer>(inputSortedArray);
    Check(inputSorted.EqualsTo(SUT.ToArray));

    for i := inputSorted.Min - 10 to inputSorted.Max + 10 do
      CheckEquals(inputSorted.Contains(i), SUT.Exists(i));

    for i := 0 to Length(input) - 1 do
    begin
      CheckTrue(SUT.Delete(input[i]));

      inputSortedArray := Copy(input, i + 1);
      TArray.Sort<Integer>(inputSortedArray);
      inputSorted := TCollections.CreateSet<Integer>(inputSortedArray);
      Check(inputSorted.EqualsTo(SUT.ToArray));
    end;
  end;

const
  COUNT = 1000;
  MAX_INPUT_LENGHT = 100;
  MAX_VALUE = 1000;
var
  i, n: Integer;
  input: TArray<Integer>;
  inputLen: Integer;
begin
  for n := 1 to COUNT do
  begin
    SUT := TRedBlackTree<Integer>.Create;
    inputLen := Random(MAX_INPUT_LENGHT) + 1;
    SetLength(input, inputLen);
    for i := 0 to inputLen - 1 do
      input[i] := Random(MAX_VALUE) + 1;
    input := TCollections.CreateSet<Integer>(input).ToArray;
    Test(input);
  end;
end;

{$ENDREGION}


{$REGION 'TTestRedBlackTreeIntegerString'}

procedure TTestRedBlackTreeIntegerString.SetUp;
begin
  SUT := TRedBlackTree<Integer, string>.Create;
end;

procedure TTestRedBlackTreeIntegerString.TearDown;
begin
  SUT := nil;
end;

function TTestRedBlackTreeIntegerString.Add(key: Integer): Boolean;
begin
  Result := SUT.Add(key, IntToStr(key));
end;

procedure TTestRedBlackTreeIntegerString.CheckKeyValuePair(expectedKey: Integer; const pair: TKeyValuePair; const msg: string);
begin
  CheckEquals(expectedKey, pair.Key, msg);
  CheckEquals(expectedKey, StrToInt(pair.Value), msg);
end;

procedure TTestRedBlackTreeIntegerString.CheckKeyValuePairs(const expectedKeys: IEnumerable<Integer>; const msg: string);
var
  i: Integer;
  pairs: TArray<TKeyValuePair>;
begin
  pairs := SUT.ToArray;
  CheckEquals(expectedKeys.Count, Length(pairs), msg);
  for i := 0 to expectedKeys.Count - 1 do
    CheckKeyValuePair(expectedKeys.ElementAt(i), pairs[i], msg);
end;

procedure TTestRedBlackTreeIntegerString.TestDelete;
var
  expected: IList<Integer>;
begin
  Add(4);
  Add(2);
  Add(6);
  Add(1);
  Add(3);
  Add(5);
  Add(7);

  expected := TCollections.CreateList<Integer>([1, 2, 3, 4, 5, 6, 7]);
  CheckKeyValuePairs(expected);

  SUT.Delete(4);
  expected.Remove(4);
  CheckKeyValuePairs(expected);

  SUT.Delete(7);
  expected.Remove(7);
  CheckKeyValuePairs(expected);

  SUT.Delete(2);
  expected.Remove(2);
  CheckKeyValuePairs(expected);

  SUT.Delete(1);
  expected.Remove(1);
  CheckKeyValuePairs(expected);

  SUT.Delete(6);
  expected.Remove(6);
  CheckKeyValuePairs(expected);

  SUT.Delete(3);
  expected.Remove(3);
  CheckKeyValuePairs(expected);

  SUT.Delete(5);
  expected.Remove(5);
  CheckKeyValuePairs(expected);
end;

procedure TTestRedBlackTreeIntegerString.TestDuplicates;
begin
  CheckTrue(Add(1));
  CheckFalse(Add(1));
  CheckTrue(Add(2));
  CheckTrue(Add(3));
  CheckFalse(Add(1));
  CheckEquals(3, SUT.Count);
end;

procedure TTestRedBlackTreeIntegerString.TestInsert;
var
  arr: TArray<TKeyValuePair>;
begin
  Add(2);
  Add(1);
  Add(3);
  Add(4);
  Add(5);
  arr := SUT.ToArray;
  CheckEquals(5, Length(arr));
  CheckKeyValuePair(1, arr[0]);
  CheckKeyValuePair(2, arr[1]);
  CheckKeyValuePair(3, arr[2]);
  CheckKeyValuePair(4, arr[3]);
  CheckKeyValuePair(5, arr[4]);

  SUT.Clear;
  Add(26);
  Add(56);
  Add(34);
  Add(98);
  Add(21);
  Add(14);
  Add(28);
  Add(92);
  Add(12);
  Add(45);
  arr := SUT.ToArray;
  CheckEquals(10, Length(arr));
  CheckKeyValuePair(12, arr[0]);
  CheckKeyValuePair(14, arr[1]);
  CheckKeyValuePair(21, arr[2]);
  CheckKeyValuePair(26, arr[3]);
  CheckKeyValuePair(28, arr[4]);
  CheckKeyValuePair(34, arr[5]);
  CheckKeyValuePair(45, arr[6]);
  CheckKeyValuePair(56, arr[7]);
  CheckKeyValuePair(92, arr[8]);
  CheckKeyValuePair(98, arr[9]);
end;

procedure TTestRedBlackTreeIntegerString.FuzzyTesting;

  function ArrayToString(const values: TArray<Integer>): string;
  var
    i: Integer;
  begin
    Result := '[';
    for i := 0 to Length(values) - 1 do
    begin
      if i > 0 then
        Result := Result + ', ';
      Result := Result + IntToStr(values[i]);
    end;
    Result := Result + ']';
  end;

  procedure Test(const input: TArray<Integer>);
  var
    i: Integer;
    inputSortedArray: TArray<Integer>;
    inputSorted: ISet<Integer>;
    inputString: string;
  begin
    inputString := ArrayToString(input);

    for i in input do
      Add(i);
    CheckEquals(Length(input), SUT.Count, inputString);

    inputSortedArray := Copy(input);
    TArray.Sort<Integer>(inputSortedArray);
    inputSorted := TCollections.CreateSet<Integer>(inputSortedArray);
    CheckKeyValuePairs(inputSorted, inputString);

    for i := inputSorted.Min - 10 to inputSorted.Max + 10 do
    begin
      CheckEquals(inputSorted.Contains(i), SUT.Exists(i));
    end;

    for i := 0 to Length(input) - 1 do
    begin
      CheckTrue(SUT.Delete(input[i]));

      inputSortedArray := Copy(input, i + 1);
      TArray.Sort<Integer>(inputSortedArray);
      inputSorted := TCollections.CreateSet<Integer>(inputSortedArray);
      CheckKeyValuePairs(inputSorted, inputString);
    end;
  end;

const
  COUNT = 1000;
  MAX_INPUT_LENGHT = 100;
  MAX_VALUE = 1000;
var
  i, n: Integer;
  input: TArray<Integer>;
  inputLen: Integer;
begin
  for n := 1 to COUNT do
  begin
    SUT := TRedBlackTree<Integer, string>.Create;
    inputLen := Random(MAX_INPUT_LENGHT) + 1;
    SetLength(input, inputLen);
    for i := 0 to inputLen - 1 do
      input[i] := Random(MAX_VALUE) + 1;
    input := TCollections.CreateSet<Integer>(input).ToArray;
    Test(input);
  end;
end;

{$ENDREGION}


{$REGION 'TTestSet'}

procedure TTestSet.SetUp;
begin
  SUT := TCollections.CreateSet<string>;
end;

procedure TTestSet.TearDown;
begin
  SUT := nil;
end;

procedure TTestSet.CheckCount(expected: Integer);
begin
  CheckEquals(expected, SUT.Count, 'Count');
end;

procedure TTestSet.TestAdd;
begin
  SUT.Add('c');
  SUT.Add('a');
  SUT.Add('b');
  SUT.Add('d');

  CheckCount(4);
end;

procedure TTestSet.TestRemove;
begin
  SUT.Add('c');
  SUT.Add('a');
  SUT.Add('b');
  SUT.Add('d');
  SUT.Remove('b');

  CheckCount(3);
end;

procedure TTestSet.TestExtract;
begin
  SUT.Add('c');
  SUT.Add('a');
  SUT.Add('b');
  SUT.Add('d');
  CheckEquals('b', SUT.Extract('b'));

  CheckCount(3);
end;

procedure TTestSet.TestEnumerate;
var
  values: TArray<string>;
  i: Integer;
  s: string;
begin
  SUT.AddRange(['a', 'c', 'b']);
  values := SUT.ToArray;
  i := 0;
  for s in SUT do
  begin
    CheckEquals(values[i], s);
    Inc(i);
  end;
  CheckEquals(3, i);
end;

procedure TTestSet.TestEnumeratorKeepsSourceAlive;
var
  e: IEnumerator<string>;
begin
  SUT.Add('a');
  e := SUT.GetEnumerator;
  SUT := nil;
  CheckTrue(e.MoveNext);
end;

procedure TTestSet.TestEnumeratorMoveNext_VersionMismatch;
var
  e: IEnumerator<string>;
begin
  e := SUT.GetEnumerator;

  ExpectedException := EInvalidOperationException;
  SUT.Add('a');
  e.MoveNext;
  ExpectedException := nil;
end;

{$ENDREGION}


{$REGION 'TTestSortedSet'}

procedure TTestSortedSet.SetUp;
begin
  SUT := TCollections.CreateSortedSet<string>;
end;

procedure TTestSortedSet.TestToArray;
var
  values: TArray<string>;
begin
  SUT.Add('c');
  SUT.Add('a');
  SUT.Add('b');
  SUT.Add('d');

  values := SUT.ToArray;

  CheckEquals(4, Length(values));
  CheckEquals('a', values[0]);
  CheckEquals('b', values[1]);
  CheckEquals('c', values[2]);
  CheckEquals('d', values[3]);
end;

{$ENDREGION}


{$REGION 'TTestMultiMapChangedEventBase'}

procedure TTestMultiMapChangedEventBase.AddEventHandlers;
begin
  SUT.OnChanged.Add(Changed);
  SUT.OnKeyChanged.Add(KeyChanged);
  SUT.OnValueChanged.Add(ValueChanged);
end;

procedure TTestMultiMapChangedEventBase.TearDown;
begin
  SUT := nil;
  inherited;
end;

procedure TTestMultiMapChangedEventBase.TestAdd;
begin
  AddEventHandlers;
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(2, 'c');

  CheckEquals(3, fChangedEvents.Count);
  CheckChanged(0, 1, 'a', caAdded);
  CheckChanged(1, 2, 'b', caAdded);
  CheckChanged(2, 2, 'c', caAdded);

  CheckEquals(2, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 1, caAdded);
  CheckKeyChanged(1, 2, caAdded);

  CheckEquals(3, fValueChangedEvents.Count);
  CheckValueChanged(0, 'a', caAdded);
  CheckValueChanged(1, 'b', caAdded);
  CheckValueChanged(2, 'c', caAdded);
end;

procedure TTestMultiMapChangedEventBase.TestClear;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(2, 'c');
  AddEventHandlers;
  SUT.Clear;

  CheckEquals(3, fChangedEvents.Count);
  CheckChanged(0, 1, 'a', caRemoved);
  CheckChanged(1, 2, 'b', caRemoved);
  CheckChanged(2, 2, 'c', caRemoved);

  CheckEquals(2, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 1, caRemoved);
  CheckKeyChanged(1, 2, caRemoved);

  CheckEquals(3, fValueChangedEvents.Count);
  CheckValueChanged(0, 'a', caRemoved);
  CheckValueChanged(1, 'b', caRemoved);
  CheckValueChanged(2, 'c', caRemoved);
end;

procedure TTestMultiMapChangedEventBase.TestDestroy;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(2, 'c');
  AddEventHandlers;
  SUT := nil;

  CheckEquals(3, fChangedEvents.Count);
  CheckChanged(0, 1, 'a', caRemoved);
  CheckChanged(1, 2, 'b', caRemoved);
  CheckChanged(2, 2, 'c', caRemoved);

  CheckEquals(2, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 1, caRemoved);
  CheckKeyChanged(1, 2, caRemoved);

  CheckEquals(3, fValueChangedEvents.Count);
  CheckValueChanged(0, 'a', caRemoved);
  CheckValueChanged(1, 'b', caRemoved);
  CheckValueChanged(2, 'c', caRemoved);
end;

procedure TTestMultiMapChangedEventBase.TestExtract;
var
  notExisting: ICollection<string>;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');
  AddEventHandlers;
  CheckEquals('c', SUT.Extract(3).First);
  CheckEquals('a', SUT.Extract(1).First);
  notExisting := SUT.Extract(1);
  CheckEquals(0, notExisting.Count);

  CheckEquals(2, fChangedEvents.Count);
  CheckChanged(0, 3, 'c', caExtracted);
  CheckChanged(1, 1, 'a', caExtracted);

  CheckEquals(2, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 3, caExtracted);
  CheckKeyChanged(1, 1, caExtracted);

  CheckEquals(2, fValueChangedEvents.Count);
  CheckValueChanged(0, 'c', caExtracted);
  CheckValueChanged(1, 'a', caExtracted);
end;

procedure TTestMultiMapChangedEventBase.TestExtractPair;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(2, 'c');
  SUT.Add(3, 'd');
  AddEventHandlers;
  SUT.Extract(2, 'e');

  CheckEquals(0, fChangedEvents.Count);
  CheckEquals(0, fKeyChangedEvents.Count);
  CheckEquals(0, fValueChangedEvents.Count);

  SUT.Extract(2, 'c');
  CheckEquals(1, fChangedEvents.Count);
  CheckChanged(0, 2, 'c', caExtracted);
  CheckEquals(0, fKeyChangedEvents.Count);
  CheckEquals(1, fValueChangedEvents.Count);
  CheckValueChanged(0, 'c', caExtracted);
  SUT.Extract(2, 'b');
  CheckEquals(2, fChangedEvents.Count);
  CheckChanged(1, 2, 'b', caExtracted);
  CheckEquals(1, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 2, caExtracted);
  CheckEquals(2, fValueChangedEvents.Count);
  CheckValueChanged(1, 'b', caExtracted);
end;

procedure TTestMultiMapChangedEventBase.TestRemove;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');
  SUT.Add(5, 'e');
  AddEventHandlers;
  Check(SUT.Remove(3));
  Check(not SUT.Remove(4, 'a'));
  Check(SUT.Remove(4, 'd'));
  Check(SUT.Remove(1));
  Check(not SUT.Remove(1));

  CheckEquals(3, fChangedEvents.Count);
  CheckChanged(0, 3, 'c', caRemoved);
  CheckChanged(1, 4, 'd', caRemoved);
  CheckChanged(2, 1, 'a', caRemoved);

  CheckEquals(3, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 3, caRemoved);
  CheckKeyChanged(1, 4, caRemoved);
  CheckKeyChanged(2, 1, caRemoved);

  CheckEquals(3, fValueChangedEvents.Count);
  CheckValueChanged(0, 'c', caRemoved);
  CheckValueChanged(1, 'd', caRemoved);
  CheckValueChanged(2, 'a', caRemoved);
end;

procedure TTestMultiMapChangedEventBase.TestRemovePair;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(2, 'c');
  SUT.Add(3, 'd');
  AddEventHandlers;
  SUT.Remove(2, 'e');

  CheckEquals(0, fChangedEvents.Count);
  CheckEquals(0, fKeyChangedEvents.Count);
  CheckEquals(0, fValueChangedEvents.Count);

  SUT.Remove(2, 'c');
  CheckEquals(1, fChangedEvents.Count);
  CheckChanged(0, 2, 'c', caRemoved);
  CheckEquals(0, fKeyChangedEvents.Count);
  CheckEquals(1, fValueChangedEvents.Count);
  CheckValueChanged(0, 'c', caRemoved);
  SUT.Remove(2, 'b');
  CheckEquals(2, fChangedEvents.Count);
  CheckChanged(1, 2, 'b', caRemoved);
  CheckEquals(1, fKeyChangedEvents.Count);
  CheckKeyChanged(0, 2, caRemoved);
  CheckEquals(2, fValueChangedEvents.Count);
  CheckValueChanged(1, 'b', caRemoved);
end;

{$ENDREGION}


{$REGION 'TTestListMultiMapChangedEvent'}

procedure TTestListMultiMapChangedEvent.SetUp;
begin
  inherited;
  SUT := TCollections.CreateMultiMap<Integer,string>;
  Sender := SUT.AsObject;
end;

{$ENDREGION}


{$REGION 'TTestHashMultiMapChangedEvent'}

procedure TTestHashMultiMapChangedEvent.SetUp;
begin
  inherited;
  SUT := TCollections.CreateHashMultiMap<Integer,string>;
  Sender := SUT.AsObject;
end;

{$ENDREGION}


{$REGION 'TTestTreeMultiMapChangedEvent'}

procedure TTestTreeMultiMapChangedEvent.SetUp;
begin
  inherited;
  SUT := TCollections.CreateTreeMultiMap<Integer,string>;
  Sender := SUT.AsObject;
end;

{$ENDREGION}

{$REGION 'TTestSortedListMultiMapChangedEvent'}

procedure TTestSortedListMultiMapChangedEvent.SetUp;
begin
  inherited;
  SUT := TCollections.CreateSortedMultiMap<Integer,string>;
  Sender := SUT.AsObject;
end;

{$ENDREGION}


{$REGION 'TTestSortedHashMultiMapChangedEvent'}

procedure TTestSortedHashMultiMapChangedEvent.SetUp;
begin
  inherited;
  SUT := TCollections.CreateSortedHashMultiMap<Integer,string>;
  Sender := SUT.AsObject;
end;

{$ENDREGION}


{$REGION 'TTestSortedTreeMultiMapChangedEvent'}

procedure TTestSortedTreeMultiMapChangedEvent.SetUp;
begin
  inherited;
  SUT := TCollections.CreateSortedTreeMultiMap<Integer,string>;
  Sender := SUT.AsObject;
end;

{$ENDREGION}


{$REGION 'TTestMultiSetBase'}

procedure TTestMultiSetBase.CheckCount(expected: Integer);
begin
  CheckEquals(expected, SUT.Count, 'Count');
end;

function TTestMultiSetBase.IsSorted: Boolean;
begin
  Result := False;
end;

procedure TTestMultiSetBase.TearDown;
begin
  SUT := nil;
  inherited;
end;

procedure TTestMultiSetBase.TestAdd;
begin
  CheckTrue(SUT.Add('a'));
  CheckCount(1);
  CheckTrue(SUT.Add('b'));
  CheckCount(2);
  CheckEquals(1, SUT.Add('a', 2));
  CheckCount(4);
  CheckEquals(3, SUT['a']);
end;

procedure TTestMultiSetBase.TestEnumerate;
var
  i: Integer;
  s: string;
  values: TArray<string>;
begin
  SUT.AddRange(['a', 'c', 'b', 'b', 'a', 'b']);
  values := SUT.ToArray;
  i := 0;
  for s in SUT do
  begin
    CheckEquals(values[i], s);
    Inc(i);
  end;
  CheckEquals(6, i);
end;

procedure TTestMultiSetBase.TestOrderedByCount;
var
  orderedByCount: IReadOnlyMultiSet<string>;
begin
  SUT.AddRange(['a', 'c', 'b', 'b', 'a', 'b']);
  orderedByCount := SUT.OrderedByCount;

  Check(orderedByCount.Items.EqualsTo(['b', 'a', 'c']));
end;

procedure TTestMultiSetBase.TestRemove;
begin
  SUT.Add('a', 3);
  SUT.Add('b');

  CheckTrue(SUT.Remove('a'));
  CheckCount(3);
  CheckEquals(2, SUT.Remove('a', 1));
  CheckCount(2);
  CheckFalse(SUT.Remove('c'));
  CheckEquals(0, SUT.Remove('c', 2));
  CheckEquals(1, SUT.Remove('b', 2));
  CheckCount(1);
end;

procedure TTestMultiSetBase.TestSetEquals;
begin
  SUT.AddRange(['b', 'c', 'a', 'b', 'a', 'b']);
  CheckTrue(SUT.SetEquals(TEnumerable.From<string>(['b', 'b', 'c', 'a', 'a', 'b'])));
  CheckFalse(SUT.SetEquals(TEnumerable.From<string>(['b', 'b', 'c', 'a', 'a', 'b', 'a'])));
  CheckFalse(SUT.SetEquals(TEnumerable.From<string>(['b', 'b', 'c', 'a', 'a', 'b', 'd'])));
  CheckFalse(SUT.SetEquals(TEnumerable.From<string>(['a', 'b', 'c'])));
  CheckFalse(SUT.SetEquals(TEnumerable.From<string>(['a', 'b', 'c'])));
end;

{$ENDREGION}


{$REGION 'TTestMultiSet'}

procedure TTestHashMultiSet.SetUp;
begin
  inherited;
  SUT := TCollections.CreateMultiSet<string>;
end;

procedure TTestHashMultiSet.TestElements;
var
  items: IReadOnlyCollection<string>;
begin
  SUT.AddRange(['b', 'c', 'a', 'b']);
  items := SUT.Items;
  CheckTrue(items.EqualsTo(['b', 'c', 'a']));
  SUT.Add('d');
  CheckTrue(items.Contains('d'));
end;

procedure TTestHashMultiSet.TestEntries;
var
  entries: TArray<TMultiSetEntry<string>>;
begin
  SUT.AddRange(['b', 'c', 'a', 'b', 'a', 'b']);
  entries := SUT.Entries.ToArray;
  CheckEquals(3, Length(entries));
  CheckEquals('b', entries[0].Item);
  CheckEquals('c', entries[1].Item);
  CheckEquals('a', entries[2].Item);
  CheckEquals(3, entries[0].Count);
  CheckEquals(1, entries[1].Count);
  CheckEquals(2, entries[2].Count);
end;

procedure TTestHashMultiSet.TestToArray;
var
  items: TArray<string>;
begin
  SUT.AddRange(['a', 'b', 'a', 'a']);

  items := SUT.ToArray;
  CheckEquals(4, Length(items));
  CheckEquals('a', items[0]);
  CheckEquals('a', items[1]);
  CheckEquals('a', items[2]);
  CheckEquals('b', items[3]);
end;

{$ENDREGION}


{$REGION 'TTestSortedMultiSet'}

function TTestTreeMultiSet.IsSorted: Boolean;
begin
  Result := True;
end;

procedure TTestTreeMultiSet.SetUp;
begin
  inherited;
  SUT := TCollections.CreateSortedMultiSet<string>;
end;

procedure TTestTreeMultiSet.TestItems;
var
  items: IReadOnlyCollection<string>;
begin
  SUT.AddRange(['b', 'c', 'a', 'b']);
  items := SUT.Items;
  CheckTrue(items.EqualsTo(['a', 'b', 'c']));

  SUT.Add('d');
  CheckTrue(items.Contains('d'));
end;

procedure TTestTreeMultiSet.TestEntries;
var
  entries: TArray<TMultiSetEntry<string>>;
begin
  SUT.AddRange(['b', 'c', 'a', 'b', 'a', 'b']);
  entries := SUT.Entries.ToArray;
  CheckEquals(3, Length(entries));
  CheckEquals('a', entries[0].Item);
  CheckEquals('b', entries[1].Item);
  CheckEquals('c', entries[2].Item);
  CheckEquals(2, entries[0].Count);
  CheckEquals(3, entries[1].Count);
  CheckEquals(1, entries[2].Count);
end;

procedure TTestTreeMultiSet.TestToArray;
var
  items: TArray<string>;
begin
  SUT.AddRange(['b', 'c', 'a', 'a']);

  items := SUT.ToArray;
  CheckEquals(4, Length(items));
  CheckEquals('a', items[0]);
  CheckEquals('a', items[1]);
  CheckEquals('b', items[2]);
  CheckEquals('c', items[3]);
end;

{$ENDREGION}


{$REGION 'TTestMultiSetChangedEventBase'}

procedure TTestMultiSetChangedEventBase.AddEventHandlers;
begin
  SUT.OnChanged.Add(Changed);
end;

procedure TTestMultiSetChangedEventBase.TearDown;
begin
  SUT := nil;
  inherited;
end;

procedure TTestMultiSetChangedEventBase.TestAdd;
begin
  AddEventHandlers;
  SUT.Add(1);
  SUT.Add(2);
  SUT.Add(3, 2);

  CheckEquals(4, fChangedEvents.Count);
  CheckChanged(0, 1, caAdded);
  CheckChanged(1, 2, caAdded);
  CheckChanged(2, 3, caAdded);
  CheckChanged(3, 3, caAdded);
end;

procedure TTestMultiSetChangedEventBase.TestClear;
begin
  SUT.Add(1);
  SUT.Add(2);
  SUT.Add(3, 2);
  AddEventHandlers;
  SUT.Clear;

  CheckEquals(4, fChangedEvents.Count);
  CheckChanged(0, 1, caRemoved);
  CheckChanged(1, 2, caRemoved);
  CheckChanged(2, 3, caRemoved);
  CheckChanged(3, 3, caRemoved);
end;

procedure TTestMultiSetChangedEventBase.TestDestroy;
begin
  SUT.Add(1);
  SUT.Add(2);
  SUT.Add(3, 2);
  AddEventHandlers;
  SUT := nil;

  CheckEquals(4, fChangedEvents.Count);
  CheckChanged(0, 1, caRemoved);
  CheckChanged(1, 2, caRemoved);
  CheckChanged(2, 3, caRemoved);
  CheckChanged(3, 3, caRemoved);
end;

procedure TTestMultiSetChangedEventBase.TestRemove;
begin
  SUT.Add(1);
  SUT.Add(2);
  SUT.Add(3, 2);
  AddEventHandlers;
  Check(SUT.Remove(3));
  Check(not SUT.Remove(4));
  Check(SUT.Remove(2));
  Check(SUT.Remove(1));
  Check(SUT.Remove(3));

  CheckEquals(4, fChangedEvents.Count);
  CheckChanged(0, 3, caRemoved);
  CheckChanged(1, 2, caRemoved);
  CheckChanged(2, 1, caRemoved);
  CheckChanged(3, 3, caRemoved);
end;

{$ENDREGION}


{$REGION 'TTestHashMultiSetChangedEvent'}

procedure TTestHashMultiSetChangedEvent.SetUp;
begin
  inherited;
  SUT := TCollections.CreateMultiSet<Integer>;
  Sender := SUT.AsObject;
end;

{$ENDREGION}


{$REGION 'TTestTreeMultiSetChangedEvent'}

procedure TTestTreeMultiSetChangedEvent.SetUp;
begin
  inherited;
  SUT := TCollections.CreateSortedMultiSet<Integer>;
  Sender := SUT.AsObject;
end;

{$ENDREGION}


{$REGION 'TEnumerableTestCase'}

procedure TEnumerableTestCase.CheckEquals(const expected, actual: IInterface);
var
  e1, e2: IEnumerator;
  v1, v2: TValue;
begin
  e1 := (actual as IEnumerable).GetEnumerator;
  e2 := (expected as IEnumerable).GetEnumerator;

  while e1.MoveNext do
  begin
    Check(e2.MoveNext);
    v1 := e1.Current;
    v2 := e2.Current;
    if (v1.Kind = tkInterface) and (v1.Kind = tkInterface) then
      CheckEquals(v1.AsInterface, v2.AsInterface)
    else
      Check(v1 = v2, 'collections not equal');
  end;
  Check(not e2.MoveNext, 'collections not equal');
end;

procedure TEnumerableTestCase.CheckEquals(const expected,
  actual: array of Integer);
var
  i: Integer;
begin
  Check(Length(expected) = Length(actual), 'collections not equal');
  for i := Low(expected) to High(expected) do
    CheckEquals(expected[i], actual[i], 'collections not equal');
end;

procedure TEnumerableTestCase.CheckEquals(const expected: array of Integer;
  const actual: IEnumerable<Integer>);
begin
  CheckEquals(TEnumerable.From<Integer>(expected), actual);
end;

class function TEnumerableTestCase.BreakingSequence<T>: IEnumerable<T>;
begin
  Result := TBreakingSequence<T>.Create;
end;

class function TEnumerableTestCase.BreakingReadOnlyCollection<T>(
  const collection: IReadOnlyCollection<T>): IReadOnlyCollection<T>;
begin
  Result := TBreakingReadOnlyCollection<T>.Create(collection);
end;

class function TEnumerableTestCase.FastInfiniteEnumerator<T>: IEnumerable<T>;
begin
  Result := TFastInfiniteEnumerator<T>.Create;
end;

class function TEnumerableTestCase.ForceNotCollection<T>(
  const source: array of T): IEnumerable<T>;
begin
  Result := TNotCollectionIterator<T>.Create(TEnumerable.From<T>(source));
end;

class function TEnumerableTestCase.ForceNotCollection<T>(
  const source: IEnumerable<T>): IEnumerable<T>;
begin
  Result := TNotCollectionIterator<T>.Create(source);
end;

class function TEnumerableTestCase.NumberRangeGuaranteedNotCollectionType(num,
  count: Integer): IEnumerable<Integer>;
begin
  Result := TNumberRangeIterator.Create(num, count);
end;

class function TEnumerableTestCase.GuaranteedRunOnce<T>(
  const source: IEnumerable<T>): IEnumerable<T>;
begin
  Result := TRunOnceEnumerable<T>.Create(source);
end;

procedure TEnumerableTestCase.CheckEmpty(const value: IInterface);
var
  enumerator: IEnumerator;
begin
  enumerator := (value as IEnumerable).GetEnumerator;
  Check(not enumerator.MoveNext, 'collection not empty');
end;

procedure TEnumerableTestCase.CheckEmpty(const values: array of Integer);
begin
  Check(Length(values) = 0, 'array not empty');
end;

{$ENDREGION}


{$REGION 'TEnumerableTestCase.TBreakingSequence<T>'}

function TEnumerableTestCase.TBreakingSequence<T>.GetEnumerator: IEnumerator<T>;
begin
  raise EInvalidOperationException.Create('');
end;

{$ENDREGION}


{$REGION 'TEnumerableTestCase.TBreakingReadOnlyCollection<T>'}

constructor TEnumerableTestCase.TBreakingReadOnlyCollection<T>.Create(
  const collection: IReadOnlyCollection<T>);
begin
  fCollection := collection;
end;

function TEnumerableTestCase.TBreakingReadOnlyCollection<T>.GetCount: Integer;
begin
  Result := fCollection.Count;
end;

{$ENDREGION}


{$REGION 'TEnumerableTestCase.TFastInfiniteEnumerator<T>'}

function TEnumerableTestCase.TFastInfiniteEnumerator<T>.Clone: TIterator<T>;
begin
  Result := Self;
end;

function TEnumerableTestCase.TFastInfiniteEnumerator<T>.TryMoveNext(
  var current: T): Boolean;
begin
  Result := True;
  current := Default(T);
end;

{$ENDREGION}


{$REGION 'TEnumerableTestCase.TNumberRangeIterator'}

constructor TEnumerableTestCase.TNumberRangeIterator.Create(num, count: Integer);
begin
  inherited Create;
  fNum := num;
  fCount := count;
end;

function TEnumerableTestCase.TNumberRangeIterator.Clone: TIterator<Integer>;
begin
  Result := TNumberRangeIterator.Create(fNum, fCount);
end;

function TEnumerableTestCase.TNumberRangeIterator.TryMoveNext(
  var current: Integer): Boolean;
begin
  if fIndex < fCount then
  begin
    current := fNum + fIndex;
    Inc(fIndex);
    Result := True;
  end
  else
    Result := False;
end;

{$ENDREGION}


{$REGION 'TEnumerableTestCase.TRunOnceEnumerable<T>'}

constructor TEnumerableTestCase.TRunOnceEnumerable<T>.Create(
  const source: IEnumerable<T>);
begin
  inherited Create;
  fSource := source;
end;

function TEnumerableTestCase.TRunOnceEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Assert(not fCalled);
  fCalled := True;
  Result := fSource.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TEnumerableTestCase.TNotCollectionIterator<T>'}

function TEnumerableTestCase.TNotCollectionIterator<T>.Clone: TIterator<T>;
begin
  Result := TNotCollectionIterator<T>.Create(Source);
end;

procedure TEnumerableTestCase.TNotCollectionIterator<T>.Start;
begin
  fEnumerator := Source.GetEnumerator;
end;

function TEnumerableTestCase.TNotCollectionIterator<T>.TryMoveNext(
  var current: T): Boolean;
begin
  Result := fEnumerator.MoveNext;
  if Result then
    current := fEnumerator.Current;
end;

{$ENDREGION}


{$REGION 'TEnumerableTestCase.TDelegateIterator<T>'}

function TEnumerableTestCase.TDelegateIterator<T>.Clone: TIterator<T>;
begin
  Result := TDelegateIterator<T>.Create(fMoveNext, fCurrent, fDispose);
end;

constructor TEnumerableTestCase.TDelegateIterator<T>.Create(
  const moveNext: Func<Boolean>; const current: Func<T>; const dispose: Action);
begin
  fMoveNext := moveNext;
  fCurrent := current;
  fDispose := dispose;
end;

procedure TEnumerableTestCase.TDelegateIterator<T>.Dispose;
begin
  fDispose;
end;

function TEnumerableTestCase.TDelegateIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  Result := fMoveNext;
  if Result then
    current := fCurrent;
end;

{$ENDREGION}


{$REGION 'TSkipTests'}

procedure TSkipTests.SkipSome;
begin
  CheckEquals(TEnumerable.Range(10, 10), NumberRangeGuaranteedNotCollectionType(0, 20).Skip(10));
end;

procedure TSkipTests.SkipSomeIList;
begin
  CheckEquals(TEnumerable.Range(10, 10), TCollections.CreateList<Integer>(NumberRangeGuaranteedNotCollectionType(0, 20)).Skip(10));
end;

procedure TSkipTests.RunOnce;
begin
  CheckEquals(TEnumerable.Range(10, 10), GuaranteedRunOnce<Integer>(TEnumerable.Range(0, 20)).Skip(10));
  CheckEquals(TEnumerable.Range(10, 10), GuaranteedRunOnce<Integer>(TCollections.CreateList<Integer>(TEnumerable.Range(0, 20))).Skip(10));
end;

procedure TSkipTests.SkipNone;
begin
  CheckEquals(TEnumerable.Range(0, 20), NumberRangeGuaranteedNotCollectionType(0, 20).Skip(0));
end;

procedure TSkipTests.SkipNoneIList;
begin
  CheckEquals(TEnumerable.Range(0, 20), TCollections.CreateList<Integer>(NumberRangeGuaranteedNotCollectionType(0, 20)).Skip(0));
end;

procedure TSkipTests.SkipExcessive;
begin
  CheckEquals(TEnumerable.Empty<Integer>, NumberRangeGuaranteedNotCollectionType(0, 20).Skip(42));
end;

procedure TSkipTests.SkipExcessiveIList;
begin
  CheckEquals(TEnumerable.Empty<Integer>, TCollections.CreateList<Integer>(NumberRangeGuaranteedNotCollectionType(0, 20)).Skip(42));
end;

procedure TSkipTests.SkipAllExactly;
begin
  CheckFalse(NumberRangeGuaranteedNotCollectionType(0, 20).Skip(20).Any);
end;

procedure TSkipTests.SkipAllExactlyIList;
begin
  CheckFalse(TCollections.CreateList<Integer>(NumberRangeGuaranteedNotCollectionType(0, 20)).Skip(20).Any);
end;

procedure TSkipTests.SkipOnEmpty;
begin
  CheckEquals(TEnumerable.Empty<Integer>, ForceNotCollection<Integer>(TEnumerable.Empty<Integer>).Skip(0));
  CheckEquals(TEnumerable.Empty<string>, ForceNotCollection<string>(TEnumerable.Empty<string>).Skip(-1));
  CheckEquals(TEnumerable.Empty<Double>, ForceNotCollection<Double>(TEnumerable.Empty<Double>).Skip(1));
end;

procedure TSkipTests.SkipOnEmptyIList;
begin
  CheckEquals(TEnumerable.Empty<Integer>, TCollections.CreateList<Integer>(TEnumerable.Empty<Integer>()).Skip(0));
  CheckEquals(TEnumerable.Empty<string>, TCollections.CreateList<string>(TEnumerable.Empty<string>()).Skip(-1));
  CheckEquals(TEnumerable.Empty<Double>, TCollections.CreateList<Double>(TEnumerable.Empty<Double>()).Skip(1));
end;

procedure TSkipTests.SkipNegative;
begin
  CheckEquals(TEnumerable.Range(0, 20), NumberRangeGuaranteedNotCollectionType(0, 20).Skip(-42));
end;

procedure TSkipTests.SkipNegativeIList;
begin
  CheckEquals(TEnumerable.Range(0, 20), TCollections.CreateList<Integer>(NumberRangeGuaranteedNotCollectionType(0, 20)).Skip(-42));
end;

procedure TSkipTests.SameResultsRepeatCallsIntQuery;
var
  q: IEnumerable<Integer>;
begin
  q := ForceNotCollection<Integer>(TEnumerable.From<Integer>([9999, 0, 888, -1, 66, -777, 1, 2, -12345])
    .Where(function(const x: Integer): Boolean begin Result := x > MinInt end));
  CheckEquals(q.Skip(0), q.Skip(0));
end;

procedure TSkipTests.SameResultsRepeatCallsIntQueryIList;
var
  q: IEnumerable<Integer>;
begin
  q := TCollections.CreateList<Integer>(TEnumerable.From<Integer>([9999, 0, 888, -1, 66, -777, 1, 2, -12345])
    .Where(function(const x: Integer): Boolean begin Result := x > MinInt end));
  CheckEquals(q.Skip(0), q.Skip(0));
end;

procedure TSkipTests.SkipOne;
var
  source, expected: IEnumerable<Nullable<Integer>>;
begin
  source := TEnumerable.From<Nullable<Integer>>([3, 100, 4, nil, 10]);
  expected := TEnumerable.From<Nullable<Integer>>([100, 4, nil, 10]);
  CheckEquals(expected, source.Skip(1));
end;

procedure TSkipTests.SkipOneNotIList;
var
  source, expected: IEnumerable<Nullable<Integer>>;
begin
  source := TEnumerable.From<Nullable<Integer>>([3, 100, 4, nil, 10]);
  expected := TEnumerable.From<Nullable<Integer>>([100, 4, nil, 10]);
  CheckEquals(expected, ForceNotCollection<Nullable<Integer>>(source.Skip(1)));
end;

procedure TSkipTests.SkipAllButOne;
var
  source, expected: IEnumerable<Nullable<Integer>>;
begin
  source := TEnumerable.From<Nullable<Integer>>([3, 100, nil, 4, 10]);
  expected := TEnumerable.From<Nullable<Integer>>([10]);
  CheckEquals(expected, source.Skip(source.Count - 1));
end;

procedure TSkipTests.SkipAllButOneNotIList;
var
  source, expected: IEnumerable<Nullable<Integer>>;
begin
  source := TEnumerable.From<Nullable<Integer>>([3, 100, nil, 4, 10]);
  expected := TEnumerable.From<Nullable<Integer>>([10]);
  CheckEquals(expected, ForceNotCollection<Nullable<Integer>>(source).Skip(source.Count - 1));
end;

procedure TSkipTests.SkipOneMoreThanAll;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([3, 100, 4, 10]);
  CheckEmpty(source.Skip(source.Count + 1));
end;

procedure TSkipTests.SkipOneMoreThanAllNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([3, 100, 4, 10]);
  CheckEmpty(ForceNotCollection<Integer>(source).Skip(source.Count + 1));
end;

procedure TSkipTests.ForcedToEnumeratorDoesntEnumerate;
var
  iterator: IEnumerable<Integer>;
  enumerator: IEnumerator<Integer>;
begin
  iterator := NumberRangeGuaranteedNotCollectionType(0, 3).Skip(2);
  CheckFalse(Supports(iterator, IEnumerator<Integer>, enumerator) and enumerator.MoveNext)
end;

procedure TSkipTests.ForcedToEnumeratorDoesntEnumerateIList;
var
  iterator: IEnumerable<Integer>;
  enumerator: IEnumerator<Integer>;
begin
  iterator := TEnumerable.From<Integer>([0, 1, 2]).Skip(2);
  CheckFalse(Supports(iterator, IEnumerator<Integer>, enumerator) and enumerator.MoveNext)
end;

procedure TSkipTests.Count;
begin
  CheckEquals(2, NumberRangeGuaranteedNotCollectionType(0, 3).Skip(1).Count);
  CheckEquals(2, TEnumerable.From<Integer>([1, 2, 3]).Skip(1).Count);
end;

procedure TSkipTests.FollowWithTake;
var
  source, expected: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([5, 6, 7, 8]);
  expected := TEnumerable.From<Integer>([6, 7]);
  CheckEquals(expected, source.Skip(1).Take(2));
end;

procedure TSkipTests.FollowWithTakeNotIList;
var
  source, expected: IEnumerable<Integer>;
begin
  source := NumberRangeGuaranteedNotCollectionType(5, 4);
  expected := TEnumerable.From<Integer>([6, 7]);
  CheckEquals(expected, source.Skip(1).Take(2));
end;

procedure TSkipTests.FollowWithTakeThenMassiveTake;
var
  source, expected: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([5, 6, 7, 8]);
  expected := TEnumerable.From<Integer>([7]);
  CheckEquals(expected, source.Skip(2).Take(1).Take(MaxInt));
end;

procedure TSkipTests.FollowWithTakeThenMassiveTakeNotIList;
var
  source, expected: IEnumerable<Integer>;
begin
  source := NumberRangeGuaranteedNotCollectionType(5, 4);
  expected := TEnumerable.From<Integer>([7]);
  CheckEquals(expected, source.Skip(2).Take(1).Take(MaxInt));
end;

procedure TSkipTests.FollowWithSkip;
var
  source, expected: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5, 6]);
  expected := TEnumerable.From<Integer>([4, 5, 6]);
  CheckEquals(expected, source.Skip(1).Skip(2).Skip(-4));
end;

procedure TSkipTests.FollowWithSkipNotIList;
var
  source, expected: IEnumerable<Integer>;
begin
  source := NumberRangeGuaranteedNotCollectionType(1, 6);
  expected := TEnumerable.From<Integer>([4, 5, 6]);
  CheckEquals(expected, source.Skip(1).Skip(2).Skip(-4));
end;

procedure TSkipTests.ElementAt;
var
  source, remaining: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5, 6]);
  remaining := source.Skip(2);
  CheckEquals(3, remaining.ElementAt(0));
  CheckEquals(4, remaining.ElementAt(1));
  CheckEquals(6, remaining.ElementAt(3));
  CheckException(EArgumentOutOfRangeException, procedure begin remaining.ElementAt(-1) end);
  CheckException(EArgumentOutOfRangeException, procedure begin remaining.ElementAt(4) end);
end;

procedure TSkipTests.ElementAtNotIList;
var
  source, remaining: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5, 6]);
  remaining := source.Skip(2);
  CheckEquals(3, remaining.ElementAt(0));
  CheckEquals(4, remaining.ElementAt(1));
  CheckEquals(6, remaining.ElementAt(3));
  CheckException(EArgumentOutOfRangeException, procedure begin remaining.ElementAt(-1) end);
  CheckException(EArgumentOutOfRangeException, procedure begin remaining.ElementAt(4) end);
end;

procedure TSkipTests.ElementAtOrDefault;
var
  source, remaining: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5, 6]);
  remaining := source.Skip(2);
  CheckEquals(3, remaining.ElementAtOrDefault(0));
  CheckEquals(4, remaining.ElementAtOrDefault(1));
  CheckEquals(6, remaining.ElementAtOrDefault(3));
  CheckEquals(0, remaining.ElementAtOrDefault(-1));
  CheckEquals(0, remaining.ElementAtOrDefault(4));
end;

procedure TSkipTests.ElementAtOrDefaultNotIList;
var
  source, remaining: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5, 6]);
  remaining := source.Skip(2);
  CheckEquals(3, remaining.ElementAtOrDefault(0));
  CheckEquals(4, remaining.ElementAtOrDefault(1));
  CheckEquals(6, remaining.ElementAtOrDefault(3));
  CheckEquals(0, remaining.ElementAtOrDefault(-1));
  CheckEquals(0, remaining.ElementAtOrDefault(4));
end;

procedure TSkipTests.First;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5]);
  CheckEquals(1, source.Skip(0).First);
  CheckEquals(3, source.Skip(2).First);
  CheckEquals(5, source.Skip(4).First);
  CheckException(EInvalidOperationException, procedure begin source.Skip(5).First end);
end;

procedure TSkipTests.FirstNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5]);
  CheckEquals(1, source.Skip(0).First);
  CheckEquals(3, source.Skip(2).First);
  CheckEquals(5, source.Skip(4).First);
  CheckException(EInvalidOperationException, procedure begin source.Skip(5).First end);
end;

procedure TSkipTests.FirstOrDefault;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5]);
  CheckEquals(1, source.Skip(0).FirstOrDefault);
  CheckEquals(3, source.Skip(2).FirstOrDefault);
  CheckEquals(5, source.Skip(4).FirstOrDefault);
  CheckEquals(0, source.Skip(5).FirstOrDefault);
end;

procedure TSkipTests.FirstOrDefaultNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5]);
  CheckEquals(1, source.Skip(0).FirstOrDefault);
  CheckEquals(3, source.Skip(2).FirstOrDefault);
  CheckEquals(5, source.Skip(4).FirstOrDefault);
  CheckEquals(0, source.Skip(5).FirstOrDefault);
end;

procedure TSkipTests.Last;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5]);
  CheckEquals(5, source.Skip(0).Last);
  CheckEquals(5, source.Skip(2).Last);
  CheckEquals(5, source.Skip(4).Last);
  CheckException(EInvalidOperationException, procedure begin source.Skip(5).Last end);
end;

procedure TSkipTests.LastNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5]);
  CheckEquals(5, source.Skip(0).Last);
  CheckEquals(5, source.Skip(2).Last);
  CheckEquals(5, source.Skip(4).Last);
  CheckException(EInvalidOperationException, procedure begin source.Skip(5).Last end);
end;

procedure TSkipTests.LastOrDefault;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5]);
  CheckEquals(5, source.Skip(0).LastOrDefault);
  CheckEquals(5, source.Skip(2).LastOrDefault);
  CheckEquals(5, source.Skip(4).LastOrDefault);
  CheckEquals(0, source.Skip(5).LastOrDefault);
end;

procedure TSkipTests.LastOrDefaultNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5]);
  CheckEquals(5, source.Skip(0).LastOrDefault);
  CheckEquals(5, source.Skip(2).LastOrDefault);
  CheckEquals(5, source.Skip(4).LastOrDefault);
  CheckEquals(0, source.Skip(5).LastOrDefault);
end;

procedure TSkipTests.ToArray;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5]);
  CheckEquals([1, 2, 3, 4, 5], source.Skip(0).ToArray);
  CheckEquals([2, 3, 4, 5], source.Skip(1).ToArray);
  CheckEquals([5], source.Skip(4).ToArray);
  CheckEmpty(source.Skip(5).ToArray);
  CheckEmpty(source.Skip(40).ToArray);
end;

procedure TSkipTests.ToArrayNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5]);
  CheckEquals([1, 2, 3, 4, 5], source.Skip(0).ToArray);
  CheckEquals([2, 3, 4, 5], source.Skip(1).ToArray);
  CheckEquals([5], source.Skip(4).ToArray);
  CheckEmpty(source.Skip(5).ToArray);
  CheckEmpty(source.Skip(40).ToArray);
end;

procedure TSkipTests.RepeatEnumerating;
var
  source, remaining: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5]);
  remaining := source.Skip(1);
  CheckEquals(remaining, remaining);
end;

procedure TSkipTests.RepeatEnumeratingNotIList;
var
  source, remaining: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5]);
  remaining := source.Skip(1);
  CheckEquals(remaining, remaining);
end;

procedure TSkipTests.LazySkipMoreThan32Bits;
var
  range, skipped: IEnumerable<Integer>;
begin
  range := NumberRangeGuaranteedNotCollectionType(1, 100);
  skipped := range.Skip(50).Skip(MaxInt); // Could cause an integer overflow
  CheckEmpty(skipped);
  CheckEquals(0, skipped.Count);
  CheckEmpty(skipped.ToArray);
end;

{$ENDREGION}


{$REGION 'TSkipTakeData'}

class function TSkipTakeData.EnumerableData: TArray<TArray<TValue>>;
var
  sourceCounts, counts: IEnumerable<Integer>;
begin
  sourceCounts := TEnumerable.From<Integer>([0, 1, 2, 3, 5, 8, 13, 55, 100, 250]);
  counts := TEnumerable.From<Integer>([1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 100, 250, 500, MaxInt]);
  counts := counts.Concat(TEnumerable.Select<Integer,Integer>(counts, function(const c: Integer): Integer begin Result := -c end)).Concat(TEnumerable.From<Integer>([0, MinInt]));

  Result := TEnumerable.SelectMany<Integer,TArray<TValue>>(sourceCounts,
    function(const sourceCount: Integer): IEnumerable<TArray<TValue>>
    var
      source: IEnumerable<Integer>;
    begin
      source := TEnumerable.Range(0, sourceCount);
      Result := TEnumerable.Select<Integer, TArray<TValue>>(counts,
        function(const count: Integer): TArray<TValue>
        begin
          Result := TArray<TValue>.Create(TValue.From<IEnumerable<Integer>>(source), count);
        end);
    end).ToArray;
  counts := nil;
end;

class function TSkipTakeData.EvaluationBehaviorData: TArray<Integer>;
begin
  Result := TEnumerable.Range(-1, 15).ToArray;
end;

{$ENDREGION}


{$REGION 'TSkipLastTests'}

procedure TSkipLastTests.SkipLast(const source: IEnumerable<Integer>; count: Integer);
var
  expected, actual: IEnumerable<Integer>;
  index: Integer;
begin
  expected := source.Reversed.Skip(count).Reversed;
  actual := source.SkipLast(count);

  CheckEquals(expected, actual);
  CheckEquals(expected.Count, actual.Count);
  CheckEquals(expected.ToArray, actual.ToArray);

  CheckEquals(expected.FirstOrDefault, actual.FirstOrDefault);
  CheckEquals(expected.LastOrDefault, actual.LastOrDefault);

  for index in TEnumerable.Range(0, expected.Count) do
    CheckEquals(expected.ElementAt(index), actual.ElementAt(index));

  CheckEquals(0, actual.ElementAtOrDefault(-1));
  CheckEquals(0, actual.ElementAtOrDefault(actual.Count));
end;

procedure TSkipLastTests.EvaluationBehavior(count: Integer);
var
  index, limit, i: Integer;
  source: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  index := 0;
  limit := Max(0, count * 2);

  source := TDelegateIterator<Integer>.Create(
    function: Boolean begin Result := index <> limit; Inc(index); end,
    function: Integer begin Result := index; end,
    procedure begin index := index xor MinInt; end);

  iterator := source.SkipLast(count).GetEnumerator;
  CheckEquals(0, index);

  for i := 1 to count do
  begin
    CheckTrue(iterator.MoveNext);
    CheckEquals(i, iterator.Current);
    CheckEquals(count + i, index);
  end;

  CheckFalse(iterator.MoveNext);
  CheckEquals(MinInt, index and MinInt);
end;

procedure TSkipLastTests.RunOnce(const source: IEnumerable<Integer>; count: Integer);
var
  expected: IEnumerable<Integer>;
begin
  expected := source.SkipLast(count);
  CheckEquals(expected, GuaranteedRunOnce<Integer>(source.SkipLast(count)));
end;

procedure TSkipLastTests.List_ChangesAfterSkipLast_ChangesReflectedInResults;
var
  list: IList<Integer>;
  e: IEnumerable<Integer>;
begin
  list := TCollections.CreateList<Integer>([1, 2, 3, 4, 5]);

  e := list.SkipLast(2);

  list.Delete(4);
  list.Delete(3);

  CheckEquals([1], e.ToArray);
end;


procedure TSkipLastTests.List_Skip_ChangesAfterSkipLast_ChangesReflectedInResults;
var
  list: IList<Integer>;
  e: IEnumerable<Integer>;
begin
  list := TCollections.CreateList<Integer>([1, 2, 3, 4, 5]);

  e := list.Skip(1).SkipLast(2);

  list.Delete(4);

  CheckEquals([2], e.ToArray);
end;

{$ENDREGION}


{$REGION 'TTakeTests'}

procedure TTakeTests.SameResultsRepeatCallsIntQuery;
var
  q: IEnumerable<Integer>;
begin
  q := TEnumerable.From<Integer>([9999, 0, 888, -1, 66, -777, 1, 2, -12345])
    .Where(function(const x: Integer): Boolean begin Result := x > MinInt end);

  CheckEquals(q.Take(9), q.Take(9));
end;

procedure TTakeTests.SameResultsRepeatCallsIntQueryIList;
var
  q: IEnumerable<Integer>;
begin
  q := TCollections.CreateList<Integer>(TEnumerable.From<Integer>([9999, 0, 888, -1, 66, -777, 1, 2, -12345])
    .Where(function(const x: Integer): Boolean begin Result := x > MinInt end));

  CheckEquals(q.Take(9), q.Take(9));
end;

procedure TTakeTests.SourceEmptyCountPositive;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([]);

  CheckEmpty(source.Take(5));
end;

procedure TTakeTests.SourceEmptyCountPositiveNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := NumberRangeGuaranteedNotCollectionType(0, 0);

  CheckEmpty(source.Take(5));
end;

procedure TTakeTests.SourceNonEmptyCountNegative;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([2, 5, 9, 1]);

  CheckEmpty(source.Take(-5));
end;

procedure TTakeTests.SourceNonEmptyCountNegativeNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([2, 5, 9, 1]);

  CheckEmpty(source.Take(-5));
end;

procedure TTakeTests.SourceNonEmptyCountZero;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([2, 5, 9, 1]);

  CheckEmpty(source.Take(0));
end;

procedure TTakeTests.SourceNonEmptyCountZeroNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([2, 5, 9, 1]);

  CheckEmpty(source.Take(0));
end;

procedure TTakeTests.SourceNonEmptyCountOne;
var
  source, expected: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([2, 5, 9, 1]);
  expected := TEnumerable.From<Integer>([2]);

  CheckEquals(expected, source.Take(1));
end;

procedure TTakeTests.SourceNonEmptyCountOneNotIList;
var
  source, expected: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([2, 5, 9, 1]);
  expected := TEnumerable.From<Integer>([2]);

  CheckEquals(expected, source.Take(1));
end;

procedure TTakeTests.SourceNonEmptyTakeAllExactly;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([2, 5, 9, 1]);

  CheckEquals(source, source.Take(source.Count));
end;

procedure TTakeTests.SourceNonEmptyTakeAllExactlyNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([2, 5, 9, 1]);

  CheckEquals(source, source.Take(source.Count));
end;

procedure TTakeTests.SourceNonEmptyTakeAllButOne;
var
  source, expected: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([2, 5, 9, 1]);
  expected := TEnumerable.From<Integer>([2, 5, 9]);

  CheckEquals(expected, source.Take(3));
end;

procedure TTakeTests.RunOnce;
var
  source, expected: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([2, 5, 9, 1]);
  expected := TEnumerable.From<Integer>([2, 5, 9]);

  CheckEquals(expected, GuaranteedRunOnce<Integer>(source).Take(3));
end;

procedure TTakeTests.SourceNonEmptyTakeAllButOneNotIList;
var
  source, expected: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([2, 5, 9, 1]);
  expected := TEnumerable.From<Integer>([2, 5, 9]);

  CheckEquals(expected, source.Take(3));
end;

procedure TTakeTests.SourceNonEmptyTakeExcessive;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([2, 5, 9, 1]);

  CheckEquals(source, source.Take(source.Count + 1));
end;

procedure TTakeTests.SourceNonEmptyTakeExcessiveNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([2, 5, 9, 1]);

  CheckEquals(source, source.Take(source.Count + 1));
end;

procedure TTakeTests.ForcedToEnumeratorDoesntEnumerate;
var
  iterator: IEnumerable<Integer>;
  enumerator: IEnumerator<Integer>;
begin
  iterator := NumberRangeGuaranteedNotCollectionType(0, 3).Take(2);
  CheckFalse(Supports(iterator, IEnumerator<Integer>, enumerator) and enumerator.MoveNext);
end;

procedure TTakeTests.ForcedToEnumeratorDoesntEnumerateIList;
var
  iterator: IEnumerable<Integer>;
  enumerator: IEnumerator<Integer>;
begin
  iterator := TCollections.CreateList<Integer>(NumberRangeGuaranteedNotCollectionType(0, 3)).Take(2);
  CheckFalse(Supports(iterator, IEnumerator<Integer>, enumerator) and enumerator.MoveNext);
end;

procedure TTakeTests.Count;
begin
  CheckEquals(2, NumberRangeGuaranteedNotCollectionType(0, 3).Take(2).Count);
  CheckEquals(2, TEnumerable.From([1, 2, 3]).Take(2).Count);
  CheckEquals(0, NumberRangeGuaranteedNotCollectionType(0, 3).Take(0).Count);
end;

procedure TTakeTests.FollowWithTake;
var
  source, expected: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([5, 6, 7, 8]);
  expected := TEnumerable.From<Integer>([5, 6]);

  CheckEquals(expected, source.Take(5).Take(3).Take(2).Take(40));
end;

procedure TTakeTests.FollowWithTakeNotIList;
var
  source, expected: IEnumerable<Integer>;
begin
  source := NumberRangeGuaranteedNotCollectionType(5, 4);
  expected := TEnumerable.From<Integer>([5, 6]);

  CheckEquals(expected, source.Take(5).Take(3).Take(2));
end;

procedure TTakeTests.FollowWithSkip;
var
  source, expected: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5, 6]);
  expected := TEnumerable.From<Integer>([3, 4, 5]);

  CheckEquals(expected, source.Take(5).Skip(2).Skip(-4));
end;

procedure TTakeTests.FollowWithSkipNotIList;
var
  source, expected: IEnumerable<Integer>;
begin
  source := NumberRangeGuaranteedNotCollectionType(1, 6);
  expected := TEnumerable.From<Integer>([3, 4, 5]);

  CheckEquals(expected, source.Take(5).Skip(2).Skip(-4));
end;

procedure TTakeTests.ElementAt;
var
  source, taken: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5, 6]);
  taken := source.Take(3);

  CheckEquals(1, taken.ElementAt(0));
  CheckEquals(3, taken.ElementAt(2));
  CheckException(EArgumentOutOfRangeException, procedure begin taken.ElementAt(-1) end);
  CheckException(EArgumentOutOfRangeException, procedure begin taken.ElementAt(3) end);
end;

procedure TTakeTests.ElementAtNotIList;
var
  source, taken: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5, 6]);
  taken := source.Take(3);

  CheckEquals(1, taken.ElementAt(0));
  CheckEquals(3, taken.ElementAt(2));
  CheckException(EArgumentOutOfRangeException, procedure begin taken.ElementAt(-1) end);
  CheckException(EArgumentOutOfRangeException, procedure begin taken.ElementAt(3) end);
end;

procedure TTakeTests.ElementAtOrDefault;
var
  source, taken: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5, 6]);
  taken := source.Take(3);

  CheckEquals(1, taken.ElementAtOrDefault(0));
  CheckEquals(3, taken.ElementAtOrDefault(2));
  CheckEquals(0, taken.ElementAtOrDefault(-1));
  CheckEquals(0, taken.ElementAtOrDefault(3));
end;

procedure TTakeTests.ElementAtOrDefaultNotIList;
var
  source, taken: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5, 6]);
  taken := source.Take(3);

  CheckEquals(1, taken.ElementAtOrDefault(0));
  CheckEquals(3, taken.ElementAtOrDefault(2));
  CheckEquals(0, taken.ElementAtOrDefault(-1));
  CheckEquals(0, taken.ElementAtOrDefault(3));
end;

procedure TTakeTests.First;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5]);

  CheckEquals(1, source.Take(1).First);
  CheckEquals(1, source.Take(4).First);
  CheckEquals(1, source.Take(40).First);
  CheckException(EInvalidOperationException, procedure begin source.Take(0).First end);
  CheckException(EInvalidOperationException, procedure begin source.Skip(5).Take(10).First end);
end;

procedure TTakeTests.FirstNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5]);

  CheckEquals(1, source.Take(1).First);
  CheckEquals(1, source.Take(4).First);
  CheckEquals(1, source.Take(40).First);
  CheckException(EInvalidOperationException, procedure begin source.Take(0).First end);
  CheckException(EInvalidOperationException, procedure begin source.Skip(5).Take(10).First end);
end;

procedure TTakeTests.FirstOrDefault;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5]);

  CheckEquals(1, source.Take(1).FirstOrDefault);
  CheckEquals(1, source.Take(4).FirstOrDefault);
  CheckEquals(1, source.Take(40).FirstOrDefault);
  CheckEquals(0, source.Take(0).FirstOrDefault);
  CheckEquals(0, source.Skip(5).Take(10).FirstOrDefault);
end;

procedure TTakeTests.FirstOrDefaultNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5]);

  CheckEquals(1, source.Take(1).FirstOrDefault);
  CheckEquals(1, source.Take(4).FirstOrDefault);
  CheckEquals(1, source.Take(40).FirstOrDefault);
  CheckEquals(0, source.Take(0).FirstOrDefault);
  CheckEquals(0, source.Skip(5).Take(10).FirstOrDefault);
end;

procedure TTakeTests.Last;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5]);

  CheckEquals(1, source.Take(1).Last);
  CheckEquals(5, source.Take(5).Last);
  CheckEquals(5, source.Take(40).Last);
  CheckException(EInvalidOperationException, procedure begin source.Take(0).Last end);
  CheckException(EInvalidOperationException, procedure begin TEnumerable.Empty<Integer>.Take(40).Last end);
end;

procedure TTakeTests.LastNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5]);

  CheckEquals(1, source.Take(1).Last);
  CheckEquals(5, source.Take(5).Last);
  CheckEquals(5, source.Take(40).Last);
  CheckException(EInvalidOperationException, procedure begin source.Take(0).Last end);
  CheckException(EInvalidOperationException, procedure begin ForceNotCollection<Integer>([]).Take(40).Last end);
end;

procedure TTakeTests.LastOrDefault;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5]);

  CheckEquals(1, source.Take(1).LastOrDefault);
  CheckEquals(5, source.Take(5).LastOrDefault);
  CheckEquals(5, source.Take(40).LastOrDefault);
  CheckEquals(0, source.Take(0).LastOrDefault);
  CheckEquals(0, TEnumerable.Empty<Integer>.Take(40).LastOrDefault);
end;

procedure TTakeTests.LastOrDefaultNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5]);

  CheckEquals(1, source.Take(1).LastOrDefault);
  CheckEquals(5, source.Take(5).LastOrDefault);
  CheckEquals(5, source.Take(40).LastOrDefault);
  CheckEquals(0, source.Take(0).LastOrDefault);
  CheckEquals(0, TEnumerable.Empty<Integer>.Take(40).LastOrDefault);
end;

procedure TTakeTests.ToArray;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5]);
  CheckEquals([1, 2, 3, 4, 5], source.Take(5).ToArray);
  CheckEquals([1, 2, 3, 4, 5], source.Take(6).ToArray);
  CheckEquals([1, 2, 3, 4, 5], source.Take(40).ToArray);
  CheckEquals([1, 2, 3, 4], source.Take(4).ToArray);
  CheckEquals([1], source.Take(1).ToArray);
  CheckEmpty(source.Take(0).ToArray);
  CheckEmpty(source.Take(-10).ToArray);
end;

procedure TTakeTests.ToArrayNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5]);
  CheckEquals([1, 2, 3, 4, 5], source.Take(5).ToArray);
  CheckEquals([1, 2, 3, 4, 5], source.Take(6).ToArray);
  CheckEquals([1, 2, 3, 4, 5], source.Take(40).ToArray);
  CheckEquals([1, 2, 3, 4], source.Take(4).ToArray);
  CheckEquals([1], source.Take(1).ToArray);
  CheckEmpty(source.Take(0).ToArray);
  CheckEmpty(source.Take(-10).ToArray);
end;

procedure TTakeTests.TakeCanOnlyBeOneList;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([2, 4, 6, 8, 10]);
  CheckEquals([2], source.Take(1));
  CheckEquals([4], source.Skip(1).Take(1));
  CheckEquals([6], source.Take(3).Skip(2));
  CheckEquals([2], source.Take(3).Take(1));
end;

procedure TTakeTests.TakeCanOnlyBeOneNotIList;
var
  source: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([2, 4, 6, 8, 10]);
  CheckEquals([2], source.Take(1));
  CheckEquals([4], source.Skip(1).Take(1));
  CheckEquals([6], source.Take(3).Skip(2));
  CheckEquals([2], source.Take(3).Take(1));
end;

procedure TTakeTests.RepeatEnumerating;
var
  source, taken: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1, 2, 3, 4, 5]);
  taken := source.Take(3);
  CheckEquals(taken, taken);
end;

procedure TTakeTests.RepeatEnumeratingNotIList;
var
  source, taken: IEnumerable<Integer>;
begin
  source := ForceNotCollection<Integer>([1, 2, 3, 4, 5]);
  taken := source.Take(3);
  CheckEquals(taken, taken);
end;

procedure TTakeTests.LazySkipAllTakenForLargeNumbers(largeNumber: Integer);
begin
  CheckEmpty(FastInfiniteEnumerator<Integer>.Take(largeNumber).Skip(largeNumber));
end;

procedure TTakeTests.LazyOverflowRegression;
var
  range, skipped, taken: IEnumerable<Integer>;
begin
  range := NumberRangeGuaranteedNotCollectionType(1, 100);
  skipped := range.Skip(42);
  taken := skipped.Take(MaxInt);
  CheckEquals(TEnumerable.Range(43, 100 - 42), taken);
  CheckEquals(100 - 42, taken.Count);
end;

procedure TTakeTests.CountOfLazySkipTakeChain(skip, take, expected: Integer);
var
  partition: IEnumerable<Integer>;
begin
  partition := NumberRangeGuaranteedNotCollectionType(1, 100).Skip(skip).Take(take);
  CheckEquals(expected, partition.Count);
  CheckEquals(expected, TEnumerable.Select<Integer,Integer>(partition, function(const i: Integer): Integer begin Result := i end).Count);
  CheckEquals(expected, Length(TEnumerable.Select<Integer,Integer>(partition, function(const i: Integer): Integer begin Result := i end).ToArray));
end;

procedure TTakeTests.FirstAndLastOfLazySkipTakeChain(
  const source: TArray<Integer>; skip, take, first, last: Integer);
var
  partition: IEnumerable<Integer>;
begin
  partition := ForceNotCollection<Integer>(source).Skip(skip).Take(take);

  CheckEquals(first, partition.FirstOrDefault);
  CheckEquals(first, partition.ElementAtOrDefault(0));
  CheckEquals(last, partition.LastOrDefault);
  CheckEquals(last, partition.ElementAtOrDefault(partition.Count - 1));
end;

procedure TTakeTests.ElementAtOfLazySkipTakeChain(const source: TArray<Integer>;
  skip, take: Integer; indices, expectedValues: TArray<Integer>);
var
  partition: IEnumerable<Integer>;
  i: Integer;
begin
  partition := ForceNotCollection<Integer>(source).Skip(skip).Take(take);

  CheckEquals(Length(expectedValues), Length(indices));
  for i := 0 to High(indices) do
    CheckEquals(expectedValues[i], partition.ElementAtOrDefault(indices[i]));
end;

{$ENDREGION}


{$REGION 'TTakeLastTests'}

procedure TTakeLastTests.TakeLast(const source: IEnumerable<Integer>; count: Integer);
var
  expected, actual: IEnumerable<Integer>;
  index: Integer;
begin
  expected := source.Reversed.Take(count).Reversed;
  actual := source.TakeLast(count);

  CheckEquals(expected, actual);
  CheckEquals(expected.Count, actual.Count);
  CheckEquals(expected.ToArray, actual.ToArray);

  CheckEquals(expected.FirstOrDefault, actual.FirstOrDefault);
  CheckEquals(expected.LastOrDefault, actual.LastOrDefault);

  for index in TEnumerable.Range(0, expected.Count) do
    CheckEquals(expected.ElementAt(index), actual.ElementAt(index));

  CheckEquals(0, actual.ElementAtOrDefault(-1));
  CheckEquals(0, actual.ElementAtOrDefault(actual.Count));
end;

procedure TTakeLastTests.EvaluationBehavior(count: Integer);
var
  index, limit, i, expected: Integer;
  source: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  index := 0;
  limit := count * 2;

  source := TDelegateIterator<Integer>.Create(
    function: Boolean begin Result := index <> limit; Inc(index) end,
    function: Integer begin Result := index end,
    procedure begin index := index xor MinInt end);

  iterator := source.TakeLast(count).GetEnumerator;
  CheckEquals(0, index);

  for i := 1 to count do
  begin
    CheckTrue(iterator.MoveNext);
    CheckEquals(count + i, iterator.Current);

    CheckEquals(MinInt, index and MinInt);
    CheckEquals(limit + 1, index and MaxInt);
  end;

  CheckFalse(iterator.MoveNext);

  expected := IfThen(count <= 0, 0, MinInt);
  CheckEquals(expected, index and MinInt);
end;

procedure TTakeLastTests.RunOnce(const source: IEnumerable<Integer>; count: Integer);
var
  expected: IEnumerable<Integer>;
begin
  expected := source.TakeLast(count);
  CheckEquals(expected, GuaranteedRunOnce<Integer>(source.TakeLast(count)));
end;

procedure TTakeLastTests.List_ChangesAfterTakeLast_ChangesReflectedInResults;
var
  list: IList<Integer>;
  e: IEnumerable<Integer>;
begin
  list := TCollections.CreateList<Integer>([1, 2, 3, 4, 5]);

  e := list.TakeLast(3);

  list.Delete(0);
  list.Delete(0);

  CheckEquals([3, 4, 5], e.ToArray);
end;

procedure TTakeLastTests.List_Skip_ChangesAfterTakeLast_ChangesReflectedInResults;
var
  list: IList<Integer>;
  e: IEnumerable<Integer>;
begin
  list := TCollections.CreateList<Integer>([1, 2, 3, 4, 5]);

  e := list.Skip(1).TakeLast(3);

  list.Delete(0);

  CheckEquals([3, 4, 5], e.ToArray);
end;

{$ENDREGION}


{$REGION 'TAtLeastTests'}

procedure TAtLeastTests.NegativeCount;
begin
  CheckException(EArgumentOutOfRangeException, procedure begin
    BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).AtLeast(-1); end);
end;

procedure TAtLeastTests.EmptySequenceHasAtLeastZeroElements;
begin
  CheckTrue(BreakingReadOnlyCollection<Integer>(TEnumerable.Empty<Integer>).AtLeast(0));
end;

procedure TAtLeastTests.EmptySequenceHasAtLeastOneElement;
begin
  CheckFalse(BreakingReadOnlyCollection<Integer>(TEnumerable.Empty<Integer>).AtLeast(1));
end;

procedure TAtLeastTests.EmptySequenceHasAtLeastManyElements;
begin
  CheckFalse(BreakingReadOnlyCollection<Integer>(TEnumerable.Empty<Integer>).AtLeast(2));
end;

procedure TAtLeastTests.SingleElementHasAtLeastZeroElements;
begin
  CheckTrue(BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).AtLeast(0));
end;

procedure TAtLeastTests.SingleElementHasAtLeastOneElement;
begin
  CheckTrue(BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).AtLeast(1));
end;

procedure TAtLeastTests.SingleElementHasAtLeastManyElements;
begin
  CheckFalse(BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).AtLeast(2));
end;

procedure TAtLeastTests.ManyElementsHasAtLeastZeroElements;
begin
  CheckTrue(BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1, 2, 3])).AtLeast(0));
end;

procedure TAtLeastTests.ManyElementsHasAtLeastOneElement;
begin
  CheckTrue(BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1, 2, 3])).AtLeast(1));
end;

procedure TAtLeastTests.ManyElementsHasAtLeastManyElements;
begin
  CheckTrue(BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1, 2, 3])).AtLeast(2));
end;

procedure TAtLeastTests.DoesNotIterateUnnecessaryElements;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.Range(1, 3).Where(
    function(const i: Integer): Boolean
    begin
      if i <= 2 then
        Result := True
      else
        raise Exception.Create('');
    end);
  CheckTrue(source.AtLeast(2));
end;

{$ENDREGION}


{$REGION 'TAtMostTests'}

procedure TAtMostTests.NegativeCount;
begin
  CheckException(EArgumentOutOfRangeException, procedure begin
    BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).AtMost(-1); end);
end;

procedure TAtMostTests.EmptySequenceHasAtMostZeroElements;
begin
  CheckTrue(BreakingReadOnlyCollection<Integer>(TEnumerable.Empty<Integer>).AtMost(0));
end;

procedure TAtMostTests.EmptySequenceHasAtMostOneElement;
begin
  CheckTrue(BreakingReadOnlyCollection<Integer>(TEnumerable.Empty<Integer>).AtMost(1));
end;

procedure TAtMostTests.SingleElementHasAtMostZeroElements;
begin
  CheckFalse(BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).AtMost(0));
end;

procedure TAtMostTests.SingleElementHasAtMostOneElement;
begin
  CheckTrue(BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).AtMost(1));
end;

procedure TAtMostTests.SingleElementHasAtMostManyElements;
begin
  CheckTrue(BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).AtMost(2));
end;

procedure TAtMostTests.ManyElementsHasAtMostOneElement;
begin
  CheckFalse(BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1, 2, 3])).AtMost(1));
end;

procedure TAtMostTests.DoesNotIterateUnnecessaryElements;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.Range(1, 4).Where(
    function(const i: Integer): Boolean
    begin
      if i <= 3 then
        Result := True
      else
        raise Exception.Create('');
    end);
  CheckFalse(source.AtMost(2));
end;

{$ENDREGION}


{$REGION 'TBetweenTests'}

procedure TBetweenTests.NegativeMin;
begin
  CheckException(EArgumentOutOfRangeException, procedure begin
    BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).Between(-1, 0); end);
end;

procedure TBetweenTests.NegativeMax;
begin
  CheckException(EArgumentOutOfRangeException, procedure begin
    BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).Between(0, -1); end);
end;

procedure TBetweenTests.MaxLesserThanMin;
begin
  CheckException(EArgumentOutOfRangeException, procedure begin
    BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).Between(1, 0); end);
end;

procedure TBetweenTests.MaxEqualsMin;
begin
  CheckTrue(BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).Between(1, 1));
end;

procedure TBetweenTests.RangeTests(count, min, max: Integer; expecting: Boolean);
begin
  CheckEquals(expecting, BreakingReadOnlyCollection<Integer>(TEnumerable.Range(1, count)).Between(min, max));
end;

procedure TBetweenTests.DoesNotIterateUnnecessaryElements;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.Range(1, 5).Where(
    function(const i: Integer): Boolean
    begin
      if i <= 4 then
        Result := True
      else
        raise Exception.Create('');
    end);
  CheckFalse(source.Between(2, 3));
end;

{$ENDREGION}


{$REGION 'TBetweenTests'}

procedure TExactlyTests.NegativeCount;
begin
  CheckException(EArgumentOutOfRangeException, procedure begin
    BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).Exactly(-1); end);
end;

procedure TExactlyTests.EmptySequenceHasExactlyZeroElements;
begin
  CheckTrue(BreakingReadOnlyCollection<Integer>(TEnumerable.Empty<Integer>).Exactly(0));
end;

procedure TExactlyTests.EmptySequenceHasExactlyOneElement;
begin
  CheckFalse(BreakingReadOnlyCollection<Integer>(TEnumerable.Empty<Integer>).Exactly(1));
end;

procedure TExactlyTests.SingleElementHasExactlyOneElement;
begin
  CheckTrue(BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1])).Exactly(1));
end;

procedure TExactlyTests.ManyElementsHasExactlyOneElement;
begin
  CheckFalse(BreakingReadOnlyCollection<Integer>(TEnumerable.From<Integer>([1, 2, 3])).Exactly(1));
end;

procedure TExactlyTests.DoesNotIterateUnnecessaryElements;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.Range(1, 4).Where(
    function(const i: Integer): Boolean
    begin
      if i <= 3 then
        Result := True
      else
        raise Exception.Create('');
    end);
  CheckFalse(source.Exactly(2));
end;

{$ENDREGION}


{$REGION 'TMemoizeTests'}

procedure TMemoizeTests.MemoizeWithPartialIterationBeforeCompleteIteration;
var
  buffer: IEnumerable<Integer>;
begin
  buffer := TEnumerable.Range(0, 10).Memoize;

  CheckEquals(buffer.Take(5), TEnumerable.Range(0, 5));
  CheckEquals(buffer, TEnumerable.Range(0, 10));
end;

procedure TMemoizeTests.MemoizeIsLazy;
begin
  BreakingSequence<Integer>.Memoize;
  Pass;
end;

procedure TMemoizeTests.MemoizeWithInMemoryCollection;
var
  collection: IReadOnlyCollection<Integer>;
begin
  collection := BreakingReadOnlyCollection<Integer>(TEnumerable.Empty<Integer>());
  CheckSame(collection.Memoize, collection);
end;

procedure TMemoizeTests.MemoizeEnumeratesOnlyOnce;
var
  count, i, n: Integer;
  range, memoized: IEnumerable<Integer>;
  arr: TArray<Integer>;
begin
  count := 10;
  range := TEnumerable.Range(1, count).Where(
    function(const x: Integer): Boolean
    begin
      Check(count > 0);
      Dec(count);
      Result := True;
    end);
  memoized := range.Memoize;

  CheckEquals(5, memoized.Take(5).Count);
  CheckEquals(5, count);
  CheckEquals(10, memoized.Count);
  CheckEquals(0, count);
  CheckEquals(10, memoized.Count);

  i := 0;
  SetLength(arr, 10);
  for n in memoized do
  begin
    arr[i] := n;
    Inc(i);
  end;
  CheckEquals([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], arr);
end;

procedure TMemoizeTests.MemoizeWithMemoizedSourceReturnsSame;
var
  memoized, memoizedAgain: IEnumerable<Integer>;
begin
  memoized := TEnumerable.Range(0, 9).Memoize;
  memoizedAgain := memoized.Memoize;

  CheckSame(memoized, memoizedAgain);
end;

{$ENDREGION}


{$REGION 'TTestChunk'}

procedure TTestChunk.RaisesWhenSizeIsNonPositive(size: Integer);
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([1]);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      TEnumerable.Chunk<Integer>(source, size);
    end);
end;

procedure TTestChunk.ChunkSourceLazily;
var
  chunks: IEnumerator<TArray<Integer>>;
begin
  chunks := TEnumerable.Chunk<Integer>(FastInfiniteEnumerator<Integer>, 5).GetEnumerator;
  chunks.MoveNext;
  CheckEquals([0, 0, 0, 0, 0], chunks.Current);
  CheckTrue(chunks.MoveNext);
end;

procedure TTestChunk.ChunkSourceRepeatCalls;
var
  source: IEnumerable<Integer>;
begin
  source := TEnumerable.From<Integer>([9999, 0, 888, -1, 66, -777, 1, 2, -12345]);
  CheckEquals(TEnumerable.Chunk<Integer>(source, 3), TEnumerable.Chunk<Integer>(source, 3));
end;

procedure TTestChunk.ChunkSourceEvenly;
var
  source: IEnumerable<Integer>;
  chunks: IEnumerator<TArray<Integer>>;
begin
  source := TEnumerable.From<Integer>([9999, 0, 888, -1, 66, -777, 1, 2, -12345]);
  chunks := TEnumerable.Chunk<Integer>(source, 3).GetEnumerator;
  chunks.MoveNext;
  CheckEquals([9999, 0, 888], chunks.Current);
  chunks.MoveNext;
  CheckEquals([-1, 66, -777], chunks.Current);
  chunks.MoveNext;
  CheckEquals([1, 2, -12345], chunks.Current);
  CheckFalse(chunks.MoveNext);
end;

procedure TTestChunk.ChunkSourceUnevenly;
var
  source: IEnumerable<Integer>;
  chunks: IEnumerator<TArray<Integer>>;
begin
  source := TEnumerable.From<Integer>([9999, 0, 888, -1, 66, -777, 1, 2]);
  chunks := TEnumerable.Chunk<Integer>(source, 3).GetEnumerator;
  chunks.MoveNext;
  CheckEquals([9999, 0, 888], chunks.Current);
  chunks.MoveNext;
  CheckEquals([-1, 66, -777], chunks.Current);
  chunks.MoveNext;
  CheckEquals([1, 2], chunks.Current);
  CheckFalse(chunks.MoveNext);
end;

procedure TTestChunk.ChunkSourceSmallerThanMaxSize;
var
  source: IEnumerable<Integer>;
  chunks: IEnumerator<TArray<Integer>>;
begin
  source := TEnumerable.From<Integer>([9999, 0]);
  chunks := TEnumerable.Chunk<Integer>(source, 3).GetEnumerator;
  chunks.MoveNext;
  CheckEquals([9999, 0], chunks.Current);
  CheckFalse(chunks.MoveNext);
end;

procedure TTestChunk.EmptySourceYieldsNoChunks;
var
  source: IEnumerable<Integer>;
  chunks: IEnumerator<TArray<Integer>>;
begin
  source := TEnumerable.From<Integer>([]);
  chunks := TEnumerable.Chunk<Integer>(source, 3).GetEnumerator;
  CheckFalse(chunks.MoveNext);
end;

procedure TTestChunk.RemovingFromSourceBeforeIterating;
var
  list: IList<Integer>;
  chunks: IEnumerator<TArray<Integer>>;
begin
  list := TCollections.CreateList<Integer>([9999, 0, 888, -1, 66, -777, 1, 2, -12345]);
  chunks := TEnumerable.Chunk<Integer>(list, 3).GetEnumerator;
  list.Remove(66);
  chunks.MoveNext;
  CheckEquals([9999, 0, 888], chunks.Current);
  chunks.MoveNext;
  CheckEquals([-1, -777, 1], chunks.Current);
  chunks.MoveNext;
  CheckEquals([2, -12345], chunks.Current);
  CheckFalse(chunks.MoveNext);
end;

procedure TTestChunk.AddingToSourceBeforeIterating;
var
  list: IList<Integer>;
  chunks: IEnumerator<TArray<Integer>>;
begin
  list := TCollections.CreateList<Integer>([9999, 0, 888, -1, 66, -777, 1, 2, -12345]);
  chunks := TEnumerable.Chunk<Integer>(list, 3).GetEnumerator;
  list.Add(10);
  chunks.MoveNext;
  CheckEquals([9999, 0, 888], chunks.Current);
  chunks.MoveNext;
  CheckEquals([-1, 66, -777], chunks.Current);
  chunks.MoveNext;
  CheckEquals([1, 2, -12345], chunks.Current);
  chunks.MoveNext;
  CheckEquals([10], chunks.Current);
  CheckFalse(chunks.MoveNext);
end;

procedure TTestChunk.DoesNotPrematurelyAllocateHugeArray;
var
  chunks: TArray<TArray<Integer>>;
begin
  chunks := TEnumerable.Chunk<Integer>(TEnumerable.Range(0, 10), MaxInt).ToArray;
  CheckEquals(TEnumerable.Range(0, 10).ToArray, chunks[0]);
end;

{$ENDREGION}


end.
