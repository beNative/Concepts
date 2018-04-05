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

unit Spring.Tests.Collections;

{$I Spring.inc}

interface

uses
  Classes,
  Generics.Collections,
  Generics.Defaults,
  TestFramework,
  Spring.TestUtils,
  Spring,
  Spring.Collections,
  Spring.Collections.LinkedLists,
{$IFNDEF DELPHI2010}
  Spring.Collections.Trees,
{$ENDIF}
  Spring.Collections.Lists;

type
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
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CheckSet(const collection: ISet<Integer>; const values: array of Integer);
  published
    procedure TestExceptWith;
    procedure TestIntersectWith;
    procedure TestIntersectWithList;
    procedure TestUnionWith;
    procedure TestSetEquals;
    procedure TestSetEqualsList;
    procedure TestIsSubsetOf;
    procedure TestIsSupersetOf;
    procedure TestOverlaps;
    procedure TestExtract;
  end;

  TTestIntegerList = class(TTestCase)
  private
    SUT: IList<Integer>;
    procedure SimpleFillList;
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
    procedure TestListInsertRangeArray;
    procedure TestListInsertRangeIEnumerable;
    procedure TestListInsertRangeIEnumerableWithExtraCapacity;
    procedure TestListSimpleDelete;
    procedure TestListMultipleDelete;
    procedure TestListSimpleExchange;
    procedure TestListReverse;
    procedure TestListReverseEmpty;
    procedure TestListSort;
    procedure TestListIndexOf;
    procedure TestLastIndexOf;
    procedure TestListMove;
    procedure TestListClear;
    procedure TestListLargeDelete;
    procedure TestQueryInterface;
    procedure TestIssue67;
    procedure TestCopyTo;
    procedure TestArrayAccess;
    procedure TestIssue53;

    procedure GetCapacity;
    procedure SetCapacity;

    procedure TestGetRange_AllItems;
    procedure TestGetRange_FirstItems;
    procedure TestGetRange_LastItems;

    procedure TestExtract_ItemNotInList;
    procedure TestExtractAll_OneItemInList;
    procedure TestExtractAll_MultipleItemsInList_RemoveSome;

    procedure TestEnumeratorMoveNext_VersionMismatch;

    procedure TestRemoveAll;

    procedure TestAddRange_EmptySource;

    procedure TestExtractAt;
    procedure TestExtractRange;
  end;

  TTestStringList = class(TTestCase)
  private
    SUT: IList<string>;
  protected
    procedure TearDown; override;
  published
    procedure TestCaseInsensitive;
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

    procedure TestReturnsMinusOneWhenNotFound;
  end;

  TTestEmptyStringIntegerDictionary = class(TTestCase)
  private
    SUT: IDictionary<string, Integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDictionaryIsInitializedEmpty;
    procedure TestDictionaryKeysAreEmpty;
    procedure TestDictionaryValuesAreEmpty;
    procedure TestDictionaryContainsReturnsFalse;
    procedure TestDictionaryValuesReferenceCounting;
  end;

  TTestStringIntegerDictionary = class(TTestCase)
  private
    SUT: IDictionary<string, Integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDictionaryCountWithAdd;
    procedure TestDictionarySimpleValues;
    procedure TestDictionaryKeys;
    procedure TestDictionaryValues;
    procedure TestDictionaryContainsValue;
    procedure TestDictionaryContainsKey;
    procedure TestMapAdd;
    procedure TestMapRemove;
    procedure TestOrdered;
    procedure TestOrdered_Issue179;
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
{$IFNDEF DELPHI2010}
    procedure TestStackTrimExcess;
{$ENDIF}
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
{$IFNDEF DELPHI2010}
    procedure TestQueueTrimExcess;
{$ENDIF}
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
    procedure TestSetOwnsObjects;
    procedure TestGetElementType;
    procedure TestExtractAt;
    procedure TestGetRangeElementType;
  end;

  TTestInterfaceList = class(TTestCase)
  private
    SUT: IList<IInvokable>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInterfaceListCreate;
    procedure TestGetElementType;
    procedure TestCopyTo;
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
    procedure TestToArray;
  end;

  TTestListAdapter = class(TTestCase)
  private
    InternalList: IList<Integer>;
    SUT: IList;
    procedure ListChanged(Sender: TObject; const Item: Integer;
      Action: TCollectionChangedAction);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestListAdd;
    procedure TestListAddRangeArray;
    procedure TestListAddRangeIEnumerable;

    procedure TestListAsReadOnlyList;

    procedure TestListClear;

    procedure TestListDelete;
    procedure TestListDeleteRange;
    procedure TestListDeleteRange2;

    procedure TestListExchange;

    procedure TestListExtract;
    procedure TestListExtractRangeArray;
    procedure TestListExtractRangeIEnumerable;

    procedure TestListGetCount;
    procedure TestListGetElementType;
    procedure TestListGetEnumerator;
    procedure TestListGetIsReadOnly;
    procedure TestListGetOnChanged;

    procedure TestListGetItem;
    procedure TestListSetItem;

    procedure TestListIndexOf;

    procedure TestListInsert;
    procedure TestListInsertRangeArray;
    procedure TestListInsertRangeIEnumerable;

    procedure TestListLastIndexOf;

    procedure TestListMove;

    procedure TestListQueryInterface;

    procedure TestListRemove;
    procedure TestListRemoveRangeArray;
    procedure TestListRemoveRangeIEnumerable;

    procedure TestListReverse;
    procedure TestListReverse2;

    procedure TestListSort;
  end;

  TTestMultiMap = class(TTestCase)
  private
    SUT: IMultiMap<Integer,Integer>;
    ValueAddedCount, ValueRemovedCount: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure ValueChanged(Sender: TObject; const Item: Integer;
      Action: TCollectionChangedAction);
  published
    procedure TestAddPair;

    procedure TestAddStringPair;

    procedure TestInternalEventHandlersDetached;
    procedure TestValueChangedCalledProperly;
    procedure TestValuesOrdered;
    procedure TestExtractValues;
  end;

  TTestBidiDictionary = class(TTestCase)
  published
    procedure AddDictionary;
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

  TTestOrderedDictionary = class(TTestCase)
  private
    SUT: IOrderedDictionary<Integer,string>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckCount(expected: Integer);
  published
    procedure TestAddKeyValue;
    procedure TestKeysGetEnumerator;
    procedure TestKeysToArray;
    procedure TestValuesGetEnumerator;
    procedure TestValuesToArray;
    procedure TestGetEnumerator;
    procedure TestGetItemByIndex;
    procedure TestAddOrSetValue;
    procedure TestRemove;
    procedure TestExtractPair;
    procedure TestToArray;
  end;

{$IFNDEF DELPHI2010}
  TTestEmptyIntegerStringMap = class(TTestCase)
  private
    SUT: IMap<Integer, string>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMapIsInitializedEmpty;
    procedure TestMapKeysAreEmpty;
    procedure TestMapValuesAreEmpty;
    procedure TestMapContainsReturnsFalse;
    procedure TestMapValuesReferenceCounting;
  end;

  TTestIntegerStringMap = class(TTestCase)
  private
    SUT: IDictionary<Integer, string>;
    const NumItems = 100;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCount;
    procedure TestMapSimpleValues;
    procedure TestKeys;
    procedure TestValues;
    procedure TestContainsValue;
    procedure TestContainsKey;
    procedure TestEnumeration;
    procedure TestToArray;
    procedure TestOrdered;
    procedure TestAddValue;
    procedure TestAddOrSetValue;
    procedure TestExtract;
    procedure TestExtractPair;
    procedure TestRemoveKey;
    procedure TestGetValueOrDefault;
    procedure TestTryGetValue;
    procedure TestIndexedGet;
    procedure TestIndexedSet;
  end;

  TTestRedBlackTree = class(TTestCase)
  private
    SUT: IBinaryTree<Integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDuplicates;
    procedure TestDelete;

    procedure FuzzyTesting;

    procedure TestInsert;
  end;
{$ENDIF}

implementation

uses
  Spring.Collections.Dictionaries,
  Spring.Collections.Queues,
  Spring.Collections.Stacks,
  StrUtils,
  SysUtils,
  TypInfo;

const
  MaxItems = 1000;
  ListCountLimit = 1000;//0000;


{$REGION 'TTestEmptyHashSet'}

procedure TTestEmptyHashSet.SetUp;
begin
  inherited;
  fSet := TCollections.CreateSet<Integer>;
  fEmpty := TCollections.CreateSet<Integer>;
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

procedure TTestNormalHashSet.SetUp;
begin
  inherited;
  fSet1 := TCollections.CreateSet<Integer>;
  fSet2 := TCollections.CreateSet<Integer>;
  fSet1.AddRange([1, 2, 3]);
  fSet2.AddRange([3, 1, 4, 5]);
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

procedure TTestIntegerList.GetCapacity;
begin
  SimpleFillList;
  CheckEquals(4, TList<Integer>(SUT).Capacity);
end;

procedure TTestIntegerList.SetCapacity;
begin
  SimpleFillList;
  TList<Integer>(SUT).Capacity := 2;
  CheckTrue(SUT.EqualsTo([1, 2]));
end;

procedure TTestIntegerList.SetUp;
begin
  inherited;
  SUT := TCollections.CreateList<Integer>;
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

procedure TTestIntegerList.TestArrayAccess;
var
  arrayAccess: IArrayAccess<Integer>;
  values: TArray<Integer>;
begin
  SimpleFillList;
  arrayAccess := SUT as IArrayAccess<Integer>;
  values := arrayAccess.Items;
  CheckEquals(4, Length(values));
  CheckEquals(3, arrayAccess.Count);
  CheckEquals(SUT[0], values[0]);
  CheckEquals(SUT[1], values[1]);
  CheckEquals(SUT[2], values[2]);
  values[1] := 4;
  CheckEquals(SUT[1], values[1]);
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
  items: IReadOnlyList<Integer>;
begin
  callCount := 0;
  SUT.AddRange([1, 2, 3, 4, 5]);
  items := SUT.ExtractAll(
    function(const i: Integer): Boolean
    begin
      Result := Odd(i);
      Inc(callCount);
    end);
  CheckEquals(3, items.Count);
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
  items: IReadOnlyList<Integer>;
begin
  callCount := 0;
  SUT.Add(1);
  items := SUT.ExtractAll(
    function(const i: Integer): Boolean
    begin
      Result := True;
      Inc(callCount);
    end);
  CheckEquals(1, items.Count);
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

type
  TIntegerList = class(TList<Integer>)
  public
    procedure Clear; override;
  end;

procedure TIntegerList.Clear;
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
  SUT.Add(1);
  SUT.Add(1);
  SUT.Add(1);
  SUT.Add(2);
  SUT.Add(3);

  CheckEquals(2, SUT.LastIndexOf(1));
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
  (SUT as TList<Integer>).Capacity := ListCountLimit;
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

procedure TTestIntegerList.TestListIndexOf;
var
  i: Integer;
begin
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

procedure TTestStringList.TearDown;
begin
  SUT := nil;
end;

procedure TTestStringList.TestCaseInsensitive;
begin
  SUT := TCollections.CreateList<string>(TStringComparer.OrdinalIgnoreCase());
  SUT.AddRange(['AAA', 'BBB', 'CCC']);
  CheckTrue(SUT.Contains('aaa'));
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


{$REGION 'TTestStringIntegerDictionary'}

procedure TTestStringIntegerDictionary.SetUp;
begin
  inherited;
  SUT := TCollections.CreateDictionary<string, Integer>;
  SUT.Add('one', 1);
  SUT.Add('two', 2);
  SUT.Add('three', 3);
end;

procedure TTestStringIntegerDictionary.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestStringIntegerDictionary.TestDictionaryContainsKey;
begin
  CheckTrue(SUT.ContainsKey('one'), '"one" not found by ContainsKey');
  CheckTrue(SUT.ContainsKey('two'), '"two" not found by ContainsKey');
  CheckTrue(SUT.ContainsKey('three'), '"three" not found by ContainsKey');
end;

procedure TTestStringIntegerDictionary.TestDictionaryContainsValue;
begin
  CheckTrue(SUT.ContainsValue(1), '1 not found by ContainsValue');
  CheckTrue(SUT.ContainsValue(2), '2 not found by ContainsValue');
  CheckTrue(SUT.ContainsValue(3), '3 not found by ContainsValue');
end;

procedure TTestStringIntegerDictionary.TestDictionaryCountWithAdd;
begin
  CheckEquals(3, SUT.Count, 'TestDictionaryCountWithAdd: Count is not correct');
end;

procedure TTestStringIntegerDictionary.TestDictionaryKeys;
var
  Result: IReadOnlyCollection<string>;
begin
  Result := SUT.Keys;
  CheckEquals(3, Result.Count, 'TestDictionaryKeys: Keys call returns wrong count');

  CheckTrue(Result.Contains('one'), 'TestDictionaryKeys: Keys doesn''t contain "one"');
  CheckTrue(Result.Contains('two'), 'TestDictionaryKeys: Keys doesn''t contain "two"');
  CheckTrue(Result.Contains('three'), 'TestDictionaryKeys: Keys doesn''t contain "three"');
end;

procedure TTestStringIntegerDictionary.TestDictionarySimpleValues;
begin
  CheckEquals(3, SUT.Count, 'TestDictionarySimpleValues: Count is not correct');

  CheckEquals(1, SUT['one']);
  CheckEquals(2, SUT['two']);
  CheckEquals(3, SUT['three']);
end;

procedure TTestStringIntegerDictionary.TestDictionaryValues;
var
  Result: IReadOnlyCollection<Integer>;
begin
  Result := SUT.Values;
  CheckEquals(3, Result.Count, 'TestDictionaryKeys: Values call returns wrong count');

  CheckTrue(Result.Contains(1), 'TestDictionaryKeys: Values doesn''t contain "one"');
  CheckTrue(Result.Contains(2), 'TestDictionaryKeys: Values doesn''t contain "two"');
  CheckTrue(Result.Contains(3), 'TestDictionaryKeys: Values doesn''t contain "three"');
end;

procedure TTestStringIntegerDictionary.TestMapAdd;
begin
  (SUT as IMap<string, Integer>).Add('ten', 10); //check if correctly overriden (not abstract)
  CheckEquals(10, SUT['ten']);
end;

procedure TTestStringIntegerDictionary.TestMapRemove;
begin
  (SUT as IMap<string, Integer>).Remove('one');
  CheckFalse(SUT.ContainsKey('one'), 'TestMapRemove: Values does contain "one"');
end;

procedure TTestStringIntegerDictionary.TestOrdered;
var
  pairs: TArray<TPair<string, Integer>>;
begin
  SUT.Clear;
  SUT.Add('1', 1);
  SUT.Add('2', 2);
  SUT.Add('3', 3);

  pairs := SUT.Ordered.ToArray;
  CheckEquals(3, Length(pairs));
  // check if sorted by key
  CheckEquals('1', pairs[0].Key);
  CheckEquals('2', pairs[1].Key);
  CheckEquals('3', pairs[2].Key);
end;

procedure TTestStringIntegerDictionary.TestOrdered_Issue179;
var
  o: IEnumerable<TPair<string,Integer>>;
  e: IEnumerator<TPair<string,Integer>>;
  i: Integer;
begin
  // this test is making sure that .Ordered is properly reference counted
  // and captures the dictionary keeping it alive

  o := SUT.Ordered;
  e := o.GetEnumerator;

  // should not destroy the dictionary because of the ordered enumerable
  SUT := nil;

  // even now it should not be destroyed because
  // the enumerator is still keeping it alive
  o := nil;

  // make sure that the dictionary is really still there and contains the items
  i := 0;
  while e.MoveNext do
    Inc(i);
  CheckEquals(3, i);

  // now setting the reference to the enumerator should finally
  // trigger the destruction of the dictionary as this was the
  // last reference keeping it alive
  e := nil;
end;

{$ENDREGION}


{$REGION 'TTestEmptyStringIntegerDictionary'}

procedure TTestEmptyStringIntegerDictionary.SetUp;
begin
  inherited;
  SUT := TCollections.CreateDictionary<string, Integer>;
end;

procedure TTestEmptyStringIntegerDictionary.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestEmptyStringIntegerDictionary.TestDictionaryContainsReturnsFalse;
begin
  CheckFalse(SUT.ContainsKey('blah'));
  CheckFalse(SUT.ContainsValue(42));
end;

procedure TTestEmptyStringIntegerDictionary.TestDictionaryIsInitializedEmpty;
begin
  CheckEquals(0, SUT.Count);
end;

procedure TTestEmptyStringIntegerDictionary.TestDictionaryKeysAreEmpty;
var
  Result: IReadOnlyCollection<string>;
begin
  Result := SUT.Keys;
  CheckEquals(0, Result.Count);
end;

procedure TTestEmptyStringIntegerDictionary.TestDictionaryValuesAreEmpty;
var
  Result: IReadOnlyCollection<Integer>;
begin
  Result := SUT.Values;
  CheckEquals(0, Result.Count);
end;

procedure TTestEmptyStringIntegerDictionary.TestDictionaryValuesReferenceCounting;
var
  query: IEnumerable<Integer>;
begin
  query := SUT.Values.Skip(1);
  CheckNotNull(query);
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
  CheckException(EListError, procedure begin SUT.Pop end, 'EListError not raised');
  CheckException(EListError, procedure begin SUT.Peek end, 'EListError not raised');
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

procedure TTestStackOfInteger.TestStackClear;
begin
  FillStack;
  SUT.Clear;
  CheckEquals(0, SUT.Count, 'Stack failed to empty after call to Clear');
end;

procedure TTestStackOfInteger.TestStackCreate;
const
  values: array[0..9] of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
begin
  SUT := TStack<Integer>.Create(values);
  CheckTrue(SUT.EqualsTo(values));
  SUT := TStack<Integer>.Create(TEnumerable.Range(0, 10));
  CheckTrue(SUT.EqualsTo(values));
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

{$IFNDEF DELPHI2010}
procedure TTestStackOfInteger.TestStackTrimExcess;
var
  stack: TStack<Integer>;
begin
  stack := SUT as TStack<Integer>;
  CheckEquals(0, stack.Capacity);
  stack.Capacity := MaxItems;
  CheckEquals(MaxItems, stack.Capacity);
  stack.TrimExcess;
  CheckEquals(0, stack.Capacity);
end;
{$ENDIF}

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
  SUT := TStack<TBytes>.Create;
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

  event.Add(TMethodPointer(method));

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
  CheckException(EListError, procedure begin SUT.Peek end, 'EListError was not raised on Peek call with empty Queue');
end;

procedure TTestEmptyQueueOfInteger.TestDequeueRaisesException;
begin
  CheckException(EListError, procedure begin SUT.Dequeue end, 'EListError was not raised on Peek call with empty Queue');
end;

{$ENDREGION}


{$REGION 'TTestQueueOfInteger'}

procedure TTestQueueOfInteger.FillQueue;
var
  i: Integer;
begin
  Check(SUT <> nil);
  for i := 0 to MaxItems - 1  do
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
  SUT := TQueue<Integer>.Create(values);
  CheckTrue(SUT.EqualsTo(values));
  SUT := TQueue<Integer>.Create(TEnumerable.Range(0, 10));
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
  ExpectedException := EListError;
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

{$IFNDEF DELPHI2010}
procedure TTestQueueOfInteger.TestQueueTrimExcess;
var
  queue: TQueue<Integer>;
begin
  queue := SUT as TQueue<Integer>;
  CheckEquals(0, queue.Capacity);
  queue.Capacity := MaxItems;
  CheckEquals(MaxItems, queue.Capacity);
  queue.TrimExcess;
  CheckEquals(0, queue.Capacity);
end;
{$ENDIF}

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

{$ENDREGION}


{$REGION 'TTestQueueOfTBytes'}

procedure TTestQueueOfTBytes.SetUp;
begin
  SUT := TQueue<TBytes>.Create;
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

  event.Add(TMethodPointer(method));

  SUT.Enqueue(0);

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
{$IFDEF AUTOREFCOUNT}
  CheckTrue(node.fOwned);
  CheckEquals(2, node.RefCount);
{$ENDIF}
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
  SUT := TObjectList<TPersistent>.Create;
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

procedure TTestObjectList.TestGetElementType;
begin
  Check(TypeInfo(TPersistent) = SUT.ElementType);
end;

procedure TTestObjectList.TestObjectListCreate;
begin
  SUT := TObjectList<TPersistent>.Create(nil) as IList<TPersistent>;
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

procedure TTestObjectList.TestSetOwnsObjects;
begin
  CheckTrue(TObjectList<TPersistent>(SUT).OwnsObjects);
  TObjectList<TPersistent>(SUT).OwnsObjects := False;
  CheckFalse(TObjectList<TPersistent>(SUT).OwnsObjects);
end;

{$ENDREGION}


{$REGION 'TTestInterfaceList'}

procedure TTestInterfaceList.SetUp;
begin
  SUT := TCollections.CreateList<IInvokable>;
end;

type
  TInvokable = class(TInterfacedObject, IInvokable);

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
    function(workingSentence, next: string): string
    begin
      Result := next + ' ' + workingSentence;
    end);
  CheckEquals('dog lazy the over jumps fox brown quick the', reversed);
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

{$ENDREGION}


{$REGION 'TTestListAdapter'}

procedure TTestListAdapter.ListChanged(Sender: TObject; const Item: Integer;
  Action: TCollectionChangedAction);
begin
  Check(True);
end;

procedure TTestListAdapter.SetUp;
begin
  InternalList := TCollections.CreateList<Integer>([1, 2, 3]);
  SUT := InternalList.AsList;
end;

procedure TTestListAdapter.TearDown;
begin
  inherited;
  SUT := nil;
  InternalList := nil;
end;

procedure TTestListAdapter.TestListAdd;
begin
  SUT.Add(4);
  CheckTrue(InternalList.EqualsTo([1, 2, 3, 4]));
end;

procedure TTestListAdapter.TestListAddRangeArray;
begin
  SUT.AddRange([4, 5]);
  CheckTrue(InternalList.EqualsTo([1, 2, 3, 4, 5]));
end;

procedure TTestListAdapter.TestListAddRangeIEnumerable;
begin
  SUT.AddRange(TEnumerable.Range(4, 2));
  CheckTrue(InternalList.EqualsTo([1, 2, 3, 4, 5]));
end;

procedure TTestListAdapter.TestListAsReadOnlyList;
var
  readOnlyList: IReadOnlyList;
  i: Integer;
begin
  readOnlyList := SUT.AsReadOnlyList;
  for i := 0 to readOnlyList.Count - 1 do
    CheckEquals(i + 1, readOnlyList[i].AsInteger);
end;

procedure TTestListAdapter.TestListClear;
begin
  SUT.Clear;
  CheckFalse(InternalList.Any)
end;

procedure TTestListAdapter.TestListDelete;
begin
  SUT.Delete(1);
  CheckTrue(InternalList.EqualsTo([1, 3]));
end;

procedure TTestListAdapter.TestListDeleteRange;
begin
  SUT.DeleteRange(1, 2);
  CheckTrue(InternalList.EqualsTo([1]));
end;

procedure TTestListAdapter.TestListDeleteRange2;
begin
  SUT.DeleteRange(0, 2);
  CheckTrue(InternalList.EqualsTo([3]));
end;

procedure TTestListAdapter.TestListExchange;
begin
  SUT.Exchange(0, 2);
  CheckTrue(InternalList.EqualsTo([3, 2, 1]));
end;

procedure TTestListAdapter.TestListExtract;
begin
  SUT.Extract(2);
  CheckTrue(InternalList.EqualsTo([1, 3]));
end;

procedure TTestListAdapter.TestListExtractRangeArray;
begin
  SUT.ExtractRange([3, 1]);
  CheckTrue(InternalList.EqualsTo([2]));
end;

procedure TTestListAdapter.TestListExtractRangeIEnumerable;
begin
  SUT.ExtractRange(TEnumerable.Range(1, 2));
  CheckTrue(InternalList.EqualsTo([3]));
end;

procedure TTestListAdapter.TestListGetCount;
begin
  CheckEquals(3, SUT.Count);
end;

procedure TTestListAdapter.TestListGetElementType;
begin
  Check(SUT.ElementType = TypeInfo(Integer));
end;

procedure TTestListAdapter.TestListGetEnumerator;
var
  v: TValue;
  i: Integer;
begin
  i := 0;
  for v in SUT do
  begin
    CheckEquals(InternalList[i], v.AsInteger);
    Inc(i);
  end;
  CheckEquals(i, 3);
end;

procedure TTestListAdapter.TestListGetIsReadOnly;
begin
  CheckFalse(SUT.IsReadOnly);
end;

procedure TTestListAdapter.TestListGetItem;
begin
  CheckEquals(2, SUT[1].AsInteger);
end;

procedure TTestListAdapter.TestListGetOnChanged;
var
  m: TMethod;
begin
  TCollectionChangedEvent<Integer>(m) := ListChanged;
  SUT.OnChanged.Add(TMethodPointer(m));
  SUT.Clear;
end;

procedure TTestListAdapter.TestListIndexOf;
begin
  CheckEquals(1, SUT.IndexOf(2));
  SUT.Add(1);
  CheckEquals(3, SUT.IndexOf(1, 1));
  CheckEquals(-1, SUT.IndexOf(3, 0, 2));
end;

procedure TTestListAdapter.TestListInsert;
begin
  SUT.Insert(0, 3);
  CheckTrue(InternalList.EqualsTo([3, 1, 2, 3]));
end;

procedure TTestListAdapter.TestListInsertRangeArray;
begin
  SUT.InsertRange(0, [3, 2]);
  CheckTrue(InternalList.EqualsTo([3, 2, 1, 2, 3]));
end;

procedure TTestListAdapter.TestListInsertRangeIEnumerable;
begin
  SUT.InsertRange(0, TEnumerable.Range(3, 2));
  CheckTrue(InternalList.EqualsTo([3, 4, 1, 2, 3]));
end;

procedure TTestListAdapter.TestListLastIndexOf;
begin
  SUT.AddRange([1, 3, 1]);
  CheckEquals(5, SUT.LastIndexOf(1));
  CheckEquals(3, SUT.LastIndexOf(1, 4));
  CheckEquals(-1, SUT.LastIndexOf(2, 5, 3));
end;

procedure TTestListAdapter.TestListMove;
begin
  SUT.Move(2, 0);
  CheckTrue(InternalList.EqualsTo([3, 1, 2]));
end;

procedure TTestListAdapter.TestListQueryInterface;
var
  list: IList<Integer>;
  collection: ICollection<Integer>;
  enumerable: IEnumerable<Integer>;
begin
  list := SUT as IList<Integer>;
  CheckSame(InternalList, list);
  collection := SUT as ICollection<Integer>;
  CheckSame(InternalList, collection);
  enumerable := SUT as IEnumerable<Integer>;
  CheckSame(enumerable as TObject, SUT as TObject);
end;

procedure TTestListAdapter.TestListRemove;
begin
  SUT.Remove(2);
  CheckTrue(InternalList.EqualsTo([1, 3]));
end;

procedure TTestListAdapter.TestListRemoveRangeArray;
begin
  SUT.RemoveRange([3, 1]);
  CheckTrue(InternalList.EqualsTo([2]));
end;

procedure TTestListAdapter.TestListRemoveRangeIEnumerable;
begin
  SUT.RemoveRange(TEnumerable.Range(1, 2));
  CheckTrue(InternalList.EqualsTo([3]));
end;

procedure TTestListAdapter.TestListReverse;
begin
  SUT.Reverse;
  CheckTrue(InternalList.EqualsTo([3, 2, 1]));
end;

procedure TTestListAdapter.TestListReverse2;
begin
  SUT.Reverse(1, 2);
  CheckTrue(InternalList.EqualsTo([1, 3, 2]));
end;

procedure TTestListAdapter.TestListSetItem;
begin
  SUT[2] := 4;
  CheckTrue(InternalList.EqualsTo([1, 2, 4]));
end;

procedure TTestListAdapter.TestListSort;
begin
  SUT.AddRange([9, 7, 5, 4, 6, 8]);
  SUT.Sort;
  CheckTrue(InternalList.EqualsTo([1, 2, 3, 4, 5, 6, 7, 8, 9]));
end;

{$ENDREGION}


{$REGION 'TTestMultiMap'}

procedure TTestMultiMap.SetUp;
begin
  SUT := TCollections.CreateMultiMap<Integer,Integer>;
  ValueAddedCount := 0;
  ValueRemovedCount := 0;
end;

procedure TTestMultiMap.TearDown;
begin
  SUT := nil;
end;

procedure TTestMultiMap.TestAddPair;
begin
  SUT.Add(TPair<Integer,Integer>.Create(1,1));
  CheckEquals(1, SUT.Count);
  CheckEquals(1, SUT[1].First);
end;

procedure TTestMultiMap.TestAddStringPair;
var
  map: IMultiMap<string,TPair<string,string>>;
  pair: TPair<string,string>;
begin
  map := TCollections.CreateMultiMap<string,TPair<string,string>>;
  pair.Key := 'Hello';
  pair.Value := 'World';
  map.Add('Test', pair);
  CheckEquals(1, map.Count);
end;

procedure TTestMultiMap.TestExtractValues;
var
  map: IMultiMap<Integer,TObject>;
  list: IList<TObject>;
  obj: TObject;
begin
  map := TCollections.CreateMultiMap<Integer,TObject>([doOwnsValues]);
  map.Add(1, TObject.Create);
  list := map.ExtractValues(1);
  CheckEquals(0, map.Count);
  CheckEquals(1, list.Count);
  map := nil;
  obj := list.ExtractAt(0);
  obj.Free;
  list := nil;
end;

procedure TTestMultiMap.TestInternalEventHandlersDetached;
var
  items: IReadOnlyList<Integer>;
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

procedure TTestMultiMap.TestValueChangedCalledProperly;
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

procedure TTestMultiMap.TestValuesOrdered;
begin
  SUT.Add(1, 1);
  SUT.Add(1, 2);
  SUT.Add(2, 3);
  SUT.Add(2, 4);
  SUT.Add(3, 5);
  SUT.Add(3, 6);
  SUT.Add(4, 7);
  SUT.Add(4, 8);

  CheckTrue(SUT.Values.Ordered.EqualsTo([1, 2, 3, 4, 5, 6, 7, 8]));
end;

procedure TTestMultiMap.ValueChanged(Sender: TObject; const Item: Integer;
  Action: TCollectionChangedAction);
begin
  case Action of
    caAdded: Inc(ValueAddedCount);
    caRemoved: Inc(ValueRemovedCount);
  end;
end;

{$ENDREGION}


{$REGION 'TTestBidiDictionary'}

procedure TTestBidiDictionary.AddDictionary;
var
  dict: IDictionary<Integer,string>;
  bidi: IBidiDictionary<Integer,string>;
begin
  dict := TCollections.CreateDictionary<Integer,string>;
  dict.Add(1,'a');
  dict.Add(2,'b');
  dict.Add(3,'c');
  dict.Add(4,'d');
  bidi := TCollections.CreateBidiDictionary<Integer,string>;
  bidi.AddRange(dict);
  CheckTrue(bidi.EqualsTo(dict));
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
  SUT := TObjectStack<TObject>.Create;
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
  SUT := TObjectQueue<TObject>.Create;
end;

procedure TTestObjectQueue.TearDown;
begin
  SUT := nil;
  inherited;
end;

{$ENDREGION}


{$REGION 'TTestOrderedDictionary'}

procedure TTestOrderedDictionary.CheckCount(expected: Integer);
begin
  CheckEquals(expected, SUT.Count, 'Count');
  CheckEquals(expected, SUT.Keys.Count, 'Keys.Count');
  CheckEquals(expected, SUT.Values.Count, 'Values.Count');
end;

procedure TTestOrderedDictionary.SetUp;
begin
  SUT := TOrderedDictionary<Integer,string>.Create;
end;

procedure TTestOrderedDictionary.TearDown;
begin
  SUT := nil;
end;

procedure TTestOrderedDictionary.TestAddKeyValue;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');

  CheckCount(4);
end;

procedure TTestOrderedDictionary.TestAddOrSetValue;
var
  values: TArray<string>;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');
  SUT.AddOrSetValue(2, 'e');

  CheckCount(4);

  values := SUT.Values.ToArray;
  CheckEquals(values[0], 'a');
  CheckEquals(values[1], 'e');
  CheckEquals(values[2], 'c');
  CheckEquals(values[3], 'd');
end;

procedure TTestOrderedDictionary.TestExtractPair;
var
  pair: TPair<Integer,string>;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');

  pair := SUT.ExtractPair(5);
  CheckEquals(5, pair.Key);
  CheckEquals('', pair.Value);
  CheckCount(4);
  pair := SUT.ExtractPair(4);
  CheckEquals(4, pair.Key);
  CheckEquals('d', pair.Value);
  CheckCount(3);
  pair := SUT.Extract(3, 'c');
  CheckEquals(3, pair.Key);
  CheckEquals('c', pair.Value);
  CheckCount(2);
end;

procedure TTestOrderedDictionary.TestGetEnumerator;
var
  pair: TPair<Integer,string>;
  i: Integer;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');

  i := 0;
  for pair in SUT do
  begin
    Inc(i);
    CheckEquals(i, pair.Key);
  end;
  CheckEquals(4, i);
end;

procedure TTestOrderedDictionary.TestGetItemByIndex;
var
  i: Integer;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');

  for i := 0 to SUT.Count - 1 do
    CheckEquals(i + 1, SUT.Items[i].Key);
end;

procedure TTestOrderedDictionary.TestKeysGetEnumerator;
var
  i, key: Integer;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');

  i := 0;
  for key in SUT.Keys do
  begin
    Inc(i);
    CheckEquals(i, key);
  end;
  CheckEquals(4, i);
end;

procedure TTestOrderedDictionary.TestKeysToArray;
var
  keys: TArray<Integer>;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');
  keys := SUT.Keys.ToArray;
  CheckEquals(4, Length(keys));
  CheckEquals(1, keys[0]);
  CheckEquals(2, keys[1]);
  CheckEquals(3, keys[2]);
  CheckEquals(4, keys[3]);
end;

procedure TTestOrderedDictionary.TestRemove;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');
  CheckTrue(SUT.Remove(3));
  CheckCount(3);
  CheckFalse(SUT.Remove(4, 'e'));
  CheckCount(3);
  CheckTrue(SUT.Remove(4, 'd'));
  CheckCount(2);
end;

procedure TTestOrderedDictionary.TestToArray;
var
  items: TArray<TPair<Integer, string>>;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');
  items := SUT.ToArray;
  CheckEquals(4, Length(items));
  CheckEquals(1, items[0].Key);
  CheckEquals(2, items[1].Key);
  CheckEquals(3, items[2].Key);
  CheckEquals(4, items[3].Key);
  CheckEquals('a', items[0].Value);
  CheckEquals('b', items[1].Value);
  CheckEquals('c', items[2].Value);
  CheckEquals('d', items[3].Value);
end;

procedure TTestOrderedDictionary.TestValuesGetEnumerator;
const
  Values: array[1..4] of string = ('a', 'b', 'c', 'd');
var
  i: Integer;
  value: string;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');
  i := 0;
  for value in SUT.Values do
  begin
    Inc(i);
    CheckEquals(Values[i], value);
  end;
  CheckEquals(4, i);
end;

procedure TTestOrderedDictionary.TestValuesToArray;
var
  values: TArray<string>;
begin
  SUT.Add(1, 'a');
  SUT.Add(2, 'b');
  SUT.Add(3, 'c');
  SUT.Add(4, 'd');
  values := SUT.Values.ToArray;
  CheckEquals(4, Length(values));
  CheckEquals('a', values[0]);
  CheckEquals('b', values[1]);
  CheckEquals('c', values[2]);
  CheckEquals('d', values[3]);
end;

{$ENDREGION}


{$REGION 'TTestEmptyIntegerStringMap'}

{$IFNDEF DELPHI2010}
procedure TTestEmptyIntegerStringMap.SetUp;
begin
  SUT := TCollections.CreateSortedDictionary<Integer, string>;
end;

procedure TTestEmptyIntegerStringMap.TearDown;
begin
  SUT := nil;
end;

procedure TTestEmptyIntegerStringMap.TestMapIsInitializedEmpty;
begin
  Check(SUT.Count = 0);
end;

procedure TTestEmptyIntegerStringMap.TestMapKeysAreEmpty;
var
  keys: IReadOnlyCollection<Integer>;
begin
  keys := SUT.Keys;
  CheckEquals(0, keys.Count);
end;

procedure TTestEmptyIntegerStringMap.TestMapValuesAreEmpty;
var
  values: IReadOnlyCollection<string>;
begin
  values := SUT.Values;
  CheckEquals(0, values.Count);
end;

procedure TTestEmptyIntegerStringMap.TestMapContainsReturnsFalse;
begin
  CheckFalse(SUT.ContainsKey(42));
  CheckFalse(SUT.ContainsValue('42'));
end;

procedure TTestEmptyIntegerStringMap.TestMapValuesReferenceCounting;
var
  query: IEnumerable<string>;
begin
  query := SUT.Values.Skip(1);
  CheckNotNull(query);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TTestIntegerStringMap'}

{$IFNDEF DELPHI2010}
procedure TTestIntegerStringMap.SetUp;
var
  i, n: Integer;
begin
  SUT := TCollections.CreateSortedDictionary<Integer, string>;
  // Add a reasonable number of items to the map, adding in an order that might
  // cause a mismatched tree if the backing red-black tree had balancing errors.
  n := Round(NumItems * 0.667);
  for i := n to Pred(NumItems) do
    SUT.Add(i, IntToStr(i));
  for i := 0 to Pred(n) do
    SUT.Add(i, IntToStr(i));
end;

procedure TTestIntegerStringMap.TearDown;
begin
  SUT := nil;
end;

procedure TTestIntegerStringMap.TestCount;
begin
  Check(SUT.Count = NumItems);
end;

procedure TTestIntegerStringMap.TestMapSimpleValues;
var
  i: Integer;
begin
  // Check all items are in the map
  Check(SUT.Count = NumItems);
  for i := 0 to Pred(NumItems) do
    Check(SUT[i] = IntToStr(i));
  // Check a couple of others are not there
  Check(not SUT.ContainsKey(-1));
  Check(not SUT.ContainsKey(NumItems));
end;

procedure TTestIntegerStringMap.TestKeys;
var
  keys: IReadOnlyCollection<Integer>;
  i: Integer;
begin
  keys := SUT.Keys;
  Check(keys.Count = NumItems);
  for i := 0 to Pred(NumItems) do
    Check(Keys.Contains(i));
end;

procedure TTestIntegerStringMap.TestValues;
var
  values: IReadOnlyCollection<string>;
  i: Integer;
begin
  Values := SUT.Values;
  Check(Values.Count = NumItems);
  for i := 0 to Pred(NumItems) do
    Check(Values.Contains(IntToStr(i)));
end;

procedure TTestIntegerStringMap.TestContainsValue;
var
  i: Integer;
begin
  // Check it contains each value it should
  for i := 0 to Pred(NumItems) do
    Check(SUT.ContainsValue(IntToStr(i)));

  // And check it does not contain other values
  Check(not SUT.ContainsValue('-1'));
  Check(not SUT.ContainsValue('test'));
  Check(not SUT.ContainsValue(''));
end;

procedure TTestIntegerStringMap.TestContainsKey;
var
  i: Integer;
begin
  // Check it contains each value it should
  for i := 0 to Pred(NumItems) do
    Check(SUT.ContainsKey(i));

  // And check it does not contain other keys
  Check(not SUT.ContainsKey(-1));
  Check(not SUT.ContainsKey(NumItems));
  Check(not SUT.ContainsKey(MaxInt));
end;

procedure TTestIntegerStringMap.TestEnumeration;
var
  i: Integer;
  item: TPair<Integer, string>;
begin
  // Check it enumerates each item, and in the expected order
  i := 0;
  for item in SUT do
  begin
    Check(item.Key = i);
    Check(item.Value = IntToStr(i));
    Inc(i);
  end;
  Check(i = NumItems);
end;

procedure TTestIntegerStringMap.TestToArray;
var
  i: Integer;
  items: TArray<TPair<Integer, string>>;
begin
  items := SUT.ToArray;
  Check(Assigned(items));
  Check(Length(items) = NumItems);

  for i := Low(items) to High(items) do
  begin
    Check(items[i].Key = i);
    Check(items[i].Value = IntToStr(i));
  end;
end;

procedure TTestIntegerStringMap.TestOrdered;
var
  items: IEnumerable<TPair<Integer, string>>;
  i: Integer;
  item: TPair<Integer, string>;
begin
  items := SUT.Ordered;
  Check(Assigned(items));

  // Check it enumerates each item, and in the expected order
  i := 0;
  for item in SUT do
  begin
    Check(item.Key = i);
    Check(item.Value = IntToStr(i));
    Inc(i);
  end;
  Check(i = NumItems);
end;

procedure TTestIntegerStringMap.TestAddValue;
begin
  SUT.Add(NumItems, IntToStr(NumItems));
  Check(SUT.Items[NumItems] = IntToStr(NumItems));
  Check(SUT.Count = NumItems+1);

  // Add should raise an exception when the item already exists
  CheckException(EListError,
    procedure
    begin
      SUT.Add(NumItems, IntToStr(NumItems));
    end);
  CheckException(EListError,
    procedure
    begin
      SUT.Add(0, IntToStr(0));
    end);
  CheckException(EListError,
    procedure
    begin
      SUT.Add(NumItems div 2, IntToStr(NumItems div 2));
    end);

  // Check it didn't actually add anything in the failure items above
  Check(SUT.Count = NumItems + 1);
end;

procedure TTestIntegerStringMap.TestAddOrSetValue;
begin
  // AddOrSet should never raise an exception when there's a duplicate already,
  // but should simply overwrite it.

  SUT.AddOrSetValue(NumItems, IntToStr(NumItems));
  Check(SUT.Items[NumItems] = IntToStr(NumItems));
  Check(SUT.Count = NumItems+1);

  // Add again
  SUT.AddOrSetValue(NumItems, 'test');
  Check(SUT.Items[NumItems] = 'test');
end;

procedure TTestIntegerStringMap.TestExtract;
begin
  Check(SUT.Count = NumItems);

  Check(SUT.Extract(0) = '0');
  Check(SUT.Count = NumItems-1); // Extract removes an item

  Check(SUT.Extract(NumItems-1) = IntToStr(NumItems-1));
  Check(SUT.Count = NumItems-2);

  Check(SUT.Extract(NumItems div 2) = IntToStr(NumItems div 2));
  Check(SUT.Count = NumItems-3);
end;

procedure TTestIntegerStringMap.TestExtractPair;
var
  item: TPair<Integer, string>;
begin
  Check(SUT.Count = NumItems);

  item := SUT.ExtractPair(0);
  Check(item.Key = 0);
  Check(item.Value = '0');
  Check(SUT.Count = NumItems-1); // Extract removes an item

  item := SUT.ExtractPair(NumItems-1);
  Check(item.Key = NumItems-1);
  Check(item.Value = IntToStr(NumItems-1));
  Check(SUT.Count = NumItems-2);

  item := SUT.ExtractPair(NumItems div 2);
  Check(item.Key = NumItems div 2);
  Check(item.Value = IntToStr(NumItems div 2));
  Check(SUT.Count = NumItems-3);
end;

procedure TTestIntegerStringMap.TestRemoveKey;
begin
  Check(SUT.Count = NumItems);

  Check(SUT.Remove(0));
  Check(SUT.Count = NumItems-1);

  Check(SUT.Remove(NumItems-1));
  Check(SUT.Count = NumItems-2);

  Check(SUT.Remove(NumItems div 2));
  Check(SUT.Count = NumItems-3);

  // But Remove should fail for items that are not present
  Check(not SUT.Remove(NumItems div 2)); // Already removed
  Check(SUT.Count = NumItems-3); // Count unchanged

  Check(not SUT.Remove(NumItems * 2)); // Never added
  Check(SUT.Count = NumItems-3); // Count unchanged
end;

procedure TTestIntegerStringMap.TestGetValueOrDefault;
begin
  // An item that exists
  Check((SUT as IReadOnlyDictionary<Integer, string>).GetValueOrDefault(0, 'test') = '0');
  // An item that does not exist
  Check((SUT as IReadOnlyDictionary<Integer, string>).GetValueOrDefault(NumItems*2, 'test') = 'test');
end;

procedure TTestIntegerStringMap.TestTryGetValue;
var
  value: string;
begin
  // An item that exists
  Check(SUT.TryGetValue(0, value));
  Check(value = '0');

  // An item that does not exist
  value := 'blah';
  Check(not SUT.TryGetValue(NumItems * 2, value));
end;

procedure TTestIntegerStringMap.TestIndexedGet;
begin
  Check(SUT[0] = '0');
  Check(SUT[NumItems div 2] = IntToStr(NumItems div 2));

  // Get returns the default value when the item doesn't exist
  Check(SUT[NumItems * 10] = '');
  Check(SUT[MaxInt] = '');
  Check(SUT[-500] = '');
end;

procedure TTestIntegerStringMap.TestIndexedSet;
var
  item: TPair<Integer, string>;
  i: Integer;
begin
  // Set overwrites existing items without an error
  Check(SUT[0] = '0');
  SUT[0] := 'hello';
  Check(SUT[0] = 'hello');

  // Use .Items explicitly (it should be the default property)
  Check(SUT.Items[NumItems div 2] = IntToStr(NumItems div 2));
  SUT.Items[NumItems div 2] := 'hello again';
  Check(SUT.Items[NumItems div 2] = 'hello again');

  // And adds new items
  Check(not SUT.ContainsKey(10000));
  SUT[10000] := 'large number';
  Check(SUT.Count = NumItems + 1);
  Check(SUT.ContainsKey(10000));
  Check(SUT.Items[10000] = 'large number');

  // And check that all the other items are untouched
  i := 0;
  for item in SUT do
  begin
    // The items changed or added above
    if i = 0 then
    begin
      Check(item.Key = 0);
      Check(item.Value = 'hello');
    end else if i = NumItems div 2 then
    begin
      Check(item.Key = NumItems div 2);
      Check(item.Value = 'hello again');
    end else if i = NumItems then
    begin
      // The last item should be the large number added above
      Check(i = NumItems);
      Check(item.Key = 10000);
      Check(item.Value = 'large number');
    end else
    begin
      // All other items in the map should not have been affected by the above
      Check(item.Key = i);
      Check(item.Value = IntToStr(i));
    end;
    Inc(i);
  end;
  Check(i = NumItems+1);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TTestRedBlackTree'}

{$IFNDEF DELPHI2010}
procedure TTestRedBlackTree.FuzzyTesting;

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
    i, n: Integer;
    output, inputSorted: TArray<Integer>;
    inputString: string;
  begin
    for i in input do
      SUT.Add(i);
    output := SUT.ToArray;

    inputString := ArrayToString(input);

    CheckEquals(Length(input), Length(output), inputString);

    inputSorted := Copy(input);
    TArray.Sort<Integer>(inputSorted);

    for n := 0 to High(output) do
      CheckEquals(inputSorted[n], output[n], inputString);

    for i := 0 to Length(input) - 1 do
    begin
      CheckTrue(SUT.Delete(input[i]));
      output := SUT.ToArray;

      inputSorted := Copy(input, i + 1);
      TArray.Sort<Integer>(inputSorted);
      for n := 0 to High(output) do
        CheckEquals(inputSorted[n], output[n], inputString);
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
  Randomize;

  for n := 1 to COUNT do
  begin
    SUT := TRedBlackTree<Integer>.Create;
    inputLen := Random(MAX_INPUT_LENGHT) + 1;
    SetLength(input, inputLen);
    for i := 0 to inputLen - 1 do
      input[i] := Random(MAX_VALUE) + 1;
    input := TEnumerable.Distinct<Integer>(TEnumerable.From<Integer>(input)).ToArray;
    Test(input);
  end;
end;

procedure TTestRedBlackTree.SetUp;
begin
  SUT := TRedBlackTree<Integer>.Create;
end;

procedure TTestRedBlackTree.TearDown;
begin
  SUT := nil;
end;

procedure TTestRedBlackTree.TestDelete;
var
  arr: TArray<Integer>;
begin
  SUT.Add(4);
  SUT.Add(2);
  SUT.Add(6);
  SUT.Add(1);
  SUT.Add(3);
  SUT.Add(5);
  SUT.Add(7);

  SUT.Delete(4);
  arr := SUT.ToArray;
  CheckEquals(6, Length(arr));
  CheckEquals(1, arr[0]);
  CheckEquals(2, arr[1]);
  CheckEquals(3, arr[2]);
  CheckEquals(5, arr[3]);
  CheckEquals(6, arr[4]);
  CheckEquals(7, arr[5]);
end;

procedure TTestRedBlackTree.TestDuplicates;
begin
  CheckTrue(SUT.Add(1));
  CheckFalse(SUT.Add(1));
  CheckTrue(SUT.Add(2));
  CheckTrue(SUT.Add(3));
  CheckFalse(SUT.Add(1));
  CheckEquals(3, SUT.Count);
end;

procedure TTestRedBlackTree.TestInsert;
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
{$ENDIF}

{$ENDREGION}


end.
