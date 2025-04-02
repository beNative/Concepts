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

/// <summary>
///   The Spring.Collections namespaces introduce the Collections Framework in
///   spring4d.
/// </summary>
unit Spring.Collections;

interface

uses
  Classes,
  Generics.Defaults,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Comparers;

const
  doOwnsKeys = Spring.doOwnsKeys;
  doOwnsValues = Spring.doOwnsValues;

  caAdded = Spring.caAdded;
  caRemoved = Spring.caRemoved;
  caExtracted = Spring.caExtracted;
  caReplaced = Spring.caReplaced;
  caMoved = Spring.caMoved;
  caReset = Spring.caReset;
  caChanged = Spring.caChanged;
  caReseted = Spring.caReset deprecated 'Use caReset instead'; //FI:O803

type
  TDictionaryOwnerships = Spring.TDictionaryOwnerships;

  TCollectionChangedAction = Spring.TCollectionChangedAction;

  TCollectionChangedEvent<T> = procedure(Sender: TObject; const Item: T;
    Action: TCollectionChangedAction) of object;

  TPair<TKey, TValue> = packed record
    Key: TKey;
    Value: TValue;
    constructor Create(const key: TKey; const value: TValue);
  end;

  ICollectionChangedEvent<T> = interface(IEvent<TCollectionChangedEvent<T>>)
  end;

  INotifyCollectionChanged<T> = interface //FI:W524
    ['{B4F756F2-B436-462D-8046-AB70377228F1}']
  {$REGION 'Property Accessors'}
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}

    /// <summary>
    ///   Occurs when an item is added, removed, or moved, or the entire list
    ///   is refreshed.
    /// </summary>
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  IEnumerator = Spring.IEnumerator;

  /// <summary>
  ///   Supports a simple iteration over a generic collection.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of elements to enumerate.
  /// </typeparam>
  IEnumerator<T> = interface //FI:W524
    ['{E6525A22-15EF-46EB-8A68-8CB202DA7D67}']
  {$REGION 'Property Accessors'}
    function GetCurrent: T;
  {$ENDREGION}

    /// <summary>
    ///   Advances the enumerator to the next element of the collection.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the enumerator was successfully advanced to the next
    ///   element; <c>False</c> if the enumerator has passed the end of the
    ///   collection.
    /// </returns>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The collection was modified after the enumerator was created.
    /// </exception>
    /// <remarks>
    ///   If MoveNext passes the end of the collection, the enumerator is
    ///   positioned after the last element in the collection and MoveNext
    ///   returns <c>False</c>. When the enumerator is at this position,
    ///   subsequent calls to MoveNext also return <c>False</c>.
    /// </remarks>
    function MoveNext: Boolean;

    /// <summary>
    ///   Gets the element in the collection at the current position of the
    ///   enumerator.
    /// </summary>
    /// <value>
    ///   The element in the collection at the current position of the
    ///   enumerator.
    /// </value>
    property Current: T read GetCurrent;
  end;

  IEnumerable = Spring.IEnumerable;

  /// <summary>
  ///   Non generic interface for adding items to a collection - this can be
  ///   used for serialization.
  /// </summary>
  ICollection = interface(IEnumerable)
    ['{4E749779-0873-498E-9597-FCF2A42C3F7B}']

    /// <summary>
    ///   Adds an item to the collection.
    /// </summary>
    /// <param name="item">
    ///   The element to add to the collection
    /// </param>
    /// <returns>
    ///   True if the collection was modified, False otherwise.
    /// </returns>
    function Add(const item: TValue): Boolean;

    /// <summary>
    ///   Removes all items from the collection.
    /// </summary>
    procedure Clear;
  end;

  /// <summary>
  ///   Represents a read-only sequence of elements. Exposes the enumerator,
  ///   which supports a simple iteration over a collection of a specified
  ///   type.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of elements to enumerate.
  /// </typeparam>
  IEnumerable<T> = interface //FI:W524
    ['{A6B46D30-5B0F-495F-B7EC-46FBC5A75D24}']

    /// <summary>
    ///   Returns an enumerator that iterates through the collection.
    /// </summary>
    /// <returns>
    ///   An enumerator that can be used to iterate through the collection.
    /// </returns>
    function GetEnumerator: IEnumerator<T>;

  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetElementType: PTypeInfo;
    function GetIsEmpty: Boolean;

    /// <summary>
    ///   Attempts to determine the number of elements in the sequence without
    ///   forcing an enumeration.
    /// </summary>
    /// <returns>
    ///   When this method returns, contains the number of elements, or -1 if
    ///   the count couldn't be determined without enumeration.
    /// </returns>
    /// <remarks>
    ///   This method is primarily for internal use to provide count based
    ///   results as efficient as possible.
    /// </remarks>
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}

    /// <summary>
    ///   Returns the reference to this instance.
    /// </summary>
    /// <returns>
    ///   The instance behind this reference.
    /// </returns>
    function AsObject: TObject;

    /// <summary>
    ///   Creates a new array which is filled with the elements in the
    ///   collection.
    /// </summary>
    /// <returns>
    ///   An array that contains the elements from the sequence.
    /// </returns>
    function ToArray: TArray<T>;

    /// <summary>
    ///   Returns the specified comparer for this instance.
    /// </summary>
    /// <returns>
    ///   The specified comparer for this instance.
    /// </returns>
    function GetComparer: IComparer<T>;

    /// <summary>
    ///   Applies an accumulator function over the sequence.
    /// </summary>
    /// <param name="func">
    ///   An accumulator function to be invoked on each element.
    /// </param>
    /// <returns>
    ///   The final accumulator value.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>func</c> is <c>nil</c>.
    /// </exception>
    function Aggregate(const func: Func<T, T, T>): T;

    /// <summary>
    ///   Determines whether all elements of the sequence satisfy a condition.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   <c>True</c> if every element of the source sequence passes the test
    ///   in the specified predicate, or if the sequence is empty; otherwise, <c>
    ///   False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function All(const predicate: Predicate<T>): Boolean;

    /// <summary>
    ///   Determines whether the sequence contains any elements.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the source sequence contains any elements; otherwise, <c>
    ///   False</c>.
    /// </returns>
    function Any: Boolean; overload;

    /// <summary>
    ///   Determines whether any element of the sequence satisfies a condition.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   <c>True</c> if any elements in the source sequence pass the test in
    ///   the specified predicate; otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function Any(const predicate: Predicate<T>): Boolean; overload;

    /// <summary>
    ///   Determines whether or not the number of elements in the sequence is
    ///   greater than or equal to the given count.
    /// </summary>
    /// <param name="count">
    ///   The minimum number of items the sequence must have for this function
    ///   to return true.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the number of elements in the sequence is greater than
    ///   or equal to the given count; otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>count</c> is negative.
    /// </exception>
    function AtLeast(count: Integer): Boolean;

    /// <summary>
    ///   Determines whether or not the number of elements in the sequence is
    ///   lesser than or equal to the given count.
    /// </summary>
    /// <param name="count">
    ///   The maximun number of items the sequence must have for this function
    ///   to return true.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the number of elements in the sequence is lesser than
    ///   or equal to the given count; otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>count</c> is negative.
    /// </exception>
    function AtMost(count: Integer): Boolean;

    /// <summary>
    ///   Computes the average of the sequence.
    /// </summary>
    /// <returns>
    ///   The average of all elements in the sequence.
    /// </returns>
    /// <exception cref="Spring|ENotSupportedException">
    ///   T is not a supported numeric type.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Average: Double; overload;

    /// <summary>
    ///   Computes the average of a sequence of Integer values that are
    ///   obtained by invoking a transform function on each element of the
    ///   input sequence.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The average of the sequence of values.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    /// <exception cref="Spring|EIntOverflow">
    ///   The sum of the elements in the sequence is larger than Int64.MaxValue.
    /// </exception>
    function Average(const selector: Func<T, Integer>): Double; overload;

    /// <summary>
    ///   Computes the average of a sequence of Int64 values that are
    ///   obtained by invoking a transform function on each element of the
    ///   input sequence.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The average of the sequence of values.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    /// <exception cref="Spring|EIntOverflow">
    ///   The sum of the elements in the sequence is larger than Int64.MaxValue.
    /// </exception>
    function Average(const selector: Func<T, Int64>): Double; overload;

    /// <summary>
    ///   Computes the average of a sequence of Single values that are
    ///   obtained by invoking a transform function on each element of the
    ///   input sequence.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The average of the sequence of values.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Average(const selector: Func<T, Single>): Double; overload;

    /// <summary>
    ///   Computes the average of a sequence of Double values that are
    ///   obtained by invoking a transform function on each element of the
    ///   input sequence.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The average of the sequence of values.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Average(const selector: Func<T, Double>): Double; overload;

    /// <summary>
    ///   Computes the average of a sequence of Currency values that are
    ///   obtained by invoking a transform function on each element of the
    ///   input sequence.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The average of the sequence of values.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Average(const selector: Func<T, Currency>): Double; overload;

    /// <summary>
    ///   Determines whether or not the number of elements in the sequence is
    ///   between an inclusive range of minimum and maximum integers.
    /// </summary>
    /// <param name="min">
    ///   The minimum number of items the sequence must have for this function
    ///   to return true.
    /// </param>
    /// <param name="max">
    ///   The maximun number of items the sequence must have for this function
    ///   to return true.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the number of elements in the sequence is between
    ///   (inclusive) the min and max given integers; otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>min</c> is negative or <c>max</c> is less than min.
    /// </exception>
    function Between(min, max: Integer): Boolean;

    /// <summary>
    ///   Concatenates two sequences.
    /// </summary>
    /// <param name="second">
    ///   The sequence to concatenate to this sequence.
    /// </param>
    /// <returns>
    ///   A sequence that contains the concatenated elements of the two input
    ///   sequences.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>second</c> is <c>nil</c>.
    /// </exception>
    function Concat(const second: IEnumerable<T>): IEnumerable<T>;

    /// <summary>
    ///   Determines whether the sequence contains a specified element by using
    ///   the default equality comparer.
    /// </summary>
    /// <param name="value">
    ///   The value to locate in the sequence.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the source sequence contains an element that has the
    ///   specified value; otherwise, <c>False</c>.
    /// </returns>
    function Contains(const value: T): Boolean; overload;

    /// <summary>
    ///   Determines whether the sequence contains a specified element by using
    ///   a specified equality comparer.
    /// </summary>
    /// <param name="value">
    ///   The value to locate in the sequence.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare values.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the source sequence contains an element that has the
    ///   specified value; otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>comparer</c> is <c>nil</c>.
    /// </exception>
    function Contains(const value: T; const comparer: IEqualityComparer<T>): Boolean; overload;

    /// <summary>
    ///   Determines whether the sequence contains a specified element by using
    ///   a specified equality comparison.
    /// </summary>
    /// <param name="value">
    ///   The value to locate in the sequence.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparison to compare values.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the source sequence contains an element that has the
    ///   specified value; otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>comparer</c> is <c>nil</c>.
    /// </exception>
    function Contains(const value: T; const comparer: TEqualityComparison<T>): Boolean; overload;

    /// <summary>
    ///   Returns the elements of the sequence or the default value in a
    ///   sequence if the sequence is empty.
    /// </summary>
    /// <returns>
    ///   A sequence that contains <c>Default(T)</c> if <c>source</c> is empty;
    ///   otherwise, <c>source</c>.
    /// </returns>
    function DefaultIfEmpty: IEnumerable<T>; overload;

    /// <summary>
    ///   Returns the elements of the sequence or the specified default value
    ///   in sequence if the sequence is empty.
    /// </summary>
    /// <param name="defaultValue">
    ///   The value to return if the sequence is empty.
    /// </param>
    /// <returns>
    ///   A sequence that contains <c>defaultValue</c> if <c>source</c> is
    ///   empty; otherwise, <c>source</c>.
    /// </returns>
    function DefaultIfEmpty(const defaultValue: T): IEnumerable<T>; overload;

    /// <summary>
    ///   Returns distinct elements from a sequence by using the default
    ///   equality comparer to compare values.
    /// </summary>
    /// <returns>
    ///   A sequence that contains distinct elements from the source sequence.
    /// </returns>
    function Distinct: IEnumerable<T>; overload;

    /// <summary>
    ///   Returns distinct elements from the sequence by using a specified
    ///   equality comparer to compare values.
    /// </summary>
    /// <param name="comparer">
    ///   An equality comparer to compare values.
    /// </param>
    /// <returns>
    ///   A sequence that contains distinct elements from the source sequence.
    /// </returns>
    function Distinct(const comparer: IEqualityComparer<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Returns the element at a specified index in the sequence.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the element to retrieve.
    /// </param>
    /// <returns>
    ///   The element at the specified position in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is less than 0 or greater than or equal to the number of
    ///   elements in the sequence.
    /// </exception>
    function ElementAt(index: Integer): T;

    /// <summary>
    ///   Returns the element at a specified index in the sequence or a default
    ///   value if the index is out of range.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the element to retrieve.
    /// </param>
    /// <returns>
    ///   <c>Default(T)</c> if the index is outside the bounds of the sequence;
    ///   otherwise, the element at the specified position in the sequence.
    /// </returns>
    function ElementAtOrDefault(index: Integer): T; overload;

    /// <summary>
    ///   Returns the element at a specified index in the sequence or the
    ///   specified default value if the index is out of range.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the element to retrieve.
    /// </param>
    /// <param name="defaultValue">
    ///   The value to return if the index is out of range.
    /// </param>
    /// <returns>
    ///   <c>defaultValue</c> if the index is outside the bounds of the
    ///   sequence; otherwise, the element at the specified position in the
    ///   sequence.
    /// </returns>
    function ElementAtOrDefault(index: Integer; const defaultValue: T): T; overload;

    /// <summary>
    ///   Determines whether the sequences is equal to the specified array by
    ///   comparing the elements by using the default equality comparer for
    ///   their type.
    /// </summary>
    /// <param name="values">
    ///   An array to compare the the sequence.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the sequence is of equal length to the array and their
    ///   corresponding elements are equal according to the default equality
    ///   comparer for their type; otherwise, <c>False</c>.
    /// </returns>
    function EqualsTo(const values: array of T): Boolean; overload;

    /// <summary>
    ///   Determines whether two sequences are equal by comparing the elements
    ///   by using the default equality comparer for their type.
    /// </summary>
    /// <param name="values">
    ///   A sequence to compare to the sequence.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the two source sequences are of equal length and their
    ///   corresponding elements are equal according to the default equality
    ///   comparer for their type; otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>values</c> is <c>nil</c>.
    /// </exception>
    function EqualsTo(const values: IEnumerable<T>): Boolean; overload;

    /// <summary>
    ///   Determines whether two sequences are equal by comparing their
    ///   elements by using a specified equality comparer.
    /// </summary>
    /// <param name="values">
    ///   A sequence to compare to the sequence.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to use to compare elements.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the two source sequences are of equal length and their
    ///   corresponding elements compare equal according to comparer;
    ///   otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>values</c> is <c>nil</c>. <br /><br />-or- <br /><br /><c>comparer</c>
    ///    is <c>nil</c>.
    /// </exception>
    function EqualsTo(const values: IEnumerable<T>; const comparer: IEqualityComparer<T>): Boolean; overload;

    /// <summary>
    ///   Determines whether or not the number of elements in the sequence is
    ///   equals to the given count.
    /// </summary>
    /// <param name="count">
    ///   The exact number of items the sequence must have for this function to
    ///   return true.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the number of elements in the sequence is equals to
    ///   the given count; otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>count</c> is negative.
    /// </exception>
    function Exactly(count: Integer): Boolean;

    /// <summary>
    ///   Produces the set difference of two sequences by using the default
    ///   equality comparer to compare values.
    /// </summary>
    /// <param name="second">
    ///   A sequence whose elements that also occur in the first sequence will
    ///   cause those elements to be removed from the returned sequence.
    /// </param>
    /// <returns>
    ///   A sequence that contains the set difference of the elements of two
    ///   sequences.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>second</c> is <c>nil</c>.
    /// </exception>
    function Exclude(const second: IEnumerable<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Produces the set difference of two sequences by using the specified
    ///   equality comparer to compare values.
    /// </summary>
    /// <param name="second">
    ///   A sequence whose elements that also occur in the first sequence will
    ///   cause those elements to be removed from the returned sequence.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare values.
    /// </param>
    /// <returns>
    ///   A sequence that contains the set difference of the elements of two
    ///   sequences.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>second</c> is <c>nil</c>.
    /// </exception>
    function Exclude(const second: IEnumerable<T>; const comparer: IEqualityComparer<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Returns the first element of the sequence.
    /// </summary>
    /// <returns>
    ///   The first element in the sequence.
    /// </returns>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function First: T; overload;

    /// <summary>
    ///   Returns the first element in the sequence that satisfies a specified
    ///   condition.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   The first element in the sequence that passes the test in the
    ///   specified predicate function.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   No element satisfies the condition in <c>predicate</c>. <br /><br />
    ///   -or- <br /><br /> The sequence is empty.
    /// </exception>
    function First(const predicate: Predicate<T>): T; overload;

    /// <summary>
    ///   Returns the first element of the sequence, or a default value if the
    ///   sequence contains no elements.
    /// </summary>
    /// <returns>
    ///   <c>Default(T)</c> if source is empty; otherwise, the first element in
    ///   the sequence.
    /// </returns>
    function FirstOrDefault: T; overload;

    /// <summary>
    ///   Returns the first element of the sequence, or the specified default
    ///   value if the sequence contains no elements.
    /// </summary>
    /// <param name="defaultValue">
    ///   The default value to return if the sequence is empty.
    /// </param>
    /// <returns>
    ///   <c>defaultValue</c> if the sequence is empty; otherwise, the first
    ///   element in the sequence.
    /// </returns>
    function FirstOrDefault(const defaultValue: T): T; overload;

    /// <summary>
    ///   Returns the first element of the sequence that satisfies the
    ///   specified condition or a default value if no such element is found.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   <c>Default(T)</c> if source is empty or if no element passes the test
    ///   specified by <c>predicate</c>; otherwise, the first element in the
    ///   sequence that passes the test specified by <c>predicate</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function FirstOrDefault(const predicate: Predicate<T>): T; overload;

    /// <summary>
    ///   Returns the first element of the sequence that satisfies the
    ///   specified condition or the specified default value if no such element
    ///   is found.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <param name="defaultValue">
    ///   The default value to return if the sequence is empty or no element
    ///   passes the condition.
    /// </param>
    /// <returns>
    ///   <c>defaultValue</c> if the sequence is empty or if no element passes
    ///   the test specified by <c>predicate</c>; otherwise, the first element
    ///   in the sequence that passes the test specified by <c>predicate</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function FirstOrDefault(const predicate: Predicate<T>; const defaultValue: T): T; overload;

    /// <summary>
    ///   Performs the specified action on each element of the sequence.
    /// </summary>
    /// <param name="action">
    ///   The action to perform on each element.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>action</c> is <c>nil</c>.
    /// </exception>
    procedure ForEach(const action: Action<T>);

    /// <summary>
    ///   Produces the set intersection of two sequences by using the default
    ///   equality comparer to compare values.
    /// </summary>
    /// <param name="second">
    ///   A sequence whose distinct elements that also appear in the first
    ///   sequence will be returned.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements that form the set intersection
    ///   of two sequences.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>second</c> is <c>nil</c>.
    /// </exception>
    function Intersect(const second: IEnumerable<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Produces the set intersection of two sequences by using the specified
    ///   equality comparer to compare values.
    /// </summary>
    /// <param name="second">
    ///   A sequence whose distinct elements that also appear in the first
    ///   sequence will be returned.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare values.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements that form the set intersection
    ///   of two sequences.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>second</c> is <c>nil</c>.
    /// </exception>
    function Intersect(const second: IEnumerable<T>; const comparer: IEqualityComparer<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Returns the last element of the sequence.
    /// </summary>
    /// <returns>
    ///   The value at the last position in the sequence.
    /// </returns>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Last: T; overload;

    /// <summary>
    ///   Returns the last element of the sequence that satisfies the specified
    ///   condition.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   The last element in the sequence that passes the test in the
    ///   specified predicate function.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   No element satisfies the condition in <c>predicate</c>. <br /><br />
    ///   -or- <br /><br /> The sequence is empty.
    /// </exception>
    function Last(const predicate: Predicate<T>): T; overload;

    /// <summary>
    ///   Returns the last element of the sequence, or a default value if the
    ///   sequence contains no elements.
    /// </summary>
    /// <returns>
    ///   <c>Default(T)</c> if the sequence is empty; otherwise, the last
    ///   element in the sequence.
    /// </returns>
    function LastOrDefault: T; overload;

    /// <summary>
    ///   Returns the last element of the sequence, or the specified default
    ///   value if the sequence contains no elements.
    /// </summary>
    /// <param name="defaultValue">
    ///   The default value to return if the sequence is empty.
    /// </param>
    /// <returns>
    ///   <c>defaultValue</c> if the sequence is empty; otherwise, the last
    ///   element in the sequence.
    /// </returns>
    function LastOrDefault(const defaultValue: T): T; overload;

    /// <summary>
    ///   Returns the last element of the sequence that satisfies the specified
    ///   condition or a default value if no such element is found.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   <c>default(T)</c> if the sequence is empty or if no elements pass the
    ///   test in the predicate function; otherwise, the last element that
    ///   passes the test in the predicate function.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function LastOrDefault(const predicate: Predicate<T>): T; overload;

    /// <summary>
    ///   Returns the last element of the sequence that satisfies the specified
    ///   condition or the specified default value if no such element is found.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <param name="defaultValue">
    ///   The default value to return if the sequence is empty or no element
    ///   passes the condition.
    /// </param>
    /// <returns>
    ///   <c>default(TSource)</c> if the sequence is empty or if no element
    ///   passes the test specified by <c>predicate</c>; otherwise, the first
    ///   element in the sequence that passes the test specified by <c>
    ///   predicate</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function LastOrDefault(const predicate: Predicate<T>; const defaultValue: T): T; overload;

    /// <summary>
    ///   Returns the maximum value in the sequence.
    /// </summary>
    /// <returns>
    ///   The maximum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Max: T; overload;

    /// <summary>
    ///   Invokes a transform function on each element of the sequence and
    ///   returns the maximum Integer value.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The maximum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Max(const selector: Func<T, Integer>): Integer; overload;

    /// <summary>
    ///   Invokes a transform function on each element of the sequence and
    ///   returns the maximum Int64 value.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The maximum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Max(const selector: Func<T, Int64>): Int64; overload;

    /// <summary>
    ///   Invokes a transform function on each element of the sequence and
    ///   returns the maximum Single value.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The maximum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Max(const selector: Func<T, Single>): Single; overload;

    /// <summary>
    ///   Invokes a transform function on each element of the sequence and
    ///   returns the maximum Double value.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The maximum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Max(const selector: Func<T, Double>): Double; overload;

    /// <summary>
    ///   Invokes a transform function on each element of the sequence and
    ///   returns the maximum Currency value.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The maximum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Max(const selector: Func<T, Currency>): Currency; overload;

    /// <summary>
    ///   Returns the maximum value in the sequence by using the specified
    ///   comparer.
    /// </summary>
    /// <param name="comparer">
    ///   A comparer to compare values.
    /// </param>
    /// <returns>
    ///   The maximum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>comparer</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Max(const comparer: IComparer<T>): T; overload;

    /// <summary>
    ///   Returns the maximum value in the sequence by using the specified
    ///   comparison.
    /// </summary>
    /// <param name="comparer">
    ///   A comparison to compare values.
    /// </param>
    /// <returns>
    ///   The maximum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>comparer</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Max(const comparer: TComparison<T>): T; overload;

    /// <summary>
    ///   Returns the minimum value in the sequence.
    /// </summary>
    /// <returns>
    ///   The minimum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Min: T; overload;

    /// <summary>
    ///   Invokes a transform function on each element of the sequence and
    ///   returns the minimum Integer value.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The minimum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Min(const selector: Func<T, Integer>): Integer; overload;

    /// <summary>
    ///   Invokes a transform function on each element of the sequence and
    ///   returns the minimum Int64 value.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The minimum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Min(const selector: Func<T, Int64>): Int64; overload;

    /// <summary>
    ///   Invokes a transform function on each element of the sequence and
    ///   returns the minimum Single value.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The minimum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Min(const selector: Func<T, Single>): Single; overload;

    /// <summary>
    ///   Invokes a transform function on each element of the sequence and
    ///   returns the minimum Double value.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The minimum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Min(const selector: Func<T, Double>): Double; overload;

    /// <summary>
    ///   Invokes a transform function on each element of the sequence and
    ///   returns the minimum Currency value.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The minimum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Min(const selector: Func<T, Currency>): Currency; overload;

    /// <summary>
    ///   Returns the minimum value in the sequence by using the specified
    ///   comparer.
    /// </summary>
    /// <param name="comparer">
    ///   A comparer to compare values.
    /// </param>
    /// <returns>
    ///   The minimum value in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>comparer</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Min(const comparer: IComparer<T>): T; overload;

    /// <summary>
    ///   Returns the minimum value in the sequence by using the specified
    ///   comparison.
    /// </summary>
    /// <param name="comparer">
    ///   A comparison to compare values.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>comparer</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The sequence is empty.
    /// </exception>
    function Min(const comparer: TComparison<T>): T; overload;

    /// <summary>
    ///   Returns a sequence that lazily caches the sequence as it is iterated
    ///   for the first time, reusing the cache thereafter for future
    ///   re-iterations. If the sequence is already cached or buffered then it
    ///   is returned verbatim.
    /// </summary>
    /// <returns>
    ///   A sequence that corresponds to a cached version of the sequence.
    /// </returns>
    function Memoize: IEnumerable<T>;

    /// <summary>
    ///   Sorts the elements of the sequence in ascending order using the
    ///   default comparer for their type.
    /// </summary>
    /// <returns>
    ///   A sequence whose elements are sorted.
    /// </returns>
    function Ordered: IEnumerable<T>; overload;

    /// <summary>
    ///   Sorts the elements of the sequence in ascending order using the
    ///   specified comparer.
    /// </summary>
    /// <returns>
    ///   A sequence whose elements are sorted.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>comparer</c> is <c>nil</c>.
    /// </exception>
    function Ordered(const comparer: IComparer<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Sorts the elements of the sequence in ascending order using the
    ///   specified comparison.
    /// </summary>
    /// <returns>
    ///   A sequence whose elements are sorted.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>comparer</c> is <c>nil</c>.
    /// </exception>
    function Ordered(const comparer: TComparison<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Inverts the order of the elements in the sequence.
    /// </summary>
    /// <returns>
    ///   A sequence whose elements correspond to those of the input sequence
    ///   in reverse order.
    /// </returns>
    function Reversed: IEnumerable<T>;

    /// <summary>
    ///   Returns a sequence of elements in random order from the original.
    /// </summary>
    /// <returns>
    ///   A sequence whose elements are randomized in their order to those of
    ///   the input sequence.
    /// </returns>
    function Shuffled: IEnumerable<T>;

    /// <summary>
    ///   Returns the only element of the sequence, and raises an exception if
    ///   there is not exactly one element in the sequence.
    /// </summary>
    /// <returns>
    ///   The single element of the sequence.
    /// </returns>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The input sequence contains more than one element. <br /><br />-or- <br /><br />
    ///    The input sequence is empty.
    /// </exception>
    function Single: T; overload;

    /// <summary>
    ///   Returns the only element of the sequence that satisfies a specified
    ///   condition, and raises an exception if more than one such element
    ///   exists.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test an element for a condition.
    /// </param>
    /// <returns>
    ///   The single element of the sequence that satisfies a condition.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   No element satisfies the condition in <c>predicate</c>. <br /><br />
    ///   -or- <br /><br />More than one element satisfies the condition in <c>
    ///   predicate</c>. <br /><br />-or- <br /><br />The sequence is empty.
    /// </exception>
    function Single(const predicate: Predicate<T>): T; overload;

    /// <summary>
    ///   Returns the only element of the sequence, or a default value if the
    ///   sequence is empty; this method raises an exception if there is more
    ///   than one element in the sequence.
    /// </summary>
    /// <returns>
    ///   The single element of the input sequence, or <c>Default(T)</c> if the
    ///   sequence contains no elements.
    /// </returns>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The input sequence contains more than one element.
    /// </exception>
    function SingleOrDefault: T; overload;

    /// <summary>
    ///   Returns the only element of the sequence, or a specified default
    ///   value if the sequence is empty; this method raises an exception if
    ///   there is more than one element in the sequence.
    /// </summary>
    /// <param name="defaultValue">
    ///   The default value to return if the sequence is empty.
    /// </param>
    /// <returns>
    ///   The single element of the input sequence, or <c>defaultValue</c> if
    ///   the sequence contains no elements.
    /// </returns>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The input sequence contains more than one element.
    /// </exception>
    function SingleOrDefault(const defaultValue: T): T; overload;

    /// <summary>
    ///   Returns the only element of the sequence that satisfies a specified
    ///   condition or a default value if no such element exists; this method
    ///   raises an exception if more than one element satisfies the condition.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test an element for a condition.
    /// </param>
    /// <returns>
    ///   The single element of the input sequence that satisfies the
    ///   condition, or <c>Default(T)</c> if no such element is found.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   More than one element satisfies the condition in <c>predicate</c>.
    /// </exception>
    function SingleOrDefault(const predicate: Predicate<T>): T; overload;

    /// <summary>
    ///   Returns the only element of the sequence that satisfies a specified
    ///   condition, or a specified default value if no such element exists;
    ///   this method raises an exception if more than one element satisfies
    ///   the condition.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test an element for a condition.
    /// </param>
    /// <param name="defaultValue">
    ///   The default value to return if the sequence is empty.
    /// </param>
    /// <returns>
    ///   The single element of the input sequence that satisfies the
    ///   condition, or <c>defaultValue</c> if no such element is found.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   More than one element satisfies the condition in <c>predicate</c>.
    /// </exception>
    function SingleOrDefault(const predicate: Predicate<T>; const defaultValue: T): T; overload;

    /// <summary>
    ///   Bypasses a specified number of elements in the sequence and then
    ///   returns the remaining elements.
    /// </summary>
    /// <param name="count">
    ///   The number of elements to skip before returning the remaining
    ///   elements.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements that occur after the specified
    ///   index in the input sequence.
    /// </returns>
    function Skip(count: Integer): IEnumerable<T>;

    /// <summary>
    ///   Omits the specified number of elements at the end of the sequence.
    /// </summary>
    /// <param name="count">
    ///   The number of elements to omit from the end of the sequence.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements minus count elements from the
    ///   end of the input sequence.
    /// </returns>
    /// <remarks>
    ///   If <c>count</c> is not a positive number, this method returns the
    ///   input sequence.
    /// </remarks>
    function SkipLast(count: Integer): IEnumerable<T>;

    /// <summary>
    ///   Bypasses elements in the sequence as long as a specified condition is
    ///   true and then returns the remaining elements.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements from the input sequence
    ///   starting at the first element in the linear series that does not pass
    ///   the test specified by <c>predicate</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function SkipWhile(const predicate: Predicate<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Bypasses elements in the sequence as long as a specified condition is
    ///   true and then returns the remaining elements. The element's index is
    ///   used in the logic of the predicate function.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each source element for a condition; the second
    ///   parameter of the function represents the index of the source element.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements from the input sequence
    ///   starting at the first element in the linear series that does not pass
    ///   the test specified by <c>predicate</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function SkipWhile(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; overload;

    /// <summary>
    ///   Computes the sum of the sequence.
    /// </summary>
    /// <returns>
    ///   The sum of all elements in the sequence.
    /// </returns>
    /// <exception cref="Spring|ENotSupportedException">
    ///   T is not a supported numeric type.
    /// </exception>
    /// <remarks>
    ///   Returns 0 when the sequence is empty.
    /// </remarks>
    function Sum: T; overload;

    /// <summary>
    ///   Computes the sum of the sequence of Single values that are obtained
    ///   by invoking a transform function on each element of the input
    ///   sequence.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The sum of the projected values.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EIntOverflow">
    ///   The sum of the elements in the sequence is larger than Int32.MaxValue.
    /// </exception>
    function Sum(const selector: Func<T, Integer>): Integer; overload;

    /// <summary>
    ///   Computes the sum of the sequence of Int64 values that are obtained
    ///   by invoking a transform function on each element of the input
    ///   sequence.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The sum of the projected values.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EIntOverflow">
    ///   The sum of the elements in the sequence is larger than Int64.MaxValue.
    /// </exception>
    function Sum(const selector: Func<T, Int64>): Int64; overload;

    /// <summary>
    ///   Computes the sum of the sequence of Single values that are obtained
    ///   by invoking a transform function on each element of the input
    ///   sequence.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The sum of the projected values.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    function Sum(const selector: Func<T, Single>): Single; overload;

    /// <summary>
    ///   Computes the sum of the sequence of Double values that are obtained
    ///   by invoking a transform function on each element of the input
    ///   sequence.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The sum of the projected values.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    function Sum(const selector: Func<T, Double>): Double; overload;

    /// <summary>
    ///   Computes the sum of the sequence of Currency values that are obtained
    ///   by invoking a transform function on each element of the input
    ///   sequence.
    /// </summary>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   The sum of the projected values.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>selector</c> is <c>nil</c>.
    /// </exception>
    function Sum(const selector: Func<T, Currency>): Currency; overload;

    /// <summary>
    ///   Returns the specified number of contiguous elements from the start of
    ///   the sequence.
    /// </summary>
    /// <param name="count">
    ///   The number of elements to return.
    /// </param>
    /// <returns>
    ///   A sequence that contains the specified number of elements from the
    ///   start of the input sequence.
    /// </returns>
    /// <remarks>
    ///   If <c>count</c> is not a positive number, the sequence is not
    ///   enumerated and an empty sequence is returned.
    /// </remarks>
    function Take(count: Integer): IEnumerable<T>;

    /// <summary>
    ///   Returns the specified number of contiguous elements from the end of
    ///   the sequence.
    /// </summary>
    /// <param name="count">
    ///   The number of elements to return.
    /// </param>
    /// <returns>
    ///   A sequence that contains the specified number of elements from the
    ///   end of the input sequence.
    /// </returns>
    /// <remarks>
    ///   If <c>count</c> is not a positive number, the sequence is not
    ///   enumerated and an empty sequence is returned.
    /// </remarks>
    function TakeLast(count: Integer): IEnumerable<T>;

    /// <summary>
    ///   Returns elements from the sequence as long as a specified condition
    ///   is true.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements from the input sequence that
    ///   occur before the element at which the test no longer passes.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function TakeWhile(const predicate: Predicate<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Returns elements from the sequence as long as a specified condition
    ///   is true. The element's index is used in the logic of the predicate
    ///   function.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each source element for a condition; the second
    ///   parameter of the function represents the index of the source element.
    /// </param>
    /// <returns>
    ///   A sequence that contains elements from the input sequence that occur
    ///   before the element at which the test no longer passes.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function TakeWhile(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; overload;

    /// <summary>
    ///   Attempts to get the element at a specified index in the sequence.
    /// </summary>
    /// <param name="value">
    ///   The element at the specified position in the sequence.
    /// </param>
    /// <param name="index">
    ///   The zero-based index of the element to retrieve.
    /// </param>
    /// <returns>
    ///   <c>True</c> if <c>index</c> is inside the bounds of the sequence;
    ///   otherwise, <c>False</c>.
    /// </returns>
    function TryGetElementAt(var value: T; index: Integer): Boolean;

    /// <summary>
    ///   Attempts to get the first element in the sequence.
    /// </summary>
    /// <param name="value">
    ///   The first element in the sequence.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the sequence was not empty; otherwise, <c>False</c>.
    /// </returns>
    function TryGetFirst(var value: T): Boolean; overload;

    /// <summary>
    ///   Attempts to get the first element in the sequence that satisfies a
    ///   specified condition.
    /// </summary>
    /// <param name="value">
    ///   The first element in the sequence that passes the test in the
    ///   specified predicate function.
    /// </param>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   <c>True</c> if an element in the sequence satiesfies the condition in
    ///   <c>predicate</c>; otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function TryGetFirst(var value: T; const predicate: Predicate<T>): Boolean; overload;

    /// <summary>
    ///   Attempts to get the last element in the sequence.
    /// </summary>
    /// <param name="value">
    ///   The last element in the sequence.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the sequence was not empty; otherwise, <c>False</c>.
    /// </returns>
    function TryGetLast(var value: T): Boolean; overload;

    /// <summary>
    ///   Attempts to get the last element in the sequence that satisfies a
    ///   specified condition.
    /// </summary>
    /// <param name="value">
    ///   The last element in the sequence that passes the test in the
    ///   specified predicate function.
    /// </param>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   <c>True</c> if an element in the sequence satiesfies the condition in
    ///   <c>predicate</c>; otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function TryGetLast(var value: T; const predicate: Predicate<T>): Boolean; overload;

    /// <summary>
    ///   Attempts to get the only element in the sequence.
    /// </summary>
    /// <param name="value">
    ///   The single element of the sequence.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the sequence contains exactly one element; otherwise, <c>
    ///   False</c>.
    /// </returns>
    function TryGetSingle(var value: T): Boolean; overload;

    /// <summary>
    ///   Attempts to get the only element in the sequence that satisfies a
    ///   specified condition.
    /// </summary>
    /// <param name="value">
    ///   The single element of the sequence that satisfies a condition.
    /// </param>
    /// <param name="predicate">
    ///   A function to test an element for a condition.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the sequence contains exactly one element that
    ///   satisfies the condition in <c>predicate</c>; otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function TryGetSingle(var value: T; const predicate: Predicate<T>): Boolean; overload;

    /// <summary>
    ///   Produces the set union of two sequences by using the default equality
    ///   comparer.
    /// </summary>
    /// <param name="second">
    ///   A sequence whose distinct elements form the second set for the union.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements from both input sequences,
    ///   excluding duplicates.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>second</c> is <c>nil</c>.
    /// </exception>
    function Union(const second: IEnumerable<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Produces the set union of two sequences by using a specified equality
    ///   comparer.
    /// </summary>
    /// <param name="second">
    ///   A sequence whose distinct elements form the second set for the union.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare values.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements from both input sequences,
    ///   excluding duplicates.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>second</c> is <c>nil</c>.
    /// </exception>
    function Union(const second: IEnumerable<T>; const comparer: IEqualityComparer<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Filters the sequence based on the specified predicate.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   A sequence that contains elements from the input sequence that
    ///   satisfy the condition.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function Where(const predicate: Predicate<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Filters the sequence based on the specified predicate. Each element's
    ///   index is used in the logic of the predicate function.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each source element for a condition; the second
    ///   parameter of the function represents the index of the source element.
    /// </param>
    /// <returns>
    ///   A sequence that contains elements from the input sequence that
    ///   satisfy the condition.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function Where(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; overload;

    /// <summary>
    ///   Gets the assigned comparer. If no comparer was assigned it returns
    ///   the default comparer.
    /// </summary>
    property Comparer: IComparer<T> read GetComparer;

    /// <summary>
    ///   Returns the number of elements in the sequence.
    /// </summary>
    /// <value>
    ///   The number of elements in the sequence.
    /// </value>
    /// <remarks>
    ///   Depending on the type of the underlying collection this might cause a
    ///   full iteration of the collection. Consider using the methods <see cref="Spring.Collections|IEnumerable&lt;T&gt;.AtLeast(Integer)">
    ///   AtLeast</see>, <see cref="Spring.Collections|IEnumerable&lt;T&gt;.AtMost(Integer)">
    ///   AtMost</see>, <see cref="Spring.Collections|IEnumerable&lt;T&gt;.Between(Integer,Integer)">
    ///   Between</see> or <see cref="Spring.Collections|IEnumerable&lt;T&gt;.Exactly(Integer)">
    ///   Exactly</see> instead.
    /// </remarks>
    property Count: Integer read GetCount;

    /// <summary>
    ///   Gets the type of the elements in the sequence.
    /// </summary>
    /// <value>
    ///   The type of the elements in the sequence.
    /// </value>
    property ElementType: PTypeInfo read GetElementType;

    /// <summary>
    ///   Determines whether the sequence contains no elements.
    /// </summary>
    /// <value>
    ///   <c>True</c> if the sequence contains no elements; otherwise, <c>False</c>
    ///   .
    /// </value>
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  /// <summary>
  ///   Represents a generic read-only collection of elements.
  /// </summary>
  IReadOnlyCollection<T> = interface(IEnumerable<T>) //FI:W524
    ['{E1368FD5-02AE-4481-A9DC-96329DFF606C}']

    /// <summary>
    ///   Copies the elements of the collection to an array, starting at a
    ///   particular array index.
    /// </summary>
    /// <param name="values">
    ///   The one-dimensional array that is the destination of the elements
    ///   copied from the collection. The array must have zero-based indexing.
    /// </param>
    /// <param name="index">
    ///   The zero-based index in array at which copying begins.
    /// </param>
    /// <returns>
    ///   The number of elements that were copied.
    /// </returns>
    function CopyTo(var values: TArray<T>; index: Integer): Integer;
  end;

  /// <summary>
  ///   Represents a generic collection of elements.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of the elements in the collection.
  /// </typeparam>
  ICollection<T> = interface(IReadOnlyCollection<T>) //FI:W524
    ['{9BFD9B06-45CD-4C80-B145-01B09D432CF0}']
    // IMPORTANT NOTICE:
    // keep this in sync with ICollectionInternal in Spring.Collections.Base

  {$REGION 'Property Accessors'}
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}

    /// <summary>
    ///   Adds an item to the collection.
    /// </summary>
    /// <param name="item">
    ///   The element to add to the collection.
    /// </param>
    /// <returns>
    ///   True if the collection was modified, False otherwise.
    /// </returns>
    function Add(const item: T): Boolean;

    /// <summary>
    ///   Adds multiple items to the collection.
    /// </summary>
    /// <param name="item">
    ///   The elements to add to the collection.
    /// </param>
    procedure AddRange(const values: array of T); overload;

    /// <summary>
    ///   Adds multiple items to the collection.
    /// </summary>
    /// <param name="item">
    ///   The elements to add to the collection.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>values</c> is <c>nil</c>.
    /// </exception>
    procedure AddRange(const values: IEnumerable<T>); overload;

    /// <summary>
    ///   Removes the first occurrence of a specific element from the
    ///   collection without triggering lifetime management for objects.
    /// </summary>
    /// <param name="item">
    ///   The element to remove from the collection.
    /// </param>
    /// <returns>
    ///   The element that was removed from the collection.
    /// </returns>
    function Extract(const item: T): T;

    /// <summary>
    ///   Removes all items from the collection.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Moves the elements of the collection to the specified collection.
    /// </summary>
    /// <param name="collection">
    ///   The collection to move the elements to.
    /// </param>
    /// <returns>
    ///   The number of elements that were moved.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>collection</c> is <c>nil</c>.
    /// </exception>
    /// <remarks>
    ///   This internally uses Extract to make sure that objects from a list
    ///   with <c>OwnsObject</c> are not getting destroyed.
    /// </remarks>
    function MoveTo(const collection: ICollection<T>): Integer; overload;

    /// <summary>
    ///   Moves the elements of the collection that are matching the specified
    ///   predicate to the specified collection.
    /// </summary>
    /// <param name="collection">
    ///   The collection to move the elements to.
    /// </param>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   The number of elements that were moved.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>collection</c> is <c>nil</c>. <br /><br />-or- <br /><br /><c>
    ///   predicate</c> is <c>nil</c>.
    /// </exception>
    /// <remarks>
    ///   This internally uses Extract to make sure that objects from a list
    ///   with <c>OwnsObject</c> are not getting destroyed.
    /// </remarks>
    function MoveTo(const collection: ICollection<T>;
      const predicate: Predicate<T>): Integer; overload;

    /// <summary>
    ///   Removes the first occurrence of a specific element from the
    ///   collection.
    /// </summary>
    /// <param name="item">
    ///   The element to remove from the collection.
    /// </param>
    /// <returns>
    ///   <c>True</c> if <c>item</c> was successfully removed from the
    ///   collection; otherwise, <c>False</c>. This method also returns <c>
    ///   False</c> if <c>item</c> is not found in the collection.
    /// </returns>
    function Remove(const item: T): Boolean;

    /// <summary>
    ///   Removes all the elements that match the conditions defined by the
    ///   specified predicate.
    /// </summary>
    /// <param name="predicate">
    ///   The predicate that defines the conditions of the elements to remove.
    /// </param>
    /// <returns>
    ///   The number of elements removed from the collection.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function RemoveAll(const predicate: Predicate<T>): Integer;

    /// <summary>
    ///   Removes the first occurrence of each element from the collection.
    /// </summary>
    /// <param name="values">
    ///   The elements to remove from the collection.
    /// </param>
    /// <returns>
    ///   The number of elements removed from the collection.
    /// </returns>
    function RemoveRange(const values: array of T): Integer; overload;

    /// <summary>
    ///   Removes the first occurrence of each element from the collection.
    /// </summary>
    /// <param name="values">
    ///   The elements to remove from the collection.
    /// </param>
    /// <returns>
    ///   The number of elements removed from the collection.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>values</c> is <c>nil</c>.
    /// </exception>
    function RemoveRange(const values: IEnumerable<T>): Integer; overload;

    /// <summary>
    ///   Removes all the elements that match the conditions defined by the
    ///   specified predicate without triggering lifetime management for
    ///   objects.
    /// </summary>
    /// <param name="predicate">
    ///   The predicate that defines the conditions of the elements to remove.
    /// </param>
    /// <returns>
    ///   The elements that were removed from the collection.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>predicate</c> is <c>nil</c>.
    /// </exception>
    function ExtractAll(const predicate: Predicate<T>): TArray<T>;

    /// <summary>
    ///   Removes the first occurrence of each element from the collection
    ///   without triggering lifetime management for objects.
    /// </summary>
    /// <param name="values">
    ///   The elements to remove from the collection.
    /// </param>
    procedure ExtractRange(const values: array of T); overload;

    /// <summary>
    ///   Removes the first occurrence of each element from the collection
    ///   without triggering lifetime management for objects.
    /// </summary>
    /// <param name="values">
    ///   The elements to remove from the collection.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>values</c> is <c>nil</c>.
    /// </exception>
    procedure ExtractRange(const values: IEnumerable<T>); overload;

    /// <summary>
    ///   Occurs when an item is added, removed, or moved, or the entire list
    ///   is refreshed.
    /// </summary>
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  /// <summary>
  ///   Represents a read-only collection of elements that can be accessed by
  ///   index.
  /// </summary>
  IReadOnlyList<T> = interface(IReadOnlyCollection<T>) //FI:W524
    ['{82A74ABB-509E-4AC0-9268-A993E7DC3AB3}']
  {$REGION 'Property Accessors'}
    function GetItem(index: Integer): T;
  {$ENDREGION}

    /// <summary>
    ///   Determines the index of a specific item in the list.
    /// </summary>
    /// <param name="item">
    ///   The element to locate in the list.
    /// </param>
    /// <returns>
    ///   The index of <c>item</c> if found in the list; otherwise, -1.
    /// </returns>
    function IndexOf(const item: T): Integer; overload;

    /// <summary>
    ///   Determines the index of a specific item in the list starting at the
    ///   specified index.
    /// </summary>
    /// <param name="item">
    ///   The element to locate in the list.
    /// </param>
    /// <param name="index">
    ///   The index where to start to locate the specified item in the list.
    /// </param>
    /// <returns>
    ///   The index of <c>item</c> if found in the list; otherwise, -1.
    /// </returns>
    function IndexOf(const item: T; index: Integer): Integer; overload;

    /// <summary>
    ///   Determines the index of a specific item in the list starting at the
    ///   specified index and for the specified count of elements.
    /// </summary>
    /// <param name="item">
    ///   The element to locate in the list.
    /// </param>
    /// <param name="index">
    ///   The index where to start to locate the specified item in the list.
    /// </param>
    /// <param name="count">
    ///   The number of items to compare item with.
    /// </param>
    /// <returns>
    ///   The index of <c>item</c> if found in the list; otherwise, -1.
    /// </returns>
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    /// <summary>
    ///   Gets the element at the specified index in the list.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the element to get.
    /// </param>
    /// <value>
    ///   The element at the specified index.
    /// </value>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the list.
    /// </exception>
    property Items[index: Integer]: T read GetItem; default;
  end;

  /// <summary>
  ///   Represents a collection of elements that can be individually accessed
  ///   by index.
  /// </summary>
  IList<T> = interface(ICollection<T>) //FI:W524
    ['{B6B4E1E1-0D29-40E1-854C-A93DEA8D1AA5}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItem(index: Integer): T;
    function GetOwnsObjects: Boolean;
    procedure SetCapacity(value: Integer);
    procedure SetCount(value: Integer);
    procedure SetItem(index: Integer; const item: T);
    procedure SetOwnsObjects(value: Boolean);
  {$ENDREGION}

    /// <summary>
    ///   Adds an item to the list.
    /// </summary>
    /// <param name="item">
    ///   The element to add to the list.
    /// </param>
    /// <returns>
    ///   The index of the item in the list.
    /// </returns>
    function Add(const item: T): Integer;

    /// <summary>
    ///   Inserts an item to the list at the specified index.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index at which item should be inserted.
    /// </param>
    /// <param name="item">
    ///   The element to insert into the list.
    /// </param>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the list.
    /// </exception>
    procedure Insert(index: Integer; const item: T);

    /// <summary>
    ///   Inserts multiple items to the list at the specified index.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index at which item should be inserted.
    /// </param>
    /// <param name="item">
    ///   The elements to insert into the list.
    /// </param>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the list.
    /// </exception>
    procedure InsertRange(index: Integer; const values: array of T); overload;

    /// <summary>
    ///   Inserts multiple items to the list at the specified index.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index at which item should be inserted.
    /// </param>
    /// <param name="item">
    ///   The elements to insert into the list.
    /// </param>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the list.
    /// </exception>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>values</c> is <c>nil</c>.
    /// </exception>
    procedure InsertRange(index: Integer; const values: IEnumerable<T>); overload;

    /// <summary>
    ///   Removes the item at the specified index.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the item to remove.
    /// </param>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the list.
    /// </exception>
    procedure Delete(index: Integer);

    /// <summary>
    ///   Removes a specified number of items at the specified index.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the item to remove.
    /// </param>
    /// <param name="count">
    ///   The number of items to remove.
    /// </param>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the list. <br /><br /><c>count</c>
    ///    exceeds the valid range in the list.
    /// </exception>
    procedure DeleteRange(index, count: Integer);

    /// <summary>
    ///   Removes the item at the specified index without triggering lifetime
    ///   management for objects.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the item to remove.
    /// </param>
    /// <returns>
    ///   The element that was removed from the list.
    /// </returns>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the list.
    /// </exception>
    function ExtractAt(index: Integer): T;

    /// <summary>
    ///   Removes a specified number of items at the specified index without
    ///   triggering lifetime management for objects.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the item to remove.
    /// </param>
    /// <param name="count">
    ///   The number of items to remove.
    /// </param>
    /// <returns>
    ///   The elements that were removed from the list.
    /// </returns>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the list. <br /><br /><c>count</c>
    ///    exceeds the valid range in the list.
    /// </exception>
    function ExtractRange(index, count: Integer): TArray<T>; overload;

    /// <summary>
    ///   Creates a shallow copy of a range of elements in the original list.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index at which the range starts.
    /// </param>
    /// <param name="count">
    ///   The number of elements in the range.
    /// </param>
    /// <returns>
    ///   A shallow copy of a range of elements in the original list.
    /// </returns>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is less than 0. <br /><br />-or- <br /><br /><c>count</c>
    ///    is less than 0. <br /><br />-or- <br /><br /><c>index</c> and <c>
    ///   count</c> do not denote a valid range of elements in the list.
    /// </exception>
    /// <remarks>
    ///   A shallow copy of a collection of reference types, or a subset of
    ///   that collection, contains only the references to the elements of the
    ///   collection. The objects themselves are not copied. The references in
    ///   the new list point to the same objects as the references in the
    ///   original list. Also if the original list has ownership over its
    ///   elements.
    /// </remarks>
    function GetRange(index, count: Integer): IList<T>;

    /// <summary>
    ///   Exchanges the position of two elements at the given indixes.
    /// </summary>
    /// <param name="index1">
    ///   Index of the first element.
    /// </param>
    /// <param name="index2">
    ///   Index of the second element.
    /// </param>
    procedure Exchange(index1, index2: Integer);

    /// <summary>
    ///   Moves an element at the given index to another position.
    /// </summary>
    /// <param name="sourceIndex">
    ///   Index of the element to be moved.
    /// </param>
    /// <param name="targetIndex">
    ///   Index where the element will be moved.
    /// </param>
    /// <remarks>
    ///   The distance an element travels is always equal to the difference
    ///   between <c>sourceIndex</c> and <c>targetIndex</c>.
    /// </remarks>
    procedure Move(sourceIndex, targetIndex: Integer);

    /// <summary>
    ///   Reverses the order of the elements in the entire list.
    /// </summary>
    procedure Reverse; overload;

    /// <summary>
    ///   Reverses the order of the elements in the specified range.
    /// </summary>
    /// <param name="index">
    ///   The zero-based starting index of the range to reverse.
    /// </param>
    /// <param name="count">
    ///   The number of elements in the range to reverse.
    /// </param>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is less than 0. <br /><br />-or- <br /><br /><c>count</c>
    ///    is less than 0. <br /><br />-or- <br /><br /><c>index</c> and <c>
    ///   count</c> do not denote a valid range of elements in the list.
    /// </exception>
    procedure Reverse(index, count: Integer); overload;

    /// <summary>
    ///   Sorts the elements in the entire list using the comparer that was
    ///   specified at construction of the list.
    /// </summary>
    procedure Sort; overload;

    /// <summary>
    ///   Sorts the elements in the entire list using the specified comparer.
    /// </summary>
    /// <param name="comparer">
    ///   The comparer to use when comparing elements.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>comparer</c> is <c>nil</c>.
    /// </exception>
    procedure Sort(const comparer: IComparer<T>); overload;

    /// <summary>
    ///   Sorts the elements in the entire list using the specified comparison.
    /// </summary>
    /// <param name="comparer">
    ///   The comparison to use when comparing elements.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>comparer</c> is <c>nil</c>.
    /// </exception>
    procedure Sort(const comparer: TComparison<T>); overload;

    /// <summary>
    ///   Sorts the elements in a range of elements in list using the specified
    ///   comparer.
    /// </summary>
    /// <param name="comparer">
    ///   The comparer to use when comparing elements.
    /// </param>
    /// <param name="index">
    ///   The zero-based starting index of the range to sort.
    /// </param>
    /// <param name="count">
    ///   The length of the range to sort.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>comparer</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is less than 0. <br /><br />-or- <br /><br /><c>count</c>
    ///    is less than 0. <br /><br />-or- <br /><br /><c>index</c> and <c>
    ///   count</c> do not specify a valid range in the list.
    /// </exception>
    procedure Sort(const comparer: IComparer<T>; index, count: Integer); overload;

    /// <summary>
    ///   Sorts the elements in a range of elements in list using the specified
    ///   comparison.
    /// </summary>
    /// <param name="comparer">
    ///   The comparer to use when comparing elements.
    /// </param>
    /// <param name="index">
    ///   The zero-based starting index of the range to sort.
    /// </param>
    /// <param name="count">
    ///   The length of the range to sort.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>comparer</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is less than 0. <br /><br />-or- <br /><br /><c>count</c>
    ///    is less than 0. <br /><br />-or- <br /><br /><c>index</c> and <c>
    ///   count</c> do not specify a valid range in the list.
    /// </exception>
    procedure Sort(const comparer: TComparison<T>; index, count: Integer); overload;

    /// <summary>
    ///   Determines the index of a specific item in the list.
    /// </summary>
    /// <param name="item">
    ///   The element to locate in the list.
    /// </param>
    /// <returns>
    ///   The index of <c>item</c> if found in the list; otherwise, -1.
    /// </returns>
    function IndexOf(const item: T): Integer; overload;

    /// <summary>
    ///   Determines the index of a specific item in the list starting at the
    ///   specified index.
    /// </summary>
    /// <param name="item">
    ///   The element to locate in the list.
    /// </param>
    /// <param name="index">
    ///   The index where to start to locate the specified item in the list.
    /// </param>
    /// <returns>
    ///   The index of <c>item</c> if found in the list; otherwise, -1.
    /// </returns>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the list.
    /// </exception>
    function IndexOf(const item: T; index: Integer): Integer; overload;

    /// <summary>
    ///   Determines the index of a specific item in the list starting at the
    ///   specified index and for the specified count of elements.
    /// </summary>
    /// <param name="item">
    ///   The element to locate in the list.
    /// </param>
    /// <param name="index">
    ///   The index where to start to locate the specified item in the list.
    /// </param>
    /// <param name="count">
    ///   The number of items to compare item with.
    /// </param>
    /// <returns>
    ///   The index of <c>item</c> if found in the list; otherwise, -1.
    /// </returns>
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    /// <summary>
    ///   Determines the index of a specific item in the list starting at the
    ///   end of the list.
    /// </summary>
    /// <param name="item">
    ///   The element to locate in the list.
    /// </param>
    /// <returns>
    ///   The index of <c>item</c> if found in the list; otherwise, -1.
    /// </returns>
    function LastIndexOf(const item: T): Integer; overload;

    /// <summary>
    ///   Determines the index of a specific item in the list starting at the
    ///   specified index towards the start of the list.
    /// </summary>
    /// <param name="item">
    ///   The element to locate in the list.
    /// </param>
    /// <param name="index">
    ///   The index where to start to locate the specified item in the list.
    /// </param>
    /// <returns>
    ///   The index of <c>item</c> if found in the list; otherwise, -1.
    /// </returns>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the list.
    /// </exception>
    function LastIndexOf(const item: T; index: Integer): Integer; overload;

    /// <summary>
    ///   Determines the index of a specific item in the list starting at the
    ///   specified index and for the specified count of elements towards the
    ///   start of the list.
    /// </summary>
    /// <param name="item">
    ///   The element to locate in the list.
    /// </param>
    /// <param name="index">
    ///   The index where to start to locate the specified item in the list.
    /// </param>
    /// <param name="count">
    ///   The number of items to compare item with.
    /// </param>
    /// <returns>
    ///   The index of <c>item</c> if found in the list; otherwise, -1.
    /// </returns>
    function LastIndexOf(const item: T; index, count: Integer): Integer; overload;

    /// <summary>
    ///   Returns the list as read-only list.
    /// </summary>
    /// <returns>
    ///   The list as read-only list.
    /// </returns>
    /// <remarks>
    ///   This method will not perform a copy but will return the same instance
    ///   as <see cref="Spring.Collections|IReadOnlyList&lt;T&gt;" />.
    /// </remarks>
    function AsReadOnly: IReadOnlyList<T>;

    /// <summary>
    ///   Sets the capacity to the actual number of elements in the list.
    /// </summary>
    procedure TrimExcess;

    /// <summary>
    ///   Gets or sets the total number of elements the internal data structure
    ///   can hold without resizing.
    /// </summary>
    /// <value>
    ///   The number of elements that the list can contain before resizing is
    ///   required.
    /// </value>
    property Capacity: Integer read GetCapacity write SetCapacity;

    /// <summary>
    ///   Gets the number of elements contained in the list.
    /// </summary>
    /// <value>
    ///   The number of elements contained in the list.
    /// </value>
    property Count: Integer read GetCount write SetCount;

    /// <summary>
    ///   Gets or sets the element at the specified index in the list.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the element to get.
    /// </param>
    /// <value>
    ///   The element at the specified index.
    /// </value>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the list.
    /// </exception>
    property Items[index: Integer]: T read GetItem write SetItem; default;

    /// <summary>
    ///   Gets or sets the state of lifetime management for objects.
    /// </summary>
    /// <value>
    ///   The state of lifetime management for objects of the list.
    /// </value>
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  IObjectList = interface(IList<TObject>)
    ['{78A32DC5-1A5B-4191-9CA5-006CD85CF1AA}']
    // DO NOT ADD ANY METHODS HERE!!!
  end;

  IInterfaceList = interface(IList<IInterface>)
    ['{B6BF9A6E-797C-4982-8D0D-B935E43D917E}']
    // DO NOT ADD ANY METHODS HERE!!!
  end;

  ILinkedList<T> = interface;

  /// <summary>
  ///   Represents a node in a <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
  ///    . This class cannot be inherited.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the element type of the linked list.
  /// </typeparam>
  TLinkedListNode<T> = class sealed
  protected
    fList: Pointer;
    fNext: TLinkedListNode<T>;
    fPrev: TLinkedListNode<T>;
    fItem: T;
    function GetList: ILinkedList<T>;
    function GetNext: TLinkedListNode<T>;
    function GetPrevious: TLinkedListNode<T>;
  public
    constructor Create(const value: T);

    property List: ILinkedList<T> read GetList;

    /// <summary>
    ///   Gets the next node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <value>
    ///   A reference to the next node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    , or <c>nil</c> if the current node is the last element (Last) of
    ///   the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </value>
    property Next: TLinkedListNode<T> read GetNext;

    /// <summary>
    ///   Gets the previous node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <value>
    ///   A reference to the previous node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    , or null if the current node is the first element (First) of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </value>
    property Previous: TLinkedListNode<T> read GetPrevious;

    /// <summary>
    ///   Gets the value contained in the node.
    /// </summary>
    /// <value>
    ///   The value contained in the node.
    /// </value>
    property Value: T read fItem write fItem;
  end;

  /// <summary>
  ///   Represents a doubly linked list.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the element type of the linked list.
  /// </typeparam>
  ILinkedList<T> = interface(ICollection<T>) //FI:W524
    ['{73351AD9-15A5-4DA0-9BB7-D8FF66A3077E}']
  {$REGION 'Property Accessors'}
    function GetFirst: TLinkedListNode<T>;
    function GetLast: TLinkedListNode<T>;
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}

    /// <summary>
    ///   Adds the specified new node after the specified existing node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> after
    ///   which to insert <c>newNode</c>.
    /// </param>
    /// <param name="value">
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   add to the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>node</c> is <c>nil</c>. <br /><br />-or- <br /><br /><c>newNode</c>
    ///    is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <c>node</c> is not in the current <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    . <br /><br />-or- <br /><br /><c>newNode</c> belongs to another <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </exception>
    procedure AddAfter(const node: TLinkedListNode<T>; const newNode: TLinkedListNode<T>); overload;

    /// <summary>
    ///   Adds a new node containing the specified value after the specified
    ///   existing node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> after
    ///   which to insert a new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///    containing <c>value</c>.
    /// </param>
    /// <param name="value">
    ///   The value to add to the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <returns>
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   containing <c>value</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>node</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <c>node</c> is not in the current <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </exception>
    function AddAfter(const node: TLinkedListNode<T>; const value: T): TLinkedListNode<T>; overload;

    /// <summary>
    ///   Adds the specified new node before the specified existing node in the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> before
    ///   which to insert <c>newNode</c>.
    /// </param>
    /// <param name="newNode">
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   add to the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>node</c> is <c>nil</c>. <br /><br />-or- <br /><br /><c>newNode</c>
    ///    is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <c>node</c> is not in the current <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    . <br /><br />-or- <br /><br /><c>newNode</c> belongs to another <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </exception>
    procedure AddBefore(const node: TLinkedListNode<T>; const newNode: TLinkedListNode<T>); overload;

    /// <summary>
    ///   Adds a new node containing the specified value before the specified
    ///   existing node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> before
    ///   which to insert a new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///    containing <c>value</c>.
    /// </param>
    /// <param name="value">
    ///   The value to add to the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <returns>
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   containing <c>value</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>node</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <c>node</c> is not in the current <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </exception>
    function AddBefore(const node: TLinkedListNode<T>; const value: T): TLinkedListNode<T>; overload;

    /// <summary>
    ///   Adds the specified new node at the start of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="node">
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   add at the start of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>node</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <c>node</c> belongs to another <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </exception>
    procedure AddFirst(const node: TLinkedListNode<T>); overload;

    /// <summary>
    ///   Adds a new node containing the specified value at the start of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="value">
    ///   The value to add at the start of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <returns>
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   containing <c>value</c>.
    /// </returns>
    function AddFirst(const value: T): TLinkedListNode<T>; overload;

    /// <summary>
    ///   Adds the specified new node at the end of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="node">
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   add at the end of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>node</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <c>node</c> belongs to another <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </exception>
    procedure AddLast(const node: TLinkedListNode<T>); overload;

    /// <summary>
    ///   Adds a new node containing the specified value at the end of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="value">
    ///   The value to add at the end of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <returns>
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   containing <c>value</c>.
    /// </returns>
    function AddLast(const value: T): TLinkedListNode<T>; overload;

    /// <summary>
    ///   Finds the first node that contains the specified value.
    /// </summary>
    /// <param name="value">
    ///   The value to locate in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <returns>
    ///   The first <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   that contains the specified value, if found; otherwise, <c>nil</c>.
    /// </returns>
    function Find(const value: T): TLinkedListNode<T>;

    /// <summary>
    ///   Finds the last node that contains the specified value.
    /// </summary>
    /// <param name="value">
    ///   The value to locate in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <returns>
    ///   The last <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   that contains the specified value, if found; otherwise, <c>nil</c>.
    /// </returns>
    function FindLast(const value: T): TLinkedListNode<T>;

    /// <summary>
    ///   Removes the specified node from the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   remove from the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>node</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <c>node</c> is not in the current <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </exception>
    procedure Remove(const node: TLinkedListNode<T>); overload;

    /// <summary>
    ///   Removes the node at the start of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The <see cref="Spring.Collections|ILinkedList&lt;T&gt;" /> is empty.
    /// </exception>
    procedure RemoveFirst;

    /// <summary>
    ///   Removes the node at the end of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The <see cref="Spring.Collections|ILinkedList&lt;T&gt;" /> is empty.
    /// </exception>
    procedure RemoveLast;

    {$WARNINGS OFF}
    /// <summary>
    ///   Gets the first node of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <value>
    ///   The first <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </value>
    property First: TLinkedListNode<T> read GetFirst;

    /// <summary>
    ///   Gets the last node of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <value>
    ///   The last <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </value>
    property Last: TLinkedListNode<T> read GetLast;
    {$WARNINGS ON}

    /// <summary>
    ///   Occurs when an item is added, removed, or moved, or the entire list
    ///   is refreshed.
    /// </summary>
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  /// <summary>
  ///   Represents a generic read-only collection of key/value pairs.
  /// </summary>
  /// <typeparam name="TKey">
  ///   The type of keys in the map.
  /// </typeparam>
  /// <typeparam name="TValue">
  ///   The type of values in the map.
  /// </typeparam>
  IReadOnlyMap<TKey, TValue> = interface(IReadOnlyCollection<TPair<TKey, TValue>>) //FI:W524
    ['{1FBECEB8-582E-4108-BB44-F21A06FE425B}']
  {$REGION 'Property Accessors'}
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetKeyType: PTypeInfo;
    function GetValues: IReadOnlyCollection<TValue>;
    function GetValueType: PTypeInfo;
  {$ENDREGION}

    /// <summary>
    ///   Determines whether the map contains the element with the specified
    ///   key/value pair.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to locate in the map.
    /// </param>
    /// <param name="value">
    ///   The value of the element to locate in the map.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the map contains an element with the specified key and
    ///   value; otherwise, <c>False</c>.
    /// </returns>
    function Contains(const key: TKey; const value: TValue): Boolean;

    /// <summary>
    ///   Determines whether the map contains an element with the specified
    ///   key.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to locate in the map.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the map contains an element with the specified key;
    ///   otherwise, <c>False</c>.
    /// </returns>
    function ContainsKey(const key: TKey): Boolean;

    /// <summary>
    ///   Determines whether the map contains an element with the specified
    ///   value.
    /// </summary>
    /// <param name="value">
    ///   The value of the element to locate in the map.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the map contains an element with the specified value;
    ///   otherwise, <c>False</c>.
    /// </returns>
    function ContainsValue(const value: TValue): Boolean;

    /// <summary>
    ///   Gets a collection containing the keys of the map.
    /// </summary>
    /// <value>
    ///   A read-only collection containing the keys of the map.
    /// </value>
    property Keys: IReadOnlyCollection<TKey> read GetKeys;

    /// <summary>
    ///   Gets a collection containing the values in the map.
    /// </summary>
    /// <value>
    ///   A read-only collection containing the values in the map.
    /// </value>
    property Values: IReadOnlyCollection<TValue> read GetValues;

    /// <summary>
    ///   Gets the type of the key of the elements in the map.
    /// </summary>
    /// <value>
    ///   The type of the key of the elements in the map.
    /// </value>
    property KeyType: PTypeInfo read GetKeyType;

    /// <summary>
    ///   Gets the type of the value of the elements in the map.
    /// </summary>
    /// <value>
    ///   The type of the value of the elements in the map.
    /// </value>
    property ValueType: PTypeInfo read GetValueType;
  end;

  /// <summary>
  ///   Represents a generic read-only collection of key/value pairs.
  /// </summary>
  /// <typeparam name="TKey">
  ///   The type of keys in the read-only dictionary.
  /// </typeparam>
  /// <typeparam name="TValue">
  ///   The type of values in the read-only dictionary.
  /// </typeparam>
  IReadOnlyDictionary<TKey, TValue> = interface(IReadOnlyMap<TKey, TValue>) //FI:W524
    ['{39F7C68B-373E-4758-808C-705D3978E38F}']
  {$REGION 'Property Accessors'}
    function GetItem(const key: TKey): TValue;
  {$ENDREGION}

    /// <summary>
    ///   Gets the value for a given key if a matching key exists in the
    ///   dictionary; returns the default value otherwise.
    /// </summary>
    function GetValueOrDefault(const key: TKey): TValue; overload;

    /// <summary>
    ///   Gets the value for a given key if a matching key exists in the
    ///   dictionary; returns the given default value otherwise.
    /// </summary>
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;

    /// <summary>
    ///   Attempts to get the value associated with the specified key.
    /// </summary>
    /// <param name="key">
    ///   The key whose value to get.
    /// </param>
    /// <param name="value">
    ///   When this method returns, the value associated with the specified
    ///   key, if the key is found; otherwise, the default value for the type
    ///   of the value parameter.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the specified key was found; otherwise, <c>False</c>.
    /// </returns>
    function TryGetValue(const key: TKey; var value: TValue): Boolean;

    /// <summary>
    ///   Gets the value associated with the specified key.
    /// </summary>
    /// <param name="key">
    ///   The key of the value to get.
    /// </param>
    /// <value>
    ///   The value associated with the specified key. If the specified key is
    ///   not found, throws an <see cref="Spring|EKeyNotFoundException" />.
    /// </value>
    /// <exception cref="Spring|EKeyNotFoundException">
    ///   <c>key</c> does not exist in the dictionary.
    /// </exception>
    property Items[const key: TKey]: TValue read GetItem; default;
  end;

  /// <summary>
  ///   Represents a generic read-only collection of key/value pairs that
  ///   preserves insertion order.
  /// </summary>
  IReadOnlyOrderedDictionary<TKey, TValue> = interface(IReadOnlyDictionary<TKey, TValue>) //FI:W524
    ['{9040B3AB-0E85-4945-9894-A229E6422126}']
  {$REGION 'Property Accessors'}
    function GetItemByIndex(index: Integer): TPair<TKey, TValue>;
  {$ENDREGION}

    /// <summary>
    ///   Determines the index of a specific element in the dictionary.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to locate in the dictionary.
    /// </param>
    /// <returns>
    ///   The index of the element with the specified key if found in the
    ///   dictionary; otherwise, -1.
    /// </returns>
    function IndexOf(const key: TKey): Integer;

    /// <summary>
    ///   Gets the element at the specified index in the dictionary.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the element to get.
    /// </param>
    /// <value>
    ///   The element at the specified index.
    /// </value>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the dictionary.
    /// </exception>
    property Items[index: Integer]: TPair<TKey, TValue> read GetItemByIndex;
  end;

  /// <summary>
  ///   Represents a generic collection of key/value pairs.
  /// </summary>
  /// <typeparam name="TKey">
  ///   The type of keys in the map.
  /// </typeparam>
  /// <typeparam name="TValue">
  ///   The type of values in the map.
  /// </typeparam>
  IMap<TKey, TValue> = interface(ICollection<TPair<TKey, TValue>>) //FI:W524
    ['{94262688-16E4-4092-926B-7B17FEF94A86}']
  {$REGION 'Property Accessors'}
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetKeyType: PTypeInfo;
    function GetOnKeyChanged: ICollectionChangedEvent<TKey>;
    function GetOnValueChanged: ICollectionChangedEvent<TValue>;
    function GetValues: IReadOnlyCollection<TValue>;
    function GetValueType: PTypeInfo;
  {$ENDREGION}

    /// <summary>
    ///   Adds an element with the specified key and value to the collection.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to add.
    /// </param>
    /// <param name="value">
    ///   The value of the element to add.
    /// </param>
    procedure Add(const key: TKey; const value: TValue); overload;

    /// <summary>
    ///   Attempts to add an element with the specified key and value to the
    ///   collection.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to add.
    /// </param>
    /// <param name="value">
    ///   The value of the element to add.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the key/value pair was added to the map; otherwise, <c>
    ///   False</c>.
    /// </returns>
    function TryAdd(const key: TKey; const value: TValue): Boolean;

    /// <summary>
    ///   Removes the element with the specified key from the collection.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to remove.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the element is successfully removed; otherwise, <c>
    ///   False</c>. This method returns <c>False</c> if <c>key</c> was not
    ///   found.
    /// </returns>
    function Remove(const key: TKey): Boolean; overload;

    /// <summary>
    ///   Removes the element with the specified key and value from the
    ///   collection.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to remove.
    /// </param>
    /// <param name="value">
    ///   The value of the element to remove.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the element was successfully removed; otherwise, <c>
    ///   False</c> . This method returns <c>False</c> if <c>key</c> was not
    ///   found.
    /// </returns>
    function Remove(const key: TKey; const value: TValue): Boolean; overload;

    /// <summary>
    ///   Removes the elements with the specified keys from the collection.
    /// </summary>
    /// <param name="keys">
    ///   The keys of the elements to remove.
    /// </param>
    /// <returns>
    ///   The number of elements that were actually removed.
    /// </returns>
    function RemoveRange(const keys: array of TKey): Integer; overload;

    /// <summary>
    ///   Removes the elements with the specified keys from the collection.
    /// </summary>
    /// <param name="keys">
    ///   The keys of the elements to remove.
    /// </param>
    /// <returns>
    ///   The number of elements that were actually removed.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>keys</c> is <c>nil</c>.
    /// </exception>
    function RemoveRange(const keys: IEnumerable<TKey>): Integer; overload;

    /// <summary>
    ///   Removes the element associated with the specified key if it matches
    ///   the specified value without triggering lifetime management for
    ///   objects.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to remove.
    /// </param>
    /// <param name="value">
    ///   The value of the element to remove.
    /// </param>
    /// <returns>
    ///   The removed key/value pair for the specified key if it existed;
    ///   otherwise, <c>Default(TPair&lt;TKey,TValue&gt;)</c>.
    /// </returns>
    function Extract(const key: TKey; const value: TValue): TPair<TKey, TValue>;

    /// <summary>
    ///   Determines whether the map contains an element with the specified
    ///   key/value pair.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to locate in the map.
    /// </param>
    /// <param name="value">
    ///   The value of the element to locate in the map.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the map contains an element with the specified key and
    ///   value; otherwise, <c>False</c>.
    /// </returns>
    function Contains(const key: TKey; const value: TValue): Boolean; overload;

    /// <summary>
    ///   Determines whether the map contains an element with the specified
    ///   key.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to locate in the map.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the map contains an element with the specified key;
    ///   otherwise, <c>False</c>.
    /// </returns>
    function ContainsKey(const key: TKey): Boolean;

    /// <summary>
    ///   Determines whether the map contains an element with the specified
    ///   value.
    /// </summary>
    /// <param name="value">
    ///   The value of the element to locate in the map.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the map contains an element with the specified value;
    ///   otherwise, <c>False</c>.
    /// </returns>
    function ContainsValue(const value: TValue): Boolean;

    /// <summary>
    ///   Gets a collection containing the keys of the map.
    /// </summary>
    /// <value>
    ///   A read-only collection containing the keys of the map.
    /// </value>
    property Keys: IReadOnlyCollection<TKey> read GetKeys;

    /// <summary>
    ///   Gets a collection containing the values in the map.
    /// </summary>
    /// <value>
    ///   A read-only collection containing the values of the map.
    /// </value>
    property Values: IReadOnlyCollection<TValue> read GetValues;

    property OnKeyChanged: ICollectionChangedEvent<TKey> read GetOnKeyChanged;
    property OnValueChanged: ICollectionChangedEvent<TValue> read GetOnValueChanged;

    /// <summary>
    ///   Gets the type of the key of the elements in the map.
    /// </summary>
    /// <value>
    ///   The type of the key of the elements in the map.
    /// </value>
    property KeyType: PTypeInfo read GetKeyType;

    /// <summary>
    ///   Gets the type of the value of the elements in the map.
    /// </summary>
    /// <value>
    ///   The type of the value of the elements in the map.
    /// </value>
    property ValueType: PTypeInfo read GetValueType;
  end;

  /// <summary>
  ///   Represents a generic collection of key/value pairs.
  /// </summary>
  /// <typeparam name="TKey">
  ///   The type of keys in the dictionary.
  /// </typeparam>
  /// <typeparam name="TValue">
  ///   The type of values in the dictionary.
  /// </typeparam>
  IDictionary<TKey, TValue> = interface(IMap<TKey, TValue>) //FI:W524
    ['{7F0D544F-6A59-4FA0-9C96-DB09029CC835}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetItem(const key: TKey): TValue;
    procedure SetCapacity(value: Integer);
    procedure SetItem(const key: TKey; const value: TValue);
  {$ENDREGION}

    /// <summary>
    ///   Adds the specified key and value to the dictionary or sets the value
    ///   associated with the specified key.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to add or set.
    /// </param>
    /// <param name="value">
    ///   The value of the element to add or set.
    /// </param>
    procedure AddOrSetValue(const key: TKey; const value: TValue); deprecated 'Use dict[key] := value instead';

    /// <summary>
    ///   Removes the value associated with the specified key without
    ///   triggering lifetime management for objects.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to remove.
    /// </param>
    /// <returns>
    ///   The removed value for the specified key if it existed; otherwise, <c>
    ///   Default(TValue)</c>.
    /// </returns>
    function Extract(const key: TKey): TValue; overload;

    /// <summary>
    ///   Gets the value for a given key if a matching key exists in the
    ///   dictionary; returns the default value otherwise.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to get.
    /// </param>
    /// <returns>
    ///   The value associated with the specified key. If the specified key is
    ///   not found, <c>Default(TValue)</c>.
    /// </returns>
    function GetValueOrDefault(const key: TKey): TValue; overload;

    /// <summary>
    ///   Gets the value for a given key if a matching key exists in the
    ///   dictionary; returns the given default value otherwise.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to get.
    /// </param>
    /// <param name="defaultValue">
    ///   The value to return if key is not found.
    /// </param>
    /// <returns>
    ///   The value associated with the specified key. If the specified key is
    ///   not found, <c>defaultValue</c>.
    /// </returns>
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;

    /// <summary>
    ///   Attempts to get and remove the value associated with the specified
    ///   key, without triggering lifetime management for objects.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to extract.
    /// </param>
    /// <param name="value">
    ///   The value associated with the specified key, if the key is found;
    ///   otherwise, <c>Default(TValue)</c>.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the key is found; otherwise, <c>False</c>.
    /// </returns>
    function TryExtract(const key: TKey; var value: TValue): Boolean;

    /// <summary>
    ///   Attempts to get the value associated with the specified key.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to get.
    /// </param>
    /// <param name="value">
    ///   The value associated with the specified key, if the key is found;
    ///   otherwise, <c>Default(TValue)</c>.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the key is found; otherwise, <c>False</c>.
    /// </returns>
    function TryGetValue(const key: TKey; var value: TValue): Boolean;

    /// <summary>
    ///   Updates the value associated with the specified key.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to update.
    /// </param>
    /// <param name="newValue">
    ///   The new value to be associated with the specified key, if the key is
    ///   found.
    /// </param>
    /// <param name="oldValue">
    ///   The original value associated with the specified key, if the key is
    ///   found; otherwise, <c>Default(TValue)</c>.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the key is found and the value updated; otherwise, <c>
    ///   False</c>.
    /// </returns>
    function TryUpdateValue(const key: TKey; const newValue: TValue; var oldValue: TValue): Boolean;

    /// <summary>
    ///   Returns the dictionary as read-only dictionary.
    /// </summary>
    /// <returns>
    ///   The dictionary as read-only dictionary.
    /// </returns>
    /// <remarks>
    ///   This method will not perform a copy but will return the same instance
    ///   as <see cref="Spring.Collections|IReadOnlyDictionary&lt;TKey,TValue&gt;" />
    ///    .
    /// </remarks>
    function AsReadOnly: IReadOnlyDictionary<TKey, TValue>;

    /// <summary>
    ///   Sets the capacity to the actual number of elements in the dictionary.
    /// </summary>
    procedure TrimExcess;

    /// <summary>
    ///   Gets or sets the total number of elements the internal data structure
    ///   can hold without resizing.
    /// </summary>
    /// <value>
    ///   The number of elements that the list can contain before resizing is
    ///   required.
    /// </value>
    property Capacity: Integer read GetCapacity write SetCapacity;

    /// <summary>
    ///   Gets or sets the value associated with the specified key.
    /// </summary>
    /// <param name="key">
    ///   The key of the value to get or set.
    /// </param>
    /// <value>
    ///   The value associated with the specified key. If the specified key is
    ///   not found, a get operation throws an <see cref="Spring|EKeyNotFoundException" />
    ///    , and a set operation creates a new element with the specified key.
    /// </value>
    /// <exception cref="Spring|EKeyNotFoundException">
    ///   The property is retrieved and <c>key</c> does not exist in the
    ///   dictionary.
    /// </exception>
    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
  end;

  /// <summary>
  ///   Represents a generic dictionary that preserves insertion order.
  /// </summary>
  IOrderedDictionary<TKey, TValue> = interface(IDictionary<TKey, TValue>) //FI:W524
    ['{299B7DFB-5104-488E-B299-0622D0B4D605}']
  {$REGION 'Property Accessors'}
    function GetItemByIndex(index: Integer): TPair<TKey, TValue>;
  {$ENDREGION}

    /// <summary>
    ///   Determines the index of a specific element in the dictionary.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to locate in the dictionary.
    /// </param>
    /// <returns>
    ///   The index of the element with the specified key if found in the
    ///   dictionary; otherwise, -1.
    /// </returns>
    function IndexOf(const key: TKey): Integer;

    /// <summary>
    ///   Returns the dictionary as read-only ordered dictionary.
    /// </summary>
    /// <returns>
    ///   The dictionary as read-only ordered dictionary.
    /// </returns>
    /// <remarks>
    ///   This method will not perform a copy but will return the same instance
    ///   as <see cref="Spring.Collections|IReadOnlyOrderedDictionary&lt;TKey,TValue&gt;" />
    ///    .
    /// </remarks>
    function AsReadOnly: IReadOnlyOrderedDictionary<TKey, TValue>;

    /// <summary>
    ///   Gets the element at the specified index in the dictionary.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the element to get.
    /// </param>
    /// <value>
    ///   The element at the specified index.
    /// </value>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the dictionary.
    /// </exception>
    property Items[index: Integer]: TPair<TKey, TValue> read GetItemByIndex;
  end;

  IBidiDictionary<TKey, TValue> = interface(IDictionary<TKey, TValue>) //FI:W524
    ['{DA8F1C48-B4F4-4487-ADAD-AF15596DD53C}']
  {$REGION 'Property Accessors'}
    function GetInverse: IBidiDictionary<TValue, TKey>;
  {$ENDREGION}

    /// <summary>
    ///   Returns the inverse view of this bidirectional dictionary, which maps
    ///   each of its values to its associated key. The two bidirectional
    ///   dictionaries are backed by the same data; any changes to one will
    ///   appear in the other.
    /// </summary>
    /// <value>
    ///   The inverse view of this bidirectional dictonary.
    /// </value>
    property Inverse: IBidiDictionary<TValue, TKey> read GetInverse;
  end;

  /// <summary>
  ///   Represents a collection of elements that have a common key.
  /// </summary>
  /// <typeparam name="TKey">
  ///   The type of the key of the IGrouping&lt;TKey, TElement&gt;.
  /// </typeparam>
  /// <typeparam name="TElement">
  ///   The type of the values in the IGrouping&lt;TKey, TElement&gt;.
  /// </typeparam>
  IGrouping<TKey, TElement> = interface(IReadOnlyCollection<TElement>) //FI:W524
    ['{CFC3071C-663A-400A-B21B-1F5E28BA4892}']
  {$REGION 'Property Accessors'}
    function GetKey: TKey;
  {$ENDREGION}

    /// <summary>
    ///   Gets the key of the grouping.
    /// </summary>
    /// <value>
    ///   The key of the grouping.
    /// </value>
    property Key: TKey read GetKey;
  end;

  IReadOnlyMultiMap<TKey, TValue> = interface(IReadOnlyMap<TKey, TValue>) //FI:W524
    ['{5411F9EC-5A56-4F40-890A-089A08AE795F}']
  {$REGION 'Property Accessors'}
    function GetGroups: IEnumerable<IGrouping<TKey, TValue>>;
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
  {$ENDREGION}

    /// <summary>
    ///   Attempts to get the values associated with the specified key.
    /// </summary>
    /// <param name="key">
    ///   The key whose values to get.
    /// </param>
    /// <param name="value">
    ///   When this method returns, the values associated with the specified
    ///   key, if the key is found; otherwise, <c>nil</c>.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the specified key was found; otherwise, <c>False</c>.
    /// </returns>
    function TryGetValues(const key: TKey; var values: IReadOnlyCollection<TValue>): Boolean;

    /// <summary>
    ///   Returns a view of the content of this multimap, each providing a key
    ///   of the multimap and the values for that key. This collection contains
    ///   exactly one entry for each distinct key in the multimap (thus it
    ///   always has the same size as Keys).
    /// </summary>
    /// <value>
    ///   A read-only collection containing the content of the multimap.
    /// </value>
    property Groups: IEnumerable<IGrouping<TKey, TValue>> read GetGroups;

    /// <summary>
    ///   Gets a collection of the values accociated with the/ specified key.
    /// </summary>
    /// <param name="key">
    ///   The key of the values to get.
    /// </param>
    /// <value>
    ///   A read-only collection containing the values accociated with the
    ///   specified key.
    /// </value>
    /// <remarks>
    ///   If the specified key does not exist in the multimap, an empty
    ///   collection is returned. <br /><br />Any changes to the multimap will
    ///   appear in the returned collections.
    /// </remarks>
    property Items[const key: TKey]: IReadOnlyCollection<TValue> read GetItems; default;
  end;

  IMultiMap<TKey, TValue> = interface(IMap<TKey, TValue>) //FI:W524
    ['{8598095E-92A7-4FCC-9F78-8EE7653B8B49}']
  {$REGION 'Property Accessors'}
    function GetGroups: IEnumerable<IGrouping<TKey, TValue>>;
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
  {$ENDREGION}

    /// <summary>
    ///   Adds an element with the specified key and value to the map and
    ///   returns if the underlying map was modified.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to add.
    /// </param>
    /// <param name="value">
    ///   The value of the element to add.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the map was modified, <c>False</c> otherwise.
    /// </returns>
    function Add(const key: TKey; const value: TValue): Boolean; overload;

    /// <summary>
    ///   Adds multiple elements with the specified values accociated with the
    ///   specified key.
    /// </summary>
    /// <param name="key">
    ///   The key of the elements to add.
    /// </param>
    /// <param name="value">
    ///   The values of the elements to add.
    /// </param>
    procedure AddRange(const key: TKey; const values: array of TValue); overload;

    /// <summary>
    ///   Adds multiple elements with the specified values accociated with the
    ///   specified key.
    /// </summary>
    /// <param name="key">
    ///   The key of the elements to add.
    /// </param>
    /// <param name="value">
    ///   The values of the elements to add.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>values</c> is <c>nil</c>.
    /// </exception>
    procedure AddRange(const key: TKey; const values: IEnumerable<TValue>); overload;

    /// <summary>
    ///   Extracts all values accociated with the given key from the multimap.
    /// </summary>
    /// <remarks>
    ///   If the multimap has doOwnsValues set the items in the returned list
    ///   are not being owned by the list but have to be freed manually or
    ///   being passed to a collection that takes ownership.
    /// </remarks>
    function Extract(const key: TKey): ICollection<TValue>; overload;

    /// <summary>
    ///   Attempts to get the values associated with the specified key.
    /// </summary>
    /// <param name="key">
    ///   The key whose values to get.
    /// </param>
    /// <param name="value">
    ///   When this method returns, the values associated with the specified
    ///   key, if the key is found; otherwise, <c>nil</c>.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the specified key was found; otherwise, <c>False</c>.
    /// </returns>
    function TryGetValues(const key: TKey; var values: IReadOnlyCollection<TValue>): Boolean;

    /// <summary>
    ///   Returns the multimap as read-only multimap.
    /// </summary>
    /// <returns>
    ///   The multimap as read-only ordered multimap.
    /// </returns>
    /// <remarks>
    ///   This method will not perform a copy but will return the same instance
    ///   as IReadOnlyMultiMap&lt;TKey, TValue&gt;.
    /// </remarks>
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;

    /// <summary>
    ///   Returns a view of the content of this multimap, each providing a key
    ///   of the multimap and the values for that key. This collection contains
    ///   exactly one entry for each distinct key in the multimap (thus it
    ///   always has the same size as Keys).
    /// </summary>
    /// <value>
    ///   A read-only collection containing the content of the multimap.
    /// </value>
    property Groups: IEnumerable<IGrouping<TKey, TValue>> read GetGroups;

    /// <summary>
    ///   Gets a collection of the values accociated with the/ specified key.
    /// </summary>
    /// <param name="key">
    ///   The key of the values to get.
    /// </param>
    /// <value>
    ///   A read-only collection containing the values accociated with the
    ///   specified key.
    /// </value>
    /// <remarks>
    ///   If the specified key does not exist in the multimap, an empty
    ///   collection is returned. <br /><br />Any changes to the multimap will
    ///   appear in the returned collections.
    /// </remarks>
    property Items[const key: TKey]: IReadOnlyCollection<TValue> read GetItems; default;
  end;

  /// <summary>
  ///   Represents a variable size last-in-first-out (LIFO) collection of
  ///   instances of the same arbitrary type.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the type of elements in the stack.
  /// </typeparam>
  IStack<T> = interface(IEnumerable<T>) //FI:W524
    ['{5BD7BDD3-0198-4727-B97C-658BF194FF63}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetOnChanged: ICollectionChangedEvent<T>;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}

    /// <summary>
    ///   Removes all elements from the stack.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Adds an element at the top of the stack.
    /// </summary>
    /// <param name="item">
    ///   The element to push onto the stack.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the element was added; otherwise, <c>False</c>.
    /// </returns>
    function Push(const item: T): Boolean;

    /// <summary>
    ///   Removes the element at the top of the stack.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    /// <remarks>
    ///   This will return nil to prevent a dangling reference if the stack has
    ///   ownership over the instances.
    /// </remarks>
    function Pop: T;

    /// <summary>
    ///   Removes and returns the element at the top of the stack. If the stack
    ///   has ownership over the instances, then ownership of the returned
    ///   element is transferred to the caller.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    function Extract: T;

    /// <summary>
    ///   Returns the element at the top of the stack without removing it.
    /// </summary>
    function Peek: T;

    /// <summary>
    ///   Returns the element at the top of the stack without removing it.
    ///   Returns <c>Default(T)</c> if the stack is empty.
    /// </summary>
    function PeekOrDefault: T;

    /// <summary>
    ///   Attempts to remove and return the element at the top of the stack.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful and the
    ///   stack does not have ownership of the instances; <c>Default(T)</c>
    ///   otherwise.
    /// </param>
    /// <returns>
    ///   <c>True</c> if an element was removed; otherwise, <c>False</c>.
    /// </returns>
    function TryPop(var item: T): Boolean;

    /// <summary>
    ///   Attempts to remove and return the element at the top of the stack. If
    ///   the stack has ownership over the instances, then ownership of the
    ///   returned element is transferred to the caller.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful; <c>
    ///   Default(T)</c> otherwise.
    /// </param>
    /// <returns>
    ///   <c>True</c> if an element was removed; otherwise, <c>False</c>.
    /// </returns>
    function TryExtract(var item: T): Boolean;

    /// <summary>
    ///   Attempts to return an element from the top of the stack without
    ///   removing it.
    /// </summary>
    /// <param name="item">
    ///   The element at the top of the stack if the operation was successful; <c>
    ///   Default(T)</c> otherwise.
    /// </param>
    /// <returns>
    ///   <c>True</c> if an element was returned; otherwise, <c>False</c>.
    /// </returns>
    function TryPeek(var item: T): Boolean;

    /// <summary>
    ///   Sets the capacity to the actual number of elements in the stack.
    /// </summary>
    procedure TrimExcess;

    /// <summary>
    ///   Gets or sets the total number of elements the internal data structure
    ///   can hold without resizing.
    /// </summary>
    /// <value>
    ///   The number of elements that the stack can contain before resizing is
    ///   required.
    /// </value>
    property Capacity: Integer read GetCapacity write SetCapacity;

    /// <summary>
    ///   Occurs when an item is added, removed, or moved, or the entire list
    ///   is refreshed.
    /// </summary>
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  /// <summary>
  ///   Represents a first-in, first-out collection of elements.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the type of elements in the queue.
  /// </typeparam>
  IQueue<T> = interface(IEnumerable<T>) //FI:W524
    ['{D305A076-3F19-497C-94E3-6BD1C7A30F3F}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetOnChanged: ICollectionChangedEvent<T>;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}

    /// <summary>
    ///   Removes all elements from the queue.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Adds an element to the end of the queue.
    /// </summary>
    /// <param name="item">
    ///   The element to add to the queue. The value can be <c>nil</c> for
    ///   reference types.
    /// </param>
    /// <returns>
    ///   True if the element was added, False otherwise.
    /// </returns>
    function Enqueue(const item: T): Boolean;

    /// <summary>
    ///   Removes the element at the beginning of the queue.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    /// <remarks>
    ///   This will return nil to prevent a dangling reference if the queue has
    ///   ownership over the instances.
    /// </remarks>
    function Dequeue: T;

    /// <summary>
    ///   Removes and returns the element at the beginning of the queue. If the
    ///   queue has ownership over the instances, then ownership of the
    ///   returned element is transferred to the caller.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    function Extract: T;

    /// <summary>
    ///   Returns the element at the beginning of the queue without removing
    ///   it.
    /// </summary>
    function Peek: T;

    /// <summary>
    ///   Returns the element at the beginning of the queue without removing
    ///   it. Returns <c>Default(T)</c> if the queue is empty.
    /// </summary>
    function PeekOrDefault: T;

    /// <summary>
    ///   Attempts to remove and return the element at the beginning of the
    ///   queue.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful and the
    ///   queue does not have ownership of the instances; <c>Default(T)</c>
    ///   otherwise.
    /// </param>
    /// <returns>
    ///   <c>True</c> if an element was removed; otherwise, <c>False</c>.
    /// </returns>
    function TryDequeue(var item: T): Boolean;

    /// <summary>
    ///   Attempts to remove and return the element at the beginning of the
    ///   queue. If the queue has ownership over the instances, then ownership
    ///   of the returned element is transferred to the caller.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful; <c>
    ///   Default(T)</c> otherwise.
    /// </param>
    /// <returns>
    ///   <c>True</c> if an element was removed; otherwise, <c>False</c>.
    /// </returns>
    function TryExtract(var item: T): Boolean;

    /// <summary>
    ///   Attempts to return an element from the beginning of the queue without
    ///   removing it.
    /// </summary>
    /// <param name="item">
    ///   The element at the beginning of the queue if the operation was
    ///   successful; <c>Default(T)</c> otherwise.
    /// </param>
    /// <returns>
    ///   <c>True</c> if an element was returned; otherwise, <c>False</c>.
    /// </returns>
    function TryPeek(var item: T): Boolean;

    /// <summary>
    ///   Sets the capacity to the actual number of elements in the queue.
    /// </summary>
    procedure TrimExcess;

    /// <summary>
    ///   Gets or sets the total number of elements the internal data structure
    ///   can hold without resizing.
    /// </summary>
    /// <value>
    ///   The number of elements that the queue can contain before resizing is
    ///   required.
    /// </value>
    property Capacity: Integer read GetCapacity write SetCapacity;

    /// <summary>
    ///   Occurs when an item is added, removed, or moved, or the entire list
    ///   is refreshed.
    /// </summary>
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  /// <summary>
  ///   Represents a double-ended queue.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the type of elements in the deque.
  /// </typeparam>
  IDeque<T> = interface(IEnumerable<T>) //FI:W524
    ['{852D8B1A-8587-4C7E-85DA-41F255887153}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetOnChanged: ICollectionChangedEvent<T>;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}

    /// <summary>
    ///   Removes all elements from the deque.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Adds an element to the front of the deque.
    /// </summary>
    /// <param name="item">
    ///   The element to add to the deque. The value can be <c>nil</c> for
    ///   reference types.
    /// </param>
    /// <returns>
    ///   True if the element was added, False otherwise.
    /// </returns>
    function AddFirst(const item: T): Boolean;

    /// <summary>
    ///   Adds an element to the back of the deque.
    /// </summary>
    /// <param name="item">
    ///   The element to add to the deque. The value can be <c>nil</c> for
    ///   reference types.
    /// </param>
    /// <returns>
    ///   True if the element was added, False otherwise.
    /// </returns>
    function AddLast(const item: T): Boolean;

    /// <summary>
    ///   Removes the element at the front of the deque.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    /// <remarks>
    ///   This will return nil to prevent a dangling reference if the deque has
    ///   ownership over the instances.
    /// </remarks>
    function RemoveFirst: T;

    /// <summary>
    ///   Removes the element at the back of the deque.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    /// <remarks>
    ///   This will return nil to prevent a dangling reference if the deque has
    ///   ownership over the instances.
    /// </remarks>
    function RemoveLast: T;

    /// <summary>
    ///   Removes and returns the element at the front of the deque. If the
    ///   deque has ownership over the instances, then ownership of the
    ///   returned element is transferred to the caller.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    function ExtractFirst: T;

    /// <summary>
    ///   Removes and returns the element at the back of the deque. If the
    ///   deque has ownership over the instances, then ownership of the
    ///   returned element is transferred to the caller.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    function ExtractLast: T;

    /// <summary>
    ///   Attempts to remove and return the element at the front of the deque.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful and the
    ///   deque does not have ownership of the instances; <c>Default(T)</c>
    ///   otherwise.
    /// </param>
    /// <returns>
    ///   <c>True</c> if an element was removed; otherwise, <c>False</c>.
    /// </returns>
    function TryRemoveFirst(var item: T): Boolean;

    /// <summary>
    ///   Attempts to remove and return the element at the back of the deque.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful and the
    ///   deque does not have ownership of the instances; <c>Default(T)</c>
    ///   otherwise.
    /// </param>
    /// <returns>
    ///   <c>True</c> if an element was removed; otherwise, <c>False</c>.
    /// </returns>
    function TryRemoveLast(var item: T): Boolean;

    /// <summary>
    ///   Attempts to remove and return the element at the front of the deque.
    ///   If the deque has ownership over the instances, then ownership of the
    ///   returned element is transferred to the caller.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful; <c>
    ///   Default(T)</c> otherwise.
    /// </param>
    /// <returns>
    ///   <c>True</c> if an element was removed; otherwise, <c>False</c>.
    /// </returns>
    function TryExtractFirst(var item: T): Boolean;

    /// <summary>
    ///   Attempts to remove and return the element at the back of the deque.
    ///   If the deque has ownership over the instances, then ownership of the
    ///   returned element is transferred to the caller.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful; <c>
    ///   Default(T)</c> otherwise.
    /// </param>
    /// <returns>
    ///   <c>True</c> if an element was removed; otherwise, <c>False</c>.
    /// </returns>
    function TryExtractLast(var item: T): Boolean;

    /// <summary>
    ///   Sets the capacity to the actual number of elements in the deque.
    /// </summary>
    procedure TrimExcess;

    /// <summary>
    ///   Gets or sets the total number of elements the internal data structure
    ///   can hold without resizing.
    /// </summary>
    /// <value>
    ///   The number of elements that the deque can contain before resizing is
    ///   required.
    /// </value>
    property Capacity: Integer read GetCapacity write SetCapacity;

    /// <summary>
    ///   Occurs when an item is added, removed, or moved, or the entire list
    ///   is refreshed.
    /// </summary>
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  /// <summary>
  ///   Provides the base interface for the abstraction of sets.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of elements in the set.
  /// </typeparam>
  ISet<T> = interface(ICollection<T>) //FI:W524
    ['{DC0B211F-E9FD-41D6-BEE0-FCB9F79327AB}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}

    /// <summary>
    ///   Removes all elements in the specified collection from the current
    ///   set.
    /// </summary>
    /// <param name="other">
    ///   The collection of items to remove from the set.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>other</c> is <c>nil</c>.
    /// </exception>
    procedure ExceptWith(const other: IEnumerable<T>);

    /// <summary>
    ///   Modifies the current set so that it contains only elements that are
    ///   also in a specified collection.
    /// </summary>
    /// <param name="other">
    ///   The collection to compare to the current set.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>other</c> is <c>nil</c>.
    /// </exception>
    procedure IntersectWith(const other: IEnumerable<T>);

    /// <summary>
    ///   Modifies the current set so that it contains all elements that are
    ///   present in either the current set or the specified collection.
    /// </summary>
    /// <param name="other">
    ///   The collection to compare to the current set.
    /// </param>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>other</c> is <c>nil</c>.
    /// </exception>
    procedure UnionWith(const other: IEnumerable<T>);

    /// <summary>
    ///   Determines whether a set is a subset of a specified collection.
    /// </summary>
    /// <param name="other">
    ///   The collection to compare to the current set.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the current set is a subset of <c>other</c>;
    ///   otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>other</c> is <c>nil</c>.
    /// </exception>
    function IsSubsetOf(const other: IEnumerable<T>): Boolean;

    /// <summary>
    ///   Determines whether the current set is a superset of a specified
    ///   collection.
    /// </summary>
    /// <param name="other">
    ///   The collection to compare to the current set.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the current set is a superset of <c>other</c>;
    ///   otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>other</c> is <c>nil</c>.
    /// </exception>
    function IsSupersetOf(const other: IEnumerable<T>): Boolean;

    /// <summary>
    ///   Determines whether the current set and the specified collection
    ///   contain the same elements.
    /// </summary>
    /// <param name="other">
    ///   The collection to compare to the current set.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the current set is equal to <c>other</c>; otherwise, <c>
    ///   False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>other</c> is <c>nil</c>.
    /// </exception>
    function SetEquals(const other: IEnumerable<T>): Boolean;

    /// <summary>
    ///   Determines whether the current set overlaps with the specified
    ///   collection.
    /// </summary>
    /// <param name="other">
    ///   The collection to compare to the current set.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the current set and <c>other</c> share at least one
    ///   common element; otherwise, <c>False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>other</c> is <c>nil</c>.
    /// </exception>
    function Overlaps(const other: IEnumerable<T>): Boolean;

    /// <summary>
    ///   Sets the capacity to the actual number of elements in the set.
    /// </summary>
    procedure TrimExcess;

    /// <summary>
    ///   Gets or sets the total number of elements the internal data structure
    ///   can hold without resizing.
    /// </summary>
    /// <value>
    ///   The number of elements that the set can contain before resizing is
    ///   required.
    /// </value>
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  /// <summary>
  ///   Represents a generic set that preserves insertion order.
  /// </summary>
  IOrderedSet<T> = interface(ISet<T>) //FI:W524
    ['{3547BF38-902F-49BA-8BB1-215E6754891D}']
  {$REGION 'Property Accessors'}
    function GetItemByIndex(index: Integer): T;
  {$ENDREGION}

    /// <summary>
    ///   Determines the index of a specific item in the set.
    /// </summary>
    /// <param name="item">
    ///   The item to locate in the dictionary.
    /// </param>
    /// <returns>
    ///   The index of <c>item</c> if found in the set; otherwise, -1.
    /// </returns>
    function IndexOf(const item: T): Integer;

    /// <summary>
    ///   Gets the element at the specified index in the set.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the element to get.
    /// </param>
    /// <value>
    ///   The element at the specified index.
    /// </value>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>index</c> is not a valid index in the set.
    /// </exception>
    property Items[index: Integer]: T read GetItemByIndex;
  end;

  TMultiSetEntry<T> = packed record
    Item: T;
    Count: Integer;
  end;

  IReadOnlyMultiSet<T> = interface(IReadOnlyCollection<T>) //FI:W524
    ['{7ECC0F3E-B73C-4821-82ED-FD84E0F81856}']
  {$REGION 'Property Accessors'}
    function GetEntries: IReadOnlyCollection<TMultiSetEntry<T>>;
    function GetItems: IReadOnlyCollection<T>;
    function GetItemCount(const item: T): Integer;
  {$ENDREGION}

    /// <summary>
    ///   Returns a view of the contents of this multiset, each providing an
    ///   element of the multiset and the count of that element. This
    ///   collection contains exactly one entry for each distinct element in
    ///   the multiset (thus it always has the same size as Elements).
    /// </summary>
    property Entries: IReadOnlyCollection<TMultiSetEntry<T>> read GetEntries;

    /// <summary>
    ///   Returns the collection of distinct elements contained in this
    ///   multiset.
    /// </summary>
    property Items: IReadOnlyCollection<T> read GetItems;

    /// <summary>
    ///   Returns the number of occurrences of an element in this multiset (the
    ///   count of the element).
    /// </summary>
    property ItemCount[const item: T]: Integer read GetItemCount; default;
  end;

  IMultiSet<T> = interface(ICollection<T>) //FI:W524
    ['{CC7C2115-EED6-4FDE-9AE6-44C253514B2F}']
  {$REGION 'Property Accessors'}
    function GetEntries: IReadOnlyCollection<TMultiSetEntry<T>>;
    function GetItems: IReadOnlyCollection<T>;
    function GetItemCount(const item: T): Integer;
    procedure SetItemCount(const item: T; count: Integer);
  {$ENDREGION}

    /// <summary>
    ///   Adds a number of occurrences of an element to the multiset.
    /// </summary>
    /// <param name="item">
    ///   The element to add occurrences of
    /// </param>
    /// <param name="count">
    ///   The number of occurrences of the element to add
    /// </param>
    /// <returns>
    ///   The count of the element before the operation, zero if the element
    ///   was not in the multiset.
    /// </returns>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>count</c> is negative.
    /// </exception>
    function Add(const item: T; count: Integer): Integer; overload;

    /// <summary>
    ///   Removes a number of occurrences of an element from the multiset. If
    ///   the multiset contains fewer than this number of occurrences to begin
    ///   with, all occurrences will be removed.
    /// </summary>
    /// <param name="item">
    ///   The element to remove occurrences of
    /// </param>
    /// <param name="count">
    ///   The count of occurrences of the element to remove
    /// </param>
    /// <returns>
    ///   The count of the element before the operation, zero if the element
    ///   was not in the multiset.
    /// </returns>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>count</c> is negative.
    /// </exception>
    function Remove(const item: T; count: Integer): Integer; overload;

    /// <summary>
    ///   Determines whether the current multiset and the specified collection
    ///   contain the same number of elements.
    /// </summary>
    /// <param name="other">
    ///   The collection to compare to the current set.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the current multiset is equal to <c>other</c>; otherwise, <c>
    ///   False</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>other</c> is <c>nil</c>.
    /// </exception>
    function SetEquals(const other: IEnumerable<T>): Boolean;

    /// <summary>
    ///   Returns a read-only view on the multiset that is ordered by the
    ///   occurences of its items starting with the highest occurence.
    /// </summary>
    function OrderedByCount: IReadOnlyMultiSet<T>;

    /// <summary>
    ///   Returns a view of the contents of this multiset, each providing an
    ///   element of the multiset and the count of that element. This
    ///   collection contains exactly one entry for each distinct element in
    ///   the multiset (thus it always has the same size as Elements).
    /// </summary>
    property Entries: IReadOnlyCollection<TMultiSetEntry<T>> read GetEntries;

    /// <summary>
    ///   Returns the collection of distinct elements contained in this
    ///   multiset.
    /// </summary>
    property Items: IReadOnlyCollection<T> read GetItems;

    /// <summary>
    ///   Gets or sets the number of occurrences of an element in this multiset
    ///   (the count of the element).
    /// </summary>
    /// <param name="item">
    ///   The item to get or set the count for.
    /// </param>
    /// <value>
    ///   The number of occurences of the specified item; 0 when it does not
    ///   exist.
    /// </value>
    property ItemCount[const item: T]: Integer read GetItemCount write SetItemCount; default;
  end;

  /// <summary>
  ///   Defines an indexer, size property, and Boolean search method for data
  ///   structures that map keys to <see cref="IEnumerable&lt;T&gt;" />
  ///   sequences of values.
  /// </summary>
  /// <typeparam name="TKey">
  ///   The type of the keys in the ILookup&lt;TKey, TElement&gt;.
  /// </typeparam>
  /// <typeparam name="TElement">
  ///   The type of the elements in the <see cref="IEnumerable&lt;T&gt;" />
  ///   sequences that make up the values in the ILookup&lt;TKey, TElement&gt;.
  /// </typeparam>
  ILookup<TKey, TElement> = interface(IReadOnlyCollection<IGrouping<TKey, TElement>>) //FI:W524
    ['{B2380533-F2B1-465B-84B2-97FA79A6EE09}']
  {$REGION 'Property Accessors'}
    function GetItem(const key: TKey): IReadOnlyCollection<TElement>;
  {$ENDREGION}

    /// <summary>
    ///   Determines whether a specified key exists in the lookup
    /// </summary>
    /// <param name="key">
    ///   The key to search for in the lookup.
    /// </param>
    /// <returns>
    ///   <c>True</c> if <c>key</c> is in the lookup; otherwise, <c>False</c>.
    /// </returns>
    function Contains(const key: TKey): Boolean;

    /// <summary>
    ///   Gets the collection of values indexed by a specified key.
    /// </summary>
    /// <param name="key">
    ///   The key of the desired collecton of values.
    /// </param>
    /// <value>
    ///   The collection of values indexed by the specified key.
    /// </value>
    property Items[const key: TKey]: IReadOnlyCollection<TElement> read GetItem; default;
  end;

  /// <summary>
  ///   Provides direct access to an array that is used for internal storage.
  /// </summary>
  IArrayAccess<T> = interface(ICountable) //FI:W524
    ['{0C6C22BE-DBFD-4EBE-9E32-6E4BBA8AC382}']
  {$REGION 'Property Accessors'}
     function GetItems: TArray<T>;
  {$ENDREGION}

    property Items: TArray<T> read GetItems;
  end;

  /// <summary>
  ///   Represents a sub range of a collection.
  /// </summary>
  /// <remarks>
  ///   This type is for internal use.
  /// </remarks>
  IPartition<T> = interface(IEnumerable<T>) //FI:W524
    ['{ACFB79AB-F593-4F2B-9720-E6CE984F6844}']
  end;

  /// <summary>
  ///   Provides static methods to create an instance of various interfaced
  ///   generic collections such as <see cref="IList&lt;T&gt;" /> or <see cref="IDictionary&lt;TKey,TValue&gt;" />
  ///    .
  /// </summary>
  TCollections = class
  protected
    const FoldedTypeKinds = [tkInteger, tkChar, tkEnumeration, tkWChar, tkInt64,
      tkUString, tkClass, tkClassRef, tkPointer, tkProcedure, tkInterface];

  {$REGION 'Internal factory methods'}
  {$IFDEF DELPHIXE7_UP}
    class procedure CreateDictionary_Int8_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int8_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int8_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int8_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int8_Method(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int8_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int8_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int8_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateDictionary_Int16_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int16_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int16_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int16_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int16_Method(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int16_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int16_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int16_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateDictionary_Int32_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int32_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int32_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int32_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int32_Method(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int32_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int32_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int32_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateDictionary_Int64_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int64_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int64_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int64_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int64_Method(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int64_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int64_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Int64_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateDictionary_Interface_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Interface_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Interface_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Interface_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Interface_Method(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Interface_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Interface_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Interface_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateDictionary_Object_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Object_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Object_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Object_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Object_Method(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Object_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Object_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Object_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateDictionary_String_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_String_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_String_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_String_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_String_Method(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_String_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_String_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_String_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateBidiDictionary_Int8_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int8_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int8_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int8_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int8_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int8_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int8_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateBidiDictionary_Int16_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int16_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int16_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int16_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int16_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int16_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int16_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateBidiDictionary_Int32_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int32_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int32_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int32_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int32_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int32_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int32_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateBidiDictionary_Int64_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int64_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int64_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int64_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int64_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int64_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Int64_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateBidiDictionary_Interface_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Interface_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Interface_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Interface_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Interface_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Interface_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Interface_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateBidiDictionary_Object_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Object_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Object_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Object_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Object_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Object_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Object_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateBidiDictionary_String_Int8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_String_Int16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_String_Int32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_String_Int64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_String_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_String_Object(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_String_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateListMultiMap_Int8_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int8_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int8_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int8_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int8_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int8_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int8_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateListMultiMap_Int16_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int16_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int16_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int16_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int16_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int16_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int16_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateListMultiMap_Int32_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int32_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int32_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int32_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int32_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int32_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int32_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateListMultiMap_Int64_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int64_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int64_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int64_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int64_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int64_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Int64_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateListMultiMap_Interface_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Interface_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Interface_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Interface_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Interface_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Interface_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Interface_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateListMultiMap_Object_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Object_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Object_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Object_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Object_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Object_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Object_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateListMultiMap_String_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_String_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_String_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_String_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_String_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_String_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_String_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateSortedListMultiMap_Int8_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int8_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int8_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int8_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int8_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int8_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int8_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateSortedListMultiMap_Int16_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int16_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int16_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int16_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int16_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int16_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int16_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateSortedListMultiMap_Int32_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int32_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int32_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int32_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int32_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int32_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int32_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateSortedListMultiMap_Int64_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int64_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int64_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int64_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int64_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int64_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Int64_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateSortedListMultiMap_Interface_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Interface_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Interface_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Interface_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Interface_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Interface_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Interface_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateSortedListMultiMap_Object_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Object_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Object_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Object_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Object_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Object_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_Object_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateSortedListMultiMap_String_Int8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_String_Int16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_String_Int32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_String_Int64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_String_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_String_Object(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateSortedListMultiMap_String_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
  {$ENDIF}

    // internal helper record because older Delphi compilers could not cope
    // with parameterized methods being passed as parameter
    type Helper<TKey, TValue> = record
      class procedure CreateListCollection(const key;
        const comparer: IInterface; elementType: PTypeInfo; var result); static;
      class procedure CreateHashSetCollection(const key;
        const comparer: IInterface; elementType: PTypeInfo; var result); static;
      class procedure CreateTreeSetCollection(const key;
        const comparer: IInterface; elementType: PTypeInfo; var result); static;
    end;

    class procedure CreateList_Int8(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateList_Int16(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateList_Int32(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateList_Int64(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateList_Method(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateList_Interface(var result; elementType: Pointer); overload; static;
    class procedure CreateList_Interface(comparer: Pointer; var result; elementType: Pointer); overload; static;
    class procedure CreateList_Object(var result; elementType: Pointer); overload; static;
    class procedure CreateList_Object(ownsObjects: Boolean; var result; elementType: Pointer); overload; static;
    class procedure CreateList_Object(comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer); overload; static;
    class procedure CreateList_String(comparer: Pointer; var result; elementType: Pointer); static;

    class procedure CreateSortedList_Int8(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateSortedList_Int16(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateSortedList_Int32(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateSortedList_Int64(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateSortedList_Interface(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateSortedList_Object(comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer); static;
    class procedure CreateSortedList_String(comparer: Pointer; var result; elementType: Pointer); static;

    class procedure CreateHashSet_Int8(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateHashSet_Int16(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateHashSet_Int32(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateHashSet_Int64(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateHashSet_Interface(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateHashSet_Object(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateHashSet_String(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;

    class procedure CreateQueue_Interface(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateQueue_Object(capacity: Integer; comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer); static;

    class procedure CreateStack_Interface(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateStack_Object(capacity: Integer; comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer); static;

    class procedure CreateObservableList_Interface(var result; elementType: Pointer); static;
    class procedure CreateObservableList_Object(ownsObjects: Boolean; var result; elementType: Pointer); static;
  {$ENDREGION}
  public
    class function CreateList<T>: IList<T>; overload; static;
    class function CreateList<T>(const comparer: IComparer<T>): IList<T>; overload; static;
    class function CreateList<T>(const comparer: TComparison<T>): IList<T>; overload; static;
    class function CreateList<T>(const values: array of T): IList<T>; overload; static;
    class function CreateList<T>(const values: IEnumerable<T>): IList<T>; overload; static;
    class function CreateList<T: class>(ownsObjects: Boolean): IList<T>; overload; static; deprecated 'Use CreateObjectList instead';
    class function CreateList<T: class>(const comparer: IComparer<T>; ownsObjects: Boolean): IList<T>; overload; static; deprecated 'Use CreateObjectList instead';
    class function CreateList<T: class>(const comparer: TComparison<T>; ownsObjects: Boolean): IList<T>; overload; static; deprecated 'Use CreateObjectList instead';

    class function CreateObjectList(elementType: PTypeInfo; ownsObjects: Boolean): IObjectList; overload; static;
    class function CreateObjectList<T: class>: IList<T>; overload; static;
    class function CreateObjectList<T: class>(ownsObjects: Boolean): IList<T>; overload; static;
    class function CreateObjectList<T: class>(const comparer: IComparer<T>; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateObjectList<T: class>(const comparer: TComparison<T>; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateObjectList<T: class>(const values: array of T; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateObjectList<T: class>(const values: IEnumerable<T>; ownsObjects: Boolean = True): IList<T>; overload; static;

    class function CreateInterfaceList(elementType: PTypeInfo): IInterfaceList; overload; static;
    class function CreateInterfaceList<T: IInterface>: IList<T>; overload; static;
    class function CreateInterfaceList<T: IInterface>(const comparer: IComparer<T>): IList<T>; overload; static;
    class function CreateInterfaceList<T: IInterface>(const comparer: TComparison<T>): IList<T>; overload; static;
    class function CreateInterfaceList<T: IInterface>(const values: array of T): IList<T>; overload; static;
    class function CreateInterfaceList<T: IInterface>(const values: IEnumerable<T>): IList<T>; overload; static;

    class function CreateStringList: IList<string>; overload; static;
    class function CreateStringList(const comparer: IComparer<string>): IList<string>; overload; static;
    class function CreateStringList(const comparer: TComparison<string>): IList<string>; overload; static;
    class function CreateStringList(const values: array of string): IList<string>; overload; static;
    class function CreateStringList(const values: IEnumerable<string>): IList<string>; overload; static;
    class function CreateStringList(const strings: TStrings; ownsObject: Boolean): IList<string>; overload; static;

    class function CreateSortedList<T>: IList<T>; overload; static;
    class function CreateSortedList<T>(const comparer: IComparer<T>): IList<T>; overload; static;
    class function CreateSortedList<T>(const comparer: TComparison<T>): IList<T>; overload; static;
    class function CreateSortedList<T>(const values: array of T): IList<T>; overload; static;
    class function CreateSortedList<T>(const values: IEnumerable<T>): IList<T>; overload; static;
    class function CreateSortedList<T: class>(ownsObjects: Boolean): IList<T>; overload; static;
    class function CreateSortedList<T: class>(const comparer: IComparer<T>; ownsObjects: Boolean): IList<T>; overload; static;
    class function CreateSortedList<T: class>(const comparer: TComparison<T>; ownsObjects: Boolean): IList<T>; overload; static;
    class function CreateSortedObjectList<T: class>(ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateSortedObjectList<T: class>(const comparer: IComparer<T>; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateSortedObjectList<T: class>(const comparer: TComparison<T>; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateSortedObjectList<T: class>(const values: array of T; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateSortedObjectList<T: class>(const values: IEnumerable<T>; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateSortedInterfaceList<T: IInterface>: IList<T>; overload; static;
    class function CreateSortedInterfaceList<T: IInterface>(const comparer: IComparer<T>): IList<T>; overload; static;
    class function CreateSortedInterfaceList<T: IInterface>(const comparer: TComparison<T>): IList<T>; overload; static;
    class function CreateSortedInterfaceList<T: IInterface>(const values: array of T): IList<T>; overload; static;
    class function CreateSortedInterfaceList<T: IInterface>(const values: IEnumerable<T>): IList<T>; overload; static;

    class function CreateObservableList<T: class>(ownsObjects: Boolean = True): IList<T>; static;
    class function CreateObservableInterfaceList<T: IInterface>: IList<T>; static;

    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships = []): IOrderedDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(capacity: Integer; ownerships: TDictionaryOwnerships = []): IOrderedDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IOrderedDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; const valueComparer: IEqualityComparer<TValue>; ownerships: TDictionaryOwnerships = []): IOrderedDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(capacity: Integer; const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IOrderedDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(capacity: Integer; const keyComparer: IEqualityComparer<TKey>; const valueComparer: IEqualityComparer<TValue>; ownerships: TDictionaryOwnerships = []): IOrderedDictionary<TKey, TValue>; overload; static;

    class function CreateSortedDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships = []): IDictionary<TKey, TValue>; overload; static;
    class function CreateSortedDictionary<TKey, TValue>(const keyComparer: IComparer<TKey>; ownerships: TDictionaryOwnerships = []): IDictionary<TKey, TValue>; overload; static;
    class function CreateSortedDictionary<TKey, TValue>(const valueComparer: IEqualityComparer<TValue>; ownerships: TDictionaryOwnerships = []): IDictionary<TKey, TValue>; overload; static;
    class function CreateSortedDictionary<TKey, TValue>(const keyComparer: IComparer<TKey>; const valueComparer: IEqualityComparer<TValue>; ownerships: TDictionaryOwnerships = []): IDictionary<TKey, TValue>; overload; static;

    class function CreateBidiDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships = []): IBidiDictionary<TKey, TValue>; overload; static;
    class function CreateBidiDictionary<TKey, TValue>(capacity: Integer; ownerships: TDictionaryOwnerships = []): IBidiDictionary<TKey, TValue>; overload; static;
    class function CreateBidiDictionary<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IBidiDictionary<TKey, TValue>; overload; static;
    class function CreateBidiDictionary<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; const valueComparer: IEqualityComparer<TValue>; ownerships: TDictionaryOwnerships = []): IBidiDictionary<TKey, TValue>; overload; static;
    class function CreateBidiDictionary<TKey, TValue>(capacity: Integer; const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IBidiDictionary<TKey, TValue>; overload; static;
    class function CreateBidiDictionary<TKey, TValue>(capacity: Integer; const keyComparer: IEqualityComparer<TKey>; const valueComparer: IEqualityComparer<TValue>; ownerships: TDictionaryOwnerships = []): IBidiDictionary<TKey, TValue>; overload; static;

    class function CreateMultiMap<TKey, TValue>(ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateMultiMap<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;

    class function CreateHashMultiMap<TKey, TValue>(ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateHashMultiMap<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateHashMultiMap<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; const valueComparer: IEqualityComparer<TValue>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;

    class function CreateTreeMultiMap<TKey, TValue>(ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateTreeMultiMap<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateTreeMultiMap<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; const valueComparer: IComparer<TValue>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;

    class function CreateSortedMultiMap<TKey, TValue>(ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateSortedMultiMap<TKey, TValue>(const keyComparer: IComparer<TKey>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;

    class function CreateSortedHashMultiMap<TKey, TValue>(ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateSortedHashMultiMap<TKey, TValue>(const keyComparer: IComparer<TKey>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateSortedHashMultiMap<TKey, TValue>(const keyComparer: IComparer<TKey>; const valueComparer: IEqualityComparer<TValue>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;

    class function CreateSortedTreeMultiMap<TKey, TValue>(ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateSortedTreeMultiMap<TKey, TValue>(const keyComparer: IComparer<TKey>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateSortedTreeMultiMap<TKey, TValue>(const keyComparer: IComparer<TKey>; const valueComparer: IComparer<TValue>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;

    class function CreateStack<T>: IStack<T>; overload; static;
    class function CreateStack<T: class>(ownsObjects: Boolean): IStack<T>; overload; static;
    class function CreateStack<T>(const values: array of T): IStack<T>; overload; static;
    class function CreateStack<T>(const values: IEnumerable<T>): IStack<T>; overload; static;

    class function CreateBoundedStack<T>(size: Integer): IStack<T>; overload; static;
    class function CreateBoundedStack<T: class>(size: Integer; ownsObjects: Boolean): IStack<T>; overload; static;

    class function CreateQueue<T>: IQueue<T>; overload; static;
    class function CreateQueue<T: class>(ownsObjects: Boolean): IQueue<T>; overload; static;
    class function CreateQueue<T>(const values: array of T): IQueue<T>; overload; static;
    class function CreateQueue<T>(const values: IEnumerable<T>): IQueue<T>; overload; static;

    class function CreateBoundedQueue<T>(size: Integer): IQueue<T>; overload; static;
    class function CreateBoundedQueue<T: class>(size: Integer; ownsObjects: Boolean): IQueue<T>; overload; static;

    class function CreateEvictingQueue<T>(size: Integer): IQueue<T>; overload; static;
    class function CreateEvictingQueue<T: class>(size: Integer; ownsObjects: Boolean): IQueue<T>; overload; static;

    class function CreateDeque<T>: IDeque<T>; overload; static;
    class function CreateDeque<T: class>(ownsObjects: Boolean): IDeque<T>; overload; static;
    class function CreateDeque<T>(const values: array of T): IDeque<T>; overload; static;
    class function CreateDeque<T>(const values: IEnumerable<T>): IDeque<T>; overload; static;

    class function CreateBoundedDeque<T>(size: Integer): IDeque<T>; overload; static;
    class function CreateBoundedDeque<T: class>(size: Integer; ownsObjects: Boolean): IDeque<T>; overload; static;

    class function CreateEvictingDeque<T>(size: Integer): IDeque<T>; overload; static;
    class function CreateEvictingDeque<T: class>(size: Integer; ownsObjects: Boolean): IDeque<T>; overload; static;

    class function CreateSet<T>: IOrderedSet<T>; overload; static;
    class function CreateSet<T>(capacity: Integer): IOrderedSet<T>; overload; static;
    class function CreateSet<T>(const comparer: IEqualityComparer<T>): IOrderedSet<T>; overload; static;
    class function CreateSet<T>(capacity: Integer; const comparer: IEqualityComparer<T>): IOrderedSet<T>; overload; static;
    class function CreateSet<T>(const values: array of T): IOrderedSet<T>; overload; static;
    class function CreateSet<T>(const values: IEnumerable<T>): IOrderedSet<T>; overload; static;
    class function CreateSet<T>(const values: array of T; const comparer: IEqualityComparer<T>): IOrderedSet<T>; overload; static;
    class function CreateSet<T>(const values: IEnumerable<T>; const comparer: IEqualityComparer<T>): IOrderedSet<T>; overload; static;

    class function CreateSortedSet<T>: ISet<T>; overload; static;
    class function CreateSortedSet<T>(const comparer: IComparer<T>): ISet<T>; overload; static;
    class function CreateSortedSet<T>(const values: array of T): ISet<T>; overload; static;
    class function CreateSortedSet<T>(const values: IEnumerable<T>): ISet<T>; overload; static;
    class function CreateSortedSet<T>(const values: array of T; const comparer: IComparer<T>): ISet<T>; overload; static;
    class function CreateSortedSet<T>(const values: IEnumerable<T>; const comparer: IComparer<T>): ISet<T>; overload; static;

    class function CreateMultiSet<T>: IMultiSet<T>; overload; static;
    class function CreateMultiSet<T>(const comparer: IEqualityComparer<T>): IMultiSet<T>; overload; static;
    class function CreateMultiSet<T>(const values: array of T): IMultiSet<T>; overload; static;
    class function CreateMultiSet<T>(const values: IEnumerable<T>): IMultiSet<T>; overload; static;
    class function CreateMultiSet<T>(const values: array of T; const comparer: IEqualityComparer<T>): IMultiSet<T>; overload; static;
    class function CreateMultiSet<T>(const values: IEnumerable<T>; const comparer: IEqualityComparer<T>): IMultiSet<T>; overload; static;

    class function CreateSortedMultiSet<T>: IMultiSet<T>; overload; static;
    class function CreateSortedMultiSet<T>(const comparer: IComparer<T>): IMultiSet<T>; overload; static;
    class function CreateSortedMultiSet<T>(const values: array of T): IMultiSet<T>; overload; static;
    class function CreateSortedMultiSet<T>(const values: IEnumerable<T>): IMultiSet<T>; overload; static;
    class function CreateSortedMultiSet<T>(const values: array of T; const comparer: IComparer<T>): IMultiSet<T>; overload; static;
    class function CreateSortedMultiSet<T>(const values: IEnumerable<T>; const comparer: IComparer<T>): IMultiSet<T>; overload; static;
  end;

  TEnumerable = class
  protected
  {$REGION 'Internal factory methods'}
    class procedure CreateEmpty_Int8(var result; elementType: Pointer); static;
    class procedure CreateEmpty_Int16(var result; elementType: Pointer); static;
    class procedure CreateEmpty_Int32(var result; elementType: Pointer); static;
    class procedure CreateEmpty_Int64(var result; elementType: Pointer); static;
    class procedure CreateEmpty_Method(var result; elementType: Pointer); static;
    class procedure CreateEmpty_Interface(var result; elementType: Pointer); static;
    class procedure CreateEmpty_Object(var result; elementType: Pointer); static;
    class procedure CreateEmpty_String(var result; elementType: Pointer); static;

    class procedure InternalFrom_Int8_DynArray(source: Pointer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Int8_OpenArray(source: Pointer; count: Integer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Int16_DynArray(source: Pointer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Int16_OpenArray(source: Pointer; count: Integer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Int32_DynArray(source: Pointer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Int32_OpenArray(source: Pointer; count: Integer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Int64_DynArray(source: Pointer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Int64_OpenArray(source: Pointer; count: Integer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Method_DynArray(source: Pointer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Method_OpenArray(source: Pointer; count: Integer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_String_DynArray(source: Pointer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_String_OpenArray(source: Pointer; count: Integer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Object_DynArray(source: Pointer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Object_OpenArray(source: Pointer; count: Integer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Interface_DynArray(source: Pointer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Interface_OpenArray(source: Pointer; count: Integer; var result; elementType: PTypeInfo); static;
    class procedure InternalOfType_Object(const source: IEnumerable<TObject>; var result; resultType: PTypeInfo); static;
  {$ENDREGION}
  public
    /// <summary>
    ///   Applies an accumulator function over a sequence. The specified seed
    ///   value is used as the initial accumulator value.
    /// </summary>
    /// <typeparam name="TSource">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TAccumulate">
    ///   The type of the accumulator value.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence aggregate over.
    /// </param>
    /// <param name="seed">
    ///   The initial accumulator value.
    /// </param>
    /// <param name="func">
    ///   An accumulator function to be invoked on each element.
    /// </param>
    /// <returns>
    ///   The final accumulator value.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>func</c> is <c>nil</c>.
    /// </exception>
    class function Aggregate<TSource, TAccumulate>(const source: IEnumerable<TSource>;
      const seed: TAccumulate; const func: Func<TAccumulate, TSource, TAccumulate>): TAccumulate; overload; static;

    /// <summary>
    ///   Applies an accumulator function over a sequence. The specified seed
    ///   value is used as the initial accumulator value, and the specified
    ///   function is used to select the result value.
    /// </summary>
    /// <typeparam name="TSource">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TAccumulate">
    ///   The type of the accumulator value.
    /// </typeparam>
    /// <typeparam name="TResult">
    ///   The type of the resulting value.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence aggregate over.
    /// </param>
    /// <param name="seed">
    ///   The initial accumulator value.
    /// </param>
    /// <param name="func">
    ///   An accumulator function to be invoked on each element.
    /// </param>
    /// <param name="resultSelector">
    ///   A function to transform the final accumulator value into the result
    ///   value.
    /// </param>
    /// <returns>
    ///   The transformed final accumulator value.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>func</c> or <c>resultSelector</c> is <c>nil</c>.
    /// </exception>
    class function Aggregate<TSource, TAccumulate, TResult>(const source: IEnumerable<TSource>;
      const seed: TAccumulate; const func: Func<TAccumulate, TSource, TAccumulate>;
      const resultSelector: Func<TAccumulate, TResult>): TResult; overload; static;

    /// <summary>
    ///   Splits the elements of a sequence into chunks of size at most <c>size</c>
    ///    .
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence whose elements to chunk.
    /// </param>
    /// <param name="size">
    ///   The maximum size of each chunk.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements the input sequence split into
    ///   chunks of size <c>size</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>size</c> is below 1.
    /// </exception>
    class function Chunk<T>(const source: IEnumerable<T>; size: Integer): IEnumerable<TArray<T>>; static;

    /// <summary>
    ///   Returns the elements of the specified sequence or the type
    ///   parameter's default value in a singleton collection if the sequence
    ///   is empty.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <param name="source">
    ///   The sequence to return a default value for if it is empty.
    /// </param>
    /// <returns>
    ///   A sequence that contains <c>Default(T)</c> if <c>source</c> is empty;
    ///   otherwise, <c>source</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> is <c>nil</c>.
    /// </exception>
    class function DefaultIfEmpty<T>(const source: IEnumerable<T>): IEnumerable<T>; overload; static;
      deprecated 'Use source.DefaultIfEmpty instead';

    /// <summary>
    ///   Returns the elements of the specified sequence or the specified value
    ///   in a singleton collection if the sequence is empty.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <param name="source">
    ///   The sequence to return a default value for if it is empty.
    /// </param>
    /// <param name="defaultValue">
    ///   The value to return if the sequence is empty.
    /// </param>
    /// <returns>
    ///   A sequence that contains <c>defaultValue</c> if <c>source</c> is
    ///   empty; otherwise, <c>source</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> is <c>nil</c>.
    /// </exception>
    class function DefaultIfEmpty<T>(const source: IEnumerable<T>; const defaultValue: T): IEnumerable<T>; overload; static;
      deprecated 'Use source.DefaultIfEmpty(defaultValue) instead';

    /// <summary>
    ///   Returns distinct elements from a sequence by using the default
    ///   equality comparer to compare values.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <param name="source">
    ///   The sequence to remove duplicate elements from.
    /// </param>
    /// <returns>
    ///   A sequence that contains distinct elements from the source sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> is <c>nil</c>.
    /// </exception>
    class function Distinct<T>(const source: IEnumerable<T>): IEnumerable<T>; overload; static;
      deprecated 'Use source.Distinct instead';

    /// <summary>
    ///   Returns distinct elements from a sequence by using a specified
    ///   equality comparer to compare values.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <param name="source">
    ///   The sequence to remove duplicate elements from.
    /// </param>
    /// <param name="comparer">
    ///   A sequence to compare values.
    /// </param>
    /// <returns>
    ///   A sequence that contains distinct elements from the source sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> is <c>nil</c>.
    /// </exception>
    /// <remarks>
    ///   If <c>comparer</c> is <c>nil</c>, the default equality comparer is
    ///   used to compare values.
    /// </remarks>
    class function Distinct<T>(const source: IEnumerable<T>;
      const comparer: IEqualityComparer<T>): IEnumerable<T>; overload; static;
      deprecated 'Use source.Distinct(comparer) instead';

    /// <summary>
    ///   Returns distinct elements from a sequence according to a specified
    ///   key selector function.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of key to distinguish elements by.
    /// </typeparam>
    /// <param name="source">
    ///   The sequence to remove duplicate elements from.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <returns>
    ///   A sequence that contains distinct elements from the source sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> is <c>nil</c>.
    /// </exception>
    class function DistinctBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>): IEnumerable<T>; overload; static;

    /// <summary>
    ///   Returns distinct elements from a sequence according to a specified
    ///   key selector function and using a specified comparer to compare keys.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of key to distinguish elements by.
    /// </typeparam>
    /// <param name="source">
    ///   The sequence to remove duplicate elements from.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare keys.
    /// </param>
    /// <returns>
    ///   A sequence that contains distinct elements from the source sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> is <c>nil</c>.
    /// </exception>
    /// <remarks>
    ///   If <c>comparer</c> is <c>nil</c>, the default equality comparer is
    ///   used to compare values.
    /// </remarks>
    class function DistinctBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const comparer: IEqualityComparer<TKey>): IEnumerable<T>; overload; static;

    /// <summary>
    ///   Produces the set difference of two sequences by using the default
    ///   equality comparer to compare values.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of the input sequences.
    /// </typeparam>
    /// <param name="first">
    ///   A sequence whose elements that are not also in <c>second</c> will be
    ///   returned.
    /// </param>
    /// <param name="second">
    ///   A sequence whose elements that also occur in <c>first</c> will cause
    ///   those elements to be removed from the returned sequence.
    /// </param>
    /// <returns>
    ///   A sequence that contains the set difference of the elements of two
    ///   sequences.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>first</c> or <c>second</c> is <c>nil</c>.
    /// </exception>
    class function &Except<T>(const first, second: IEnumerable<T>): IEnumerable<T>; overload; static;
      deprecated 'Use first.Exclude(second) instead';

    /// <summary>
    ///   Produces the set difference of two sequences by using the specified
    ///   equality comparer to compare values.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of the input sequences.
    /// </typeparam>
    /// <param name="first">
    ///   A sequence whose elements that are not also in <c>second</c> will be
    ///   returned.
    /// </param>
    /// <param name="second">
    ///   A sequence whose elements that also occur in <c>first</c> will cause
    ///   those elements to be removed from the returned sequence.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare values.
    /// </param>
    /// <returns>
    ///   A sequence that contains the set difference of the elements of two
    ///   sequences.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>first</c> or <c>second</c> is <c>nil</c>.
    /// </exception>
    /// <remarks>
    ///   If <c>comparer</c> is <c>nil</c>, the default equality comparer is
    ///   used to compare values.
    /// </remarks>
    class function &Except<T>(const first, second: IEnumerable<T>;
      const comparer: IEqualityComparer<T>): IEnumerable<T>; overload; static;
      deprecated 'Use first.Exclude(second, comparer) instead';

    /// <summary>
    ///   Returns an empty sequence that has the specified type argument.
    /// </summary>
    /// <typeparam name="T">
    ///   The type to assign to the type parameter of the returned sequence.
    /// </typeparam>
    /// <returns>
    ///   An empty sequence whose type argument is <c>T</c>.
    /// </returns>
    class function Empty<T>: IReadOnlyList<T>; static;

    /// <summary>
    ///   Returns a sequence that contains the specified values.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements in <c>values</c>.
    /// </typeparam>
    /// <param name="values">
    ///   The values to return a sequence for.
    /// </param>
    /// <returns>
    ///   A sequence that contains the specified values.
    /// </returns>
    class function From<T>(const values: array of T): IReadOnlyList<T>; overload; static;

    /// <summary>
    ///   Returns a sequence that contains the specified values.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements in <c>values</c>.
    /// </typeparam>
    /// <param name="values">
    ///   The values to return a sequence for.
    /// </param>
    /// <returns>
    ///   A sequence that contains the specified values.
    /// </returns>
    class function From<T>(const values: TArray<T>): IReadOnlyList<T>; overload; static;

    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence whose elements to group.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <returns>
    ///   A sequence where each element is a group that contains a sequence of
    ///   elements and a key.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> is <c>nil</c>.
    /// </exception>
    class function GroupBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>): IEnumerable<IGrouping<TKey,T>>; overload; static;

    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function and compares the keys by using a specified
    ///   comparer.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence whose elements to group.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare keys.
    /// </param>
    /// <returns>
    ///   A sequence where each element is a group that contains a collection of
    ///   elements and a key.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> is <c>nil</c>.
    /// </exception>
    /// <remarks>
    ///   If <c>comparer</c> is <c>nil</c>, the default equality comparer is
    ///   used to compare keys.
    /// </remarks>
    class function GroupBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const comparer: IEqualityComparer<TKey>): IEnumerable<IGrouping<TKey,T>>; overload; static;

    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function and projects the elements for each group by using a
    ///   specified function.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <typeparam name="TElement">
    ///   The type of the elements in the grouping.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence whose elements to group.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <param name="elementSelector">
    ///   A function to map each source element to an element in the grouping.
    /// </param>
    /// <returns>
    ///   A sequence where each element is a group that contains a collection of
    ///   elements of type <c>TElement</c> and a key.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> or <c>elementSelector</c> is <c>nil</c>.
    /// </exception>
    class function GroupBy<T, TKey, TElement>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const elementSelector: Func<T, TElement>): IEnumerable<IGrouping<TKey,TElement>>; overload; static;

    /// <summary>
    ///   Groups the elements of a sequence according to a key selector
    ///   function. The keys are compared by using a comparer and each group's
    ///   elements are projected by using a specified function.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <typeparam name="TElement">
    ///   The type of the elements in the grouping.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence whose elements to group.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <param name="elementSelector">
    ///   A function to map each source element to an element in the grouping.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare keys.
    /// </param>
    /// <returns>
    ///   A sequence where each element is a group that contains a collection
    ///   of elements of type <c>TElement</c> and a key.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> or <c>elementSelector</c> is <c>
    ///   nil</c>.
    /// </exception>
    /// <remarks>
    ///   If <c>comparer</c> is <c>nil</c>, the default equality comparer is
    ///   used to compare keys.
    /// </remarks>
    class function GroupBy<T, TKey, TElement>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const elementSelector: Func<T, TElement>;
      const comparer: IEqualityComparer<TKey>): IEnumerable<IGrouping<TKey,TElement>>; overload; static;

    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function and creates a result value from each group and its
    ///   key.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <typeparam name="TResult">
    ///   The type of the result value returned by <c>resultSelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence whose elements to group.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <param name="resultSelector">
    ///   A function to create a result value from each group.
    /// </param>
    /// <returns>
    ///   A collection of elements of type <c>TResult</c> where each element
    ///   represents a projection over a group and its key.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> or <c>resultSelector</c> is <c>
    ///   nil</c>.
    /// </exception>
    class function GroupBy<T, TKey, TResult>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const resultSelector: Func<TKey, IEnumerable<T>, TResult>): IEnumerable<TResult>; overload; static;

    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function and creates a result value from each group and its
    ///   key. The keys are compared by using a specified comparer.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <typeparam name="TResult">
    ///   The type of the result value returned by <c>resultSelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence whose elements to group.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <param name="resultSelector">
    ///   A function to create a result value from each group.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare keys.
    /// </param>
    /// <returns>
    ///   A collection of elements of type <c>TResult</c> where each element
    ///   represents a projection over a group and its key.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> or <c>resultSelector</c> is <c>
    ///   nil</c>.
    /// </exception>
    /// <remarks>
    ///   If <c>comparer</c> is <c>nil</c>, the default equality comparer is
    ///   used to compare keys.
    /// </remarks>
    class function GroupBy<T, TKey, TResult>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const resultSelector: Func<TKey, IEnumerable<T>, TResult>;
      const comparer: IEqualityComparer<TKey>): IEnumerable<TResult>; overload; static;

    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function and creates a result value from each group and its
    ///   key. The elements of each group are projected by using a specified
    ///   function.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <typeparam name="TElement">
    ///   The type of the elements in each grouping.
    /// </typeparam>
    /// <typeparam name="TResult">
    ///   The type of the result value returned by <c>resultSelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence whose elements to group.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <param name="elementSelector">
    ///   A function to map each source element to an element in a grouping.
    /// </param>
    /// <param name="resultSelector">
    ///   A function to create a result value from each group.
    /// </param>
    /// <returns>
    ///   A collection of elements of type <c>TResult</c> where each element
    ///   represents a projection over a group and its key.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> or <c>elementSelector</c> or <c>
    ///   resultSelector</c> is <c>nil</c>.
    /// </exception>
    class function GroupBy<T, TKey, TElement, TResult>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const elementSelector: Func<T, TElement>;
      const resultSelector: Func<TKey, IEnumerable<TElement>, TResult>): IEnumerable<TResult>; overload; static;

    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function and creates a result value from each group and its
    ///   key. Key values are compared by using a specified comparer, and the
    ///   elements of each group are projected by using a specified function.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <typeparam name="TElement">
    ///   The type of the elements in each grouping.
    /// </typeparam>
    /// <typeparam name="TResult">
    ///   The type of the result value returned by <c>resultSelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence whose elements to group.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <param name="elementSelector">
    ///   A function to map each source element to an element in a grouping.
    /// </param>
    /// <param name="resultSelector">
    ///   A function to create a result value from each group.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare keys.
    /// </param>
    /// <returns>
    ///   A collection of elements of type <c>TResult</c> where each element
    ///   represents a projection over a group and its key.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> or <c>elementSelector</c> or <c>
    ///   resultSelector</c> is <c>nil</c>.
    /// </exception>
    /// <remarks>
    ///   If <c>comparer</c> is <c>nil</c>, the default equality comparer is
    ///   used to compare keys.
    /// </remarks>
    class function GroupBy<T, TKey, TElement, TResult>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const elementSelector: Func<T, TElement>;
      const resultSelector: Func<TKey, IEnumerable<TElement>, TResult>;
      const comparer: IEqualityComparer<TKey>): IEnumerable<TResult>; overload; static;

    /// <summary>
    ///   Produces the set intersection of two sequences by using the default
    ///   equality comparer to compare values.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of the input sequences.
    /// </typeparam>
    /// <param name="first">
    ///   A sequence whose distinct elements that also appear in <c>second</c>
    ///   will be returned.
    /// </param>
    /// <param name="second">
    ///   A sequence whose distinct elements that also appear in <c>first</c>
    ///   will be returned.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements that form the set intersection
    ///   of two sequences.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>first</c> or <c>second</c> is <c>nil</c>.
    /// </exception>
    class function Intersect<T>(const first, second: IEnumerable<T>): IEnumerable<T>; overload; static;
      deprecated 'Use first.Intersect(second) instead';

    /// <summary>
    ///   Produces the set intersection of two sequences by using the specified
    ///   equality comparer to compare values.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of the input sequences.
    /// </typeparam>
    /// <param name="first">
    ///   A sequence whose distinct elements that also appear in <c>second</c>
    ///   will be returned.
    /// </param>
    /// <param name="second">
    ///   A sequence whose distinct elements that also appear in <c>first</c>
    ///   will be returned.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare values.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements that form the set intersection
    ///   of two sequences.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>first</c> or <c>second</c> is <c>nil</c>.
    /// </exception>
    /// <remarks>
    ///   If <c>comparer</c> is <c>nil</c>, the default equality comparer is
    ///   used to compare values.
    /// </remarks>
    class function Intersect<T>(const first, second: IEnumerable<T>;
      const comparer: IEqualityComparer<T>): IEnumerable<T>; overload; static;
      deprecated 'Use first.Intersect(second, comparer) instead';

    /// <summary>
    ///   Returns the maximum value in a generic sequence according to a
    ///   specified key selector function.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of key to compare elements by.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to determine the maximum value of.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <returns>
    ///   The value with the maximum key in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> is <c>nil</c>.
    /// </exception>
    class function MaxBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>): T; overload; static;

    /// <summary>
    ///   Returns the maximum value in a generic sequence according to a
    ///   specified key selector function and key comparer.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of key to compare elements by.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to determine the maximum value of.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <param name="comparer">
    ///   The comparer to compare keys.
    /// </param>
    /// <returns>
    ///   The value with the maximum key in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> or <c>comparer</c> is <c>nil</c>.
    /// </exception>
    class function MaxBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const comparer: IComparer<TKey>): T; overload; static;

    /// <summary>
    ///   Returns the minimum value in a generic sequence according to a
    ///   specified key selector function.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of key to compare elements by.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to determine the minimum value of.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <returns>
    ///   The value with the minimum key in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> is <c>nil</c>.
    /// </exception>
    class function MinBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>): T; overload; static;

    /// <summary>
    ///   Returns the minimum value in a generic sequence according to a
    ///   specified key selector function and key comparer.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of key to compare elements by.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to determine the minimum value of.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract the key for each element.
    /// </param>
    /// <param name="comparer">
    ///   The comparer to compare keys.
    /// </param>
    /// <returns>
    ///   The value with the minimum key in the sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> or <c>comparer</c> is <c>nil</c>.
    /// </exception>
    class function MinBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const comparer: IComparer<TKey>): T; overload; static;

    /// <summary>
    ///   Filters the elements of a sequence based on a specified type.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TResult">
    ///   The type to filter the elements of the sequence on.
    /// </typeparam>
    /// <param name="source">
    ///   The sequence whose elements to filter.
    /// </param>
    /// <returns>
    ///   A sequence that contains elements from the input sequence of type <c>
    ///   TResult</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> is <c>nil</c>.
    /// </exception>
    class function OfType<T, TResult>(const source: IEnumerable<T>): IEnumerable<TResult>; static;

    /// <summary>
    ///   Sorts the elements of a sequence in ascending order according to a
    ///   key.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to order.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract a key from an element.
    /// </param>
    /// <returns>
    ///   A sequence whose elements are sorted according to a key.
    /// </returns>
    class function OrderBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T,TKey>): IEnumerable<T>; overload; static;

    /// <summary>
    ///   Sorts the elements of a sequence in ascending order by using a
    ///   specified comparer.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to order.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract a key from an element.
    /// </param>
    /// <param name="comparer">
    ///   A comparer to compare keys.
    /// </param>
    /// <returns>
    ///   A sequence whose elements are sorted according to a key.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> or <c>comparer</c> is <c>nil</c>.
    /// </exception>
    class function OrderBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T,TKey>;
      const comparer: IComparer<TKey>): IEnumerable<T>; overload; static;

    /// <summary>
    ///   Sorts the elements of a sequence in descending order according to a
    ///   key.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to order.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract a key from an element.
    /// </param>
    /// <returns>
    ///   A sequence whose elements are sorted in descending order according to
    ///   a key.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> is <c>nil</c>.
    /// </exception>
    class function OrderByDescending<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T,TKey>): IEnumerable<T>; overload; static;

    /// <summary>
    ///   Sorts the elements of a sequence in descending order by using a
    ///   specified comparer.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to order.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract a key from an element.
    /// </param>
    /// <param name="comparer">
    ///   A comparer to compare keys.
    /// </param>
    /// <returns>
    ///   A sequence whose elements are sorted in descending order according to
    ///   a key.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> or <c>comparer</c> is <c>nil</c>.
    /// </exception>
    class function OrderByDescending<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T,TKey>;
      const comparer: IComparer<TKey>): IEnumerable<T>; overload; static;

    /// <summary>
    ///   Generates a sequence of integral numbers within a specified range.
    /// </summary>
    /// <param name="start">
    ///   The value of the first integer in the sequence.
    /// </param>
    /// <param name="count">
    ///   The number of sequential integers to generate.
    /// </param>
    /// <returns>
    ///   A read-only list that contains a range of sequential integral
    ///   numbers.
    /// </returns>
    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   <c>count</c> is less than 0. <br /><br />-or- <br /><br /><c>start</c>
    ///    + <c>count</c> -1 is larger than <c>MaxInt</c>.
    /// </exception>
    class function Range(start, count: Integer): IReadOnlyList<Integer>; static;

    /// <summary>
    ///   Generates a sequence that contains one repeated value.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the value to be repeated in the result sequence.
    /// </typeparam>
    /// <param name="element">
    ///   The value to be repeated.
    /// </param>
    /// <param name="count">
    ///   The number of times to repeat the value in the generated sequence.
    /// </param>
    /// <returns>
    ///   A sequence that contains a repeated value.
    /// </returns>
    /// <exception cref="EArgumentOutOfRangeException">
    ///   <c>count</c> is less than 0.
    /// </exception>
    class function Repeated<T>(const element: T; count: Integer): IEnumerable<T>; static;

    /// <summary>
    ///   Projects each element of a sequence into a new form by incorporating
    ///   the element's index.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TResult">
    ///   The type of the value returned by <c>selector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to invoke a transform function on.
    /// </param>
    /// <param name="selector">
    ///   A transform function to apply to each source element; the second
    ///   parameter of the function represents the index of the source element.
    /// </param>
    /// <returns>
    ///   A sequence whose elements are the result of invoking the transform
    ///   function on each element of <c>source</c>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>selector</c> is <c>nil</c>.
    /// </exception>
    class function Select<T, TResult>(const source: IEnumerable<T>;
      const selector: Func<T, TResult>): IEnumerable<TResult>; overload; static;

    /// <summary>
    ///   Projects each element of a sequence into a new form.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TResult">
    ///   The type of the value returned by <c>selector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to invoke a transform function on.
    /// </param>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   A sequence whose elements are the result of invoking the transform
    ///   function on each element of source.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>selector</c> is <c>nil</c>.
    /// </exception>
    class function Select<T, TResult>(const source: IEnumerable<T>;
      const selector: Func<T, Integer, TResult>): IEnumerable<TResult>; overload; static;

    /// <summary>
    ///   Projects each element of a sequence to an <see cref="Spring.Collections|IEnumerable&lt;T&gt;" />
    ///    and flattens the resulting sequences into one sequence.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TResult">
    ///   The type of the elements of the sequence returned by <c>selector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to project.
    /// </param>
    /// <param name="selector">
    ///   A transform function to apply to each element.
    /// </param>
    /// <returns>
    ///   A sequence whose elements are the result of invoking the one-to-many
    ///   transform function on each element of the input sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>selector</c> is <c>nil</c>.
    /// </exception>
    class function SelectMany<T, TResult>(const source: IEnumerable<T>;
      const selector: Func<T, IEnumerable<TResult>>): IEnumerable<TResult>; overload; static;

    /// <summary>
    ///   Projects each element of a sequence to an <see cref="Spring.Collections|IEnumerable&lt;T&gt;" />
    ///   , and flattens the resulting sequences into one sequence. The index
    ///   of each source element is used in the projected form of that element.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TResult">
    ///   The type of the elements of the sequence returned by <c>selector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to project.
    /// </param>
    /// <param name="selector">
    ///   A transform function to apply to each source element; the second
    ///   parameter of the function represents the index of the source element.
    /// </param>
    /// <returns>
    ///   A sequence whose elements are the result of invoking the one-to-many
    ///   transform function on each element of an input sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>selector</c> is <c>nil</c>.
    /// </exception>
    class function SelectMany<T, TResult>(const source: IEnumerable<T>;
      const selector: Func<T, Integer, IEnumerable<TResult>>): IEnumerable<TResult>; overload; static;

    /// <summary>
    ///   Projects each element of a sequence to an <see cref="Spring.Collections|IEnumerable&lt;T&gt;" />
    ///   , flattens the resulting sequences into one sequence, and invokes a
    ///   result selector function on each element therein.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TCollection">
    ///   The type of the intermediate elements collected by <c>
    ///   collectionSelector</c>.
    /// </typeparam>
    /// <typeparam name="TResult">
    ///   The type of the elements of the resulting sequence.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to project.
    /// </param>
    /// <param name="collectionSelector">
    ///   A transform function to apply to each element of the input sequence.
    /// </param>
    /// <param name="resultSelector">
    ///   A transform function to apply to each element of the intermediate
    ///   sequence.
    /// </param>
    /// <returns>
    ///   A sequence whose elements are the result of invoking the one-to-many
    ///   transform function <c>collectionSelector</c> on each element of <c>
    ///   source</c> and then mapping each of those sequence elements and their
    ///   corresponding source element to a result element.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>collectionSelector</c> or <c>resultSelector</c>
    ///   is <c>nil</c>.
    /// </exception>
    class function SelectMany<T, TCollection, TResult>(const source: IEnumerable<T>;
      const collectionSelector: Func<T, IEnumerable<TCollection>>;
      const resultSelector: Func<T, TCollection, TResult>): IEnumerable<TResult>; overload; static;

    /// <summary>
    ///   Projects each element of a sequence to an <see cref="Spring.Collections|IEnumerable&lt;T&gt;" />
    ///    , flattens the resulting sequences into one sequence, and invokes a
    ///   result selector function on each element therein. The index of each
    ///   source element is used in the intermediate projected form of that
    ///   element.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TCollection">
    ///   The type of the intermediate elements collected by <c>
    ///   collectionSelector</c>.
    /// </typeparam>
    /// <typeparam name="TResult">
    ///   The type of the elements of the resulting sequence.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence of values to project.
    /// </param>
    /// <param name="collectionSelector">
    ///   A transform function to apply to each source element; the second
    ///   parameter of the function represents the index of the source element.
    /// </param>
    /// <param name="resultSelector">
    ///   A transform function to apply to each element of the intermediate
    ///   sequence.
    /// </param>
    /// <returns>
    ///   A sequence whose elements are the result of invoking the one-to-many
    ///   transform function collectionSelector on each element of source and
    ///   then mapping each of those sequence elements and their corresponding
    ///   source element to a result element.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>collectionSelector</c> or <c>resultSelector</c>
    ///   is <c>nil</c>.
    /// </exception>
    class function SelectMany<T, TCollection, TResult>(const source: IEnumerable<T>;
      const collectionSelector: Func<T, Integer, IEnumerable<TCollection>>;
      const resultSelector: Func<T, TCollection, TResult>): IEnumerable<TResult>; overload; static;

    /// <summary>
    ///   Creates a dictionary from a sequence according to a specified key
    ///   selector function.
    /// </summary>
    /// <typeparam name="TSource">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence to create a dictionary from.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract a key from each element.
    /// </param>
    /// <returns>
    ///   A dictionary that contains keys and values. The values within each
    ///   group are in the same order as in source.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EArgumentException">
    ///   <c>keySelector</c> produces duplicate keys for two elements.
    /// </exception>
    class function ToDictionary<TSource, TKey>(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>): IOrderedDictionary<TKey, TSource>; overload; static;

    /// <summary>
    ///   Creates a dictionary from a sequence according to a specified key
    ///   selector function and key comparer.
    /// </summary>
    /// <typeparam name="TSource">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence to create a dictionary from.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract a key from each element.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare keys.
    /// </param>
    /// <returns>
    ///   A dictionary that contains keys and values. The values within each
    ///   group are in the same order as in source.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> is <c>nil</c>.
    /// </exception>
    /// <exception cref="Spring|EArgumentException">
    ///   <c>keySelector</c> produces duplicate keys for two elements.
    /// </exception>
    /// <remarks>
    ///   If <c>comparer</c> is <c>nil</c>, the default equality comparer is
    ///   used to compare keys.
    /// </remarks>
    class function ToDictionary<TSource, TKey>(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>;
      const comparer: IEqualityComparer<TKey>): IOrderedDictionary<TKey, TSource>; overload; static;

    /// <summary>
    ///   Creates a dictionary from a sequence according to specified key
    ///   selector and element selector functions.
    /// </summary>
    /// <typeparam name="TSource">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <typeparam name="TElement">
    ///   The type of the value returned by <c>elementSelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence to create a dictionary from.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract a key from each element.
    /// </param>
    /// <param name="elementSelector">
    ///   A transform function to produce a result element value from each
    ///   element.
    /// </param>
    /// <returns>
    ///   A dictionary that contains values of type <c>TElement</c> selected
    ///   from the input sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> or <c>elementSelector</c> is <c>
    ///   nil</c>.
    /// </exception>
    /// <exception cref="Spring|EArgumentException">
    ///   <c>keySelector</c> produces duplicate keys for two elements.
    /// </exception>
    class function ToDictionary<TSource, TKey, TElement>(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>;
      const elementSelector: Func<TSource, TElement>): IOrderedDictionary<TKey, TElement>; overload; static;

    /// <summary>
    ///   Creates a dictionary from a sequence according to specified key
    ///   selector and element selector functions.
    /// </summary>
    /// <typeparam name="TSource">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <typeparam name="TElement">
    ///   The type of the value returned by <c>elementSelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   A sequence to create a dictionary from.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract a key from each element.
    /// </param>
    /// <param name="elementSelector">
    ///   A transform function to produce a result element value from each
    ///   element.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare keys.
    /// </param>
    /// <returns>
    ///   A dictionary that contains values of type <c>TElement</c> selected
    ///   from the input sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> or <c>elementSelector</c> is <c>
    ///   nil</c>.
    /// </exception>
    /// <exception cref="Spring|EArgumentException">
    ///   <c>keySelector</c> produces duplicate keys for two elements.
    /// </exception>
    /// <remarks>
    ///   If <c>comparer</c> is <c>nil</c>, the default equality comparer is
    ///   used to compare keys.
    /// </remarks>
    class function ToDictionary<TSource, TKey, TElement>(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>;
      const elementSelector: Func<TSource, TElement>;
      const comparer: IEqualityComparer<TKey>): IOrderedDictionary<TKey, TElement>; overload; static;

    /// <summary>
    ///   Creates a lookup from a sequence according to a specified key
    ///   selector function.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   The sequence to create a lookup from.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract a key from each element.
    /// </param>
    /// <returns>
    ///   A lookup that contains keys and values. The values within each group
    ///   are in the same order as in source.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> is <c>nil</c>.
    /// </exception>
    /// <remarks>
    ///   The method returns a lookup, a one-to-many dictionary that maps keys
    ///   to collections of values. A lookup differs from a dictionary, which
    ///   performs a one-to-one mapping of keys to single values.
    /// </remarks>
    class function ToLookup<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>): ILookup<TKey, T>; overload; static;

    /// <summary>
    ///   Creates a lookup from a sequence according to a specified key
    ///   selector function.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   The sequence to create a lookup from.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract a key from each element.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare keys.
    /// </param>
    /// <returns>
    ///   A lookup that contains keys and values. The values within each group
    ///   are in the same order as in source.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> is <c>nil</c>.
    /// </exception>
    /// <remarks>
    ///   The method returns a lookup, a one-to-many dictionary that maps keys
    ///   to collections of values. A lookup differs from a dictionary, which
    ///   performs a one-to-one mapping of keys to single values. <br /><br />
    ///   If <c>comparer</c> is <c>nil</c>, the default equality comparer is
    ///   used to compare keys.
    /// </remarks>
    class function ToLookup<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const comparer: IEqualityComparer<TKey>): ILookup<TKey, T>; overload; static;

    /// <summary>
    ///   Creates a lookup from a sequence according to specified key selector
    ///   and element selector functions.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <typeparam name="TElement">
    ///   The type of the value returned by <c>elementSelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   The sequence to create a lookup from.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract a key from each element.
    /// </param>
    /// <param name="elementSelector">
    ///   A transform function to produce a result element value from each
    ///   element.
    /// </param>
    /// <returns>
    ///   A lookup that contains values of type <c>TElement</c> selected from
    ///   the input sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> is <c>nil</c>.
    /// </exception>
    /// <remarks>
    ///   The method returns a lookup, a one-to-many dictionary that maps keys
    ///   to collections of values. A lookup differs from a dictionary, which
    ///   performs a one-to-one mapping of keys to single values.
    /// </remarks>
    class function ToLookup<T, TKey, TElement>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const elementSelector: Func<T, TElement>): ILookup<TKey, TElement>; overload; static;

    /// <summary>
    ///   Creates a lookup from a sequence according to a specified key
    ///   selector function, a comparer and an element selector function.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of <c>source</c>.
    /// </typeparam>
    /// <typeparam name="TKey">
    ///   The type of the key returned by <c>keySelector</c>.
    /// </typeparam>
    /// <typeparam name="TElement">
    ///   The type of the value returned by <c>elementSelector</c>.
    /// </typeparam>
    /// <param name="source">
    ///   The sequence to create a lookup from.
    /// </param>
    /// <param name="keySelector">
    ///   A function to extract a key from each element.
    /// </param>
    /// <param name="elementSelector">
    ///   A transform function to produce a result element value from each
    ///   element.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare keys.
    /// </param>
    /// <returns>
    ///   A lookup that contains values of type <c>TElement</c> selected from
    ///   the input sequence.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>source</c> or <c>keySelector</c> or <c>elementSelector</c> is <c>
    ///   nil</c>.
    /// </exception>
    /// <remarks>
    ///   The method returns a lookup, a one-to-many dictionary that maps keys
    ///   to collections of values. A lookup differs from a dictionary, which
    ///   performs a one-to-one mapping of keys to single values. <br /><br />
    ///   If <c>comparer</c> is <c>nil</c>, the default equality comparer is
    ///   used to compare keys.
    /// </remarks>
    class function ToLookup<T, TKey, TElement>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const elementSelector: Func<T, TElement>;
      const comparer: IEqualityComparer<TKey>): ILookup<TKey, TElement>; overload; static;

    /// <summary>
    ///   Produces the set union of two sequences by using the default equality
    ///   comparer.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of the input sequences.
    /// </typeparam>
    /// <param name="first">
    ///   A sequence whose distinct elements form the first set for the union.
    /// </param>
    /// <param name="second">
    ///   A sequence whose distinct elements form the second set for the union.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements from both input sequences,
    ///   excluding duplicates.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>first</c> or <c>second</c> is <c>nil</c>.
    /// </exception>
    class function Union<T>(const first, second: IEnumerable<T>): IEnumerable<T>; overload; static;
      deprecated 'Use first.Union(second) instead';

    /// <summary>
    ///   Produces the set union of two sequences by using a specified equality
    ///   comparer.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the elements of the input sequences.
    /// </typeparam>
    /// <param name="first">
    ///   A sequence whose distinct elements form the first set for the union.
    /// </param>
    /// <param name="second">
    ///   A sequence whose distinct elements form the second set for the union.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare values.
    /// </param>
    /// <returns>
    ///   A sequence that contains the elements from both input sequences,
    ///   excluding duplicates.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>first</c> or <c>second</c> is <c>nil</c>.
    /// </exception>
    /// <remarks>
    ///   If <c>comparer</c> is <c>nil</c>, the default equality comparer is
    ///   used to compare values.
    /// </remarks>
    class function Union<T>(const first, second: IEnumerable<T>;
      const comparer: IEqualityComparer<T>): IEnumerable<T>; overload; static;
      deprecated 'Use first.Union(second, comparer) instead';

    /// <summary>
    ///   Produces a sequence of tuples with elements from the two specified
    ///   sequences.
    /// </summary>
    /// <typeparam name="TFirst">
    ///   The type of the elements of the first input sequence.
    /// </typeparam>
    /// <typeparam name="TSecond">
    ///   The type of the elements of the second input sequence.
    /// </typeparam>
    /// <param name="first">
    ///   The first sequence to merge.
    /// </param>
    /// <param name="second">
    ///   The second sequence to merge.
    /// </param>
    /// <returns>
    ///   A sequence of tuples with elements taken from the first and second
    ///   sequences, in that order.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>first</c> or <c>second</c> is <c>nil</c>.
    /// </exception>
    class function Zip<TFirst, TSecond>(
      const first: IEnumerable<TFirst>;
      const second: IEnumerable<TSecond>): IEnumerable<Tuple<TFirst,TSecond>>; overload; static;

    /// <summary>
    ///   Applies a specified function to the corresponding elements of two
    ///   sequences, producing a sequence of the results.
    /// </summary>
    /// <typeparam name="TFirst">
    ///   The type of the elements of the first input sequence.
    /// </typeparam>
    /// <typeparam name="TSecond">
    ///   The type of the elements of the second input sequence.
    /// </typeparam>
    /// <typeparam name="TResult">
    ///   The type of the elements of the result sequence.
    /// </typeparam>
    /// <param name="first">
    ///   The first sequence to merge.
    /// </param>
    /// <param name="second">
    ///   The second sequence to merge.
    /// </param>
    /// <param name="resultSelector">
    ///   A function that specifies how to merge the elements from the two
    ///   sequences.
    /// </param>
    /// <returns>
    ///   A sequence that contains merged elements of two input sequences.
    /// </returns>
    /// <exception cref="Spring|EArgumentNilException">
    ///   <c>first</c> or <c>second</c> or <c>resultSelector</c> is <c>nil</c>.
    /// </exception>
    class function Zip<TFirst, TSecond, TResult>(
      const first: IEnumerable<TFirst>;
      const second: IEnumerable<TSecond>;
      const resultSelector: Func<TFirst, TSecond, TResult>): IEnumerable<TResult>; overload; static;
  end;

  TStringComparer = Spring.Comparers.TStringComparer;

  TIdentityFunction<T> = record
  strict private const
    {$J+}
    VTable: Pointer = nil;
    Methods: array[0..3] of Pointer = (
      @NopQueryInterface,
      @NopRef,
      @NopRef,
      nil);
    {$IFNDEF WRITEABLECONST_ON}{$J-}{$ENDIF}
    function Invoke(const x: T): T;
    class function GetInstance: Func<T, T>; static;
  public
    class property Instance: Func<T, T> read GetInstance;
  end;

  TCollectionHelper = class helper for TCollection
  public
    function AsList: IList<TCollectionItem>; overload;
    function AsList<T: TCollectionItem>: IList<T>; overload;
  end;

  AutoInitAttribute = class(ManagedAttribute)
    constructor Create(ownsObjects: Boolean = True);
  end;

function GetElementType(typeInfo: PTypeInfo): PTypeInfo;

implementation

uses
  Rtti,
  Spring.Collections.Base,
  Spring.Collections.Dictionaries,
  Spring.Collections.Extensions,
  Spring.Collections.Lists,
  Spring.Collections.LinkedLists,
  Spring.Collections.MultiMaps,
  Spring.Collections.MultiSets,
  Spring.Collections.Queues,
  Spring.Collections.Sets,
  Spring.Collections.Stacks,
  Spring.ResourceStrings;


function GetElementType(typeInfo: PTypeInfo): PTypeInfo;
var
  typeParams: TArray<string>;
  qualifiedName: string;
  item: TRttiType;
begin
  typeParams := GetGenericTypeParameters(typeInfo.TypeName);
  if DynArrayLength(typeParams) <> 1 then
    Exit(nil);

  qualifiedName := typeParams[0];
  item := TType.Context.FindType(qualifiedName);
  if Assigned(item) then
    Result := item.Handle
  else
  begin
    for item in TType.Context.GetTypes do
      if SameText(item.Name, qualifiedName) then
        Exit(item.Handle);
    Result := nil;
  end;
end;

type
  TOfTypeIterator = class(TEnumerableBase<TObject>, IInterface, IEnumerable<TObject>)
  private type
  {$REGION 'Nested Types'}
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TOfTypeIterator;
      fResultClass: TClass;
      fEnumerator: IEnumerator<TObject>;
      fCurrent: TObject;
      function GetCurrent: TObject;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fSource: IEnumerable<TObject>;
    fResultClass: TClass;
  protected
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(const source: IEnumerable<TObject>; resultClass: TClass);
    function GetEnumerator: IEnumerator<TObject>;
  end;


{$REGION 'TPair<TKey, TValue>'}

constructor TPair<TKey, TValue>.Create(const key: TKey; const value: TValue);
begin
  Self.Key := key;
  Self.Value := value;
end;

{$ENDREGION}


{$REGION 'TCollections'}

{$IFDEF DELPHIXE7_UP}
class procedure TCollections.CreateDictionary_Int8_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int8,Int8>(Result) := TFoldedDictionary<Int8,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int8_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int8,Int16>(Result) := TFoldedDictionary<Int8,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int8_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int8,Int32>(Result) := TFoldedDictionary<Int8,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int8_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int8,Int64>(Result) := TFoldedDictionary<Int8,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int8_Method(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int8,TMethodPointer>(Result) := TFoldedDictionary<Int8,TMethodPointer>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<TMethodPointer>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int8_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int8,IInterface>(Result) := TFoldedDictionary<Int8,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int8_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int8,TObject>(Result) := TFoldedDictionary<Int8,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int8_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int8,string>(Result) := TFoldedDictionary<Int8,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int16_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int16,Int8>(Result) := TFoldedDictionary<Int16,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int16_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int16,Int16>(Result) := TFoldedDictionary<Int16,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int16_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int16,Int32>(Result) := TFoldedDictionary<Int16,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int16_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int16,Int64>(Result) := TFoldedDictionary<Int16,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int16_Method(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int16,TMethodPointer>(Result) := TFoldedDictionary<Int16,TMethodPointer>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<TMethodPointer>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int16_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int16,IInterface>(Result) := TFoldedDictionary<Int16,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int16_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int16,TObject>(Result) := TFoldedDictionary<Int16,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int16_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int16,string>(Result) := TFoldedDictionary<Int16,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int32_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int32,Int8>(Result) := TFoldedDictionary<Int32,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int32_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int32,Int16>(Result) := TFoldedDictionary<Int32,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int32_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int32,Int32>(Result) := TFoldedDictionary<Int32,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int32_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int32,Int64>(Result) := TFoldedDictionary<Int32,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int32_Method(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int32,TMethodPointer>(Result) := TFoldedDictionary<Int32,TMethodPointer>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<TMethodPointer>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int32_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int32,IInterface>(Result) := TFoldedDictionary<Int32,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int32_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int32,TObject>(Result) := TFoldedDictionary<Int32,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int32_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int32,string>(Result) := TFoldedDictionary<Int32,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int64_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int64,Int8>(Result) := TFoldedDictionary<Int64,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int64_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int64,Int16>(Result) := TFoldedDictionary<Int64,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int64_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int64,Int32>(Result) := TFoldedDictionary<Int64,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int64_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int64,Int64>(Result) := TFoldedDictionary<Int64,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int64_Method(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int64,TMethodPointer>(Result) := TFoldedDictionary<Int64,TMethodPointer>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<TMethodPointer>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int64_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int64,IInterface>(Result) := TFoldedDictionary<Int64,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int64_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int64,TObject>(Result) := TFoldedDictionary<Int64,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Int64_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<Int64,string>(Result) := TFoldedDictionary<Int64,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<IInterface,Int8>(Result) := TFoldedDictionary<IInterface,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<IInterface,Int16>(Result) := TFoldedDictionary<IInterface,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<IInterface,Int32>(Result) := TFoldedDictionary<IInterface,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<IInterface,Int64>(Result) := TFoldedDictionary<IInterface,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_Method(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<IInterface,TMethodPointer>(Result) := TFoldedDictionary<IInterface,TMethodPointer>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<TMethodPointer>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<IInterface,IInterface>(Result) := TFoldedDictionary<IInterface,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<IInterface,TObject>(Result) := TFoldedDictionary<IInterface,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<IInterface,string>(Result) := TFoldedDictionary<IInterface,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Object_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<TObject,Int8>(Result) := TFoldedDictionary<TObject,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Object_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<TObject,Int16>(Result) := TFoldedDictionary<TObject,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Object_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<TObject,Int32>(Result) := TFoldedDictionary<TObject,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Object_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<TObject,Int64>(Result) := TFoldedDictionary<TObject,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Object_Method(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<TObject,TMethodPointer>(Result) := TFoldedDictionary<TObject,TMethodPointer>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<TMethodPointer>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Object_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<TObject,IInterface>(Result) := TFoldedDictionary<TObject,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Object_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<TObject,TObject>(Result) := TFoldedDictionary<TObject,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Object_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<TObject,string>(Result) := TFoldedDictionary<TObject,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<string,Int8>(Result) := TFoldedDictionary<string,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<string,Int16>(Result) := TFoldedDictionary<string,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<string,Int32>(Result) := TFoldedDictionary<string,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<string,Int64>(Result) := TFoldedDictionary<string,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_Method(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<string,TMethodPointer>(Result) := TFoldedDictionary<string,TMethodPointer>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<TMethodPointer>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<string,IInterface>(Result) := TFoldedDictionary<string,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<string,TObject>(Result) := TFoldedDictionary<string,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IOrderedDictionary<string,string>(Result) := TFoldedDictionary<string,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int8_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int8,Int8>(Result) := TFoldedBidiDictionary<Int8,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int8_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int8,Int16>(Result) := TFoldedBidiDictionary<Int8,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int8_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int8,Int32>(Result) := TFoldedBidiDictionary<Int8,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int8_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int8,Int64>(Result) := TFoldedBidiDictionary<Int8,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int8_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int8,IInterface>(Result) := TFoldedBidiDictionary<Int8,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int8_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int8,TObject>(Result) := TFoldedBidiDictionary<Int8,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int8_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int8,string>(Result) := TFoldedBidiDictionary<Int8,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int8>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int16_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int16,Int8>(Result) := TFoldedBidiDictionary<Int16,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int16_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int16,Int16>(Result) := TFoldedBidiDictionary<Int16,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int16_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int16,Int32>(Result) := TFoldedBidiDictionary<Int16,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int16_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int16,Int64>(Result) := TFoldedBidiDictionary<Int16,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int16_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int16,IInterface>(Result) := TFoldedBidiDictionary<Int16,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int16_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int16,TObject>(Result) := TFoldedBidiDictionary<Int16,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int16_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int16,string>(Result) := TFoldedBidiDictionary<Int16,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int16>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int32_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int32,Int8>(Result) := TFoldedBidiDictionary<Int32,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int32_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int32,Int16>(Result) := TFoldedBidiDictionary<Int32,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int32_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int32,Int32>(Result) := TFoldedBidiDictionary<Int32,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int32_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int32,Int64>(Result) := TFoldedBidiDictionary<Int32,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int32_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int32,IInterface>(Result) := TFoldedBidiDictionary<Int32,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int32_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int32,TObject>(Result) := TFoldedBidiDictionary<Int32,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int32_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int32,string>(Result) := TFoldedBidiDictionary<Int32,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int32>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int64_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int64,Int8>(Result) := TFoldedBidiDictionary<Int64,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int64_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int64,Int16>(Result) := TFoldedBidiDictionary<Int64,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int64_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int64,Int32>(Result) := TFoldedBidiDictionary<Int64,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int64_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int64,Int64>(Result) := TFoldedBidiDictionary<Int64,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int64_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int64,IInterface>(Result) := TFoldedBidiDictionary<Int64,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int64_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int64,TObject>(Result) := TFoldedBidiDictionary<Int64,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Int64_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<Int64,string>(Result) := TFoldedBidiDictionary<Int64,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<Int64>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Interface_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<IInterface,Int8>(Result) := TFoldedBidiDictionary<IInterface,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Interface_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<IInterface,Int16>(Result) := TFoldedBidiDictionary<IInterface,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Interface_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<IInterface,Int32>(Result) := TFoldedBidiDictionary<IInterface,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Interface_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<IInterface,Int64>(Result) := TFoldedBidiDictionary<IInterface,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Interface_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<IInterface,IInterface>(Result) := TFoldedBidiDictionary<IInterface,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Interface_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<IInterface,TObject>(Result) := TFoldedBidiDictionary<IInterface,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Interface_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<IInterface,string>(Result) := TFoldedBidiDictionary<IInterface,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Object_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<TObject,Int8>(Result) := TFoldedBidiDictionary<TObject,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Object_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<TObject,Int16>(Result) := TFoldedBidiDictionary<TObject,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Object_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<TObject,Int32>(Result) := TFoldedBidiDictionary<TObject,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Object_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<TObject,Int64>(Result) := TFoldedBidiDictionary<TObject,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Object_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<TObject,IInterface>(Result) := TFoldedBidiDictionary<TObject,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Object_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<TObject,TObject>(Result) := TFoldedBidiDictionary<TObject,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Object_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<TObject,string>(Result) := TFoldedBidiDictionary<TObject,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<TObject>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_String_Int8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<string,Int8>(Result) := TFoldedBidiDictionary<string,Int8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<Int8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_String_Int16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<string,Int16>(Result) := TFoldedBidiDictionary<string,Int16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<Int16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_String_Int32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<string,Int32>(Result) := TFoldedBidiDictionary<string,Int32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<Int32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_String_Int64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<string,Int64>(Result) := TFoldedBidiDictionary<string,Int64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<Int64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_String_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<string,IInterface>(Result) := TFoldedBidiDictionary<string,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_String_Object(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<string,TObject>(Result) := TFoldedBidiDictionary<string,TObject>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<TObject>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_String_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<string,string>(Result) := TFoldedBidiDictionary<string,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int8_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,Int8>(Result) := TFoldedMultiMap<Int8,Int8>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int8>(keyComparer),
    nil, Helper<Int8,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int8_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,Int16>(Result) := TFoldedMultiMap<Int8,Int16>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int8>(keyComparer),
    nil, Helper<Int8,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int8_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,Int32>(Result) := TFoldedMultiMap<Int8,Int32>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int8>(keyComparer),
    nil, Helper<Int8,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int8_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,Int64>(Result) := TFoldedMultiMap<Int8,Int64>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int8>(keyComparer),
    nil, Helper<Int8,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int8_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,IInterface>(Result) := TFoldedMultiMap<Int8,IInterface>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int8>(keyComparer),
    nil, Helper<Int8,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int8_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,TObject>(Result) := TFoldedMultiMap<Int8,TObject>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int8>(keyComparer),
    nil, Helper<Int8,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int8_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,string>(Result) := TFoldedMultiMap<Int8,string>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int8>(keyComparer),
    nil, Helper<Int8,string>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int16_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,Int8>(Result) := TFoldedMultiMap<Int16,Int8>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int16>(keyComparer),
    nil, Helper<Int16,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int16_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,Int16>(Result) := TFoldedMultiMap<Int16,Int16>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int16>(keyComparer),
    nil, Helper<Int16,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int16_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,Int32>(Result) := TFoldedMultiMap<Int16,Int32>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int16>(keyComparer),
    nil, Helper<Int16,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int16_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,Int64>(Result) := TFoldedMultiMap<Int16,Int64>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int16>(keyComparer),
    nil, Helper<Int16,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int16_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,IInterface>(Result) := TFoldedMultiMap<Int16,IInterface>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int16>(keyComparer),
    nil, Helper<Int16,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int16_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,TObject>(Result) := TFoldedMultiMap<Int16,TObject>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int16>(keyComparer),
    nil, Helper<Int16,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int16_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,string>(Result) := TFoldedMultiMap<Int16,string>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int16>(keyComparer),
    nil, Helper<Int16,string>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int32_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,Int8>(Result) := TFoldedMultiMap<Int32,Int8>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int32>(keyComparer),
    nil, Helper<Int32,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int32_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,Int16>(Result) := TFoldedMultiMap<Int32,Int16>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int32>(keyComparer),
    nil, Helper<Int32,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int32_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,Int32>(Result) := TFoldedMultiMap<Int32,Int32>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int32>(keyComparer),
    nil, Helper<Int32,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int32_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,Int64>(Result) := TFoldedMultiMap<Int32,Int64>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int32>(keyComparer),
    nil, Helper<Int32,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int32_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,IInterface>(Result) := TFoldedMultiMap<Int32,IInterface>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int32>(keyComparer),
    nil, Helper<Int32,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int32_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,TObject>(Result) := TFoldedMultiMap<Int32,TObject>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int32>(keyComparer),
    nil, Helper<Int32,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int32_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,string>(Result) := TFoldedMultiMap<Int32,string>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int32>(keyComparer),
    nil, Helper<Int32,string>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int64_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,Int8>(Result) := TFoldedMultiMap<Int64,Int8>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int64>(keyComparer),
    nil, Helper<Int64,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int64_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,Int16>(Result) := TFoldedMultiMap<Int64,Int16>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int64>(keyComparer),
    nil, Helper<Int64,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int64_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,Int32>(Result) := TFoldedMultiMap<Int64,Int32>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int64>(keyComparer),
    nil, Helper<Int64,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int64_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,Int64>(Result) := TFoldedMultiMap<Int64,Int64>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int64>(keyComparer),
    nil, Helper<Int64,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int64_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,IInterface>(Result) := TFoldedMultiMap<Int64,IInterface>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int64>(keyComparer),
    nil, Helper<Int64,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int64_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,TObject>(Result) := TFoldedMultiMap<Int64,TObject>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int64>(keyComparer),
    nil, Helper<Int64,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Int64_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,string>(Result) := TFoldedMultiMap<Int64,string>.Create(
    keyType, valueType, elementType, IEqualityComparer<Int64>(keyComparer),
    nil, Helper<Int64,string>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Interface_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,Int8>(Result) := TFoldedMultiMap<IInterface,Int8>.Create(
    keyType, valueType, elementType, IEqualityComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Interface_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType,valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,Int16>(Result) := TFoldedMultiMap<IInterface,Int16>.Create(
    keyType, valueType, elementType, IEqualityComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Interface_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,Int32>(Result) := TFoldedMultiMap<IInterface,Int32>.Create(
    keyType, valueType, elementType, IEqualityComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Interface_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,Int64>(Result) := TFoldedMultiMap<IInterface,Int64>.Create(
    keyType, valueType, elementType, IEqualityComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Interface_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,IInterface>(Result) := TFoldedMultiMap<IInterface,IInterface>.Create(
    keyType, valueType, elementType, IEqualityComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Interface_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,TObject>(Result) := TFoldedMultiMap<IInterface,TObject>.Create(
    keyType, valueType, elementType, IEqualityComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Interface_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,string>(Result) := TFoldedMultiMap<IInterface,string>.Create(
    keyType, valueType, elementType, IEqualityComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,string>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Object_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,Int8>(Result) := TFoldedMultiMap<TObject,Int8>.Create(
    keyType, valueType, elementType, IEqualityComparer<TObject>(keyComparer),
    nil, Helper<TObject,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Object_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,Int16>(Result) := TFoldedMultiMap<TObject,Int16>.Create(
    keyType, valueType, elementType, IEqualityComparer<TObject>(keyComparer),
    nil, Helper<TObject,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Object_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,Int32>(Result) := TFoldedMultiMap<TObject,Int32>.Create(
    keyType, valueType, elementType, IEqualityComparer<TObject>(keyComparer),
    nil, Helper<TObject,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Object_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,Int64>(Result) := TFoldedMultiMap<TObject,Int64>.Create(
    keyType, valueType, elementType, IEqualityComparer<TObject>(keyComparer),
    nil, Helper<TObject,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Object_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,IInterface>(Result) := TFoldedMultiMap<TObject,IInterface>.Create(
    keyType, valueType, elementType, IEqualityComparer<TObject>(keyComparer),
    nil, Helper<TObject,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Object_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,TObject>(Result) := TFoldedMultiMap<TObject,TObject>.Create(
    keyType, valueType, elementType, IEqualityComparer<TObject>(keyComparer),
    nil, Helper<TObject,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_Object_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,string>(Result) := TFoldedMultiMap<TObject,string>.Create(
    keyType, valueType, elementType, IEqualityComparer<TObject>(keyComparer),
    nil, Helper<TObject,string>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_String_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,Int8>(Result) := TFoldedMultiMap<string,Int8>.Create(
    keyType, valueType, elementType, IEqualityComparer<string>(keyComparer),
    nil, Helper<string,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_String_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,Int16>(Result) := TFoldedMultiMap<string,Int16>.Create(
    keyType, valueType, elementType, IEqualityComparer<string>(keyComparer),
    nil, Helper<string,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_String_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,Int32>(Result) := TFoldedMultiMap<string,Int32>.Create(
    keyType, valueType, elementType, IEqualityComparer<string>(keyComparer),
    nil, Helper<string,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_String_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,Int64>(Result) := TFoldedMultiMap<string,Int64>.Create(
    keyType, valueType, elementType, IEqualityComparer<string>(keyComparer),
    nil, Helper<string,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_String_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,IInterface>(Result) := TFoldedMultiMap<string,IInterface>.Create(
    keyType, valueType, elementType, IEqualityComparer<string>(keyComparer),
    nil, Helper<string,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_String_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,TObject>(Result) := TFoldedMultiMap<string,TObject>.Create(
    keyType, valueType, elementType, IEqualityComparer<string>(keyComparer),
    nil, Helper<string,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateListMultiMap_String_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,string>(Result) := TFoldedMultiMap<string,string>.Create(
    keyType, valueType, elementType, IEqualityComparer<string>(keyComparer),
    nil, Helper<string,string>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int8_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,Int8>(Result) := TFoldedSortedMultiMap<Int8,Int8>.Create(
    keyType, valueType, elementType, IComparer<Int8>(keyComparer),
    nil, Helper<Int8,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int8_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,Int16>(Result) := TFoldedSortedMultiMap<Int8,Int16>.Create(
    keyType, valueType, elementType, IComparer<Int8>(keyComparer),
    nil, Helper<Int8,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int8_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,Int32>(Result) := TFoldedSortedMultiMap<Int8,Int32>.Create(
    keyType, valueType, elementType, IComparer<Int8>(keyComparer),
    nil, Helper<Int8,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int8_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,Int64>(Result) := TFoldedSortedMultiMap<Int8,Int64>.Create(
    keyType, valueType, elementType, IComparer<Int8>(keyComparer),
    nil, Helper<Int8,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int8_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,IInterface>(Result) := TFoldedSortedMultiMap<Int8,IInterface>.Create(
    keyType, valueType, elementType, IComparer<Int8>(keyComparer),
    nil, Helper<Int8,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int8_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,TObject>(Result) := TFoldedSortedMultiMap<Int8,TObject>.Create(
    keyType, valueType, elementType, IComparer<Int8>(keyComparer),
    nil, Helper<Int8,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int8_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int8,string>(Result) := TFoldedSortedMultiMap<Int8,string>.Create(
    keyType, valueType, elementType, IComparer<Int8>(keyComparer),
    nil, Helper<Int8,string>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int16_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,Int8>(Result) := TFoldedSortedMultiMap<Int16,Int8>.Create(
    keyType, valueType, elementType, IComparer<Int16>(keyComparer),
    nil, Helper<Int16,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int16_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,Int16>(Result) := TFoldedSortedMultiMap<Int16,Int16>.Create(
    keyType, valueType, elementType, IComparer<Int16>(keyComparer),
    nil, Helper<Int16,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int16_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,Int32>(Result) := TFoldedSortedMultiMap<Int16,Int32>.Create(
    keyType, valueType, elementType, IComparer<Int16>(keyComparer),
    nil, Helper<Int16,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int16_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,Int64>(Result) := TFoldedSortedMultiMap<Int16,Int64>.Create(
    keyType, valueType, elementType, IComparer<Int16>(keyComparer),
    nil, Helper<Int16,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int16_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,IInterface>(Result) := TFoldedSortedMultiMap<Int16,IInterface>.Create(
    keyType, valueType, elementType, IComparer<Int16>(keyComparer),
    nil, Helper<Int16,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int16_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,TObject>(Result) := TFoldedSortedMultiMap<Int16,TObject>.Create(
    keyType, valueType, elementType, IComparer<Int16>(keyComparer),
    nil, Helper<Int16,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int16_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int16,string>(Result) := TFoldedSortedMultiMap<Int16,string>.Create(
    keyType, valueType, elementType, IComparer<Int16>(keyComparer),
    nil, Helper<Int16,string>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int32_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,Int8>(Result) := TFoldedSortedMultiMap<Int32,Int8>.Create(
    keyType, valueType, elementType, IComparer<Int32>(keyComparer),
    nil, Helper<Int32,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int32_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,Int16>(Result) := TFoldedSortedMultiMap<Int32,Int16>.Create(
    keyType, valueType, elementType, IComparer<Int32>(keyComparer),
    nil, Helper<Int32,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int32_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,Int32>(Result) := TFoldedSortedMultiMap<Int32,Int32>.Create(
    keyType, valueType, elementType, IComparer<Int32>(keyComparer),
    nil, Helper<Int32,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int32_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,Int64>(Result) := TFoldedSortedMultiMap<Int32,Int64>.Create(
    keyType, valueType, elementType, IComparer<Int32>(keyComparer),
    nil, Helper<Int32,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int32_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,IInterface>(Result) := TFoldedSortedMultiMap<Int32,IInterface>.Create(
    keyType, valueType, elementType, IComparer<Int32>(keyComparer),
    nil, Helper<Int32,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int32_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,TObject>(Result) := TFoldedSortedMultiMap<Int32,TObject>.Create(
    keyType, valueType, elementType, IComparer<Int32>(keyComparer),
    nil, Helper<Int32,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int32_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int32,string>(Result) := TFoldedSortedMultiMap<Int32,string>.Create(
    keyType, valueType, elementType, IComparer<Int32>(keyComparer),
    nil, Helper<Int32,string>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int64_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,Int8>(Result) := TFoldedSortedMultiMap<Int64,Int8>.Create(
    keyType, valueType, elementType, IComparer<Int64>(keyComparer),
    nil, Helper<Int64,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int64_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,Int16>(Result) := TFoldedSortedMultiMap<Int64,Int16>.Create(
    keyType, valueType, elementType, IComparer<Int64>(keyComparer),
    nil, Helper<Int64,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int64_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,Int32>(Result) := TFoldedSortedMultiMap<Int64,Int32>.Create(
    keyType, valueType, elementType, IComparer<Int64>(keyComparer),
    nil, Helper<Int64,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int64_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,Int64>(Result) := TFoldedSortedMultiMap<Int64,Int64>.Create(
    keyType, valueType, elementType, IComparer<Int64>(keyComparer),
    nil, Helper<Int64,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int64_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,IInterface>(Result) := TFoldedSortedMultiMap<Int64,IInterface>.Create(
    keyType, valueType, elementType, IComparer<Int64>(keyComparer),
    nil, Helper<Int64,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int64_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,TObject>(Result) := TFoldedSortedMultiMap<Int64,TObject>.Create(
    keyType, valueType, elementType, IComparer<Int64>(keyComparer),
    nil, Helper<Int64,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Int64_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<Int64,string>(Result) := TFoldedSortedMultiMap<Int64,string>.Create(
    keyType, valueType, elementType, IComparer<Int64>(keyComparer),
    nil, Helper<Int64,string>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Interface_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,Int8>(Result) := TFoldedSortedMultiMap<IInterface,Int8>.Create(
    keyType, valueType, elementType, IComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Interface_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType,valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,Int16>(Result) := TFoldedSortedMultiMap<IInterface,Int16>.Create(
    keyType, valueType, elementType, IComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Interface_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,Int32>(Result) := TFoldedSortedMultiMap<IInterface,Int32>.Create(
    keyType, valueType, elementType, IComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Interface_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,Int64>(Result) := TFoldedSortedMultiMap<IInterface,Int64>.Create(
    keyType, valueType, elementType, IComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Interface_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,IInterface>(Result) := TFoldedSortedMultiMap<IInterface,IInterface>.Create(
    keyType, valueType, elementType, IComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Interface_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,TObject>(Result) := TFoldedSortedMultiMap<IInterface,TObject>.Create(
    keyType, valueType, elementType, IComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Interface_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,string>(Result) := TFoldedSortedMultiMap<IInterface,string>.Create(
    keyType, valueType, elementType, IComparer<IInterface>(keyComparer),
    nil, Helper<IInterface,string>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Object_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,Int8>(Result) := TFoldedSortedMultiMap<TObject,Int8>.Create(
    keyType, valueType, elementType, IComparer<TObject>(keyComparer),
    nil, Helper<TObject,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Object_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,Int16>(Result) := TFoldedSortedMultiMap<TObject,Int16>.Create(
    keyType, valueType, elementType, IComparer<TObject>(keyComparer),
    nil, Helper<TObject,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Object_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,Int32>(Result) := TFoldedSortedMultiMap<TObject,Int32>.Create(
    keyType, valueType, elementType, IComparer<TObject>(keyComparer),
    nil, Helper<TObject,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Object_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,Int64>(Result) := TFoldedSortedMultiMap<TObject,Int64>.Create(
    keyType, valueType, elementType, IComparer<TObject>(keyComparer),
    nil, Helper<TObject,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Object_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,IInterface>(Result) := TFoldedSortedMultiMap<TObject,IInterface>.Create(
    keyType, valueType, elementType, IComparer<TObject>(keyComparer),
    nil, Helper<TObject,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Object_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,TObject>(Result) := TFoldedSortedMultiMap<TObject,TObject>.Create(
    keyType, valueType, elementType, IComparer<TObject>(keyComparer),
    nil, Helper<TObject,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_Object_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<TObject,string>(Result) := TFoldedSortedMultiMap<TObject,string>.Create(
    keyType, valueType, elementType, IComparer<TObject>(keyComparer),
    nil, Helper<TObject,string>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_String_Int8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,Int8>(Result) := TFoldedSortedMultiMap<string,Int8>.Create(
    keyType, valueType, elementType, IComparer<string>(keyComparer),
    nil, Helper<string,Int8>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_String_Int16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,Int16>(Result) := TFoldedSortedMultiMap<string,Int16>.Create(
    keyType, valueType, elementType, IComparer<string>(keyComparer),
    nil, Helper<string,Int16>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_String_Int32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,Int32>(Result) := TFoldedSortedMultiMap<string,Int32>.Create(
    keyType, valueType, elementType, IComparer<string>(keyComparer),
    nil, Helper<string,Int32>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_String_Int64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,Int64>(Result) := TFoldedSortedMultiMap<string,Int64>.Create(
    keyType, valueType, elementType, IComparer<string>(keyComparer),
    nil, Helper<string,Int64>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_String_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,IInterface>(Result) := TFoldedSortedMultiMap<string,IInterface>.Create(
    keyType, valueType, elementType, IComparer<string>(keyComparer),
    nil, Helper<string,IInterface>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_String_Object(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,TObject>(Result) := TFoldedSortedMultiMap<string,TObject>.Create(
    keyType, valueType, elementType, IComparer<string>(keyComparer),
    nil, Helper<string,TObject>.CreateListCollection, ownerships);
end;

class procedure TCollections.CreateSortedListMultiMap_String_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,string>(Result) := TFoldedSortedMultiMap<string,string>.Create(
    keyType, valueType, elementType, IComparer<string>(keyComparer),
    nil, Helper<string,string>.CreateListCollection, ownerships);
end;
{$ENDIF}

class procedure TCollections.Helper<TKey, TValue>.CreateListCollection(
  const key; const comparer: IInterface; elementType: PTypeInfo; var result);
begin
  IGroupingInternal<TKey, TValue>(result) := TValueList<TKey, TValue>.Create(
    TKey(key), elementType);
end;

class procedure TCollections.Helper<TKey, TValue>.CreateHashSetCollection(
  const key; const comparer: IInterface; elementType: PTypeInfo; var result);
begin
  IGroupingInternal<TKey, TValue>(result) := TValueHashSet<TKey, TValue>.Create(
    TKey(key), elementType, IEqualityComparer<TValue>(comparer));
end;

class procedure TCollections.Helper<TKey, TValue>.CreateTreeSetCollection(
  const key; const comparer: IInterface; elementType: PTypeInfo; var result);
begin
  IGroupingInternal<TKey, TValue>(result) := TValueTreeSet<TKey, TValue>.Create(
    TKey(key), elementType, IComparer<TValue>(comparer));
end;

class procedure TCollections.CreateList_Int8(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<Int8>(result) := TFoldedList<Int8>.Create(elementType, IComparer<Int8>(comparer));
end;

class procedure TCollections.CreateList_Int16(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<Int16>(result) := TFoldedList<Int16>.Create(elementType, IComparer<Int16>(comparer));
end;

class procedure TCollections.CreateList_Int32(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<Int32>(result) := TFoldedList<Int32>.Create(elementType, IComparer<Int32>(comparer));
end;

class procedure TCollections.CreateList_Int64(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<Int64>(result) := TFoldedList<Int64>.Create(elementType, IComparer<Int64>(comparer));
end;

class procedure TCollections.CreateList_Method(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<TMethodPointer>(Result) := TFoldedList<TMethodPointer>.Create(elementType, IComparer<TMethodPointer>(comparer));
end;

class procedure TCollections.CreateList_Interface(var result; elementType: Pointer);
begin
  IList<IInterface>(Result) := TFoldedList<IInterface>.Create(elementType, nil);
end;

class procedure TCollections.CreateList_Interface(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<IInterface>(Result) := TFoldedList<IInterface>.Create(elementType, IComparer<IInterface>(comparer));
end;

class procedure TCollections.CreateList_Object(var result; elementType: Pointer);
begin
  IList<TObject>(Result) := TFoldedList<TObject>.Create(elementType, nil, True);
end;

class procedure TCollections.CreateList_Object(ownsObjects: Boolean; var result; elementType: Pointer);
begin
  IList<TObject>(Result) := TFoldedList<TObject>.Create(elementType, nil, ownsObjects);
end;

class procedure TCollections.CreateList_Object(comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer);
begin
  IList<TObject>(Result) := TFoldedList<TObject>.Create(elementType, IComparer<TObject>(comparer), ownsObjects);
end;

class procedure TCollections.CreateList_String(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<string>(Result) := TFoldedList<string>.Create(elementType, IComparer<string>(comparer));
end;

class procedure TCollections.CreateSortedList_Int8(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<Int8>(result) := TFoldedSortedList<Int8>.Create(elementType, IComparer<Int8>(comparer));
end;

class procedure TCollections.CreateSortedList_Int16(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<Int16>(result) := TFoldedSortedList<Int16>.Create(elementType, IComparer<Int16>(comparer));
end;

class procedure TCollections.CreateSortedList_Int32(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<Int32>(result) := TFoldedSortedList<Int32>.Create(elementType, IComparer<Int32>(comparer));
end;

class procedure TCollections.CreateSortedList_Int64(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<Int64>(result) := TFoldedSortedList<Int64>.Create(elementType, IComparer<Int64>(comparer));
end;

class procedure TCollections.CreateSortedList_Interface(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<IInterface>(result) := TFoldedSortedList<IInterface>.Create(elementType, IComparer<IInterface>(comparer));
end;

class procedure TCollections.CreateSortedList_Object(comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer);
begin
  IList<TObject>(result) := TFoldedSortedList<TObject>.Create(elementType, IComparer<TObject>(comparer), ownsObjects);
end;

class procedure TCollections.CreateSortedList_String(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<string>(result) := TFoldedSortedList<string>.Create(elementType, IComparer<string>(comparer));
end;

class procedure TCollections.CreateHashSet_Int8(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  IOrderedSet<Int8>(result) := TFoldedHashSet<Int8>.Create(elementType, capacity, IEqualityComparer<Int8>(comparer));
end;

class procedure TCollections.CreateHashSet_Int16(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  IOrderedSet<Int16>(result) := TFoldedHashSet<Int16>.Create(elementType, capacity, IEqualityComparer<Int16>(comparer));
end;

class procedure TCollections.CreateHashSet_Int32(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  IOrderedSet<Int32>(result) := TFoldedHashSet<Int32>.Create(elementType, capacity, IEqualityComparer<Int32>(comparer));
end;

class procedure TCollections.CreateHashSet_Int64(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  IOrderedSet<Int64>(result) := TFoldedHashSet<Int64>.Create(elementType, capacity, IEqualityComparer<Int64>(comparer));
end;

class procedure TCollections.CreateHashSet_Interface(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  IOrderedSet<IInterface>(result) := TFoldedHashSet<IInterface>.Create(elementType, capacity, IEqualityComparer<IInterface>(comparer));
end;

class procedure TCollections.CreateHashSet_Object(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  IOrderedSet<TObject>(result) := TFoldedHashSet<TObject>.Create(elementType, capacity, IEqualityComparer<TObject>(comparer));
end;

class procedure TCollections.CreateHashSet_String(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  IOrderedSet<string>(result) := TFoldedHashSet<string>.Create(elementType, capacity, IEqualityComparer<string>(comparer));
end;

class procedure TCollections.CreateQueue_Object(capacity: Integer; comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer);
begin
  IQueue<TObject>(result) := TFoldedQueue<TObject>.Create(elementType, IComparer<TObject>(comparer), ownsObjects);
  IQueue<TObject>(result).Capacity := capacity;
end;

class procedure TCollections.CreateQueue_Interface(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  IQueue<IInterface>(result) := TFoldedQueue<IInterface>.Create(elementType, IComparer<IInterface>(comparer));
  IQueue<IInterface>(result).Capacity := capacity;
end;

class procedure TCollections.CreateStack_Interface(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  IStack<IInterface>(result) := TFoldedStack<IInterface>.Create(elementType, IComparer<IInterface>(comparer));
  IStack<IInterface>(result).Capacity := capacity;
end;

class procedure TCollections.CreateStack_Object(capacity: Integer; comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer);
begin
  IStack<TObject>(result) := TFoldedStack<TObject>.Create(elementType, IComparer<TObject>(comparer), ownsObjects);
  IStack<TObject>(result).Capacity := capacity;
end;

class procedure TCollections.CreateObservableList_Interface(var result; elementType: Pointer);
begin
  IList<IInterface>(result) := TObservableInterfaceList.Create(elementType, nil);
end;

class procedure TCollections.CreateObservableList_Object(ownsObjects: Boolean; var result; elementType: Pointer);
begin
  IList<TObject>(result) := TObservableObjectList.Create(elementType, nil, ownsObjects);
end;

class function TCollections.CreateList<T>: IList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateList_Object(False, Result, TypeInfo(T));
    tkInterface: CreateList_Interface(Result, TypeInfo(T));
    tkUString: CreateList_String(nil, Result, TypeInfo(T));
    tkMethod: CreateList_Method(nil, Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateList_Int8(nil, Result, TypeInfo(T));
        2: CreateList_Int16(nil, Result, TypeInfo(T));
        4: CreateList_Int32(nil, Result, TypeInfo(T));
        8: CreateList_Int64(nil, Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TList<T>.Create;
  end;
end;

class function TCollections.CreateList<T>(const comparer: IComparer<T>): IList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateList_Object(Pointer(comparer), False, Result, TypeInfo(T));
    tkInterface: CreateList_Interface(Pointer(comparer), Result, TypeInfo(T));
    tkUString: CreateList_String(Pointer(comparer), Result, TypeInfo(T));
    tkMethod: CreateList_Method(Pointer(comparer), Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateList_Int8(Pointer(comparer), Result, TypeInfo(T));
        2: CreateList_Int16(Pointer(comparer), Result, TypeInfo(T));
        4: CreateList_Int32(Pointer(comparer), Result, TypeInfo(T));
        8: CreateList_Int64(Pointer(comparer), Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TList<T>.Create(comparer);
  end;
end;

class function TCollections.CreateList<T>(
  const comparer: TComparison<T>): IList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateList_Object(PPointer(@comparer)^, False, Result, TypeInfo(T));
    tkInterface: CreateList_Interface(PPointer(@comparer)^, Result, TypeInfo(T));
    tkUString: CreateList_String(PPointer(@comparer)^, Result, TypeInfo(T));
    tkMethod: CreateList_Method(PPointer(@comparer)^, Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateList_Int8(PPointer(@comparer)^, Result, TypeInfo(T));
        2: CreateList_Int16(PPointer(@comparer)^, Result, TypeInfo(T));
        4: CreateList_Int32(PPointer(@comparer)^, Result, TypeInfo(T));
        8: CreateList_Int64(PPointer(@comparer)^, Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TList<T>.Create(IComparer<T>(PPointer(@comparer)^));
  end;
end;

class function TCollections.CreateList<T>(const values: array of T): IList<T>;
begin
  Result := CreateList<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateList<T>(const values: IEnumerable<T>): IList<T>;
begin
  Result := CreateList<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateList<T>(ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateList<T>(const comparer: IComparer<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(Pointer(comparer), ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateList<T>(const comparer: TComparison<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(PPointer(@comparer)^, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateObjectList(elementType: PTypeInfo; //FI:W521
  ownsObjects: Boolean): IObjectList;
begin
  CreateList_Object(ownsObjects, Result, elementType)
end;

class function TCollections.CreateObjectList<T>: IList<T>;
begin
  CreateList_Object(Result, TypeInfo(T));
end;

class function TCollections.CreateObjectList<T>(ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateObjectList<T>(const comparer: IComparer<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(Pointer(comparer), ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateObjectList<T>(const comparer: TComparison<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(PPointer(@comparer)^, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateObjectList<T>(const values: array of T;
  ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(ownsObjects, Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateObjectList<T>(const values: IEnumerable<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(ownsObjects, Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateInterfaceList(
  elementType: PTypeInfo): IInterfaceList;
begin
  CreateList_Interface(Result, elementType);
end;

class function TCollections.CreateInterfaceList<T>: IList<T>;
begin
  CreateList_Interface(Result, TypeInfo(T));
end;

class function TCollections.CreateInterfaceList<T>(
  const comparer: IComparer<T>): IList<T>;
begin
  CreateList_Interface(Pointer(comparer), Result, TypeInfo(T));
end;

class function TCollections.CreateInterfaceList<T>(
  const comparer: TComparison<T>): IList<T>;
begin
  CreateList_Interface(PPointer(@comparer)^, Result, TypeInfo(T));
end;

class function TCollections.CreateInterfaceList<T>(
  const values: array of T): IList<T>;
begin
  CreateList_Interface(Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateInterfaceList<T>(
  const values: IEnumerable<T>): IList<T>;
begin
  CreateList_Interface(Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateStringList: IList<string>;
begin
  CreateList_String(nil, Result, TypeInfo(string));
end;

class function TCollections.CreateStringList(
  const comparer: IComparer<string>): IList<string>;
begin
  CreateList_String(PPointer(@comparer)^, Result, TypeInfo(string));
end;

class function TCollections.CreateStringList(
  const comparer: TComparison<string>): IList<string>;
begin
  CreateList_String(PPointer(@comparer)^, Result, TypeInfo(string));
end;

class function TCollections.CreateStringList(
  const values: array of string): IList<string>;
begin
  CreateList_String(nil, Result, TypeInfo(string));
  Result.AddRange(values);
end;

class function TCollections.CreateStringList(
  const values: IEnumerable<string>): IList<string>;
begin
  CreateList_String(nil, Result, TypeInfo(string));
  Result.AddRange(values);
end;

class function TCollections.CreateStringList(const strings: TStrings;
  ownsObject: Boolean): IList<string>;
begin
  Result := TStringsAdapter.Create(strings, ownsObject);
end;

class function TCollections.CreateObservableList<T>(
  ownsObjects: Boolean): IList<T>;
begin
  CreateObservableList_Object(ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateObservableInterfaceList<T>: IList<T>;
begin
  CreateObservableList_Interface(Result, TypeInfo(T));
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IOrderedDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds + tkMethods) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_Object_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_Object_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_Object_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_Object_Method(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_Object_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_Object_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_Object_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_Object_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_Interface_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_Interface_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_Interface_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_Interface_Method(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_Interface_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_Interface_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_Interface_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_Interface_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_String_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_String_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_String_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_String_Method(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_String_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_String_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_String_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_String_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int8_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int8_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int8_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int8_Method(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int8_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int8_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int8_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int8_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int16_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int16_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int16_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int16_Method(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int16_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int16_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int16_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int16_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int32_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int32_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int32_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int32_Method(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int32_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int32_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int32_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int32_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int64_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int64_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int64_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int64_Method(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int64_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int64_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int64_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int64_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
{$ENDIF}
  Result := TDictionary<TKey, TValue>.Create(0, nil, nil, ownerships);
end;

class function TCollections.CreateDictionary<TKey, TValue>(capacity: Integer;
  ownerships: TDictionaryOwnerships): IOrderedDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds + tkMethods) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_Object_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_Object_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_Object_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_Object_Method(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_Object_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_Object_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_Object_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_Object_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_Interface_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_Interface_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_Interface_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_Interface_Method(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_Interface_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_Interface_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_Interface_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_Interface_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_String_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_String_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_String_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_String_Method(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_String_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_String_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_String_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_String_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int8_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int8_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int8_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int8_Method(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int8_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int8_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int8_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int8_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int16_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int16_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int16_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int16_Method(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int16_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int16_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int16_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int16_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int32_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int32_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int32_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int32_Method(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int32_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int32_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int32_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int32_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int64_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int64_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int64_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int64_Method(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int64_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int64_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int64_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int64_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
{$ENDIF}
  Result := TDictionary<TKey, TValue>.Create(capacity, nil, nil, ownerships);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IOrderedDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds + tkMethods) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_Object_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_Object_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_Object_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_Object_Method(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_Object_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_Object_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_Object_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_Object_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_Interface_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_Interface_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_Interface_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_Interface_Method(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_Interface_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_Interface_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_Interface_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_Interface_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_String_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_String_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_String_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_String_Method(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_String_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_String_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_String_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_String_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int8_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int8_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int8_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int8_Method(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int8_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int8_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int8_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int8_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int16_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int16_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int16_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int16_Method(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int16_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int16_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int16_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int16_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int32_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int32_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int32_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int32_Method(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int32_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int32_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int32_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int32_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int64_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int64_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int64_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int64_Method(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int64_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int64_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int64_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int64_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
{$ENDIF}
  Result := TDictionary<TKey, TValue>.Create(0, keyComparer, nil, ownerships);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships): IOrderedDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds + tkMethods) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_Object_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_Object_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_Object_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_Object_Method(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_Object_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_Object_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_Object_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_Object_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_Interface_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_Interface_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_Interface_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_Interface_Method(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_Interface_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_Interface_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_Interface_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_Interface_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_String_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_String_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_String_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_String_Method(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_String_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_String_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_String_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_String_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int8_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int8_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int8_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int8_Method(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int8_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int8_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int8_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int8_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int16_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int16_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int16_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int16_Method(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int16_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int16_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int16_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int16_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int32_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int32_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int32_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int32_Method(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int32_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int32_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int32_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int32_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int64_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int64_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int64_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int64_Method(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int64_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int64_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int64_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int64_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
{$ENDIF}
  Result := TDictionary<TKey, TValue>.Create(0, keyComparer, valueComparer, ownerships);
end;

class function TCollections.CreateDictionary<TKey, TValue>(capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IOrderedDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds + tkMethods) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_Object_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_Object_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_Object_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_Object_Method(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_Object_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_Object_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_Object_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_Object_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_Interface_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_Interface_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_Interface_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_Interface_Method(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_Interface_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_Interface_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_Interface_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_Interface_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_String_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_String_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_String_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_String_Method(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_String_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_String_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_String_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_String_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int8_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int8_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int8_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int8_Method(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int8_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int8_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int8_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int8_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int16_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int16_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int16_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int16_Method(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int16_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int16_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int16_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int16_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int32_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int32_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int32_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int32_Method(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int32_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int32_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int32_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int32_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int64_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int64_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int64_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int64_Method(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int64_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int64_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int64_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int64_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
{$ENDIF}
  Result := TDictionary<TKey, TValue>.Create(capacity, keyComparer, nil, ownerships);
end;

class function TCollections.CreateDictionary<TKey, TValue>(capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships): IOrderedDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds + tkMethods) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_Object_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_Object_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_Object_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_Object_Method(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_Object_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_Object_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_Object_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_Object_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_Interface_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_Interface_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_Interface_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_Interface_Method(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_Interface_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_Interface_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_Interface_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_Interface_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateDictionary_String_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateDictionary_String_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateDictionary_String_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkMethod: CreateDictionary_String_Method(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateDictionary_String_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateDictionary_String_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateDictionary_String_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateDictionary_String_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int8_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int8_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int8_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int8_Method(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int8_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int8_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int8_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int8_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int16_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int16_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int16_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int16_Method(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int16_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int16_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int16_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int16_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int32_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int32_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int32_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int32_Method(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int32_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int32_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int32_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int32_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateDictionary_Int64_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateDictionary_Int64_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateDictionary_Int64_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkMethod: CreateDictionary_Int64_Method(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateDictionary_Int64_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Int64_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Int64_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Int64_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
{$ENDIF}
  Result := TDictionary<TKey, TValue>.Create(capacity, keyComparer, valueComparer, ownerships);
end;

class function TCollections.CreateMultiMap<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateListMultiMap_Object_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateListMultiMap_Object_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateListMultiMap_Object_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateListMultiMap_Object_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateListMultiMap_Object_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateListMultiMap_Object_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateListMultiMap_Object_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateListMultiMap_Interface_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateListMultiMap_Interface_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateListMultiMap_Interface_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateListMultiMap_Interface_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateListMultiMap_Interface_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateListMultiMap_Interface_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateListMultiMap_Interface_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateListMultiMap_String_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateListMultiMap_String_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateListMultiMap_String_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateListMultiMap_String_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateListMultiMap_String_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateListMultiMap_String_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateListMultiMap_String_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateListMultiMap_Int8_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateListMultiMap_Int8_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateListMultiMap_Int8_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateListMultiMap_Int8_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_Int8_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_Int8_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_Int8_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateListMultiMap_Int16_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateListMultiMap_Int16_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateListMultiMap_Int16_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateListMultiMap_Int16_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_Int16_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_Int16_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_Int16_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateListMultiMap_Int32_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateListMultiMap_Int32_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateListMultiMap_Int32_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateListMultiMap_Int32_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_Int32_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_Int32_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_Int32_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateListMultiMap_Int64_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateListMultiMap_Int64_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateListMultiMap_Int64_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateListMultiMap_Int64_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_Int64_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_Int64_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_Int64_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        end;
    end;
  end
  else
  case GetTypeKind(TValue) of
    tkClass: IMultiMap<TKey,TObject>(Result) := TFoldedMultiMap<TKey,TObject>.Create(
      TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>), nil, nil,
      Helper<TKey,TObject>.CreateListCollection, ownerships);
    tkInterface: IMultiMap<TKey,IInterface>(Result) := TFoldedMultiMap<TKey,IInterface>.Create(
      TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>), nil, nil,
      Helper<TKey,IInterface>.CreateListCollection, ownerships);
  else{$ELSE}begin{$ENDIF}
    Result := TMultiMap<TKey,TValue>.Create(nil, nil,
      Helper<TKey,TValue>.CreateListCollection, ownerships);
  end;
end;

class function TCollections.CreateMultiMap<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateListMultiMap_Object_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateListMultiMap_Object_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateListMultiMap_Object_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateListMultiMap_Object_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateListMultiMap_Object_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateListMultiMap_Object_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateListMultiMap_Object_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateListMultiMap_Interface_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateListMultiMap_Interface_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateListMultiMap_Interface_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateListMultiMap_Interface_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateListMultiMap_Interface_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateListMultiMap_Interface_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateListMultiMap_Interface_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateListMultiMap_String_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateListMultiMap_String_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateListMultiMap_String_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateListMultiMap_String_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateListMultiMap_String_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateListMultiMap_String_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateListMultiMap_String_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateListMultiMap_Int8_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateListMultiMap_Int8_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateListMultiMap_Int8_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateListMultiMap_Int8_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_Int8_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_Int8_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_Int8_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateListMultiMap_Int16_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateListMultiMap_Int16_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateListMultiMap_Int16_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateListMultiMap_Int16_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_Int16_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_Int16_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_Int16_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateListMultiMap_Int32_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateListMultiMap_Int32_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateListMultiMap_Int32_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateListMultiMap_Int32_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_Int32_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_Int32_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_Int32_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateListMultiMap_Int64_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateListMultiMap_Int64_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateListMultiMap_Int64_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateListMultiMap_Int64_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_Int64_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_Int64_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_Int64_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
  case GetTypeKind(TValue) of
    tkClass: IMultiMap<TKey,TObject>(Result) := TFoldedMultiMap<TKey,TObject>.Create(
      TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>), keyComparer, nil,
      Helper<TKey,TObject>.CreateListCollection, ownerships);
    tkInterface: IMultiMap<TKey,IInterface>(Result) := TFoldedMultiMap<TKey,IInterface>.Create(
      TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>), keyComparer, nil,
      Helper<TKey,IInterface>.CreateListCollection, ownerships);
  else{$ELSE}begin{$ENDIF}
    Result := TMultiMap<TKey,TValue>.Create(keyComparer, nil,
      Helper<TKey,TValue>.CreateListCollection, ownerships);
  end;
end;

class function TCollections.CreateHashMultiMap<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TMultiMap<TKey, TValue>.Create(nil, nil,
    Helper<TKey, TValue>.CreateHashSetCollection, ownerships);
end;

class function TCollections.CreateHashMultiMap<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TMultiMap<TKey, TValue>.Create(keyComparer, nil,
    Helper<TKey, TValue>.CreateHashSetCollection, ownerships);
end;

class function TCollections.CreateHashMultiMap<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TMultiMap<TKey, TValue>.Create(keyComparer, valueComparer,
    Helper<TKey, TValue>.CreateHashSetCollection, ownerships);
end;

class function TCollections.CreateTreeMultiMap<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TMultiMap<TKey, TValue>.Create(nil, nil,
    Helper<TKey, TValue>.CreateTreeSetCollection, ownerships);
end;

class function TCollections.CreateTreeMultiMap<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TMultiMap<TKey, TValue>.Create(keyComparer, nil,
    Helper<TKey, TValue>.CreateTreeSetCollection, ownerships);
end;

class function TCollections.CreateTreeMultiMap<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IComparer<TValue>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TMultiMap<TKey, TValue>.Create(keyComparer, valueComparer,
    Helper<TKey, TValue>.CreateTreeSetCollection, ownerships);
end;

class function TCollections.CreateMultiSet<T>: IMultiSet<T>;
begin
  Result := THashMultiSet<T>.Create(nil);
end;

class function TCollections.CreateMultiSet<T>(
  const comparer: IEqualityComparer<T>): IMultiSet<T>;
begin
  Result := THashMultiSet<T>.Create(comparer);
end;

class function TCollections.CreateMultiSet<T>(const values: array of T): IMultiSet<T>;
begin
  Result := THashMultiSet<T>.Create(nil);
  Result.AddRange(values);
end;

class function TCollections.CreateMultiSet<T>(const values: IEnumerable<T>): IMultiSet<T>;
begin
  Result := THashMultiSet<T>.Create(nil);
  Result.AddRange(values);
end;

class function TCollections.CreateMultiSet<T>(const values: array of T;
  const comparer: IEqualityComparer<T>): IMultiSet<T>;
begin
  Result := THashMultiSet<T>.Create(comparer);
  Result.AddRange(values);
end;

class function TCollections.CreateMultiSet<T>(const values: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): IMultiSet<T>;
begin
  Result := THashMultiSet<T>.Create(comparer);
  Result.AddRange(values);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IBidiDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_Object_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_Object_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_Object_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_Object_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_Object_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_Object_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_Object_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_Interface_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_Interface_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_Interface_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_Interface_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_Interface_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_Interface_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_Interface_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_String_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_String_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_String_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_String_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_String_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_String_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_String_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int8_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int8_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int8_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int8_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int8_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int8_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int8_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int16_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int16_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int16_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int16_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int16_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int16_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int16_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int32_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int32_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int32_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int32_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int32_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int32_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int32_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int64_Object(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int64_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int64_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int64_Int8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int64_Int16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int64_Int32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int64_Int64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
{$ENDIF}
  Result := TBidiDictionary<TKey, TValue>.Create(0, nil, nil, ownerships);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>(capacity: Integer;
  ownerships: TDictionaryOwnerships): IBidiDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_Object_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_Object_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_Object_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_Object_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_Object_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_Object_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_Object_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_Interface_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_Interface_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_Interface_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_Interface_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_Interface_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_Interface_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_Interface_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_String_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_String_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_String_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_String_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_String_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_String_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_String_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int8_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int8_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int8_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int8_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int8_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int8_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int8_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int16_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int16_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int16_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int16_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int16_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int16_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int16_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int32_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int32_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int32_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int32_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int32_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int32_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int32_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int64_Object(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int64_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int64_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int64_Int8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int64_Int16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int64_Int32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int64_Int64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
{$ENDIF}
  Result := TBidiDictionary<TKey, TValue>.Create(capacity, nil, nil, ownerships);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IBidiDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_Object_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_Object_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_Object_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_Object_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_Object_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_Object_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_Object_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_Interface_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_Interface_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_Interface_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_Interface_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_Interface_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_Interface_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_Interface_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_String_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_String_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_String_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_String_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_String_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_String_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_String_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int8_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int8_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int8_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int8_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int8_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int8_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int8_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int16_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int16_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int16_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int16_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int16_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int16_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int16_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int32_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int32_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int32_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int32_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int32_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int32_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int32_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int64_Object(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int64_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int64_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int64_Int8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int64_Int16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int64_Int32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int64_Int64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
{$ENDIF}
  Result := TBidiDictionary<TKey, TValue>.Create(0, keyComparer, nil, ownerships);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships): IBidiDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_Object_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_Object_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_Object_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_Object_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_Object_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_Object_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_Object_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_Interface_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_Interface_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_Interface_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_Interface_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_Interface_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_Interface_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_Interface_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_String_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_String_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_String_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_String_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_String_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_String_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_String_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int8_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int8_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int8_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int8_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int8_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int8_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int8_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int16_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int16_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int16_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int16_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int16_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int16_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int16_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int32_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int32_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int32_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int32_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int32_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int32_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int32_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int64_Object(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int64_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int64_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int64_Int8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int64_Int16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int64_Int32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int64_Int64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
{$ENDIF}
  Result := TBidiDictionary<TKey, TValue>.Create(0, keyComparer, valueComparer, ownerships);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>(capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IBidiDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_Object_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_Object_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_Object_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_Object_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_Object_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_Object_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_Object_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_Interface_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_Interface_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_Interface_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_Interface_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_Interface_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_Interface_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_Interface_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_String_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_String_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_String_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_String_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_String_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_String_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_String_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int8_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int8_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int8_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int8_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int8_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int8_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int8_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int16_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int16_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int16_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int16_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int16_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int16_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int16_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int32_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int32_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int32_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int32_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int32_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int32_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int32_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int64_Object(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int64_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int64_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int64_Int8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int64_Int16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int64_Int32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int64_Int64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
{$ENDIF}
  Result := TBidiDictionary<TKey, TValue>.Create(capacity, keyComparer, nil, ownerships);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>(capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships): IBidiDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_Object_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_Object_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_Object_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_Object_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_Object_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_Object_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_Object_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_Interface_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_Interface_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_Interface_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_Interface_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_Interface_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_Interface_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_Interface_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateBidiDictionary_String_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateBidiDictionary_String_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateBidiDictionary_String_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateBidiDictionary_String_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateBidiDictionary_String_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateBidiDictionary_String_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateBidiDictionary_String_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int8_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int8_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int8_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int8_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int8_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int8_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int8_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int16_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int16_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int16_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int16_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int16_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int16_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int16_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int32_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int32_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int32_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int32_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int32_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int32_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int32_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateBidiDictionary_Int64_Object(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateBidiDictionary_Int64_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateBidiDictionary_Int64_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Int64_Int8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Int64_Int16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Int64_Int32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Int64_Int64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
{$ENDIF}
  Result := TBidiDictionary<TKey, TValue>.Create(capacity, keyComparer, valueComparer, ownerships);
end;

class function TCollections.CreateStack<T>: IStack<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateStack_Object(0, nil, False, Result, TypeInfo(T));
    tkInterface: CreateStack_Interface(0, nil, Result, TypeInfo(T));
  else{$ELSE}begin{$ENDIF}
    Result := TStack<T>.Create;
  end;
end;

class function TCollections.CreateStack<T>(ownsObjects: Boolean): IStack<T>;
begin
  CreateStack_Object(0, nil, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateStack<T>(const values: array of T): IStack<T>;
begin
  Result := TStack<T>.Create(values);
end;

class function TCollections.CreateStack<T>(
  const values: IEnumerable<T>): IStack<T>;
begin
  Result := TStack<T>.Create(values);
end;

class function TCollections.CreateBoundedStack<T>(size: Integer): IStack<T>;
begin
  Result := TBoundedStack<T>.Create(size);
end;

class function TCollections.CreateBoundedStack<T>(size: Integer; ownsObjects: Boolean): IStack<T>;
begin
  Result := TBoundedStack<T>.Create(size, ownsObjects);
end;

class function TCollections.CreateQueue<T>: IQueue<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateQueue_Object(0, nil, False, Result, TypeInfo(T));
    tkInterface: CreateQueue_Interface(0, nil, Result, TypeInfo(T));
  else{$ELSE}begin{$ENDIF}
    Result := TQueue<T>.Create;
  end;
end;

class function TCollections.CreateQueue<T>(ownsObjects: Boolean): IQueue<T>;
begin
  CreateQueue_Object(0, nil, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateQueue<T>(const values: array of T): IQueue<T>;
begin
  Result := TQueue<T>.Create(values);
end;

class function TCollections.CreateQueue<T>(const values: IEnumerable<T>): IQueue<T>;
begin
  Result := TQueue<T>.Create(values);
end;

class function TCollections.CreateBoundedQueue<T>(size: Integer): IQueue<T>;
begin
  Result := TBoundedQueue<T>.Create(size);
end;

class function TCollections.CreateBoundedQueue<T>(size: Integer;
  ownsObjects: Boolean): IQueue<T>;
begin
  Result := TBoundedQueue<T>.Create(size, ownsObjects);
end;

class function TCollections.CreateEvictingQueue<T>(size: Integer): IQueue<T>;
begin
  Result := TEvictingQueue<T>.Create(size);
end;

class function TCollections.CreateEvictingQueue<T>(size: Integer;
  ownsObjects: Boolean): IQueue<T>;
begin
  Result := TEvictingQueue<T>.Create(size, ownsObjects);
end;

class function TCollections.CreateDeque<T>: IDeque<T>;
begin
  Result := TDeque<T>.Create;
end;

class function TCollections.CreateDeque<T>(ownsObjects: Boolean): IDeque<T>;
begin
  Result := TDeque<T>.Create(0, ownsObjects);
end;

class function TCollections.CreateDeque<T>(const values: array of T): IDeque<T>;
begin
  Result := TDeque<T>.Create(values);
end;

class function TCollections.CreateDeque<T>(const values: IEnumerable<T>): IDeque<T>;
begin
  Result := TDeque<T>.Create(values);
end;

class function TCollections.CreateBoundedDeque<T>(size: Integer): IDeque<T>;
begin
  Result := TBoundedDeque<T>.Create(size);
end;

class function TCollections.CreateBoundedDeque<T>(size: Integer;
  ownsObjects: Boolean): IDeque<T>;
begin
  Result := TBoundedDeque<T>.Create(size, ownsObjects);
end;

class function TCollections.CreateEvictingDeque<T>(size: Integer): IDeque<T>;
begin
  Result := TEvictingDeque<T>.Create(size);
end;

class function TCollections.CreateEvictingDeque<T>(size: Integer;
  ownsObjects: Boolean): IDeque<T>;
begin
  Result := TEvictingDeque<T>.Create(size, ownsObjects);
end;

class function TCollections.CreateSet<T>: IOrderedSet<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateHashSet_Object(0, nil, Result, TypeInfo(T));
    tkInterface: CreateHashSet_Interface(0, nil, Result, TypeInfo(T));
    tkUString: CreateHashSet_String(0, nil, Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateHashSet_Int8(0, nil, Result, TypeInfo(T));
        2: CreateHashSet_Int16(0, nil, Result, TypeInfo(T));
        4: CreateHashSet_Int32(0, nil, Result, TypeInfo(T));
        8: CreateHashSet_Int64(0, nil, Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := THashSet<T>.Create(0, nil);
  end;
end;

class function TCollections.CreateSet<T>(capacity: Integer): IOrderedSet<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateHashSet_Object(capacity, nil, Result, TypeInfo(T));
    tkInterface: CreateHashSet_Interface(capacity, nil, Result, TypeInfo(T));
    tkUString: CreateHashSet_String(capacity, nil, Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateHashSet_Int8(capacity, nil, Result, TypeInfo(T));
        2: CreateHashSet_Int16(capacity, nil, Result, TypeInfo(T));
        4: CreateHashSet_Int32(capacity, nil, Result, TypeInfo(T));
        8: CreateHashSet_Int64(capacity, nil, Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := THashSet<T>.Create(capacity, nil);
  end;
end;

class function TCollections.CreateSet<T>(
  const comparer: IEqualityComparer<T>): IOrderedSet<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateHashSet_Object(0, Pointer(comparer), Result, TypeInfo(T));
    tkInterface: CreateHashSet_Interface(0, Pointer(comparer), Result, TypeInfo(T));
    tkUString: CreateHashSet_String(0, Pointer(comparer), Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateHashSet_Int8(0, Pointer(comparer), Result, TypeInfo(T));
        2: CreateHashSet_Int16(0, Pointer(comparer), Result, TypeInfo(T));
        4: CreateHashSet_Int32(0, Pointer(comparer), Result, TypeInfo(T));
        8: CreateHashSet_Int64(0, Pointer(comparer), Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := THashSet<T>.Create(0, comparer);
  end;
end;

class function TCollections.CreateSet<T>(capacity: Integer;
  const comparer: IEqualityComparer<T>): IOrderedSet<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateHashSet_Object(capacity, Pointer(comparer), Result, TypeInfo(T));
    tkInterface: CreateHashSet_Interface(capacity, Pointer(comparer), Result, TypeInfo(T));
    tkUString: CreateHashSet_String(0, Pointer(comparer), Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateHashSet_Int8(capacity, Pointer(comparer), Result, TypeInfo(T));
        2: CreateHashSet_Int16(capacity, Pointer(comparer), Result, TypeInfo(T));
        4: CreateHashSet_Int32(capacity, Pointer(comparer), Result, TypeInfo(T));
        8: CreateHashSet_Int64(capacity, Pointer(comparer), Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := THashSet<T>.Create(capacity, comparer);
  end;
end;

class function TCollections.CreateSet<T>(const values: array of T): IOrderedSet<T>;
begin
  Result := CreateSet<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateSet<T>(const values: IEnumerable<T>): IOrderedSet<T>;
begin
  Result := CreateSet<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateSet<T>(const values: array of T;
  const comparer: IEqualityComparer<T>): IOrderedSet<T>;
begin
  Result := CreateSet<T>(comparer);
  Result.AddRange(values);
end;

class function TCollections.CreateSet<T>(const values: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): IOrderedSet<T>;
begin
  Result := CreateSet<T>(comparer);
  Result.AddRange(values);
end;

class function TCollections.CreateSortedList<T>: IList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateSortedList_Object(nil, False, Result, TypeInfo(T));
    tkInterface: CreateSortedList_Interface(nil, Result, TypeInfo(T));
    tkUString: CreateSortedList_String(nil, Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateSortedList_Int8(nil, Result, TypeInfo(T));
        2: CreateSortedList_Int16(nil, Result, TypeInfo(T));
        4: CreateSortedList_Int32(nil, Result, TypeInfo(T));
        8: CreateSortedList_Int64(nil, Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TSortedList<T>.Create;
  end;
end;

class function TCollections.CreateSortedList<T>(
  const comparer: IComparer<T>): IList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateSortedList_Object(Pointer(comparer), False, Result, TypeInfo(T));
    tkInterface: CreateSortedList_Interface(Pointer(comparer), Result, TypeInfo(T));
    tkUString: CreateSortedList_String(Pointer(comparer), Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateSortedList_Int8(Pointer(comparer), Result, TypeInfo(T));
        2: CreateSortedList_Int16(Pointer(comparer), Result, TypeInfo(T));
        4: CreateSortedList_Int32(Pointer(comparer), Result, TypeInfo(T));
        8: CreateSortedList_Int64(Pointer(comparer), Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TSortedList<T>.Create(comparer);
  end;
end;

class function TCollections.CreateSortedList<T>(
  const comparer: TComparison<T>): IList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateSortedList_Object(PPointer(@comparer)^, False, Result, TypeInfo(T));
    tkInterface: CreateSortedList_Interface(PPointer(@comparer)^, Result, TypeInfo(T));
    tkUString: CreateSortedList_String(PPointer(@comparer)^, Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateSortedList_Int8(PPointer(@comparer)^, Result, TypeInfo(T));
        2: CreateSortedList_Int16(PPointer(@comparer)^, Result, TypeInfo(T));
        4: CreateSortedList_Int32(PPointer(@comparer)^, Result, TypeInfo(T));
        8: CreateSortedList_Int64(PPointer(@comparer)^, Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TSortedList<T>.Create(IComparer<T>(PPointer(@comparer)^));
  end;
end;

class function TCollections.CreateSortedList<T>(
  const values: array of T): IList<T>;
begin
  Result := CreateSortedList<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateSortedList<T>(
  const values: IEnumerable<T>): IList<T>;
begin
  Result := CreateSortedList<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateSortedList<T>(
  ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(nil, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedList<T>(const comparer: IComparer<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(Pointer(comparer), ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedList<T>(const comparer: TComparison<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(PPointer(@comparer)^, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedObjectList<T>(
  ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(nil, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedObjectList<T>(
  const comparer: IComparer<T>; ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(Pointer(comparer), ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedObjectList<T>(
  const comparer: TComparison<T>; ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(PPointer(@comparer)^, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedObjectList<T>(const values: array of T;
  ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(nil, ownsObjects, Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateSortedObjectList<T>(
  const values: IEnumerable<T>; ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(nil, ownsObjects, Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateSortedInterfaceList<T>: IList<T>;
begin
  CreateSortedList_Interface(nil, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedInterfaceList<T>(
  const comparer: IComparer<T>): IList<T>;
begin
  CreateSortedList_Interface(Pointer(comparer), Result, TypeInfo(T));
end;

class function TCollections.CreateSortedInterfaceList<T>(
  const comparer: TComparison<T>): IList<T>;
begin
  CreateSortedList_Interface(PPointer(@comparer)^, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedInterfaceList<T>(
  const values: array of T): IList<T>;
begin
  CreateSortedList_Interface(nil, Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateSortedInterfaceList<T>(
  const values: IEnumerable<T>): IList<T>;
begin
  CreateSortedList_Interface(nil, Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateSortedSet<T>: ISet<T>;
begin
  Result := TSortedSet<T>.Create(nil);
end;

class function TCollections.CreateSortedSet<T>(
  const comparer: IComparer<T>): ISet<T>;
begin
  Result := TSortedSet<T>.Create(comparer);
end;

class function TCollections.CreateSortedSet<T>(const values: array of T): ISet<T>;
begin
  Result := TSortedSet<T>.Create(nil);
  Result.AddRange(values);
end;

class function TCollections.CreateSortedSet<T>(const values: IEnumerable<T>): ISet<T>;
begin
  Result := TSortedSet<T>.Create(nil);
  Result.AddRange(values);
end;

class function TCollections.CreateSortedSet<T>(const values: array of T;
  const comparer: IComparer<T>): ISet<T>;
begin
  Result := TSortedSet<T>.Create(comparer);
  Result.AddRange(values);
end;

class function TCollections.CreateSortedSet<T>(const values: IEnumerable<T>;
  const comparer: IComparer<T>): ISet<T>;
begin
  Result := TSortedSet<T>.Create(comparer);
  Result.AddRange(values);
end;

class function TCollections.CreateSortedDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
  Result := TSortedDictionary<TKey, TValue>.Create(nil, nil, ownerships);
end;

class function TCollections.CreateSortedDictionary<TKey, TValue>(
  const keyComparer: IComparer<TKey>;
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
  Result := TSortedDictionary<TKey, TValue>.Create(keyComparer, nil, ownerships);
end;

class function TCollections.CreateSortedDictionary<TKey, TValue>(
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
  Result := TSortedDictionary<TKey, TValue>.Create(nil, valueComparer, ownerships);
end;

class function TCollections.CreateSortedDictionary<TKey, TValue>(
  const keyComparer: IComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
  Result := TSortedDictionary<TKey, TValue>.Create(keyComparer, valueComparer, ownerships);
end;

class function TCollections.CreateSortedMultiSet<T>: IMultiSet<T>;
begin
  Result := TTreeMultiSet<T>.Create(nil);
end;

class function TCollections.CreateSortedMultiSet<T>(
  const comparer: IComparer<T>): IMultiSet<T>;
begin
  Result := TTreeMultiSet<T>.Create(comparer);
end;

class function TCollections.CreateSortedMultiMap<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateSortedListMultiMap_Object_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateSortedListMultiMap_Object_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateSortedListMultiMap_Object_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateSortedListMultiMap_Object_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateSortedListMultiMap_Object_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateSortedListMultiMap_Object_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateSortedListMultiMap_Object_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateSortedListMultiMap_Interface_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateSortedListMultiMap_Interface_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateSortedListMultiMap_Interface_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateSortedListMultiMap_Interface_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateSortedListMultiMap_Interface_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateSortedListMultiMap_Interface_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateSortedListMultiMap_Interface_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateSortedListMultiMap_String_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateSortedListMultiMap_String_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateSortedListMultiMap_String_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateSortedListMultiMap_String_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateSortedListMultiMap_String_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateSortedListMultiMap_String_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateSortedListMultiMap_String_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateSortedListMultiMap_Int8_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateSortedListMultiMap_Int8_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateSortedListMultiMap_Int8_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateSortedListMultiMap_Int8_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateSortedListMultiMap_Int8_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateSortedListMultiMap_Int8_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateSortedListMultiMap_Int8_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateSortedListMultiMap_Int16_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateSortedListMultiMap_Int16_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateSortedListMultiMap_Int16_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateSortedListMultiMap_Int16_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateSortedListMultiMap_Int16_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateSortedListMultiMap_Int16_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateSortedListMultiMap_Int16_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateSortedListMultiMap_Int32_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateSortedListMultiMap_Int32_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateSortedListMultiMap_Int32_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateSortedListMultiMap_Int32_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateSortedListMultiMap_Int32_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateSortedListMultiMap_Int32_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateSortedListMultiMap_Int32_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateSortedListMultiMap_Int64_Object(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateSortedListMultiMap_Int64_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateSortedListMultiMap_Int64_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateSortedListMultiMap_Int64_Int8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateSortedListMultiMap_Int64_Int16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateSortedListMultiMap_Int64_Int32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateSortedListMultiMap_Int64_Int64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        end;
    end;
  end
  else
  case GetTypeKind(TValue) of
    tkClass: IMultiMap<TKey,TObject>(Result) := TFoldedSortedMultiMap<TKey,TObject>.Create(
      TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>), nil, nil,
      Helper<TKey,TObject>.CreateListCollection, ownerships);
    tkInterface: IMultiMap<TKey,IInterface>(Result) := TFoldedSortedMultiMap<TKey,IInterface>.Create(
      TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>), nil, nil,
      Helper<TKey,IInterface>.CreateListCollection, ownerships);
  else{$ELSE}begin{$ENDIF}
    Result := TSortedMultiMap<TKey, TValue>.Create(nil, nil,
      Helper<TKey,TValue>.CreateListCollection, ownerships);
  end;
end;

class function TCollections.CreateSortedMultiMap<TKey, TValue>(
  const keyComparer: IComparer<TKey>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  case GetTypeKind(TKey) of
    tkClass:
      case GetTypeKind(TValue) of
        tkClass: CreateSortedListMultiMap_Object_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateSortedListMultiMap_Object_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateSortedListMultiMap_Object_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateSortedListMultiMap_Object_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateSortedListMultiMap_Object_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateSortedListMultiMap_Object_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateSortedListMultiMap_Object_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkInterface:
      case GetTypeKind(TValue) of
        tkClass: CreateSortedListMultiMap_Interface_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateSortedListMultiMap_Interface_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateSortedListMultiMap_Interface_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateSortedListMultiMap_Interface_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateSortedListMultiMap_Interface_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateSortedListMultiMap_Interface_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateSortedListMultiMap_Interface_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
    tkUString:
      case GetTypeKind(TValue) of
        tkClass: CreateSortedListMultiMap_String_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkInterface: CreateSortedListMultiMap_String_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        tkUString: CreateSortedListMultiMap_String_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
      else
        case SizeOf(TValue) of
          1: CreateSortedListMultiMap_String_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          2: CreateSortedListMultiMap_String_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          4: CreateSortedListMultiMap_String_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          8: CreateSortedListMultiMap_String_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        end;
      end;
  else
    case SizeOf(TKey) of
      1:
        case GetTypeKind(TValue) of
          tkClass: CreateSortedListMultiMap_Int8_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateSortedListMultiMap_Int8_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateSortedListMultiMap_Int8_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateSortedListMultiMap_Int8_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateSortedListMultiMap_Int8_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateSortedListMultiMap_Int8_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateSortedListMultiMap_Int8_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      2:
        case GetTypeKind(TValue) of
          tkClass: CreateSortedListMultiMap_Int16_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateSortedListMultiMap_Int16_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateSortedListMultiMap_Int16_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateSortedListMultiMap_Int16_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateSortedListMultiMap_Int16_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateSortedListMultiMap_Int16_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateSortedListMultiMap_Int16_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      4:
        case GetTypeKind(TValue) of
          tkClass: CreateSortedListMultiMap_Int32_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateSortedListMultiMap_Int32_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateSortedListMultiMap_Int32_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateSortedListMultiMap_Int32_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateSortedListMultiMap_Int32_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateSortedListMultiMap_Int32_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateSortedListMultiMap_Int32_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
      8:
        case GetTypeKind(TValue) of
          tkClass: CreateSortedListMultiMap_Int64_Object(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkInterface: CreateSortedListMultiMap_Int64_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          tkUString: CreateSortedListMultiMap_Int64_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
        else
          case SizeOf(TValue) of
            1: CreateSortedListMultiMap_Int64_Int8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateSortedListMultiMap_Int64_Int16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateSortedListMultiMap_Int64_Int32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateSortedListMultiMap_Int64_Int64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
        end;
    end;
  end
  else
  case GetTypeKind(TValue) of
    tkClass: IMultiMap<TKey,TObject>(Result) := TFoldedSortedMultiMap<TKey,TObject>.Create(
      TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>), nil, nil,
      Helper<TKey,TObject>.CreateListCollection, ownerships);
    tkInterface: IMultiMap<TKey,IInterface>(Result) := TFoldedSortedMultiMap<TKey,IInterface>.Create(
      TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>), nil, nil,
      Helper<TKey,IInterface>.CreateListCollection, ownerships);
  else{$ELSE}begin{$ENDIF}
    Result := TSortedMultiMap<TKey, TValue>.Create(keyComparer, nil,
      Helper<TKey, TValue>.CreateListCollection, ownerships);
  end;
end;

class function TCollections.CreateSortedHashMultiMap<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TSortedMultiMap<TKey, TValue>.Create(nil, nil,
    Helper<TKey, TValue>.CreateHashSetCollection, ownerships);
end;

class function TCollections.CreateSortedHashMultiMap<TKey, TValue>(
  const keyComparer: IComparer<TKey>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TSortedMultiMap<TKey, TValue>.Create(keyComparer, nil,
    Helper<TKey, TValue>.CreateHashSetCollection, ownerships);
end;

class function TCollections.CreateSortedHashMultiMap<TKey, TValue>(
  const keyComparer: IComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TSortedMultiMap<TKey, TValue>.Create(keyComparer, valueComparer,
    Helper<TKey, TValue>.CreateHashSetCollection, ownerships);
end;

class function TCollections.CreateSortedTreeMultiMap<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TSortedMultiMap<TKey, TValue>.Create(nil, nil,
    Helper<TKey, TValue>.CreateTreeSetCollection, ownerships);
end;

class function TCollections.CreateSortedTreeMultiMap<TKey, TValue>(
  const keyComparer: IComparer<TKey>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TSortedMultiMap<TKey, TValue>.Create(keyComparer, nil,
    Helper<TKey, TValue>.CreateTreeSetCollection, ownerships);
end;

class function TCollections.CreateSortedTreeMultiMap<TKey, TValue>(
  const keyComparer: IComparer<TKey>;
  const valueComparer: IComparer<TValue>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TSortedMultiMap<TKey, TValue>.Create(keyComparer, valueComparer,
    Helper<TKey, TValue>.CreateTreeSetCollection, ownerships);
end;

class function TCollections.CreateSortedMultiSet<T>(
  const values: IEnumerable<T>): IMultiSet<T>;
begin
  Result := TTreeMultiSet<T>.Create(nil);
  Result.AddRange(values);
end;

class function TCollections.CreateSortedMultiSet<T>(
  const values: array of T): IMultiSet<T>;
begin
  Result := TTreeMultiSet<T>.Create(nil);
  Result.AddRange(values);
end;

class function TCollections.CreateSortedMultiSet<T>(const values: array of T;
  const comparer: IComparer<T>): IMultiSet<T>;
begin
  Result := TTreeMultiSet<T>.Create(comparer);
  Result.AddRange(values);
end;

class function TCollections.CreateSortedMultiSet<T>(
  const values: IEnumerable<T>; const comparer: IComparer<T>): IMultiSet<T>;
begin
  Result := TTreeMultiSet<T>.Create(comparer);
  Result.AddRange(values);
end;


{$ENDREGION}


{$REGION 'TEnumerable'}

class procedure TEnumerable.CreateEmpty_Int8(var result; elementType: Pointer);
begin
  with TEnumerableExtension.Create(TEnumerableExtension<Int8>, nil, TExtensionKind.Empty) do
    IInterface(result) := IInterface(@IMT);
end;

class procedure TEnumerable.CreateEmpty_Int16(var result; elementType: Pointer);
begin
  with TEnumerableExtension.Create(TEnumerableExtension<Int16>, nil, TExtensionKind.Empty) do
    IInterface(result) := IInterface(@IMT);
end;

class procedure TEnumerable.CreateEmpty_Int32(var result; elementType: Pointer);
begin
  with TEnumerableExtension.Create(TEnumerableExtension<Int32>, nil, TExtensionKind.Empty) do
    IInterface(result) := IInterface(@IMT);
end;

class procedure TEnumerable.CreateEmpty_Int64(var result; elementType: Pointer);
begin
  with TEnumerableExtension.Create(TEnumerableExtension<Int64>, nil, TExtensionKind.Empty) do
    IInterface(result) := IInterface(@IMT);
end;

class procedure TEnumerable.CreateEmpty_Interface(var result; elementType: Pointer);
begin
  with TEnumerableExtension.Create(TEnumerableExtension<IInterface>, nil, TExtensionKind.Empty) do
    IInterface(result) := IInterface(@IMT);
end;

class procedure TEnumerable.CreateEmpty_Method(var result; elementType: Pointer);
begin
  with TEnumerableExtension.Create(TEnumerableExtension<TMethodPointer>, nil, TExtensionKind.Empty) do
    IInterface(result) := IInterface(@IMT);
end;

class procedure TEnumerable.CreateEmpty_Object(var result; elementType: Pointer);
begin
  with TEnumerableExtension.Create(TEnumerableExtension<TObject>, nil, TExtensionKind.Empty) do
    IInterface(result) := IInterface(@IMT);
end;

class procedure TEnumerable.CreateEmpty_String(var result; elementType: Pointer);
begin
  with TEnumerableExtension.Create(TEnumerableExtension<string>, nil, TExtensionKind.Empty) do
    IInterface(result) := IInterface(@IMT);
end;

class function TEnumerable.Aggregate<TSource, TAccumulate>(
  const source: IEnumerable<TSource>; const seed: TAccumulate;
  const func: Func<TAccumulate, TSource, TAccumulate>): TAccumulate;
var
  enumerator: IEnumerator<TSource>;
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(func) then RaiseHelper.ArgumentNil(ExceptionArgument.func);

  Result := seed;
  enumerator := source.GetEnumerator;
  while enumerator.MoveNext do
    Result := func(Result, enumerator.Current);
end;

class function TEnumerable.Aggregate<TSource, TAccumulate, TResult>(
  const source: IEnumerable<TSource>; const seed: TAccumulate;
  const func: Func<TAccumulate, TSource, TAccumulate>;
  const resultSelector: Func<TAccumulate, TResult>): TResult;
var
  enumerator: IEnumerator<TSource>;
  res: TAccumulate;
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(func) then RaiseHelper.ArgumentNil(ExceptionArgument.func);
  if not Assigned(resultSelector) then RaiseHelper.ArgumentNil(ExceptionArgument.resultSelector);

  res := seed;
  enumerator := source.GetEnumerator;
  while enumerator.MoveNext do
    res := func(res, enumerator.Current);
  Result := resultSelector(res);
end;

class function TEnumerable.Chunk<T>(const source: IEnumerable<T>;
  size: Integer): IEnumerable<TArray<T>>;
begin
  Result := TChunkIterator<T>.Create(source, size);
end;

class function TEnumerable.DefaultIfEmpty<T>(
  const source: IEnumerable<T>): IEnumerable<T>;
var
  defaultValue: T;
begin
  defaultValue := Default(T);
  Result := TDefaultIfEmptyIterator<T>.Create(source, defaultValue);
end;

class function TEnumerable.DefaultIfEmpty<T>(const source: IEnumerable<T>;
  const defaultValue: T): IEnumerable<T>;
begin
  Result := TDefaultIfEmptyIterator<T>.Create(source, defaultValue);
end;

class function TEnumerable.Distinct<T>(
  const source: IEnumerable<T>): IEnumerable<T>;
begin
  Result := source.Distinct;
end;

class function TEnumerable.Distinct<T>(const source: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): IEnumerable<T>;
begin
  Result := source.Distinct(comparer);
end;

class function TEnumerable.DistinctBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>): IEnumerable<T>;
begin
  Result := TDistinctByIterator<T, TKey>.Create(source, keySelector, nil);
end;

class function TEnumerable.DistinctBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>;
  const comparer: IEqualityComparer<TKey>): IEnumerable<T>;
begin
  Result := TDistinctByIterator<T, TKey>.Create(source, keySelector, comparer);
end;

class function TEnumerable.Empty<T>: IReadOnlyList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateEmpty_Object(Result, TypeInfo(T));
    tkInterface: CreateEmpty_Interface(Result, TypeInfo(T));
    tkUString: CreateEmpty_String(Result, TypeInfo(T));
    tkMethod: CreateEmpty_Method(Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateEmpty_Int8(Result, TypeInfo(T));
        2: CreateEmpty_Int16(Result, TypeInfo(T));
        4: CreateEmpty_Int32(Result, TypeInfo(T));
        8: CreateEmpty_Int64(Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    with TEnumerableExtension.Create(TEnumerableExtension<T>, nil, TExtensionKind.Empty) do
      IInterface(result) := IInterface(@IMT);
  end;
end;

class function TEnumerable.&Except<T>(const first,
  second: IEnumerable<T>): IEnumerable<T>;
begin
  Result := first.Exclude(second);
end;

class function TEnumerable.&Except<T>(const first, second: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): IEnumerable<T>;
begin
  Result := first.Exclude(second, comparer);
end;

class procedure TEnumerable.InternalFrom_Object_DynArray(source: Pointer;
  var result; elementType: PTypeInfo);
begin
  IReadOnlyList<TObject>(Result) :=
    TFoldedArrayIterator<TObject>.Create(TArray<TObject>(source), elementType);
end;

class procedure TEnumerable.InternalFrom_Object_OpenArray(source: Pointer;
  count: Integer; var result; elementType: PTypeInfo);
begin
  IReadOnlyList<TObject>(Result) :=
    TFoldedArrayIterator<TObject>.Create(source, count, elementType);
end;

class procedure TEnumerable.InternalFrom_String_DynArray(source: Pointer;
  var result; elementType: PTypeInfo);
begin
  IReadOnlyList<string>(Result) :=
    TFoldedArrayIterator<string>.Create(TArray<string>(source), elementType);
end;

class procedure TEnumerable.InternalFrom_String_OpenArray(source: Pointer;
  count: Integer; var result; elementType: PTypeInfo);
begin
  IReadOnlyList<string>(Result) :=
    TFoldedArrayIterator<string>.Create(source, count, elementType);
end;

class procedure TEnumerable.InternalOfType_Object(const source: IEnumerable<TObject>;
  var result; resultType: PTypeInfo);
begin
  IEnumerable<TObject>(Result) := TOfTypeIterator.Create(source, GetTypeData(resultType).ClassType);
end;

class procedure TEnumerable.InternalFrom_Int8_DynArray(source: Pointer;
  var result; elementType: PTypeInfo);
begin
  IReadOnlyList<Int8>(Result) :=
    TFoldedArrayIterator<Int8>.Create(TArray<Int8>(source), elementType);
end;

class procedure TEnumerable.InternalFrom_Int8_OpenArray(source: Pointer;
  count: Integer; var result; elementType: PTypeInfo);
begin
  IReadOnlyList<Int8>(Result) :=
    TFoldedArrayIterator<Int8>.Create(source, count, elementType);
end;

class procedure TEnumerable.InternalFrom_Int16_DynArray(source: Pointer;
  var result; elementType: PTypeInfo);
begin
  IReadOnlyList<Int16>(Result) :=
    TFoldedArrayIterator<Int16>.Create(TArray<Int16>(source), elementType);
end;

class procedure TEnumerable.InternalFrom_Int16_OpenArray(source: Pointer;
  count: Integer; var result; elementType: PTypeInfo);
begin
  IReadOnlyList<Int16>(Result) :=
    TFoldedArrayIterator<Int16>.Create(source, count, elementType);
end;

class procedure TEnumerable.InternalFrom_Int32_DynArray(source: Pointer;
  var result; elementType: PTypeInfo);
begin
  IReadOnlyList<Int32>(Result) :=
    TFoldedArrayIterator<Int32>.Create(TArray<Int32>(source), elementType);
end;

class procedure TEnumerable.InternalFrom_Int32_OpenArray(source: Pointer;
  count: Integer; var result; elementType: PTypeInfo);
begin
  IReadOnlyList<Int32>(Result) :=
    TFoldedArrayIterator<Int32>.Create(source, count, elementType);
end;

class procedure TEnumerable.InternalFrom_Int64_DynArray(source: Pointer;
  var result; elementType: PTypeInfo);
begin
  IReadOnlyList<Int64>(Result) :=
    TFoldedArrayIterator<Int64>.Create(TArray<Int64>(source), elementType);
end;

class procedure TEnumerable.InternalFrom_Int64_OpenArray(source: Pointer;
  count: Integer; var result; elementType: PTypeInfo);
begin
  IReadOnlyList<Int64>(Result) :=
    TFoldedArrayIterator<Int64>.Create(source, count, elementType);
end;

class procedure TEnumerable.InternalFrom_Interface_DynArray(source: Pointer;
  var result; elementType: PTypeInfo);
begin
  IReadOnlyList<IInterface>(Result) :=
    TFoldedArrayIterator<IInterface>.Create(TArray<IInterface>(source), elementType);
end;

class procedure TEnumerable.InternalFrom_Interface_OpenArray(source: Pointer;
  count: Integer; var result; elementType: PTypeInfo);
begin
  IReadOnlyList<IInterface>(Result) :=
    TFoldedArrayIterator<IInterface>.Create(source, count, elementType);
end;

class procedure TEnumerable.InternalFrom_Method_DynArray(source: Pointer;
  var result; elementType: PTypeInfo);
begin
  IReadOnlyList<TMethodPointer>(Result) :=
    TFoldedArrayIterator<TMethodPointer>.Create(TArray<TMethodPointer>(source), elementType);
end;

class procedure TEnumerable.InternalFrom_Method_OpenArray(source: Pointer;
  count: Integer; var result; elementType: PTypeInfo);
begin
  IReadOnlyList<TMethodPointer>(Result) :=
    TFoldedArrayIterator<TMethodPointer>.Create(source, count, elementType);
end;

class function TEnumerable.From<T>(const values: array of T): IReadOnlyList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: InternalFrom_Object_OpenArray(@values, Length(values), Result, TypeInfo(T));
    tkInterface: InternalFrom_Interface_OpenArray(@values, Length(values), Result, TypeInfo(T));
    tkUString: InternalFrom_String_OpenArray(@values, Length(values), Result, TypeInfo(T));
    tkMethod: InternalFrom_Method_OpenArray(@values, Length(values), Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: InternalFrom_Int8_OpenArray(@values, Length(values), Result, TypeInfo(T));
        2: InternalFrom_Int16_OpenArray(@values, Length(values), Result, TypeInfo(T));
        4: InternalFrom_Int32_OpenArray(@values, Length(values), Result, TypeInfo(T));
        8: InternalFrom_Int64_OpenArray(@values, Length(values), Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TArrayIterator<T>.Create(values);
  end;
end;

class function TEnumerable.From<T>(const values: TArray<T>): IReadOnlyList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: InternalFrom_Object_DynArray(values, Result, TypeInfo(T));
    tkInterface: InternalFrom_Interface_DynArray(values, Result, TypeInfo(T));
    tkUString: InternalFrom_String_DynArray(values, Result, TypeInfo(T));
    tkMethod: InternalFrom_Method_DynArray(values, Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: InternalFrom_Int8_DynArray(values, Result, TypeInfo(T));
        2: InternalFrom_Int16_DynArray(values, Result, TypeInfo(T));
        4: InternalFrom_Int32_DynArray(values, Result, TypeInfo(T));
        8: InternalFrom_Int64_DynArray(values, Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TArrayIterator<T>.Create(values);
  end;
end;

class function TEnumerable.GroupBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>): IEnumerable<IGrouping<TKey, T>>;
begin
  IEnumerable<IInterface>(Result) := TGroupedEnumerable<T, TKey, T>.Create(
    source, keySelector, TIdentityFunction<T>.Instance);
end;

class function TEnumerable.GroupBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>;
  const comparer: IEqualityComparer<TKey>): IEnumerable<IGrouping<TKey, T>>;
begin
  IEnumerable<IInterface>(Result) := TGroupedEnumerable<T, TKey, T>.Create(
    source, keySelector, TIdentityFunction<T>.Instance, comparer);
end;

class function TEnumerable.GroupBy<T, TKey, TElement>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const elementSelector: Func<T, TElement>): IEnumerable<IGrouping<TKey, TElement>>;
begin
  IEnumerable<IInterface>(Result) := TGroupedEnumerable<T, TKey, TElement>.Create(
    source, keySelector, elementSelector);
end;

class function TEnumerable.GroupBy<T, TKey, TElement>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const elementSelector: Func<T, TElement>;
  const comparer: IEqualityComparer<TKey>): IEnumerable<IGrouping<TKey, TElement>>;
begin
  IEnumerable<IInterface>(Result) := TGroupedEnumerable<T, TKey, TElement>.Create(
    source, keySelector, elementSelector, comparer);
end;

class function TEnumerable.GroupBy<T, TKey, TResult>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const resultSelector: Func<TKey, IEnumerable<T>, TResult>): IEnumerable<TResult>;
begin
  Result := TGroupedEnumerable<T, TKey, T, TResult>.Create(
    source, keySelector, TIdentityFunction<T>.Instance, resultSelector);
end;

class function TEnumerable.GroupBy<T, TKey, TResult>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const resultSelector: Func<TKey, IEnumerable<T>, TResult>;
  const comparer: IEqualityComparer<TKey>): IEnumerable<TResult>;
begin
  Result := TGroupedEnumerable<T, TKey, T, TResult>.Create(
    source, keySelector, TIdentityFunction<T>.Instance, resultSelector, comparer);
end;

class function TEnumerable.GroupBy<T, TKey, TElement, TResult>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const elementSelector: Func<T, TElement>;
  const resultSelector: Func<TKey, IEnumerable<TElement>, TResult>): IEnumerable<TResult>;
begin
  Result := TGroupedEnumerable<T, TKey, TElement, TResult>.Create(
    source, keySelector, elementSelector, resultSelector);
end;

class function TEnumerable.GroupBy<T, TKey, TElement, TResult>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const elementSelector: Func<T, TElement>;
  const resultSelector: Func<TKey, IEnumerable<TElement>, TResult>;
  const comparer: IEqualityComparer<TKey>): IEnumerable<TResult>;
begin
  Result := TGroupedEnumerable<T, TKey, TElement, TResult>.Create(
    source, keySelector, elementSelector, resultSelector, comparer);
end;

class function TEnumerable.Intersect<T>(const first,
  second: IEnumerable<T>): IEnumerable<T>;
begin
  Result := first.Intersect(second);
end;

class function TEnumerable.Intersect<T>(const first, second: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): IEnumerable<T>;
begin
  Result := first.Intersect(second, comparer);
end;

class function TEnumerable.MaxBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>): T;
var
  keyComparer: Pointer;
begin
  keyComparer := _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T));
  Result := MaxBy<T, TKey>(source, keySelector, IComparer<TKey>(keyComparer));
end;

class function TEnumerable.MaxBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>; const comparer: IComparer<TKey>): T;
var
  enumerator: IEnumerator<T>;
  item: T;
  maxKey, key: TKey;
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(keySelector) then RaiseHelper.ArgumentNil(ExceptionArgument.keySelector);
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);

  enumerator := source.GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;

  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(enumerator).GetCurrent(Result)
  else
  {$ENDIF}
  Result := enumerator.Current;
  maxKey := keySelector(Result);
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    key := keySelector(item);
    if comparer.Compare(key, maxKey) > 0 then
    begin
      maxKey := key;
      Result := item;
    end;
  end;
end;

class function TEnumerable.MinBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>): T;
var
  keyComparer: Pointer;
begin
  keyComparer := _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T));
  Result := MinBy<T, TKey>(source, keySelector, IComparer<TKey>(keyComparer));
end;

class function TEnumerable.MinBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>; const comparer: IComparer<TKey>): T;
var
  enumerator: IEnumerator<T>;
  item: T;
  minKey, key: TKey;
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(keySelector) then RaiseHelper.ArgumentNil(ExceptionArgument.keySelector);
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);

  enumerator := source.GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;

  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(enumerator).GetCurrent(Result)
  else
  {$ENDIF}
  Result := enumerator.Current;
  minKey := keySelector(Result);
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    key := keySelector(item);
    if comparer.Compare(key, minKey) < 0 then
    begin
      minKey := key;
      Result := item;
    end;
  end;
end;

class function TEnumerable.OfType<T, TResult>(
  const source: IEnumerable<T>): IEnumerable<TResult>;
begin
  {$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(T) = tkClass) and (GetTypeKind(TResult) = tkClass) then
    InternalOfType_Object(IEnumerable<TObject>(source), Result, TypeInfo(TResult))
  else
  {$ENDIF}
  Result := TOfTypeIterator<T, TResult>.Create(source);
end;

class function TEnumerable.OrderBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>): IEnumerable<T>;
begin
  Result := TOrderedEnumerable<T,TKey>.Create(source, keySelector);
end;

class function TEnumerable.OrderBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>;
  const comparer: IComparer<TKey>): IEnumerable<T>;
begin
  Result := TOrderedEnumerable<T,TKey>.Create(source, keySelector, comparer);
end;

class function TEnumerable.OrderByDescending<T, TKey>(
  const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>): IEnumerable<T>;
begin
  Result := TOrderedEnumerable<T,TKey>.Create(source, keySelector, nil, True);
end;

class function TEnumerable.OrderByDescending<T, TKey>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const comparer: IComparer<TKey>): IEnumerable<T>;
begin
  Result := TOrderedEnumerable<T,TKey>.Create(source, keySelector, comparer, True);
end;

class function TEnumerable.Range(start, count: Integer): IReadOnlyList<Integer>; //FI:W521
begin
  if (count >= 0) and (Int64(start) + count <= Cardinal(MaxInt) + 1) then
    if count = 0 then
      Result := TEnumerable.Empty<Integer>
    else
      Result := TRangeIterator.Create(start, count)
  else
    RaiseHelper.ArgumentOutOfRange_Count;
end;

class function TEnumerable.Repeated<T>(const element: T; count: Integer): IEnumerable<T>; //FI:W521
begin
  if count = 0 then
    Result := TEnumerable.Empty<T>
  else if count > 0 then
    Result := TRepeatIterator<T>.Create(element, count)
  else
    RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
end;

class function TEnumerable.Select<T, TResult>(const source: IEnumerable<T>;
  const selector: Func<T, TResult>): IEnumerable<TResult>;
begin
  Result := TSelectIterator<T, TResult>.Create(source, selector);
end;

class function TEnumerable.Select<T, TResult>(const source: IEnumerable<T>;
  const selector: Func<T, Integer, TResult>): IEnumerable<TResult>;
begin
  Result := TSelectIndexIterator<T, TResult>.Create(source, selector);
end;

class function TEnumerable.SelectMany<T, TResult>(const source: IEnumerable<T>;
  const selector: Func<T, IEnumerable<TResult>>): IEnumerable<TResult>;
begin
  Result := TSelectManyIterator<T, TResult>.Create(source, selector);
end;

class function TEnumerable.SelectMany<T, TResult>(const source: IEnumerable<T>;
  const selector: Func<T, Integer, IEnumerable<TResult>>): IEnumerable<TResult>;
begin
  Result := TSelectManyIndexIterator<T, TResult>.Create(source, selector);
end;

class function TEnumerable.SelectMany<T, TCollection, TResult>(
  const source: IEnumerable<T>;
  const collectionSelector: Func<T, IEnumerable<TCollection>>;
  const resultSelector: Func<T, TCollection, TResult>): IEnumerable<TResult>;
begin
  Result := TSelectManyIterator<T, TCollection, TResult>.Create(
    source, collectionSelector, resultSelector);
end;

class function TEnumerable.SelectMany<T, TCollection, TResult>(
  const source: IEnumerable<T>;
  const collectionSelector: Func<T, Integer, IEnumerable<TCollection>>;
  const resultSelector: Func<T, TCollection, TResult>): IEnumerable<TResult>;
begin
  Result := TSelectManyIndexIterator<T, TCollection, TResult>.Create(
    source, collectionSelector, resultSelector);
end;

class function TEnumerable.ToDictionary<TSource, TKey>(
  const source: IEnumerable<TSource>;
  const keySelector: Func<TSource, TKey>): IOrderedDictionary<TKey, TSource>;
begin
  Result := ToDictionary<TSource, TKey, TSource>(source, keySelector,
    TIdentityFunction<TSource>.Instance, nil);
end;

class function TEnumerable.ToDictionary<TSource, TKey>(
  const source: IEnumerable<TSource>; const keySelector: Func<TSource, TKey>;
  const comparer: IEqualityComparer<TKey>): IOrderedDictionary<TKey, TSource>;
begin
  Result := ToDictionary<TSource, TKey, TSource>(source, keySelector,
    TIdentityFunction<TSource>.Instance, comparer);
end;

class function TEnumerable.ToDictionary<TSource, TKey, TElement>(
  const source: IEnumerable<TSource>; const keySelector: Func<TSource, TKey>;
  const elementSelector: Func<TSource, TElement>): IOrderedDictionary<TKey, TElement>;
begin
  Result := ToDictionary<TSource, TKey, TElement>(source, keySelector,
    elementSelector, nil);
end;

class function TEnumerable.ToDictionary<TSource, TKey, TElement>(
  const source: IEnumerable<TSource>; const keySelector: Func<TSource, TKey>;
  const elementSelector: Func<TSource, TElement>;
  const comparer: IEqualityComparer<TKey>): IOrderedDictionary<TKey, TElement>;
var
  item: TSource;
begin
  if not Assigned(source) then RaiseHelper.ArgumentNil(ExceptionArgument.source);
  if not Assigned(keySelector) then RaiseHelper.ArgumentNil(ExceptionArgument.keySelector);
  if not Assigned(elementSelector) then RaiseHelper.ArgumentNil(ExceptionArgument.elementSelector);

  Result := TCollections.CreateDictionary<TKey, TElement>(comparer);
  for item in source do
    Result.Add(keySelector(item), elementSelector(item));
end;

class function TEnumerable.ToLookup<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>): ILookup<TKey, T>;
begin
  ILookupInternal<TKey, T>(Result) := TLookup<TKey, T>.Create<T>(
    source, keySelector, TIdentityFunction<T>.Instance);
end;

class function TEnumerable.ToLookup<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>; const comparer: IEqualityComparer<TKey>): ILookup<TKey, T>;
begin
  ILookupInternal<TKey, T>(Result) := TLookup<TKey, T>.Create<T>(
    source, keySelector, TIdentityFunction<T>.Instance, comparer);
end;

class function TEnumerable.ToLookup<T, TKey, TElement>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const elementSelector: Func<T, TElement>): ILookup<TKey, TElement>;
begin
  ILookupInternal<TKey, TElement>(Result) := TLookup<TKey, TElement>.Create<T>(
    source, keySelector, elementSelector);
end;

class function TEnumerable.ToLookup<T, TKey, TElement>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const elementSelector: Func<T, TElement>;
  const comparer: IEqualityComparer<TKey>): ILookup<TKey, TElement>;
begin
  ILookupInternal<TKey, TElement>(Result) := TLookup<TKey, TElement>.Create<T>(
    source, keySelector, elementSelector, comparer);
end;

class function TEnumerable.Union<T>(const first, second: IEnumerable<T>): IEnumerable<T>;
begin
  Result := first.Union(second);
end;

class function TEnumerable.Union<T>(const first, second: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): IEnumerable<T>;
begin
  Result := first.Union(second, comparer);
end;

class function TEnumerable.Zip<TFirst, TSecond>(
  const first: IEnumerable<TFirst>;
  const second: IEnumerable<TSecond>): IEnumerable<Tuple<TFirst, TSecond>>;
begin
  Result := TZipIterator<TFirst, TSecond, Tuple<TFirst, TSecond>>.Create(
    first, second,
    function(const first: TFirst; const second: TSecond): Tuple<TFirst, TSecond>
    begin
      Result := Tuple<TFirst, TSecond>.Create(first, second);
    end);
end;

class function TEnumerable.Zip<TFirst, TSecond, TResult>(
  const first: IEnumerable<TFirst>; const second: IEnumerable<TSecond>;
  const resultSelector: Func<TFirst, TSecond, TResult>): IEnumerable<TResult>;
begin
  Result := TZipIterator<TFirst, TSecond, TResult>.Create(first, second, resultSelector);
end;

{$ENDREGION}


{$REGION 'TLinkedListNode<T>'}

constructor TLinkedListNode<T>.Create(const value: T);
begin
  fItem := value;
end;

function TLinkedListNode<T>.GetList: ILinkedList<T>;
begin
  Result := TLinkedList<T>(fList);
end;

function TLinkedListNode<T>.GetNext: TLinkedListNode<T>;
begin
  if Assigned(fNext) and (fNext <> TLinkedList<T>(fList).fHead) then
    Result := fNext
  else
    Result := nil;
end;

function TLinkedListNode<T>.GetPrevious: TLinkedListNode<T>;
begin
  if Assigned(fPrev) and (Self <> TLinkedList<T>(fList).fHead) then
    Result := fPrev
  else
    Result := nil;
end;

{$ENDREGION}


{$REGION 'TCollectionHelper'}

function TCollectionHelper.AsList: IList<TCollectionItem>;
begin
  Result := TCollectionList<TCollectionItem>.Create(Self);
end;

function TCollectionHelper.AsList<T>: IList<T>;
begin
  Result := TCollectionList<T>.Create(Self);
end;

{$ENDREGION}


{$REGION 'AutoInitAttribute'}

function InitElementType(fieldType: PTypeInfo): PTypeInfo;
begin
  Assert(fieldType.Kind = tkInterface);
  Assert(fieldType.TypeData.GUID = IList<TObject>);
  Result := GetElementType(fieldType);
  Assert(Result.Kind in [tkClass, tkInterface]);
end;

constructor AutoInitAttribute.Create(ownsObjects: Boolean);
var
  elementType: PTypeInfo;
begin
  elementType := nil;
  inherited Create(
    procedure(fieldType: PTypeInfo; var value)
    begin
      if elementType = nil then
        elementType := InitElementType(fieldType);

      case elementType.Kind of
        tkClass:     IInterface(value) := TCollections.CreateObjectList(elementType, ownsObjects);
        tkInterface: IInterface(value) := TCollections.CreateInterfaceList(elementType);
      end;
    end);
end;

{$ENDREGION}


{$REGION 'TIdentityFunction<T>'}

class function TIdentityFunction<T>.GetInstance: Func<T, T>;
begin
  VTable := @Methods;
  Methods[3] := @TIdentityFunction<T>.Invoke;
  Pointer(Result) := @VTable;
end;

function TIdentityFunction<T>.Invoke(const x: T): T;
begin
  Result := x;
end;

{$ENDREGION}


{$REGION 'TOfTypeIterator'}

constructor TOfTypeIterator.Create(const source: IEnumerable<TObject>; resultClass: TClass);
begin
  fSource := source;
  fResultClass := resultClass;
end;

function TOfTypeIterator.GetElementType: PTypeInfo;
begin
  Result := fResultClass.ClassInfo;
end;

function TOfTypeIterator.GetEnumerator: IEnumerator<TObject>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    Parent := Self;
    fResultClass := Self.fResultClass;
  end;
end;

{$ENDREGION}


{$REGION 'TOfTypeIterator.TEnumerator'}

function TOfTypeIterator.TEnumerator.GetCurrent: TObject;
begin
  Result := fCurrent;
end;

function TOfTypeIterator.TEnumerator.MoveNext: Boolean; //FI:W521
var
  current: TObject;
begin
  repeat
    if Assigned(fEnumerator) then
    begin
      repeat
        Result := fEnumerator.MoveNext;
        if not Result then
          Break;
        current := fEnumerator.Current;
        if current = nil then
          Continue;
        fCurrent := current;
        Result := current.InheritsFrom(fResultClass);
        if Result then Break;
      until False;
      Exit;
    end;
  {$IFDEF MSWINDOWS}
    IEnumerableInternal(Parent.fSource).GetEnumerator(IEnumerator(fEnumerator));
  {$ELSE}
    fEnumerator := Parent.fSource.GetEnumerator;
  {$ENDIF}
  until False;
end;

{$ENDREGION}


end.
