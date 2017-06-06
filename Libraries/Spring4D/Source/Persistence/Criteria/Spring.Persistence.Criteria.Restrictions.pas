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

unit Spring.Persistence.Criteria.Restrictions;

interface

uses
  Spring,
  Spring.Persistence.Criteria.Interfaces,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   The criterion package may be used by applications as a framework for
  ///   building new kinds of Criterion. However, it is intended that most
  ///   applications will simply use the built-in criterion types via the
  ///   static factory methods of this class.
  /// </summary>
  Restrictions = record
  public
    /// <summary>
    ///   Apply an <b>"equal"</b> constraint to the named property.
    /// </summary>
    class function Eq(const propertyName: string; const value: TValue): ICriterion; static;

    /// <summary>
    ///   Apply a <b>"not equal"</b> constraint to the named property.
    /// </summary>
    class function NotEq(const propertyName: string; const value: TValue): ICriterion; static;

    /// <summary>
    ///   Apply a <b>"greater than or equal"</b> constraint to the named
    ///   property.
    /// </summary>
    class function GEq(const propertyName: string; const value: TValue): ICriterion; static;

    /// <summary>
    ///   Apply a <b>"greater than"</b> constraint to the named property.
    /// </summary>
    class function Gt(const propertyName: string; const value: TValue): ICriterion; static;

    /// <summary>
    ///   Apply an <b>"is null"</b> constraint to the named property.
    /// </summary>
    class function IsNull(const propertyName: string): ICriterion; static;

    /// <summary>
    ///   Apply an <b>"is not null"</b> constraint to the named property.
    /// </summary>
    class function IsNotNull(const propertyName: string): ICriterion; static;

    /// <summary>
    ///   Apply a <b>"like"</b> constraint to the named property.
    /// </summary>
    class function Like(const propertyName, value: string;
      matchMode: TMatchMode = mmExact; ignoreCase: Boolean = False): ICriterion; static;

    /// <summary>
    ///   Apply a <b>"not like"</b> constraint to the named property.
    /// </summary>
    class function NotLike(const propertyName, value: string;
      matchMode: TMatchMode = mmExact; ignoreCase: Boolean = False): ICriterion; static;

    /// <summary>
    ///   Apply a <b>"less than or equal"</b> constraint to the named property.
    /// </summary>
    class function LEq(const propertyName: string; const value: TValue): ICriterion; static;

    /// <summary>
    ///   Apply a <b>"less than"</b> constraint to the named property.
    /// </summary>
    class function Lt(const propertyName: string; const value: TValue): ICriterion; static;

    /// <summary>
    ///   Apply an <b>"in"</b> constraint to the named property.
    /// </summary>
    class function &In(const propertyName: string;
      const values: TArray<TValue>; ignoreCase: Boolean = False): ICriterion; overload; static;

    /// <summary>
    ///   Apply an <b>"in"</b> constraint to the named property.
    /// </summary>
    class function &In(const propertyName: string;
      const values: array of const; ignoreCase: Boolean = False): ICriterion; overload; static;

    /// <summary>
    ///   Apply an <b>"in"</b> constraint to the named property.
    /// </summary>
    class function &In<T>(const propertyName: string;
      const values: TArray<T>; ignoreCase: Boolean = False): ICriterion; overload; static;

    /// <summary>
    ///   Apply an <b>"not in"</b> constraint to the named property.
    /// </summary>
    class function NotIn(const propertyName: string;
      const values: TArray<TValue>; ignoreCase: Boolean = False): ICriterion; overload; static;

    /// <summary>
    ///   Apply an <b>"not in"</b> constraint to the named property.
    /// </summary>
    class function NotIn(const propertyName: string;
      const values: array of const; ignoreCase: Boolean = False): ICriterion; overload; static;

    /// <summary>
    ///   Apply an <b>"not in"</b> constraint to the named property.
    /// </summary>
    class function NotIn<T>(const propertyName: string;
      const values: TArray<T>; ignoreCase: Boolean = False): ICriterion; overload; static;

    /// <summary>
    ///   Return the conjuction of two expressions.
    /// </summary>
    class function &And(const leftExpression, rightExpression: ICriterion): ICriterion; static;

    /// <summary>
    ///   Return the disjuction of two expressions.
    /// </summary>
    class function &Or(const leftExpression, rightExpression: ICriterion): ICriterion; static;

    /// <summary>
    ///   Return the negation of an expression.
    /// </summary>
    class function &Not(const expression: ICriterion): ICriterion; static;

    /// <summary>
    ///   Apply a "<b>not equal</b>" constraint to two properties.
    /// </summary>
    class function NeProperty(const propertyName, otherPropertyName: string): ICriterion; static;

    /// <summary>
    ///   Apply an "<b>equal</b>" constraint to two properties.
    /// </summary>
    class function EqProperty(const propertyName, otherPropertyName: string): ICriterion; static;

    /// <summary>
    ///   Apply a "<b>greater than or equal</b>" constraint to two properties.
    /// </summary>
    class function GeProperty(const propertyName, otherPropertyName: string): ICriterion; static;

    /// <summary>
    ///   Apply a "<b>greater than</b>" constraint to two properties.
    /// </summary>
    class function GtProperty(const propertyName, otherPropertyName: string): ICriterion; static;

    /// <summary>
    ///   Apply a "<b>less than or equal</b>" constraint to two properties.
    /// </summary>
    class function LeProperty(const propertyName, otherPropertyName: string): ICriterion; static;

    /// <summary>
    ///   Apply a "<b>less than</b>" constraint to two properties.
    /// </summary>
    class function LtProperty(const propertyName, otherPropertyName: string): ICriterion; static;

    /// <summary>
    ///   Apply a "<b>between</b>" constraint to the named property.
    /// </summary>
    class function Between(const propertyName: string; const lowValue, highValue: TValue): ICriterion; static;

    /// <summary>
    ///   Group expressions together in a single conjunction (A and B and C...)
    /// </summary>
    class function Conjunction: IJunction; static;

    /// <summary>
    ///   Group expressions together in a single disjunction (A or B or C...)
    /// </summary>
    class function Disjunction: IJunction; static;
  end;

implementation

uses
  Spring.Persistence.Criteria.Criterion.Conjunction,
  Spring.Persistence.Criteria.Criterion.Disjunction,
  Spring.Persistence.Criteria.Criterion.SimpleExpression,
  Spring.Persistence.Criteria.Criterion.NullExpression,
  Spring.Persistence.Criteria.Criterion.LikeExpression,
  Spring.Persistence.Criteria.Criterion.InExpression,
  Spring.Persistence.Criteria.Criterion.LogicalExpression,
  Spring.Persistence.Criteria.Criterion.PropertyExpression,
  Spring.Persistence.Criteria.Criterion.BetweenExpression;

function Copy(const values: array of const): TArray<TValue>;
var
  i: Integer;
begin
  SetLength(Result, Length(values));
  for i := 0 to High(values) do
    Result[i] := TValue.FromVarRec(TVarRec(values[i]));
end;


{$REGION 'Restrictions'}

class function Restrictions.&Or(
  const leftExpression, rightExpression: ICriterion): ICriterion;
begin
  Result := TLogicalExpression.Create(leftExpression, rightExpression, woOr);
end;

class function Restrictions.NeProperty(
  const propertyName, otherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(propertyName, otherPropertyName, woNotEqual);
end;

class function Restrictions.&Not(const expression: ICriterion): ICriterion;
begin
  Result := TLogicalExpression.Create(expression, nil, woNot);
end;

class function Restrictions.&And(
  const leftExpression, rightExpression: ICriterion): ICriterion;
begin
  Result := TLogicalExpression.Create(leftExpression, rightExpression, woAnd);
end;

class function Restrictions.Between(const propertyName: string;
  const lowValue, highValue: TValue): ICriterion;
begin
  Result := TBetweenExpression.Create(propertyName, lowValue, highValue, woBetween);
end;

class function Restrictions.Conjunction: IJunction;
begin
  Result := TConjunction.Create;
end;

class function Restrictions.Disjunction: IJunction;
begin
  Result := TDisjunction.Create;
end;

class function Restrictions.Eq(const propertyName: string;
  const value: TValue): ICriterion;
begin
  Result := TSimpleExpression.Create(propertyName, value, woEqual);
end;

class function Restrictions.EqProperty(
  const propertyName, otherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(propertyName, otherPropertyName, woEqual);
end;

class function Restrictions.GeProperty(
  const propertyName, otherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(propertyName, otherPropertyName, woMoreOrEqual);
end;

class function Restrictions.GEq(const propertyName: string;
  const value: TValue): ICriterion;
begin
  Result := TSimpleExpression.Create(propertyName, value, woMoreOrEqual);
end;

class function Restrictions.Gt(const propertyName: string;
  const value: TValue): ICriterion;
begin
  Result := TSimpleExpression.Create(propertyName, value, woMore);
end;

class function Restrictions.GtProperty(
  const propertyName, otherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(propertyName, otherPropertyName, woMore);
end;

class function Restrictions.&In(const propertyName: string;
  const values: TArray<TValue>; ignoreCase: Boolean): ICriterion;
begin
  Result := TInExpression.Create(propertyName, values, woIn, ignoreCase);
end;

class function Restrictions.&In(const propertyName: string;
  const values: array of const; ignoreCase: Boolean): ICriterion;
begin
  Result := TInExpression.Create(propertyName, Copy(values), woIn, ignoreCase);
end;

class function Restrictions.&In<T>(const propertyName: string;
  const values: TArray<T>; ignoreCase: Boolean): ICriterion;
begin
  Result := TInExpression<T>.Create(propertyName, values, woIn, ignoreCase);
end;

class function Restrictions.IsNotNull(const propertyName: string): ICriterion;
begin
  Result := TNullExpression.Create(propertyName, woIsNotNull);
end;

class function Restrictions.IsNull(const propertyName: string): ICriterion;
begin
  Result := TNullExpression.Create(propertyName, woIsNull);
end;

class function Restrictions.LeProperty(
  const propertyName, otherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(propertyName, otherPropertyName, woLessOrEqual);
end;

class function Restrictions.LEq(const propertyName: string;
  const value: TValue): ICriterion;
begin
  Result := TSimpleExpression.Create(propertyName, value, woLessOrEqual);
end;

class function Restrictions.Like(const propertyName, value: string;
  matchMode: TMatchMode; ignoreCase: Boolean): ICriterion;
begin
  Result := TLikeExpression.Create(propertyName, value, woLike, matchMode, ignoreCase);
end;

class function Restrictions.Lt(const propertyName: string;
  const value: TValue): ICriterion;
begin
  Result := TSimpleExpression.Create(propertyName, value, woLess);
end;

class function Restrictions.LtProperty(
  const propertyName, otherPropertyName: string): ICriterion;
begin
  Result := TPropertyExpression.Create(propertyName, otherPropertyName, woLess);
end;

class function Restrictions.NotEq(const propertyName: string;
  const value: TValue): ICriterion;
begin
  Result := TSimpleExpression.Create(propertyName, value, woNotEqual);
end;

class function Restrictions.NotIn(const propertyName: string;
  const values: TArray<TValue>; ignoreCase: Boolean): ICriterion;
begin
  Result := TInExpression.Create(propertyName, values, woNotIn, ignoreCase);
end;

class function Restrictions.NotIn(const propertyName: string;
  const values: array of const; ignoreCase: Boolean): ICriterion;
begin
  Result := TInExpression.Create(propertyName, Copy(values), woNotIn, ignoreCase);
end;

class function Restrictions.NotIn<T>(const propertyName: string;
  const values: TArray<T>; ignoreCase: Boolean): ICriterion;
begin
  Result := TInExpression<T>.Create(propertyName, values, woNotIn, ignoreCase);
end;

class function Restrictions.NotLike(const propertyName, value: string;
  matchMode: TMatchMode; ignoreCase: Boolean): ICriterion;
begin
  Result := TLikeExpression.Create(propertyName, value, woNotLike, matchMode, ignoreCase);
end;

{$ENDREGION}


end.
