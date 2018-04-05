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

unit Spring.Persistence.Criteria.Interfaces;

interface

uses
  Rtti,
  Spring.Collections,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type

  /// <summary>
  ///   Represents paged fetches.
  /// </summary>
  /// <remarks>
  ///   Pages are zero indexed.
  /// </remarks>
  IDBPage<T: class> = interface(IInvokable)
    ['{384357E2-A0B1-4EEE-9A22-2C01479D4148}']
    function GetPageIndex: Integer;
    function GetPageSize: Integer;
    function GetPageCount: Integer;
    function GetItemCount: Integer;
    function GetItems: IList<T>;

    property PageIndex: Integer read GetPageIndex;
    property PageSize: Integer read GetPageSize;
    property PageCount: Integer read GetPageCount;
    property ItemCount: Integer read GetItemCount;
    property Items: IList<T> read GetItems;
  end;

  /// <summary>
  ///   Represents an ordering imposed upon a <see cref="Spring.Persistence.Criteria.Interfaces|ICriteria&lt;T&gt;" />
  ///    result set.
  /// </summary>
  IOrderBy = interface(IInvokable)
    ['{F0047369-10D6-4A4D-9BB8-FD5699936D5D}']
    function GetEntityClass: TClass;
    function GetPropertyName: string;
    function GetSortingDirection: TSortingDirection;
    procedure SetEntityClass(value: TClass);
    property EntityClass: TClass read GetEntityClass write SetEntityClass;
    property PropertyName: string read GetPropertyName;
    property SortingDirection: TSortingDirection read GetSortingDirection;
  end;

  /// <summary>
  ///   An object-oriented representation of a query criterion that may be used
  ///   as a restriction in a <see cref="Spring.Persistence.Criteria.Interfaces|ICriteria&lt;T&gt;" />
  ///    query. Built-in criterion types are provided by the <see cref="Spring.Persistence.Criteria.Restrictions|Restrictions" />
  ///    factory class. This interface might be implemented by application
  ///   classes that define custom restriction criteria.
  /// </summary>
  ICriterion = interface(IInvokable)
    ['{E22DFB1C-0E0E-45F4-9740-9469164B4557}']
    function GetEntityClass: TClass;
    procedure SetEntityClass(value: TClass);
    function ToSqlString(const params: IList<TDBParam>;
      const command: TWhereCommand; const generator: ISQLGenerator;
      addToCommand: Boolean): string;
    property EntityClass: TClass read GetEntityClass write SetEntityClass;
  end;

  IJunction = interface(ICriterion)
    ['{2266D5F6-E2AD-4B6C-8244-432621918B0C}']
    function Add(const criterion: ICriterion): IJunction;
  end;

  /// <summary>
  ///   <see cref="Spring.Persistence.Criteria.Interfaces|ICriteria&lt;T&gt;" />
  ///    is a simplified API for retrieving entities by composing <see cref="Spring.Persistence.Criteria.Interfaces|ICriterion" />
  ///    objects. This is a very convenient approach for functionality like
  ///   "search" screens where there is a variable number of conditions to be
  ///   placed upon the result set. The <see cref="Spring.Persistence.Core.Session|TSession" />
  ///    is a factory for <see cref="Spring.Persistence.Criteria.Interfaces|ICriteria&lt;T&gt;" />
  ///    . <see cref="Spring.Persistence.Criteria.Interfaces|ICriterion" />
  ///   instances are usually obtained via the factory methods on <see cref="Spring.Persistence.Criteria.Restrictions|Restrictions" />
  ///    .
  /// </summary>
  /// <example>
  ///   <code lang="Delphi">var
  ///   cats: IList&lt;TCat&gt;;
  /// begin
  ///   cats := session.CreateCriteria&lt;TCat&gt;
  ///     .Add(Restrictions.Like('name', 'Iz%'))
  ///     .Add(Restrictions.Gt('weight', MIN_WEIGHT))
  ///     .OrderBy(TOrder.Asc('age')).ToList;</code>
  ///   <code lang="Delphi">var
  ///   cats: IList&lt;TCat&gt;;
  ///   Weight, Age: Prop;
  /// begin
  ///   Weight := GetProp('Weight');
  ///   Age := GetProp('Age');
  ///   cats := session.FindWhere&lt;TCat&gt;    (Weight &gt; MIN_WEIGHT)
  /// .OrderBy(Age.Desc).ToList;</code>
  /// </example>
  ICriteria<T: class, constructor> = interface(IInvokable)
    ['{09428AF2-3A36-44DB-B0E7-8B7D7620ED1C}']

    /// <summary>
    ///   Add a restriction to constrain the results to be retrieved.
    /// </summary>
    function Add(const criterion: ICriterion): ICriteria<T>;
    /// <summary>
    ///   Add a restriction to constrain the results to be retrieved.
    /// </summary>
    function Where(const criterion: ICriterion): ICriteria<T>;

    /// <summary>
    ///   Add an ordering to the result set.
    /// </summary>
    function OrderBy(const orderBy: IOrderBy): ICriteria<T>;

    /// <summary>
    ///   Clear current Criteria.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Returns the results.
    /// </summary>
    function ToList: IList<T>;

    /// <summary>
    ///   Returns the results in pages.
    /// </summary>
    /// <param name="page">
    ///   Page index. Zero (0) indexed
    /// </param>
    /// <param name="itemsPerPage">
    ///   Items include in on page.
    /// </param>
    function Page(page, itemsPerPage: Integer): IDBPage<T>;
  end;

  /// <summary>
  ///   A factory for property specific criterion and projection instances.
  /// </summary>
  /// <remarks>
  ///   For detailed methods documentation look in <see cref="Spring.Persistence.Criteria.Restrictions|Restrictions" />
  ///    .
  /// </remarks>
  IProperty = interface(IInvokable)
    ['{2F58C81C-4817-43E7-BA3F-7570FE2A6823}']
    function Eq(const value: TValue): ICriterion;
    function NotEq(const value: TValue): ICriterion;
    function GEq(const value: TValue): ICriterion;
    function Gt(const value: TValue): ICriterion;
    function IsNull: ICriterion;
    function IsNotNull: ICriterion;
    function Like(const value: string; matchMode: TMatchMode = mmExact; ignoreCase: Boolean = False): ICriterion;
    function NotLike(const value: string; matchMode: TMatchMode = mmExact; ignoreCase: Boolean = False): ICriterion;
    function LEq(const value: TValue): ICriterion;
    function Lt(const value: TValue): ICriterion;
    function &In(const values: TArray<string>; ignoreCase: Boolean = False): ICriterion; overload;
    function &In(const values: TArray<Integer>): ICriterion; overload;
    function NotIn(const values: TArray<string>; ignoreCase: Boolean = False): ICriterion; overload;
    function NotIn(const values: TArray<Integer>): ICriterion; overload;
    function Between(const low, high: TValue): ICriterion;

    function EqProperty(const other: IProperty): ICriterion; overload;
    function EqProperty(const otherPropertyName: string): ICriterion; overload;
    function NeProperty(const other: IProperty): ICriterion; overload;
    function NeProperty(const otherPropertyName: string): ICriterion; overload;
    function GeProperty(const other: IProperty): ICriterion; overload;
    function GeProperty(const otherPropertyName: string): ICriterion; overload;
    function GtProperty(const other: IProperty): ICriterion; overload;
    function GtProperty(const otherPropertyName: string): ICriterion; overload;
    function LeProperty(const other: IProperty): ICriterion; overload;
    function LeProperty(const otherPropertyName: string): ICriterion; overload;
    function LtProperty(const other: IProperty): ICriterion; overload;
    function LtProperty(const otherPropertyName: string): ICriterion; overload;

    function GetEntityClass: TClass;
    function GetPropertyName: string;

    function Asc: IOrderBy;
    function Desc: IOrderBy;

    property EntityClass: TClass read GetEntityClass;
    property PropertyName: string read GetPropertyName;
  end;

implementation

end.
