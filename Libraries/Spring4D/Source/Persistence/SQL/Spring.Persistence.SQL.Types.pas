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

unit Spring.Persistence.SQL.Types;

interface

uses
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes,
  TypInfo;

type
  /// <summary>
  ///   Represents Database table.
  /// </summary>
  TSQLTable = class
  private
    fName: string;
    fSchema: string;
    fDescription: string;
    fIndex: Integer;
    function GetName: string;
    function GetNameWithoutSchema: string;
    function GetAlias: string;
  public
    constructor Create(const index: Integer = 0);
    class function CreateFromClass(entityClass: TClass): TSQLTable;

    procedure SetFromAttribute(const attribute: TableAttribute);

    property Alias: string read GetAlias;
    property Description: string read fDescription write fDescription;
    property Name: string read GetName write fName;
    property Schema: string read fSchema write fSchema;
    property NameWithoutSchema: string read GetNameWithoutSchema;
  end;

  /// <summary>
  ///   Represents field of the database table.
  /// </summary>
  TSQLField = class
  private
    fName: string;
    fTable: TSQLTable;
    function GetName: string;
    function GetTable: TSQLTable;
  public
    constructor Create(const name: string; const table: TSQLTable); virtual;

    property Name: string read GetName;
    property Table: TSQLTable read GetTable;
  end;

  TSQLParamField = class(TSQLField)
  private
    fColumn: ColumnAttribute;
    fParamName: string;
  public
    constructor Create(const name: string; const table: TSQLTable;
      const column: ColumnAttribute; const paramName: string); reintroduce; virtual;

    property Column: ColumnAttribute read fColumn;
    property ParamName: string read fParamName write fParamName;
  end;

  /// <summary>
  ///   Represents field of the database table which is used in <c>select</c>
  ///   statements.
  /// </summary>
  TSQLSelectField = class(TSQLField)
  private
    fNeedsAlias: Boolean;
  public
    constructor Create(const name: string; const table: TSQLTable;
      needsAlias: Boolean = False); reintroduce;
    property NeedsAlias: Boolean read fNeedsAlias;
  end;

  /// <summary>
  ///   Represents field of the database table which is used in <c>insert</c>
  ///   statements.
  /// </summary>
  TSQLInsertField = class(TSQLParamField);

  /// <summary>
  ///   Represents field of the database table which is used in <c>update</c>
  ///   statements.
  /// </summary>
  TSQLUpdateField = class(TSQLParamField);

  /// <summary>
  ///   Represents field of the database table which is used in <c>create table</c>
  ///    statements.
  /// </summary>
  TSQLCreateField = class(TSQLField)
  private
    fIsPrimaryKey: Boolean;
    fIsIdentity: Boolean;
    fTypeInfo: PTypeInfo;
    fLength: Integer;
    fScale: Integer;
    fProperties: TColumnProperties;
    fDescription: string;
    fPrecision: Integer;
    fColumnAttribute: ColumnAttribute;
  public
    procedure SetFromAttribute(const attribute: ColumnAttribute); virtual;
    function Clone: TSQLCreateField;

    property Description: string read fDescription;
    property IsPrimaryKey: Boolean read fIsPrimaryKey;
    property IsIdentity: Boolean read fIsIdentity;
    property TypeInfo: PTypeInfo read fTypeInfo write fTypeInfo;
    property Length: Integer read fLength;
    property Precision: Integer read fPrecision;
    property Scale: Integer read fScale;
    property Properties: TColumnProperties read fProperties;
  end;

  /// <summary>
  ///   Represents foreign key field of the database table.
  /// </summary>
  TSQLForeignKeyField = class(TSQLField)
  private
    fReferencedColumnName: string;
    fConstraints: TForeignStrategies;
    fReferencedTableName: string;
    function GetForeignKeyName: string;
    function GetConstraintsAsString: string;
  public
    constructor Create(const name: string; const table: TSQLTable;
      const referencedColumnName, referencedTableName: string;
      constraints: TForeignStrategies); reintroduce; overload;

    property ForeignKeyName: string read GetForeignKeyName;
    property ReferencedColumnName: string read fReferencedColumnName write fReferencedColumnName;
    property ReferencedTableName: string read fReferencedTableName write fReferencedTableName;
    property Constraints: TForeignStrategies read fConstraints write fConstraints;
    property ConstraintsAsString: string read GetConstraintsAsString;
  end;

  TMatchMode = (mmExact, mmStart, mmEnd, mmAnywhere);

  TWhereOperator = (woEqual, woNotEqual, woMore, woLess, woLike, woNotLike,
    woMoreOrEqual, woLessOrEqual, woIn, woNotIn, woIsNull, woIsNotNull, woOr,
    woOrEnd, woAnd, woAndEnd, woNot, woNotEnd, woBetween, woJunction);

  TStartOperators = set of TWhereOperator;

  TEndOperators = set of TWhereOperator;

  /// <summary>
  ///   Represents field of the database table which is used in <c>where</c>
  ///   clause.
  /// </summary>
  TSQLWhereField = class(TSQLParamField)
  private
    fWhereOperator: TWhereOperator;
    fLeftSQL: string;
    fRightSQL: string;
    fParamName2: string;
    fIgnoreCase: Boolean;
  public
    constructor Create(const name: string; const table: TSQLTable; ignoreCase: Boolean = False); reintroduce; overload;
    constructor Create(const leftSQL, rightSQL: string); reintroduce; overload;

    property WhereOperator: TWhereOperator read fWhereOperator write fWhereOperator;
    property LeftSQL: string read fLeftSQL write fLeftSQL;
    property RightSQL: string read fRightSQL write fRightSQL;
    property ParamName2: string read fParamName2 write fParamName2;
    property IgnoreCase: Boolean read fIgnoreCase;
  end;

  /// <summary>
  ///   Represents field of the database table which is used in <c>where</c>
  ///   clause.
  /// </summary>
  TSQLWherePropertyField = class(TSQLWhereField)
  private
    fOtherTable: TSQLTable;
  public
    constructor Create(const leftPropertyName, rightPropertyName: string;
      const leftTable, rightTable: TSQLTable); overload;

    property OtherTable: TSQLTable read fOtherTable write fOtherTable;
  end;

  /// <summary>
  ///   Represents field of the database table which is used in <c>group by</c>
  ///   clause.
  /// </summary>
  TSQLGroupByField = class(TSQLField)
  end;

  TSortingDirection = (stAscending, stDescending);

  /// <summary>
  ///   Represents field of the database table which is used in <c>order by</c>
  ///   clause.
  /// </summary>
  TSQLOrderByField = class(TSQLField)
  private
    fSortingDirection: TSortingDirection;
  public
    constructor Create(const name: string; const table: TSQLTable;
      sortingDirection: TSortingDirection = stAscending); reintroduce;

    property SortingDirection: TSortingDirection read fSortingDirection;
  end;

  TSQLJoinType = (jtInner, jtLeft);

  /// <summary>
  ///   Represents <c>join</c> segment.
  /// </summary>
  TSQLJoinSegment = class
  private
    fPrimaryKeyField: TSQLField;
    fForeignKeyField: TSQLField;
  public
    constructor Create(const primaryKeyField, foreignKeyField: TSQLField);
    destructor Destroy; override;

    property PrimaryKeyField: TSQLField read fPrimaryKeyField;
    property ForeignKeyField: TSQLField read fForeignKeyField;
  end;

  /// <summary>
  ///   Represents <c>join</c> of database tables.
  /// </summary>
  TSQLJoin = class
  private
    fJoinType: TSQLJoinType;
    fSegments: IList<TSQLJoinSegment>;
  public
    constructor Create(const joinType: TSQLJoinType);

    property JoinType: TSQLJoinType read fJoinType write fJoinType;
    property Segments: IList<TSQLJoinSegment> read fSegments write fSegments;
  end;

const
  WhereOperatorNames: array[TWhereOperator] of string = (
    {woEqual =}'=', {woNotEqual =}'<>', {woMore = }'>', {woLess = }'<', {woLike = }'LIKE', {woNotLike = }'NOT LIKE',
    {woMoreOrEqual = }'>=', {woLessOrEqual = }'<=', {woIn = }'IN', {woNotIn = }'NOT IN', {woIsNull}'IS NULL', {woIsNotNull}'IS NOT NULL',
    {woOr}'OR', {woOrEnd}'', {woAnd}'AND', {woAndEnd}'', {woNot}'NOT', {woNotEnd}'', {woBetween}'BETWEEN', {woJunction}''
  );

  StartOperators: TStartOperators = [woOr, woAnd, woNot];

  EndOperators: TEndOperators = [woOrEnd, woAndEnd, woNotEnd];

  StartEndOperators = [woOr, woOrEnd, woAnd, woAndEnd, woNot, woNotEnd];

  SortingDirectionNames: array[TSortingDirection] of string = (
    ' ASC', // stAscending
    ' DESC' // stDescending
  );

  JoinTypeNames: array[TSQLJoinType] of string = (
    ' INNER JOIN ',     // jtInner
    ' LEFT OUTER JOIN ' // jtOuter
  );

  ForeignStrategyNames: array[TForeignStrategy] of string = (
    ' ON DELETE SET NULL',    // fsOnDeleteSetNull
    ' ON DELETE SET DEFAULT', // fsOnDeleteSetDefault
    ' ON DELETE CASCADE',     // fsOnDeleteCascade
    ' ON DELETE NO ACTION',   // fsOnDeleteNoAction
    ' ON UPDATE SET NULL',    // fsOnUpdateSetNull
    ' ON UPDATE SET DEFAULT', // fsOnUpdateSetDefault
    ' ON UPDATE CASCADE',     // fsOnUpdateCascade
    ' ON UPDATE NO ACTION'    // fsOnUpdateNoAction
    );

function GetMatchModeString(matchMode: TMatchMode; const pattern: string;
  ignoreCase: Boolean = True): string;
function GetEndOperator(startOperator: TWhereOperator): TWhereOperator;

implementation

uses
  StrUtils,
  SysUtils,
  Spring,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Reflection;

function GetMatchModeString(matchMode: TMatchMode; const pattern: string;
  ignoreCase: Boolean): string;
const
  MATCH_CHAR = '%';
begin
  case matchMode of
    mmExact: Result := pattern;
    mmStart: Result := pattern + MATCH_CHAR;
    mmEnd: Result := MATCH_CHAR + pattern;
    mmAnywhere: Result := MATCH_CHAR + pattern + MATCH_CHAR;
  end;
  Result := QuotedStr(Result);
  if ignoreCase then
    Result := AnsiUpperCase(Result);
end;

function GetEndOperator(startOperator: TWhereOperator): TWhereOperator;
begin
  Result := startOperator;
  case startOperator of
    woOr: Result := woOrEnd;
    woAnd: Result := woAndEnd;
    woNot: Result := woNotEnd;
  end;
end;


{$REGION 'TSQLTable'}

constructor TSQLTable.Create(const index: Integer);
begin
  inherited Create;
  fIndex := index;
end;

class function TSQLTable.CreateFromClass(entityClass: TClass): TSQLTable;
var
  entityData: TEntityData;
begin
  if not Assigned(entityClass) then
    Exit(nil);

  entityData := TEntityCache.Get(entityClass);
  Result := TSQLTable.Create;
  Result.SetFromAttribute(entityData.EntityTable);
end;

function TSQLTable.GetAlias: string;
begin
  Result := 't' + IntToStr(fIndex);
end;

function TSQLTable.GetName: string;
begin
  if fSchema <> '' then
    Result := fSchema + '.' + fName
  else
    Result := fName
end;

function TSQLTable.GetNameWithoutSchema: string;
begin
  Result := fName;
end;

procedure TSQLTable.SetFromAttribute(const attribute: TableAttribute);
begin
  fName := attribute.TableName;
  fSchema := attribute.Schema;
end;

{$ENDREGION}


{$REGION 'TSQLField'}

constructor TSQLField.Create(const name: string; const table: TSQLTable);
begin
  inherited Create;
  fName := name;
  fTable := table;
end;

function TSQLField.GetName: string;
begin
  Result := fName;
end;

function TSQLField.GetTable: TSQLTable;
begin
  Result := fTable;
end;

{$ENDREGION}


{$REGION 'TSQLJoin'}

constructor TSQLJoin.Create(const joinType: TSQLJoinType);
begin
  inherited Create;
  fJoinType := joinType;
  fSegments := TCollections.CreateObjectList<TSQLJoinSegment>;
end;

{$ENDREGION}


{$REGION 'TSQLJoinSegment'}

constructor TSQLJoinSegment.Create(const primaryKeyField, foreignKeyField: TSQLField);
begin
  inherited Create;
  fPrimaryKeyField := primaryKeyField;
  fForeignKeyField := foreignKeyField;
end;

destructor TSQLJoinSegment.Destroy;
begin
  fForeignKeyField.Free;
  fPrimaryKeyField.Free;
  inherited Destroy;
end;

{$ENDREGION}


{$REGION 'TSQLOrderField'}

constructor TSQLOrderByField.Create(const name: string;
  const table: TSQLTable; sortingDirection: TSortingDirection);
begin
  inherited Create(name, table);
  fSortingDirection := sortingDirection;
end;

{$ENDREGION}


{$REGION 'TSQLWhereField'}

constructor TSQLWhereField.Create(const name: string; const table: TSQLTable;
  ignoreCase: Boolean);
begin
  inherited Create(name, table, nil, ':' + name);
  fWhereOperator := woEqual;
  fIgnoreCase := ignoreCase;
end;

constructor TSQLWhereField.Create(const leftSQL, rightSQL: string);
begin
  inherited Create(name, table, nil, ':' + name);
  fWhereOperator := woOr;
  fLeftSQL := leftSQL;
  fRightSQL := rightSQL;
end;

{$ENDREGION}


{$REGION 'TSQLCreateField'}

function TSQLCreateField.Clone: TSQLCreateField;
begin
  Result := TSQLCreateField.Create(fName, fTable);
  Result.SetFromAttribute(fColumnAttribute);
end;

procedure TSQLCreateField.SetFromAttribute(const attribute: ColumnAttribute);
begin
  Assert(Assigned(attribute));
  fColumnAttribute := attribute;
  fProperties := attribute.Properties;
  fLength := attribute.Length;
  fScale := attribute.Scale;
  fDescription := attribute.Description;
  fPrecision := attribute.Precision;
  fIsIdentity := attribute.IsIdentity;
  if fIsIdentity then
    fIsPrimaryKey := fIsIdentity
  else
    fIsPrimaryKey := cpPrimaryKey in attribute.Properties;
  fTypeInfo := attribute.Member.MemberType.Handle;
end;

{$ENDREGION}


{$REGION 'TSQLForeignKeyField'}

constructor TSQLForeignKeyField.Create(const name: string;
  const table: TSQLTable; const referencedColumnName,
  referencedTableName: string; constraints: TForeignStrategies);
begin
  inherited Create(name, table);
  fReferencedColumnName := referencedColumnName;
  fReferencedTableName := referencedTableName;
  fConstraints := constraints;
end;

function TSQLForeignKeyField.GetConstraintsAsString: string;
var
  constraint: TForeignStrategy;
begin
  Result := '';

  for constraint in fConstraints do
    Result := Result + ForeignStrategyNames[constraint];
end;

function TSQLForeignKeyField.GetForeignKeyName: string;
begin
  Result := Format('FK_%0:s_%1:s', [fTable.NameWithoutSchema, Name]);
end;

{$ENDREGION}


{$REGION 'TSQLWherePropertyField'}

constructor TSQLWherePropertyField.Create(const leftPropertyName,
  rightPropertyName: string; const leftTable, rightTable: TSQLTable);
begin
  inherited Create(leftPropertyName, rightPropertyName);
  fTable := leftTable;
  fOtherTable := rightTable;
end;

{$ENDREGION}


{$REGION 'TSQLParamField'}

constructor TSQLParamField.Create(const name: string; const table: TSQLTable;
  const column: ColumnAttribute; const paramName: string);
begin
  inherited Create(name, table);
  fColumn := column;
  fParamName := paramName;
end;

{$ENDREGION}


{$REGION 'TSQLSelectField'}

constructor TSQLSelectField.Create(const name: string; const table: TSQLTable;
  needsAlias: Boolean);
begin
  inherited Create(name, table);
  fNeedsAlias := needsAlias;
end;

{$ENDREGION}


end.
