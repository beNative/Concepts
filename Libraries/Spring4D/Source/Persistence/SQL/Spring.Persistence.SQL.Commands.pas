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

unit Spring.Persistence.SQL.Commands;

interface

uses
  Spring.Collections,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Types;

type
  TDMLCommandType = (ctSelect, ctInsert, ctUpdate, ctDelete, ctUpdateVersion);

  /// <summary>
  ///   Represents abstract DML command.
  /// </summary>
  TDMLCommand = class abstract
  private
    fTable: TSQLTable;
    fEntity: TObject;
    fParameterNames: IDictionary<string,Integer>;
  protected
    procedure SetCommandFieldsFromColumns(
      const columns: IList<ColumnAttribute>); virtual; abstract;
  public
    constructor Create(const table: TSQLTable); virtual;

    function GetAndIncParameterName(const fieldName: string): string; virtual;
    function GetExistingParameterName(const fieldName: string): string; virtual;

    property Entity: TObject read fEntity write fEntity;
    property Table: TSQLTable read fTable;
  end;

  TWhereCommand = class(TDMLCommand)
  private
    fWhereFields: IList<TSQLWhereField>;
  public
    constructor Create(const table: TSQLTable); override;

    property WhereFields: IList<TSQLWhereField> read fWhereFields;
  end;

  /// <summary>
  ///   Represents <c>select</c> command.
  /// </summary>
  TSelectCommand = class(TWhereCommand)
  private
    fSelectFields: IList<TSQLSelectField>;
    fJoins: IList<TSQLJoin>;
    fGroupByFields: IList<TSQLGroupByField>;
    fOrderByFields: IList<TSQLOrderByField>;
    fPrimaryKeyColumn: ColumnAttribute;
    fForeignColumn: ForeignJoinColumnAttribute;
    fTables: IList<TSQLTable>;
    fOwnTable: Boolean;
  protected
    procedure InternalSetAssociations(entityClass: TClass;
      const baseTable: TSQLTable; var index: Integer);
  public
    constructor Create(const table: TSQLTable); overload; override;
    constructor Create(entityClass: TClass); reintroduce; overload;
    destructor Destroy; override;

    function FindTable(entityClass: TClass): TSQLTable;
    function FindCorrespondingTable(const table: TSQLTable): TSQLTable;
    procedure SetAssociations(entityClass: TClass); overload;
    procedure SetCommandFieldsFromColumns(const columns: IList<ColumnAttribute>); override;
    procedure SetFromPrimaryColumn;
    procedure SetFromForeignColumn(const entityData: TEntityData;
      foreignTableClass: TClass);

    property SelectFields: IList<TSQLSelectField> read fSelectFields;
    property Joins: IList<TSQLJoin> read fJoins;
    property GroupByFields: IList<TSQLGroupByField> read fGroupByFields;
    property OrderByFields: IList<TSQLOrderByField> read fOrderByFields;
    property PrimaryKeyColumn: ColumnAttribute read fPrimaryKeyColumn write fPrimaryKeyColumn;
    property ForeignColumn: ForeignJoinColumnAttribute read fForeignColumn write fForeignColumn;
    property Tables: IList<TSQLTable> read fTables;
  end;

  /// <summary>
  ///   Represents <c>insert</c> command.
  /// </summary>
  TInsertCommand = class(TDMLCommand)
  private
    fInsertFields: IList<TSQLInsertField>;
    fSequence: SequenceAttribute;
  public
    constructor Create(const table: TSQLTable); override;

    procedure SetCommandFieldsFromColumns(const columns: IList<ColumnAttribute>); override;

    property InsertFields: IList<TSQLInsertField> read fInsertFields;
    property Sequence: SequenceAttribute read fSequence write fSequence;
  end;

  /// <summary>
  ///   Represents <c>update</c> command.
  /// </summary>
  TUpdateCommand = class(TWhereCommand)
  private
    fUpdateFields: IList<TSQLUpdateField>;
    fPrimaryKeyColumn: ColumnAttribute;
    fVersionColumn: ColumnAttribute;
  public
    constructor Create(const table: TSQLTable); override;

    procedure SetCommandFieldsFromColumns(const columns: IList<ColumnAttribute>); override;

    property UpdateFields: IList<TSQLUpdateField> read fUpdateFields;
    property PrimaryKeyColumn: ColumnAttribute read fPrimaryKeyColumn write fPrimaryKeyColumn;
    property VersionColumn: ColumnAttribute read fVersionColumn write fVersionColumn;
  end;

  /// <summary>
  ///   Represents <c>delete</c> command.
  /// </summary>
  TDeleteCommand = class(TWhereCommand)
  private
    fPrimaryKeyColumnName: string;
    procedure SetPrimaryKeyColumnName(const value: string);
  public
    constructor Create(const table: TSQLTable); override;

    procedure SetCommandFieldsFromColumns(const columns: IList<ColumnAttribute>); override;

    property PrimaryKeyColumnName: string read fPrimaryKeyColumnName write SetPrimaryKeyColumnName;
  end;

  /// <summary>
  ///   Represents <c>create table</c> command.
  /// </summary>
  TCreateTableCommand = class(TDMLCommand)
  private
    fColumns: IList<TSQLCreateField>;
    fColumnNames: IList<string>;
    fTableExists: Boolean;
  public
    constructor Create(const table: TSQLTable); override;

    procedure SetCommandFieldsFromColumns(const columns: IList<ColumnAttribute>); override;

    property Columns: IList<TSQLCreateField> read fColumns;
    property ColumnNames: IList<string> read fColumnNames;
    property TableExists: Boolean read fTableExists write fTableExists;
  end;

  /// <summary>
  ///   Represents <c>create foreign key</c> command.
  /// </summary>
  TCreateForeignKeyCommand = class(TCreateTableCommand)
  private
    fForeignKeys: IList<TSQLForeignKeyField>;
  public
    constructor Create(const table: TSQLTable); override;

    procedure SetCommandFieldsFromColumns(const columns: IList<ColumnAttribute>); override;

    property ForeignKeys: IList<TSQLForeignKeyField> read fForeignKeys;
  end;

  /// <summary>
  ///   Represents <c>create sequence</c> command.
  /// </summary>
  TCreateSequenceCommand = class
  private
    fSequence: SequenceAttribute;
    fSequenceExists: Boolean;
  public
    constructor Create(const sequenceAttribute: SequenceAttribute); virtual;

    property Sequence: SequenceAttribute read fSequence write fSequence;
    property SequenceExists: Boolean read fSequenceExists write fSequenceExists;
  end;

implementation

uses
  Rtti,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Reflection;


{$REGION 'TSelectCommand'}

constructor TSelectCommand.Create(const table: TSQLTable);
begin
  inherited Create(table);
  fSelectFields := TCollections.CreateObjectList<TSQLSelectField>;
  fJoins := TCollections.CreateObjectList<TSQLJoin>;
  fGroupByFields := TCollections.CreateObjectList<TSQLGroupByField>;
  fOrderByFields := TCollections.CreateObjectList<TSQLOrderByField>;
  fTables := TCollections.CreateObjectList<TSQLTable>(True);
  fForeignColumn := nil;
end;

constructor TSelectCommand.Create(entityClass: TClass);
var
  entityData: TEntityData;
begin
  fOwnTable := True;
  entityData := TEntityCache.Get(entityClass);
  fTable := TSQLTable.CreateFromClass(entityClass);
  Create(fTable);
  SetCommandFieldsFromColumns(entityData.Columns);
  PrimaryKeyColumn := entityData.PrimaryKeyColumn;
  SetAssociations(entityClass);
end;

destructor TSelectCommand.Destroy;
begin
  if fOwnTable then
    fTable.Free;
  inherited Destroy;
end;

function TSelectCommand.FindCorrespondingTable(const table: TSQLTable): TSQLTable;
var
  currentTable: TSQLTable;
begin
  Result := table;

  if table = nil then
    Exit;

  for currentTable in fTables do
    if SameText(currentTable.NameWithoutSchema, table.NameWithoutSchema) then
      Exit(currentTable);
end;

function TSelectCommand.FindTable(entityClass: TClass): TSQLTable;
var
  tableName: string;
  currentTable: TSQLTable;
begin
  if entityClass = nil then
    Exit(fTable);

  tableName := TEntityCache.Get(entityClass).EntityTable.TableName;

  for currentTable in fTables do
    if SameText(currentTable.NameWithoutSchema, tableName) then
      Exit(currentTable);
  Result := fTable;
end;

procedure TSelectCommand.InternalSetAssociations(entityClass: TClass;
  const baseTable: TSQLTable; var index: Integer);
var
  manyToOneColumn: ManyToOneAttribute;
  memberType: TRttiType;
  entityData: TEntityData;
  table: TSQLTable;
  column: ColumnAttribute;
  selectField: TSQLSelectField;
  join: TSQLJoin;
begin
  for manyToOneColumn in TEntityCache.Get(entityClass).ManyToOneColumns do
  begin
    memberType := manyToOneColumn.Member.MemberType;
    if IsLazyType(memberType.Handle) then
      memberType := memberType.GetGenericArguments[0];
    entityData := TEntityCache.Get(memberType.AsInstance.MetaclassType);

    Inc(index);
    table := TSQLTable.Create(index);
    table.SetFromAttribute(entityData.EntityTable);
    fTables.Add(table);
    for column in entityData.Columns do
    begin
      selectField := TSQLSelectField.Create(column.ColumnName, table, True);
      fSelectFields.Add(selectField);
    end;

    join := TSQLJoin.Create(jtLeft);
    join.Segments.Add(TSQLJoinSegment.Create(
      TSQLField.Create(entityData.PrimaryKeyColumn.ColumnName, table),
      TSQLField.Create(manyToOneColumn.MappedByColumn.ColumnName, baseTable)));
    fJoins.Add(join);

    // support associates having associates themselves.
    InternalSetAssociations(memberType.AsInstance.MetaclassType, table, index);
  end;
end;

procedure TSelectCommand.SetAssociations(entityClass: TClass);
var
  index: Integer;
begin
  // When SetAssociations is called initially this value should be provided
  // starting from 0 (base table) and is incremented for each associate recursively
  index := 0;
  InternalSetAssociations(entityClass, fTable, index);
end;

procedure TSelectCommand.SetFromForeignColumn(const entityData: TEntityData;
  foreignTableClass: TClass);
var
  primaryKeyColumnAttribute: ColumnAttribute;
  whereField: TSQLWhereField;
  foreignEntityData: TEntityData;
begin
  fForeignColumn := nil;
  foreignEntityData := TEntityCache.Get(foreignTableClass);
  primaryKeyColumnAttribute := foreignEntityData.PrimaryKeyColumn;
  if not Assigned(primaryKeyColumnAttribute) then
    Exit;

  fForeignColumn := entityData.GetForeignKeyColumn(
    foreignEntityData.EntityTable, primaryKeyColumnAttribute);
  if not Assigned(fForeignColumn) then
    Exit;

  whereField := TSQLWhereField.Create(fForeignColumn.Name, fTable, nil,
    GetAndIncParameterName(fForeignColumn.Name));
  WhereFields.Add(whereField);
end;

procedure TSelectCommand.SetFromPrimaryColumn;
var
  whereField: TSQLWhereField;
begin
  fForeignColumn := nil;
  if Assigned(fPrimaryKeyColumn) then
  begin
    whereField := TSQLWhereField.Create(fPrimaryKeyColumn.ColumnName, fTable,
      fPrimaryKeyColumn, GetAndIncParameterName(fPrimaryKeyColumn.ColumnName));
    WhereFields.Add(whereField);
  end;
end;

procedure TSelectCommand.SetCommandFieldsFromColumns(
  const columns: IList<ColumnAttribute>);
var
  column: ColumnAttribute;
  selectField: TSQLSelectField;
begin
  Assert(Assigned(columns), 'AColumns not assigned');
  fSelectFields.Clear;
  fJoins.Clear;
  WhereFields.Clear;
  fGroupByFields.Clear;
  fOrderByFields.Clear;

  for column in columns do
  begin
    selectField := TSQLSelectField.Create(column.ColumnName, fTable);
    fSelectFields.Add(selectField);
  end;
end;

{$ENDREGION}


{$REGION 'TInsertCommand'}

constructor TInsertCommand.Create(const table: TSQLTable);
begin
  inherited Create(table);
  fInsertFields := TCollections.CreateObjectList<TSQLInsertField>;
  fSequence := nil;
end;

procedure TInsertCommand.SetCommandFieldsFromColumns(
  const columns: IList<ColumnAttribute>);
var
  column: ColumnAttribute;
  insertField: TSQLInsertField;
begin
  Assert(Assigned(columns), 'AColumns not assigned');

  fInsertFields.Clear;
  for column in columns do
    if column.CanInsert then
    begin
      insertField := TSQLInsertField.Create(column.ColumnName, fTable, column,
        GetAndIncParameterName(column.ColumnName));
      fInsertFields.Add(insertField);
    end;
end;

{$ENDREGION}


{$REGION 'TUpdateCommand'}

constructor TUpdateCommand.Create(const table: TSQLTable);
begin
  inherited Create(table);
  fUpdateFields := TCollections.CreateObjectList<TSQLUpdateField>;
  fPrimaryKeyColumn := nil;
end;

procedure TUpdateCommand.SetCommandFieldsFromColumns(
  const columns: IList<ColumnAttribute>);
var
  updateField: TSQLUpdateField;
  whereField: TSQLWhereField;
  column: ColumnAttribute;
begin
  Assert(Assigned(columns), 'AColumns not assigned');

  fUpdateFields.Clear;
  fWhereFields.Clear;
  for column in columns do
    if column.CanUpdate and not Column.IsVersionColumn then
    begin
      updateField := TSQLUpdateField.Create(column.ColumnName, fTable, column,
        GetAndIncParameterName(column.ColumnName));
      fUpdateFields.Add(updateField);
    end;

  if Assigned(fPrimaryKeyColumn) then
  begin
    whereField := TSQLWhereField.Create(fPrimaryKeyColumn.ColumnName, fTable,
      fPrimaryKeyColumn, GetAndIncParameterName(fPrimaryKeyColumn.ColumnName));
    fWhereFields.Add(whereField);
  end;

  if Assigned(fVersionColumn) then
  begin
    updateField := TSQLUpdateField.Create(fVersionColumn.ColumnName, fTable,
      fVersionColumn, GetAndIncParameterName(fVersionColumn.ColumnName));
    fUpdateFields.Add(updateField);

    whereField := TSQLWhereField.Create(fVersionColumn.ColumnName, fTable,
      fVersionColumn, GetAndIncParameterName(fVersionColumn.ColumnName));
    fWhereFields.Add(whereField);
  end;
end;

{$ENDREGION}


{$REGION 'TDeleteCommand'}

constructor TDeleteCommand.Create(const table: TSQLTable);
begin
  inherited Create(table);
  fPrimaryKeyColumnName := '';
end;

procedure TDeleteCommand.SetCommandFieldsFromColumns(
  const columns: IList<ColumnAttribute>);
begin
  SetPrimaryKeyColumnName(fPrimaryKeyColumnName);
end;

procedure TDeleteCommand.SetPrimaryKeyColumnName(const value: string);
var
  whereField: TSQLWhereField;
begin
  Assert(value <> '', 'Primary key column name is not specified for deletion');

  fPrimaryKeyColumnName := value;
  fWhereFields.Clear;
  whereField := TSQLWhereField.Create(fPrimaryKeyColumnName, fTable, nil,
    GetAndIncParameterName(fPrimaryKeyColumnName));
  fWhereFields.Add(whereField);
end;

{$ENDREGION}


{$REGION 'TDMLCommand'}

constructor TDMLCommand.Create(const table: TSQLTable);
begin
  inherited Create;
  fTable := table;
  fParameterNames := TCollections.CreateDictionary<string,Integer>;
end;

function TDMLCommand.GetAndIncParameterName(const fieldName: string): string;
var
  index: Integer;
  upperCaseFieldName: string;
begin
  upperCaseFieldName := AnsiUpperCase(fieldName);
  if fParameterNames.TryGetValue(upperCaseFieldName, index) then
    Inc(index)
  else
    index := 1;
  fParameterNames.AddOrSetValue(upperCaseFieldName, index);
  Result := Format(':%s%d', [upperCaseFieldName, index]);
end;

function TDMLCommand.GetExistingParameterName(const fieldName: string): string;
var
  index: Integer;
  upperCaseFieldName: string;
begin
  upperCaseFieldName := AnsiUpperCase(fieldName);
  if not fParameterNames.TryGetValue(upperCaseFieldName, index) then
    index := 1;
  Result := Format(':%s%d', [upperCaseFieldName, index]);
end;

{$ENDREGION}


{$REGION 'TCreateTableCommand'}

constructor TCreateTableCommand.Create(const table: TSQLTable);
begin
  inherited Create(table);
  fColumns := TCollections.CreateObjectList<TSQLCreateField>(True);
  fColumnNames := TCollections.CreateList<string>(TStringComparer.OrdinalIgnoreCase());
end;

procedure TCreateTableCommand.SetCommandFieldsFromColumns(
  const columns: IList<ColumnAttribute>);
var
  column: ColumnAttribute;
  field: TSQLCreateField;
begin
  fColumns.Clear;
  fColumnNames.Clear;

  for column in columns do
  begin
    field := TSQLCreateField.Create(column.ColumnName, fTable);
    field.SetFromAttribute(column);
    fColumns.Add(field);
  end;
end;

{$ENDREGION}


{$REGION 'TCreateForeignKeyCommand'}

constructor TCreateForeignKeyCommand.Create(const table: TSQLTable);
begin
  inherited Create(table);
  fForeignKeys := TCollections.CreateObjectList<TSQLForeignKeyField>(True);
end;

procedure TCreateForeignKeyCommand.SetCommandFieldsFromColumns(
  const columns: IList<ColumnAttribute>);
var
  column: ColumnAttribute;
  foreignKeyColumn: ForeignJoinColumnAttribute;
  foreignKeyField: TSQLForeignKeyField;
begin
  inherited SetCommandFieldsFromColumns(columns);
  fForeignKeys.Clear;
  for column in columns do
  begin
    if column.Member.TryGetCustomAttribute<ForeignJoinColumnAttribute>(foreignKeyColumn) then
    begin
      foreignKeyField := TSQLForeignKeyField.Create(foreignKeyColumn.Name, fTable);
      foreignKeyField.Constraints := foreignKeyColumn.ForeignStrategies;
      foreignKeyField.ReferencedColumnName := foreignKeyColumn.ReferencedColumnName;
      foreignKeyField.ReferencedTableName := foreignKeyColumn.ReferencedTableName;
      fForeignKeys.Add(foreignKeyField);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TCreateSequenceCommand'}

constructor TCreateSequenceCommand.Create(
  const sequenceAttribute: SequenceAttribute);
begin
  inherited Create;
  fSequence := sequenceAttribute;
end;

{$ENDREGION}


{$REGION 'TWhereCommand'}

constructor TWhereCommand.Create(const table: TSQLTable);
begin
  inherited Create(table);
  fWhereFields := TCollections.CreateObjectList<TSQLWhereField>;
end;

{$ENDREGION}


end.
