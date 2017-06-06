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

unit Spring.Persistence.SQL.Generators.Ansi;

interface

uses
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.Abstract,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Represents base class responsible for generating Ansi SQL compatible
  ///   statements. To write your custom SQL generator it is recommended to
  ///   inherit your type from this class and override needed methods which
  ///   requires different treatment for current database type.
  /// </summary>
  TAnsiSQLGenerator = class(TAbstractSQLGenerator)
  protected
    function DoGenerateBackupTableUsingCreate(const tableName: string): TArray<string>;
    function DoGenerateBackupTable(const tableName: string): TArray<string>; virtual;
    function DoGenerateRestoreTable(const tableName: string;
      const createColumns: IList<TSQLCreateField>;
      const dbColumns: IList<string>): TArray<string>; virtual;
    function DoGenerateCreateTable(const tableName: string;
      const columns: IList<TSQLCreateField>): string; virtual;

    function GetTableNameWithAlias(const table: TSQLTable): string; virtual;
    function GetQualifiedFieldName(const field: TSQLField): string; virtual;
    function GetEscapedFieldName(const field: TSQLField): string; virtual;
    function GetGroupByAsString(const groupFields: IList<TSQLGroupByField>): string; virtual;
    function GetJoinAsString(const join: TSQLJoin): string; virtual;
    function GetJoinsAsString(const joinFields: IList<TSQLJoin>): string; virtual;
    function GetOrderAsString(const orderByFields: IList<TSQLOrderByField>): string; virtual;
    function GetWhereAsString(const whereFields: IList<TSQLWhereField>): string; virtual;
    function GetSelectFieldsAsString(const selectFields: IList<TSQLSelectField>): string; virtual;
    function GetCreateFieldsAsString(const createFields: IList<TSQLCreateField>): string; overload; virtual;
    function GetCreateFieldsAsString(const createFields: IList<string>): string; overload; virtual;
    function GetCopyFieldsAsString(const createFields: IList<TSQLCreateField>;
      const copyFields: IList<string>): string; virtual;
    function GetTempTableName: string; virtual;
    function GetPrimaryKeyDefinition(const field: TSQLCreateField): string; virtual;
    function GetSplitStatementSymbol: string; virtual;
    procedure ParseFullTableName(const fullTableName: string;
      out tableName, schemaName: string); virtual;
    function GetEscapeChar: Char; override;
    function GetUpdateVersionFieldQuery(const command: TUpdateCommand;
      const versionColumn: VersionAttribute; const version, primaryKey: Variant): Variant; override;
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateWhere(const field: TSQLWhereField): string; override;
    function GenerateSelect(const command: TSelectCommand): string; override;
    function GenerateInsert(const command: TInsertCommand): string; override;
    function GenerateUpdate(const command: TUpdateCommand): string; override;
    function GenerateDelete(const command: TDeleteCommand): string; override;
    function GenerateCreateTable(const command: TCreateTableCommand): IList<string>; override;
    function GenerateCreateForeignKey(const command: TCreateForeignKeyCommand): IList<string>; override;

    /// <summary>
    ///   First drop sequence and then create it
    /// </summary>
    function GenerateCreateSequence(const command: TCreateSequenceCommand): string; override;
    function GenerateGetNextSequenceValue(const sequence: SequenceAttribute): string; override;
    function GenerateGetLastInsertId(const identityColumn: ColumnAttribute): string; override;
    function GeneratePagedQuery(const sql: string; limit, offset: Integer): string; override;
    function GenerateGetQueryCount(const sql: string): string; override;
    function GetSQLDataTypeName(const field: TSQLCreateField): string; override;
    function GetSQLTableCount(const tableName: string): string; override;
    function GetSQLSequenceCount(const sequenceName: string): string; override;
    function GetTableColumns(const tableName: string): string; override;

    /// <summary>
    ///   Constructs a query which checks if given table exists in the
    ///   database. Query which executes returned statement should return 0 or
    ///   raise an exception if table does not exist and &gt; 0 when it exists.
    /// </summary>
    function GetSQLTableExists(const tableName: string): string; override;
  end;

implementation

uses
  Rtti,
  StrUtils,
  SysUtils,
  TypInfo,
  Variants,
  Spring,
  Spring.Persistence.Core.Exceptions,
  Spring.Reflection;


{$REGION 'TAnsiSQLGenerator'}

function TAnsiSQLGenerator.DoGenerateBackupTable(
  const tableName: string): TArray<string>;
begin
  //select old data to temporary table
  SetLength(Result, 2);
  Result[0] := Format('SELECT * INTO %0:s FROM %1:s',
    [GetTempTableName, tableName]);
  //drop table
  Result[1] := Format('DROP TABLE %0:s ', [tableName]);
end;

function TAnsiSQLGenerator.DoGenerateBackupTableUsingCreate(
  const tableName: string): TArray<string>;
begin
  SetLength(Result, 2);
  Result[0] := Format('CREATE TABLE %0:s AS SELECT * FROM %1:s',
    [GetTempTableName, tableName]);
  //drop table
  Result[1] := Format('DROP TABLE %0:s ', [tableName]);
end;

function TAnsiSQLGenerator.DoGenerateCreateTable(const tableName: string;
  const columns: IList<TSQLCreateField>): string;
var
  sqlBuilder: TStringBuilder;
  i: Integer;
  field: TSQLCreateField;
begin
  sqlBuilder := TStringBuilder.Create;
  try
    sqlBuilder.AppendFormat('CREATE TABLE %0:s ', [tableName])
      .Append('(')
      .AppendLine;
    for i := 0 to columns.Count - 1 do
    begin
      field := columns[i];
      if i > 0 then
        sqlBuilder.Append(', ').AppendLine;

      sqlBuilder.AppendFormat('%0:s %1:s %2:s %3:s', [
        GetEscapedFieldName(field),
        GetSQLDataTypeName(field),
        IfThen(cpNotNull in field.Properties, 'NOT NULL', 'NULL'),
        IfThen(cpPrimaryKey in field.Properties, GetPrimaryKeyDefinition(field))]);
    end;

    sqlBuilder.AppendLine.Append(')');

    Result := sqlBuilder.ToString;
  finally
    sqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.DoGenerateRestoreTable(const tableName: string;
  const createColumns: IList<TSQLCreateField>;
  const dbColumns: IList<string>): TArray<string>;
begin
  SetLength(Result, 2);

  Result[0] := Format('INSERT INTO %0:s (%2:s) SELECT %3:s FROM %1:s' + sLineBreak,
    [tableName, GetTempTableName, GetCreateFieldsAsString(createColumns),
    GetCopyFieldsAsString(createColumns, dbColumns)]);

  //drop temporary table
  Result[1] := Format('DROP TABLE %0:s', [GetTempTableName]);
end;

function TAnsiSQLGenerator.GenerateCreateForeignKey(
  const command: TCreateForeignKeyCommand): IList<string>;
var
  sqlBuilder: TStringBuilder;
  field: TSQLForeignKeyField;
begin
  Result := TCollections.CreateList<string>;
  sqlBuilder := TStringBuilder.Create;
  try
    for field in command.ForeignKeys do
    begin
      sqlBuilder.Clear;
      sqlBuilder.AppendFormat('ALTER TABLE %0:s ', [command.Table.Name])
        .AppendLine
        .AppendFormat('ADD CONSTRAINT %0:s', [field.ForeignKeyName])
        .AppendLine
        .AppendFormat('FOREIGN KEY(%0:s)', [GetEscapedFieldName(field)])
        .AppendLine
        .AppendFormat(' REFERENCES %0:s (%1:s)', [field.ReferencedTableName,
          AnsiQuotedStr(field.ReferencedColumnName, GetEscapeChar)])
        .AppendLine
        .Append(field.ConstraintsAsString);

      Result.Add(sqlBuilder.ToString);
    end;
  finally
    sqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GenerateCreateSequence(
  const command: TCreateSequenceCommand): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GenerateCreateTable(
  const command: TCreateTableCommand): IList<string>;
begin
  Assert(Assigned(command));
  Result := TCollections.CreateList<string>;

  if command.TableExists then
    Result.AddRange(DoGenerateBackupTable(command.Table.Name));

  Result.Add(DoGenerateCreateTable(command.Table.Name, command.Columns));

  if command.TableExists then
    Result.AddRange(DoGenerateRestoreTable(
      command.Table.Name, command.Columns, command.ColumnNames));
end;

function TAnsiSQLGenerator.GenerateDelete(const command: TDeleteCommand): string;
var
  sqlBuilder: TStringBuilder;
  i: Integer;
  field: TSQLWhereField;
begin
  Assert(Assigned(command));

  sqlBuilder := TStringBuilder.Create;
  try
    sqlBuilder.AppendFormat('DELETE FROM %0:s', [command.Table.Name]);

    for i := 0 to command.WhereFields.Count - 1 do
    begin
      field := command.WhereFields[i];
      if i = 0 then
        sqlBuilder.AppendLine.Append(' WHERE ')
      else
        sqlBuilder.Append(' AND ');

      {TODO -oLinas -cGeneral : implement where operators}

      sqlBuilder.AppendFormat('%0:s = %1:s',
        [GetEscapedFieldName(field), field.ParamName]);
    end;

    sqlBuilder.Append(GetSplitStatementSymbol);

    Result := sqlBuilder.ToString;
  finally
    sqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GenerateGetLastInsertId(
  const identityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GenerateGetNextSequenceValue(
  const sequence: SequenceAttribute): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GenerateGetQueryCount(const sql: string): string;
var
  sqlBuilder: TStringBuilder;
  sqlStatement: string;
begin
  sqlBuilder := TStringBuilder.Create;
  try
    sqlStatement := sql;
    if EndsStr(';', sqlStatement) then
      SetLength(sqlStatement, Length(sqlStatement) - 1);

    sqlBuilder.Append('SELECT COUNT(*) FROM (')
      .AppendLine
      .Append(sqlStatement)
      .AppendLine
      .Append(') AS ORM_GET_QUERY_COUNT').Append(GetSplitStatementSymbol);

    Result := sqlBuilder.ToString;
  finally
    sqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GenerateInsert(const command: TInsertCommand): string;
var
  i: Integer;
  field: TSQLInsertField;
  fields, params: string;
begin
  Assert(Assigned(command));
  Assert(command.InsertFields.Any);

  Result := 'INSERT INTO ';

  fields := '';
  params := '';

  for i := 0 to command.InsertFields.Count - 1 do
  begin
    field := command.InsertFields[i];
    if i > 0 then
    begin
      fields := fields + ', ';
      params := params + ', ';
    end;

    fields := fields + GetEscapedFieldName(field);
    params := params + field.ParamName;
  end;

  Result := Result + command.Table.Name + ' (' + sLineBreak + '  ' + fields + ')' + sLineBreak +
    '  VALUES (' + sLineBreak + params + ')' + GetSplitStatementSymbol;
end;

function TAnsiSQLGenerator.GeneratePagedQuery(const sql: string;
  limit, offset: Integer): string;
var
  sqlStatement: string;
begin
  sqlStatement := sql;
  if EndsStr(';', sqlStatement) then
    SetLength(sqlStatement, Length(sqlStatement) - 1);

  Result := sqlStatement + Format(' LIMIT %1:d, %0:d%2:s',
    [limit, offset, GetSplitStatementSymbol]);
end;

function TAnsiSQLGenerator.GenerateSelect(const command: TSelectCommand): string;
var
  sqlBuilder: TStringBuilder;
begin
  Assert(Assigned(command));
  Assert(command.SelectFields.Any);

  sqlBuilder := TStringBuilder.Create;
  try
    sqlBuilder.Append('SELECT ')
      .Append(GetSelectFieldsAsString(command.SelectFields)).AppendLine
      .Append(' FROM ').Append(GetTableNameWithAlias(command.Table))
      .Append(GetJoinsAsString(command.Joins))
      .Append(GetWhereAsString(command.WhereFields))
      .Append(GetGroupByAsString(command.GroupByFields))
      .Append(GetOrderAsString(command.OrderByFields))
      .Append(GetSplitStatementSymbol);

    Result := sqlBuilder.ToString;
  finally
    sqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GenerateUpdate(const command: TUpdateCommand): string;
var
  sqlBuilder: TStringBuilder;
  updateField: TSQLUpdateField;
  whereField: TSQLWhereField;
  i: Integer;
begin
  Assert(Assigned(command));

  if not command.UpdateFields.Any then
    Exit('');

  sqlBuilder := TStringBuilder.Create;
  try
    sqlBuilder.Append('UPDATE ')
      .Append(command.Table.Name)
      .Append(' SET ').AppendLine;

    for i := 0 to command.UpdateFields.Count - 1 do
    begin
      updateField := command.UpdateFields[i];
      if i > 0 then
        sqlBuilder.Append(', ');

      sqlBuilder.AppendFormat('%0:s = %1:s',
        [GetEscapedFieldName(updateField), updateField.ParamName]);
    end;

    for i := 0 to command.WhereFields.Count - 1 do
    begin
      whereField := command.WhereFields[i];
      if i = 0 then
        sqlBuilder.AppendLine.Append(' WHERE ')
      else
        sqlBuilder.Append(' AND ');

      sqlBuilder.AppendFormat('%0:s = %1:s',
        [GetEscapedFieldName(whereField), whereField.ParamName]);
    end;

    sqlBuilder.Append(GetSplitStatementSymbol);

    Result := sqlBuilder.ToString;
  finally
    sqlBuilder.Free;
  end;
end;

function TAnsiSQLGenerator.GenerateWhere(const field: TSQLWhereField): string;
begin
  if field is TSQLWherePropertyField then
    Result := Format('(%s %s %s)', [
      field.Table.Alias + '.' + field.LeftSQL,
      WhereOperatorNames[field.WhereOperator],
      TSQLWherePropertyField(field).OtherTable.Alias + '.' + field.RightSQL])
  else
    case field.WhereOperator of
      woIsNull, woIsNotNull:
        Result := GetQualifiedFieldName(field) + ' ' + WhereOperatorNames[field.WhereOperator];
      woLike, woNotLike, woIn, woNotIn:
        // TODO: support parameter
        if field.IgnoreCase then
          Result := Format('UPPER(%s) %s %s', [
            GetQualifiedFieldName(field),
            WhereOperatorNames[field.WhereOperator],
            field.RightSQL])
        else
          Result := Format('%s %s %s', [
            GetQualifiedFieldName(field),
            WhereOperatorNames[field.WhereOperator],
            field.RightSQL]);
      woOr, woAnd:
        Result := Format('(%s %s %s)', [
          field.LeftSQL,
          WhereOperatorNames[field.WhereOperator],
          field.RightSQL]);
      woNot:
        Result := Format('%s (%s)', [
          WhereOperatorNames[field.WhereOperator],
          field.LeftSQL]);
      woOrEnd, woAndEnd, woNotEnd: Result := '';
      woJunction: Result := Format('(%s)', [field.LeftSQL]);
      woBetween:
        Result := Format('(%s %s %s AND %s)', [
          GetQualifiedFieldName(field),
          WhereOperatorNames[field.WhereOperator],
          field.ParamName,
          field.ParamName2]);
    else
      Result := Format('%s %s %s', [
        GetQualifiedFieldName(field),
        WhereOperatorNames[field.WhereOperator],
        field.ParamName]);
    end;
end;

function TAnsiSQLGenerator.GetCreateFieldsAsString(
  const createFields: IList<TSQLCreateField>): string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to createFields.Count - 1 do
  begin
    if i > 0 then
      Result := Result + ', ';

    Result := Result + GetEscapedFieldName(createFields[i]);
  end;
end;

function TAnsiSQLGenerator.GetQualifiedFieldName(const field: TSQLField): string;
begin
  Result := field.Table.Alias + '.' + GetEscapedFieldName(field);
end;

function TAnsiSQLGenerator.GetCopyFieldsAsString(
  const createFields: IList<TSQLCreateField>;
  const copyFields: IList<string>): string;
var
  i: Integer;
  field: TSQLCreateField;
begin
  Result := '';

  for i := 0 to createFields.Count - 1 do
  begin
    field := createFields[i];
    if i > 0 then
      Result := Result + ', ';

    if copyFields.Contains(field.Name) then
      Result := Result + GetEscapedFieldname(field)
    else
      Result := Result + 'NULL';
  end;
end;

function TAnsiSQLGenerator.GetCreateFieldsAsString(const createFields: IList<string>): string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to createFields.Count - 1 do
  begin
    if i > 0 then
      Result := Result + ', ';

    Result := Result + createFields[i];
  end;
end;

function TAnsiSQLGenerator.GetEscapedFieldName(const field: TSQLField): string;
begin
  Result := AnsiQuotedStr(field.Name, GetEscapeChar);
  if (field is TSQLSelectField) and TSQLSelectField(field).NeedsAlias then
    Result := Result + ' AS ' + field.Table.Alias + '$' + field.Name;
end;

function TAnsiSQLGenerator.GetEscapeChar: Char;
begin
  Result := '"';
end;

function TAnsiSQLGenerator.GetGroupByAsString(
  const groupFields: IList<TSQLGroupByField>): string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to groupFields.Count - 1 do
  begin
    if i > 0 then
      Result := Result + ', '
    else
      Result := sLineBreak + '  GROUP BY ';

    Result := Result + GetQualifiedFieldName(groupFields[i]);
  end;
end;

function TAnsiSQLGenerator.GetJoinAsString(const join: TSQLJoin): string;
var
  i: Integer;
  segment: TSQLJoinSegment;
begin
  Assert(join.Segments.Any);

  Result := JoinTypeNames[join.JoinType];

  for i := 0 to join.Segments.Count - 1 do
  begin
    segment := join.Segments[i];
    if i > 0 then
      Result := Result + ' AND ';

    Result := Result +
      GetTableNameWithAlias(segment.PrimaryKeyField.Table) + ' ON '  +
      GetQualifiedFieldName(segment.PrimaryKeyField) + ' = ' + GetQualifiedFieldName(segment.ForeignKeyField);
  end;
end;

function TAnsiSQLGenerator.GetJoinsAsString(const joinFields: IList<TSQLJoin>): string;
var
  field: TSQLJoin;
begin
  Result := '';

  for field in joinFields do
    Result := Result + sLineBreak + ' ' + GetJoinAsString(field);
end;

function TAnsiSQLGenerator.GetOrderAsString(
  const orderByFields: IList<TSQLOrderByField>): string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to orderByFields.Count - 1 do
  begin
    if i > 0 then
      Result := Result + ', '
    else
      Result := sLineBreak + '  ORDER BY ';

    Result := Result + GetQualifiedFieldName(orderByFields[i]) +
      SortingDirectionNames[orderByFields[i].SortingDirection];
  end;
end;

function TAnsiSQLGenerator.GetPrimaryKeyDefinition(const field: TSQLCreateField): string;
begin
  Result := 'PRIMARY KEY';
end;

function TAnsiSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlAnsiSQL;
end;

procedure TAnsiSQLGenerator.ParseFullTableName(const fullTableName: string;
  out tableName, schemaName: string);
var
  i: Integer;
begin
  i := Pos('.', fullTableName);
  if i > 1 then
  begin
    schemaName := Copy(fullTableName, 1, i - 1);
    tableName := Copy(fullTableName, i + 1);
  end
  else
  begin
    tableName := fullTableName;
    schemaName := '';
  end;
end;

function TAnsiSQLGenerator.GetSelectFieldsAsString(
  const selectFields: IList<TSQLSelectField>): string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to selectFields.Count - 1 do
  begin
    if i > 0 then
      Result := Result + ', ';

    Result := Result + GetQualifiedFieldName(selectFields[i]);
  end;
end;

function TAnsiSQLGenerator.GetSplitStatementSymbol: string;
begin
  Result := ';';
end;

function TAnsiSQLGenerator.GetSQLDataTypeName(const field: TSQLCreateField): string;
var
  typeInfo: PTypeInfo;
  createField: TSQLCreateField;
begin
  Assert(Assigned(field));
  Result := 'INTEGER';

  typeInfo := field.TypeInfo;

  case typeInfo.Kind of
    tkUnknown: ;
    tkInteger, tkSet:
      if field.Precision > 0 then
        Result := Format('NUMERIC(%0:d, %1:d)', [field.Precision, field.Scale]);
    tkEnumeration:
      if typeInfo = System.TypeInfo(Boolean) then
        Result := 'BIT';
    tkInt64:
      if field.Precision > 0 then
        Result := Format('NUMERIC(%0:d, %1:d)', [field.Precision, field.Scale])
      else
        Result := 'BIGINT';
    tkChar: Result := Format('CHAR(%d)', [field.Length]);
    tkFloat:
      if typeInfo = System.TypeInfo(TDate) then
        Result := 'DATE'
      else if typeInfo = System.TypeInfo(TDateTime) then
        Result := 'TIMESTAMP'
      else if typeInfo = System.TypeInfo(TTime) then
        Result := 'TIME'
      else
        if field.Precision > 0 then
          Result := Format('NUMERIC(%0:d, %1:d)', [field.Precision, field.Scale])
        else
          Result := 'FLOAT';
    tkString, tkLString: Result := Format('VARCHAR(%d)', [field.Length]);
    tkClass, tkArray, tkDynArray, tkVariant: Result := 'BLOB';
    tkMethod: ;
    tkWChar: Result := Format('NCHAR(%d)', [field.Length]);
    tkWString, tkUString: Result := Format('NVARCHAR(%d)', [field.Length]);
    tkRecord:
      if IsNullable(typeInfo) or IsLazyType(typeInfo) then
      begin
        createField := field.Clone;
        try
          createField.TypeInfo := typeInfo.RttiType.GetGenericArguments[0].Handle;
          Result := GetSQLDataTypeName(createField);
        finally
          createField.Free;
        end;
      end;
    tkInterface: ;
    tkClassRef: ;
    tkPointer: ;
    tkProcedure: ;
  end;
end;

function TAnsiSQLGenerator.GetSQLSequenceCount(const sequenceName: string): string;
begin
  Result := '';
end;

function TAnsiSQLGenerator.GetSQLTableCount(const tableName: string): string;
begin
  Result := Format('SELECT COUNT(*) FROM %0:s %1:s', [tableName, GetSplitStatementSymbol]);
end;

function TAnsiSQLGenerator.GetSQLTableExists(const tableName: string): string;
begin
  Result := ''; //override to implement specific functionality
end;

function TAnsiSQLGenerator.GetTableColumns(const tableName: string): string;
begin
  Result := Format('SELECT * FROM %0:s WHERE 1<>2 %1:s', [tableName, GetSplitStatementSymbol]);
end;

function TAnsiSQLGenerator.GetTableNameWithAlias(const table: TSQLTable): string;
begin
  Result := table.Name + ' ' + table.Alias;
end;

function TAnsiSQLGenerator.GetTempTableName: string;
begin
  Result := TBL_TEMP;
end;

function TAnsiSQLGenerator.GetUpdateVersionFieldQuery(
  const command: TUpdateCommand; const versionColumn: VersionAttribute;
  const version, primaryKey: Variant): Variant;
begin
  Result := Format('UPDATE %0:s SET %1:s = coalesce(%1:s,0) + 1 WHERE (%2:s = %3:s) AND (coalesce(%1:s,0) = %4:s)',
    [command.Table.Name, versionColumn.ColumnName,
    command.PrimaryKeyColumn.ColumnName, VarToStr(primaryKey), VarToStr(version)]);
end;

function TAnsiSQLGenerator.GetWhereAsString(const whereFields: IList<TSQLWhereField>): string;
var
  i, index: Integer;
  field: TSQLWhereField;
begin
  Result := '';

  index := 0;
  for i := 0 to whereFields.Count - 1 do
  begin
    if i < index then
      Continue;

    index := i;

    field := whereFields[i];
    if i = 0 then
      Result := sLineBreak + '  WHERE '
    else
      Result := Result + ' AND ';

    if field.WhereOperator in StartOperators then
      index := FindEnd(whereFields, i, field.WhereOperator, GetEndOperator(field.WhereOperator));

    Result := Result + GenerateWhere(field);
    Inc(index);
  end;
end;

{$ENDREGION}


end.
