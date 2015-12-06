{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2015 Spring4D Team                           }
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

unit Spring.Persistence.SQL.Generators.SQLite3;

interface

uses
  Generics.Collections,
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Represents <b>SQLite3</b> SQL generator.
  /// </summary>
  TSQLiteSQLGenerator = class(TAnsiSQLGenerator)
  private const
    TBL_TEMP = 'ORMTEMPTABLE';
  protected
    function DoGenerateBackupTable(const tableName: string): TArray<string>; override;
    function DoGenerateRestoreTable(const tableName: string;
      const createColumns: IList<TSQLCreateField>; const dbColumns: IList<string>): TArray<string>; override;
    function DoGenerateCreateTable(const tableName: string; const columns: IList<TSQLCreateField>): string; override;
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateCreateForeignKey(const command: TCreateForeignKeyCommand): IList<string>; override;
    function GenerateGetLastInsertId(const identityColumn: ColumnAttribute): string; override;
    function GetSQLDataTypeName(const field: TSQLCreateField): string; override;
    function GetSQLTableExists(const tableName: string): string; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  Spring,
  Spring.Persistence.SQL.Register;


{$REGION 'TSQLiteSQLGenerator'}

function TSQLiteSQLGenerator.DoGenerateBackupTable(const tableName: string): TArray<string>;
begin
  SetLength(Result, 3);
  Result[0] := Format(' DROP TABLE IF EXISTS %0:S ', [TBL_TEMP]);
  //select old data to temporary table
  Result[1] := Format(' CREATE TEMPORARY TABLE %0:S AS SELECT * FROM %1:S ',
    [TBL_TEMP, tableName]);
  //drop table
  Result[2] := Format(' DROP TABLE IF EXISTS %0:S ', [tableName]);
end;

function TSQLiteSQLGenerator.DoGenerateCreateTable(const tableName: string;
  const columns: IList<TSQLCreateField>): string;
var
  LSqlBuilder: TStringBuilder;
  i: Integer;
  LField: TSQLCreateField;
begin
  LSqlBuilder := TStringBuilder.Create;
  try
    LSqlBuilder.AppendFormat(' CREATE TABLE %0:S ', [tableName])
      .Append('(')
      .AppendLine;
    for i := 0 to columns.Count - 1 do
    begin
      LField := columns[i];
      if i > 0 then
        LSqlBuilder.Append(',').AppendLine;

      //0 - Column name, 1 - Column data type name, 2 - NOT NULL condition
      LSqlBuilder.AppendFormat(' %0:S %1:S %2:S %3:S %4:S %5:S',
        [
          LField.Name
          ,GetSQLDataTypeName(LField)
          ,IfThen(cpPrimaryKey in LField.Properties, 'PRIMARY KEY')
          ,IfThen(LField.IsIdentity, 'AUTOINCREMENT')
          ,IfThen(cpUnique in LField.Properties, 'UNIQUE')
          ,IfThen(cpNotNull in LField.Properties, 'NOT NULL', 'NULL')
        ]
      );
    end;
    LSqlBuilder.Append(')');

    Result := LSqlBuilder.ToString;
  finally
    LSqlBuilder.Free;
  end;
end;

function TSQLiteSQLGenerator.DoGenerateRestoreTable(const tableName: string;
  const createColumns: IList<TSQLCreateField>;
  const dbColumns: IList<string>): TArray<string>;
begin
  SetLength(Result, 2);
  Result[0] := Format(' INSERT INTO %0:S (%2:S) SELECT %3:S FROM %1:S',
    [tableName, TBL_TEMP, GetCreateFieldsAsString(createColumns),
    GetCopyFieldsAsString(createColumns, dbColumns)]);

  //drop temporary table
  Result[1] := Format(' DROP TABLE IF EXISTS %0:S', [TBL_TEMP]);
end;

function TSQLiteSQLGenerator.GenerateCreateForeignKey(
  const command: TCreateForeignKeyCommand): IList<string>;
var
  LSqlBuilder: TStringBuilder;
  LCreateTableString: string;
  i: Integer;
  LField: TSQLForeignKeyField;
  LRes: TArray<string>;
begin
  Assert(Assigned(command));
  Result := TCollections.CreateList<string>;

  if not command.ForeignKeys.Any then
    Exit;

  LSqlBuilder := TStringBuilder.Create;
  try
    LRes := DoGenerateBackupTable(command.Table.Name);
    Result.AddRange(LRes);
    //recreate table with foreign keys
    LCreateTableString := DoGenerateCreateTable(command.Table.Name, command.Columns);
    //remove ")" from the end of the string
    SetLength(LCreateTableString, Length(LCreateTableString)-1);

    LSqlBuilder.Append(LCreateTableString).Append(',').AppendLine;
    for i := 0 to command.ForeignKeys.Count - 1 do
    begin
      LField := command.ForeignKeys[i];
      if i > 0 then
        LSqlBuilder.Append(',').AppendLine;

      LSqlBuilder.AppendFormat(' CONSTRAINT %0:S FOREIGN KEY (%1:S) REFERENCES %2:S (%3:S)',
        [LField.ForeignKeyName, LField.Name, LField.ReferencedTableName, LField.ReferencedColumnName]);

    end;
    LSqlBuilder.Append(');');

    Result.Add(LSqlBuilder.ToString);

    LRes := DoGenerateRestoreTable(command.Table.Name, command.Columns, command.ColumnNames);
    Result.AddRange(LRes);
  finally
    LSqlBuilder.Free;
  end;
end;

function TSQLiteSQLGenerator.GenerateGetLastInsertId(
  const identityColumn: ColumnAttribute): string;
begin
  Result := 'SELECT last_insert_rowid();';
end;

function TSQLiteSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlSQLite;
end;

function TSQLiteSQLGenerator.GetSQLDataTypeName(
  const field: TSQLCreateField): string;
var
  typeInfo: PTypeInfo;
begin
  typeInfo := field.TypeInfo;
  if (typeInfo = System.TypeInfo(TDateTime))
    or (typeInfo = System.TypeInfo(TTime)) then
    Exit('DATETIME')
  else if (typeInfo = System.TypeInfo(TDate)) then
    Exit('DATE');

  Result := inherited GetSQLDataTypeName(field);
  if ContainsStr(Result, 'CHAR') then
    Result := 'TEXT';
end;

function TSQLiteSQLGenerator.GetSQLTableExists(const tableName: string): string;
begin
  Result := 'select count(*) from sqlite_master where [type] = ''table'' and lower([name]) = ''' +
    LowerCase(tableName) + ''' ';
end;

initialization
  TSQLGeneratorRegister.RegisterGenerator(TSQLiteSQLGenerator.Create);

end.
