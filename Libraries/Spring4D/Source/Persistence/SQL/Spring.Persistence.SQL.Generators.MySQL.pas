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

unit Spring.Persistence.SQL.Generators.MySQL;

interface

uses
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Represents <b>MySQL</b> SQL generator.
  /// </summary>
  TMySQLGenerator = class(TAnsiSQLGenerator)
  protected
    function DoGenerateBackupTable(const tableName: string): TArray<string>; override;
    function DoGenerateCreateTable(const tableName: string;
      const columns: IList<TSQLCreateField>): string; override;
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateCreateSequence(const command: TCreateSequenceCommand): string; override;
    function GenerateGetLastInsertId(const identityColumn: ColumnAttribute): string; override;
    function GenerateGetNextSequenceValue(const sequence: SequenceAttribute): string; override;
    function GetSQLDataTypeName(const field: TSQLCreateField): string; override;
    function GetEscapeChar: Char; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  Spring.Persistence.SQL.Register;


{$REGION 'TMySQLGenerator'}

function TMySQLGenerator.DoGenerateBackupTable(
  const tableName: string): TArray<string>;
begin
  Result := DoGenerateBackupTableUsingCreate(tableName);
end;

function TMySQLGenerator.DoGenerateCreateTable(const tableName: string;
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

      sqlBuilder.AppendFormat('%0:s %1:s %2:s %3:s %4:s', [
        GetEscapedFieldName(field),
        GetSQLDataTypeName(field),
        IfThen(cpNotNull in field.Properties, 'NOT NULL', 'NULL'),
        IfThen(field.IsIdentity, 'AUTO_INCREMENT'),
        IfThen(cpPrimaryKey in field.Properties, GetPrimaryKeyDefinition(field))]);
    end;

    sqlBuilder.AppendLine.Append(')');

    Result := sqlBuilder.ToString;
  finally
    sqlBuilder.Free;
  end;
end;

function TMySQLGenerator.GenerateCreateSequence(
  const command: TCreateSequenceCommand): string;
begin
  Result := '';
end;

function TMySQLGenerator.GenerateGetLastInsertId(
  const identityColumn: ColumnAttribute): string;
begin
  // Workaround for LAST_INSERT_ID returning UNSIGNED BIGINT since MySQL 5.6.9
  // as this usually maps to field of ftFloat (at least it does in ZEOS)
  Result := 'SELECT CAST(LAST_INSERT_ID() as SIGNED INTEGER);';
end;

function TMySQLGenerator.GenerateGetNextSequenceValue(
  const sequence: SequenceAttribute): string;
begin
  Result := '';
end;

// Pü 2014-06-01:
// MySQL has the backtick as FieldEscape unless MySQL is set to Ansi mode
function TMySQLGenerator.GetEscapeChar: Char;
begin
  Result := '`';
end;

function TMySQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlMySQL;
end;

function TMySQLGenerator.GetSQLDataTypeName(const field: TSQLCreateField): string;
begin
  Result := inherited GetSQLDataTypeName(field);
  if StartsText('NUMERIC', Result) then
    Result := 'DECIMAL' + Copy(Result, 8, Length(Result))
  else if StartsText('NCHAR', Result) then
    Result := Copy(Result, 2, Length(Result)) + ' CHARACTER SET ucs2'
  else if StartsText('NVARCHAR', Result) then
    Result := Copy(Result, 2, Length(Result)) + ' CHARACTER SET ucs2';
end;

{$ENDREGION}


initialization
  TSQLGeneratorRegister.RegisterGenerator(TMySQLGenerator.Create);

end.
