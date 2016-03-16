{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2016 Spring4D Team                           }
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

unit Spring.Persistence.SQL.Generators.Firebird;

interface

uses
  Spring.Persistence.Mapping.Attributes,
  Spring.Collections,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Represents <b>Firebird/Interbase</b> SQL generator.
  /// </summary>
  TFirebirdSQLGenerator = class(TAnsiSQLGenerator)
  protected
    function DoGenerateBackupTable(const tableName: string): TArray<string>; override;
    function DoGenerateRestoreTable(const tableName: string;
      const createColumns: IList<TSQLCreateField>; const dbColumns: IList<string>): TArray<string>; override;
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateCreateSequence(const command: TCreateSequenceCommand): string; override;
    function GenerateGetLastInsertId(const identityColumn: ColumnAttribute): string; override;
    function GenerateGetNextSequenceValue(const sequence: SequenceAttribute): string; override;
    function GeneratePagedQuery(const sql: string; limit, offset: Integer): string; override;
    function GetSQLSequenceCount(const sequenceName: string): string; override;
    function GetSQLDataTypeName(const field: TSQLCreateField): string; override;
    function GetSQLTableExists(const tableName: string): string; override;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.SQL.Register;


{$REGION 'TFirebirdSQLGenerator'}

function TFirebirdSQLGenerator.DoGenerateBackupTable(
  const tableName: string): TArray<string>;
begin
  raise EORMUnsupportedOperation.CreateFmt('Firebird does not support copying table %s.', [tableName]);
end;

function TFirebirdSQLGenerator.DoGenerateRestoreTable(const tableName: string;
  const createColumns: IList<TSQLCreateField>;
  const dbColumns: IList<string>): TArray<string>;
begin
  raise EORMUnsupportedOperation.CreateFmt('Firebird does not support copying table %s', [tableName]);
end;

function TFirebirdSQLGenerator.GenerateCreateSequence(
  const command: TCreateSequenceCommand): string;
var
  LSequence: SequenceAttribute;
begin
  LSequence := command.Sequence;
  Result := '';
  if command.SequenceExists then
    Result := Format('DROP SEQUENCE %0:S; ', [LSequence.SequenceName]);

  Result := Result + Format('CREATE SEQUENCE %0:S;', [LSequence.SequenceName]);
end;

function TFirebirdSQLGenerator.GenerateGetLastInsertId(
  const identityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

function TFirebirdSQLGenerator.GenerateGetNextSequenceValue(
  const sequence: SequenceAttribute): string;
begin
  Assert(Assigned(sequence));
  Result := Format('SELECT NEXT VALUE FOR %0:S FROM RDB$DATABASE;', [sequence.SequenceName]);
end;

function TFirebirdSQLGenerator.GeneratePagedQuery(const sql: string;
  limit, offset: Integer): string;
var
  LSQL: string;
begin
  LSQL := sql;
  if EndsStr(';', LSQL) then
    SetLength(LSQL, Length(LSQL)-1);

  Result := LSQL + Format(' ROWS %1:D TO %0:D;', [offset + limit, offset]);
end;

function TFirebirdSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlFirebird;
end;

function TFirebirdSQLGenerator.GetSQLDataTypeName(
  const field: TSQLCreateField): string;
begin
  Result := inherited GetSQLDataTypeName(field);
  if StartsText(Result, 'NCHAR') then
    Result := Copy(Result, 2, Length(Result)) + ' CHARACTER SET UNICODE_FSS'
  else if StartsText(Result, 'NVARCHAR') then
    Result := Copy(Result, 2, Length(Result)) + ' CHARACTER SET UNICODE_FSS';
end;

function TFirebirdSQLGenerator.GetSQLSequenceCount(const sequenceName: string): string;
begin
  Result := Format('SELECT COUNT(*) '+
    'FROM RDB$GENERATORS '+
    'WHERE (RDB$SYSTEM_FLAG=0) AND (RDB$GENERATOR_NAME = %0:S); ', [QuotedStr(sequenceName)]);
end;

function TFirebirdSQLGenerator.GetSQLTableExists(const tableName: string): string;
begin
  Result := Format('select count(*) from rdb$relations where rdb$relation_name = %0:S '+
    ' and rdb$view_blr is null and (rdb$system_flag is null or rdb$system_flag = 0)'
    , [QuotedStr(tableName)]);
end;

{$ENDREGION}


initialization
  TSQLGeneratorRegister.RegisterGenerator(TFirebirdSQLGenerator.Create);

end.
