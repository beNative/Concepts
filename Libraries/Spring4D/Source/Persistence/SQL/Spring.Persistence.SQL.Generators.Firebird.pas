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
  sequence: SequenceAttribute;
begin
  sequence := command.Sequence;
  Result := Format('CREATE SEQUENCE %0:s;', [sequence.SequenceName]);
  if command.SequenceExists then
    Result := Format('DROP SEQUENCE %0:s;', [sequence.SequenceName]) + sLineBreak + Result;
end;

function TFirebirdSQLGenerator.GenerateGetLastInsertId(
  const identityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

function TFirebirdSQLGenerator.GenerateGetNextSequenceValue(
  const sequence: SequenceAttribute): string;
begin
  Result := Format('SELECT NEXT VALUE FOR %0:s FROM RDB$DATABASE;', [sequence.SequenceName]);
end;

function TFirebirdSQLGenerator.GeneratePagedQuery(const sql: string;
  limit, offset: Integer): string;
var
  s: string;
begin
  s := sql;
  if EndsStr(';', s) then
    SetLength(s, Length(s) - 1);

  // Row numbers are 1-based
  // see http://www.firebirdsql.org/refdocs/langrefupd20-select.html#langrefupd20-select-rows
  Result := s + Format(' ROWS %0:d TO %1:d;', [offset + 1, offset + limit]);
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
  Result := Format('SELECT COUNT(*) FROM RDB$GENERATORS WHERE RDB$GENERATOR_NAME = %0:s ' +
    ' AND RDB$SYSTEM_FLAG = 0;', [QuotedStr(sequenceName)]);
end;

function TFirebirdSQLGenerator.GetSQLTableExists(const tableName: string): string;
begin
  Result := Format('SELECT COUNT(*) FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = %0:s ' +
    ' AND RDB$VIEW_BLR IS NULL AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0);', [
    QuotedStr(tableName)]);
end;

{$ENDREGION}


initialization
  TSQLGeneratorRegister.RegisterGenerator(TFirebirdSQLGenerator.Create);

end.
