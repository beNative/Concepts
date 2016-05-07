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

unit Spring.Persistence.SQL.Interfaces;

interface

uses
  Classes,
  Rtti,
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Types,
  Spring.Persistence.SQL.Params;

const
  qlAnsiSQL     = 'QUERY_LANGUAGE_ANSI';
  qlSQLite      = 'QUERY_LANGUAGE_SQLITE';
  qlMSSQL       = 'QUERY_LANGUAGE_MSSQL';
  qlASA         = 'QUERY_LANGUAGE_ASA';
  qlOracle      = 'QUERY_LANGUAGE_ORACLE';
  qlFirebird    = 'QUERY_LANGUAGE_FIREBIRD';
  qlPostgreSQL  = 'QUERY_LANGUAGE_POSTGRESQL';
  qlMySQL       = 'QUERY_LANGUAGE_MYSQL';
  qlNoSQL       = 'QUERY_LANGUAGE_NOSQL';
  qlMongoDB     = 'QUERY_LANGUAGE_MONGODB';

const
  SQL_BEGIN_TRAN = 'BEGIN TRANSACTION ';
  SQL_BEGIN_SAVEPOINT = 'SAVEPOINT ';
  SQL_COMMIT_TRAN = 'COMMIT TRANSACTION ';
  SQL_ROLLBACK_TRAN = 'ROLLBACK TRANSACTION ';
  SQL_ROLLBACK_SAVEPOINT = 'ROLLBACK TO SAVEPOINT ';

type
  TQueryLanguage = type string;

  ICommandExecutionListener = interface
    ['{590E86C8-0B05-4BFE-9B26-3A9A4D0510BF}']
    procedure ExecutingCommand(const command: string; const list: IList);
  end;

  ISQLGenerator = interface
    ['{8F46D275-50E4-4DE8-9E56-7D6599935E32}']
    function GetQueryLanguage: TQueryLanguage;
    function GenerateWhere(const field: TSQLWhereField): string;
    function GenerateSelect(const command: TSelectCommand): string;
    function GenerateInsert(const command: TInsertCommand): string;
    function GenerateUpdate(const command: TUpdateCommand): string;
    function GenerateDelete(const command: TDeleteCommand): string;
    function GenerateCreateTable(const command: TCreateTableCommand): IList<string>;
    function GenerateCreateForeignKey(const command: TCreateForeignKeyCommand): IList<string>;
    function GenerateCreateSequence(const command: TCreateSequenceCommand): string;
    function GenerateGetNextSequenceValue(const sequence: SequenceAttribute): string;
    function GenerateGetLastInsertId(const identityColumn: ColumnAttribute): string;
    function GeneratePagedQuery(const sql: string; limit, offset: Integer): string;
    function GenerateGetQueryCount(const sql: string): string;
    function GenerateUniqueId: Variant;
    function GetSQLTableCount(const tablename: string): string;
    function GetSQLSequenceCount(const sequenceName: string): string;
    function GetTableColumns(const tableName: string): string;
    function GetSQLTableExists(const tableName: string): string;
    function GetEscapeChar: Char;
    function GetUpdateVersionFieldQuery(const command: TUpdateCommand;
      const versionColumn: VersionAttribute; const version, primaryKey: Variant): Variant;
    function GetParamClass: TDBParamClass;
    function CreateParam(const paramField: TSQLParamField; const value: TValue): TDBParam;

    property QueryLanguage: TQueryLanguage read GetQueryLanguage;
  end;

implementation

uses
  // auto register all generators
  Spring.Persistence.SQL.Generators.Register;

end.
