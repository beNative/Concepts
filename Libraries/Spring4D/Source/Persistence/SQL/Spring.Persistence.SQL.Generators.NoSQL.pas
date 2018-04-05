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

unit Spring.Persistence.SQL.Generators.NoSQL;

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
  ///   Represents base <b>NoSQL</b> database statements generator.
  /// </summary>
  TNoSQLGenerator = class(TAbstractSQLGenerator)
  public
    function GetQueryLanguage: TQueryLanguage; override;

    function GenerateCreateTable(const command: TCreateTableCommand): IList<string>; override;
    function GenerateCreateForeignKey(const command: TCreateForeignKeyCommand): IList<string>; override;
    function GenerateCreateSequence(const command: TCreateSequenceCommand): string; override;
    function GenerateGetNextSequenceValue(const sequence: SequenceAttribute): string; override;
    function GenerateGetLastInsertId(const identityColumn: ColumnAttribute): string; override;

    function GetSQLSequenceCount(const sequenceName: string): string; override;
    function GetTableColumns(const tableName: string): string; override;
    function GetSQLDataTypeName(const field: TSQLCreateField): string; override;
    function GetSQLTableExists(const tableName: string): string; override;
    function GetEscapeChar: Char; override;
  end;

implementation

{ TNoSQLGenerator }

function TNoSQLGenerator.GenerateCreateForeignKey(
  const command: TCreateForeignKeyCommand): IList<string>;
begin
  Result := TCollections.CreateList<string>;
end;

function TNoSQLGenerator.GenerateCreateSequence(
  const command: TCreateSequenceCommand): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GenerateCreateTable(
  const command: TCreateTableCommand): IList<string>;
begin
  Result := TCollections.CreateList<string>;
end;

function TNoSQLGenerator.GenerateGetLastInsertId(
  const identityColumn: ColumnAttribute): string;
begin
  Result := ' ';
end;

function TNoSQLGenerator.GenerateGetNextSequenceValue(
  const sequence: SequenceAttribute): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GetEscapeChar: Char;
begin
  Result := '"';
end;

function TNoSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlNoSQL;
end;

function TNoSQLGenerator.GetSQLDataTypeName(const field: TSQLCreateField): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GetSQLSequenceCount(const sequenceName: string): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GetSQLTableExists(const tableName: string): string;
begin
  Result := '';
end;

function TNoSQLGenerator.GetTableColumns(const tableName: string): string;
begin
  Result := '';
end;

end.
