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

unit Spring.Persistence.SQL.Generators.PostgreSQL;

interface

uses
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Represents <b>PostgreSQL</b> SQL generator.
  /// </summary>
  TPostgreSQLGenerator = class(TAnsiSQLGenerator)
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateCreateSequence(const command: TCreateSequenceCommand): string; override;
    function GenerateGetLastInsertId(const identityColumn: ColumnAttribute): string; override;
    function GenerateGetNextSequenceValue(const sequence: SequenceAttribute): string; override;
    function GetSQLDataTypeName(const field: TSQLCreateField): string; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  Spring.Persistence.SQL.Register;


{$REGION 'TPostgreSQLGenerator'}

function TPostgreSQLGenerator.GenerateCreateSequence(
  const command: TCreateSequenceCommand): string;
var
  sequence: SequenceAttribute;
begin
  sequence := command.Sequence;

  Result := Format('DROP SEQUENCE IF EXISTS %0:S; ', [sequence.SequenceName]);
  Result := Result + Format('CREATE SEQUENCE %0:S INCREMENT %1:D MINVALUE %2:D;',
    [sequence.SequenceName, sequence.Increment, sequence.InitialValue]);
end;

function TPostgreSQLGenerator.GenerateGetLastInsertId(
  const identityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

function TPostgreSQLGenerator.GenerateGetNextSequenceValue(
  const sequence: SequenceAttribute): string;
begin
  Result := Format('SELECT nextval(%0:S);', [QuotedStr(sequence.SequenceName)]);
end;

function TPostgreSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlPostgreSQL;
end;

function TPostgreSQLGenerator.GetSQLDataTypeName(
  const field: TSQLCreateField): string;
begin
  Result := inherited GetSQLDataTypeName(field);
  if Result = 'FLOAT' then
    Result := 'DOUBLE PRECISION'
  else if StartsText('NCHAR', Result) then
    Result := Copy(Result, 2, Length(Result))
  else if StartsText('NVARCHAR', Result) then
    Result := Copy(Result, 2, Length(Result))
  else if Result = 'BLOB' then
    Result := 'BYTEA'
  else if Result = 'BIT' then
    Result := 'BOOLEAN';
end;

{$ENDREGION}


initialization
  TSQLGeneratorRegister.RegisterGenerator(TPostgreSQLGenerator.Create);

end.
