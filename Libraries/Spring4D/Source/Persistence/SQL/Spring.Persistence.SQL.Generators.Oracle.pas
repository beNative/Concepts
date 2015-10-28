{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

unit Spring.Persistence.SQL.Generators.Oracle;

interface

uses
  Rtti,
  TypInfo,
  DB,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types,
  Spring.Persistence.SQL.Params;

type
  /// <summary>
  ///   Represents <b>Oracle</b> SQL generator.
  /// </summary>
  TOracleSQLGenerator = class(TAnsiSQLGenerator)
  protected
    function GetSplitStatementSymbol: string; override;
  public
    function DoGenerateBackupTable(const tableName: string): TArray<string>; override;
    function GenerateGetQueryCount(const sql: string): string; override;
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateCreateSequence(const command: TCreateSequenceCommand): string; override;
    function GenerateGetLastInsertId(const identityColumn: ColumnAttribute): string; override;
    function GenerateGetNextSequenceValue(const sequence: SequenceAttribute): string; override;
    function GeneratePagedQuery(const sql: string; limit, offset: Integer): string; override;
    function GetSQLSequenceCount(const sequenceName: string): string; override;
    function GetSQLDataTypeName(const field: TSQLCreateField): string; override;
    function GetSQLTableExists(const tableName: string): string; override;

    function GetParamClass: TDBParamClass; override;
  end;

  TOracleDBParam = class(TDBParam)
  protected
    function TypeInfoToFieldType(typeInfo: PTypeInfo): TFieldType; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  Variants,
  Spring.Persistence.SQL.Register;


{$REGION 'TOracleSQLGenerator'}

function TOracleSQLGenerator.DoGenerateBackupTable(const tableName: string): TArray<string>;
begin
  Result := DoGenerateBackupTableUsingCreate(tableName);
end;

function TOracleSQLGenerator.GenerateCreateSequence(
  const command: TCreateSequenceCommand): string;
var
  LSequence: SequenceAttribute;
begin
  LSequence := command.Sequence;
  Result := 'BEGIN ';
  if command.SequenceExists then
    Result := Result + 'EXECUTE IMMEDIATE ' + QuotedStr(Format('DROP SEQUENCE "%0:S" ', [LSequence.SequenceName])) + ';';

  Result := Result + ' EXECUTE IMMEDIATE ' + QuotedStr(Format('CREATE SEQUENCE "%0:S" '+
    ' MINVALUE 1 MAXVALUE 9999999999999999999999999999 INCREMENT BY %2:D START WITH %1:D CACHE 20 NOORDER NOCYCLE',
    [LSequence.SequenceName, LSequence.InitialValue, LSequence.Increment])) + ';';

  Result := Result + ' END;';
end;

function TOracleSQLGenerator.GenerateGetLastInsertId(
  const identityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

function TOracleSQLGenerator.GenerateGetNextSequenceValue(
  const sequence: SequenceAttribute): string;
begin
  Assert(Assigned(sequence));
  Result := Format('select %0:S.nextval from dual', [sequence.SequenceName]);
end;

function TOracleSQLGenerator.GenerateGetQueryCount(const sql: string): string;
var
  LBuilder: TStringBuilder;
  LSQL: string;
begin
  LBuilder := TStringBuilder.Create;
  try
    LSQL := sql;
    if EndsStr(';', LSQL) then
      SetLength(LSQL, Length(LSQL)-1);

    LBuilder.Append('SELECT COUNT(*) FROM (')
      .AppendLine
      .Append(LSQL)
      .AppendLine
      .Append(')').Append(GetSplitStatementSymbol)
      ;

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TOracleSQLGenerator.GeneratePagedQuery(const sql: string;
  limit, offset: Integer): string;
var
  LBuilder: TStringBuilder;
  LSQL: string;
begin
  LBuilder := TStringBuilder.Create;
  LSQL := sql;
  try
    if EndsStr(';', LSQL) then
      SetLength(LSQL, Length(LSQL)-1);

    LBuilder.Append('SELECT * FROM (')
      .AppendLine.Append(' SELECT AROWNUM.*, ROWNUM r___  FROM (  ')
      .Append(LSQL)
      .Append(') AROWNUM ')
      .AppendFormat('WHERE ROWNUM < (%0:D+%1:D) )', [offset+1, limit])
      .AppendLine
      .AppendFormat(' WHERE r___ > = %0:D', [offset+1]);

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TOracleSQLGenerator.GetParamClass: TDBParamClass;
begin
  Result := TOracleDBParam;
end;

function TOracleSQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlOracle;
end;

function TOracleSQLGenerator.GetSplitStatementSymbol: string;
begin
  Result := '';
end;

function TOracleSQLGenerator.GetSQLDataTypeName(
  const field: TSQLCreateField): string;
begin
  Result := inherited GetSQLDataTypeName(field);
  if StartsText('NUMERIC', Result) then
    Result := 'NUMBER' + Copy(Result, 8, Length(Result))
  else if StartsText('NVARCHAR', Result) and (not StartsText('NVARCHAR2', Result)) then
    Result := 'NVARCHAR2' + Copy(Result, 9, Length(Result))
  else if StartsText('VARCHAR', Result) and (not StartsText('VARCHAR2', Result)) then
    Result := 'VARCHAR2' + Copy(Result, 8, Length(Result))
  else if Result = 'BIT' then
    Result := 'SMALLINT' // 'PLS_INTEGER'
  else if Result = 'FLOAT' then
    Result := 'BINARY_DOUBLE';
end;

function TOracleSQLGenerator.GetSQLSequenceCount(const sequenceName: string): string;
begin
  Result := Format('SELECT COUNT(*) FROM USER_SEQUENCES WHERE SEQUENCE_NAME = %0:S ',
    [QuotedStr(sequenceName)]);
end;

function TOracleSQLGenerator.GetSQLTableExists(const tableName: string): string;
var
  LSchema, LTable: string;
begin
  ParseFullTablename(tableName, LTable, LSchema);
  if (LSchema <> '') then
    Result := Format('SELECT COUNT(*) FROM ALL_OBJECTS WHERE OBJECT_TYPE = (''TABLE'') AND UPPER(OBJECT_NAME) = %0:S AND UPPER(OWNER) = %1:S'
    , [QuotedStr(UpperCase(LTable)), QuotedStr(UpperCase(LSchema))])
  else
    Result := Format('SELECT COUNT(*) FROM ALL_OBJECTS WHERE OBJECT_TYPE = (''TABLE'') AND UPPER(OBJECT_NAME) = %0:S'
    , [QuotedStr(UpperCase(LTable))]);
end;

{$ENDREGION}


{$REGION 'TOracleDBParam'}

function TOracleDBParam.TypeInfoToFieldType(typeInfo: PTypeInfo): TFieldType;
begin
  Result := inherited TypeInfoToFieldType(typeInfo);
  if Assigned(typeInfo) then
    case typeInfo.Kind of
      tkClass, tkArray, tkDynArray, tkInterface:
        Result := ftOraBlob;
    end;
end;

{$ENDREGION}


initialization
  TSQLGeneratorRegister.RegisterGenerator(TOracleSQLGenerator.Create);

end.
