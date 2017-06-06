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

unit Spring.Persistence.SQL.Generators.ASA;

interface

uses
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Represents <b>Sybase ASA</b> SQL generator.
  /// </summary>
  TASASQLGenerator = class(TAnsiSQLGenerator)
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateGetLastInsertId(const identityColumn: ColumnAttribute): string; override;
    function GeneratePagedQuery(const sql: string; limit, offset: Integer): string; override;
    function GetSQLDataTypeName(const field: TSQLCreateField): string; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  Spring.Persistence.SQL.Register;


{$REGION 'TASASQLGenerator'}

function TASASQLGenerator.GenerateGetLastInsertId(
  const identityColumn: ColumnAttribute): string;
begin
  Result := 'SELECT @@IDENTITY;';
end;

function TASASQLGenerator.GeneratePagedQuery(const sql: string;
  limit, offset: Integer): string;
var
  sqlBuilder: TStringBuilder;
  s: string;
begin
  sqlBuilder := TStringBuilder.Create;
  s := sql;
  try
    if EndsStr(';', s) then
      SetLength(s, Length(s) - 1);

    sqlBuilder.Append('SELECT * FROM (')
      .AppendLine
      .Append('  SELECT *, ROW_NUMBER() OVER (ORDER BY (NULL)) AS ORM_ROW_NUM FROM (')
      .AppendLine.Append('    ')
      .Append(s)
      .Append(') AS ORM_TOTAL_1')
      .AppendLine
      .Append('  ) AS ORM_TOTAL_2')
      .AppendLine
      .AppendFormat(' WHERE (ORM_ROW_NUM>%0:d) AND (ORM_ROW_NUM <= %0:d+%1:d);', [offset, limit]);

    Result := sqlBuilder.ToString;
  finally
    sqlBuilder.Free;
  end;
end;

function TASASQLGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlASA;
end;

function TASASQLGenerator.GetSQLDataTypeName(const field: TSQLCreateField): string;
begin
  Result := inherited GetSQLDataTypeName(field);
  if Result = 'BLOB' then
    Result := 'IMAGE'
  else if Result = 'TIMESTAMP' then
    Result := 'DATETIME';
end;

{$ENDREGION}


initialization
  TSQLGeneratorRegister.RegisterGenerator(TASASQLGenerator.Create);

end.
