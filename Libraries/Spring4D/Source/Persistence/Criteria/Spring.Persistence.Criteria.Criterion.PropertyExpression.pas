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

unit Spring.Persistence.Criteria.Criterion.PropertyExpression;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Criteria.Criterion.Abstract,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TPropertyExpression = class(TAbstractCriterion)
  private
    fOperator: TWhereOperator;
    fPropertyName: string;
    fOtherPropertyName: string;
    fTable: TSQLTable;
    fOtherTable: TSQLTable;
  protected
    function GetWhereOperator: TWhereOperator; override;
    function ToSqlString(const params: IList<TDBParam>;
      const command: TWhereCommand; const generator: ISQLGenerator;
      addToCommand: Boolean): string; override;
  public
    constructor Create(const propertyName, otherPropertyName: string;
      whereOperator: TWhereOperator; const table: TSQLTable = nil;
      const otherTable: TSQLTable = nil); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;


{$REGION 'TPropertyExpression'}

constructor TPropertyExpression.Create(
  const propertyName, otherPropertyName: string;
  whereOperator: TWhereOperator; const table, otherTable: TSQLTable);
begin
  inherited Create;
  fPropertyName := propertyName;
  fOtherPropertyName := otherPropertyName;
  fOperator := whereOperator;
  fTable := table;
  fOtherTable := otherTable;
end;

destructor TPropertyExpression.Destroy;
begin
  fTable.Free;
  fOtherTable.Free;
  inherited Destroy;
end;

function TPropertyExpression.GetWhereOperator: TWhereOperator;
begin
  Result := fOperator;
end;

function TPropertyExpression.ToSqlString(const params: IList<TDBParam>;
  const command: TWhereCommand; const generator: ISQLGenerator;
  addToCommand: Boolean): string;
var
  whereField: TSQLWherePropertyField;
  table, otherTable: TSQLTable;
begin
  table := GetCriterionTable(command, fTable);
  otherTable := GetCriterionTable(command, fOtherTable);

  if not Assigned(table) then
    table := command.Table;

  if not Assigned(otherTable) then
    otherTable := command.Table;

  whereField := TSQLWherePropertyField.Create(
    fPropertyName, fOtherPropertyName, table, otherTable);
  whereField.WhereOperator := WhereOperator;

  Result := generator.GenerateWhere(whereField);

  if addToCommand then
    command.WhereFields.Add(whereField)
  else
    whereField.Free;
end;

{$ENDREGION}


end.
