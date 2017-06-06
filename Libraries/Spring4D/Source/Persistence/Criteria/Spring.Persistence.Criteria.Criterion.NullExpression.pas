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

unit Spring.Persistence.Criteria.Criterion.NullExpression;

interface

uses
  Spring.Collections,
  Spring.Persistence.Criteria.Criterion.Abstract,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TNullExpression = class(TAbstractCriterion)
  private
    fPropertyName: string;
    fOperator: TWhereOperator;
  protected
    function GetWhereOperator: TWhereOperator; override;
    function ToSqlString(const params: IList<TDBParam>;
      const command: TWhereCommand; const generator: ISQLGenerator;
      addToCommand: Boolean): string; override;
  public
    constructor Create(const propertyName: string; whereOperator: TWhereOperator); virtual;
  end;

implementation


{$REGION 'TNullExpression'}

constructor TNullExpression.Create(const propertyName: string;
  whereOperator: TWhereOperator);
begin
  inherited Create;
  fPropertyName := propertyName;
  fOperator := whereOperator;
end;

function TNullExpression.GetWhereOperator: TWhereOperator;
begin
  Result := fOperator;
end;

function TNullExpression.ToSqlString(const params: IList<TDBParam>;
  const command: TWhereCommand; const generator: ISQLGenerator;
  addToCommand: Boolean): string;
var
  whereField: TSQLWhereField;
begin
  whereField := TSQLWhereField.Create(fPropertyName, GetCriterionTable(command));
  whereField.WhereOperator := WhereOperator;

  Result := generator.GenerateWhere(whereField);

  if addToCommand then
    command.WhereFields.Add(whereField)
  else
    whereField.Free;
end;

{$ENDREGION}


end.
