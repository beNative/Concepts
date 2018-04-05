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

unit Spring.Persistence.Criteria.Criterion.SimpleExpression;

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
  TSimpleExpression = class(TAbstractCriterion)
  private
    fPropertyName: string;
    fOperator: TWhereOperator;
    fValue: TValue;
  protected
    function GetWhereOperator: TWhereOperator; override;
    function ToSqlString(const params: IList<TDBParam>;
      const command: TWhereCommand; const generator: ISQLGenerator;
      addToCommand: Boolean): string; override;
  public
    constructor Create(const propertyName: string; const value: TValue;
      whereOperator: TWhereOperator); virtual;

    property PropertyName: string read fPropertyName;
    property Value: TValue read fValue;
  end;

implementation


{$REGION 'TSimpleExpression'}

constructor TSimpleExpression.Create(const propertyName: string;
  const value: TValue; whereOperator: TWhereOperator);
begin
  inherited Create;
  fPropertyName := propertyName;
  fValue := value;
  fOperator := whereOperator;
end;

function TSimpleExpression.GetWhereOperator: TWhereOperator;
begin
  Result := fOperator;
end;

function TSimpleExpression.ToSqlString(const params: IList<TDBParam>;
  const command: TWhereCommand; const generator: ISQLGenerator;
  addToCommand: Boolean): string;
var
  paramName: string;
  whereField: TSQLWhereField;
  param: TDBParam;
begin
  paramName := command.GetAndIncParameterName(fPropertyName);

  whereField := TSQLWhereField.Create(fPropertyName, GetCriterionTable(command));
  whereField.WhereOperator := WhereOperator;
  whereField.ParamName := paramName;

  Result := generator.GenerateWhere(whereField);

  param := generator.GetParamClass.Create(paramName, fValue);
  params.Add(param);

  if addToCommand then
    command.WhereFields.Add(whereField)
  else
    whereField.Free;
end;

{$ENDREGION}


end.
