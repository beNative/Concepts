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

unit Spring.Persistence.Criteria.Criterion.BetweenExpression;

interface

uses
  Rtti,
  Spring.Collections,
  Spring.Persistence.Criteria.Criterion.Abstract,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TBetweenExpression = class(TAbstractCriterion)
  private
    fPropertyName: string;
    fLowValue: TValue;
    fOperator: TWhereOperator;
    fHighValue: TValue;
  protected
    function GetWhereOperator: TWhereOperator; override;
    function ToSqlString(const params: IList<TDBParam>;
      const command: TWhereCommand; const generator: ISQLGenerator;
      addToCommand: Boolean): string; override;
  public
    constructor Create(const propertyName: string;
      const lowValue, highValue: TValue; whereOperator: TWhereOperator); virtual;

    property PropertyName: string read fPropertyName;
    property LowValue: TValue read fLowValue;
    property HighValue: TValue read fHighValue write fHighValue;
  end;

implementation


{$REGION 'TBetweenExpression'}

constructor TBetweenExpression.Create(const propertyName: string;
  const lowValue, highValue: TValue; whereOperator: TWhereOperator);
begin
  inherited Create;
  fPropertyName := propertyName;
  fLowValue := lowValue;
  fHighValue := highValue;
  fOperator := whereOperator;
end;

function TBetweenExpression.GetWhereOperator: TWhereOperator;
begin
  Result := fOperator;
end;

function TBetweenExpression.ToSqlString(const params: IList<TDBParam>;
  const command: TWhereCommand; const generator: ISQLGenerator;
  addToCommand: Boolean): string;
var
  param: TDBParam;
  whereField: TSQLWhereField;
  paramName, paramName2: string;
begin
  paramName := command.GetAndIncParameterName(fPropertyName);
  paramName2 := command.GetAndIncParameterName(fPropertyName);
  whereField := TSQLWhereField.Create(fPropertyName, GetCriterionTable(command));
  whereField.WhereOperator := WhereOperator;
  whereField.ParamName := paramName;
  whereField.ParamName2 := paramName2;

  Result := generator.GenerateWhere(whereField);

  if addToCommand then
    command.WhereFields.Add(whereField)
  else
    whereField.Free;

  // 1st parameter Low
  param := generator.GetParamClass.Create(paramName, fLowValue);
  params.Add(param);
  // 2nd parameter High
  param := generator.GetParamClass.Create(paramName2, fHighValue);
  params.Add(param);
end;

{$ENDREGION}


end.
