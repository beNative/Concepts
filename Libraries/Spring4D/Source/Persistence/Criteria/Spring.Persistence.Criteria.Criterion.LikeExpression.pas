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

unit Spring.Persistence.Criteria.Criterion.LikeExpression;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Criteria.Criterion.SimpleExpression,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TLikeExpression = class(TSimpleExpression)
  private
    fMatchMode: TMatchMode;
    fIgnoreCase: Boolean;
  protected
    function ToSqlString(const params: IList<TDBParam>;
      const command: TWhereCommand; const generator: ISQLGenerator;
      addToCommand: Boolean): string; override;
  public
    constructor Create(const propertyName: string; const value: TValue;
      whereOperator: TWhereOperator; matchMode: TMatchMode;
      ignoreCase: Boolean); reintroduce; overload;
  end;

implementation


{$REGION 'TLikeExpression'}

constructor TLikeExpression.Create(const propertyName: string;
  const value: TValue; whereOperator: TWhereOperator; matchMode: TMatchMode;
  ignoreCase: Boolean);
begin
  inherited Create(propertyName, value, whereOperator);
  fMatchMode := matchMode;
  fIgnoreCase := ignoreCase;
end;

function TLikeExpression.ToSqlString(const params: IList<TDBParam>;
  const command: TWhereCommand; const generator: ISQLGenerator;
  addToCommand: Boolean): string;
var
  whereField: TSQLWhereField;
begin
  whereField := TSQLWhereField.Create(PropertyName, GetCriterionTable(command), fIgnoreCase);
  whereField.WhereOperator := WhereOperator;
  whereField.RightSQL := GetMatchModeString(fMatchMode, Value.AsString, fIgnoreCase);

  Result := generator.GenerateWhere(whereField);

  if addToCommand then
    command.WhereFields.Add(whereField)
  else
    whereField.Free;
end;

{$ENDREGION}


end.
