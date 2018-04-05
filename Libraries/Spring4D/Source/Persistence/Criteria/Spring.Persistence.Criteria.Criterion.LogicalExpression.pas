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

unit Spring.Persistence.Criteria.Criterion.LogicalExpression;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Criteria.Criterion.Abstract,
  Spring.Persistence.Criteria.Interfaces,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TLogicalExpression = class(TAbstractCriterion)
  private
    fOperator: TWhereOperator;
    fLeft: ICriterion;
    fRight: ICriterion;
  protected
    function GetWhereOperator: TWhereOperator; override;
    function ToSqlString(const params: IList<TDBParam>;
      const command: TWhereCommand; const generator: ISQLGenerator;
      addToCommand: Boolean): string; override;
  public
    constructor Create(const left, right: ICriterion; whereOperator: TWhereOperator); virtual;
  end;

implementation


{$REGION 'TLogicalExpression'}

constructor TLogicalExpression.Create(const left, right: ICriterion;
  whereOperator: TWhereOperator);
begin
  inherited Create;
  fLeft := left;
  fRight := right;
  fOperator := whereOperator;
end;

function TLogicalExpression.GetWhereOperator: TWhereOperator;
begin
  Result := fOperator;
end;

function TLogicalExpression.ToSqlString(const params: IList<TDBParam>;
  const command: TWhereCommand; const generator: ISQLGenerator;
  addToCommand: Boolean): string;
var
  whereField, endOp: TSQLWhereField;
begin
  whereField := TSQLWhereField.Create('', '');
  whereField.WhereOperator := WhereOperator;
  if addToCommand then
    command.WhereFields.Add(whereField);
  whereField.LeftSQL := fLeft.ToSqlString(params, command, generator, addToCommand);
  if Assigned(fRight) then
    whereField.RightSQL := fRight.ToSqlString(params, command, generator, addToCommand);

  Result := generator.GenerateWhere(whereField);

  endOp := TSQLWhereField.Create('', '');
  endOp.WhereOperator := GetEndOperator(fOperator);
  if addToCommand then
    command.WhereFields.Add(endOp)
  else
  begin
    whereField.Free;
    endOp.Free;
  end;
end;

{$ENDREGION}


end.
