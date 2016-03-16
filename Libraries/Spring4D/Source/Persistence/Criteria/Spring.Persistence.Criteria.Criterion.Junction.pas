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

unit Spring.Persistence.Criteria.Criterion.Junction;

interface

uses
  Spring.Collections,
  Spring.Persistence.Criteria.Criterion.Abstract,
  Spring.Persistence.Criteria.Interfaces,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TJunction = class(TAbstractCriterion, IJunction)
  private
    fCriterions: IList<ICriterion>;
  protected
    function ToSqlString(const params: IList<TDBParam>;
      const command: TWhereCommand; const generator: ISQLGenerator;
      addToCommand: Boolean): string; override;
  public
    constructor Create; virtual;

    function Add(const criterion: ICriterion): IJunction;
  end;

implementation


{$REGION 'TJunction'}

constructor TJunction.Create;
begin
  inherited Create;
  fCriterions := TCollections.CreateList<ICriterion>;
end;

function TJunction.Add(const criterion: ICriterion): IJunction;
begin
  fCriterions.Add(criterion);
  Result := Self;
end;

function TJunction.ToSqlString(const params: IList<TDBParam>;
  const command: TWhereCommand; const generator: ISQLGenerator;
  addToCommand: Boolean): string;
var
  i: Integer;
  criterion: ICriterion;
  sql: string;
  whereField: TSQLWhereField;
begin
  Result := '';
  for i := 0 to fCriterions.Count - 1 do
  begin
    if i <> 0 then
      Result := Result + ' ' + WhereOperatorNames[WhereOperator] + ' ';

    criterion := fCriterions[i];
    sql := criterion.ToSqlString(params, command, generator, False);
    if criterion is TJunction then
      sql := '(' + sql + ')';
    Result := Result + sql;
  end;

  if addToCommand then
  begin
    whereField := TSQLWhereField.Create(Result, '');
    whereField.WhereOperator := woJunction;
    command.WhereFields.Add(whereField);
  end;
end;

{$ENDREGION}


end.
