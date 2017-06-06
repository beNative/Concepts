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

unit Spring.Persistence.Criteria.Criterion.InExpression;

interface

uses
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Persistence.Criteria.Criterion.SimpleExpression,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  TInExpression = class(TSimpleExpression)
  private
    fValues: TArray<TValue>;
    fIgnoreCase: Boolean;
    function ValuesToSeparatedString: string;
  protected
    function ToSqlString(const params: IList<TDBParam>;
      const command: TWhereCommand; const generator: ISQLGenerator;
      addToCommand: Boolean): string; override;
  public
    constructor Create(const propertyName: string; const values: TArray<TValue>;
      whereOperator: TWhereOperator; ignoreCase: Boolean); reintroduce;
  end;

  TInExpression<T> = class(TInExpression)
  public
    constructor Create(const propertyName: string; const values: TArray<T>;
      whereOperator: TWhereOperator; caseSensitive: Boolean); reintroduce;
  end;

implementation

uses
  SysUtils,
  TypInfo;


{$REGION 'TInExpression'}

constructor TInExpression.Create(const propertyName: string;
  const values: TArray<TValue>; whereOperator: TWhereOperator;
  ignoreCase: Boolean);
begin
  inherited Create(propertyName, TValue.Empty, whereOperator);
  fValues := values;
  fIgnoreCase := ignoreCase;
end;

function TInExpression.ToSqlString(const params: IList<TDBParam>;
  const command: TWhereCommand; const generator: ISQLGenerator;
  addToCommand: Boolean): string;
var
  whereField: TSQLWhereField;
begin
  whereField := TSQLWhereField.Create(PropertyName, GetCriterionTable(command), fIgnoreCase);
  whereField.WhereOperator := WhereOperator;
  whereField.RightSQL := ValuesToSeparatedString;

  Result := generator.GenerateWhere(whereField);

  if addToCommand then
    command.WhereFields.Add(whereField)
  else
    whereField.Free;
end;

function TInExpression.ValuesToSeparatedString: string;
var
  i: Integer;
  value: TValue;
  s: string;
begin
  if fValues = nil then
    Exit('NULL');

  Result := '(';
  for i := Low(fValues) to High(fValues) do
  begin
    if i > 0 then
      Result := Result + ',';

    value := fValues[i];
    if value.IsString then
      s := QuotedStr(value.AsString)
    else
      s := value.ToString;
    if fIgnoreCase then
      s := AnsiUpperCase(s);
    Result := Result + s;
  end;
  Result := Result + ')';
end;

{$ENDREGION}


{$REGION 'TInExpression<T>'}

constructor TInExpression<T>.Create(const propertyName: string;
  const values: TArray<T>; whereOperator: TWhereOperator; caseSensitive: Boolean);
var
  i: Integer;
begin
  inherited Create(propertyName, nil, whereOperator, caseSensitive);
  SetLength(fValues, Length(values));
  for i := Low(values) to High(values) do
    fValues[i] := TValue.From<T>(values[i]);
end;

{$ENDREGION}


end.
