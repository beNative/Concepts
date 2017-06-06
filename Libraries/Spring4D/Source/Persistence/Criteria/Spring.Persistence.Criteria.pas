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

unit Spring.Persistence.Criteria;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Session,
  Spring.Persistence.Criteria.Interfaces,
  Spring.Persistence.SQL.Params;

type
  /// <summary>
  ///   Implementation of <see cref="Spring.Persistence.Criteria.Interfaces|ICriteria&lt;T&gt;" />
  ///    interface.
  /// </summary>
  TCriteria<T: class, constructor> = class(TInterfacedObject, ICriteria<T>)
  private
    fCriterions: IList<ICriterion>;
    fOrderBy: IList<IOrderBy>;
    {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
    fSession: TSession;
    function GetCriterions: IReadOnlyList<ICriterion>;
  protected
    constructor Create(const session: TSession); virtual;

    function GenerateSqlStatement(const params: IList<TDBParam>): string;
    function Page(page, itemsPerPage: Integer): IDBPage<T>;
  public
    function Add(const criterion: ICriterion): ICriteria<T>;
    function Where(const criterion: ICriterion): ICriteria<T>;
    function OrderBy(const orderBy: IOrderBy): ICriteria<T>;

    procedure Clear;
    function ToList: IList<T>;

    property Criterions: IReadOnlyList<ICriterion> read GetCriterions;
  end;

implementation

uses
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Register,
  Spring.Persistence.SQL.Types;


{$REGION 'TCriteria<T>'}

constructor TCriteria<T>.Create(const session: TSession);
begin
  inherited Create;
  fSession := session;
  fCriterions := TCollections.CreateList<ICriterion>;
  fOrderBy := TCollections.CreateList<IOrderBy>;
end;

function TCriteria<T>.Add(const criterion: ICriterion): ICriteria<T>;
begin
  fCriterions.Add(criterion);
  Result := Self;
end;

procedure TCriteria<T>.Clear;
begin
  fCriterions.Clear;
  fOrderBy.Clear;
end;

function TCriteria<T>.GenerateSqlStatement(const params: IList<TDBParam>): string;
var
  criterion: ICriterion;
  orderField: TSQLOrderByField;
  orderBy: IOrderBy;
  command: TSelectCommand;
  generator: ISQLGenerator;
begin
  command := TSelectCommand.Create(TClass(T));
  generator := TSQLGeneratorRegister.GetGenerator(fSession.Connection.QueryLanguage);
  try
    for criterion in fCriterions do
      criterion.ToSqlString(params, command, generator, True);

    for orderBy in fOrderBy do
    begin
      orderField := TSQLOrderByField.Create(orderBy.GetPropertyName,
        command.FindTable(orderBy.GetEntityClass),
        orderBy.SortingDirection);
      command.OrderByFields.Add(orderField);
    end;

    Result := generator.GenerateSelect(command);
  finally
    command.Free;
  end;
end;

function TCriteria<T>.GetCriterions: IReadOnlyList<ICriterion>;
begin
  Result := fCriterions.AsReadOnlyList;
end;

function TCriteria<T>.OrderBy(const orderBy: IOrderBy): ICriteria<T>;
begin
  fOrderBy.Add(orderBy);
  Result := Self;
end;

function TCriteria<T>.Page(page, itemsPerPage: Integer): IDBPage<T>;
var
  params: IList<TDBParam>;
  sql: string;
begin
  params := TCollections.CreateObjectList<TDBParam>;
  sql := GenerateSqlStatement(params);
  Result := fSession.Page<T>(page, itemsPerPage, sql, params);
end;

function TCriteria<T>.ToList: IList<T>;
var
  params: IList<TDBParam>;
  sql: string;
begin
  params := TCollections.CreateObjectList<TDBParam>;
  sql := GenerateSqlStatement(params);
  Result := fSession.GetList<T>(sql, params);
end;

function TCriteria<T>.Where(const criterion: ICriterion): ICriteria<T>;
begin
  Result := Add(criterion);
end;

{$ENDREGION}


end.
