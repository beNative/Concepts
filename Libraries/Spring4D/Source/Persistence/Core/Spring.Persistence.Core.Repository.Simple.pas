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

unit Spring.Persistence.Core.Repository.Simple;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Criteria.Interfaces,
  Spring.Persistence.Core.Session;

type
  TSimpleRepository<T: class, constructor; TID> = class(TInterfacedObject, IPagedRepository<T, TID>)
  private
    {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
    fSession: TSession;
    fNamespace: string;
  protected
    function GetNamespaceFromType: string; virtual;

    function Execute(const query: string; const params: array of TValue): NativeUInt; virtual;
    function Query(const query: string; const params: array of TValue): IList<T>; virtual;

    function Count: Integer; virtual;

    function FindOne(const id: TID): T; virtual;
    function FindAll: IList<T>; virtual;
    function FindWhere: ICriteria<T>; overload; virtual;
    function FindWhere(const expression: ICriterion): IList<T>; overload; virtual;

    function Save(const entity: T): T; overload; virtual;
    function Save(const entities: IEnumerable<T>): IEnumerable<T>; overload; virtual;
    procedure SaveCascade(const entity: T); virtual;

    procedure Insert(const entity: T); overload; virtual;
    procedure Insert(const entities: IEnumerable<T>); overload; virtual;

    procedure Delete(const entity: T); overload; virtual;
    procedure Delete(const entities: IEnumerable<T>); overload; virtual;
    procedure DeleteAll;

    function Page(page, itemsPerPage: Integer): IDBPage<T>; virtual;

    function Exists(const id: TID): Boolean; virtual;
  public
    constructor Create(const session: TSession); virtual;

    property Namespace: string read fNamespace;
  end;

implementation

uses
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Mapping.Attributes;


{$REGION 'TSimpleRepository<T, TID>'}

constructor TSimpleRepository<T, TID>.Create(const session: TSession);
begin
  inherited Create;
  fSession := session;
  fNamespace := GetNamespaceFromType;
end;

function TSimpleRepository<T, TID>.Count: Integer;
begin
  Result := fSession.Page<T>(1, 1).ItemCount;
end;

procedure TSimpleRepository<T, TID>.Delete(const entity: T);
begin
  fSession.Delete(entity);
end;

procedure TSimpleRepository<T, TID>.Delete(const entities: IEnumerable<T>);
var
  transaction: IDBTransaction;
begin
  transaction := fSession.BeginTransaction;
  fSession.DeleteList<T>(entities);
  transaction.Commit;
end;

procedure TSimpleRepository<T, TID>.DeleteAll;
begin
  Delete(FindAll);
end;

function TSimpleRepository<T, TID>.Execute(const query: string;
  const params: array of TValue): NativeUInt;
begin
  Result := fSession.Execute(query, params);
end;

function TSimpleRepository<T, TID>.Exists(const id: TID): Boolean;
var
  entity: T;
begin
  entity := FindOne(id);
  try
    Result := Assigned(entity);
  finally
    entity.Free;
  end;
end;

function TSimpleRepository<T, TID>.FindAll: IList<T>;
begin
  Result := fSession.FindAll<T>;
end;

function TSimpleRepository<T, TID>.FindOne(const id: TID): T;
begin
  Result := fSession.FindOne<T>(TValue.From<TID>(id));
end;

function TSimpleRepository<T, TID>.FindWhere: ICriteria<T>;
begin
  Result := fSession.CreateCriteria<T>;
end;

function TSimpleRepository<T, TID>.FindWhere(
  const expression: ICriterion): IList<T>;
begin
  Result := fSession.FindWhere<T>(expression);
end;

function TSimpleRepository<T, TID>.GetNamespaceFromType: string;
var
  table: TableAttribute;
begin
  table := TEntityCache.Get(T).EntityTable;
  if Assigned(table) then
    Exit(table.Namespace);
  Result := '';
end;

function TSimpleRepository<T, TID>.Query(const query: string;
  const params: array of TValue): IList<T>;
begin
  Result := fSession.GetList<T>(query, params);
end;

procedure TSimpleRepository<T, TID>.Insert(const entity: T);
begin
  fSession.Insert(entity);
end;

procedure TSimpleRepository<T, TID>.Insert(const entities: IEnumerable<T>);
var
  transaction: IDBTransaction;
begin
  transaction := fSession.BeginTransaction;
  fSession.InsertList<T>(entities);
  transaction.Commit;
end;

function TSimpleRepository<T, TID>.Page(page, itemsPerPage: Integer): IDBPage<T>;
begin
  Result := fSession.Page<T>(page, itemsPerPage);
end;

function TSimpleRepository<T, TID>.Save(const entity: T): T;
begin
  fSession.Save(entity);
  Result := entity;
end;

procedure TSimpleRepository<T, TID>.SaveCascade(const entity: T);
var
  transaction: IDBTransaction;
begin
  transaction := fSession.BeginTransaction;
  fSession.SaveAll(entity);
  transaction.Commit;
end;

function TSimpleRepository<T, TID>.Save(const entities: IEnumerable<T>): IEnumerable<T>;
var
  transaction: IDBTransaction;
begin
  transaction := fSession.BeginTransaction;
  fSession.SaveList<T>(entities);
  Result := entities;
  transaction.Commit;
end;

{$ENDREGION}


end.
