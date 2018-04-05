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

unit Spring.Persistence.Core.Session;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.AbstractSession,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Criteria.Interfaces,
  Spring.Persistence.SQL.Params;

type
  /// <summary>
  ///   The main runtime class between an application and ORM. This is the
  ///   central API class abstracting the notion of a persistence service. The
  ///   main function of the Session is to offer create, read and delete
  ///   operations for instances of mapped entity classes. <see cref="Spring.Persistence.Core.Session|TSession.Insert(TObject)">
  ///   Insert</see> results in an SQL <c>insert</c>, <see cref="Spring.Persistence.Core.Session|TSession.Delete(TObject)">
  ///   Delete</see> in an SQL <c>delete</c> and <see cref="Spring.Persistence.Core.Session|TSession.Update(TObject)">
  ///   Update</see> in an SQL <c>update</c>. Changes to persistent instances
  ///   are detected at flush time and also result in an SQL <c>update</c>. <see cref="Spring.Persistence.Core.Session|TSession.Save(TObject)">
  ///   Save</see> results in either an <c>insert</c> or an <c>update</c>. It
  ///   is not intended that implementers be thread-safe. Instead each
  ///   thread/transaction should obtain its own instance from a
  ///   SessionFactory.
  /// </summary>
  TSession = class(TAbstractSession)
  protected
    function GetQueryCountSql(const sql: string): string;
    function GetQueryCount(const sql: string; const params: IEnumerable<TDBParam>): Int64;
  public
    /// <summary>
    ///   Starts a new List Session. ListSession monitors changes in the
    ///   specified list and can commit or rollback these changes to the
    ///   database
    /// </summary>
    /// <remarks>
    ///   Can return newly started list transaction interface which controls
    ///   how changes will be reflected in the database.
    /// </remarks>
    function BeginListSession<T: class, constructor>(
      const list: IList<T>): IListSession<T>;

    /// <summary>
    ///   Starts a new transaction.
    /// </summary>
    /// <remarks>
    ///   Can optionally return newly started transaction interface.
    /// </remarks>
    function BeginTransaction: IDBTransaction;

    /// <summary>
    ///   Create a new ICriteria&lt;T&gt; instance, for the given entity class,
    ///   or a superclass of an entity class.
    /// </summary>
    function CreateCriteria<T: class, constructor>: ICriteria<T>;

    /// <summary>
    ///   Create a new ICriteria&lt;T&gt; instance, for the given entity class
    ///   using expression to filter elements
    /// </summary>
    /// <example>
    ///   <code lang="Delphi">Age := GetProp(CUSTAGE);
    /// session.FindWhere&lt;TCustomer&gt;(Age = 10);</code>
    /// </example>
    function FindWhere<T:class, constructor>(const expression: ICriterion): IList<T>;

    /// <summary>
    ///   Executes sql statement which does not return resultset.
    /// </summary>
    function Execute(const sql: string; const params: array of TValue): NativeUInt;

    /// <summary>
    ///   <para>
    ///     Executes given sql statement and returns first column value. SQL
    ///     statement should be like this:
    ///   </para>
    ///   <code lang="Delphi">SELECT COUNT(*) FROM TABLE;</code>
    /// </summary>
    function ExecuteScalar<T>(const sql: string; const params: array of TValue): T;

    /// <summary>
    ///   Tries to retrieves first and only model from the sql statement. If
    ///   not succeeds, returns false.
    /// </summary>
    function TryFirst<T: class, constructor>(const sql: string;
      const params: array of TValue; out value: T): Boolean;

    /// <summary>
    ///   Retrieves first and only model from the sql statement. Raises an <c>
    ///   exception</c> if model does not exist.
    /// </summary>
    function First<T: class, constructor>(const sql: string;
      const params: array of TValue): T;

    /// <summary>
    ///   Retrieves first and only model or the default value if model does not
    ///   exist.
    /// </summary>
    function FirstOrDefault<T: class, constructor>(const sql: string;
      const params: array of TValue): T;

    /// <summary>
    ///   Retrieves only one entity model from the database. Raises an <c>
    ///   exception</c> if model does not exist.
    /// </summary>
    function Single<T: class, constructor>(const sql: string;
      const params: array of TValue): T;

    /// <summary>
    ///   Retrieves only one entity model from the database. Returns default
    ///   value if model does not exist.
    /// </summary>
    function SingleOrDefault<T: class, constructor>(const sql: string;
      const params: array of TValue): T;

    /// <summary>
    ///   Retrieves multiple models from the sql statement.
    /// </summary>
    function GetList<T: class, constructor>(const sql: string;
      const params: array of TValue): IList<T>; overload;

    /// <summary>
    ///   Retrieves multiple models from the sql statement.
    /// </summary>
    function GetList<T: class, constructor>(const sql: string;
      const params: IEnumerable<TDBParam>): IList<T>; overload;

    /// <summary>
    ///   Retrieves single model from the database based on its primary key
    ///   value. If record not found, nil is returned.
    /// </summary>
    function FindOne<T: class, constructor>(const id: TValue): T;

    /// <summary>
    ///   Retrieves all models from PODO database table.
    /// </summary>
    function FindAll<T: class, constructor>: IList<T>;

    /// <summary>
    ///   Inserts model to the database.
    /// </summary>
    procedure Insert(const entity: TObject);

    /// <summary>
    ///   Inserts model and its aggregations to the database.
    /// </summary>
    procedure InsertAll(const entity: TObject);

    /// <summary>
    ///   Inserts models to the database.
    /// </summary>
    procedure InsertList<T: class, constructor>(const entities: IEnumerable<T>);

    /// <summary>
    ///   Checks if given entity is newly created (does not exist in the
    ///   database yet).
    /// </summary>
    function IsNew(const entity: TObject): Boolean;

    /// <summary>
    ///   Updates model in the database.
    /// </summary>
    procedure Update(const entity: TObject);

    /// <summary>
    ///   Updates model and its aggregations in the database.
    /// </summary>
    procedure UpdateAll(const entity: TObject);

    /// <summary>
    ///   Updates multiple models in the database.
    /// </summary>
    procedure UpdateList<T: class, constructor>(const entities: IEnumerable<T>);

    /// <summary>
    ///   Removes model from the database.
    /// </summary>
    procedure Delete(const entity: TObject);

    /// <summary>
    ///   Removes models from the database.
    /// </summary>
    procedure DeleteList<T: class, constructor>(const entities: IEnumerable<T>);

    /// <summary>
    ///   Fetches data in pages. Pages are 1-indexed.
    /// </summary>
    function Page<T: class, constructor>(index, size: Integer): IDBPage<T>; overload;

    /// <summary>
    ///   Fetches data in pages. You do not need to write custom sql for this,
    ///   just use ordinary sql. All the work will be done for you. Pages are
    ///   1-indexed.
    /// </summary>
    function Page<T: class, constructor>(index, size: Integer;
      const sql: string; const params: array of TValue): IDBPage<T>; overload;

    /// <summary>
    ///   Fetches data in pages. You do not need to write custom sql for this,
    ///   just use ordinary sql. All the work will be done for you. Pages are
    ///   1-indexed.
    /// </summary>
    function Page<T: class, constructor>(index, size: Integer;
      const sql: string; const params: IEnumerable<TDBParam>): IDBPage<T>; overload;

    /// <summary>
    ///   Saves the entity to the database. It will do update or the insert
    ///   based on the entity state.
    /// </summary>
    procedure Save(const entity: TObject);

    /// <summary>
    ///   Saves the entity and all entities it contains to the database. It
    ///   will do update or the insert based on the entity state.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     Use with caution when inserting new entities containing identity
    ///     primary keys. If both base (main) and sub entities are newly
    ///     created then framework won't be able to resolve their
    ///     relationships because their primary keys aren't known at save
    ///     time.
    ///   </para>
    ///   <para>
    ///     Works best when entities are updated.
    ///   </para>
    /// </remarks>
    procedure SaveAll(const entity: TObject);

    /// <summary>
    ///   Saves entities to the database. It will do update or the insert based
    ///   on the entity state.
    /// </summary>
    procedure SaveList<T: class, constructor>(const entities: IEnumerable<T>);

    /// <summary>
    /// Registers IRowMapper<T> interface to use when mapping given class into result object
    /// </summary>
    procedure RegisterRowMapper<T: class, constructor>(const rowMapper: IRowMapper<T>);

    /// <summary>
    /// Unregisters IRowMapper<T> interface
    /// </summary>
    procedure UnregisterRowMapper<T: class, constructor>;
  end;

implementation

uses
  TypInfo,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.ListSession,
  Spring.Persistence.Core.ResourceStrings,
  Spring.Persistence.Criteria,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Register,
  Spring.Reflection,
  Spring.ResourceStrings;


{$REGION 'TSession'}

function TSession.BeginListSession<T>(const list: IList<T>): IListSession<T>;
begin
  Result := TListSession<T>.Create(Self, list);
end;

function TSession.BeginTransaction: IDBTransaction;
begin
  Result := Connection.BeginTransaction;
end;

function TSession.CreateCriteria<T>: ICriteria<T>;
begin
  Result := TCriteria<T>.Create(Self);
end;

procedure TSession.Delete(const entity: TObject);
var
  deleter: IDeleteCommand;
begin
  deleter := GetDeleteCommandExecutor(entity.ClassType);
  DoDelete(entity, deleter);
end;

procedure TSession.DeleteList<T>(const entities: IEnumerable<T>);
var
  deleter: IDeleteCommand;
  entity: T;
begin
  deleter := GetDeleteCommandExecutor(T);
  for entity in entities do
    DoDelete(entity, deleter);
end;

function TSession.Execute(const sql: string; const params: array of TValue): NativeUInt;
var
  statement: IDBStatement;
begin
  statement := Connection.CreateStatement;
  statement.SetSQLCommand(sql);
  statement.SetParams(TDBParams.Create(params));
  Result := statement.Execute;
end;

function TSession.ExecuteScalar<T>(const sql: string; const params: array of TValue): T;
var
  results: IDBResultSet;
  fieldValue: Variant;
  value, convertedValue: TValue;
begin
  results := GetResultSet(sql, TDBParams.Create(params));
  if not results.IsEmpty then
  begin
    fieldValue := results.GetFieldValue(0);

    value := TValue.FromVariant(fieldValue);
    if not value.TryConvert(TypeInfo(T), convertedValue) then
      raise EORMCannotConvertValue.CreateResFmt(@SIncompatibleTypes,
        [value.TypeInfo.Name, PTypeInfo(TypeInfo(T)).Name]);
    Result := convertedValue.AsType<T>;
  end
  else
    Result := Default(T);
end;

function TSession.FindAll<T>: IList<T>;
var
  selecter: ISelectCommand;
  results: IDBResultSet;
begin
  selecter := GetSelectCommandExecutor(T);
  results := selecter.SelectAll;
  Result := TCollections.CreateObjectList<T>(True);
  MapEntitiesFromResultSet(results, Result as IObjectList, T);
end;

function TSession.FindOne<T>(const id: TValue): T;
var
  selecter: ISelectCommand;
  results: IDBResultSet;
begin
  selecter := GetSelectByIdCommandExecutor(T, id);
  results := selecter.Select;
  if not results.IsEmpty then
    Result := T(MapEntityFromResultSetRow(results, T))
  else
    Result := Default(T);
end;

function TSession.FindWhere<T>(const expression: ICriterion): IList<T>;
begin
  Result := CreateCriteria<T>.Where(expression).ToList;
end;

function TSession.First<T>(const sql: string; const params: array of TValue): T;
begin
  if not TryFirst<T>(sql, params, Result) then
    raise EORMRecordNotFoundException.CreateRes(@SRecordNotFound);
end;

function TSession.FirstOrDefault<T>(const sql: string; const params: array of TValue): T;
begin
  if not TryFirst<T>(sql, params, Result) then
    Result := Default(T);
end;

function TSession.GetList<T>(const sql: string;
  const params: array of TValue): IList<T>;
begin
  Result := GetList<T>(sql, TDBParams.Create(params));
end;

function TSession.GetList<T>(const sql: string;
  const params: IEnumerable<TDBParam>): IList<T>;
begin
  Result := TCollections.CreateObjectList<T>(True);
  FetchFromCustomQuery(sql, params, Result as IObjectList, TClass(T));
end;

function TSession.GetQueryCount(const sql: string;
  const params: IEnumerable<TDBParam>): Int64;
var
  results: IDBResultSet;
begin
  results := GetResultSet(GetQueryCountSql(sql), params);
  if results.IsEmpty then
    Result := 0
  else
    Result := results.GetFieldValue(0);
end;

function TSession.GetQueryCountSql(const sql: string): string;
var
  generator: ISQLGenerator;
begin
  generator := TSQLGeneratorRegister.GetGenerator(Connection.QueryLanguage);
  Result := generator.GenerateGetQueryCount(sql);
end;

procedure TSession.Insert(const entity: TObject);
var
  inserter: IInsertCommand;
begin
  inserter := GetInsertCommandExecutor(entity.ClassType);
  DoInsert(entity, inserter);
end;

procedure TSession.InsertAll(const entity: TObject);
var
  entityData: TEntityData;
  relation: TObject;
begin
  entityData := TEntityCache.Get(entity.ClassType);
  entityData.GetRelations(entity, ManyToOneAttribute).ForEach(Insert);
  Insert(entity);

  for relation in entityData.GetRelations(entity, OneToManyAttribute) do
  begin
    UpdateForeignKeys(entity, relation);
    InsertAll(relation);
  end;
end;

procedure TSession.InsertList<T>(const entities: IEnumerable<T>);
var
  inserter: IInsertCommand;
  entity: T;
begin
  inserter := GetInsertCommandExecutor(T);
  for entity in entities do
    DoInsert(entity, inserter);
end;

function TSession.IsNew(const entity: TObject): Boolean;
begin
  Result := not OldStateEntities.IsMapped(entity);
end;

function TSession.Page<T>(index, size: Integer): IDBPage<T>;
begin
  Result := CreateCriteria<T>.Page(index, size);
end;

function TSession.Page<T>(index, size: Integer; const sql: string;
  const params: array of TValue): IDBPage<T>;
begin
  Result := Page<T>(index, size, sql, TDBParams.Create(params));
end;

function TSession.Page<T>(index, size: Integer; const sql: string;
  const params: IEnumerable<TDBParam>): IDBPage<T>;
var
  pager: TPager;
begin
  pager := TPager.Create(Connection, index, size);
  Result := TDriverPageAdapter<T>.Create(pager);
  pager.ItemCount := GetQueryCount(sql, params);
  FetchFromCustomQuery(pager.BuildSQL(sql), params, Result.Items as IObjectList, TClass(T));
end;

procedure TSession.RegisterRowMapper<T>(const rowMapper: IRowMapper<T>);
begin
  RegisterNonGenericRowMapper(T, rowMapper as IRowMapper);
end;

procedure TSession.Save(const entity: TObject);
begin
  if IsNew(entity) then
    Insert(entity)
  else
    Update(entity);
end;

procedure TSession.SaveAll(const entity: TObject);
var
  entityData: TEntityData;
  relation: TObject;
begin
  entityData := TEntityCache.Get(entity.ClassType);
  entityData.GetRelations(entity, ManyToOneAttribute).ForEach(Save);
  Save(entity);

  for relation in entityData.GetRelations(entity, OneToManyAttribute) do
  begin
    UpdateForeignKeys(entity, relation);
    SaveAll(relation);
  end;
end;

procedure TSession.SaveList<T>(const entities: IEnumerable<T>);
var
  inserts, updates: IList<T>;
  entity: T;
begin
  inserts := TCollections.CreateList<T>;
  updates := TCollections.CreateList<T>;
  for entity in entities do
    if IsNew(entity) then
      inserts.Add(entity)
    else
      updates.Add(entity);
  if inserts.Any then
    InsertList<T>(inserts);
  if updates.Any then
    UpdateList<T>(updates);
end;

function TSession.Single<T>(const sql: string; const params: array of TValue): T;
begin
  Result := First<T>(sql, params);
end;

function TSession.SingleOrDefault<T>(const sql: string; const params: array of TValue): T;
begin
  Result := FirstOrDefault<T>(sql, params);
end;

function TSession.TryFirst<T>(const sql: string; const params: array of TValue;
  out value: T): Boolean;
var
  results: IDBResultSet;
begin
  results := GetResultSet(sql, TDBParams.Create(params));
  Result := not results.IsEmpty;
  if Result then
    value := T(MapEntityFromResultSetRow(results, T))
  else
    value := Default(T);
end;

procedure TSession.UnregisterRowMapper<T>;
begin
  UnregisterNonGenericRowMapper(T);
end;

procedure TSession.Update(const entity: TObject);
var
  updater: IUpdateCommand;
begin
  updater := GetUpdateCommandExecutor(entity.ClassType);
  DoUpdate(entity, updater);
end;

procedure TSession.UpdateAll(const entity: TObject);
var
  entityData: TEntityData;
  relation: TObject;
begin
  entityData := TEntityCache.Get(entity.ClassType);
  entityData.GetRelations(entity, ManyToOneAttribute).ForEach(Update);
  Update(entity);

  for relation in entityData.GetRelations(entity, OneToManyAttribute) do
  begin
    UpdateForeignKeys(entity, relation);
    UpdateAll(relation);
  end;
end;

procedure TSession.UpdateList<T>(const entities: IEnumerable<T>);
var
  updater: IUpdateCommand;
  entity: T;
begin
  updater := GetUpdateCommandExecutor(T);
  for entity in entities do
    DoUpdate(entity, updater);
end;

{$ENDREGION}


end.
