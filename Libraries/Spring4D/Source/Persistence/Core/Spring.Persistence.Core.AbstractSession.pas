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

unit Spring.Persistence.Core.AbstractSession;

interface

uses
  Rtti,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Params,
  Spring.Reflection;

type
  TAbstractSession = class
  private type
    IRowMapperInternal = interface(IRowMapper)
      procedure MapAssociation(const entity: IEntityWrapper;
        const resultSet: IDBResultSet); overload;
    end;

    TRowMapperInternal = class(TInterfacedObject, IRowMapper, IRowMapperInternal)
    private
      fEntityClass: TClass;
      fSession: TAbstractSession;
      fEntityWrapper: IEntityWrapper;
      fRowMappers: IList<IRowMapperInternal>;
      fMember: TRttiMember;
      fColumnsData: TColumnDataList;

      function ResolveLazyValue(const entity: IEntityWrapper;
        const columnMember: TRttiMember): TValue;

      procedure SetAssociations(const entity: IEntityWrapper;
        const resultSet: IDBResultSet);
      procedure SetEntityFromColumns(const entity: IEntityWrapper;
        const resultSet: IDBResultSet);
      procedure SetOneToManyColumns(const entity: IEntityWrapper);

      procedure MapAssociation(const baseEntity: IEntityWrapper;
        const resultSet: IDBResultSet); overload;
      procedure MapEntityFromColumns(const entity: IEntityWrapper;
        const resultSet: IDBResultSet);
    public
      constructor Create(entityClass: TClass; const session: TAbstractSession); overload;
      constructor Create(entityClass: TClass; const session: TAbstractSession;
        const member: TRttiMember; var index: Integer); overload;
      destructor Destroy; override;
      function MapRow(const resultSet: IDBResultSet): TObject; overload;
    end;
  private
    fConnection: IDBConnection;
    fRowMappers: IDictionary<TClass,IRowMapper>;
    fOldStateEntities: IEntityMap;

    {$REGION 'Lazy resolution mechanism - internal use only'}
    function MapAggregatedObject(const resultSet: IDBResultSet;
      const baseEntity: TObject; entityClass: TClass): TObject;

    function LoadOneToManyAssociation(const id: TValue; const entity: TObject;
      const column: ColumnAttribute; entityClass: TClass): IDBResultSet;

    function GetLazyValueAsInterface(const id: TValue; const entity: TObject;
      const column: ColumnAttribute; entityClass: TClass): IInterface;
    function GetLazyValueAsObject(const id: TValue; const entity: TObject;
      const column: ColumnAttribute; entityClass: TClass): TObject;

    function ResolveLazyInterface(const id: TValue; const entity: TObject;
      interfaceType: TRttiType; const column: ColumnAttribute): TValue;
    function ResolveLazyObject(const id: TValue; const entity: TObject;
      entityClass: TClass; const column: ColumnAttribute): TValue;
    {$ENDREGION}
  protected
    /// <summary>
    ///   Retrieves multiple models from the resultset into a list of entities.
    /// </summary>
    procedure FetchFromCustomQuery(const sql: string;
      const params: IEnumerable<TDBParam>; const list: IObjectList;
      entityClass: TClass); overload;

    /// <summary>
    ///   Maps resultset into a list of entities
    /// </summary>
    procedure MapEntitiesFromResultSet(const resultSet: IDBResultSet;
      const list: IObjectList; entityClass: TClass);

    /// <summary>
    ///   Maps resultset row into an entity instance
    /// </summary>
    function MapEntityFromResultSetRow(const resultSet: IDBResultSet;
      entityClass: TClass): TObject;

    /// <summary>
    ///   Get the resultset containing the item(s) matching the given id.
    /// </summary>
    function GetResultSetById(entityClass: TClass; const id: TValue;
      foreignEntityClass: TClass = nil;
      const selectColumn: ColumnAttribute = nil): IDBResultSet;

    /// <summary>
    ///   Gets the resultset from SQL statement.
    /// </summary>
    function GetResultSet(const sql: string;
      const params: IEnumerable<TDBParam>): IDBResultSet; overload;

    procedure AttachEntity(const entity: TObject); virtual;
    procedure DetachEntity(const entity: TObject); virtual;

    procedure DoInsert(const entity: TObject; const inserter: IInsertCommand); virtual;
    procedure DoUpdate(const entity: TObject; const updater: IUpdateCommand); virtual;
    procedure DoDelete(const entity: TObject; const deleter: IDeleteCommand); virtual;

    function GetInsertCommandExecutor(entityClass: TClass): IInsertCommand; virtual;
    function GetUpdateCommandExecutor(entityClass: TClass): IUpdateCommand; virtual;
    function GetDeleteCommandExecutor(entityClass: TClass): IDeleteCommand; virtual;
    function GetSelectCommandExecutor(entityClass: TClass): ISelectCommand; virtual;
    function GetSelectByIdCommandExecutor(entityClass: TClass; const id: TValue;
      foreignEntityClass: TClass = nil;
      const selectColumn: ColumnAttribute = nil): ISelectCommand; virtual;

    procedure RegisterNonGenericRowMapper(entityClass: TClass;
      const rowMapper: IRowMapper);
    procedure UnregisterNonGenericRowMapper(entityClass: TClass);

    procedure UpdateForeignKeys(const baseEntity, subEntity: TObject);
  public
    constructor Create(const connection: IDBConnection); overload; virtual;
    constructor Create(const connection: IDBConnection;
      const entityMap: IEntityMap); reintroduce; overload;
    destructor Destroy; override;

    property Connection: IDBConnection read fConnection;
    property OldStateEntities: IEntityMap read fOldStateEntities;
  end;

implementation

uses
  Classes,
  SysUtils,
  Variants,
  Spring.Persistence.Core.EntityMap,
  Spring.Persistence.Core.EntityWrapper,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.ResourceStrings,
  Spring.Persistence.SQL.Commands.Delete,
  Spring.Persistence.SQL.Commands.Insert,
  Spring.Persistence.SQL.Commands.Select,
  Spring.Persistence.SQL.Commands.Update;


{$REGION 'TAbstractSession'}

constructor TAbstractSession.Create(const connection: IDBConnection);
begin
  inherited Create;
  fConnection := connection;
  fRowMappers := TCollections.CreateDictionary<TClass,IRowMapper>;
  if fOldStateEntities = nil then
    fOldStateEntities := TEntityMap.Create;
end;

constructor TAbstractSession.Create(const connection: IDBConnection;
  const entityMap: IEntityMap);
begin
  fOldStateEntities := entityMap;
  Create(connection);
end;

destructor TAbstractSession.Destroy;
begin
  fOldStateEntities := nil;
  inherited Destroy;
end;

procedure TAbstractSession.AttachEntity(const entity: TObject);
begin
  fOldStateEntities.AddOrReplace(entity);
end;

procedure TAbstractSession.DetachEntity(const entity: TObject);
begin
  fOldStateEntities.Remove(entity);
end;

procedure TAbstractSession.DoDelete(const entity: TObject;
  const deleter: IDeleteCommand);
begin
  deleter.Execute(entity);
  DetachEntity(entity);
end;

function TAbstractSession.LoadOneToManyAssociation(const id: TValue;
  const entity: TObject; const column: ColumnAttribute;
  entityClass: TClass): IDBResultSet;
var
  baseEntityClass,
  entityToLoadClass: TClass;
begin
  baseEntityClass := entity.ClassType;
  entityToLoadClass := entityClass;

  if not TEntityCache.IsValidEntity(entityToLoadClass) then
    entityToLoadClass := baseEntityClass;

  if entityToLoadClass = baseEntityClass then
    baseEntityClass := nil;

  Result := GetResultSetById(entityToLoadClass, id, baseEntityClass, column);
end;

procedure TAbstractSession.DoInsert(const entity: TObject;
  const inserter: IInsertCommand);
begin
  inserter.Execute(entity);
  AttachEntity(entity);
end;

function TAbstractSession.MapAggregatedObject(const resultSet: IDBResultSet;
  const baseEntity: TObject; entityClass: TClass): TObject;

  function Convert(const value: Variant): TValue;
  var
    stream: TMemoryStream;
    p: Pointer;
  begin
    if VarIsArray(value) then
    begin
      stream := TMemoryStream.Create;
      p := VarArrayLock(value);
      try
        stream.Write(p^, VarArrayHighBound(value, VarArrayDimCount(value)) + 1);
      finally
        VarArrayUnlock(value);
      end;
      Result := stream;
    end
    else
      Result := TValue.FromVariant(value);
  end;

var
  fieldValue: Variant;
  value, convertedValue: TValue;
begin
  Result := nil;
  if not resultSet.IsEmpty then
  begin
    fieldValue := resultSet.GetFieldValue(0);
    value := Convert(fieldValue);
    try
      if value.TryConvert(entityClass.ClassInfo, convertedValue) then
        Result := convertedValue.AsObject;
    finally
      if value.IsObject and (value.AsObject <> convertedValue.AsObject) then
        value.Free;
    end;     
  end;
end;

procedure TAbstractSession.DoUpdate(const entity: TObject;
  const updater: IUpdateCommand);
begin
  updater.Execute(entity);
  AttachEntity(entity);
end;

procedure TAbstractSession.FetchFromCustomQuery(const sql: string;
  const params: IEnumerable<TDBParam>; const list: IObjectList; entityClass: TClass);
var
  results: IDBResultSet;
begin
  results := GetResultSet(sql, params);
  MapEntitiesFromResultSet(results, list, entityClass);
end;

function TAbstractSession.GetDeleteCommandExecutor(
  entityClass: TClass): IDeleteCommand;
begin
  Result := TDeleteExecutor.Create(Connection);
  Result.Build(entityClass);
end;

function TAbstractSession.GetInsertCommandExecutor(
  entityClass: TClass): IInsertCommand;
begin
  Result := TInsertExecutor.Create(Connection);
  Result.Build(entityClass);
end;

function TAbstractSession.GetLazyValueAsInterface(const id: TValue;
  const entity: TObject; const column: ColumnAttribute;
  entityClass: TClass): IInterface;
var
  results: IDBResultSet;
begin
  if not Assigned(entity) or id.IsEmpty then
    Exit(nil);

  results := LoadOneToManyAssociation(id, entity, column, entityClass);
  Result := TCollections.CreateObjectList<TObject>(True);
  MapEntitiesFromResultSet(results, Result as IObjectList, entityClass);
end;

function TAbstractSession.GetLazyValueAsObject(const id: TValue;
  const entity: TObject; const column: ColumnAttribute;
  entityClass: TClass): TObject;
var
  results: IDBResultSet;
begin
  if not Assigned(entity) or id.IsEmpty then
    Exit(nil);

  results := LoadOneToManyAssociation(id, entity, column, entityClass);
  Result := MapAggregatedObject(results, entity, entityClass);
end;

procedure TAbstractSession.MapEntitiesFromResultSet(
  const resultSet: IDBResultSet; const list: IObjectList;
  entityClass: TClass);
var
  rowMapper: IRowMapper;
  entity: TObject;
begin
  if not fRowMappers.TryGetValue(entityClass, rowMapper) then
    rowMapper := TRowMapperInternal.Create(entityClass, Self);

  while not resultSet.IsEmpty do
  begin
    entity := rowMapper.MapRow(resultSet);
    list.Add(entity);
    resultSet.Next;
  end;
end;

function TAbstractSession.MapEntityFromResultSetRow(
  const resultSet: IDBResultSet; entityClass: TClass): TObject;
var
  rowMapper: IRowMapper;
begin
  if not fRowMappers.TryGetValue(entityClass, rowMapper) then
    rowMapper := TRowMapperInternal.Create(entityClass, Self);

  Result := rowMapper.MapRow(resultSet);
end;

procedure TAbstractSession.RegisterNonGenericRowMapper(entityClass: TClass;
  const rowMapper: IRowMapper);
begin
  if fRowMappers.ContainsKey(entityClass) then
    raise EORMRowMapperAlreadyRegistered.CreateFmt('Row Mapper already registered for type: %s', [entityClass.ClassName]);
  fRowMappers.Add(entityClass, rowMapper);
end;

function TAbstractSession.ResolveLazyInterface(const id: TValue;
  const entity: TObject; interfaceType: TRttiType;
  const column: ColumnAttribute): TValue;
var
  entityClass: TClass;
  capturedId: TValue;
  factory: TFunc<IInterface>;
{$IFDEF AUTOREFCOUNT}
  capturedSelf: Pointer;
  capturedEntity: Pointer;
{$ENDIF}
begin
  if not interfaceType.IsGenericTypeOf('IEnumerable<>') then
    raise EORMUnsupportedType.CreateFmt('Unsupported type: %s', [interfaceType.Name]);
  entityClass := interfaceType.GetGenericArguments[0].AsInstance.MetaclassType;

  // Break reference held by the anonymous function closure (RSP-10176).
  // Do not use __ObjRelease but use unsafe pointer here, if the lazy is
  // released before TAbstractSession, it would destroy it.
{$IFDEF AUTOREFCOUNT}
  capturedSelf := Self;
  capturedEntity := entity;
{$ENDIF}
  capturedId := id;
  factory :=
    function: IInterface
    begin
{$IFDEF AUTOREFCOUNT}
      with TAbstractSession(capturedSelf) do
{$ENDIF}
        Result := GetLazyValueAsInterface(capturedId,
          {$IFNDEF AUTOREFCOUNT}entity,{$ELSE}capturedEntity,{$ENDIF}
          column, entityClass);
    end;
  Result := TValue.From<Lazy<IInterface>>(TLazy<IInterface>.Create(factory));
end;

function TAbstractSession.ResolveLazyObject(const id: TValue;
  const entity: TObject; entityClass: TClass;
  const column: ColumnAttribute): TValue;
var
  capturedId: TValue;
  factory: TFunc<TObject>;
{$IFDEF AUTOREFCOUNT}
  capturedSelf: Pointer;
  capturedEntity: Pointer;
{$ENDIF}
begin
  // Break reference held by the anonymous function closure (RSP-10176).
  // See above for details.
{$IFDEF AUTOREFCOUNT}
  capturedSelf := Self;
  capturedEntity := entity;
{$ENDIF}
  capturedId := id;
  factory :=
    function: TObject
    begin
{$IFDEF AUTOREFCOUNT}
      with TAbstractSession(capturedSelf) do
{$ENDIF}
        Result := GetLazyValueAsObject(capturedId,
          {$IFNDEF AUTOREFCOUNT}entity,{$ELSE}capturedEntity,{$ENDIF}
          column, entityClass);
    end;
  Result := TValue.From<Lazy<TObject>>(TLazy<TObject>.Create(factory, True));
end;

function TAbstractSession.GetResultSet(const sql: string;
  const params: IEnumerable<TDBParam>): IDBResultSet;
var
  statement: IDBStatement;
begin
  statement := Connection.CreateStatement;
  statement.SetSQLCommand(sql);
  if Assigned(params) then
    statement.SetParams(params);
  Result := statement.ExecuteQuery;
end;

function TAbstractSession.GetResultSetById(entityClass: TClass;
  const id: TValue; foreignEntityClass: TClass;
  const selectColumn: ColumnAttribute): IDBResultSet;
var
  selecter: ISelectCommand;
begin
  selecter := GetSelectByIdCommandExecutor(
    entityClass, id, foreignEntityClass, selectColumn);
  Result := selecter.Select;
end;

function TAbstractSession.GetSelectByIdCommandExecutor(entityClass: TClass;
  const id: TValue; foreignEntityClass: TClass;
  const selectColumn: ColumnAttribute): ISelectCommand;
begin
  Result := TSelectExecutor.Create(Connection, id, foreignEntityClass, selectColumn);
  Result.Build(entityClass);
end;

function TAbstractSession.GetSelectCommandExecutor(
  entityClass: TClass): ISelectCommand;
begin
  Result := TSelectExecutor.Create(Connection);
  Result.Build(entityClass);
end;

function TAbstractSession.GetUpdateCommandExecutor(
  entityClass: TClass): IUpdateCommand;
begin
  Result := TUpdateExecutor.Create(Connection, fOldStateEntities);
  Result.Build(entityClass);
end;

procedure TAbstractSession.UnregisterNonGenericRowMapper(entityClass: TClass);
begin
  fRowMappers.Remove(entityClass);
end;

procedure TAbstractSession.UpdateForeignKeys(
  const baseEntity, subEntity: TObject);
var
  baseEntityData, subEntityData: TEntityData;
  forColAttribute: ForeignJoinColumnAttribute;
  primaryKeyValue: TValue;
  primaryKeyTableName: string;
begin
  baseEntityData := TEntityCache.Get(baseEntity.ClassType);
  subEntityData := TEntityCache.Get(subEntity.ClassType);

  primaryKeyValue := baseEntityData.PrimaryKeyColumn.Member.GetValue(baseEntity);
  primaryKeyTableName := baseEntityData.EntityTable.TableName;
  for forColAttribute in subEntityData.ForeignKeyColumns do
    if SameText(forColAttribute.ReferencedTableName, primaryKeyTableName) then
      forColAttribute.Member.SetValue(subEntity, primaryKeyValue);
end;

{$ENDREGION}


{$REGION 'TAbstractSession.TRowMapperInternal'}

constructor TAbstractSession.TRowMapperInternal.Create(entityClass: TClass;
  const session: TAbstractSession);
var
  index: Integer;
begin
  index := 0;
  Create(entityClass, session, nil, index);
end;

constructor TAbstractSession.TRowMapperInternal.Create(entityClass: TClass;
  const session: TAbstractSession; const member: TRttiMember; var index: Integer);

  procedure ResolveColumns;
  var
    i: Integer;
    columnData: TColumnData;
  begin
    for i := fColumnsData.Count - 1 downto 0 do
    begin
      // dealing with records here so assignments necessary (cannot just set members)
      columnData := fColumnsData[i];
      columnData.ColumnName := Format('t%d$%s', [index, columnData.ColumnName]);
      fColumnsData[i] := columnData;
      if columnData.IsPrimaryKey then
        fColumnsData.PrimaryKeyColumn := columnData;
    end;
  end;

var
  column: ManyToOneAttribute;
begin
  inherited Create;
  fEntityClass := entityClass;
  fSession := session;

  if index = 0 then
    fEntityWrapper := TEntityWrapper.Create(entityClass)
  else
  begin
    fColumnsData := TColumnDataList.Create;
    fColumnsData.Assign(TEntityCache.Get(fEntityClass).ColumnsData);
    ResolveColumns;
    fEntityWrapper := TEntityWrapper.Create(fEntityClass, fColumnsData);
    fMember := member;
  end;

  if fEntityWrapper.HasManyToOneRelations then
  begin
    fRowMappers := TCollections.CreateList<IRowMapperInternal>;
    for column in fEntityWrapper.ManyToOneColumns do
    begin
      Inc(index);
      fRowMappers.Add(TRowMapperInternal.Create(
        column.Member.MemberType.AsInstance.MetaclassType, session, column.Member, index));
    end;
  end;
end;

destructor TAbstractSession.TRowMapperInternal.Destroy;
begin
  fColumnsData.Free;
  inherited Destroy;
end;

procedure TAbstractSession.TRowMapperInternal.MapAssociation(
  const baseEntity: IEntityWrapper; const resultSet: IDBResultSet);
var
  entity: TObject;
begin
  entity := fMember.GetValue(baseEntity.Entity).AsObject;
  if not Assigned(entity) then
  begin
    entity := TActivator.CreateInstance(fEntityClass);
    fMember.SetValue(baseEntity.Entity, entity);
  end;
  fEntityWrapper.Entity := entity;
  MapEntityFromColumns(fEntityWrapper, resultSet);
end;

procedure TAbstractSession.TRowMapperInternal.MapEntityFromColumns(
  const entity: IEntityWrapper; const resultSet: IDBResultSet);
begin
  SetEntityFromColumns(entity, resultSet);
  SetOneToManyColumns(entity);
  SetAssociations(entity, resultSet);
  fSession.AttachEntity(entity.Entity);
end;

function TAbstractSession.TRowMapperInternal.MapRow(
  const resultSet: IDBResultSet): TObject;
begin
  Result := TActivator.CreateInstance(fEntityClass);
  fEntityWrapper.Entity := Result;
  MapEntityFromColumns(fEntityWrapper, resultSet);
end;

function TAbstractSession.TRowMapperInternal.ResolveLazyValue(
  const entity: IEntityWrapper; const columnMember: TRttiMember): TValue;
var
  memberType: PTypeInfo;
  targetType: TRttiType;
  column: ColumnAttribute;
  id: TValue;
begin
  memberType := columnMember.MemberType.Handle;
  if GetLazyKind(memberType) <> lkRecord then
    raise EORMUnsupportedType.CreateFmt('Unsupported type: %s - expected Lazy<T>', [memberType.TypeName]);
  targetType := memberType.RttiType.GetGenericArguments[0];
  if targetType = nil then
    raise EORMUnsupportedType.CreateFmt('Unsupported type: %s - insufficient rtti', [memberType.TypeName]);
  Result := TValue.Empty;

  column := columnMember.GetCustomAttribute<ColumnAttribute>;
  id := entity.PrimaryKeyValue;
  case targetType.TypeKind of
    tkClass: Result := fSession.ResolveLazyObject(id, entity.Entity, targetType.AsInstance.MetaclassType, column);
    tkInterface: Result := fSession.ResolveLazyInterface(id, entity.Entity, targetType, column);
  else
    raise EORMUnsupportedType.CreateFmt('Unsupported type: %s', [targetType.Name]);
  end;
  if not Result.IsEmpty then
    TValueData(Result).FTypeInfo := memberType;
end;

procedure TAbstractSession.TRowMapperInternal.SetAssociations(
  const entity: IEntityWrapper; const resultSet: IDBResultSet);
var
  rowMapper: IRowMapperInternal;
begin
  if Assigned(fRowMappers) then
    for rowMapper in fRowMappers do
      rowMapper.MapAssociation(entity, resultSet);
end;

procedure TAbstractSession.TRowMapperInternal.SetEntityFromColumns(
  const entity: IEntityWrapper; const resultSet: IDBResultSet);
var
  columnData: TColumnData;
  fieldValue: Variant;
  value: TValue;
  i: Integer;
begin
  entity.SetPrimaryKeyValue(entity.GetPrimaryKeyValue(resultSet));
  for i := 0 to entity.ColumnsData.Count - 1 do
  begin
    columnData := entity.ColumnsData[i];
    if columnData.IsPrimaryKey then
      Continue;

    if columnData.IsLazy then
    begin
      value := ResolveLazyValue(entity, columnData.Member);
      entity.SetValue(columnData.Member, value);
    end
    else
    begin
      try
        fieldValue := resultSet.GetFieldValue(columnData.ColumnName);
      except
        raise EORMColumnNotFound.CreateResFmt(@SColumnNotFound, [columnData.ColumnName]);
      end;
      value := TValue.FromVariant(fieldValue);
      value := value.Convert(columnData.Member.MemberType.Handle);
      entity.SetValue(columnData.Member, value);
    end;
  end;
end;

procedure TAbstractSession.TRowMapperInternal.SetOneToManyColumns(
  const entity: IEntityWrapper);
var
  column: OneToManyAttribute;
  value: TValue;
  items: IObjectList;
  entityClass: TClass;
  id: TValue;
  results: IDBResultSet;
begin
  for column in entity.OneToManyColumns do
  begin
    if column.Member.IsField then
    begin
      value := ResolveLazyValue(entity, column.Member);
      if not value.IsEmpty then
        entity.SetValue(column.Member, value);
    end;
    if column.Member.IsProperty then
    begin
      if not column.Member.MemberType.IsInterface
        or not Supports(entity.GetValue(column.Member).AsInterface, IObjectList, items) then
        raise EORMUnsupportedType.CreateFmt(
          'Unsupported type: %s - expected IList<T: class>', [column.Member.MemberType.Name]);
      entityClass := items.ElementType.TypeData.ClassType;
      id := entity.PrimaryKeyValue;
      results := fSession.GetResultSetById(entityClass, id, entity.Entity.ClassType);
      fSession.MapEntitiesFromResultSet(results, items, entityClass);
    end;
  end;
end;

{$ENDREGION}


end.
