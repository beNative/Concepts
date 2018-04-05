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

unit Spring.Persistence.Core.EntityCache;

interface

uses
  Classes,
  Spring,
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes;

type
  TColumnDataList = class(TPersistent)
  private
    fList: IList<TColumnData>;
    fPrimaryKeyColumn: TColumnData;
    fPrimaryKeyExists: Boolean;
    function GetCount: Integer;
    function GetItem(index: Integer): TColumnData;
    procedure SetItem(index: Integer; const value: TColumnData);
    procedure SetPrimaryKeyColumn(const value: TColumnData);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;

    function GetEnumerator: IEnumerator<TColumnData>;

    function Add(const columnData: TColumnData): Integer;
    procedure Delete(index: Integer);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TColumnData read GetItem write SetItem; default;
    property PrimaryKeyColumn: TColumnData read fPrimaryKeyColumn write SetPrimaryKeyColumn;
    property PrimaryKeyExists: Boolean read fPrimaryKeyExists;
  end;

  TEntityData = class(TPersistent)
  private
    fColumns: IList<ColumnAttribute>;
    fSelectColumns: IList<ColumnAttribute>;
    fColumnsData: TColumnDataList;
    fForeignKeyColumns: IList<ForeignJoinColumnAttribute>;
    fPrimaryKeyColumn: ColumnAttribute;
    fTable: TableAttribute;
    fOneToManyColumns: IList<OneToManyAttribute>;
    fManyToOneColumns: IList<ManyToOneAttribute>;
    fSequence: SequenceAttribute;
    fEntityClass: TClass;
    fVersionColumn: VersionAttribute;
    class function GetAttributes<T: TORMAttribute>(entityClass: TClass): IList<T>; static;
    procedure InitColumnsData;
    procedure InitORMAttributes;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(entityClass: TClass);
    destructor Destroy; override;

    function HasSequence: Boolean;
    function HasManyToOneRelations: Boolean;
    function HasOneToManyRelations: Boolean;
    function HasVersionColumn: Boolean;

    function GetForeignKeyColumn(const table: TableAttribute;
      const primaryKeyColumn: ColumnAttribute): ForeignJoinColumnAttribute;
    function GetPrimaryKeyValueAsString(const instance: TObject): string;
    function GetRelations(const entity: TObject;
      const relationAttributeClass: TAttributeClass): IList<TObject>;

    property Columns: IList<ColumnAttribute> read fColumns;
    property SelectColumns: IList<ColumnAttribute> read fSelectColumns;
    property ColumnsData: TColumnDataList read fColumnsData write fColumnsData;
    property ForeignKeyColumns: IList<ForeignJoinColumnAttribute> read fForeignKeyColumns;
    property OneToManyColumns: IList<OneToManyAttribute> read fOneToManyColumns;
    property ManyToOneColumns: IList<ManyToOneAttribute> read fManyToOneColumns;
    property PrimaryKeyColumn: ColumnAttribute read fPrimaryKeyColumn;
    property Sequence: SequenceAttribute read fSequence write fSequence;
    property VersionColumn: VersionAttribute read fVersionColumn;
    property EntityClass: TClass read fEntityClass;
    property EntityTable: TableAttribute read fTable;
  end;

  /// <summary>
  ///   Class which holds cached data of annotated entities.
  /// </summary>
  TEntityCache = class
  strict private
    class var fEntities: IDictionary<TClass,TEntityData>;
    class var fLock: ICriticalSection;
  public
    class constructor Create;

    class function Get(entityClass: TClass): TEntityData; static;
    class function IsValidEntity(entityClass: TClass): Boolean; static;
  end;

implementation

uses
  Generics.Collections,
  SysUtils,
  Rtti,
  Spring.Persistence.Core.Exceptions,
  Spring.Reflection;


{$REGION 'TColumnDataList'}

constructor TColumnDataList.Create;
begin
  inherited Create;
  fList := TCollections.CreateList<TColumnData>;
end;

function TColumnDataList.Add(const columnData: TColumnData): Integer;
begin
  Result := fList.Add(columnData);
end;

procedure TColumnDataList.AssignTo(Dest: TPersistent);
var
  target: TColumnDataList;
begin
  if Dest is TColumnDataList then
  begin
    target := TColumnDataList(Dest);

    target.fList.Clear;
    target.fList.AddRange(fList);

    target.fPrimaryKeyColumn := fPrimaryKeyColumn;
    target.fPrimaryKeyExists := fPrimaryKeyExists;
  end;
end;

procedure TColumnDataList.Delete(index: Integer);
begin
  fList.Delete(index);
end;

function TColumnDataList.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TColumnDataList.GetEnumerator: IEnumerator<TColumnData>;
begin
  Result := fList.GetEnumerator;
end;

function TColumnDataList.GetItem(index: Integer): TColumnData;
begin
  Result := fList[index];
end;

procedure TColumnDataList.SetItem(index: Integer; const value: TColumnData);
begin
  fList[index] := value;
end;

procedure TColumnDataList.SetPrimaryKeyColumn(const value: TColumnData);
begin
  fPrimaryKeyColumn := value;
  fPrimaryKeyExists := True;
end;

//function TColumnDataList.TryGetPrimaryKeyColumn(out primaryKeyColumn: TColumnData): Boolean;
//begin
//  Result := fPrimaryKeyExists;
//  if Result then
//    primaryKeyColumn := fPrimaryKeyColumn;
//end;

{$ENDREGION}


{$REGION 'TEntityData'}

constructor TEntityData.Create(entityClass: TClass);
begin
  inherited Create;

  fEntityClass := entityClass;
  InitORMAttributes;

  fTable := TType.GetType(fEntityClass).GetCustomAttribute<TableAttribute>(True);
  if not Assigned(fTable) then
    raise EORMUnsupportedType.CreateFmt('Type %s requires [Table] attribute', [fEntityClass.ClassName]);
  fColumns := GetAttributes<ColumnAttribute>(fEntityClass);
  fPrimaryKeyColumn := fColumns.FirstOrDefault(
    function(const column: ColumnAttribute): Boolean
    begin
      Result := column.IsPrimaryKey;
    end);
  if not Assigned(fPrimaryKeyColumn) then
    raise EORMUnsupportedType.CreateFmt('Type %s requires primary key [Column]', [fEntityClass.ClassName]);
  fForeignKeyColumns := GetAttributes<ForeignJoinColumnAttribute>(fEntityClass);
  fOneToManyColumns := GetAttributes<OneToManyAttribute>(fEntityClass);
  fManyToOneColumns := GetAttributes<ManyToOneAttribute>(fEntityClass);
  fSequence := TType.GetType(fEntityClass).GetCustomAttribute<SequenceAttribute>(True);

  fSelectColumns := TCollections.CreateList<ColumnAttribute>;
  fColumnsData := TColumnDataList.Create;
  InitColumnsData;
end;

destructor TEntityData.Destroy;
begin
  fColumnsData.Free;
  inherited Destroy;
end;

procedure TEntityData.AssignTo(Dest: TPersistent);
var
  target: TEntityData;
begin
  if Dest is TEntityData then
  begin
    target := TEntityData(Dest);

    target.fTable := fTable;
    target.fColumns.Clear;
    target.fColumns.AddRange(Columns);

    target.fSelectColumns.Clear;
    target.fSelectColumns.AddRange(SelectColumns);

    target.fColumnsData.fList.Clear;
    target.fColumnsData.fList.AddRange(fColumnsData.fList);

    target.fForeignKeyColumns.Clear;
    target.fForeignKeyColumns.AddRange(fForeignKeyColumns);

    target.fPrimaryKeyColumn := fPrimaryKeyColumn;

    target.fOneToManyColumns.Clear;
    target.fOneToManyColumns.AddRange(fOneToManyColumns);

    target.fManyToOneColumns.Clear;
    target.fManyToOneColumns.AddRange(ManyToOneColumns);

    target.fSequence := fSequence;
    target.fVersionColumn := fVersionColumn;
  end;
end;

class function TEntityData.GetAttributes<T>(entityClass: TClass): IList<T>;
var
  rttiType: TRttiType;
  attribute: T;
  rttiField: TRttiField;
  rttiProperty: TRttiProperty;
begin
  Result := TCollections.CreateList<T>;
  rttiType := TType.GetType(entityClass);

  for rttiField in rttiType.GetFields do
    for attribute in rttiField.GetCustomAttributes<T> do
      Result.Add(attribute);

  for rttiProperty in rttiType.GetProperties do
    for attribute in rttiProperty.GetCustomAttributes<T> do
      Result.Add(attribute);
end;

function TEntityData.GetForeignKeyColumn(const table: TableAttribute;
  const primaryKeyColumn: ColumnAttribute): ForeignJoinColumnAttribute;
begin
  Result := fForeignKeyColumns.SingleOrDefault(
    function(const foreignColumn: ForeignJoinColumnAttribute): Boolean
    begin
      Result := SameText(primaryKeyColumn.ColumnName, foreignColumn.ReferencedColumnName)
       and SameText(Table.TableName, foreignColumn.ReferencedTableName);
    end);
end;

function TEntityData.GetPrimaryKeyValueAsString(
  const instance: TObject): string;
begin
  Result := fPrimaryKeyColumn.Member.GetValue(instance).ToType<string>;
end;

function TEntityData.GetRelations(const entity: TObject;
  const relationAttributeClass: TAttributeClass): IList<TObject>;

  function GetMemberValueDeep(const initialValue: TValue): TValue;
  begin
    Result := initialValue;
    if IsNullable(Result.TypeInfo) then
      Result := initialValue.GetNullableValue
    else if IsLazyType(Result.TypeInfo) then
      if not initialValue.TryGetLazyValue(Result) then
        Result := TValue.Empty;
  end;

  function GetSubEntityFromMemberDeep(const entity: TObject;
    const rttiMember: TRttiMember): IList<TObject>;
  var
    memberValue: TValue;
    value: TValue;
    objects: IObjectList;
    current: TObject;
  begin
    Result := TCollections.CreateList<TObject>;

    memberValue := rttiMember.GetValue(entity);
    if memberValue.IsEmpty then
      Exit;

    value := GetMemberValueDeep(memberValue);
    if value.IsEmpty then
      Exit;

    if value.IsInterface and Supports(value.AsInterface, IObjectList, objects) then
    begin
      for current in objects do
        Result.Add(current);
      value := TValue.Empty;
    end;

    if value.IsObject and (value.AsObject <> nil) then
      Result.Add(value.AsObject);
  end;

var
  rttiType: TRttiType;
  field: TRttiField;
  prop: TRttiProperty;
begin
  Result := TCollections.CreateList<TObject>;

  rttiType := TType.GetType(entity.ClassType);
  for field in rttiType.GetFields do
    if field.HasCustomAttribute(relationAttributeClass)
      and not field.HasCustomAttribute(TransientAttribute) then
      Result.AddRange(GetSubEntityFromMemberDeep(entity, field));

  for prop in rttiType.GetProperties do
    if prop.HasCustomAttribute(relationAttributeClass)
      and not prop.HasCustomAttribute(TransientAttribute) then
      Result.AddRange(GetSubEntityFromMemberDeep(entity, prop));
end;

function TEntityData.HasManyToOneRelations: Boolean;
begin
  Result := fManyToOneColumns.Any;
end;

function TEntityData.HasOneToManyRelations: Boolean;
begin
  Result := fOneToManyColumns.Any;
end;

function TEntityData.HasSequence: Boolean;
begin
  Result := Assigned(fSequence);
end;

function TEntityData.HasVersionColumn: Boolean;
begin
  Result := Assigned(fVersionColumn);
end;

procedure TEntityData.InitColumnsData;
var
  column: ColumnAttribute;
  columnData: TColumnData;
begin
  for column in fColumns do
  begin
    columnData.ColumnName := column.ColumnName;
    columnData.Properties := column.Properties;
    columnData.Member := column.Member;
    columnData.IsLazy := IsLazyType(column.Member.MemberType.Handle);

    if column.IsPrimaryKey then
      fColumnsData.PrimaryKeyColumn := columnData;

    if column.IsVersionColumn then
      fVersionColumn := column as VersionAttribute;

    if not columnData.IsLazy then
      fSelectColumns.Add(column);

    fColumnsData.Add(columnData);
  end;
end;

procedure TEntityData.InitORMAttributes;
var
  rttiType: TRttiType;
  attribute: TORMAttribute;
  rttiField: TRttiField;
  rttiProperty: TRttiProperty;
begin
  rttiType := TType.GetType(fEntityClass);

  for attribute in rttiType.GetCustomAttributes<TORMAttribute>(True) do
  begin
    attribute.EntityClass := fEntityClass;
    attribute.MemberKind := mkClass;
  end;

  for rttiField in rttiType.GetFields do
    for attribute in rttiField.GetCustomAttributes<TORMAttribute> do
    begin
      attribute.EntityClass := fEntityClass;
      attribute.MemberKind := mkField;
      attribute.Member := rttiField;
    end;

  for rttiProperty in rttiType.GetProperties do
    for attribute in rttiProperty.GetCustomAttributes<TORMAttribute> do
    begin
      attribute.EntityClass := fEntityClass;
      attribute.MemberKind := mkProperty;
      attribute.Member := rttiProperty;
    end;
end;

{$ENDREGION}


{$REGION 'TEntityCache'}

class constructor TEntityCache.Create;
begin
  fLock := TInterfacedCriticalSection.Create;
  fEntities := TCollections.CreateDictionary<TClass,TEntityData>([doOwnsValues], 100);
end;

class function TEntityCache.Get(entityClass: TClass): TEntityData;
begin
  fLock.Enter;
  try
    if not fEntities.TryGetValue(entityClass, Result) then
    begin
      Result := TEntityData.Create(entityClass);
      fEntities.Add(entityClass, Result);
    end;
  finally
    fLock.Leave;
  end;
end;

class function TEntityCache.IsValidEntity(entityClass: TClass): Boolean;
var
  entityType: TRttiType;
begin
  fLock.Enter;
  try
    if fEntities.ContainsKey(entityClass) then
      Exit(True);
  finally
    fLock.Leave;
  end;
  entityType := TType.GetType(entityClass);
  Result := entityType.HasCustomAttribute<TableAttribute>(True)
    and TEntityData.GetAttributes<ColumnAttribute>(entityClass).Any(
      function(const column: ColumnAttribute): Boolean
      begin
        Result := column.IsPrimaryKey;
      end);
end;

{$ENDREGION}


end.
