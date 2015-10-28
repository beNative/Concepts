unit Spring.Persistence.Core.EntityWrapper;

interface

uses
  Rtti,
  Spring.Collections,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes;

type
  TEntityWrapper = class(TInterfacedObject, IEntityWrapper)
  private
    fEntity: TObject;
    fEntityClassData: TEntityData;
    fColumnsData: TColumnDataList;
    function GetEntity: TObject;
    function GetColumnsData: TColumnDataList;
    procedure SetEntity(const value: TObject);
  public
    constructor Create(const entity: TObject;
      const columnsData: TColumnDataList = nil); overload;
    constructor Create(entityClass: TClass;
      const columnsData: TColumnDataList = nil); overload;

    function GetPrimaryKeyValue: TValue; overload;
    function GetPrimaryKeyValue(const resultSet: IDBResultSet): TValue; overload;

    function GetValue(const member: TRttiMember): TValue;

    procedure SetValue(const member: TRttiMember; const value: TValue);
    procedure SetPrimaryKeyValue(const value: TValue);

    function HasOneToManyRelations: Boolean;
    function HasManyToOneRelations: Boolean;
    function GetOneToManyColumns: IEnumerable<OneToManyAttribute>;
    function GetManyToOneColumns: IEnumerable<ManyToOneAttribute>;
    function GetForeignKeyColumns: IEnumerable<ForeignJoinColumnAttribute>;

    function GetTableName: string;

    property Entity: TObject read GetEntity;
    property ColumnsData: TColumnDataList read GetColumnsData;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  Spring,
  Spring.Persistence.Core.Consts,
  Spring.Persistence.Core.EmbeddedEntity,
  Spring.Persistence.Core.Exceptions,
  Spring.Reflection;


{$REGION 'TEntityWrapper'}

constructor TEntityWrapper.Create(const entity: TObject;
  const columnsData: TColumnDataList);
begin
  Create(entity.ClassType, columnsData);
  fEntity := entity;
end;

constructor TEntityWrapper.Create(entityClass: TClass;
  const columnsData: TColumnDataList);
begin
  inherited Create;
  fEntityClassData := TEntityCache.Get(entityClass);
  fColumnsData := columnsData;
  if not Assigned(fColumnsData) then
    fColumnsData := fEntityClassData.ColumnsData;
end;

function TEntityWrapper.GetColumnsData: TColumnDataList;
begin
  Result := fColumnsData;
end;

function TEntityWrapper.GetValue(const member: TRttiMember): TValue;
begin
  Result := member.GetValue(fEntity);
end;

function TEntityWrapper.GetEntity: TObject;
begin
  Result := fEntity;
end;

function TEntityWrapper.GetForeignKeyColumns: IEnumerable<ForeignJoinColumnAttribute>;
begin
  Result := fEntityClassData.ForeignKeyColumns;
end;

function TEntityWrapper.GetManyToOneColumns: IEnumerable<ManyToOneAttribute>;
begin
  Result := fEntityClassData.ManyToOneColumns;
end;

function TEntityWrapper.GetOneToManyColumns: IEnumerable<OneToManyAttribute>;
begin
  Result := fEntityClassData.OneToManyColumns;
end;

function TEntityWrapper.GetPrimaryKeyValue: TValue;
begin
  Result := GetValue(fEntityClassData.PrimaryKeyColumn.Member);
end;

function TEntityWrapper.GetPrimaryKeyValue(const resultSet: IDBResultSet): TValue;
var
  value: Variant;
  columnData: TColumnData;
begin
  if resultSet is TEmbeddedEntity then
    Exit(TValue.Empty);

  if not fColumnsData.PrimaryKeyExists then
    raise EORMPrimaryKeyColumnNotFound.CreateFmt(
      'Primary key column cannot be found for entity: %s', [fEntity.ClassName]);

  try
    value := resultSet.GetFieldValue(fColumnsData.PrimaryKeyColumn.ColumnName);
  except
    raise EORMPrimaryKeyColumnNotFound.CreateFmt(EXCEPTION_PRIMARYKEY_NOTFOUND,
      [columnData.ColumnName]);
  end;
  Result := TValue.FromVariant(value);
end;

function TEntityWrapper.GetTableName: string;
begin
  Result := fEntityClassData.EntityTable.TableName;
end;

function TEntityWrapper.HasManyToOneRelations: Boolean;
begin
  Result := fEntityClassData.HasManyToOneRelations;
end;

function TEntityWrapper.HasOneToManyRelations: Boolean;
begin
  Result := fEntityClassData.HasOneToManyRelations;
end;

procedure TEntityWrapper.SetValue(const member: TRttiMember; const value: TValue);
begin
  try
    member.SetValue(fEntity, value);
  except
    on E: EInvalidCast do
      raise EORMInvalidConversion.CreateFmt('Error while setting member %s - converting %s to %s failed',
        [member.Name, value.TypeInfo.TypeName, member.MemberType.Name]);
  end;
end;

procedure TEntityWrapper.SetEntity(const value: TObject);
begin
  fEntity := value;
end;

procedure TEntityWrapper.SetPrimaryKeyValue(const value: TValue);
begin
  if not value.IsEmpty then
    SetValue(fEntityClassData.PrimaryKeyColumn.Member, value);
end;

{$ENDREGION}


end.
