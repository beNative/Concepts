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
  Spring.Persistence.Core.ResourceStrings,
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
    raise EORMPrimaryKeyColumnNotFound.CreateResFmt(@SPrimaryKeyNotFound,
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

  procedure RaiseInvalidConversion;
  begin
    raise EORMInvalidConversion.CreateFmt('Error while setting member %s - converting %s to %s failed',
      [member.Name, value.TypeInfo.TypeName, member.MemberType.Name]);
  end;

begin
  try
    member.SetValue(fEntity, value.ConvertTo(member.MemberType.Handle));
  except
    on E: EInvalidCast do
      RaiseInvalidConversion;
    on E: EConvertError do
      RaiseInvalidConversion;
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
