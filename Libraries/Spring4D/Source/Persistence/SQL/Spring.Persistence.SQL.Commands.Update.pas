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

unit Spring.Persistence.SQL.Commands.Update;

interface

uses
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Responsible for building and executing <c>update</c> statements.
  /// </summary>
  TUpdateExecutor = class(TAbstractCommandExecutor, IUpdateCommand)
  private
    fTable: TSQLTable;
    fCommand: TUpdateCommand;
    fColumns: IList<ColumnAttribute>;
    fEntityMap: IEntityMap;
  protected
    function GetCommand: TDMLCommand; override;
  public
    constructor Create(const connection: IDBConnection;
      const entityMap: IEntityMap); reintroduce;
    destructor Destroy; override;

    procedure Build(entityClass: TClass); override;
    procedure BuildParams(const entity: TObject); override;
    procedure Execute(const entity: TObject);
  end;

implementation

uses
  Spring,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.SQL.Params,
  Spring.Reflection;


{$REGION 'TUpdateCommand'}

constructor TUpdateExecutor.Create(const connection: IDBConnection;
  const entityMap: IEntityMap);
begin
  inherited Create(connection);
  fTable := TSQLTable.Create;
  fColumns := TCollections.CreateList<ColumnAttribute>;
  fCommand := TUpdateCommand.Create(fTable);
  fEntityMap := entityMap;
end;

destructor TUpdateExecutor.Destroy;
begin
  fTable.Free;
  fCommand.Free;
  inherited Destroy;
end;

procedure TUpdateExecutor.Execute(const entity: TObject);
var
  statement: IDBStatement;
  sqlStatement: string;
  affectedRows: Integer;
begin
  Assert(Assigned(entity));

  fColumns.Clear;
  if fEntityMap.IsMapped(entity) then
  begin
    fColumns := fEntityMap.GetChangedMembers(entity, EntityData);
    if fColumns.Count = 0 then
      Exit;
  end
  else
    fColumns.AddRange(EntityData.Columns);

  fCommand.VersionColumn := entityData.VersionColumn;
  fCommand.SetCommandFieldsFromColumns(fColumns);
  if not fCommand.UpdateFields.Any then
    Exit;

  fCommand.Entity := entity;
  sqlStatement := Generator.GenerateUpdate(fCommand);
  if sqlStatement = '' then
    raise EORMCannotGenerateQueryStatement.Create(entity);

  statement := Connection.CreateStatement;
  statement.SetSQLCommand(sqlStatement);
  BuildParams(entity);
  statement.SetParams(SQLParameters);
  affectedRows := statement.Execute;

  if EntityData.HasVersionColumn then
  begin
    if affectedRows = 0 then
      raise EORMOptimisticLockException.Create(entity);

    EntityData.VersionColumn.IncrementValue(entity);
  end;
end;

function TUpdateExecutor.GetCommand: TDMLCommand;
begin
  Result := fCommand;
end;

procedure TUpdateExecutor.Build(entityClass: TClass);
begin
  inherited Build(entityClass);

  fTable.SetFromAttribute(EntityData.EntityTable);
  fCommand.PrimaryKeyColumn := EntityData.PrimaryKeyColumn;
end;

procedure TUpdateExecutor.BuildParams(const entity: TObject);
var
  param: TDBParam;
  updateField: TSQLUpdateField;
  whereField: TSQLWhereField;
begin
  inherited BuildParams(entity);

  for updateField in fCommand.UpdateFields do
  begin
    if updateField.Column.IsVersionColumn then
      param := Generator.CreateParam(updateField, updateField.Column.GetValue(entity).AsInteger + 1)
    else
      param := Generator.CreateParam(updateField, updateField.Column.GetValue(entity));
    SQLParameters.Add(param);
  end;

  for whereField in fCommand.WhereFields do
  begin
    param := Generator.CreateParam(whereField, whereField.Column.GetValue(entity));
    SQLParameters.Add(param);
  end;
end;

{$ENDREGION}


end.
