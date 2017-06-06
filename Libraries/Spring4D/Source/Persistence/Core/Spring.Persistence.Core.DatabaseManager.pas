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

unit Spring.Persistence.Core.DatabaseManager;

interface

uses
  Spring.Collections,
  Spring.Persistence.Core.Interfaces;

type
  /// <summary>
  ///   Responsible for building database structure from annotated entities.
  /// </summary>
  TDatabaseManager = class
  private
    fConnection: IDBConnection;
    fEntities: IList<TClass>;
  protected
    function GetCreateForeignKeyExecutor(entityClass: TClass): IDDLCommand;
    function GetCreateSequenceExecutor(entityClass: TClass): IDDLCommand;
    function GetCreateTableExecutor(entityClass: TClass): IDDLCommand;
    procedure BuildTables(const entities: IEnumerable<TClass>);
    procedure BuildForeignKeys(const entities: IEnumerable<TClass>);
    procedure BuildSequences(const entities: IEnumerable<TClass>);
  public
    constructor Create(const connection: IDBConnection);

    procedure BuildDatabase;

    procedure RegisterEntity(entityClass: TClass);
    procedure ClearEntities;
    procedure UnregisterEntity(entityClass: TClass);

    function EntityExists(entityClass: TClass): Boolean;

    property Connection: IDBConnection read fConnection;
  end;

implementation

uses
  Rtti,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands.CreateForeignKey,
  Spring.Persistence.SQL.Commands.CreateSequence,
  Spring.Persistence.SQL.Commands.CreateTable,
  Spring.Reflection;


{$REGION 'TDatabaseManager'}

constructor TDatabaseManager.Create(const connection: IDBConnection);

  function GetEntities: IList<TClass>;
  var
    rttiType: TRttiType;
  begin
    Result := TCollections.CreateList<TClass>;
    for rttiType in TType.Types do
      if rttiType.IsClass and rttiType.HasCustomAttribute<EntityAttribute> then
        Result.Add(rttiType.AsInstance.MetaclassType);
  end;

begin
  inherited Create;
  fConnection := connection;
  fEntities := GetEntities;
end;

procedure TDatabaseManager.BuildDatabase;
var
  transaction: IDBTransaction;
begin
  if not fEntities.IsEmpty then
  begin
    transaction := Connection.BeginTransaction;

    BuildTables(fEntities);
    BuildForeignKeys(fEntities);
    BuildSequences(fEntities);

    transaction.Commit;
  end;
end;

procedure TDatabaseManager.BuildForeignKeys(const entities: IEnumerable<TClass>);
var
  entityClass: TClass;
  createForeignKey: IDDLCommand;
begin
  for entityClass in entities do
  begin
    createForeignKey := GetCreateForeignKeyExecutor(entityClass);
    createForeignKey.Execute;
  end;
end;

procedure TDatabaseManager.BuildSequences(const entities: IEnumerable<TClass>);
var
  entityClass: TClass;
  createSequence: IDDLCommand;
begin
  for entityClass in entities do
  begin
    createSequence := GetCreateSequenceExecutor(entityClass);
    createSequence.Execute;
  end;
end;

procedure TDatabaseManager.BuildTables(const entities: IEnumerable<TClass>);
var
  entityClass: TClass;
  createTable: IDDLCommand;
begin
  for entityClass in entities do
  begin
    createTable := GetCreateTableExecutor(entityClass);
    createTable.Execute;
  end;
end;

procedure TDatabaseManager.ClearEntities;
begin
  fEntities.Clear;
end;

function TDatabaseManager.EntityExists(entityClass: TClass): Boolean;
var
  createTable: IDDLCommand;
begin
  createTable := GetCreateTableExecutor(entityClass);
  Result := createTable.TableExists;
end;

function TDatabaseManager.GetCreateTableExecutor(
  entityClass: TClass): IDDLCommand;
begin
  Result := TCreateTableExecutor.Create(fConnection);
  Result.Build(entityClass);
end;

function TDatabaseManager.GetCreateForeignKeyExecutor(
  entityClass: TClass): IDDLCommand;
begin
  Result := TCreateForeignKeyExecutor.Create(fConnection);
  Result.Build(entityClass);
end;

function TDatabaseManager.GetCreateSequenceExecutor(
  entityClass: TClass): IDDLCommand;
begin
  Result := TCreateSequenceExecutor.Create(fConnection);
  Result.Build(entityClass);
end;

procedure TDatabaseManager.RegisterEntity(entityClass: TClass);
begin
  fEntities.Add(entityClass);
end;

procedure TDatabaseManager.UnregisterEntity(entityClass: TClass);
begin
  fEntities.Remove(entityClass)
end;

{$ENDREGION}


end.
