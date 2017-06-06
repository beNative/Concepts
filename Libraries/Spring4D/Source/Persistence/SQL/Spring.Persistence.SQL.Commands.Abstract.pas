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

unit Spring.Persistence.SQL.Commands.Abstract;

interface

uses
  Spring.Collections,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params;

type
  TAbstractCommandExecutor = class(TInterfacedObject, IDBCommand)
  private
    fConnection: IDBConnection;
    fGenerator: ISQLGenerator;
    fSQL: string;
    fEntityData: TEntityData;
    fParams: IList<TDBParam>;
  protected
    procedure FillDbTableColumns(const tableName: string; const columns: IList<string>);
    function GetCommand: TDMLCommand; virtual; abstract;

    property Command: TDMLCommand read GetCommand;
    property Connection: IDBConnection read fConnection;
    property EntityData: TEntityData read fEntityData;
    property Generator: ISQLGenerator read fGenerator;
    property SQL: string read fSQL write fSQL;
    property SQLParameters: IList<TDBParam> read fParams;
  public
    constructor Create(const connection: IDBConnection); virtual;

    procedure Build(entityClass: TClass); virtual;
    procedure BuildParams(const entity: TObject); virtual;

    function TableExists: Boolean; overload; virtual;
    function TableExists(const tableName: string): Boolean; overload; virtual;
  end;

implementation

uses
  Spring,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.SQL.Register;


{$REGION 'TAbstractCommandExecutor'}

constructor TAbstractCommandExecutor.Create(const connection: IDBConnection);
begin
  Guard.CheckNotNull(connection, 'connection');
  inherited Create;
  fConnection := connection;
  fGenerator := TSQLGeneratorRegister.GetGenerator(fConnection.QueryLanguage);
  fParams := TCollections.CreateObjectList<TDBParam>;
end;

procedure TAbstractCommandExecutor.Build(entityClass: TClass);
begin
  fEntityData := TEntityCache.Get(entityClass);
end;

procedure TAbstractCommandExecutor.BuildParams(const entity: TObject);
begin
  fParams.Clear;
  if Assigned(Command) then
    Command.Entity := entity;
end;

procedure TAbstractCommandExecutor.FillDbTableColumns(const tableName: string;
  const columns: IList<string>);
var
  sqlStatement: string;
  statement: IDBStatement;
  results: IDBResultSet;
  i: Integer;
begin
  sqlStatement := Generator.GetTableColumns(tableName);
  if sqlStatement <> '' then
  begin
    statement := Connection.CreateStatement;
    statement.SetSQLCommand(sqlStatement);
    results := statement.ExecuteQuery;
    columns.Clear;
    for i := 0 to results.GetFieldCount - 1 do
      columns.Add(results.GetFieldName(i));
  end;
end;

function TAbstractCommandExecutor.TableExists: Boolean;
begin
  Result := TableExists(EntityData.EntityTable.TableName);
end;

function TAbstractCommandExecutor.TableExists(const tableName: string): Boolean;
var
  sqlStatement: string;
  useTableExists: Boolean;
  statement: IDBStatement;
  results: IDBResultSet;
begin
  sqlStatement := Generator.GetSQLTableExists(tableName);
  useTableExists := sqlStatement <> '';
  if not useTableExists then
    sqlStatement := Generator.GetSQLTableCount(tableName);
  if sqlStatement <> '' then
  begin
    statement := Connection.CreateStatement;
    try
      statement.SetSQLCommand(sqlStatement);
      results := statement.ExecuteQuery;

      if useTableExists then
        Result := results.GetFieldValue(0) > 0
      else
        Result := not results.IsEmpty;
    except
      Result := False;
    end;
  end
  else
    Result := False;
end;

{$ENDREGION}


end.
