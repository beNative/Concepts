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

unit Spring.Persistence.SQL.Commands.CreateTable;

interface

uses
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Responsible for building and executing statements which create tables
  ///   in the database.
  /// </summary>
  TCreateTableExecutor = class(TAbstractCommandExecutor, IDDLCommand)
  private
    fCommand: TCreateTableCommand;
    fTable: TSQLTable;
    fSQLs: IList<string>;
  protected
    function GetCommand: TDMLCommand; override;
  public
    constructor Create(const connection: IDBConnection); override;
    destructor Destroy; override;

    procedure Build(entityClass: TClass); override;
    procedure Execute;
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions;


{$REGION 'TTableCreateCommand'}

constructor TCreateTableExecutor.Create(const connection: IDBConnection);
begin
  inherited Create(connection);
  fTable := TSQLTable.Create;
  fCommand := TCreateTableCommand.Create(fTable);
end;

destructor TCreateTableExecutor.Destroy;
begin
  fCommand.Free;
  fTable.Free;
  inherited Destroy;
end;

procedure TCreateTableExecutor.Build(entityClass: TClass);
begin
  inherited Build(entityClass);

  fTable.SetFromAttribute(EntityData.EntityTable);
  fCommand.SetCommandFieldsFromColumns(EntityData.Columns);
  fCommand.TableExists := TableExists(fTable.Name);
  if fCommand.TableExists then
    FillDbTableColumns(fTable.Name, fCommand.ColumnNames);

  fSQLs := Generator.GenerateCreateTable(fCommand);
end;

procedure TCreateTableExecutor.Execute;
var
  statement: IDBStatement;
  sqlStatement: string;
begin
  for sqlStatement in fSQLs do
  begin
    if sqlStatement = '' then
      Continue;

    statement := Connection.CreateStatement;
    statement.SetSQLCommand(sqlStatement);
    statement.Execute;
  end;
end;

function TCreateTableExecutor.GetCommand: TDMLCommand;
begin
  Result := fCommand;
end;

{$ENDREGION}


end.
