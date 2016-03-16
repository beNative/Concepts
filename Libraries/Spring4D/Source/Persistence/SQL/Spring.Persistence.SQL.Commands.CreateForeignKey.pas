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

unit Spring.Persistence.SQL.Commands.CreateForeignKey;

interface

uses
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Responsible for building and executing statements to create foreign
  ///   keys.
  /// </summary>
  TCreateForeignKeyExecutor = class(TAbstractCommandExecutor, IDDLCommand)
  private
    fCommand: TCreateForeignKeyCommand;
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
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Mapping.Attributes;


{$REGION 'TForeignKeyCreateCommand'}

constructor TCreateForeignKeyExecutor.Create(const connection: IDBConnection);
begin
  inherited Create(connection);
  fTable := TSQLTable.Create;
  fCommand := TCreateForeignKeyCommand.Create(fTable);
end;

destructor TCreateForeignKeyExecutor.Destroy;
begin
  fCommand.Free;
  fTable.Free;
  inherited Destroy;
end;

procedure TCreateForeignKeyExecutor.Build(entityClass: TClass);
begin
  inherited Build(entityClass);

  fTable.SetFromAttribute(EntityData.EntityTable);
  fCommand.SetCommandFieldsFromColumns(EntityData.Columns);
  fCommand.TableExists := TableExists(fTable.Name);
  if fCommand.TableExists then
    FillDbTableColumns(fTable.Name, fCommand.ColumnNames);
  fSQLs := Generator.GenerateCreateForeignKey(fCommand);
end;

procedure TCreateForeignKeyExecutor.Execute;
var
  sqlStatement: string;
  statement: IDBStatement;
begin
  for sqlStatement in fSQLs do
  begin
    if sqlStatement = '' then
      Exit;

    statement := Connection.CreateStatement;
    statement.SetSQLCommand(sqlStatement);
    statement.Execute;
  end;
end;

function TCreateForeignKeyExecutor.GetCommand: TDMLCommand;
begin
  Result := fCommand;
end;

{$ENDREGION}


end.
