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

unit Spring.Persistence.SQL.Commands.Delete;

interface

uses
  Rtti,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Responsible for building and executing <c>delete</c> statements.
  /// </summary>
  TDeleteExecutor = class(TAbstractCommandExecutor, IDeleteCommand)
  private
    fTable: TSQLTable;
    fCommand: TDeleteCommand;
  protected
    function GetCommand: TDMLCommand; override;
    function GetPrimaryKeyValue(const entity: TObject): TValue;
  public
    constructor Create(const connection: IDBConnection); override;
    destructor Destroy; override;

    procedure Build(entityClass: TClass); override;
    procedure BuildParams(const entity: TObject); override;

    procedure Execute(const entity: TObject);
    procedure ExecuteById(const id: TValue);
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions,
  Spring.Reflection;


{$REGION 'TDeleteCommand'}

constructor TDeleteExecutor.Create(const connection: IDBConnection);
begin
  inherited Create(connection);
  fTable := TSQLTable.Create;
  fCommand := TDeleteCommand.Create(fTable);
end;

destructor TDeleteExecutor.Destroy;
begin
  fCommand.Free;
  fTable.Free;
  inherited Destroy;
end;

procedure TDeleteExecutor.Build(entityClass: TClass);
begin
  inherited Build(entityClass);

  fTable.SetFromAttribute(EntityData.EntityTable);
  fCommand.PrimaryKeyColumnName := EntityData.PrimaryKeyColumn.ColumnName;
  SQL := Generator.GenerateDelete(fCommand);
end;

procedure TDeleteExecutor.BuildParams(const entity: TObject);
var
  param: TDBParam;
  whereField: TSQLWhereField;
begin
  inherited BuildParams(entity);

  for whereField in fCommand.WhereFields do
  begin
    param := Generator.CreateParam(whereField, GetPrimaryKeyValue(entity));
    SQLParameters.Add(param);
  end;
end;

procedure TDeleteExecutor.Execute(const entity: TObject);
begin
  ExecuteById(GetPrimaryKeyValue(entity));
end;

procedure TDeleteExecutor.ExecuteById(const id: TValue);
var
  statement: IDBStatement;
  field: TSQLWhereField;
  param: TDBParam;
begin
  statement := Connection.CreateStatement;
  statement.SetSQLCommand(SQL);
  SQLParameters.Clear;
  field := fCommand.WhereFields.First;

  param := Generator.CreateParam(field, id);
  SQLParameters.Add(param);
  statement.SetParams(SQLParameters);
  statement.Execute;
end;

function TDeleteExecutor.GetCommand: TDMLCommand;
begin
  Result := fCommand;
end;

function TDeleteExecutor.GetPrimaryKeyValue(const entity: TObject): TValue;
begin
  Assert(EntityData.PrimaryKeyColumn <> nil);
  Result := EntityData.PrimaryKeyColumn.Member.GetValue(entity);
end;

{$ENDREGION}


end.
