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

unit Spring.Persistence.SQL.Commands.Select;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Represents <c>select</c> executor. Responsible for building and
  ///   executing <c>select</c> statements.
  /// </summary>
  TSelectExecutor = class(TAbstractCommandExecutor, ISelectCommand)
  private
    fTable: TSQLTable;
    fCommand: TSelectCommand;
    fColumns: IList<ColumnAttribute>;
    fID: TValue;
    fSelectColumn: ColumnAttribute;
    fForeignEntityClass: TClass;
  protected
    function GetCommand: TDMLCommand; override;
    function ShouldFetchFromOneColumn: Boolean;
  public
    constructor Create(const connection: IDBConnection); overload; override;
    constructor Create(const connection: IDBConnection; const id: TValue;
      foreignEntityClass: TClass;
      const selectColumn: ColumnAttribute); reintroduce; overload;
    destructor Destroy; override;

    procedure Build(entityClass: TClass); override;
    procedure BuildParams(const entity: TObject); override;

    function Select: IDBResultSet;
    function SelectAll: IDBResultSet;
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.SQL.Params;


{$REGION 'TSelectCommand'}

constructor TSelectExecutor.Create(const connection: IDBConnection);
begin
  inherited Create(connection);
  fColumns := TCollections.CreateList<ColumnAttribute>;
  fTable := TSQLTable.Create;
  fCommand := TSelectCommand.Create(fTable);
end;

constructor TSelectExecutor.Create(const connection: IDBConnection;
  const id: TValue; foreignEntityClass: TClass;
  const selectColumn: ColumnAttribute);
begin
  Create(connection);
  fID := id;
  fForeignEntityClass := foreignEntityClass;
  fSelectColumn := selectColumn;
end;

destructor TSelectExecutor.Destroy;
begin
  fCommand.Free;
  fTable.Free;
  inherited Destroy;
end;

procedure TSelectExecutor.Build(entityClass: TClass);
begin
  inherited Build(entityClass);

  fTable.SetFromAttribute(EntityData.EntityTable);

  if ShouldFetchFromOneColumn then
    fColumns.Add(fSelectColumn)
  else
    fColumns.AddRange(EntityData.SelectColumns);

  fCommand.PrimaryKeyColumn := EntityData.PrimaryKeyColumn;
  fCommand.SetCommandFieldsFromColumns(fColumns);
  fCommand.SetAssociations(entityClass);
end;

procedure TSelectExecutor.BuildParams(const entity: TObject);
var
  param: TDBParam;
  whereField: TSQLWhereField;
begin
  Assert(not Assigned(entity), 'Entity should not be assigned here');
  inherited BuildParams(entity);

  for whereField in fCommand.WhereFields do
  begin
    param := Generator.CreateParam(whereField, fID);
    SQLParameters.Add(param);
  end;
end;

function TSelectExecutor.GetCommand: TDMLCommand;
begin
  Result := fCommand;
end;

function TSelectExecutor.Select: IDBResultSet;
var
  statement: IDBStatement;
begin
  fCommand.WhereFields.Clear;

  if Assigned(fForeignEntityClass) then
    fCommand.SetFromForeignColumn(EntityData, fForeignEntityClass)
  else
    fCommand.SetFromPrimaryColumn;

  SQL := Generator.GenerateSelect(fCommand);
  statement := Connection.CreateStatement;
  statement.SetSQLCommand(SQL);
  BuildParams(nil);
  statement.SetParams(SQLParameters);
  Result := statement.ExecuteQuery;
end;

function TSelectExecutor.SelectAll: IDBResultSet;
var
  statement: IDBStatement;
begin
  fCommand.WhereFields.Clear;

  SQL := Generator.GenerateSelect(fCommand);
  statement := Connection.CreateStatement;
  statement.SetSQLCommand(SQL);
  Result := statement.ExecuteQuery;
end;

function TSelectExecutor.ShouldFetchFromOneColumn: Boolean;
begin
  Result := Assigned(fSelectColumn);
end;

{$ENDREGION}


end.
