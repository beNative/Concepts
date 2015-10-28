{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

unit Spring.Persistence.Adapters.SQLite;

interface

uses
  SQLiteTable3,
  Spring.Collections,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Params;

type
  ESQLiteStatementAdapterException = class(EORMAdapterException);

  /// <summary>
  ///   Represents SQLite3 resultset.
  /// </summary>
  TSQLiteResultSetAdapter = class(TDriverResultSetAdapter<ISQLiteTable>)
  public
    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function FieldExists(const fieldName: string): Boolean; override;
    function GetFieldValue(index: Integer): Variant; overload; override;
    function GetFieldValue(const fieldName: string): Variant; overload; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(index: Integer): string; override;
  end;

  /// <summary>
  ///   Represents SQLite3 statement.
  /// </summary>
  TSQLiteStatementAdapter = class(TDriverStatementAdapter<ISQLitePreparedStatement>)
  public
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParam(const param: TDBParam); virtual;
    procedure SetParams(const params: IEnumerable<TDBParam>); overload; override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  /// <summary>
  ///   Represents SQLite3 connection.
  /// </summary>
  TSQLiteConnectionAdapter = class(TDriverConnectionAdapter<TSQLiteDatabase>)
  public
    procedure AfterConstruction; override;
    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
  end;

  /// <summary>
  ///   Represents SQLite3 transaction.
  /// </summary>
  TSQLiteTransactionAdapter = class(TDriverTransactionAdapter<TSQLiteDatabase>)
  protected
    function InTransaction: Boolean; override;
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Consts,
  Spring.Persistence.SQL.Generators.SQLite3,
  Spring.Persistence.SQL.Interfaces;


{$REGION 'TSQLiteResultSetAdapter'}

function TSQLiteResultSetAdapter.GetFieldValue(index: Integer): Variant;
begin
  Result := DataSet.Fields[index].Value;
end;

function TSQLiteResultSetAdapter.FieldExists(const fieldName: string): Boolean;
begin
  Result := DataSet.FindField(fieldName) <> nil;
end;

function TSQLiteResultSetAdapter.GetFieldCount: Integer;
begin
  Result := DataSet.FieldCount;
end;

function TSQLiteResultSetAdapter.GetFieldName(index: Integer): string;
begin
  Result := DataSet.Fields[index].Name;
end;

function TSQLiteResultSetAdapter.GetFieldValue(const fieldName: string): Variant;
begin
  Result := DataSet.FieldByName[fieldName].Value;
end;

function TSQLiteResultSetAdapter.IsEmpty: Boolean;
begin
  Result := DataSet.EOF;
end;

function TSQLiteResultSetAdapter.Next: Boolean;
begin
  Result := DataSet.Next;
end;

{$ENDREGION}


{$REGION 'TSQLiteStatementAdapter'}

function TSQLiteStatementAdapter.Execute: NativeUInt;
var
  affectedRows: Integer;
begin
  inherited;
  if Statement.ExecSQL(affectedRows) then
    Result := affectedRows
  else
    Result := 0;
end;

function TSQLiteStatementAdapter.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
var
  query: ISQLiteTable;
begin
  inherited;
  query := Statement.ExecQueryIntf;
  Result := TSQLiteResultSetAdapter.Create(query);
end;

procedure TSQLiteStatementAdapter.SetParam(const param: TDBParam);
begin
  Statement.SetParamVariant(param.Name, param.ToVariant);
end;

procedure TSQLiteStatementAdapter.SetParams(const params: IEnumerable<TDBParam>);
begin
  inherited;
  params.ForEach(SetParam);
end;

procedure TSQLiteStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  Statement.PrepareStatement(commandText);
end;

{$ENDREGION}


{$REGION 'TSQLiteConnectionAdapter'}

procedure TSQLiteConnectionAdapter.AfterConstruction;
begin
  inherited;
  QueryLanguage := qlSQLite;
end;

function TSQLiteConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Assigned(Connection) then
  begin
    Connection.Connected := true;
    inherited;
    Connection.ExecSQL(SQL_BEGIN_SAVEPOINT + GetTransactionName);

    Result := TSQLiteTransactionAdapter.Create(Connection);
    Result.TransactionName := GetTransactionName;
  end;
end;

procedure TSQLiteConnectionAdapter.Connect;
begin
  if Assigned(Connection) then
    Connection.Connected := True;
end;

function TSQLiteConnectionAdapter.CreateStatement: IDBStatement;
var
  statement: TSQLitePreparedStatement;
  adapter: TSQLiteStatementAdapter;
begin
  if Assigned(Connection) then
  begin
    statement := TSQLitePreparedStatement.Create(Connection);
    adapter := TSQLiteStatementAdapter.Create(statement);
    adapter.ExecutionListeners := ExecutionListeners;
    Result := adapter;
  end
  else
    Result := nil;
end;

procedure TSQLiteConnectionAdapter.Disconnect;
begin
  if Assigned(Connection) then
    Connection.Connected := False;
end;

function TSQLiteConnectionAdapter.IsConnected: Boolean;
begin
  Result := Assigned(Connection) and Connection.Connected;
end;

{$ENDREGION}


{$REGION 'TSQLiteTransactionAdapter'}

procedure TSQLiteTransactionAdapter.Commit;
begin
  if Assigned(Transaction) then
    Transaction.ExecSQL('RELEASE SAVEPOINT ' + TransactionName);
end;

function TSQLiteTransactionAdapter.InTransaction: Boolean;
begin
  Result := Assigned(Transaction) and Transaction.IsTransactionOpen;
end;

procedure TSQLiteTransactionAdapter.Rollback;
begin
  if Assigned(Transaction) then
    Transaction.ExecSQL('ROLLBACK TRANSACTION TO SAVEPOINT ' + TransactionName);
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TSQLiteConnectionAdapter>(dtSQLite);

end.
