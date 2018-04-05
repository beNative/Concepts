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

unit Spring.Persistence.Adapters.SQLite;

interface

uses
  SysUtils,
  SQLiteTable3,
  Spring.Collections,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Params;

type
  ESQLiteAdapterException = class(EORMAdapterException);

  /// <summary>
  ///   Represents SQLite3 resultset.
  /// </summary>
  TSQLiteResultSetAdapter = class(TDriverAdapterBase, IDBResultSet)
  private
    fDataSet: ISQLiteTable;
  public
    constructor Create(const dataSet: ISQLiteTable;
      const exceptionHandler: IORMExceptionHandler);

    function IsEmpty: Boolean;
    function Next: Boolean;
    function FieldExists(const fieldName: string): Boolean;
    function GetFieldValue(index: Integer): Variant; overload;
    function GetFieldValue(const fieldName: string): Variant; overload;
    function GetFieldCount: Integer;
    function GetFieldName(index: Integer): string;
  end;

  /// <summary>
  ///   Represents SQLite3 statement.
  /// </summary>
  TSQLiteStatementAdapter = class(TDriverStatementAdapter<ISQLitePreparedStatement>)
  public
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParam(const param: TDBParam); virtual;
    procedure SetParams(const params: IEnumerable<TDBParam>); override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  /// <summary>
  ///   Represents SQLite3 connection.
  /// </summary>
  TSQLiteConnectionAdapter = class(TDriverConnectionAdapter<TSQLiteDatabase>)
  public
    constructor Create(const connection: TSQLiteDatabase); override;
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

  TSQLiteExceptionHandler = class(TORMExceptionHandler)
  protected
    function GetAdapterException(const exc: Exception;
      const defaultMsg: string): Exception; override;
  end;

implementation

uses
  DB,
  Variants,
  Spring,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.ResourceStrings,
  Spring.Persistence.SQL.Generators.SQLite3,
  Spring.Persistence.SQL.Interfaces;


{$REGION 'TSQLiteResultSetAdapter'}

constructor TSQLiteResultSetAdapter.Create(const dataSet: ISQLiteTable;
  const exceptionHandler: IORMExceptionHandler);
begin
  inherited Create(exceptionHandler);
  fDataSet := dataSet;
end;

function TSQLiteResultSetAdapter.GetFieldValue(index: Integer): Variant;
begin
  try
    Result := fDataSet.Fields[index].Value;
  except
    raise HandleException;
  end;
end;

function TSQLiteResultSetAdapter.FieldExists(const fieldName: string): Boolean;
begin
  Result := fDataSet.FindField(fieldName) <> nil;
end;

function TSQLiteResultSetAdapter.GetFieldCount: Integer;
begin
  Result := fDataSet.FieldCount;
end;

function TSQLiteResultSetAdapter.GetFieldName(index: Integer): string;
begin
  Result := fDataSet.Fields[index].Name;
end;

function TSQLiteResultSetAdapter.GetFieldValue(const fieldName: string): Variant;
begin
  try
    Result := fDataSet.FieldByName[fieldName].Value;
  except
    raise HandleException;
  end;
end;

function TSQLiteResultSetAdapter.IsEmpty: Boolean;
begin
  Result := fDataSet.EOF;
end;

function TSQLiteResultSetAdapter.Next: Boolean;
begin
  try
    Result := fDataSet.Next;
  except
    raise HandleException;
  end;
end;

{$ENDREGION}


{$REGION 'TSQLiteStatementAdapter'}

function TSQLiteStatementAdapter.Execute: NativeUInt;
var
  affectedRows: Integer;
begin
  inherited;
  try
    if Statement.ExecSQL(affectedRows) then
      Result := affectedRows
    else
      Result := 0;
  except
    raise HandleException;
  end;
end;

function TSQLiteStatementAdapter.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
var
  query: ISQLiteTable;
begin
  inherited;
  try
    query := Statement.ExecQueryIntf;
  except
    raise HandleException;
  end;
  Result := TSQLiteResultSetAdapter.Create(query, ExceptionHandler);
end;

procedure TSQLiteStatementAdapter.SetParam(const param: TDBParam);
var
  value: Variant;
begin
  value := param.ToVariant;
  if VarIsNull(value) then
    Statement.SetParamNull(param.Name)
  else
    case param.ParamType of
      ftDate: Statement.SetParamDate(param.Name, param.Value.ToType<TDate>);
      ftTime: Statement.SetParamTime(param.Name, param.Value.ToType<TTime>);
      ftDateTime: Statement.SetParamDateTime(param.Name, param.Value.ToType<TDateTime>);
    else
      Statement.SetParamVariant(param.Name, value);
    end;
end;

procedure TSQLiteStatementAdapter.SetParams(const params: IEnumerable<TDBParam>);
begin
  inherited;
  params.ForEach(SetParam);
end;

procedure TSQLiteStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  try
    Statement.PrepareStatement(commandText);
  except
    raise HandleException;
  end;
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
  try
    Connection.Connected := true;
    inherited;
    Connection.ExecSQL(SQL_BEGIN_SAVEPOINT + GetTransactionName);

    Result := TSQLiteTransactionAdapter.Create(Connection, ExceptionHandler);
    Result.TransactionName := GetTransactionName;
  except
    raise HandleException;
  end
  else
    Result := nil;
end;

procedure TSQLiteConnectionAdapter.Connect;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := True;
  except
    raise HandleException;
  end;
end;

constructor TSQLiteConnectionAdapter.Create(const connection: TSQLiteDatabase);
begin
  Create(connection, TSQLiteExceptionHandler.Create);
end;

function TSQLiteConnectionAdapter.CreateStatement: IDBStatement;
var
  statement: TSQLitePreparedStatement;
  adapter: TSQLiteStatementAdapter;
begin
  if Assigned(Connection) then
  begin
    statement := TSQLitePreparedStatement.Create(Connection);
    adapter := TSQLiteStatementAdapter.Create(statement, ExceptionHandler);
    adapter.ExecutionListeners := ExecutionListeners;
    Result := adapter;
  end
  else
    Result := nil;
end;

procedure TSQLiteConnectionAdapter.Disconnect;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := False;
  except
    raise HandleException;
  end;
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
  try
    Transaction.ExecSQL('RELEASE SAVEPOINT ' + TransactionName);
  except
    raise HandleException;
  end;
end;

function TSQLiteTransactionAdapter.InTransaction: Boolean;
begin
  Result := Assigned(Transaction) and Transaction.IsTransactionOpen;
end;

procedure TSQLiteTransactionAdapter.Rollback;
begin
  if Assigned(Transaction) then
  try
    Transaction.ExecSQL('ROLLBACK TRANSACTION TO SAVEPOINT ' + TransactionName);
  except
    raise HandleException;
  end;
end;

{$ENDREGION}


{$REGION 'TSQLiteExceptionHandler'}

function TSQLiteExceptionHandler.GetAdapterException(const exc: Exception;
  const defaultMsg: string): Exception;
begin
  if exc is ESQLiteConstraintException then
    Result := EORMConstraintException.Create(defaultMsg,
      ESQLiteException(exc).ErrorCode)
  else if exc is ESQLiteException then
    Result := ESQLiteAdapterException.Create(defaultMsg,
      ESQLiteException(exc).ErrorCode)
  else
    Result := nil;
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TSQLiteConnectionAdapter>(dtSQLite);

end.
