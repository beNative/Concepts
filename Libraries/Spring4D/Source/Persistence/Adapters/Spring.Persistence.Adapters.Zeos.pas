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

unit Spring.Persistence.Adapters.Zeos;

interface

uses
  SysUtils,
  ZAbstractConnection,
  ZAbstractDataset,
  ZDataset,
  Spring.Collections,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Params;

type
  EZeosAdapterException = class(EORMAdapterException);

  /// <summary>
  ///   Represents Zeos resultset.
  /// </summary>
  TZeosResultSetAdapter = class(TDriverResultSetAdapter<TZAbstractDataset>);

  /// <summary>
  ///   Represents Zeos statement.
  /// </summary>
  TZeosStatementAdapter = class(TDriverStatementAdapter<TZQuery>)
  public
    destructor Destroy; override;

    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParam(const param: TDBParam); virtual;
    procedure SetParams(const params: IEnumerable<TDBParam>); override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  /// <summary>
  ///   Represents Zeos connection.
  /// </summary>
  TZeosConnectionAdapter = class(TDriverConnectionAdapter<TZAbstractConnection>)
  protected
    constructor Create(const connection: TZAbstractConnection;
      const exceptionHandler: IORMExceptionHandler); override;
  public
    constructor Create(const connection: TZAbstractConnection); override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
  end;

  /// <summary>
  ///   Represents Zeos transaction.
  /// </summary>
  TZeosTransactionAdapter = class(TDriverTransactionAdapter<TZAbstractConnection>)
  protected
    function InTransaction: Boolean; override;
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

  TZeosExceptionHandler = class(TORMExceptionHandler)
  protected
    function GetAdapterException(const exc: Exception;
      const defaultMsg: string): Exception; override;
  end;

implementation

uses
  DB,
  StrUtils,
  Spring,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.ResourceStrings;


{$REGION 'TZeosStatementAdapter' }

destructor TZeosStatementAdapter.Destroy;
begin
  Statement.Free;
  inherited Destroy;
end;

function TZeosStatementAdapter.Execute: NativeUInt;
begin
  inherited;
  try
    Statement.ExecSQL;
    Result := Statement.RowsAffected;
  except
    raise HandleException;
  end;
end;

function TZeosStatementAdapter.ExecuteQuery(
  serverSideCursor: Boolean): IDBResultSet;
var
  query: TZQuery;
begin
  inherited;
  query := TZQuery.Create(nil);
  query.Connection := Statement.Connection;
  query.SQL.Text := Statement.SQL.Text;
  query.Params.AssignValues(Statement.Params);
  query.DisableControls;
  try
    // if you get an EZSQLException with message 'Can not open a Resultset'.
    // apply ZDbcMySqlStatement.diff and ZDbcStatement.diff (based on 7.1.4-stable)
    query.Open;
    Result := TZeosResultSetAdapter.Create(query, ExceptionHandler);
  except
    on E: Exception do
    begin
      query.Free;
      raise HandleException(Format(SCannotOpenQuery, [E.Message]));
    end;
  end;
end;

procedure TZeosStatementAdapter.SetParam(const param: TDBParam);
var
  paramName: string;
begin
  paramName := param.Name;
  if StartsStr(':', param.Name) then
    paramName := Copy(param.Name, 2, Length(param.Name));

  if param.Value.IsType<TGUID> then
    Statement.Params.ParamByName(paramName).AsBytes := param.Value.ToType<TArray<Byte>>
  else
    Statement.Params.ParamValues[paramName] := param.ToVariant;
end;

procedure TZeosStatementAdapter.SetParams(const params: IEnumerable<TDBParam>);
begin
  inherited;
  params.ForEach(SetParam);
end;

procedure TZeosStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  Statement.SQL.Text := commandText;
end;

{$ENDREGION}


{$REGION 'TZeosConnectionAdapter'}

function TZeosConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Assigned(Connection) then
  try
    if not Connection.Ping then
      Connection.Reconnect;

    if not Connection.InTransaction then
      Connection.StartTransaction;

    Result := TZeosTransactionAdapter.Create(Connection, ExceptionHandler);
  except
    raise HandleException;
  end
  else
    Result := nil;
end;

procedure TZeosConnectionAdapter.Connect;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := True;
  except
    raise HandleException;
  end;
end;

constructor TZeosConnectionAdapter.Create(
  const connection: TZAbstractConnection);
begin
  Create(connection, TZeosExceptionHandler.Create);
end;

constructor TZeosConnectionAdapter.Create(
  const connection: TZAbstractConnection;
  const exceptionHandler: IORMExceptionHandler);
begin
  inherited Create(connection, exceptionHandler);
  Connection.LoginPrompt := False;
  if StartsText('mysql', Connection.Protocol) then
    if Connection.Properties.IndexOfName('CLIENT_MULTI_STATEMENTS') < 0 then
      Connection.Properties.AddPair('CLIENT_MULTI_STATEMENTS', 'TRUE');
end;

function TZeosConnectionAdapter.CreateStatement: IDBStatement;
var
  statement: TZQuery;
  adapter: TZeosStatementAdapter;
begin
  if Assigned(Connection) then
  begin
    statement := TZQuery.Create(nil);
    statement.Connection := Connection;

    adapter := TZeosStatementAdapter.Create(statement, ExceptionHandler);
    adapter.ExecutionListeners := ExecutionListeners;
    Result := adapter;
  end
  else
    Result := nil;
end;

procedure TZeosConnectionAdapter.Disconnect;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := False;
  except
    raise HandleException;
  end;
end;

function TZeosConnectionAdapter.IsConnected: Boolean;
begin
  Result := Assigned(Connection) and Connection.Connected;
end;

{$ENDREGION}


{$REGION 'TZeosTransactionAdapter'}

procedure TZeosTransactionAdapter.Commit;
begin
  if Assigned(Transaction) then
  try
    Transaction.Commit;
  except
    raise HandleException;
  end;
end;

function TZeosTransactionAdapter.InTransaction: Boolean;
begin
  Result := Assigned(Transaction) and Transaction.InTransaction;
end;

procedure TZeosTransactionAdapter.Rollback;
begin
  if Assigned(Transaction) then
  try
    Transaction.Rollback;
  except
    raise HandleException;
  end;
end;

{$ENDREGION}


{$REGION 'TZeosExceptionHandler'}

function TZeosExceptionHandler.GetAdapterException(const exc: Exception;
  const defaultMsg: string): Exception;
begin
  if exc is EDatabaseError then
    Result := EZeosAdapterException.Create(defaultMsg)
  else
    Result := nil;
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TZeosConnectionAdapter>(dtZeos);

end.
