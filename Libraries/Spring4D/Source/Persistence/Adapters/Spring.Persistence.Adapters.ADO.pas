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

unit Spring.Persistence.Adapters.ADO;

interface

{$IFDEF MSWINDOWS}
uses
  ADODB,
  ComObj,
  SysUtils,
  Spring.Collections,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.Mapping.Attributes;

type
  EADOAdapterException = class(EORMAdapterException);

  /// <summary>
  ///   Represent ADO resultset.
  /// </summary>
  TADOResultSetAdapter = class(TDriverResultSetAdapter<TADODataSet>);

  /// <summary>
  ///   Represent ADO statement.
  /// </summary>
  TADOStatementAdapter = class(TDriverStatementAdapter<TADOQuery>)
  public
    destructor Destroy; override;
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParam(const param: TDBParam); virtual;
    procedure SetParams(const params: IEnumerable<TDBParam>); override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  /// <summary>
  ///   Represent ADO connection.
  /// </summary>
  TADOConnectionAdapter = class(TDriverConnectionAdapter<TADOConnection>)
  protected
    constructor Create(const connection: TADOConnection;
      const exceptionHandler: IORMExceptionHandler); override;
  public
    constructor Create(const connection: TADOConnection); override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
  end;

  /// <summary>
  ///   Represent ADO transaction.
  /// </summary>
  TADOTransactionAdapter = class(TDriverTransactionAdapter<TADOConnection>)
  protected
    function InTransaction: Boolean; override;
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

  /// <summary>
  ///   Represent ADO SQL generator.
  /// </summary>
  TADOSQLGenerator = class(TAnsiSQLGenerator)
  public
    function GenerateGetLastInsertId(const identityColumn: ColumnAttribute): string; override;
  end;

  TADOExceptionHandler = class(TORMExceptionHandler)
  protected
    function GetAdapterException(const exc: Exception;
      const defaultMsg: string): Exception; override;
    function GetDriverException(const exc: EOleSysError;
      const defaultMsg: string): Exception; virtual;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  DB,
  Variants,
  Spring,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.ResourceStrings;


{$REGION 'TADOStatementAdapter'}

destructor TADOStatementAdapter.Destroy;
begin
  Statement.Free;
  inherited Destroy;
end;

function TADOStatementAdapter.Execute: NativeUInt;
begin
  inherited;
  try
    Result := Statement.ExecSQL;
  except
    raise HandleException;
  end;
end;

function TADOStatementAdapter.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
var
  dataSet: TADODataSet;
begin
  inherited;
  dataSet := TADODataSet.Create(nil);
//  if AServerSideCursor then
//    LStmt.CursorLocation := clUseServer;
  dataSet.CursorType := ctOpenForwardOnly;
  dataSet.CacheSize := 50;
  dataSet.Connection := Statement.Connection;
  dataSet.CommandText := Statement.SQL.Text;
  dataSet.Parameters.AssignValues(Statement.Parameters);
  dataSet.DisableControls;
  try
    dataSet.Open;
    Result := TADOResultSetAdapter.Create(dataSet, ExceptionHandler);
  except
    on E: Exception do
    begin
      dataSet.Free;
      raise HandleException(Format(SCannotOpenQuery, [E.Message]));
    end;
  end;
end;

procedure TADOStatementAdapter.SetParam(const param: TDBParam);
var
  paramName: string;
  parameter: TParameter;
begin
  paramName := param.GetNormalizedParamName;
  parameter := Statement.Parameters.ParamByName(paramName);
  parameter.Value := param.ToVariant;
  if VarIsNull(parameter.Value) or VarIsEmpty(parameter.Value) then
    parameter.DataType := param.ParamType;
end;

procedure TADOStatementAdapter.SetParams(const params: IEnumerable<TDBParam>);
begin
  inherited;
  params.ForEach(SetParam);
end;

procedure TADOStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  Statement.SQL.Text := commandText;
end;

{$ENDREGION}


{$REGION 'TADOConnectionAdapter'}

function TADOConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := True;
    if not Connection.InTransaction then
      Connection.BeginTrans;
    Result := TADOTransactionAdapter.Create(Connection, ExceptionHandler);
  except
    raise HandleException;
  end
  else
    Result := nil;
end;

procedure TADOConnectionAdapter.Connect;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := True;
  except
    raise HandleException;
  end;
end;

constructor TADOConnectionAdapter.Create(const connection: TADOConnection);
begin
  Create(connection, TADOExceptionHandler.Create);
end;

constructor TADOConnectionAdapter.Create(const connection: TADOConnection;
  const exceptionHandler: IORMExceptionHandler);
begin
  inherited Create(connection, exceptionHandler);
  Connection.LoginPrompt := False;
end;

function TADOConnectionAdapter.CreateStatement: IDBStatement;
var
  statement: TADOQuery;
  adapter: TADOStatementAdapter;
begin
  if Assigned(Connection) then
  begin
    statement := TADOQuery.Create(nil);
    statement.Connection := Connection;

    adapter := TADOStatementAdapter.Create(statement, ExceptionHandler);
    adapter.ExecutionListeners := ExecutionListeners;
    Result := adapter;
  end
  else
    Result := nil;
end;

procedure TADOConnectionAdapter.Disconnect;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := False;
  except
    raise HandleException;
  end;
end;

function TADOConnectionAdapter.IsConnected: Boolean;
begin
  if Assigned(Connection) then
  try
    Result := Connection.Connected
  except
    raise HandleException;
  end
  else
    Result := False;
end;

{$ENDREGION}


{$REGION 'TADOTransactionAdapter'}

procedure TADOTransactionAdapter.Commit;
begin
  if Assigned(Transaction) then
  try
    Transaction.CommitTrans;
  except
    raise HandleException;
  end;
end;

function TADOTransactionAdapter.InTransaction: Boolean;
begin
  if Assigned(Transaction) then
    Result := Transaction.InTransaction
  else
    Result := False;
end;

procedure TADOTransactionAdapter.Rollback;
begin
  if Assigned(Transaction) then
  try
    Transaction.RollbackTrans;
  except
    raise HandleException;
  end;
end;

{$ENDREGION}


{$REGION 'TADOSQLGenerator'}

function TADOSQLGenerator.GenerateGetLastInsertId(
  const identityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

{$ENDREGION}


{$REGION 'TADOExceptionHandler'}

function TADOExceptionHandler.GetAdapterException(const exc: Exception;
  const defaultMsg: string): Exception;
begin
  // All ADO calls should be safecalls which means we can solely rely on OLE
  // exception handling.
  if exc is EOleSysError then
  begin
    // OLE specific exception
    Result := GetDriverException(EOleSysError(exc), defaultMsg);
    if not Assigned(Result) then
      Result := EADOAdapterException.Create(defaultMsg,
        EOleSysError(exc).ErrorCode);
  end
  else if exc is EDatabaseError then
    Result := EADOAdapterException.Create(defaultMsg)
  else if exc is ESafecallException then // Safecall excpetion handler is not set
    Result := EADOAdapterException.Create(defaultMsg)
  else
    Result := nil;
end;

function TADOExceptionHandler.GetDriverException(
  const exc: EOleSysError; const defaultMsg: string): Exception;
begin
  Result := nil;
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TADOConnectionAdapter>(dtADO);
{$ENDIF}

end.
