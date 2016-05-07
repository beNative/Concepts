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

unit Spring.Persistence.Adapters.FireDAC;

interface

uses
  DB,
  FireDAC.DApt,
  FireDAC.Comp.Client,
  FireDAC.Stan.Async,
  FireDAC.Stan.Def,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  SysUtils,
  Spring.Collections,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Params;

type
  EFireDACAdapterException = class(EORMAdapterException);

  TFireDACResultSetAdapter = class(TDriverResultSetAdapter<TFDQuery>);

  TFireDACStatementAdapter = class(TDriverStatementAdapter<TFDQuery>)
  public
    destructor Destroy; override;
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParam(const param: TDBParam); virtual;
    procedure SetParams(const params: IEnumerable<TDBParam>); override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  TFireDACConnectionAdapter = class(TDriverConnectionAdapter<TFDConnection>)
  protected
    constructor Create(const connection: TFDConnection;
      const exceptionHandler: IORMExceptionHandler); override;
  public
    constructor Create(const connection: TFDConnection); override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
  end;

  TFireDACTransactionAdapter = class(TDriverTransactionAdapter<TFDTransaction>)
  private
    fOwnsObject: Boolean;
  protected
    function InTransaction: Boolean; override;
  public
    constructor Create(const transaction: TFDTransaction;
      const exceptionHandler: IORMExceptionHandler;
      ownsObject: Boolean = False); reintroduce;
    destructor Destroy; override;
    procedure Commit; override;
    procedure Rollback; override;
  end;

  TFireDACExceptionHandler = class(TORMExceptionHandler)
  protected
    function GetAdapterException(const exc: Exception;
      const defaultMsg: string): Exception; override;
  end;

implementation

uses
  StrUtils,
  Variants,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.ResourceStrings;


{$REGION 'TFireDACStatementAdapter'}

destructor TFireDACStatementAdapter.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  Statement.Free;
{$ELSE}
  Statement.DisposeOf;
{$ENDIF}
  inherited Destroy;
end;

function TFireDACStatementAdapter.Execute: NativeUInt;
begin
  inherited;
  try
    Statement.ExecSQL;
    Result := Statement.RowsAffected;
  except
    raise HandleException;
  end
end;

function TFireDACStatementAdapter.ExecuteQuery(
  serverSideCursor: Boolean): IDBResultSet;
var
  query: TFDQuery;
begin
  inherited;
  query := TFDQuery.Create(nil);
  query.Connection := Statement.Connection;
  query.SQL.Text := Statement.SQL.Text;
  query.Params.AssignValues(Statement.Params);
  query.DisableControls;
  if AllowServerSideCursor and serverSideCursor then
    query.FetchOptions.CursorKind := ckForwardOnly;
  try
    query.OpenOrExecute;
    Result := TFireDACResultSetAdapter.Create(query, ExceptionHandler);
  except
    on E:Exception do
    begin
      query.Free;
      raise HandleException(Format(SCannotOpenQuery, [E.Message]));
    end;
  end;
end;

procedure TFireDACStatementAdapter.SetParam(const param: TDBParam);
var
  paramName: string;
  parameter: TFDParam;
begin
  paramName := param.GetNormalizedParamName;
  parameter := Statement.ParamByName(paramName);
  parameter.DataType := param.ParamType;
  parameter.Value := param.ToVariant;
end;

procedure TFireDACStatementAdapter.SetParams(const params: IEnumerable<TDBParam>);
begin
  inherited;
  params.ForEach(SetParam);
end;

procedure TFireDACStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  Statement.SQL.Text := commandText;
end;

{$ENDREGION}


{$REGION 'TFireDACConnectionAdapter'}

constructor TFireDACConnectionAdapter.Create(const connection: TFDConnection);
begin
  Create(connection, TFireDACExceptionHandler.Create);
end;

constructor TFireDACConnectionAdapter.Create(const connection: TFDConnection;
  const exceptionHandler: IORMExceptionHandler);
begin
  inherited Create(connection, exceptionHandler);
  Connection.LoginPrompt := False;
end;

function TFireDACConnectionAdapter.BeginTransaction: IDBTransaction;
var
  transaction: TFDTransaction;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := True;
    if not Connection.InTransaction or Connection.TxOptions.EnableNested then
    begin
      transaction := TFDTransaction.Create(nil);
      transaction.Connection := Connection;
      transaction.StartTransaction;
      Result := TFireDACTransactionAdapter.Create(transaction, ExceptionHandler,
        True);
    end
    else
      raise EFireDACAdapterException.Create('Transaction already started, and EnableNested transaction is false');
  except
    raise HandleException;
  end
  else
    Result := nil;
end;

procedure TFireDACConnectionAdapter.Connect;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := True;
  except
    raise HandleException;
  end;
end;

function TFireDACConnectionAdapter.CreateStatement: IDBStatement;
var
  statement: TFDQuery;
  adapter: TFireDACStatementAdapter;
begin
  if Assigned(Connection) then
  begin
    statement := TFDQuery.Create(nil);
    statement.Connection := Connection;

    adapter := TFireDACStatementAdapter.Create(statement, ExceptionHandler);
    adapter.ExecutionListeners := ExecutionListeners;
    adapter.AllowServerSideCursor := AllowServerSideCursor;
    Result := adapter;
  end
  else
    Result := nil;
end;

destructor TFireDACConnectionAdapter.Destroy;
begin
{$IFDEF AUTOREFCOUNT}
  if AutoFreeConnection then
    Connection.DisposeOf;
{$ENDIF}
  inherited;
end;

procedure TFireDACConnectionAdapter.Disconnect;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := False;
  except
    raise HandleException;
  end;
end;

function TFireDACConnectionAdapter.IsConnected: Boolean;
begin
  Result := Assigned(Connection) and Connection.Connected;
end;

{$ENDREGION}


{$REGION 'TFireDACTransactionAdapter'}

constructor TFireDACTransactionAdapter.Create(const transaction: TFDTransaction;
  const exceptionHandler: IORMExceptionHandler; ownsObject: Boolean);
begin
  inherited Create(transaction, exceptionHandler);
  fOwnsObject := ownsObject
end;

destructor TFireDACTransactionAdapter.Destroy;
begin
  try
    inherited Destroy;
  finally
    if fOwnsObject then
    begin
{$IFDEF NEXTGEN}
      fTransaction.DisposeOf;
{$ENDIF}
      fTransaction.Free;
    end;
  end;
end;

procedure TFireDACTransactionAdapter.Commit;
begin
  if Assigned(Transaction) then
  try
    Transaction.Commit;
  except
    raise HandleException;
  end;
end;

function TFireDACTransactionAdapter.InTransaction: Boolean;
begin
  Result := Assigned(Transaction) and Transaction.Active;
end;

procedure TFireDACTransactionAdapter.Rollback;
begin
  if Assigned(Transaction) then
  try
    Transaction.Rollback;
  except
    raise HandleException;
  end;
end;

{$ENDREGION}


{$REGION 'TFireDACExceptionHandler'}

function TFireDACExceptionHandler.GetAdapterException(const exc: Exception;
  const defaultMsg: string): Exception;
begin
  if exc is EFDDBEngineException then
    with EFDDBEngineException(exc) do
  begin
    case Kind of
      ekUKViolated,
      ekFKViolated :
        Result := EORMConstraintException.Create(defaultMsg, ErrorCode);
      else
        Result := EFireDACAdapterException.Create(defaultMsg, ErrorCode);
    end;
  end
  else if exc is EDatabaseError then
    Result := EFireDACAdapterException.Create(defaultMsg)
  else
    Result := nil;
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TFireDACConnectionAdapter>(dtFireDAC);

end.
