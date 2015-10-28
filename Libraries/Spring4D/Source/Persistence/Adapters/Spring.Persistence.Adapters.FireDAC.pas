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

  TFireDACResultSetAdapter = class(TDriverResultSetAdapter<TFDQuery>)
  private
    fFieldCache: IFieldCache;
  public
    constructor Create(const dataSet: TFDQuery); override;
    destructor Destroy; override;

    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function FieldExists(const fieldName: string): Boolean; override;
    function GetFieldValue(index: Integer): Variant; overload; override;
    function GetFieldValue(const fieldName: string): Variant; overload; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(index: Integer): string; override;
  end;

  TFireDACStatementAdapter = class(TDriverStatementAdapter<TFDQuery>)
  public
    destructor Destroy; override;
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParam(const param: TDBParam); virtual;
    procedure SetParams(const params: IEnumerable<TDBParam>); overload; override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  TFireDACConnectionAdapter = class(TDriverConnectionAdapter<TFDConnection>)
  public
    constructor Create(const connection: TFDConnection); override;

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
      ownsObject: Boolean = False); reintroduce;
    destructor Destroy; overload; override;
    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  StrUtils,
  Variants,
  Spring.Persistence.Adapters.FieldCache,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Consts;


{$REGION 'TFireDACResultSetAdapter'}

constructor TFireDACResultSetAdapter.Create(const dataSet: TFDQuery);
begin
  inherited Create(DataSet);
  dataSet.DisableControls;
  fFieldCache := TFieldCache.Create(dataSet);
end;

destructor TFireDACResultSetAdapter.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  DataSet.Free;
{$ELSE}
  Dataset.DisposeOf;
{$ENDIF}
  inherited Destroy;
end;

function TFireDACResultSetAdapter.FieldExists(
  const fieldName: string): Boolean;
begin
  Result := fFieldCache.FieldExists(fieldName);
end;

function TFireDACResultSetAdapter.GetFieldCount: Integer;
begin
  Result := DataSet.FieldCount;
end;

function TFireDACResultSetAdapter.GetFieldName(index: Integer): string;
begin
  Result := DataSet.Fields[index].FieldName;
end;

function TFireDACResultSetAdapter.GetFieldValue(index: Integer): Variant;
begin
  Result := DataSet.Fields[index].Value;
end;

function TFireDACResultSetAdapter.GetFieldValue(
  const fieldName: string): Variant;
begin
  Result := fFieldCache.GetFieldValue(fieldName);
end;

function TFireDACResultSetAdapter.IsEmpty: Boolean;
begin
  Result := DataSet.Eof;
end;

function TFireDACResultSetAdapter.Next: Boolean;
begin
  DataSet.Next;
  Result := not DataSet.Eof;
end;

{$ENDREGION}


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
  Statement.ExecSQL;
  Result := Statement.RowsAffected;
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
    Result := TFireDACResultSetAdapter.Create(query);
  except
    on E:Exception do
    begin
      //make sure that resultset is always created to avoid memory leak
      Result := TFireDACResultSetAdapter.Create(query);
      raise EFireDACAdapterException.CreateFmt(EXCEPTION_CANNOT_OPEN_QUERY, [E.Message]);
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
  inherited Create(connection);
  Connection.LoginPrompt := False;
end;

function TFireDACConnectionAdapter.BeginTransaction: IDBTransaction;
var
  transaction: TFDTransaction;
begin
  if Assigned(Connection) then
  begin
    Connection.Connected := True;
    if not Connection.InTransaction or Connection.TxOptions.EnableNested then
    begin
      transaction := TFDTransaction.Create(nil);
      transaction.Connection := Connection;
      transaction.StartTransaction;
      Result := TFireDACTransactionAdapter.Create(transaction, True);
    end
    else
      raise EFireDACAdapterException.Create('Transaction already started, and EnableNested transaction is false');
  end
  else
    Result := nil;
end;

procedure TFireDACConnectionAdapter.Connect;
begin
  if Assigned(Connection) then
    Connection.Connected := True;
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

    adapter := TFireDACStatementAdapter.Create(statement);
    adapter.ExecutionListeners := ExecutionListeners;
    adapter.AllowServerSideCursor := AllowServerSideCursor;
    Result := adapter;
  end
  else
    Result := nil;
end;

procedure TFireDACConnectionAdapter.Disconnect;
begin
  if Assigned(Connection) then
    Connection.Connected := False;
end;

function TFireDACConnectionAdapter.IsConnected: Boolean;
begin
  Result := Assigned(Connection) and Connection.Connected;
end;

{$ENDREGION}


{$REGION 'TFireDACTransactionAdapter'}

constructor TFireDACTransactionAdapter.Create(const transaction: TFDTransaction;
  ownsObject: Boolean);
begin
  inherited Create(transaction);
  fOwnsObject := ownsObject
end;

destructor TFireDACTransactionAdapter.Destroy;
begin
  inherited Destroy;
  if fOwnsObject then
    fTransaction.Free;
end;

procedure TFireDACTransactionAdapter.Commit;
begin
  if Assigned(Transaction) then
    Transaction.Commit;
end;

function TFireDACTransactionAdapter.InTransaction: Boolean;
begin
  Result := Assigned(Transaction) and Transaction.Active;
end;

procedure TFireDACTransactionAdapter.Rollback;
begin
  if Assigned(Transaction) then
    Transaction.Rollback;
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TFireDACConnectionAdapter>(dtFireDAC);

end.
