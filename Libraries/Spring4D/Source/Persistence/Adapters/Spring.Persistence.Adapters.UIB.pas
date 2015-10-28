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

unit Spring.Persistence.Adapters.UIB;

interface

uses
  uib,
  uibdataset,
  uiblib,
  Spring.Collections,
  Spring.Persistence.Adapters.FieldCache,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Params;

type
  EUIBAdapterException = class(EORMAdapterException);

  /// <summary>
  ///   Represents Unified Interbase resultset.
  /// </summary>
  TUIBResultSetAdapter = class(TDriverResultSetAdapter<TUIBDataSet>)
  private
    fFieldCache: IFieldCache;
    fIsNewTransaction: Boolean;
  public
    constructor Create(const dataSet: TUIBDataSet); override;
    destructor Destroy; override;

    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function FieldExists(const fieldName: string): Boolean; override;
    function GetFieldValue(index: Integer): Variant; overload; override;
    function GetFieldValue(const fieldname: string): Variant; overload; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(index: Integer): string; override;

    property IsNewTransaction: Boolean read fIsNewTransaction write fIsNewTransaction;
  end;

  /// <summary>
  ///   Represents Unified Interbase statement.
  /// </summary>
  TUIBStatementAdapter = class(TDriverStatementAdapter<TUIBStatement>)
  protected
    procedure AssignParams(const source, target: TSQLParams); virtual;
  public
    destructor Destroy; override;

    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParam(const param: TDBParam);
    procedure SetParams(const params: IEnumerable<TDBParam>); override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  /// <summary>
  ///   Represents Unified Interbase connection.
  /// </summary>
  TUIBConnectionAdapter = class(TDriverConnectionAdapter<TUIBDataBase>)
  public
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
  end;

  /// <summary>
  ///   Represents Unified Interbase transaction.
  /// </summary>
  TUIBTransactionAdapter = class(TDriverTransactionAdapter<TUIBTransaction>)
  protected
    function InTransaction: Boolean; override;
  public
    constructor Create(const transaction: TUIBTransaction); override;
    destructor Destroy; override;

    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  DB,
  StrUtils,
  SysUtils,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Consts,
  Spring.Persistence.SQL.Generators.Firebird,
  Spring.Persistence.SQL.Interfaces;


{$REGION 'TUIBResultSetAdapter'}

constructor TUIBResultSetAdapter.Create(const dataSet: TUIBDataSet);
begin
  inherited Create(dataSet);
  Dataset.OnClose := etmStayIn;
  fFieldCache := TFieldCache.Create(dataSet);
end;

destructor TUIBResultSetAdapter.Destroy;
begin
  if fIsNewTransaction then
    DataSet.Transaction.Free;
  DataSet.Free;
  inherited;
end;

function TUIBResultSetAdapter.FieldExists(const fieldName: string): Boolean;
begin
  Result := fFieldCache.FieldExists(fieldName);
end;

function TUIBResultSetAdapter.GetFieldCount: Integer;
begin
  Result := DataSet.FieldCount;
end;

function TUIBResultSetAdapter.GetFieldName(index: Integer): string;
begin
  Result := DataSet.Fields[index].FieldName;
end;

function TUIBResultSetAdapter.GetFieldValue(index: Integer): Variant;
begin
  Result := DataSet.Fields[index].Value;
end;

function TUIBResultSetAdapter.GetFieldValue(const fieldname: string): Variant;
begin
  Result := fFieldCache.GetFieldValue(fieldname);
end;

function TUIBResultSetAdapter.IsEmpty: Boolean;
begin
  Result := DataSet.Eof;
end;

function TUIBResultSetAdapter.Next: Boolean;
begin
  DataSet.Next;
  Result := not DataSet.Eof;
end;

{$ENDREGION}


{$REGION 'TUIBStatementAdapter'}

destructor TUIBStatementAdapter.Destroy;
begin
  Statement.Free;
  inherited Destroy;
end;

procedure TUIBStatementAdapter.AssignParams(const source, target: TSQLParams);
var
  i: Integer;
begin
  for i := 0 to source.ParamCount - 1 do
    target.AsVariant[i] := source.AsVariant[i];
end;

function TUIBStatementAdapter.Execute: NativeUInt;
begin
  inherited;
  Statement.Prepare;
  Statement.ExecSQL;
  Result := Statement.RowsAffected;
  Statement.Close(etmStayIn);
end;

function TUIBStatementAdapter.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
var
  query: TUIBDataSet;
  isNewTransaction: Boolean;
  transaction: TUIBTransaction;
  adapter: TUIBResultSetAdapter;
begin
  inherited;
  query := TUIBDataSet.Create(nil);
  isNewTransaction := Statement.DataBase.TransactionsCount < 1;
  if not isNewTransaction then
    transaction := Statement.DataBase.Transactions[0]
  else
  begin
    transaction := TUIBTransaction.Create(nil);
    transaction.DefaultAction := etmRollback;
    transaction.DataBase := Statement.DataBase;
  end;
  transaction.DefaultAction := etmRollback;
  query.DisableControls;
  query.Transaction := transaction;
  query.Database := Statement.DataBase;
  query.UniDirectional := True;
  query.SQL.Text := Statement.SQL.Text;
  AssignParams(Statement.Params, query.Params);
  try
    query.Open;
    adapter := TUIBResultSetAdapter.Create(query);
    adapter.IsNewTransaction := isNewTransaction;
    Result := adapter;
  except
    on E: Exception do
      raise EUIBAdapterException.CreateFmt(EXCEPTION_CANNOT_OPEN_QUERY, [E.Message]);
  end;
end;

procedure TUIBStatementAdapter.SetParam(const param: TDBParam);
var
  paramName: string;
begin
  paramName := param.Name;
  // strip leading : in param name because UIB does not like them
  if StartsStr(':', param.Name) then
    paramName := Copy(param.Name, 2, Length(param.Name));
  Statement.Params.ByNameAsVariant[paramName] := param.ToVariant;
end;

procedure TUIBStatementAdapter.SetParams(const params: IEnumerable<TDBParam>);
begin
  inherited;
  params.ForEach(SetParam);
end;

procedure TUIBStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  Statement.SQL.Text := commandText;
end;

{$ENDREGION}


{$REGION 'TUIBConnectionAdapter'}

destructor TUIBConnectionAdapter.Destroy;
var
  i: Integer;
begin
  for i := 0 to Connection.TransactionsCount - 1 do
    Connection.Transactions[i].Free;
  inherited Destroy;
end;

procedure TUIBConnectionAdapter.AfterConstruction;
begin
  inherited;
  QueryLanguage := qlFirebird;
end;

function TUIBConnectionAdapter.BeginTransaction: IDBTransaction;
var
  transaction: TUIBTransaction;
begin
  if Assigned(Connection) then
  begin
    Connection.Connected := True;

    transaction := TUIBTransaction.Create(nil);
    transaction.DataBase := Connection;
    transaction.DefaultAction := etmRollback;
    transaction.StartTransaction;

    Result := TUIBTransactionAdapter.Create(transaction);
  end
  else
    Result := nil;
end;

procedure TUIBConnectionAdapter.Connect;
begin
  if Assigned(Connection) then
    Connection.Connected := True;
end;

function TUIBConnectionAdapter.CreateStatement: IDBStatement;
var
  statement: TUIBStatement;
  transaction: TUIBTransaction;
  adapter: TUIBStatementAdapter;
begin
  if Assigned(Connection) then
  begin
    statement := TUIBStatement.Create(nil);
    if Connection.TransactionsCount > 0 then
      transaction := Connection.Transactions[Connection.TransactionsCount - 1]
    else
    begin
      transaction := TUIBTransaction.Create(nil);
      transaction.DefaultAction := etmRollback;
      transaction.DataBase := Connection;
    end;

    statement.DataBase := Connection;
    statement.Transaction := transaction;

    adapter := TUIBStatementAdapter.Create(statement);
    adapter.ExecutionListeners := ExecutionListeners;
    Result := adapter;
  end
  else
    Result := nil;
end;

procedure TUIBConnectionAdapter.Disconnect;
begin
  if Assigned(Connection) then
    Connection.Connected := False;
end;

function TUIBConnectionAdapter.IsConnected: Boolean;
begin
  Result := Assigned(Connection) and Connection.Connected;
end;

{$ENDREGION}


{$REGION 'TUIBTransactionAdapter'}

procedure TUIBTransactionAdapter.Commit;
begin
  if Assigned(fTransaction) then
    fTransaction.Commit;
end;

constructor TUIBTransactionAdapter.Create(const transaction: TUIBTransaction);
begin
  inherited Create(transaction);
  fTransaction.DefaultAction := etmRollback;
  if not InTransaction then
    fTransaction.StartTransaction;
end;

destructor TUIBTransactionAdapter.Destroy;
begin
  fTransaction.Free;
  inherited Destroy;
end;

function TUIBTransactionAdapter.InTransaction: Boolean;
begin
  Result := fTransaction.InTransaction;
end;

procedure TUIBTransactionAdapter.Rollback;
begin
  if Assigned(fTransaction) then
    fTransaction.RollBack;
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TUIBConnectionAdapter>(dtUIB);

end.
