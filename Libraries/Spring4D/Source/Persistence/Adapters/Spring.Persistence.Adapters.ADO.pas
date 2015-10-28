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

unit Spring.Persistence.Adapters.ADO;

interface

{$IFDEF MSWINDOWS}
uses
  ADODB,
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
  TADOResultSetAdapter = class(TDriverResultSetAdapter<TADODataSet>)
  private
    fFieldCache: IFieldCache;
  public
    constructor Create(const dataSet: TADODataSet); override;
    destructor Destroy; override;

    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function FieldExists(const fieldName: string): Boolean; override;
    function GetFieldValue(index: Integer): Variant; overload; override;
    function GetFieldValue(const fieldName: string): Variant; overload; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(index: Integer): string; override;
  end;

  /// <summary>
  ///   Represent ADO statement.
  /// </summary>
  TADOStatementAdapter = class(TDriverStatementAdapter<TADOQuery>)
  public
    constructor Create(const statement: TADOQuery); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParam(const param: TDBParam); virtual;
    procedure SetParams(const params: IEnumerable<TDBParam>); overload; override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  /// <summary>
  ///   Represent ADO connection.
  /// </summary>
  TADOConnectionAdapter = class(TDriverConnectionAdapter<TADOConnection>)
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
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  DB,
  SysUtils,
  Variants,
  Spring,
  Spring.Persistence.Adapters.FieldCache,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Consts;


{$REGION 'TADOResultSetAdapter'}

constructor TADOResultSetAdapter.Create(const dataSet: TADODataSet);
begin
  inherited Create(DataSet);
  DataSet.DisableControls;
//  DataSet.CursorLocation := clUseServer;
//  DataSet.CursorType := ctOpenForwardOnly;
  fFieldCache := TFieldCache.Create(dataSet);
end;

destructor TADOResultSetAdapter.Destroy;
begin
  DataSet.Free;
  inherited Destroy;
end;

function TADOResultSetAdapter.FieldExists(const fieldName: string): Boolean;
begin
  Result := fFieldCache.FieldExists(fieldName);
end;

function TADOResultSetAdapter.GetFieldCount: Integer;
begin
  Result := DataSet.FieldCount;
end;

function TADOResultSetAdapter.GetFieldName(index: Integer): string;
begin
  Result := DataSet.Fields[index].FieldName;
end;

function TADOResultSetAdapter.GetFieldValue(index: Integer): Variant;
begin
  Result := DataSet.Fields[index].Value;
end;

function TADOResultSetAdapter.GetFieldValue(const fieldName: string): Variant;
begin
  Result := fFieldCache.GetFieldValue(fieldName);
end;

function TADOResultSetAdapter.IsEmpty: Boolean;
begin
  Result := DataSet.Eof;
end;

function TADOResultSetAdapter.Next: Boolean;
begin
  DataSet.Next;
  Result := not DataSet.Eof;
end;

{$ENDREGION}


{$REGION 'TADOStatementAdapter'}

constructor TADOStatementAdapter.Create(const statement: TADOQuery);
begin
  inherited Create(statement);
end;

destructor TADOStatementAdapter.Destroy;
begin
  Statement.Free;
  inherited Destroy;
end;

function TADOStatementAdapter.Execute: NativeUInt;
begin
  inherited;
  Result := Statement.ExecSQL;
end;

function TADOStatementAdapter.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
var
  LStmt: TADODataSet;
begin
  inherited;
  LStmt := TADODataSet.Create(nil);
//  if AServerSideCursor then
//    LStmt.CursorLocation := clUseServer;
  LStmt.CursorType := ctOpenForwardOnly;
  LStmt.CacheSize := 50;
  LStmt.Connection := Statement.Connection;
  LStmt.CommandText := Statement.SQL.Text;
  LStmt.Parameters.AssignValues(Statement.Parameters);
  LStmt.DisableControls;
  try
    LStmt.Open;
    Result := TADOResultSetAdapter.Create(LStmt);
  except
    on E: Exception do
    begin
      //make sure that resultset is always created to avoid memory leak
      Result := TADOResultSetAdapter.Create(LStmt);
      raise EADOAdapterException.CreateFmt(EXCEPTION_CANNOT_OPEN_QUERY, [E.Message]);
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
  begin
    Connection.Connected := True;
    if not Connection.InTransaction then
      Connection.BeginTrans;
    Result := TADOTransactionAdapter.Create(Connection);
  end
  else
    Result := nil;
end;

procedure TADOConnectionAdapter.Connect;
begin
  if Assigned(Connection) then
    Connection.Connected := True;
end;

constructor TADOConnectionAdapter.Create(const connection: TADOConnection);
begin
  inherited Create(connection);
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

    adapter := TADOStatementAdapter.Create(statement);
    adapter.ExecutionListeners := ExecutionListeners;
    Result := adapter;
  end
  else
    Result := nil;
end;

procedure TADOConnectionAdapter.Disconnect;
begin
  if Assigned(Connection) then
    Connection.Connected := False;
end;

function TADOConnectionAdapter.IsConnected: Boolean;
begin
  if Assigned(Connection) then
    Result := Connection.Connected
  else
    Result := False;
end;

{$ENDREGION}


{$REGION 'TADOTransactionAdapter'}

procedure TADOTransactionAdapter.Commit;
begin
  if Assigned(Transaction) then
    Transaction.CommitTrans;
end;

function TADOTransactionAdapter.InTransaction: Boolean;
begin
  Result := Transaction.InTransaction;
end;

procedure TADOTransactionAdapter.Rollback;
begin
  if Assigned(Transaction) then
    Transaction.RollbackTrans;
end;

{$ENDREGION}


{$REGION 'TADOSQLGenerator'}

function TADOSQLGenerator.GenerateGetLastInsertId(
  const identityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TADOConnectionAdapter>(dtADO);
{$ENDIF}

end.
