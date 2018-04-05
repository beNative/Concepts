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

unit Spring.Persistence.Core.Base;

interface

uses
  DB,
  SysUtils,
  Spring.Collections,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Criteria.Interfaces,
  Spring.Persistence.SQL.Commands.Page,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params;

type
  TDriverAdapterBase = class(TInterfacedObject)
  private
    fExceptionHandler: IORMExceptionHandler;
  protected
    constructor Create(const exceptionHandler: IORMExceptionHandler);
    function HandleException(const defaultMsg: string = ''): Exception;
    property ExceptionHandler: IORMExceptionHandler read fExceptionHandler;
  end;

  /// <summary>
  ///   Base <see cref="Spring.Persistence.Core.Interfaces|IDBResultSet" />
  ///   adapter which descendents must override.
  /// </summary>
  TDriverResultSetAdapter<T: TDataSet> = class(TDriverAdapterBase, IDBResultSet)
  private
    fDataSet: T;
    fFieldCache: IFieldCache;
  protected
    function IsEmpty: Boolean;
    function Next: Boolean;
    function FieldExists(const fieldName: string): Boolean;
    function GetFieldValue(index: Integer): Variant; overload;
    function GetFieldValue(const fieldname: string): Variant; overload;
    function GetFieldCount: Integer;
    function GetFieldName(index: Integer): string;
  public
    constructor Create(const dataSet: T;
      const exceptionHandler: IORMExceptionHandler);
    destructor Destroy; override;

    property DataSet: T read fDataSet;
  end;

  /// <summary>
  ///   Base <see cref="Spring.Persistence.Core.Interfaces|IDBConnection" />
  ///   adapter which descendents must override.
  /// </summary>
  TDriverConnectionAdapter<T> = class(TDriverAdapterBase, IDBConnection)
  private
    fConnection: T;
    fListeners: IList<TExecutionListenerProc>;
    fQueryLanguage: TQueryLanguage;
    fAllowServerSideCursor: Boolean;
    fAutoFreeConnection: Boolean;
    fTransationId: Integer;

    function GetAutoFreeConnection: Boolean;
    function GetExecutionListeners: IList<TExecutionListenerProc>;
    function GetQueryLanguage: TQueryLanguage;
    procedure SetAutoFreeConnection(value: Boolean);
    procedure SetQueryLanguage(const value: TQueryLanguage);
  protected
    constructor Create(const connection: T;
      const exceptionHandler: IORMExceptionHandler); overload; virtual;

    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    function IsConnected: Boolean; virtual; abstract;
    function CreateStatement: IDBStatement; virtual; abstract;
    function BeginTransaction: IDBTransaction; virtual;
    function GetTransactionName: string; virtual;
    function GenerateNewID: Integer; virtual;
    procedure AddExecutionListener(const listenerProc: TExecutionListenerProc);
    procedure ClearExecutionListeners;
  public
    constructor Create(const connection: T); overload; virtual; abstract;
    destructor Destroy; override;

    property AllowServerSideCursor: Boolean read fAllowServerSideCursor write fAllowServerSideCursor;
    property AutoFreeConnection: Boolean read GetAutoFreeConnection write SetAutoFreeConnection;
    property Connection: T read fConnection;
    property ExecutionListeners: IList<TExecutionListenerProc> read GetExecutionListeners;
    property QueryLanguage: TQueryLanguage read GetQueryLanguage write SetQueryLanguage;
    property TransactionId: Integer read fTransationId;
  end;

  /// <summary>
  ///   Base <see cref="Spring.Persistence.Core.Interfaces|IDBStatement" />
  ///   adapter which descendents must override.
  /// </summary>
  TDriverStatementAdapter<T> = class(TDriverAdapterBase, IDBStatement)
  private
    fStatement: T;
    fListeners: IList<TExecutionListenerProc>;
    fParams: IEnumerable<TDBParam>;
    fSql: string;
    fQuery: Variant;
    fQueryMetadata: TQueryMetadata;
    fAllowServerSideCursor: Boolean;
  protected
    procedure NotifyListeners; virtual;
  public
    constructor Create(const statement: T;
      const exceptionHandler: IORMExceptionHandler); virtual;

    procedure SetSQLCommand(const commandText: string); virtual;
    procedure SetQuery(const metadata: TQueryMetadata; const query: Variant); virtual;
    procedure SetParams(const params: IEnumerable<TDBParam>); virtual;
    function Execute: NativeUInt; virtual;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; virtual;

    function NativeQueryPresent: Boolean; virtual;

    property AllowServerSideCursor: Boolean read fAllowServerSideCursor write fAllowServerSideCursor;
    property ExecutionListeners: IList<TExecutionListenerProc> read fListeners write fListeners;
    property Statement: T read fStatement;
    property Query: Variant read fQuery write fQuery;
    property QueryMetadata: TQueryMetadata read fQueryMetadata write fQueryMetadata;
  end;

  /// <summary>
  ///   Base <see cref="Spring.Persistence.Core.Interfaces|IDBTransaction" />
  ///   adapter which descendents must override.
  /// </summary>
  TDriverTransactionAdapter<T> = class(TDriverAdapterBase, IDBTransaction)
  private
    fTransactionName: string;
    function GetTransactionName: string;
    procedure SetTransactionName(const value: string);
  protected
    fTransaction: T;
    procedure Commit; virtual; abstract;
    procedure Rollback; virtual; abstract;
    function InTransaction: Boolean; virtual; abstract;
  public
    constructor Create(const transaction: T;
      const exceptionHandler: IORMExceptionHandler); virtual;
    destructor Destroy; override;

    property Transaction: T read fTransaction;
    property TransactionName: string read GetTransactionName write SetTransactionName;
  end;

  /// <summary>
  ///   Responsible for building paged queries.
  /// </summary>
  TPager = class
  private
    fConnection: IDBConnection;
    fPageIndex: Integer;
    fPageSize: Integer;
    fItemCount: Integer;
    fGenerator: ISQLGenerator;
  public
    constructor Create(const connection: IDBConnection; index, size: Integer);

    function BuildSQL(const sql: string): string;

    property Connection: IDBConnection read fConnection;
    property PageIndex: Integer read fPageIndex;
    property PageSize: Integer read fPageSize;
    property ItemCount: Integer read fItemCount write fItemCount;
  end;

  /// <summary>
  ///   Base <see cref="Spring.Persistence.Core.Interfaces|IDBPage&lt;T&gt;" />
  ///   adapter which descendents must override.
  /// </summary>
  TDriverPageAdapter<T: class> = class(TInterfacedObject, IDBPage<T>)
  private
    fItems: IList<T>;
    fPager: TPager;
    function GetPageIndex: Integer;
    function GetPageSize: Integer;
    function GetPageCount: Integer;
    function GetItemCount: Integer;
    function GetItems: IList<T>;
  public
    constructor Create(const pager: TPager); virtual;
    destructor Destroy; override;

    property PageIndex: Integer read GetPageIndex;
    property PageSize: Integer read GetPageSize;
    property PageCount: Integer read GetPageCount;
    property ItemCount: Integer read GetItemCount;
    property Items: IList<T> read GetItems;
  end;

implementation

uses
  StrUtils,
  TypInfo,
  Variants,
  Spring,
  Spring.Persistence.Adapters.FieldCache,
  Spring.Persistence.SQL.Register,
  Spring.Reflection;


{$REGION 'TDriverAdapterBase'}

constructor TDriverAdapterBase.Create(
  const exceptionHandler: IORMExceptionHandler);
begin
  inherited Create;
  fExceptionHandler := exceptionHandler;
end;

function TDriverAdapterBase.HandleException(const defaultMsg: string): Exception;
begin
  Result := fExceptionHandler.HandleException(defaultMsg);
end;

{$ENDREGION}


{$REGION 'TDriverResultSetAdapter<T>'}

constructor TDriverResultSetAdapter<T>.Create(const dataSet: T;
  const exceptionHandler: IORMExceptionHandler);
begin
  inherited Create(exceptionHandler);
  fDataSet := dataSet;
  fDataSet.DisableControls;
  fFieldCache := TFieldCache.Create(dataSet);
end;

destructor TDriverResultSetAdapter<T>.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  fDataSet.Free;
{$ELSE}
  fDataset.DisposeOf;
{$ENDIF}
  inherited Destroy;
end;

function TDriverResultSetAdapter<T>.FieldExists(
  const fieldName: string): Boolean;
begin
  Result := fFieldCache.FieldExists(fieldName);
end;

function TDriverResultSetAdapter<T>.GetFieldCount: Integer;
begin
  Result := fDataSet.FieldCount;
end;

function TDriverResultSetAdapter<T>.GetFieldName(index: Integer): string;
begin
  Result := fDataSet.Fields[index].FieldName;
end;

function TDriverResultSetAdapter<T>.GetFieldValue(index: Integer): Variant;
begin
  try
    Result := fDataSet.Fields[index].Value;
  except
    raise HandleException;
  end;
end;

function TDriverResultSetAdapter<T>.GetFieldValue(
  const fieldName: string): Variant;
begin
  try
    Result := fFieldCache.GetFieldValue(fieldName);
  except
    raise HandleException;
  end;
end;

function TDriverResultSetAdapter<T>.IsEmpty: Boolean;
begin
  Result := fDataSet.Eof;
end;

function TDriverResultSetAdapter<T>.Next: Boolean;
begin
  try
    fDataSet.Next;
  except
    raise HandleException;
  end;
  Result := not DataSet.Eof;
end;

{$ENDREGION}


{$REGION 'TDriverConnectionAdapter<T>'}

procedure TDriverConnectionAdapter<T>.AddExecutionListener(
  const listenerProc: TExecutionListenerProc);
begin
  fListeners.Add(listenerProc);
end;

function TDriverConnectionAdapter<T>.BeginTransaction: IDBTransaction;
begin
  GenerateNewID;
  Result := nil;
end;

procedure TDriverConnectionAdapter<T>.ClearExecutionListeners;
begin
  fListeners.Clear;
end;

constructor TDriverConnectionAdapter<T>.Create(const connection: T;
  const exceptionHandler: IORMExceptionHandler);
begin
  inherited Create(exceptionHandler);
  fConnection := connection;
  fListeners := TCollections.CreateList<TExecutionListenerProc>;
  fQueryLanguage := qlAnsiSQL;
  fTransationId := 0;
  fAllowServerSideCursor := True;
  fAutoFreeConnection := False;
end;

destructor TDriverConnectionAdapter<T>.Destroy;
begin
  if TType.Kind<T> = tkClass then
    if AutoFreeConnection then
      PObject(@fConnection).Free;
  inherited Destroy;
end;

function TDriverConnectionAdapter<T>.GenerateNewID: Integer;
begin
  AtomicIncrement(fTransationId);
  Result := fTransationId;
end;

function TDriverConnectionAdapter<T>.GetAutoFreeConnection: Boolean;
begin
  Result := fAutoFreeConnection;
end;

function TDriverConnectionAdapter<T>.GetExecutionListeners: IList<TExecutionListenerProc>;
begin
  Result := fListeners;
end;

function TDriverConnectionAdapter<T>.GetQueryLanguage: TQueryLanguage;
begin
  Result := fQueryLanguage;
end;

function TDriverConnectionAdapter<T>.GetTransactionName: string;
begin
  Result := 'T' + IntToStr(fTransationId);
end;

procedure TDriverConnectionAdapter<T>.SetAutoFreeConnection(value: Boolean);
begin
  fAutoFreeConnection := value;
end;

procedure TDriverConnectionAdapter<T>.SetQueryLanguage(const value: TQueryLanguage);
begin
  fQueryLanguage := value;
end;

{$ENDREGION}


{$REGION 'TDriverStatementAdapter<T>'}

constructor TDriverStatementAdapter<T>.Create(const statement: T;
  const exceptionHandler: IORMExceptionHandler);
begin
  inherited Create(exceptionHandler);
  fStatement := statement;
  fQuery := Null;
end;

function TDriverStatementAdapter<T>.Execute: NativeUInt;
begin
  NotifyListeners;
  Result := 0;
end;

function TDriverStatementAdapter<T>.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
begin
  NotifyListeners;
  Result := nil;
end;

function TDriverStatementAdapter<T>.NativeQueryPresent: Boolean;
begin
  Result := not VarIsNull(fQuery);
end;

procedure TDriverStatementAdapter<T>.NotifyListeners;
var
  listener: TExecutionListenerProc;
  params: IEnumerable<TDBParam>;
begin
  if Assigned(fListeners) and (fSql <> '') then
  begin
    params := fParams;
    if not Assigned(params) then
      params := TEnumerable.Empty<TDBParam>;

    for listener in fListeners do
      listener(fSql, params);
  end;
end;

procedure TDriverStatementAdapter<T>.SetParams(const params: IEnumerable<TDBParam>);
begin
  fParams := params;
end;

procedure TDriverStatementAdapter<T>.SetQuery(const metadata: TQueryMetadata;
  const query: Variant);
begin
  case TQueryMetadata.GetQueryType(query) of
    qtQueryText: SetSQLCommand(query);
    qtQueryEntity:
    begin
      fQuery := query;
      fQueryMetadata := metadata;
    end;
  end;
end;

procedure TDriverStatementAdapter<T>.SetSQLCommand(const commandText: string);
begin
  fSql := commandText;
end;

{$ENDREGION}


{$REGION 'TDriverPageAdapter<T>'}

constructor TDriverPageAdapter<T>.Create(const pager: TPager);
begin
  inherited Create;
  fPager := pager;
  fItems := TCollections.CreateObjectList<T>(True);
end;

destructor TDriverPageAdapter<T>.Destroy;
begin
  fPager.Free;
  inherited Destroy;
end;

function TDriverPageAdapter<T>.GetItemCount: Integer;
begin
  Result := fPager.ItemCount;
end;

function TDriverPageAdapter<T>.GetItems: IList<T>;
begin
  Result := fItems;
end;

function TDriverPageAdapter<T>.GetPageCount: Integer;
var
  itemCount: Integer;
  pageSize: Integer;
begin
  itemCount := GetItemCount;
  pageSize := GetPageSize;
  Result := itemCount div pageSize;
  if (itemCount mod pageSize) <> 0 then
    Inc(Result);
end;

function TDriverPageAdapter<T>.GetPageIndex: Integer;
begin
  Result := fPager.PageIndex;
end;

function TDriverPageAdapter<T>.GetPageSize: Integer;
begin
  Result := fPager.PageSize;
end;

{$ENDREGION}


{$REGION 'TDriverTransactionAdapter<T>'}

constructor TDriverTransactionAdapter<T>.Create(const transaction: T;
  const exceptionHandler: IORMExceptionHandler);
begin
  inherited Create(exceptionHandler);
  fTransaction := transaction;
end;

destructor TDriverTransactionAdapter<T>.Destroy;
begin
  if InTransaction then
    Rollback;
  inherited Destroy;
end;

function TDriverTransactionAdapter<T>.GetTransactionName: string;
begin
  Result := fTransactionName;
end;

procedure TDriverTransactionAdapter<T>.SetTransactionName(const value: string);
begin
  fTransactionName := value;
end;

{$ENDREGION}


{$REGION 'TPager'}

constructor TPager.Create(const connection: IDBConnection; index, size: Integer);
begin
  inherited Create;
  fConnection := connection;
  fGenerator := TSQLGeneratorRegister.GetGenerator(connection.QueryLanguage);
  fPageIndex := index;
  fPageSize := size;
end;

function TPager.BuildSQL(const sql: string): string;
var
  offset: Integer;
begin
  offset := 0;
  if fPageIndex > 1 then
    offset := (fPageIndex - 1) * fPageSize;
  Result := fGenerator.GeneratePagedQuery(sql, fPageSize, offset);
end;

{$ENDREGION}


end.
