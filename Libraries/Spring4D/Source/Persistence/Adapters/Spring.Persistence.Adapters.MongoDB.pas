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

unit Spring.Persistence.Adapters.MongoDB;

{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  SysUtils,
  MongoDB,
  MongoBson,
  Spring.Collections,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Params;

type
  TPageInfo = record
    Limit: Integer;
    Offset: Integer;
  end;

  EMongoDBStatementAdapterException = class(EORMAdapterException);

  TMongoStatementType = (mstInsert, mstUpdate, mstDelete, mstSelect, mstSelectCount, mstSelectOrder, mtPage);

  /// <summary>
  ///   Represents MongoDB connection.
  /// </summary>
  TMongoDBConnection = class(TMongoReplset)
  protected
    function GetConnected: Boolean; virtual;
    procedure SetConnected(value: Boolean); virtual;
  public
    function GetCollectionFromFullName(const fullConnectionName: string): string; virtual;
  public
    property Connected: Boolean read GetConnected write SetConnected;
  end;

  /// <summary>
  ///   Represents MongoDB query.
  /// </summary>
  TMongoDBQuery = class(TMongoCursor)
  private
    fConnection: TMongoDBConnection;
    function GetConnection: TMongoDBConnection;
  public
    constructor Create(const connection: TMongoDBConnection); overload;

    property Connection: TMongoDBConnection read GetConnection;
  end;

  /// <summary>
  ///   Represents MongoDB resultset.
  /// </summary>
  TMongoResultSetAdapter = class(TDriverAdapterBase, IDBResultSet)
  private
    fDataSet: TMongoDBQuery;
    fDoc: IBSONDocument;
    fIsInjected: Boolean;
  public
    constructor Create(const dataSet: TMongoDBQuery;
      const exceptionHandler: IORMExceptionHandler);
    destructor Destroy; override;

    function IsEmpty: Boolean;
    function Next: Boolean;
    function GetFieldValue(index: Integer): Variant; overload;
    function GetFieldValue(const fieldName: string): Variant; overload;
    function GetFieldCount: Integer;
    function GetFieldName(index: Integer): string;
    function FieldExists(const fieldName: string): Boolean;

    property Document: IBSONDocument read fDoc write fDoc;
    property IsInjected: Boolean read fIsInjected write fIsInjected;
  end;

  /// <summary>
  ///   Represents MongoDB statement.
  /// </summary>
  TMongoStatementAdapter = class(TDriverStatementAdapter<TMongoDBQuery>)
  private
    fStatementText: string;
    fStatementType: TMongoStatementType;
    fFullCollectionName: string;
  protected
    function GetStatementType(var statementText: string): TMongoStatementType; virtual;
    function GetStatementPageInfo(const statementText: string; out pageInfo: TPageInfo): string; virtual;
    function IsObjectId(const value: string): Boolean; virtual;
    function GetJsonPartFromStatementText(const tagName: string): string; virtual;
  public
    constructor Create(const statement: TMongoDBQuery;
      const exceptionHandler: IORMExceptionHandler); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetQuery(const metadata: TQueryMetadata; const query: Variant); override;
    procedure SetParam(const param: TDBParam);
    procedure SetParams(const params: IEnumerable<TDBParam>); override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
    function GetQueryText: string;
    function GetFullCollectionName: string; virtual;
  end;

  /// <summary>
  ///   Represents MongoDB connection.
  /// </summary>
  TMongoConnectionAdapter = class(TDriverConnectionAdapter<TMongoDBConnection>, IDBConnection)
  public
    constructor Create(const connection: TMongoDBConnection); override;
    procedure AfterConstruction; override;
    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
  end;

  /// <summary>
  ///   Represents MongoDB transaction.
  /// </summary>
  TMongoTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    fConnection: TMongoDBConnection;
  protected
    function GetTransactionName: string;
    procedure SetTransactionName(const value: string);
  public
    constructor Create(const connection: TMongoDBConnection);

    procedure Commit;
    procedure Rollback;
  end;

  TMongoDBExceptionHandler = class(TORMExceptionHandler)
  protected
    function GetAdapterException(const exc: Exception;
      const defaultMsg: string): Exception; override;
  end;

implementation

uses
  StrUtils,
  Variants,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.MongoDB,
  Spring.Persistence.SQL.Interfaces;


const
  MONGO_STATEMENT_TYPES: array[TMongoStatementType] of string = ('I', 'U', 'D', 'S', 'count', 'SO', 'page');


{$REGION 'TMongoDBConnection'}

function TMongoDBConnection.GetCollectionFromFullName(const fullConnectionName: string): string;
var
  i: Integer;
begin
  Result := fullConnectionName;
  i := PosEx('.', Result);
  if i > 0 then
    Result := Copy(Result, i + 1, Length(Result));
end;

function TMongoDBConnection.GetConnected: Boolean;
begin
  Result := checkConnection;
end;

procedure TMongoDBConnection.SetConnected(value: Boolean);
begin
  if not Connected then
  begin
    if value then
      Connect;
  end
  else
    if not value then
      Disconnect;
end;

{$ENDREGION}


{$REGION 'TMongoDBQuery'}

constructor TMongoDBQuery.Create(const connection: TMongoDBConnection);
begin
  inherited Create;
  fConnection := connection;
end;

function TMongoDBQuery.GetConnection: TMongoDBConnection;
begin
  Result := fConnection;
end;

{$ENDREGION}


{$REGION 'TMongoResultSetAdapter'}

constructor TMongoResultSetAdapter.Create(const dataSet: TMongoDBQuery;
  const exceptionHandler: IORMExceptionHandler);
begin
  inherited Create(exceptionHandler);
  fDataSet := dataSet;
end;

destructor TMongoResultSetAdapter.Destroy;
begin
  fDataSet.Free;
  inherited Destroy;
end;

function TMongoResultSetAdapter.FieldExists(const fieldName: string): Boolean;
var
  iterator: TBsonIterator;
begin
  Result := False;
  if Assigned(fDoc) then
  begin
    iterator := fDoc.find(fieldName);
    if Assigned(iterator) then
    begin
      Result := True;
      iterator.Free;
    end;
  end;
end;

function TMongoResultSetAdapter.GetFieldCount: Integer;
var
  iterator: TBsonIterator;
begin
  Result := 0;
  if Assigned(fDoc) then
  begin
    iterator := fDoc.iterator;
    while iterator.next do
      Inc(Result);
  end;
end;

function TMongoResultSetAdapter.GetFieldName(index: Integer): string;
var
  iterator: TBsonIterator;
  i: Integer;
begin
  Assert(Assigned(fDoc), 'Document not assigned');
  iterator := fDoc.iterator;
  i := 0;
  while iterator.next do
  begin
    if index = i then
      Exit(iterator.key);
    Inc(i);
  end;
  Result := '';
end;

function TMongoResultSetAdapter.GetFieldValue(const fieldName: string): Variant;
begin
  Assert(Assigned(fDoc), 'Document not assigned');
  Result := fDoc.value(fieldName);
end;

function TMongoResultSetAdapter.GetFieldValue(index: Integer): Variant;
begin
  Assert(Assigned(fDoc), 'Document not assigned');
  Result := fDoc.value(GetFieldname(index));
end;

function TMongoResultSetAdapter.IsEmpty: Boolean;
begin
  Result := not IsInjected and not fDataSet.next;
  if not Result and not IsInjected then
    fDoc := fDataSet.value;
end;

function TMongoResultSetAdapter.Next: Boolean;
begin
  Result := True;
end;

{$ENDREGION}


{$REGION 'TMongoStatementAdapter'}

constructor TMongoStatementAdapter.Create(const statement: TMongoDBQuery;
  const exceptionHandler: IORMExceptionHandler);
begin
  inherited Create(statement, ExceptionHandler);
  fStatementText := '';
  fStatementType := mstSelect;
end;

destructor TMongoStatementAdapter.Destroy;
begin
  Statement.Free;
  inherited Destroy;
end;

function TMongoStatementAdapter.Execute: NativeUInt;
var
  doc, findDoc: IBSONDocument;
  intf: IInterface;
  ok: Boolean;
  value: Variant;
begin
  inherited;
  Result := 0;
  ok := False;

  if NativeQueryPresent then
  begin
    case QueryMetadata.QueryOperation of
      ctUpdateVersion:
      begin
        intf := Query;
        doc := intf as IBsonDocument;
        findDoc := Statement.Connection.findAndModify(GetFullCollectionName,
          doc, bsonEmpty, BSON(['$inc', BSON(['_version', 1])]));
        value := findDoc['value'];
        if not VarIsNull(value) then
          Result := 1;
        Exit;
      end;
    end;
  end;

  if fStatementType <> mstUpdate then
    doc := JsonToBson(fStatementText);

  case fStatementType of
    mstInsert: ok := Statement.Connection.insert(GetFullCollectionName, doc);
    mstUpdate:
    begin
      findDoc := JsonToBson(GetJsonPartFromStatementText(MONGO_STATEMENT_TYPES[mstUpdate]));
      doc := JsonToBson(fStatementText);
      ok := Statement.Connection.Update(GetFullCollectionName, findDoc, doc);
    end;
    mstDelete: ok := Statement.Connection.remove(GetFullCollectionName, doc);
    mstSelect: ok := Statement.Connection.findOne(GetFullCollectionName, doc) <> nil;
    mstSelectCount: Exit(Trunc(Statement.Connection.Count(GetFullCollectionName, doc)));
  end;
  if ok then
    Result := 1;
end;

function TMongoStatementAdapter.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
var
  dbQuery: TMongoDBQuery;
  resultSet: TMongoResultSetAdapter;
  pageInfo: TPageInfo;
  count: Double;
begin
  inherited;
  dbQuery := TMongoDBQuery.Create(Statement.Connection);

  if fStatementType = mtPage then
  begin
    fStatementText := GetStatementPageInfo(fStatementText, pageInfo);
    dbQuery.limit := pageInfo.Limit;
    dbQuery.skip := pageInfo.Offset;
  end;

  if fStatementType = mstSelectOrder then
  begin
    dbQuery.sort := JsonToBson(GetJsonPartFromStatementText(MONGO_STATEMENT_TYPES[mstSelectOrder]));
  end;

  resultSet := TMongoResultSetAdapter.Create(dbQuery, ExceptionHandler);
  resultSet.Document := JsonToBson(fStatementText);
  case fStatementType of
    mstInsert:
    begin
      Statement.Connection.Insert(GetFullCollectionName, resultSet.Document);
      resultSet.IsInjected := True;
    end;
    mstSelect, mtPage, mstSelectOrder:
    begin
      dbQuery.query := resultSet.Document;
      if Statement.Connection.find(GetFullCollectionName, dbQuery) then
      begin
        resultSet.Document := nil;
      end;
    end;
    mstSelectCount:
    begin
      count := Statement.Connection.count(GetFullCollectionName, resultSet.Document);
      resultSet.Document := BSON(['n', count]);
      resultSet.IsInjected := True;
    end;
  end;
  Result := resultSet;
end;

function TMongoStatementAdapter.GetFullCollectionName: string;
begin
  Result := fFullCollectionName;
end;

function TMongoStatementAdapter.GetJsonPartFromStatementText(const tagName: string): string;
var
  i, n: Integer;
begin
  //read length
  i := PosEx('_', fStatementText);
  if i < 0 then
    Exit('{}');
  n := StrToInt(Copy(fStatementText, Length(tagName) + 1, i - (Length(tagName) + 1)));
  Result := Copy(fStatementText, i + 1, n);
  i := PosEx(']', fStatementText);
  fStatementText := Copy(fStatementText, i + 1, Length(fStatementText));
end;

function TMongoStatementAdapter.GetQueryText: string;
begin
  Result := fStatementText;
end;

function TMongoStatementAdapter.GetStatementPageInfo(const statementText: string; out pageInfo: TPageInfo): string;
var
  startPos, endPos: Integer;
begin
  //ex. page1_10_[Collection]{}
  startPos := PosEx('_', statementText);
  pageInfo.Limit := StrToInt(Copy(statementText, 5, startPos - 5) );
  endPos := PosEx('_', statementText, startPos+1);
  pageInfo.Offset := StrToInt( Copy(statementText, startPos + 1, endPos - startPos - 1) );
  endPos := PosEx(']', statementText, endPos);
  Result := Copy(statementText, endPos+1, Length(statementText));
end;

function TMongoStatementAdapter.GetStatementType(var statementText: string): TMongoStatementType;
var
  identifier: string;
  startIndex, endIndex: Integer;
begin
  statementText := Trim(statementText);
  if Length(statementText) = 0 then
    statementText := 'S';

  startIndex := PosEx('[', statementText);
  endIndex := PosEx(']', statementText);
  fFullCollectionName := Copy(statementText, startIndex+1, endIndex - startIndex-1);

  identifier := statementText[1];
  if identifier = 'I' then
    Result := mstInsert
  else if identifier = 'U' then
    Exit(mstUpdate)
  else if identifier = 'D' then
    Result := mstDelete
  else if StartsStr('count', statementText) then
  begin
    identifier := 'count';
    Result := mstSelectCount;
  end
  else if StartsStr('page', statementText) then
    Exit(mtPage)
  else if StartsStr('SO', statementText) then
    Exit(mstSelectOrder)
  else
    Result := mstSelect;

  statementText := Copy(statementText, Length(fFullCollectionName) + 2 + Length(identifier) + 1, Length(statementText));
end;

function TMongoStatementAdapter.IsObjectId(const value: string): Boolean;
begin
  Result := StartsText('ObjectID("', value);
end;

procedure TMongoStatementAdapter.SetParam(const param: TDBParam);
var
  value: string;
begin
  if VarType(param.ToVariant) = varUnknown then
    Exit;
  value := VarToStrDef(param.ToVariant, 'null');
  case VarType(param.ToVariant) of
    varString, varUString, varStrArg, varOleStr:
    begin
      if IsObjectId(value) then   //ObjectID("sdsd457845")
      begin
        value := ReplaceStr(value, '"', '\"');
        value := Format('"%s"', [value]);
      end
      else
        value := AnsiQuotedStr(value, '"');
    end;
  end;

  fStatementText := StringReplace(fStatementText, param.Name, value, [rfReplaceAll]);
end;

procedure TMongoStatementAdapter.SetParams(const params: IEnumerable<TDBParam>);
begin
  inherited;
  if fStatementText = '' then
    Exit;

  params.ForEach(SetParam);
end;

procedure TMongoStatementAdapter.SetQuery(const metadata: TQueryMetadata;
  const query: Variant);
begin
  inherited;
  fFullCollectionName := metadata.TableName;
end;

procedure TMongoStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  if commandText = '' then
    Exit;

  fStatementText := commandText;
  fStatementType := GetStatementType(fStatementText);
end;

{$ENDREGION}


{$REGION 'TMongoConnectionAdapter'}

procedure TMongoConnectionAdapter.AfterConstruction;
begin
  inherited;
  QueryLanguage := qlMongoDB;
end;

function TMongoConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Assigned(Connection) then
    Result := TMongoTransactionAdapter.Create(Connection);
end;

procedure TMongoConnectionAdapter.Connect;
begin
  if Assigned(Connection) then
    Connection.Connected := True;
end;

constructor TMongoConnectionAdapter.Create(
  const connection: TMongoDBConnection);
begin
  Create(connection, TMongoDBExceptionHandler.Create);
end;

function TMongoConnectionAdapter.CreateStatement: IDBStatement;
var
  statement: TMongoDBQuery;
  adapter: TMongoStatementAdapter;
begin
  if Assigned(Connection) then
  begin
    statement := TMongoDBQuery.Create(Connection);
    adapter := TMongoStatementAdapter.Create(statement, ExceptionHandler);
    adapter.ExecutionListeners := ExecutionListeners;
    Result := adapter;
  end
  else
    Result := nil;
end;

procedure TMongoConnectionAdapter.Disconnect;
begin
  if Assigned(Connection) then
    Connection.Connected := False;
end;

function TMongoConnectionAdapter.IsConnected: Boolean;
begin
  Result := Assigned(Connection) and Connection.Connected;
end;

{$ENDREGION}


{$REGION 'TMongoTransactionAdapter'}

constructor TMongoTransactionAdapter.Create(const connection: TMongoDBConnection);
begin
  inherited Create;
  fConnection := connection;
end;

procedure TMongoTransactionAdapter.Commit;
begin
end;

function TMongoTransactionAdapter.GetTransactionName: string;
begin
  Result := '';
end;

procedure TMongoTransactionAdapter.Rollback;
begin
end;

procedure TMongoTransactionAdapter.SetTransactionName(const value: string);
begin
end;

{$ENDREGION}


{$REGION 'TMongoDBExceptionHandler'}

function TMongoDBExceptionHandler.GetAdapterException(const exc: Exception;
  const defaultMsg: string): Exception;
begin
  Result := nil;
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TMongoConnectionAdapter>(dtMongo);

end.
