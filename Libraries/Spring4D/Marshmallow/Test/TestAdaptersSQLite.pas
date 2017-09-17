unit TestAdaptersSQLite;

interface

uses
  TestFramework,
  Rtti,
  SysUtils,
  SQLiteTable3,
  Spring,
  Spring.Collections,
  Spring.Persistence.Adapters.SQLite,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.SQL.Params,
  Spring.Reflection,
  Spring.TestUtils;

type
  TSQLiteExceptionHandlerAccess = class(TSQLiteExceptionHandler);
  TSQLiteExceptionHandlerTest = class(TTestCase<TSQLiteExceptionHandlerAccess>)
  published
    procedure TestGetAdapterException_ESQLiteException;
    procedure TestGetAdapterException_ESQLiteConstraintException;
    procedure TestGetAdapterException_Others_Return_Nil;
  end;

  TBaseSQLiteConnectionAdapterTest = class(TTestCase)
  protected const
    SQL_SELECT_ALL = 'SELECT * FROM CUSTOMERS;';
  strict private
    fTestDb: TSQLiteDatabase;
  strict protected
    fConnection: IDBConnection;
    procedure CreateTables;
    procedure DeleteAllCustomers;
    procedure DeleteCustomer(AID: Integer);
    function GetCustomersCount: Integer;
    function InsertCustomer(AName: string; AAge: Integer; AHeight: Double): Int64;
    property TestDb: TSQLiteDatabase read fTestDb;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TSQLiteResultSetAdapterTest = class(TBaseSQLiteConnectionAdapterTest)
  private
    fResultSet: IDBResultSet;
    function CreateResultSet: IDBResultSet;
    procedure WithDamagedStatementDo(const proc: TProc);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsEmpty;
    procedure TestNext;
    procedure TestNext_Exception;
    procedure TestGetFieldValue;
    procedure TestGetFieldValue_Exception;
  end;

  TSQLiteStatementAdapterTest = class(TBaseSQLiteConnectionAdapterTest)
  private
    fStatement: IDBStatement;
    fCommandText: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetSQLCommand;
    procedure TestSetSQLCommand_Exception;
    procedure TestSetParams;
    procedure TestExecute;
    procedure TestExecute_Exception;
    procedure TestExecuteQuery;
    procedure TestExecuteQuery_Exception;
  end;

  TSQLiteConnectionAdapterTest = class(TBaseSQLiteConnectionAdapterTest)
  private
    function CreateFailDB: TSQLiteDatabase;
  published
    procedure TestConnect;
    procedure TestConnect_Exception;
    procedure TestDisconnect;
    procedure TestIsConnected;
    procedure TestCreateStatement;
    procedure TestBeginTransaction;
    procedure TestBeginTransaction_Exception;
    procedure TestGetQueryLanguage;
  end;

  TSQLiteTransactionAdapterTest = class(TBaseSQLiteConnectionAdapterTest)
  private
    fTransaction: IDBTransaction;
  protected
    function CreateAndBeginTransaction: IDBTransaction;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCommit;
    procedure TestCommit_Exception;
    procedure TestRollback;
    procedure TestRollback_Exception;
  end;

implementation

uses
  DB,
  Spring.Persistence.SQL.Interfaces;

{$REGION 'TSQLiteExceptionHandlerTest'}

procedure TSQLiteExceptionHandlerTest.TestGetAdapterException_ESQLiteConstraintException;
var
  exc, result: Shared<Exception>;
begin
  exc := ESQLiteConstraintException.Create('', 1);
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EORMConstraintException);
  CheckEqualsString('message', result.Value.Message);
  CheckEquals(1, EORMConstraintException(result.Value).ErrorCode);
end;

procedure TSQLiteExceptionHandlerTest.TestGetAdapterException_ESQLiteException;
var
  exc, result: Shared<Exception>;
begin
  exc := ESQLiteException.Create('');
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, ESQLiteAdapterException);
  CheckEqualsString('message', result.Value.Message);
  CheckEquals(-1, ESQLiteAdapterException(result.Value).ErrorCode);
end;

procedure TSQLiteExceptionHandlerTest.TestGetAdapterException_Others_Return_Nil;
var
  exc, result: Shared<Exception>;
begin
  exc := Exception.Create('');
  result := SUT.GetAdapterException(exc, '');
  CheckNull(result);
end;

{$ENDREGION}


{$REGION 'TBaseSQLiteConnectionAdapterTest'}

procedure TBaseSQLiteConnectionAdapterTest.CreateTables;
begin
  fTestDb.ExecSQL('CREATE TABLE IF NOT EXISTS CUSTOMERS ([ID] INTEGER PRIMARY KEY, [AGE] INTEGER NULL,'+
    '[NAME] VARCHAR (255), [HEIGHT] FLOAT, [PICTURE] BLOB); ');
  if not fTestDb.TableExists('CUSTOMERS') then
    raise Exception.Create('Table CUSTOMERS does not exist');
end;

procedure TBaseSQLiteConnectionAdapterTest.DeleteAllCustomers;
begin
  fTestDb.ExecSQL('DELETE FROM CUSTOMERS;');
end;

procedure TBaseSQLiteConnectionAdapterTest.DeleteCustomer(AID: Integer);
begin
  fTestDb.ExecSQL('DELETE FROM CUSTOMERS WHERE ID = ?', [AID]);
end;

function TBaseSQLiteConnectionAdapterTest.GetCustomersCount: Integer;
var
  lTable: ISQLiteTable;
begin
  lTable := fTestDb.GetUniTableIntf('SELECT COUNT(*) FROM CUSTOMERS;');
  Result := lTable.Fields[0].AsIntegerDef;
end;

function TBaseSQLiteConnectionAdapterTest.InsertCustomer(AName: string;
  AAge: Integer; AHeight: Double): Int64;
var
  lStmt: ISQLitePreparedStatement;
begin
  Result := -1;
  lStmt := fTestDb.GetPreparedStatementIntf('INSERT INTO CUSTOMERS ([NAME], [AGE], [HEIGHT]) VALUES (:NAME, :AGE, :HEIGHT);');

  lStmt.SetParamInt(':AGE', AAge);
  lStmt.SetParamText(':NAME', AName);
  lStmt.SetParamFloat(':HEIGHT', AHeight);

  if lStmt.ExecSQL then
  begin
    Result := fTestDb.GetLastInsertRowID;
  end;
end;

procedure TBaseSQLiteConnectionAdapterTest.SetUp;
begin
  inherited;
  fTestDb := TSQLiteDatabase.Create(':memory:');
  fConnection := TSQLiteConnectionAdapter.Create(fTestDb);
  fConnection.AutoFreeConnection := True;
  CreateTables;
end;

procedure TBaseSQLiteConnectionAdapterTest.TearDown;
begin
  fConnection := nil;
  fTestDb := nil;
  inherited;
end;

{$ENDREGION}


{$REGION 'TSQLiteResultSetAdapterTest'}

function TSQLiteResultSetAdapterTest.CreateResultSet: IDBResultSet;
var
  lStatement: IDBStatement;
begin
  lStatement := fConnection.CreateStatement;
  lStatement.SetSQLCommand(SQL_SELECT_ALL);
  Result := lStatement.ExecuteQuery;
end;

procedure TSQLiteResultSetAdapterTest.SetUp;
begin
  inherited;
  fResultSet := CreateResultSet;
end;

procedure TSQLiteResultSetAdapterTest.TearDown;
begin
  fResultSet := nil;
  inherited;
end;

procedure TSQLiteResultSetAdapterTest.TestIsEmpty;
var
  lId: Int64;
begin
  CheckTrue(fResultSet.IsEmpty);

  lId := InsertCustomer('Test', 15, 1.1);
  fResultSet := CreateResultSet;

  CheckFalse(fResultSet.IsEmpty);

  DeleteCustomer(lId);
  fResultSet := CreateResultSet;

  CheckTrue(fResultSet.IsEmpty);
end;

procedure TSQLiteResultSetAdapterTest.TestNext;
begin
  CheckFalse(fResultSet.Next);

  InsertCustomer('Test', 15, 1.1);
  InsertCustomer('Test2', 65, 95.1);
  fResultSet := CreateResultSet;

  CheckTrue(fResultSet.Next);
  CheckFalse(fResultSet.Next);
end;

procedure TSQLiteResultSetAdapterTest.TestNext_Exception;
begin
  InsertCustomer('Test', 15, 1.1);
  fResultSet := CreateResultSet;

  WithDamagedStatementDo(
    procedure
    begin
      CheckException(ESQLiteAdapterException,
        procedure begin fResultSet.Next end);
    end);
end;

procedure TSQLiteResultSetAdapterTest.WithDamagedStatementDo(const proc: TProc);
var
  sqlTable: TObject;
  field: TRttiField;
  tempStmt: TValue;
begin
  field := TType.GetType((fResultSet as TObject).ClassType).GetField('fDataSet');
  sqlTable := field.GetValue(fResultSet as TObject).AsInterface as TObject;
  field := TType.GetType(sqlTable.ClassType).GetField('fStmt');
  tempStmt := field.GetValue(sqlTable);
  field.SetValue(sqlTable, nil);
  try
    proc();
  finally
    field.SetValue(sqlTable, tempStmt);
  end;
end;

procedure TSQLiteResultSetAdapterTest.TestGetFieldValue;
var
  lId1, lId2: Int64;
begin
  lId1 := InsertCustomer('Test', 15, 1.1);
  lId2 := InsertCustomer('Test2', 65, 95.1);

  fResultSet := CreateResultSet;

  CheckEquals(lId1, fResultSet.GetFieldValue(0));
  CheckEquals(15, fResultSet.GetFieldValue(1));
  CheckEquals(15, fResultSet.GetFieldValue('AGE'));

  CheckEqualsString('Test', fResultSet.GetFieldValue(2));
  CheckEqualsString('Test', fResultSet.GetFieldValue('NAME'));

  CheckTrue(fResultSet.Next);
  CheckEquals(lId2, fResultSet.GetFieldValue(0));
end;

procedure TSQLiteResultSetAdapterTest.TestGetFieldValue_Exception;
begin
  CheckException(ESQLiteAdapterException,
    procedure begin fResultSet.GetFieldValue(0) end);
  CheckException(ESQLiteAdapterException,
    procedure begin fResultSet.GetFieldValue('NAME') end);
end;

{$ENDREGION}


{$REGION 'TSQLiteStatementAdapterTest'}

procedure TSQLiteStatementAdapterTest.SetUp;
begin
  inherited;

  fCommandText := 'SELECT * FROM CUSTOMERS WHERE NAME = :NAME;';

  fStatement := fConnection.CreateStatement;
end;

procedure TSQLiteStatementAdapterTest.TearDown;
begin
  fStatement := nil;
  inherited;
end;

procedure TSQLiteStatementAdapterTest.TestSetSQLCommand;
begin
  fStatement.SetSQLCommand(fCommandText);
  Pass;
end;

procedure TSQLiteStatementAdapterTest.TestSetSQLCommand_Exception;
begin
  ExpectedException := ESQLiteAdapterException;
  fStatement.SetSQLCommand('TOTALLY INVALID SQL');
end;

procedure TSQLiteStatementAdapterTest.TestSetParams;
var
  Params: IList<TDBParam>;
  LParam: TDBParam;
begin
  Params := TCollections.CreateObjectList<TDBParam>;
  LParam := TDBParam.Create(':NAME', 'Test');
  Params.Add(LParam);

  fStatement.SetParams(Params);
  Pass;
end;

procedure TSQLiteStatementAdapterTest.TestExecute;
var
  ReturnValue: NativeUInt;
begin
  fStatement.SetSQLCommand('INSERT INTO CUSTOMERS(NAME) VALUES (:NAME);');

  TestSetParams;

  ReturnValue := fStatement.Execute;

  CheckEquals(1, ReturnValue);
end;

procedure TSQLiteStatementAdapterTest.TestExecuteQuery;
var
  ReturnValue: IDBResultset;
begin
  fStatement.SetSQLCommand(fCommandText);

  TestSetParams;

  ReturnValue := fStatement.ExecuteQuery;

  CheckTrue(Assigned(ReturnValue));
  CheckTrue(ReturnValue.IsEmpty);

  InsertCustomer('Test', 15, 1.1);
  fStatement := fConnection.CreateStatement;
  fStatement.SetSQLCommand(fCommandText);

  TestSetParams;

  ReturnValue := fStatement.ExecuteQuery;

  CheckTrue(Assigned(ReturnValue));
  CheckFalse(ReturnValue.IsEmpty);
end;

procedure TSQLiteStatementAdapterTest.TestExecuteQuery_Exception;
begin
  InsertCustomer('Test', 1, 1);
  fStatement.SetSQLCommand('INSERT INTO CUSTOMERS(ID,NAME) VALUES (1, "A");');
  ExpectedException := EORMConstraintException;
  fStatement.ExecuteQuery;
end;

procedure TSQLiteStatementAdapterTest.TestExecute_Exception;
begin
  InsertCustomer('Test', 1, 1);
  fStatement.SetSQLCommand('INSERT INTO CUSTOMERS(ID,NAME) VALUES (1, "A");');
  ExpectedException := EORMConstraintException;
  fStatement.Execute;
end;

{$ENDREGION}


{$REGION 'TSQLiteConnectionAdapterTest'}

procedure TSQLiteConnectionAdapterTest.TestConnect;
begin
  fConnection.Connect;

  CheckTrue(TestDb.Connected);
end;

procedure TSQLiteConnectionAdapterTest.TestConnect_Exception;
begin
  fConnection := TSQLiteConnectionAdapter.Create(CreateFailDB);
  fConnection.AutoFreeConnection := True;
  ExpectedException:=ESQLiteAdapterException;
  fConnection.Connect;
end;

procedure TSQLiteConnectionAdapterTest.TestDisconnect;
begin
  fConnection.Disconnect;

  CheckFalse(TestDb.Connected);
end;

procedure TSQLiteConnectionAdapterTest.TestIsConnected;
var
  ReturnValue: Boolean;
begin
  fConnection.Disconnect;
  ReturnValue := fConnection.IsConnected;
  CheckFalse(ReturnValue);
  fConnection.Connect;
  ReturnValue := fConnection.IsConnected;
  CheckTrue(ReturnValue);
end;

procedure TSQLiteConnectionAdapterTest.TestCreateStatement;
var
  ReturnValue: IDBStatement;
begin
  ReturnValue := fConnection.CreateStatement;

  CheckTrue(Assigned(ReturnValue));
end;

function TSQLiteConnectionAdapterTest.CreateFailDB: TSQLiteDatabase;
begin
  Result := TSQLiteDatabase.Create('');
  // Some name that should fail. Posix has almost no restrictions but we know
  // null cannot be a directory. On Windows this will fail as well (made sure
  // by tho colon).
  Result.Filename:='/dev/null/fail:';
end;

procedure TSQLiteConnectionAdapterTest.TestBeginTransaction;
var
  ReturnValue: IDBTransaction;
begin
  ReturnValue := fConnection.BeginTransaction;
  CheckTrue(Assigned(ReturnValue));
  CheckTrue(ReturnValue.TransactionName <> '');
end;

procedure TSQLiteConnectionAdapterTest.TestBeginTransaction_Exception;
begin
  fConnection := TSQLiteConnectionAdapter.Create(CreateFailDB);
  fConnection.AutoFreeConnection := True;
  ExpectedException:=ESQLiteAdapterException;
  fConnection.BeginTransaction;
end;

procedure TSQLiteConnectionAdapterTest.TestGetQueryLanguage;
begin
  CheckEquals(qlSQLite, fConnection.QueryLanguage);
end;

function TSQLiteTransactionAdapterTest.CreateAndBeginTransaction: IDBTransaction;
begin
  Result := fConnection.BeginTransaction;
end;

{$ENDREGION}


{$REGION 'TSQLiteTransactionAdapterTest'}

procedure TSQLiteTransactionAdapterTest.SetUp;
begin
  inherited;
  fTransaction := CreateAndBeginTransaction;
end;

procedure TSQLiteTransactionAdapterTest.TearDown;
begin
  fTransaction := nil;
  inherited;
end;

procedure TSQLiteTransactionAdapterTest.TestCommit;
begin
  InsertCustomer('Test', 15, 1.1);
  fTransaction.Commit;
  CheckEquals(1, GetCustomersCount);

  fTransaction := CreateAndBeginTransaction;
  DeleteAllCustomers;
  fTransaction.Commit;
  CheckEquals(0, GetCustomersCount);
end;

procedure TSQLiteTransactionAdapterTest.TestCommit_Exception;
begin
  fConnection.Disconnect;
  ExpectedException := ESQLiteAdapterException;
  fTransaction.Commit;
end;

procedure TSQLiteTransactionAdapterTest.TestRollback;
begin
  InsertCustomer('Test', 15, 1.1);
  fTransaction.Rollback;
  CheckEquals(0, GetCustomersCount);

  InsertCustomer('Test', 15, 1.1);
  fTransaction := CreateAndBeginTransaction;
  InsertCustomer('Test2', 15, 1.1);
  fTransaction.Rollback;
  CheckEquals(1, GetCustomersCount);
end;

procedure TSQLiteTransactionAdapterTest.TestRollback_Exception;
begin
  fConnection.Disconnect;
  ExpectedException := ESQLiteAdapterException;
  fTransaction.Rollback;
end;

{$ENDREGION}


initialization

RegisterTests('Spring.Persistence.Adapters', [
    TSQLiteExceptionHandlerTest.Suite,
    TSQLiteConnectionAdapterTest.Suite,
    TSQLiteStatementAdapterTest.Suite,
    TSQLiteResultSetAdapterTest.Suite,
    TSQLiteTransactionAdapterTest.Suite
  ]);

end.
