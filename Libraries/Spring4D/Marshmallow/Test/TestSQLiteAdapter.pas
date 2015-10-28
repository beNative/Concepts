unit TestSQLiteAdapter;

interface

uses
  TestFramework,
  SysUtils,
  SQLiteTable3,
  Spring.Collections,
  Spring.Persistence.Adapters.SQLite,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.TestUtils;

type
  TSQLiteResultSetAdapterTest = class(TTestCase)
  private
    FSQLiteResultSetAdapter: TSQLiteResultSetAdapter;
    FDataset: ISQLiteTable;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsEmpty;
    procedure TestNext;
    procedure TestGetFieldValue;
  end;

  TSQLiteStatementAdapterTest = class(TTestCase)
  private
    FSQLiteStatementAdapter: TSQLiteStatementAdapter;
    FCommandText: string;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetSQLCommand;
    procedure TestSetParams;
    procedure TestExecute;
    procedure TestExecuteQuery;
  end;

  TSQLiteConnectionAdapterTest = class(TTestCase)
  private
    FSQLiteConnectionAdapter: TSQLiteConnectionAdapter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnect;
    procedure TestDisconnect;
    procedure TestIsConnected;
    procedure TestCreateStatement;
    procedure TestBeginTransaction;
    procedure TestGetQueryLanguage;
  end;

  TSQLiteTransactionAdapterTest = class(TTestCase)
  private
    FSQLiteTransactionAdapter: TSQLiteTransactionAdapter;
  protected
    function CreateAndBeginTransaction: TSQLiteTransactionAdapter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCommit;
    procedure TestRollback;
  end;

implementation

uses
  DB,
  Spring.Persistence.SQL.Interfaces;

var
  TestDB: TSQLiteDatabase = nil;

const
  SQL_SELECT_ALL = 'SELECT * FROM CUSTOMERS;';

procedure CreateTables;
begin
  TestDB.ExecSQL('CREATE TABLE IF NOT EXISTS CUSTOMERS ([ID] INTEGER PRIMARY KEY, [AGE] INTEGER NULL,'+
    '[NAME] VARCHAR (255), [HEIGHT] FLOAT, [PICTURE] BLOB); ');
  if not TestDB.TableExists('CUSTOMERS') then
    raise Exception.Create('Table CUSTOMERS does not exist');
end;

function InsertCustomer(AName: string; AAge: Integer; AHeight: Double): Int64;
var
  LStmt: ISQLitePreparedStatement;
begin
  Result := -1;
  LStmt := TestDB.GetPreparedStatementIntf('INSERT INTO CUSTOMERS ([NAME], [AGE], [HEIGHT]) VALUES (:NAME, :AGE, :HEIGHT);');

  LStmt.SetParamInt(':AGE', AAge);
  LStmt.SetParamText(':NAME', AName);
  LStmt.SetParamFloat(':HEIGHT', AHeight);

  if LStmt.ExecSQL then
  begin
    Result := TestDB.GetLastInsertRowID;
  end;
end;

procedure DeleteCustomer(AID: Integer);
begin
  TestDB.ExecSQL('DELETE FROM CUSTOMERS WHERE ID = ?', [AID]);
end;

procedure DeleteAllCustomers;
begin
  TestDB.ExecSQL('DELETE FROM CUSTOMERS;');
end;

function GetCustomersCount: Integer;
var
  LTable: ISQLiteTable;
begin
  LTable := TestDB.GetUniTableIntf('SELECT COUNT(*) FROM CUSTOMERS;');
  Result := LTable.Fields[0].AsIntegerDef;
end;


procedure TSQLiteResultSetAdapterTest.SetUp;
begin
  DeleteAllCustomers;
  FDataset := TestDB.GetUniTableIntf(SQL_SELECT_ALL);
  FSQLiteResultSetAdapter := TSQLiteResultSetAdapter.Create(FDataset);
end;

procedure TSQLiteResultSetAdapterTest.TearDown;
begin
  DeleteAllCustomers;
  FSQLiteResultSetAdapter.Free;
  FSQLiteResultSetAdapter := nil;
  FDataset := nil;
end;

procedure TSQLiteResultSetAdapterTest.TestIsEmpty;
var
  LID: Int64;
  LAdapter: TSQLiteResultSetAdapter;
  LTable: ISQLiteTable;
begin
  CheckTrue(FSQLiteResultSetAdapter.IsEmpty);

  LID := InsertCustomer('Test', 15, 1.1);

  LTable := TestDB.GetUniTableIntf(SQL_SELECT_ALL);

  LAdapter := TSQLiteResultSetAdapter.Create(LTable);
  try
    CheckFalse(LAdapter.IsEmpty);
  finally
    LAdapter.Free;
  end;

  DeleteCustomer(LID);

  LTable := TestDB.GetUniTableIntf(SQL_SELECT_ALL);

  LAdapter := TSQLiteResultSetAdapter.Create(LTable);
  try
    CheckTrue(LAdapter.IsEmpty);
  finally
    LAdapter.Free;
  end;
end;

procedure TSQLiteResultSetAdapterTest.TestNext;
var
  LID1, LID2: Int64;
  LAdapter: TSQLiteResultSetAdapter;
  LTable: ISQLiteTable;
begin
  CheckFalse(FSQLiteResultSetAdapter.Next);

  LID1 := InsertCustomer('Test', 15, 1.1);
  LID2 := InsertCustomer('Test2', 65, 95.1);
  try
    LTable := TestDB.GetUniTableIntf(SQL_SELECT_ALL);

    LAdapter := TSQLiteResultSetAdapter.Create(LTable);
    try
      CheckTrue(LAdapter.Next);
      CheckFalse(LAdapter.Next);
    finally
      LAdapter.Free;
    end;
  finally
    DeleteCustomer(LID1);
    DeleteCustomer(LID2);
  end;
end;

procedure TSQLiteResultSetAdapterTest.TestGetFieldValue;
var
  LID1, LID2: Int64;
  LAdapter: TSQLiteResultSetAdapter;
  LTable: ISQLiteTable;
begin
  LID1 := InsertCustomer('Test', 15, 1.1);
  LID2 := InsertCustomer('Test2', 65, 95.1);
  try
    LTable := TestDB.GetUniTableIntf(SQL_SELECT_ALL);

    LAdapter := TSQLiteResultSetAdapter.Create(LTable);
    try
      CheckTrue(LAdapter.GetFieldValue(0) = LID1);
      CheckEquals(15, LAdapter.GetFieldValue(1));
      CheckEquals(15, LAdapter.GetFieldValue('AGE'));

      CheckEqualsString('Test', LAdapter.GetFieldValue(2));
      CheckEqualsString('Test', LAdapter.GetFieldValue('NAME'));

    finally
      LAdapter.Free;
    end;
  finally
    DeleteCustomer(LID1);
    DeleteCustomer(LID2);
  end;
end;

procedure TSQLiteStatementAdapterTest.SetUp;
var
  LStmt: ISQLitePreparedStatement;
begin
  FCommandText := 'SELECT * FROM CUSTOMERS WHERE NAME = :NAME;';

  LStmt := TestDB.GetPreparedStatementIntf(SQL_SELECT_ALL);

  FSQLiteStatementAdapter := TSQLiteStatementAdapter.Create(LStmt);
end;

procedure TSQLiteStatementAdapterTest.TearDown;
begin
  FSQLiteStatementAdapter.Free;
  FSQLiteStatementAdapter := nil;
end;

procedure TSQLiteStatementAdapterTest.TestSetSQLCommand;
begin
  FSQLiteStatementAdapter.SetSQLCommand(FCommandText);
  Pass;
end;

procedure TSQLiteStatementAdapterTest.TestSetParams;
var
  Params: IList<TDBParam>;
  LParam: TDBParam;
begin
  Params := TCollections.CreateObjectList<TDBParam>;
  LParam := TDBParam.Create(':NAME', 'Test');
  Params.Add(LParam);

  FSQLiteStatementAdapter.SetParams(Params);
  Pass;
end;

procedure TSQLiteStatementAdapterTest.TestExecute;
var
  ReturnValue: NativeUInt;
begin
  FSQLiteStatementAdapter.SetSQLCommand('INSERT INTO CUSTOMERS(NAME) VALUES (:NAME);');

  TestSetParams;

  ReturnValue := FSQLiteStatementAdapter.Execute;

  CheckEquals(1, ReturnValue);
end;

procedure TSQLiteStatementAdapterTest.TestExecuteQuery;
var
  ReturnValue: IDBResultset;
  LStmt, LAdapter: TSQLiteStatementAdapter;
begin
  FSQLiteStatementAdapter.SetSQLCommand(FCommandText);

  TestSetParams;

  ReturnValue := FSQLiteStatementAdapter.ExecuteQuery;

  CheckTrue(Assigned(ReturnValue));
  CheckTrue(ReturnValue.IsEmpty);

  InsertCustomer('Test', 15, 1.1);
  LAdapter := FSQLiteStatementAdapter;
  LStmt := TSQLiteStatementAdapter.Create(TestDB.GetPreparedStatement(FCommandText));
  try
    LStmt.SetSQLCommand(FCommandText);
    FSQLiteStatementAdapter := LStmt;
    TestSetParams;
    FSQLiteStatementAdapter := LAdapter;
    ReturnValue := LStmt.ExecuteQuery;
    CheckTrue(Assigned(ReturnValue));
    CheckFalse(ReturnValue.IsEmpty);
  finally
    LStmt.Free;
  end;
end;

procedure TSQLiteConnectionAdapterTest.SetUp;
begin
  FSQLiteConnectionAdapter := TSQLiteConnectionAdapter.Create(TestDB);
end;

procedure TSQLiteConnectionAdapterTest.TearDown;
begin
  FSQLiteConnectionAdapter.Free;
  FSQLiteConnectionAdapter := nil;
end;

procedure TSQLiteConnectionAdapterTest.TestConnect;
begin
  FSQLiteConnectionAdapter.Connect;

  CheckTrue(TestDB.Connected);
end;

procedure TSQLiteConnectionAdapterTest.TestDisconnect;
begin
  FSQLiteConnectionAdapter.Disconnect;

  CheckFalse(TestDB.Connected);
end;

procedure TSQLiteConnectionAdapterTest.TestIsConnected;
var
  ReturnValue: Boolean;
begin
  FSQLiteConnectionAdapter.Disconnect;
  ReturnValue := FSQLiteConnectionAdapter.IsConnected;
  CheckFalse(ReturnValue);
  FSQLiteConnectionAdapter.Connect;
  ReturnValue := FSQLiteConnectionAdapter.IsConnected;
  CheckTrue(ReturnValue);
end;

procedure TSQLiteConnectionAdapterTest.TestCreateStatement;
var
  ReturnValue: IDBStatement;
begin
  ReturnValue := FSQLiteConnectionAdapter.CreateStatement;

  CheckTrue(Assigned(ReturnValue));
end;

procedure TSQLiteConnectionAdapterTest.TestBeginTransaction;
var
  ReturnValue: IDBTransaction;
begin
  ReturnValue := FSQLiteConnectionAdapter.BeginTransaction;
  CheckTrue(Assigned(ReturnValue));
  CheckTrue(ReturnValue.TransactionName <> '');
end;

procedure TSQLiteConnectionAdapterTest.TestGetQueryLanguage;
begin
  CheckEquals(qlSQLite, FSQLiteConnectionAdapter.QueryLanguage);
end;

function TSQLiteTransactionAdapterTest.CreateAndBeginTransaction: TSQLiteTransactionAdapter;
begin
  Result := TSQLiteTransactionAdapter.Create(TestDB);
  Result.TransactionName := 'T1';
  Result.Transaction.ExecSQL('SAVEPOINT T1');
end;

procedure TSQLiteTransactionAdapterTest.SetUp;
begin
  FSQLiteTransactionAdapter := CreateAndBeginTransaction;
end;

procedure TSQLiteTransactionAdapterTest.TearDown;
begin
  FSQLiteTransactionAdapter.Free;
  FSQLiteTransactionAdapter := nil;
end;

procedure TSQLiteTransactionAdapterTest.TestCommit;
begin
  InsertCustomer('Test', 15, 1.1);
  FSQLiteTransactionAdapter.Commit;
  CheckEquals(1, GetCustomersCount);

  FSQLiteTransactionAdapter.Free;
  FSQLiteTransactionAdapter := CreateAndBeginTransaction;
  DeleteAllCustomers;
  FSQLiteTransactionAdapter.Commit;
  CheckEquals(0, GetCustomersCount);
end;

procedure TSQLiteTransactionAdapterTest.TestRollback;
begin
  InsertCustomer('Test', 15, 1.1);
  FSQLiteTransactionAdapter.Rollback;
  CheckEquals(0, GetCustomersCount);

  InsertCustomer('Test', 15, 1.1);
  FSQLiteTransactionAdapter.Free;
  FSQLiteTransactionAdapter := CreateAndBeginTransaction;
  InsertCustomer('Test2', 15, 1.1);
  FSQLiteTransactionAdapter.Rollback;
  CheckEquals(1, GetCustomersCount);
end;

type
  TSQLiteEvents = class
  public
    class procedure DoOnAfterOpen(Sender: TObject);
  end;

{ TSQLiteEvents }

class procedure TSQLiteEvents.DoOnAfterOpen(Sender: TObject);
begin
  CreateTables;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TSQLiteResultSetAdapterTest.Suite);
  RegisterTest(TSQLiteStatementAdapterTest.Suite);
  RegisterTest(TSQLiteConnectionAdapterTest.Suite);
  RegisterTest(TSQLiteTransactionAdapterTest.Suite);

  TestDB := TSQLiteDatabase.Create(':memory:');

  //create tables
  TestDB.OnAfterOpen := TSQLiteEvents.DoOnAfterOpen;

  CreateTables;

  //insert data

finalization
  TestDB.Free;

end.

