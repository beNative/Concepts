unit TestDatabaseManager;

interface

uses
  TestFramework,
  SysUtils,
  Spring.Persistence.Core.DatabaseManager,
  Spring.Persistence.Core.Interfaces,
  Spring.TestUtils;

type
  TDatabaseManagerTest = class(TTestCase)
  private
    FConnection: IDBConnection;
    FDatabaseManager: TDatabaseManager;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBuildDatabase;
    procedure TestUpdateDatabase;
    procedure TestEntityExists;
  end;

implementation

uses
  Spring.Persistence.Core.ConnectionFactory,
  TestEntities,
  SQLiteTable3;

const
  FILE_CONN_SQLITE = 'Conn_Sqlite.json';

function GetTableCount(const AConnection: IDBConnection): Integer;
var
  LStmt: IDBStatement;
begin
  LStmt := AConnection.CreateStatement;
  LStmt.SetSQLCommand('SELECT COUNT(*) FROM sqlite_master WHERE type=''table'';');
  Result := LStmt.ExecuteQuery.GetFieldValue(0);
end;

function GetTables(const AConnection: IDBConnection): string;
var
  LStmt: IDBStatement;
  LRes: IDBResultset;
begin
  Result := '';
  LStmt := AConnection.CreateStatement;
  LStmt.SetSQLCommand('SELECT name FROM sqlite_master WHERE type=''table'';');
  LRes := LStmt.ExecuteQuery;
  while not LRes.IsEmpty do
  begin
    Result := Result + LRes.GetFieldValue(0) + ' ';
    LRes.Next;
  end;
end;

procedure TDatabaseManagerTest.SetUp;
var
  sDir: string;
begin
  inherited;
  sDir := IncludeTrailingPathDelimiter(ExtractFileDir(PictureFilename));
  FConnection := TConnectionFactory.GetInstanceFromFile(dtSQLite, sDir + FILE_CONN_SQLITE);
  FDatabaseManager := TDatabaseManager.Create(FConnection);
end;

procedure TDatabaseManagerTest.TearDown;
begin
  FConnection := nil;
  FDatabaseManager.Free;
  inherited;
end;

procedure TDatabaseManagerTest.TestBuildDatabase;
var
  sTables: string;
  iCount: Integer;
begin
  CheckEquals(0, GetTableCount(FConnection));
  FDatabaseManager.BuildDatabase;

  iCount := 10;

  sTables := GetTables(FConnection);
  if Pos('sqlite_sequence', sTables) > 0 then
    Inc(iCount);

  CheckEquals(iCount, GetTableCount(FConnection));
end;

procedure TDatabaseManagerTest.TestEntityExists;
begin
  CheckFalse(FDatabaseManager.EntityExists(TCustomer));
  FDatabaseManager.BuildDatabase;
  CheckTrue(FDatabaseManager.EntityExists(TCustomer));
end;

procedure TDatabaseManagerTest.TestUpdateDatabase;
var
  LUpdateManager: TDatabaseManager;
  LConn: IDBConnection;
begin
  LConn := TConnectionFactory.GetInstance(dtSQLite, TSQLiteDatabase.Create(':memory:'));
  LConn.AutoFreeConnection := True;
  LUpdateManager := TDatabaseManager.Create(LConn);
  try
    LUpdateManager.BuildDatabase;
  finally
    LUpdateManager.Free;
  end;
  Pass;
end;

initialization
  RegisterTest('Spring.Persistence.Core', TDatabaseManagerTest.Suite);

end.
