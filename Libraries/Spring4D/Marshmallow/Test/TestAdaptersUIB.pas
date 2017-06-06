unit TestAdaptersUIB;

interface

uses
  StrUtils,
  TestFramework,
  uib,
  uibdataset,
  Spring.Persistence.Adapters.UIB,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.Session;

type
  TestTUIBResultSetAdapter = class(TTestCase)
  private
    FDataset: TUIBDataSet;
    FTransaction: TUIBTransaction;
    FUIBResultSetAdapter: TUIBResultSetAdapter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsEmpty;
    procedure TestNext;
    procedure TestGetFieldValue;
    procedure TestGetFieldValue1;
    procedure TestGetFieldCount;
  end;

  TestTUIBConnectionAdapter = class(TTestCase)
  private
    FConnection: IDBConnection;
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

  TestTUIBRegression = class(TTestCase)
  private
    FConnection: IDBConnection;
    FManager: TSession;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFindAll;
    procedure TestSave;
  end;

implementation

uses
  SysUtils,
  TestEntities,
  Spring.Collections,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.SQL.Interfaces;

var
  TestDB: TUIBDataBase = nil;
  ConnJson: string = '{' +
                     '  "uib.TUIBDataBase": {' +
                     '    "UserName": "SYSDBA",' +
                     '    "PassWord": "masterkey",' +
                     '    "DatabaseName": "%s"' +
                     '  }' +
                     '}';

function GetFirstValue(const ASQL: string): Variant;
var
  LTran: TUIBTransaction;
  LDataset: TUIBDataSet;
begin
  LDataset := TUIBDataSet.Create(nil);
  LTran := TUIBTransaction.Create(nil);
  LTran.DataBase := TestDB;
  try

    LDataset.Database := TestDB;
    LDataset.Transaction := LTran;
    LDataset.DisableControls;
    LDataset.SQL.Text := ASQL;
    LDataset.Open;

    Result := LDataset.Fields[0].Value;

  finally
    LTran.RemoveDatabase(TestDB);
    LTran.Free;
    LDataset.Free;
  end;
end;

procedure TestTUIBResultSetAdapter.SetUp;
begin
  FTransaction := TUIBTransaction.Create(nil);
  FTransaction.DataBase := TestDB;
  FDataset := TUIBDataSet.Create(nil);
  FDataset.Database := TestDB;
  FDataset.Transaction := FTransaction;
  FDataset.UniDirectional := True;
  FDataset.SQL.Text := 'select * from IMONES;';
  FDataset.Open;
  FUIBResultSetAdapter := TUIBResultSetAdapter.Create(FDataset, nil);
end;

procedure TestTUIBResultSetAdapter.TearDown;
begin
  FTransaction.Free;
  FUIBResultSetAdapter.Free;
  FUIBResultSetAdapter := nil;
end;

procedure TestTUIBResultSetAdapter.TestIsEmpty;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FUIBResultSetAdapter.IsEmpty;
  CheckFalse(ReturnValue);
end;

procedure TestTUIBResultSetAdapter.TestNext;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FUIBResultSetAdapter.Next;
  CheckTrue(ReturnValue);
end;

procedure TestTUIBResultSetAdapter.TestGetFieldValue;
var
  ReturnValue: Variant;
begin
  ReturnValue := FUIBResultSetAdapter.GetFieldValue(0);
  CheckEquals(2, Integer(ReturnValue));
end;

procedure TestTUIBResultSetAdapter.TestGetFieldValue1;
var
  ReturnValue: Variant;
begin
  ReturnValue := FUIBResultSetAdapter.GetFieldValue('IMONESID');
  CheckEquals(2, Integer(ReturnValue));
end;

procedure TestTUIBResultSetAdapter.TestGetFieldCount;
var
  ReturnValue: Integer;
begin
  ReturnValue := FUIBResultSetAdapter.GetFieldCount;
  CheckTrue(ReturnValue > 100);
end;

{ TestTUIBConnectionAdapter }

procedure TestTUIBConnectionAdapter.SetUp;
var
  sDir: string;
begin
  inherited;
  sDir := IncludeTrailingPathDelimiter(ExtractFileDir(PictureFilename));
  FConnection := TConnectionFactory.GetInstance(dtUIB, ConnJson);
end;

procedure TestTUIBConnectionAdapter.TearDown;
begin
  inherited;
  FConnection := nil;
end;


procedure TestTUIBConnectionAdapter.TestBeginTransaction;
var
  LTran: IDBTransaction;
  LVal: Variant;
  LStmt: IDBStatement;
begin
  LVal := GetFirstValue(Format('SELECT COUNT(*) FROM VEIKLOS WHERE VEIKLOSID = %d', [283]));
  CheckEquals(0, Integer(LVal));
  LTran := FConnection.BeginTransaction;

  LStmt := FConnection.CreateStatement;
  LStmt.SetSQLCommand('insert into VEIKLOS("VEIKLOSID","KODAS","PAVADINIMAS","SAVAITES_DIENOS",'+
    '"MENESIO_VALANDOS","SAVAITES_VALANDOS","IM_DALIS_SAV","ITERPE","ITERPTAS","KOREGAVO","KOREGUOTAS")'+
    ' values (''283'',''ZVER282'',''Valymo árenginiai, maðinos'',Null,Null,Null,Null,''SYSDBA'',''2007-02-02 12:26:24'',Null,Null); ');
  LStmt.Execute;

  LStmt.SetSQLCommand(Format('SELECT COUNT(*) FROM VEIKLOS WHERE VEIKLOSID = %d', [283]));
  LVal := LStmt.ExecuteQuery.GetFieldValue(0);
  CheckEquals(1, Integer(LVal));
  LTran.Rollback;

  LStmt.SetSQLCommand(Format('SELECT COUNT(*) FROM VEIKLOS WHERE VEIKLOSID = %d', [283]));
  LVal := LStmt.ExecuteQuery.GetFieldValue(0);
  CheckEquals(0, Integer(LVal));
end;

procedure TestTUIBConnectionAdapter.TestConnect;
begin
  FConnection.Disconnect;
  FConnection.Connect;
  CheckTrue(FConnection.IsConnected);
end;

procedure TestTUIBConnectionAdapter.TestCreateStatement;
var
  LStmt: IDBStatement;
begin
  LStmt := FConnection.CreateStatement;
  CheckTrue(Assigned(LStmt));
end;

procedure TestTUIBConnectionAdapter.TestDisconnect;
begin
  CheckTrue(FConnection.IsConnected);
  FConnection.Disconnect;
  CheckFalse(FConnection.IsConnected);
  FConnection.Connect;
  CheckTrue(FConnection.IsConnected);
end;

procedure TestTUIBConnectionAdapter.TestGetQueryLanguage;
begin
  CheckEquals(qlFirebird, FConnection.QueryLanguage);
end;

procedure TestTUIBConnectionAdapter.TestIsConnected;
begin
  CheckTrue(FConnection.IsConnected);
end;

{ TestTUIBRegression }

procedure TestTUIBRegression.SetUp;
var
  sDir: string;
begin
  inherited;
  sDir := IncludeTrailingPathDelimiter(ExtractFileDir(PictureFilename));
  FConnection := TConnectionFactory.GetInstance(dtUIB, ConnJson);
  FManager := TSession.Create(FConnection);
end;

procedure TestTUIBRegression.TearDown;
begin
  FManager.Free;
  FConnection := nil;
  inherited;
end;

procedure TestTUIBRegression.TestFindAll;
var
  AList: IList<TUIBCompany>;
begin
  AList := FManager.FindAll<TUIBCompany>();
  CheckEquals(4, AList.Count);
end;

procedure TestTUIBRegression.TestSave;
var
  LCompany: TUIBCompany;
  LTran: IDBTransaction;
  sTel: string;
  iCount: Integer;
begin
  LCompany := FManager.FindOne<TUIBCompany>(3);
  try
    CheckEquals(3, LCompany.ID);

    LTran := FManager.Connection.BeginTransaction;
    LCompany.Phone := '+3701258746';

    FManager.Save(LCompany);

    sTel := FManager.ExecuteScalar<string>('SELECT TELEFONAS FROM IMONES WHERE IMONESID = :0;', [3]);
    CheckEqualsString(LCompany.Phone, sTel);
  finally
    LCompany.Free;
  end;


  LCompany := TUIBCompany.Create;
  try
    LCompany.Name := 'ORM';
    LCompany.Phone := '118';

    FManager.Save(LCompany);
    sTel := FManager.ExecuteScalar<string>('SELECT TELEFONAS FROM IMONES WHERE PAVADINIMAS = :0;',
      [LCompany.Name]);
    CheckEqualsString(LCompany.Phone, sTel);

    iCount := FManager.ExecuteScalar<Integer>('SELECT COUNT(*) FROM IMONES;', []);
    CheckEquals(5, iCount);
  finally
    LCompany.Free;
  end;
end;

var
  FileName: string = 'ALGA.GDB';

procedure Init;
begin
  TestDB := TUIBDataBase.Create(nil);
  // Register any test cases with the test runner
  if FileExists(FileName) then
  begin
    FileName := ExpandFileName(FileName);
    TestDB.UserName := 'SYSDBA';
    TestDB.PassWord := 'masterkey';
    TestDB.DatabaseName := 'localhost/gds_db:' + FileName;
    try
      TestDB.Connected := True;
      if TestDB.Connected then
      begin
        ConnJson := Format(ConnJson, [ReplaceStr(TestDB.DatabaseName, '\', '\\')]);
        CreateTestTables(TConnectionFactory.GetInstance(dtUIB, ConnJson),
          [TUIBCompany]);
        RegisterTests('Spring.Persistence.Adapters', [
          TestTUIBResultSetAdapter.Suite,
          TestTUIBConnectionAdapter.Suite,
          TestTUIBRegression.Suite
        ]);
      end;
    except
      raise;
    end;
  end;
end;

initialization
  Init;

finalization
  TestDB.Free;

end.

