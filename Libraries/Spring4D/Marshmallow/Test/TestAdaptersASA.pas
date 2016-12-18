unit TestAdaptersASA;

interface

uses
  TestFramework,
  ADODB,
  Rtti,
  Spring.Mocking,
  Spring.Persistence.Adapters.ADO,
  Spring.Persistence.Adapters.ASA,
  Spring.Persistence.SQL.Generators.ASA,
  Spring.TestUtils,
  TestAdaptersADO;

type
  TASAConnectionAdapterTest = class(TBaseADOAdapterTest)
  strict private
    FASAConnectionAdapter: TASAConnectionAdapter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetQueryLanguage;
    procedure TestTransaction;
    procedure TestConnectException;
    procedure TestBeginTransactionException;
  end;

  TASASQLGeneratorTest = class(TTestCase)
  strict private
    FASASQLGenerator: TASASQLGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateGetLastInsertId;
  end;

implementation

uses
  SysUtils,
  Spring.Persistence.SQL.Interfaces;


{$REGION 'TASAConnectionAdapterTest'}

procedure TASAConnectionAdapterTest.SetUp;
begin
  inherited;
  FASAConnectionAdapter := TASAConnectionAdapter.Create(TADOConnection.Create(nil));
  FASAConnectionAdapter.AutoFreeConnection := True;
end;

procedure TASAConnectionAdapterTest.TearDown;
begin
  FASAConnectionAdapter.Free;
  inherited;
end;

procedure TASAConnectionAdapterTest.TestBeginTransactionException;
begin
  ExpectedException := EADOAdapterException;
  FASAConnectionAdapter.BeginTransaction;
end;

procedure TASAConnectionAdapterTest.TestConnectException;
begin
  ExpectedException := EADOAdapterException;
  FASAConnectionAdapter.Connect;
end;

procedure TASAConnectionAdapterTest.TestTransaction;
begin
  FASAConnectionAdapter.Connection.ConnectionObject := fMockConnectionObject;

  // Test connect exception
  CheckException(EADOAdapterException,
    procedure begin FASAConnectionAdapter.BeginTransaction end);

  SetupOpen;

  // Test commit - with exception
  SetupExecute(['BEGIN TRANSACTION T1', 'COMMIT TRANSACTION T1']);
  with FASAConnectionAdapter.BeginTransaction do
  begin
    Commit;
    // Next commit is not allowed, simulates exception in the driver
    CheckException(EADOAdapterException,
      procedure begin Commit end);
  end;

  // Test rollback - with exception
  SetupExecute(['BEGIN TRANSACTION T2', 'ROLLBACK TRANSACTION T2']);
  with FASAConnectionAdapter.BeginTransaction do
  begin
    Rollback;
    // Next rollback is not allowed, simulates exception in the driver
    CheckException(EADOAdapterException,
      procedure begin Rollback end);
  end;
end;

procedure TASAConnectionAdapterTest.TestGetQueryLanguage;
begin
  CheckEquals(qlASA, FASAConnectionAdapter.QueryLanguage);
end;

{$ENDREGION}


{$REGION 'TASASQLGeneratorTest'}

procedure TASASQLGeneratorTest.SetUp;
begin
  FASASQLGenerator := TASASQLGenerator.Create;
end;

procedure TASASQLGeneratorTest.TearDown;
begin
  FASASQLGenerator.Free;
end;

procedure TASASQLGeneratorTest.TestGenerateGetLastInsertId;
var
  ReturnValue: string;
begin
  ReturnValue := FASASQLGenerator.GenerateGetLastInsertId(nil);
  CheckEqualsString('SELECT @@IDENTITY;', ReturnValue);
end;

{$ENDREGION}


initialization
  RegisterTests('Spring.Persistence.Adapters', [
    TASASQLGeneratorTest.Suite,
    TASAConnectionAdapterTest.Suite
  ]);

end.
