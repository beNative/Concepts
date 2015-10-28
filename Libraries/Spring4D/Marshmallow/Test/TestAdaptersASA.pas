unit TestAdaptersASA;

interface

uses
  TestFramework,
  ADODB,
  Spring.Persistence.Adapters.ASA,
  Spring.Persistence.SQL.Generators.ASA;

type
  TASAConnectionAdapterTest = class(TTestCase)
  strict private
    FASAConnectionAdapter: TASAConnectionAdapter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetQueryLanguage;
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
  Spring.Persistence.SQL.Interfaces;


{$REGION 'TASAConnectionAdapterTest'}

procedure TASAConnectionAdapterTest.SetUp;
begin
  FASAConnectionAdapter := TASAConnectionAdapter.Create(TADOConnection.Create(nil));
  FASAConnectionAdapter.AutoFreeConnection := True;
end;

procedure TASAConnectionAdapterTest.TearDown;
begin
  FASAConnectionAdapter.Free;
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
  RegisterTest(TASASQLGeneratorTest.Suite);
  RegisterTest(TASAConnectionAdapterTest.Suite);

end.

