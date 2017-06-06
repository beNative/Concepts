unit TestPostgreSQLGenerator;

interface

uses
  TestFramework,
  Spring.Persistence.SQL.Interfaces;

type
  TPostgreSQLGeneratorTest = class(TTestCase)
  private
    FSQLGenerator: ISQLGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGeneratePagedQuery;
  end;

implementation

uses
  Spring.Persistence.SQL.Generators.PostgreSQL;


{$REGION 'TPostgreSQLGeneratorTest'}

procedure TPostgreSQLGeneratorTest.SetUp;
begin
  FSQLGenerator := TPostgreSQLGenerator.Create;
end;

procedure TPostgreSQLGeneratorTest.TearDown;
begin
  FSQLGenerator := nil;
end;

const
  SQL_PAGED_TEST = 'SELECT * FROM TESTDB.COMPANIES WHERE COMPANY = 1;';
  SQL_EXPECTED_PAGED = 'SELECT * FROM TESTDB.COMPANIES WHERE COMPANY = 1 LIMIT 10 OFFSET 0 ;';
  SQL_EXPECTED_PAGED_2 = 'SELECT * FROM TESTDB.COMPANIES WHERE COMPANY = 1 LIMIT 10 OFFSET 20 ;';

procedure TPostgreSQLGeneratorTest.TestGeneratePagedQuery;
var
  LSQL: string;
begin
  LSQL := FSQLGenerator.GeneratePagedQuery(SQL_PAGED_TEST, 10, 0);
  CheckEqualsString(SQL_EXPECTED_PAGED, LSQL);

  LSQL := FSQLGenerator.GeneratePagedQuery(SQL_PAGED_TEST, 10, 20);
  CheckEqualsString(SQL_EXPECTED_PAGED_2, LSQL);
end;

{$ENDREGION}


initialization
  RegisterTest('Spring.Persistence.Generators', TPostgreSQLGeneratorTest.Suite);

end.
