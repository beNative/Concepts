unit TestSQLServerSQLGenerator;

interface

uses
  TestFramework,
  Spring.Persistence.SQL.Generators.MSSQL;

type
  TMSSQLSQLGeneratorTest = class(TTestCase)
  private
    FSQLGenerator: TMSSQLServerSQLGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGeneratePagedQuery;
  end;

implementation

uses
  SysUtils;


{$REGION 'TMSSQLSQLGeneratorTest'}

procedure TMSSQLSQLGeneratorTest.SetUp;
begin
  FSQLGenerator := TMSSQLServerSQLGenerator.Create;
end;

procedure TMSSQLSQLGeneratorTest.TearDown;
begin
  FSQLGenerator.Free;
end;

const
  SQL_PAGED_TEST = 'SELECT * FROM TESTDB.COMPANIES WHERE COMPANY = 1;';
  SQL_EXPECTED_PAGED = 'SELECT * FROM ('+#13#10+
		'  SELECT *, ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS ORM_ROW_NUM FROM ('+#13#10+
		'    SELECT * FROM TESTDB.COMPANIES WHERE COMPANY = 1) AS ORM_TOTAL_1'+#13#10+
		'  ) AS ORM_TOTAL_2'+#13#10+
		' WHERE (ORM_ROW_NUM > 0) AND (ORM_ROW_NUM <= 0 + 10);';

procedure TMSSQLSQLGeneratorTest.TestGeneratePagedQuery;
var
  LSQL: string;
begin
  LSQL := FSQLGenerator.GeneratePagedQuery(SQL_PAGED_TEST, 10, 0);
  CheckEqualsString(Trim(SQL_EXPECTED_PAGED), Trim(LSQL));
end;

{$ENDREGION}


initialization
  RegisterTest('Spring.Persistence.Generators', TMSSQLSQLGeneratorTest.Suite);

end.

