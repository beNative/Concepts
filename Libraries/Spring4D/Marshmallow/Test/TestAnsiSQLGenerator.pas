unit TestAnsiSQLGenerator;

interface

uses
  TestFramework,
  Spring.Collections,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  TAnsiSQLGeneratorTest = class(TTestCase)
  private
    FSQLGenerator: ISQLGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateSelect;
    procedure TestGenerateInsert;
    procedure TestGenerateUpdate;
    procedure TestGenerateDelete;
    procedure TestGenerateCreateTable;
    procedure TestGenerateCreateFK;
    procedure TestGenerateCreateSequence;
    procedure TestGenerateGetNextSequenceValue;
    procedure TestGenerateGetLastInsertId;
    procedure TestGeneratePagedQuery;
    procedure TestGenerateGetQueryCount;

    procedure TestGenerateSelectWithAggregation;
    procedure TestGenerateSelectWithNestedAggregation;
  end;

implementation

uses
  TestEntities,
  Spring.TestUtils,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Mapping.Attributes;

function CreateTestTable: TSQLTable;
begin
  Result := TSQLTable.Create;
  Result.Schema := 'TEST';
  Result.Name := 'CUSTOMERS';
  Result.Description := 'Customers table';
end;

function CreateTestJoinTable: TSQLTable;
begin
  Result := TSQLTable.Create(1);
  Result.Schema := 'TEST';
  Result.Name := 'PRODUCTS';
  Result.Description := 'Products table';
end;

function CreateTestCOUNTRYTable: TSQLTable;
begin
  Result := TSQLTable.Create(2);
  Result.Schema := 'TEST';
  Result.Name := 'COUNTRIES';
  Result.Description := 'Countries table';
end;

procedure TAnsiSQLGeneratorTest.SetUp;
begin
  FSQLGenerator := TAnsiSQLGenerator.Create;
end;

procedure TAnsiSQLGeneratorTest.TearDown;
begin
  FSQLGenerator := nil;
end;

const
  SQL_SELECT_TEST_SIMPLE = 'SELECT t0."NAME", t0."AGE", t0."HEIGHT"' + sLineBreak +
    ' FROM TEST.CUSTOMERS t0;';

  SQL_SELECT_TEST_JOIN = 'SELECT t0."NAME", t0."AGE", t0."HEIGHT"' + sLineBreak +
    ' FROM TEST.CUSTOMERS t0' + sLineBreak +
    '  INNER JOIN TEST.PRODUCTS t1 ON t1."ID" = t0."PRODID"' +
    ';';

  SQL_SELECT_TEST_JOIN_2 = 'SELECT t0."NAME", t0."AGE", t0."HEIGHT", t2."COUNTRYNAME"' + sLineBreak +
    ' FROM TEST.CUSTOMERS t0' + sLineBreak +
    '  INNER JOIN TEST.PRODUCTS t1 ON t1."ID" = t0."PRODID"' +sLineBreak +
    '  LEFT OUTER JOIN TEST.COUNTRIES t2 ON t2."ID" = t0."COUNTRYID"' +
    ';';

  SQL_SELECT_TEST_JOIN_2_ORDER = 'SELECT t0."NAME", t0."AGE", t0."HEIGHT", t2."COUNTRYNAME"' + sLineBreak +
    ' FROM TEST.CUSTOMERS t0' + sLineBreak +
    '  INNER JOIN TEST.PRODUCTS t1 ON t1."ID" = t0."PRODID"' + sLineBreak +
    '  LEFT OUTER JOIN TEST.COUNTRIES t2 ON t2."ID" = t0."COUNTRYID"' + sLineBreak +
    '  ORDER BY t0."AGE" DESC'+
    ';';

  SQL_SELECT_TEST_JOIN_2_ORDER_MULTIPLE = 'SELECT t0."NAME", t0."AGE", t0."HEIGHT", t2."COUNTRYNAME"' + sLineBreak +
    ' FROM TEST.CUSTOMERS t0' + sLineBreak +
    '  INNER JOIN TEST.PRODUCTS t1 ON t1."ID" = t0."PRODID"' + sLineBreak +
    '  LEFT OUTER JOIN TEST.COUNTRIES t2 ON t2."ID" = t0."COUNTRYID"' + sLineBreak +
    '  ORDER BY t0."AGE" DESC, t2."COUNTRYNAME" ASC'+
    ';';

  SQL_SELECT_TEST_JOIN_2_ORDER_GROUP = 'SELECT t0."NAME", t0."AGE", t0."HEIGHT", t2."COUNTRYNAME"' + sLineBreak +
    ' FROM TEST.CUSTOMERS t0' + sLineBreak +
    '  INNER JOIN TEST.PRODUCTS t1 ON t1."ID" = t0."PRODID"' + sLineBreak +
    '  LEFT OUTER JOIN TEST.COUNTRIES t2 ON t2."ID" = t0."COUNTRYID"' + sLineBreak +
    '  GROUP BY t0."HEIGHT", t0."NAME", t0."AGE", t2."COUNTRYNAME"' + sLineBreak +
    '  ORDER BY t0."AGE" DESC, t2."COUNTRYNAME" ASC' +
    ';';

  SQL_SELECT_TEST_JOIN_2_ORDER_GROUP_WHERE = 'SELECT t0."NAME", t0."AGE", t0."HEIGHT", t2."COUNTRYNAME"' + sLineBreak +
    ' FROM TEST.CUSTOMERS t0' + sLineBreak +
    '  INNER JOIN TEST.PRODUCTS t1 ON t1."ID" = t0."PRODID"' + sLineBreak +
    '  LEFT OUTER JOIN TEST.COUNTRIES t2 ON t2."ID" = t0."COUNTRYID"' + sLineBreak +
    '  WHERE t0."NAME" = :NAME' + sLineBreak +
    '  GROUP BY t0."HEIGHT", t0."NAME", t0."AGE", t2."COUNTRYNAME"' + sLineBreak +
    '  ORDER BY t0."AGE" DESC, t2."COUNTRYNAME" ASC' +
    ';';

procedure TAnsiSQLGeneratorTest.TestGenerateSelect;
var
  sSql: string;
  LCommand: TSelectCommand;
  LTable, LJoinTable, LCountriesTable: TSQLTable;
  LJoin: TSQLJoin;
begin
  LTable := CreateTestTable;
  LJoinTable := CreateTestJoinTable;
  LCountriesTable := CreateTestCOUNTRYTable;
  LCommand := TSelectCommand.Create(LTable);
  try
    LCommand.SelectFields.Add(TSQLSelectField.Create('NAME', LTable));
    LCommand.SelectFields.Add(TSQLSelectField.Create('AGE', LTable));
    LCommand.SelectFields.Add(TSQLSelectField.Create('HEIGHT', LTable));

    sSql := FSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsString(SQL_SELECT_TEST_SIMPLE, sSql);

    LJoin := TSQLJoin.Create(jtInner);
    LJoin.Segments.Add(
      TSQLJoinSegment.Create(
        TSQLField.Create('ID', LJoinTable),
        TSQLField.Create('PRODID', LTable)));
    LCommand.Joins.Add(LJoin);

    sSql := FSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsString(SQL_SELECT_TEST_JOIN, sSql);

    LCommand.SelectFields.Add(TSQLSelectField.Create('COUNTRYNAME', LCountriesTable));
    LJoin := TSQLJoin.Create(jtLeft);
    LJoin.Segments.Add(
      TSQLJoinSegment.Create(
        TSQLField.Create('ID', LCountriesTable),
        TSQLField.Create('COUNTRYID', LTable)));
    LCommand.Joins.Add(LJoin);

    sSql := FSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsString(SQL_SELECT_TEST_JOIN_2, sSql);

    LCommand.OrderByFields.Add(TSQLOrderByField.Create('AGE', LTable, stDescending));

    sSql := FSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsString(SQL_SELECT_TEST_JOIN_2_ORDER, sSql);

    LCommand.OrderByFields.Add(TSQLOrderByField.Create('COUNTRYNAME', LCountriesTable, stAscending));
    sSql := FSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsString(SQL_SELECT_TEST_JOIN_2_ORDER_MULTIPLE, sSql);

    LCommand.GroupByFields.Add(TSQLGroupByField.Create('HEIGHT', LTable));
    LCommand.GroupByFields.Add(TSQLGroupByField.Create('NAME', LTable));
    LCommand.GroupByFields.Add(TSQLGroupByField.Create('AGE', LTable));
    LCommand.GroupByFields.Add(TSQLGroupByField.Create('COUNTRYNAME', LCountriesTable));

    sSql := FSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsString(SQL_SELECT_TEST_JOIN_2_ORDER_GROUP, sSql);

    LCommand.WhereFields.Add(TSQLWhereField.Create('NAME', LTable));

    sSql := FSQLGenerator.GenerateSelect(LCommand);
    CheckEqualsString(SQL_SELECT_TEST_JOIN_2_ORDER_GROUP_WHERE, sSql);
  finally
    LTable.Free;
    LJoinTable.Free;
    LCountriesTable.Free;
    LCommand.Free;
  end;
end;

procedure TAnsiSQLGeneratorTest.TestGenerateSelectWithAggregation;
const
  expectedSql =
    'SELECT t0."ID", t0."DETAIL1_ID", t0."DETAIL2_ID", t1."ID" AS t1$ID, t2."ID" AS t2$ID' + sLineBreak +
    ' FROM MASTER t0' + sLineBreak +
    '  LEFT OUTER JOIN DETAIL t1 ON t1."ID" = t0."DETAIL1_ID"' + sLineBreak +
    '  LEFT OUTER JOIN DETAIL t2 ON t2."ID" = t0."DETAIL2_ID"' +
    ';';
var
  select: TSelectCommand;
  sql: string;
begin
  select := TSelectCommand.Create(TMasterEntity);
  try
    sql := FSQLGenerator.GenerateSelect(select);
    CheckEqualsString(expectedSql, sql);
  finally
    select.Free;
  end;
end;

procedure TAnsiSQLGeneratorTest.TestGenerateSelectWithNestedAggregation;
const
  expectedSql =
    'SELECT t0."SID", t0."SBS_SID", t0."TAX_CODE_RULE_SID", t1."SID" AS t1$SID, t1."SBS_SID" AS t1$SBS_SID, t2."SID" AS t2$SID, t3."SID" AS t3$SID' + sLineBreak +
    ' FROM TAX t0' + sLineBreak +
    '  LEFT OUTER JOIN TAX_CODE_RULE t1 ON t1."SID" = t0."TAX_CODE_RULE_SID"' + sLineBreak +
    '  LEFT OUTER JOIN SUBSIDIARY t2 ON t2."SID" = t1."SBS_SID"' + sLineBreak +
    '  LEFT OUTER JOIN SUBSIDIARY t3 ON t3."SID" = t0."SBS_SID";';
var
  select: TSelectCommand;
  sql: string;
begin
  select := TSelectCommand.Create(TTax);
  try
    sql := FSQLGenerator.GenerateSelect(select);
    CheckEqualsString(expectedSql, sql);
  finally
    select.Free;
  end;
end;

const
  SQL_INSERT_TEST = 'INSERT INTO TEST.CUSTOMERS ('+ sLineBreak +
    '  "NAME", "AGE", "HEIGHT")'+ sLineBreak +
    '  VALUES ('+ sLineBreak +
    ':NAME1, :AGE1, :HEIGHT1);';

  SQL_INSERT_TEST_WITHOUT_SCHEMA = 'INSERT INTO CUSTOMERS ('+ sLineBreak +
    '  "NAME", "AGE", "HEIGHT")'+ sLineBreak +
    '  VALUES ('+ sLineBreak +
    ':NAME1, :AGE1, :HEIGHT1);';

procedure TAnsiSQLGeneratorTest.TestGenerateInsert;
var
  ReturnValue: string;
  LCommand: TInsertCommand;
  LTable: TSQLTable;
begin
  LTable := CreateTestTable;
  LCommand := TInsertCommand.Create(LTable);
  try
    LCommand.InsertFields.Add(TSQLInsertField.Create('NAME', LTable, nil, ':NAME1'));
    LCommand.InsertFields.Add(TSQLInsertField.Create('AGE', LTable, nil, ':AGE1'));
    LCommand.InsertFields.Add(TSQLInsertField.Create('HEIGHT', LTable, nil, ':HEIGHT1'));

    ReturnValue := FSQLGenerator.GenerateInsert(LCommand);
    CheckEqualsString(SQL_INSERT_TEST, ReturnValue);

    LTable.Schema := '';
    ReturnValue := FSQLGenerator.GenerateInsert(LCommand);
    CheckEqualsString(SQL_INSERT_TEST_WITHOUT_SCHEMA, ReturnValue);
  finally
    LCommand.Free;
    LTable.Free;
  end;
end;

procedure TAnsiSQLGeneratorTest.TestGeneratePagedQuery;
const
  SQL_PAGED_TEST = 'SELECT * FROM TEST.CUSTOMERS WHERE CUSTID = 1;';
  SQL_EXPECTED_PAGED = 'SELECT * FROM TEST.CUSTOMERS WHERE CUSTID = 1 LIMIT 0, 10;';
  SQL_EXPECTED_PAGED_2 = 'SELECT * FROM TEST.CUSTOMERS WHERE CUSTID = 1 LIMIT 20, 10;';
var
  LSQL: string;
begin
  LSQL := FSQLGenerator.GeneratePagedQuery(SQL_PAGED_TEST, 10, 0);
  CheckEqualsString(SQL_EXPECTED_PAGED, LSQL);

  LSQL := FSQLGenerator.GeneratePagedQuery(SQL_PAGED_TEST, 10, 20);
  CheckEqualsString(SQL_EXPECTED_PAGED_2, LSQL);
end;

const
  SQL_UPDATE_TEST =
    'UPDATE TEST.CUSTOMERS SET ' + sLineBreak +
    '"NAME" = :NAME1, "AGE" = :AGE1, "HEIGHT" = :HEIGHT1' + sLineBreak +
    ' WHERE "ID" = :ID1;';

procedure TAnsiSQLGeneratorTest.TestGenerateUpdate;
var
  ReturnValue: string;
  LCommand: TUpdateCommand;
  LTable: TSQLTable;
begin
  LTable := CreateTestTable;
  LCommand := TUpdateCommand.Create(LTable);
  try
    LCommand.UpdateFields.Add(TSQLUpdateField.Create('NAME', LTable, nil,':NAME1'));
    LCommand.UpdateFields.Add(TSQLUpdateField.Create('AGE', LTable, nil, ':AGE1'));
    LCommand.UpdateFields.Add(TSQLUpdateField.Create('HEIGHT', LTable, nil, ':HEIGHT1'));
    LCommand.WhereFields.Add(TSQLWhereField.Create('ID', LTable, nil, ':ID1'));

    ReturnValue := FSQLGenerator.GenerateUpdate(LCommand);
    CheckEqualsString(SQL_UPDATE_TEST, ReturnValue);
  finally
    LCommand.Free;
    LTable.Free;
  end;
end;

const
  SQL_DELETE_TEST =
    'DELETE FROM TEST.CUSTOMERS' + sLineBreak +
    ' WHERE "ID" = :ID1;';

procedure TAnsiSQLGeneratorTest.TestGenerateDelete;
var
  ReturnValue: string;
  LCommand: TDeleteCommand;
  LTable: TSQLTable;
begin
  LTable := CreateTestTable;
  LCommand := TDeleteCommand.Create(LTable);
  try
    LCommand.WhereFields.Add(TSQLWhereField.Create('ID', LTable, nil, ':ID1'));

    ReturnValue := FSQLGenerator.GenerateDelete(LCommand);
    CheckEqualsString(SQL_DELETE_TEST, ReturnValue);
  finally
    LCommand.Free;
    LTable.Free;
  end;
end;

procedure TAnsiSQLGeneratorTest.TestGenerateCreateTable;
var
  ReturnValue: IList<string>;
  LCommand: TCreateTableCommand;
  LTable: TSQLTable;
  LCols: IList<ColumnAttribute>;
begin
  LTable := CreateTestTable;
  LCommand := TCreateTableCommand.Create(LTable);
  try
    LCols := TEntityCache.Get(TCustomer).Columns;
    LCommand.SetCommandFieldsFromColumns(LCols);

    ReturnValue := FSQLGenerator.GenerateCreateTable(LCommand);
    CheckTrue(ReturnValue.Any);
  finally
    LTable.Free;
    LCommand.Free;
    LCols := nil;
  end;
end;

procedure TAnsiSQLGeneratorTest.TestGenerateCreateFK;
var
  LSQL: IList<string>;
  LCommand: TCreateForeignKeyCommand;
  LTable: TSQLTable;
  LCols: IList<ColumnAttribute>;
begin
  LTable := CreateTestTable;
  LCommand := TCreateForeignKeyCommand.Create(LTable);
  try
    LCols := TEntityCache.Get(TCustomer).Columns;
    LCommand.SetCommandFieldsFromColumns(LCols);
    LCommand.ForeignKeys.Add(
      TSQLForeignKeyField.Create(
        'FKColumn', LTable, 'RefColumn', 'RefTable', [fsOnDeleteCascade, fsOnUpdateCascade]));

    LSQL := FSQLGenerator.GenerateCreateForeignKey(LCommand);
    CheckTrue(LSQL.Any);
  finally
    LTable.Free;
    LCommand.Free;
  end;
end;

procedure TAnsiSQLGeneratorTest.TestGenerateCreateSequence;
var
  ReturnValue: string;
begin
  ReturnValue := FSQLGenerator.GenerateCreateSequence(nil);
  CheckEqualsString('', ReturnValue);
end;

procedure TAnsiSQLGeneratorTest.TestGenerateGetNextSequenceValue;
var
  ReturnValue: string;
begin
  ReturnValue := FSQLGenerator.GenerateGetNextSequenceValue(nil);
  CheckEqualsString('', ReturnValue);
end;

const
  SQL_COUNT_TEST = 'SELECT * FROM TEST.CUSTOMERS WHERE CUSTID = 1;';
  SQL_COUNT =
    'SELECT COUNT(*) FROM (' + sLineBreak +
    'SELECT * FROM TEST.CUSTOMERS WHERE CUSTID = 1' + sLineBreak +
    ') AS ORM_GET_QUERY_COUNT;';

procedure TAnsiSQLGeneratorTest.TestGenerateGetQueryCount;
var
  LSQL: string;
begin
  LSQL := FSQLGenerator.GenerateGetQueryCount(SQL_COUNT_TEST);
  CheckEqualsString(SQL_COUNT, LSQL);
end;

procedure TAnsiSQLGeneratorTest.TestGenerateGetLastInsertId;
var
  ReturnValue: string;
begin
  ReturnValue := FSQLGenerator.GenerateGetLastInsertId(nil);
  CheckEquals('', ReturnValue);
end;

initialization
  RegisterTest('Spring.Persistence.Generators', TAnsiSQLGeneratorTest.Suite);

end.

