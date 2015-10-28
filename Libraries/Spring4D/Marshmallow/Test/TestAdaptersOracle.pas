unit TestAdaptersOracle;

interface

uses
  TestFramework,
  TestEntities,
  SysUtils,
  Spring.Persistence.Adapters.Oracle,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.Session,
  Spring.Persistence.SQL.Generators.Oracle;

type
  TOracleConnectionAdapterTest = class(TTestCase)
  private
    FConnectionAdapter: TOracleConnectionAdapter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetQueryLanguage;
  end;

  TOracleSessionTest = class(TTestCase)
  private
    FConnection: IDBConnection;
    FManager: TSession;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure First;
    procedure Save_Delete;
    procedure Page;
  end;

  TOracleSQLGeneratorTest = class(TTestCase)
  private
    fSut: TOracleSQLGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure UseTOracleDBParams;
    procedure CreateParamCreatesTOracleDBParam;
    procedure WhenDataTypeNVARCHAR_ReplaceToNVARCHAR2;
    procedure GenerateCorrectCreateSequence;
  end;

implementation

uses
  ADODB,
  DB,
  TypInfo,
  Variants,
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.DatabaseManager,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Criteria.Interfaces,
  Spring.Persistence.Criteria.Properties,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Reflection;

const
  TBL_COMPANY = 'Vikarina.IMONES';

procedure CreateTestTables(AConnection: IDBConnection);
var
  LDBManager: TDatabaseManager;
begin
  LDBManager := TDatabaseManager.Create(AConnection);
  try
    LDBManager.ClearEntities;
    LDBManager.RegisterEntity(TCompany);
    LDBManager.BuildDatabase;
  finally
    LDBManager.Free;
  end;
end;

function CreateTestConnection: TADOConnection;
begin
  Result := TADOConnection.Create(nil);
  Result.LoginPrompt := False;
  //Provider=OraOLEDB.Oracle;Data Source=SERVER1;User ID=SYSTEM;Password=master
  Result.ConnectionString := Format('Provider=OraOLEDB.Oracle;Data Source=%0:S;Password=%1:S;User ID=%2:S'
    , ['SERVER1', 'master', 'SYSTEM']);
  Result.Open;
end;

procedure DropTestTables;
var
  LConn: TADOConnection;
begin
  LConn := CreateTestConnection;
  try
    LConn.Execute('DROP TABLE ' + TBL_COMPANY);
  finally
    LConn.Free;
  end;
end;

procedure InsertCompany(AID: Integer; const ACompName: string);
var
  LConn: TADOConnection;
begin
  LConn := CreateTestConnection;
  try
    LConn.Execute(Format('INSERT INTO '+ TBL_COMPANY + ' (IMONE, IMPAV) VALUES (%0:D, %1:S)',
      [AID, QuotedStr(ACompName)]));
  finally
    LConn.Free;
  end;
end;

function GetTableCount(const ATablename: string): Variant;
var
  LConn: TADOConnection;
  LResults: _Recordset;
begin
  LConn := CreateTestConnection;
  try
    LResults := LConn.Execute(Format('SELECT COUNT(*) FROM %0:S', [ATablename]));

    if LResults.RecordCount > 0 then
      Result := LResults.Fields.Item[0].Value
    else
      Result := Unassigned;
  finally
    LConn.Free;
  end;
end;


{$REGION 'TOracleConnectionAdapterTest'}

procedure TOracleConnectionAdapterTest.TestGetQueryLanguage;
begin
  CheckEquals(qlOracle, FConnectionAdapter.QueryLanguage);
end;

procedure TOracleConnectionAdapterTest.SetUp;
begin
  inherited;
  FConnectionAdapter := TOracleConnectionAdapter.Create(CreateTestConnection);
  FConnectionAdapter.AutoFreeConnection := True;
end;

procedure TOracleConnectionAdapterTest.TearDown;
begin
  FConnectionAdapter.Free;
  inherited;
end;

{$ENDREGION}


{$REGION 'TOracleSessionTest'}

procedure TOracleSessionTest.First;
var
  LCompany: TCompany;
begin
  //insert company
  InsertCompany(1, 'Oracle Company');
  LCompany := FManager.FindOne<TCompany>(1);
  try
    CheckTrue(Assigned(LCompany));
    CheckEquals(1, LCompany.ID);
    CheckEquals('Oracle Company', LCompany.Name);
  finally
    LCompany.Free;
  end;
end;

procedure TOracleSessionTest.Page;
var
  LCriteria: ICriteria<TCompany>;
  LItems: IList<TCompany>;
  Imone: IProperty;
  i: Integer;
begin
  for i := 1 to 20 do
  begin
    InsertCompany(i, Format('%D Company', [i]));
  end;
  Imone := TProperty<TCompany>.Create('IMONE');

  LCriteria := FManager.CreateCriteria<TCompany>;
  LCriteria.Add(Imone.GEq(1));
  LItems := LCriteria.Page(1, 10).Items;
  CheckEquals(10, LItems.Count);

  LItems := LCriteria.Page(2, 10).Items;
  CheckEquals(10, LItems.Count);
end;

procedure TOracleSessionTest.Save_Delete;
var
  LCompany: TCompany;
begin
  LCompany := TCompany.Create;
  try
    InsertCompany(1, 'Oracle Company');
    LCompany.Name := 'Inserted Company';
    LCompany.ID := 2;
    LCompany.Logo.LoadFromFile(PictureFilename);
    FManager.Save(LCompany);
    CheckEquals(2, GetTableCount(TBL_COMPANY));

    FManager.Delete(LCompany);
    CheckEquals(1, GetTableCount(TBL_COMPANY));
  finally
    LCompany.Free;
  end;
end;

procedure TOracleSessionTest.SetUp;
begin
  inherited;
  FConnection := TConnectionFactory.GetInstance(dtOracle, CreateTestConnection);
  FConnection.AutoFreeConnection := True;
  FManager := TSession.Create(FConnection);

  FConnection.AddExecutionListener(
    procedure(const command: string; const params: IEnumerable<TDBParam>)
    var
      i: Integer;
      param: TDBParam;
    begin
      Status(command);
      i := 0;
      for param in params do
      begin
        Status(Format('%2:D Param %0:S = %1:S', [param.Name, VarToStrDef(param.ToVariant, 'NULL'), i]));
        Inc(i);
      end;
      Status('-----');
    end);

  CreateTestTables(FConnection);
end;

procedure TOracleSessionTest.TearDown;
begin
  DropTestTables;
  FManager.Free;
  FConnection := nil;
  inherited;
end;

{$ENDREGION}


{$REGION 'TOracleSQLGeneratorTest'}

procedure TOracleSQLGeneratorTest.CreateParamCreatesTOracleDBParam;
var
  field: TSQLInsertField;
  table: TSQLTable;
  param: TDBParam;
begin
  table := TSQLTable.CreateFromClass(TCustomer);
  field := TSQLInsertField.Create('MiddleName', table,
    TType.GetType<TCustomer>.GetProperty('MiddleName').GetCustomAttribute<ColumnAttribute>,
    ':MiddleName');
  param := fSut.CreateParam(field, TValue.Empty);
  CheckIs(param, TOracleDBParam);
  CheckEquals(Ord(ftWideString), Ord(param.ParamType),
    'ParamType should be ftWidestring but was: ' + GetEnumName(System.TypeInfo(TFieldType), Ord(param.ParamType)));
  table.Free;
  field.Free;
  param.Free;
end;

procedure TOracleSQLGeneratorTest.GenerateCorrectCreateSequence;
var
  command: TCreateSequenceCommand;
  actual, expected: string;
begin
  //issue #84
  command := TCreateSequenceCommand.Create(TEntityCache.Get(TUIBCompany).Sequence);

  actual := fSut.GenerateCreateSequence(command);
  expected := 'BEGIN  EXECUTE IMMEDIATE ''CREATE SEQUENCE "GNR_IMONESID"  '+
    'MINVALUE 1 MAXVALUE 9999999999999999999999999999 INCREMENT BY 1 START WITH 1 CACHE 20 NOORDER NOCYCLE''; END;';
  CheckEquals(expected, actual);
  command.Free;
end;

procedure TOracleSQLGeneratorTest.SetUp;
begin
  inherited;
  fSut := TOracleSQLGenerator.Create;
end;

procedure TOracleSQLGeneratorTest.TearDown;
begin
  inherited;
  fSut.Free;
end;

procedure TOracleSQLGeneratorTest.UseTOracleDBParams;
begin
  CheckEquals(TOracleDBParam, fSut.GetParamClass);
end;

procedure TOracleSQLGeneratorTest.WhenDataTypeNVARCHAR_ReplaceToNVARCHAR2;
var
  field: TSQLCreateField;
  table: TSQLTable;
  actual, expected: string;
begin
  table := TSQLTable.CreateFromClass(TCustomer);
  field := TSQLCreateField.Create('MiddleName', table);
  field.SetFromAttribute(TType.GetType<TCustomer>.GetProperty(
    'MiddleName').GetCustomAttribute<ColumnAttribute>);

  actual := fSut.GetSQLDataTypeName(field);
  expected := 'NVARCHAR2(50)';
  CheckEquals(expected, actual);
  table.Free;
  field.Free;
end;

{$ENDREGION}


initialization
  RegisterTest(TOracleSQLGeneratorTest.Suite);
  if FileExists('D:\Oracle\oraociei11.dll') then
  begin
    RegisterTest(TOracleConnectionAdapterTest.Suite);
    RegisterTest(TOracleSessionTest.Suite);
  end;

end.
