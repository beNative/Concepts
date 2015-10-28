unit TestConnectionFactory;

interface

uses
  TestFramework, Spring.Persistence.Core.Interfaces, Generics.Collections, SysUtils,
  Spring.Persistence.Core.ConnectionFactory;

type
  TConnectionFactoryTest = class(TTestCase)
  public
{$IFDEF MSWINDOWS}
    procedure When_ADOConn_And_Json_GetInstance;
{$ENDIF}
  published
    procedure TestGetInstance;
    procedure TestGetInstance1;
    procedure TestGetInstanceFromFilename;
  end;

implementation

uses
  SQLiteTable3,
  Spring.Persistence.Adapters.SQLite,
  Spring.Persistence.SQL.Interfaces,
  TestEntities;

procedure TConnectionFactoryTest.TestGetInstance;
var
  ReturnValue: IDBConnection;
  AConcreteConnection: TObject;
begin
  AConcreteConnection := TSQLiteDatabase.Create(':memory:');
  try
    ReturnValue := nil;
    CheckFalse(Assigned(ReturnValue));
    ReturnValue := TConnectionFactory.GetInstance(dtSQLite, AConcreteConnection);
    CheckTrue(Assigned(ReturnValue));
  finally
    AConcreteConnection.Free;
  end;
end;

const
  JSON_SQLITE = '{"SQLiteTable3.TSQLiteDatabase": { "Filename": ":memory:" } }';

procedure TConnectionFactoryTest.TestGetInstance1;
var
  ReturnValue: IDBConnection;
begin
  ReturnValue := TConnectionFactory.GetInstance(dtSQLite, JSON_SQLITE);
  CheckTrue(Assigned(ReturnValue));
  CheckEquals(qlSQLite, ReturnValue.QueryLanguage);
  CheckTrue(ReturnValue.IsConnected);
end;

const
  FILE_JSON = 'Conn_Sqlite.json';

procedure TConnectionFactoryTest.TestGetInstanceFromFilename;
var
  ReturnValue: IDBConnection;
  sDir: string;
begin
  sDir := IncludeTrailingPathDelimiter(ExtractFileDir(PictureFilename));
  ReturnValue := TConnectionFactory.GetInstanceFromFile(dtSQLite, sDir + FILE_JSON);
  CheckTrue(Assigned(ReturnValue));
  CheckEquals(qlSQLite, ReturnValue.QueryLanguage);
  CheckTrue(ReturnValue.IsConnected);
end;

{$IFDEF MSWINDOWS}

procedure TConnectionFactoryTest.When_ADOConn_And_Json_GetInstance;
const
  JSON: string = '{'+
    '"Data.Win.ADODB.TADOConnection": {'+
    '    "LoginPrompt": "False", '+
    '    "ConnectionString": ""'+
    '}'+
'}';
begin
  CheckNotNull(TConnectionFactory.GetInstance(dtADO, JSON));
end;

{$ENDIF}

initialization
  // Register any test cases with the test runner
  RegisterTest(TConnectionFactoryTest.Suite);
end.

