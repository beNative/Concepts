unit TestConnectionFactory;

{$I Spring.inc}

interface

uses
  TestFramework;

type
  TConnectionFactoryTest = class(TTestCase)
  public
{$IFDEF MSWINDOWS}
    procedure TestGetInstanceADO;
{$ENDIF}
  published
    procedure TestGetInstance;
    procedure TestGetInstance1;
    procedure TestGetInstanceFromFilename;
  end;

implementation

uses
  SysUtils,
  SQLiteTable3,
  Spring.Persistence.Adapters.SQLite,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Interfaces,
  TestEntities;

procedure TConnectionFactoryTest.TestGetInstance;
var
  connection: IDBConnection;
  externalConnection: TObject;
begin
  externalConnection := TSQLiteDatabase.Create(':memory:');
  connection := TConnectionFactory.GetInstance(dtSQLite, externalConnection);
  connection.AutoFreeConnection := True;
  CheckNotNull(connection);
end;

procedure TConnectionFactoryTest.TestGetInstance1;
const
  JSON_SQLITE = '{"SQLiteTable3.TSQLiteDatabase": { "Filename": ":memory:" } }';
var
  connection: IDBConnection;
begin
  connection := TConnectionFactory.GetInstance(dtSQLite, JSON_SQLITE);
  CheckNotNull(connection);
  CheckEquals(qlSQLite, connection.QueryLanguage);
  CheckTrue(connection.IsConnected);
end;

procedure TConnectionFactoryTest.TestGetInstanceFromFilename;
const
  FILE_JSON = 'Conn_Sqlite.json';
var
  connection: IDBConnection;
  directory: string;
begin
  directory := IncludeTrailingPathDelimiter(ExtractFileDir(PictureFilename));
  connection := TConnectionFactory.GetInstanceFromFile(dtSQLite, directory + FILE_JSON);
  CheckNotNull(connection);
  CheckEquals(qlSQLite, connection.QueryLanguage);
  CheckTrue(connection.IsConnected);
end;

{$IFDEF MSWINDOWS}
procedure TConnectionFactoryTest.TestGetInstanceADO;
const
{$IFDEF HAS_UNITSCOPE}
  JSON = '{'+
    '  "Data.Win.ADODB.TADOConnection": {' +
    '    "LoginPrompt": "False", ' +
    '    "ConnectionString": ""' +
    '  }' +
    '}';
{$ELSE}
  JSON = '{'+
    '  "ADODB.TADOConnection": {' +
    '    "LoginPrompt": "False", ' +
    '    "ConnectionString": ""' +
    '  }' +
    '}';
{$ENDIF}
begin
  CheckNotNull(TConnectionFactory.GetInstance(dtADO, JSON));
end;
{$ENDIF}

initialization
  RegisterTest('Spring.Persistence.Core', TConnectionFactoryTest.Suite);

end.
