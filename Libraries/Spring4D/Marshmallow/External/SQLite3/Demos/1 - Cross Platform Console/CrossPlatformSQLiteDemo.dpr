program CrossPlatformSQLiteDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SQLite3 in '..\..\Source\SQLite3.pas',
  SQLiteTable3 in '..\..\Source\SQLiteTable3.pas',
  SysUtils,
  Classes;

procedure LoadData1();
var
  slDBpath: string;
  sldb: TSQLiteDatabase;
  sSQL: String;
  ts: TStringStream;
  iVal: Double;
  i: Integer;
  sltb: TSQLiteTable;
begin
  WriteLn('Load Data 1');
  slDBpath := IncludeTrailingPathDelimiter(GetHomePath) + IncludeTrailingPathDelimiter('Test') + 'test.db';
  if not ForceDirectories(ExtractFileDir(slDBpath)) then
  begin
    WriteLn('Cannot create path: ' + slDBpath);
  end;

  sltb := nil;
  sldb := TSQLiteDatabase.Create(slDBpath);
  try

    if sldb.TableExists('testTable') then
    begin
      sSQL := 'DROP TABLE testtable';
      sldb.ExecSQL(sSQL);
    end;

    sSQL := 'CREATE TABLE testtable ([ID] INTEGER PRIMARY KEY,[OtherID] INTEGER NULL,';
    sSQL := sSQL +
      '[Name] VARCHAR (255),[Number] FLOAT, [notes] BLOB, [picture] BLOB COLLATE NOCASE);';

    sldb.ExecSQL(sSQL);
    WriteLn('Table created');
    sldb.ExecSQL('CREATE INDEX TestTableName ON [testtable]([Name]);');
    WriteLn('Index created');
    // begin a transaction
  //  sldb.BeginTransaction;
    WriteLn('Insert some data...');
    sSQL := 'INSERT INTO testtable(Name,OtherID,Number) VALUES ("Some Name",4,587.6594);';
    // do the insert
    sldb.ExecSQL(sSQL);

    sSQL := 'INSERT INTO testtable(Name,OtherID,Number,Notes) VALUES ("Another Name",12,4758.3265,"More notes");';
    // do the insert
    sldb.ExecSQL(sSQL);

    // add the notes using a parameter
    ts := TStringStream.Create('Here are some notes with a unicode smiley: ' +
      char($263A), TEncoding.UTF8);
    try

      // insert the text into the db
      sldb.UpdateBlob('UPDATE testtable set notes = ? WHERE OtherID = 4', ts);
      WriteLn('Inserted text into blob');
    finally
      ts.Free;
    end;

    // query the data
    sltb := sldb.GetTable('SELECT * FROM testtable');

    Writeln('Table Record Count: ');
    WriteLn(sltb.Count);

  finally
    sldb.Free;
    if sltb <> nil then
      sltb.Free;
  end;
end;


begin
  ReportMemoryLeaksOnShutdown := True;
  try
    LoadData1;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
