unit TestSQLiteVirtualTable3;

{$I ..\sv.inc}
interface

uses
  TestFramework, SQLite3, SQLiteTable3, SQLiteVirtualTable3;

type
  TestTSQLiteModule = class(TTestCase)
  published
    procedure TestCreateModule;
    procedure TestCreateTable;
  end;

  TestTSQLiteVirtualTable = class(TTestCase)
  private
    FDB: TSQLiteDatabase;
    FModule: TSQLiteModule;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInsert;
    procedure TestDeleteAll;
    procedure TestUpdateAll;
    procedure TestSelect;
    procedure TestRename;
    procedure TestOverloadedFunction;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils
  {$ELSE}
  Classes, SysUtils
  {$ENDIF}
  ;

type
  TSQLiteTestModule = class(TSQLiteModule)
  private
    FTest: TTestCase;
    FTestInsertParameters: Boolean;
    FForSelect: Boolean;
    FForFunctionOverride: Boolean;
  protected
    function GetColumnsDeclaration(Arguments: TStrings; out Declaration: string): Boolean; override;
    function GetNewTable(const Name: string; Arguments: TStrings; APVTab: Psqlite3_vtab): TSQLiteVirtualTable; override;
  public
    constructor Create(ATest: TTestCase; ADatabase: TSQLiteDatabase);

    property TestInsertParameters: Boolean read FTestInsertParameters write FTestInsertParameters;
    property ForSelect: Boolean read FForSelect write FForSelect;
    property ForFunctionOverride: Boolean read FForFunctionOverride write FForFunctionOverride;
  end;

  TSQLiteTestVirtualTable = class(TSQLiteVirtualTable)
  private
    FRows: TArray<Int64>;
    FDoConnectCalled: Boolean;
    FDoDisconnectCalled: Boolean;
    FLikeOverloadCalled: Boolean;
    FGlobOverloadCalled: Boolean;

    function Test: TTestCase;
    procedure LikeOverload(pContext: Psqlite3_context; Values: TPsqlite3_valueList);
    procedure GlobOverload(pContext: Psqlite3_context; Values: TPsqlite3_valueList);
  protected
    function DoDelete(RowId: Int64): Boolean; override;
    function DoInsert(RowId: Int64; Values: TPsqlite3_valueList): Boolean; override;
    function DoUpdate(ExistingRowId: Int64; NewRowId: Int64; Values: TPsqlite3_valueList): Boolean; override;
    function DoGetBestIndex(Constraints: TSQLiteVirtualTableConstraintList; OrderBys: TSQLiteVirtualTableOrderByList;
                            out IndexId: Integer; out IndexParameters: Pointer;
                            out OutputInOrder: Boolean; out EstimatedCost: Double;
                            ConstraintsUsage: TSQLiteVirtualTableConstraintUsageList): Boolean; override;
    function DoRename(const NewName: string): Boolean; override;
    function DoConnect: Boolean; override;
    function DoDisconnect: Boolean; override;
    function DoGetFunctionOverload(const FunctionName: string; FunctionArgumentCount: Integer): TSQLiteVirtualTableFunctionOverload; override;

    function GetNewCursor: TSQLiteVirtualTableCursor; override;
  end;

  TSQLiteTestVirtualTableCursor = class(TSQLiteVirtualTableCursor)
  private
    FRowId: Int64;
    FRows: TArray<Int64>;
  protected
    function DoInitialize(IndexId: Integer; IndexParameters: Pointer; Values: TPsqlite3_valueList): Boolean; override;
    function DoNext: Boolean; override;
    function GetRowId(out RowId: Int64): Boolean; override;
    function GetEof: Boolean; override;
    function GetColumnValue(SQLiteContext: Psqlite3_context; ColumnNumber: Integer): Boolean; override;
  public
    constructor Create(ATable: TSQLiteVirtualTable);
  end;

{ TestTSQLiteModule }

procedure TestTSQLiteModule.TestCreateModule;
var
  DB: TSQLiteDatabase;
begin
  // must not crash
  DB := TSQLiteDatabase.Create(':memory:');
  try
    TSQLiteTestModule.Create(Self, DB).Free;
    Check(True);
  finally
    DB.Free;
  end;
end;

procedure TestTSQLiteModule.TestCreateTable;
var
  DB: TSQLiteDatabase;
  Module: TSQLiteTestModule;
begin
  // must not crash
  DB := TSQLiteDatabase.Create(':memory:');
  try
    Module := TSQLiteTestModule.Create(Self, DB);
    try
      TSQLiteTable.Create(DB, 'CREATE VIRTUAL TABLE SomeTable USING ' + Module.Name + '(a INTEGER);').Free;
      Check(True);
    finally
      Module.Free;
    end;
  finally
    DB.Free;
  end;
end;

{ TestTSQLiteVirtualTable }

procedure TestTSQLiteVirtualTable.TestSelect;
var
  Table: TSQLiteTable;
begin
  TSQLiteTestModule(FModule).TestInsertParameters := False;
  TSQLiteTestModule(FModule).ForSelect := True;

  TSQLiteTable.Create(FDB, 'INSERT INTO SomeTable VALUES (50);').Free;
  TSQLiteTable.Create(FDB, 'INSERT INTO SomeTable VALUES (60);').Free;
  TSQLiteTable.Create(FDB, 'INSERT INTO SomeTable VALUES (70);').Free;
  Table := TSQLiteTable.Create(FDB, 'SELECT * FROM SomeTable WHERE a < 65;');
  try
    CheckEquals(2, Table.RowCount, 'Bad row count');
    CheckEquals(50, Table.FieldAsInteger(0), 'Bad value for row 0');
    Check(Table.Next, 'Going to next should not fail');
    CheckEquals(60, Table.FieldAsInteger(0), 'Bad value for row 1');
  finally
    Table.Free;
  end;
end;

procedure TestTSQLiteVirtualTable.SetUp;
begin
  inherited SetUp;

  FDB := TSQLiteDatabase.Create(':memory:');
  FModule := TSQLiteTestModule.Create(Self, FDB);

  TSQLiteTable.Create(FDB, 'CREATE VIRTUAL TABLE SomeTable USING ' + FModule.Name + '(a INTEGER);').Free;
end;

procedure TestTSQLiteVirtualTable.TearDown;
begin
  FModule.Free;
  FDB.Free;

  inherited Destroy;
end;

procedure TestTSQLiteVirtualTable.TestDeleteAll;
begin
  TSQLiteTable.Create(FDB, 'INSERT INTO SomeTable VALUES (50);').Free;
  TSQLiteTable.Create(FDB, 'DELETE FROM SomeTable;').Free;
end;

procedure TestTSQLiteVirtualTable.TestInsert;
begin
  TSQLiteTable.Create(FDB, 'INSERT INTO SomeTable VALUES (50);').Free;
end;

procedure TestTSQLiteVirtualTable.TestOverloadedFunction;
var
  Table: TSQLiteTestVirtualTable;
begin
  TSQLiteTestModule(FModule).TestInsertParameters := False;
  TSQLiteTestModule(FModule).ForFunctionOverride := True;

  TSQLiteTable.Create(FDB, 'INSERT INTO SomeTable VALUES (50);').Free;
  TSQLiteTable.Create(FDB, 'SELECT a FROM SomeTable WHERE a like ''a'' AND a GLOB ''5'';').Free;

  Table := TSQLiteTestVirtualTable(FModule.VirtualTables[0]);

  Check(Table.FLikeOverloadCalled, 'Like overload should have been called');
  Check(Table.FGlobOverloadCalled, 'Glob overload should have been called');
end;

procedure TestTSQLiteVirtualTable.TestRename;
var
  Table: TSQLiteTestVirtualTable;
begin
  TSQLiteTable.Create(FDB, 'ALTER TABLE SomeTable RENAME TO NewName;').Free;
  TSQLiteTable.Create(FDB, 'UPDATE NewName SET a = 40;').Free;

  Table := TSQLiteTestVirtualTable(FModule.VirtualTables[0]);

  Check(Table.FDoDisconnectCalled, 'DoDisconnect should have been called');
  Check(Table.FDoConnectCalled, 'DoConnect should have been called');
end;

procedure TestTSQLiteVirtualTable.TestUpdateAll;
begin
  TSQLiteTable.Create(FDB, 'INSERT INTO SomeTable VALUES (50);').Free;
  TSQLiteTable.Create(FDB, 'UPDATE SomeTable SET a = 40;').Free;
end;

{ TSQLiteTestModule }

constructor TSQLiteTestModule.Create(ATest: TTestCase; ADatabase: TSQLiteDatabase);
begin
  inherited Create(ADatabase, 'test');

  FTest := ATest;
  FTestInsertParameters := True;
end;

function TSQLiteTestModule.GetColumnsDeclaration(Arguments: TStrings; out Declaration: string): Boolean;
begin
  Arguments.QuoteChar := ' ';
  Arguments.Delimiter := ',';
  Declaration := Arguments.DelimitedText;
  Result := True;
end;

function TSQLiteTestModule.GetNewTable(const Name: string; Arguments: TStrings; APVTab: Psqlite3_vtab): TSQLiteVirtualTable;
begin
  Result := TSQLiteTestVirtualTable.Create(Self, Name, APVTab);
end;

{ TSQLiteTestVirtualTable }

function TSQLiteTestVirtualTable.DoConnect: Boolean;
begin
  FDoConnectCalled := True;
  Result := True;
end;

function TSQLiteTestVirtualTable.DoDelete(RowId: Int64): Boolean;
begin
  Test.CheckEquals(-1, RowId, 'Bad RowId in Delete');
  Result := True;
end;

function TSQLiteTestVirtualTable.DoDisconnect: Boolean;
begin
  FDoDisconnectCalled := True;
  Result := True;
end;

function TSQLiteTestVirtualTable.DoGetBestIndex(
  Constraints: TSQLiteVirtualTableConstraintList;
  OrderBys: TSQLiteVirtualTableOrderByList; out IndexId: Integer;
  out IndexParameters: Pointer; out OutputInOrder: Boolean;
  out EstimatedCost: Double;
  ConstraintsUsage: TSQLiteVirtualTableConstraintUsageList): Boolean;
begin
  if TSQLiteTestModule(Module).ForFunctionOverride then
    IndexId := 2
  else if TSQLiteTestModule(Module).ForSelect then
    IndexId := 1
  else
    IndexId := 0;
  IndexParameters := nil;
  OutputInOrder := False;
  EstimatedCost := 1;
  Result := True;
end;

function TSQLiteTestVirtualTable.DoGetFunctionOverload(
  const FunctionName: string;
  FunctionArgumentCount: Integer): TSQLiteVirtualTableFunctionOverload;
begin
  if SameText(FunctionName, 'like') then
    Result := TSQLiteVirtualTableFunctionOverload.Create(LikeOverload)
  else if SameText(FunctionName, 'glob') then
    Result := TSQLiteVirtualTableFunctionOverload.Create(GlobOverload)
  else
    Result := nil;
end;

function TSQLiteTestVirtualTable.DoInsert(RowId: Int64;
  Values: TPsqlite3_valueList): Boolean;
begin
  Test.CheckEquals(1, Values.Count, 'Bad value count');
  if TSQLiteTestModule(Module).TestInsertParameters then
    Test.CheckEquals(50, sqlite3_value_int64(Values[0]), 'The value is not the right one');

  SetLength(FRows, Length(FRows) + 1);
  FRows[Length(FRows) - 1] := sqlite3_value_int64(Values[0]);

  Result := True;
end;

function TSQLiteTestVirtualTable.DoRename(const NewName: string): Boolean;
begin
  Test.CheckEquals('NewName', NewName);
  Result := True;
end;

function TSQLiteTestVirtualTable.DoUpdate(ExistingRowId, NewRowId: Int64;
  Values: TPsqlite3_valueList): Boolean;
begin
  Test.CheckEquals(-1, ExistingRowId, 'Bad ExistingRowId in Update');
  Test.CheckEquals(-1, NewRowId, 'Bad NewRowId in Update');
  Test.CheckEquals(40, sqlite3_value_int64(Values[0]), 'The new value is not the right one');
  Result := True;
end;

function TSQLiteTestVirtualTable.GetNewCursor: TSQLiteVirtualTableCursor;
begin
  Result := TSQLiteTestVirtualTableCursor.Create(Self);
end;

procedure TSQLiteTestVirtualTable.GlobOverload(pContext: Psqlite3_context;
  Values: TPsqlite3_valueList);
begin
  FGlobOverloadCalled := True;
  sqlite3_result_int(pContext, 1);
end;

procedure TSQLiteTestVirtualTable.LikeOverload(pContext: Psqlite3_context;
  Values: TPsqlite3_valueList);
begin
  FLikeOverloadCalled := True;
  sqlite3_result_int(pContext, 1);
end;

function TSQLiteTestVirtualTable.Test: TTestCase;
begin
  Result := TSQLiteTestModule(Module).FTest;
end;

{ TSQLiteTestVirtualTableCursor }

constructor TSQLiteTestVirtualTableCursor.Create(ATable: TSQLiteVirtualTable);
begin
  inherited Create(ATable);

  FRowId := -1;
end;

function TSQLiteTestVirtualTableCursor.DoInitialize(IndexId: Integer;
  IndexParameters: Pointer; Values: TPsqlite3_valueList): Boolean;
var
  RowCount: Integer;
  I: Integer;
  Value: Int64;
begin
  if IndexId = 0 then
  begin
    SetLength(FRows, 0);
  end
  else if IndexId = 1 then
  begin
    RowCount := 0;
    Value := sqlite3_value_int64(Values[0]);
    SetLength(FRows, Length(TSQLiteTestVirtualTable(Table).FRows));
    for I := 0 to Length(FRows) - 1 do
      if TSQLiteTestVirtualTable(Table).FRows[I] < Value then
      begin
        FRows[RowCount] := TSQLiteTestVirtualTable(Table).FRows[I];
        Inc(RowCount);
      end;
    SetLength(FRows, RowCount);
  end
  else if IndexId = 2 then
  begin
    FRows := TSQLiteTestVirtualTable(Table).FRows;
  end;

  if Length(FRows) > 0 then
    FRowId := 0
  else
    FRowId := -1;

  Result := True;
end;

function TSQLiteTestVirtualTableCursor.DoNext: Boolean;
begin
  Inc(FRowId);
  Result := True;
end;

function TSQLiteTestVirtualTableCursor.GetColumnValue(
  SQLiteContext: Psqlite3_context; ColumnNumber: Integer): Boolean;
begin
  sqlite3_result_int64(SQLiteContext, FRows[FRowId]);
  Result := True;
end;

function TSQLiteTestVirtualTableCursor.GetEof: Boolean;
begin
  Result := (FRowId < 0) or (FRowId >= Length(FRows));
end;

function TSQLiteTestVirtualTableCursor.GetRowId(out RowId: Int64): Boolean;
begin
  RowId := FRowId;
  Result := True;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTSQLiteModule.Suite);
  RegisterTest(TestTSQLiteVirtualTable.Suite);

end.
