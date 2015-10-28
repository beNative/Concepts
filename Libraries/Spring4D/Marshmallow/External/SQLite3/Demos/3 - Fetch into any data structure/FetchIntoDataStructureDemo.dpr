// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program FetchIntoDataStructureDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Generics.Collections,
  SQLite3 in '..\..\Source\SQLite3.pas',
  SQLiteTable3 in '..\..\Source\SQLiteTable3.pas';

type
  //class must have constructor without arguments
  TMyData = class
  private
    FID: Integer;
    FOtherID: Integer;
    FName: string;
    FNumber: Double;
    FEmpty: Integer;

  public
    // can load properties values from db which are public or published
    property ID: Integer read FID write FID;
    property OtherID: Integer read FOtherID write FOtherID;
    property Name: string read FName write FName;
    property Number: Double read FNumber write FNumber;
    property Empty: Integer read FEmpty write FEmpty;
  end;


procedure Demo;
var
  dst: TSQLiteUniTable;
  db: TSQLiteDatabase;
  pstm: TSQLitePreparedStatement;
  ID: Int64;
  slDBpath: string;
  i: Integer;
  val: Variant;
  iCount: Cardinal;
  bNull: Boolean;
  AList: TObjectList<TMyData>;
  dat: TMyData;
begin
  //use db file from first demo
  //you must execute first demo for this file to be created!
  iCount := 0;
  slDBpath := IncludeTrailingPathDelimiter(GetHomePath) + IncludeTrailingPathDelimiter('Test') + 'test.db';
  db := TSQLiteDatabase.Create(slDBpath);
  try

    pstm := TSQLitePreparedStatement.Create(db, 'select * from testtable');
    //get unidirectional query for very fast loading
    dst := pstm.ExecQuery;

    if Assigned(dst) then
    begin
      while not dst.EOF do
      begin
        for i := 0 to dst.FieldCount - 1 do
        begin
          val := dst.Fields[i].Value;
          bNull := dst.Fields[i].IsNull;
        end;
        dst.Next;
        Inc(iCount);
      end;

    end;

    WriteLn(Format('Records: %D',[iCount]));

    AList := TObjectList<TMyData>.Create;
    try
      if pstm.ExecSQLAndMapData<TMyData>(AList) then
      begin
        for dat in AList do
        begin
          i := dat.FOtherID;
        end;
      end;

      WriteLn(Format('Records: %D',[AList.Count]));

    finally
      AList.Free;
    end;


  finally
    dst.Free;
    pstm.Free;
    db.Free;
  end;
end;

begin
  try
    Demo;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
