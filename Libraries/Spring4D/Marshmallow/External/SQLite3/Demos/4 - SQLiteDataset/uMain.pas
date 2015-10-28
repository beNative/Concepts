unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, DBClient, Grids, DBGrids, SQLiteTable3, MidasLib, ObjDS,
  SQLite3Dataset;

type
  TMyData = class
  private
    FID: Integer;
    FOtherID: Integer;
    FName: string;
    FNumber: Double;
    FEmpty: Integer;
  public
    [TFieldMap('ID', ftInteger, 0, false)]
    property ID: Integer read FID write FID;
    [TFieldMap('OtherID', ftInteger, 0, false)]
    property OtherID: Integer read FOtherID write FOtherID;
    [TFieldMap('Name', ftWideString, 50, false)]
    property Name: string read FName write FName;
    [TFieldMap('Number', ftFloat, 0, false)]
    property Number: Double read FNumber write FNumber;
    property Empty: Integer read FEmpty write FEmpty;
  end;

  TForm1 = class(TForm)
    dbg1: TDBGrid;
    ds1: TDataSource;
    btn1: TButton;
    btn2: TButton;
    cbFilter: TCheckBox;
    btn3: TButton;
    btn4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure cbFilterClick(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
  private
    { Private declarations }
    DB: TSQLiteDatabase;
    Dst: TSQLiteDataset;
    UpdSQL: TSQLiteUpdateSQL;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Generics.Collections;

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  stmt: TSQLitePreparedStatement;
  List: TObjectList<TMyData>;
  Map: TDataSetMapping;
begin
  //load data to dataset
  stmt := DB.GetPreparedStatement('select * from testtable limit 500');
  List :=  TObjectList<TMyData>.Create;
  Map := TDataSetMapping.Create(Dst);
  Dst.DisableControls;
  try
    stmt.ExecSQLAndMapData<TMyData>(List);

    Map.MappedType := List.ClassInfo;
    Map.Value := List;

    DSt.MergeChangeLog;
  finally
    DSt.EnableControls;
    stmt.Free;
    List.Free;
    Map.Free;
  end;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  Dst.ApplyUpdates();
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  Dst.Active := not dst.Active;
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
  Dst.RefreshRecord;
end;

procedure TForm1.cbFilterClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
  begin
    Dst.StatusFilter := [];
  end
  else
  begin
    Dst.StatusFilter := [usInserted, usDeleted, usModified, usUnmodified];
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DB := TSQLiteDatabase.Create('test.db');
  Dst := TSQLiteDataset.Create(Self);
  Dst.Database := DB;
  ds1.DataSet := Dst;

  UpdSQL := TSQLiteUpdateSQL.Create(Self);
  UpdSQL.DeleteSQL.Text := 'delete from testtable where ID = :ID';
  UpdSQL.ModifySQL.Text := 'update testtable set OtherID = :OtherID, name = :name, number = :number '+
    'where ID = :ID';
  UpdSQL.InsertSQL.Text := 'insert into testtable (OtherID, name, number) VALUES (:OtherId,:name,:number)';
  UpdSQL.RefreshSQL.Text := 'select * from testtable where ID = :ID';
  Dst.UpdateSQL := UpdSQL;

  Dst.AutoIncFieldName := 'ID';
  Dst.CommandText := 'select * from testtable limit 500';
 // Dst.CommandText := 'select ID, OtherID, name, number from testtable limit 500';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DB.Free;
  Dst.Free;
  UpdSQL.Free;
end;


end.
