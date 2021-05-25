unit Main;

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLIntf, LCLType, LMessages, LResources, SQLdb, odbcconn,
{$ELSE}
  Windows, Messages, Mask, ADODB, DBClient, Provider,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, DBCtrls, KGrids, KDBGrids, ActnList,
  ExtCtrls, KFunctions, KGraphics, KControls, KIcon,
  KDialogs;

type

  { TMainForm }

  TMainForm = class(TForm)
    ACPrint: TAction;
    BUPrint: TButton;
    EDConnectionString: TEdit;
    PSDMain: TKPrintSetupDialog;
    Label1: TLabel;
    Label2: TLabel;
    EDTable: TEdit;
    EDFirstCol: TDBEdit;
    BUOpen: TButton;
    BUClose: TButton;
    ACOpen: TAction;
    ACClose: TAction;
    DBNav: TDBNavigator;
    DSMain: TDataSource;
    Label3: TLabel;
    BUAutoSize: TButton;
    DBGrid: TKDBGrid;
    ALMain: TActionList;
    BUAppend: TButton;
    ACAppend: TAction;
    procedure DBGridDrawCell(Sender: TObject; ACol, ARow: Integer; R: TRect;
      State: TKGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure DBGridCustomSortRows(Sender: TObject; ByIndex: Integer;
      SortMode: TKGridSortMode; var Sorted: Boolean);
    procedure DBGridEditorCreate(Sender: TObject; ACol, ARow: Integer;
      var AEditor: TWinControl);
    procedure ACOpenExecute(Sender: TObject);
    procedure ACOpenUpdate(Sender: TObject);
    procedure ACCloseExecute(Sender: TObject);
    procedure ACCloseUpdate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ACPrintExecute(Sender: TObject);
    procedure ACPrintUpdate(Sender: TObject);
    procedure BUAutoSizeClick(Sender: TObject);
    procedure ACAppendExecute(Sender: TObject);
    procedure ACAppendUpdate(Sender: TObject);
    procedure DBNavBeforeAction(Sender: TObject; Button: TNavigateBtn);
  private
    { Private declarations }
    procedure DoTableOpen;
    procedure DoTableClose;
    function IsTableOpen: Boolean;
  public
  {$IFDEF FPC}
    CN: TODBCConnection;
    Table: TSQLQuery;
    Trans: TSQLTransaction;
    Query: string;
  {$ELSE}
    CN: TADOConnection;
    Table: TADOTable;
  {$ENDIF}
  {$IFDEF FPC}
    procedure MyAfterPost(DataSet: TDataSet);
  {$ENDIF}
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
{$IFDEF FPC}
  CN := TODBCConnection.Create(Self);
  CN.LoginPrompt := False;
  Trans := TSQLTransaction.Create(Self);
  Trans.DataBase := CN;
  Table := TSQLQuery.Create(Self);
  Table.DataBase := CN;
  Table.Active := False;
  Table.ParseSQL := True;
  Table.UsePrimaryKeyAsKey := False;
  Table.Transaction := Trans;
  Table.UpdateMode := upWhereChanged;
  Table.AfterPost := MyAfterPost;
{$ELSE}
  CN := TADOConnection.Create(Self);
  CN.LoginPrompt := False;
  Table := TADOTable.Create(Self);
  Table.Connection := CN;
  Table.Active := False;
{$ENDIF}
  DSMain.DataSet := Table;
  DBGrid.DoubleBuffered := True; // TKGrid is pretty flicker free but this is still better
//  TKDBGridCol(DBGrid.Columns.Items[0]).FieldName := 'Id';
//  (DBGrid.Columns.Items[0] as TKDBGridCol).FieldName := 'Id';
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
  // no way for kgrid to get notified...
  DBGrid.HideCellHint;
end;

procedure TMainForm.ACCloseExecute(Sender: TObject);
begin
  DoTableClose;
end;

procedure TMainForm.ACCloseUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := IsTableOpen;
end;

procedure TMainForm.ACAppendExecute(Sender: TObject);
begin
  DBGrid.AppendRow;
  // this inserts before last row:
//  DBGrid.DataSource.DataSet.Last;
//  DBGrid.InsertRow(DBGrid.DataSource.DataSet.RecordCount + DBgrid.FixedRows - 1);
end;

procedure TMainForm.ACAppendUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := IsTableOpen;
end;

{$IFDEF FPC}
procedure TMainForm.MyAfterPost(DataSet: TDataSet);
begin
  try
    Table.ApplyUpdates; // must call this to apply the post in Lazarus
    Trans.Commit;
  except
    on E: Exception do
      MessageBox(Handle, PChar('Problem with updating: ' + E.Message), PChar(Caption), MB_OK);
  end;
end;
{$ENDIF}

procedure TMainForm.ACOpenExecute(Sender: TObject);
begin
  try
  {$IFDEF TK_TEST}
  // this is here just to test the grid with my local db :-)
    if EDTable.Text = '' then
      EDTable.Text := 'images';
    if EDConnectionString.Text = '' then
      EDConnectionString.Text := 'Localhost-MySQL'; //'DRIVER={MySQL ODBC 5.3 ANSI Driver}; SERVER=localhost; PORT=3306; DATABASE=local; UID=root; PASSWORD=root;OPTION=3;';
  {$ENDIF}
  {$IFDEF FPC}
    if CN.DatabaseName <> EDConnectionString.Text then
    begin
      CN.Connected := False;
      CN.DatabaseName := EDConnectionString.Text;
      CN.Connected := True;
    end;
    Query := 'SELECT * FROM ' + EDTable.Text;
    Table.SQL.Clear;
    Table.SQL.Add(Query);
  {$ELSE}
    if CN.ConnectionString <> EDConnectionString.Text then
    begin
      CN.Connected := False;
      CN.ConnectionString := EDConnectionString.Text;
      CN.Connected := True;
    end;
    Table.TableName := EDTable.Text;
  {$ENDIF}
    DoTableOpen;
    DBGrid.PageSetup.Title := 'Table: ' + EDTable.Text;
    EDFirstCol.DataField := Table.FieldDefs[0].Name;
  except
    MessageBox(Handle, 'Cannot connect or find the table!', PChar(Caption), MB_OK);
  end;
end;

procedure TMainForm.ACOpenUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not IsTableOpen;
end;

procedure TMainForm.ACPrintExecute(Sender: TObject);
begin
  DBGrid.LoadAllRecords;
  PSDMain.Execute;
end;

procedure TMainForm.ACPrintUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := DBGrid.CanPrint and IsTableOpen;
end;

procedure TMainForm.BUAutoSizeClick(Sender: TObject);
begin
  DBGrid.AutoSizeRow(DBGrid.Row);
end;

procedure TMainForm.DBGridCustomSortRows(Sender: TObject; ByIndex: Integer;
  SortMode: TKGridSortMode; var Sorted: Boolean);
var
  ColumnName, Sort: WideString;
begin
  // adapt as you wish or better what your db supports
  if TKDBGridCol(TKCustomGrid(Sender).Cols[ByIndex]).DataType in
    [ftString, ftWideString, ftInteger, ftSmallInt, ftWord, ftLargeInt, ftFloat,
     ftDate, ftTime, ftDateTime, ftBCD, ftFmtBCD, ftCurrency, ftBoolean] then
  begin
    ColumnName := TKDBGridCol(TKCustomGrid(Sender).Cols[ByIndex]).FieldName; // get column name
    case SortMode of
      smNone: Sort := '';
      smDown: Sort := ColumnName + ' ASC';
      smUp: Sort := ColumnName + ' DESC';
    end;
  {$IFDEF FPC}
    if Sort <> '' then Sort := ' ORDER BY ' + Sort;
    Table.SQL.Clear;
    Table.SQL.Add(Query + Sort);
    DoTableOpen;
  {$ELSE}
    Table.Sort := Sort;
  {$ENDIF}
    Sorted := True;
  end else
    Sorted := False;
end;

procedure TMainForm.DBGridEditorCreate(Sender: TObject; ACol, ARow: Integer;
  var AEditor: TWinControl);
begin
  // you have still full control about the inplace editor but
  // here we just use the default handling
  // (if you delete this event the same is used in TKDBGridCell)
  DBGrid.DefaultEditorCreate(ACol, ARow, AEditor);
  // use default handling for other inplace editor events
end;

procedure TMainForm.DBNavBeforeAction(Sender: TObject; Button: TNavigateBtn);
begin
  DBGrid.EditorMode := False;
end;

procedure TMainForm.DoTableClose;
begin
  Table.Close;
end;

procedure TMainForm.DoTableOpen;
begin
  Table.Close;
  Table.Open;
end;

function TMainForm.IsTableOpen: Boolean;
begin
  Result := Table.Active;
end;

procedure TMainForm.DBGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  R: TRect; State: TKGridDrawState);
begin
  with TKCustomGrid(Sender) do
  begin
    Cell[ACol, ARow].ApplyDrawProperties;
    CellPainter.GraphicHPadding := 3;
    if ARow = 3 then
      CellPainter.Canvas.Font.Color := clBlue;
    CellPainter.DefaultDraw;
  end;
end;

end.
