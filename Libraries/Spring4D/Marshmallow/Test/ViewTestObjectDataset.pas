unit ViewTestObjectDataset;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DBCtrls, Grids, DBGrids, DB, StdCtrls, ComCtrls
  ,Spring.Persistence.ObjectDataset
  ;

type
  TfrmObjectDatasetTest = class(TForm)
    dsList: TDataSource;
    dbgList: TDBGrid;
    DBNavigator1: TDBNavigator;
    edFilter: TEdit;
    cbFiltered: TCheckBox;
    sbTotal: TStatusBar;
    dbgClone: TDBGrid;
    Splitter1: TSplitter;
    dsClone: TDataSource;
    procedure edFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure dbgListTitleClick(Column: TColumn);
    procedure FormCreate(Sender: TObject);
    procedure cbFilteredClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FIndex: Integer;
    FDataset: TObjectDataset;
  protected
    procedure DoAfterScroll(ADataset: TDataSet);
  public
    { Public declarations }

    property Dataset: TObjectDataset read FDataset write FDataset;
  end;

var
  frmObjectDatasetTest: TfrmObjectDatasetTest;

implementation


{$R *.dfm}

procedure TfrmObjectDatasetTest.cbFilteredClick(Sender: TObject);
begin
  (dsList.DataSet as TObjectDataset).Filtered := TCheckBox(Sender).Checked;
end;

procedure TfrmObjectDatasetTest.dbgListTitleClick(Column: TColumn);
var
  sDir: string;
begin
  if FIndex mod 2 = 0 then
    sDir := ' ASC'
  else
    sDir := ' DESC';

  (dsList.DataSet as TObjectDataset).Sort := Column.FieldName + sDir;
  Inc(FIndex);
end;

procedure TfrmObjectDatasetTest.DoAfterScroll(ADataset: TDataSet);
begin
  if ADataset.Active and not (ADataset.Eof or ADataset.Bof) then
  begin
    sbTotal.SimpleText := Format('%d from %D records. Filter: %D', [ADataset.RecNo, ADataset.RecordCount, FDataset.FilterCount]);
  end;
end;

procedure TfrmObjectDatasetTest.edFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    dsList.DataSet.Filter := edFilter.Text;
  end;
end;

procedure TfrmObjectDatasetTest.FormCreate(Sender: TObject);
begin
  FIndex := 0;
end;

procedure TfrmObjectDatasetTest.FormShow(Sender: TObject);
begin
  Dataset.AfterScroll := DoAfterScroll;
end;

end.
