unit Concepts.DevExpress.cxGridViewPresenter.Form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DB, ComCtrls, ActnList, StdCtrls,

  cxControls, cxGridLevel, cxClasses, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGrid, cxTL, cxInplaceContainer, cxTLData, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, cxEdit, cxTextEdit, cxTLdxBarBuiltInMenu,

  DSharp.Windows.CustomPresenter, DSharp.Windows.ColumnDefinitions,
  DSharp.Collections.ObservableCollection,
  DSharp.Linq.QueryProvider.SQL, DSharp.Bindings,
  DSharp.Windows.TreeViewPresenter, DSharp.DevExpress.GridViewPresenter,
  DSharp.DevExpress.TreeListPresenter,

  Concepts.Types.Contact, cxNavigator, System.Actions, VirtualTrees;

  {
    Now a presenter exists for every component. One could be created that
    supports several components using a registration mechanism.
  }

type
  TfrmcxGridViewPresenter = class(TForm)
    {$REGION 'designer controls'}
    pnlMain                : TPanel;
    pgcMain                : TPageControl;
    tsGridView             : TTabSheet;
    grdMain                : TcxGrid;
    tvwMain                : TcxGridTableView;
    grlMain                : TcxGridLevel;
    tsTreelist             : TTabSheet;
    lstMain                : TcxVirtualTreeList;
    sbrMain                : TStatusBar;
    aclMain                : TActionList;
    actFillList            : TAction;
    btnFillList            : TButton;
    tvpMain                : TTreeViewPresenter;
    tsVirtualTree          : TTabSheet;
    vstMain                : TVirtualStringTree;
    cxgrdclmnMainColumn1: TcxGridColumn;
    cxgrdclmnMainColumn2: TcxGridColumn;
    cxgrdclmnMainColumn3: TcxGridColumn;
    cxgrdclmnMainColumn4: TcxGridColumn;
    cxgrdclmnMainColumn5: TcxGridColumn;
    cxtrlstclmnMainColumn1: TcxTreeListColumn;
    cxtrlstclmnMainColumn2: TcxTreeListColumn;
    cxtrlstclmnMainColumn3: TcxTreeListColumn;
    cxtrlstclmnMainColumn4: TcxTreeListColumn;
    cxtrlstclmnMainColumn5: TcxTreeListColumn;
    cxtrlstclmnMainColumn6: TcxTreeListColumn;
    {$ENDREGION}
    procedure actFillListExecute(Sender: TObject);

  private
    FList: IList;

  protected
    procedure FillList;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  DSharp.Bindings.Notifications,

  Concepts.Types.RandomData;

{$REGION 'construction and destruction'}
procedure TfrmcxGridViewPresenter.AfterConstruction;
begin
  inherited;

  FList := TObservableCollection<TContact>.Create(True);

  { for the dx controls the columns have to be created. This is not required
    for the TVirtualStringTree. }
  gvpMain.UseColumnDefinitions := True;
  tlpMain.UseColumnDefinitions := True;
  tvpMain.UseColumnDefinitions := True;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmcxGridViewPresenter.actFillListExecute(Sender: TObject);
begin
  FillList;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmcxGridViewPresenter.FillList;
var
  I: Integer;
  C: TContact;
begin
  tlpMain.View.ItemsSource := nil;
  gvpMain.View.ItemsSource := nil;
  tvpMain.View.ItemsSource := nil;
  FList.Clear;
  for I := 0 to 1000 do
  begin
    C := TContact.Create;
    with C do
    begin
      Firstname   := RandomData.FirstName(gnMale);
      Lastname    := RandomData.LastName;
      CompanyName := RandomData.CompanyName;
      Email       := RandomData.Email(Firstname, Lastname);
      Address     := RandomData.Address;
    end;
    FList.Add(C);
  end;
  tlpMain.View.ItemsSource := FList;
  gvpMain.View.ItemsSource := FList;
  tvpMain.View.ItemsSource := FList;
  tvwMain.ApplyBestFit;
  lstMain.ApplyBestFit;
  vstMain.Header.AutoFitColumns(False);
end;
{$ENDREGION}

end.
