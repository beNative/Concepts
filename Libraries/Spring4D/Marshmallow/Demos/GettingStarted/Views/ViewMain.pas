unit ViewMain;

interface

uses
  ActnList,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  Forms,
  Graphics,
  Menus,
  Messages,
  SysUtils,
  Variants,
  Windows,
  Spring.Collections,
  SQLiteTable3,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.Session,
  ProductModel,
  System.Actions;


type
  TMainForm = class(TForm)
    CommitAction: TAction;
    CommitChangesMenuItem: TMenuItem;
    DatabaseMenuItem: TMenuItem;
    MainActionList: TActionList;
    N1MenuItem: TMenuItem;
    N2MenuItem: TMenuItem;
    N3MenuItem: TMenuItem;
    ProductAddAction: TAction;
    ProductAddMenuItem: TMenuItem;
    ProductEditAction: TAction;
    ProductEditMenuItem: TMenuItem;
    ProductRemoveAction: TAction;
    ProductRemoveMenuItem: TMenuItem;
    ProductsListView: TListView;
    ProductsMenuItem: TMenuItem;
    ProductsRefreshAction: TAction;
    ProductsRefreshMenuItem: TMenuItem;
    RebuildDatabaseAction: TAction;
    RebuildDatabaseMenuItem: TMenuItem;
    ViewMainMenu: TMainMenu;
    ViewStatusBar: TStatusBar;
    procedure CommitActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ProductAddActionExecute(Sender: TObject);
    procedure ProductEditActionExecute(Sender: TObject);
    procedure ProductEditActionUpdate(Sender: TObject);
    procedure ProductRemoveActionExecute(Sender: TObject);
    procedure ProductsListViewData(Sender: TObject; Item: TListItem);
    procedure ProductsListViewDblClick(Sender: TObject);
    procedure ProductsRefreshActionExecute(Sender: TObject);
    procedure RebuildDatabaseActionExecute(Sender: TObject);
  private
    FConnection: IDBConnection;
    FDatabase: TSQLiteDatabase;
    FProducts: IList<TProduct>;
    FSession: TSession;
  protected
    procedure DoAddNewProduct;
    procedure DoBuildDatabase;
    procedure DoCheckEntities;
    procedure DoCommitChanges;
    procedure DoEditProduct;
    procedure DoReLoadProducts;
    procedure DoRemoveProduct;
    procedure DoRepaint;
  public
    property Connection: IDBConnection read FConnection;
    property Session: TSession read FSession;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Spring.Persistence.Core.DatabaseManager,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Adapters.SQLite,
  ViewEditProduct;

{$R *.dfm}

procedure TMainForm.ProductAddActionExecute(Sender: TObject);
begin
  DoAddNewProduct;
end;

procedure TMainForm.RebuildDatabaseActionExecute(Sender: TObject);
begin
  DoBuildDatabase;
end;

procedure TMainForm.CommitActionExecute(Sender: TObject);
begin
  DoCommitChanges;
end;

procedure TMainForm.ProductEditActionExecute(Sender: TObject);
begin
  DoEditProduct;
end;

procedure TMainForm.ProductEditActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(ProductsListView.Selected);
end;

procedure TMainForm.ProductsRefreshActionExecute(Sender: TObject);
begin
  DoReLoadProducts;
end;

procedure TMainForm.ProductRemoveActionExecute(Sender: TObject);
begin
  DoRemoveProduct;
end;

procedure TMainForm.DoAddNewProduct;
var
  LProduct: TProduct;
  LConfirmed: Boolean;
begin
  LConfirmed := False;
  LProduct := TProduct.Create;
  LProduct.CreationDate := Trunc(Now);
  LProduct.CreationDateTime := Now;
  LProduct.CreationTime := Frac(Now);
  try
    LConfirmed := TProductEditForm.Edit(LProduct);
    if LConfirmed then
    begin
      FProducts.Add(LProduct);
      DoRepaint;
    end;
  finally
    if not LConfirmed then
      LProduct.Free;
  end;
end;

procedure TMainForm.DoBuildDatabase;
var
  LDBManager: TDatabaseManager;
begin
  LDBManager := TDatabaseManager.Create(FConnection);
  try
    LDBManager.BuildDatabase;
  finally
    LDBManager.Free;
  end;
end;

procedure TMainForm.DoCheckEntities;
var
  LDBManager: TDatabaseManager;
begin
  LDBManager := TDatabaseManager.Create(FConnection);
  try
    if not LDBManager.EntityExists(TProduct) then
    begin
      LDBManager.BuildDatabase;
      DoReLoadProducts;
    end;
  finally
    LDBManager.Free;
  end;
end;

procedure TMainForm.DoCommitChanges;
var
  LTran: IDBTransaction;
begin
  LTran := FSession.BeginTransaction;
  FSession.SaveList<TProduct>(FProducts);
  LTran.Commit;
end;

procedure TMainForm.DoEditProduct;
var
  LProduct: TProduct;
begin
  LProduct := FProducts[ProductsListView.Selected.Index];
  if TProductEditForm.Edit(LProduct) then
  begin
    DoRepaint;
  end;
end;

procedure TMainForm.DoRepaint;
begin
  ProductsListView.Items.Count := FProducts.Count;
  ProductsListView.Invalidate;
  ViewStatusBar.Panels[0].Text := Format('Total: %d', [FProducts.Count]);
end;

procedure TMainForm.DoReLoadProducts;
begin
  ProductsListView.Items.BeginUpdate;
  try
    FProducts := FSession.FindAll<TProduct>;
    DoRepaint;
  finally
    ProductsListView.Items.EndUpdate;
  end;
end;

procedure TMainForm.DoRemoveProduct;
var
  LProduct: TProduct;
begin
  LProduct := FProducts[ProductsListView.Selected.Index];
  FSession.Delete(LProduct);
  FProducts.Remove(LProduct);
  DoRepaint;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDatabase := TSQLiteDatabase.Create(Self);
  FDatabase.Filename := 'products.db3';
  FConnection := TSQLiteConnectionAdapter.Create(FDatabase);

  FConnection.AutoFreeConnection := True;
  FConnection.Connect;
  FSession := TSession.Create(FConnection);
  FProducts := TCollections.CreateObjectList<TProduct>(True);

  DoCheckEntities;
  DoRepaint;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FProducts := nil;
  FSession.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if FConnection.IsConnected then
  begin
    DoReLoadProducts;
  end;
end;

procedure TMainForm.ProductsListViewData(Sender: TObject; Item: TListItem);
var
  LProduct: TProduct;
begin
  LProduct := FProducts[Item.Index];
  Item.Caption := LProduct.Name;
  Item.SubItems.Add(CurrToStr(LProduct.Price));
  Item.SubItems.Add(IntToStr(LProduct.Quantity));
end;

procedure TMainForm.ProductsListViewDblClick(Sender: TObject);
var
  LSelected: TListItem;
begin
  LSelected := ProductsListView.Selected;
  if Assigned(LSelected) then
  begin
    DoEditProduct;
  end;
end;

end.
