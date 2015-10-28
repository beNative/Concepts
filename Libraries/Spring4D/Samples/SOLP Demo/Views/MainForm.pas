unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, VirtualTrees,
  DSharp.Windows.ColumnDefinitions, Vcl.ComCtrls, Vcl.Grids,
  DSharp.Bindings.VCLControls, DSharp.Bindings, DSharp.Windows.CustomPresenter,
  DSharp.Windows.TreeViewPresenter, Spring.Container.Common;

type
  TCustomersView = class(TForm)
    VirtualStringTree1: TVirtualStringTree;
    Panel1: TPanel;
    Button1: TButton;
    Edit1: TEdit;
    CustomersPresenter: TTreeViewPresenter;
    BindingGroup1: TBindingGroup;
  private
    fDataContext: TObject;
    procedure SetDataContext(const Value: TObject);
    function GetDataContext: TObject;
    { Private declarations }
  public
    { Public declarations }
    [Inject('customersViewModel')]
    property DataContext: TObject read GetDataContext write SetDataContext;
  end;

implementation

{$R *.dfm}

function TCustomersView.GetDataContext: TObject;
begin
  Result := fDataContext;
end;

procedure TCustomersView.SetDataContext(const Value: TObject);
begin
  fDataContext := Value;
  BindingGroup1.AddBinding(fDataContext, 'Customers',
    CustomersPresenter, 'View.ItemsSource');
  BindingGroup1.NotifyPropertyChanged(Self, 'DataContext');
end;

end.
