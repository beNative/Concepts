unit CustomersViewModel;

interface

uses
  Customer,
  DSharp.Core.PropertyChangedBase,
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces;

type
  TCustomersViewModel = class(TPropertyChangedBase)
  private
    fCustomers: IObjectList;
    fCustomerId: string;
    fCustomerRepository: Lazy<IPagedRepository<TCustomer,string>>;
  public
    constructor Create(const customerRespository: Lazy<IPagedRepository<TCustomer,string>>);

    procedure LoadCustomers(Sender: TObject);
    property Customers: IObjectList read fCustomers;
    property CustomerId: string read fCustomerId write fCustomerId;
  end;

implementation

{ TCustomersViewModel }

constructor TCustomersViewModel.Create(
  const customerRespository: Lazy<IPagedRepository<TCustomer, string>>);
begin
  inherited Create;
  fCustomerRepository := customerRespository;
end;

procedure TCustomersViewModel.LoadCustomers(Sender: TObject);
begin
  // load customers
  if fCustomerId = '' then
    fCustomers := fCustomerRepository.Value.FindAll as IObjectList
  else
  begin
    fCustomers := TCollections.CreateList<TCustomer>(True) as IObjectList;
    fCustomers.Add(fCustomerRepository.Value.FindOne(fCustomerId));
  end;

  NotifyOfPropertyChange('Customers');
end;

end.
