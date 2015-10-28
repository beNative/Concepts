unit Registrations;

interface

uses
  Spring.Container;

procedure RegisterTypes;

implementation

uses
  MainForm, Forms,
  CustomersViewModel, Customer,
  Spring.Persistence.Core.Interfaces,
  CustomerRepository;

procedure RegisterTypes;
var
  container: TContainer;
begin
  container := GlobalContainer;

  container.RegisterType<IPagedRepository<TCustomer,string>, TCustomerRepository>;

  container.RegisterType<TObject, TCustomersViewModel>('customersViewModel').AsSingleton;

  container.RegisterType<TCustomersView, TCustomersView>.DelegateTo(
    function: TCustomersView
    begin
      Application.CreateForm(TCustomersView, Result);
    end);

  container.Build;
end;

end.
