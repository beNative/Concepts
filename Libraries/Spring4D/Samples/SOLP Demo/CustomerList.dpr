program CustomerList;

uses
  Vcl.Forms,
  Spring.Container,
  MainForm in 'Views\MainForm.pas' {CustomersView},
  CustomersViewModel in 'ViewModels\CustomersViewModel.pas',
  Customer in 'Models\Customer.pas',
  CustomerRepository in 'Services\CustomerRepository.pas',
  Registrations in 'Registrations.pas';

{$R *.res}

begin
  RegisterTypes;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  GlobalContainer.Resolve<TCustomersView>;
  Application.Run;
  ReportMemoryLeaksOnShutdown := True;
end.
