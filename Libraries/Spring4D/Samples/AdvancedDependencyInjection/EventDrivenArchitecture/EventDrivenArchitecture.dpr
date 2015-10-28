program EventDrivenArchitecture;

{$APPTYPE CONSOLE}

uses
  Diagnostics,
  SysUtils,
  Spring.Container,
  Events in 'Events.pas',
  EventHandlers in 'EventHandlers.pas',
  EventPublisher in 'EventPublisher.pas',
  Interfaces in 'Interfaces.pas',
  OrderController in 'OrderController.pas';

procedure Main;
var
  container: TContainer;
  orderController: IOrderController;
begin
  container := GlobalContainer;
  container.RegisterInstance<TContainer>(container);
  container.RegisterType<TServiceLocatorAdapter>.AsSingleton;
  container.RegisterType<TOrderController>;
  container.RegisterType<TEventPublisher>.AsSingleton;
  container.RegisterType<TEmailOrderConfirmation>;
  container.Build;

  orderController := container.Resolve<IOrderController>;
  orderController.SubmitOrder;
end;

begin
  try
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Writeln('Press <Enter> to continue');
  Readln;
  ReportMemoryLeaksOnShutdown := True;
end.
