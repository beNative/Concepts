unit uRegistrations;

interface

uses
  Spring.Container;

procedure RegisterTypes(const container: TContainer);

implementation

uses
  Spring.Container.Core,
  Spring.Reflection,
  uOrderInterfaces,
  uOrderEntry,
  uOrderEntryDecorator,
  uOrderProcessor,
  uOrderValidator;

procedure RegisterTypes(const container: TContainer);
begin
  container.RegisterType<TOrderEntry>;
  container.RegisterType<TOrderValidator>;
  container.RegisterType<TOrderProcessor>;

  container.RegisterDecorator<IOrderEntry, TOrderEntryLoggingDecorator>;
  container.RegisterDecorator<IOrderValidator, TOrderValidatorLoggingDecorator>;

  container.RegisterDecorator<IOrderEntry, TOrderEntryTransactionDecorator>(
    function(const m: TComponentModel): Boolean
    begin
      Result := m.ComponentType.HasCustomAttribute<TransactionAttribute>;
    end
  );

  container.Build;
end;

end.
