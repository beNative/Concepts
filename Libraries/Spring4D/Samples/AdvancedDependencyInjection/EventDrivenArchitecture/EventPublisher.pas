unit EventPublisher;

interface

uses
  Interfaces,
  Spring.Services;

type
  TEventPublisher = class(TInterfacedObject, IEventPublisher)
  private
    fLocator: IServiceLocator;
  public
    constructor Create(const locator: IServiceLocator);
    procedure Publish(const event: TObject; ownsObject: Boolean = True);
  end;

implementation

uses
  Rtti,
  SysUtils,
  Spring,
  Spring.Reflection;

{ TEventPublisher }

constructor TEventPublisher.Create(const locator: IServiceLocator);
begin
  fLocator := locator;
end;

procedure TEventPublisher.Publish(const event: TObject; ownsObject: Boolean = True);
const
  IEventSubscriberName = 'IEventSubscriber<*>';
var
  consumerTypeName: string;
  consumerType: TRttiType;
  consumers: TArray<TValue>;
  consumer: TValue;
begin
  consumerTypeName := StringReplace(IEventSubscriberName, '*', GetQualifiedClassName(event), []);
  consumerType := TType.FindType(consumerTypeName);
  consumers := fLocator.GetAllServices(consumerType.Handle);
  for consumer in consumers do
    IEventSubscriber(consumer.AsInterface).Handle(event);
  if ownsObject then
    event.Free;
end;

end.
