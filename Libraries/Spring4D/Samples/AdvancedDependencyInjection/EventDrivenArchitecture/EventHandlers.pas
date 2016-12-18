unit EventHandlers;

interface

uses
  Interfaces,
  Events;

type
  TEmailOrderConfirmation = class(TInterfacedObject, IEventSubscriber<TOrderSubmittedEvent>)
  public
    procedure Handle(const event: TOrderSubmittedEvent);
  end;

implementation

{ TEmailOrderConfirmation }

procedure TEmailOrderConfirmation.Handle(
  const event: TOrderSubmittedEvent);
begin
  Writeln('send email');
end;

end.
