unit OrderController;

interface

uses
  Interfaces;

type
  TOrderController = class(TInterfacedObject, IOrderController)
  private
    fEventPublisher: IEventPublisher;
  public
    constructor Create(const eventPublisher: IEventPublisher);
    procedure SubmitOrder;
  end;

implementation

uses
  Events;

{ TOrderController }

constructor TOrderController.Create(const eventPublisher: IEventPublisher);
begin
  fEventPublisher := eventPublisher;
end;

procedure TOrderController.SubmitOrder;
begin
  fEventPublisher.Publish(TOrderSubmittedEvent.Create);
end;

end.
