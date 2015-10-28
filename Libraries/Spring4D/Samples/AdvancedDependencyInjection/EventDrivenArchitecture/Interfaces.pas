unit Interfaces;

interface

type
  IEventSubscriber<T: class> = interface
    ['{F703AB27-3F4E-4628-B01D-E6B7B8E38901}']
    procedure Handle(const event: T);
  end;

  IEventSubscriber = IEventSubscriber<TObject>;

  IEventPublisher = interface
    ['{DBD075E3-38BA-4CED-BCDF-5ADD80C929BD}']
    procedure Publish(const event: TObject; ownsObject: Boolean = True);
  end;

  IOrderController = interface
    ['{0483CD99-2EC4-4437-85AD-C533F3C788A3}']
    procedure SubmitOrder;
  end;

implementation

end.
