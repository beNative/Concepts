program ChainOfResponsibility;

{$APPTYPE CONSOLE}

uses
  Spring,
  Spring.Container;

type
  IHandler = interface
    ['{E25FC300-ABD5-47AD-9D76-D0D6593554E9}']
    procedure HandleRequest(request: Integer);
  end;

  THandler = class(TInterfacedObject, IHandler)
  private
    fSuccessor: IHandler;
  protected
    property Successor: IHandler read fSuccessor;
  public
    constructor Create(const successor: IHandler);
    procedure HandleRequest(request: Integer); virtual; abstract;
  end;

  TConcreteHandler1 = class(THandler)
  public
    procedure HandleRequest(request: Integer); override;
  end;

  TConcreteHandler2 = class(THandler)
  public
    procedure HandleRequest(request: Integer); override;
  end;

  TConcreteHandler3 = class(THandler)
  public
    procedure HandleRequest(request: Integer); override;
  end;

constructor THandler.Create(const successor: IHandler);
begin
  fSuccessor := successor;
end;

procedure TConcreteHandler1.HandleRequest(request: Integer);
begin
  if (request >= 0) and (request < 10) then
    Writeln(ClassName, ' handled request ',  request)
  else if Assigned(Successor) then
    Successor.HandleRequest(request);
end;

procedure TConcreteHandler2.HandleRequest(request: Integer);
begin
  if (request >= 10) and (request < 20) then
    Writeln(ClassName, ' handled request ',  request)
  else if Assigned(Successor) then
    Successor.HandleRequest(request);
end;

procedure TConcreteHandler3.HandleRequest(request: Integer);
begin
  if (request >= 20) and (request < 30) then
    Writeln(ClassName, ' handled request ',  request)
  else if Assigned(Successor) then
    Successor.HandleRequest(request);
end;

var
  requests: array of Integer = [2, 5, 14, 22, 18, 3, 27, 20];
  request: Integer;
  handler: IHandler;
begin
  GlobalContainer.RegisterType<IHandler,TConcreteHandler1>('handler1')
    .InjectConstructor(['handler2']).AsDefault;
  GlobalContainer.RegisterType<IHandler,TConcreteHandler2>('handler2')
    .InjectConstructor(['handler3']);
  GlobalContainer.RegisterType<IHandler,TConcreteHandler3>('handler3')
    .InjectConstructor([nil]);
  GlobalContainer.Build;

  handler := GlobalContainer.Resolve<IHandler>;

  for request in requests do
    handler.HandleRequest(request);
  Readln;
end.
