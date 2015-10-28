unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMainForm = class(TForm)
    btnContainer: TButton;
    btnMock: TButton;
    btnMockRegistration: TButton;
    procedure btnContainerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnMockClick(Sender: TObject);
    procedure btnMockRegistrationClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  BasketController,
  Interfaces,
  Spring.Collections,
  Spring.Container,
  Spring.Container.AutoMockExtension,
  Spring.Mocking;

procedure TMainForm.btnContainerClick(Sender: TObject);
var
  container: TContainer;
  sut: TBasketController;
  m: IMock<ICommandChannel>;
begin
  container := TContainer.Create;
  try
    container.AddExtension<TAutoMockExtension>;
    container.RegisterType<TBasketController>;
    container.Build;

    sut := container.Resolve<TBasketController>;
    try
      sut.Post(nil);
      m := container.Resolve<IMock<ICommandChannel>>;
      m.Received(1).Send(nil);

      container.Resolve<IMock<INumberParserFactory>>.Setup.Raises(EMockException, 'hello mock!').When.Create('f');
      container.Resolve<INumberParserFactory>.Create('g');
      container.Resolve<INumberParserFactory>.Create('f'); // will raise EMockException
    finally
      sut.Free;
    end;
  finally
    container.Free;
  end;
end;

procedure TMainForm.btnMockClick(Sender: TObject);
var
  commandChannel: Mock<ICommandChannel>;
  parser: INumberParser;
  i: Integer;

  factory: INumberParserFactory;
  numbers: IEnumerable<Integer>;
begin
  // example one
  commandChannel := Mock<ICommandChannel>.Create(TMockBehavior.Strict);
  commandChannel.Setup.Executes(
    function(const i: TCallInfo): TValue
    begin
      ShowMessage(i.Method.ToString);
      Result := 42;
    end).When.Send(nil);
//  commandChannel.Setup.Returns(42);//.WithAnyArgs.WhenCalling.Send(nil);
//  commandChannel.Setup.Returns(43).When.Send2(Self);
//  commandChannel.Setup.Raises(Exception, 'test failed').When.Send2(Self);
  commandChannel.Setup.Returns([42, 43, 44]).When.Send2(Self);

  Assert(commandChannel.Instance.Send(nil) = 42); // message will show
  Assert(commandChannel.Instance.Send2(Self) = 42);
  Assert(commandChannel.Instance.Send2(Self) = 43);
  Assert(commandChannel.Instance.Send2(Self) = 44);
  try
    i := commandChannel.Instance.Send2(nil); // raises unexpected call!
  except
    on e: Exception do
      MessageDlg(e.Message, mtError, [mbOK], 0);
  end;

  commandChannel.Received(3).Send2(Self);
  commandChannel.ReceivedWithAnyArgs(3).Send2(nil);

  // example two
  numbers := TCollections.CreateList<Integer>([1, 2, 3]);
  factory := Mock<INumberParserFactory>.Create;
  Mock.From(factory.Create(','))
    .Setup.Returns(numbers).When.Parse('an expression');
  parser := factory.Create(',');
  Assert(factory.Create(',').Parse('an expression').EqualsTo([1, 2, 3]));

  Mock.From(parser).ReceivedWithAnyArgs(1).Parse('');
end;

procedure TMainForm.btnMockRegistrationClick(Sender: TObject);
var
  container: TContainer;
  sut: TBasketController;
begin
  container := TContainer.Create;
  try
    container.RegisterType<ICommandChannel>.DelegateTo(
      function: ICommandChannel
      begin
        Result := Mock<ICommandChannel>.Create;
      end);
    container.RegisterType<TBasketController>.AsSingleton;
    container.Build;
    sut := container.Resolve<TBasketController>;
    sut.Post(nil);
  finally
    container.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

end.
