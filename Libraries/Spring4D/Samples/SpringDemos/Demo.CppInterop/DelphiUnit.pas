unit DelphiUnit;

interface

uses
  Spring.Container.Common;

function InitApp : IContainer;
procedure RunApp;

implementation

uses
  Spring.Container,
  Fmx.Forms,
  Services,
  MainFrm;

var Container : TContainer;

type
  TDelphiImpl2WithCppDep = class(TInterfacedObject, IService2)
  private
    [Inject]
    FDep: IService1;
  public
    procedure Bar;
  end;

function InitApp : IContainer;
begin
  Assert(Container = nil);
  Container := TContainer.Create;
  Result := Container;
end;

procedure RunApp;
begin
  Container.RegisterType<TDelphiImpl2WithCppDep>;
  Container.Build;
  Container.Resolve<IService1>.Foo;
  Container.Resolve<IService2>.Bar;
  Container.Resolve<IService1>('cppdep').Foo;
  if (not IsConsole) then
  begin
    Application.CreateForm(TMainForm, MainForm);
    Application.Initialize;
    Application.Run;
  end;
end;

{ TService2WithCppDep }

procedure TDelphiImpl2WithCppDep.Bar;
begin
  if (IsConsole) then
    Writeln('TDelphiImpl2WithCppDep.Bar');
  FDep.Foo;
end;

initialization
finalization
  Container.Free;

end.

