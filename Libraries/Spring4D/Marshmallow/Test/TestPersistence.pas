unit TestPersistence;

interface

uses
  TestFramework, Spring.Persistence.Core.Interfaces, TestEntities;

type
  TPersistenceTest = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInterface_Rtti;
  end;

implementation

uses
  SysUtils
  ,Rtti
  ,TypInfo
  ;

{ PersistenceTests }

procedure TPersistenceTest.SetUp;
begin
  inherited;
end;

procedure TPersistenceTest.TearDown;
begin
  inherited;
end;

type
  ITest = interface(IInvokable)
    ['{5EB34AA7-6C35-4030-AC63-4AF9306BE59C}']
    function GetCount: Integer;
    procedure SetCount(AValue: Integer);
    property Count: Integer read GetCount write SetCount;
  end;

  TTestas = class(TInterfacedObject, ITest)
  private
    FCount: Integer;
  public
    function GetCount: Integer;
    procedure SetCount(AValue: Integer);
    property Count: Integer read GetCount write SetCount;
  end;

  TBean = class
  private
    FTest: ITest;
  protected

  public
    property Test: ITest read FTest write FTest;
  end;

procedure TPersistenceTest.TestInterface_Rtti;
var
  LBeanType: TRttiType;
  LBean: TBean;
  LProp: TRttiProperty;
  LValue: TValue;
  LCountMethod: TRttiMethod;
  Lintf: IInterface;
  LInterfaceType: TRttiInterfaceType;
begin
  LBean := TBean.Create;
  try
    LBean.Test := TTestas.Create;
    LBean.Test.Count := 3;
    LBeanType := TRttiContext.Create.GetType(LBean.ClassType);
    LProp := LBeanType.GetProperty('Test');

    LValue := LProp.GetValue(LBean);
    LCountMethod := LProp.PropertyType.GetMethod('GetCount');
    CheckTrue(Assigned(LCountMethod));

    if LProp.PropertyType is TRttiInterfaceType then
    begin
      LInterfaceType := TRttiInterfaceType(LProp.PropertyType);
      if Supports(LValue.AsInterface, LInterfaceType.GUID, Lintf) then
      begin
        LCountMethod := LInterfaceType.GetMethod('SetCount');
        CheckTrue(Assigned(LCountMethod));
        TValue.Make(@Lintf, LInterfaceType.Handle, LValue);
        LCountMethod.Invoke(LValue, [10]);
        LProp.SetValue(LBean, LValue);
        CheckEquals(10, LBean.Test.Count);
      end;
    end;
  finally
    LBean.Free;
  end;
end;

{ TTestas }

function TTestas.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TTestas.SetCount(AValue: Integer);
begin
  FCount := AValue;
end;

initialization
  RegisterTest('Spring.Persistence.Core', TPersistenceTest.Suite);

end.
