{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit Spring.Tests.Container;

{$I Spring.inc}
{$I Spring.Tests.inc}

interface

uses
  Classes,
  SysUtils,
  TestFramework,
  Spring,
  Spring.Services,
  Spring.Container,
  Spring.Container.Common,
  // DO NOT CHANGE ORDER OF FOLLOWING UNITS !!!
  Spring.Tests.Container.Interfaces,
  Spring.Tests.Container.Components;

type
  TContainerTestCase = class abstract(TTestCase)
  protected
    fContainer: TContainer;
    procedure CheckIs(AInterface: IInterface; AClass: TClass; msg: string = ''); overload;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

{$IFDEF AUTOREFCOUNT}
  TTestGlobalContainer = class(TTestCase)
  published
    procedure TestGlobalContainerDirectUse;
    procedure TestGlobalContainerAssignToVariable;
    procedure TestServiceLocatorDirectUse;
    procedure TestServiceLocatorAssignToVariable;
  end;
{$ENDIF}

  TTestEmptyContainer = class(TContainerTestCase)
  published
    procedure TestResolveUnknownInterfaceService;
//    procedure TestResolveUnknownClassService;
    procedure TestRegisterNonGuidInterfaceService;
    procedure TestRegisterGenericInterfaceService;
    procedure TestRegisterUnassignableService;
    procedure TestRegisterTwoUnnamedServicesImplicit;
    procedure TestRegisterTwoUnnamedServicesExplicit;
    procedure TestRegisterTwoNamedDifferentServices;
    procedure TestResolveReturnsUnnamedService;
    procedure TestResolveAll;
    procedure TestResolveAllNonGeneric;
  end;

  TTestSimpleContainer = class(TContainerTestCase)
  published
    procedure TestIssue13;
    procedure TestInterfaceService;
    procedure TestAbstractClassService;
    procedure TestServiceSameAsComponent;
    procedure TestBootstrap;
    procedure TestSingleton;
    procedure TestTransient;
    procedure TestPerThread;
    procedure TestPooled;
    procedure TestPooledRefCountedInterface;
    procedure TestInitializable;
    procedure TestRecyclable;
    procedure TestDisposable;

    procedure TestIssue41_DifferentName;
    procedure TestIssue41_DifferentService;
    procedure TestIssue41_DifferentLifetimes;

    procedure TestIssue49;
    procedure TestIssue50;

    procedure TestResolveFuncWithTwoTypes;
    procedure TestResolveUnknownClasses;

    procedure TestResolveComponentDoesNotFallbackToTObject;
    procedure TestResolveComponentDoesCallOverriddenConstructor;

    procedure TestRecordConstructorNotConsidered;

{$IFDEF DELPHIXE_UP}
    procedure TestClassContainsAbstractMethods;
{$ENDIF}
  end;

  // Same Service, Different Implementations
  TTestDifferentServiceImplementations = class(TContainerTestCase)
  private
    fNameService: INameService;
    fAnotherNameService: INameService;
    fServices: TArray<INameService>;
    fServiceValues: TArray<TValue>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNameService;
    procedure TestAnotherNameService;
    procedure TestResolveAll;
    procedure TestResolveAllNonGeneric;
    procedure TestUnsatisfiedDependency;
  end;

  // Same Component, Different Services
  TTestImplementsDifferentServices = class(TContainerTestCase)
  private
    fNameService: INameService;
    fAgeService: IAgeService;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNameService;
    procedure TestAgeService;
  end;

  TTestActivatorDelegate = class(TContainerTestCase)
  private
    fPrimitive: IPrimitive;
    fExpectedInteger: Integer;
    fExpectedString: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNameService;
    procedure TestIntegerArgument;
    procedure TestStringArgument;
  end;

  TTypedInjectionTestCase = class abstract(TContainerTestCase)
  private
    fNameService: INameService;
    fInjectionExplorer: IInjectionExplorer;
  protected
    procedure DoRegisterComponents; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConstructorInjection;
    procedure TestMethodInjection;
    procedure TestPropertyInjection;
    procedure TestFieldInjection;
  end;

  TTestTypedInjectionByCoding = class(TTypedInjectionTestCase)
  protected
    procedure DoRegisterComponents; override;
  end;

  TTestTypedInjectionsByAttribute = class(TTypedInjectionTestCase)
  protected
    procedure DoRegisterComponents; override;
  end;

  TNamedInjectionsTestCase = class(TContainerTestCase)
  private
    fExplorer: IInjectionExplorer;
  protected
    procedure DoRegisterComponents; virtual;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConstructorInjection;
    procedure TestMethodInjection;
    procedure TestPropertyInjection;
    procedure TestFieldInjection;
  end;

  TTestNamedInjectionsByCoding = class(TNamedInjectionsTestCase)
  protected
    procedure DoRegisterComponents; override;
  end;

  TTestNamedInjectionsByAttribute = class(TNamedInjectionsTestCase)
  protected
    procedure DoRegisterComponents; override;
  end;

  TTestDirectCircularDependency = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestResolve;
  end;

  TTestCrossedCircularDependency = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestResolveChicken;
    procedure TestResolveEgg;
  end;

  TTestCircularReferenceLazySingleton = class(TContainerTestCase)
  published
    procedure TestResolveLazySingleton;
  end;

  TTestPerResolve = class(TContainerTestCase)
  published
    procedure TestResolveCircularDependency;
  end;

  TTestImplementsAttribute = class(TContainerTestCase)
  published
    procedure TestImplements;
  end;

  TTestRegisterInterfaces = class(TContainerTestCase)
  published
    procedure TestOneService;
    procedure TestTwoServices;
    procedure TestInheritedService;
    procedure TestTwoServicesWithSameName;
    procedure TestParentInterface;
  end;

  TTestDefaultResolve = class(TContainerTestCase)
  published
    procedure TestResolve;
    procedure TestResolveDependency;
    procedure TestRegisterDefault;
    procedure TestNoDefault;
  end;

  TTestInjectionByValue = class(TContainerTestCase)
  published
    procedure TestInjectField;
  end;

  TTestResolverOverride = class(TContainerTestCase)
  private
    fDummy: TObject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestResolve;
    procedure TestResolveWithClass;
    procedure TestResolveWithMultipleParams;
    procedure TestResolveWithDependency;
    procedure TestResolveWithDependencySubClass;
  end;

  TTestRegisterInterfaceTypes = class(TContainerTestCase)
  published
    procedure TestOneService;
    procedure TestTwoServices;
    procedure TestOneServiceAsSingleton;
    procedure TestOneServiceAsSingletonPerThread;
    procedure TestTwoRegistrations;
  end;

  TTestLazyDependencies = class(TContainerTestCase)
  private
    fCalled: Boolean;
  protected
    procedure SetUp; override;
    procedure PerformChecks; virtual;
  published
    procedure TestDependencyTypeIsFunc;
{$IFNDEF IOSSIMULATOR}
    procedure TestDependencyTypeIsRecord;
    procedure TestDependencyTypeIsInterface;
{$ELSE}
    // These tests fail due to some compiler/RTL problem, the circular
    // dependency exception is raised but during re-raise in some finally block
    // AV is raised instead. This is rare corner case not considered to be a
    // major issue.
{$ENDIF}
    procedure TestDependencyTypeIsSingletonAlreadyInstantiated;
    procedure TestDependencyTypeIsSingletonNotYetInstantiated;
    procedure TestResolveLazyOfSingletonCreatedLater;
  end;

  TTestLazyDependenciesDetectRecursion = class(TTestLazyDependencies)
  protected
    procedure PerformChecks; override;
  end;

  TTestManyDependencies = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestInjectArray;
    procedure TestInjectEnumerable;
    procedure TestInjectArrayOfLazy;
    procedure TestNoRecursion;
    procedure TestNoRecursion_Issue18;
    procedure TestNoRecursion_TwoDifferentModelsWithSameComponentType;
    procedure TestRecursion_TwoDifferentModelsWithSameComponentType;
    procedure TestResolveArrayOfLazy;
  end;

  TTestDecorators = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure RegisterOneDecoratorResolveReturnsDecorator;
    procedure RegisterOneDecoratorWithFailingConditionResolvesWithoutDecorator;
    procedure RegisterTwoDecoratorsResolveReturnsLastDecorator;
    procedure RegisterTwoDecoratorsResolveWithParameterReturnsCorrectValue;
    procedure RegisterOneDecoratorInitializeIsCalled;
    procedure RegisterTwoDecoratorsResolveLazyReturnsLastDecorator;
    procedure RegisterTwoDecoratorsResolveArrayAllDecorated;
  end;

implementation

uses
  Spring.Collections,
  Spring.Container.Core,
  Spring.Container.Resolvers,
  Spring.TestUtils,
{$IFDEF DELPHIXE_UP}
  Spring.Mocking,
{$ENDIF}
  Spring.Logging;

type
  TObjectAccess = class(TObject);


{$REGION 'TContainerTestCase'}

procedure TContainerTestCase.CheckIs(AInterface: IInterface; AClass: TClass; msg: string);
begin
  CheckIs(AInterface as TObject, AClass, msg);
end;

procedure TContainerTestCase.SetUp;
begin
  inherited;
  fContainer := TContainer.Create;
end;

procedure TContainerTestCase.TearDown;
begin
  fContainer.Free;
  inherited;
end;

{$ENDREGION}


{$REGION 'TTestGlobalContainer'}
{$IFDEF AUTOREFCOUNT}

procedure TTestGlobalContainer.TestGlobalContainerAssignToVariable;
var AContainer: TContainer;
begin
  AContainer := GlobalContainer;
  CheckEquals(2, TObjectAccess(AContainer).FRefCount);
end;

procedure TTestGlobalContainer.TestGlobalContainerDirectUse;
begin
  CheckEquals(1, TObjectAccess(GlobalContainer).FRefCount);
end;

procedure TTestGlobalContainer.TestServiceLocatorAssignToVariable;
var ALocator: TServiceLocator;
begin
  ALocator := ServiceLocator;
  CheckEquals(2, TObjectAccess(ALocator).FRefCount);
end;

procedure TTestGlobalContainer.TestServiceLocatorDirectUse;
begin
  CheckEquals(1, TObjectAccess(ServiceLocator).FRefCount);
end;

{$ENDIF}
{$ENDREGION}


{$REGION 'TTestEmptyContainer'}

procedure TTestEmptyContainer.TestResolveUnknownInterfaceService;
begin
  ExpectedException := EResolveException;
  fContainer.Resolve<INameService>;
end;

//procedure TTestEmptyContainer.TestResolveUnknownClassService;
//begin
//  ExpectedException := EResolveException;
//  fContainer.Resolve<TFoo2>;
//end;

procedure TTestEmptyContainer.TestRegisterNonGuidInterfaceService;
begin
  ExpectedException := ERegistrationException;
  fContainer.RegisterType<TNonGuid>.Implements<INonGuid>;
end;

procedure TTestEmptyContainer.TestRegisterTwoNamedDifferentServices;
var
  nameService: INameService;
  ageService: IAgeService;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>;
  fContainer.RegisterType<TNameAgeComponent>.Implements<IAgeService>;
  fContainer.Build;
  nameService := fContainer.Resolve<INameService>;
  CheckTrue(nameService is TNameService);
  ageService := fContainer.Resolve<IAgeService>;
  CheckTrue(ageService is TNameAgeComponent);
end;

procedure TTestEmptyContainer.TestRegisterTwoUnnamedServicesExplicit;
var
  service: INameService;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>;
  fContainer.RegisterType<TAnotherNameService>.Implements<INameService>;
  fContainer.Build;
  service := fContainer.Resolve<INameService>;
  CheckTrue(service is TAnotherNameService);
end;

procedure TTestEmptyContainer.TestRegisterTwoUnnamedServicesImplicit;
var
  service: INameService;
begin
  fContainer.RegisterType<TNameService>;
  fContainer.RegisterType<TAnotherNameService>;
  fContainer.Build;
  service := fContainer.Resolve<INameService>;
  CheckTrue(service is TAnotherNameService);
end;

procedure TTestEmptyContainer.TestRegisterGenericInterfaceService;
begin
  ExpectedException := ERegistrationException;
  fContainer.RegisterType<TNonGuid<TObject>>.Implements<INonGuid<TObject>>;
end;

procedure TTestEmptyContainer.TestRegisterUnassignableService;
begin
  ExpectedException := ERegistrationException;
  fContainer.RegisterType<TContainer>.Implements<IDispatch>;
end;

procedure TTestEmptyContainer.TestResolveAll;
var
  services: TArray<INameService>;
begin
  services := fContainer.ResolveAll<INameService>;
  CheckEquals(0, Length(services));
end;

procedure TTestEmptyContainer.TestResolveAllNonGeneric;
var
  services: TArray<TValue>;
begin
  services := fContainer.ResolveAll(TypeInfo(INameService));
  CheckEquals(0, Length(services));
end;

procedure TTestEmptyContainer.TestResolveReturnsUnnamedService;
var
  service: INameService;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>;
  fContainer.RegisterType<TAnotherNameService>.Implements<INameService>('some');
  fContainer.Build;
  service := fContainer.Resolve<INameService>;
  CheckTrue(service is TNameService, 'service should be a TNameService instance.');
end;

{$ENDREGION}


{$REGION 'TTestSimpleContainer'}

procedure TTestSimpleContainer.TestInterfaceService;
var
  service: INameService;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>;
  fContainer.Build;
  service := fContainer.Resolve<INameService>;
  try
    CheckNotNull(service, 'service should not be nil.');
    CheckTrue(service is TNameService, 'service should be a TNameService instance.');
    CheckEquals(TNameService.NameString, service.Name);
  finally
{$IFNDEF AUTOREFCOUNT}
    fContainer.Release(service);
{$ENDIF}
    service := nil;
  end;
end;

procedure TTestSimpleContainer.TestIssue13;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>.AsSingleton;
  fContainer.Build;
  Pass;
end;

procedure TTestSimpleContainer.TestAbstractClassService;
var
  service: TAgeServiceBase;
begin
  fContainer.RegisterType<TAgeServiceImpl>.Implements<TAgeServiceBase>;
  fContainer.Build;
  service := fContainer.Resolve<TAgeServiceBase>;
  try
    CheckIs(service, TAgeServiceImpl, 'service should be a TNameService instance.');
    CheckEquals(TAgeServiceImpl.DefaultAge, service.Age);
  finally
{$IFNDEF AUTOREFCOUNT}
    fContainer.Release(service);
{$ELSE}
    service := nil;
{$ENDIF}
  end;
end;

procedure TTestSimpleContainer.TestServiceSameAsComponent;
var
  service: TAgeServiceBase;
begin
  fContainer.RegisterType<TAgeServiceImpl>;
  fContainer.Build;
  service := fContainer.Resolve<TAgeServiceImpl>;
  try
    CheckNotNull(service, 'service should not be null.');
    CheckEquals(TAgeServiceImpl.DefaultAge, service.Age);
  finally
{$IFNDEF AUTOREFCOUNT}
    fContainer.Release(service);
{$ELSE}
    service := nil;
{$ENDIF}
  end;
end;

procedure TTestSimpleContainer.TestBootstrap;
var
  component: TBootstrapComponent;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>.AsSingleton;
  fContainer.RegisterType<TAgeServiceImpl>.Implements<TAgeServiceBase>.AsSingleton;
  fContainer.RegisterType<TBootstrapComponent>;
  fContainer.Build;
  component := fContainer.Resolve<TBootstrapComponent>;
  try
    CheckNotNull(component, 'component should not be nil.');
    CheckNotNull(component.NameService, 'NameService');
    CheckEquals(TNameService.NameString, component.NameService.Name);
    CheckNotNull(component.AgeService, 'AgeService');
    CheckEquals(TAgeServiceImpl.DefaultAge, component.AgeService.Age);
  finally
{$IFNDEF AUTOREFCOUNT}
    fContainer.Release(component);
{$ELSE}
    component := nil;
{$ENDIF}
  end;
end;

type
  TAbstractMethodsTest = class
  public
    procedure FooBar; virtual; abstract;
  end;

{$IFDEF DELPHIXE_UP}
procedure TTestSimpleContainer.TestClassContainsAbstractMethods;
var
  logger: Mock<ILogger>;
begin
  fContainer.Kernel.Logger := logger;
  fContainer.RegisterType<TAbstractMethodsTest>;
  fContainer.Build;

  logger.Received(1).Warn(Arg.IsAny<string>);
  Pass;
end;
{$ENDIF}

procedure TTestSimpleContainer.TestSingleton;
var
  obj1, obj2: TAgeServiceBase;
begin
  fContainer.RegisterType<TAgeServiceImpl>
    .Implements<TAgeServiceBase>
    .AsSingleton;
  fContainer.Build;
  obj1 := fContainer.Resolve<TAgeServiceBase>;
  obj2 := fContainer.Resolve<TAgeServiceBase>;
  try
    CheckNotNull(obj1, 'obj1 should not be nil');
    CheckNotNull(obj2, 'obj2 should not be nil');
    CheckSame(obj1, obj2, 'obj1 should be the same as obj2.');
  finally
{$IFNDEF AUTOREFCOUNT}
    fContainer.Release(obj1);
    try
      // might raise an exception because ClassType is nil with FastMM4 full debug
      fContainer.Release(obj2);
    except
    end;
{$ELSE}
    obj1 := nil;
    obj2 := nil;
{$ENDIF}
  end;
end;

procedure TTestSimpleContainer.TestTransient;
var
  obj1, obj2: TAgeServiceBase;
begin
  fContainer.RegisterType<TAgeServiceImpl>
    .Implements<TAgeServiceBase>;
  fContainer.Build;
  obj1 := fContainer.Resolve<TAgeServiceBase>;
  obj2 := fContainer.Resolve<TAgeServiceBase>;
  try
    CheckNotNull(obj1, 'obj1 should not be nil');
    CheckNotNull(obj2, 'obj2 should not be nil');
    CheckTrue(obj1 <> obj2, 'obj1 should not be the same as obj2.');
  finally
{$IFNDEF AUTOREFCOUNT}
    fContainer.Release(obj1);
    fContainer.Release(obj2);
{$ELSE}
    obj1 := nil;
    obj2 := nil;
{$ENDIF}
  end;
end;

type
  TTestSingletonThread = class(TThread)
  protected
    fContainer: TContainer;
    fService1: INameService;
    fService2: INameService;
    procedure Execute; override;
  public
    constructor Create(container: TContainer);
    property Service1: INameService read fService1;
    property Service2: INameService read fService2;
  end;

{ TTestSingletonThread }

constructor TTestSingletonThread.Create(container: TContainer);
begin
  inherited Create(False);
  fContainer := container;
end;

procedure TTestSingletonThread.Execute;
begin
  fService1 := fContainer.Resolve<INameService>;
  fService2 := fContainer.Resolve<INameService>;
end;

procedure TTestSimpleContainer.TestPerThread;
var
  thread1, thread2: TTestSingletonThread;
begin
  fContainer.RegisterType<TNameService>
    .Implements<INameService>
    .AsSingletonPerThread;
  fContainer.Build;
  thread1 := TTestSingletonThread.Create(fContainer);
  thread2 := TTestSingletonThread.Create(fContainer);
  try
    thread1.WaitFor;
    thread2.WaitFor;
    CheckTrue(thread1.Service1 is TNameService, 'thread1.Service1 should be TNameService.');
    CheckTrue(thread2.Service1 is TNameService, 'thread2.Service1 should be TNameService.');
    CheckSame(thread1.Service1, thread1.Service2, 'thread1');
    CheckSame(thread2.Service1, thread2.Service2, 'thread2');
    CheckTrue(thread1.Service1 <> thread2.Service2, 'thread1 and thread2 should own different instances.');
  finally
    thread1.Free;
    thread2.Free;
  end;
end;

procedure TTestSimpleContainer.TestPooled;
var
  service1, service2, service3: INameService;
  count: Integer;
begin
  count := 0;
  fContainer.RegisterType<TNameService>
    .Implements<INameService>
    .DelegateTo(
      function: TNameService
      begin
        Result := TNameService.Create;
        Inc(count);
      end)
    .AsPooled(2, 2);
  fContainer.Build;
  CheckEquals(0, count); // pool did not create any instances yet
  service1 := fContainer.Resolve<INameService>;
  CheckEquals(2, count); // pool created the minimum number of instances upon first request
  service2 := fContainer.Resolve<INameService>;
  service3 := fContainer.Resolve<INameService>;
  CheckEquals(3, count); // pool created one additional instance
  service3 := nil;
  service2 := nil;
  service1 := nil;
  service1 := fContainer.Resolve<INameService>; // pool collects unused instances up to maximum count
  service2 := fContainer.Resolve<INameService>;
  CheckEquals(3, count);
  service3 := fContainer.Resolve<INameService>; // pool creates a new instance again
  CheckEquals(4, count);
end;

procedure TTestSimpleContainer.TestPooledRefCountedInterface;
var
  service1, service2, service3: INameService;
  count: Integer;
begin
  count := 0;
  fContainer.RegisterType<TCustomNameService>
    .Implements<INameService>
    .DelegateTo(
      function: TCustomNameService
      begin
        Result := TCustomNameService.Create;
        Inc(count);
      end)
    .AsPooled(2, 2);
  fContainer.Build;
  CheckEquals(0, count); // pool did not create any instances yet
  service1 := fContainer.Resolve<INameService>;
  CheckEquals(2, count); // pool created the minimum number of instances upon first request
  service2 := fContainer.Resolve<INameService>;
  service3 := fContainer.Resolve<INameService>;
  CheckEquals(3, count); // pool created one additional instance
  service3 := nil;
  service2 := nil;
  service1 := nil;
  service1 := fContainer.Resolve<INameService>; // pool collects unused instances up to maximum count
  service2 := fContainer.Resolve<INameService>;
  CheckEquals(3, count);
  service3 := fContainer.Resolve<INameService>; // pool creates a new instance again
  CheckEquals(4, count);
end;

procedure TTestSimpleContainer.TestRecordConstructorNotConsidered;
begin
  fContainer.RegisterType<Lazy<INameService>>.DelegateTo(
    function: Lazy<INameService>
    begin
    end);
  // Lazy<T> has a constructor which the TConstructorInspector mistakenly inspected
  fContainer.Build;
  Pass;
end;

procedure TTestSimpleContainer.TestRecyclable;
var
  service1, service2: IAnotherService;
  instance: Pointer;
begin
  fContainer.RegisterType<TRecyclableComponent>.AsPooled(2, 2);
  fContainer.Build;
  service1 := fContainer.Resolve<IAnotherService>;
  service2 := fContainer.Resolve<IAnotherService>;
  CheckTrue(service2 is TRecyclableComponent, 'Unknown component');
  instance := TRecyclableComponent(service2); // remember second because the first will be returned again later
  CheckTrue(TRecyclableComponent(instance).IsInitialized, 'IsInitialized');
  CheckFalse(TRecyclableComponent(instance).IsRecycled, 'IsRecycled');
  service1 := nil;
  service2 := nil;
  service1 := fContainer.Resolve<IAnotherService>; // pool has no available and collects all unused
  CheckFalse(TRecyclableComponent(instance).IsInitialized, 'IsInitialized');
  CheckTrue(TRecyclableComponent(instance).IsRecycled, 'IsRecycled');
end;

procedure TTestSimpleContainer.TestDisposable;

  // place in a separate call to generate finalization around returned
  // TRegistration
  procedure Register;
  begin
    fContainer.RegisterType<TDisposableComponent>.AsSingleton;
  end;

var
  service: IAnotherService;
  disposed: Boolean;
  component: TDisposableComponent;
begin
  Register;
  fContainer.Build;
  service := fContainer.Resolve<IAnotherService>;
  component:=(service as TDisposableComponent);
  component.OnDispose :=
    procedure
    begin
      disposed := True;
    end;
  service := nil;
{$IFDEF AUTOREFCOUNT}
  component := nil;
{$ENDIF}
  disposed := False;
  FreeAndNil(fContainer);
  CheckTrue(disposed);
end;

type
  TTestComponent = class(TComponent);

  TTestComponent2 = class(TComponent)
  private
    fConstructorCalled: Boolean;
  public
    constructor Create(owner: TComponent); override;
  end;

constructor TTestComponent2.Create(owner: TComponent);
begin
  inherited;
  fConstructorCalled := True;
end;

procedure TTestSimpleContainer.TestResolveComponentDoesCallOverriddenConstructor;
var
  obj: TTestComponent2;
begin
  fContainer.RegisterType<TTestComponent2>;
  fContainer.Build;
  obj := fContainer.Resolve<TTestComponent2>;
  try
    CheckIs(obj, TTestComponent2);
    CheckTrue(obj.fConstructorCalled);
    CheckNull(obj.Owner);
  finally
    obj.Free;
  end;
end;

procedure TTestSimpleContainer.TestResolveComponentDoesNotFallbackToTObject;
var
  obj: TComponent;
begin
  fContainer.RegisterType<TTestComponent>;
  fContainer.Build;
  obj := fContainer.Resolve<TTestComponent>;
  try
    CheckIs(obj, TTestComponent);
    Check(obj.ComponentStyle = [csInheritable]);
    CheckNull(obj.Owner);
  finally
    obj.Free;
  end;
end;

procedure TTestSimpleContainer.TestResolveFuncWithTwoTypes;
begin
  fContainer.RegisterType<TNameService>;
  fContainer.Build;

  CheckException(EResolveException,
    procedure
    begin
      fContainer.Resolve<TFunc<IAgeService, INameService>>;
    end);
end;


procedure TTestSimpleContainer.TestResolveUnknownClasses;
var
  factory: ISomeFactory;
begin
  fContainer.RegisterType<ISomeService, TSomeService>;
  fContainer.RegisterType<ISomeFactory, TSomeFactory>;
  fContainer.Build;

  factory := fContainer.Resolve<ISomeFactory>;
  Pass;
end;

procedure TTestSimpleContainer.TestInitializable;
var
  service: IAnotherService;
begin
  fContainer.RegisterType<TInitializableComponent>;
  fContainer.Build;
  service := fContainer.Resolve<IAnotherService>;
  CheckTrue(service is TInitializableComponent, 'Unknown component.');
  CheckTrue(TInitializableComponent(service).IsInitialized, 'IsInitialized');
end;

procedure TTestSimpleContainer.TestIssue41_DifferentLifetimes;
var
  service1, service2: INameService;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>;
  fContainer.RegisterType<TDynamicNameService>.Implements<IAnotherNameService>.AsSingleton;
  fContainer.Build;

  service1 := fContainer.Resolve<INameService>;
  service2 := fContainer.Resolve<IAnotherNameService>;

  Check((service1 as TObject) <> (service2 as TObject), 'resolved services should be different');
end;

procedure TTestSimpleContainer.TestIssue41_DifferentName;
var
  service: INameService;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>('first').DelegateTo(
    function: TDynamicNameService
    begin
      Result := TDynamicNameService.Create('first');
    end
    );
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>('second').DelegateTo(
    function: TDynamicNameService
    begin
      Result := TDynamicNameService.Create('second');
    end
    );
  fContainer.Build;

  service := fContainer.Resolve<INameService>('second');
  CheckEquals('second', service.Name, 'resolving of service "second" failed');

  service := fContainer.Resolve<INameService>('first');
  CheckEquals('first', service.Name, 'resolving of service "first" failed');
end;

procedure TTestSimpleContainer.TestIssue41_DifferentService;
var
  service: INameService;
  anotherService: IAnotherNameService;
begin
   fContainer.RegisterType<TDynamicNameService>.Implements<INameService>.DelegateTo(
    function: TDynamicNameService
    begin
      Result := TDynamicNameService.Create('first');
    end
    );
  fContainer.RegisterType<TDynamicNameService>.Implements<IAnotherNameService>.DelegateTo(
    function: TDynamicNameService
    begin
      Result := TDynamicNameService.Create('second');
    end
    );
  fContainer.Build;

  anotherService := fContainer.Resolve<IAnotherNameService>;
  CheckEquals('second', anotherService.Name, 'resolving of service "second" failed');

  service := fContainer.Resolve<INameService>;
  CheckEquals('first', service.Name, 'resolving of service "first" failed');
end;

procedure TTestSimpleContainer.TestIssue49;
var
  count: Integer;
  obj: TNameServiceWithAggregation;
begin
  fContainer.RegisterType<TNameServiceWithAggregation>.Implements<INameService>
    .InjectField('fAgeService').InjectField('fAgeService')
    .InjectProperty('AgeService').InjectProperty('AgeService')
    .InjectMethod('Init').InjectMethod('Init');
  fContainer.RegisterType<TNameAgeComponent>.Implements<IAgeService>.DelegateTo(
    function: TNameAgeComponent
    begin
      Result := TNameAgeComponent.Create;
      Inc(count);
    end);
  fContainer.Build;
  fContainer.Build;

  count := 0;
  obj := fContainer.Resolve<INameService> as TNameServiceWithAggregation;
  CheckEquals(2, count); // field and property
  CheckEquals(1, obj.MethodCallCount);
end;

procedure TTestSimpleContainer.TestIssue50;
var
  count: Integer;
  obj: TNameServiceWithAggregation;
begin
  fContainer.RegisterType<TNameServiceWithAggregation>.Implements<INameService>.DelegateTo(
    function: TNameServiceWithAggregation
    begin
      Result := TNameServiceWithAggregation.Create;
    end);
  fContainer.RegisterType<TNameAgeComponent>.Implements<IAgeService>.DelegateTo(
    function: TNameAgeComponent
    begin
      Result := TNameAgeComponent.Create;
      Inc(count);
    end);
  fContainer.Build;

  count := 0;
  obj := fContainer.Resolve<INameService> as TNameServiceWithAggregation;
  CheckEquals(2, count); // field and property
  CheckEquals(1, obj.MethodCallCount);
end;

{$ENDREGION}


{$REGION 'TTestDifferentImplementations'}

procedure TTestDifferentServiceImplementations.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<TNameService>
    .Implements<INameService>('default')
    .AsSingleton;
  fContainer.RegisterType<TAnotherNameService>
    .Implements<INameService>('another');
  fContainer.Build;
  fNameService := fContainer.Resolve<INameService>('default');
  fAnotherNameService := fContainer.Resolve<INameService>('another');
  fServices := fContainer.ResolveAll<INameService>;
  fServiceValues := fContainer.ResolveAll(TypeInfo(INameService));
end;

procedure TTestDifferentServiceImplementations.TearDown;
begin
{$IFNDEF AUTOREFCOUNT}
  fContainer.Release(fAnotherNameService);
{$ENDIF}
  fAnotherNameService := nil;

{$IFNDEF AUTOREFCOUNT}
  fContainer.Release(fNameService);
{$ENDIF}
  fNameService := nil;

  fServices := Default(TArray<INameService>);
  fServiceValues := Default(TArray<TValue>);

  inherited TearDown;
end;

procedure TTestDifferentServiceImplementations.TestNameService;
begin
  CheckNotNull(fNameService, 'fNameService should not be nil.');
  CheckTrue(fNameService is TNameService, 'fNameService should be an instance of TNameService.');
  CheckEquals(TNameService.NameString, fNameService.Name);
end;

procedure TTestDifferentServiceImplementations.TestAnotherNameService;
begin
  CheckNotNull(fAnotherNameService, 'fAnotherNameService should not be nil.');
  CheckTrue(fAnotherNameService is TAnotherNameService, 'fAnotherNameService should be an instance of TAnotherNameService.');
  CheckEquals(TAnotherNameService.NameString, fAnotherNameService.Name);
end;

procedure TTestDifferentServiceImplementations.TestResolveAll;
begin
  CheckEquals(2, Length(fServices), 'Count of fServices should be 2.');
  CheckTrue(fServices[0] is TNameService);
  CheckTrue(fServices[1] is TAnotherNameService);
end;

procedure TTestDifferentServiceImplementations.TestResolveAllNonGeneric;
begin
  CheckEquals(2, Length(fServiceValues), 'Count of fServiceValues should be 2.');
  CheckTrue((fServiceValues[0].AsType<INameService>) is TNameService);
  CheckTrue((fServiceValues[1].AsType<INameService>) is TAnotherNameService);
end;

procedure TTestDifferentServiceImplementations.TestUnsatisfiedDependency;
begin
  ExpectedException := EResolveException;
  fContainer.Resolve<INameService>;
end;

{$ENDREGION}


{$REGION 'TTestActivatorDelegate'}

procedure TTestActivatorDelegate.SetUp;
begin
  inherited SetUp;
  fExpectedInteger := 26;
  fExpectedString := 'String';
  fContainer.RegisterType<TNameService>
    .Implements<INameService>
    .AsSingleton;
  fContainer.RegisterType<TPrimitiveComponent>
    .Implements<IPrimitive>
    .DelegateTo(
      function: TPrimitiveComponent
      begin
        Result := TPrimitiveComponent.Create(
          fContainer.Resolve<INameService>,
          fExpectedInteger,
          fExpectedString
        );
      end
    );
  fContainer.Build;
  fPrimitive := fContainer.Resolve<IPrimitive>;
  CheckNotNull(fPrimitive, 'fPrimitive should not be nil.');
  CheckNotNull(fPrimitive.NameService, 'fPrimitive.NameService should not be nil.');
end;

procedure TTestActivatorDelegate.TestNameService;
begin
  CheckNotNull(fPrimitive.NameService, 'NameService should not be nil.');
  CheckTrue(fPrimitive.NameService is TNameService, 'Unexpected type.');
  CheckEquals(TNameService.NameString, fPrimitive.NameService.Name);
end;

procedure TTestActivatorDelegate.TearDown;
begin
  inherited;
  fPrimitive := nil;
  fExpectedString := '';
end;

procedure TTestActivatorDelegate.TestIntegerArgument;
begin
  CheckEquals(fExpectedInteger, fPrimitive.IntegerArg);
end;

procedure TTestActivatorDelegate.TestStringArgument;
begin
  CheckEquals(fExpectedString, fPrimitive.StringArg);
end;

{$ENDREGION}


{$REGION 'TTestTypedInjections'}

procedure TTypedInjectionTestCase.SetUp;
begin
  inherited SetUp;
  DoRegisterComponents;
  fContainer.Build;
  fNameService := fContainer.Resolve<INameService>;
  CheckIs(fNameService, TNameService, 'fNameService should be TNameService.');
  CheckEquals(TNameService.NameString, fNameService.Name, 'fNameService.Name is wrong.');
  fInjectionExplorer := fContainer.Resolve<IInjectionExplorer>;
end;

procedure TTypedInjectionTestCase.TearDown;
begin
{$IFNDEF AUTOREFCOUNT}
  fContainer.Release(fInjectionExplorer);
  fContainer.Release(fNameService);
{$ENDIF}
  fInjectionExplorer := nil;
  fNameService := nil;
  inherited TearDown;
end;

procedure TTypedInjectionTestCase.TestConstructorInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.ConstructorInjection);
end;

procedure TTypedInjectionTestCase.TestPropertyInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.PropertyInjection);
end;

procedure TTypedInjectionTestCase.TestMethodInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.MethodInjection);
end;

procedure TTypedInjectionTestCase.TestFieldInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.FieldInjection);
end;

{$ENDREGION}


{$REGION 'TTestTypedInjectionByCoding'}

procedure TTestTypedInjectionByCoding.DoRegisterComponents;
begin
  fContainer.RegisterType<TNameService>
    .Implements<INameService>
    .AsSingleton;
  fContainer.RegisterType<TInjectionExplorer>
    .Implements<IInjectionExplorer>
    .InjectConstructor([TypeInfo(INameService)])
    .InjectProperty('PropertyInjection')
    .InjectMethod('SetMethodInjection')
    .InjectField('fFieldInjection');
end;

{$ENDREGION}


{$REGION 'TTestTypedInjectionsByAttribute'}

procedure TTestTypedInjectionsByAttribute.DoRegisterComponents;
begin
  fContainer.RegisterType<TNameService>
    .Implements<INameService>
    .AsSingleton;
  fContainer.RegisterType<TInjectionExplorerComponent>
    .Implements<IInjectionExplorer>;
end;

{$ENDREGION}


{$REGION 'TTestDirectCircularDependency'}

procedure TTestDirectCircularDependency.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<TCircularDependencyChicken>.Implements<IChicken>;
  fContainer.Build;
end;

procedure TTestDirectCircularDependency.TestResolve;
var
  chicken: IChicken;
begin
  ExpectedException := ECircularDependencyException;
  chicken := fContainer.Resolve<IChicken>;
  Pass;
end;

{$ENDREGION}


{$REGION 'TTestCrossedCircularDependency'}

procedure TTestCrossedCircularDependency.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<TCircularDependencyChicken>.Implements<IChicken>;
  fContainer.RegisterType<TEgg>.Implements<IEgg>;
  fContainer.Build;
end;

procedure TTestCrossedCircularDependency.TestResolveChicken;
var
  chicken: IChicken;
begin
  ExpectedException := ECircularDependencyException;
  chicken := fContainer.Resolve<IChicken>;
end;

procedure TTestCrossedCircularDependency.TestResolveEgg;
var
  egg: IEgg;
begin
  ExpectedException := ECircularDependencyException;
  egg := fContainer.Resolve<IEgg>;
end;

{$ENDREGION}


{$REGION 'TTestCircularReferenceLazySingleton'}

procedure TTestCircularReferenceLazySingleton.TestResolveLazySingleton;
var
  chicken: IChicken;
begin
  fContainer.RegisterType<TChicken>.AsSingleton;
  fContainer.RegisterType<TEggLazyChicken>;
  fContainer.Build;

  chicken := fContainer.Resolve<IChicken>;
  CheckSame(chicken, chicken.Egg.Chicken);
  chicken.Egg := nil;
end;

{$ENDREGION}


{$REGION 'TTestPerResolve'}

procedure TTestPerResolve.TestResolveCircularDependency;
var
  chicken: IChicken;
begin
  fContainer.RegisterType<IChicken, TChicken>
    .InjectConstructor
    .PerResolve
    .InjectProperty('Egg');
  fContainer.RegisterType<IEgg, TEgg>;
  fContainer.Build;
  chicken := fContainer.Resolve<IChicken>;
  CheckSame(chicken, chicken.Egg.Chicken);
  chicken.Egg := nil;
end;

{$ENDREGION}


{$REGION 'TNamedInjectionsTestCase'}

procedure TNamedInjectionsTestCase.DoRegisterComponents;
begin
  fContainer.RegisterType<TNameService>
    .Implements<INameService>('default')
    .AsSingleton;
  fContainer.RegisterType<TAnotherNameService>
    .Implements<INameService>('another')
    .AsSingleton;
end;

procedure TNamedInjectionsTestCase.SetUp;
begin
  inherited SetUp;
  DoRegisterComponents;
  fContainer.Build;
  fExplorer := fContainer.Resolve<IInjectionExplorer>;
end;

procedure TNamedInjectionsTestCase.TearDown;
begin
  inherited;
  fExplorer := nil;
end;

procedure TNamedInjectionsTestCase.TestConstructorInjection;
begin
  CheckNotNull(fExplorer.ConstructorInjection);
  CheckEquals(TNameService.NameString, fExplorer.ConstructorInjection.Name);
end;

procedure TNamedInjectionsTestCase.TestPropertyInjection;
begin
  CheckNotNull(fExplorer.PropertyInjection);
  CheckEquals(TAnotherNameService.NameString, fExplorer.PropertyInjection.Name);
end;

procedure TNamedInjectionsTestCase.TestMethodInjection;
begin
  CheckNotNull(fExplorer.MethodInjection);
  CheckEquals(TAnotherNameService.NameString, fExplorer.MethodInjection.Name);
end;

procedure TNamedInjectionsTestCase.TestFieldInjection;
begin
  CheckNotNull(fExplorer.FieldInjection);
  CheckEquals(TNameService.NameString, fExplorer.FieldInjection.Name);
end;

{$ENDREGION}


{$REGION 'TTestNamedInjectionsByCoding'}

procedure TTestNamedInjectionsByCoding.DoRegisterComponents;
begin
  inherited DoRegisterComponents;
  fContainer.RegisterType<TInjectionExplorer>
    .Implements<IInjectionExplorer>
    .InjectConstructor(['default'])
    .InjectProperty('PropertyInjection', 'another')
    .InjectMethod('SetMethodInjection', ['another'])
    .InjectField('fFieldInjection', 'default')
    .AsSingleton;
end;

{$ENDREGION}


{$REGION 'TTestNamedInjectionsByAttribute'}

procedure TTestNamedInjectionsByAttribute.DoRegisterComponents;
begin
  inherited DoRegisterComponents;
  fContainer.RegisterType<TInjectionComponent>
    .Implements<IInjectionExplorer>;
end;

{$ENDREGION}


{$REGION 'TTestImplementsDifferentServices'}

procedure TTestImplementsDifferentServices.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<TNameService>
    .Implements<INameService>('another');
  fContainer.RegisterType<TNameAgeComponent>
    .Implements<INameService>('default')
    .Implements<IAgeService>
    .AsSingleton;
  fContainer.Build;
  fNameService := fContainer.Resolve<INameService>('default');
  fAgeService := fContainer.Resolve<IAgeService>;
  CheckNotNull(fNameService, 'fNameService should not be nil.');
  CheckNotNull(fAgeService, 'fAgeService should not be nil.');
end;

procedure TTestImplementsDifferentServices.TearDown;
begin
{$IFNDEF AUTOREFCOUNT}
  fContainer.Release(fAgeService);
  fContainer.Release(fNameService);
{$ENDIF}
  fAgeService := nil;
  fNameService := nil;
  inherited TearDown;
end;

procedure TTestImplementsDifferentServices.TestNameService;
begin
  Check(fNameService is TNameAgeComponent);
  CheckEquals(TNameAgeComponent.NameString, fNameService.Name);
end;

procedure TTestImplementsDifferentServices.TestAgeService;
begin
  Check(fAgeService is TNameAgeComponent);
  CheckEquals(TNameAgeComponent.DefaultAge, fAgeService.Age);
end;

{$ENDREGION}


{$REGION 'TTestImplementsAttribute'}

type
  IS1 = interface
    ['{E6DE68D5-988C-4817-880E-58903EE8B78C}']
  end;

  IS2 = interface
    ['{0DCC1BD5-28C1-4A94-8667-AC72BA25C682}']
  end;

  TS1 = class(TInterfacedObject, IS1)
  end;

  [Implements(TypeInfo(IS1), 'b')]
  [Implements(TypeInfo(IS2))]
  TS2 = class(TInterfacedObject, IS1, IS2)
  end;

procedure TTestImplementsAttribute.TestImplements;
var
  s1: IS1;
  s2: IS2;
begin
  fContainer.RegisterType<TS1>.Implements<IS1>('a');
  fContainer.RegisterType<TS2>;
  fContainer.Build;
  s1 := fContainer.Resolve<IS1>('a');
  CheckTrue(s1 is TS1, 'a');
  s1 := fContainer.Resolve<IS1>('b');
  CheckTrue(s1 is TS2, 'b');
  s2 := fContainer.Resolve<IS2>;
  CheckTrue(s2 is TS2, 's2');
end;

{$ENDREGION}


{$REGION 'TTestRegisterInterfaces'}

type
  TComplex = class(TNameAgeComponent, IAnotherService)
  end;

procedure TTestRegisterInterfaces.TestOneService;
var
  service: INameService;
begin
  fContainer.RegisterType<TNameService>;
  fContainer.Build;
  service := fContainer.Resolve<INameService>;
  CheckTrue(service is TNameService);
end;

procedure TTestRegisterInterfaces.TestParentInterface;
var
  service: INameService;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<IAnotherNameService>('another');
  fContainer.Build;
  service := fContainer.Resolve<INameService>('another');
  CheckIs(service, TDynamicNameService);
end;

procedure TTestRegisterInterfaces.TestTwoServices;
var
  s1: INameService;
  s2: IAgeService;
begin
  fContainer.RegisterType<TNameAgeComponent>;
  fContainer.Build;
  s1 := fContainer.Resolve<INameService>;
  s2 := fContainer.Resolve<IAgeService>;
  CheckTrue(s1 is TNameAgeComponent, 's1');
  CheckTrue(s2 is TNameAgeComponent, 's2');
end;

procedure TTestRegisterInterfaces.TestTwoServicesWithSameName;
var
  s1: INameService;
  s2: Spring.Tests.Container.Interfaces.INameService;
begin
  fContainer.RegisterType<TNameServiceWithTwoInterfaces>;
  fContainer.Build;
  s1 := fContainer.Resolve<INameService>;
  s2 := fContainer.Resolve<Spring.Tests.Container.Interfaces.INameService>;
  CheckTrue(s1 is TNameServiceWithTwoInterfaces, 's1');
  CheckTrue(s2 is TNameServiceWithTwoInterfaces, 's2');
end;

procedure TTestRegisterInterfaces.TestInheritedService;
var
  s1: INameService;
  s2: IAgeService;
  s3: IAnotherService;
begin
  CheckTrue(Assigned(TypeInfo(IAnotherService)));
  fContainer.RegisterType<TComplex>;
  fContainer.Build;
  s1 := fContainer.Resolve<INameService>;
  s2 := fContainer.Resolve<IAgeService>;
  s3 := fContainer.Resolve<IAnotherService>;
  CheckTrue(s1 is TComplex, 's1');
  CheckTrue(s2 is TComplex, 's2');
  CheckTrue(s3 is TComplex, 's3');
end;

{$ENDREGION}


{$REGION 'TTestDefaultResolve'}

procedure TTestDefaultResolve.TestNoDefault;
begin
  StartExpectingException(EResolveException);
  fContainer.Resolve<INameService>;
  StopExpectingException;
end;

procedure TTestDefaultResolve.TestRegisterDefault;
begin
  StartExpectingException(ERegistrationException);
  fContainer.RegisterType<INameService, TNameService>.AsDefault<IAgeService>;
  StopExpectingException;
end;

procedure TTestDefaultResolve.TestResolve;
var
  service: INameService;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>;
  fContainer.RegisterType<TAnotherNameService>.Implements<INameService>('another');
  fContainer.Build;
  service := fContainer.Resolve<INameService>;
  CheckEquals('Name', service.Name);
end;

procedure TTestDefaultResolve.TestResolveDependency;
var
  component: TBootstrapComponent;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>;
  fContainer.RegisterType<TAnotherNameService>.Implements<INameService>('another');
  fContainer.RegisterType<TBootstrapComponent>.AsSingleton;
  fContainer.Build;
  component := fContainer.Resolve<TBootstrapComponent>;
  CheckEquals('Name', component.NameService.Name);
end;

{$ENDREGION}


{$REGION 'TTestInjectionByValue'}

procedure TTestInjectionByValue.TestInjectField;
begin
  fContainer.RegisterType<TPrimitiveComponent>.Implements<IPrimitive>
    .InjectField('fNameService', TValue.From<INameService>(TNameService.Create));
  fContainer.Build;
  CheckTrue(fContainer.Resolve<IPrimitive>.NameService <> nil)
end;

{$ENDREGION}


{$REGION 'TTestResolverOverride'}

procedure TTestResolverOverride.SetUp;
begin
  inherited;
  fDummy := TObject.Create;
end;

procedure TTestResolverOverride.TearDown;
begin
  inherited;
  fDummy.Free;
end;

procedure TTestResolverOverride.TestResolve;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>('dynamic');
  fContainer.Build;

  CheckEquals('test', fContainer.Resolve<INameService>(['test']).Name);

  CheckEquals('test', fContainer.Resolve<INameService>('dynamic', ['test']).Name);

  CheckEquals('test', fContainer.Resolve<INameService>([TNamedValue.From('test', 'name')]).Name);
end;

procedure TTestResolverOverride.TestResolveWithClass;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>('dynamic');
  fContainer.Build;

  CheckEquals(fdummy.ClassName, fContainer.Resolve<INameService>([fDummy]).Name);

  CheckEquals(fdummy.ClassName, fContainer.Resolve<INameService>([TNamedValue.From(fDummy, 'obj')]).Name);
end;

procedure TTestResolverOverride.TestResolveWithDependency;
var
  service: INameService;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>('dynamic')
    .InjectField('fAgeService');
  fContainer.RegisterType<TNameAgeComponent>.Implements<IAgeService>;
  fContainer.Build;

  service := fContainer.Resolve<INameService>([fDummy]);
  CheckEquals(fdummy.ClassName, service.Name);
  CheckNotNull((service as TDynamicNameService).AgeService);

  service := fContainer.Resolve<INameService>([TNamedValue.From(fDummy, 'obj')]);
  CheckEquals(fdummy.ClassName, service.Name);
  CheckNotNull((service as TDynamicNameService).AgeService);
end;

procedure TTestResolverOverride.TestResolveWithDependencySubClass;
var
  service: INameService;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>('dynamic')
    .InjectField('fAgeService');
  fContainer.RegisterType<TNameAgeComponent>.Implements<IAgeService>;
  fContainer.Build;

  // ctor requires TObject but we pass a sub class
  fDummy.Free;
  fDummy := TPersistent.Create;

  service := fContainer.Resolve<INameService>([TNamedValue.From(fDummy, 'obj')]);
  CheckEquals(fdummy.ClassName, service.Name);
  CheckNotNull((service as TDynamicNameService).AgeService);
end;

procedure TTestResolverOverride.TestResolveWithMultipleParams;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>('dynamic');
  fContainer.Build;

  CheckEquals('test' + fDummy.ClassName, fContainer.Resolve<INameService>(['test', fDummy]).Name);

  CheckEquals(fdummy.ClassName, fContainer.Resolve<INameService>([TNamedValue.From(fDummy, 'obj')]).Name);
end;

{$ENDREGION}


{$REGION 'TTestRegisterFactory'}

procedure TTestRegisterInterfaceTypes.TestOneService;
begin
  fContainer.RegisterType<INameService>.DelegateTo(
    function: INameService
    begin
      Result := TDynamicNameService.Create('test');
    end);
  fContainer.Build;

  CheckEquals('test', fContainer.Resolve<INameService>.Name);
end;

procedure TTestRegisterInterfaceTypes.TestOneServiceAsSingleton;
var
  count: Integer;
begin
  count := 0;
  fContainer.RegisterType<INameService>.DelegateTo(
    function: INameService
    begin
      Result := TDynamicNameService.Create('test');
      Inc(count);
    end).AsSingleton;
  fContainer.Build;

  CheckEquals('test', fContainer.Resolve<INameService>.Name);
  CheckEquals('test', fContainer.Resolve<INameService>.Name);
  CheckEquals(1, count);
end;

procedure TTestRegisterInterfaceTypes.TestOneServiceAsSingletonPerThread;
var
  count: Integer;
begin
  count := 0;
  fContainer.RegisterType<INameService>.DelegateTo(
    function: INameService
    begin
      Result := TDynamicNameService.Create('test');
      Inc(count);
    end).AsSingletonPerThread;
  fContainer.Build;

  CheckEquals('test', fContainer.Resolve<INameService>.Name);
  CheckEquals('test', fContainer.Resolve<INameService>.Name);
  CheckEquals(1, count);
end;

procedure TTestRegisterInterfaceTypes.TestTwoRegistrations;
begin
  fContainer.RegisterType<INameService, INameService>('service1').DelegateTo(
    function: INameService
    begin
      Result := TDynamicNameService.Create('service1');
    end);
  fContainer.RegisterType<INameService, INameService>('service2').DelegateTo(
    function: INameService
    begin
      Result := TDynamicNameService.Create('service2');
    end);
  fContainer.Build;

  CheckEquals('service1', fContainer.Resolve<INameService>('service1').Name);
  CheckEquals('service2', fContainer.Resolve<INameService>('service2').Name);
end;

procedure TTestRegisterInterfaceTypes.TestTwoServices;
begin
  fContainer.RegisterType<INameService, INameService>.DelegateTo(
    function: INameService
    begin
      Result := TDynamicNameService.Create('test');
    end).Implements<IAnotherNameService>('another');
  fContainer.Build;

  CheckEquals('test', fContainer.Resolve<INameService>.Name);
  CheckEquals('test', fContainer.Resolve<IAnotherNameService>.Name);
end;

{$ENDREGION}


{$REGION 'TTestResolveLazy'}

procedure TTestLazyDependencies.PerformChecks;
var
  nameService: INameService;
begin
  fCalled := False;
  nameService := fContainer.Resolve<INameService>('service');
  CheckFalse(fCalled);
  CheckEquals(TNameService.NameString, nameService.Name);
  CheckTrue(fCalled);
end;

procedure TTestLazyDependencies.SetUp;
begin
  inherited;

  fContainer.RegisterType<INameService>.Implements<INameService>('lazy').DelegateTo(
    function: INameService
    begin
      Result := TNameService.Create;
      fCalled := True;
    end);
end;

procedure TTestLazyDependencies.TestDependencyTypeIsFunc;
begin
  fContainer.RegisterType<TNameServiceLazyWithFunc>.Implements<INameService>('service');
  fContainer.Build;

  PerformChecks;
end;

{$IFNDEF IOSSIMULATOR}
procedure TTestLazyDependencies.TestDependencyTypeIsInterface;
begin
  fContainer.RegisterType<TNameServiceLazyWithInterface>.Implements<INameService>('service');
  fContainer.Build;

  PerformChecks;
end;

procedure TTestLazyDependencies.TestDependencyTypeIsRecord;
begin
  fContainer.RegisterType<TNameServiceLazyWithRecord>.Implements<INameService>('service');
  fContainer.Build;

  PerformChecks;
end;
{$ENDIF}

type
  TTestWithLazySingletonBase = class
  private
    fA: INameService;
    fB: Lazy<INameService>;
  public
    property A: INameService read fA;
    property B: Lazy<INameService> read fB;
  end;

  TTestWithLazySingletonResolveFirst = class(TTestWithLazySingletonBase)
  public
    constructor Create(const a: INameService; const b: Lazy<INameService>);
  end;

  TTestWithLazySingletonResolveLast = class(TTestWithLazySingletonBase)
  public
    constructor Create(const b: Lazy<INameService>; const a: INameService);
  end;

constructor TTestWithLazySingletonResolveFirst.Create(const a: INameService;
  const b: Lazy<INameService>);
begin
  fA := a;
  fB := b;
end;

constructor TTestWithLazySingletonResolveLast.Create(
  const b: Lazy<INameService>; const a: INameService);
begin
  fA := a;
  fB := b;
end;

procedure TTestLazyDependencies.TestDependencyTypeIsSingletonAlreadyInstantiated;
var
  test: TTestWithLazySingletonResolveFirst;
begin
  fContainer.RegisterType<INameService, TNameService>.AsSingleton;
  fContainer.RegisterType<TTestWithLazySingletonResolveFirst>;
  fContainer.Build;

  test := fContainer.Resolve<TTestWithLazySingletonResolveFirst>;
  try
    CheckSame(test.A, test.B.Value);
  finally
    test.Free;
  end;
end;

procedure TTestLazyDependencies.TestDependencyTypeIsSingletonNotYetInstantiated;
var
  test: TTestWithLazySingletonResolveLast;
begin
  fContainer.RegisterType<INameService, TNameService>.AsSingleton;
  fContainer.RegisterType<TTestWithLazySingletonResolveLast>;
  fContainer.Build;

  test := fContainer.Resolve<TTestWithLazySingletonResolveLast>;
  try
    CheckSame(test.A, test.B.Value);
  finally
    test.Free;
  end;
end;

procedure TTestLazyDependencies.TestResolveLazyOfSingletonCreatedLater;
var
  x: Lazy<INameService>;
  y: INameService;
begin
  fContainer.RegisterType<INameService, TNameService>.AsSingleton;
  fContainer.Build;

  x := fContainer.Resolve<Lazy<INameService>>;
  CheckFalse(x.IsValueCreated);
  CheckIs(x.Value, TNameService);
  y := fContainer.Resolve<INameService>;
  CheckSame(x.Value, y);
end;

{$ENDREGION}


{$REGION 'TTestResolveLazyRecursive'}

procedure TTestLazyDependenciesDetectRecursion.PerformChecks;
var
  model: TComponentModel;
begin
  model := fContainer.Kernel.Registry.FindOne('service');
  fContainer.Kernel.Injector.InjectField(model, 'fNameService', 'service');

  ExpectedException := ECircularDependencyException;
  inherited;
end;

{$ENDREGION}


{$REGION 'TTestManyDependencies'}

procedure TTestManyDependencies.SetUp;
begin
  inherited;
  fContainer.RegisterType<ICollectionItem, TCollectionItemA>('a');
  fContainer.RegisterType<ICollectionItem, TCollectionItemB>('b');
  fContainer.RegisterType<ICollectionItem, TCollectionItemC>('c');
end;

procedure TTestManyDependencies.TestInjectArray;
var
  service: ICollectionService;
begin
  fContainer.RegisterType<ICollectionService, TCollectionServiceA>;
  fContainer.Build;
  service := fContainer.Resolve<ICollectionService>;
  CheckEquals(3, Length(service.CollectionItems));
  CheckIs(service.CollectionItems[0], TCollectionItemA);
  CheckIs(service.CollectionItems[1], TCollectionItemB);
  CheckIs(service.CollectionItems[2], TCollectionItemC);
end;

procedure TTestManyDependencies.TestInjectArrayOfLazy;
var
  service: ICollectionService;
begin
  fContainer.RegisterType<ICollectionService, TCollectionServiceC>;
  fContainer.Build;
  service := fContainer.Resolve<ICollectionService>;
  CheckEquals(3, Length(service.CollectionItems));
  CheckIs(service.CollectionItems[0], TCollectionItemA);
  CheckIs(service.CollectionItems[1], TCollectionItemB);
  CheckIs(service.CollectionItems[2], TCollectionItemC);
end;

procedure TTestManyDependencies.TestInjectEnumerable;
var
  service: ICollectionService;
begin
  fContainer.RegisterType<ICollectionService, TCollectionServiceB>;
  fContainer.RegisterType<IInterface, TCollectionServiceB>;

  fContainer.Build;
  service := fContainer.Resolve<ICollectionService>;
  CheckEquals(3, Length(service.CollectionItems));
  CheckIs(service.CollectionItems[0], TCollectionItemA);
  CheckIs(service.CollectionItems[1], TCollectionItemB);
  CheckIs(service.CollectionItems[2], TCollectionItemC);
end;

procedure TTestManyDependencies.TestNoRecursion;
var
  service: ICollectionItem;
begin
  fContainer.RegisterType<ICollectionItem, TCollectionItemD>;
  fContainer.Build;
  service := fContainer.Resolve<ICollectionItem>;
  CheckIs(service, TCollectionItemD);
  CheckEquals(3, Length((service as TCollectionItemD).CollectionItems));
  CheckIs((service as TCollectionItemD).CollectionItems[0], TCollectionItemA);
  CheckIs((service as TCollectionItemD).CollectionItems[1], TCollectionItemB);
  CheckIs((service as TCollectionItemD).CollectionItems[2], TCollectionItemC);
end;

procedure TTestManyDependencies.TestNoRecursion_Issue18;
var
  service: ICollectionService;
begin
  fContainer.RegisterType<ICollectionItem, TCollectionItemD>;
  fContainer.RegisterType<ICollectionService, TCollectionServiceD>;
  fContainer.Build;
  service := fContainer.Resolve<ICollectionService>;
  Pass;
end;

procedure TTestManyDependencies.TestNoRecursion_TwoDifferentModelsWithSameComponentType;
var
  service: ICollectionItem;
begin
  fContainer.RegisterType<ICollectionItem, TCollectionItemD>('d')
  .DelegateTo(
    function: TCollectionItemD
    begin
      Result := TCollectionItemD.Create(nil);
    end);
  fContainer.RegisterType<ICollectionItem, TCollectionItemD>;
  fContainer.Build;
  service := fContainer.Resolve<ICollectionItem>;
  CheckEquals(4, Length((service as TCollectionItemD).CollectionItems));
end;

procedure TTestManyDependencies.TestRecursion_TwoDifferentModelsWithSameComponentType;
var
  service: ICollectionItem;
begin
  fContainer.RegisterType<ICollectionItem, TCollectionItemD>('d');
  fContainer.RegisterType<ICollectionItem, TCollectionItemD>;
  fContainer.Build;
  ExpectedException := ECircularDependencyException;
  service := fContainer.Resolve<ICollectionItem>;
  CheckEquals(4, Length((service as TCollectionItemD).CollectionItems));
end;

procedure TTestManyDependencies.TestResolveArrayOfLazy;
var
  services: TArray<Lazy<ICollectionItem>>;
  service: Lazy<ICollectionItem>;
begin
  fContainer.RegisterType<ICollectionItem, TCollectionItemD>;
  fContainer.Build;
  services := fContainer.ResolveAll<Lazy<ICollectionItem>>;
  CheckEquals(3, Length(services));
  CheckIs(services[0].Value, TCollectionItemA);
  CheckIs(services[1].Value, TCollectionItemB);
  CheckIs(services[2].Value, TCollectionItemC);
  service := fContainer.Resolve<Lazy<ICollectionItem>>;
  CheckIs(service.Value, TCollectionItemD);
end;

{$ENDREGION}


{$REGION 'TTestDecorators'}

procedure TTestDecorators.RegisterOneDecoratorInitializeIsCalled;
var
  service: IAnotherService;
begin
  fContainer.RegisterType<IAnotherService, TInitializableComponent>;
  fContainer.RegisterDecorator<IAnotherService, TAnotherServiceDecorator>;
  fContainer.Build;
  service := fContainer.Resolve<IAnotherService>;
  CheckIs(service, TAnotherServiceDecorator);
  service := (service as TAnotherServiceDecorator).Service;
  CheckIs(service, TInitializableComponent);
  Check((service as TInitializableComponent).IsInitialized);
end;

procedure TTestDecorators.RegisterOneDecoratorResolveReturnsDecorator;
var
  service: IAgeService;
begin
  fContainer.RegisterDecorator<IAgeService, TAgeServiceDecorator>;
  fContainer.Build;
  service := fContainer.Resolve<IAgeService>;
  CheckIs(service, TAgeServiceDecorator);
  CheckEquals(TNameAgeComponent.DefaultAge, service.Age);
end;

procedure TTestDecorators.RegisterOneDecoratorWithFailingConditionResolvesWithoutDecorator;
var
  service: IAgeService;
  conditionCalled: Boolean;
begin
  conditionCalled := False;
  fContainer.RegisterDecorator<IAgeService, TAgeServiceDecorator>(
    function(const m: TComponentModel): Boolean
    begin
      Result := False;
      conditionCalled := True;
    end);
  CheckFalse(conditionCalled);
  fContainer.Build;
  service := fContainer.Resolve<IAgeService>;
  CheckTrue(conditionCalled);
  CheckIs(service, TNameAgeComponent);
  CheckEquals(TNameAgeComponent.DefaultAge, service.Age);
end;

procedure TTestDecorators.RegisterTwoDecoratorsResolveArrayAllDecorated;
var
  services: TArray<IAgeService>;
begin
  fContainer.RegisterDecorator<IAgeService, TAgeServiceDecorator>;
  fContainer.RegisterDecorator<IAgeService, TAgeServiceDecorator2>;

  fContainer.RegisterType<IAgeService, TAgeService>('ageService');
  fContainer.Build;

  services := fContainer.Resolve<TArray<IAgeService>>;
  CheckEquals(2, Length(services));
  CheckIs(services[0], TAgeServiceDecorator2);
  CheckIs(services[1], TAgeServiceDecorator2);
end;

procedure TTestDecorators.RegisterTwoDecoratorsResolveLazyReturnsLastDecorator;
var
  service, service2: IAgeService;
begin
  fContainer.RegisterDecorator<IAgeService, TAgeServiceDecorator>;
  fContainer.RegisterDecorator<IAgeService, TAgeServiceDecorator2>;

  fContainer.RegisterType<IAgeService, TAgeService>('ageService');
  fContainer.Build;

  service := fContainer.Resolve<IAgeService>('ageService');

  CheckIs(service, TAgeServiceDecorator2);
  service2 := (service as TAgeServiceDecorator2).AgeService;
  CheckIs(service2, TAgeServiceDecorator);
  service2 := (service2 as TAgeServiceDecorator).AgeService;
  CheckIs(service2, TAgeService);
  service2 := (service2 as TAgeService).AgeService;
  CheckIs(service2, TAgeServiceDecorator2);
  service2 := (service2 as TAgeServiceDecorator2).AgeService;
  CheckIs(service2, TAgeServiceDecorator);
  service2 := (service2 as TAgeServiceDecorator).AgeService;
  CheckIs(service2, TNameAgeComponent);
end;

procedure TTestDecorators.RegisterTwoDecoratorsResolveReturnsLastDecorator;
var
  service: IAgeService;
begin
  fContainer.RegisterDecorator<IAgeService, TAgeServiceDecorator>;
  fContainer.RegisterDecorator<IAgeService, TAgeServiceDecorator2>;
  fContainer.Build;
  service := fContainer.Resolve<IAgeService>;
  CheckIs(service, TAgeServiceDecorator2);
  CheckEquals(TNameAgeComponent.DefaultAge, service.Age);
end;

procedure TTestDecorators.RegisterTwoDecoratorsResolveWithParameterReturnsCorrectValue;
var
  service: IAgeService;
begin
  fContainer.RegisterDecorator<IAgeService, TAgeServiceDecorator>;
  fContainer.RegisterDecorator<IAgeService, TAgeServiceDecorator2>;
  fContainer.Build;
  service := fContainer.Resolve<IAgeService>([42]);
  CheckIs(service, TAgeServiceDecorator2);
  CheckEquals(42, service.Age);
end;

procedure TTestDecorators.SetUp;
begin
  inherited;

  fContainer.RegisterType<TNameAgeComponent>;
  fContainer.Build;
end;

{$ENDREGION}


end.
