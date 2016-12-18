{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2016 Spring4D Team                           }
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

unit Spring.Tests.Interception;

{$I Spring.inc}
{$I Spring.Tests.inc}

interface

uses
  Generics.Defaults,
  SysUtils,
  TestFramework,
  Spring,
  Spring.Interception,
  Spring.Mocking,
  Spring.Tests.Container,
  Spring.Tests.Interception.Types;

type
  TFreezableTest = class(TTestCase)
  private
    function GetInterceptedMethodsCountFor<TInterceptor: class,
      IInterceptor, IHasCount>(freezable: TObject): Integer;
  published
    procedure IsFreezable_should_be_false_for_objects_created_with_ctor;
    procedure IsFreezable_should_be_true_for_objects_created_with_MakeFreezable;
    procedure Freezable_should_work_normally;
    procedure Frozen_object_should_throw_ObjectFrozenException_when_trying_to_set_a_property;
    procedure Frozen_object_should_not_throw_when_trying_to_read_it;
    procedure Freeze_nonFreezable_object_should_throw_NotFreezableObjectException;
    procedure Freezable_should_not_intercept_property_getters;
    procedure Freezable_should_not_intercept_normal_methods;
    procedure Freezable_should_intercept_property_setters;
    procedure DynProxyGetTarget_should_return_proxy_itself;
    procedure Freezable_should_log_getters_and_setters;
    procedure Freezable_should_not_intercept_methods;
//    procedure Freezable_should_not_hold_any_reference_to_created_objects;
    procedure Freezable_should_freeze_classes_with_nonVirtual_methods;
    procedure Freezable_should_throw_when_trying_to_freeze_classes_with_nonVirtual_setters;
  end;

  TProxyTest = class(TTestCase)
  private
    procedure UseSomewhereElse(const person: TPerson);
  published
    procedure Should_be_able_to_wrap_interface_with_one_method;
    procedure Should_be_able_to_write_interface_with_two_methods;

    procedure ClassProxy_should_implement_additional_interfaces;
    procedure ClassProxy_for_class_already_implementing_additional_interfaces;
    procedure ClassProxy_should_not_fail_on_safecall;
    procedure InterfaceProxy_should_implement_additional_interfaces;

    procedure InterfaceProxy_with_additional_interfaces_handles_refcount;

    procedure Mixin;

    procedure CustomProxyWithSafecallMethod;

    procedure CastingToImplementingObjectWorks;
  end;

  TStorageTests = class(TTestCase)
  private
    fPrimaryStorage: TPrimaryStorage;
    fSecondaryStorage: TSecondaryStorage;
    fSUT: TStorageFactory;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Save_should_use_primaryStorage_when_it_is_up;
    procedure Save_should_use_secondaryStorage_when_primaryStorage_is_down;
    procedure Save_should_go_back_to_primaryStorage_when_is_goes_from_down_to_up;
  end;

  TTestInterception = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestByAttributes;
    procedure TestByRegistrationOfClass;
    procedure TestByRegistrationOfInterface;
    procedure TestByRegistrationWithSelector;
  end;

  TTestAutoMockingExtension = class(TContainerTestCase)
  private
    function GetMock<T>: Mock<T>;
    function GetIMock<T>: IMock<T>;
  protected
    procedure SetUp; override;
  published
    procedure Test;
    procedure ResolveIMockFromAutoMockingContainer;
    procedure ResolveMockFromAutoMockingContainer;
    procedure ResolveIMockIndirectlyFromAutoMockingContainer;
    procedure ResolveMockIndirectlyFromAutoMockingContainer;
  end;

implementation

uses
  Spring.Collections,
  Spring.Container.AutoMockExtension,
  Spring.Container.Core,
  Spring.Interception.CustomProxy,
  Spring.TestUtils;


{$REGION 'TFreezableTest'}

procedure TFreezableTest.IsFreezable_should_be_false_for_objects_created_with_ctor;
var
  nonFreezablePet: TPet;
begin
  nonFreezablePet := TPet.Create;
  try
    CheckFalse(TFreezable.IsFreezable(nonFreezablePet));
  finally
    nonFreezablePet.Free;
  end;
end;

procedure TFreezableTest.IsFreezable_should_be_true_for_objects_created_with_MakeFreezable;
var
  freezablePet: TPet;
begin
  freezablePet := TFreezable.MakeFreezable<TPet>;
  try
    CheckTrue(TFreezable.IsFreezable(freezablePet));
  finally
    freezablePet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_work_normally;
var
  pet: TPet;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.Age := 3;
    pet.Deceased := True;
    pet.Name := 'Rex';
    pet.Age := pet.Age + Length(pet.Name);
    pet.ToString;
  finally
    pet.Free;
  end;
  Pass;
end;

procedure TFreezableTest.Frozen_object_should_throw_ObjectFrozenException_when_trying_to_set_a_property;
var
  pet: TPet;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.Age := 3;

    TFreezable.Freeze(pet);

    ExpectedException := EObjectFrozenException;
    pet.Name := 'This should throw';
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Frozen_object_should_not_throw_when_trying_to_read_it;
var
  pet: TPet;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.Age := 3;

    TFreezable.Freeze(pet);

    CheckEquals(3, pet.Age);
    CheckEquals('', pet.Name);
    CheckEquals(False, pet.Deceased);
    pet.ToString;
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freeze_nonFreezable_object_should_throw_NotFreezableObjectException;
var
  pet: TPet;
begin
  pet := TPet.Create;
  try
    ExpectedException := ENotFreezableObjectException;
    TFreezable.Freeze(pet);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_not_intercept_property_getters;
var
  pet: TPet;
  interceptedMethodsCount: Integer;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    TFreezable.Freeze(pet);
    CheckEquals(0, pet.Age);
    interceptedMethodsCount := GetInterceptedMethodsCountFor<TFreezableInterceptor>(pet);
    CheckEquals(0, interceptedMethodsCount);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_not_intercept_normal_methods;
var
  pet: TPet;
  notUsed: string;
  interceptedMethodsCount: Integer;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    notUsed := pet.ToString;
    interceptedMethodsCount := GetInterceptedMethodsCountFor<TFreezableInterceptor>(pet);
    CheckEquals(0, interceptedMethodsCount);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_intercept_property_setters;
var
  pet: TPet;
  interceptedMethodsCount: Integer;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.Age := 5;
    interceptedMethodsCount := GetInterceptedMethodsCountFor<TFreezableInterceptor>(pet);
    CheckEquals(1, interceptedMethodsCount);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.DynProxyGetTarget_should_return_proxy_itself;
var
  pet: TPet;
  hack: IProxyTargetAccessor;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.GetInterface(IProxyTargetAccessor, hack);
    CheckNotNull(hack);
    CheckSame(pet, hack.GetTarget.AsObject);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_log_getters_and_setters;
var
  pet: TPet;
  logsCount, freezeCount: Integer;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.Age := 4;
    CheckEquals(4, pet.Age);
    logsCount := GetInterceptedMethodsCountFor<TCallLoggingInterceptor>(pet);
    freezeCount := GetInterceptedMethodsCountFor<TFreezableInterceptor>(pet);
    CheckEquals(2, logsCount);
    CheckEquals(1, freezeCount);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_not_intercept_methods;
var
  pet: TPet;
  logsCount, freezeCount: Integer;
begin
  pet := TFreezable.MakeFreezable<TPet>;
  try
    pet.ToString;
    logsCount := GetInterceptedMethodsCountFor<TCallLoggingInterceptor>(pet);
    freezeCount := GetInterceptedMethodsCountFor<TFreezableInterceptor>(pet);

    // base implementation of ToString calls each property getter, that we intercept
    // so there will be 3 calls if method is not intercepter, otherwise 4.
    CheckEquals(3, logsCount);
    CheckEquals(0, freezeCount);
  finally
    pet.Free;
  end;
end;

procedure TFreezableTest.Freezable_should_freeze_classes_with_nonVirtual_methods;
var
  pet: TPetWithNonVirtualMethod;
begin
  pet := TFreezable.MakeFreezable<TPetWithNonVirtualMethod>;
  try
    pet.Name := 'Rex';
    pet.NonVirtualMethod;
  finally
    pet.Free;
  end;
  Pass;
end;

procedure TFreezableTest.Freezable_should_throw_when_trying_to_freeze_classes_with_nonVirtual_setters;
begin
  ExpectedException := EInvalidOperationException;
  try
    TFreezable.MakeFreezable<TPetWithNonVirtualSetter>;
  except
    on E: Exception do
    begin
      CheckEquals('Property NonVirtualProperty is not virtual. Cannot freeze classes with non-virtual properties.', E.Message);
      raise;
    end;
  end;
end;

function TFreezableTest.GetInterceptedMethodsCountFor<TInterceptor>(
  freezable: TObject): Integer;
var
  hack: IProxyTargetAccessor;
  loggingInterceptor: IHasCount;
begin
  Assert(TFreezable.IsFreezable(freezable));
  Supports(freezable, IProxyTargetAccessor, hack);
  Assert(Assigned(hack));
  loggingInterceptor := hack.GetInterceptors.Where(
    function(const i: IInterceptor): Boolean
    begin
      Result := i is TInterceptor;
    end).Single as IHasCount;
  Result := loggingInterceptor.Count;
end;

{$ENDREGION}


{$REGION 'TProxyTest'}

procedure TProxyTest.Should_be_able_to_wrap_interface_with_one_method;
var
  len: TFunc<string, Integer>;
  wrapped: IAnsweringEngine;
  i: Integer;
begin
  len :=
    function(s: string): Integer
    begin
      Result := Length(s);
    end;
  wrapped := TDelegateWrapper.WrapAs<IAnsweringEngine>(@len);
  CheckNotNull(wrapped);
  i := wrapped.GetAnswer('Answer to Life the Universe and Everything');
  CheckEquals(42, i);
end;

type
  {$M+}
  TEqualsFunc<T> = reference to function(const Left, Right: T): Boolean;
  TGetHashCodeFunc<T> = reference to function(const Value: T): Integer;
  {$M-}

procedure TProxyTest.Should_be_able_to_write_interface_with_two_methods;
var
  compare: TEqualsFunc<string>;
  getHashCode: TGetHashCodeFunc<string>;
  comparer: IEqualityComparer<string>;
  stringByLength: IDictionary<string, string>;
  atFive: string;
begin
  compare :=
    function(const s1, s2: string): Boolean
    begin
      Result := Length(s1) = Length(s2);
    end;
  getHashCode :=
    function(const s: string): Integer
    begin
      Result := Length(s);
    end;

  comparer := TDelegateWrapper.WrapAs<IEqualityComparer<string>>(
    [@compare, @getHashCode]);
  stringByLength := TCollections.CreateDictionary<string, string>(
    Generics.Defaults.IEqualityComparer<string>(comparer));
  stringByLength.Add('four', 'some string');
  stringByLength.Add('five!', 'some other string');
  CheckEquals(2, stringByLength.Count);
  atFive := stringByLength['12345'];
  CheckEquals('some other string', atFive);
end;

type
  TInvalidationInterceptor = class(TInterfacedObject, IInterceptor)
    procedure Intercept(const invocation: IInvocation);
  end;

procedure TInvalidationInterceptor.Intercept(const invocation: IInvocation);
begin
end;

procedure TProxyTest.ClassProxy_should_implement_additional_interfaces;
var
  proxy: TObject;
begin
  proxy := TProxyGenerator.CreateClassProxy(
    TEnsurePartnerStatusRule,
    [TypeInfo(ISupportsInvalidation)],
    [TInvalidationInterceptor.Create]);
  try
    CheckTrue(Supports(proxy, ISupportsInvalidation));
  finally
    proxy.Free;
  end;
end;

procedure TProxyTest.ClassProxy_should_not_fail_on_safecall;
var
  proxy: TObject;
begin
  // Since safecall does not have Extended info generated for the method, it
  // may fail with EInsufficientRtti in case internal checks are in wrong order.
  proxy := TProxyGenerator.CreateClassProxy<TSafeCallObject>([]);
  proxy.Free;
  Pass;
end;

type
  TMyMock = class(TCustomProxy)
  public
    procedure SomeProcedure; virtual; safecall;
    function SafeCallProcedure(const s: string): Integer; virtual; safecall;
  end;

procedure TMyMock.SomeProcedure;
begin
  Intercept;
end;

function TMyMock.SafeCallProcedure(const s: string): Integer;
begin
  Result := Intercept([s]).AsInteger;
end;

procedure TProxyTest.CustomProxyWithSafecallMethod;
var
  mock: Mock<TMyMock>;
begin
  mock := Mock<TMyMock>.Create;
  mock.Setup.Returns(42).When.SafeCallProcedure('test');
  CheckEquals(42, mock.Instance.SafeCallProcedure('test'));
  mock.Received(Times.Once).SafeCallProcedure('test');

  mock := Mock<TMyMock>.Create;
  mock.Setup.Executes.When.SomeProcedure;
  mock.Instance.SomeProcedure;
  mock.Received(Times.Once).SomeProcedure;

  Pass;
end;

procedure TProxyTest.CastingToImplementingObjectWorks;
var
  storage: IStorage;
begin
  storage := TPrimaryStorage.Create;
  storage := TProxyGenerator.CreateInterfaceProxyWithTarget(storage, []);
  (storage as TPrimaryStorage).Save('');
  Pass;
end;

procedure TProxyTest.ClassProxy_for_class_already_implementing_additional_interfaces;
var
  proxy: TObject;
  intf: ISupportsInvalidation;
begin
  proxy := TProxyGenerator.CreateClassProxy(
    TApplyDiscountRule,
    [TypeInfo(ISupportsInvalidation)], []);
  CheckTrue(Supports(proxy, ISupportsInvalidation, intf));
  ExpectedException := ENotImplementedException;
  intf.Invalidate;
end;

procedure TProxyTest.InterfaceProxy_should_implement_additional_interfaces;
var
  proxy: TObject;
  intf: ISupportsInvalidation;
begin
  proxy := TProxyGenerator.CreateInterfaceProxyWithTarget(
    TypeInfo(IClientRule),
    [TypeInfo(ISupportsInvalidation)],
    TApplyDiscountRule.Create, []);
  CheckTrue(Supports(proxy, ISupportsInvalidation, intf));
  intf.Invalidate;
end;

procedure TProxyTest.InterfaceProxy_with_additional_interfaces_handles_refcount;
var
  proxy: TObject;
  clientRule: IClientRule;
  invalidation: ISupportsInvalidation;
begin
  proxy := TProxyGenerator.CreateInterfaceProxyWithoutTarget(
    TypeInfo(IClientRule), [TypeInfo(ISupportsInvalidation)], []);
  CheckTrue(Supports(proxy, IClientRule, clientRule));
  CheckTrue(Supports(clientRule, ISupportsInvalidation, invalidation));
  CheckTrue(Supports(invalidation, IClientRule, clientRule));
  clientrule := nil;
  invalidation := nil;
end;

procedure TProxyTest.UseSomewhereElse(const person: TPerson);
var
  dictionary: IDictionary<string, TDateTime>;
  date: TDateTime;
begin
  Supports(person, IDictionary<string, TDateTime>, dictionary);
  date := dictionary['Next Leave'];
  Status(Format('Next leave date is %s', [DateToStr(date)]));
end;

procedure TProxyTest.Mixin;
var
  options: TProxyGenerationOptions;
  person: TPerson;
  dictionary: IDictionary<string, TDateTime>;
begin
  options := TProxyGenerationOptions.Default;
  options.AddMixinInstance(TCollections.CreateDictionary<string, TDateTime> as TObject);
  person := TProxyGenerator.CreateClassProxy(TPerson, options, []) as TPerson;
  try
    CheckTrue(Supports(person, IDictionary<string, TDateTime>, dictionary));
    dictionary.Add('Next Leave', IncMonth(Now, 4));
    UseSomewhereElse(person);
  finally
    person.Free;
  end;
end;

{$ENDREGION}


{$REGION 'TStorageTests'}

procedure TStorageTests.SetUp;
begin
  fPrimaryStorage := TPrimaryStorage.Create;
  fPrimaryStorage.IsUp := True;
  fSecondaryStorage := TSecondaryStorage.Create;
  fSUT := TStorageFactory.Create(fPrimaryStorage);
  fSUT.SecondaryStorage := fSecondaryStorage;
end;

procedure TStorageTests.TearDown;
begin
  fSUT.Free;
  fPrimaryStorage := nil;
  fSecondaryStorage := nil;
end;

procedure TStorageTests.Save_should_use_primaryStorage_when_it_is_up;
var
  storage: IStorage;
  msg: string;
begin
  storage := fSUT.GetStorage;
  msg := 'message';
  storage.Save(msg);

  CheckFalse(fSecondaryStorage.Items.Any);
  CheckTrue(fPrimaryStorage.Items.Any);
  CheckEquals(msg, fPrimaryStorage.Items.First);
end;

procedure TStorageTests.Save_should_use_secondaryStorage_when_primaryStorage_is_down;
var
  storage: IStorage;
  msg: string;
begin
  fPrimaryStorage.IsUp := False;
  storage := fSUT.GetStorage;
  msg := 'message';
  storage.Save(msg);

  CheckFalse(fPrimaryStorage.Items.Any);
  CheckTrue(fSecondaryStorage.Items.Any);
  CheckEquals(msg, fSecondaryStorage.Items.First);
end;

procedure TStorageTests.Save_should_go_back_to_primaryStorage_when_is_goes_from_down_to_up;
var
  storage: IStorage;
  msg1, msg2, msg3: string;
  primary, secondary: IList<string>;
begin
  storage := fSUT.GetStorage;
  msg1 := 'message1';
  msg2 := 'message2';
  msg3 := 'message3';

  storage.Save(msg1);
  fPrimaryStorage.IsUp := False;
  storage.Save(msg2);
  fPrimaryStorage.IsUp := True;
  storage.Save(msg3);
  primary := fPrimaryStorage.Items;
  secondary := fSecondaryStorage.Items;

  CheckEquals(2, primary.Count);
  CheckEquals(1, secondary.Count);
  CheckTrue(primary.Contains(msg1));
  CheckTrue(primary.Contains(msg3));
  CheckTrue(secondary.Contains(msg2));
end;

{$ENDREGION}


{$REGION 'TTestInterception'}

procedure TTestInterception.SetUp;
begin
  inherited;
  fContainer.RegisterType<TExceptionAspect>
    .Implements<TExceptionAspect>
    .Implements<IInterceptor>('except')
    .InjectField('fEatAll', True);
end;

procedure TTestInterception.TestByAttributes;
var
  service: IService;
begin
  fContainer.RegisterType<TServiceWithAttributes>;
  fContainer.Build;

  service := fContainer.Resolve<IService>;
  service.DoSomething;
  FCheckCalled := True;
end;

procedure TTestInterception.TestByRegistrationOfClass;
var
  service: IService;
begin
  fContainer.RegisterType<TService>.InterceptedBy('except');
  fContainer.Build;

  service := fContainer.Resolve<IService>;
  service.DoSomething;
  FCheckCalled := True;
end;

procedure TTestInterception.TestByRegistrationOfInterface;
var
  service: IService;
begin
  fContainer.RegisterType<IService>.DelegateTo(
    function: IService
    begin
      Result := TService.Create;
    end).InterceptedBy('except');
  fContainer.Build;

  service := fContainer.Resolve<IService>;
  service.DoSomething;
  FCheckCalled := True;
end;

procedure TTestInterception.TestByRegistrationWithSelector;
var
  service: IService;
begin
  fContainer.Kernel.ProxyFactory.AddInterceptorSelector(
    TMyInterceptorSelector.Create as IModelInterceptorsSelector);
  fContainer.RegisterType<TService>;
  fContainer.Build;

  service := fContainer.Resolve<IService>;
  service.DoSomething;
  FCheckCalled := True;
end;

{$ENDREGION}


{$REGION 'TTestMocks'}

function TTestAutoMockingExtension.GetIMock<T>: IMock<T>;
begin
  Result := fContainer.Resolve<IMock<T>>;
end;

function TTestAutoMockingExtension.GetMock<T>: Mock<T>;
begin
  Result := fContainer.Resolve<Mock<T>>;
end;

procedure TTestAutoMockingExtension.SetUp;
begin
  inherited;
  fContainer.AddExtension<TAutoMockExtension>;
  fContainer.Build;
end;

procedure TTestAutoMockingExtension.Test;
var
  sut: TBasketController;
begin
  fContainer.RegisterType<TBasketController>;
  fContainer.Build;

  sut := fContainer.Resolve<TBasketController>;
  try
    sut.Post(nil);
    fContainer.Resolve<IMock<ICommandChannel>>.Received(1).Send(nil);
  finally
    sut.Free;
  end;
  Pass;
end;

procedure TTestAutoMockingExtension.ResolveIMockFromAutoMockingContainer;
begin
  fContainer.Resolve<IMock<ICommandChannel>>;
  Pass;
end;

procedure TTestAutoMockingExtension.ResolveIMockIndirectlyFromAutoMockingContainer;
begin
  GetIMock<ICommandChannel<TObject>>;
  Pass;
end;

procedure TTestAutoMockingExtension.ResolveMockFromAutoMockingContainer;
begin
  fContainer.Resolve<Mock<ICommandChannel>>;
  Pass;
end;

procedure TTestAutoMockingExtension.ResolveMockIndirectlyFromAutoMockingContainer;
begin
  GetMock<ICommandChannel<TObject>>;
  Pass;
end;

{$ENDREGION}


end.
