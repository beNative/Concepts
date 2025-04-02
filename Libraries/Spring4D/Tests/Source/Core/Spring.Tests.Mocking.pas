{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2024 Spring4D Team                           }
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

unit Spring.Tests.Mocking;

{$I Spring.Tests.inc}

interface

uses
  TestFramework,
  Spring.TestUtils;

type
  TParameterMatchingTests = class(TTestCase)
  private
    procedure Notify(Sender: TObject);
  published
    procedure ArgsEvaluationOrder;
    procedure ArgsStackProperlyCleaned;

    procedure OutParameterCanBeSet;
    procedure OutParameterCanBePassed;
    procedure OutParameterManagedType;
    procedure VerifyChecksParameterValuesProperly;
    procedure TestVariant;
    procedure TestDynArray;
    procedure TestRecord;
    procedure TestRecord2;
    procedure TestDynArrayOfRec2;
    procedure TestStaticArrayOfRec2;
    procedure TestRegex;
    procedure TestEnum;
    procedure TestSet;
    procedure TestClass;
    {$IF not defined(WIN64) or defined(DELPHIXE4_UP)}
    procedure TestMethod;
    {$IFEND}
    {$IFDEF DELPHIX_SYDNEY_UP}
    procedure TestMRecord;
    {$ENDIF}

    procedure ReturnsMultipleValues;
    procedure WrapperObjectsNotLeaking;
    procedure ResetClearsProperly;
  end;

  ReceivedChecksForInputValueOfVarParams = class(TTestCase)
  published
    procedure WhenParamTypeIsInteger;
    procedure WhenParamTypeIsString;
    procedure WhenParamTypeIsVariant;
  end;

  MockReturnsOtherMockInDynamicMode = class(TTestCase)
  published
    procedure WhenNoExpectationWasDefined;
    procedure NotWhenExpectationWasDefined;

    procedure DependingOnInputArguments;
    procedure EventTypeIsHandled;
  end;

  MockDynamicallySupportsOtherInterfaces = class(TTestCase)
  published
    procedure ResultOfAsFunctionSurvivesScope;
    procedure SetupAsResultOfFunction;
    procedure WhenAsFunctionIsCalled;
    procedure MockCastToInterface;
  end;

  MockSequenceTest = class(TTestCase)
  published
    procedure AssertsWrongOrder;
    procedure SequenceWorksUsingWith;
    procedure ResetClearsSequence;
    procedure SequenceAllowsMultipleExpectedCalls;
  end;

implementation

uses
  Classes,
  SysUtils,
  Spring,
  Spring.Mocking;

type
  TTestEnum = (One, Two, Three);
  ShortEnum = 0..31;
  TTestSet = set of {$IFNDEF DELPHIX_RIO_UP}ShortEnum{$ELSE}Byte{$ENDIF}; // see RSP-16153
  {$IFDEF DELPHIX_SYDNEY_UP}
  TTestMRec = record
    value: Integer;
    class operator Initialize(out value: TTestMRec);
  end;
  {$M+}
  IMockTestMRec = interface
    procedure Test(const rec: TTestMRec);
  end;
  {$ENDIF}

  {$M+}
  IMockTest = interface
    procedure Test1(i: Integer; const s: string);
    procedure Test2(const s: string; i: Integer; b: Boolean);
    procedure Test3(const s1: string; o: TObject; const s2: string);
    procedure Test4(const s1: string; o: ITest; const s2: string);
    procedure Test5(const s1: string; var x: Integer; o: ITest; const s2: string);
    procedure TestVariant(const v: Variant);
    procedure TestDynArray(const v: TArray<string>);
    procedure TestEnum(const value: TTestEnum);
    procedure TestSet(const n: Integer; const value: TTestSet; const i: Integer = 0);
    procedure TestObject(const obj: TObject);
    procedure TestClass(const cls: TClass);
    procedure TestMethod(const event: TNotifyEvent);
    function GetNext: Integer;

    function GetEvent: IInvokableNotifyEvent<Integer>;
  end;

  IVarParamTest = interface
    procedure TestInteger(var i: Integer);
    procedure TestString(var s: string);
    procedure TestVariant(var v: Variant);
  end;

  IChild = interface
    ['{8B6803C9-CF42-45FC-AA96-8F558FE32F8B}']
    function GetNumber: Integer;
  end;

  IParent = interface
    ['{CAEAF1BD-3145-4CF5-A1E4-CAA137B0AF3E}']
    function GetChild: IChild;
  end;

  INameHolder = interface
    function GetName: string;
    property Name: string read GetName;
  end;

  IFoo = interface
    function Foo(index: Integer): INameHolder;
  end;

  TRec = record
    Int: Integer;
    Str: string;
  end;

  TRec2 = record
    obj: TObject;
  end;

  TRec2Array = array[0..1] of TRec2;

  TFoo = class
  public
    function Method(const rec: TRec): Integer; virtual; abstract;
    function Method2(const rec: TRec2): Integer; virtual; abstract;
    function DynArrayOfRec2(const rec: TArray<TRec2>): Integer; virtual; abstract;
    function StaticArrayOfRec2(const rec: TRec2Array): Integer; virtual; abstract;
  end;

  IObserver = interface(IInvokable)
  end;

  ISubject = interface(IInvokable)
    procedure Attach(const observer: IObserver);
    procedure Detach(const observer: IObserver);
  end;

  TService = class(TRefCountedObject, IObserver)
  private
    fSubject: ISubject;
  public
    constructor Create(const subject: ISubject);
    destructor Destroy; override;
  end;

constructor TService.Create(const subject: ISubject);
begin
  fSubject := subject;
  fSubject.Attach(Self);
end;

destructor TService.Destroy;
begin
  if Assigned(fSubject) then
    fSubject.Detach(Self);
  inherited;
end;

{$REGION 'TTestMRec'}

{$IFDEF DELPHIX_SYDNEY_UP}
class operator TTestMRec.Initialize(out value: TTestMRec);
begin
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TParameterMatchingTests'}

procedure TParameterMatchingTests.ArgsEvaluationOrder;
var
  mock: Mock<IMockTest>;
  sut: IMockTest;
  dummy: Integer;
begin
  mock := Mock<IMockTest>.Create(TMockBehavior.Strict);
  sut := mock.Instance;

  with mock.Setup.Executes do
  begin
    When.Test1(Arg.IsAny<Integer>, Arg.IsEqual('test'));
    sut.Test1(10, 'test');
    sut.Test1(-10, 'test');

    When.Test1(Arg.IsAny<Integer>, Arg.IsAny<string>);
    sut.Test1(0, '');

    When.Test1(Arg.IsEqual(10), Arg.IsEqual('test'));
    sut.Test1(10, 'test');

    When.Test2(Arg.IsAny<string>, Arg.IsEqual(10), Arg.IsEqual(True));
    sut.Test2('', 10, True);

    When.Test3(Arg.IsAny<string>, Arg.IsEqual(Self), Arg.IsAny<string>);
    sut.Test3('', Self, '');

    When.Test4(Arg.IsAny<string>, Arg.IsEqual<ITest>(Self), Arg.IsAny<string>);
    sut.Test4('', Self, '');

    When.Test5(Arg.IsAny<string>, dummy, Arg.IsEqual<ITest>(Self), Arg.IsAny<string>);
    sut.Test5('', dummy, Self, '');
  end;

  mock.Reset;
  with mock.Setup.Executes do
  begin
    When.Test1(Arg.IsInRange(3, 6), Arg.IsAny<string>);
    sut.Test1(4, '');
    CheckException(EMockException,
      procedure
      begin
        sut.Test1(2, '');
      end);
  end;

  mock.Reset;
  with mock.Setup.Executes do
  begin
    When.Test1(Arg.IsIn([3, 4, 5, 6]), Arg.IsAny<string>);
    sut.Test1(4, '');
    CheckException(EMockException,
      procedure
      begin
        sut.Test1(2, '');
      end);
  end;

  Pass;
end;

type
  IOutParamTest = interface(IInvokable)
    procedure Test(out value: Integer);
    procedure TestIntf(out value: IInterface; i: Integer);
    function TestStr(out value: string): Integer;
  end;

procedure TParameterMatchingTests.ArgsStackProperlyCleaned;
var
  mock: Mock<IMockTest>;
begin
  mock := Mock<IMockTest>.Create(TMockBehavior.Strict);
  mock.Received(Times.Never).Test1(Arg.IsAny<Integer>, Arg.IsAny<string>);

  mock.Setup.Executes.When.Test2(Arg.IsAny<string>, Arg.IsAny<Integer>, Arg.IsAny<Boolean>);
  mock.Instance.Test2('', 0, False);
  mock.Received(Times.Once).Test2(Arg.IsAny<string>, Arg.IsAny<Integer>, Arg.IsAny<Boolean>);

  mock.Setup.Executes.When(Args.Any).Test1(Arg.IsAny<Integer>, Arg.IsAny<string>);
  mock.Setup.Executes.When.Test1(Arg.IsAny<Integer>, Arg.IsAny<string>);

  Pass;
end;

procedure TParameterMatchingTests.Notify(Sender: TObject);
begin
end;

procedure TParameterMatchingTests.OutParameterCanBePassed;
{$IFDEF DELPHIXE8_UP}
var
  mock: Mock<IOutParamTest>;
  i: Integer;
  intfIn, intfOut: IInterface;
  s: string;
begin
  mock.Setup.Executes.When.Test(Arg.Ref<Integer>(42).Return);
  i := 0;
  mock.Instance.Test(i);
  CheckEquals(42, i);

  intfIn := TInterfacedObject.Create;
  mock.Setup.Executes.When.TestIntf(Arg.Ref(intfIn).Return, Arg = 0);
  mock.Instance.TestIntf(intfOut, 0);
  CheckSame(intfIn, intfOut);

  mock.Setup.Returns([1, 2]).When.TestStr(Arg.Ref('12').Return);
  CheckEquals(1, mock.Instance.TestStr(s));
  CheckEqualsString('12', s);
  CheckEquals(2, mock.Instance.TestStr(s));
  CheckEqualsString('12', s);
{$ELSE}
begin
  Pass;
{$ENDIF}
end;

procedure TParameterMatchingTests.OutParameterCanBeSet;
var
  mock: Mock<IOutParamTest>;
  i: Integer;
begin
  mock.Setup.Executes(
    function(const call: TCallInfo): TValue
    begin
      CheckEquals(42, call[0].AsInteger);
      call[0] := 43;
    end).When(Args.Any).Test(i);
  i := 42;
  mock.Instance.Test(i);
  CheckEquals(43, i);
end;

procedure TParameterMatchingTests.OutParameterManagedType;
var
  mock: Mock<IOutParamTest>;
  intf: IInterface;
begin
  mock.Setup.Executes.When.TestIntf(intf, Arg.IsAny<Integer>);
  Pass;
end;

procedure TParameterMatchingTests.ResetClearsProperly;
var
  subject: Mock<ISubject>;
  observer: IObserver;
begin
  subject.Behavior := TMockbehavior.Strict;
  with subject.Setup do
  begin
    Executes.When(Args.Any).Attach(nil);
    Executes.When(Args.Any).Detach(nil);
  end;
  observer := TService.Create(subject);
  observer := nil;
  subject.Free;
  Pass;
end;

procedure TParameterMatchingTests.ReturnsMultipleValues;
var
  mock: Mock<IChild>;
begin
  mock.Setup.Returns([1,2,3]).When.GetNumber;
  CheckEquals(1, mock.Instance.GetNumber);
  CheckEquals(2, mock.Instance.GetNumber);
  CheckEquals(3, mock.Instance.GetNumber);
  CheckEquals(0, mock.Instance.GetNumber);
  mock.Received(4).GetNumber;
  mock.Behavior := TMockBehavior.Strict;
  ExpectedException := EMockException;
  mock.Instance.GetNumber;
end;

procedure TParameterMatchingTests.TestClass;
var
  mock: Mock<IMockTest>;
begin
  mock.Setup.Executes.When.TestClass(TObject);
  mock.Instance.TestClass(TObject);
  mock.Received.TestClass(TObject);
  mock.Setup.Executes.When.TestClass(Arg.IsAny<TClass>);
  mock.Instance.TestClass(nil);
  mock.Received.TestClass(nil);
  Pass;
end;

procedure TParameterMatchingTests.TestDynArray;
var
  mock: Mock<IMockTest>;
  arr: TArray<string>;
begin
  arr := TArray<string>.Create('test');
  mock.Setup.Executes.When.TestDynArray(Arg.IsAny<TArray<string>>);
  mock.Instance.TestDynArray(nil);
  mock.Instance.TestDynArray(arr);
  mock.Received(2).TestDynArray(Arg.IsAny<TArray<string>>);
  mock.Received(1).TestDynArray(nil);
  mock.Received(1).TestDynArray(arr);
  Pass;
end;

procedure TParameterMatchingTests.TestDynArrayOfRec2;
var
  mock: Mock<TFoo>;
  rec: TRec2;
begin
  rec.Obj := nil;
  mock.Setup.Returns(42).When.DynArrayOfRec2(Arg.IsAny<TArray<TRec2>>);
  CheckEquals(42, mock.Instance.DynArrayOfRec2(TArray<TRec2>.Create(rec)));
  mock.Received(1).DynArrayOfRec2(Arg.IsAny<TArray<TRec2>>);
  Pass;
end;

procedure TParameterMatchingTests.TestEnum;
var
  mock: Mock<IMockTest>;
begin
  mock.Setup.Executes.When.TestEnum(Arg.IsAny<TTestEnum>);
  mock.Instance.TestEnum(One);
  mock.Received(1).TestEnum(One);
  mock.Received(0).TestEnum(Two);
  Pass;
end;

{$IF not defined(WIN64) or defined(DELPHIXE4_UP)}
procedure TParameterMatchingTests.TestMethod;
var
  mock: Mock<IMockTest>;
begin
  mock.Setup.Executes.When.TestMethod(Notify);
  mock.Instance.TestMethod(Notify);
  mock.Received.TestMethod(Notify);
  mock.Setup.Executes.When.TestMethod(Arg.IsAny<TNotifyEvent>());
  mock.Instance.TestMethod(nil);
  mock.Received.TestMethod(nil);
  Pass;
end;
{$IFEND}

{$IFDEF DELPHIX_SYDNEY_UP}
procedure TParameterMatchingTests.TestMRecord;
var
  mock: Mock<IMockTestMRec>;
  rec: TTestMRec;
begin
  mock.Setup.Executes.When.Test(rec);
  mock.Instance.Test(rec);
  mock.Received.Test(rec);
  mock.Setup.Executes.When.Test(Arg.IsAny<TTestMRec>);
  mock.Instance.Test(rec);
  mock.Received(2).Test(Arg.IsAny<TTestMRec>);
  Pass;
end;
{$ENDIF}

procedure TParameterMatchingTests.TestRecord;
var
  mock: Mock<TFoo>;
  rec: TRec;
begin
  rec.Int := -42;
  rec.Str := 'test';
  mock.Setup.Returns(42).When.Method(Arg.IsAny<TRec>);
  CheckEquals(42, mock.Instance.Method(rec));
  mock.Received(1).Method(Arg.IsAny<TRec>);
  Pass;
end;

procedure TParameterMatchingTests.TestRecord2;
var
  mock: Mock<TFoo>;
  rec: TRec2;
begin
  rec.Obj := nil;
  mock.Setup.Returns(42).When.Method2(Arg.IsAny<TRec2>);
  CheckEquals(42, mock.Instance.Method2(rec));
  mock.Received(1).Method2(Arg.IsAny<TRec2>);
  Pass;
end;

procedure TParameterMatchingTests.TestRegex;
var
  mock: Mock<IMockTest>;
begin
  mock.Instance.Test1(0, 'foo');
  mock.Instance.Test1(0, 'bar');
  mock.Instance.Test1(0, 'baz');
  mock.Received(2).Test1(Arg.IsAny<Integer>, Arg.IsRegex('(foo|bar)'));
  Pass;
end;

procedure TParameterMatchingTests.TestSet;
var
  mock: Mock<IMockTest>;
begin
  mock.Setup.Executes.When.TestSet(Arg.IsAny<Integer>, Arg.IsAny<TTestSet>, Arg.IsAny<Integer>);
  mock.Instance.TestSet(0, [1]);
  mock.Received(1).TestSet(0, [1]);
  mock.Received(0).TestSet(0, [2,3]);
  Pass;
end;

procedure TParameterMatchingTests.TestStaticArrayOfRec2;
var
  mock: Mock<TFoo>;
  arr: TRec2Array;
begin
  arr[0].Obj := nil;
  arr[1].Obj := nil;
  mock.Setup.Returns(42).When.StaticArrayOfRec2(Arg.IsAny<TRec2Array>);
  CheckEquals(42, mock.Instance.StaticArrayOfRec2(arr));
  mock.Received(1).StaticArrayOfRec2(Arg.IsAny<TRec2Array>);
  Pass;
end;

procedure TParameterMatchingTests.TestVariant;
var
  mock: Mock<IMockTest>;
begin
  mock.Instance.TestVariant(42);
  mock.Received.TestVariant(42);
  mock.Received.TestVariant(Arg.IsEqual<Integer>(42));
  Pass;
end;

procedure TParameterMatchingTests.VerifyChecksParameterValuesProperly;
var
  mock: Mock<IMockTest>;
  sut: IMockTest;
begin
  sut := mock;
  sut.Test1(4, 'test');
  mock.Received(Times.Once).Test1(Arg.IsAny<Integer>, Arg.IsEqual('test'));
  CheckException(EMockException,
    procedure
    begin
      mock.Received.Test1(Arg.IsIn<Integer>([3, 5]), Arg.IsAny<string>);
    end);

  CheckException(EConvertError,
    procedure
    begin
      mock.Setup.Returns<string>('foobar').When.GetNext;
    end);
end;

procedure TParameterMatchingTests.WrapperObjectsNotLeaking;
var
  mock: Mock<IMockTest>;
begin
  mock.Setup.Executes.When(Args.Any).TestObject(Arg.IsAny<TObject>);
  mock.Instance.TestObject(nil);
  mock.Received.TestObject(Arg = nil);
  Pass;
end;

{$ENDREGION}


{$REGION 'ReceivedChecksForInputValueOfVarParams'}

procedure ReceivedChecksForInputValueOfVarParams.WhenParamTypeIsInteger;
var
  m: Mock<IVarParamTest>;
  i: Integer;
begin
  i := 1;
  m.Behavior := TMockBehavior.Strict;
  m.Setup.Executes(
    function(const info: TCallInfo): TValue
    begin
      info[0] := 2;
    end).When.TestInteger(i);
  m.Instance.TestInteger(i);
  i := 1;
  m.Received.TestInteger(i);
  FCheckCalled := True;
end;

procedure ReceivedChecksForInputValueOfVarParams.WhenParamTypeIsString;
var
  m: Mock<IVarParamTest>;
  s: string;
begin
  s := '1';
  m.Behavior := TMockBehavior.Strict;
  m.Setup.Executes(
    function(const info: TCallInfo): TValue
    begin
      info[0] := '2';
    end).When.TestString(s);
  m.Instance.TestString(s);
  s := '1';
  m.Received.TestString(s);
  FCheckCalled := True;
end;

procedure ReceivedChecksForInputValueOfVarParams.WhenParamTypeIsVariant;
var
  m: Mock<IVarParamTest>;
  v: Variant;
begin
  v := '1';
  m.Behavior := TMockBehavior.Strict;
  m.Setup.Executes(
    function(const info: TCallInfo): TValue
    begin
      info[0] := '2';
    end).When.TestVariant(v);
  m.Instance.TestVariant(v);
  v := '1';
  m.Received.TestVariant(v);
  FCheckCalled := True;
end;

{$ENDREGION}


{$REGION 'MockReturnsOtherMockInDynamicMode'}

procedure MockReturnsOtherMockInDynamicMode.DependingOnInputArguments;
var
  foo: IFoo;
begin
  foo := Mock<IFoo>.Create;
  Mock.From(foo.Foo(1)).Setup.Returns('One').When.Name;
  Mock.From(foo.Foo(2)).Setup.Returns('Two').When.Name;
  CheckEquals('One', foo.Foo(1).Name);
  CheckEquals('Two', foo.Foo(2).Name);
end;

procedure MockReturnsOtherMockInDynamicMode.EventTypeIsHandled;
var
  mock: Mock<IMockTest>;
  event: IInvokableNotifyEvent<Integer>;
begin
  event := mock.Instance.GetEvent;
  CheckNotNull(event);
  event.Add(nil);
  event.Remove(nil);
  CheckFalse(event.CanInvoke);
  CheckException(ENotSupportedException,
    procedure
    begin
      event.Invoke(nil, 0);
    end);
end;

procedure MockReturnsOtherMockInDynamicMode.NotWhenExpectationWasDefined;
var
  parentMock: Mock<IParent>;
  child: IChild;
begin
  parentMock.Setup.Returns(nil).When.GetChild;
  child := parentMock.Instance.GetChild;
  CheckNull(child);
  ExpectedException := EMockException;
  Mock.From(child);
end;

procedure MockReturnsOtherMockInDynamicMode.WhenNoExpectationWasDefined;
var
  parentMock: Mock<IParent>;
  child: IChild;
  childMock: Mock<IChild>;
begin
  child := parentMock.Instance.GetChild;
  CheckNotNull(child);
  CheckEquals(0, child.GetNumber);
  childMock := Mock.From(child);
  childMock.Setup.Returns(42).When.GetNumber;
  CheckEquals(42, child.GetNumber);
  childMock.Received(2).GetNumber;

  child := parentMock.Instance.GetChild;
  CheckEquals(42, child.GetNumber);
  childMock.Received(3).GetNumber;
end;

{$ENDREGION}


{$REGION 'MockDynamicallySupportsOtherInterfaces'}

procedure MockDynamicallySupportsOtherInterfaces.ResultOfAsFunctionSurvivesScope;

  procedure SpecifyExpectation(const mock: Mock<IParent>); overload;
  begin
    mock.AsType<IChild>.Setup.Returns(42).When.GetNumber;
    CheckEquals(42, mock.AsType<IChild>.Instance.GetNumber);
    mock.AsType<IChild>.AsType<IParent>;
  end;

var
  mock: Mock<IParent>;
begin
  SpecifyExpectation(mock);
  CheckEquals(42, mock.AsType<IChild>.Instance.GetNumber);
end;

procedure MockDynamicallySupportsOtherInterfaces.MockCastToInterface;
var
  mock: Mock<IParent>;
  childMock: Mock<IChild>;
begin
  mock.Setup.Returns(mock.AsType<IChild>).When.GetChild;
  CheckSame(mock.Instance as IChild, mock.Instance.GetChild);

  childMock := mock.AsType<IChild>;
  mock.Setup.Returns<Mock<IChild>>([childMock, childMock]).When.GetChild;
  CheckSame(mock.Instance as IChild, mock.Instance.GetChild);
  CheckSame(mock.Instance as IChild, mock.Instance.GetChild);

  mock.Reset;
end;

procedure MockDynamicallySupportsOtherInterfaces.SetupAsResultOfFunction;
var
  parentMock: Mock<IParent>;
  childMock: Mock<IChild>;
begin
  childMock := parentMock.AsType<IChild>;
  parentMock.Setup.Returns(parentMock.AsType<IChild>.Instance).When.GetChild;

  CheckSame(parentMock.Instance as IChild, parentMock.Instance.GetChild);
  CheckSame(parentMock.Instance, parentMock.Instance.GetChild as IParent);

  parentMock.Reset;
end;

procedure MockDynamicallySupportsOtherInterfaces.WhenAsFunctionIsCalled;
var
  parentMock: Mock<IParent>;
begin
  parentMock.AsType<IChild>.Setup.Returns(42).When.GetNumber;
  CheckEquals(42, (parentMock.Instance as IChild).GetNumber);
end;

{$ENDREGION}


{$REGION 'MockSequenceTest'}

procedure MockSequenceTest.AssertsWrongOrder;
var
  seq: MockSequence;
  mock: Mock<IMockTest>;
  mock2: Mock<IChild>;
begin
  CheckFalse(seq.Completed);
  mock.Behavior := TMockBehavior.Strict;
  mock2.Behavior := TMockBehavior.Strict;
  mock.Setup(seq).Executes.When.Test1(0, '');
  mock2.Setup(seq).Returns([2, 3]).When.GetNumber;
  mock.Setup(seq).Executes.When.Test2('', 0, False);
  CheckFalse(seq.Completed);

  mock.Instance.Test1(0, '');
  CheckEquals(2, mock2.Instance.GetNumber);
  CheckEquals(3, mock2.Instance.GetNumber);
  mock.Instance.Test2('', 0, False);
  Check(seq.Completed);
end;

procedure MockSequenceTest.ResetClearsSequence;
var
  mock: Mock<IMockTest>;
  seq: MockSequence;
begin
  mock.Behavior := TMockBehavior.Strict;
  mock.Setup(seq).Executes.When.Test1(1, 'a');
  mock.Instance.Test1(1, 'a');

  seq.Reset;
  mock.Setup(seq).Executes.When.Test1(2, 'b');
  mock.Instance.Test1(2, 'b');

  Pass;
end;

procedure MockSequenceTest.SequenceAllowsMultipleExpectedCalls;
var
  mock: Mock<IMockTest>;
  seq: MockSequence;
begin
  mock.Behavior := TMockBehavior.Strict;
  with mock.Setup(seq) do
  begin
    Returns<Integer>([1, 2]).When.GetNext;
    Raises<EInvalidOperationException>.When.GetNext;
  end;

  with mock.Instance do
  begin
    CheckEquals(1, GetNext);
    CheckEquals(2, GetNext);
    CheckException(EInvalidOperationException,
      procedure
      begin
        GetNext;
      end);
  end;

  Check(seq.Completed);
end;

procedure MockSequenceTest.SequenceWorksUsingWith;
var
  seq: MockSequence;
  mock: Mock<IMockTest>;
begin
  mock.Behavior := TMockBehavior.Strict;
  with mock.Setup(seq).Executes do
  begin
    When.Test1(1, 'a');
    When.Test2('b', 2, True);
  end;

  with mock.Instance do
  begin
    Test1(1, 'a');
    Test2('b', 2, True);
  end;

  Check(seq.Completed);
end;

{$ENDREGION}


end.
