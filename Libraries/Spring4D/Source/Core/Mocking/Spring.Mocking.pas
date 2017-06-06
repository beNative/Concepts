{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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

{$I Spring.inc}

unit Spring.Mocking;

interface

uses
  Rtti,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.DesignPatterns,
  Spring.Interception,
  Spring.Mocking.Matching,
  Spring.Times;

{$SCOPEDENUMS ON}

type
  TMockBehavior = (Dynamic, Strict);

  Times = Spring.Times.Times;

  TValue = Rtti.TValue;

const
  Arg: TArg = ();
  Args: TArgs = ();
  DefaultMockBehavior = TMockBehavior.Dynamic;

type
  TCallInfo = record
  private
    fInvocation: IInvocation;
    fCallCount: Integer;
    function GetArg(index: Integer): TValue;
    function GetCallCount: Integer;
    function GetMethod: TRttiMethod;
    procedure SetArg(index: Integer; const value: TValue);
  public
    constructor Create(const invocation: IInvocation; callCount: Integer);
    property Args[index: Integer]: TValue read GetArg write SetArg; default;
    property CallCount: Integer read GetCallCount;
    property Method: TRttiMethod read GetMethod;
  end;

  TMockAction = reference to function(const callInfo: TCallInfo): TValue;

  IWhen = interface(IInvokable)
    ['{DAD0B65B-8CEF-4513-ADE8-38E8F9AAFA9A}']
    procedure When; overload;
    procedure When(const match: TArgMatch); overload;
  end;

  ISetup = interface(IInvokable)
    ['{0BC12D48-41FF-46D0-93B3-773EE19D75ED}']
    function Executes: IWhen; overload;
    function Executes(const action: TMockAction): IWhen; overload;

    function Raises(const exceptionClass: ExceptClass;
      const msg: string = ''): IWhen; overload;
    function Raises(const exceptionClass: ExceptClass;
      const msg: string; const args: array of const): IWhen; overload;

    function Returns(const value: TValue): IWhen; overload;
    function Returns(const values: array of TValue): IWhen; overload;
  end;

  IMock = interface(IInvokable)
    ['{7D386664-22CF-4555-B03E-61319C39BC12}']
    function GetInstance: TValue;
    function GetTypeInfo: PTypeInfo;

    procedure Reset;

    function Setup: ISetup;

    procedure Received; overload;
    procedure Received(const times: Times); overload;
    procedure Received(const match: TArgMatch); overload;
    procedure Received(const times: Times; const match: TArgMatch); overload;

    property Instance: TValue read GetInstance;
    property TypeInfo: PTypeInfo read GetTypeInfo;
  end;

  IWhen<T> = interface(IInvokable)
    ['{4162918E-4DE6-47D6-B609-D5A17F3FBE2B}']
    function When: T; overload;
    function When(const match: TArgMatch): T; overload;
  end;

  ISetup<T> = interface(IInvokable)
    ['{CD661866-EB29-400C-ABC8-19FC8D59FFAD}']
    function Executes: IWhen<T>; overload;
    function Executes(const action: TMockAction): IWhen<T>; overload;

    function Raises(const exceptionClass: ExceptClass;
      const msg: string = ''): IWhen<T>; overload;
    function Raises(const exceptionClass: ExceptClass;
      const msg: string; const args: array of const): IWhen<T>; overload;

    function Returns(const value: TValue): IWhen<T>; overload;
    function Returns(const values: array of TValue): IWhen<T>; overload;
  end;

  IMockSequence = interface
  {$REGION 'Property Accessors'}
    function GetCompleted: Boolean;
    function GetCurrent: Integer;
  {$ENDREGION}

    function AddStep: Integer;
    procedure MoveNext;

    property Completed: Boolean read GetCompleted;
    property Current: Integer read GetCurrent;
  end;

  IMock<T> = interface(IInvokable)
    ['{67AD5AD2-1C23-41BA-8F5D-5C28B3C7ABF7}']
  {$REGION 'Property Accessors'}
    function GetBehavior: TMockBehavior;
    function GetCallBase: Boolean;
    function GetInstance: T;
    function GetTypeInfo: PTypeInfo;
    procedure SetBehavior(const value: TMockBehavior);
    procedure SetCallBase(const value: Boolean);
  {$ENDREGION}

    procedure Reset;

    function Setup: ISetup<T>; overload;
    function Setup(const sequence: IMockSequence): ISetup<T>; overload;

    function Received: T; overload;
    function Received(const times: Times): T; overload;
    function Received(const match: TArgMatch): T; overload;
    function Received(const times: Times; const match: TArgMatch): T; overload;

    property Behavior: TMockBehavior read GetBehavior write SetBehavior;
    property CallBase: Boolean read GetCallBase write SetCallBase;
    property Instance: T read GetInstance;
    property TypeInfo: PTypeInfo read GetTypeInfo;
  end;

  EMockException = class(Exception);

  Setup<T> = record
  private
    fSetup: ISetup<T>;
  public
    function Executes: IWhen<T>; overload;
    function Executes(const action: TMockAction): IWhen<T>; overload;

    function Raises<TException: Exception>(
      const msg: string = ''): IWhen<T>; overload;
    function Raises<TException: Exception>(
      const msg: string; const args: array of const): IWhen<T>; overload;

    function Returns<TResult>(const value: TResult): IWhen<T>; overload;
    function Returns<TResult>(const values: array of TResult): IWhen<T>; overload;
  end;

  Mock<T> = record
  private
    fMock: IMock<T>;
  {$HINTS OFF}
    // cause record to be passed as reference on const parameter
    // because it does not fit in a register
    fDummy: Pointer;
  {$HINTS ON}
    procedure EnsureInitialized; inline;
    function GetInstance: T;
    function GetBehavior: TMockBehavior;
    function GetCallBase: Boolean;
    procedure SetBehavior(const value: TMockBehavior);
    procedure SetCallBase(const value: Boolean);
  public
    class function Create(
      behavior: TMockBehavior = DefaultMockBehavior): Mock<T>; overload; static;
    class function Create(behavior: TMockBehavior;
      const args: array of TValue): Mock<T>; overload; static;
    class function Create(const args: array of TValue;
      behavior: TMockBehavior = DefaultMockBehavior): Mock<T>; overload; static;

    procedure Free;
    procedure Reset;

    class operator Implicit(const value: IMock): Mock<T>;
    class operator Implicit(const value: Mock<T>): IMock;
    class operator Implicit(const value: Mock<T>): IMock<T>;
    class operator Implicit(const value: Mock<T>): T;

    function Setup: Setup<T>; overload;
    function Setup(const sequence: IMockSequence): Setup<T>; overload;

    function Received: T; overload;
    function Received(const times: Times): T; overload;
    function Received(const match: TArgMatch): T; overload;
    function Received(const times: Times; const match: TArgMatch): T; overload;

    function AsType<TInterface: IInterface>: Mock<TInterface>;

    property Behavior: TMockBehavior read GetBehavior write SetBehavior;
    property CallBase: Boolean read GetCallBase write SetCallBase;
    property Instance: T read GetInstance;
  end;

  Mock = record
  public
    class function From<T: IInterface>(const value: T): Mock<T>; static;
  end;

  MockSequence = record
  private
    fSequence: IMockSequence;
  {$HINTS OFF}
    // cause record to be passed as reference on const parameter
    // because it does not fit in a register
    fDummy: Pointer;
  {$HINTS ON}
    procedure EnsureInitialized;
    function GetCompleted: Boolean;
  public
    class operator Implicit(const value: MockSequence): IMockSequence;

    procedure Reset;
    property Completed: Boolean read GetCompleted;
  end;

implementation

uses
  Spring.Helpers,
  Spring.Mocking.Core,
  Spring.Mocking.Interceptor;


{$REGION 'TCallInfo'}

constructor TCallInfo.Create(const invocation: IInvocation; callCount: Integer);
begin
  fInvocation := invocation;
  fCallCount := callCount;
end;

function TCallInfo.GetArg(index: Integer): TValue;
begin
  Result := fInvocation.Arguments[index];
end;

function TCallInfo.GetCallCount: Integer;
begin
  Result := fCallCount;
end;

function TCallInfo.GetMethod: TRttiMethod;
begin
  Result := fInvocation.Method;
end;

procedure TCallInfo.SetArg(index: Integer; const value: TValue);
var
  parameter: TRttiParameter;
begin
  parameter := fInvocation.Method.GetParameters[index];
  if parameter.Flags * [pfVar, pfOut] = [] then
    raise EMockException.CreateFmt('parameter "%s" is not var or out', [parameter.Name]);
  fInvocation.Arguments[index] := value;
end;

{$ENDREGION}


{$REGION 'Setup<T>'}

function Setup<T>.Executes: IWhen<T>;
begin
  Result := fSetup.Executes;
end;

function Setup<T>.Executes(const action: TMockAction): IWhen<T>;
begin
  Result := fSetup.Executes(action);
end;

function Setup<T>.Raises<TException>(const msg: string): IWhen<T>;
begin
  Result := fSetup.Raises(ExceptClass(TException), msg);
end;

function Setup<T>.Raises<TException>(const msg: string;
  const args: array of const): IWhen<T>;
begin
  Result := fSetup.Raises(ExceptClass(TException), msg, args);
end;

function Setup<T>.Returns<TResult>(const value: TResult): IWhen<T>;
begin
  Result := fSetup.Returns(TValue.From<TResult>(value));
end;

function Setup<T>.Returns<TResult>(const values: array of TResult): IWhen<T>;
var
  i: Integer;
  tempValues: TArray<TValue>;
begin
  SetLength(tempValues, Length(values));
  for i := 0 to High(values) do
    tempValues[i] := TValue.From<TResult>(values[i]);
  Result := fSetup.Returns(tempValues);
end;

{$ENDREGION}


{$REGION 'Mock<T>'}

class function Mock<T>.Create(behavior: TMockBehavior): Mock<T>;
begin
  Result.fMock := TMock<T>.Create(behavior, []);
end;

class function Mock<T>.Create(behavior: TMockBehavior;
  const args: array of TValue): Mock<T>;
begin
  Result.fMock := TMock<T>.Create(behavior, args);
end;

class function Mock<T>.Create(const args: array of TValue;
  behavior: TMockBehavior): Mock<T>;
begin
  Result.fMock := TMock<T>.Create(behavior, args);
end;

function Mock<T>.AsType<TInterface>: Mock<TInterface>;
var
  typeData: PTypeData;
  source: T;
  proxy: IDynamicProxy;
  target: TInterface;
begin
  EnsureInitialized;
  typeData := GetTypeData(TypeInfo(TInterface));
  if not (ifHasGuid in typeData.IntfFlags) then
    raise EMockException.Create('interface without guid not supported');
  source := fMock.Instance;
  if not Supports(PInterface(@source)^, IDynamicProxy, proxy) then
    raise EMockException.Create('fatal error');
  proxy.AddAdditionalInterface(TypeInfo(TInterface), TProxyGenerationOptions.Default);
  PInterface(@source)^.QueryInterface(typeData.Guid, target);
  Result.fMock := Mock.From<TInterface>(target).fMock;
end;

procedure Mock<T>.EnsureInitialized;
begin
  if not Assigned(fMock) then
    fMock := TMock<T>.Create(DefaultMockBehavior, [])
end;

procedure Mock<T>.Free;
begin
  fMock := nil;
end;

class operator Mock<T>.Implicit(const value: IMock): Mock<T>;
begin
  Assert(Assigned(value));
  Assert(value.TypeInfo = System.TypeInfo(T));
  Result.fMock := value as IMock<T>;
end;

class operator Mock<T>.Implicit(const value: Mock<T>): IMock;
begin
  value.EnsureInitialized;
  Result := value.fMock as IMock;
end;

class operator Mock<T>.Implicit(const value: Mock<T>): IMock<T>;
begin
  value.EnsureInitialized;
  Result := value.fMock;
end;

class operator Mock<T>.Implicit(const value: Mock<T>): T;
begin
  value.EnsureInitialized;
  Result := value.fMock.Instance;
end;

function Mock<T>.GetBehavior: TMockBehavior;
begin
  EnsureInitialized;
  Result := fMock.Behavior;
end;

function Mock<T>.GetCallBase: Boolean;
begin
  EnsureInitialized;
  Result := fMock.CallBase;
end;

function Mock<T>.GetInstance: T;
begin
  EnsureInitialized;
  Result := fMock.Instance;
end;

procedure Mock<T>.SetBehavior(const value: TMockBehavior);
begin
  EnsureInitialized;
  fMock.Behavior := value;
end;

procedure Mock<T>.SetCallBase(const value: Boolean);
begin
  EnsureInitialized;
  fMock.CallBase := value
end;

function Mock<T>.Setup: Setup<T>;
begin
  EnsureInitialized;
  Result.fSetup := fMock.Setup;
end;

function Mock<T>.Setup(const sequence: IMockSequence): Setup<T>;
begin
  EnsureInitialized;
  Result.fSetup := fMock.Setup(sequence);
end;

function Mock<T>.Received: T;
begin
  EnsureInitialized;
  Result := fMock.Received;
end;

function Mock<T>.Received(const times: Times): T;
begin
  EnsureInitialized;
  Result := fMock.Received(times);
end;

function Mock<T>.Received(const match: TArgMatch): T;
begin
  EnsureInitialized;
  Result := fMock.Received(match);
end;

function Mock<T>.Received(const times: Times; const match: TArgMatch): T;
begin
  EnsureInitialized;
  Result := fMock.Received(times, match);
end;

procedure Mock<T>.Reset;
begin
  fMock.Reset;
end;

{$ENDREGION}


{$REGION 'Mock'}

class function Mock.From<T>(const value: T): Mock<T>;
var
  accessor: IProxyTargetAccessor;
  proxy: TValue;
  mock: TMock;
begin
  if not Supports(PInterface(@value)^, IProxyTargetAccessor, accessor) then
    raise EMockException.Create('value is not a mock');
  TValue.Make(@value, System.TypeInfo(T), proxy);
  mock := TMock<T>.NewInstance as TMock;
  mock.Create(TypeInfo(T), accessor.GetInterceptors.First(
    function(const interceptor: IInterceptor): Boolean
    begin
      Result := (interceptor as TObject) is TMockInterceptor
    end) as TMockInterceptor, proxy);
  Result.fMock := mock as IMock<T>;
end;

{$ENDREGION}


{$REGION 'MockSequence'}

procedure MockSequence.EnsureInitialized;
begin
  if not Assigned(fSequence) then
    fSequence := TMockSequence.Create;
end;

function MockSequence.GetCompleted: Boolean;
begin
  Result := Assigned(fSequence) and fSequence.Completed;
end;

class operator MockSequence.Implicit(const value: MockSequence): IMockSequence;
begin
  value.EnsureInitialized;
  Result := value.fSequence;
end;

procedure MockSequence.Reset;
begin
  fSequence := nil;
end;

{$ENDREGION}


end.
