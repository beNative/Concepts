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

{$I Spring.inc}

unit Spring.Mocking.Interceptor;

interface

uses
  Rtti,
  Spring.Collections,
  Spring.Interception,
  Spring.Mocking,
  Spring.Mocking.Matching;

{$SCOPEDENUMS ON}

type
  TMethodCall = class
  private
    fAction: TMockAction;
    fCallCount: Integer;
    fMatch: TArgMatch;
  public
    constructor Create(const action: TMockAction; const match: TArgMatch);

    function Invoke(const invocation: IInvocation): TValue;

    property CallCount: Integer read fCallCount;
    property Match: TArgMatch read fMatch;
  end;

  TMockInterceptor = class(TInterfacedObject, IInterface, IInterceptor)
  private
    type
      TMockState = (Arrange, Act, Assert);
  private
    fBehavior: TMockBehavior;
    fCallBase: Boolean;
    fCurrentAction: TMockAction;
    fCurrentTimes: Times;
    fMatch: TArgMatch;
    fExpectedCalls: IMultiMap<TRttiMethod,TMethodCall>;
    fReceivedCalls: IMultiMap<TRttiMethod,TArray<TValue>>;
    fState: TMockState;
    class function CreateArgMatch(const arguments: TArray<TValue>;
      const parameters: TArray<TRttiParameter>): TArgMatch; static;
    class function CreateMock(const invocation: IInvocation): TMockAction; static;
    procedure InterceptArrange(const invocation: IInvocation);
    procedure InterceptAct(const invocation: IInvocation);
    procedure InterceptAssert(const invocation: IInvocation);
  protected
    procedure Intercept(const invocation: IInvocation);
  public
    constructor Create(behavior: TMockBehavior = DefaultMockBehavior);

    procedure Received(const times: Times); overload;
    procedure Received(const times: Times; const match: TArgMatch); overload;

    procedure Reset;

    procedure Returns(const action: TMockAction);

    procedure When; overload;
    procedure When(const match: TArgMatch); overload;

    property Behavior: TMockBehavior read fBehavior write fBehavior;
    property CallBase: Boolean read fCallBase write fCallBase;
  end;

implementation

uses
  Generics.Collections,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Mocking.Core,
  Spring.Times;

resourcestring
  SUnexpectedMethodCall = 'unexpected call of %s with arguments: %s';
  SUnexpectedCallCount = 'unexpected call count: %s';

function ArgsToString(const values: TArray<TValue>): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(values) do
  begin
    if Result <> '' then
      Result := Result + ', ';
    if values[i].IsString then
      Result := Result + QuotedStr(values[i].ToString)
    else if values[i].IsInstance then
      Result := Result + 'nil'
    else
      Result := Result + values[i].ToString;
  end;
end;


{$REGION 'TMockInterceptor'}

constructor TMockInterceptor.Create(behavior: TMockBehavior);
begin
  inherited Create;
  fBehavior := behavior;
  fState := TMockState.Act;
  fExpectedCalls := TCollections.CreateMultiMap<TRttiMethod,TMethodCall>([doOwnsValues]);
  fReceivedCalls := TCollections.CreateMultiMap<TRttiMethod,TArray<TValue>>();
end;

class function TMockInterceptor.CreateMock(
  const invocation: IInvocation): TMockAction;
var
  mock: IMock;
begin
  mock := TMock.Create(invocation.Method.ReturnType.Handle);
  Result :=
    function(const callInfo: TCallInfo): TValue
    begin
      Result := mock.Instance;
    end;
end;

class function TMockInterceptor.CreateArgMatch(const arguments: TArray<TValue>;
  const parameters: TArray<TRttiParameter>): TArgMatch;
begin
  Result :=
    function(const args: TArray<TValue>): Boolean
    var
      i: Integer;
    begin
      if Length(arguments) <> Length(args) then
        Exit(False);
      for i := Low(arguments) to High(arguments) do
      begin
        if Assigned(parameters) and (pfOut in parameters[i].Flags) then
          Continue;
        if not arguments[i].Equals(args[i]) then
          Exit(False);
      end;
      Result := True;
    end;
end;

procedure TMockInterceptor.Intercept(const invocation: IInvocation);
begin
  case fState of
    TMockState.Arrange:
      InterceptArrange(invocation);
    TMockState.Act:
      InterceptAct(invocation);
    TMockState.Assert:
      InterceptAssert(invocation);
  end;
end;

procedure TMockInterceptor.InterceptAct(const invocation: IInvocation);
var
  methodCall: TMethodCall;
begin
  methodCall := fExpectedCalls[invocation.Method].LastOrDefault(
    function(const m: TMethodCall): Boolean
    begin
      Result := m.Match(invocation.Arguments);
    end);

  if not Assigned(methodCall) then
    if fCallBase then
      invocation.Proceed
    else
      if fBehavior = TMockBehavior.Strict then
        raise EMockException.CreateResFmt(@SUnexpectedMethodCall, [
          invocation.Method.ToString, ArgsToString(invocation.Arguments)])
      else
        // create results for params
        if invocation.Method.HasExtendedInfo
          and (invocation.Method.MethodKind = mkFunction)
          and (invocation.Method.ReturnType.TypeKind = tkInterface) then
        begin
          methodCall := TMethodCall.Create(CreateMock(invocation),
            CreateArgMatch(invocation.Arguments, invocation.Method.GetParameters));
          fExpectedCalls.Add(invocation.Method, methodCall);
        end;

  fReceivedCalls.Add(invocation.Method, Copy(invocation.Arguments));
  if Assigned(methodCall) then
    invocation.Result := methodCall.Invoke(invocation);
end;

procedure TMockInterceptor.InterceptArrange(const invocation: IInvocation);
var
  methodCall: TMethodCall;
begin
  try
    if not Assigned(fMatch) then
      fMatch := TMatcherFactory.CreateMatchers(invocation.Arguments, invocation.Method.GetParameters);
    if not Assigned(fMatch) then
      fMatch := CreateArgMatch(invocation.Arguments, invocation.Method.GetParameters);
    methodCall := TMethodCall.Create(fCurrentAction, fMatch);
    fExpectedCalls.Add(invocation.Method, methodCall);
  finally
    fState := TMockState.Act;
    fMatch := nil;
  end;
end;

procedure TMockInterceptor.InterceptAssert(const invocation: IInvocation);
var
  arguments: IReadOnlyList<TArray<TValue>>;
  callCount: Integer;
begin
  try
    if not Assigned(fMatch) then
      fMatch := TMatcherFactory.CreateMatchers(invocation.Arguments, invocation.Method.GetParameters);
    if fReceivedCalls.TryGetValues(invocation.Method, arguments) then
    begin
      if not Assigned(fMatch) then
        fMatch := CreateArgMatch(invocation.Arguments, invocation.Method.GetParameters);
      callCount := arguments.Where(fMatch).Count;
    end
    else
      callCount := 0;
  finally
    fState := TMockState.Act;
    fMatch := nil;
  end;
  if not fCurrentTimes.Verify(callCount) then
    raise EMockException.CreateResFmt(@SUnexpectedCallCount, [
      fCurrentTimes.ToString(callCount)]);
end;

procedure TMockInterceptor.Received(const times: Times);
begin
  fState := TMockState.Assert;
  fCurrentTimes := times;
end;

procedure TMockInterceptor.Received(const times: Times; const match: TArgMatch);
begin
  fState := TMockState.Assert;
  fCurrentTimes := times;
  fMatch := match;
end;

procedure TMockInterceptor.Reset;
begin
  fState := TMockState.Act;
  fExpectedCalls.Clear;
  fReceivedCalls.Clear;
end;

procedure TMockInterceptor.Returns(const action: TMockAction);
begin
  fCurrentAction := action;
end;

procedure TMockInterceptor.When;
begin
  fState := TMockState.Arrange;
end;

procedure TMockInterceptor.When(const match: TArgMatch);
begin
  fState := TMockState.Arrange;
  fMatch := match;
end;

{$ENDREGION}


{$REGION 'TMethodCall'}

constructor TMethodCall.Create(const action: TMockAction; const match: TArgMatch);
begin
  inherited Create;
  fAction := action;
  fMatch := match;
end;

function TMethodCall.Invoke(const invocation: IInvocation): TValue;
begin
  Inc(fCallCount);
  if Assigned(fAction) then
    Result := fAction(TCallInfo.Create(invocation, fCallCount))
  else if invocation.Method.HasExtendedInfo
    and (invocation.Method.MethodKind = mkFunction) then
    Result := Result.Cast(invocation.Method.ReturnType.Handle)
  else
    Result := TValue.Empty;
end;

{$ENDREGION}


end.
