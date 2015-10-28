{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Interception,
  Spring.Mocking;

{$SCOPEDENUMS ON}

type
  TMethodCall = class
  private
    fAction: TMockAction;
    fArguments: Nullable<TArray<TValue>>;
    fCallCount: Integer;
  public
    constructor Create(const action: TMockAction;
      const arguments: Nullable<TArray<TValue>>);

    function Invoke(const invocation: IInvocation): TValue;

    property Arguments: Nullable<TArray<TValue>> read fArguments;
    property CallCount: Integer read fCallCount;
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
    fArgsMatching: Boolean;
    fExpectedCalls: IMultiMap<TRttiMethod,TMethodCall>;
    fReceivedCalls: IMultiMap<TRttiMethod,TArray<TValue>>;
    fState: TMockState;
    function CreateMock(const invocation: IInvocation): TMockAction;
  protected
    procedure Intercept(const invocation: IInvocation);
  public
    constructor Create(behavior: TMockBehavior = DefaultMockBehavior);

    procedure Received(const times: Times);
    procedure ReceivedForAnyArgs(const times: Times);

    procedure Returns(const action: TMockAction);

    procedure When;
    procedure WhenForAnyArgs;

    property Behavior: TMockBehavior read fBehavior;
    property CallBase: Boolean read fCallBase write fCallBase;
  end;

implementation

uses
  TypInfo,
  Spring.Mocking.Core,
  Spring.Reflection,
  Spring.Times;

resourcestring
  SUnexpectedMethodCall = 'unexpected call of %s with arguments: %s';
  SUnexpectedCallCount = 'unexpected call count: %s';

function ArgsEqual(const left, right: TArray<TValue>): Boolean;
var
  i: Integer;
begin
  Result := Length(left) = Length(right);
  for i := Low(left) to High(left) do
    if not left[i].Equals(right[i]) then
      Exit(False);
end;

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

function TMockInterceptor.CreateMock(
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

procedure TMockInterceptor.Intercept(const invocation: IInvocation);
var
  methodCalls: IReadOnlyCollection<TMethodCall>;
  methodCall: TMethodCall;
  arguments: IReadOnlyCollection<TArray<TValue>>;
  callCount: Integer;
  args: Nullable<TArray<TValue>>;
begin
  case fState of
    TMockState.Arrange:
    begin
      if fArgsMatching then
        args := invocation.Arguments;
      methodCall := TMethodCall.Create(fCurrentAction, args);
      fExpectedCalls.Add(invocation.Method, methodCall);
      fState := TMockState.Act;
    end;
    TMockState.Act:
    begin
      if fExpectedCalls.TryGetValues(invocation.Method, methodCalls) then
        methodCall := methodCalls.LastOrDefault(
          function(const m: TMethodCall): Boolean
          begin
            Result := not m.Arguments.HasValue
              or ArgsEqual(m.Arguments, invocation.Arguments);
          end)
      else
        methodCall := nil;

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
                invocation.Arguments);
              fExpectedCalls.Add(invocation.Method, methodCall);
            end;

      if Assigned(methodCall) then
        invocation.Result := methodCall.Invoke(invocation);
      fReceivedCalls.Add(invocation.Method, invocation.Arguments);
    end;
    TMockState.Assert:
    begin
      if fReceivedCalls.TryGetValues(invocation.Method, arguments) then
      begin
        callCount := arguments.Where(
          function(const args: TArray<TValue>): Boolean
          begin
            Result := not fArgsMatching or ArgsEqual(args, invocation.Arguments);
          end).Count;
      end
      else
        callCount := 0;
      if not fCurrentTimes.Verify(callCount) then
        raise EMockException.CreateResFmt(@SUnexpectedCallCount, [
          fCurrentTimes.ToString(callCount)]);
    end;
  end;
end;

procedure TMockInterceptor.Received(const times: Times);
begin
  fState := TMockState.Assert;
  fCurrentTimes := times;
  fArgsMatching := True;
end;

procedure TMockInterceptor.ReceivedForAnyArgs(const times: Times);
begin
  fState := TMockState.Assert;
  fCurrentTimes := times;
  fArgsMatching := False;
end;

procedure TMockInterceptor.Returns(const action: TMockAction);
begin
  fCurrentAction := action;
end;

procedure TMockInterceptor.When;
begin
  fState := TMockState.Arrange;
  fArgsMatching := True;
end;

procedure TMockInterceptor.WhenForAnyArgs;
begin
  fState := TMockState.Arrange;
  fArgsMatching := False;
end;

{$ENDREGION}


{$REGION 'TMethodCall'}

constructor TMethodCall.Create(const action: TMockAction;
  const arguments: Nullable<TArray<TValue>>);
begin
  inherited Create;
  fAction := action;
  fArguments := arguments;
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
