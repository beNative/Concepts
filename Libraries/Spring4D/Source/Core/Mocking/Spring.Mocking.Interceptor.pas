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

{$I Spring.inc}

unit Spring.Mocking.Interceptor;

interface

uses
  Rtti,
  TypInfo,
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
    fSequence: IMockSequence;
    fPosition: Integer;
    fRefArgs: TArray<TValue>;
    procedure PutRefArgs(const invocation: IInvocation);
  public
    constructor Create(const action: TMockAction; const match: TArgMatch;
      const sequence: IMockSequence);

    function Invoke(const invocation: IInvocation): TValue;
    function Match(const args: TArray<TValue>): Boolean;

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
    fCurrentValues: TArray<TValue>;
    fCurrentTimes: Times;
    fMatch: TArgMatch;
    fExpectedCalls: IMultiMap<TRttiMethod,TMethodCall>;
    fReceivedCalls: IMultiMap<TRttiMethod,TArray<TValue>>;
    fResetCount: Byte;
    fState: TMockState;
    fSequence: IMockSequence;
    class function CreateArgMatch(const arguments: TArray<TValue>;
      const parameters: TArray<TRttiParameter>): TArgMatch; static;
    function CreateMethodCalls(const method: TRttiMethod): TArray<TMethodCall>;
    class function CreateEvent(const returnType: PTypeInfo): TMockAction; static;
    class function CreateMock(const returnType: PTypeInfo): TMockAction; static;
    procedure InterceptArrange(const invocation: IInvocation);
    procedure InterceptAct(const invocation: IInvocation);
    procedure InterceptAssert(const invocation: IInvocation);
    procedure SetSequence(const value: IMockSequence);
  protected
    procedure Intercept(const invocation: IInvocation);
  public
    constructor Create(behavior: TMockBehavior);

    procedure Executes(const action: TMockAction);

    procedure Received(const times: Times); overload;
    procedure Received(const times: Times; const match: TArgMatch); overload;

    procedure Reset;

    procedure Returns(const values: array of TValue);

    procedure SetDefaultExpectations(const target: TValue);

    procedure When; overload;
    procedure When(const match: TArgMatch); overload;

    property Behavior: TMockBehavior read fBehavior write fBehavior;
    property CallBase: Boolean read fCallBase write fCallBase;
    property Sequence: IMockSequence read fSequence write SetSequence;
  end;

implementation

uses
  SysUtils,
  Spring,
  Spring.Mocking.Core,
  Spring.Reflection,
  Spring.Times;

resourcestring
  SCallCountExceeded = 'call count exceeded: %s';
  SSequenceNotSupported = 'using sequence requires mock behavior to be strict';
  SUnexpectedMethodCall = 'unexpected call of %s';
  SUnexpectedMethodCallArgs = 'unexpected call of %s with arguments: %s';
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
    else if values[i].IsInstance and values[i].IsEmpty then
      Result := Result + 'nil'
    else
      Result := Result + FormatValue(values[i]);
  end;
end;

type
  TResultsMockAction = class(TInterfacedObject, TMockAction)
  private
    fValues: TArray<TValue>;
    fBehavior: ^TMockBehavior;
    function Invoke(const callInfo: TCallInfo): TValue;
    constructor Create(const values: TArray<TValue>; var behavior: TMockBehavior);
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

function TMockInterceptor.CreateMethodCalls(const method: TRttiMethod): TArray<TMethodCall>;

  function CastToReturnType(const value: TValue; const method: TRttiMethod): TValue;
  var
    rttiType, returnType: TRttiType;
    mock: TMock;
  begin
    Result := value;
    if Assigned(value.TypeInfo) and method.HasExtendedInfo then
    begin
      returnType := method.ReturnType;
      if Assigned(returnType) and (value.TypeInfo <> returnType.Handle) then
      begin
        rttiType := value.TypeInfo.RttiType;
        if rttiType.IsGenericTypeOf('Mock<>') then
        begin
          mock := IMock(value.GetReferenceToRawData^) as TMock;
          Result := mock.Instance;
        end;
        Result := Result.Convert(returnType.Handle);
      end;
    end;
  end;

var
  i: Integer;
  values: TArray<TValue>;
  action: TResultMockAction;
begin
  SetLength(Result, 1);
  if Assigned(fCurrentAction) then
  begin
    if PInterface(@fCurrentAction)^ is TResultMockAction then
    begin
      action := PInterface(@fCurrentAction)^ as TResultMockAction;
      action.Value := CastToReturnType(action.Value, method);
    end;
    Result[0] := TMethodCall.Create(fCurrentAction, fMatch, fSequence)
  end
  else
  begin
    values := fCurrentValues;
    if Assigned(fSequence) then
    begin
      SetLength(Result, Length(fCurrentValues));
      for i := 0 to High(values) do
      begin
        action := TResultMockAction.Create;
        action.Value := CastToReturnType(values[i], method);
        Result[i] := TMethodCall.Create(action, fMatch, fSequence);
      end;
    end
    else
    begin
      for i := 0 to High(values) do
        values[i] := CastToReturnType(values[i], method);
      Result[0] := TMethodCall.Create(
        TResultsMockAction.Create(values, fBehavior), fMatch, fSequence);
    end;
  end;

  for i := 0 to High(Result) do
    Result[i].fRefArgs := RefArgs.values;
  RefArgs.values := nil;
end;

function GetBoolean(inst: Pointer): Boolean; //FI:O804
begin
  Result := False;
end;

function GetMethodPointer(inst: Pointer): TMethodPointer; //FI:O804
begin
  Result := nil;
end;

procedure SetBoolean(inst: Pointer; const value: Boolean);
begin //FI:W519
end;

procedure SetMethodPointer(inst: Pointer; const value: TMethodPointer);
begin //FI:W519
end;

procedure RemoveAll(inst: Pointer; instance: Pointer);
begin //FI:W519
end;

procedure Clear(inst: Pointer);
begin //FI:W519
end;

function GetInvoke(inst: Pointer): TMethodPointer; //FI:O804
begin
  raise ENotSupportedException.Create('');
end;

class function TMockInterceptor.CreateEvent(const returnType: PTypeInfo): TMockAction;
const
  EventMock_Vtable: array[0..16] of Pointer =
  (
    @NopQueryInterface,
    @NopRef,
    @NopRef,
    // IEvent
    @GetBoolean,            // GetCanInvoke
    @GetBoolean,            // GetEnabled
    @GetMethodPointer,      // GetOnChanged
    @GetBoolean,            // GetUseFreeNotification
    @SetBoolean,            // SetEnabled
    @SetMethodPointer,      // SetOnChanged
    @SetBoolean,            // SetUseFreeNotification
    @SetMethodPointer,      // Add
    @SetMethodPointer,      // Remove
    @RemoveAll,             // RemoveAll
    @Clear,                 // Clear
    // IEvent<T>
    @SetMethodPointer,      // Add
    @SetMethodPointer,      // Remove
    // IInvokableEvent<T>
    @GetInvoke              // GetInvoke
  );

  EventMock_Instance: Pointer = @EventMock_Vtable;

const
  event: Pointer = @EventMock_Instance;
begin
  Result :=
    function(const callInfo: TCallInfo): TValue
    begin
      Result := TValue.From(event, returnType);
    end;
end;

class function TMockInterceptor.CreateMock(const returnType: PTypeInfo): TMockAction;
var
  mock: IMock;
begin
  mock := TMock.Create(returnType);
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

procedure TMockInterceptor.Executes(const action: TMockAction);
begin
  fCurrentAction := action;
  fCurrentValues := nil;
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
  method: TRttiMethod;
  returnType: TRttiType;
  action: TMockAction;
begin
  methodCall := fExpectedCalls[invocation.Method].LastOrDefault(
    function(const m: TMethodCall): Boolean
    begin
      Result := m.Match(invocation.Arguments);
    end);

  if not Assigned(methodCall) and (fResetCount = 0) then
    if fCallBase then
      invocation.Proceed
    else
      if fBehavior = TMockBehavior.Strict then
      begin
        if invocation.Arguments = nil then
          raise EMockException.CreateResFmt(@SUnexpectedMethodCall, [invocation.Method.ToString])
        else
          raise EMockException.CreateResFmt(@SUnexpectedMethodCallArgs, [invocation.Method.ToString,
            ArgsToString(invocation.Arguments)]);
      end
      else
      begin
        method := invocation.Method;
        // create results for params
        if method.HasExtendedInfo and (method.MethodKind = mkFunction) then
        begin
          returnType := method.ReturnType;
          if HasMethodInfo(returnType.Handle) then
            action := CreateMock(returnType.Handle)
          else if returnType.IsGenericTypeOf('Spring.IEvent<>')
            or Assigned(returnType.BaseType) and returnType.BaseType.IsGenericTypeOf('Spring.IEvent<>') then
            action := CreateEvent(returnType.Handle);

          if Assigned(action) then
          begin
            methodCall := TMethodCall.Create(action,
              CreateArgMatch(invocation.Arguments, invocation.Method.GetParameters), nil);
            fExpectedCalls.Add(invocation.Method, methodCall);
          end;
        end;
      end;

  if fResetCount = 0 then
    fReceivedCalls.Add(invocation.Method, Copy(invocation.Arguments));
  if Assigned(methodCall) then
    invocation.Result := methodCall.Invoke(invocation);
end;

procedure TMockInterceptor.InterceptArrange(const invocation: IInvocation);
begin
  try
    if not Assigned(fMatch) then
      fMatch := TMatcherFactory.CreateMatchers(invocation.Arguments, invocation.Method.GetParameters)
    else
      TMatcherFactory.ClearConditionStack;
    if not Assigned(fMatch) then
      fMatch := CreateArgMatch(invocation.Arguments, invocation.Method.GetParameters);
    fExpectedCalls.AddRange(invocation.Method, CreateMethodCalls(invocation.Method));
  finally
    CleanupArguments(invocation.Arguments);
    fState := TMockState.Act;
    fMatch := nil;
  end;
end;

procedure TMockInterceptor.InterceptAssert(const invocation: IInvocation);
var
  arguments: IReadOnlyCollection<TArray<TValue>>;
  callCount: Integer;
begin
  try
    if not Assigned(fMatch) then
      fMatch := TMatcherFactory.CreateMatchers(invocation.Arguments, invocation.Method.GetParameters)
    else
      TMatcherFactory.ClearConditionStack;
    if fReceivedCalls.TryGetValues(invocation.Method, arguments) then
    begin
      if not Assigned(fMatch) then
        fMatch := CreateArgMatch(invocation.Arguments, invocation.Method.GetParameters);
      callCount := arguments.Where(fMatch).Count;
    end
    else
      callCount := 0;
  finally
    CleanupArguments(invocation.Arguments);
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
  Inc(fResetCount);
  try
    fCurrentAction := nil;
    fCurrentValues := nil;
    fReceivedCalls.Clear;
    fExpectedCalls.Clear;
  finally
    Dec(fResetCount);
  end;
end;

procedure TMockInterceptor.Returns(const values: array of TValue);
begin
  fCurrentAction := nil;
  fCurrentValues := TArray.Copy<TValue>(values);
end;

procedure TMockInterceptor.SetDefaultExpectations(const target: TValue);
type
  TCallInfoAccess = record
    Invocation: IInvocation;
    CallCount: Integer;
  end;
var
  instance: TValue;
  method: TRttiMethod;
begin
  instance := target;
  for method in TType.GetType(target.TypeInfo).GetMethods do
    fExpectedCalls.Add(method, TMethodCall.Create(
      function(const call: TCallInfo): TValue
      begin
        Result := TCallInfoAccess(call).Invocation.Method.Invoke(
          instance, TCallInfoAccess(call).Invocation.Arguments);
      end, Args.Any, nil));
end;

procedure TMockInterceptor.SetSequence(const value: IMockSequence);
begin
  if Assigned(value) and (fBehavior <> TMockBehavior.Strict) then
    raise EMockException.CreateRes(@SSequenceNotSupported);
  fSequence := value;
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

constructor TMethodCall.Create(const action: TMockAction; const match: TArgMatch;
  const sequence: IMockSequence);
begin
  inherited Create;
  fAction := action;
  fMatch := match;
  fSequence := sequence;
  if Assigned(fSequence) then
    fPosition := fSequence.AddStep;
end;

function TMethodCall.Invoke(const invocation: IInvocation): TValue;
begin
  Inc(fCallCount);
  try
    if Assigned(fAction) then
    begin
      Result := fAction(TCallInfo.Create(invocation, fCallCount));
      if invocation.Method.HasExtendedInfo
        and (invocation.Method.MethodKind = mkFunction) then
        Result := Result.Convert(invocation.Method.ReturnType.Handle);
    end
    else if invocation.Method.HasExtendedInfo
      and (invocation.Method.MethodKind = mkFunction) then
      Result := Result.Cast(invocation.Method.ReturnType.Handle)
    else
      Result := TValue.Empty;
    PutRefArgs(invocation);
  finally
    if Assigned(fSequence) then
      fSequence.MoveNext;
  end;
end;

function TMethodCall.Match(const args: TArray<TValue>): Boolean;
begin
  if Assigned(fSequence) and (fPosition <> fSequence.Current) then
    Exit(False);
  Result := fMatch(args);
end;

procedure TMethodCall.PutRefArgs(const invocation: IInvocation);
var
  parameters: TArray<TRttiParameter>;
  i, k: Integer;
begin
  if fRefArgs = nil then
    Exit;

  parameters := invocation.Method.GetParameters;
  for i := 0 to High(parameters) do
    if parameters[i].Flags * [pfVar, pfOut] <> [] then
      for k := 0 to High(fRefArgs) do
        if parameters[i].ParamType.Handle = fRefArgs[k].TypeInfo then
          invocation.Arguments[i] := fRefArgs[k];
end;

{$ENDREGION}


{$REGION 'TResultsMockAction'}

constructor TResultsMockAction.Create(const values: TArray<TValue>;
  var behavior: TMockBehavior);
begin
  fValues := values;
  fBehavior := @behavior;
end;

function TResultsMockAction.Invoke(const callInfo: TCallInfo): TValue; //FI:W521
begin
  if callInfo.CallCount <= Length(fValues) then
    Result := fValues[callInfo.CallCount - 1]
  else
    if fBehavior^ = TMockBehavior.Strict then
      raise EMockException.CreateResFmt(@SCallCountExceeded, [
        Times.AtMost(Length(fValues)).ToString(callInfo.CallCount)]);
end;

{$ENDREGION}


end.
