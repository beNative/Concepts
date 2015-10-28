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

unit Spring.Mocking.Core;

interface

uses
  Rtti,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Interception,
  Spring.Mocking,
  Spring.Mocking.Interceptor,
  Spring.Times;

type
  TMock = class(TInterfacedObject, IMock, ISetup, IWhen)
  private
    fTypeInfo: PTypeInfo;
    fInterceptor: TMockInterceptor;
    fProxy: TValue;
    function GetCallBase: Boolean;
    function GetInstance: TValue;
    function GetTypeInfo: PTypeInfo;
    procedure SetCallBase(const value: Boolean);
    function CreateProxy(typeInfo: PTypeInfo;
      const interceptor: TMockInterceptor): TValue;
  public
    constructor Create(typeInfo: PTypeInfo; const interceptor: TMockInterceptor;
      const proxy: TValue) overload;
    constructor Create(typeInfo: PTypeInfo;
      behavior: TMockBehavior = DefaultMockBehavior); overload;
    constructor Create(typeInfo: PTypeInfo;
      behavior: TMockBehavior; const args: array of TValue); overload;
    destructor Destroy; override;

  {$REGION 'Implements IMock'}
    function Setup: ISetup;

    procedure Received; overload;
    procedure Received(const times: Times); overload;
    procedure ReceivedWithAnyArgs; overload;
    procedure ReceivedWithAnyArgs(const times: Times); overload;
  {$ENDREGION}

  {$REGION 'Implements ISetup'}
    function Executes: IWhen; overload;
    function Executes(const action: TMockAction): IWhen; overload;

    function Raises(const exceptionClass: ExceptClass;
      const msg: string = ''): IWhen; overload;
    function Raises(const exceptionClass: ExceptClass;
      const msg: string; const args: array of const): IWhen; overload;

    function Returns(const value: TValue): IWhen; overload;
    function Returns(const values: array of TValue): IWhen; overload;
  {$ENDREGION}

  {$REGION 'Implements IWhen'}
    procedure When;
    procedure WhenForAnyArgs;
  {$ENDREGION}

    property Instance: TValue read GetInstance;
    property TypeInfo: PTypeInfo read GetTypeInfo;
  end;

  TMock<T> = class(TMock, IMock<T>, ISetup<T>, IWhen<T>)
  private
    function GetInstance: T;
  public
    constructor Create(behavior: TMockBehavior; const args: array of TValue);

  {$REGION 'Implements IMock<T>'}
    function Setup: ISetup<T>;

    function Received: T; overload;
    function Received(const times: Times): T; overload;
    function ReceivedWithAnyArgs: T; overload;
    function ReceivedWithAnyArgs(const times: Times): T; overload;
  {$ENDREGION}

  {$REGION 'Implements ISetup<T>'}
    function Executes: IWhen<T>; overload;
    function Executes(const action: TMockAction): IWhen<T>; overload;

    function Raises(const exceptionClass: ExceptClass;
      const msg: string = ''): IWhen<T>; overload;
    function Raises(const exceptionClass: ExceptClass;
      const msg: string; const args: array of const): IWhen<T>; overload;

    function Returns(const value: TValue): IWhen<T>; overload;
    function Returns(const values: array of TValue): IWhen<T>; overload;
  {$ENDREGION}

  {$REGION 'Implements IWhen<T>'}
    function When: T;
    function WhenForAnyArgs: T;
  {$ENDREGION}

    property Instance: T read GetInstance;
  end;

implementation

uses
  Spring.Collections,
  Spring.Reflection,
  Spring.ResourceStrings;

resourcestring
  SCallCountExceeded = 'call count exceeded: %s';


{$REGION 'TMock'}

constructor TMock.Create(typeInfo: PTypeInfo; behavior: TMockBehavior);
begin
  Create(typeInfo, behavior, []);
end;

constructor TMock.Create(typeInfo: PTypeInfo; behavior: TMockBehavior;
  const args: array of TValue);
var
  types: array of PTypeInfo;
  i: Integer;
  ctor: TRttiMethod;
begin
  inherited Create;
  fTypeInfo := typeInfo;
  fInterceptor := TMockInterceptor.Create(behavior);
  fProxy := CreateProxy(typeInfo, fInterceptor);
  if (fTypeInfo.Kind = tkClass) and (Length(args) > 0) then
  begin
    SetLength(types, Length(args));
    for i := 0 to High(args) do
      types[i] := args[i].TypeInfo;
    if TType.GetType(fTypeInfo).Methods.TryGetFirst(ctor,
      TMethodFilters.IsConstructor and
      TMethodFilters.HasParameterTypes(types)) then
      ctor.Invoke(fProxy, args);
  end;
end;

constructor TMock.Create(typeInfo: PTypeInfo;
  const interceptor: TMockInterceptor; const proxy: TValue);
begin
  inherited Create;
  fTypeInfo := typeInfo;
  fInterceptor := interceptor;
  fProxy := proxy;
end;

destructor TMock.Destroy;
begin
  if fTypeInfo.Kind = tkClass then
{$IFNDEF AUTOREFCOUNT}
    fProxy.AsObject.Free;
{$ELSE}
    fProxy := TValue.Empty;
{$ENDIF}
  inherited;
end;

function TMock.CreateProxy(typeInfo: PTypeInfo;
  const interceptor: TMockInterceptor): TValue;
var
  intf: IInterface;
begin
  case typeInfo.Kind of
    tkClass:
      Result := TProxyGenerator.CreateClassProxy(
        typeInfo.TypeData.ClassType, [interceptor]);
    tkInterface:
    begin
      Supports(TProxyGenerator.CreateInterfaceProxyWithoutTarget(
        typeInfo, [interceptor]), GetTypeData(typeInfo).Guid, intf);
      TValue.Make(@intf, typeInfo, Result);
    end;
  else
    raise ENotSupportedException.CreateResFmt(@STypeNotSupported, [typeInfo.TypeName]);
  end;
end;

function TMock.GetCallBase: Boolean;
begin
  Result := fInterceptor.CallBase;
end;

function TMock.GetInstance: TValue;
begin
  Result := fProxy;
end;

function TMock.GetTypeInfo: PTypeInfo;
begin
  Result := fTypeInfo;
end;

function TMock.Executes: IWhen;
begin
  fInterceptor.Returns(nil);
  Result := Self;
end;

function TMock.Executes(const action: TMockAction): IWhen;
begin
  fInterceptor.Returns(action);
  Result := Self;
end;

function TMock.Raises(const exceptionClass: ExceptClass;
  const msg: string): IWhen;
begin
  fInterceptor.Returns(
    function(const callInfo: TCallInfo): TValue
    begin
      raise exceptionClass.Create(msg);
    end);
  Result := Self;
end;

function TMock.Raises(const exceptionClass: ExceptClass;
  const msg: string; const args: array of const): IWhen;
var
  s: string;
begin
  s := Format(msg, args);
  fInterceptor.Returns(
    function(const callInfo: TCallInfo): TValue
    begin
      raise exceptionClass.Create(s);
    end);
  Result := Self;
end;

function TMock.Returns(const value: TValue): IWhen;
var
  tempValue: TValue;
begin
  tempValue := value;
  fInterceptor.Returns(
    function(const callInfo: TCallInfo): TValue
    begin
      Result := tempValue;
    end);
  Result := Self;
end;

function TMock.Returns(const values: array of TValue): IWhen;
var
  tempValues: TArray<TValue>;
begin
  tempValues := TArray.Copy<TValue>(values);
  fInterceptor.Returns(
    function(const callInfo: TCallInfo): TValue
    begin
      if callInfo.CallCount <= Length(tempValues) then
        Result := tempValues[callInfo.CallCount - 1]
      else
        if fInterceptor.Behavior = TMockBehavior.Strict then
          raise EMockException.CreateResFmt(@SCallCountExceeded, [
            Times.AtMost(Length(tempValues)).ToString(callInfo.CallCount)]);
    end);
  Result := Self;
end;

procedure TMock.Received;
begin
  fInterceptor.Received(Times.AtLeastOnce);
end;

procedure TMock.Received(const times: Times);
begin
  fInterceptor.Received(times);
end;

procedure TMock.ReceivedWithAnyArgs;
begin
  fInterceptor.ReceivedForAnyArgs(Times.AtLeastOnce);
end;

procedure TMock.ReceivedWithAnyArgs(const times: Times);
begin
  fInterceptor.ReceivedForAnyArgs(times);
end;

procedure TMock.SetCallBase(const value: Boolean);
begin
  fInterceptor.CallBase := value;
end;

function TMock.Setup: ISetup;
begin
  Result := Self;
end;

procedure TMock.When;
begin
  fInterceptor.When;
end;

procedure TMock.WhenForAnyArgs;
begin
  fInterceptor.WhenForAnyArgs;
end;

{$ENDREGION}


{$REGION 'TMock<T>'}

constructor TMock<T>.Create(behavior: TMockBehavior;
  const args: array of TValue);
begin
  inherited Create(System.TypeInfo(T), behavior, args);
end;

function TMock<T>.GetInstance: T;
begin
  Result := fProxy.AsType<T>;
end;

function TMock<T>.Executes: IWhen<T>;
begin
  inherited Executes;
  Result := Self;
end;

function TMock<T>.Executes(const action: TMockAction): IWhen<T>;
begin
  inherited Executes(action);
  Result := Self;
end;

function TMock<T>.Raises(const exceptionClass: ExceptClass;
  const msg: string = ''): IWhen<T>;
begin
  inherited Raises(exceptionClass, msg);
  Result := Self;
end;

function TMock<T>.Raises(const exceptionClass: ExceptClass;
  const msg: string; const args: array of const): IWhen<T>;
begin
  inherited Raises(exceptionClass, msg, args);
  Result := Self;
end;

function TMock<T>.Received: T;
begin
  inherited Received;
  Result := Instance;
end;

function TMock<T>.Received(const times: Times): T;
begin
  inherited Received(times);
  Result := Instance;
end;

function TMock<T>.ReceivedWithAnyArgs: T;
begin
  inherited ReceivedWithAnyArgs;
  Result := Instance;
end;

function TMock<T>.ReceivedWithAnyArgs(const times: Times): T;
begin
  inherited ReceivedWithAnyArgs(times);
  Result := Instance;
end;

function TMock<T>.Returns(const value: TValue): IWhen<T>;
begin
  inherited Returns(value);
  Result := Self;
end;

function TMock<T>.Returns(const values: array of TValue): IWhen<T>;
begin
  inherited Returns(values);
  Result := Self;
end;

function TMock<T>.Setup: ISetup<T>;
begin
  inherited Setup;
  Result := Self;
end;

function TMock<T>.When: T;
begin
  fInterceptor.When;
  Result := Instance;
end;

function TMock<T>.WhenForAnyArgs: T;
begin
  fInterceptor.WhenForAnyArgs;
  Result := Instance;
end;

{$ENDREGION}


end.
