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

unit Spring.Mocking.Core;

interface

uses
  Rtti,
  SysUtils,
  TypInfo,
  Spring,
  Spring.DesignPatterns,
  Spring.Interception,
  Spring.Mocking,
  Spring.Mocking.Interceptor,
  Spring.Mocking.Matching,
  Spring.Times;

type
  TMock = class(TInterfacedObject, IMock, ISetup, IWhen)
  private
    fTypeInfo: PTypeInfo;
    fInterceptor: TMockInterceptor;
    fProxy: TValue;
    function GetBehavior: TMockBehavior;
    function GetCallBase: Boolean;
    function GetInstance: TValue;
    function GetTypeInfo: PTypeInfo;
    procedure SetBehavior(const value: TMockBehavior);
    procedure SetCallBase(const value: Boolean);
    function CreateProxy(typeInfo: PTypeInfo;
      const interceptor: TMockInterceptor;
      const constructorArguments: array of TValue): TValue;
  public
    constructor Create(typeInfo: PTypeInfo; const interceptor: TMockInterceptor;
      const proxy: TValue) overload;
    constructor Create(typeInfo: PTypeInfo;
      behavior: TMockBehavior = DefaultMockBehavior); overload;
    constructor Create(typeInfo: PTypeInfo;
      behavior: TMockBehavior; const args: array of TValue); overload;
    destructor Destroy; override;

  {$REGION 'Implements IMock'}
    procedure Reset;

    function Setup: ISetup; overload;
    function Setup(const sequence: IMockSequence): ISetup; overload;

    procedure Received; overload;
    procedure Received(const times: Times); overload;
    procedure Received(const match: TArgMatch); overload;
    procedure Received(const times: Times; const match: TArgMatch); overload;
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
    procedure When; overload;
    procedure When(const match: TArgMatch); overload;
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
    function Setup: ISetup<T>; overload;
    function Setup(const sequence: IMockSequence): ISetup<T>; overload;

    function Received: T; overload;
    function Received(const times: Times): T; overload;
    function Received(const match: TArgMatch): T; overload;
    function Received(const times: Times; const match: TArgMatch): T; overload;
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
    function When: T; overload;
    function When(const match: TArgMatch): T; overload;
  {$ENDREGION}

    property Instance: T read GetInstance;
  end;

  TMockSequence = class(TInterfacedObject, IMockSequence)
  private
    fCount, fCurrent: Integer;
    function GetCompleted: Boolean;
    function GetCurrent: Integer;
  public
    function AddStep: Integer;
    procedure MoveNext;
  end;

implementation

uses
  Spring.Collections,
  Spring.Reflection,
  Spring.ResourceStrings;


{$REGION 'TMock'}

constructor TMock.Create(typeInfo: PTypeInfo; behavior: TMockBehavior);
begin
  Create(typeInfo, behavior, []);
end;

constructor TMock.Create(typeInfo: PTypeInfo; behavior: TMockBehavior;
  const args: array of TValue);
begin
  inherited Create;
  fTypeInfo := typeInfo;
  fInterceptor := TMockInterceptor.Create(behavior);
  fProxy := CreateProxy(typeInfo, fInterceptor, args);
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
  inherited Destroy;
end;

function TMock.CreateProxy(typeInfo: PTypeInfo;
  const interceptor: TMockInterceptor;
  const constructorArguments: array of TValue): TValue;
var
  intf: IInterface;
begin
  case typeInfo.Kind of
    tkClass:
      Result := TProxyGenerator.CreateClassProxy(
        typeInfo.TypeData.ClassType, constructorArguments, [interceptor]);
    tkInterface:
    begin
      Supports(TProxyGenerator.CreateInterfaceProxyWithoutTarget(
        typeInfo, [interceptor]), typeInfo.TypeData.Guid, intf);
      TValue.Make(@intf, typeInfo, Result);
    end;
  else
    raise ENotSupportedException.CreateResFmt(@STypeNotSupported, [typeInfo.TypeName]);
  end;
end;

function TMock.GetBehavior: TMockBehavior;
begin
  Result := fInterceptor.Behavior;
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
  fInterceptor.Executes(
    function(const callInfo: TCallInfo): TValue
    begin
    end);
  Result := Self;
end;

function TMock.Executes(const action: TMockAction): IWhen;
begin
  fInterceptor.Executes(action);
  Result := Self;
end;

function TMock.Raises(const exceptionClass: ExceptClass;
  const msg: string): IWhen;
begin
  fInterceptor.Executes(
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
  fInterceptor.Executes(
    function(const callInfo: TCallInfo): TValue
    begin
      raise exceptionClass.Create(s);
    end);
  Result := Self;
end;

function TMock.Returns(const value: TValue): IWhen;
var
  capturedValue: TValue;
begin
  capturedValue := value;
  fInterceptor.Executes(
    function(const callInfo: TCallInfo): TValue
    begin
      Result := capturedValue;
    end);
  Result := Self;
end;

function TMock.Returns(const values: array of TValue): IWhen;
begin
  fInterceptor.Returns(TArray.Copy<TValue>(values));
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

procedure TMock.Received(const match: TArgMatch);
begin
  fInterceptor.Received(Times.AtLeastOnce, match);
end;

procedure TMock.Received(const times: Times; const match: TArgMatch);
begin
  fInterceptor.Received(times, match);
end;

procedure TMock.Reset;
begin
  fInterceptor.Reset;
end;

procedure TMock.SetBehavior(const value: TMockBehavior);
begin
  fInterceptor.Behavior := value;
end;

procedure TMock.SetCallBase(const value: Boolean);
begin
  fInterceptor.CallBase := value;
end;

function TMock.Setup(const sequence: IMockSequence): ISetup;
begin
  fInterceptor.Sequence := sequence;
  Result := Self;
end;

function TMock.Setup: ISetup;
begin
  fInterceptor.Sequence := nil;
  Result := Self;
end;

procedure TMock.When;
begin
  fInterceptor.When;
end;

procedure TMock.When(const match: TArgMatch);
begin
  fInterceptor.When(match);
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

function TMock<T>.Received(const match: TArgMatch): T;
begin
  inherited Received(match);
  Result := Instance;
end;

function TMock<T>.Received(const times: Times; const match: TArgMatch): T;
begin
  inherited Received(times, match);
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

function TMock<T>.Setup(const sequence: IMockSequence): ISetup<T>;
begin
  inherited Setup(sequence);
  Result := Self;
end;

function TMock<T>.When: T;
begin
  fInterceptor.When;
  Result := Instance;
end;

function TMock<T>.When(const match: TArgMatch): T;
begin
  fInterceptor.When(match);
  Result := Instance;
end;

{$ENDREGION}


{$REGION 'TMockSequence'}

function TMockSequence.AddStep: Integer;
begin
  Result := fCount;
  Inc(fCount);
end;

function TMockSequence.GetCompleted: Boolean;
begin
  Result := (fCurrent > 0) and (fCurrent = fCount);
end;

function TMockSequence.GetCurrent: Integer;
begin
  Result := fCurrent;
end;

procedure TMockSequence.MoveNext;
begin
  Inc(fCurrent);
end;

{$ENDREGION}


end.
