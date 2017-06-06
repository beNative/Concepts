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

unit Spring.Interception.CustomProxy;

interface

uses
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Interception,
  Spring.Interception.AbstractInvocation;

type
  TCustomProxyClass = class of TCustomProxy;
  TCustomProxy = class(TInterfaceBase)
  private type
    TInvocation = class(TAbstractInvocation)
    protected
      procedure InvokeMethodOnTarget; override;
    end;
  private
    fInterceptors: IList<IInterceptor>;
    fInterceptorSelector: IInterceptorSelector;
    fType: TRttiType;
    function FindMethod(addr: Pointer): TRttiMethod;
    function HandleInvoke(addr: Pointer; const args: array of TValue): TValue;
  protected
    function Intercept: TValue; overload;
    function Intercept(const args: array of TValue): TValue; overload;
  public
    constructor Create(const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor);
  end;

implementation

uses
  Spring.Reflection;


{$REGION 'TCustomProxy'}

constructor TCustomProxy.Create(const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor);
begin
  inherited Create;
  fInterceptors := TCollections.CreateInterfaceList<IInterceptor>(interceptors);
  fInterceptorSelector := options.Selector;
  fType := TType.GetType(ClassInfo);
end;

function TCustomProxy.FindMethod(addr: Pointer): TRttiMethod;
var
  method: TRttiMethod;
  delta, candidateDelta: NativeInt;
begin
  Result := nil;
  candidateDelta := High(NativeInt);
  for method in fType.GetMethods do
  begin
    delta := NativeInt(addr) - NativeInt(method.CodeAddress);
    if (delta > 0) and (delta < candidateDelta) then
    begin
      candidateDelta := delta;
      Result := method;
    end;
  end;
  if not Assigned(Result) then
    raise EInterceptionException.CreateFmt('method not found at address %p', [addr]);
end;

function TCustomProxy.HandleInvoke(addr: Pointer;
  const args: array of TValue): TValue;
var
  method: TRttiMethod;
  i: Integer;
  arguments: TArray<TValue>;
  interceptors: TArray<IInterceptor>;
  invocation: IInvocation;
begin
  method := FindMethod(addr);
  arguments := TArray.Copy<TValue>(args);
  interceptors := fInterceptorSelector.SelectInterceptors(method, fInterceptors).ToArray;
  invocation := TInvocation.Create(Self, interceptors, method, arguments);
  try
    invocation.Proceed;
  finally
    for i := 0 to High(args) do
      PValue(@args[i])^ := arguments[i];
    Result := invocation.Result;
  end;
end;

function TCustomProxy.Intercept: TValue;
begin
  Result := HandleInvoke(ReturnAddress, []);
end;

function TCustomProxy.Intercept(const args: array of TValue): TValue;
begin
  Result := HandleInvoke(ReturnAddress, args);
end;

{$ENDREGION}


{$REGION 'TCustomProxy.TInvocation'}

procedure TCustomProxy.TInvocation.InvokeMethodOnTarget;
begin
end;

{$ENDREGION}


end.
