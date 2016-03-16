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

unit Spring.Interception.InterfaceProxy;

interface

uses
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Interception,
  Spring.Interception.AbstractInvocation,
  Spring.VirtualInterface;

type
  TInterfaceProxy = class(TVirtualInterface, IProxyTargetAccessor, IDynamicProxy)
  private
    type
      TInvocation = class(TAbstractInvocation, IChangeProxyTarget)
      protected
        procedure ChangeInvocationTarget(const target: TValue);
        procedure InvokeMethodOnTarget; override;
      end;
  private
    fInterceptors: IList<IInterceptor>;
    fInterceptorSelector: IInterceptorSelector;
    fAdditionalInterfaces: IList<TInterfaceProxy>;
    fTarget: TValue;
    fTypeInfo: PTypeInfo;
    procedure AddAdditionalInterface(typeInfo: PTypeInfo;
      const options: TProxyGenerationOptions);
    function GetInterceptors: IEnumerable<IInterceptor>;
    function GetTarget: TValue;
  protected
    procedure GenerateInterfaces(const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions);
    procedure HandleInvoke(Method: TRttiMethod; const Args: TArray<TValue>;
      out Result: TValue); virtual;
  public
    constructor Create(proxyType: PTypeInfo;
      const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions;
      const target: IInterface;
      const interceptors: array of IInterceptor);

    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  end;

  TAggregatedInterfaceProxy = class(TInterfaceProxy)
  private
    fController: Pointer;
  protected
    function _AddRef: Integer; override;
    function _Release: Integer; override;
  public
    constructor Create(proxyType: PTypeInfo;
      const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions;
      const target: IInterface;
      const interceptors: array of IInterceptor;
      const controller: IInterface);

    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  end;

implementation

uses
  TypInfo,
  Spring.Reflection,
  Spring.ResourceStrings;


{$REGION 'TInterfaceProxy'}

constructor TInterfaceProxy.Create(proxyType: PTypeInfo;
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions; const target: IInterface;
  const interceptors: array of IInterceptor);
begin
  if not proxyType.RttiType.Methods.Any then
    raise EInvalidOperationException.CreateResFmt(
      @STypeParameterContainsNoRtti, [proxyType.Name]);

  inherited Create(proxyType, HandleInvoke);
{$IFDEF AUTOREFCOUNT}
  // Release reference held by ancestor RawCallBack (bypass RSP-10177)
  __ObjRelease;
  // Release reference created by passing closure to HandleInvoke (RSP-10176)
  __ObjRelease;
{$ENDIF}
  fInterceptors := TCollections.CreateInterfaceList<IInterceptor>(interceptors);
  fInterceptorSelector := options.Selector;
  fTarget := TValue.From(target);
  fTypeInfo := proxyType;
  // Do not own the object, let ARC deal with its lifetime. Calling DisposeOf
  // causes an AV since we need to release the refcount above to ever let it
  // release by the main reference (it. the variable containing result of
  // this ctor). Calling DisposeOf will clear the internal data which makes the
  // object free its memory until all references are cleared, once they AR, they
  // could cause an AV. Normal release chain however is immune to that.
  fAdditionalInterfaces := TCollections.CreateObjectList<TInterfaceProxy>
    {$IFDEF AUTOREFCOUNT}(False){$ENDIF};
  GenerateInterfaces(additionalInterfaces, options);
end;

procedure TInterfaceProxy.AddAdditionalInterface(typeInfo: PTypeInfo;
  const options: TProxyGenerationOptions);
begin
  if not fAdditionalInterfaces.Any(
    function(const proxy: TInterfaceProxy): Boolean
    begin
      Result := proxy.fTypeInfo = typeInfo;
    end) then
    fAdditionalInterfaces.Add(TAggregatedInterfaceProxy.Create(
      typeInfo, [], options, nil, fInterceptors.ToArray, Self));
end;

procedure TInterfaceProxy.GenerateInterfaces(
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions);
var
  i: Integer;
begin
  for i := Low(additionalInterfaces) to High(additionalInterfaces) do
    fAdditionalInterfaces.Add(TAggregatedInterfaceProxy.Create(
      additionalInterfaces[i], [], options, nil, fInterceptors.ToArray, Self));
end;

function TInterfaceProxy.GetInterceptors: IEnumerable<IInterceptor>;
begin
  Result := fInterceptors;
end;

function TInterfaceProxy.GetTarget: TValue;
begin
  Result := fTarget;
end;

procedure TInterfaceProxy.HandleInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  arguments: TArray<TValue>;
  interceptors: TArray<IInterceptor>;
  invocation: IInvocation;
  i: Integer;
begin
  arguments := Copy(Args, 1);
  interceptors := fInterceptorSelector.SelectInterceptors(method, fInterceptors).ToArray;
  invocation := TInvocation.Create(fTarget, interceptors, Method, arguments);
  try
    invocation.Proceed;
  finally
    for i := 1 to High(Args) do
      Args[i] := arguments[i - 1];
    Result := invocation.Result;
  end;
end;

function TInterfaceProxy.QueryInterface(const IID: TGUID; out Obj): HResult;
var
  i: Integer;
begin
  Result := inherited;
  if Result = S_OK then
    Exit;
  if not fTarget.IsEmpty then
  begin
    Result := fTarget.AsInterface.QueryInterface(IID, Obj);
    if Result = S_OK then
      Exit;
  end;
  for i := 0 to fAdditionalInterfaces.Count - 1 do
    if fAdditionalInterfaces[i].QueryInterface(IID, obj) = S_OK then
      Exit(S_OK);
end;

{$ENDREGION}


{$REGION 'TInterfaceProxy.TInvocation'}

procedure TInterfaceProxy.TInvocation.ChangeInvocationTarget(
  const target: TValue);
begin
  fTarget := target;
end;

procedure TInterfaceProxy.TInvocation.InvokeMethodOnTarget;
begin
  if not fTarget.IsEmpty then
    fResult := fMethod.Invoke(fTarget, fArguments)
  else
    raise ENotImplementedException.Create(fMethod.Parent.DefaultName + '.' + fMethod.Name);
end;

{$ENDREGION}


{$REGION 'TAggregatedInterfaceProxy'}

constructor TAggregatedInterfaceProxy.Create(proxyType: PTypeInfo;
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions; const target: IInterface;
  const interceptors: array of IInterceptor; const controller: IInterface);
begin
  inherited Create(proxyType, additionalInterfaces, options, target, interceptors);
  fController := Pointer(controller);
end;

function TAggregatedInterfaceProxy.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  Result := inherited;
  if Result <> S_OK then
    Result := IInterface(FController).QueryInterface(IID, Obj);
end;

function TAggregatedInterfaceProxy._AddRef: Integer;
begin
  Result := IInterface(FController)._AddRef;
end;

function TAggregatedInterfaceProxy._Release: Integer;
begin
  Result := IInterface(FController)._Release;
end;

{$ENDREGION}


end.
