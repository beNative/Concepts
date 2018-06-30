{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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

unit Spring.Interception.ClassProxy;

interface

uses
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Interception,
  Spring.Interception.AbstractInvocation,
  Spring.MethodIntercept,
  Spring.VirtualClass;

type
  TClassProxy = class(TVirtualClass)
  private
    type
      TInvocation = class(TAbstractInvocation)
      protected
        procedure InvokeMethodOnTarget; override;
      end;

      TProxyTargetAccessor = class(TInterfacedObject, IProxyTargetAccessor)
      private
        fInterceptors: IEnumerable<IInterceptor>;
        fTarget: TValue;
        function GetInterceptors: IEnumerable<IInterceptor>;
        function GetTarget: TValue;
      public
        constructor Create(const target: TValue;
          const interceptors: IEnumerable<IInterceptor>);
      end;

    class var fProxies: IDictionary<Pointer, TClassProxy>;
  private
    fIntercepts: IList<TMethodIntercept>;
    fInterceptors: IList<IInterceptor>;
    fInterceptorSelector: IInterceptorSelector;
    fAdditionalInterfaces: TArray<IInterface>;
    function GetProxyTargetAccessor: IProxyTargetAccessor;
    class procedure ProxyFreeInstance(const Self: TObject); static;
  protected
    function CollectInterceptableMethods(
      const hook: IProxyGenerationHook): IEnumerable<TRttiMethod>;
    procedure GenerateIntercepts(const options: TProxyGenerationOptions);
    procedure GenerateInterfaces(const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions);
    procedure HandleInvoke(UserData: Pointer; const Args: TArray<TValue>;
      out Result: TValue); virtual;
  public
    constructor Create(proxyType: TClass;
      const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor);
    destructor Destroy; override;
    class constructor Create;

    function CreateInstance: TObject; overload;
    function CreateInstance(const constructorArguments: array of TValue): TObject; overload;
  end;

implementation

uses
  Generics.Collections,
  SysUtils,
  TypInfo,
  Spring.Interception.InterfaceProxy,
  Spring.Reflection;


{$REGION 'TClassProxy'}

constructor TClassProxy.Create(proxyType: TClass;
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor);
begin
  inherited Create(proxyType);
  ProxyClassData.FreeInstance := ProxyFreeInstance;
  fIntercepts := TCollections.CreateObjectList<TMethodIntercept>;
  fInterceptors := TCollections.CreateInterfaceList<IInterceptor>(interceptors);
  fInterceptorSelector := options.Selector;
  GenerateIntercepts(options);
  GenerateInterfaces(additionalInterfaces, options);
end;

destructor TClassProxy.Destroy;
begin
  FreeMem(ProxyClassData.IntfTable);
  inherited Destroy;
end;

class constructor TClassProxy.Create;
begin
  fProxies := TCollections.CreateDictionary<Pointer, TClassProxy>([doOwnsValues]);
end;

function TClassProxy.CreateInstance: TObject;
begin
  Result := CreateInstance([]);
end;

function TClassProxy.CreateInstance(
  const constructorArguments: array of TValue): TObject;
var
  table: PInterfaceTable;
  i: Integer;
begin
  Result := TActivator.CreateInstance(ProxyClass, constructorArguments);
  fProxies.Add(Result, Self);

  table := ProxyClassData.IntfTable;
  for i := 1 to table.EntryCount - 1 do
    PPointer(@PByte(Result)[table.Entries[i].ImplGetter and $00FFFFFF])^ :=
      Pointer(fAdditionalInterfaces[i - 1]);
end;

function TClassProxy.CollectInterceptableMethods(
  const hook: IProxyGenerationHook): IEnumerable<TRttiMethod>;
begin
  Result := TType.GetType(ProxyClass).Methods.Where(
    function(const method: TRttiMethod): Boolean
    begin
      if not method.HasExtendedInfo then
        Exit(False);
      if not (method.MethodKind in [mkFunction, mkProcedure]) then
        Exit(False);
      if method.DispatchKind <> dkVtable then
      begin
        hook.NonVirtualMemberNotification(method);
        Exit(False);
      end;
      if method.VirtualIndex < 0 then
        Exit(False);
      if fIntercepts[method.VirtualIndex] <> nil then
        Exit(False);
      Result := not Assigned(hook) or hook.ShouldInterceptMethod(method);
    end);
end;

procedure TClassProxy.HandleInvoke(UserData: Pointer;
  const Args: TArray<TValue>; out Result: TValue);
var
  method: TRttiMethod;
  arguments: TArray<TValue>;
  interceptors: TArray<IInterceptor>;
  invocation: IInvocation;
  i: Integer;
begin
  method := TMethodIntercept(UserData).Method;
  arguments := Copy(Args, 1);
  interceptors := fInterceptorSelector.SelectInterceptors(method, fInterceptors).ToArray;
  invocation := TInvocation.Create(Args[0], interceptors, method, arguments);
  try
    invocation.Proceed;
  finally
    for i := 1 to High(Args) do
      Args[i] := arguments[i - 1];
    Result := invocation.Result;
  end;
end;

procedure TClassProxy.GenerateIntercepts(const options: TProxyGenerationOptions);
var
  method: TRttiMethod;
  virtualMethodCount: Integer;
  intercept: TMethodIntercept;
begin
  virtualMethodCount := GetVirtualMethodCount(ProxyClass);
  fIntercepts.Count := virtualMethodCount;

  for method in CollectInterceptableMethods(options.Hook) do
  begin
    intercept := TMethodIntercept.Create(method, HandleInvoke);
    fIntercepts[method.VirtualIndex] := intercept;
    PVirtualMethodTable(ProxyClass)[method.VirtualIndex] := intercept.CodeAddress;
  end;
{$IFDEF AUTOREFCOUNT}
  // Release reference created by passing closure to HandleInvoke (RSP-10176)
  if Assigned(intercept) then
    __ObjRelease;
{$ENDIF}
end;

procedure TClassProxy.GenerateInterfaces(
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions);
var
  entryCount, size: Integer;
  table: PInterfaceTable;
  i, index: Integer;
  offset: NativeUInt;

  interfaces: IEnumerable<TRttiInterfaceType>;
  intf: TRttiInterfaceType;
  additionalInterface: PTypeInfo;
begin
  entryCount := 1 + Length(additionalInterfaces);
  for i := 0 to options.Mixins.Count - 1 do
    Inc(entryCount, TType.GetType(options.Mixins[i].ClassType).GetInterfaces.Count);
  size := SizeOf(Integer) + SizeOf(TInterfaceEntry) * EntryCount;
{$IFDEF CPU64BITS}
  Inc(size, SizeOf(Int32));
{$ENDIF}
  GetMem(table, size);
  table.EntryCount := entryCount;
  ProxyClassData.IntfTable := table;

  // add IProxyTargetAccessor
  table.Entries[0].IID := IProxyTargetAccessor;
  table.Entries[0].VTable := nil;
  table.Entries[0].IOffset := 0;
  table.Entries[0].ImplGetter := NativeUInt(@TClassProxy.GetProxyTargetAccessor);

  SetLength(fAdditionalInterfaces, entryCount);

  offset := ProxyClassData.InstanceSize - hfFieldSize;
  Inc(ProxyClassData.InstanceSize, (entryCount - 1) * SizeOf(Pointer));

  // add other interfaces
  for i := 1 to Length(additionalInterfaces) do
  begin
    additionalInterface := additionalInterfaces[i - 1];
    TInterfaceProxy.Create(additionalInterface, [], options, nil,
      fInterceptors.ToArray).QueryInterface(
      additionalInterface.TypeData.Guid, fAdditionalInterfaces[i - 1]);
    table.Entries[i].IID := additionalInterfaces[i - 1].TypeData.Guid;
    table.Entries[i].VTable := nil;
    table.Entries[i].IOffset := 0;
{$IFDEF CPU64BITS}
    table.Entries[i].ImplGetter := offset or $FF00000000000000;
{$ELSE}
    table.Entries[i].ImplGetter := offset or $FF000000;
{$ENDIF}
    Inc(offset, SizeOf(Pointer));
  end;

  index := Length(additionalInterfaces) + 1;
  for i := 0 to options.Mixins.Count - 1 do
  begin
    interfaces := TType.GetType(options.Mixins[i].ClassType).GetInterfaces;
    for intf in interfaces do
    begin
      Supports(options.Mixins[i], intf.Guid, fAdditionalInterfaces[index]);

      table.Entries[index].IID := intf.GUID;
      table.Entries[index].VTable := nil;
      table.Entries[index].IOffset := 0;
{$IFDEF CPU64BITS}
      table.Entries[index].ImplGetter := offset or $FF00000000000000;
{$ELSE}
      table.Entries[index].ImplGetter := offset or $FF000000;
{$ENDIF}

      Inc(offset, SizeOf(Pointer));
      Inc(index);
    end;
  end;
end;

function TClassProxy.GetProxyTargetAccessor: IProxyTargetAccessor;
begin
  // Do not access any instance members here!!!
  // Self is not a TClassProxy but this method needs to be compatible with
  // function: IInterface of object as declared in System.InvokeImplGetter
  Result := TProxyTargetAccessor.Create(Self, fProxies[Self].fInterceptors);
end;

class procedure TClassProxy.ProxyFreeInstance(const Self: TObject);
begin
  GetClassData(ClassParent).FreeInstance(Self); // inherited
  fProxies.Remove(Self);
end;

{$ENDREGION}


{$REGION 'TClassProxy.TInvocation'}

procedure TClassProxy.TInvocation.InvokeMethodOnTarget;
var
  params: TArray<TRttiParameter>;
  args: TArray<TValue>;
  i: Integer;
begin
  params := fMethod.GetParameters;
  SetLength(args, Length(fArguments) + 1);
  args[0] := fTarget;

  // convert arguments for Invoke call (like done in the DispatchInvoke methods
  for i := Low(fArguments) to High(fArguments) do
    PassArg(params[i], fArguments[i], args[i + 1], fMethod.CallingConvention);

  if not fTarget.IsEmpty then
    fResult := Rtti.Invoke(fMethod.CodeAddress, args, fMethod.CallingConvention, fMethod.ReturnTypeHandle);
end;

{$ENDREGION}


{$REGION 'TClassProxy.TProxyTargetAccessor'}

constructor TClassProxy.TProxyTargetAccessor.Create(const target: TValue;
  const interceptors: IEnumerable<IInterceptor>);
begin
  inherited Create;
  fInterceptors := interceptors;
  fTarget := target;
end;

function TClassProxy.TProxyTargetAccessor.GetInterceptors: IEnumerable<IInterceptor>;
begin
  Result := fInterceptors;
end;

function TClassProxy.TProxyTargetAccessor.GetTarget: TValue;
begin
  Result := fTarget;
end;

{$ENDREGION}


end.
