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

unit Spring.Interception;

interface

uses
  Rtti,
  SysUtils,
  Spring,
  Spring.Collections;

type
  EInterceptionException = class(Exception);

  IInvocation = interface
    ['{A307FB1B-CA96-4B20-84FE-A71B286F0924}']
    function GetArguments: TArray<TValue>;
    function GetMethod: TRttiMethod;
    function GetResult: TValue;
    function GetTarget: TValue;
    procedure SetResult(const value: TValue);

    procedure Proceed;

    property Arguments: TArray<TValue> read GetArguments;
    property Method: TRttiMethod read GetMethod;
    property Result: TValue read GetResult write SetResult;
    property Target: TValue read GetTarget;
  end;

  IInterceptor = interface
    ['{B33137BC-AAEE-40D7-86F3-BD6A65F4F7C7}']
    procedure Intercept(const invocation: IInvocation);
  end;

  IProxyTargetAccessor = interface
    ['{4A1EBB4E-ED31-44FB-AA87-E349610DC82F}']
    function GetInterceptors: IEnumerable<IInterceptor>;
    function GetTarget: TValue;
  end;

  IChangeProxyTarget = interface
    ['{2578A89C-8485-4F96-B841-E4FB849F3F9B}']
    procedure ChangeInvocationTarget(const target: TValue);
  end;

  IProxyGenerationHook = interface
    ['{53C88475-2A5A-4FC3-B0AE-04CB85758746}']
    procedure NonVirtualMemberNotification(const method: TRttiMethod);
    function ShouldInterceptMethod(const method: TRttiMethod): Boolean;
  end;

  IInterceptorSelector = interface
    ['{4BE1377C-2710-4867-8CCF-E098FF0DC783}']
    function SelectInterceptors(const method: TRttiMethod;
      const interceptors: IEnumerable<IInterceptor>): IEnumerable<IInterceptor>;
  end;

  TProxyGenerationOptions = record
  private
    type
      TAllMethodsHook = class(TInterfacedObject, IProxyGenerationHook)
      public
        procedure NonVirtualMemberNotification(const method: TRttiMethod);
        function ShouldInterceptMethod(const method: TRttiMethod): Boolean;
      end;

      TAllInterceptorsSelector = class(TInterfacedObject, IInterceptorSelector)
      public
        function SelectInterceptors(const method: TRttiMethod;
          const interceptors: IEnumerable<IInterceptor>): IEnumerable<IInterceptor>;
      end;
  private
    fHook: IProxyGenerationHook;
    fSelector: IInterceptorSelector;
    fMixins: IList<TObject>;
  public
    constructor Create(const hook: IProxyGenerationHook);
    class function Default: TProxyGenerationOptions; static;
    procedure AddMixinInstance(const instance: TObject);
    property Hook: IProxyGenerationHook read fHook write fHook;
    property Mixins: IList<TObject> read fMixins;
    property Selector: IInterceptorSelector read fSelector write fSelector;
  end;

  IDynamicProxy = interface
    ['{7DDA3562-3F82-4FA0-9869-9FB27E0CF89D}']
    procedure AddAdditionalInterface(typeInfo: PTypeInfo;
      const options: TProxyGenerationOptions);
  end;

  TProxyGenerator = record
  public
    class function CreateClassProxy<T: class>(
      const interceptors: array of IInterceptor): T; overload; static;
    class function CreateClassProxy<T: class>(
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): T; overload; static;

    class function CreateClassProxy(classType: TClass;
      const additionalInterfaces: array of PTypeInfo;
      const interceptors: array of IInterceptor): TObject; overload; static;
    class function CreateClassProxy(classType: TClass;
      const options: TProxyGenerationOptions;
      const constructorArguments: array of TValue;
      const interceptors: array of IInterceptor): TObject; overload; static;
    class function CreateClassProxy(classType: TClass;
      const constructorArguments: array of TValue;
      const interceptors: array of IInterceptor): TObject; overload; static;
    class function CreateClassProxy(classType: TClass;
      const interceptors: array of IInterceptor): TObject; overload; static;
    class function CreateClassProxy(classType: TClass;
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): TObject; overload; static;
    class function CreateClassProxy(classType: TClass;
      const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): TObject; overload; static;
    class function CreateClassProxy(classType: TClass;
      const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions;
      const constructorArguments: array of TValue;
      const interceptors: array of IInterceptor): TObject; overload; static;

    class function CreateInterfaceProxyWithTarget<T: IInterface>(const target: T;
      const interceptors: array of IInterceptor): T; overload; static;
    class function CreateInterfaceProxyWithTarget<T: IInterface>(const target: T;
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): T; overload; static;

    class function CreateInterfaceProxyWithTarget(proxyType: PTypeInfo;
      const target: IInterface;
      const interceptors: array of IInterceptor): TObject; overload; static;
    class function CreateInterfaceProxyWithTarget(proxyType: PTypeInfo;
      const target: IInterface; const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): TObject; overload; static;
    class function CreateInterfaceProxyWithTarget(proxyType: PTypeInfo;
      const additionalInterfaces: array of PTypeInfo;
      const target: IInterface;
      const interceptors: array of IInterceptor): TObject; overload; static;
    class function CreateInterfaceProxyWithTarget(proxyType: PTypeInfo;
      const additionalInterfaces: array of PTypeInfo;
      const target: IInterface; const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): TObject; overload; static;

    class function CreateInterfaceProxyWithoutTarget<T: IInterface>(
      const interceptor: IInterceptor): T; overload; static;
    class function CreateInterfaceProxyWithoutTarget<T: IInterface>(
      const interceptors: array of IInterceptor): T; overload; static;
    class function CreateInterfaceProxyWithoutTarget<T: IInterface>(
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): T; overload; static;

    class function CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
      const interceptor: IInterceptor): TObject; overload; static;
    class function CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
      const interceptors: array of IInterceptor): TObject; overload; static;
    class function CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
      const additionalInterfaces: array of PTypeInfo;
      const interceptors: array of IInterceptor): TObject; overload; static;
    class function CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): TObject; overload; static;
    class function CreateInterfaceProxyWithoutTarget(proxyType: PTypeInfo;
      const additionalInterfaces: array of PTypeInfo;
      const options: TProxyGenerationOptions;
      const interceptors: array of IInterceptor): TObject; overload; static;
  end;

implementation

uses
  TypInfo,
  Spring.Interception.ClassProxy,
  Spring.Interception.CustomProxy,
  Spring.Interception.InterfaceProxy;


{$REGION 'TProxyGenerator'}

class function TProxyGenerator.CreateClassProxy<T>(
  const interceptors: array of IInterceptor): T;
begin
  Result := T(CreateClassProxy(
    TClass(T), TProxyGenerationOptions.Default, interceptors));
end;

class function TProxyGenerator.CreateClassProxy<T>(
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): T;
begin
  Result := T(CreateClassProxy(TClass(T), options, interceptors));
end;

class function TProxyGenerator.CreateClassProxy(classType: TClass;
  const additionalInterfaces: array of PTypeInfo;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateClassProxy(classType, additionalInterfaces,
    TProxyGenerationOptions.Default, interceptors);
end;

class function TProxyGenerator.CreateClassProxy(classType: TClass;
  const options: TProxyGenerationOptions;
  const constructorArguments: array of TValue;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateClassProxy(
    classType, [], options, constructorArguments, interceptors);
end;

class function TProxyGenerator.CreateClassProxy(classType: TClass;
  const constructorArguments: array of TValue;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateClassProxy(classType, [], TProxyGenerationOptions.Default,
    constructorArguments, interceptors);
end;

class function TProxyGenerator.CreateClassProxy(classType: TClass;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateClassProxy(
    classType, [], TProxyGenerationOptions.Default, [], interceptors);
end;

class function TProxyGenerator.CreateClassProxy(classType: TClass;
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateClassProxy(classType, [], options, [], interceptors);
end;

class function TProxyGenerator.CreateClassProxy(classType: TClass;
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateClassProxy(
    classType, additionalInterfaces, options, [], interceptors);
end;

class function TProxyGenerator.CreateClassProxy(classType: TClass;
  const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions;
  const constructorArguments: array of TValue;
  const interceptors: array of IInterceptor): TObject;
var
  proxy: TClassProxy;
begin
  if classType.InheritsFrom(TCustomProxy) then
    Result := TCustomProxyClass(classType).Create(options, interceptors)
  else
  begin
    proxy := TClassProxy.Create(
      classType, additionalInterfaces, options, interceptors);
    Result := proxy.CreateInstance(constructorArguments);
  end;
end;

class function TProxyGenerator.CreateInterfaceProxyWithTarget<T>(
  const target: T; const interceptors: array of IInterceptor): T;
begin
  Supports(CreateInterfaceProxyWithTarget(TypeInfo(T), IInterface(target),
    TProxyGenerationOptions.Default, interceptors),
    GetTypeData(TypeInfo(T)).Guid, Result);
end;

class function TProxyGenerator.CreateInterfaceProxyWithTarget<T>(
  const target: T; const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): T;
begin
  Supports(CreateInterfaceProxyWithTarget(
    TypeInfo(T), IInterface(target), options, interceptors),
    GetTypeData(TypeInfo(T)).Guid, Result);
end;

class function TProxyGenerator.CreateInterfaceProxyWithTarget(
  proxyType: PTypeInfo; const target: IInterface;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithTarget(
    proxyType, [], target, TProxyGenerationOptions.Default, interceptors);
end;

class function TProxyGenerator.CreateInterfaceProxyWithTarget(
  proxyType: PTypeInfo; const target: IInterface;
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithTarget(
    proxyType, [], target, options, interceptors);
end;

class function TProxyGenerator.CreateInterfaceProxyWithTarget(
  proxyType: PTypeInfo; const additionalInterfaces: array of PTypeInfo;
  const target: IInterface; const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithTarget(proxyType, additionalInterfaces,
    target, TProxyGenerationOptions.Default, interceptors);
end;

class function TProxyGenerator.CreateInterfaceProxyWithTarget(
  proxyType: PTypeInfo; const additionalInterfaces: array of PTypeInfo;
  const target: IInterface; const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := TInterfaceProxy.Create(
    proxyType, additionalInterfaces, options, target, interceptors);
end;

class function TProxyGenerator.CreateInterfaceProxyWithoutTarget<T>(
  const interceptor: IInterceptor): T;
begin
  Supports(CreateInterfaceProxyWithoutTarget(TypeInfo(T), interceptor),
    GetTypeData(TypeInfo(T)).Guid, Result);
end;

class function TProxyGenerator.CreateInterfaceProxyWithoutTarget<T>(
  const interceptors: array of IInterceptor): T;
begin
  Supports(CreateInterfaceProxyWithoutTarget(TypeInfo(T), interceptors),
    GetTypeData(TypeInfo(T)).Guid, Result);
end;

class function TProxyGenerator.CreateInterfaceProxyWithoutTarget<T>(
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): T;
begin
  Supports(CreateInterfaceProxyWithoutTarget(TypeInfo(T), options, interceptors),
    GetTypeData(TypeInfo(T)).Guid, Result);
end;

class function TProxyGenerator.CreateInterfaceProxyWithoutTarget(
  proxyType: PTypeInfo; const interceptor: IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithoutTarget(
    proxyType, [], TProxyGenerationOptions.Default, [interceptor]);
end;

class function TProxyGenerator.CreateInterfaceProxyWithoutTarget(
  proxyType: PTypeInfo; const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithoutTarget(
    proxyType, [], TProxyGenerationOptions.Default, interceptors);
end;

class function TProxyGenerator.CreateInterfaceProxyWithoutTarget(
  proxyType: PTypeInfo; const additionalInterfaces: array of PTypeInfo;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithoutTarget(
    proxyType, additionalInterfaces, TProxyGenerationOptions.Default, interceptors);
end;

class function TProxyGenerator.CreateInterfaceProxyWithoutTarget(
  proxyType: PTypeInfo; const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := CreateInterfaceProxyWithoutTarget(
    proxyType, [], options, interceptors);
end;

class function TProxyGenerator.CreateInterfaceProxyWithoutTarget(
  proxyType: PTypeInfo; const additionalInterfaces: array of PTypeInfo;
  const options: TProxyGenerationOptions;
  const interceptors: array of IInterceptor): TObject;
begin
  Result := TInterfaceProxy.Create(
    proxyType, additionalInterfaces, options, nil, interceptors);
end;

{$ENDREGION}


{$REGION 'TProxyGenerationOptions'}

constructor TProxyGenerationOptions.Create(
  const hook: IProxyGenerationHook);
begin
  fHook := hook;
  fMixins := TCollections.CreateObjectList<TObject>(False);
end;

class function TProxyGenerationOptions.Default: TProxyGenerationOptions;
begin
  Result := TProxyGenerationOptions.Create(TAllMethodsHook.Create);
  Result.Selector := TAllInterceptorsSelector.Create;
end;

procedure TProxyGenerationOptions.AddMixinInstance(const instance: TObject);
begin
  Guard.CheckNotNull(instance, 'instance');
  fMixins.Add(instance);
end;

{$ENDREGION}


{$REGION 'TProxyGenerationOptions.TAllMethodsHook'}

procedure TProxyGenerationOptions.TAllMethodsHook.NonVirtualMemberNotification(
  const method: TRttiMethod);
begin
end;

function TProxyGenerationOptions.TAllMethodsHook.ShouldInterceptMethod(
  const method: TRttiMethod): Boolean;
begin
  Result := True;
end;

{$ENDREGION}


{$REGION 'TProxyGenerationOptions.TAllInterceptorsSelector'}

function TProxyGenerationOptions.TAllInterceptorsSelector.SelectInterceptors(
  const method: TRttiMethod;
  const interceptors: IEnumerable<IInterceptor>): IEnumerable<IInterceptor>;
begin
  Result := interceptors;
end;

{$ENDREGION}


end.
