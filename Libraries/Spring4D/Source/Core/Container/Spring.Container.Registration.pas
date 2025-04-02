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

unit Spring.Container.Registration;

interface

uses
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Collections.Events,
  Spring.VirtualInterface,
  Spring.Container.Common,
  Spring.Container.Core;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}{$ENDIF}

type
  /// <summary>
  ///   TComponentRegistry
  /// </summary>
  TComponentRegistry = class(TInterfacedObject, IComponentRegistry)
  private
    fKernel: TKernel;
    fModels: IList<TComponentModel>;
    fDefaultRegistrations: IDictionary<PTypeInfo, TComponentModel>;
    fUnnamedRegistrations: IMultiMap<PTypeInfo, TComponentModel>;
    fServiceTypeMappings: IMultiMap<PTypeInfo, TComponentModel>;
    fServiceNameMappings: IDictionary<string, TComponentModel>;
    fOnChanged: TCollectionChangedEventImpl<TComponentModel>;
    function GetOnChanged: ICollectionChangedEvent<TComponentModel>;
  protected
    function InternalResolveParams(const method: TRttiMethod;
      const args: TArray<TValue>; paramResolution: TParamResolution): TArray<TValue>;
    procedure InternalRegisterFactory(const model: TComponentModel;
      const invokeEvent: TVirtualInterfaceInvokeEvent);
    procedure RegisterUnnamed(const model: TComponentModel; serviceType: PTypeInfo);
    procedure Validate(const componentType, serviceType: TRttiType; var serviceName: string);
  public
    constructor Create(const kernel: TKernel);
    destructor Destroy; override;

    function RegisterComponent(componentTypeInfo: PTypeInfo): TComponentModel;
    procedure RegisterService(const model: TComponentModel; serviceType: PTypeInfo;
      const serviceName: string = '');
    procedure RegisterDefault(const model: TComponentModel; serviceType: PTypeInfo);

    procedure RegisterFactory(const model: TComponentModel); overload;
    procedure RegisterFactory(const model: TComponentModel;
      paramResolution: TParamResolution); overload;
    procedure RegisterFactory(const model: TComponentModel;
      const resolvedServiceName: string); overload;
    procedure RegisterFactory(const model: TComponentModel;
      const resolvedServiceName: string;
      paramResolution: TParamResolution); overload;

    procedure UnregisterAll;
    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(const serviceName: string): Boolean; overload;
    function HasService(serviceType: PTypeInfo; const serviceName: string): Boolean; overload;
    function HasDefault(serviceType: PTypeInfo): Boolean;
    function FindOne(componentType: PTypeInfo): TComponentModel; overload;
    function FindOne(const serviceName: string): TComponentModel; overload;
    function FindOne(serviceType: PTypeInfo; const argument: TValue): TComponentModel; overload;
    function FindDefault(serviceType: PTypeInfo): TComponentModel;
    function FindAll: IEnumerable<TComponentModel>; overload;
    function FindAll(serviceType: PTypeInfo): IEnumerable<TComponentModel>; overload;
  end;

  /// <summary>
  ///   Internal helper for non-generic fluent style registration of a type.
  /// </summary>
  TRegistration = class
  private
    function GetKernel: TKernel; inline;
    function GetModel: TComponentModel; inline;
    procedure InterceptedBy(const interceptorRef: TInterceptorReference; where: TWhere); overload;
    property Kernel: TKernel read GetKernel;
  public
    function Implements(serviceType: PTypeInfo): TRegistration; overload;
    function Implements(serviceType: PTypeInfo; const serviceName: string): TRegistration; overload;
    function Implements<TServiceType>: TRegistration; overload; inline;
    function Implements<TServiceType>(const serviceName: string): TRegistration; overload; inline;

    {$REGION 'Typed Injections'}

    function InjectConstructor: TRegistration; overload;
    function InjectConstructor(const parameterTypes: array of PTypeInfo): TRegistration; overload;
    function InjectProperty(const propertyName: string): TRegistration; overload;
    function InjectMethod(const methodName: string): TRegistration; overload;
    function InjectMethod(const methodName: string; const parameterTypes: array of PTypeInfo): TRegistration; overload;
    function InjectField(const fieldName: string): TRegistration; overload;

    function InjectParameter(const parameterName: string; const value: TValue): TRegistration; overload;
    function InjectParameter(parameterType: PTypeInfo; const value: TValue): TRegistration; overload;
    function InjectParameter<TParameterType>(const value: TParameterType): TRegistration; overload; inline;
    function InjectParameter<TParameterType>(const serviceName: string): TRegistration; overload; inline;

    {$ENDREGION}

    {$REGION 'Named/Valued Injections'}

    function InjectConstructor(const arguments: array of TValue): TRegistration; overload;
    function InjectProperty(const propertyName: string; const value: TValue): TRegistration; overload;
    function InjectMethod(const methodName: string; const arguments: array of TValue): TRegistration; overload;
    function InjectField(const fieldName: string; const value: TValue): TRegistration; overload;

    {$ENDREGION}

    function AsSingleton(refCounting: TRefCounting = TRefCounting.Unknown): TRegistration;
    function AsSingletonPerThread(refCounting: TRefCounting = TRefCounting.Unknown): TRegistration;
    function AsTransient: TRegistration;
    function AsPooled(minPoolSize, maxPoolSize: Integer): TRegistration;

    function PerResolve: TRegistration;

    function AsDefault: TRegistration; overload;
    function AsDefault(serviceType: PTypeInfo): TRegistration; overload;
    function AsDefault<TServiceType>: TRegistration; overload; inline;

    function AsCustom(const lifetimeManager: IInterface): TRegistration; overload;
    function AsCustom<TLifetimeManagerType: class, constructor, ILifetimeManager>: TRegistration; overload; inline;

    function AsFactory: TRegistration; overload;
    function AsFactory(paramResolution: TParamResolution): TRegistration; overload;
    function AsFactory(const resolvedServiceName: string): TRegistration; overload;
    function AsFactory(const resolvedServiceName: string; paramResolution: TParamResolution): TRegistration; overload;

    function InterceptedBy(interceptorType: PTypeInfo;
      where: TWhere = TWhere.Last): TRegistration; overload;
    function InterceptedBy(const name: string;
      where: TWhere = TWhere.Last): TRegistration; overload;
    function InterceptedBy<TInterceptorType>(
      where: TWhere = TWhere.Last): TRegistration; overload; inline;

    property Model: TComponentModel read GetModel;
  end;

implementation

uses
  Generics.Defaults,
  TypInfo,
  Spring.Collections.Lists,
  Spring.Comparers,
  Spring.Container.Resolvers,
  Spring.Container.ResourceStrings,
  Spring.Reflection,
  StrUtils;

function GetDefaultParamResolution(factoryType: TRttiType): TParamResolution;
begin
  if StartsStr('Spring.Func<', factoryType.DefaultName) then
    Result := TParamResolution.ByType
  else
    Result := TParamResolution.ByName;
end;


{$REGION 'TComponentRegistry'}

constructor TComponentRegistry.Create(const kernel: TKernel);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(kernel, 'kernel');
{$ENDIF}

  inherited Create;
  fKernel := kernel;
  fOnChanged := TCollectionChangedEventImpl<TComponentModel>.Create;
  fModels := TCollections.CreateObjectList<TComponentModel>(True);
  fModels.OnChanged.Add(fOnChanged.Invoke);
  fDefaultRegistrations := TCollections.CreateDictionary<PTypeInfo, TComponentModel>(
    IEqualityComparer<PTypeInfo>(GetTypeInfoEqualityComparer));;
  fDefaultRegistrations.OnValueChanged.Add(fOnChanged.Invoke);
  fUnnamedRegistrations := TCollections.CreateMultiMap<PTypeInfo, TComponentModel>(
    IEqualityComparer<PTypeInfo>(GetTypeInfoEqualityComparer));;
  fUnnamedRegistrations.OnValueChanged.Add(fOnChanged.Invoke);
  fServiceTypeMappings := TCollections.CreateMultiMap<PTypeInfo, TComponentModel>(
    IEqualityComparer<PTypeInfo>(GetTypeInfoEqualityComparer));;
  fServiceTypeMappings.OnValueChanged.Add(fOnChanged.Invoke);
  fServiceNameMappings := TCollections.CreateDictionary<string, TComponentModel>;
  fServiceNameMappings.OnValueChanged.Add(fOnChanged.Invoke);
end;

destructor TComponentRegistry.Destroy;
begin
  fOnChanged.Free;
  inherited;
end;

procedure TComponentRegistry.Validate(const componentType, serviceType: TRttiType;
  var serviceName: string);
begin
  if not serviceType.IsAssignableFrom(componentType)
    and not componentType.IsInterface then
    raise ERegistrationException.CreateResFmt(@SIncompatibleTypes, [
      componentType.DefaultName, serviceType.DefaultName]);
  if serviceName = '' then
    serviceName := serviceType.DefaultName + '@' + componentType.DefaultName;
  if HasService(serviceName) then
    raise ERegistrationException.CreateResFmt(@SDuplicateServiceName, [serviceName]);
end;

procedure TComponentRegistry.UnregisterAll;
begin
  fOnChanged.Enabled := False;
  try
    fServiceNameMappings.Clear;
    fServiceTypeMappings.Clear;
    fDefaultRegistrations.Clear;
    fUnnamedRegistrations.Clear;
    fModels.Clear;
  finally
    fOnChanged.Enabled := True;
  end;
end;

procedure TComponentRegistry.RegisterService(const model: TComponentModel;
  serviceType: PTypeInfo; const serviceName: string);
var
  internalServiceName: string;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(model, 'model');
  Guard.CheckNotNull(serviceType, 'serviceType');
{$ENDIF}

  internalServiceName := serviceName;
  Validate(model.ComponentType, serviceType.RttiType, internalServiceName);
  model.Services[internalServiceName] := serviceType;
  fServiceTypeMappings.Add(serviceType, model);
  fServiceNameMappings.Add(internalServiceName, model);
  if serviceName = '' then
  begin
    RegisterDefault(model, serviceType);
    RegisterUnnamed(model, serviceType);
  end;
end;

procedure TComponentRegistry.RegisterUnnamed(const model: TComponentModel;
  serviceType: PTypeInfo);
begin
  fUnnamedRegistrations.Add(serviceType, model);
end;

procedure TComponentRegistry.RegisterDefault(const model: TComponentModel;
  serviceType: PTypeInfo);
begin
  if not model.HasService(serviceType) then
    raise ERegistrationException.CreateResFmt(@SServiceNotFound, [
      serviceType.TypeName]);
  fDefaultRegistrations[serviceType] := model;
end;

type
  TVirtualInterfaceHack = class(TInterfacedObject)
  private
    VTable: PVTable;
  end;

function TComponentRegistry.InternalResolveParams(const method: TRttiMethod;
  const args: TArray<TValue>; paramResolution: TParamResolution): TArray<TValue>;
var
  params: TArray<TRttiParameter>;
  i: Integer;
begin
  SetLength(Result, Length(args) - 1);
  params := method.GetParameters;
  for i := 0 to High(params) do
    case paramResolution of
      TParamResolution.ByName:
        Result[i] := TNamedValue.Create(args[i + 1], params[i].Name);
      TParamResolution.ByType:
        Result[i] := TTypedValue.Create(args[i + 1], params[i].ParamType.Handle);
    else
      Result[i] := args[i + 1];
    end;
end;

procedure TComponentRegistry.InternalRegisterFactory(
  const model: TComponentModel; const invokeEvent: TVirtualInterfaceInvokeEvent);
var
  methods: TArray<TRttiMethod>;
  maxVirtualIndex: SmallInt;
  method: TRttiMethod;
begin
  methods := model.ComponentType.GetMethods;

  if methods = nil then
    raise ERegistrationException.CreateResFmt(@SUnsupportedFactoryType, [
      model.ComponentTypeName]);

  for method in methods do
    if not Assigned(method.ReturnType)
      or method.Parameters.Any(TParameterFilters.HasFlags([pfOut])) then
      raise ERegistrationException.CreateResFmt(@SUnsupportedFactoryMethod, [
        model.ComponentTypeName, method.ToString]);

  maxVirtualIndex := 2;
  for method in methods do
    if maxVirtualIndex < method.VirtualIndex then
      maxVirtualIndex := method.VirtualIndex;

  model.ActivatorDelegate :=
    function: TValue
    var
      factory: TVirtualInterface;
      intf: IInterface;
    begin
      factory := TVirtualInterface.Create(model.ComponentTypeInfo, invokeEvent);
      if IsMethodReference(model.ComponentTypeInfo) then
        if maxVirtualIndex > 3 then
          TVirtualInterfaceHack(factory).VTable[3] :=
            TVirtualInterfaceHack(factory).VTable[maxVirtualIndex];
      factory.QueryInterface(model.ComponentTypeInfo.TypeData.Guid, intf);
      TValue.Make(@intf, model.ComponentTypeInfo, Result);
    end;
end;

procedure TComponentRegistry.RegisterFactory(const model: TComponentModel);
begin
  RegisterFactory(model, GetDefaultParamResolution(model.ComponentType));
end;

procedure TComponentRegistry.RegisterFactory(const model: TComponentModel;
  paramResolution: TParamResolution);
var
  invokeEvent: TVirtualInterfaceInvokeEvent;
begin
  invokeEvent :=
    procedure(method: TRttiMethod; const args: TArray<TValue>; out result: TValue)
    var
      arguments: TArray<TValue>;
    begin
      arguments := InternalResolveParams(method, args, paramResolution);
      result := (fKernel as IKernelInternal).Resolve(
        method.ReturnType.Handle, arguments);
    end;

  InternalRegisterFactory(model, invokeEvent);
end;

procedure TComponentRegistry.RegisterFactory(const model: TComponentModel;
  const resolvedServiceName: string);
begin
  RegisterFactory(model, resolvedServiceName, GetDefaultParamResolution(model.ComponentType));
end;

procedure TComponentRegistry.RegisterFactory(const model: TComponentModel;
  const resolvedServiceName: string; paramResolution: TParamResolution);
var
  invokeEvent: TVirtualInterfaceInvokeEvent;
begin
  invokeEvent :=
    procedure(method: TRttiMethod; const args: TArray<TValue>; out result: TValue)
    var
      arguments: TArray<TValue>;
    begin
      arguments := InternalResolveParams(method, args, paramResolution);
      result := (fKernel as IKernelInternal).Resolve(
        resolvedServiceName, arguments);
    end;

  InternalRegisterFactory(model, invokeEvent);
end;

function TComponentRegistry.RegisterComponent(
  componentTypeInfo: PTypeInfo): TComponentModel;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(componentTypeInfo, 'componentTypeInfo');
{$ENDIF}

  Result := TComponentModel.Create(componentTypeInfo.RttiType);
  Result.Kernel := fKernel;
  fModels.Add(Result);
end;

function TComponentRegistry.FindOne(const serviceName: string): TComponentModel;
begin
  fServiceNameMappings.TryGetValue(serviceName, Result);
end;

function TComponentRegistry.FindOne(componentType: PTypeInfo): TComponentModel;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(componentType, 'componentType');
{$ENDIF}

  Result := fModels.FirstOrDefault(
    function(const model: TComponentModel): Boolean
    begin
      Result := model.ComponentTypeInfo = componentType;
    end);
end;

function TComponentRegistry.FindOne(serviceType: PTypeInfo;
  const argument: TValue): TComponentModel;
var
  serviceName: string;
begin
  if argument.IsEmpty then
  begin
    if not HasService(serviceType) then
      raise EResolveException.CreateResFmt(@SCannotResolveType, [
        serviceType.TypeName])
    else
    begin
      Result := FindDefault(serviceType);
      if not Assigned(Result) then
        raise EResolveException.CreateResFmt(@SNoDefaultFound, [
          serviceType.TypeName]);
    end;
  end
  else if argument.IsString then
  begin
    serviceName := argument.AsString;
    Result := FindOne(serviceName);
    if not Assigned(Result) then
      raise EResolveException.CreateResFmt(@SServiceNotFound, [serviceName]);
    if not IsAssignableFromRelaxed(serviceType, Result.Services[serviceName]) then
      raise EResolveException.CreateResFmt(@SCannotResolveTypeNamed, [
        serviceType.TypeName, serviceName]);
  end
  else
    raise EResolveException.CreateResFmt(@SCannotResolveType, [
      serviceType.TypeName]);
end;

function TComponentRegistry.GetOnChanged: ICollectionChangedEvent<TComponentModel>;
begin
  Result := fOnChanged;
end;

function TComponentRegistry.FindDefault(
  serviceType: PTypeInfo): TComponentModel;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(serviceType, 'serviceType');
{$ENDIF}

  if not fDefaultRegistrations.TryGetValue(serviceType, Result) then
    fServiceTypeMappings[serviceType].TryGetSingle(Result);
end;

function TComponentRegistry.FindAll: IEnumerable<TComponentModel>;
begin
  Result := fModels;
end;

function TComponentRegistry.FindAll(
  serviceType: PTypeInfo): IEnumerable<TComponentModel>;
var
  models: IReadOnlyCollection<TComponentModel>;
  unnamedModels: IReadOnlyCollection<TComponentModel>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(serviceType, 'serviceType');
{$ENDIF}

  if fServiceTypeMappings.TryGetValues(serviceType, models) then
  begin
    if fUnnamedRegistrations.TryGetValues(serviceType, unnamedModels) then
      Result := models.Exclude(unnamedModels)
    else
      Result := models;
  end
  else
    Result := TEnumerable.Empty<TComponentModel>;
end;

function TComponentRegistry.HasService(serviceType: PTypeInfo): Boolean;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(serviceType, 'serviceType');
{$ENDIF}

  Result := fServiceTypeMappings.ContainsKey(serviceType);
end;

function TComponentRegistry.HasService(const serviceName: string): Boolean;
begin
  Result := fServiceNameMappings.ContainsKey(serviceName);
end;

function TComponentRegistry.HasService(serviceType: PTypeInfo;
  const serviceName: string): Boolean;
var
  model: TComponentModel;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(serviceType, 'serviceType');
{$ENDIF}

  Result := fServiceNameMappings.TryGetValue(serviceName, model)
    and model.HasService(serviceType);
end;

function TComponentRegistry.HasDefault(serviceType: PTypeInfo): Boolean;
begin
  Result := fDefaultRegistrations.ContainsKey(serviceType)
    or (fServiceTypeMappings[serviceType].Count = 1);
end;

{$ENDREGION}


{$REGION 'TRegistration'}

function TRegistration.GetKernel: TKernel;
begin
  Result := TComponentModel(Self).Kernel;
end;

function TRegistration.GetModel: TComponentModel;
begin
  Result := TComponentModel(Self);
end;

function TRegistration.Implements(serviceType: PTypeInfo): TRegistration;
begin
  Kernel.Registry.RegisterService(Model, serviceType);
  Result := Self;
end;

function TRegistration.Implements(serviceType: PTypeInfo;
  const serviceName: string): TRegistration;
begin
  Kernel.Registry.RegisterService(Model, serviceType, serviceName);
  Result := Self;
end;

function TRegistration.Implements<TServiceType>: TRegistration;
begin
  Result := Implements(TypeInfo(TServiceType));
end;

function TRegistration.Implements<TServiceType>(
  const serviceName: string): TRegistration;
begin
  Result := Implements(TypeInfo(TServiceType), serviceName);
end;

function TRegistration.InjectConstructor: TRegistration;
begin
  Kernel.Injector.InjectConstructor(Model);
  Result := Self;
end;

function TRegistration.InjectConstructor(
  const parameterTypes: array of PTypeInfo): TRegistration;
begin
  Kernel.Injector.InjectConstructor(Model, parameterTypes);
  Result := Self;
end;

function TRegistration.InjectParameter(const parameterName: string;
  const value: TValue): TRegistration;
begin
  Model.ParameterInjections.Add(TNamedValue.Create(value, parameterName));
  Result := Self;
end;

function TRegistration.InjectParameter(parameterType: PTypeInfo;
  const value: TValue): TRegistration;
begin
  Model.ParameterInjections.Add(TTypedValue.Create(value, parameterType));
  Result := Self;
end;

function TRegistration.InjectParameter<TParameterType>(
  const value: TParameterType): TRegistration;
begin
  Result := InjectParameter(TypeInfo(TParameterType), TValue.From(value, TypeInfo(TParameterType)));
end;

function TRegistration.InjectParameter<TParameterType>(
  const serviceName: string): TRegistration;
begin
  Result := InjectParameter(TypeInfo(TParameterType), serviceName);
end;

function TRegistration.InjectProperty(
  const propertyName: string): TRegistration;
begin
  Kernel.Injector.InjectProperty(Model, propertyName);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string;
  const parameterTypes: array of PTypeInfo): TRegistration;
begin
  Kernel.Injector.InjectMethod(Model, methodName, parameterTypes);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string): TRegistration;
begin
  Kernel.Injector.InjectMethod(Model, methodName);
  Result := Self;
end;

function TRegistration.InjectField(const fieldName: string): TRegistration;
begin
  Kernel.Injector.InjectField(Model, fieldName);
  Result := Self;
end;

function TRegistration.InjectConstructor(
  const arguments: array of TValue): TRegistration;
begin
  Kernel.Injector.InjectConstructor(Model, arguments);
  Result := Self;
end;

function TRegistration.InjectProperty(const propertyName: string;
  const value: TValue): TRegistration;
begin
  Kernel.Injector.InjectProperty(Model, propertyName, value);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string;
  const arguments: array of TValue): TRegistration;
begin
  Kernel.Injector.InjectMethod(Model, methodName, arguments);
  Result := Self;
end;

function TRegistration.InjectField(const fieldName: string;
  const value: TValue): TRegistration;
begin
  Kernel.Injector.InjectField(Model, fieldName, value);
  Result := Self;
end;

function TRegistration.AsSingleton(refCounting: TRefCounting): TRegistration;
begin
  Model.LifetimeType := TLifetimeType.Singleton;
  Model.RefCounting := refCounting;
  Result := Self;
end;

function TRegistration.AsSingletonPerThread(refCounting: TRefCounting): TRegistration;
begin
  Model.LifetimeType := TLifetimeType.SingletonPerThread;
  Model.RefCounting := refCounting;
  Result := Self;
end;

function TRegistration.AsTransient: TRegistration;
begin
  Model.LifetimeType := TLifetimeType.Transient;
  Result := Self;
end;

function TRegistration.AsPooled(minPoolSize, maxPoolSize: Integer): TRegistration;
begin
  Model.LifetimeType := TLifetimeType.Pooled;
  Model.MinPoolsize := minPoolSize;
  Model.MaxPoolsize := maxPoolSize;
  Result := Self;
end;

function TRegistration.PerResolve: TRegistration;
begin
  Model.LifetimeType := TLifetimeType.PerResolve;
  Result := Self;
end;

function TRegistration.AsDefault: TRegistration;
var
  serviceType: PTypeInfo;
begin
  for serviceType in Model.Services.Values do
    Kernel.Registry.RegisterDefault(Model, serviceType);
  Result := Self;
end;

function TRegistration.AsDefault(serviceType: PTypeInfo): TRegistration;
begin
  Kernel.Registry.RegisterDefault(Model, serviceType);
  Result := Self;
end;

function TRegistration.AsDefault<TServiceType>: TRegistration;
begin
  Result := AsDefault(TypeInfo(TServiceType));
end;

function TRegistration.AsCustom(
  const lifetimeManager: IInterface): TRegistration;
begin
  Model.LifetimeType := TLifetimeType.Custom;
  Model.LifetimeManager := lifetimeManager as ILifetimeManager;
  Result := Self;
end;

function TRegistration.AsCustom<TLifetimeManagerType>: TRegistration;
begin
  Result := AsCustom(TLifetimeManagerType.Create);
end;

function TRegistration.AsFactory: TRegistration;
begin
  Kernel.Registry.RegisterFactory(Model);
  Result := Self;
end;

function TRegistration.AsFactory(
  paramResolution: TParamResolution): TRegistration;
begin
  Kernel.Registry.RegisterFactory(Model, paramResolution);
  Result := Self;
end;

function TRegistration.AsFactory(const resolvedServiceName: string): TRegistration;
begin
  Kernel.Registry.RegisterFactory(Model, resolvedServiceName);
  Result := Self;
end;

function TRegistration.AsFactory(const resolvedServiceName: string;
  paramResolution: TParamResolution): TRegistration;
begin
  Kernel.Registry.RegisterFactory(Model, resolvedServiceName, paramResolution);
  Result := Self;
end;

procedure TRegistration.InterceptedBy(
  const interceptorRef: TInterceptorReference; where: TWhere);
begin
  case where of
    TWhere.First: Model.Interceptors.Insert(0, interceptorRef);
  else
    Model.Interceptors.Add(interceptorRef);
  end;
end;

function TRegistration.InterceptedBy(interceptorType: PTypeInfo;
  where: TWhere): TRegistration;
begin
  InterceptedBy(TInterceptorReference.Create(interceptorType), where);
  Result := Self;
end;

function TRegistration.InterceptedBy(const name: string;
  where: TWhere): TRegistration;
begin
  InterceptedBy(TInterceptorReference.Create(name), where);
  Result := Self;
end;

function TRegistration.InterceptedBy<TInterceptorType>(
  where: TWhere = TWhere.Last): TRegistration;
begin
  Result := InterceptedBy(TypeInfo(TInterceptorType), where);
end;

{$ENDREGION}


end.
