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

unit Spring.Container.Core;

interface

uses
  Classes,
  Rtti,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Logging,
  Spring.Container.Common,
  Spring.Patterns.Specification;

type
  { Forward Declarations }
  TComponentModel = class;
  IComponentBuilder = interface;
  IComponentRegistry = interface;
  IDecoratorResolver = interface;
  IBuilderInspector = interface;
  ISubDependencyResolver = interface;
  IDependencyResolver = interface;
  IInjection = interface;
  IDependencyInjector = interface;
  ILifetimeManager = interface;
  IComponentActivator = interface;
  IContainerExtension = interface;
  ICreationContext = interface;
  IProxyFactory = interface;

  TActivatorDelegate<T> = reference to function: T;

  TDependencyModel = record
  private
    fTargetType: TRttiType;
    fTarget: TRttiNamedObject;
    function GetParentType: TRttiType; inline;
    function GetTargetTypeInfo: PTypeInfo; inline;
    function GetTargetTypeName: string; inline;
  public
    constructor Create(const targetType: TRttiType;
      const target: TRttiNamedObject);

    property ParentType: TRttiType read GetParentType;
    property TargetType: TRttiType read fTargetType;
    property Target: TRttiNamedObject read fTarget;
    property Name: string read GetTargetTypeName;
    property TypeInfo: PTypeInfo read GetTargetTypeInfo;
  end;

  TKernel = class;

  IKernelInternal = interface
    ['{14669EBA-4E57-4DF4-919D-377D8E90144C}']
    function Resolve(serviceType: PTypeInfo): TValue; overload;
    function Resolve(serviceType: PTypeInfo;
      const arguments: array of TValue): TValue; overload;
    function Resolve(const serviceName: string): TValue; overload;
    function Resolve(const serviceName: string;
      const arguments: array of TValue): TValue; overload;
    function ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
  end;

  /// <summary>
  ///   Extends the container.
  /// </summary>
  IContainerExtension = interface
    ['{E78748FB-D75C-447C-B984-9782A8F26C20}']
    procedure Initialize;
    procedure InitializeExtension(const kernel: TKernel);
  end;

  /// <summary>
  ///   Manages the registration of components.
  /// </summary>
  IComponentRegistry = interface
    ['{CBCA1D0F-1244-4AB4-AB07-091053932166}']
  {$REGION 'Property Accessors'}
    function GetOnChanged: ICollectionChangedEvent<TComponentModel>;
  {$ENDREGION}

    function RegisterComponent(componentType: PTypeInfo): TComponentModel;
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

    property OnChanged: ICollectionChangedEvent<TComponentModel> read GetOnChanged;
  end;

  IDecoratorResolver = interface
    ['{AC84E128-1C52-465A-9B10-C79A58DD3BEA}']
    procedure AddDecorator(decoratedType: PTypeInfo;
      const decoratorModel: TComponentModel;
      const condition: Predicate<TComponentModel>);

    function Resolve(const dependency: TDependencyModel;
      const model: TComponentModel; const context: ICreationContext;
      const decoratee: TValue): TValue;
  end;

  /// <summary>
  ///   Component Builder
  /// </summary>
  IComponentBuilder = interface
    ['{8309EBC7-9699-47CF-B177-4BC9B787EBE0}']
  {$REGION 'Property Accessors'}
    function GetOnBuild: INotifyEvent<TComponentModel>;
  {$ENDREGION}

    // Inspectors (Policies)
    procedure AddInspector(const inspector: IBuilderInspector);
    procedure RemoveInspector(const inspector: IBuilderInspector);
    procedure ClearInspectors;
    // Build
    procedure Build(const model: TComponentModel);
    procedure BuildAll;

    property OnBuild: INotifyEvent<TComponentModel> read GetOnBuild;
  end;

  /// <summary>
  ///   Builder Inspector
  /// </summary>
  IBuilderInspector = interface
    ['{3E2F36D1-2C0D-4D6A-91B3-49B09BD31318}']
    procedure ProcessModel(const kernel: TKernel; const model: TComponentModel);
  end;

  /// <summary>
  ///   Lifetime Manager
  /// </summary>
  ILifetimeManager = interface
    ['{7DF9A902-B07A-468B-B201-B4561A921CF5}']
    function Resolve(const context: ICreationContext;
      const model: TComponentModel): TValue;
    procedure Release(const instance: TValue);
  end;

  /// <summary>
  ///   Component Activator
  /// </summary>
  IComponentActivator = interface
    ['{18E6DF78-C947-484F-A0A8-D9A5B0BEC887}']
    function CreateInstance(const context: ICreationContext): TValue;
  end;

  /// <summary>
  ///   Represents an injection of a member. e.g. constructor, method, property
  ///   and even field.
  /// </summary>
  IInjection = interface
    ['{864AAA38-4F93-4BB9-AD8A-B796FCD2EFE0}']
  {$REGION 'Property Accessors'}
    function GetDependencyCount: Integer;
    function GetTarget: TRttiMember;
    function GetTargetName: string;
    function GetHasTarget: Boolean;
    function GetArguments: TArray<TValue>;
    function GetDependencies: TArray<TDependencyModel>;
  {$ENDREGION}

    procedure Initialize(const target: TRttiMember);
    procedure InitializeArguments(const arguments: array of TValue);
    procedure InitializeDependencies(const parameterTypes: array of PTypeInfo);
    procedure Inject(const instance: TValue; const arguments: array of TValue);

    property DependencyCount: Integer read GetDependencyCount;
    property Target: TRttiMember read GetTarget;
    property TargetName: string read GetTargetName;
    property HasTarget: Boolean read GetHasTarget;
    property Arguments: TArray<TValue> read GetArguments;
    property Dependencies: TArray<TDependencyModel> read GetDependencies;
  end;

  IInjectionList = IList<IInjection>;

  IDependencyInjector = interface
    ['{4627FFB4-F8D9-4A70-A281-8D323FAAA4CB}']
    function InjectConstructor(const model: TComponentModel;
      const parameterTypes: array of PTypeInfo): IInjection; overload;
    function InjectMethod(const model: TComponentModel;
      const methodName: string): IInjection; overload;
    function InjectMethod(const model: TComponentModel; const methodName: string;
      const parameterTypes: array of PTypeInfo): IInjection; overload;
    function InjectProperty(const model: TComponentModel;
      const propertyName: string): IInjection; overload;
    function InjectField(const model: TComponentModel;
      const fieldName: string): IInjection; overload;

    function InjectConstructor(const model: TComponentModel): IInjection; overload;
    function InjectConstructor(const model: TComponentModel;
      const arguments: array of TValue): IInjection; overload;
    function InjectMethod(const model: TComponentModel; const methodName: string;
      const arguments: array of TValue): IInjection; overload;
    function InjectProperty(const model: TComponentModel;
      const propertyName: string; const value: TValue): IInjection; overload;
    function InjectField(const model: TComponentModel;
      const fieldName: string; const value: TValue): IInjection; overload;
  end;

  ISubDependencyResolver = interface
    ['{E360FFAD-2235-49D1-9A4F-50945877E337}']
    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): TValue;
  end;

  /// <summary>
  ///   Used during a component request, passed along to the whole process.
  ///   This will allow some data to be passed along the process, which is used
  ///   to detect cycled dependency graphs and also being used to provide
  ///   arguments to components.
  /// </summary>
  ICreationContext = interface(ISubDependencyResolver)
    ['{0E788A94-AD9B-4951-85C1-40F877BB8A24}']
    function EnterResolution(const model: TComponentModel;
      out instance: TValue): Boolean;
    procedure LeaveResolution(const model: TComponentModel);

    function AddArgument(const argument: TValue): Integer;
    procedure RemoveTypedArgument(index: Integer);
    procedure AddPerResolve(const model: TComponentModel; const instance: TValue);
    function TryHandle(const injection: IInjection;
      out handled: IInjection): Boolean;
  end;

  IDependencyResolver = interface(ISubDependencyResolver)
    ['{15ADEA1D-7C3F-48D5-8E85-84B4332AFF5F}']
    function CanResolve(const context: ICreationContext;
      const dependencies: TArray<TDependencyModel>;
      const arguments: TArray<TValue>): Boolean; overload;
    function Resolve(const context: ICreationContext;
      const dependencies: TArray<TDependencyModel>;
      const arguments: TArray<TValue>): TArray<TValue>; overload;

    procedure AddSubResolver(const subResolver: ISubDependencyResolver);
    procedure RemoveSubResolver(const subResolver: ISubDependencyResolver);
  end;

  TInterceptorReference = record
  private
    fTypeInfo: PTypeInfo;
    fName: string;
  public
    constructor Create(typeInfo: PTypeInfo); overload;
    constructor Create(const name: string); overload;

    class function ForType<T>: TInterceptorReference; static;

    property TypeInfo: PTypeInfo read fTypeInfo;
    property Name: string read fName;
  end;

  IModelInterceptorsSelector = interface
    ['{118AE0DF-C257-4395-83AF-65F86AB12A2D}']
    function HasInterceptors(const model: TComponentModel): Boolean;
    function SelectInterceptors(const model: TComponentModel;
      const interceptors: array of TInterceptorReference): TArray<TInterceptorReference>;
  end;

  IProxyFactory = interface
    ['{4813914F-810D-451D-8AED-205C3F82C068}']
    procedure AddInterceptorSelector(const selector: IModelInterceptorsSelector);

    function CreateInstance(const context: ICreationContext;
      const instance: TValue; const model: TComponentModel;
      const constructorArguments: array of TValue): TValue;
  end;

  TKernel = class(TInterfaceBase)
  private
    fRegistry: IComponentRegistry;
    fBuilder: IComponentBuilder;
    fInjector: IDependencyInjector;
    fResolver: IDependencyResolver;
    fProxyFactory: IProxyFactory;
    fExtensions: IList<IContainerExtension>;
    fLogger: ILogger;
    fDecoratorResolver: IDecoratorResolver;
    procedure SetLogger(const logger: ILogger);
  public
    constructor Create;

    procedure AddExtension(const extension: IContainerExtension);

    property Builder: IComponentBuilder read fBuilder;
    property Injector: IDependencyInjector read fInjector;
    property Registry: IComponentRegistry read fRegistry;
    property Resolver: IDependencyResolver read fResolver;
    property Logger: ILogger read fLogger write SetLogger;
    property ProxyFactory: IProxyFactory read fProxyFactory;
    property DecoratorResolver: IDecoratorResolver read fDecoratorResolver;
  end;

  /// <summary>
  ///   TComponentModel
  /// </summary>
  TComponentModel = class
  private
    fKernel: TKernel;
    fComponentType: TRttiType;
    fLifetimeType: TLifetimeType;
    fLifetimeManager: ILifetimeManager;
    fComponentActivator: IComponentActivator;
    fActivatorDelegate: TActivatorDelegate;
    fMinPoolsize: Integer;
    fMaxPoolsize: Integer;
    fRefCounting: TRefCounting;
    fServices: IDictionary<string, PTypeInfo>;
    fConstructorInjections: IInjectionList;
    fMethodInjections: IInjectionList;
    fPropertyInjections: IInjectionList;
    fFieldInjections: IInjectionList;
    fInterceptors: IList<TInterceptorReference>;
    fParameterInjections: IList<TValue>;
    function GetComponentTypeInfo: PTypeInfo;
    function GetComponentTypeName: string;
    procedure SetRefCounting(const value: TRefCounting);
  public
    constructor Create(const componentType: TRttiType);

    function HasService(serviceType: PTypeInfo): Boolean;
    function GetServiceName(serviceType: PTypeInfo): string;
    function GetServiceType(const serviceName: string): PTypeInfo;

    property Kernel: TKernel read fKernel write fKernel;

    property ComponentType: TRttiType read fComponentType;
    property ComponentTypeInfo: PTypeInfo read GetComponentTypeInfo;
    property ComponentTypeName: string read GetComponentTypeName;
    property Services: IDictionary<string, PTypeInfo> read fServices;
    property MinPoolsize: Integer read fMinPoolsize write fMinPoolsize;
    property MaxPoolsize: Integer read fMaxPoolsize write fMaxPoolsize;
    property RefCounting: TRefCounting read fRefCounting write SetRefCounting;

    property LifetimeType: TLifetimeType read fLifetimeType write fLifetimeType;
    property LifetimeManager: ILifetimeManager read fLifetimeManager write fLifetimeManager;
    property ComponentActivator: IComponentActivator read fComponentActivator write fComponentActivator;
    property ActivatorDelegate: TActivatorDelegate read fActivatorDelegate write fActivatorDelegate;

    property ConstructorInjections: IInjectionList read fConstructorInjections;
    property MethodInjections: IInjectionList read fMethodInjections;
    property PropertyInjections: IInjectionList read fPropertyInjections;
    property FieldInjections: IInjectionList read fFieldInjections;
    property ParameterInjections: IList<TValue> read fParameterInjections;

    property Interceptors: IList<TInterceptorReference> read fInterceptors;
  end;

  TValueHolder = class(TInterfacedObject, Func<TValue>)
  private
    // DON'T CHANGE ORDER!!!
    fLifetimeWatcher: IInterface;
    fValue: TValue;
    type
      TComponentHolder = class(TComponent, IInterface)
      private
        fRefCount: Integer;
        fValue: PValue;
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;
      protected
        procedure Notification(Component: TComponent; Operation: TOperation); override;
      public
        constructor Create(value: PValue); reintroduce;
        destructor Destroy; override;
      end;
  public
    constructor Create(const value: TValue; refCounting: TRefCounting); overload;
    constructor Create(const value: TValue; const lifetimeWatcher: IInterface); overload;
    destructor Destroy; override;
    function Invoke: TValue;
  end;

  EContainerException = class(Exception);

  ERegistrationException = class(EContainerException);
  EBuilderException = class(EContainerException);
  EInjectionException = class(EContainerException);

  EResolveException = class(EContainerException);
  ECircularDependencyException = class(EResolveException);

  EActivatorException = class(EContainerException);

  TInjectableMethodFilter = class(TSpecification<TRttiMethod>)
  private
    fKernel: TKernel;
    fModel: TComponentModel;
    fArguments: TArray<TValue>;
  public
    constructor Create(const kernel: TKernel; const model: TComponentModel;
      const arguments: TArray<TValue>);
    function IsSatisfiedBy(const method: TRttiMethod): Boolean; override;
  end;

  TContainsMemberFilter = class(TSpecification<IInjection>)
  private
    fMember: TRttiMember;
  public
    constructor Create(const member: TRttiMember);
    function IsSatisfiedBy(const injection: IInjection): Boolean; override;
  end;

  TInjectionFilters = class
  public
    class function ContainsMember(const member: TRttiMember): Specification<IInjection>;
    class function IsInjectableMethod(const kernel: TKernel;
      const model: TComponentModel;
      const arguments: TArray<TValue>): Specification<TRttiMethod>;
  end;

implementation

uses
  TypInfo,
  Spring.Container.Builder,
  Spring.Container.ComponentActivator,
  Spring.Container.Injection,
  Spring.Container.LifetimeManager,
  Spring.Container.ProxyFactory,
  Spring.Container.Registration,
  Spring.Container.Resolvers,
  Spring.Container.ResourceStrings,
  Spring.Logging.NullLogger,
  Spring.Reflection;


{$REGION 'TDependencyModel'}

constructor TDependencyModel.Create(const targetType: TRttiType;
  const target: TRttiNamedObject);
begin
  fTargetType := targetType;
  fTarget := target;
end;

function TDependencyModel.GetParentType: TRttiType;
begin
  if fTarget is TRttiParameter then
    Result := fTarget.Parent.Parent as TRttiType
  else
    Result := fTarget.Parent as TRttiType;
end;

function TDependencyModel.GetTargetTypeInfo: PTypeInfo;
begin
  if Assigned(fTargetType) then
    Result := fTargetType.Handle
  else
    Result := nil;
end;

function TDependencyModel.GetTargetTypeName: string;
begin
  Result := fTargetType.DefaultName;
end;

{$ENDREGION'}


{$REGION 'TInterceptorReference'}

constructor TInterceptorReference.Create(typeInfo: PTypeInfo);
begin
  fTypeInfo := typeInfo;
  fName := '';
end;

constructor TInterceptorReference.Create(const name: string);
begin
  fTypeInfo := nil;
  fName := name;
end;

class function TInterceptorReference.ForType<T>: TInterceptorReference;
begin
  Result := TInterceptorReference.Create(System.TypeInfo(T));
end;

{$ENDREGION}


{$REGION 'TKernel'}

constructor TKernel.Create;
begin
  inherited Create;
  fLogger := TNullLogger.GlobalInstance;
  fRegistry := TComponentRegistry.Create(Self);
  fBuilder := TComponentBuilder.Create(Self);
  fInjector := TDependencyInjector.Create;
  fResolver := TDependencyResolver.Create(Self);
  fProxyFactory := TProxyFactory.Create(Self);
  fExtensions := TCollections.CreateInterfaceList<IContainerExtension>;
  fDecoratorResolver := TDecoratorResolver.Create;
end;

procedure TKernel.AddExtension(const extension: IContainerExtension);
begin
  fExtensions.Add(extension);
  extension.InitializeExtension(Self);
  extension.Initialize;
end;

procedure TKernel.SetLogger(const logger: ILogger);
begin
  if Assigned(logger) then
    fLogger := logger
  else
    fLogger := TNullLogger.GlobalInstance;
end;

{$ENDREGION}


{$REGION 'TComponentModel'}

constructor TComponentModel.Create(const componentType: TRttiType);
begin
  inherited Create;
  fComponentType := componentType;
  fServices := TCollections.CreateDictionary<string, PTypeInfo>;
  fConstructorInjections := TCollections.CreateInterfaceList<IInjection>;
  fMethodInjections := TCollections.CreateInterfaceList<IInjection>;
  fPropertyInjections := TCollections.CreateInterfaceList<IInjection>;
  fFieldInjections := TCollections.CreateInterfaceList<IInjection>;
  fInterceptors := TCollections.CreateList<TInterceptorReference>;
  fParameterInjections := TCollections.CreateList<TValue>;
end;

function TComponentModel.GetComponentTypeInfo: PTypeInfo;
begin
  Result := fComponentType.Handle;
end;

function TComponentModel.GetComponentTypeName: string;
begin
  Result := fComponentType.DefaultName;
end;

function TComponentModel.GetServiceName(serviceType: PTypeInfo): string;
begin
  Guard.CheckNotNull(serviceType, 'serviceType');
  Result := fServices.FirstOrDefault(
    function(const item: TPair<string, PTypeInfo>): Boolean
    begin
      Result := SameTypeInfo(item.Value, serviceType);
    end).Key;
end;

function TComponentModel.GetServiceType(const serviceName: string): PTypeInfo;
begin
  Result := fServices[serviceName];
end;

function TComponentModel.HasService(serviceType: PTypeInfo): Boolean;
begin
  Result := fServices.ContainsValue(serviceType);
end;

procedure TComponentModel.SetRefCounting(const value: TRefCounting);
begin
  if (value = TRefCounting.True) and fComponentType.IsInstance
    and not Supports(fComponentType.AsInstance.MetaclassType, IInterface) then
    raise ERegistrationException.CreateResFmt(@SMissingInterface, [fComponentType.DefaultName]);
  fRefCounting := Value;
end;

{$ENDREGION}


{$REGION 'TValueHolder'}

constructor TValueHolder.Create(const value: TValue; refCounting: TRefCounting);
var
  lifetimeWatcher: IInterface;
  component: TComponent;
  componentHolder: TComponentHolder;
begin
  Guard.CheckNotNull(not value.IsEmpty, 'value');

  if ((refCounting = TRefCounting.Unknown) and value.IsType(TypeInfo(TInterfacedObject)))
    or (refCounting = TRefCounting.True) then
    value.AsObject.GetInterface(IInterface, lifetimeWatcher)
  else
    if value.Kind = tkInterface then
      lifetimeWatcher := value.AsInterface
    else
      if value.TryAsType(TypeInfo(TComponent), component) then
      begin
        componentHolder := TComponentHolder.Create(@fValue);
        componentHolder.FreeNotification(component);
        lifetimeWatcher := componentHolder;
      end
      else
        lifetimeWatcher := nil;
  Create(value, lifetimeWatcher);
end;

constructor TValueHolder.Create(const value: TValue;
  const lifetimeWatcher: IInterface);
begin
  fValue := value;
  fLifetimeWatcher := lifetimeWatcher;
end;

destructor TValueHolder.Destroy; //FI:W504
begin
  if not Assigned(fLifetimeWatcher) and fValue.IsObject then
    fValue.AsObject.Free;
  // explicitly set to nil to keep correct order
  fLifetimeWatcher := nil;
  fValue := nil;
end;

function TValueHolder.Invoke: TValue;
begin
  Result := fValue;
end;

{$ENDREGION}


{$REGION 'TValueHolder.TComponentHolder'}

constructor TValueHolder.TComponentHolder.Create(value: PValue);
begin
  inherited Create(nil);
  fValue := value;
end;

destructor TValueHolder.TComponentHolder.Destroy;
begin
  fValue^.AsObject.Free;
  inherited Destroy;
end;

function TValueHolder.TComponentHolder._AddRef: Integer;
begin
  Result := AtomicIncrement(fRefCount);
end;

function TValueHolder.TComponentHolder._Release: Integer;
begin
  Result := AtomicDecrement(fRefCount);
  if Result = 0 then
    Destroy;
end;

procedure TValueHolder.TComponentHolder.Notification(Component: TComponent;
  Operation: TOperation);
begin
  inherited Notification(Component, Operation);
  if Operation = opRemove then
    fValue^ := nil;
end;

{$ENDREGION}


{$REGION 'TInjectableMethodFilter'}

constructor TInjectableMethodFilter.Create(const kernel: TKernel;
  const model: TComponentModel; const arguments: TArray<TValue>);
begin
  inherited Create;
  fKernel := kernel;
  fModel := model;
  fArguments := arguments;
end;

function TInjectableMethodFilter.IsSatisfiedBy(
  const method: TRttiMethod): Boolean;
var
  params: TArray<TRttiParameter>;
  dependencies: TArray<TDependencyModel>;
  i: Integer;
begin
  params := method.GetParameters;
  SetLength(dependencies, Length(params));
  for i := Low(dependencies) to High(dependencies) do
    dependencies[i] := TDependencyModel.Create(params[i].ParamType, params[i]);
  Result := fKernel.Resolver.CanResolve(nil, dependencies, fArguments);
end;

{$ENDREGION}


{$REGION 'TContainsMemberFilter'}

constructor TContainsMemberFilter.Create(const member: TRttiMember);
begin
  inherited Create;
  fMember := member;
end;

function TContainsMemberFilter.IsSatisfiedBy(
  const injection: IInjection): Boolean;
begin
  Result := injection.Target = fmember;
end;

{$ENDREGION}


{$REGION 'TInjectionFilters'}

class function TInjectionFilters.ContainsMember(
  const member: TRttiMember): Specification<IInjection>;
begin
  Result := TContainsMemberFilter.Create(member);
end;

class function TInjectionFilters.IsInjectableMethod(const kernel: TKernel;
  const model: TComponentModel;
  const arguments: TArray<TValue>): Specification<TRttiMethod>;
begin
  Result := TInjectableMethodFilter.Create(kernel, model, arguments);
end;

{$ENDREGION}


end.
