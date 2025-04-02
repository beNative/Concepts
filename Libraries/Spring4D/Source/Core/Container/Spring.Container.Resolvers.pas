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

unit Spring.Container.Resolvers;

interface

uses
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  TSubDependencyResolverBase = class abstract(TInterfacedObject, ISubDependencyResolver)
  private
    fKernel: TKernel;
  protected
    property Kernel: TKernel read fKernel;
  public
    constructor Create(const kernel: TKernel);

    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; virtual;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; virtual; abstract;
  end;

  TDependencyResolver = class(TSubDependencyResolverBase, IDependencyResolver)
  private
    fSubResolvers: IList<ISubDependencyResolver>;
  protected
    function CanResolveFromArgument(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean;
    function CanResolveFromContext(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean;
    function CanResolveFromSubResolvers(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean;
    function InternalResolveValue(const context: ICreationContext;
      const model: TComponentModel; const dependency: TDependencyModel;
      const instance: TValue): TValue;
  public
    constructor Create(const kernel: TKernel);

    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; overload; override;
    function CanResolve(const context: ICreationContext;
      const dependencies: TArray<TDependencyModel>;
      const arguments: TArray<TValue>): Boolean; reintroduce; overload; virtual;

    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; overload; override;
    function Resolve(const context: ICreationContext;
      const dependencies: TArray<TDependencyModel>;
      const arguments: TArray<TValue>): TArray<TValue>; reintroduce; overload; virtual;

    procedure AddSubResolver(const subResolver: ISubDependencyResolver);
    procedure RemoveSubResolver(const subResolver: ISubDependencyResolver);
  end;

  TLazyResolver = class(TSubDependencyResolverBase)
  private
    function InternalResolve<T>(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue; lazyKind: TLazyKind): TValue;
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; override;
  end;

  TDynamicArrayResolver = class(TSubDependencyResolverBase)
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; override;
  end;

  TCollectionResolver = class(TSubDependencyResolverBase)
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; override;
  end;

  TComponentOwnerResolver = class(TSubDependencyResolverBase)
  private
    fVirtualIndex: SmallInt;
  public
    constructor Create(const kernel: TKernel);

    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; override;
  end;

  TDecoratorResolver = class(TInterfacedObject, IDecoratorResolver)
  private
    type
      TDecoratorEntry = record
        DecoratorModel: TComponentModel;
        Condition: Predicate<TComponentModel>;
      end;
  private
    fDecorators: IMultiMap<PTypeInfo, TDecoratorEntry>;
    function GetDecorators(decoratedType: PTypeInfo;
      const decoratedModel: TComponentModel): IEnumerable<TComponentModel>;
  public
    constructor Create;

    procedure AddDecorator(decoratedType: PTypeInfo;
      const decoratorModel: TComponentModel;
      const condition: Predicate<TComponentModel>);
    function Resolve(const dependency: TDependencyModel;
      const model: TComponentModel; const context: ICreationContext;
      const decoratee: TValue): TValue;
  end;

implementation

uses
  Classes,
  Generics.Defaults,
  StrUtils,
  TypInfo,
  Spring.Collections.Lists,
  Spring.Comparers,
  Spring.Container.CreationContext,
  Spring.Container.ResourceStrings,
  Spring.Reflection;


{$REGION 'TSubDependencyResolverBase'}

constructor TSubDependencyResolverBase.Create(const kernel: TKernel);
begin
{$IFNDEF DISABLE_GUARD}
  Guard.CheckNotNull(kernel, 'kernel');
{$ENDIF}

  inherited Create;
  fKernel := kernel;
end;

function TSubDependencyResolverBase.CanResolve(const context: ICreationContext; //FI:O804
  const dependency: TDependencyModel; const argument: TValue): Boolean;
begin
  if not argument.IsEmpty and argument.IsString then
    Result := not Kernel.Registry.HasService(dependency.TypeInfo, argument.AsString)
      and (dependency.TypeInfo <> argument.TypeInfo)
  else
    Result := not Kernel.Registry.HasService(dependency.TypeInfo)
      and (dependency.TypeInfo <> argument.TypeInfo);
end;

{$ENDREGION}


{$REGION 'TDependencyResolver'}

constructor TDependencyResolver.Create(const kernel: TKernel);
begin
  inherited Create(kernel);
  fSubResolvers := TCollections.CreateInterfaceList<ISubDependencyResolver>;
end;

procedure TDependencyResolver.AddSubResolver(
  const subResolver: ISubDependencyResolver);
begin
  fSubResolvers.Add(subResolver);
end;

procedure TDependencyResolver.RemoveSubResolver(
  const subResolver: ISubDependencyResolver);
begin
  fSubResolvers.Remove(subResolver);
end;

function TDependencyResolver.InternalResolveValue(
  const context: ICreationContext; const model: TComponentModel;
  const dependency: TDependencyModel; const instance: TValue): TValue;
var
  intf: Pointer;
  typeInfo: PTypeInfo;
  typeData: PTypeData;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(context, 'context');
  Guard.CheckNotNull(dependency, 'dependency');
  Guard.CheckNotNull(not instance.IsEmpty, 'instance');
{$ENDIF}

  typeInfo := dependency.TypeInfo;
  if typeInfo.Kind = tkInterface then
  begin
    typeData := typeInfo.TypeData;
    if instance.IsObject then
    begin
      if ifHasGuid in typeData.IntfFlags then
        instance.AsObject.GetInterface(typeData.Guid, intf)
      else
        GetInterfaceByTypeInfo(instance.AsObject, typeInfo, intf);
    end
    else
    begin
      if IsMethodReference(typeInfo) then
      begin
        intf := nil;
        IInterface(intf) := instance.AsInterface;
      end
      else
        instance.AsInterface.QueryInterface(typeData.Guid, intf);
    end;
    TValue.MakeWithoutCopy(@intf, typeInfo, Result);
    Result := Kernel.ProxyFactory.CreateInstance(context, Result, model, []);
  end
  else
    Result := instance;

  Result := Kernel.DecoratorResolver.Resolve(dependency, model, context, Result);
  // ensure that the typeinfo of Result is exactly the one that was requested
  TValueData(Result).FTypeInfo := typeInfo;
end;

function TDependencyResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
var
  kind: TTypeKind;
  serviceName: string;
  componentModel: TComponentModel;
begin
  if dependency.TypeInfo = nil then
    Exit(True);

  if CanResolveFromArgument(context, dependency, argument) then
    Exit(True);

  if CanResolveFromContext(context, dependency, argument) then
    Exit(True);

  if CanResolveFromSubResolvers(context, dependency, argument) then
    Exit(True);

  if argument.IsEmpty then
    Result := Kernel.Registry.HasDefault(dependency.TypeInfo)
  else if argument.TryAsType(TypeInfo(TTypeKind), kind) and (kind = tkDynArray) then
    Result := Kernel.Registry.HasService(dependency.TypeInfo)
  else
  begin
    Result := argument.IsString;
    if Result then
    begin
      serviceName := argument.AsString;
      componentModel := Kernel.Registry.FindOne(serviceName);
      Result := Assigned(componentModel)
        and IsAssignableFromRelaxed(dependency.TypeInfo, componentModel.Services[serviceName]);
    end;
  end;
end;

function TDependencyResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  i: Integer;
  componentModel: TComponentModel;
  instance: TValue;
begin
  if dependency.TypeInfo = nil then
    Exit(TValue.Empty);

  if CanResolveFromContext(context, dependency, argument) then
    Exit(context.Resolve(context, dependency, argument));

  for i := fSubResolvers.Count - 1 downto 0 do
    if fSubResolvers[i].CanResolve(context, dependency, argument) then
      Exit(fSubResolvers[i].Resolve(context, dependency, argument));

  if CanResolveFromArgument(context, dependency, argument) then
    Exit(argument);

  componentModel := Kernel.Registry.FindOne(dependency.TypeInfo, argument);

  if context.EnterResolution(componentModel, instance) then
  try
    instance := componentModel.LifetimeManager.Resolve(context, componentModel);
  finally
    context.LeaveResolution(componentModel);
  end;
  Result := InternalResolveValue(context, componentModel, dependency, instance);
end;

function TDependencyResolver.CanResolve(const context: ICreationContext;
  const dependencies: TArray<TDependencyModel>;
  const arguments: TArray<TValue>): Boolean;
var
  i: Integer;
begin
  if Length(dependencies) = Length(arguments) then
  begin
    for i := Low(dependencies) to High(dependencies) do
      if not CanResolve(context, dependencies[i], arguments[i]) then
        Exit(False);
  end
  else if arguments = nil then
  begin
    for i := Low(dependencies) to High(dependencies) do
      if not CanResolve(context, dependencies[i], nil) then
        Exit(False);
  end
  else
    Exit(False);
  Result := True;
end;

function TDependencyResolver.CanResolveFromArgument(const context: ICreationContext; //FI:O804
  const dependency: TDependencyModel;
  const argument: TValue): Boolean;
begin
  Result := Assigned(argument.TypeInfo) and argument.IsType(dependency.TypeInfo);
  if not Result and (argument.Kind in [tkInteger, tkFloat, tkInt64]) then
    Result := argument.Kind = dependency.TypeInfo.Kind;
  if Result and argument.IsString then
    Result := not Kernel.Registry.HasService(dependency.TypeInfo, argument.AsString);
end;

function TDependencyResolver.CanResolveFromContext(
  const context: ICreationContext;  const dependency: TDependencyModel;
  const argument: TValue): Boolean;
begin
  Result := Assigned(context)
    and context.CanResolve(context, dependency, argument);
end;

function TDependencyResolver.CanResolveFromSubResolvers(
  const context: ICreationContext; const dependency: TDependencyModel;
  const argument: TValue): Boolean;
var
  i: Integer;
begin
  for i := fSubResolvers.Count - 1 downto 0 do
    if fSubResolvers[i].CanResolve(context, dependency, argument) then
      Exit(True);
  Result := False;
end;

function TDependencyResolver.Resolve(const context: ICreationContext;
  const dependencies: TArray<TDependencyModel>;
  const arguments: TArray<TValue>): TArray<TValue>;
var
  i: Integer;
begin
  if Assigned(arguments) and (Length(arguments) <> Length(dependencies)) then
    raise EResolveException.CreateRes(@SUnsatisfiedResolutionArgumentCount);
  SetLength(Result, Length(dependencies));
  if Assigned(arguments) then
    for i := Low(dependencies) to High(dependencies) do
      Result[i] := Resolve(context, dependencies[i], arguments[i])
  else
    for i := Low(dependencies) to High(dependencies) do
      Result[i] := Resolve(context, dependencies[i], nil);
end;

{$ENDREGION}


{$REGION 'TLazyResolver'}

function TLazyResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
var
  targetType: TRttiType;
  dependencyModel: TDependencyModel;
begin
  Result := IsLazyType(dependency.TypeInfo)
    and inherited CanResolve(context, dependency, argument);
  if Result then
  begin
    targetType := GetLazyType(dependency.TargetType.Handle).RttiType;
    dependencyModel := TDependencyModel.Create(targetType, dependency.Target);
    Result := Kernel.Resolver.CanResolve(context, dependencyModel, argument);
  end;
end;

function TLazyResolver.InternalResolve<T>(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue;
  lazyKind: TLazyKind): TValue;
var
  dependencyModel: TDependencyModel;
  value: TValue;
  factory: Func<T>;
begin
  dependencyModel := dependency;
  value := argument;
  factory :=
    function: T
    begin
      Kernel.Resolver.Resolve(context, dependencyModel, value).AsType(TypeInfo(T), Result);
    end;

  case lazyKind of //FI:W535
    lkFunc: Result := TValue.From<Func<T>>(factory);
    lkRecord: Result := TValue.From<Lazy<T>>(Lazy<T>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<T>>(Lazy<T>.Create(factory));
  end;
end;

function TLazyResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  lazyKind: TLazyKind;
  targetType: TRttiType;
  dependencyModel: TDependencyModel;
  componentModel: TComponentModel;
  hasEntered: Boolean;
begin
  if not IsLazyType(dependency.TypeInfo) then
    raise EResolveException.CreateResFmt(@SCannotResolveType, [dependency.Name]);

  lazyKind := GetLazyKind(dependency.TypeInfo);
  targetType := GetLazyType(dependency.TargetType.Handle).RttiType;
  dependencyModel := TDependencyModel.Create(targetType, dependency.Target);
  if Kernel.Registry.HasService(targetType.Handle) then
  begin
    componentModel := Kernel.Registry.FindOne(targetType.Handle, argument);
    hasEntered := context.EnterResolution(componentModel, Result);
  end
  else
  begin
    componentModel := nil;
    hasEntered := False;
  end;
  try
    case targetType.TypeKind of
      tkClass: Result := InternalResolve<TObject>(
        context, dependencyModel, argument, lazyKind);
      tkInterface: Result := InternalResolve<IInterface>(
        context, dependencyModel, argument, lazyKind);
      tkUString: Result := InternalResolve<string>(
        context, dependencyModel, argument, lazyKind);
      tkInteger: Result := InternalResolve<Integer>(
        context, dependencyModel, argument, lazyKind);
      tkInt64: Result := InternalResolve<Int64>(
        context, dependencyModel, argument, lazyKind);
    else
      raise EResolveException.CreateResFmt(@SCannotResolveType, [dependency.Name]);
    end;
    TValueData(Result).FTypeInfo := dependency.TypeInfo;
  finally
    if hasEntered then
      context.LeaveResolution(componentModel);
  end;
end;

{$ENDREGION}


{$REGION 'TDynamicArrayResolver'}

function ResolveDynamicArray(const kernel: TKernel; const context: ICreationContext;
  const dependency: TDependencyModel; const targetType: TRttiType): TArray<TValue>;
var
  serviceType: PTypeInfo;
  models: TArray<TComponentModel>;
  i: Integer;
  serviceName: string;
begin
  // TODO: remove dependency on lazy type
  serviceType := targetType.Handle;
  if IsLazyType(serviceType) then
    serviceType := GetLazyType(serviceType);
  models := kernel.Registry.FindAll(serviceType).ToArray;

  SetLength(Result, Length(models));
  for i := Low(models) to High(models) do
  begin
    serviceName := models[i].GetServiceName(serviceType);
    Result[i] := Kernel.Resolver.Resolve(context, dependency, serviceName);
  end;
end;

function TDynamicArrayResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
var
  targetType: TRttiType;
  dependencyModel: TDependencyModel;
begin
  targetType := dependency.TargetType;
  Result := targetType.IsDynamicArray
    and inherited CanResolve(context, dependency, argument);
  if Result then
  begin
    targetType := targetType.AsDynamicArray.ElementType;
    dependencyModel := TDependencyModel.Create(targetType, dependency.Target);
    Result := Kernel.Resolver.CanResolve(context, dependencyModel, TValue.From(tkDynArray));
  end;
end;

function TDynamicArrayResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  targetType: TRttiType;
  dependencyModel: TDependencyModel;
  values: TArray<TValue>;
begin
  targetType := dependency.TargetType;
  if not targetType.IsDynamicArray then
    raise EResolveException.CreateResFmt(@SCannotResolveType, [dependency.Name]);
  targetType := targetType.AsDynamicArray.ElementType;
  dependencyModel := TDependencyModel.Create(targetType, dependency.Target);

  values := ResolveDynamicArray(Kernel, context, dependencyModel, targetType);
  Result := TValue.FromArray(dependency.TypeInfo, values);
end;

{$ENDREGION}


{$REGION 'TCollectionResolver'}

function TCollectionResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
const
  SupportedTypes: array[0..4] of string = (
    'IList<>', 'IReadOnlyList<>', 'ICollection<>', 'IReadOnlyCollection<T>', 'IEnumerable<>');
var
  targetType: TRttiType;
  dependencyModel: TDependencyModel;
begin
  targetType := dependency.TargetType;
  Result := targetType.IsGenericType
    and MatchStr(targetType.GetGenericTypeDefinition, SupportedTypes)
    and inherited CanResolve(context, dependency, argument);
  if Result then
  begin
    targetType := GetElementType(targetType.Handle).RttiType;
    dependencyModel := TDependencyModel.Create(targetType, dependency.Target);
    Result := targetType.IsClassOrInterface
      and Kernel.Resolver.CanResolve(context, dependencyModel, TValue.From(tkDynArray));
  end;
end;

function TCollectionResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  itemType: TRttiType;
  dependencyModel: TDependencyModel;
  values: TArray<TValue>;
  list: IInterface;
  i: Integer;
begin
  itemType := GetElementType(dependency.TargetType.Handle).RttiType;
  dependencyModel := TDependencyModel.Create(itemType, dependency.Target);
  values := ResolveDynamicArray(Kernel, context, dependencyModel, itemType);
  case itemType.TypeKind of
    tkClass:
    begin
      list := TCollections.CreateObjectList(itemType.Handle, False);
      for i := Low(values) to High(values) do
        IObjectList(list).Add(values[i].AsObject);
      TValue.Make(@list, dependency.TypeInfo, Result);
    end;
    tkInterface:
    begin
      list := TCollections.CreateInterfaceList(itemType.Handle);
      for i := Low(values) to High(values) do
        Spring.Collections.IInterfaceList(list).Add(values[i].AsInterface);
      TValue.Make(@list, dependency.TypeInfo, Result);
    end;
  else
    raise EResolveException.CreateResFmt(@SCannotResolveType, [dependency.Name]);
  end;
end;

{$ENDREGION}


{$REGION 'TComponentOwnerResolver'}

constructor TComponentOwnerResolver.Create(const kernel: TKernel);
begin
  inherited Create(kernel);
  fVirtualIndex := TType.GetType(TComponent).Constructors.First.VirtualIndex;
end;

function TComponentOwnerResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
var
  method: TRttiMethod;
begin
  if dependency.TypeInfo <> TypeInfo(TComponent) then
    Exit(False);
  if Kernel.Registry.HasService(dependency.TypeInfo) then
    Exit(False);
  if not argument.IsEmpty then
    Exit(False);
  if not (dependency.Target is TRttiParameter) then
    Exit(False);

  method := TRttiMethod(dependency.Target.Parent);
  Result := (method.VirtualIndex = fVirtualIndex)
    and (method.Parent.AsInstance.MetaclassType.InheritsFrom(TComponent));
end;

function TComponentOwnerResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
begin
  TValue.Make(nil, TComponent.ClassInfo, Result);
end;

{$ENDREGION}


{$REGION 'TDecoratorResolver'}

constructor TDecoratorResolver.Create;
begin
  inherited Create;
  fDecorators := TCollections.CreateMultiMap<PTypeInfo, TDecoratorEntry>(
    IEqualityComparer<PTypeInfo>(GetTypeInfoEqualityComparer));
end;

procedure TDecoratorResolver.AddDecorator(decoratedType: PTypeInfo;
  const decoratorModel: TComponentModel;
  const condition: Predicate<TComponentModel>);
var
  entry: TDecoratorEntry;
begin
  entry.DecoratorModel := decoratorModel;
  entry.Condition := condition;
  fDecorators.Add(decoratedType, entry);
end;

function TDecoratorResolver.GetDecorators(decoratedType: PTypeInfo;
  const decoratedModel: TComponentModel): IEnumerable<TComponentModel>;
begin
  Result := TEnumerable.Select<TDecoratorEntry,TComponentModel>(
    fDecorators[decoratedType].Where(
      function(const entry: TDecoratorEntry): Boolean
      begin
        Result := not Assigned(entry.Condition) or entry.Condition(decoratedModel);
      end),
    function(const entry: TDecoratorEntry): TComponentModel
    begin
      Result := entry.DecoratorModel;
    end);
end;

function TDecoratorResolver.Resolve(const dependency: TDependencyModel;
  const model: TComponentModel; const context: ICreationContext;
  const decoratee: TValue): TValue;
var
  decoratorModel: TComponentModel;
  index: Integer;
begin
  Result := decoratee;
  for decoratorModel in GetDecorators(dependency.TypeInfo, model) do
  begin
    // TODO: make this more explicit to just inject on the decorator constructor
    index := context.AddArgument(TTypedValue.Create(Result, dependency.TypeInfo));
    try
      Result := decoratorModel.LifetimeManager.Resolve(context, decoratorModel).Cast(dependency.TypeInfo);
    finally
      context.RemoveTypedArgument(index);
    end;
  end;
end;

{$ENDREGION}


end.
