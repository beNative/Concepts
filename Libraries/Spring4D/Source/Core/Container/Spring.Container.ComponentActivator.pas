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

unit Spring.Container.ComponentActivator;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  /// <summary>
  ///   Abstract ComponentActivator
  /// </summary>
  TComponentActivatorBase = class abstract(TInterfacedObject, IComponentActivator)
  private
    fKernel: TKernel;
    fModel: TComponentModel;
  protected
    procedure ExecuteInjections(var instance: TValue; const context: ICreationContext); overload;
    procedure ExecuteInjections(const instance: TValue;
      const injections: IList<IInjection>; const context: ICreationContext); overload;
    property Kernel: TKernel read fKernel;
    property Model: TComponentModel read fModel;
  public
    constructor Create(const kernel: TKernel; const model: TComponentModel);
    function CreateInstance(const context: ICreationContext): TValue; overload; virtual; abstract;
  end;

  /// <summary>
  ///   Activates an instance by reflection.
  /// </summary>
  TReflectionComponentActivator = class(TComponentActivatorBase)
  protected
    function SelectEligibleConstructor(
      const context: ICreationContext): IInjection; virtual;
    function TryHandle(const context: ICreationContext;
      const candidate: IInjection; var winner: IInjection): Boolean; virtual;
  public
    function CreateInstance(const context: ICreationContext): TValue; override;
  end;

  /// <summary>
  ///   Activates an instance by a TActivatorDelegate delegate.
  /// </summary>
  TDelegateComponentActivator = class(TComponentActivatorBase)
  public
    function CreateInstance(const context: ICreationContext): TValue; override;
  end;

implementation

uses
  Rtti,
  SysUtils,
  TypInfo,
  Spring.Container.Common,
  Spring.Container.ResourceStrings,
  Spring.Reflection;

type
  TInterfacedObjectAccess = class(TInterfacedObject);

const
  SingletonLifetimes = [
    TLifetimeType.Singleton,
    TLifetimeType.PerResolve,
    TLifetimeType.SingletonPerThread
  ];


{$REGION 'TComponentActivatorBase'}

constructor TComponentActivatorBase.Create(const kernel: TKernel;
  const model: TComponentModel);
begin
  Guard.CheckNotNull(kernel, 'kernel');
  Guard.CheckNotNull(model, 'model');
  inherited Create;
  fKernel := kernel;
  fModel := model;
end;

procedure TComponentActivatorBase.ExecuteInjections(var instance: TValue;
  const context: ICreationContext);
var
  doRefCountRelease: Boolean;
begin
  doRefCountRelease := False;
  if Model.LifetimeType in SingletonLifetimes then
  begin
    context.AddPerResolve(Model, instance);
    if (instance.Kind = tkClass) and (TObject(TValueData(instance).FAsObject) is TInterfacedObject) then
    begin
      AtomicIncrement(TInterfacedObjectAccess(TValueData(instance).FAsObject).FRefCount);
      doRefCountRelease := True;
    end;
  end;
  try
    try
      ExecuteInjections(instance, Model.FieldInjections, context);
      ExecuteInjections(instance, Model.PropertyInjections, context);
      ExecuteInjections(instance, Model.MethodInjections, context);
    except
      on E: Exception do
      begin
        if not instance.IsEmpty and instance.IsObject then
        begin
          instance.AsObject.Free;
          instance := nil;
        end;
        if E is EContainerException then
          raise
        else
          Exception.RaiseOuterException(EResolveException.CreateResFmt(
            @SCannotResolveType, [Model.ComponentTypeName]));
      end;
    end;
  finally
    if doRefCountRelease then
      AtomicDecrement(TInterfacedObjectAccess(TValueData(instance).FAsObject).FRefCount);
  end;
end;

procedure TComponentActivatorBase.ExecuteInjections(const instance: TValue;
  const injections: IList<IInjection>; const context: ICreationContext);
var
  injection: IInjection;
  arguments: TArray<TValue>;
begin
  for injection in injections do
  begin
    fKernel.Logger.Log('injecting ' + injection.Target.ToString);
    arguments := Kernel.Resolver.Resolve(
      context, injection.Dependencies, injection.Arguments);
    injection.Inject(instance, arguments);
  end;
end;

{$ENDREGION}


{$REGION 'TReflectionComponentActivator'}

function TReflectionComponentActivator.CreateInstance(
  const context: ICreationContext): TValue;
var
  injection: IInjection;
  arguments: TArray<TValue>;
begin
  injection := SelectEligibleConstructor(context);
  if injection = nil then
    raise EActivatorException.CreateResFmt(@SUnsatisfiedConstructor, [Model.ComponentTypeName]);
  arguments := Kernel.Resolver.Resolve(
    context, injection.Dependencies, injection.Arguments);
  Result := TActivator.CreateInstance(
    Model.ComponentType.AsInstance, injection.Target.AsMethod, arguments);
  ExecuteInjections(Result, context);
end;

function TReflectionComponentActivator.SelectEligibleConstructor(
  const context: ICreationContext): IInjection;
var
  candidate, winner: IInjection;
  maxCount: Integer;
begin
  winner := nil;
  maxCount := -1;

  for candidate in Model.ConstructorInjections do
  begin
    if candidate.Target.HasCustomAttribute<InjectAttribute> then
    begin
      fKernel.Logger.Log('selected by [Inject] attribute ' +
        StringReplace(candidate.Target.ToString,
        ' ', ' ' + candidate.Target.Parent.DefaultName + '.', []));
      winner := candidate;
      Break;
    end;
    if candidate.DependencyCount > maxCount then
      if TryHandle(context, candidate, winner) then
        maxCount := winner.DependencyCount;
  end;
  Result := winner;
end;

function TReflectionComponentActivator.TryHandle(const context: ICreationContext;
  const candidate: IInjection; var winner: IInjection): Boolean;
var
  injection: IInjection;
begin
  fKernel.Logger.Log('inspecting ' + StringReplace(candidate.Target.ToString,
    ' ', ' ' + candidate.Target.Parent.DefaultName + '.', []));
  Result := context.TryHandle(candidate, injection)
    and Kernel.Resolver.CanResolve(
    context, injection.Dependencies, injection.Arguments);
  if Result then
    winner := injection;
end;

{$ENDREGION}


{$REGION 'TDelegateComponentActivator'}

function TDelegateComponentActivator.CreateInstance(
  const context: ICreationContext): TValue;
begin
  if not Assigned(Model.ActivatorDelegate) then
    raise EActivatorException.CreateRes(@SActivatorDelegateExpected);
  fKernel.Logger.Log('using delegate to retrieve instance');
  Result := Model.ActivatorDelegate();
  ExecuteInjections(Result, context);
end;

{$ENDREGION}


end.
