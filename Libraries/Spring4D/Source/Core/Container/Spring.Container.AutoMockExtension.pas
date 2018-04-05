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

unit Spring.Container.AutoMockExtension;

interface

uses
  Spring.Container.Extensions;

type
  TAutoMockExtension = class(TContainerExtension)
  protected
    procedure Initialize; override;
  end;

implementation

uses
  Rtti,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Container.Common,
  Spring.Container.Core,
  Spring.Reflection,
  Spring.Mocking,
  Spring.Mocking.Core;

type
  TAutoMockResolver = class(TInterfacedObject, ISubDependencyResolver)
  private
    fKernel: IKernel;
    procedure EnsureMockRegistered(const mockedType: TRttiType);
    class function TryGetMockedType(const targetType: TRttiType;
      out mockedType: TRttiType): Boolean; static;
  public
    constructor Create(const kernel: IKernel);

    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): TValue;
  end;


{$REGION 'TAutoMockExtension'}

procedure TAutoMockExtension.Initialize;
begin
  Kernel.Resolver.AddSubResolver(
    TAutoMockResolver.Create(Kernel) as ISubDependencyResolver);
end;

{$ENDREGION}


{$REGION 'TAutoMockResolver'}

constructor TAutoMockResolver.Create(const kernel: IKernel);
begin
  inherited Create;
  fKernel := kernel;
end;

function TAutoMockResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
var
  mockedType: TRttiType;
begin
  if dependency.TargetType.IsGenericType
    and TryGetMockedType(dependency.TargetType, mockedType)
    and mockedType.IsInterface and not mockedType.IsType<IInterface> then
    Exit(True);

  if dependency.TargetType.IsInterface and not IsLazyType(dependency.TypeInfo) then
    if argument.IsEmpty then
      Exit(not fKernel.Registry.HasService(dependency.TypeInfo))
    else
      if argument.IsType<string> then
        Exit(not fKernel.Registry.HasService(dependency.TypeInfo, argument.AsString));

  Result := False;
end;

procedure TAutoMockResolver.EnsureMockRegistered(const mockedType: TRttiType);
var
  mockName: string;
  mockModel: TComponentModel;
begin
  mockName := 'IMock<' + mockedType.DefaultName + '>';
  if not fKernel.Registry.HasService(mockName) then
  begin
    // only for interfaces
    mockModel := fKernel.Registry.RegisterComponent(TMock<IInterface>.ClassInfo);
    fKernel.Registry.RegisterService(mockModel, TypeInfo(IMock<IInterface>), mockName);
    mockModel.ActivatorDelegate :=
      function: TValue
      var
        mock: TMock;
      begin
        mock := TMock<IInterface>.NewInstance as TMock;
        mock.Create(mockedType.Handle);
        Result := mock;
      end;
    mockModel.LifetimeType := TLifetimeType.Singleton;
    fKernel.Builder.Build(mockModel);
  end;
end;

function TAutoMockResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  mockDirectly: Boolean;
  mockedType: TRttiType;
  mockName: string;
begin
  mockDirectly := dependency.TargetType.IsGenericType
    and TryGetMockedType(dependency.TargetType, mockedType);
  if not mockDirectly then
    mockedType := dependency.TargetType;
  mockName := 'IMock<' + mockedType.DefaultName + '>';
  EnsureMockRegistered(mockedType);
  Result := (fKernel as IKernelInternal).Resolve(mockName);
  if mockDirectly then
  begin
    TValueData(Result).FTypeInfo := dependency.TargetType.Handle;
    Exit;
  end
  else
    Result := (Result.AsType<IMock<IInterface>> as IMock).Instance;
end;

class function TAutoMockResolver.TryGetMockedType(const targetType: TRttiType;
  out mockedType: TRttiType): Boolean;
begin
  if SameText(targetType.GetGenericTypeDefinition, 'IMock<>') then
    mockedType := targetType.GetMethod('GetInstance').ReturnType
  else if SameText(targetType.GetGenericTypeDefinition, 'Mock<>') then
    mockedType := targetType.GetField('fMock').FieldType.GetMethod('GetInstance').ReturnType
  else
    Exit(False);
  Result := True;
end;

{$ENDREGION}


end.
