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

unit Spring.Tests.Container.Components;

interface

uses
  Classes,
  SysUtils,
  TestFramework,
  Spring,
  Spring.Collections,
  Spring.Container,
  Spring.Container.Common,
  Spring.Container.Core,
  Spring.Tests.Container.Interfaces;

type
  {$REGION 'INameService, TNameService and TAnotherNameService'}

  INameService = interface
    ['{96163ACB-E3FD-412E-A9A6-5084CE1BC25A}']
    function GetName: string;
    property Name: string read GetName;
  end;

  IAnotherNameService = interface(INameService)
    ['{C39760F7-C5E6-48BC-A890-FB0103E4AD39}']
  end;

  TNameService = class(TInterfacedObject, INameService)
  private
    fName: string;
    function GetName: string;
  public
    constructor Create;
    property Name: string read GetName;
  public
    const NameString: string = 'Name';
  end;

  TNameServiceWithTwoInterfaces = class(TNameService,
    Spring.Tests.Container.Interfaces.INameService)
  end;

  TAnotherNameService = class(TInterfacedObject, INameService)
  private
    fName: string;
    function GetName: string;
  public
    constructor Create;
    property Name: string read GetName;
  public
    const NameString: string = 'Another Name';
  end;

  IAgeService = interface;

  TDynamicNameService = class(TNameService, IAnotherNameService)
  private
    fAgeService: IAgeService;
  public
    constructor Create(const name: string); overload;
    constructor Create(obj: TObject); overload;
    constructor Create(const name: string; obj: TObject); overload;

    property AgeService: IAgeService read fAgeService;
  end;

  {$ENDREGION}


  {$REGION 'TRefCounted and TCustomNameService'}

  TRefCounted = class(TObject, IRefCounted)
  private
{$IFNDEF AUTOREFCOUNT}
    fRefCount: Integer;
{$ENDIF}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetRefCount: Integer;
  end;

  TCustomNameService = class(TRefCounted, INameService)
  private
    fName: string;
    function GetName: string;
  public
    constructor Create;
    property Name: string read GetName;
  public
    const NameString: string = 'Custom Name';
  end;

  {$ENDREGION}


  {$REGION 'TAgeServiceBase and TAgeServiceImpl'}

  TAgeServiceBase = class abstract
  protected
    function GetAge: Integer; virtual; abstract;
  public
    property Age: Integer read GetAge;
  end;

  TAgeServiceImpl = class(TAgeServiceBase)
  private
    fAge: Integer;
  protected
    function GetAge: Integer; override;
  public
    constructor Create;
    const DefaultAge: Integer = 28;
  end;

  {$ENDREGION}


  {$REGION 'IAgeService and TNameAgeComponent'}

  IAgeService = interface
    ['{E859410D-9980-47A9-8EA7-120C35F6BDCC}']
    function GetAge: Integer;
    property Age: Integer read GetAge;
  end;

  TNameAgeComponent = class(TInterfacedObject, INameService, IAgeService)
  private
    fAge: Integer;
    function GetName: string;
    function GetAge: Integer;
  public
    constructor Create(age: Integer); overload;
    property Name: string read GetName;
    property Age: Integer read GetAge;
    const NameString: string = 'Complex';
    const DefaultAge: Integer = 100;
  end;

  TAgeService = class(TInterfacedObject, IAgeService)
  private
    fAgeService: Lazy<IAgeService>;
    function GetAge: Integer;
    function GetAgeService: IAgeService;
  public
    constructor Create(const ageService: Lazy<IAgeService>);
    property AgeService: IAgeService read GetAgeService;
  end;

  TAgeServiceDecorator = class(TInterfacedObject, IAgeService)
  private
    fAgeServive: IAgeService;
  public
    constructor Create(const ageService: IAgeService);
    function GetAge: Integer;
    property AgeService: IAgeService read fAgeServive;
  end;

  TAgeServiceDecorator2 = class(TInterfacedObject, IAgeService)
  private
    fAgeServive: IAgeService;
  public
    constructor Create(const ageService: IAgeService);
    function GetAge: Integer;
    property AgeService: IAgeService read fAgeServive;
  end;

  {$ENDREGION}


  {$REGION 'TNameServiceWithAggregation'}

  TNameServiceWithAggregation = class(TNameService)
  private
    fMethodCallCount: Integer;
  protected
    [Inject]
    fAgeService: IAgeService;
  public
    [Inject]
    procedure Init;
    [Inject]
    property AgeService: IAgeService read fAgeService write fAgeService;
    property MethodCallCount: Integer read fMethodCallCount;
  end;

  {$ENDREGION}


  {$REGION 'IAnotherService, TInitializableComponent and TRecyclableComponent'}

  IAnotherService = interface
    ['{6BE967C9-C4EE-40FD-805D-B48320A0F510}']
  end;

  TInitializableComponent = class(TInterfacedObject, IInitializable, IAnotherService)
  private
    fIsInitialized: Boolean;
  public
    procedure Initialize;
    property IsInitialized: Boolean read fIsInitialized;
  end;

  TRecyclableComponent = class(TInterfacedObject, IInitializable, IRecyclable, IAnotherService)
  private
    fIsInitialized: Boolean;
    fIsRecycled: Boolean;
  public
    procedure Initialize;
    procedure Recycle;
    property IsInitialized: Boolean read fIsInitialized;
    property IsRecycled: Boolean read fIsRecycled;
  end;

  TDisposableComponent = class(TInterfacedObject, IInitializable, IDisposable, IAnotherService)
  private
    fIsInitialized: Boolean;
    fOnDispose: TProc;
  public
    procedure Initialize;
    procedure Dispose;
    property OnDispose: TProc read fOnDispose write fOnDispose;
  end;

  TAnotherServiceDecorator = class(TInterfacedObject, IAnotherService)
  private
    fAnotherService: IAnotherService;
  public
    constructor Create(const anotherService: IAnotherService);
    property Service: IAnotherService read fAnotherService;
  end;

  {$ENDREGION}


  {$REGION 'TBootstrapComponent'}

  TBootstrapComponent = class
  private
    fNameService: INameService;
    fAgeService: TAgeServiceBase;
  public
    constructor Create(const nameService: INameService); overload;
    constructor Create(const nameService: INameService; ageService: TAgeServiceBase); overload;
    property NameService: INameService read fNameService;
    property AgeService: TAgeServiceBase read fAgeService;
  end;

  {$ENDREGION}


  {$REGION 'TPrimitiveComponent'}

  IPrimitive = interface
    ['{827709E0-7B09-43E7-A438-055E9800B4CA}']
    function GetNameService: INameService;
    function GetIntegerArg: Integer;
    function GetStringArg: string;
    property NameService: INameService read GetNameService;
    property IntegerArg: Integer read GetIntegerArg;
    property StringArg: string read GetStringArg;
  end;

  TPrimitiveComponent = class(TInterfacedObject, IPrimitive)
  private
    fNameService: INameService;
    fIntegerArg: Integer;
    fStringArg: string;
    fIntegerProperty: Integer;
    fStringProperty: string;
  protected
    { IPrimitive }
    function GetNameService: INameService;
    function GetIntegerArg: Integer;
    function GetStringArg: string;
  public
    constructor Create(const nameService: INameService; integerArg: Integer;
      const stringArg: string);
    property NameService: INameService read fNameService;
    property IntegerArg: Integer read fIntegerArg;
    property StringArg: string read fStringArg;
    property IntegerProperty: Integer read fIntegerProperty;
    property StringProperty: string read fStringProperty;
  end;

  {$ENDREGION}


  {$REGION 'IInjectionExplorer and TInjectionServiceImpl'}

  IInjectionExplorer = interface
    ['{47439E73-A21E-48DB-A1C9-0A14639FFC01}']
    {$REGION 'Property Getters and Setters'}

    function GetConstructorInjection: INameService;
    function GetPropertyInjection: INameService;
    function GetMethodInjection: INameService;
    function GetFieldInjection: INameService;

    {$ENDREGION}

    property ConstructorInjection: INameService read GetConstructorInjection;
    property PropertyInjection: INameService read GetPropertyInjection;
    property MethodInjection: INameService read GetMethodInjection;
    property FieldInjection: INameService read GetFieldInjection;
  end;

  TInjectionExplorer = class(TInterfacedObject, IInjectionExplorer)
  private
    fConstructorInjection: INameService;
    fPropertyInjection: INameService;
    fMethodInjection: INameService;
    fFieldInjection: INameService;
  protected
    { Implements IInjectionService }
    function GetConstructorInjection: INameService;
    function GetPropertyInjection: INameService;
    function GetMethodInjection: INameService;
    function GetFieldInjection: INameService;
  public
    constructor Create(const service: INameService); overload;
    constructor Create(const nameService: INameService; const anotherService: INameService); overload;
    procedure SetMethodInjection(const value: INameService);
    property PropertyInjection: INameService read fPropertyInjection write fPropertyInjection;
  end;

  TInjectionExplorerComponent = class(TInterfacedObject, IInjectionExplorer)
  private
    fConstructorInjection: INameService;
    fPropertyInjection: INameService;
    fMethodInjection: INameService;
    [Inject]
    fFieldInjection: INameService;
  protected
    { Implements IInjectionService }
    function GetConstructorInjection: INameService;
    function GetPropertyInjection: INameService;
    function GetMethodInjection: INameService;
    function GetFieldInjection: INameService;
  public
    [Inject]
    constructor Create(const service: INameService); overload;
    constructor Create(const nameService: INameService; const anotherService: INameService); overload;
    [Inject]
    procedure SetMethodInjection(const value: INameService);
    [Inject]
    property PropertyInjection: INameService read fPropertyInjection write fPropertyInjection;
  end;

  TInjectionComponent = class(TInterfacedObject, IInjectionExplorer)
  private
    fConstructorInjection: INameService;
    fPropertyInjection: INameService;
    fMethodInjection: INameService;
    [Inject('default')]
    fFieldInjection: INameService;
  protected
    { Implements IInjectionService }
    function GetConstructorInjection: INameService;
    function GetPropertyInjection: INameService;
    function GetMethodInjection: INameService;
    function GetFieldInjection: INameService;
  public
    [Inject]
    constructor Create([Inject('default')] const service: INameService); overload;
    constructor Create(const nameService: INameService; const anotherService: INameService); overload;
    [Inject]
    procedure SetMethodInjection([Inject('another')]const value: INameService);
    [Inject('another')]
    property PropertyInjection: INameService read fPropertyInjection write fPropertyInjection;
  end;

  {$ENDREGION}


  {$REGION 'Non-Guid Interface Services and Implementations'}

  INonGuid = interface
  end;

  TNonGuid = class(TInterfacedObject, INonGuid)
  end;

  INonGuid<T> = interface
  end;

  TNonGuid<T> = class(TInterfacedObject, INonGuid<T>)
  end;

  {$ENDREGION}


  {$REGION 'Circular Dependency Services & Implementations ("Chicken-Egg")'}

  // IChicken <== TChicken --> IEgg <== TEgg --> IChicken

  IEgg = interface;

  IChicken = interface
    ['{88C4F5E9-85B4-43D4-9265-0A9FAD099055}']
    function GetEgg: IEgg;
    procedure SetEgg(const egg: IEgg);
    property Egg: IEgg read GetEgg write SetEgg;
  end;

  IEgg = interface
    ['{9BFC513F-635C-42CD-B29D-9E66D47882A6}']
    function Chicken: IChicken;
  end;

  TChicken = class(TInterfacedObject, IChicken)
  private
    fEgg: IEgg;
    function GetEgg: IEgg;
    procedure SetEgg(const egg: IEgg);
  public
    constructor Create(const egg: IEgg);
    property Egg: IEgg read GetEgg write SetEgg;
  end;

  TEgg = class(TInterfacedObject, IEgg)
  private
    fChicken: IChicken;
    function Chicken: IChicken;
  public
    constructor Create(const chicken: IChicken);
  end;

  TCircularDependencyChicken = class(TInterfacedObject, IChicken)
  private
    fChicken: IChicken;
    function GetEgg: IEgg;
    procedure SetEgg(const egg: IEgg);
  public
    constructor Create(const chicken: IChicken);
  end;

  {$ENDREGION}


  {$REGION 'Lazy dependencies'}

  TNameServiceLazyWithFunc = class(TInterfacedObject, INameService)
  private
    [Inject('lazy')]
    fNameService: TFunc<INameService>;
  public
    function GetName: string;
  end;

  TNameServiceLazyWithRecord = class(TInterfacedObject, INameService)
  private
    [Inject('lazy')]
    fNameService: Lazy<INameService>;
  public
    function GetName: string;
  end;

  TNameServiceLazyWithInterface = class(TInterfacedObject, INameService)
  private
    [Inject('lazy')]
    fNameService: ILazy<INameService>;
  public
    function GetName: string;
  end;

  {$ENDREGION}


  {$REGION 'Many dependencies'}

  ICollectionItem = interface
    ['{9B5C78C1-8077-4866-A854-14A20D632342}']
  end;

  TCollectionItemA = class(TInterfacedObject, ICollectionItem)
  end;

  TCollectionItemB = class(TInterfacedObject, ICollectionItem)
  end;

  TCollectionItemC = class(TInterfacedObject, ICollectionItem)
  end;

  TCollectionItemD = class(TInterfacedObject, ICollectionItem)
  private
    fCollectionItems: TArray<ICollectionItem>;
  public
    constructor Create(const collectionItems: TArray<ICollectionItem>);
    property CollectionItems: TArray<ICollectionItem> read fCollectionItems;
  end;

  ICollectionService = interface
    ['{31D36D13-CC5A-4FFB-A285-3146EDBAECAB}']
    function GetCollectionItems: TArray<ICollectionItem>;
    property CollectionItems: TArray<ICollectionItem> read GetCollectionItems;
  end;

  TCollectionService = class abstract(TInterfacedObject, ICollectionService)
  protected
    fCollectionItems: TArray<ICollectionItem>;
    function GetCollectionItems: TArray<ICollectionItem>; virtual;
  public
    property CollectionItems: TArray<ICollectionItem> read GetCollectionItems;
  end;

  TCollectionServiceA = class(TCollectionService)
  public
    [Inject]
    constructor Create(const collectionItems: TArray<ICollectionItem>);
  end;

  TCollectionServiceB = class(TCollectionService)
  public
    [Inject]
    constructor Create(const collectionItems: IEnumerable<ICollectionItem>);
  end;

  TCollectionServiceC = class(TCollectionService)
  protected
    fCollectionItemFactories: TArray<TFunc<ICollectionItem>>;
    function GetCollectionItems: TArray<ICollectionItem>; override;
  public
    [Inject]
    constructor Create(const collectionItems: TArray<TFunc<ICollectionItem>>);
  end;

  TCollectionServiceD = class(TCollectionService)
  public
    constructor Create(const collectionItem: ICollectionItem);
  end;

  {$ENDREGION}


  ISomeService = interface
    ['{3DD10F1E-C064-47DB-B78E-F7EB9B643FB3}']
  end;

  TSomeService = class(TInterfacedObject, ISomeService);

  ICommonInterface = interface
    ['{0B64ADC9-A375-4614-96EA-2BF38E34FCD9}']
  end;

  ISomeFactory = interface
    ['{8E32B7D2-49BC-4F85-9C2D-79295D9A0901}']
    function CreateObject(const name: string): ICommonInterface;
  end;

  TSomeFactory = class(TInterfacedObject, ISomeFactory)
  private
    fContainer: TContainer;
  public
    constructor Create(const container: TContainer);
    function CreateObject(const name: string): ICommonInterface;
  end;

  TTypeA = class(TInterfacedObject, ICommonInterface)
  public
    constructor Create(const someService: ISomeService);
  end;

  TTypeB = class(TInterfacedObject, ICommonInterface)
  public
    constructor Create(const someService: ISomeService);
  end;

implementation

uses
  StrUtils;

{ TNameService }

constructor TNameService.Create;
begin
  inherited Create;
  fName := NameString;
end;

function TNameService.GetName: string;
begin
  Result := fName;
end;

{ TAnotherNameService }

constructor TAnotherNameService.Create;
begin
  inherited Create;
  fName := NameString;
end;

function TAnotherNameService.GetName: string;
begin
  Result := fName;
end;

{ TDynamicNameService }

constructor TDynamicNameService.Create(const name: string);
begin
  inherited Create;
  fName := name;
end;

constructor TDynamicNameService.Create(obj: TObject);
begin
  inherited Create;
  fName := obj.ToString;
end;

constructor TDynamicNameService.Create(const name: string; obj: TObject);
begin
  inherited Create;
  fName := name + obj.ToString;
end;

{ TAgeServiceImpl }

constructor TAgeServiceImpl.Create;
begin
  inherited Create;
  fAge := DefaultAge;
end;

function TAgeServiceImpl.GetAge: Integer;
begin
  Result := fAge;
end;

{ TBootstrapComponent }

constructor TBootstrapComponent.Create(const nameService: INameService);
begin
  Guard.CheckNotNull(nameService, 'nameService');
  inherited Create;
  fNameService := nameService;
end;

constructor TBootstrapComponent.Create(const nameService: INameService;
  ageService: TAgeServiceBase);
begin
  Guard.CheckNotNull(nameService, 'nameService');
  Guard.CheckNotNull(ageService, 'ageService');
  inherited Create;
  fNameService := nameService;
  fAgeService := ageService;
end;

{ TPrimitiveComponent }

constructor TPrimitiveComponent.Create(const nameService: INameService;
  integerArg: Integer; const stringArg: string);
begin
  Guard.CheckNotNull(nameService, 'nameService');
  inherited Create;
  fNameService := nameService;
  fIntegerArg := integerArg;
  fStringArg := stringArg;
end;

function TPrimitiveComponent.GetNameService: INameService;
begin
  Result := fNameService;
end;

function TPrimitiveComponent.GetIntegerArg: Integer;
begin
  Result := fIntegerArg;
end;

function TPrimitiveComponent.GetStringArg: string;
begin
  Result := fStringArg;
end;

{ TInjectionExplorer }

constructor TInjectionExplorer.Create(const service: INameService);
begin
  inherited Create;
  fConstructorInjection := service;
end;

constructor TInjectionExplorer.Create(const nameService: INameService;
  const anotherService: INameService);
begin
  raise Exception.Create('This constructor should not be called.');
end;

function TInjectionExplorer.GetConstructorInjection: INameService;
begin
  Result := fConstructorInjection;
end;

function TInjectionExplorer.GetPropertyInjection: INameService;
begin
  Result := fPropertyInjection;
end;

function TInjectionExplorer.GetMethodInjection: INameService;
begin
  Result := fMethodInjection;
end;

function TInjectionExplorer.GetFieldInjection: INameService;
begin
  Result := fFieldInjection;
end;

procedure TInjectionExplorer.SetMethodInjection(const value: INameService);
begin
  fMethodInjection := value;
end;

{ TInjectionExplorerComponent }

constructor TInjectionExplorerComponent.Create(const service: INameService);
begin
  Guard.CheckTrue(fConstructorInjection = nil, 'This constructor should only called once.');
  inherited Create;
  fConstructorInjection := service;
end;

constructor TInjectionExplorerComponent.Create(const nameService: INameService;
  const anotherService: INameService);
begin
  raise Exception.Create('This constructor should not be called.');
end;

function TInjectionExplorerComponent.GetConstructorInjection: INameService;
begin
  Result := fConstructorInjection;
end;

function TInjectionExplorerComponent.GetPropertyInjection: INameService;
begin
  Result := fPropertyInjection;
end;

function TInjectionExplorerComponent.GetMethodInjection: INameService;
begin
  Result := fMethodInjection;
end;

function TInjectionExplorerComponent.GetFieldInjection: INameService;
begin
  Result := fFieldInjection;
end;

procedure TInjectionExplorerComponent.SetMethodInjection(const value: INameService);
begin
  fMethodInjection := value;
end;

{ TCircularDependencyChicken }

constructor TCircularDependencyChicken.Create(const chicken: IChicken);
begin
  inherited Create;
  fChicken := chicken;
end;

function TCircularDependencyChicken.GetEgg: IEgg;
begin
  Result := nil;
end;

procedure TCircularDependencyChicken.SetEgg(const egg: IEgg);
begin
end;

{ TChicken }

constructor TChicken.Create(const egg: IEgg);
begin
  inherited Create;
  fEgg := egg;
end;

function TChicken.GetEgg: IEgg;
begin
  Result := fEgg;
end;

procedure TChicken.SetEgg(const egg: IEgg);
begin
  fEgg := egg;
end;

{ TEgg }

constructor TEgg.Create(const chicken: IChicken);
begin
  inherited Create;
  fChicken := chicken;
end;

function TEgg.Chicken: IChicken;
begin
  Result := fChicken
end;

{ TNameAgeComponent }

constructor TNameAgeComponent.Create(age: Integer);
begin
  fAge := age;
end;

function TNameAgeComponent.GetAge: Integer;
begin
  Result := TNameAgeComponent.DefaultAge;
  if fAge > 0 then
    Result := fAge;
end;

function TNameAgeComponent.GetName: string;
begin
  Result := TNameAgeComponent.NameString
end;

{ TInjectionComponent }

constructor TInjectionComponent.Create(const service: INameService);
begin
  inherited Create;
  fConstructorInjection := service;
end;

constructor TInjectionComponent.Create(const nameService,
  anotherService: INameService);
begin
  raise Exception.Create('The constructor should not be called.');
end;

function TInjectionComponent.GetConstructorInjection: INameService;
begin
  Result := fConstructorInjection;
end;

function TInjectionComponent.GetFieldInjection: INameService;
begin
  Result := fFieldInjection;
end;

function TInjectionComponent.GetMethodInjection: INameService;
begin
  Result := fMethodInjection;
end;

function TInjectionComponent.GetPropertyInjection: INameService;
begin
  Result := fPropertyInjection;
end;

procedure TInjectionComponent.SetMethodInjection(const value: INameService);
begin
  fMethodInjection := value;
end;

{ TInitializableComponent }

procedure TInitializableComponent.Initialize;
begin
  fIsInitialized := True;
end;

{ TNameServiceWithAggregation }

procedure TNameServiceWithAggregation.Init;
begin
  Inc(fMethodCallCount);
end;

{ TNameServiceLazyWithFunc }

function TNameServiceLazyWithFunc.GetName: string;
begin
  Result := fNameService.Name;
end;

{ TNameServiceLazyWithRecord }

function TNameServiceLazyWithRecord.GetName: string;
begin
  Result := fNameService.Value.Name;
end;

{ TNameServiceLazyWithInterface }

function TNameServiceLazyWithInterface.GetName: string;
begin
  Result := fNameService.Value.Name;
end;

{ TRefCounted }

function TRefCounted.GetRefCount: Integer;
begin
  Result := fRefCount;
end;

function TRefCounted.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TRefCounted._AddRef: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Inc(fRefCount);
  Result := fRefCount;
{$ELSE}
  Result := __ObjAddRef;
{$ENDIF}
end;

function TRefCounted._Release: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Dec(fRefCount);
  Result := fRefCount;
  if Result = 0 then
    Destroy;
{$ELSE}
  Result := __ObjRelease;
{$ENDIF}
end;

{ TCustomNameService }

constructor TCustomNameService.Create;
begin
  inherited Create;
  fName := NameString;
end;

function TCustomNameService.GetName: string;
begin
  Result := fName;
end;

{ TRecyclableComponent }

procedure TRecyclableComponent.Initialize;
begin
  fIsInitialized := True;
  fIsRecycled := False;
end;

procedure TRecyclableComponent.Recycle;
begin
  fIsInitialized := False;
  fIsRecycled := True;
end;

{ TDisposableComponent }

procedure TDisposableComponent.Dispose;
begin
  if Assigned(fOnDispose) then
    fOnDispose;
end;

procedure TDisposableComponent.Initialize;
begin
  if not fIsInitialized then
    fIsInitialized := True
  else
    raise Exception.Create('Initialize called twice');
end;

{ TAgeServiceDecorator }

constructor TAgeServiceDecorator.Create(const ageService: IAgeService);
begin
  fAgeServive := ageService;
end;

function TAgeServiceDecorator.GetAge: Integer;
begin
  Result := fAgeServive.Age;
end;

{ TAgeServiceDecorator2 }

constructor TAgeServiceDecorator2.Create(const ageService: IAgeService);
begin
  fAgeServive := ageService;
end;

function TAgeServiceDecorator2.GetAge: Integer;
begin
  Result := fAgeServive.Age;
end;

{ TCollectionService }

function TCollectionService.GetCollectionItems: TArray<ICollectionItem>;
begin
  Result := fCollectionItems;
end;

{ TCollectionServiceA }

constructor TCollectionServiceA.Create(
  const collectionItems: TArray<ICollectionItem>);
begin
  fCollectionItems := collectionItems;
end;

{ TCollectionServiceB }

constructor TCollectionServiceB.Create(
  const collectionItems: IEnumerable<ICollectionItem>);
begin
  fCollectionItems := collectionItems.ToArray;
end;

{ TCollectionItemD }

constructor TCollectionItemD.Create(
  const collectionItems: TArray<ICollectionItem>);
begin
  fCollectionItems := collectionItems;
end;

{ TCollectionServiceC }

constructor TCollectionServiceC.Create(
  const collectionItems: TArray<TFunc<ICollectionItem>>);
begin
  fCollectionItemFactories := collectionItems;
end;

function TCollectionServiceC.GetCollectionItems: TArray<ICollectionItem>;
var
  i: Integer;
begin
  if not Assigned(fCollectionItems) then
  begin
    SetLength(fCollectionItems, Length(fCollectionItemFactories));
    for i := Low(fCollectionItemFactories) to High(fCollectionItemFactories) do
      fCollectionItems[i] := fCollectionItemFactories[i]();
  end;
  Result := fCollectionItems;
end;

{ TCollectionServiceD }

constructor TCollectionServiceD.Create(const collectionItem: ICollectionItem);
begin
  inherited Create;;
  SetLength(fCollectionItems, 1);
  fCollectionItems[0] := collectionItem;
end;

{ TSomeFactory }

constructor TSomeFactory.Create(const container: TContainer);
begin
  inherited Create;
  fContainer := container;
end;

function TSomeFactory.CreateObject(const name: string): ICommonInterface;
const
  args: array[0..1] of string = ('A', 'B');
begin
  case IndexStr(name, args) of
    0: Result := fContainer.Resolve<TTypeA>;
    1: Result := fContainer.Resolve<TTypeB>;
  else
    raise ENotSupportedException.Create('');
  end;
end;

{ TTypeA }

constructor TTypeA.Create(const someService: ISomeService);
begin
end;

{ TTypeB }

constructor TTypeB.Create(const someService: ISomeService);
begin
end;

{ TAnotherServiceDecorator }

constructor TAnotherServiceDecorator.Create(
  const anotherService: IAnotherService);
begin
  inherited Create;
  fAnotherService := anotherService;
end;

{ TAgeService }

constructor TAgeService.Create(const ageService: Lazy<IAgeService>);
begin
  fAgeService := ageService;
end;

function TAgeService.GetAge: Integer;
begin
  Result := fAgeService.Value.Age;
end;

function TAgeService.GetAgeService: IAgeService;
begin
  Result := fAgeService;
end;

end.
