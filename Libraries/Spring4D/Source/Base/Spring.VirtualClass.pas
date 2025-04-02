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

unit Spring.VirtualClass;

interface

{$R-}

uses
  Classes,
  SyncObjs,
  TypInfo;

{$IF not Declared(CPP_ABI_ADJUST)}
const
  CPP_ABI_ADJUST = 0;
{$IFEND}

const
  MinVirtualIndex = -11 - (CPP_ABI_ADJUST div SizeOf(Pointer));

type
{$POINTERMATH ON}
  PVirtualMethodTable = ^Pointer;
{$POINTERMATH OFF}

  TEquals = function (const Self: TObject; Obj: TObject): Boolean;
  TGetHashCode = function (const Self: TObject): Integer;
  TToString = function (const Self: TObject): string;
  TSafeCallException = function (const Self: TObject; ExceptObject: TObject;
    ExceptAddr: Pointer): HResult;
  TAfterConstruction = procedure (const Self: TObject);
  TBeforeDestruction = procedure (const Self: TObject);
  TDispatch = procedure (const Self: TObject; var Message);
  TDefaultHandler = procedure (const Self: TObject; var Message);
  TNewInstance = function (Self: TClass): TObject;
  TFreeInstance = procedure (const Self: TObject);
  TDestroy = procedure (const Self: TObject; OuterMost: ShortInt);

  PClass = ^TClass;
  PClassData = ^TClassData;
  TClassData = record
  strict private
    function GetVirtualMethodCount: Integer;
  public
    SelfPtr: TClass;
    IntfTable: PInterfaceTable;
    AutoTable: Pointer;
    InitTable: PTypeInfo;
    TypeInfo: PTypeInfo;
    FieldTable: PVmtFieldTable;
    MethodTable: PVmtMethodTable;
    DynamicTable: Pointer;
    ClassName: PShortString;
    InstanceSize: Integer;
    Parent: PClass;

    Equals: TEquals;
    GetHashCode: TGetHashCode;
    ToString: TToString;
    SafeCallException: TSafeCallException;
    AfterConstruction: TAfterConstruction;
    BeforeDestruction: TBeforeDestruction;
    Dispatch: TDispatch;
    DefaultHandler: TDefaultHandler;
    NewInstance: TNewInstance;
    FreeInstance: TFreeInstance;
    Destroy: TDestroy;

{$IF CPP_ABI_ADJUST > 0)}
    CPP_API: array[1..CPP_ABI_ADJUST div SizeOf(Pointer)] of Pointer;
{$IFEND}

    VirtualMethods: array[0..999] of Pointer;

    property VirtualMethodCount: Integer read GetVirtualMethodCount;
  end;

  TVirtualClass = class
  private
    fProxyClass: TClass;
    function GetClassProxyData: PClassData; inline;
  public
    constructor Create(classType: TClass);
    destructor Destroy; override;

    property ProxyClass: TClass read fProxyClass;
    property ProxyClassData: PClassData read GetClassProxyData;
  end;

  TVirtualClasses = class
  strict private
    fClasses: TList;
    fLock: TCriticalSection;
    class var fDefault: TVirtualClasses;
  protected
    class property Default: TVirtualClasses read fDefault;
  public
    constructor Create;
    destructor Destroy; override;

    class constructor Create;
    class destructor Destroy;

    function GetVirtualClass(classType: TClass): TClass;
    procedure Proxify(const instance: TObject);
    procedure Unproxify(const instance: TObject);
  end;

function CreateVirtualClass(classType: TClass): TClass;
procedure DestroyVirtualClass(classType: TClass);

function GetClassData(classType: TClass): PClassData; inline;

function GetVirtualMethodAddress(classType: TClass; virtualIndex: Integer): Pointer;
function GetVirtualMethodCount(classType: TClass): Integer;
function GetVirtualMethodIndex(classType: TClass; method: Pointer): Integer;
function IsVirtualMethodOverride(baseClass, classType: TClass; method: Pointer): Boolean;

implementation

uses
  Spring;


{$REGION 'Routines'}

function GetClassData(classType: TClass): PClassData;
begin
  Result := Pointer(NativeInt(classType) + vmtSelfPtr);
end;

function CreateVirtualClass(classType: TClass): TClass;
var
  virtualMethodCount: Integer;
  size: Integer;
  classData: PClassData;
begin
  virtualMethodCount := GetVirtualMethodCount(classType);
  size := virtualMethodCount * SizeOf(Pointer) - vmtSelfPtr;
  classData := AllocMem(size);
  Result := TClass(PByte(classData) - vmtSelfPtr);
  Move((PByte(classType) + vmtSelfPtr)^, classData^, size);
  classData.SelfPtr := Result;
  Pointer(classData.Parent) := GetClassData(classType);
end;

procedure DestroyVirtualClass(classType: TClass);
begin
  FreeMem(PByte(classType) + vmtSelfPtr);
end;

function GetVirtualMethodIndex(classType: TClass; method: Pointer): Integer;
var
  classData: PClassData;
  i: Integer;
begin
  while classType <> nil do
  begin
    classData := GetClassData(classType);
    for i := MinVirtualIndex to classData.VirtualMethodCount - 1 do
      if classData.VirtualMethods[i] = method then
        Exit(i);
    classType := classType.ClassParent;
  end;
  Result := Low(Integer);
end;

function GetVirtualMethodCount(classType: TClass): Integer;
begin
  Result := GetClassData(classType).VirtualMethodCount;
end;

function GetVirtualMethodAddress(classType: TClass; virtualIndex: Integer): Pointer;
var
  classData: PClassData;
begin
  if virtualIndex < MinVirtualIndex then
    Exit(nil);
  classData := GetClassData(classType);
  if virtualIndex < classData.VirtualMethodCount then
    Result := classData.VirtualMethods[virtualIndex]
  else
    Result := nil;
end;

function IsVirtualMethodOverride(baseClass, classType: TClass; method: Pointer): Boolean;
var
  virtualMethodIndex: Integer;
begin
  virtualMethodIndex := GetVirtualMethodIndex(baseClass, method);
  if virtualMethodIndex >= MinVirtualIndex then
    Result := method <> GetVirtualMethodAddress(classType, virtualMethodIndex)
  else
    Result := False;
end;

{$ENDREGION}


{$REGION 'TClassData'}

function TClassData.GetVirtualMethodCount: Integer;
begin
  if Assigned(Parent) and (TypeInfo = PPointer(@PByte(Parent^)[vmtTypeInfo])^) then
    Exit(GetClassData(Parent^).VirtualMethodCount);

  if InitTable <> nil then
    Result := Integer((PByte(InitTable) - PByte(@VirtualMethods[0])) div SizeOf(Pointer))
  else if FieldTable <> nil then
    Result := Integer((PByte(FieldTable) - PByte(@VirtualMethods[0])) div SizeOf(Pointer))
  else if MethodTable <> nil then
    Result := Integer((PByte(MethodTable) - PByte(@VirtualMethods[0])) div SizeOf(Pointer))
  else
    Result := 0;
end;

{$ENDREGION}


{$REGION 'TVirtualClass'}

constructor TVirtualClass.Create(classType: TClass);
begin
  fProxyClass := CreateVirtualClass(classType);
end;

destructor TVirtualClass.Destroy; //FI:W504
begin
  DestroyVirtualClass(fProxyClass);
end;

function TVirtualClass.GetClassProxyData: PClassData;
begin
  Result := GetClassData(fProxyClass);
end;

{$ENDREGION}


{$REGION 'TVirtualClasses'}

constructor TVirtualClasses.Create;
begin
  fClasses := TList.Create;
  fLock := TCriticalSection.Create;
end;

destructor TVirtualClasses.Destroy; //FI:W504
var
  classType: Pointer;
begin
  for classType in fClasses do
    
	// when this code runs after finalization of this unit
	// there might still be object instances being proxified
	// with these classes - any deallocation here would cause
	// issues such as access violations and alike during the 
	// finalization/destruction of these objects
	// to avoid such issues the deallocation is left to the
	// operation system when the process ends and they are
	// exluced from memory leak reporting
    RegisterExpectedMemoryLeak(PByte(classType) + vmtSelfPtr);

  fLock.Free;
  fClasses.Free;
end;

class constructor TVirtualClasses.Create;
begin
  fDefault := TVirtualClasses.Create;
end;

class destructor TVirtualClasses.Destroy;
begin
  fDefault.Free;
end;

function TVirtualClasses.GetVirtualClass(classType: TClass): TClass;
var
  i: Integer;
begin
  fLock.Enter;
  try
    if fClasses.IndexOf(classType) > -1 then
      Exit(classType);
    for i := 0 to fClasses.Count - 1 do
      if TClass(fClasses[i]).ClassParent = classType then
        Exit(fClasses[i]);
    Result := CreateVirtualClass(classType);
    fClasses.Add(Result);
  finally
    fLock.Release;
  end;
end;

procedure TVirtualClasses.Proxify(const instance: TObject);
begin
  PPointer(instance)^ := GetVirtualClass(PPointer(instance)^);
end;

procedure TVirtualClasses.Unproxify(const instance: TObject);
var
  classType: TClass;
begin
  classType := PPointer(instance)^;
  if fClasses.IndexOf(classType) > -1 then
    PPointer(instance)^ := classType.ClassParent;
end;

{$ENDREGION}


end.
