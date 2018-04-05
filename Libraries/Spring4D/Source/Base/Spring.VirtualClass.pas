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

unit Spring.VirtualClass;

interface

{$RANGECHECKS OFF}

uses
  Classes,
  SyncObjs,
  TypInfo;

{$IF not declared(CPP_ABI_ADJUST)}
const
  CPP_ABI_ADJUST = 0;
{$IFEND}

const
  MinVirtualIndex = -11 - (CPP_ABI_ADJUST div SizeOf(Pointer)){$IFDEF AUTOREFCOUNT} - 2{$ENDIF};

type
{$POINTERMATH ON}
  PVirtualMethodTable = ^Pointer;
{$POINTERMATH OFF}

{$IFDEF AUTOREFCOUNT}
  // after looking at several functions in System.pas (like _AfterConstruction,
  // etc.) we realized that Self (or Instance) is passed as const, this has
  // some implication on NextGen regarding reference counting (most visible
  // during FreeInstance)
  TObjAddRef = function (const Self: TObject): Integer;
  TObjRelease = function (const Self: TObject): Integer;
{$ENDIF}
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
{$IFNDEF NEXTGEN}
    ClassName: PShortString;
{$ELSE}
    ClassName: TTypeInfoFieldAccessor;
{$ENDIF}
    InstanceSize: Integer;
    Parent: PClass;

{$IFDEF AUTOREFCOUNT}
    __ObjAddRef: TObjAddRef;
    __ObjRelease: TObjRelease;
{$ENDIF}
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
  if Assigned(Parent) and (TypeInfo = Parent^.ClassInfo) then
    Exit(GetClassData(Parent^).VirtualMethodCount);

  if InitTable <> nil then
    Result := (PByte(InitTable) - PByte(@VirtualMethods[0])) div SizeOf(Pointer)
  else if FieldTable <> nil then
    Result := (PByte(FieldTable) - PByte(@VirtualMethods[0])) div SizeOf(Pointer)
  else if MethodTable <> nil then
    Result := (PByte(MethodTable) - PByte(@VirtualMethods[0])) div SizeOf(Pointer)
  else
    Result := 0;
end;

{$ENDREGION}


{$REGION 'TVirtualClass'}

constructor TVirtualClass.Create(classType: TClass);
begin
  inherited Create;
  fProxyClass := CreateVirtualClass(classType);
end;

destructor TVirtualClass.Destroy;
begin
  DestroyVirtualClass(fProxyClass);
  inherited Destroy;
end;

function TVirtualClass.GetClassProxyData: PClassData;
begin
  Result := GetClassData(fProxyClass);
end;

{$ENDREGION}


{$REGION 'TVirtualClasses'}

constructor TVirtualClasses.Create;
begin
  inherited Create;
  fClasses := TList.Create;
  fLock := TCriticalSection.Create;
end;

destructor TVirtualClasses.Destroy;
var
  classType: Pointer;
begin
  for classType in fClasses do
    DestroyVirtualClass(classType);
  fLock.Free;
  fClasses.Free;
  inherited Destroy;
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
  PPointer(instance)^ := GetVirtualClass(instance.ClassType);
end;

procedure TVirtualClasses.Unproxify(const instance: TObject);
var
  classType: TClass;
begin
  classType := instance.ClassType;
  if fClasses.IndexOf(classType) > -1 then
    PPointer(instance)^ := classType.ClassParent;
end;

{$ENDREGION}


end.
