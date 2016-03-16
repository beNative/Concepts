{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2016 Spring4D Team                           }
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

type
  TVirtualClasses = class
  private
    fClasses: TList;
    fLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    function GetVirtualClass(classType: TClass): TClass;
    procedure Proxify(const instance: TObject);
    procedure Unproxify(const instance: TObject);
  end;

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
    SelfPtr: TClass;
    IntfTable: Pointer;
    AutoTable: Pointer;
    InitTable: Pointer;
    TypeInfo: PTypeInfo;
    FieldTable: Pointer;
    MethodTable: Pointer;
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

    VirtualMethods: PVirtualMethodTable;
  end;

  PVmtMethodExTable = ^TVmtMethodExTable;
  TVmtMethodExTable = packed record
   {Count: Word;}
   {Entry: array[1..Count] of TVmtMethodEntry;}
    ExCount: Word;
    ExEntry: array[0..0] of TVmtMethodExEntry;
  end;

  TVirtualClass = class
  private
    fClassProxy: TClass;
    function GetClassProxyData: PClassData; inline;
  public
    constructor Create(classType: TClass);
    destructor Destroy; override;

    property ClassProxy: TClass read fClassProxy;
    property ClassProxyData: PClassData read GetClassProxyData;
  end;

function CreateVirtualClass(classType: TClass): TClass;
procedure DestroyVirtualClass(classType: TClass);

function GetClassData(classType: TClass): PClassData; inline;

function GetVirtualMethodExTable(classType: TClass): PVmtMethodExTable;
function GetVirtualMethodAddress(classType: TClass; virtualIndex: SmallInt): Pointer;
function GetVirtualMethodCount(classType: TClass): Integer;
function GetVirtualMethodIndex(classType: TClass; method: Pointer): SmallInt;
function IsVirtualMethodOverride(baseClass, classType: TClass; method: Pointer): Boolean;

implementation

uses
  Rtti;


{$REGION 'Routines'}

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

function GetClassData(classType: TClass): PClassData;
begin
  Result := Pointer(NativeInt(classType) + vmtSelfPtr);
end;

function GetVirtualMethodExTable(classType: TClass): PVmtMethodExTable;
var
  p: PByte;
  count, paramCount: Word;
  methEnd: PByte;
begin
  p := PPointer(PByte(classType) + vmtMethodTable)^;
  if p <> nil then
  begin
    // skip the first entries
    count := PVmtMethodTable(p).Count;
    Inc(p, SizeOf(Word)); // Count
    while count > 0 do
    begin
      methEnd := p + PVmtMethodEntry(p).Len;
      Inc(p, SizeOf(Word));     // Len
      Inc(p, SizeOf(Pointer));  // CodeAddress
      Inc(p, p^ + 1);           // Name
      if methEnd > p then
      begin
        paramCount := PVmtMethodEntryTail(p).ParamCount;
        Inc(p, SizeOf(TVmtMethodEntryTail));
        while paramCount > 0 do
        begin
          // skip params
          Inc(p, SizeOf(Byte));       // Flags
          Inc(p, SizeOf(PPTypeInfo)); // ParamType
          Inc(p, SizeOf(Byte));       // ParOff
          Inc(p, p^ + 1);             // Name
          Inc(p, PWord(p)^);          // AttrData
          Dec(paramCount);
        end;
      end;
      Dec(count);
    end;
  end;
  Result := PVmtMethodExTable(p);
end;

function GetVirtualMethodIndex(classType: TClass; method: Pointer): SmallInt;
var
  table: PVmtMethodExTable;
  i: Word;
begin
  while classType <> nil do
  begin
    table := GetVirtualMethodExTable(classType);
    if table <> nil then
      for i := 0 to table.ExCount - 1 do
        if table.ExEntry[i].Entry.CodeAddress = method then
          Exit(table.ExEntry[i].VirtualIndex);
    classType := classType.ClassParent;
  end;
  Result := -1;
end;

function GetVirtualMethodCount(classType: TClass): Integer;
var
  context: TRttiContext;
  rttiType: TRttiType;
  method: TRttiMethod;
begin
  rttiType := context.GetType(classType);
  Result := 0;
  for method in rttiType.GetMethods do
  begin
    if method.DispatchKind <> dkVtable then
      Continue;
    if method.VirtualIndex >= Result then
      Result := method.VirtualIndex + 1;
  end;
end;

function GetVirtualMethodAddress(classType: TClass; virtualIndex: SmallInt): Pointer;
var
  table: PVmtMethodExTable;
  i: Word;
begin
  while classType <> nil do
  begin
    table := GetVirtualMethodExTable(classType);
    if table <> nil then
      for i := 0 to table.ExCount - 1 do
        if table.ExEntry[i].VirtualIndex = virtualIndex then
          Exit(table.ExEntry[i].Entry.CodeAddress);
    classType := classType.ClassParent;
  end;
  Result := nil;
end;

function IsVirtualMethodOverride(baseClass, classType: TClass; method: Pointer): Boolean;
var
  virtualMethodIndex: SmallInt;
begin
  virtualMethodIndex := GetVirtualMethodIndex(baseClass, method);
  Result := method <> GetVirtualMethodAddress(classType, virtualMethodIndex);
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
  inherited;
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


{$REGION 'TVirtualClass'}

constructor TVirtualClass.Create(classType: TClass);
begin
  inherited Create;
  fClassProxy := CreateVirtualClass(classType);
end;

destructor TVirtualClass.Destroy;
begin
  DestroyVirtualClass(fClassProxy);
  inherited;
end;

function TVirtualClass.GetClassProxyData: PClassData;
begin
  Result := GetClassData(fClassProxy);
end;

{$ENDREGION}


end.
