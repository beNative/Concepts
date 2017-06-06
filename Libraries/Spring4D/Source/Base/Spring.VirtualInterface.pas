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

unit Spring.VirtualInterface;

interface

uses
  Generics.Collections,
  Rtti,
  TypInfo,
  Spring.MethodIntercept;

type
{$IFDEF DELPHIXE2_UP}
  TVirtualInterfaceInvokeEvent = Rtti.TVirtualInterfaceInvokeEvent;
{$ELSE}
  TVirtualInterfaceInvokeEvent = reference to procedure(Method: TRttiMethod;
    const Args: TArray<TValue>; out Result: TValue);
{$ENDIF}

{$IFDEF DELPHIXE2_UP}
  TVirtualInterface = Rtti.TVirtualInterface;
{$ELSE}
  TVirtualInterface = class(TInterfacedObject, IInterface)
  private
    fMethodTable: Pointer;
    fInterfaceID: TGUID;
    fContext: TRttiContext;
    fIntercepts: TObjectList<TMethodIntercept>;
    fOnInvoke: TVirtualInterfaceInvokeEvent;
    function QueryInterfaceFromIntf(const IID: TGUID; out Obj): HResult;
    function _AddRefFromIntf: Integer;
    function _ReleaseFromIntf: Integer;
  protected
    procedure DoInvoke(UserData: Pointer;
      const Args: TArray<TValue>; out Result: TValue);
    procedure ErrorProc;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
  public
    constructor Create(typeInfo: PTypeInfo); overload;
    constructor Create(typeInfo: PTypeInfo;
      invokeEvent: TVirtualInterfaceInvokeEvent); overload;
    destructor Destroy; override;

    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    property OnInvoke: TVirtualInterfaceInvokeEvent read fOnInvoke write fOnInvoke;
  end;
{$ENDIF}

implementation

uses
{$IFDEF DELPHIXE}
  Spring.Patches.GetInvokeInfo,
  Spring.Patches.QC93646,
  Spring.Patches.QC98671,
  Spring.Patches.QC107219,
{$ENDIF}
  RTLConsts;


{$REGION 'TVirtualInterface'}

{$IFNDEF DELPHIXE2_UP}
constructor TVirtualInterface.Create(typeInfo: PTypeInfo);
type
{$POINTERMATH ON}
  PVTable = ^Pointer;
{$POINTERMATH OFF}
var
  i: Integer;
  maxVirtualIndex: SmallInt;
  methods: TArray<TRttiMethod>;
  method: TRttiMethod;
  rttiType: TRttiType;
begin
  fIntercepts := TObjectList<TMethodIntercept>.Create();
  rttiType := fContext.GetType(typeInfo);
  fInterfaceID := TRttiInterfaceType(rttiType).GUID;

  maxVirtualIndex := 2;
  methods := rttiType.GetMethods;

  for method in methods do
  begin
    if maxVirtualIndex < method.VirtualIndex then
      maxVirtualIndex := method.VirtualIndex;
    fIntercepts.Add(TMethodIntercept.Create(method, DoInvoke));
  end;

  fMethodTable := AllocMem(SizeOf(Pointer) * (maxVirtualIndex + 1));

  PVTable(fMethodTable)[0] := @TVirtualInterface.QueryInterfaceFromIntf;
  PVTable(fMethodTable)[1] := @TVirtualInterface._AddRefFromIntf;
  PVTable(fMethodTable)[2] := @TVirtualInterface._ReleaseFromIntf;

  for i := 0 to fIntercepts.Count - 1 do
    PVTable(fMethodTable)[fIntercepts[i].VirtualIndex] := fIntercepts[i].CodeAddress;

  for i := 3 to maxVirtualIndex do
    if not Assigned(PVTable(fMethodTable)[i]) then
      PVTable(fMethodTable)[i] := @TVirtualInterface.ErrorProc;
end;

constructor TVirtualInterface.Create(TypeInfo: PTypeInfo;
  InvokeEvent: TVirtualInterfaceInvokeEvent);
begin
  Create(TypeInfo);
  fOnInvoke := InvokeEvent;
end;

destructor TVirtualInterface.Destroy;
begin
  if Assigned(fMethodTable) then
    FreeMem(fMethodTable);
  fIntercepts.Free;
  inherited Destroy;
end;

procedure TVirtualInterface.DoInvoke(UserData: Pointer;
  const Args: TArray<TValue>; out Result: TValue);
begin
  if Assigned(fOnInvoke) then
    fOnInvoke(TMethodIntercept(UserData).Method, Args, Result);
end;

procedure TVirtualInterface.ErrorProc;
begin
  raise EInsufficientRtti.CreateRes(@SInsufficientRtti);
end;

function TVirtualInterface.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IID = fInterfaceID then
  begin
    _AddRef();
    Pointer(Obj) := @fMethodTable;
    Result := S_OK;
  end
  else
    Result := inherited QueryInterface(IID, Obj);
end;

function TVirtualInterface.QueryInterfaceFromIntf(
  const IID: TGUID; out Obj): HResult;
asm
  add dword ptr [esp+$04],-$0C
  mov eax,[esp+$04]
  mov eax,[eax]
  jmp dword ptr [eax+$08]
end;

function TVirtualInterface._AddRef: Integer;
begin
  Result := inherited _AddRef;
end;

function TVirtualInterface._AddRefFromIntf: Integer;
asm
  add dword ptr [esp+$04],-$0c
  mov eax,[esp+$04]
  mov eax,[eax]
  jmp dword ptr [eax]
end;

function TVirtualInterface._Release: Integer;
begin
  Result := inherited _Release;
end;

function TVirtualInterface._ReleaseFromIntf: Integer;
asm
  add dword ptr [esp+$04],-$0C
  mov eax,[esp+$04]
  mov eax,[eax]
  jmp dword ptr [eax+$04]
end;
{$ENDIF}

{$ENDREGION}


end.
