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

unit Spring.Patches.GetInvokeInfo;

interface

implementation

{$IFDEF DELPHIXE}
uses
  Rtti,
  SysUtils,
  TypInfo,
  Windows;

function GetActualAddr(Proc: Pointer): Pointer;
type
  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;
    Addr: PPointer;
  end;
begin
  Result := Proc;
  if (Proc <> nil) and (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then
    Result := PAbsoluteIndirectJmp(Proc).Addr^;
end;

function FindMethodBytes(StartAddress: Pointer; const Bytes: array of SmallInt; MaxCount: Integer): PByte;

  function XCompareMem(P: PByte; C: PSmallint; Len: Integer): Boolean;
  begin
    while (Len > 0) and ((C^ = -1) or (P^ = Byte(C^))) do
    begin
      Dec(Len);
      Inc(P);
      Inc(C);
    end;
    Result := Len = 0;
  end;

var
  FirstByte: Byte;
  EndAddress: PByte;
  Len: Integer;
begin
  FirstByte := Bytes[0];
  Len := Length(Bytes) - 1;
  Result := StartAddress;
  EndAddress := Result + MaxCount;
  while Result < EndAddress do
  begin
    while (Result < EndAddress) and (Result[0] <> FirstByte) do
      Inc(Result);
    if (Result < EndAddress) and XCompareMem(Result + 1, @Bytes[1], Len) then
      Exit;
    Inc(Result);
  end;
  Result := nil;
end;

function PassByRef(TypeInfo: PTypeInfo; CC: TCallConv; IsConst: Boolean = False): Boolean;
begin
  if TypeInfo = nil then
    Exit(False);
  case TypeInfo^.Kind of
    tkString:
      Result := GetTypeData(TypeInfo)^.MaxLength > SizeOf(Pointer);
    tkVariant:
      Result := IsConst or not (CC in [ccCdecl, ccStdCall, ccSafeCall]);
    tkArray:
      Result := GetTypeData(TypeInfo)^.ArrayData.Size > SizeOf(Pointer);
    tkRecord:
      if (CC in [ccCdecl, ccStdCall, ccSafeCall]) and not IsConst then
        Result := False
      else
        Result := GetTypeData(TypeInfo)^.RecSize > SizeOf(Pointer);
  else
    Result := False;
  end;
end;

const
  TRttiMethod_CreateImplementationBytesCmp: array[0..11] of Byte = (
    $53,                // push ebx                        //  0
    $56,                // push esi                        //  1
    $57,                // push edi                        //  2
    $8B, $F9,           // mov edi,ecx                     //  3
    $8B, $F2,           // mov esi,edx                     //  5
    $8B, $D8,           // mov ebx,eax                     //  7
    $8B, $C3,           // mov eax,ebx                     //  9
    $E8                 // call TRttiMethod.GetInvokeInfo  // 11
  );

  TRttiMethod_GetInvokeInfoBytes: array[0..25] of SmallInt = (
    $B0, $01,             // mov al,$01                //  0
    $50,                  // push eax                  //  2
    $8B, $C6,             // mov eax,esi               //  3
    $8B, $10,             // mov edx,[eax]             //  5
    $FF, $52, $10,        // call dword ptr [edx+$10]  //  7
    $E8, -1, -1, -1, -1,  // call Generics + $4C9D7C   // 10
    $8B, $D0,             // mov edx,eax               // 15
    $8B, $45, $F8,        // mov eax,[ebp-$08]         // 17
    $59,                  // pop ecx                   // 20
    $E8, -1, -1, -1, -1   // call TMethodImplementation.TInvokeInfo.AddParameter // 21
  );

var
  OrgInvokeInfoAddParameter: procedure(AInvokeInfo: TObject; AType: PTypeInfo; ByRef: Boolean);

procedure FixInvokeInfoAddParameter(AInvokeInfo: TObject; AType: PTypeInfo; ByRef: Boolean;
  p: TRttiParameter; Method: TRttiMethod);
begin
  OrgInvokeInfoAddParameter(AInvokeInfo, p.ParamType.Handle,
    ([pfVar, pfOut] * p.Flags <> []) or
    PassByRef(p.ParamType.Handle, Method.CallingConvention, pfConst in p.Flags));
end;

procedure FixInvokeInfoAddParameterEnter(AInvokeInfo: TObject; AType: PTypeInfo; ByRef: Boolean);
asm
  push esi // p is kept in ESI
  push edi // TMethodRtti is kept in EDI
  call FixInvokeInfoAddParameter
end;

procedure ApplyPatch;
var
  p: PByte;
  n: UINT_PTR;
  offset: Integer;
begin
  // Fix TRttiMethod.GetInvokeInfo
  // Find the private TRttiMethod.GetInvokeInfo method
  p := GetActualAddr(@TRttiMethod.CreateImplementation);
  if CompareMem(p, @TRttiMethod_CreateImplementationBytesCmp[0], SizeOf(TRttiMethod_CreateImplementationBytesCmp)) then
  begin
    p := PByte(p + 11 + 5) + PInteger(@p[11 + 1])^; // relative call => absolute address
    p := FindMethodBytes(p, TRttiMethod_GetInvokeInfoBytes, 400);
    if p <> nil then
    begin
      @OrgInvokeInfoAddParameter := PByte(p + 21 + 5) + PInteger(@p[21 + 1])^; // relative call => absolute address
      offset := PByte(@FixInvokeInfoAddParameterEnter) - (p + 21 + 5);
      if not WriteProcessMemory(GetCurrentProcess, p + 21 + 1, @offset, SizeOf(offset), n) then
        RaiseLastOSError;
    end;
  end;
end;

initialization
  ApplyPatch;
{$ENDIF}

end.
