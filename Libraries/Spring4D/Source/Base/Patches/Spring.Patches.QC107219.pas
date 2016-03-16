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

unit Spring.Patches.QC107219;

interface

implementation

{$IFDEF DELPHIXE}
uses
  Rtti,
  SysUtils,
  TypInfo,
  Windows;

function IsManaged(TypeInfo: PTypeInfo): Boolean;
var
  elTypePtr: PPTypeInfo;
begin
  if TypeInfo = nil then
    Exit(False);
  case TypeInfo^.Kind of
    tkDynArray, tkUString, tkWString, tkLString, tkInterface, tkVariant, tkMethod:
      Result := True;
    tkRecord:
      Result := GetTypeData(TypeInfo)^.ManagedFldCount > 0;
    tkArray:
    begin
      elTypePtr := GetTypeData(TypeInfo)^.ArrayData.ElType;
      Result := (elTypePtr <> nil) and IsManaged(elTypePtr^);
    end;
  else
    Result := False;
  end;
end;

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

procedure RedirectFunction(OrgProc, NewProc: Pointer);
type
  TJmpBuffer = packed record
    Jmp: Byte;
    Offset: Integer;
  end;
var
  n: UINT_PTR;
  JmpBuffer: TJmpBuffer;
begin
  JmpBuffer.Jmp := $E9;
  JmpBuffer.Offset := PByte(NewProc) - (PByte(OrgProc) + 5);
  if not WriteProcessMemory(GetCurrentProcess, OrgProc, @JmpBuffer, SizeOf(JmpBuffer), n) then
    RaiseLastOSError;
end;

procedure ApplyPatch;
begin
  RedirectFunction(GetActualAddr(@Rtti.IsManaged), @IsManaged);
end;

initialization
  ApplyPatch;
{$ENDIF}

end.
