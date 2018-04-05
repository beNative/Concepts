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

unit Spring.Patches.QC98671;

interface

implementation

{$IFDEF DELPHIXE}
uses
  Rtti,
  SysUtils,
  TypInfo,
  Windows;

type
  PInterceptFrame = Pointer;

  PParamLoc = ^TParamLoc;
  TParamLoc = record
    FTypeInfo: PTypeInfo;
    FByRefParam: Boolean;
    FOffset: Integer;
    procedure SetArg(AFrame: PInterceptFrame; const Value: TValue);
  end;

var
  TParamLoc_SetArg: procedure(var Self: TParamLoc; AFrame: PInterceptFrame; const Value: TValue);

procedure TParamLoc.SetArg(AFrame: PInterceptFrame; const Value: TValue);
begin
  // Fix from XE2
  if FByRefParam then
    TParamLoc_SetArg(Self, AFrame, Value);
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

procedure ApplyPatch;
const
  SetArgCallBytes: array[0..18] of SmallInt = (
    $8D, $04, $5B, // lea eax,[ebx+ebx*2]
    $8B, $55, $F0, // mov edx,[ebp-$10]
    $8D, $0C, $C2, // lea ecx,[edx+eax*8]
    $8B, $55, $FC, // mov edx,[ebp-$04]
    $8D, $04, $82, // lea eax,[edx+eax*4]
    $8B, $55, $F4, // mov edx,[ebp-$0c]
    $E8            // call TMethodImplementation.TParamLoc.SetArg
  );
var
  ctx: TRttiContext;
  p: PByte;
  offset: Integer;
  n: UINT_PTR;
begin
  // Get the code pointer of the TMethodImplementation.TInvokeInfo.GetParamLocs method for which
  // extended RTTI is available to find the private types TInvokeInfo private method SaveArguments.
  p := ctx.GetType(TMethodImplementation).GetField('FInvokeInfo').FieldType.GetMethod('GetParamLocs').CodeAddress;

  // Find for the "locs[i].SetArg(AFrame, Args[i]);" call and replace it with a call to our function.
  p := FindMethodBytes(p - 128 - SizeOf(SetArgCallBytes), SetArgCallBytes, 128 - SizeOf(SetArgCallBytes));
  if p <> nil then
  begin
    // Replace the call to SetArg with our method.
    @TParamLoc_SetArg := (p + 19 + 4) + PInteger(@p[19])^;
    offset := PByte(@TParamLoc.SetArg) - (p + 19 + 4);
    if not WriteProcessMemory(GetCurrentProcess, p + 19, @offset, SizeOf(offset), n) then
      RaiseLastOSError;
  end;
end;

initialization
  ApplyPatch;
{$ENDIF}

end.
