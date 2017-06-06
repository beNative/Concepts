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

unit Spring.Cryptography.Utils;

interface

function ByteSwap32(const x: UInt32): UInt32;
function ByteSwap64(const x: UInt64): UInt64;
function RotateLeft32(const x: UInt32; const y: Byte): UInt32;
function RotateRight32(const x: UInt32; const y: Byte): UInt32;
function RotateRight64(const x: Int64; const y: Byte): Int64;

implementation

uses
  SysUtils;

function ByteSwap32(const x: UInt32): UInt32;
{$IFDEF ASSEMBLER}
{$IFDEF CPUX86}
asm
  bswap eax
end;
{$ELSE CPUX64}
asm
  mov eax,ecx
  bswap eax
end;
{$ENDIF}
{$ELSE}
begin
  Result := (x shr 16) + (x shl 16);
  Result := ((Result shl 8) and $FF00FF00) + ((Result shr 8) and $00FF00FF);
end;
{$ENDIF}

function ByteSwap64(const x: UInt64): UInt64;
{$IFDEF ASSEMBLER}
{$IFDEF CPUX86}
asm
  mov edx,x.Int64Rec.Lo
  mov eax,x.Int64Rec.Hi
  bswap edx
  bswap eax
end;
{$ELSE}
asm
  mov rax,rcx
  bswap rax
end;
{$ENDIF}
{$ELSE}
begin
  Int64Rec(Result).Bytes[0] := Int64Rec(x).Bytes[7];
  Int64Rec(Result).Bytes[1] := Int64Rec(x).Bytes[6];
  Int64Rec(Result).Bytes[2] := Int64Rec(x).Bytes[5];
  Int64Rec(Result).Bytes[3] := Int64Rec(x).Bytes[4];
  Int64Rec(Result).Bytes[4] := Int64Rec(x).Bytes[3];
  Int64Rec(Result).Bytes[5] := Int64Rec(x).Bytes[2];
  Int64Rec(Result).Bytes[6] := Int64Rec(x).Bytes[1];
  Int64Rec(Result).Bytes[7] := Int64Rec(x).Bytes[0];
end;
{$ENDIF}

function RotateLeft32(const x: UInt32; const y: Byte): UInt32;
{$IFDEF ASSEMBLER}
{$IFDEF CPUX86}
asm
  mov cl,dl
  rol eax,cl
end;
{$ELSE}
asm
  mov eax,ecx
  mov cl,dl
  rol eax,cl
end;
{$ENDIF}
{$ELSE}
begin
  Result := (x shl y) or (x shr (32 - y));
end;
{$ENDIF}

function RotateRight32(const x: UInt32; const y: Byte): UInt32;
{$IFDEF ASSEMBLER}
{$IFDEF CPUX86}
asm
  mov cl,dl
  ror eax,cl
end;
{$ELSE CPUX64}
asm
  mov eax,ecx
  mov cl,dl
  ror eax,cl
end;
{$ENDIF}
{$ELSE}
begin
  Result := (x shr y) or (x shl (32 - y));
end;
{$ENDIF}

function RotateRight64(const x: Int64; const y: Byte): Int64;
begin
  Result := (x shr y) or (x shl (64 - y));
end;

end.
