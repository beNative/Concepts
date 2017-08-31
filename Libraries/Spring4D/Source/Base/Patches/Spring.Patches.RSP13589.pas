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

unit Spring.Patches.RSP13589;

interface

implementation

{$IFNDEF DELPHIX_BERLIN_UP}{$IFDEF MSWINDOWS}
uses
  RTLConsts,
  Rtti,
  SysConst,
  SysUtils,
  TypInfo,
  Windows;

type
  {$M+}
  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished])}
  TRecordMethodHelper = record
    procedure RecordMethod;
  end;
  {$M-}

  TRttiMethodFix = class(TRttiMethod)
  private
    function RecordDispatchInvoke(Instance: TValue;
      const Args: array of TValue): TValue;
  end;

procedure TRecordMethodHelper.RecordMethod;
begin
end;

function PassByRef(TypeInfo: PTypeInfo; CC: TCallConv; IsConst: Boolean = False): Boolean;
{$IFDEF CPUARM64}
var
  ctx: TRTTIContext;
{$ENDIF CPUARM64}
begin
  if TypeInfo = nil then
    Exit(False);
  case TypeInfo^.Kind of
{$IF Defined(CPUX86)}
    tkArray:
      Result := GetTypeData(TypeInfo)^.ArrayData.Size > SizeOf(Pointer);
    tkRecord:
      if (CC in [ccCdecl, ccStdCall, ccSafeCall]) and not IsConst then
        Result := False
      else
        Result := GetTypeData(TypeInfo)^.RecSize > SizeOf(Pointer);
    tkVariant:
      Result := IsConst or not (CC in [ccCdecl, ccStdCall, ccSafeCall]);
{$ELSEIF Defined(CPUX64)}
    tkArray:
      Result := GetTypeData(TypeInfo)^.ArrayData.Size > SizeOf(Pointer);
    tkRecord:
      Result := not (GetTypeData(TypeInfo)^.RecSize in [1,2,4,8]);
    tkMethod,
    tkVariant:
      Result := True;
{$ELSEIF Defined(CPUARM32)}
    tkArray,
    tkRecord:
      Result := CC in [ccReg, ccPascal];
    tkMethod,
    tkVariant:
      Result := True;
{$ELSEIF Defined(CPUARM64)}
    tkArray:
      if ctx.GetType(TypeInfo).IsHFA then
        Result := CC in [ccReg, ccPascal]
      else
        Result := (GetTypeData(TypeInfo)^.ArrayData.Size > 16) or (CC in [ccReg, ccPascal]);
    tkRecord:
      if ctx.GetType(TypeInfo).IsHFA then
        Result := CC in [ccReg, ccPascal]
      else
        Result := (GetTypeData(TypeInfo)^.RecSize > 16) or (CC in [ccReg, ccPascal]);
    tkMethod,
    tkVariant:
      Result := True;
{$IFEND CPUTYPE}
{$IFNDEF NEXTGEN}
    tkString:
      Result := GetTypeData(TypeInfo)^.MaxLength > SizeOf(Pointer);
{$ENDIF !NEXTGEN}
  else
    Result := False;
  end;
end;

procedure PushSelfFirst(CC: TCallConv; var argList: TArray<TValue>;
  var Index: Integer; const Value: TValue); inline;
begin
{$IFDEF CPUX86}
  if CC = ccPascal then Exit;
{$ENDIF CPUX86}
  argList[Index] := Value;
  Inc(Index);
end;

procedure PushSelfLast(CC: TCallConv; var argList: TArray<TValue>;
  var Index: Integer; const Value: TValue); inline;
begin
{$IFDEF CPUX86}
  if CC <> ccPascal then Exit;
  argList[Index] := Value;
{$ELSE !CPUX86}

{$ENDIF CPUX86}
end;

procedure PassArg(Par: TRttiParameter; const ArgSrc: TValue; var ArgDest: TValue; CC: TCallConv);
begin
  if Par.ParamType = nil then
    ArgDest := TValue.From<Pointer>(ArgSrc.GetReferenceToRawData)
  else if Par.Flags * [pfVar, pfOut] <> [] then
  begin
    if Par.ParamType.Handle <> ArgSrc.TypeInfo then
      raise EInvalidCast.CreateRes(@SByRefArgMismatch);
    ArgDest := TValue.From<Pointer>(ArgSrc.GetReferenceToRawData);
  end
  else if (pfConst in Par.Flags) and
    PassByRef(Par.ParamType.Handle, CC, True) then
  begin
    if TypeInfo(TValue) = Par.ParamType.Handle then
      ArgDest := TValue.From(ArgSrc)
    else
    begin
      if Par.ParamType.Handle <> ArgSrc.TypeInfo then
        raise EInvalidCast.CreateRes(@SByRefArgMismatch);
      ArgDest := TValue.From(ArgSrc.GetReferenceToRawData);
    end
  end
  else
    ArgDest := ArgSrc.Cast(Par.ParamType.Handle);
end;

function TRttiMethodFix.RecordDispatchInvoke(Instance: TValue;
  const Args: array of TValue): TValue;
var
  argList: TArray<TValue>;
  parList: TArray<TRttiParameter>;
  i, currArg: Integer;
  inst: TValue;
begin
  if not IsStatic then
    if (Instance.Kind = tkPointer) and
      (
        (
          (Instance.TypeData^.RefType <> nil) and
          (Instance.TypeData^.RefType^ = Parent.Handle)
        )
      or
        (
          (Instance.TypeData^.RefType = nil) or
          (Instance.TypeData^.RefType^ = nil)
        )
      ) then
    begin
      inst := Instance;
    end
    else
    begin
      if Instance.TypeInfo <> Parent.Handle then
        raise EInvalidCast.CreateRes(@SInvalidCast);
      inst := TValue.From(Instance.GetReferenceToRawData);
    end;

  parList := GetParameters;
  if Length(Args) <> Length(parList) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);

  if IsStatic then
    SetLength(argList, Length(Args))
  else
    SetLength(argList, Length(Args) + 1);

  currArg := 0;
  if not IsStatic then
    PushSelfFirst(CallingConvention, argList, currArg, inst);

  for i := 0 to Length(Args) - 1 do
  begin
    PassArg(parList[i], Args[i], argList[currArg], CallingConvention);
    Inc(currArg);
  end;

  if not IsStatic then
    PushSelfLast(CallingConvention, argList, currArg, inst);

  if ReturnType <> nil then
    Result := System.Rtti.Invoke(CodeAddress, argList, CallingConvention, ReturnType.Handle, IsStatic)
  else
    Result := System.Rtti.Invoke(CodeAddress, argList, CallingConvention, nil);
end;

function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
begin
  Result := PPointer(UINT_PTR(AClass) + UINT_PTR(Index * SizeOf(Pointer)))^;
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
var
  ctx: TRttiContext;
  method: TRttiMethod;
begin
  // Fix TRttiRecordMethod.DispatchInvoke
  method := ctx.GetType(TypeInfo(TRecordMethodHelper)).GetMethod('RecordMethod');
  RedirectFunction(GetVirtualMethod(method.ClassType, 13), @TRttiMethodFix.RecordDispatchInvoke);
end;

initialization
  ApplyPatch;
{$ENDIF}{$ENDIF}

end.
