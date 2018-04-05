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

unit Spring.Patches.QC93646;

interface

implementation

{$IFDEF DELPHIXE}
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
  IIntfMethodHelper = interface
    procedure IntfMethod;
  end;

  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished])}
  TInstanceMethodHelper = class(TObject)
  public
    procedure InstanceMethod;
  end;
  {$M-}

  TRttiMethodFix = class(TRttiMethod)
  public
    function IntfDispatchInvoke(Instance: TValue; const Args: array of TValue): TValue;
    function InstanceDispatchInvoke(Instance: TValue; const Args: array of TValue): TValue;
  end;

procedure TInstanceMethodHelper.InstanceMethod;
begin
end;

type
  PPVtable = ^PVtable;
  PVtable = ^TVtable;
  TVtable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

procedure PushSelfFirst(CC: TCallConv; var argList: TArray<TValue>;
  var Index: Integer; const Value: TValue); inline;
begin
  if CC <> TCallConv.ccPascal then
  begin
    argList[Index] := Value;
    Inc(Index);
  end;
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

procedure PassArg(Par: TRttiParameter; const ArgSrc: TValue; var ArgDest: TValue; CC: TCallConv);
begin
  if Par.ParamType = nil then
    ArgDest := TValue.From<Pointer>(ArgSrc.GetReferenceToRawData)
  else if Par.Flags * [TParamFlag.pfVar, TParamFlag.pfOut] <> [] then
  begin
    if Par.ParamType.Handle <> ArgSrc.TypeInfo then
      raise EInvalidCast.CreateRes(@SByRefArgMismatch);
    ArgDest := TValue.From<Pointer>(ArgSrc.GetReferenceToRawData);
  end
  else if (TParamFlag.pfConst in Par.Flags) and
    PassByRef(Par.ParamType.Handle, CC, True) then
  begin
    if Par.ParamType.Handle <> ArgSrc.TypeInfo then
      raise EInvalidCast.CreateRes(@SByRefArgMismatch);
    ArgDest := TValue.From(ArgSrc.GetReferenceToRawData);
  end
  else
    ArgDest := ArgSrc.Cast(Par.ParamType.Handle);
end;

procedure CheckCodeAddress(code: Pointer);
begin
  if (code = nil) or (PPointer(code)^ = nil) then
    raise EInsufficientRtti.CreateRes(@SInsufficientRtti);
end;

procedure PushSelfLast(CC: TCallConv; var argList: TArray<TValue>;
  var Index: Integer; const Value: TValue); inline;
begin
  if CC = TCallConv.ccPascal then
    argList[Index] := Value;
end;

function TRttiMethodFix.IntfDispatchInvoke(Instance: TValue; const Args: array of TValue): TValue;
var
  code: Pointer;
  argCount: Integer;
  argList: TArray<TValue>;
  parList: TArray<TRttiParameter>;
  i, currArg: Integer;
  inst: PPVtable;
begin
  parList := GetParameters;
  if Length(Args) <> Length(parList) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);

  argCount := Length(Args);
  SetLength(argList, argCount + 1);

  currArg := 0;
  inst := PPVtable(Instance.AsInterface);
  PushSelfFirst(CallingConvention, argList, currArg, Instance);

  for i := 0 to Length(Args) - 1 do
  begin
    PassArg(parList[i], Args[i], argList[currArg], CallingConvention);
    Inc(currArg);
  end;

  Assert(DispatchKind = dkInterface);
  code := inst^^[VirtualIndex];
  CheckCodeAddress(code);

  PushSelfLast(CallingConvention, argList, currArg, Instance);

  if ReturnType <> nil then
    Result := Rtti.Invoke(code, argList, CallingConvention, ReturnType.Handle)
  else
    Result := Rtti.Invoke(code, argList, CallingConvention, nil);
end;

function TRttiMethodFix.InstanceDispatchInvoke(Instance: TValue; const Args: array of TValue): TValue;
var
  code: Pointer;
  argCount: Integer;
  argList: TArray<TValue>;
  parList: TArray<TRttiParameter>;
  i, CurrArg: Integer;
  cls: TClass;
  obj: TObject;
  alloc: Boolean;
begin
  parList := GetParameters;
  if Length(Args) <> Length(parList) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);

  argCount := Length(Args);
  if IsConstructor or IsDestructor then
    Inc(argCount);
  if not IsStatic then
    Inc(argCount);

  SetLength(argList, argCount);
  CurrArg := 0;
  cls := nil;

  alloc := True;
  obj := nil;

  if not IsStatic then
  begin
    if IsConstructor then
    begin
      alloc := Instance.TryAsType<TClass>(cls);
      if alloc then
        obj := nil
      else
      begin
        obj := Instance.AsObject;
        if obj <> nil then
          cls := obj.ClassType
        else
          cls := nil;
      end;
      if alloc then
        PushSelfFirst(CallingConvention, argList, CurrArg, cls)
      else
        PushSelfFirst(CallingConvention, argList, CurrArg, obj);
      argList[CurrArg] := alloc;
      Inc(CurrArg);
    end
    else if IsDestructor then
    begin
      cls := Instance.AsObject.ClassType;
      PushSelfFirst(CallingConvention, argList, CurrArg, Instance);
      argList[CurrArg] := True;
      Inc(CurrArg);
    end
    else if IsClassMethod then
    begin
      cls := Instance.AsClass;
      PushSelfFirst(CallingConvention, argList, CurrArg, Instance);
    end
    else
    begin
      cls := Instance.AsObject.ClassType;
      PushSelfFirst(CallingConvention, argList, CurrArg, Instance);
    end;

    if (cls <> nil) and not cls.InheritsFrom(TRttiInstanceType(Parent).MetaclassType) then
      raise EInvalidCast.CreateRes(@SInvalidCast);
  end;

  for i := 0 to Length(Args) - 1 do
  begin
    PassArg(parList[i], Args[i], argList[CurrArg], CallingConvention);
    Inc(CurrArg);
  end;

  if IsStatic then
    code := CodeAddress
  else
    case DispatchKind of
      dkVtable: code := PVtable(cls)^[VirtualIndex];
      dkDynamic: code := GetDynaMethod(cls, VirtualIndex);
    else
      code := CodeAddress;
    end;

  CheckCodeAddress(code);

  if not IsStatic then
  begin
    if IsConstructor then
    begin
      if alloc then
        PushSelfLast(CallingConvention, argList, CurrArg, cls)
      else
        PushSelfLast(CallingConvention, argList, CurrArg, obj);
    end
    else
      PushSelfLast(CallingConvention, argList, CurrArg, Instance);
  end;

  if ReturnType <> nil then
    Result := Rtti.Invoke(code, argList, CallingConvention, ReturnType.Handle)
  else if IsConstructor then
    Result := Rtti.Invoke(code, argList, CallingConvention, cls.ClassInfo)
  else
    Result := Rtti.Invoke(code, argList, CallingConvention, nil);
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
  // Fix TRttiIntfMethod.DispatchInvoke
  method := ctx.GetType(TypeInfo(IIntfMethodHelper)).GetMethod('IntfMethod');
  RedirectFunction(GetVirtualMethod(method.ClassType, 13), @TRttiMethodFix.IntfDispatchInvoke);

  // Fix TRttiInstanceMethodEx.DispatchInvoke
  method := ctx.GetType(TInstanceMethodHelper).GetMethod('InstanceMethod');
  RedirectFunction(GetVirtualMethod(method.ClassType, 13), @TRttiMethodFix.InstanceDispatchInvoke);
end;

initialization
  ApplyPatch;
{$ENDIF}

end.
