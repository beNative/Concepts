{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit DDuce.Reflect;

{$I DDuce.inc}

{
  Reflect provides some static routines to expose the contents of a type
  using the RTTI.
}

interface

uses
  System.Rtti, System.TypInfo,

  DDuce.DynamicRecord;

type
  Reflect = record
  private
    class function GetOrdValue(
      AInfo           : PTypeInfo;
      const ASetParam
    ): Integer; static;

  public
    class function EnumName<T>(const AArg: T): string; static;
    class function OrdValue<T>(const AArg: T): Integer; static;
    class function EnumNamesFromSet<T>(const AArg: T): string; static;

    class function Fields<T>(const AArg: T): IDynamicRecord; static;
    class function Properties<T: class, constructor>(
      const AArg: T
    ): IDynamicRecord; overload; static;
  end;

implementation




  //SizeOf(Integer) * 8 - 1;

{
function SetToString(ATypeInfo: PTypeInfo; const AValue;
  AQuoteValues: Boolean = True; ABrackets: Boolean = True;
  ATrimChars: Integer = -1): string;
var
  S    : TIntegerSet;
  I    : Integer;
  N    : Integer;
  Name : string;

  function GetOrdValue(Info: PTypeInfo; const SetParam): Integer;
  begin
    Result := 0;

    case GetTypeData(Info)^.OrdType of
      otSByte, otUByte:
        Result := Byte(SetParam);
      otSWord, otUWord:
        Result := Word(SetParam);
      otSLong, otULong:
        Result := Integer(SetParam);
    end;
  end;

  function GetPrefixLength(const AString: string): Integer;
  var
    C: Char;
    N: Integer;
  begin
    N := 0;
    if Length(AString) > 0 then
    begin
      C := AString[1];
      while (N < Length(AString)) and C.IsLower do
      begin
        Inc(N);
        C := AString[N + 1];
      end;
    end;
    Result := N;
  end;

begin
  Result := '';
  Integer(S) := GetOrdValue(ATypeInfo, AValue);
  ATypeInfo := GetTypeData(ATypeInfo)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
  begin
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Name := GetEnumName(ATypeInfo, I);

      if ATrimChars >= 0 then
        N := ATrimChars
      else
        N := GetPrefixLength(Name);

      if N > 0 then
        Name := Copy(Name, N + 1, Length(Name) - N + 1);

      if AQuoteValues then
        Name := QuotedStr(Name);

      Result := Result + Name;
    end;
  end;
  if ABrackets and (Result <> '') then
    Result := '(' + Result + ')';
end;
}


{$REGION 'private methods'}
class function Reflect.GetOrdValue(AInfo: PTypeInfo; const ASetParam): Integer;
begin
  Result := 0;

  case GetTypeData(AInfo)^.OrdType of
    otSByte, otUByte:
      Result := Byte(ASetParam);
    otSWord, otUWord:
      Result := Word(ASetParam);
    otSLong, otULong:
      Result := Integer(ASetParam);
  end;
end;
{$ENDREGION}

{ Returns the type name of a given enumerated type instance. }

class function Reflect.EnumName<T>(const AArg: T): string;
begin
  Result := GetEnumName(TypeInfo(T), OrdValue(AArg));
end;

// fields are read-only for the moment.

{ Returns the fields of the given instance (record or object). }

class function Reflect.Fields<T>(const AArg: T): IDynamicRecord;
begin
  Result := TRecord.CreateDynamicRecord;
  Result.From(TValue.From(AArg), False, True, True, []);
end;

class function Reflect.OrdValue<T>(const AArg: T): Integer;
var
  V: TValue;
begin
  Result := 0;
  V := TValue.From<T>(AArg);
  Result := V.AsOrdinal;
end;

{ Returns the properties of the given instance (record or object). }

class function Reflect.Properties<T>(const AArg: T): IDynamicRecord;
begin
  Result := TRecord.CreateDynamicRecord;
  Result.From(TValue.From(AArg), True, False, True, []);
end;

class function Reflect.EnumNamesFromSet<T>(const AArg: T): string;
type
  TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1;
var
  S    : TIntegerSet;
  I    : Integer;
  N    : Integer;
  Name : string;
begin
  Result := '';
  Integer(S) := GetOrdValue(TypeInfo(T), AArg);
  //ATypeInfo := GetTypeData(ATypeInfo)^.CompType^;
  //ATypeInfo :=
  for I := 0 to SizeOf(Integer) * 8 - 1 do
  begin
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Name := EnumName(I);// GetEnumName(ATypeInfo, I);
      Result := Result + Name;
    end;
  end;

end;


end.
