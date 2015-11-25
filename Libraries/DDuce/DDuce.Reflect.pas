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
  System.Rtti,

  DDuce.DynamicRecord;

type
  Reflect = record
  public
    class function EnumName<T>(const AArg: T): string; static;
    class function OrdValue<T>(const AArg: T): Integer; static;
    class function SetElementNames<T>(const AArg: T): string; static;

    class function Fields<T>(const AArg: T): IDynamicRecord; static;
    class function Properties<T: class, constructor>(
      const AArg: T
    ): IDynamicRecord; overload; static;
  end;

implementation

uses
  System.TypInfo;

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
  //Result.From(TValue.From(AArg), True, False, True, []);
  Result.From(AArg, True, False, True, []);
end;

// ElementNamesOfSet

class function Reflect.SetElementNames<T>(const AArg: T): string;
begin
  // TODO
end;

end.


