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

unit Spring.Mocking.Matching;

{$I Spring.inc}

interface

uses
  Rtti,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.DesignPatterns;

type
  TArgMatch = TPredicate<TArray<TValue>>;

  TMatcherFactory = record
  private
    class threadvar conditionStack: TArray<TPredicate<TValue>>;

    class function AddMatcher(const condition: TPredicate<TValue>): Integer; static;

    class function GetIndex(const v: TValue): Integer; static;
    class procedure SetIndex(typeInfo: PTypeInfo; index: Integer; var Result); static;

    /// <summary>
    ///   Wraps the index of the parameter matcher in a value of type T.
    /// </summary>
    class function WrapIndex<T>(index: Integer): T; static;
  public
    /// <summary>
    ///   Creates a new matcher and returns its index wrapped into a value of
    ///   type T.
    /// </summary>
    class function CreateMatcher<T>(const condition: TPredicate<TValue>): T; static;

    /// <summary>
    ///   Creates an array of match predicates based on the passed arguments
    ///   that contain the index of the according match predicate on the thread
    ///   local <c>conditionsStack</c> variable that was filled by <c>
    ///   Spring.Mocking.Arg</c> .
    /// </summary>
    class function CreateMatchers(const indizes: TArray<TValue>;
      const parameters: TArray<TRttiParameter>): TPredicate<TArray<TValue>>; static;
  end;

  TRangeKind = (Inclusive, Exclusive);

  TArg = record
  private
    fIndex: Integer;
  public
    class function IsAny<T>: T; overload; static;
    class function IsAny<T>(const condition: TPredicate<T>): T; overload; static;
    class function IsEqual<T>(const value: T): T; static;
    class function IsIn(const values: TByteSet): Byte; overload; static;
    class function IsIn<T>(const values: array of T): T; overload; static;
    class function IsIn<T>(const values: IEnumerable<T>): T; overload; static;
    class function IsInRange<T>(const lowValue, highValue: T;
      rangeKind: TRangeKind = TRangeKind.Inclusive): T; static;
    class function IsNil<T>: T; static;
    class function IsNotIn(const values: TByteSet): Byte; overload; static;
    class function IsNotIn<T>(const values: array of T): T; overload; static;
    class function IsNotIn<T>(const values: IEnumerable<T>): T; overload; static;
    class function IsNotNil<T>: T; static;
    class function IsRegex(const pattern: string): string; static;
  end;

  TArgs = record
  strict private
    class function GetAny: TPredicate<TArray<TValue>>; static;
    class function GetItems(index: Integer): TArg; static;
  public
    class property Any: TPredicate<TArray<TValue>> read GetAny;
    class property Items[index: Integer]: TArg read GetItems; default;
  end;

implementation

uses
  Generics.Defaults,
  RegularExpressions,
  TypInfo,
  Spring.ResourceStrings;

type
  TIndexWrapper = class(TInterfacedObject)
  private
    fIndex: Integer;
    constructor Create(index: Integer);
  end;

function GetIndexFail(const v: TValue): Integer;
begin
  raise ENotSupportedException.CreateResFmt(@STypeNotSupported, [v.TypeInfo.TypeName]);
end;

function GetIndexOrdinal(const v: TValue): Integer;
begin
  Result := v.AsOrdinal;
end;

function GetIndexFloat(const v: TValue): Integer;
begin
  Result := Trunc(v.AsExtended);
end;

function GetIndexString(const v: TValue): Integer;
begin
  Result := StrToInt(v.AsString);
end;

function GetIndexObject(const v: TValue): Integer;
begin
  Result := v.AsType<TIndexWrapper>.fIndex;
{$IFNDEF AUTOREFCOUNT}
  v.AsType<TIndexWrapper>.Free;
{$ELSE}
  v.AsType<TIndexWrapper>.DisposeOf;
{$ENDIF}
end;

function GetIndexInterface(const v: TValue): Integer;
begin
  Result := (v.AsType<IInterface> as TIndexWrapper).fIndex;
  PValue(@v)^ := TValue.Empty;
end;

function GetIndexRecord(const v: TValue): Integer;
var
  fields: TArray<TRttiField>;
begin
  fields := TType.GetType(v.TypeInfo).GetFields;
  if Length(fields) = 0 then
    raise ENotSupportedException.CreateResFmt(@STypeNotSupported, [v.TypeInfo.TypeName]);
  Result := TMatcherFactory.GetIndex(fields[0].GetValue(v.GetReferenceToRawData));
end;

function GetIndexArray(const v: TValue): Integer;
begin
  Result := TMatcherFactory.GetIndex(v.GetArrayElement(0));
end;

function GetIndexVariant(const v: TValue): Integer;
begin
  Result := v.AsVariant;
end;

procedure SetIndexFail(typeInfo: PTypeInfo; index: Integer; var Result); //FI:O804
begin
  raise ENotSupportedException.CreateResFmt(@STypeNotSupported, [typeInfo.TypeName]);
end;

procedure SetIndexOrdinal(typeInfo: PTypeInfo; index: Integer; var Result); //FI:O804
begin
  PByte(@Result)^ := index;
end;

procedure SetIndexFloat(typeInfo: PTypeInfo; index: Integer; var Result);
begin
  case typeInfo.TypeData.FloatType of
    ftSingle:
      PSingle(@Result)^ := index;
    ftDouble:
      PDouble(@Result)^ := index;
    ftExtended:
      PExtended(@Result)^ := index;
    ftComp:
      PComp(@Result)^ := index;
    ftCurr:
      PCurrency(@Result)^ := index;
  end;
end;

procedure SetIndexString(typeInfo: PTypeInfo; index: Integer; var Result);
var
  s: string;
begin
  s := IntToStr(index);
  case typeInfo.Kind of
    tkWChar:
    begin
      Assert(index <= 9); // only support up to index 9 for a Char
      PWideChar(@Result)^ := s[1];
    end;
{$IFNDEF NEXTGEN}
    tkLString:
      PAnsiString(@Result)^ := AnsiString(s);
    tkWString:
      PWideString(@Result)^ := s;
{$ENDIF}
    tkUString:
      PUnicodeString(@Result)^ := s;
  end;
end;

procedure SetIndexObject(typeInfo: PTypeInfo; index: Integer; var Result); //FI:O804
begin
  TObject(PPointer(@Result)^) := TIndexWrapper.Create(index);
end;

procedure SetIndexInterface(typeInfo: PTypeInfo; index: Integer; var Result); //FI:O804
begin
  IInterface(PPointer(@Result)^) := TIndexWrapper.Create(index);
end;

procedure SetIndexRecord(typeInfo: PTypeInfo; index: Integer; var Result);
var
  fields: TArray<TRttiField>;
begin
  fields := typeInfo.RttiType.GetFields;
  if Length(fields) = 0 then
    raise ENotSupportedException.CreateResFmt(@STypeNotSupported, [typeInfo.TypeName]);
  TMatcherFactory.SetIndex(fields[0].FieldType.Handle, index, Result);
end;

procedure SetIndexArray(typeInfo: PTypeInfo; index: Integer; var Result);
const
  len: NativeInt = 1;
begin
  DynArraySetLength(PPointer(@Result)^, typeInfo, 1, @len);
  TMatcherFactory.SetIndex(typeInfo.TypeData.DynArrElType^, index, PPointer(@Result)^^);
end;

procedure SetIndexVariant(typeInfo: PTypeInfo; index: Integer; var Result); //FI:O804
begin
  PVariant(@Result)^ := index;
end;


{$REGION 'TMatcherFactory'}

class function TMatcherFactory.CreateMatchers(const indizes: TArray<TValue>;
  const parameters: TArray<TRttiParameter>): TPredicate<TArray<TValue>>;
var
  refParamCount, emptyParamCount, i: Integer;
  conditions: TArray<TPredicate<TValue>>;
begin
  if Assigned(conditionStack) then
  begin
    try
      refParamCount := 0;
      for i := 0 to High(parameters) do
        if parameters[i].Flags * [pfVar, pfOut] <> [] then
          Inc(refParamCount);
      emptyParamCount := 0;
      for i := 0 to High(indizes) do
        if indizes[i].IsEmpty then // nil can be passed without Arg, we handle it
          Inc(emptyParamCount);

      if Length(conditionStack) + refParamCount + emptyParamCount <> Length(indizes) then
        raise ENotSupportedException.Create('when using Arg all arguments must be passed using this way');

      SetLength(conditions, Length(indizes));
      for i := Low(indizes) to High(indizes) do
        if (parameters[i].Flags * [pfVar, pfOut] = []) and not indizes[i].IsEmpty then
          conditions[i] := conditionStack[GetIndex(indizes[i])];
    finally
      conditionStack := nil;
    end;

    Result :=
      function(const args: TArray<TValue>): Boolean
      var
        i: Integer;
      begin
        for i := Low(conditions) to High(conditions) do
          if Assigned(conditions[i]) and not conditions[i](args[i]) then
            Exit(False);
        Result := True;
      end;
  end
  else
    Result := nil;
end;

class function TMatcherFactory.CreateMatcher<T>(
  const condition: TPredicate<TValue>): T;
var
  index: Integer;
begin
  index := AddMatcher(condition);
  Result := WrapIndex<T>(index);
end;

class function TMatcherFactory.AddMatcher(
  const condition: TPredicate<TValue>): Integer;
begin
  Result := Length(conditionStack);
  SetLength(conditionStack, Result + 1);
  conditionStack[Result] := condition;
end;

class function TMatcherFactory.GetIndex(const v: TValue): Integer;
const
  Handlers: array[TTypeKind] of function(const v: TValue): Integer = (
    GetIndexFail, GetIndexOrdinal, GetIndexOrdinal, GetIndexOrdinal, GetIndexFloat,
    GetIndexFail, GetIndexOrdinal, GetIndexObject, GetIndexFail, GetIndexString,
    GetIndexString, GetIndexString, GetIndexVariant, GetIndexFail, GetIndexRecord,
    GetIndexInterface, GetIndexOrdinal, GetIndexArray, GetIndexString, GetIndexFail,
    GetIndexFail, GetIndexFail);
begin
  Result := Handlers[TValueData(v).FTypeInfo.Kind](v);
end;

class procedure TMatcherFactory.SetIndex(typeInfo: PTypeInfo; index: Integer; var Result);
const
  Handlers: array[TTypeKind] of procedure (typeInfo: PTypeInfo; index: Integer; var Result) = (
    SetIndexFail, SetIndexOrdinal, SetIndexOrdinal, SetIndexOrdinal, SetIndexFloat,
    SetIndexFail, SetIndexOrdinal, SetIndexObject, SetIndexFail, SetIndexString,
    SetIndexString, SetIndexString, SetIndexVariant, SetIndexFail, SetIndexRecord,
    SetIndexInterface, SetIndexOrdinal, SetIndexArray, SetIndexString, SetIndexFail,
    SetIndexFail, SetIndexFail);
begin
  Handlers[typeInfo.Kind](typeInfo, index, Result);
end;

class function TMatcherFactory.WrapIndex<T>(index: Integer): T;
var
  f: TRttiField;
  v: TValue;
begin
  Result := Default(T);
  SetIndex(TypeInfo(T), index, Result);
end;

{$ENDREGION}


{$REGION 'TIndexWrapper'}

constructor TIndexWrapper.Create(index: Integer);
begin
  fIndex := index;
end;

{$ENDREGION}


{$REGION 'TArg'}

class function TArg.IsAny<T>: T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := True;
    end);
end;

class function TArg.IsAny<T>(const condition: TPredicate<T>): T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := condition(arg.AsType<T>);
    end);
end;

class function TArg.IsEqual<T>(const value: T): T;
var
  comparer: IEqualityComparer<T>;
begin
  comparer := TEqualityComparer<T>.Default;
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := comparer.Equals(arg.AsType<T>, value);
    end);
end;

class function TArg.IsIn(const values: TByteSet): Byte;
var
  capturedValues: TByteSet;
begin
  capturedValues := values;
  Result := TMatcherFactory.CreateMatcher<Byte>(
    function(const arg: TValue): Boolean
    begin
      Result := arg.AsType<Byte> in capturedValues;
    end);
end;

class function TArg.IsIn<T>(const values: array of T): T;
var
  capturedValues: TArray<T>;
begin
  capturedValues := TArray.Copy<T>(values);
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := TArray.Contains<T>(capturedValues, arg.AsType<T>);
    end);
end;

class function TArg.IsIn<T>(const values: IEnumerable<T>): T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := values.Contains(arg.AsType<T>);
    end);
end;

class function TArg.IsInRange<T>(const lowValue, highValue: T;
  rangeKind: TRangeKind): T;
var
  comparer: IComparer<T>;
begin
  comparer := TComparer<T>.Default;
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    var
      value: T;
    begin
      value := arg.AsType<T>;
      if rangeKind = TRangeKind.Exclusive then
        Result := (comparer.Compare(value, lowValue) > 0)
          and (comparer.Compare(value, highValue) < 0)
      else
        Result := (comparer.Compare(value, lowValue) >= 0)
          and (comparer.Compare(value, highValue) <= 0);
    end);
end;

class function TArg.IsNil<T>: T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := arg.IsEmpty;
    end);
end;

class function TArg.IsNotIn<T>(const values: array of T): T;
var
  capturedValues: TArray<T>;
begin
  capturedValues := TArray.Copy<T>(values);
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := not TArray.Contains<T>(capturedValues, arg.AsType<T>);
    end);
end;

class function TArg.IsNotIn(const values: TByteSet): Byte;
var
  capturedValues: TByteSet;
begin
  capturedValues := values;
  Result := TMatcherFactory.CreateMatcher<Byte>(
    function(const arg: TValue): Boolean
    begin
      Result := not (arg.AsType<Byte> in capturedValues);
    end);
end;

class function TArg.IsNotIn<T>(const values: IEnumerable<T>): T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := not values.Contains(arg.AsType<T>);
    end);
end;

class function TArg.IsNotNil<T>: T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := not arg.IsEmpty;
    end);
end;

class function TArg.IsRegex(const pattern: string): string;
var
  regex: TRegEx;
begin
  regex := TRegEx.Create(pattern);
  Result := TMatcherFactory.CreateMatcher<string>(
    function(const arg: TValue): Boolean
    begin
      Result := regex.IsMatch(pattern);
    end);
end;

{$ENDREGION}


{$REGION 'TArgs'}

class function TArgs.GetAny: TPredicate<TArray<TValue>>;
begin
  Result :=
    function(const args: TArray<TValue>): Boolean
    begin
      Result := True;
    end;
end;

class function TArgs.GetItems(index: Integer): TArg;
begin
  Result.fIndex := index;
end;

{$ENDREGION}


end.
