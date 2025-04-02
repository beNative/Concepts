{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2024 Spring4D Team                           }
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
  Spring,
  Spring.Collections;

type
  TArgMatch = Predicate<TArray<TValue>>;

  TMatcherFactory = record
  private
    class threadvar conditionStack: TArray<Predicate<TValue>>;

    class function AddMatcher(const condition: Predicate<TValue>): Integer; static;

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
    class function CreateMatcher<T>(const condition: Predicate<TValue>): T; static;

    /// <summary>
    ///   Creates an array of match predicates based on the passed arguments
    ///   that contain the index of the according match predicate on the thread
    ///   local <c>conditionsStack</c> variable that was filled by <c>
    ///   Spring.Mocking.Arg</c> .
    /// </summary>
    class function CreateMatchers(const indizes: TArray<TValue>;
      const parameters: TArray<TRttiParameter>): Predicate<TArray<TValue>>; static;

    class procedure ClearConditionStack; static;
  end;

  RefArgs = record
  private
    class threadvar fValues: TArray<TValue>;
    class function GetValues: TArray<TValue>; static;
    class procedure SetValues(const values: TArray<TValue>); static;
  public
    class procedure Add(const value; typeInfo: PTypeInfo); static;
    class property Values: TArray<TValue> read GetValues write SetValues;
  end;

  TRangeKind = (Inclusive, Exclusive);

  TArg = record
  private type
    TArg<T> = record
      class var IsAny: T;
    end;
    TRef<T> = record
      class var Return: T;
    end;
  private
    fIndex: Integer;
  public
    class function IsAny<T>: T; overload; static;
    class function IsAny<T>(const condition: Predicate<T>): T; overload; static;
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

    class function Ref<T>: TArg<T>; overload; static; inline;
    class function Ref<T>(const value: T): TRef<T>; overload; static; inline;

    class function &&op_Equality<T>(const left: TArg; const right: T): T; static;
    class function &&op_Inequality<T>(const left: TArg; const right: T): T; static;
  end;

  TArgs = record
  strict private
    class function GetAny: Predicate<TArray<TValue>>; static;
    class function GetItems(index: Integer): TArg; static;
  public
    class property Any: Predicate<TArray<TValue>> read GetAny;
    class property Items[index: Integer]: TArg read GetItems; default;
  end;

  TAny = record
    class operator Implicit(const value: TAny): Boolean; overload;
    class operator Implicit(const value: TAny): Integer; overload;
    class operator Implicit(const value: TAny): Pointer; overload;
    class operator Implicit(const value: TAny): string; overload;
  end;

procedure CleanupArguments(const arguments: array of TValue);

implementation

uses
  Generics.Defaults,
  RegularExpressions,
  SysUtils,
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
  Result := PByte(v.GetReferenceToRawData)^;
end;

function GetIndexFloat(const v: TValue): Integer;
begin
  Result := Integer(Trunc(v.AsExtended));
end;

function GetIndexString(const v: TValue): Integer;
begin
  Result := StrToInt(v.AsString);
end;

function GetIndexObject(const v: TValue): Integer;
begin
  Result := TIndexWrapper(TValueData(v).FAsObject).fIndex;
end;

function GetIndexInterface(const v: TValue): Integer;
begin
  Result := (PInterface(TValueData(v).FValueData.GetReferenceToRawData)^ as TIndexWrapper).fIndex;
end;

function GetIndexPointer(const v: TValue): Integer;
begin
  Result := Integer(TValueData(v).FAsPointer);
end;

function GetIndexRecord(const v: TValue): Integer;
var
  fields: TArray<TRttiField>;
begin
  fields := TType.GetType(v.TypeInfo).GetFields;
  if fields = nil then
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

procedure SetIndexOrdinal(typeInfo: PTypeInfo; //FI:O804
  index: Integer; var Result);
begin
  PByte(@Result)^ := Byte(index);
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
    tkLString:
      PAnsiString(@Result)^ := AnsiString(s);
    tkWString:
      PWideString(@Result)^ := s;
    tkUString:
      PUnicodeString(@Result)^ := s;
  end;
end;

procedure SetIndexObject(typeInfo: PTypeInfo; //FI:O804
  index: Integer; var Result);
begin
  TObject(Result) := TIndexWrapper.Create(index);
end;

procedure SetIndexInterface(typeInfo: PTypeInfo; //FI:O804
  index: Integer; var Result);
begin
  IInterface(PPointer(@Result)^) := TIndexWrapper.Create(index);
end;

procedure SetIndexPointer(typeInfo: PTypeInfo; //FI:O804
  index: Integer; var Result);
begin
  NativeInt(Result) := index;
end;

procedure SetIndexRecord(typeInfo: PTypeInfo; index: Integer; var Result);
var
  fields: TArray<TRttiField>;
begin
  fields := typeInfo.RttiType.GetFields;
  if fields = nil then
    raise ENotSupportedException.CreateResFmt(@STypeNotSupported, [typeInfo.TypeName]);
  TMatcherFactory.SetIndex(fields[0].FieldType.Handle, index, Result);
end;

procedure SetIndexArray(typeInfo: PTypeInfo; index: Integer; var Result);
const
  len: NativeInt = 1;
begin
  case typeInfo.Kind of
    tkArray:
      TMatcherFactory.SetIndex(typeInfo.TypeData.ArrayData.ElType^, index, Result);
    tkDynArray:
    begin
      DynArraySetLength(PPointer(@Result)^, typeInfo, 1, @len);
      TMatcherFactory.SetIndex(typeInfo.TypeData.DynArrElType^, index, PPointer(@Result)^^);
    end;
  end;
end;

procedure SetIndexVariant(typeInfo: PTypeInfo; //FI:O804
  index: Integer; var Result);
begin
  PVariant(@Result)^ := index;
end;

procedure CleanupArguments(const arguments: array of TValue);
type
  PValueData = ^TValueData;
var
  i: Integer;
  fields: TArray<TRttiField>;
  value: TValue;
begin
  for i := 0 to High(arguments) do
    if arguments[i].IsType(TypeInfo(TIndexWrapper)) then
    begin
      TObject(TValueData(arguments[i]).FAsObject).Free;
      PValueData(@arguments[i]).FAsObject := nil;
    end else
      case arguments[i].Kind of
        tkRecord{$IF Declared(tkMRecord)}, tkMRecord{$IFEND}:
        begin
          fields := TType.GetType(arguments[i].TypeInfo).GetFields;
          if fields = nil then
            Continue;
          value := fields[0].GetValue(arguments[i].GetReferenceToRawData);
          CleanupArguments(value);
        end;
        tkArray, tkDynArray:
        begin
          if arguments[i].GetArrayLength = 0 then
            Continue;
          value := arguments[i].GetArrayElement(0);
          CleanupArguments(value);
        end;
      end;
end;


{$REGION 'TMatcherFactory'}

class function TMatcherFactory.CreateMatchers(const indizes: TArray<TValue>;
  const parameters: TArray<TRttiParameter>): Predicate<TArray<TValue>>;
var
  argCount, indexCount, i: Integer;
  conditions: TArray<Predicate<TValue>>;
begin
  argCount := Length(conditionStack);
  if argCount > 0 then
  begin
    try
      indexCount := Length(indizes);
      for i := 0 to indexCount - 1 do
        if (parameters[i].Flags * [pfVar, pfOut] <> []) // ignore var or out parameters
          or indizes[i].IsEmpty then // nil can be passed without Arg, we handle it
          Inc(argCount);

      if argCount <> indexCount then
        raise ENotSupportedException.Create('when using Arg all arguments must be passed using this way');

      SetLength(conditions, indexCount);
      for i := 0 to indexCount - 1 do
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
  const condition: Predicate<TValue>): T;
var
  index: Integer;
begin
  index := AddMatcher(condition);
  Result := WrapIndex<T>(index);
end;

class procedure TMatcherFactory.ClearConditionStack;
begin
  conditionStack := nil;
end;

class function TMatcherFactory.AddMatcher(
  const condition: Predicate<TValue>): Integer;
begin
  Result := Length(conditionStack);
  SetLength(conditionStack, Result + 1);
  conditionStack[Result] := condition;
end;

class function TMatcherFactory.GetIndex(const v: TValue): Integer;
const
  Handlers: array[TTypeKind] of function(const v: TValue): Integer = (
    GetIndexFail, GetIndexOrdinal, GetIndexOrdinal, GetIndexOrdinal, GetIndexFloat,
    GetIndexFail, GetIndexOrdinal, GetIndexObject, GetIndexPointer, GetIndexString,
    GetIndexString, GetIndexString, GetIndexVariant, GetIndexArray, GetIndexRecord,
    GetIndexInterface, GetIndexOrdinal, GetIndexArray, GetIndexString, GetIndexPointer,
    GetIndexObject, GetIndexPointer {$IF Declared(tkMRecord)}, GetIndexRecord{$IFEND});
begin
  Result := Handlers[TValueData(v).FTypeInfo.Kind](v) - 1;
end;

class procedure TMatcherFactory.SetIndex(typeInfo: PTypeInfo; index: Integer; var Result);
const
  Handlers: array[TTypeKind] of procedure (typeInfo: PTypeInfo; index: Integer; var Result) = (
    SetIndexFail, SetIndexOrdinal, SetIndexOrdinal, SetIndexOrdinal, SetIndexFloat,
    SetIndexFail, SetIndexOrdinal, SetIndexObject, SetIndexPointer, SetIndexString,
    SetIndexString, SetIndexString, SetIndexVariant, SetIndexArray, SetIndexRecord,
    SetIndexInterface, SetIndexOrdinal, SetIndexArray, SetIndexString, SetIndexPointer,
    SetIndexObject, SetIndexPointer {$IF Declared(tkMRecord)}, SetIndexRecord{$IFEND});
begin
  Handlers[typeInfo.Kind](typeInfo, index + 1, Result);
end;

class function TMatcherFactory.WrapIndex<T>(index: Integer): T;
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


{$REGION 'RefArgs'}

class procedure RefArgs.Add(const value; typeInfo: PTypeInfo);
var
  i, count: Integer;
begin
  count := Length(fValues);
  for i := 0 to count - 1 do
    if fValues[i].TypeInfo = typeInfo then
      raise ENotSupportedException.Create('multiple by reference parameters of the same type are not supported');
  SetLength(fValues, count + 1);
  TValue.Make(@value, typeInfo, fValues[count]);
end;

class function RefArgs.GetValues: TArray<TValue>;
begin
  Result := fValues;
end;

class procedure RefArgs.SetValues(const values: TArray<TValue>);
begin
  fValues := values;
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

class function TArg.IsAny<T>(const condition: Predicate<T>): T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    var
      argValue: T;
    begin
      arg.AsType(TypeInfo(T), argValue);
      Result := condition(argValue);
    end);
end;

class function TArg.IsEqual<T>(const value: T): T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := arg.Convert(TypeInfo(T)).Equals(TValue.From(value, TypeInfo(T)));
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
    var
      argValue: T;
    begin
      arg.AsType(TypeInfo(T), argValue);
      Result := TArray.Contains<T>(capturedValues, argValue);
    end);
end;

class function TArg.IsIn<T>(const values: IEnumerable<T>): T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    var
      argValue: T;
    begin
      arg.AsType(TypeInfo(T), argValue);
      Result := values.Contains(argValue);
    end);
end;

class function TArg.IsInRange<T>(const lowValue, highValue: T;
  rangeKind: TRangeKind): T;
var
  comparer: Pointer;
begin
  comparer := _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T));
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    var
      argValue: T;
    begin
      arg.AsType(TypeInfo(T), argValue);
      if rangeKind = TRangeKind.Exclusive then
        Result := (IComparer<T>(comparer).Compare(argValue, lowValue) > 0)
          and (IComparer<T>(comparer).Compare(argValue, highValue) < 0)
      else
        Result := (IComparer<T>(comparer).Compare(argValue, lowValue) >= 0)
          and (IComparer<T>(comparer).Compare(argValue, highValue) <= 0);
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
    var
      argValue: T;
    begin
      arg.AsType(TypeInfo(T), argValue);
      Result := not TArray.Contains<T>(capturedValues, argValue);
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
    var
      argValue: T;
    begin
      arg.AsType(TypeInfo(T), argValue);
      Result := not values.Contains(argValue);
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
      Result := regex.IsMatch(arg.AsString);
    end);
end;

class function TArg.&&op_Equality<T>(const left: TArg; //FI:O804
  const right: T): T;
var
  comparer: Pointer;
begin
  if (TType.Kind<T> = tkPointer) and (PPointer(@right)^ = nil) then
    Result := TMatcherFactory.CreateMatcher<T>(
      function(const arg: TValue): Boolean
      begin
        Result := arg.IsEmpty;
      end)
  else
  begin
    comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
    Result := TMatcherFactory.CreateMatcher<T>(
      function(const arg: TValue): Boolean
      begin
        Result := IEqualityComparer<T>(comparer).Equals(arg.AsType<T>, right);
      end);
  end;
end;

class function TArg.&&op_Inequality<T>(const left: TArg; //FI:O804
  const right: T): T;
var
  comparer: Pointer;
begin
  if (TType.Kind<T> = tkPointer) and (PPointer(@right)^ = nil) then
    Result := TMatcherFactory.CreateMatcher<T>(
      function(const arg: TValue): Boolean
      begin
        Result := not arg.IsEmpty;
      end)
  else
  begin
    comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
    Result := TMatcherFactory.CreateMatcher<T>(
      function(const arg: TValue): Boolean
      begin
        Result := not IEqualityComparer<T>(comparer).Equals(arg.AsType<T>, right);
      end);
  end;
end;

class function TArg.Ref<T>: TArg<T>; //FI:W521
begin //FI:W519
end;

class function TArg.Ref<T>(const value: T): TRef<T>; //FI:W521
begin
  RefArgs.Add(value, TypeInfo(T));
end;

{$ENDREGION}


{$REGION 'TArgs'}

class function TArgs.GetAny: Predicate<TArray<TValue>>;
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


{$REGION 'TAny'}

class operator TAny.Implicit(const value: TAny): Boolean;
begin
  Result := TMatcherFactory.CreateMatcher<Boolean>(
    function(const arg: TValue): Boolean
    begin
      Result := True;
    end);
end;

class operator TAny.Implicit(const value: TAny): Integer;
begin
  Result := TMatcherFactory.CreateMatcher<Integer>(
    function(const arg: TValue): Boolean
    begin
      Result := True;
    end);
end;

class operator TAny.Implicit(const value: TAny): Pointer;
begin
  Result := TMatcherFactory.CreateMatcher<Pointer>(
    function(const arg: TValue): Boolean
    begin
      Result := True;
    end);
end;

class operator TAny.Implicit(const value: TAny): string;
begin
  Result := TMatcherFactory.CreateMatcher<string>(
    function(const arg: TValue): Boolean
    begin
      Result := True;
    end);
end;

{$ENDREGION}


end.
