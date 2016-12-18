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

unit Spring.Mocking.Matching;

{$I Spring.inc}

interface

uses
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.DesignPatterns;

type
  TArgMatch = TPredicate<TArray<TValue>>;

  TMatcherFactory = record
  private
    class threadvar conditions: TArray<TPredicate<TValue>>;

    type
      TIndexWrapper = class(TInterfacedObject)
      private
        fIndex: Integer;
        constructor Create(index: Integer);
      end;

    class function AddMatcher(const condition: TPredicate<TValue>): Integer; static;

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
    ///   Creates an array match predicate based on the passed arguments that
    ///   contain the index of the according match predicate on the global <c>
    ///   conditions</c> variable passed by <c>Spring.Mocking.Arg</c>.
    /// </summary>
    class function CreateMatchers(const indizes: TArray<TValue>): TPredicate<TArray<TValue>>; static;
  end;

  TRangeKind = (Inclusive, Exclusive);

  Arg = record
  private
    fIndex: Integer;
  public
    class function IsAny<T>: T; overload; static;
    class function IsAny<T>(const condition: TPredicate<T>): T; overload; static;
    class function IsEqual<T>(const value: T): T; static;
    class function IsIn<T>(const values: array of T): T; overload; static;
    class function IsIn<T>(const values: IEnumerable<T>): T; overload; static;
    class function IsInRange<T>(const lowValue, highValue: T;
      rangeKind: TRangeKind = TRangeKind.Inclusive): T; static;
    class function IsNil<T>: T; static;
    class function IsNotIn<T>(const values: array of T): T; overload; static;
    class function IsNotIn<T>(const values: IEnumerable<T>): T; overload; static;
    class function IsNotNil<T>: T; static;
  end;

  Args = record
  strict private
    class function GetAny: TPredicate<TArray<TValue>>; static;
    class function GetItems(index: Integer): Arg; static;
  public
    class property Any: TPredicate<TArray<TValue>> read GetAny;
    class property Items[index: Integer]: Arg read GetItems; default;
  end;

implementation

uses
  Generics.Defaults,
  Rtti,
  TypInfo;


{$REGION 'TMatcherFactory'}

class function TMatcherFactory.CreateMatchers(
  const indizes: TArray<TValue>): TPredicate<TArray<TValue>>;
var
  capturedConditions: TArray<TPredicate<TValue>>;
  idxArr: TArray<Integer>;
  i: Integer;
begin
  if Assigned(conditions) then
  begin
    if Length(conditions) <> Length(indizes) then
      raise ENotSupportedException.Create('when using Arg all arguments must be passed using this way');

    SetLength(idxArr, Length(indizes));
    for i := Low(idxArr) to High(idxArr) do
      case indizes[i].Kind of
        tkInteger, tkChar, tkWChar, tkEnumeration, tkSet, tkInt64:
          idxArr[indizes[i].AsOrdinal] := i;
        tkLString, tkWString, tkUString:
          idxArr[StrToInt(indizes[i].AsString)] := i;
        tkClass:
        begin
          idxArr[indizes[i].AsType<TIndexWrapper>.fIndex] := i;
{$IFNDEF AUTOREFCOUNT}
          indizes[i].AsType<TIndexWrapper>.Free;
{$ELSE}
          indizes[i].AsType<TIndexWrapper>.DisposeOf;
{$ENDIF}
        end;
        tkInterface:
        begin
          idxArr[(indizes[i].AsType<IInterface> as TIndexWrapper).fIndex] := i;
          PValue(@indizes[i])^ := TValue.Empty;
        end;
        tkVariant:
          idxArr[Integer(indizes[i].AsVariant)] := i;
        tkDynArray:
          idxArr[TArray<Integer>(indizes[i].GetReferenceToRawData^)[0]] := i;
      else
        raise ENotSupportedException.Create('type not supported');
      end;

    capturedConditions := conditions;
    conditions := nil;
    Result :=
      function(const args: TArray<TValue>): Boolean
      var
        i: Integer;
      begin
        for i := Low(idxArr) to High(idxArr) do
          if not capturedConditions[i](args[idxArr[i]]) then
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
  Result := Length(conditions);
  SetLength(conditions, Result + 1);
  conditions[Result] := condition;
end;

class function TMatcherFactory.WrapIndex<T>(index: Integer): T;
begin
  Result := Default(T);
  case TType.Kind<T> of
    tkInteger, tkChar, tkWChar, tkEnumeration, tkSet, tkInt64:
      PByte(@Result)^ := index;
{$IFNDEF NEXTGEN}
    tkLString:
      PAnsiString(@Result)^ := AnsiString(IntToStr(index));
    tkWString:
      PWideString(@Result)^ := IntToStr(index);
{$ENDIF}
    tkUString:
      PUnicodeString(@Result)^ := IntToStr(index);
    tkClass:
      TObject(PPointer(@Result)^) := TIndexWrapper.Create(index);
    tkInterface:
      IInterface(PPointer(@Result)^) := TIndexWrapper.Create(index);
    tkVariant:
      PVariant(@Result)^ := index;
    tkDynArray:
      TArray<Integer>(PPointer(@Result)^) := TArray<Integer>.Create(index);
  else
    raise ENotSupportedException.Create('type not supported');
  end;
end;

{$ENDREGION}


{$REGION 'TMatcherFactory.TIndexWrapper'}

constructor TMatcherFactory.TIndexWrapper.Create(index: Integer);
begin
  fIndex := index;
end;

{$ENDREGION}


{$REGION 'Arg'}

class function Arg.IsAny<T>: T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := True;
    end);
end;

class function Arg.IsAny<T>(const condition: TPredicate<T>): T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := condition(arg.AsType<T>);
    end);
end;

class function Arg.IsEqual<T>(const value: T): T;
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

class function Arg.IsIn<T>(const values: array of T): T;
var
  capturedValues: TArray<T>;
  comparer: IEqualityComparer<T>;
begin
  capturedValues := TArray.Copy<T>(values);
  comparer := TEqualityComparer<T>.Default;
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    var
      i: Integer;
    begin
      for i := Low(capturedValues) to High(capturedValues) do
        if comparer.Equals(arg.AsType<T>, capturedValues[i]) then
          Exit(True);
      Result := False;
    end);
end;

class function Arg.IsIn<T>(const values: IEnumerable<T>): T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := values.Contains(arg.AsType<T>);
    end);
end;

class function Arg.IsInRange<T>(const lowValue, highValue: T;
  rangeKind: TRangeKind): T;
var
  comparer: IComparer<T>;
begin
  comparer := TComparer<T>.Default;
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      if rangeKind = TRangeKind.Exclusive then
        Result := (comparer.Compare(arg.AsType<T>, lowValue) > 0)
          and (comparer.Compare(arg.AsType<T>, highValue) < 0)
      else
        Result := (comparer.Compare(arg.AsType<T>, lowValue) >= 0)
          and (comparer.Compare(arg.AsType<T>, highValue) <= 0);
    end);
end;

class function Arg.IsNil<T>: T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := arg.IsEmpty;
    end);
end;

class function Arg.IsNotIn<T>(const values: array of T): T;
var
  capturedValues: TArray<T>;
  comparer: IEqualityComparer<T>;
begin
  capturedValues := TArray.Copy<T>(values);
  comparer := TEqualityComparer<T>.Default;
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    var
      i: Integer;
    begin
      for i := Low(capturedValues) to High(capturedValues) do
        if comparer.Equals(arg.AsType<T>, capturedValues[i]) then
          Exit(False);
      Result := True;
    end);
end;

class function Arg.IsNotIn<T>(const values: IEnumerable<T>): T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := not values.Contains(arg.AsType<T>);
    end);
end;

class function Arg.IsNotNil<T>: T;
begin
  Result := TMatcherFactory.CreateMatcher<T>(
    function(const arg: TValue): Boolean
    begin
      Result := not arg.IsEmpty;
    end);
end;

{$ENDREGION}


{$REGION 'Args'}

class function Args.GetAny: TPredicate<TArray<TValue>>;
begin
  Result :=
    function(const args: TArray<TValue>): Boolean
    begin
      Result := True;
    end;
end;

class function Args.GetItems(index: Integer): Arg;
begin
  Result.fIndex := index;
end;

{$ENDREGION}


end.
