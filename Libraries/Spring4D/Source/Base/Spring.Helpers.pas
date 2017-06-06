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

/// <summary>
///   Provides many easy to use class and record helpers to extend some common
///   types in the RTL.
/// </summary>
unit Spring.Helpers;

{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  Types;

type
  /// <summary>
  ///   Provides a static method to create a TMethod structure with an instance
  ///   and a methodaddress.
  /// </summary>
  TMethodHelper = record helper for TMethod
  public
    class function Create(const instance, methodAddress: Pointer): TMethod; static;
  end;

  TStreamHelper = class helper for TStream
  public
    /// <summary>
    ///   Reads a value of a value type, which could be an Integer, record,
    ///   etc., from the stream.
    /// </summary>
    /// <remarks>
    ///   <note type="tip">
    ///     The generic argument could be omitted if the compiler can
    ///     automatically inreference the type.
    ///   </note>
    /// </remarks>
    /// <example>
    ///   <para>
    ///     The following example demonstrates how to use the generic <c>
    ///     ReadBuffer&lt;T&gt;</c> and <c>WriteBuffer&lt;T&gt;</c>methods.
    ///   </para>
    ///   <code lang="Delphi">procedure TestStreamHelper;
    /// var
    ///   stream: TStream;
    ///   value: Integer;
    /// begin
    ///   stream := TMemoryStream.Create;
    ///   try
    ///     value := 2;
    ///     stream.WriteBuffer(value);
    ///     stream.Position := 0;
    ///     stream.ReadBuffer&lt;Integer&gt;(value);
    ///   finally
    ///     stream.Free;
    ///   end;
    /// end;</code>
    /// </example>
    procedure ReadBuffer<T: record>(var value: T); overload;

    /// <summary>
    ///   Writes a value of a value type to the stream.
    /// </summary>
    procedure WriteBuffer<T: record>(const value: T); overload;
  end;

  TStringsHelper = class helper for TStrings
  private
    function GetIsEmpty: Boolean;
  public
    /// <summary>
    ///   Add an array of string to the list.
    /// </summary>
    procedure AddStrings(const strings: array of string); overload;

    /// <summary>
    ///   Adds or updates a name-value pair.
    /// </summary>
    /// <remarks>
    ///   <note type="warning">
    ///     There is a <c>Values[name: string]</c>property in the TStrings
    ///     class, but the entry will be removed if the value is empty.
    ///   </note>
    /// </remarks>
    procedure AddOrUpdate(const name, value: string);

//    procedure Remove(const s: string);

    /// <summary>
    ///   Executes a procedure during batch updating of the list.
    /// </summary>
    /// <exception cref="Spring|EArgumentNullException">
    ///   Raised if the<paramref name="strings" /> is nil or the <paramref name="proc" />
    ///    is not assigned.
    /// </exception>
    procedure ExecuteUpdate(const proc: TProc);

    /// <summary>
    ///   Extract all name entries and add them to the <paramref name="strings" />
    ///    list.
    /// </summary>
    /// <exception cref="Spring|EArgumentNullException">
    ///   Raised if the <paramref name="strings" /> is nil.
    /// </exception>
    /// <seealso cref="ExtractValues(TStrings)">
    ///   ExtractValues
    /// </seealso>
    procedure ExtractNames(const strings: TStrings);

    /// <summary>
    ///   Extract all value entries and add them to the <paramref name="strings" />
    ///    list.
    /// </summary>
    /// <exception cref="Spring|EArgumentNullException">
    ///   Raised if the <paramref name="strings" /> is nil.
    /// </exception>
    /// <seealso cref="ExtractNames(TStrings)" />
    procedure ExtractValues(const strings: TStrings);

    /// <summary>
    ///   Returns a string array that contains all the <b>name</b>entries in
    ///   the string list.
    /// </summary>
    function GetNames: TStringDynArray;

    /// <summary>
    ///   Returns a string array that contains all the <b>value</b>entries in
    ///   the string list.
    /// </summary>
    function GetValues: TStringDynArray;

//    function GetValue(const name: string): string; overload;
//    function GetValue(const index: Integer): string; overload;

    /// <summary>
    ///   Gets the corresponding value of the name entry if there is such an
    ///   entry and the value is not empty, otherwise, returns the default
    ///   value specified by the <paramref name="default" />param.
    /// </summary>
    function GetValueOrDefault<T>(const name: string; const default: T): T; experimental;

    /// <summary>
    ///   Try finding a name entry in the list.
    /// </summary>
    function TryFindName(const name: string; var index: Integer): Boolean;

    /// <summary>
    ///   Try finding a value entry in the list.
    /// </summary>
    function TryFindValue(const value: string; var index: Integer): Boolean;

    /// <summary>
    ///   Try finding an object in the list.
    /// </summary>
    function TryFindObject(const obj: TObject; var index: Integer): Boolean;

    /// <summary>
    ///   Determines whether the list contains the specified name entry.
    /// </summary>
    function ContainsName(const name: string): Boolean;

    /// <summary>
    ///   Determines whether the list contains the specified value entry.
    /// </summary>
    function ContainsValue(const value: string): Boolean;

    /// <summary>
    ///   Determines whether the list contains the specified object.
    /// </summary>
    function ContainsObject(const obj: TObject): Boolean;

    /// <summary>
    ///   Converts the string list to a dynamic string array.
    /// </summary>
    function ToArray: TStringDynArray;

    /// <summary>
    ///   Gets a value indicates whether the strings is empty.
    /// </summary>
    /// <value>
    ///   Returns true if the count of the list is zero, otherwise, returns
    ///   false.
    /// </value>
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TCollectionHelper = class helper for TCollection
  public
    /// <param name="proc">
    ///   the anonymous method that will be executed within the batch update.
    /// </param>
    procedure ExecuteUpdate(const proc: TProc);
  end;

implementation

uses
  Rtti,
  TypInfo,
  Spring.ResourceStrings;


{$REGION 'TMethodHelper'}

class function TMethodHelper.Create(const instance,
  methodAddress: Pointer): TMethod;
begin
  Result.Code := methodAddress;
  Result.Data := instance;
end;

{$ENDREGION}


{$REGION 'TStreamHelper'}

procedure TStreamHelper.ReadBuffer<T>(var value: T);
begin
  ReadBuffer(value, SizeOf(T));
end;

procedure TStreamHelper.WriteBuffer<T>(const value: T);
begin
  WriteBuffer(value, SizeOf(T));
end;

{$ENDREGION}


{$REGION 'TStringsHelper'}

procedure TStringsHelper.AddStrings(const strings: array of string);
var
  s: string;
begin
  BeginUpdate;
  try
    for s in strings do
    begin
      Add(s);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TStringsHelper.AddOrUpdate(const name, value: string);
var
  index: Integer;
begin
  index := IndexOfName(name);
  if index <> -1 then
    Strings[index] := name + NameValueSeparator + value
  else
    Add(name + NameValueSeparator + value);
end;

procedure TStringsHelper.ExecuteUpdate(const proc: TProc);
begin
  BeginUpdate;
  try
    Clear;
    proc;
  finally
    EndUpdate;
  end;
end;

procedure TStringsHelper.ExtractNames(const strings: TStrings);
var
  i: Integer;
begin
  strings.BeginUpdate;
  try
    for i := 0 to Count - 1 do
      strings.Add(Self.Names[i]);
  finally
    strings.EndUpdate;
  end;
end;

procedure TStringsHelper.ExtractValues(const strings: TStrings);
var
  i: Integer;
begin
  strings.BeginUpdate;
  try
    for i := 0 to Count - 1 do
      strings.Add(Self.ValueFromIndex[i]);
  finally
    strings.EndUpdate;
  end;
end;

function TStringsHelper.TryFindName(const name: string;
  var index: Integer): Boolean;
begin
  index := IndexOfName(name);
  Result := index > -1;
end;

function TStringsHelper.TryFindValue(const value: string;
  var index: Integer): Boolean;
var
  v: string;
  i: Integer;
begin
  index := -1;
  Result := False;
  for i := 0 to Count - 1 do
  begin
    v := ValueFromIndex[i];
    if SameText(v, value) then
    begin
      index := i;
      Exit(True);
    end;
  end;
end;

function TStringsHelper.TryFindObject(const obj: TObject;
  var index: Integer): Boolean;
begin
  index := IndexOfObject(obj);
  Result := index > -1;
end;

function TStringsHelper.ContainsName(const name: string): Boolean;
begin
  Result := IndexOfName(name) > -1;
end;

function TStringsHelper.ContainsValue(const value: string): Boolean;
var
  index: Integer;
begin
  Result := TryFindValue(value, index);
end;

function TStringsHelper.ContainsObject(const obj: TObject): Boolean;
begin
  Result := IndexOfObject(obj) > -1;
end;

function TStringsHelper.GetValueOrDefault<T>(const name: string;
  const default: T): T;
var
  index: Integer;
  value: string;
begin
  index := IndexOfName(name);
  if index > -1 then
    value := ValueFromIndex[index];
  if value <> '' then
    Result := TValue.From<string>(value).AsType<T>  // TODO: Fix this ASAP because TValue.AsType<T> sucks...
  else
    Result := default;
end;

function TStringsHelper.GetNames: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Names[i];
end;

function TStringsHelper.GetValues: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := ValueFromIndex[i];
end;

function TStringsHelper.ToArray: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Strings[i];
end;

function TStringsHelper.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

{$ENDREGION}


{$REGION 'TCollectionHelper'}

procedure TCollectionHelper.ExecuteUpdate(const proc: TProc);
begin
  BeginUpdate;
  try
    Clear;
    proc;
  finally
    EndUpdate;
  end;
end;

{$ENDREGION}


end.
