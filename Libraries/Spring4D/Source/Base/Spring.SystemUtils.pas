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

/// <summary>
///   Provides the internal utilities for Spring4D.
/// </summary>
unit Spring.SystemUtils;

interface

uses
  SysUtils,
  Types,
  TypInfo,
  Spring;

type
  TEnum = Spring.TEnum deprecated 'Use Spring.TEnum instead';

/// <summary>
///   Retrieves the byte length of a unicode string.
/// </summary>
/// <remarks>
///   Although there is already a routine <c>SysUtils.ByteLength(string)</c>
///   function, it only supports unicode strings and doesn't provide overloads
///   for WideStrings and AnsiStrings.
/// </remarks>
function GetByteLength(const s: string): Integer; overload; inline;

{$IFNDEF NEXTGEN}
/// <summary>
///   Retrieves the byte length of a WideString.
/// </summary>
function GetByteLength(const s: WideString): Integer; overload; inline;

/// <summary>
///   Retrieves the byte length of a <c>RawByteString</c> (AnsiString or
///   UTF8String).
/// </summary>
function GetByteLength(const s: RawByteString): Integer; overload; inline;
{$ENDIF NEXTGEN}


/// <summary>
///   Splits a string into different parts delimited by the specified delimiter
///   characters.
/// </summary>
/// <remarks>
///   Each element of separator defines a separate delimiter character. If two
///   delimiters are adjacent, or a delimiter is found at the beginning or end
///   of the buffer, the corresponding array element contains an additional
///   empty string is removeEmptyEntries is not True
/// </remarks>
function SplitString(const buffer: string; const separators: TSysCharSet;
  removeEmptyEntries: Boolean = False): TStringDynArray; overload;
function SplitString(const buffer: TCharArray; const separators: TSysCharSet;
  removeEmptyEntries: Boolean = False): TStringDynArray; overload;
function SplitString(const buffer: PChar; len: Integer; const separators: TSysCharSet;
  removeEmptyEntries: Boolean = False): TStringDynArray; overload;

/// <summary>
///   Returns a string array that contains the substrings in the buffer that
///   are delimited by null char (#0) and ends with an additional null char.
/// </summary>
/// <example>
///   <code lang="Delphi">procedure TestSplitNullTerminatedStrings;
/// var
///   buffer: string;
///   strings: TStringDynArray;
///   s: string;
/// begin
///   buffer := 'C:'#0'D:'#0'E:'#0#0;
///   strings := SplitString(PChar(buffer));
///   for s in strings do
///     Writeln(s);
/// end;</code>
/// </example>
function SplitString(const buffer: PChar): TStringDynArray; overload;

/// <summary>
///   Converts a string to a TDateTime value using the specified format, with a
///   Boolean success code.
/// </summary>
function TryStrToDateTimeFmt(const s, format: string; out value: TDateTime): Boolean;

/// <summary>
///   Converts a string to a TDateTime value using the specified format.
/// </summary>
function StrToDateTimeFmt(const s, format: string): TDateTime;

implementation

uses
  DateUtils,
  Spring.ResourceStrings;


function GetByteLength(const s: string): Integer;
begin
  Result := Length(s) * SizeOf(Char);
end;

{$IFNDEF NEXTGEN}
function GetByteLength(const s: WideString): Integer;
begin
  Result := Length(s) * SizeOf(WideChar);
end;

function GetByteLength(const s: RawByteString): Integer;
begin
  Result := Length(s);
end;
{$ENDIF}

function SplitString(const buffer: string; const separators: TSysCharSet;
  removeEmptyEntries: Boolean): TStringDynArray;
begin
  Result := SplitString(PChar(buffer), Length(buffer), separators, removeEmptyEntries);
end;

function SplitString(const buffer: TCharArray; const separators: TSysCharSet;
  removeEmptyEntries: Boolean): TStringDynArray;
begin
  Result := SplitString(PChar(buffer), Length(buffer), separators, removeEmptyEntries)
end;

function SplitString(const buffer: PChar; len: Integer; const separators: TSysCharSet;
  removeEmptyEntries: Boolean): TStringDynArray;

  procedure AppendEntry(buffer: PChar; len: Integer; var strings: TStringDynArray);
  var
    entry: string;
  begin
    SetString(entry, buffer, len);
    if not removeEmptyEntries or (entry <> '') then
    begin
      SetLength(strings, Length(strings) + 1);
      strings[High(strings)] := entry;
    end;
  end;

var
  head, tail, p: PChar;
begin
  Guard.CheckRange(len >= 0, 'len');

  Result := nil;
  if (buffer = nil) or (len = 0) then
    Exit;
  head := buffer;
  tail := head + len - 1;
  p := head;
  while p <= tail do
  begin
    if CharInSet(p^, separators) then
    begin
      AppendEntry(head, p - head, Result);
      head := StrNextChar(p);
    end;
    if p = tail then
      AppendEntry(head, p - head + 1, Result);
    p := StrNextChar(p);
  end;
end;

function SplitString(const buffer: PChar): TStringDynArray;
var
  p: PChar;
  i: Integer;
begin
  Result := nil;
  if buffer = nil then
    Exit;
  i := 0;
  p := buffer;
  while p^ <> #0 do
  begin
    p := StrEnd(P);
    Inc(p);
    Inc(i);
  end;
  if i = 0 then
    Exit;
  SetLength(Result, i);
  i := 0;
  p := buffer;
  while p^ <> #0 do
  begin
    Result[i] := p;
    p := StrEnd(P);
    Inc(p);
    Inc(i);
  end;
end;

function TryStrToDateTimeFmt(const s, format: string; out value: TDateTime): Boolean;
var
  localString: string;
  stringFormat: string;
  year, month, day: Word;
  hour, minute, second, milliSecond: Word;

  function ExtractElementDef(const element: string; const defaultValue: Integer = 0): Integer;
  var
    position: Integer;
  begin
    position := Pos(element, stringFormat);
    if position > 0 then
      Result := StrToInt(Copy(localString, position, Length(element)))
    else
      Result := defaultValue;
  end;

begin
  localString := Trim(s);
  stringFormat := UpperCase(format);
  Result := Length(localString) = Length(stringFormat);
  if Result then
  try
    year := ExtractElementDef('YYYY', 0);
    if year = 0 then
    begin
      year := ExtractElementDef('YY', 1899);
      if year < 1899 then
        Inc(year, (YearOf(Today) div 100) * 100);
    end;
    month := ExtractElementDef('MM', 12);
    day := ExtractElementDef('DD', 30);
    hour := ExtractElementDef('HH');
    minute := ExtractElementDef('NN');
    second := ExtractElementDef('SS');
    milliSecond := ExtractElementDef('ZZZ');
    value := EncodeDateTime(year, month, day, hour, minute, second, milliSecond);
  except
    Result := False;
  end;
end;

function StrToDateTimeFmt(const s, format: string): TDateTime;
begin
  if not TryStrToDateTimeFmt(s, format, Result) then
    raise EConvertError.CreateResFmt(@SInvalidDateTime, [s]);
end;

end.
