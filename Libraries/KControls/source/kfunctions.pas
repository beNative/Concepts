{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kfunctions; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
 {$IFDEF MSWINDOWS}
  Windows,
 {$ELSE}
  {$IF DEFINED(UNIX) and (FPC_FULLVERSION>=20701)}
    UnixCP,
  {$IFEND}
 {$ENDIF}
  LazUTF8,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  Classes, Contnrs, SysUtils;

const
  { Carriage return character. }
  cCR = #13;
  { Line feed character. }
  cLF = #10;
  { TAB character. }
  cTAB = #9;
  { SPACE character. }
  cSPACE = #32;
  { String terminator. }
  cNULL = #0;
  { Set of word break characters. }
  cWordBreaks = [cNULL, cTAB, cSPACE];
  { Set of line break characters. }
  cLineBreaks = [cCR, cLF];
  { Text ellipsis string. }
  cEllipsis = '...';
  { Alphabetic letters. }
  cLetters = ['a'..'z', 'A'..'Z'];
  { Number. }
  cNumbers = ['0'..'9'];

{$IFnDEF FPC}
  lcl_fullversion = 0;
{$ENDIF}

{$IFDEF MSWINDOWS}
  { @exclude }
  SHFolderDll = 'SHFolder.dll';
{$ENDIF}

{$IFDEF UNIX}
  cEOL = cLF;
  cFirstEOL = cLF;
{$ELSE}
  cEOL = cCR + cLF;
  cFirstEOL = cCR;
{$ENDIF}

{$IFDEF MSWINDOWS}
  cDirectoryDelimiter = '\';
{$ELSE}
  cDirectoryDelimiter = '/';
{$ENDIF}

  cUTF16FirstSurrogateBegin = $D800;
  cUTF16FirstSurrogateEnd = $DBFF;
  cUTF16SecondSurrogateBegin = $DC00;
  cUTF16SecondSurrogateEnd = $DFFF;

  cHexFmtText = '%.2x';
  cHexBase = 16;
  cHexDigitCount = 2;

type
  //PInteger = ^Integer; defined by System.pas
  { Static array for Integer. }
  TIntegers = array[0..MaxInt div SizeOf(Integer) - 1] of Integer;
  { Pointer for TIntegers. }
  PIntegers = ^TIntegers;
  { Dynamic array for Integer. }
  TDynIntegers = array of Integer;

  //PCardinal = ^Cardinal; defined by System.pas
  { Static array for Cardinal. }
  TCardinals = array[0..MaxInt div SizeOf(Cardinal) - 1] of Cardinal;
  { Pointer for TCardinals. }
  PCardinals = ^TCardinals;
  { Dynamic array for Cardinal. }
  TDynCardinals = array of Cardinal;

  //PShortInt = ^ShortInt; defined by System.pas
  { Static array for ShortInt. }
  TShortInts = array[0..MaxInt div SizeOf(ShortInt) - 1] of ShortInt;
  { Pointer for TShortInts. }
  PShortInts = ^TShortInts;
  { Dynamic array for ShortInt. }
  TDynShortInts = array of ShortInt;

  //PSmallInt = ^SmallInt; defined by System.pas
  { Static array for SmallInt. }
  TSmallInts = array[0..MaxInt div SizeOf(SmallInt) - 1] of SmallInt;
  { Pointer for TSmallInts. }
  PSmallInts = ^TSmallInts;
  { Dynamic array for SmallInt. }
  TDynSmallInts = array of SmallInt;

  //PLongInt = ^LongInt; defined by System.pas
  { Static array for LongInt. }
  TLongInts = array[0..MaxInt div SizeOf(LongInt) - 1] of LongInt;
  { Pointer for TLongInts. }
  PLongInts = ^TLongInts;
  { Dynamic array for LongInt. }
  TDynLongInts = array of LongInt;

  //PInt64 = ^Int64; defined by System.pas
  { Static array for Int64. }
  TInt64s = array[0..MaxInt div SizeOf(Int64) - 1] of Int64;
  { Pointer for TInt64s. }
  PInt64s = ^TInt64s;
  { Dynamic array for Int64. }
  TDynInt64s = array of Int64;

  //PByte = ^Byte; defined by System.pas
  { Static array for Byte. }
  TBytes = array[0..MaxInt div SizeOf(Byte) - 1] of Byte;
  { Pointer for TBytes. }
  PBytes = ^TBytes;
  { Dynamic array for Byte. }
  TDynBytes = array of Byte;

  //PWord = ^Word; defined by System.pas
  { Static array for Word. }
  TWords = array[0..MaxInt div SizeOf(Word) - 1] of Word;
  { Pointer for TWords. }
  PWords = ^TWords;
  { Dynamic array for Word. }
  TDynWords = array of Word;

  //PLongWord = ^LongWord; defined by System.pas
  { Static array for LongWord. }
  TLongWords = array[0..MaxInt div SizeOf(LongWord) - 1] of LongWord;
  { Pointer for TLongWords. }
  PLongWords = ^TLongWords;
  { Dynamic array for LongWord. }
  TDynLongWords = array of LongWord;

{$IF DEFINED(COMPILER10_UP) OR DEFINED(FPC)}
 {$IFDEF FPC}
  PUInt64 = ^UInt64;
 {$ELSE}
  //PUInt64 = ^UInt64; defined by System.pas
 {$ENDIF}
  { Static array for UInt64. }
  TUInt64s = array[0..MaxInt div SizeOf(UInt64) - 1] of UInt64;
  { Pointer for TUInt64s. }
  PUInt64s = ^TUInt64s;
  { Dynamic array for UInt64. }
  TDynUInt64s = array of UInt64;
{$IFEND}

  //PSingle = ^Single; defined by System.pas
  { Static array for Single. }
  TSingles = array[0..MaxInt div SizeOf(Single) - 1] of Single;
  { Pointer for TSingles. }
  PSingles = ^TSingles;
  { Dynamic array for Single. }
  TDynSingles = array of Single;

  //PDouble = ^Double; defined by System.pas
  { Static array for Double. }
  TDoubles = array[0..MaxInt div SizeOf(Double) - 1] of Double;
  { Pointer for TDoubles. }
  PDoubles = ^TDoubles;
  { Dynamic array for Double. }
  TDynDoubles = array of Double;

{$IFNDEF FPC}
  //PExtended = ^Extended; defined by System.pas
  { Static array for Extended. }
  TExtendeds = array[0..MaxInt div SizeOf(Extended) - 1] of Extended;
  { Pointer for TExtendeds. }
  PExtendeds = ^TExtendeds;
  { Dynamic array for Extended. }
  TDynExtendeds = array of Extended;
{$ENDIF}

  //PChar is special type
  { Static array for Char. }
  TChars = array[0..MaxInt div SizeOf(Char) - 1] of Char;
  { Pointer for TChars. }
  PChars = ^TChars;
  { Dynamic array for Char. }
  TDynChars = array of Char;

  //PAnsiChar is special type
  { Static array for AnsiChar. }
  TAnsiChars = array[0..MaxInt div SizeOf(AnsiChar) - 1] of AnsiChar;
  { Pointer for TChars. }
  PAnsiChars = ^TAnsiChars;
  { Dynamic array for Char. }
  TDynAnsiChars = array of AnsiChar;

  PBoolean = ^Boolean;
  { Static array for Double. }
  TBooleans = array[0..MaxInt div SizeOf(Boolean) - 1] of Boolean;
  { Pointer for TBooleans. }
  PBooleans = ^TBooleans;
  { Dynamic array for Double. }
  TDynBooleans = array of Boolean;

{$IFDEF FPC}
  { TKString is UTF8 string in Lazarus. }
  TKString = string;
  { TKChar is UTF8 character in Lazarus. }
  TKChar = string[7]; // UTF-8 character is at most 6 bytes plus a #0
  { PKChar is pointer to UTF8 character in Lazarus. }
  PKChar = ^TKChar;
  { PKText is PChar (null terminated UTF8 string) in Lazarus. }
  PKText = PChar;
{$ELSE}
 {$IFDEF STRING_IS_UNICODE}
  { TKString is UnicodeString (UTF16) in unicode aware Delphi. }
  TKString = string;
  { TKChar is Char in unicode aware Delphi. }
  TKChar = Char;
  { PKChar is pointer to Char in unicode aware Delphi. }
  PKChar = ^Char;
  { PKText is PChar in unicode aware Delphi. }
  PKText = PChar;
 {$ELSE}
  { TKString is WideString in old non-unicode Delphi versions. }
  TKString = WideString;
  { TKChar is WideChar in old non-unicode Delphi versions. }
  TKChar = WideChar;
  { PKChar is pointer to WideChar in old non-unicode Delphi versions. }
  PKChar = ^WideChar;
  { PKText is PWideChar in old non-unicode Delphi versions. }
  PKText = PWideChar;
  {$ENDIF}
 {$ENDIF}

  { Useful structure to handle general data and size as a single item }
  TDataSize = record
    Data: Pointer;
    Size: Int64;
  end;
  { Pointer for TDataSize }
  PDataSize = ^TDataSize;

  { Set type for @link(CharInSetEx). }
  TKSysCharSet = set of AnsiChar;

  { Defines a currency format settings for @link(FormatCurrency). }
  TKCurrencyFormat = record
    CurrencyFormat,
    CurrencyDecimals: Byte;
    CurrencyString: TKString;
    DecimalSep: Char;
    ThousandSep: Char;
    UseThousandSep: Boolean;
  end;

  { @abstract(Declares a structure that holds both column and row span of a cell)
    <UL>
    <LH>Members:</LH>
    <LI><I>ColSpan</I> - column span.</LI>
    <LI><I>RowSpan</I> - row span.</LI>
    </UL> }
  TKCellSpan = record
    ColSpan: Integer;
    RowSpan: Integer;
  end;

  { @abstract(Declares a structure that holds point coordinates as 64-bit wide integers)
    <UL>
    <LH>Members:</LH>
    <LI><I>X</I> - X coord.</LI>
    <LI><I>Y</I> - Y coord.</LI>
    </UL> }
  TKPoint64 = record
    X, Y: Int64;
  end;

  { Pointer }
  PKPoint64 = ^TKPoint64;

  { @abstract(Declares a structure that holds rectangle coordinates as 64-bit wide integers)
    <UL>
    <LH>Members:</LH>
    <LI><I>Left</I> - left coord.</LI>
    <LI><I>Right</I> - right coord.</LI>
    <LI><I>Top</I> - top coord.</LI>
    <LI><I>Bottom</I> - bottom coord.</LI>
    </UL> }
  TKRect64 = record
    Left, Right, Top, Bottom: Int64;
  end;

  { Pointer }
  PKRect64 = ^TKRect64;

  { @abstract(Declares the digit position in a hex string)
    <UL>
    <LH>Members:</LH>
    <LI><I>Index</I> - byte index</LI>
    <LI><I>Digit</I> - digit index</LI>
    </UL>
  }
  TKHexDigitPosition = record
    Index: Int64;
    Digit: Integer;
  end;

  TKLogType = (
    lgNone,
    lgError,
    lgWarning,
    lgNote,
    lgHint,
    lgInfo,
    lgInputError,
    lgIOError,
    lgAll
  );

  TKObjectList = class;

  TKObject = class(TObject)
  private
    FParent: TKObjectList;
    procedure SetParent(const Value: TKObjectList);
  protected
    FUpdateLock: Integer;
    procedure CallBeforeUpdate; virtual;
    procedure CallAfterUpdate; virtual;
    procedure ParentChanged; virtual;
  public
    constructor Create; virtual;
    procedure Assign(ASource: TKObject); virtual;
    function EqualProperties(AValue: TKObject): Boolean; virtual;
    procedure LockUpdate; virtual;
    procedure UnLockUpdate; virtual;
    function UpdateUnlocked: Boolean; virtual;
    property Parent: TKObjectList read FParent write SetParent;
  end;

  TKObjectClass = class of TKObject;

  TKObjectList = class(TObjectList)
  protected
    FUpdateLock: Integer;
    procedure CallBeforeUpdate; virtual;
    procedure CallAfterUpdate; virtual;
  public
    constructor Create; virtual;
    function Add(AObject: TObject): Integer;
    procedure Assign(ASource: TKObjectList); virtual;
    function EqualProperties(AValue: TKObjectList): Boolean; virtual;
    procedure Insert(Index: Integer; AObject: TObject);
    procedure LockUpdate; virtual;
    procedure UnLockUpdate; virtual;
    function UpdateUnlocked: Boolean; virtual;
  end;

  TKPersistent = class(TPersistent)
  private
    FChanged: Boolean;
    FUpdateLock: Integer;
  protected
    { Call in property setters to track changes to this class. }
    procedure Changed;
    { Override to perform requested actions when changing properties in this class.
      Update will be called either immediately when you call Changed or on next
      UnlockUpdate call if Changed has been called while updating was locked. }
    procedure Update; virtual; abstract;
  public
    { Creates the instance. }
    constructor Create; virtual;
    { Locks updating. Use this if you assign many properties at the
      same time. Every LockUpdate call must have a corresponding
      @link(TKPersistent.UnlockUpdate) call, please use a try-finally section. }
    procedure LockUpdate;
    { Unlocks page setup updating and updates the page settings.
      Each @link(TKPersistent.LockUpdate) call must be always followed
      by the UnlockUpdate call. }
    procedure UnlockUpdate(ACallUpdate: Boolean = True);
    { Returns True if updating is not locked, i.e. there is no open
      LockUpdate and UnlockUpdate pair. }
    function UpdateUnlocked: Boolean;
    property UpdateLock: Integer read FUpdateLock;
  end;

{ Replaces possible decimal separators in S with DecimalSeparator variable.}
function AdjustDecimalSeparator(const S: string): string;

{ Converts an AnsiString into a TKString. If CodePage is not set
  the current system code page for ANSI-UTFx translations will be used. }
function AnsiStringToString(const Text: AnsiString; CodePage: Cardinal = 0): TKString;

{$IFNDEF FPC}
function AnsiStringToWideChar(const Text: AnsiString; CodePage: Cardinal = 0): PWideChar;
{$ENDIF}

type
  { Callback for binary search data item comparison. }
  TBsCompareProc = function(Data: Pointer; Index: Integer; KeyPtr: Pointer): Integer;

{ Performs binary search on previously sorted data given by AData and ACount.
  KeyPtr is the pointer to the key which is passed to the ACompareProc.
  The items are compared by CompareProc callback. Returns the zero based index
  of the matched data or -1 if no match has been found. }
function BinarySearch(AData: Pointer; ACount: Integer; KeyPtr: Pointer;
  ACompareProc: TBSCompareProc; ASortedDown: Boolean): Integer;

{ Compiler independent Delphi2009-like CharInSet function for ANSI characters. }
function CharInSetEx(AChar: AnsiChar; const ASet: TKSysCharSet): Boolean; overload;

{ Compiler independent Delphi2009-like CharInSet function for Unicode characters. }
function CharInSetEx(AChar: WideChar; const ASet: TKSysCharSet): Boolean; overload;

{ Compares two Integers. Returns 1 if I1 > I2, -1 if I1 < I2 and 0 if I1 = I2. }
function CompareIntegers(I1, I2: Integer): Integer;

{ Compares two PWideChar strings. Returns 1 if W1 > W2, -1 if W1 < W2 and
  0 if W1 = W2. The strings will be compared using the default user locale
  unless another locale has been specified in Locale. }
function CompareWideChars(W1, W2: PWideChar{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal = LOCALE_USER_DEFAULT{$ENDIF}): Integer;

{$IFDEF STRING_IS_UNICODE}
{ Compares two Unicode strings (Lazarus, Delphi 2009 and better). Returns 1 if S1 > S2,
  -1 if S1 < S2 and 0 if S1 = S2. The strings will be compared using the default
  user locale unless another locale has been specified in Locale. }
function CompareChars(S1, S2: PChar{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal = LOCALE_USER_DEFAULT{$ENDIF}): Integer;
{$ENDIF}

{ Compares two WideString strings. Returns 1 if W1 > W2, -1 if W1 < W2 and
  0 if W1 = W2. The strings will be compared using the default user locale
  unless another locale has been specified in Locale. }
function CompareWideStrings(W1, W2: WideString{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal = LOCALE_USER_DEFAULT{$ENDIF}): Integer;

{$IFDEF STRING_IS_UNICODE}
{ Compares two Unicode strings (Lazarus, Delphi 2009 and better). Returns 1 if S1 > S2,
  -1 if S1 < S2 and 0 if S1 = S2. The strings will be compared using the default
  user locale unless another locale has been specified in Locale. }
function CompareStrings(S1, S2: string{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal = LOCALE_USER_DEFAULT{$ENDIF}): Integer;
{$ENDIF}

{ Converts tab characters in a string to space characters. }
procedure ConvertTabsToSpaces(var AText: TKString; ASpacesForTab: Integer);

{ Creates given directory, even if more folders have to be created. }
function CreateMultipleDir(const Dir: string): Boolean;

{ Converts hexadecimal digit to nibble. }
function DigitToNibble(Digit: AnsiChar; var Nibble: Byte): Boolean;

{ Performs integer division. If there is a nonzero remainder,
  the result will be incremented. }
function DivUp(Dividend, Divisor: Integer): Integer;

{ Performs 64-bit integer division. If there is a nonzero remainder,
  the result will be incremented. }
function DivUp64(Dividend, Divisor: Int64): Int64;

{ Performs integer division. If there is a nonzero remainder,
  the result will be decremented. }
function DivDown(Dividend, Divisor: Integer): Integer;

{ Performs 64-bit integer division. If there is a nonzero remainder,
  the result will be decremented. }
function DivDown64(Dividend, Divisor: Int64): Int64;

{ Ensures the path given by APath has slash at the end. }
procedure EnsureLastPathSlash(var APath: string);

{ Ensures the path given by APath has slash at the end. }
function EnsureLastPathSlashFnc(const APath: string): string;

{ Raises a general exception with associated message Msg. }
procedure Error(const Msg: string);

{ Swaps values of two SmallInt variables. }
procedure Exchange(var Value1, Value2: SmallInt); overload;
{ Swaps values of two ShortInt variables. }
procedure Exchange(var Value1, Value2: ShortInt); overload;
{ Swaps values of two Integer variables. }
procedure Exchange(var Value1, Value2: Integer); overload;
{ Swaps values of two Int64 variables. }
procedure Exchange(var Value1, Value2: Int64); overload;
{ Swaps values of two Byte variables. }
procedure Exchange(var Value1, Value2: Byte); overload;
{ Swaps values of two Word variables. }
procedure Exchange(var Value1, Value2: Word); overload;
{ Swaps values of two Cardinal variables. }
procedure Exchange(var Value1, Value2: Cardinal); overload;
{$IFDEF COMPILER10_UP }
{ Swaps values of two UInt64 variables. }
procedure Exchange(var Value1, Value2: UInt64); overload;
{$ENDIF}
{ Swaps values of two Single variables. }
procedure Exchange(var Value1, Value2: Single); overload;
{ Swaps values of two Double variables. }
procedure Exchange(var Value1, Value2: Double); overload;
{$IFNDEF FPC}
{ Swaps values of two Extended variables. }
procedure Exchange(var Value1, Value2: Extended); overload;
{$ENDIF}
{ Swaps values of two Char variables. }
procedure Exchange(var Value1, Value2: Char); overload;

{ Returns file name without path and extension. }
function ExtractFileRawName(const APath: string): string;

{ Formats the given currency value with to specified parameters. Not thread safe. }
function FormatCurrency(Value: Currency; const AFormat: TKCurrencyFormat): TKString;

{ Returns the module version for given module. Tested under WinX, Linux, OSX. }
function GetAppVersion(const ALibName: string; out MajorVersion, MinorVersion, BuildNumber, RevisionNumber: Word): Boolean;

{ Returns the module version string for given module. Tested under WinX, Linux, OSX. }
function GetAppVersionString(const ALibName, ACodePage, AString: string; out AValue: string): Boolean;

{ Returns number of a specific character in a string. }
function GetCharCount(const AText: TKString; AChar: TKChar): Integer;

{ Returns the standard locale dependent format settings. }
function GetFormatSettings: TFormatSettings;

{ Converts an integer into binary string with custom alignment
  (given by Digits). }
function IntToAscii(Value: Int64; Digits: Integer): string;
{ Converts an integer into binary digit string with custom alignment
  (given by Digits) and suffix. }
function IntToBinStr(Value: Int64; Digits: Integer; const Suffix: string): string;
{ Converts an integer value into BCD number. }
function IntToBCD(Value: Cardinal): Cardinal;
{ Converts a signed integer into decimal digit string with custom alignment
  (given by Digits). }
function IntToDecStr(Value: Int64; Digits: Integer = 0): string;
{ Converts a unsigned integer into decimal digit string with custom alignment
  (given by Digits). }
function UIntToDecStr(Value: UInt64; Digits: Integer = 0): string;
{ Converts an integer into hexadecimal digit string with custom alignment
  (given by Digits), prefix and suffix. Digits represented by alphabetical
  characters can be either in lower or upper case. }
function IntToHexStr(Value: Int64; Digits: Integer; const Prefix, Suffix: string;
  UseLowerCase: Boolean): string;
{ Converts an integer into octal digit string. }
function IntToOctStr(Value: Int64): string;
{ Converts an integer into roman number. }
function IntToRoman(Value: Integer; AUpperCase: Boolean): string;
{ Converts an integer into latin alphabetic numbering. }
function IntToLatin(Value: Integer; AUpperCase: Boolean): string;

{ Calculates an integer power from an integer number. }
function IntPowerInt(Value: Int64; Exponent: Integer): Int64;

{ Converts a binary string into integer with custom alignment (given by Digits). }
function AsciiToInt(S: string; Digits: Integer): Int64;
{ Converts a BCD number into integer value. }
function BCDToInt(Value: Cardinal): Cardinal;
{ Converts a binary digit string into integer with custom alignment
  (given by Digits) and sign of a value represented by the string (given by Signed).
  Code returns either zero for a successful conversion or the position of
  first bad character. }
function BinStrToInt(S: string; Digits: Integer; Signed: Boolean;
  var Code: Integer): Int64;
{ Converts a decimal digit string into integer. Code returns either zero for
  a successful conversion or the position of first bad character. Equals to Val. }
function DecStrToInt(S: string; var Code: Integer): Int64;
{ Converts a hexadecimal digit string into integer with custom alignment
  (given by Digits) and sign of a value represented by the string (given by Signed).
  Code returns either zero for a successful conversion or the position of
  first bad character. }
function HexStrToInt(S: string; Digits: Integer; Signed: Boolean;
  var Code: Integer): Int64;
{ Converts an octal digit string into integer. Code returns either zero for
  a successful conversion or the position of first bad character. }
function OctStrToInt(S: string; var Code: Integer): Int64;

{ Calls SysUtils.Format. }
function KFormat(const Format: string; const Args: array of const;
  const AFormatSettings: TFormatSettings): string; overload;

{ Calls SysUtils.WideFormat. }
function KFormat(const Format: WideString; const Args: array of const;
  const AFormatSettings: TFormatSettings): WideString; overload;

{ Makes a @link(TKCellSpan) record from AColumns and ARows. }
function MakeCellSpan(AColumns, ARows: Integer): TKCellSpan;

{ Returns a clipped ShortInt value so that it lies between Min and Max }
function MinMax(Value, Min, Max: ShortInt): ShortInt; overload;
{ Returns a clipped SmallInt value so that it lies between Min and Max }
function MinMax(Value, Min, Max: SmallInt): SmallInt; overload;
{ Returns a clipped Integer value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Integer): Integer; overload;
{ Returns a clipped Int64 value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Int64): Int64; overload;
{ Returns a clipped Single value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Single): Single; overload;
{ Returns a clipped Double value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Double): Double; overload;
{$IFNDEF FPC}
{ Returns a clipped Extended value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Extended): Extended; overload;
{$ENDIF}

{ Fill the data & size structure. }
function MakeDataSize(AData: Pointer; ASize: Integer): TDataSize;

{ Converts nibble to hexadecimal digit. }
function NibbleToDigit(Nibble: Byte; UpperCase: Boolean): AnsiChar;

type
  { Callback for quicksort data item comparison. }
  TQsCompareProc = function(Data: Pointer; Index1, Index2: Integer): Integer;
  { Callback for quicksort data item exchange. }
  TQsExchangeProc = procedure(Data: Pointer; Index1, Index2: Integer);

{ Sorts Count number of items by means of a non recursive quicksort algorithm.
  The items are compared by CompareProc callback and sorted by
  ExchangeProc callback. }
procedure QuickSortNR(AData: Pointer; ACount: Integer; ACompareProc: TQsCompareProc;
  AExchangeProc: TQsExchangeProc; ASortedDown: Boolean);

{ Sorts Count number of items by means of a recursive quicksort algorithm.
  The items are compared by CompareProc callback and sorted by
  ExchangeProc callback. }
procedure QuickSort(AData: Pointer; ACount: Integer; ACompareProc: TQsCompareProc;
  AExchangeProc: TQsExchangeProc; ASortedDown: Boolean);

{ Add AX and AY to APoint. }
procedure OffsetPoint(var APoint: TPoint; AX, AY: Integer); overload;

{ Add AOffset to APoint. }
procedure OffsetPoint(var APoint: TPoint; const AOffset: TPoint); overload;

{ Normalizes the given input rectangle. }
function NormalizeRect(const ARect: TRect): TRect;

{ Create 64-bit point structure. }
function Point64(AX, AY: Int64): TKPoint64;

{ Convert point structure to 64-bit point structure. }
function PointToPoint64(const APoint: TPoint): TKPoint64;

{ Convert point structure to 64-bit point structure. }
function Point64ToPoint(const APoint: TKPoint64): TPoint;

{ Examines if APoint lies within ARect. }
function Pt64InRect(const ARect: TRect; const APoint: TKPoint64): Boolean;

{ Create 64-bit rectangle structure. }
function Rect64(ALeft, ATop, ARight, ABottom: Int64): TKRect64;

{ Examines if some part of Rect lies within Bounds. }
function RectInRect(Bounds, Rect: TRect): Boolean;

{ Examines if Rect lies fully within Bounds. }
function RectInRectFully(Bounds, Rect: TRect): Boolean;

{ Add AX and AY to ARect. }
procedure OffsetRect(var ARect: TRect; AX, AY: Integer); overload;

{ Add AOffset to ARect. }
procedure OffsetRect(var ARect: TRect; const AOffset: TPoint); overload;

{ Ensures the path given by APath has no slash at the end. }
procedure StripLastPathSlash(var APath: string);

{ Ensures the path given by APath has no slash at the end. }
function StripLastPathSlashFnc(const APath: string): string;

{ Returns next character index for given string and character index.
  Takes MBCS (UTF16 in Delphi and UTF8 in Lazarus) into account. }
function StrNextCharIndex(const AText: TKString; Index: Integer): Integer;

{ Returns previous character index for given string and character index.
  Takes MBCS (UTF16 in Delphi and UTF8 in Lazarus) into account. }
function StrPreviousCharIndex(const AText: TKString; Index: Integer): Integer;

{ Converts byte index to code point index for given string and byte index.
  Takes MBCS (UTF16 in Delphi and UTF8 in Lazarus) into account. }
function StrByteIndexToCPIndex(const AText: TKString; ByteIndex: Integer): Integer;

{ Converts code point index to byte index for given string and code point index.
  Takes MBCS (UTF16 in Delphi and UTF8 in Lazarus) into account. }
function StrCPIndexToByteIndex(const AText: TKString; CPIndex: Integer): Integer;

{ Returns the index for given string where character at given index begins.
  Takes MBCS (UTF16 in Delphi and UTF8 in Lazarus) into account. }
function StringCharBegin(const AText: TKString; Index: Integer): Integer;

{ Returns the number of characters in a string.
  Takes MBCS (UTF16 in Delphi and UTF8 in Lazarus) into account. }
function StringLength(const AText: TKString): Integer;

{ Performs standard Copy operation.
  Takes MBCS (UTF16 in Delphi and UTF8 in Lazarus) into account. }
function StringCopy(const ASource: TKString; At, Count: Integer): TKString;

{ Performs standard Delete operation.
  Takes MBCS (UTF16 in Delphi and UTF8 in Lazarus) into account. }
procedure StringDelete(var ASource: TKString; At, Count: Integer);

{ Trims characters specified by ASet from the beginning and end of AText.
  New text length is returned by ALen. }
procedure TrimWhiteSpaces(const AText: TKString; var AStart, ALen: Integer; const ASet: TKSysCharSet); overload;

{ Trims characters specified by ASet from the beginning and end of AText. }
procedure TrimWhiteSpaces(var AText: TKString; const ASet: TKSysCharSet); overload;

{$IFNDEF FPC}
{ Trims characters specified by ASet from the beginning and end of AText. }
procedure TrimWhiteSpaces(var AText: AnsiString; const ASet: TKSysCharSet); overload;
{$ENDIF}

{ Converts a TKString into AnsiString. If CodePage is not set
  the current system code page for ANSI-UTFx translations will be used. }
function StringToAnsiString(const AText: TKString; CodePage: Cardinal = 0): AnsiString;

function StringToUTF8(const AText: string): AnsiString;

{ Converts specified character of TKString into TKChar. }
function StringToChar(const AText: TKString; AIndex: Integer): TKChar;

{$IFDEF MSWINDOWS}
function GetWindowsFolder(CSIDL: Cardinal; var APath: string): Boolean;

function RunExecutable(const AFileName: string; AWaitForIt: Boolean): DWORD;
{$ENDIF}

function SystemCodePage: Integer;

function NativeUTFToUnicode(const AText: TKString): WideChar;
function UnicodeUpperCase(const AText: TKString): TKString;
function UnicodeLowerCase(const AText: TKString): TKString;
function UnicodeToNativeUTF(const AParam: WideChar): TKString;
function UnicodeStringReplace(const AText, AOldPattern, ANewPattern: TKString;
  AFlags: TReplaceFlags): TKString;

function UTF8ToString(const AText: AnsiString): string;

{ Creates a selection structure from given Index and Digit parameters }
function MakeHexDigitPosition(Index: Int64; Digit: Integer): TKHexDigitPosition;

{ Converts a hexadecimal digit character ('0'..'F') to binary value }
function DigitToBin(Value: AnsiChar): Integer;

{ Examines/converts hexadecimal digit string to binary value string. Returns
  True if the digit string is valid.
  <UL>
  <LH>Parameters:</LH>
  <LI><I>S</I> - hexadecimal digit string (e.g. 'AF01 DC05 3'). White spaces will
  be ignored. When Convert is True, the converted binary value string will be returned
  via this parameter (in this exammple '#A#F#0#1#D#C#0#5#3').</LI>
  <LI><I>Convert</I> - the digit string will be converted if True, otherwise it will
  be examined only.</LI>
  </UL> }
function DigitsToBinStr(var S: AnsiString; Convert: Boolean = True): Boolean;

{ Converts a binary value string into binary data. If the binary value string
  is not divisible by 2, it will be right padded with zero. Example:
  '#A#F#0#1#D#C#0#5#3' is converted into '#AF#01#DC#05#30'. }
function BinStrToBinary(const S: AnsiString): AnsiString;

{ Converts binary value (0..15) to hexadecimal digit character ('0'..'F') }
function BinToDigit(Value: Byte): AnsiChar;

{ Converts binary data into hexadecimal digit string.
  <UL>
  <LH>Parameters:</LH>
  <LI><I>Buffer</I> - binary data - intended for @link(TKCustomHexEditor.Buffer)</LI>
  <LI><I>SelStart, SelEnd</I> - specifies which part of the buffer is about to be
  converted. SelStart.Index must be lower or equal to SelEnd.Index - intended for
  @link(TKCustomHexEditor.GetRealSelStart) and @link(TKCustomHexEditor.GetRealSelEnd).</LI>
  </UL>
  Example: '#AF#01#DC#05#30' is converted into 'AF01DC0530'.
  If AInsertSpaces is True then resulting string is 'AF 01 DC 05 30'. }
function BinaryToDigits(Buffer: PBytes; SelStart, SelEnd: TKHexDigitPosition;
  AInsertSpaces: Boolean = False): AnsiString; overload;

{ Convertes binary data into hexadecimal digit string. Entire data used. }
function BinaryToDigits(Buffer: PBytes; ASize: Int64;
  AInsertSpaces: Boolean = False): AnsiString; overload;

{ Convertes binary data into hexadecimal digit string. Uses AnsiString as source data. }
function BinaryToDigits(const Source: AnsiString;
  AInsertSpaces: Boolean = False): AnsiString; overload;

{ Converts a binary value string into hexadecimal digit string. If the binary value string
  is not divisible by 2, it will be trimmed. Example:
  '#A#F#0#1#D#C#0#5#3' is converted into 'AF01DC05'.
  If AInsertSpaces is True then resulting string is 'AF 01 DC 05'. }
function BinStrToDigits(const Source: AnsiString;
  AInsertSpaces: Boolean = False): AnsiString;

{ Insert spaces into hexadecimal digit string.
  Example: 'AF01DC05' becomes 'AF 01 DC 05'. }
function InsertSpacesToDigits(const Source: AnsiString): AnsiString;

{ Replaces a hexadecimal digit in the given binary value. Returns the original
  value with a replaced digit.
  <UL>
  <LH>Parameters:</LH>
  <LI><I>Value</I> - original binary value</LI>
  <LI><I>Digit</I> - digit value (0..15)</LI>
  <LI><I>Pos</I> - digit position (order)</LI>
  </UL>
  Example: Value = $A18D, Digit = $C, Pos = 3: Result = $AC8D }
function ReplaceDigit(Value, Digit, Pos: Integer): Integer;

implementation

uses
  Math, TypInfo
{$IFDEF FPC}
  , versionresource
{$ENDIF}
{$IFDEF USE_WIDEWINPROCS}
  , KWideWinProcs
{$ENDIF}
{$IFDEF FPC}
  , LConvEncoding
{$ENDIF}
  ;

{ TKObject }

constructor TKObject.Create;
begin
  inherited;
  FParent := nil;
  FUpdateLock := 0;
end;

procedure TKObject.Assign(ASource: TKObject);
begin
end;

procedure TKObject.CallAfterUpdate;
begin
end;

procedure TKObject.CallBeforeUpdate;
begin
end;

function TKObject.EqualProperties(AValue: TKObject): Boolean;
begin
  Result := True;
end;

procedure TKObject.LockUpdate;
begin
  if FUpdateLock <= 0 then
    CallBeforeUpdate;
  Inc(FUpdateLock);
end;

procedure TKObject.ParentChanged;
begin
end;

procedure TKObject.SetParent(const Value: TKObjectList);
begin
  if Value <> FParent then
  begin
    FParent := Value;
    ParentChanged;
  end;
end;

procedure TKObject.UnLockUpdate;
begin
  if FUpdateLock > 0 then
  begin
    Dec(FUpdateLock);
    if FUpdateLock = 0 then
      CallAfterUpdate;
  end;
end;

function TKObject.UpdateUnlocked: Boolean;
begin
  Result := FUpdateLock <= 0;
end;

{ TKObjectList }

constructor TKObjectList.Create;
begin
  inherited;
  FUpdateLock := 0;
end;

function TKObjectList.Add(AObject: TObject): Integer;
begin
  if AObject is TKObject then
    TKObject(AObject).Parent := Self;
  Result := inherited Add(AObject);
end;

procedure TKObjectList.Assign(ASource: TKObjectList);
var
  I: Integer;
  Cls: TKObjectClass;
  SrcItem, DstItem: TKObject;
begin
  if ASource <> nil then
  begin
    Clear;
    for I := 0 to ASource.Count - 1 do
    begin
      SrcItem := ASource.Items[I] as TKObject;
      Cls := TKObjectClass(SrcItem.ClassType);
      DstItem := Cls.Create;
      DstItem.Parent := Self;
      DstItem.Assign(SrcItem);
      Add(DstItem);
    end;
  end;
end;

procedure TKObjectList.CallBeforeUpdate;
begin
end;

procedure TKObjectList.CallAfterUpdate;
begin
end;

function TKObjectList.EqualProperties(AValue: TKObjectList): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AValue <> nil then
  begin
    Result := AValue.Count = Count;
    if Result then
    begin
      for I := 0 to Count - 1 do
        if not TKObject(Items[I]).EqualProperties(TKObject(AValue[I])) then
        begin
          Result := False;
          Break;
        end;
    end;
  end;
end;

procedure TKObjectList.Insert(Index: Integer; AObject: TObject);
begin
  if AObject is TKObject then
    TKObject(AObject).Parent := Self;
  inherited Insert(Index, AObject);
end;

procedure TKObjectList.LockUpdate;
begin
  if FUpdateLock <= 0 then
    CallBeforeUpdate;
  Inc(FUpdateLock);
end;

procedure TKObjectList.UnLockUpdate;
begin
  if FUpdateLock > 0 then
  begin
    Dec(FUpdateLock);
    if FUpdateLock = 0 then
      CallAfterUpdate;
  end;
end;

function TKObjectList.UpdateUnlocked: Boolean;
begin
  Result := FUpdateLock <= 0;
end;

{ TKPersistent }

constructor TKPersistent.Create;
begin
  inherited;
  FUpdateLock := 0;
  FChanged := False;
end;

procedure TKPersistent.Changed;
begin
  if FUpdateLock = 0 then
    Update
  else
    FChanged := True;
end;

procedure TKPersistent.LockUpdate;
begin
  Inc(FUpdateLock);
  FChanged := False;
end;

procedure TKPersistent.UnlockUpdate(ACallUpdate: Boolean);
begin
  if FUpdateLock > 0 then
  begin
    Dec(FUpdateLock);
    if (FUpdateLock = 0) and FChanged and ACallUpdate then
      Update;
  end;
end;

function TKPersistent.UpdateUnlocked: Boolean;
begin
  Result := FUpdateLock = 0;
end;

function AdjustDecimalSeparator(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(Result) do
    if CharInSetEx(Result[I], [',', '.']) then
      Result[I] := GetFormatSettings.DecimalSeparator;
end;

function AnsiStringToString(const Text: AnsiString; CodePage: Cardinal): TKString;
var
{$IFDEF FPC}
  CP: string;
{$ELSE}
  Len: Integer;
{$ENDIF}
begin
{$IFDEF FPC}
  if CodePage = 0 then
    CP := 'ansi'
  else
    CP := Format('cp%d', [Codepage]);
  Result := LConvEncoding.ConvertEncoding(Text, CP, 'utf8');
{$ELSE}
  Len := MultiByteToWideChar(CodePage, 0, PAnsiChar(Text), -1, nil, 0);
  SetLength(Result, Len shr 1);
  MultiByteToWideChar(CodePage, 0, PAnsiChar(Text), -1, PWideChar(Result), Len);
{$ENDIF}
end;

{$IFNDEF FPC}
function AnsiStringToWideChar(const Text: AnsiString; CodePage: Cardinal): PWideChar;
var
  Len: Integer;
begin
  Len := MultiByteToWideChar(CodePage, 0, PAnsiChar(Text), -1, nil, 0);
  GetMem(Result, Len shl 1);
  MultiByteToWideChar(CodePage, 0, PAnsiChar(Text), -1, Result, Len);
end;
{$ENDIF}

function BinarySearch(AData: Pointer; ACount: Integer; KeyPtr: Pointer;
  ACompareProc: TBsCompareProc; ASortedDown: Boolean): Integer;
var
  Lo, Hi, Index, Ret: Integer;
begin
  Result := -1;
  Lo := 0;
  Hi := ACount - 1;
  repeat
    Index := (Lo + Hi) div 2;
    Ret := ACompareProc(AData, Index, KeyPtr);
    if ASortedDown and (Ret < 0) or not ASortedDown and (Ret > 0) then
      Hi := Index - 1
    else
      Lo := Index + 1
  until (Lo > Hi) or (Ret = 0);
  if Ret = 0 then
    Result := Index;
end;

function CharInSetEx(AChar: AnsiChar; const ASet: TKSysCharSet): Boolean;
begin
  Result := AChar in ASet;
end;

function CharInSetEx(AChar: WideChar; const ASet: TKSysCharSet): Boolean;
begin
  Result := (Ord(AChar) < $100) and
  {$IFDEF COMPILER12_UP}
    CharInSet(AChar, ASet);
  {$ELSE}
    (AnsiChar(AChar) in ASet);
  {$ENDIF}
end;

function CompareIntegers(I1, I2: Integer): Integer;
begin
  if I1 > I2 then Result := 1
  else if I1 < I2 then Result := -1
  else Result := 0;
end;

function CompareWideChars(W1, W2: PWideChar{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal{$ENDIF}): Integer;
begin
  if (W1 = nil) or (W2 = nil) then
  begin
    if W1 <> nil then Result := 1
    else if W2 <> nil then Result := -1
    else Result := 0;
  end else
  begin
  {$IFDEF USE_WIDEWINPROCS}
    Result := WideWinProcs.CompareString(Locale, 0, W1, -1, W2, -1);
    Dec(Result, 2);
  {$ELSE}
    Result := WideCompareStr(WideString(W1), WideString(W2));
  {$ENDIF}
  end;
end;

{$IFDEF STRING_IS_UNICODE}
function CompareChars(S1, S2: PChar{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal{$ENDIF}): Integer;
begin
  if (S1 = nil) or (S2 = nil) then
  begin
    if S1 <> nil then Result := 1
    else if S2 <> nil then Result := -1
    else Result := 0;
  end else
  begin
  {$IFDEF USE_WIDEWINPROCS}
    Result := WideWinProcs.CompareString(Locale, 0, PWideChar(S1), -1, PWideChar(S2), -1);
    Dec(Result, 2);
  {$ELSE}
    Result := CompareStr(string(S1), string(S2));
  {$ENDIF}
  end;
end;
{$ENDIF}

function CompareWideStrings(W1, W2: WideString{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal{$ENDIF}): Integer;
begin
{$IFDEF USE_WIDEWINPROCS}
  Result := WideWinProcs.CompareString(Locale, 0, PWideChar(W1), -1, PWideChar(W2), -1);
  Dec(Result, 2);
{$ELSE}
  Result := WideCompareStr(W1, W2);
{$ENDIF}
end;

{$IFDEF STRING_IS_UNICODE}
function CompareStrings(S1, S2: string{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal{$ENDIF}): Integer;
begin
{$IFDEF USE_WIDEWINPROCS}
  Result := WideWinProcs.CompareString(Locale, 0, PWideChar(S1), -1, PWideChar(S2), -1);
  Dec(Result, 2);
{$ELSE}
  Result := CompareStr(S1, S2);
{$ENDIF}
end;
{$ENDIF}

procedure ConvertTabsToSpaces(var AText: TKString; ASpacesForTab: Integer);
var
  TabCount: Integer;
  S: TKString;
  I, J, K: Integer;
begin
  if ASpacesForTab >= 0 then
  begin
    TabCount := GetCharCount(AText, cTAB);
    if TabCount > 0 then
    begin
      SetLength(S, Length(AText) + (ASpacesForTab - 1) * TabCount);
      J := 1;
      for I := 1 to Length(AText) do
      begin
        if AText[I] = cTAB then
        begin
          for K := 0 to ASpacesForTab - 1 do
          begin
            S[J] := cSPACE;
            Inc(J);
          end;
        end else
        begin
          S[J] := AText[I];
          Inc(J);
        end;
      end;
      AText := S;
    end;
  end;
end;

function CreateMultipleDir(const Dir: string): Boolean;
var
  I: Integer;
  T: string;
begin
  for I := 1 to Length(Dir) do
  begin
    if CharInSetEx(Dir[I], ['/', '\']) then
    begin
      T := Copy(Dir, 1, I - 1);
      if not (DirectoryExists(T) or CreateDir(T)) then Break;
    end;
  end;
  if not DirectoryExists(Dir) then
    CreateDir(Dir);
  Result := DirectoryExists(Dir);
end;

function DigitToNibble(Digit: AnsiChar; var Nibble: Byte): Boolean;
begin
  Result := True;
  if (Digit >= '0') and (Digit <= '9') then
    Nibble := Ord(Digit) - Ord('0')
  else if (Digit >= 'a') and (Digit <= 'f') then
    Nibble := Ord(Digit) - Ord('a') + 10
  else if (Digit >= 'A') and (Digit <= 'F') then
    Nibble := Ord(Digit) - Ord('A') + 10
  else
    Result := False;
end;

function DivUp(Dividend, Divisor: Integer): Integer;
begin
  if Divisor = 0 then
    Result := 0
  else if Dividend mod Divisor > 0 then
    Result := Dividend div Divisor + 1
  else
    Result := Dividend div Divisor;
end;

function DivUp64(Dividend, Divisor: Int64): Int64;
begin
  if Divisor = 0 then
    Result := 0
  else if Dividend mod Divisor > 0 then
    Result := Dividend div Divisor + 1
  else
    Result := Dividend div Divisor;
end;

function DivDown(Dividend, Divisor: Integer): Integer;
begin
  if Divisor = 0 then
    Result := 0
  else if Dividend mod Divisor < 0 then
    Result := Dividend div Divisor - 1
  else
    Result := Dividend div Divisor;
end;

function DivDown64(Dividend, Divisor: Int64): Int64;
begin
  if Divisor = 0 then
    Result := 0
  else if Dividend mod Divisor < 0 then
    Result := Dividend div Divisor - 1
  else
    Result := Dividend div Divisor;
end;

procedure EnsureLastPathSlash(var APath: string);
begin
  if APath <> '' then
    if not CharInSetEx(APath[Length(APath)], ['\', '/']) then APath := APath + cDirectoryDelimiter;
end;

function EnsureLastPathSlashFnc(const APath: string): string;
begin
  Result := APath;
  EnsureLastPathSlash(Result);
end;

procedure Exchange(var Value1, Value2: ShortInt);
var
  Tmp: ShortInt;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: SmallInt);
var
  Tmp: SmallInt;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Integer);
var
  Tmp: Integer;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Int64);
var
  Tmp: Int64;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Byte);
var
  Tmp: Byte;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Word);
var
  Tmp: Word;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Cardinal);
var
  Tmp: Cardinal;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

{$IFDEF COMPILER10_UP }
procedure Exchange(var Value1, Value2: UINT64);
var
  Tmp: UINT64;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;
{$ENDIF}

procedure Exchange(var Value1, Value2: Single);
var
  Tmp: Single;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Double);
var
  Tmp: Double;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

{$IFNDEF FPC}
procedure Exchange(var Value1, Value2: Extended);
var
  Tmp: Extended;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;
{$ENDIF}

procedure Exchange(var Value1, Value2: Char);
var
  Tmp: Char;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

function ExtractFileRawName(const APath: string): string;
var
  I: Integer;
begin
  Result := ExtractFileName(APath);
  I := Length(Result);
  while I > 1 do
  begin
    if Result[I] = '.' then
    begin
      SetLength(Result, I - 1);
      Break;
    end;
    Dec(I);
  end;
end;

procedure Error(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

function FormatCurrency(Value: Currency; const AFormat: TKCurrencyFormat): TKString;
var
  Fmt: string;
  FS: TFormatSettings;
begin
  if AFormat.UseThousandSep then
  begin
    FS.ThousandSeparator := AFormat.ThousandSep;
    Fmt := '%.*n';
  end else
    Fmt := '%.*f';
  FS.DecimalSeparator := AFormat.DecimalSep;
  case AFormat.CurrencyFormat of
    0: Result := KFormat('%s' + Fmt, [AFormat.CurrencyString, AFormat.CurrencyDecimals, Value], FS);
    1: Result := KFormat(Fmt + '%s', [AFormat.CurrencyDecimals, Value, AFormat.CurrencyString], FS);
    2: Result := KFormat('%s ' + Fmt, [AFormat.CurrencyString, AFormat.CurrencyDecimals, Value], FS);
  else
    Result := KFormat(Fmt + ' %s', [AFormat.CurrencyDecimals, Value, AFormat.CurrencyString], FS);
  end;
end;

function GetAppVersion(const ALibName: string; out MajorVersion, MinorVersion, BuildNumber, RevisionNumber: Word): Boolean;
var
{$IFDEF FPC}
  Info: TVersionResource;
  Stream: TResourceStream;
  ResID: Integer;
  Res: TFPResourceHandle;
{$ELSE}
  dwHandle, dwLen: DWORD;
  BufLen: Cardinal;
  lpData: LPTSTR;
  pFileInfo: ^VS_FIXEDFILEINFO;
{$ENDIF}
begin
  Result := False;
{$IFDEF FPC}
  Info := TVersionResource.Create;
  try
    ResID := 1;
    // Defensive code to prevent failure if no resource available...
    Res := FindResource(HInstance, PChar(PtrInt(ResID)), PChar(RT_VERSION));
    If Res = 0 Then
      Exit;

    Stream := TResourceStream.CreateFromID(HInstance, ResID, {$IFDEF LCLWinCE}PWideChar{$ELSE}PChar{$ENDIF}(RT_VERSION));
    Try
      Info.SetCustomRawDataStream(Stream);
      MajorVersion := Info.FixedInfo.FileVersion[0];
      MinorVersion := Info.FixedInfo.FileVersion[1];
      BuildNumber := Info.FixedInfo.FileVersion[2];
      RevisionNumber := Info.FixedInfo.FileVersion[3];
      Info.SetCustomRawDataStream(nil);
    Finally
      Stream.Free;
    End;
    Result := True;
  finally
    Info.Free;
  end;
{$ELSE}
  dwLen := GetFileVersionInfoSize(PChar(ALibName), dwHandle);
  if dwLen <> 0 then
  begin
    GetMem(lpData, dwLen);
    try
      if GetFileVersionInfo(PChar(ALibName), dwHandle, dwLen, lpData) then
      begin
        if VerQueryValue(lpData, '\\', Pointer(pFileInfo), BufLen) then
        begin
          MajorVersion := HIWORD(pFileInfo.dwFileVersionMS);
          MinorVersion := LOWORD(pFileInfo.dwFileVersionMS);
          BuildNumber := HIWORD(pFileInfo.dwFileVersionLS);
          RevisionNumber := LOWORD(pFileInfo.dwFileVersionLS);
          Result := True;
        end;
      end;
    finally
      FreeMem(lpData);
    end;
  end;
{$ENDIF}
end;

function GetAppVersionString(const ALibName, ACodePage, AString: string; out AValue: string): Boolean;
{$IFnDEF FPC}
type
  TTranslation = packed record
    wLanguage: Word;
    wCodePage: Word;
  end;
{$ENDIF}
var
{$IFDEF FPC}
  Info: TVersionResource;
  Stream: TResourceStream;
  ResID: Integer;
  Res: TFPResourceHandle;
{$ELSE}
  dwHandle, dwLen: DWORD;
  BufLen: Cardinal;
  lpData: LPTSTR;
  pValue: Pointer;
  Translation: ^TTranslation;
  CP: string;
  Tmp: WideString;
{$ENDIF}
begin
  Result := False;
{$IFDEF FPC}
  Info := TVersionResource.Create;
  try
    ResID := 1;
    // Defensive code to prevent failure if no resource available...
    Res := FindResource(HInstance, PChar(PtrInt(ResID)), PChar(RT_VERSION));
    If Res = 0 Then
      Exit;

    Stream := TResourceStream.CreateFromID(HInstance, ResID, {$IFDEF LCLWinCE}PWideChar{$ELSE}PChar{$ENDIF}(RT_VERSION));
    Try
      Info.SetCustomRawDataStream(Stream);
      if Info.StringFileInfo.Count > 0 then
      begin
        AValue := Info.StringFileInfo.Items[0].Values[AString];
        Result := True;
      end;
      Info.SetCustomRawDataStream(nil);
    Finally
      Stream.Free;
    End;
  finally
    Info.Free;
  end;
{$ELSE}
  dwLen := GetFileVersionInfoSize(PChar(ALibName), dwHandle);
  if dwLen <> 0 then
  begin
    GetMem(lpData, dwLen);
    try
      if GetFileVersionInfo(PChar(ALibName), dwHandle, dwLen, lpData) then
      begin
        if ACodePage = '' then
        begin
          // retrieve first codepage available
          if VerQueryValue(lpData, '\VarFileInfo\Translation', Pointer(Translation), BufLen) and (BufLen >= 4) then
            CP := Format('%4.4x%4.4x', [Translation.wLanguage, Translation.wCodePage])
          else
            CP := ''; // error!
        end else
          CP := ACodePage;
        if (CP <> '') and (VerQueryValue(lpData, PChar(Format('\StringFileInfo\%s\%s', [CP, AString])), pValue, BufLen)) then
        begin
          SetString(Tmp, PChar(pValue), BufLen);
          AValue := string(Tmp);
          Result := True;
        end;
      end;
    finally
      FreeMem(lpData);
    end;
  end;
{$ENDIF}
end;

function GetCharCount(const AText: TKString; AChar: TKChar): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(AText) do
    if AText[I] = AChar then
      Inc(Result);
end;

function GetFormatSettings: TFormatSettings;
begin
{$IFDEF FPC}
  Result := FormatSettings;
{$ELSE}
 {$IFDEF COMPILER15_UP}
  Result := TFormatSettings.Create;
 {$ELSE}
  GetLocaleFormatSettings(GetThreadLocale, Result);
 {$ENDIF}
{$ENDIF}
end;

function IntToAscii(Value: Int64; Digits: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := 0;
  while I < Digits do
  begin
    Result := Result + Chr(Value and $FF);
    Value := Value shr 8;
    Inc(I);
  end;
end;

function IntToBCD(Value: Cardinal): Cardinal;
var
  Exp: Cardinal;
begin
  Result := 0;
  Exp := 1;
  while (Value > 0) and (Exp > 0) do
  begin
    Result := Result + Value mod 10 * Exp;
    Value := Value div 10;
    Exp := Exp * 16;
  end;
end;

function IntToBinStr(Value: Int64; Digits: Integer; const Suffix: string): string;
var
  B: Byte;
  C: Char;
begin
  Result := '';
  if Digits <> 0 then
    Digits := MinMax(Digits, 1, 64);
  repeat
    B := Byte(Value and $1);
    Value := Value shr 1;
    C := Chr(Ord('0') + B);
    Result := C + Result;
  until (Value = 0) or ((Digits <> 0) and (Length(Result) = Digits));
  while Length(Result) < Digits do
    Result := '0' + Result;
  Result := Result + Suffix;
end;

function IntToDecStr(Value: Int64; Digits: Integer): string;
var
  B: Byte;
  C: Char;
  Signum: Boolean;
begin
  Result := '';
  Signum := Value < 0;
  {if Signum  then
  asm
    nop
  end;}
  repeat
    B := Byte(Value mod 10);
    if Signum then
      B := 256 - B;
    Value := Value div 10;
    C := Chr(Ord('0') + B);
    Result := C + Result;
  until Value = 0;
  while Length(Result) < Digits do
    Result := '0' + Result;
  if Signum then
    Result := '-' + Result;
end;

function UIntToDecStr(Value: UInt64; Digits: Integer): string;
var
  B: Byte;
  C: Char;
begin
  Result := '';
  repeat
    B := Byte(Value mod 10);
    Value := Value div 10;
    C := Chr(Ord('0') + B);
    Result := C + Result;
  until Value = 0;
  while Length(Result) < Digits do
    Result := '0' + Result;
end;

function IntToHexStr(Value: Int64; Digits: Integer; const Prefix, Suffix: string; UseLowerCase: Boolean): string;
var
  B: Byte;
begin
  Result := '';
  if Digits <> 0 then
    Digits := MinMax(Digits, 1, 16);
  repeat
    B := Byte(Value and $F);
    Value := Value shr 4;
    Result := Char(NibbleToDigit(B, not UseLowerCase)) + Result;
  until (Value = 0) or ((Digits <> 0) and (Length(Result) = Digits));
  while Length(Result) < Digits do
    Result := '0' + Result;
  Result := Prefix + Result + Suffix;
end;

function IntToOctStr(Value: Int64): string;
var
  B: Byte;
  C: Char;
  Signum: Boolean;
begin
  if Value < 0 then
  begin
    Signum := True;
    Value := -Value;
  end else
    Signum := False;
  Result := '';
  repeat
    B := Byte(Value mod 8);
    Value := Value div 8;
    C := Chr(Ord('0') + B);
    Result := C + Result;
  until Value = 0;
  Result := '0' + Result;
  if Signum then
    Result := '-' + Result;
end;

function IntToRoman(Value: Integer; AUpperCase: Boolean): string;
begin
  Result := '';
  while Value >= 1000 do begin
    Result := Result + 'M';
    Value := Value - 1000;
  end; { while }

  if Value >= 900 then begin
    Result := Result + 'CM';
    Value := Value - 900;
  end; { if }

  while Value >= 500 do begin
    Result := Result + 'D';
    Value := Value - 500;
  end; { while }

  if Value >= 400 then begin
    Result := Result + 'CD';
    Value := Value - 400;
  end; { if }

  while Value >= 100 do begin
    Result := Result + 'C';
    Value := Value - 100;
  end; { while }

  if Value >= 90 then begin
    Result := Result + 'XC';
    Value := Value - 90;
  end; { if }

  while Value >= 50 do begin
    Result := Result + 'L';
    Value := Value - 50;
  end; { while }

  if Value >= 40 then begin
    Result := Result + 'XL';
    Value := Value - 40;
  end; { while }

  while Value >= 10 do begin
    Result := Result + 'X';
    Value := Value - 10;
  end; { while }

  if Value >= 9 then begin
    Result := Result + 'IX';
    Value := Value - 9;
  end; { if }

  while Value >= 5 do begin
    Result := Result + 'V';
    Value := Value - 5;
  end; { while }

  if Value >= 4 then begin
    Result := Result + 'IV';
    Value := Value - 4;
  end; { if }

  while Value > 0 do begin
    Result := Result + 'I';
    Dec(Value);
  end; { while }
  if not AUpperCase then
    Result := LowerCase(Result);
end;

function IntToLatin(Value: Integer; AUpperCase: Boolean): string;
var
  OrdA: Integer;
begin
  Result := '';
  if AUpperCase then
    OrdA := Ord('A')
  else
    OrdA := Ord('a');
  while Value > 0 do
  begin
    Result := Chr(Value mod 26 + OrdA - 1) + Result;
    Value := Value div 26;
  end;
end;

function IntPowerInt(Value: Int64; Exponent: Integer): Int64;
begin
  Result := Value;
  while Exponent > 1 do
  begin
    Result := Result * Value;
    Dec(Exponent);
  end;
end;

function AsciiToInt(S: string; Digits: Integer): Int64;
var
  I: Integer;
begin
  Result := 0;
  I := Min(Length(S), Digits);
  while I > 0 do
  begin
    Result := Result shl 8;
    Result := Ord(S[I]) + Result;
    Dec(I);
  end;
end;

function BCDToInt(Value: Cardinal): Cardinal;
var
  Exp: Cardinal;
begin
  Result := 0;
  Exp := 1;
  while Value > 0 do
  begin
    Result := Result + Min(Value and 15, 9) * Exp;
    Value := Value shr 4;
    Exp := Exp * 10;
  end;
end;

function BinStrToInt(S: string; Digits: Integer; Signed: Boolean; var Code: Integer): Int64;
var
  I, L, Len: Integer;
  N: Byte;
  C: Char;
  M: Int64;
begin
  Result := 0;
  Code := 0;
  L := 0;
  Len := Length(S);
  if (Digits = 0) or (Digits > 64) then
    Digits := 64;
  if (Len >= 1) and CharInSetEx(S[Len], ['b', 'B']) then
  begin
    Delete(S, Len, 1);
    Dec(Len);
  end;
  I := 1;
  while I <= Len do
  begin
    C := S[I];
    N := 255;
    if (C >= '0') and (C <= '1') then N := Ord(C) - Ord('0');
    if N > 1 then
    begin
      Code := I;
      Break;
    end
    else if (N > 0) or (Result <> 0) then
    begin
      if L >= Digits then
      begin
        Code := I;
        Break;
      end;
      Result := Result shl 1;
      Inc(Result, N);
      Inc(L);
    end;
    Inc(I);
  end;
  if Signed and (Digits < 64) then
  begin
    M := Int64(1) shl Digits;
    if Result >= M shr 1 - 1 then
      Dec(Result, M);
  end;
end;

function DecStrToInt(S: string; var Code: Integer): Int64;
var
  I, Len: Integer;
  N: Byte;
  C: Char;
  Minus: Boolean;
begin
  Result := 0;
  Code := 0;
  Len := Length(S);
  Minus := S[1] = '-';
  if Minus then I := 2 else I := 1;
  while I <= Len do
  begin
    C := S[I];
    N := 255;
    if (C >= '0') and (C <= '9') then N := Ord(C) - Ord('0');
    if N > 9 then
    begin
      Code := I;
      Break;
    end
    else if (N > 0) or (Result <> 0) then
    begin
      Result := Result * 10;
      Inc(Result, N);
    end;
    Inc(I);
  end;
  if Minus then Result := -Result;
end;

function HexStrToInt(S: string; Digits: Integer; Signed: Boolean; var Code: Integer): Int64;
var
  I, L, Len: Integer;
  N: Byte;
  C: AnsiChar;
  M: Int64;
begin
  Result := 0;
  Code := 0;
  L := 0;
  Len := Length(S);
  if (Digits = 0) or (Digits > 16) then
    Digits := 16;
  if (Len >= 2) and (AnsiChar(S[1]) = '0') and CharInSetEx(S[2], ['x', 'X']) then
    I := 3
  else if (Len >= 1) and CharInSetEx(S[1], ['x', 'X', '$']) then
    I := 2
  else
    I := 1;
  while I <= Len do
  begin
    C := AnsiChar(S[I]);
    N := 255;
    DigitToNibble(C, N);
    if N > 15 then
    begin
      if CharInSetEx(C, ['h', 'H']) then
      begin
        if Len > I then Code := I + 1;
      end else
        Code := I;
      Break;
    end
    else if (N > 0) or (Result <> 0) then
    begin
      if L >= Digits then
      begin
        Code := I;
        Break;
      end;
      Result := Result shl 4;
      Inc(Result, N);
      Inc(L);
    end;
    Inc(I);
  end;
  if Signed and (Digits < 16) then
  begin
    M := Int64(1) shl (Digits shl 2);
    if Result >= M shr 1 - 1 then
      Dec(Result, M);
  end;
end;

function OctStrToInt(S: string; var Code: Integer): Int64;
var
  I, Len: Integer;
  N: Byte;
  C: Char;
  Minus: Boolean;
begin
  Result := 0;
  Code := 0;
  Len := Length(S);
  Minus := S[1] = '-';
  if Minus then I := 2 else I := 1;
  while I <= Len do
  begin
    C := S[I];
    N := 255;
    if (C >= '0') and (C <= '7') then N := Ord(C) - Ord('0');
    if N > 7 then
    begin
      Code := I;
      Break;
    end
    else if (N > 0) or (Result <> 0) then
    begin
      Result := Result * 8;
      Inc(Result, N);
    end;
    Inc(I);
  end;
  if Minus then Result := -Result;
end;

function KFormat(const Format: string; const Args: array of const; const AFormatSettings: TFormatSettings): string;
begin
  Result := SysUtils.Format(Format, Args, AFormatSettings);
end;

function KFormat(const Format: WideString; const Args: array of const; const AFormatSettings: TFormatSettings): WideString;
begin
  Result := SysUtils.WideFormat(Format, Args, AFormatSettings);
end;

function MakeCellSpan(AColumns, ARows: Integer): TKCellSpan;
begin
  Result.ColSpan := AColumns;
  Result.RowSpan := ARows;
end;

function MinMax(Value, Min, Max: ShortInt): ShortInt;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: SmallInt): SmallInt;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: Integer): Integer;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: Int64): Int64;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: Single): Single;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: Double): Double;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

{$IFNDEF FPC}
function MinMax(Value, Min, Max: Extended): Extended;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;
{$ENDIF}

function MakeDataSize(AData: Pointer; ASize: Integer): TDataSize;
begin
  Result.Data := AData;
  Result.Size := ASize;
end;

function NibbleToDigit(Nibble: Byte; UpperCase: Boolean): AnsiChar;
begin
  if Nibble < 10 then
    Result := AnsiChar(Ord('0') + Nibble)
  else if UpperCase then
    Result := AnsiChar(Ord('A') + Nibble - 10)
  else
    Result := AnsiChar(Ord('a') + Nibble - 10);
end;

procedure QuickSortNR(AData: Pointer; ACount: Integer; ACompareProc: TQsCompareProc;
  AExchangeProc: TQsExchangeProc; ASortedDown: Boolean);
type
  TStackItem = record
    LIndex, RIndex: Integer;
  end;
const
  cStackGrow = 100;
var
  Key, Left, Right, L, R, LBack, RBack, StackLen, StackPtr: Integer;
  Stack: array of TStackItem;
begin
  { this is the non recursive quick sort algorithm to avoid stack overflows.
    Right parts of divided arrays are stored into a stack-like array
    in dynamic memory for later use. }
  Left := 0;
  Right := ACount - 1;
  SetLength(Stack, cStackGrow);
  StackPtr := 0;
  with Stack[StackPtr] do begin LIndex := Left; RIndex := Right end;
  repeat
    with Stack[StackPtr] do begin Left := LIndex; Right := RIndex end;
    Dec(StackPtr);
    repeat
      L := Left;
      R := Right;
      Key := (L + R) div 2;
      LBack := Left - 1;
      RBack := Right;
      repeat
        if ASortedDown then
        begin
          while (L < Right) and (ACompareProc(AData, L, Key) < 0) do Inc(L);
          while (R > Left) and (ACompareProc(AData, R, Key) > 0) do Dec(R);
        end else
        begin
          while (L < Right) and (ACompareProc(AData, L, Key) > 0) do Inc(L);
          while (R > Left) and (ACompareProc(AData, R, Key) < 0) do Dec(R);
        end;
        if L <= R then
        begin
          if L < R then
            if (L = Key) or (R = Key) then
            begin
              // preserve Key, exchange later
              LBack := L;
              RBack := R;
            end else
              AExchangeProc(AData, L, R);
          Dec(R);
          Inc(L);
        end;
      until L >= R;
      // exchange anything with former Key
      if LBack >= Left then
        AExchangeProc(AData, LBack, RBack);
      if L < Right then
      begin
        Inc(StackPtr);
        StackLen := Length(Stack);
        if StackPtr >= StackLen then
          SetLength(Stack, StackLen + cStackGrow);
        with Stack[StackPtr] do begin LIndex := L; RIndex := Right end;
      end;
      Right := R;
    until Left >= Right;
  until StackPtr < 0;
end;

procedure QuickSort(AData: Pointer; ACount: Integer; ACompareProc: TQsCompareProc;
  AExchangeProc: TQsExchangeProc; ASortedDown: Boolean);

  procedure Sort(const Left, Right: Integer);
  var
    Key, L, R, LBack, RBack: Integer;
  begin
    Key := (Left + Right) div 2;
    L := Left;
    R := Right;
    LBack := Left - 1;
    RBack := Right;
    repeat
      if ASortedDown then
      begin
        while (L < Right) and (ACompareProc(AData, L, Key) < 0) do Inc(L);
        while (R > Left) and (ACompareProc(AData, R, Key) > 0) do Dec(R);
      end else
      begin
        while (L < Right) and (ACompareProc(AData, L, Key) > 0) do Inc(L);
        while (R > Left) and (ACompareProc(AData, R, Key) < 0) do Dec(R);
      end;
      if L <= R then
      begin
        if L < R then
          if (L = Key) or (R = Key) then
          begin
            // preserve Key, exchange later
            LBack := L;
            RBack := R;
          end else
            AExchangeProc(AData, L, R);
        Inc(L);
        Dec(R);
      end;
    until L >= R;
    // exchange anything with former Key
    if LBack >= Left then
      AExchangeProc(AData, LBack, RBack);
    if Left < R  then Sort(Left, R);
    if L < Right then Sort(L, Right);
  end;

begin
  if ACount > 1 then
    Sort(0, ACount - 1);
end;

procedure OffsetPoint(var APoint: TPoint; AX, AY: Integer);
begin
  Inc(APoint.X, AX);
  Inc(APoint.Y, AY);
end;

procedure OffsetPoint(var APoint: TPoint; const AOffset: TPoint);
begin
  Inc(APoint.X, AOffset.X);
  Inc(APoint.Y, AOffset.Y);
end;

function NormalizeRect(const ARect: TRect): TRect;
begin
  Result := ARect;
  if Result.Left > Result.Right then
    Exchange(Result.Left, Result.Right);
  if Result.Top > Result.Bottom then
    Exchange(Result.Top, Result.Bottom);
end;

function Point64(AX, AY: Int64): TKPoint64;
begin
  Result.X:= AX;
  Result.Y:= AY;
end;

function PointToPoint64(const APoint: TPoint): TKPoint64;
begin
  Result.X:= APoint.x;
  Result.Y:= APoint.y;
end;

function Point64ToPoint(const APoint: TKPoint64): TPoint;
begin
  Result.X:= Integer(APoint.X);
  Result.Y:= Integer(APoint.Y);
end;

function Pt64InRect(const ARect: TRect; const APoint: TKPoint64): Boolean;
begin
  Result :=
    (APoint.X >= ARect.Left) and (APoint.X < ARect.Right) and
    (APoint.Y >= ARect.Top) and (APoint.Y < ARect.Bottom);
end;

function Rect64(ALeft, ATop, ARight, ABottom: Int64): TKRect64;
begin
  Result.Left:= ALeft;
  Result.Top:= ATop;
  Result.Right:= ARight;
  Result.Bottom:= ABottom;
end;

function RectInRect(Bounds, Rect: TRect): Boolean;
begin
  Result :=
    (Rect.Left < Bounds.Right) and (Rect.Right >= Bounds.Left) and
    (Rect.Top < Bounds.Bottom) and (Rect.Bottom >= Bounds.Top);
end;

function RectInRectFully(Bounds, Rect: TRect): Boolean;
begin
  Result :=
    (Rect.Left >= Bounds.Left) and (Rect.Right <= Bounds.Right) and
    (Rect.Top >= Bounds.Top) and (Rect.Bottom <= Bounds.Bottom);
end;

procedure OffsetRect(var ARect: TRect; AX, AY: Integer);
begin
  Inc(ARect.Left, AX);
  Inc(ARect.Top, AY);
  Inc(ARect.Right, AX);
  Inc(ARect.Bottom, AY);
end;

procedure OffsetRect(var ARect: TRect; const AOffset: TPoint);
begin
  Inc(ARect.Left, AOffset.X);
  Inc(ARect.Top, AOffset.Y);
  Inc(ARect.Right, AOffset.X);
  Inc(ARect.Bottom, AOffset.Y);
end;

procedure StripLastPathSlash(var APath: string);
begin
  if APath <> '' then
    if CharInSetEx(APath[Length(APath)], ['\', '/']) then Delete(APath, Length(APath), 1);
end;

function StripLastPathSlashFnc(const APath: string): string;
begin
  Result := APath;
  StripLastPathSlash(Result);
end;

function StrNextCharIndex(const AText: TKString; Index: Integer): Integer;
begin
{$IFDEF FPC}
  Result := Index + LazUTF8.UTF8CharacterLength(@AText[Index]);
{$ELSE}
  if (Word(AText[Index]) >= cUTF16FirstSurrogateBegin) and (Word(AText[Index]) <= cUTF16FirstSurrogateEnd) then
    Result := Index + 2
  else
    Result := Index + 1;
{$ENDIF}
end;

function StrPreviousCharIndex(const AText: TKString; Index: Integer): Integer;
begin
{$IFDEF FPC}
  Result := Index - LazUTF8.UTF8CharacterLength(@AText[StringCharBegin(AText, Index - 1)]);
{$ELSE}
  if (Word(AText[Index - 1]) >= cUTF16SecondSurrogateBegin) and (Word(AText[Index - 1]) <= cUTF16SecondSurrogateEnd) then
    Result := Index - 2
  else
    Result := Index - 1;
{$ENDIF}
end;

function StrByteIndexToCPIndex(const AText: TKString; ByteIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  I := 1;
  while I < ByteIndex do
  begin
  {$IFDEF FPC}
    Inc(I, LazUTF8.UTF8CharacterLength(@AText[I]));
  {$ELSE}
    if (Word(AText[I]) >= cUTF16FirstSurrogateBegin) or (Word(AText[I]) > cUTF16FirstSurrogateEnd) then
      Inc(I, 2)
    else
      Inc(I);
  {$ENDIF}
    Inc(Result);
  end;
end;

function StrCPIndexToByteIndex(const AText: TKString; CPIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to CPIndex do
  begin
{$IFDEF FPC}
    Inc(Result, LazUTF8.UTF8CharacterLength(@AText[Result]));
{$ELSE}
    if (Word(AText[Result]) >= cUTF16FirstSurrogateBegin) and (Word(AText[Result]) <= cUTF16FirstSurrogateEnd) then
      Inc(Result, 2)
    else
      Inc(Result);
{$ENDIF}
    if Result > Length(AText) then
      Break;
  end;
end;

function StringCharBegin(const AText: TKString; Index: Integer): Integer;
begin
{$IFDEF FPC}
  Result := LazUTF8.UTF8CharToByteIndex(PChar(AText), Length(AText), Index)
{$ELSE}
  if (Word(AText[Index - 1]) >= cUTF16SecondSurrogateBegin) and (Word(AText[Index - 1]) <= cUTF16SecondSurrogateEnd) then
    Result := Index - 1
  else
    Result := Index
{$ENDIF}
end;

function StringLength(const AText: TKString): Integer;
var
  I: Integer;
begin
{$IFDEF FPC}
  Result := LazUTF8.UTF8Length(AText)
{$ELSE}
  Result := 0;
  for I := 1 to Length(AText) do
    if (Word(AText[I]) < cUTF16SecondSurrogateBegin) or (Word(AText[I]) > cUTF16SecondSurrogateEnd) then
      Inc(Result);
{$ENDIF}
end;

function StringCopy(const ASource: TKString; At, Count: Integer): TKString;
{$IFnDEF FPC}
var
  ByteFrom, ByteTo: Integer;
{$ENDIF}
begin
{$IFDEF FPC}
  Result := UTF8Copy(ASource, At, Count);
{$ELSE}
  ByteFrom := StrCPIndexToByteIndex(ASource, At);
  ByteTo := StrCPIndexToByteIndex(ASource, At + Count);
  Result := Copy(ASource, ByteFrom, ByteTo - ByteFrom);
{$ENDIF}
end;

procedure StringDelete(var ASource: TKString; At, Count: Integer);
{$IFnDEF FPC}
var
  ByteFrom, ByteTo: Integer;
{$ENDIF}
begin
{$IFDEF FPC}
  LazUTF8.UTF8Delete(ASource, At, Count);
{$ELSE}
  ByteFrom := StrCPIndexToByteIndex(ASource, At);
  ByteTo := StrCPIndexToByteIndex(ASource, At + Count);
  Delete(ASource, ByteFrom, ByteTo - ByteFrom);
{$ENDIF}
end;

procedure TrimWhiteSpaces(const AText: TKString; var AStart, ALen: Integer; const ASet: TKSysCharSet);
begin
  while (ALen > 0) and CharInSetEx(AText[AStart], ASet) do
  begin
    Inc(AStart);
    Dec(ALen);
  end;
  while (ALen > 0) and CharInSetEx(AText[AStart + ALen - 1], ASet) do
    Dec(ALen);
end;

procedure TrimWhiteSpaces(var AText: TKString; const ASet: TKSysCharSet);
begin
  while (Length(AText) > 0) and CharInSetEx(AText[1], ASet) do
    Delete(AText, 1, 1);
  while (Length(AText) > 0) and CharInSetEx(AText[Length(AText)], ASet) do
    Delete(AText, Length(AText), 1);
end;

{$IFNDEF FPC}
procedure TrimWhiteSpaces(var AText: AnsiString; const ASet: TKSysCharSet);
begin
  while (Length(AText) > 0) and CharInSetEx(AText[1], ASet) do
    Delete(AText, 1, 1);
  while (Length(AText) > 0) and CharInSetEx(AText[Length(AText)], ASet) do
    Delete(AText, Length(AText), 1);
end;
{$ENDIF}

function StringToAnsiString(const AText: TKString; CodePage: Cardinal): AnsiString;
var
{$IFDEF FPC}
  CP: string;
{$ELSE}
  Len: Integer;
  W: WideString;
  DefaultChar: AnsiChar;
{$ENDIF}
begin
{$IFDEF FPC}
  if CodePage = 0 then
    CP := 'ansi'
  else
    CP := Format('cp%d', [Codepage]);
  Result := LConvEncoding.ConvertEncoding(AText, 'utf8', CP);
{$ELSE}
  if AText <> '' then
  begin
    DefaultChar := #0;
    W := WideString(AText);
    Len := WideCharToMultiByte(CodePage, 0, PWideChar(W), -1, nil, 0, @DefaultChar, nil);
    SetLength(Result, Len - 1);
    WideCharToMultiByte(CodePage, 0, PWideChar(W), -1, PAnsiChar(Result), Len, @DefaultChar, nil);
  end else
    Result := '';
{$ENDIF}
end;

function StringToUTF8(const AText: string): AnsiString;
begin
{$IFDEF FPC}
  Result := AText;
{$ELSE}
  Result := UTF8Encode(AText);
{$ENDIF}
end;

function StringToChar(const AText: TKString; AIndex: Integer): TKChar;
begin
{$IFDEF FPC}
  Result := LazUTF8.UTF8Copy(AText, AIndex, 1);
{$ELSE}
  Result := AText[AIndex];
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
function GetWindowsFolder(CSIDL: Cardinal; var APath: string): Boolean;
type
  TSHGetFolderPathProc = function(hWnd: HWND; CSIDL: Integer; hToken: THandle;
    dwFlags: DWORD; pszPath: PAnsiChar): HResult; stdcall;
var
  SHFolderHandle: HMODULE;
  SHGetFolderPathProc: TSHGetFolderPathProc;
  Buffer: PAnsiChar;
begin
  Result := False;
  APath := '';
  SHFolderHandle := GetModuleHandle(SHFolderDll);
  if SHFolderHandle <> 0 then
  begin
    SHGetFolderPathProc := GetProcAddress(SHFolderHandle, 'SHGetFolderPathA');
    if Assigned(SHGetFolderPathProc) then
    begin
      GetMem(Buffer, MAX_PATH);
      try
        if Succeeded(SHGetFolderPathProc(0, CSIDL, 0, 0, Buffer)) then
        begin
          APath := string(Buffer);
          Result := True;
        end
      finally
        FreeMem(Buffer);
      end;
    end;
  end;
end;

function RunExecutable(const AFileName: string; AWaitForIt: Boolean): DWORD;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ErrMsg: PChar;
begin
  Result := STILL_ACTIVE;
  GetStartupInfo(StartupInfo);
  if CreateProcess(nil, PChar(AFileName), nil, nil, IsConsole,
    NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
  begin
    try
      if (not AWaitForIt) or (WaitForSingleObject(ProcessInfo.hProcess,INFINITE) = WAIT_OBJECT_0) then
        GetExitCodeProcess(ProcessInfo.hProcess, Result);
    finally
      CloseHandle(ProcessInfo.hThread);
      CloseHandle(ProcessInfo.hProcess);
    end;
  end else
  begin
    if FormatMessage(Format_Message_Allocate_Buffer or Format_Message_From_System, nil,
      GetLastError, 0, @errMsg, 0, nil) <> 0 then
    begin
      try
        Error('CreateProcess failed with error "' + String (errMsg) + '".');
      finally
        LocalFree (HLOCAL(errMsg));
      end;
    end;
  end;
end;
{$ENDIF}

function SystemCodePage: Integer;
begin
{$IFDEF MSWINDOWS}
  Result := getACP;
{$ELSE}
 {$IF DEFINED(UNIX) and (FPC_FULLVERSION>=20701)}
  Result := GetSystemCodepage;
 {$ELSE}
  Result := 0;
 {$IFEND}
{$ENDIF}
end;

function NativeUTFToUnicode(const AText: TKString): WideChar;
{$IFDEF FPC}
var
  CharLen: Integer;
{$ENDIF}
begin
{$IFDEF FPC}
  Result := WideChar(LazUTF8.UTF8CharacterToUnicode(PChar(AText), CharLen));
{$ELSE}
  Result := AText[1];
{$ENDIF}
end;

function UnicodeUpperCase(const AText: TKString): TKString;
begin
{$IFDEF FPC}
  Result := LazUTF8.UTF8UpperCase(AText);
{$ELSE}
 {$IFDEF STRING_IS_UNICODE}
  Result := AnsiUpperCase(AText);
 {$ELSE}
  Result := WideUpperCase(AText);
 {$ENDIF}
{$ENDIF} 
end;

function UnicodeLowerCase(const AText: TKString): TKString;
begin
{$IFDEF FPC}
  Result := LazUTF8.UTF8LowerCase(AText);
{$ELSE}
 {$IFDEF STRING_IS_UNICODE}
  Result := AnsiLowerCase(AText);
 {$ELSE}
  Result := WideLowerCase(AText);
 {$ENDIF}
{$ENDIF}
end;

function UnicodeToNativeUTF(const AParam: WideChar): TKString;
begin
{$IFDEF FPC}
  Result := LazUTF8.UnicodeToUTF8(Cardinal(AParam));
{$ELSE}
  Result := AParam;
{$ENDIF}
end;

function UnicodeStringReplace(const AText, AOldPattern, ANewPattern: TKString;
  AFlags: TReplaceFlags): TKString;
var
  SearchStr, Pattern, Candidate: TKString;
  I, NewI, PatternLen, SearchLen: Integer;
  DoInc, Found: Boolean;
begin
  Result := '';
  if rfIgnoreCase in AFlags then
  begin
    SearchStr := UnicodeUpperCase(AText);
    Pattern := UnicodeUpperCase(AOldPattern);
  end else
  begin
    SearchStr := AText;
    Pattern := AOldPattern;
  end;
  PatternLen := Length(Pattern);
  SearchLen := Length(SearchStr);
  Found := False;
  I := 1;
  while (I <= SearchLen) do
  begin
    DoInc := True;
    if (rfReplaceAll in AFlags) or not Found then
    begin
      if SearchStr[I] = Pattern[1] then
      begin
        Candidate := Copy(SearchStr, I, PatternLen);
        if Candidate = Pattern then
        begin
          Result := Result + ANewPattern;
          Inc(I, PatternLen);
          DoInc := False;
          Found := True;
        end;
      end;
    end;
    if DoInc then
    begin
      NewI := StrNextCharIndex(SearchStr, I);
      Result := Result + Copy(SearchStr, I, NewI - I);
      I := NewI;
    end;
  end;
end;

function UTF8ToString(const AText: AnsiString): string;
begin
{$IFDEF FPC}
  Result := AText;
{$ELSE}
  {$IFDEF COMPILER12_UP}
    Result := System.UTF8ToString(AText);
  {$ELSE}
    Result := System.UTF8Decode(AText);
  {$ENDIF}
{$ENDIF}
end;

function MakeHexDigitPosition(Index: Int64; Digit: Integer): TKHexDigitPosition;
begin
  Result.Index := Index;
  Result.Digit := Digit;
end;

function DigitToBin(Value: AnsiChar): Integer;
begin
  if ((Value >= 'a') and (Value <= 'f')) then Result := Ord(Value) - Ord('a') + 10
  else if ((Value >= 'A') and (Value <= 'F')) then Result := Ord(Value) - Ord('A') + 10
  else if ((Value >= '0') and (Value <= '9')) then Result := Ord(Value) - Ord('0')
  else Result := -1;
end;

function DigitsToBinStr(var S: AnsiString; Convert: Boolean = True): Boolean;
var
  I, J, K: Integer;
  T: AnsiString;
begin
  // check and convert text characters to hex values 0..15
  Result := True;
  if Convert then
    SetLength(T, Length(S));
  J := 0;
  for I := 1 to Length(S) do if not CharInSetEx(S[I], [cTAB, cSPACE]) then
  begin
    K := DigitToBin(S[I]);
    if K >= 0 then
    begin
      if Convert then
      begin
        Inc(J);
        T[J] := AnsiChar(K)
      end;
    end else
    begin
      Result := False;
      Break;
    end;
  end;
  if Result and Convert then
  begin
    SetLength(T, J);
    S := T;
  end;
end;

function BinStrToBinary(const S: AnsiString): AnsiString;
var
  I, J, L: Integer;
  B1, B2: Byte;
begin
  L := Length(S);
  Result := '';
  if L > 0 then
  begin
    SetLength(Result, DivUp(L, 2));
    if L = 1 then
      Result := S
    else
    begin
      J := 1;
      for I := 1 to Length(Result) do
      begin
        B1 := Byte(S[J]); Inc(J);
        if J <= L then
        begin
          B2 := Byte(S[J]); Inc(J);
        end else
          B2 := 0;
        Result[I] := AnsiChar(B1 shl 4 + B2);
      end;
    end;
  end;
end;

function BinToDigit(Value: Byte): AnsiChar;
begin
  if Value >= $10 then
    Result := '0'
  else if Value >= $A then
    Result := AnsiChar(Ord('A') + Value - 10)
  else
    Result := AnsiChar(Ord('0') + Value)
end;

function BinaryToDigits(Buffer: PBytes; SelStart, SelEnd: TKHexDigitPosition;
  AInsertSpaces: Boolean): AnsiString;
var
  I, J, SpaceCount: Integer;
begin
  if AInsertSpaces then
    SpaceCount := SelEnd.Index - SelStart.Index
  else
    SpaceCount := 0;
  SetLength(Result, (SelEnd.Index - SelStart.Index) * cHexDigitCount - SelStart.Digit + SelEnd.Digit + SpaceCount);
  J := 1;
  for I := SelStart.Index to SelEnd.Index do
  begin
    if ((I > SelStart.Index) or (SelStart.Digit < 1)) and ((I < SelEnd.Index) or (SelEnd.Digit > 0)) then
    begin
      Result[J] := BinToDigit((Buffer[I] shr 4) and $F);
      Inc(J);
    end;
    if ((I > SelStart.Index) or (SelStart.Digit < 2)) and ((I < SelEnd.Index) or (SelEnd.Digit > 1)) then
    begin
      Result[J] := BinToDigit(Buffer[I] and $F);
      Inc(J);
    end;
    if AInsertSpaces and (I < SelEnd.Index) then
    begin
      Result[J] := ' ';
      Inc(J);
    end;
  end;
end;

function BinaryToDigits(Buffer: PBytes; ASize: Int64; AInsertSpaces: Boolean = False): AnsiString;
begin
  Result := BinaryToDigits(Buffer, MakeHexDigitPosition(0, 0), MakeHexDigitPosition(ASize - 1, cHexDigitCount), AInsertSpaces);
end;

function BinaryToDigits(const Source: AnsiString; AInsertSpaces: Boolean): AnsiString;
begin
  Result := BinaryToDigits(PBytes(@Source[1]), MakeHexDigitPosition(0, 0), MakeHexDigitPosition(Length(Source) - 1, cHexDigitCount), AInsertSpaces);
end;

function BinStrToDigits(const Source: AnsiString; AInsertSpaces: Boolean): AnsiString;
var
  I, J, CharLen, SpaceCount: Integer;
begin
  CharLen := Length(Source) div 2;
  if AInsertSpaces then
    SpaceCount := CharLen - 1
  else
    SpaceCount := 0;
  SetLength(Result, CharLen * 2 + SpaceCount);
  J := 1;
  for I := 1 to CharLen do
  begin
    Result[J] := BinToDigit(Ord(Source[I * 2 - 1]));
    Inc(J);
    Result[J] := BinToDigit(Ord(Source[I * 2]));
    Inc(J);
    if AInsertSpaces and (I < CharLen) then
    begin
      Result[J] := ' ';
      Inc(J);
    end;
  end;
end;

function InsertSpacesToDigits(const Source: AnsiString): AnsiString;
var
  I, J, CharLen, SpaceCount: Integer;
begin
  CharLen := Length(Source) div 2;
  SpaceCount := CharLen - 1;
  SetLength(Result, CharLen * 2 + SpaceCount);
  J := 1;
  for I := 1 to CharLen do
  begin
    Result[J] := Source[I * 2 - 1];
    Inc(J);
    Result[J] := Source[I * 2];
    Inc(J);
    if I < CharLen then
    begin
      Result[J] := ' ';
      Inc(J);
    end;
  end;
end;

function ReplaceDigit(Value, Digit, Pos: Integer): Integer;
var
  I, Mask, O: Integer;
begin
  O := 1;
  for I := Pos to cHexDigitCount - 2 do
    O := O * cHexBase;
  Mask := cHexBase - 1;
  Result := (((Value div O) and not Mask) + (Digit and Mask)) * O + Value mod O;
end;

end.

