{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kwidewinprocs; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}

interface

{$IFDEF USE_WIDEWINPROCS}

type
  { Procedural type for @link(TKWideWinProcs.CompareString). }
  TCompareStringW = function(Locale, dwCmpFlags: Cardinal; lpString1: PWideChar; cchCount1:
    Integer; lpString2: PWideChar; cchCount2: Integer): Integer; stdcall;

  { Procedural type for @link(TKWideWinProcs.LStrLenW). }
  TLStrLenW = function(lpString: PWideChar): Integer;

 { Unicode equivalents of ANSI Win32 API functions not available in Win9X
   without Unicode Layer for Win9X. Only those used in KControls. }
  TKWideWinProcs = class(TObject)
  private
    FCompareStringW: TCompareStringW;
    FLStrLenW: TLStrLenW;
  public
    { Creates the instance. }
    constructor Create;
    { See MSDN for help. }
    function CompareString(Locale, dwCmpFlags: Cardinal; lpString1: PWideChar;
      cchCount1: Integer; lpString2: PWideChar; cchCount2: Integer): Integer;
    { See MSDN for help. }
    function LStrLenW(lpString: PWideChar): Integer;
  end;

var
  WideWinProcs: TKWideWinProcs;

{$ENDIF}

implementation

{$IFDEF USE_WIDEWINPROCS}

uses
  Windows, KFunctions;

{ TWideWinProcs }

constructor TKWideWinProcs.Create;
begin
  FCompareStringW := GetProcAddress(GetModuleHandle('kernel32.dll'), 'CompareStringW');
  FLStrLenW := GetProcAddress(GetModuleHandle('kernel32.dll'), 'lstrlenW');
end;

function TKWideWinProcs.CompareString(Locale, dwCmpFlags: Cardinal;
  lpString1: PWideChar; cchCount1: Integer; lpString2: PWideChar;
  cchCount2: Integer): Integer;
begin
  if Assigned(FCompareStringW) then
    Result := FCompareStringW(Locale, dwCmpFlags, lpString1, cchCount1,
      lpString2, cchCount2)
  else
    Result := CompareStringA(Locale, dwCmpFlags, PAnsiChar(WideCharToAnsiString(lpString1)),
      cchCount1, PAnsiChar(WideCharToAnsiString(lpString2)), cchCount2);
end;

function TKWideWinProcs.LStrLenW(lpString: PWideChar): Integer;
begin
  if Assigned(FLStrLenW) then
    Result := FLStrLenW(lpString)
  else
    Result := LStrLenA(PAnsiChar(WideCharToAnsiString(lpString)));
end;

initialization
  WideWinProcs := TKWideWinProcs.Create;
finalization
  WideWinProcs.Free;
{$ENDIF}
end.
