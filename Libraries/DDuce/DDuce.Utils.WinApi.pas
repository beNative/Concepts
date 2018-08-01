{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I DDuce.inc}

unit DDuce.Utils.Winapi;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils;

function GetExenameForProcess(AProcessId: DWORD): string;

function GetExenameForWindow(AWndHandle: HWND): string;

function GetExenameForProcessUsingPsAPI(AProcessId: DWORD): string;

function GetExenameForProcessUsingToolhelp32(AProcessId: DWORD): string;

procedure GetIPAddresses(AStrings: TStrings); overload;

function GetExternalIP(out AIP: string): Boolean;

function GetIP(const AHostName: string): string;

implementation

uses
  Winapi.PsAPI, Winapi.TlHelp32, Winapi.WinSock, Winapi.WinInet,

  DDuce.Utils;

{$REGION 'interfaced routines'}
function GetExenameForProcessUsingPsAPI(AProcessId: DWORD): string;
var
  I           : DWORD;
  LCBNeeded   : DWORD;
  LModules    : array [1 .. 1024] of HINST;
  LProcHandle : THandle;
  LFileName   : array [0 .. 512] of Char;
begin
  SetLastError(0);
  Result      := '';
  LProcHandle := OpenProcess(
    PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
    False,
    AProcessId
  );
  if LProcHandle <> 0 then
  begin
    try
      if EnumProcessModules(
        LProcHandle,
        @LModules[1],
        SizeOf(LModules),
        LCBNeeded
      ) then
      begin
        for I := 1 to LCBNeeded div SizeOf(HINST) do
        begin
          if GetModuleFilenameEx(LProcHandle, LModules[I], LFileName,
            SizeOf(LFileName)) > 0 then
          begin
            if CompareText(ExtractFileExt(LFileName), '.EXE') = 0 then
            begin
              Result := LFileName;
              Break;
            end;
          end;
        end;
      end;
    finally
      CloseHandle(LProcHandle);
    end;
  end;
end;

function GetExenameForProcessUsingToolhelp32(AProcessId: DWORD): string;
var
  LSnapshot  : THandle;
  LProcEntry : TProcessEntry32;
  LRet       : BOOL;
begin
  SetLastError(0);
  Result   := '';
  LSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if LSnapshot <> INVALID_HANDLE_VALUE then
    try
      LProcEntry.dwSize := SizeOf(LProcEntry);
      LRet              := Process32First(LSnapshot, LProcEntry);
      while LRet do
      begin
        if LProcEntry.th32ProcessID = AProcessId then
        begin
          Result := LProcEntry.szExeFile;
          Break;
        end
        else
          LRet := Process32Next(LSnapshot, LProcEntry);
      end;
    finally
      CloseHandle(LSnapshot);
    end;
end;

function GetExenameForProcess(AProcessId: DWORD): string;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion <= 4) then
    Result := GetExenameForProcessUsingPSAPI(AProcessId)
  else
    Result := GetExenameForProcessUsingToolhelp32(AProcessId);
  Result := ExtractFileName(Result)
end;

function GetExenameForWindow(AWndHandle: HWND): string;
var
  LProcessID: DWORD;
begin
  Result := '';
  if IsWindow(AWndHandle) then
  begin
    GetWindowThreadProcessID(AWndHandle, LProcessID);
    if LProcessID <> 0 then
      Result := GetExenameForProcess(LProcessID);
  end;
end;

procedure GetIPAddresses(AStrings: TStrings);
type
  TaPInAddr = array [0 .. 10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  PHE       : PHostEnt;
  PPtr      : PaPInAddr;
  Buffer    : array [0 .. 63] of AnsiChar;
  I         : Integer;
  GInitData : TWSAData;
begin
  WSAStartup($101, GInitData);
  AStrings.Clear;
  GetHostName(Buffer, SizeOf(Buffer));
  PHE := GetHostByName(Buffer);
  if PHE = nil then
    Exit;
  PPtr := PaPInAddr(PHE^.h_addr_list);
  I    := 0;
  while PPtr^[I] <> nil do
  begin
    AStrings.Add(string(inet_ntoa(PPtr^[I]^)));
    Inc(I);
  end;
  WSACleanup;
end;

function GetExternalIP(out AIP: string): Boolean;
const
  BUFFER_SIZE = 1024;
  URL_LIST    : array[0..2] of string = (
    'http://bot.whatismyipaddress.com',
    'http://icanhazip.com',
    'http://myip.dnsomatic.com'
  );
var
  INETHandle : Pointer;
  URLHandle  : Pointer;
  BytesRead  : Cardinal;
  Buffer     : Pointer;
  OStream    : TStringStream;
  I          : Integer;
  URL        : string;
  DataString : string;
begin
  Result := False;
  for I := Low(URL_LIST) to High(URL_LIST) do
  begin
    URL := URL_LIST[I];
    INETHandle := InternetOpen(PChar(URL), 0, nil, nil, 0);
    if Assigned(INETHandle) then
    try
      URLHandle := InternetOpenUrl(INETHandle, PChar(URL), nil, 0, 0, 0);
      if Assigned(URLHandle) then
      try
        GetMem(Buffer, BUFFER_SIZE);
        try
          OStream := TStringStream.Create;
          try
            repeat
              if not InternetReadFile(URLHandle, Buffer, BUFFER_SIZE, BytesRead) then
                Break;

              if BytesRead > 0 then
                OStream.WriteBuffer(Buffer^, BytesRead);
            until BytesRead = 0;

            if OStream.Size > 0 then
            begin
              DataString := Trim(OStream.DataString);
              if IsValidIP(DataString) then
              begin
                AIP := DataString;
                Exit(True);
              end;
            end;
          finally
            OStream.Free;
          end;
        finally
          FreeMem(Buffer, BUFFER_SIZE);
        end;
      finally
        InternetCloseHandle(URLHandle);
      end;
    finally
      InternetCloseHandle(INETHandle);
    end;
  end;
end;

{ source:
  https://stackoverflow.com/questions/18254209/how-to-get-the-ip-address-from-a-dns-for-a-host-name }

function GetIP(const AHostName: string): string;
var
  LWSAData : TWSAData;
  R        : PHostEnt;
  A        : TInAddr;
begin
  Result := '0.0.0.0';
  WSAStartup($101, LWSAData);
  R := Winapi.Winsock.GetHostByName(PAnsiChar(AnsiString(AHostName)));
  if Assigned(R) then
  begin
    A := PInAddr(r^.h_Addr_List^)^;
    Result := string(Winapi.WinSock.inet_ntoa(A));
  end;
end;
{$ENDREGION}

end.
