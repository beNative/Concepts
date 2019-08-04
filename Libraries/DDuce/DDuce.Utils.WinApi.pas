{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

type
  TProcessId = DWORD;

function GetTotalCpuUsagePct : Double;

function GetProcessCpuUsagePct(AProcessId: TProcessId): Double;

function GetExenameForProcess(AProcessId: TProcessId): string;

function GetExenameForWindow(AWndHandle: HWND): string;

function GetExenameForProcessUsingPsAPI(AProcessId: TProcessId): string;

function GetExenameForProcessUsingToolhelp32(AProcessId: TProcessId): string;

procedure GetIPAddresses(AStrings: TStrings); overload;

function GetExternalIP(out AIP: string): Boolean;

function GetIP(const AHostName: string): string;

procedure RunApplication(
  const AParams : string;
  const AFile   : string;
  AWait         : Boolean = True
);

procedure OpenLink(const ALink: string);

implementation

uses
  Winapi.PsAPI, Winapi.TlHelp32, Winapi.WinSock, Winapi.WinInet,
  Winapi.ShellAPI, Winapi.Messages,
  Vcl.Forms,

  System.DateUtils, System.Generics.Collections,

  DDuce.Utils;

type
  TSystemTimesRec = record
    KernelTime : TFileTIme;
    UserTime   : TFileTIme;
  end;

  TProcessTimesRec = record
    KernelTime : TFileTIme;
    UserTime   : TFileTIme;
  end;

  TProcessCpuUsage = class
    LastSystemTimes           : TSystemTimesRec;
    LastProcessTimes          : TProcessTimesRec;
    ProcessCPUusagePercentage : Double;
  end;

  TProcessCpuUsageList = TObjectDictionary<TProcessId, TProcessCpuUsage>;

var
  LatestProcessCpuUsageCache : TProcessCpuUsageList;

{$REGION 'interfaced routines'}
function GetExenameForProcessUsingPsAPI(AProcessId: TProcessId): string;
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

function GetExenameForProcessUsingToolhelp32(AProcessId: TProcessId): string;
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

function GetExenameForProcess(AProcessId: TProcessId): string;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion <= 4) then
    Result := GetExenameForProcessUsingPSAPI(AProcessId)
  else
    Result := GetExenameForProcessUsingToolhelp32(AProcessId);
  Result := ExtractFileName(Result)
end;

function GetExenameForWindow(AWndHandle: HWND): string;
var
  LProcessID: TProcessId;
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

procedure RunApplication(const AParams: string; const AFile: string;
  AWait: Boolean);

  procedure ResetMemory(out P; Size: Longint);
  begin
    if Size > 0 then
    begin
      Byte(P) := 0;
      FillChar(P, Size, 0);
    end;
  end;

  function PCharOrNil(const S: string): PChar;
  begin
    Result := Pointer(S);
  end;

  function ShellExecAndWait(const FileName: string;
    const Parameters: string = ''; const Verb: string = '';
    CmdShow: Integer = SW_HIDE; const Directory: string = ''): Boolean;
  var
    SEI : TShellExecuteInfo;
    Res : LongBool;
    Msg : tagMSG;
  begin
    ResetMemory(SEI, SizeOf(SEI));
    SEI.cbSize := SizeOf(SEI);
    SEI.fMask  := SEE_MASK_DOENVSUBST or SEE_MASK_FLAG_NO_UI
      or SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
    SEI.lpFile       := PChar(FileName);
    SEI.lpParameters := PCharOrNil(Parameters);
    SEI.lpVerb       := PCharOrNil(Verb);
    SEI.nShow        := CmdShow;
    SEI.lpDirectory  := PCharOrNil(Directory);
    {$TYPEDADDRESS ON}
    Result := ShellExecuteEx(@SEI);
    {$IFNDEF TYPEDADDRESS_ON}
    {$TYPEDADDRESS OFF}
    {$ENDIF ~TYPEDADDRESS_ON}
    if Result then
    begin
      WaitForInputIdle(SEI.hProcess, INFINITE);
      while WaitForSingleObject(SEI.hProcess, 10) = WAIT_TIMEOUT do
        repeat
          Msg.hwnd := 0;
          Res := PeekMessage(Msg, SEI.Wnd, 0, 0, PM_REMOVE);
          if Res then
          begin
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end;
        until not Res;
      CloseHandle(SEI.hProcess);
    end;
  end;
   // borrowed from Project JEDI Code Library (JCL)
  function ShellExecEx(const FileName: string; const Parameters: string = '';
    const Verb: string = ''; CmdShow: Integer = SW_SHOWNORMAL): Boolean;
  var
    SEI: TShellExecuteInfo;
  begin
    ResetMemory(SEI, SizeOf(SEI));
    SEI.cbSize := SizeOf(SEI);
    SEI.fMask := SEE_MASK_DOENVSUBST or SEE_MASK_FLAG_NO_UI;
    SEI.lpFile := PChar(FileName);
    SEI.lpParameters := PCharOrNil(Parameters);
    SEI.lpVerb := PCharOrNil(Verb);
    SEI.nShow := CmdShow;
    {$TYPEDADDRESS ON}
    Result := ShellExecuteEx(@SEI);
    {$IFNDEF TYPEDADDRESS_ON}
    {$TYPEDADDRESS OFF}
    {$ENDIF ~TYPEDADDRESS_ON}
  end;

begin
  if FileExists(AFile) then
  begin
    if AWait then
      ShellExecAndWait(AFile, AParams)
    else
      ShellExecEx(AFile, AParams);
  end
  else
    raise Exception.CreateFmt('"%s" not found', [AFile]);
end;

procedure OpenLink(const ALink: string);
begin
  ShellExecute(Application.MainForm.Handle, 'open', PWideChar(ALink), nil, nil, SW_SHOW);
end;
{$ENDREGION}

function GetRunningProcessIDs: TArray<TProcessId>;
var
  SnapProcHandle: THandle;
  ProcEntry: TProcessEntry32;
  NextProc: Boolean;
begin
  SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if SnapProcHandle <> INVALID_HANDLE_VALUE then
  begin
    try
      ProcEntry.dwSize := SizeOf(ProcEntry);
      NextProc := Process32First(SnapProcHandle, ProcEntry);
      while NextProc do
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := ProcEntry.th32ProcessID;
        NextProc := Process32Next(SnapProcHandle, ProcEntry);
      end;
    finally
      CloseHandle(SnapProcHandle);
    end;
    TArray.Sort<TProcessId>(Result);
  end;
end;

function GetProcessCpuUsagePct(AProcessId: TProcessId): Double;
  function SubtractFileTime(FileTime1: TFileTIme; FileTime2: TFileTIme): TFileTIme;
  begin
    Result := TFileTIme(Int64(FileTime1) - Int64(FileTime2));
  end;

var
  LProcessCpuUsage          : TProcessCpuUsage;
  LProcessHandle            : THandle;
  LSystemTimes              : TSystemTimesRec;
  LSystemDiffTimes          : TSystemTimesRec;
  LProcessDiffTimes         : TProcessTimesRec;
  LProcessTimes             : TProcessTimesRec;

  LSystemTimesIdleTime      : TFileTime;
  LProcessTimesCreationTime : TFileTime;
  LProcessTimesExitTime     : TFileTime;
begin
  Result := 0.0;

  LatestProcessCpuUsageCache.TryGetValue(AProcessId, LProcessCpuUsage);
  if LProcessCpuUsage = nil then
  begin
    LProcessCpuUsage := TProcessCpuUsage.Create;
    LatestProcessCpuUsageCache.Add(AProcessId, LProcessCpuUsage);
  end;
  // method from:
  // http://www.philosophicalgeek.com/2009/01/03/determine-cpu-usage-of-current-process-c-and-c/
  LProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, AProcessId);
  if LProcessHandle <> 0 then
  begin
    try
      if GetSystemTimes(LSystemTimesIdleTime, LSystemTimes.KernelTime, LSystemTimes.UserTime) then
      begin
        LSystemDiffTimes.KernelTime := SubtractFileTime(LSystemTimes.KernelTime, LProcessCpuUsage.LastSystemTimes.KernelTime);
        LSystemDiffTimes.UserTime := SubtractFileTime(LSystemTimes.UserTime, LProcessCpuUsage.LastSystemTimes.UserTime);
        LProcessCpuUsage.LastSystemTimes := LSystemTimes;
        if GetProcessTimes(LProcessHandle, LProcessTimesCreationTime, LProcessTimesExitTime, LProcessTimes.KernelTime, LProcessTimes.UserTime) then
        begin
          LProcessDiffTimes.KernelTime := SubtractFileTime(LProcessTimes.KernelTime, LProcessCpuUsage.LastProcessTimes.KernelTime);
          LProcessDiffTimes.UserTime := SubtractFileTime(LProcessTimes.UserTime, LProcessCpuUsage.LastProcessTimes.UserTime);
          LProcessCpuUsage.LastProcessTimes := LProcessTimes;
          if (Int64(LSystemDiffTimes.KernelTime) + Int64(LSystemDiffTimes.UserTime)) > 0 then
            Result := (Int64(LProcessDiffTimes.KernelTime) + Int64(LProcessDiffTimes.UserTime)) / (Int64(LSystemDiffTimes.KernelTime) + Int64(LSystemDiffTimes.UserTime)) * 100;
        end;
      end;
    finally
      CloseHandle(LProcessHandle);
    end;
  end;
end;

procedure DeleteNonExistingProcessIDsFromCache(const RunningProcessIds: TArray<TProcessId>);
var
  FoundKeyIdx: Integer;
  Keys: TArray<TProcessId>;
  n: Integer;
begin
  Keys := LatestProcessCpuUsageCache.Keys.ToArray;
  for n := Low(Keys) to High(Keys) do
  begin
    if not TArray.BinarySearch<TProcessId>(RunningProcessIds, Keys[n], FoundKeyIdx) then
      LatestProcessCpuUsageCache.Remove(Keys[n]);
  end;
end;

function GetTotalCpuUsagePct(): Double;
var
  LProcessId         : TProcessId;
  LRunningProcessIds : TArray<TProcessId>;
begin
  Result := 0.0;
  LRunningProcessIds := GetRunningProcessIDs;

  DeleteNonExistingProcessIDsFromCache(LRunningProcessIds);

  for LProcessId in LRunningProcessIds do
    Result := Result + GetProcessCpuUsagePct(LProcessId);
end;



{
uses
  SysUtils,
  ActiveX,
  ComObj,
  Variants;

// Serial Port Name

procedure  GetMSSerial_PortNameInfo;
const
  wbemFlagForwardOnly = $00000020;
var
  FSWbemLocator : OLEVariant;
  FWMIService   : OLEVariant;
  FWbemObjectSet: OLEVariant;
  FWbemObject   : OLEVariant;
  oEnum         : IEnumvariant;
  iValue        : LongWord;
begin;
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService   := FSWbemLocator.ConnectServer('localhost', 'root\WMI', '', '');
  FWbemObjectSet:= FWMIService.ExecQuery('SELECT * FROM MSSerial_PortName','WQL',wbemFlagForwardOnly);
  oEnum         := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
  while oEnum.Next(1, FWbemObject, iValue) = 0 do
  begin
    Writeln(Format('Active          %s',[FWbemObject.Active]));// Boolean
    Writeln(Format('InstanceName    %s',[FWbemObject.InstanceName]));// String
    Writeln(Format('PortName        %s',[FWbemObject.PortName]));// String

    Writeln('');
    FWbemObject:=Unassigned;
  end;
end;


begin
 try
    CoInitialize(nil);
    try
      GetMSSerial_PortNameInfo;
      Readln;
    finally
    CoUninitialize;
    end;
 except
    on E:Exception do
    begin
        Writeln(E.Classname, ':', E.Message);
        Readln;
    end;
  end;
end.
}

initialization
  LatestProcessCpuUsageCache := TProcessCpuUsageList.Create([doOwnsValues]);
  GetTotalCpuUsagePct;

finalization
  LatestProcessCpuUsageCache.Free;

end.
