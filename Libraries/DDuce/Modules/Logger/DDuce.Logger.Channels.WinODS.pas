{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Logger.Channels.WinODS;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.SysUtils, System.Contnrs;

const
  WM_ODS = WM_USER + 1; // OutputDebugString

type
  // the thread that catches OutputDebugString content
  TODSThread = class(TThread)
  protected
    hCloseEvent : THandle;
    procedure Execute; override;
  end;

  PODSRec = ^TODSRec;

  TODSRec = record
    OriginalOrder : cardinal;
    // Original order when inserted. Used to Unsort nodes
    Time : string; // time of send
    ProcessName : string; // optional : the name of the process that send traces
    LeftMsg : string; // Left col
  end;

  TODSTemp = class
    Index : Cardinal;
    // Original order when inserted. Used to Unsort nodes
    Time : string; // time of send
    ProcessName : string; // optional : the name of the process that send traces
    MsgText : string; // Left col
  end;

  // some helper functions to retreive Process name from its ID
function GetExenameForProcessUsingToolhelp32(AProcessID: DWORD): string;
function GetExenameForProcessUsingPSAPI(AProcessID: DWORD): string;
function GetExenameForProcess(AProcessID: DWORD): string;
function GetExenameForWindow(AWndHandle: HWND): string;

implementation

uses
  Winapi.PsAPI, Winapi.TlHelp32,
  System.SyncObjs, System.DateUtils;

var
  LastChildOrder : Cardinal;
  // Order of the last child, used to insert sub nodes and unsort them
  OdsMessageStack : TObjectList;

{$REGION 'interfaced routines'}
  // some helper functions to retreive Process name from its ID

function GetExenameForProcessUsingPSAPI(AProcessID: DWORD): string;
var
  I, cbNeeded: DWORD;
  Modules    : array [1 .. 1024] of HINST;
  ProcHandle : THandle;
  FileName   : array [0 .. 512] of Char;
begin
  SetLastError(0);
  Result     := '';
  ProcHandle := OpenProcess(
    PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
    False, AProcessID);
  if ProcHandle <> 0 then
    try
      if EnumProcessModules(ProcHandle, @Modules[1],
        sizeof(Modules), cbNeeded)
      then
        for I := 1 to cbNeeded div sizeof(HINST) do
        begin
          if GetModuleFilenameEx(ProcHandle, Modules[I], FileName,
            sizeof(FileName)
            ) > 0
          then
            if CompareText(ExtractFileExt(FileName), '.EXE') = 0
            then
            begin
              Result := FileName;
              Break;
            end;
        end; { For }
    finally
      CloseHandle(ProcHandle);
    end;
end;

function GetExenameForProcessUsingToolhelp32(AProcessID: DWORD): string;
var
  Snapshot : THandle;
  ProcEntry: TProcessEntry32;
  Ret      : BOOL;
begin
  SetLastError(0);
  Result   := '';
  Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if Snapshot <> INVALID_HANDLE_VALUE then
    try
      ProcEntry.dwSize := sizeof(ProcEntry);
      Ret              := Process32FIRST(Snapshot, ProcEntry);
      while Ret do
      begin
        if ProcEntry.th32ProcessID = AProcessID then
        begin
          Result := ProcEntry.szExeFile;
          Break;
        end
        else
          Ret := Process32Next(Snapshot, ProcEntry);
      end;
    finally
      CloseHandle(Snapshot);
    end;
end;

function GetExenameForProcess(AProcessID: DWORD): string;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
    (Win32MajorVersion <= 4)
  then
    Result := GetExenameForProcessUsingPSAPI(AProcessID)
  else
    Result := GetExenameForProcessUsingToolhelp32(AProcessID);

  Result := ExtractFileName(Result)
end;

function GetExenameForWindow(AWndHandle: HWND): string;
var
  ProcessID: DWORD;
begin
  Result := '';
  if IsWindow(AWndHandle) then
  begin
    GetWindowThreadProcessID(AWndHandle, ProcessID);
    if ProcessID <> 0 then
      Result := GetExenameForProcess(ProcessID);
  end;
end;
{$ENDREGION}

{$REGION 'TODSThread'}
procedure TODSThread.Execute;
var
  AckEvent   : THandle;
  ReadyEvent : THandle;
  SharedFile : THandle;
  SharedMem  : pointer;
  Ret        : DWORD;
  SA         : SECURITY_ATTRIBUTES;
  SD         : SECURITY_DESCRIPTOR;
  ODSTemp    : TODSTemp;

  // Buffer: Pointer;
  // dwSizeofDataToWrite : Word ;
  HandlesToWaitFor : array [0 .. 1] of THandle;
begin
  SA.nLength              := SizeOf(SECURITY_ATTRIBUTES);
  SA.bInheritHandle       := TRUE;
  SA.lpSecurityDescriptor := @SD;

  if not InitializeSecurityDescriptor(@SD, SECURITY_DESCRIPTOR_REVISION) then
    Exit;

  if not SetSecurityDescriptorDacl(@SD, TRUE, nil { (PACL)NULL } , False) then
    Exit;

  AckEvent := CreateEvent(@SA, False, TRUE, 'DBWIN_BUFFER_READY');
  // initial state = CLEARED !!!
  if AckEvent = 0 then
    Exit;

  ReadyEvent := CreateEvent(@SA, False, False, 'DBWIN_DATA_READY');
  // initial state = CLEARED
  if ReadyEvent = 0 then
    Exit;

  SharedFile := CreateFileMapping(
    THandle(-1), 
    @SA, 
    PAGE_READWRITE, 
    0, 
    4096,
    'DBWIN_BUFFER'
  );
  if SharedFile = 0 then
    Exit;

  SharedMem := MapViewOfFile(SharedFile, FILE_MAP_READ, 0, 0, 512);
  if SharedMem = nil then
    Exit;

  while not Terminated do
  begin
    HandlesToWaitFor[0] := hCloseEvent;
    HandlesToWaitFor[1] := ReadyEvent;

    SetEvent(AckEvent); // set ACK event to allow buffer to be used
    Ret := WaitForMultipleObjects(2, @HandlesToWaitFor, False { bWaitAll } ,
      3000 { INFINITE } );

    // ret := WaitForSingleObject(ReadyEvent, 10000 {INFINITE} );
    case Ret of
      WAIT_TIMEOUT :
        Continue;

      WAIT_OBJECT_0 :
        begin // hCloseEvent
          Break;
        end;
      WAIT_OBJECT_0 + 1 :
        begin
          // if XMLConfig.ods.Enabled.Value then begin
          // cannot be added directly from a thread. Send the message to the main thread
          TMonitor.Enter(Self);
          try
            ODSTemp             := TODSTemp.Create;
            ODSTemp.Time        := FormatDateTime('hh:mm:ss:zzz', now);
            ODSTemp.ProcessName := GetExenameForProcess(LPDWORD(SharedMem)^);
            // '$' + inttohex (pThisPid^,2)
            ODSTemp.MsgText := string(PAnsiChar(SharedMem) + sizeof(DWORD));
            // the native version of OutputDebugString is ASCII. result is always AnsiString
            ODSTemp.Index := LastChildOrder;
            inc(LastChildOrder);

            OdsMessageStack.Add(ODSTemp);
          finally
            TMonitor.Exit(Self);
          end;
        end;
      WAIT_FAILED: // Wait failed.  Shouldn't happen.
        Continue;
    end;
  end;
  UnmapViewOfFile(SharedMem);
  CloseHandle(SharedFile);
end;
{$ENDREGION}

initialization
  //OdsMessageStack := TObjectList.Create(False);
  {
  procedure TFrm_ODS.TimerInfo;
var
   ODSTemp : TODSTemp ;
   ODSRec : PODSRec ;
   node : pvirtualnode ;
   messageAdded : boolean ;
begin

   messageAdded := false ;
   while true do begin
      // get the first element in the MessageStack
      criticalsection.Enter ;
      try
         if OdsMessageStack.Count = 0 then begin
            ODSTemp := nil ;
         end else begin
            ODSTemp := TODSTemp( OdsMessageStack.Items[0] ) ;
            OdsMessageStack.Delete(0);  // MessageStack  don't own the objects
         end ;
      finally
         criticalsection.Leave ;
      end;

      if ODSTemp = nil then
         break ;  // quit the loop

      if messageAdded = false then
         VstDebugString.BeginUpdate ;
      messageAdded := true ;

      node := VstDebugString.AddChild (nil) ;
      // ensure node is initialized. Needed when the node is free to call onFreeNode
      VstDebugString.ReinitNode(node,false);
      ODSRec := VstDebugString.GetNodeData(node) ;
      ODSRec.Time          := ODSTemp.Time ;
      ODSRec.ProcessName   := ODSTemp.ProcessName ;
      ODSRec.LeftMsg       := ODSTemp.LeftMsg ;
      OdsRec.OriginalOrder := ODSTemp.OriginalOrder ;
      ODSTemp.Free ;             // free the message

      if TraceConfig.AppDisplay_FocusToReceivedMessage then
         NodeToFocus := node ;

      LastModified := now ;
      //VstDebugString.RepaintNode(node);

      CheckAutoClear() ;

      // check if the node can be displayed according the filters
      if Filter <> nil then
         Filter.CheckNode(node) ;

   end ;   // loop


   // autosort if at least one column in sort
   if Sorter.SortColumns.Count <> 0 then
      Sorter.sort (nil) ;
   if messageAdded then
      VstDebugString.endUpdate ;
   TracesInfo.Caption := TimeToStr(LastModified)
                         + ', not filtered lines : ' + inttostr(VstDebugString.RootNode.ChildCount) ;
   if NodeToFocus <> nil then begin
      VstDebugString.ClearSelection();
      VstDebugString.Selected [NodeToFocus] := true ;
      VstDebugString.FocusedNode := NodeToFocus;
      VstDebugString.ScrollIntoView (NodeToFocus,false,false);
   end;
   NodeToFocus := nil ;
end;
}

end.
