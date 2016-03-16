unit DDuce.Logger.Channels.WinODS;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.SysUtils, System.Contnrs;

const
  WM_ODS = WM_USER + 1; // OutputDebugString

type
  // the thread that catch outputDebugString
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
    OriginalOrder : cardinal;
    // Original order when inserted. Used to Unsort nodes
    Time : string; // time of send
    ProcessName : string; // optional : the name of the process that send traces
    LeftMsg : string; // Left col
  end;

  // some helper functions to retreive Process name from its ID
function GetExenameForProcessUsingToolhelp32(pid: DWORD): string;
function GetExenameForProcessUsingPSAPI(pid: DWORD): string;
function GetExenameForProcess(pid: DWORD): string;
function GetExenameForWindow(wnd: HWND): string;

implementation

uses
  PSAPI, Tlhelp32,
  System.SyncObjs,
  System.DateUtils;

var
  LastChildOrder : cardinal;
  // Order of the last child, used to insert sub nodes and unsort them
  OdsMessageStack : TObjectList;

  // some helper functions to retreive Process name from its ID

function GetExenameForProcessUsingPSAPI(pid: DWORD): string;
var
  i, cbNeeded: DWORD;
  modules    : array [1 .. 1024] of HINST;
  ProcHandle : THandle;
  filename   : array [0 .. 512] of Char;
begin
  SetLastError(0);
  Result     := '';
  ProcHandle := OpenProcess(
    PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
    False, pid);
  if ProcHandle <> 0 then
    try
      if EnumProcessModules(ProcHandle, @modules[1],
        sizeof(modules), cbNeeded)
      then
        for i := 1 to cbNeeded div sizeof(HINST) do
        begin
          if GetModuleFilenameEx(ProcHandle, modules[i], filename,
            sizeof(filename)
            ) > 0
          then
            if CompareText(ExtractFileExt(filename), '.EXE') = 0
            then
            begin
              Result := filename;
              Break;
            end;
        end; { For }
    finally
      CloseHandle(ProcHandle);
    end;
end;

function GetExenameForProcessUsingToolhelp32(pid: DWORD): string;
var
  snapshot : THandle;
  procentry: TProcessEntry32;
  ret      : BOOL;
begin
  SetLastError(0);
  Result   := '';
  snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if snapshot <> INVALID_HANDLE_VALUE then
    try
      procentry.dwSize := sizeof(procentry);
      ret              := Process32FIRST(snapshot, procentry);
      while ret do
      begin
        if procentry.th32ProcessID = pid then
        begin
          Result := procentry.szExeFile;
          Break;
        end
        else
          ret := Process32Next(snapshot, procentry);
      end;
    finally
      CloseHandle(snapshot);
    end;
end;

function GetExenameForProcess(pid: DWORD): string;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
    (Win32MajorVersion <= 4)
  then
    Result := GetExenameForProcessUsingPSAPI(pid)
  else
    Result := GetExenameForProcessUsingToolhelp32(pid);

  Result := ExtractFileName(Result)
end;

function GetExenameForWindow(wnd: HWND): string;
var
  pid: DWORD;
begin
  Result := '';
  if IsWindow(wnd) then
  begin
    GetWindowThreadProcessID(wnd, pid);
    if pid <> 0 then
      Result := GetExenameForProcess(pid);
  end;
end;

// OutputDebugString support
{ TODSThread }

procedure TODSThread.Execute;
var
  AckEvent   : THandle;
  ReadyEvent : THandle;
  SharedFile : THandle;
  SharedMem  : pointer;
  ret        : DWORD;
  sa         : SECURITY_ATTRIBUTES;
  sd         : SECURITY_DESCRIPTOR;
  ODSTemp    : TODSTemp;

  // Buffer: Pointer;
  // dwSizeofDataToWrite : Word ;
  HandlesToWaitFor : array [0 .. 1] of THandle;
begin
  sa.nLength              := sizeof(SECURITY_ATTRIBUTES);
  sa.bInheritHandle       := TRUE;
  sa.lpSecurityDescriptor := @sd;

  if not InitializeSecurityDescriptor(@sd, SECURITY_DESCRIPTOR_REVISION) then
    Exit;

  if not SetSecurityDescriptorDacl(@sd, TRUE, nil { (PACL)NULL } , False) then
    Exit;

  AckEvent := CreateEvent(@sa, False, TRUE, 'DBWIN_BUFFER_READY');
  // initial state = CLEARED !!!
  if AckEvent = 0 then
    Exit;

  ReadyEvent := CreateEvent(@sa, False, False, 'DBWIN_DATA_READY');
  // initial state = CLEARED
  if ReadyEvent = 0 then
    Exit;

  SharedFile := CreateFileMapping(THandle(-1), @sa, PAGE_READWRITE, 0, 4096,
    'DBWIN_BUFFER');
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
    ret := WaitForMultipleObjects(2, @HandlesToWaitFor, False { bWaitAll } ,
      3000 { INFINITE } );

    // ret := WaitForSingleObject(ReadyEvent, 10000 {INFINITE} );
    case ret of
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
          // Criticalsection.Enter ;
          TMonitor.Enter(Self);
          try
            ODSTemp             := TODSTemp.Create;
            ODSTemp.Time        := FormatDateTime('hh:mm:ss:zzz', now);
            ODSTemp.ProcessName := GetExenameForProcess(LPDWORD(SharedMem)^);
            // '$' + inttohex (pThisPid^,2)
            ODSTemp.LeftMsg := string(PAnsiChar(SharedMem) + sizeof(DWORD));
            // the native version of OutputDebugString is ASCII. result is always AnsiString
            ODSTemp.OriginalOrder := LastChildOrder;
            inc(LastChildOrder);

            OdsMessageStack.Add(ODSTemp);
          finally
            TMonitor.Exit(Self);
            // Criticalsection.Release ;
          end;

          // end ;
        end;
      WAIT_FAILED: // Wait failed.  Shouldn't happen.
        Continue;
    end;
  end;
  UnmapViewOfFile(SharedMem);
  CloseHandle(SharedFile);
end;

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
