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

unit DDuce.Logger.Channels.WinODS;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.SysUtils, System.Contnrs;

const
  WM_ODS = WM_USER + 1; // OutputDebugString

type
  // the thread that catches OutputDebugString
  TODSThread = class(TThread)
  protected
    hCloseEvent : THandle;
    procedure Execute; override;
  end;

  PODSRec = ^TODSRec;

  TODSRec = record
    OriginalOrder : UInt32; // Original order when inserted. Used to Unsort nodes
    Time          : string; // time of send
    ProcessName   : string; // optional : the name of the process that send traces
    LeftMsg       : string; // Left col
  end;

  TODSTemp = class
    OriginalOrder : UInt32;
    // Original order when inserted. Used to Unsort nodes
    Time          : string; // time of send
    ProcessName   : string; // optional : the name of the process that send traces
    LeftMsg       : string; // Left col
  end;

implementation

uses
  System.SyncObjs, System.DateUtils,

  DDuce.Utils.Winapi;

var
  LastChildOrder  : UInt32;
  // Order of the last child, used to insert sub nodes and unsort them
  OdsMessageStack : TObjectList;

{ TODSThread }

procedure TODSThread.Execute;
var
  LAckEvent         : THandle;
  LReadyEvent       : THandle;
  LSharedFile       : THandle;
  LSharedMem        : Pointer;
  LRet              : DWORD;
  SA                : SECURITY_ATTRIBUTES;
  SD                : SECURITY_DESCRIPTOR;
  LODSTemp          : TODSTemp;
  LHandlesToWaitFor : array [0 .. 1] of THandle;
begin
  SA.nLength              := sizeof(SECURITY_ATTRIBUTES);
  SA.bInheritHandle       := TRUE;
  SA.lpSecurityDescriptor := @SD;

  if not InitializeSecurityDescriptor(@SD, SECURITY_DESCRIPTOR_REVISION) then
    Exit;

  if not SetSecurityDescriptorDacl(@SD, TRUE, nil { (PACL)NULL } , False) then
    Exit;

  LAckEvent := CreateEvent(@SA, False, TRUE, 'DBWIN_BUFFER_READY');
  // initial state = CLEARED !!!
  if LAckEvent = 0 then
    Exit;

  LReadyEvent := CreateEvent(@SA, False, False, 'DBWIN_DATA_READY');
  // initial state = CLEARED
  if LReadyEvent = 0 then
    Exit;

  LSharedFile := CreateFileMapping(THandle(-1), @SA, PAGE_READWRITE, 0, 4096,
    'DBWIN_BUFFER');
  if LSharedFile = 0 then
    Exit;

  LSharedMem := MapViewOfFile(LSharedFile, FILE_MAP_READ, 0, 0, 512);
  if LSharedMem = nil then
    Exit;

  while not Terminated do
  begin
    LHandlesToWaitFor[0] := hCloseEvent;
    LHandlesToWaitFor[1] := LReadyEvent;

    SetEvent(LAckEvent); // set ACK event to allow buffer to be used
    LRet := WaitForMultipleObjects(2, @LHandlesToWaitFor, False { bWaitAll } ,
      3000 { INFINITE } );

    // LRet := WaitForSingleObject(LReadyEvent, 10000 {INFINITE} );
    case LRet of
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
            LODSTemp             := TODSTemp.Create;
            LODSTemp.Time        := FormatDateTime('hh:mm:ss:zzz', now);
            LODSTemp.ProcessName := GetExenameForProcess(LPDWORD(LSharedMem)^);
            // '$' + inttohex (pThisPid^,2)
            LODSTemp.LeftMsg := string(PAnsiChar(LSharedMem) + sizeof(DWORD));
            // the native version of OutputDebugString is ASCII. result is always AnsiString
            LODSTemp.OriginalOrder := LastChildOrder;
            inc(LastChildOrder);

            OdsMessageStack.Add(LODSTemp);
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
  UnmapViewOfFile(LSharedMem);
  CloseHandle(LSharedFile);
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
