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

unit DDuce.WinIPC.Client;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils;

  { IPC using WM_COPYDATA messages. A TWinIPCClient is used to send WM_COPYDATA
    messages to the corresponding TWinIPCServer. }

type
  TWinIPCClient = class
  private
    FServerProcessId : Integer;
    FServerThreadId  : Integer;

  protected
    function GetServerProcessId: Integer;
    function GetServerThreadId: Integer;
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    function GetServerHandle: THandle;

  public
    function Connect: Boolean;

    procedure SendStream(AStream: TStream);

    property ServerHandle: THandle
      read GetServerHandle;

    property ServerProcessId: Integer
      read GetServerProcessId;

    property ServerThreadId: Integer
      read GetServerThreadId;

    property Connected: Boolean
      read GetConnected write SetConnected;
  end;

implementation

uses
  WinApi.Messages,
  Vcl.Forms;

const
// old name maintained for backwards compatibility
  MSG_WND_CLASSNAME : PChar = 'FPCMsgWindowCls';
  SERVER_WINDOWNAME : PChar = 'ipc_log_server';

resourcestring
  SServerNotActive = 'Server with ID %s is not active.';

{$REGION 'property access methods'}
function TWinIPCClient.GetConnected: Boolean;
begin
  Result := ServerHandle <> 0;
end;

procedure TWinIPCClient.SetConnected(const Value: Boolean);
begin
  if Value then
    Connect;
end;

function TWinIPCClient.GetServerHandle: THandle;
begin
  Result := FindWindow(MSG_WND_CLASSNAME, SERVER_WINDOWNAME);
end;

function TWinIPCClient.GetServerProcessId: Integer;
begin
  Result := FServerProcessId;
end;

function TWinIPCClient.GetServerThreadId: Integer;
begin
  Result := FServerThreadId;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TWinIPCClient.Connect: Boolean;
begin
  Result := ServerHandle <> 0;
  if Result then
  begin
    FServerThreadId := GetWindowThreadProcessId(
      HWND(ServerHandle),
      Cardinal(FServerProcessId)
    );
  end
  else
  begin
    FServerThreadId  := 0;
    FServerProcessId := 0;
  end;
end;

{ Sends a stream of data as a WM_COPY message through a TCopyDataStruct
  instance by assigning the data as follows:

    dwData: gets the current process ID.
    cbData: length in bytes of the datbuffer.
    lpData: pointer to buffer to send (cbData bytes in size).
}

procedure TWinIPCClient.SendStream(AStream: TStream);
var
  CDS  : TCopyDataStruct;
  Data : TMemoryStream;
  MS   : TMemoryStream;
begin
  if Connected then
  begin
    if AStream is TMemoryStream then
    begin
      Data := TMemoryStream(AStream);
      MS   := nil
    end
    else
    begin
      MS := TMemoryStream.Create;
      try
        Data := MS;
        MS.CopyFrom(AStream, 0);
        MS.Seek(0, soFromBeginning);
      finally
        FreeAndNil(MS);
      end;
    end;
    CDS.dwData := GetCurrentProcessId;
    CDS.lpData := Data.Memory;
    CDS.cbData := Data.Size;
    Winapi.Windows.SendMessage(
      ServerHandle,
      WM_COPYDATA,
      0,
      Integer(@CDS)
    );
  end
  else
  begin
    raise Exception.Create('IPC client not connected');
  end;
end;
{$ENDREGION}

end.
