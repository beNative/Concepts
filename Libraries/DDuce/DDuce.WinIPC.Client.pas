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

unit DDuce.WinIPC.Client;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils;

  { IPC using WM_COPYDATA messages. }
type
  TWinIPCClient = class
  strict protected
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    function GetServerHandle: THandle;

    property ServerHandle: THandle
      read GetServerHandle;

  public
    function Connect: Boolean;

    procedure SendStream(AStream: TStream);

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
{$ENDREGION}

{$REGION 'public methods'}
function TWinIPCClient.Connect: Boolean;
begin
  Result := ServerHandle <> 0;
end;

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
