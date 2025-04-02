{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Winipc.Client;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils;

{ IPC using WM_COPYDATA messages. A TWinipcClient is used to send WM_COPYDATA
  messages to the corresponding TWinIPCServer. }

type
  TWinipcClient = class
  private
    FServerProcessId          : Integer;
    FServerThreadId           : Integer;
    FServerMsgWindowClassName : string;
    FServerWindowName         : string;

  protected
    {$REGION 'property access methods'}
    function GetServerProcessId: Integer;
    function GetServerThreadId: Integer;
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    function GetServerHandle: THandle;
    {$ENDREGION}

  public
    procedure AfterConstruction; override;
    constructor Create(
      const AServerMsgWindowClassName : string = '';
      const AServerWindowName         : string = ''
    );

    function Connect: Boolean;
    procedure SendStream(AStream: TBytesStream);

    property ServerMsgWindowClassName: string
      read FServerMsgWindowClassName write FServerMsgWindowClassName;

    property ServerWindowName: string
      read FServerWindowName write FServerWindowName;

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
  Vcl.Forms,

  DDuce.Resources;

{$REGION 'construction and destruction'}
constructor TWinipcClient.Create(const AServerMsgWindowClassName,
  AServerWindowName: string);
begin
  inherited Create;
  FServerMsgWindowClassName := AServerMsgWindowClassName;
  FServerWindowName         := AServerWindowName;
end;

procedure TWinipcClient.AfterConstruction;
begin
  inherited AfterConstruction;
  if FServerMsgWindowClassName.IsEmpty then
    FServerMsgWindowClassName := MSG_WND_CLASSNAME;
  if FServerWindowName.IsEmpty then
    FServerWindowName := SERVER_WINDOWNAME;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWinipcClient.GetConnected: Boolean;
begin
  Result := ServerHandle <> 0;
end;

procedure TWinipcClient.SetConnected(const Value: Boolean);
begin
  if Value then
    Connect;
end;

function TWinipcClient.GetServerHandle: THandle;
begin
  Result := FindWindow(PChar(ServerMsgWindowClassName), PChar(ServerWindowName));
end;

function TWinipcClient.GetServerProcessId: Integer;
begin
  Result := FServerProcessId;
end;

function TWinipcClient.GetServerThreadId: Integer;
begin
  Result := FServerThreadId;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TWinipcClient.Connect: Boolean;
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

procedure TWinipcClient.SendStream(AStream: TBytesStream);
var
  CDS  : TCopyDataStruct;
begin
  if Connected then
  begin
    CDS.dwData := GetCurrentProcessId;
    CDS.lpData := AStream.Memory;
    CDS.cbData := AStream.Size;
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
