{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.WinIPC.Server;

interface

uses
  Winapi.Windows,
  System.Classes;

type
  TWinIPCServer = class
  private
    FMsgWindowClass : TWndClass;
    FOnMessage      : TNotifyEvent;
    FMsgData        : TStream;
    FActive         : Boolean;
    FServerHandle   : THandle;

    procedure SetActive(const AValue : Boolean);

  protected
    procedure DoMessage;
    function AllocateHWnd: THandle;
    procedure ReadMsgData(var Msg : TMsg);
    procedure StartServer;
    procedure StopServer;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Active : Boolean
      read FActive write SetActive;

    property MsgData : TStream
      read FMsgData;

    property OnMessage : TNotifyEvent
      read FOnMessage write FOnMessage;
  end;

implementation

uses
  Winapi.Messages,
  System.SysUtils,
  Vcl.Forms;

const
// old name maintained for backwards compatibility
  MSG_WND_CLASSNAME : PChar = 'FPCMsgWindowCls';
  SERVER_WINDOWNAME : PChar = 'ipc_log_server';

resourcestring
  SFailedToRegisterWindowClass = 'Failed to register message window class';
  SFailedToCreateWindow        = 'Failed to create message window %s';

{$REGION 'non-interfaced routines'}
function MsgWndProc(AWindowHandle: THandle; AMessage, WParam, LParam: LongInt):
  LongInt; stdcall;
var
  WIS : TWinIPCServer;
  Msg : TMsg;
begin
  Result := 0;
  if AMessage = WM_COPYDATA then
  begin
    WIS := TWinIPCServer(GetWindowLongPtr(AWindowHandle, GWL_USERDATA));
    if Assigned(WIS) then
    begin
      Msg.Message := AMessage;
      Msg.WParam  := WParam;
      Msg.LParam  := LParam;
      WIS.ReadMsgData(Msg);
      WIS.DoMessage;
      Result := 1;
    end
  end
  else
    Result := DefWindowProc(AWindowHandle, AMessage, WParam, LParam);
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TWinIPCServer.AfterConstruction;
begin
  inherited AfterConstruction;
  FMsgData := TStringStream.Create;
end;

procedure TWinIPCServer.BeforeDestruction;
begin
  FreeAndNil(FMsgData);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TWinIPCServer.SetActive(const AValue : Boolean);
begin
  if Active <> AValue then
  begin
    if AValue then
      StartServer
    else
      StopServer;
  end;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TWinIPCServer.DoMessage;
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self);
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TWinIPCServer.AllocateHWnd: THandle;
var
  WC : TWndClass;
begin
  Pointer(FMsgWindowClass.lpfnWndProc) := @MsgWndProc;
  FMsgWindowClass.hInstance            := HInstance;
  FMsgWindowClass.lpszClassName        := MSG_WND_CLASSNAME;
  if not GetClassInfo(hInstance, MSG_WND_CLASSNAME, WC)
    and (Winapi.Windows.RegisterClass(FMsgWindowClass) = 0) then
      raise Exception.Create(SFailedToRegisterWindowClass);
  Result := CreateWindowEx(
    WS_EX_TOOLWINDOW,
    MSG_WND_CLASSNAME,
    SERVER_WINDOWNAME,
    WS_POPUP,
    0,
    0,
    0,
    0,
    0,
    0,
    HInstance,
    nil
  );
  if Result <> 0 then
    SetWindowLongPtr(Result, GWL_USERDATA, NativeInt(Self))
  else
    raise Exception.CreateFmt(SFailedToCreateWindow, [SERVER_WINDOWNAME])
end;

procedure TWinIPCServer.StartServer;
begin
  FServerHandle  := AllocateHWnd;
  FActive := True;
end;

procedure TWinIPCServer.StopServer;
begin
  DestroyWindow(FServerHandle);
  FServerHandle := 0;
  FActive := False;
end;

procedure TWinIPCServer.ReadMsgData(var Msg: TMsg);
var
  CDS : PCopyDataStruct;
begin
  CDS := PCopyDataStruct(Msg.LParam);
  FMsgData.Size := 0;
  FMsgData.Seek(0, soFrombeginning);
  FMsgData.WriteBuffer(CDS^.lpData^, CDS^.cbData);
end;
{$ENDREGION}

end.
