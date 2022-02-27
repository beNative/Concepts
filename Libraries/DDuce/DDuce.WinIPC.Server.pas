{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Winipc.Server;

interface

uses
  Winapi.Windows,
  System.Classes;

{ A TWinIPCServer instance is used to handle received WM_COPYDATA messages
  from one or more TWinIPCClient instances. }

type
  TWinipcMessageEvent = procedure(
    ASender   : TObject;
    ASourceId : UInt32; // unique source id for the sending process
    AData     : TStream
  ) of object;

type
  TWinipcServer = class
  private
    FMsgWindowClass     : TWndClass;
    FOnMessage          : TWinipcMessageEvent;
    FMsgData            : TBytesStream;
    FActive             : Boolean;
    FServerHandle       : THandle;
    FMsgWindowClassName : string;
    FWindowName         : string;

    procedure SetActive(const AValue : Boolean);
    function GetMsgData: TBytesStream;

  protected
    procedure DoMessage(ASourceId: UInt32; AData: TStream);
    function AllocateHWnd: THandle;
    procedure ReadMsgData(var AMsg : TMsg);
    procedure StartServer;
    procedure StopServer;

  public
    procedure AfterConstruction; override;
    constructor Create(
      const AMsgWindowClassName : string = '';
      const AWindowName         : string = ''
    );
    destructor Destroy; override;

    property Active : Boolean
      read FActive write SetActive;

    property MsgData : TBytesStream
      read GetMsgData;

    property OnMessage : TWinipcMessageEvent
      read FOnMessage write FOnMessage;

    property MsgWindowClassName: string
      read FMsgWindowClassName write FMsgWindowClassName;

    property WindowName: string
      read FWindowName write FWindowName;

  end;

implementation

uses
  Winapi.Messages,
  System.SysUtils,
  Vcl.Forms,

  DDuce.Utils, DDuce.Utils.Winapi, DDuce.Resources;

{$REGION 'non-interfaced routines'}
function MsgWndProc(AWindowHandle: THandle; AMessage, WParam, LParam: LongInt):
  LongInt; stdcall;
var
  WIS : TWinipcServer;
  Msg : TMsg;
begin
  Result := 0;
  if AMessage = WM_COPYDATA then
  begin
    WIS := TWinipcServer(GetWindowLongPtr(AWindowHandle, GWL_USERDATA));
    if Assigned(WIS) then
    begin
      Msg.Message := AMessage;
      Msg.WParam  := WParam;
      Msg.LParam  := LParam;
      WIS.ReadMsgData(Msg);
      Result := 1;
    end
  end
  else
    Result := DefWindowProc(AWindowHandle, AMessage, WParam, LParam);
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
constructor TWinipcServer.Create(const AMsgWindowClassName, AWindowName: string);
begin
  inherited Create;
  FMsgWindowClassName := AMsgWindowClassName;
  FWindowName         := AWindowName
end;

procedure TWinipcServer.AfterConstruction;
begin
  inherited AfterConstruction;
  if FMsgWindowClassName.IsEmpty then
    FMsgWindowClassName := MSG_WND_CLASSNAME;
  if FWindowName.IsEmpty then
    FWindowName := SERVER_WINDOWNAME;
  FMsgData := TBytesStream.Create;
end;

destructor TWinipcServer.Destroy;
begin
  FreeAndNil(FMsgData);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TWinipcServer.SetActive(const AValue : Boolean);
begin
  if Active <> AValue then
  begin
    if AValue then
      StartServer
    else
      StopServer;
  end;
end;

function TWinipcServer.GetMsgData: TBytesStream;
begin
  Result := FMsgData;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TWinipcServer.DoMessage(ASourceId: UInt32; AData: TStream);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, ASourceId, AData);
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TWinipcServer.AllocateHWnd: THandle;
var
  WC : TWndClass;
begin
  Pointer(FMsgWindowClass.lpfnWndProc) := @MsgWndProc;
  FMsgWindowClass.hInstance            := HInstance; // Handle of this instance
  FMsgWindowClass.lpszClassName        := PChar(MsgWindowClassName);

  if not GetClassInfo(HInstance, MSG_WND_CLASSNAME, WC)
    and (Winapi.Windows.RegisterClass(FMsgWindowClass) = 0) then
      raise Exception.Create(SFailedToRegisterWindowClass);

  Result := CreateWindowEx(
    WS_EX_TOOLWINDOW,
    PChar(MsgWindowClassName),
    PChar(WindowName),
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
    raise Exception.CreateFmt(SFailedToCreateWindow, [WindowName])
end;

procedure TWinipcServer.StartServer;
begin
  FServerHandle  := AllocateHWnd;
  FActive := True;
end;

procedure TWinipcServer.StopServer;
begin
  DestroyWindow(FServerHandle);
  FServerHandle := 0;
  FActive := False;
end;

{ This method gets called for each received message. }

procedure TWinipcServer.ReadMsgData(var AMsg: TMsg);
var
  CDS              : PCopyDataStruct;
  LClientProcessId : Integer;
begin
  CDS := PCopyDataStruct(AMsg.LParam);
  FMsgData.Clear;
  FMsgData.Seek(0, soFrombeginning);
  FMsgData.WriteBuffer(CDS^.lpData^, CDS^.cbData);
  LClientProcessId := CDS.dwData;
  DoMessage(LClientProcessId, FMsgData);
end;
{$ENDREGION}

end.
