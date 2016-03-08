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

unit DDuce.WinIPC;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils;

  { TWinIPCClient }

  { IPC using WM_COPYDATA messages. }
type
  TWinIPCClient = class (TComponent)
  strict private
    FActive         : Boolean;
    FServerID       : string;
    FServerInstance : string;
    FWindowName     : string;
    FHWND           : HWnd;

    procedure SetActive(const AValue: Boolean);
    procedure SetServerID(const AValue: string);
    procedure SetServerInstance(const AValue: string);

    procedure UpdateWindowName;

  public
    procedure Connect;
    procedure Disconnect;
    function  ServerRunning: Boolean;
    procedure SendStream(AStream: TStream);

    property Active: Boolean
      read FActive write SetActive;

    property ServerID: string
      read FServerID write SetServerID;

    property ServerInstance: string
      read FServerInstance write SetServerInstance;
  end;

implementation

uses
  WinApi.Messages;

const
  MSG_WND_CLASSNAME : PChar = 'FPCMsgWindowCls';

resourcestring
  SErrServerNotActive = 'Server with ID %s is not active.';

{$REGION 'property access methods'}
procedure TWinIPCClient.SetActive(const AValue: Boolean);
begin
  if AValue <> Active then
  begin
    FActive := AValue;
    if FActive then
      Connect
    else
      Disconnect;
  end;
end;

procedure TWinIPCClient.SetServerID(const AValue: string);
begin
  FServerID := AValue;
  UpdateWindowName;
end;

procedure TWinIPCClient.SetServerInstance(const AValue: string);
begin
  FWindowName := AValue;
  UpdateWindowName;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TWinIPCClient.UpdateWindowName;
begin
  if FServerInstance <> '' then
    FWindowName := FServerID + '_' + FServerInstance
  else
    FWindowName := FServerID;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TWinIPCClient.Connect;
begin
  FHWND := FindWindow(MSG_WND_CLASSNAME, PChar(FWindowName));
  if FHWND = 0 then
    raise Exception.Create(Format(SErrServerNotActive, [FServerID]));
end;

procedure TWinIPCClient.Disconnect;
begin
  FHWND := 0;
end;

function TWinIPCClient.ServerRunning: Boolean;
begin
  Result := FindWindow(MSG_WND_CLASSNAME, PChar(FWindowName)) <> 0;
end;

procedure TWinIPCClient.SendStream(AStream: TStream);
var
  CDS  : TCopyDataStruct;
  Data : TMemoryStream;
  MS   : TMemoryStream;
begin
  if AStream is TMemoryStream then
  begin
    Data := TMemoryStream(AStream);
    MS   := nil
  end
  else
  begin
    MS := TMemoryStream.Create;
    Data := MS;
  end;
  try
    if Assigned(MS) then
    begin
      MS.CopyFrom(AStream, 0);
      MS.Seek(0, soFromBeginning);
    end;
    CDS.lpData := Data.Memory;
    CDS.cbData := Data.Size;
    WinApi.Windows.SendMessage(FHWnd, WM_COPYDATA, 0, NativeInt(@CDS));
  finally
    FreeAndNil(MS);
  end;
end;
{$ENDREGION}

end.
