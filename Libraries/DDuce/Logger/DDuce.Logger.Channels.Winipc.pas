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

{$I .\..\DDuce.inc}

unit DDuce.Logger.Channels.Winipc;

interface

uses
  System.Classes, System.SysUtils,

  DDuce.Winipc.Client,
  DDuce.Logger.Interfaces, DDuce.Logger.Channels.Base;

type
  TWinipcChannel = class(TCustomLogChannel, ILogChannel, IWinipcChannel)
  private
    FClient : TWinipcClient; // sends to the server
    FBuffer : TBytesStream;

  protected
    function GetConnected: Boolean; override;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function Connect: Boolean; override;
    function Disconnect: Boolean; override;

    function Write(const AMsg: TLogMessage): Boolean; override;

  end;

implementation

uses
  Spring.Helpers;

{$REGION 'construction and destruction'}
procedure TWinipcChannel.AfterConstruction;
begin
  inherited AfterConstruction;
  FBuffer := TBytesStream.Create;
  FClient := TWinipcClient.Create;
  FClient.Connect;
end;

destructor TWinipcChannel.Destroy;
begin
  FClient.Free;
  FBuffer.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWinipcChannel.GetConnected: Boolean;
begin
  Result := FClient.Connected;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TWinipcChannel.Connect: Boolean;
begin
  Result := FClient.Connect;
end;

function TWinipcChannel.Disconnect: Boolean;
begin
  FClient.Connected := False;
  Result := True;
end;

{
  Data is streamed in following order:
    - Message type:  4 bytes (Integer)
    - TimeStamp:     8 bytes (Double)
    - TextSize:      4 bytes (Integer)
    - Text:          TextSize bytes (UTF8 encoded, backwards compatible with Ansi)
    - DataSize:      4 bytes (Integer)
    - Data:          DataSize bytes
}

function TWinipcChannel.Write(const AMsg: TLogMessage): Boolean;
const
  ZERO_BUF : Integer = 0;
var
  LTextSize : Integer;
  LDataSize : Integer;
begin
  Result := False;
  if Enabled then
  begin
    if not Connected and AutoConnect then
      Connect;
    if Connected then
    begin
      FBuffer.Clear;
      LTextSize := Length(AMsg.Text);
      FBuffer.Seek(0, soFromBeginning);
      FBuffer.WriteBuffer(AMsg.MsgType);
      FBuffer.WriteBuffer(AMsg.LogLevel);
      FBuffer.WriteBuffer(AMsg.Reserved1);
      FBuffer.WriteBuffer(AMsg.Reserved2);
      FBuffer.WriteBuffer(AMsg.TimeStamp);
      FBuffer.WriteBuffer(LTextSize);
      if LTextSize > 0 then
        FBuffer.WriteBuffer(AMsg.Text[1], LTextSize);
      if AMsg.Data <> nil then
      begin
        LDataSize := AMsg.Data.Size;
        FBuffer.WriteBuffer(LDataSize);
        AMsg.Data.Position := 0;
        FBuffer.CopyFrom(AMsg.Data, LDataSize);
      end
      else
        FBuffer.WriteBuffer(ZERO_BUF); // indicates empty stream
      FClient.SendStream(FBuffer);
      Result := True;
    end;
  end;
end;
{$ENDREGION}

end.
