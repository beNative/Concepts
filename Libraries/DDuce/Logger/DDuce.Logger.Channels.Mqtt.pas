{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Logger.Channels.MQTT;

interface

{ Channel holding a MQTT publisher socket where one or more Logviewers can
  subscribe to.

  The channel connects to a MQTT broker (like Mosquitto).
}

uses
  System.Classes, System.SysUtils,

  Spring, MQTT,

  DDuce.Logger.Interfaces, DDuce.Logger.Channels.Base;

type
  TMQTTChannel = class(TCustomLogChannel, ILogChannel, IMQTTChannel)
  private
    FBuffer : TStringStream;
    FMQTT   : Lazy<TMQTT>;
    FPort   : Integer;
    FBroker : string;

  protected
    {$REGION 'property access methods'}
    function GetMQTT: TMQTT;
    function GetBroker: string;
    procedure SetBroker(const Value: string);
    function GetPort: Integer; override;
    function GetConnected: Boolean; override;
    {$ENDREGION}

    property Broker: string
      read GetBroker write SetBroker;

    property MQTT: TMQTT
      read GetMQTT;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    constructor Create(
      const ABroker : string = '';
      APort         : Integer = 1883;
      AActive       : Boolean = True
    ); reintroduce; overload; virtual;

    function Connect: Boolean; override;
    function Disconnect: Boolean; override;

    function Write(const AMsg: TLogMessage): Boolean; override;

  end;

implementation

uses
  Spring.Helpers, Spring.SystemUtils;

{$REGION 'construction and destruction'}
procedure TMQTTChannel.AfterConstruction;
begin
  inherited AfterConstruction;
  if Enabled then
  begin
    Connect;
  end;
end;

procedure TMQTTChannel.BeforeDestruction;
begin
  if FMQTT.IsValueCreated then
    FMQTT.Value.Free; // we need to do an explicit call to Free
  FBuffer.Free;
  inherited BeforeDestruction;
end;

constructor TMQTTChannel.Create(const ABroker: string; APort: Integer;
  AActive: Boolean);
begin
  inherited Create(AActive);
  FPort   := APort;
  FBroker := ABroker;
  FBuffer := TStringStream.Create;
  FMQTT.Create(function: TMQTT
    begin
      Result := TMQTT.Create(FBroker, FPort);
  // some brokers require these to have a value
      Result.WillTopic := 'a';
      Result.WillMsg   := 'a';
    end
  );
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TMQTTChannel.GetBroker: string;
begin
  Result := FBroker;
end;

procedure TMQTTChannel.SetBroker(const Value: string);
begin
  if Value <> Broker then
  begin
    FBroker := Value;
  end;
end;

function TMQTTChannel.GetConnected: Boolean;
begin
  Result := MQTT.Connected;
end;

function TMQTTChannel.GetMQTT: TMQTT;
begin
  Result := FMQTT.Value;
end;

function TMQTTChannel.GetPort: Integer;
begin
  Result := FPort;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TMQTTChannel.Connect: Boolean;
begin
  Result := MQTT.Connect;
end;

function TMQTTChannel.Disconnect: Boolean;
begin
  Result := MQTT.Disconnect;
end;

function TMQTTChannel.Write(const AMsg: TLogMessage): Boolean;
const
  ZeroBuf: Integer = 0;
var
  TextSize : Integer;
  DataSize : Integer;
begin
  if Enabled then
  begin
    if not Connected and AutoConnect then
      Connect;
    if Connected then
    begin
      TextSize := Length(AMsg.Text);
      FBuffer.Seek(0, soFromBeginning);
      FBuffer.WriteBuffer(AMsg.MsgType);
      FBuffer.WriteBuffer(AMsg.LogLevel);
      FBuffer.WriteBuffer(AMsg.Reserved1);
      FBuffer.WriteBuffer(AMsg.Reserved2);
      FBuffer.WriteBuffer(AMsg.TimeStamp);
      FBuffer.WriteBuffer(TextSize);
      if TextSize > 0 then
      begin
        FBuffer.WriteBuffer(AMsg.Text[1], TextSize);
      end;
      if AMsg.Data <> nil then
      begin
        DataSize := AMsg.Data.Size;
        FBuffer.WriteBuffer(DataSize);
        AMsg.Data.Position := 0;
        FBuffer.CopyFrom(AMsg.Data, DataSize);
      end
      else
        FBuffer.WriteBuffer(ZeroBuf);
      Result := MQTT.Publish('Test', UTF8Encode(FBuffer.DataString));
    end
    else
    begin
      Result := False;
    end;
  end
  else
  begin
    Result := False;
  end;
end;
{$ENDREGION}

end.
