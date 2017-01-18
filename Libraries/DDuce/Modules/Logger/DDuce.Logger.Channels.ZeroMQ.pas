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

unit DDuce.Logger.Channels.ZeroMQ;

//{$I DDuce.inc}

interface

{ Channel holding a ZeroMQ publisher socket where one or more Logviewers can
  subscribe to.

  The channel creates a PUB socket and binds it to its local IP address.

}

uses
  System.Classes, System.SysUtils,

  ZeroMQ,

  DDuce.Logger.Interfaces, DDuce.Logger.Channels.Base;

const
  DEFAULT_PORT = 5555;

type
  TZeroMQChannel = class(TCustomLogChannel)
  strict private
    FBuffer    : TStringStream;
    FZMQ       : IZeroMQ;
    FPublisher : IZMQPair;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function GetActive: Boolean; override;

    function Write(const AMsg: TLogMessage): Boolean; override;
    function Connect: Boolean; override;
  end;

implementation

uses
  ZeroMQ.API;

{$REGION 'construction and destruction'}
procedure TZeroMQChannel.AfterConstruction;
begin
  inherited AfterConstruction;
  if FileExists(LIBZEROMQ) then
  begin
    FBuffer := TStringStream.Create;
    FZMQ := TZeroMQ.Create;
  end;
end;

procedure TZeroMQChannel.BeforeDestruction;
begin
  if Assigned(FBuffer) then
  begin
    FBuffer.Free;
  end;
  if Assigned(FPublisher)  then
  begin
    FPublisher.Close;
  end;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TZeroMQChannel.GetActive: Boolean;
begin
  Result := Assigned(FZMQ) and inherited GetActive;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TZeroMQChannel.Connect: Boolean;
begin
  if Active then
  begin
    FPublisher := FZMQ.Start(ZMQSocket.Publisher);
    Connected :=
      FPublisher.Bind(Format('tcp://%s:%d', ['*', DEFAULT_PORT])) <> -1;
  end;
  Result := Connected;
end;

function TZeroMQChannel.Write(const AMsg: TLogMessage): Boolean;
const
  ZeroBuf: Integer = 0;
var
  TextSize : Integer;
  DataSize : Integer;
begin
  if Active then
  begin
    if not Connected then
      Connect;
    if Connected then
    begin
      TextSize := Length(AMsg.MsgText);
      FBuffer.Seek(0, soFromBeginning);
      FBuffer.WriteBuffer(AMsg.MsgType, SizeOf(Integer));
      FBuffer.WriteBuffer(AMsg.MsgTime, SizeOf(TDateTime));
      FBuffer.WriteBuffer(TextSize, SizeOf(Integer));
      FBuffer.WriteBuffer(AMsg.MsgText[1], TextSize);
      if AMsg.Data <> nil then
      begin
        DataSize := AMsg.Data.Size;
        FBuffer.WriteBuffer(DataSize, SizeOf(Integer));
        AMsg.Data.Position := 0;
        FBuffer.CopyFrom(AMsg.Data, DataSize);
      end
      else
        FBuffer.WriteBuffer(ZeroBuf, SizeOf(Integer));
      Result := FPublisher.SendString(FBuffer.DataString) > 0;
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
