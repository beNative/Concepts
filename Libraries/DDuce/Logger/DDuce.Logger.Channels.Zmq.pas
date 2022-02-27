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

unit DDuce.Logger.Channels.Zmq;

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
  DEFAULT_ENDPOINT = 'tcp://*:5555';

type
  TZmqChannel = class(TCustomLogChannel, ILogChannel, IZmqChannel)
  private
    FBuffer    : TStringStream;
    FZmq       : IZeroMQ;
    FPublisher : IZMQPair;
    FPort      : Integer;
    FEndPoint  : string;
    FBound     : Boolean;

  protected
    {$REGION 'property access methods'}
    function GetZmqVersion: string;
    function GetEnabled: Boolean; override;
    function GetEndPoint: string;
    procedure SetEndPoint(const Value: string);
    function GetPort: Integer; override;
    function GetConnected: Boolean; override;
    {$ENDREGION}

    { The endpoint argument is a string consisting of two parts as follows:
      transport ://address. The transport part specifies the underlying
      transport protocol to use. The meaning of the address part is specific to
      the underlying transport protocol selected.
      Currently only tcp is supported as transport. }
    property EndPoint : string
      read GetEndPoint write SetEndPoint;

    property ZmqVersion: string
      read GetZmqVersion;

  public
    procedure AfterConstruction; override;
    constructor Create(
      const AEndPoint : string = '';
      AActive         : Boolean = True
    ); reintroduce; overload; virtual;
    destructor Destroy; override;

    function Connect: Boolean; override;
    function Disconnect: Boolean; override;

    function Write(const AMsg: TLogMessage): Boolean; override;

  end;

implementation

uses
  Spring, Spring.Helpers, Spring.SystemUtils,

  ZeroMQ.API;

{$REGION 'construction and destruction'}
constructor TZmqChannel.Create(const AEndPoint: string; AActive: Boolean);
begin
  inherited Create(AActive);
  FEndPoint := AEndPoint;
end;

procedure TZmqChannel.AfterConstruction;
begin
  inherited AfterConstruction;
  Guard.CheckTrue(
    FileExists(LIBZEROMQ),
    Format('ZeroMQ library (%s) is missing!', [LIBZEROMQ])
  );
  FBuffer := TStringStream.Create('', TEncoding.ANSI);
  FZmq    := TZeroMQ.Create;
  if FEndPoint = '' then
  begin
    FEndPoint := DEFAULT_ENDPOINT;
  end;
  if Enabled then
  begin
    FBound := Connect;
  end;
end;

destructor TZmqChannel.Destroy;
begin
  if Assigned(FBuffer) then
  begin
    FBuffer.Free;
  end;
  if Assigned(FPublisher) then
  begin
    FPublisher.Close;
    FPublisher := nil;
  end;
  FZmq := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TZmqChannel.GetEnabled: Boolean;
begin
  Result := Assigned(FZmq) and inherited GetEnabled;
end;

function TZmqChannel.GetConnected: Boolean;
begin
  Result := FBound;
end;

function TZmqChannel.GetEndPoint: string;
begin
  Result := FEndPoint;
end;

procedure TZmqChannel.SetEndPoint(const Value: string);
begin
  if Value <> EndPoint then
  begin
    FEndPoint := Value;
  end;
end;

function TZmqChannel.GetPort: Integer;
begin
  Result := FPort;
end;

function TZmqChannel.GetZmqVersion: string;
var
  LMajor : Integer;
  LMinor : Integer;
  LPatch : Integer;
begin
  zmq_version(@LMajor, @LMinor, @LPatch);
  Result := Format('%d.%d.%d', [LMajor, LMinor, LPatch]);
end;
{$ENDREGION}

{$REGION 'public methods'}
{ Returns true if successful. }

function TZmqChannel.Connect: Boolean;
var
  S : string;
  A : TStringDynArray;
begin
  if Enabled then
  begin
    FBuffer.Clear;
    FPublisher := FZmq.Start(ZMQSocket.Publisher);
    FBound := FPublisher.Bind(FEndPoint) = 0; // 0 when successful
    // Physical connection is always a bit after Connected reports True.
    // https://stackoverflow.com/questions/11634830/zeromq-always-loses-the-first-message
    if Connected then
    begin
      Sleep(100);
    end;
  end;
  if Connected then
  begin
    S := FPublisher.LastEndPoint;
    A := SplitString(S, [':']);
    FPort := StrToIntDef(A[High(A)], 0);
    FEndPoint := S;
  end;
  Result := Connected;
end;

{ Returns True if successful. }

function TZmqChannel.Disconnect: Boolean;
begin
  Result := True;
  if Connected then
  begin
    FBuffer.Clear;
    Result := FPublisher.Close;
    FPublisher := nil;
    FBound := False;
  end;
end;

function TZmqChannel.Write(const AMsg: TLogMessage): Boolean;
const
  ZERO_BUFFER : Integer = 0;
var
  LTextSize : Integer;
  LDataSize : Integer;
begin
  if Enabled then
  begin
    if not Connected and AutoConnect then
      Connect;
    if Connected then
    begin
      FBuffer.Clear;
      FBuffer.Size := 0;
      LTextSize := Length(AMsg.Text);
      FBuffer.Seek(0, soFromBeginning);
      FBuffer.WriteBuffer(AMsg.MsgType);
      FBuffer.WriteBuffer(AMsg.LogLevel);
      FBuffer.WriteBuffer(AMsg.Reserved1);
      FBuffer.WriteBuffer(AMsg.Reserved2);
      FBuffer.WriteBuffer(AMsg.TimeStamp);
      FBuffer.WriteBuffer(LTextSize);
      if LTextSize > 0 then
      begin
        FBuffer.WriteBuffer(AMsg.Text[1], LTextSize);
      end;
      if AMsg.Data <> nil then
      begin
        LDataSize := AMsg.Data.Size;
        FBuffer.WriteBuffer(LDataSize);
        AMsg.Data.Position := 0;
        FBuffer.CopyFrom(AMsg.Data, LDataSize);
      end
      else
        FBuffer.WriteBuffer(ZERO_BUFFER);
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
