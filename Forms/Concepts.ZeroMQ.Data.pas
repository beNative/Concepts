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

{$I Concepts.inc}

unit Concepts.ZeroMQ.Data;

interface

uses
  System.Classes, System.SysUtils,

  ZeroMQ;

const
  ZMQ_DEFAULT_PORT = 5555;

type
  TZeroMQDevice = class(TInterfacedObject)
  private
    FZMQ       : IZeroMQ;
    FPair      : IZMQPair;
    FPort      : Integer;
    FName      : string;

    {$REGION 'property access methods'}
    function GetConnectionString: string;
    function GetEvents: ZMQEvents;
    function GetPort: Integer;
    function GetSocketType: ZMQSocket;
    function GetTransport: string;
    procedure SetEvents(const Value: ZMQEvents);
    procedure SetPort(const Value: Integer);
    procedure SetSocketType(const Value: ZMQSocket);
    procedure SetTransport(const Value: string);
    function GetName: string;
    procedure SetName(const Value: string);
    {$ENDREGION}

  public
    procedure AfterConstruction; override;
    constructor Create(AZMQ: IZeroMQ);

    function ToString: string; override;

    property ZMQ: IZeroMQ
      read FZMQ write FZMQ;

    property Pair: IZMQPair
      read FPair write FPair;

    property SocketType: ZMQSocket
      read GetSocketType write SetSocketType;

    property Port: Integer
      read GetPort write SetPort default ZMQ_DEFAULT_PORT;

    property Transport: string
      read GetTransport write SetTransport;

    property ConnectionString: string
      read GetConnectionString;

    { Set of all event types to monitor. }
    property Events: ZMQEvents
      read GetEvents write SetEvents;

    property Name: string
      read GetName write SetName;
  end;

const
  ZMQTransports : array[0..4] of string = (
    'tcp',
    'inproc', // every connection needs to share the same IZeroMQ
    'ipc',
    'pgm',
    'egm'
  );
  ZMQEventNames : array[ZMQEvent] of string = (
    'Connected',
    'Delayed',
    'Retried',
    'Listening',
    'BindFailed',
    'Accepted',
    'AcceptFailed',
    'Closed',
    'CloseFailed',
    'Disconnected',
    'MonitorStopped'
  );

implementation

{$REGION 'construction and destruction'}
procedure TZeroMQDevice.AfterConstruction;
begin
  inherited AfterConstruction;
  FPort := ZMQ_DEFAULT_PORT;
end;

constructor TZeroMQDevice.Create(AZMQ: IZeroMQ);
begin
  FZMQ := AZMQ;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TZeroMQDevice.GetConnectionString: string;
begin
  //
end;

function TZeroMQDevice.GetEvents: ZMQEvents;
begin
//
end;

function TZeroMQDevice.GetName: string;
begin
  Result := FName;
end;

function TZeroMQDevice.GetPort: Integer;
begin
  Result := FPort;
end;

function TZeroMQDevice.GetSocketType: ZMQSocket;
begin

end;

function TZeroMQDevice.GetTransport: string;
begin

end;

procedure TZeroMQDevice.SetEvents(const Value: ZMQEvents);
begin

end;

procedure TZeroMQDevice.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TZeroMQDevice.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

procedure TZeroMQDevice.SetSocketType(const Value: ZMQSocket);
begin

end;

procedure TZeroMQDevice.SetTransport(const Value: string);
begin

end;
{$ENDREGION}

{$REGION 'public methods'}
function TZeroMQDevice.ToString: string;
begin
//
end;
{$ENDREGION}

end.
