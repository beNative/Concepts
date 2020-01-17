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
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
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

    property Port: Integer
      read GetPort write SetPort default ZMQ_DEFAULT_PORT;

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
function TZeroMQDevice.GetName: string;
begin
  Result := FName;
end;

function TZeroMQDevice.GetPort: Integer;
begin
  Result := FPort;
end;

procedure TZeroMQDevice.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TZeroMQDevice.SetPort(const Value: Integer);
begin
  FPort := Value;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TZeroMQDevice.ToString: string;
begin
//
end;
{$ENDREGION}

end.
