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

unit DDuce.Logger.Channels.Base;

//{$I DDuce.inc}

interface

uses
  DDuce.Logger.Interfaces;

type
  TCustomLogChannel = class(TInterfacedObject, ILogChannel)
  strict private
    FActive    : Boolean;
    FConnected : Boolean;

  strict protected
    function GetActive: Boolean; virtual;
    procedure SetActive(const Value: Boolean); virtual;
    function GetConnected: Boolean; virtual;
    procedure SetConnected(const Value: Boolean); virtual;

  public
    constructor Create(AActive : Boolean = True); virtual;

    function Write(const AMsg: TLogMessage): Boolean; virtual; abstract;

    function Connect: Boolean; virtual;
    function Disconnect: Boolean; virtual;

    { Indicates that messages from the Logger object will be sent through this
      channel. }
    property Active: Boolean
      read GetActive write SetActive;

    { True when the channel is connected with the receiver instance. A channel
      can only connect when it is set Active first.  }
    property Connected: Boolean
      read GetConnected write SetConnected;
  end;

implementation

{$REGION 'construction and destruction'}
constructor TCustomLogChannel.Create(AActive: Boolean);
begin
  inherited Create;
  Active := AActive;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TCustomLogChannel.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TCustomLogChannel.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

function TCustomLogChannel.GetConnected: Boolean;
begin
  Result := Active and FConnected;
end;

procedure TCustomLogChannel.SetConnected(const Value: Boolean);
begin
  FConnected := Value;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TCustomLogChannel.Connect: Boolean;
begin
  Result := False;
end;

function TCustomLogChannel.Disconnect: Boolean;
begin
  Result := False;
end;
{$ENDREGION}

end.
