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
    FActive: Boolean;

  strict protected
    function GetActive: Boolean; virtual;
    procedure SetActive(const Value: Boolean); virtual;
    function GetConnected: Boolean; virtual;
    procedure SetConnected(const Value: Boolean); virtual;

  public
    procedure AfterConstruction; override;

    procedure Clear; virtual; abstract;
    procedure Write(const AMsg: TLogMessage); virtual; abstract;

    function Connect: Boolean; virtual;
    function Disconnect: Boolean; virtual;

    { Indicates that messages from the Logger object will be sent through this
      channel. }
    property Active: Boolean
      read GetActive write SetActive;

    { True when the channel is connected with the server (receiver) instance. }
    property Connected: Boolean
      read GetConnected write SetConnected;
  end;

implementation

{$REGION 'property access methods'}
function TCustomLogChannel.GetActive: Boolean;
begin
  Result := FActive;
end;

function TCustomLogChannel.GetConnected: Boolean;
begin
  Result := False;
end;

procedure TCustomLogChannel.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TCustomLogChannel.SetConnected(const Value: Boolean);
begin
  if Value then
    Connect
  else
    Disconnect;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TCustomLogChannel.AfterConstruction;
begin
  inherited AfterConstruction;
  Active := True;
end;

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
