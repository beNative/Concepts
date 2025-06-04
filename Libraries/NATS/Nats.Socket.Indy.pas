{******************************************************************************}
{                                                                              }
{  NATS.Delphi: Delphi Client Library for NATS                                 }
{  Copyright (c) 2022 Paolo Rossi                                              }
{  https://github.com/paolo-rossi/nats.delphi                                  }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit Nats.Socket.Indy;

interface

uses
  System.SysUtils, IdTCPClient, IdTCPConnection, IdGlobal,

  Nats.Consts,
  Nats.Socket;

type
  TNatsSocketIndy = class(TNatsSocket)
  private
    FClient: TIdTCPClient;
  protected
    function GetConnected: Boolean; override;
    function GetHost: string; override;
    function GetPort: Integer; override;
    function GetTimeout: Cardinal; override;
    function GetMaxLineLength: Cardinal; override;
    procedure SetHost(const Value: string); override;
    procedure SetPort(const Value: Integer); override;
    procedure SetTimeout(const Value: Cardinal); override;
    procedure SetMaxLineLength(const Value: Cardinal); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Open(); override;
    procedure Close(); override;

    procedure SendBytes(const AValue: TBytes); override;
    procedure SendString(const AValue: string); override;

    function ReceiveString: string; override;
    function ReceiveBytes: TBytes; override;
  end;


implementation


{ TNatsSocketIndy }

procedure TNatsSocketIndy.Close;
begin
  FClient.Disconnect;
end;

constructor TNatsSocketIndy.Create;
begin
  FClient := TIdTCPClient.Create(nil);
end;

destructor TNatsSocketIndy.Destroy;
begin
  FClient.Free;
  inherited;
end;

function TNatsSocketIndy.GetConnected: Boolean;
begin
  Result := FClient.Connected;
end;

function TNatsSocketIndy.GetHost: string;
begin
  Result := FClient.Host;
end;

function TNatsSocketIndy.GetPort: Integer;
begin
  Result := FClient.Port;
end;

function TNatsSocketIndy.GetTimeout: Cardinal;
begin
  Result := FClient.ReadTimeout;
end;

function TNatsSocketIndy.GetMaxLineLength: Cardinal;
begin
  Result := FClient.IOHandler.MaxLineLength;
end;

procedure TNatsSocketIndy.Open;
begin
  FClient.Connect;
end;

function TNatsSocketIndy.ReceiveBytes: TBytes;
var
  LRes: string;
begin
  LRes := FClient.IOHandler.ReadLn(IndyTextEncoding_UTF8);
  Result := TEncoding.UTF8.GetBytes(LRes);
end;

function TNatsSocketIndy.ReceiveString: string;
begin
  Result := FClient.IOHandler.ReadLn(NatsConstants.CR_LF, IndyTextEncoding_UTF8);
end;

procedure TNatsSocketIndy.SendBytes(const AValue: TBytes);
begin
  FClient.IOHandler.Write(TIdBytes(AValue));
  FClient.IOHandler.Write(NatsConstants.CR_LF);
end;

procedure TNatsSocketIndy.SendString(const AValue: string);
begin
  FClient.IOHandler.Write(AValue, IndyTextEncoding_UTF8);
  FClient.IOHandler.Write(NatsConstants.CR_LF);
end;

procedure TNatsSocketIndy.SetHost(const Value: string);
begin
  FClient.Host := Value;
end;

procedure TNatsSocketIndy.SetMaxLineLength(const Value: Cardinal);
begin
  FClient.IOHandler.MaxLineLength := Value;
end;

procedure TNatsSocketIndy.SetPort(const Value: Integer);
begin
  FClient.Port := Value;
end;

procedure TNatsSocketIndy.SetTimeout(const Value: Cardinal);
begin
  FClient.ReadTimeout := Value;
end;

initialization
  TNatsSocketRegistry.Register<TNatsSocketIndy>('Indy', True);

end.
