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
unit Nats.Classes;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Rtti,
  System.Generics.Collections,

  Nats.Entities;

type
  TNatsCommandClient = (CONNECT, PING, PONG, PUB, SUB, UNSUB);
  TNatsCommandServer = (INFO, PING, PONG, MSG, OK, ERR);

  TNatsArgsINFO = record
  private
    FInfoStr: string;
    procedure SetInfoStr(const Value: string);
  public
    Info: TNatsServerInfo;
    property InfoStr: string read FInfoStr write SetInfoStr;
  end;

  TNatsArgsMSG = record
    Id: Integer;
    Subject: string;
    ReplyTo: string;
    Bytes: Integer;
    Payload: string;
  end;

  TNatsCommand = record
    CommandType: TNatsCommandServer;
    Arguments: TValue;

    function GetArgAsInfo: TNatsArgsINFO;
    function GetArgAsMsg: TNatsArgsMSG;
  end;

  TNatsCommandQueue = class(TQueue<TNatsCommand>)
  end;

  TNatsMsgHandler = reference to procedure (const AMsg: TNatsArgsMSG);
  TNatsPingHandler = reference to procedure ();
  TNatsConnectHandler = reference to procedure (AInfo: TNatsServerInfo; var AConnectOptions: TNatsConnectOptions);
  TNatsDisconnectHandler = reference to procedure ();

  TNatsThread = class abstract(TThread)
  protected
    FStopEvent: TLightweightEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Stop;
  end;


implementation

{ TNatsThread }

constructor TNatsThread.Create;
begin
  inherited Create(True);
  FStopEvent := TLightweightEvent.Create;
end;

destructor TNatsThread.Destroy;
begin
  FStopEvent.Free;
  inherited;
end;

procedure TNatsThread.Stop;
begin
  FStopEvent.SetEvent;
  Terminate;
end;

{ TNatsCommand }

function TNatsCommand.GetArgAsInfo: TNatsArgsINFO;
begin
  Result := Arguments.AsType<TNatsArgsINFO>;
end;

function TNatsCommand.GetArgAsMsg: TNatsArgsMSG;
begin
  Result := Arguments.AsType<TNatsArgsMSG>;
end;

procedure TNatsArgsINFO.SetInfoStr(const Value: string);
begin
  FInfoStr := Value;
  Info := TNatsServerInfo.FromJSONString(Value);
end;

end.
