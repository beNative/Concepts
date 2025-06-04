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
unit NATS.Parser;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Rtti, System.Generics.Collections,

  Nats.Entities,
  Nats.Classes;

type
  TNatsParser = class
  private
    function ParseINFO(const ACommand: string): TNatsCommand;
    function ParseMSG(const ACommand: string): TNatsCommand;
  public
    function Parse(const ACommand: string): TNatsCommand;
    function ParsePayload(const ACommand: TNatsCommand; APayload: string): TNatsCommand;
  end;

implementation

uses
  Nats.Consts,
  Nats.Exceptions;

{ TNatsParser }

function TNatsParser.Parse(const ACommand: string): TNatsCommand;
var
  LSplit: TArray<string>;
begin
  if ACommand.Trim.StartsWith(NatsConstants.Protocol.INFO) then
  begin
    Exit(ParseINFO(ACommand));
  end;

  if ACommand.Trim.StartsWith(NatsConstants.Protocol.MSG) then
  begin
    Exit(ParseMSG(ACommand));
  end;

  if ACommand.Trim.StartsWith(NatsConstants.Protocol.PING) then
  begin
    Result.CommandType := TNatsCommandServer.PING;
    Exit(Result);
  end;

  if ACommand.Trim.StartsWith(NatsConstants.Protocol.PONG) then
  begin
    Result.CommandType := TNatsCommandServer.PONG;
    Exit(Result);
  end;

  if ACommand.Trim.StartsWith(NatsConstants.Protocol.OK) then
  begin
    Result.CommandType := TNatsCommandServer.OK;
    Exit(Result);
  end;

  if ACommand.Trim.StartsWith(NatsConstants.Protocol.ERR) then
  begin
    Result.CommandType := TNatsCommandServer.ERR;
    Exit(Result);
  end;

  raise ENatsException.Create('Parsing error or NATS command not supported');
end;

function TNatsParser.ParseINFO(const ACommand: string): TNatsCommand;
var
  LSplit: TArray<string>;
  LArg: TNatsArgsINFO;
begin
  Result.CommandType := TNatsCommandServer.INFO;
  LSplit := ACommand.Split([NatsConstants.SPC]);
  if Length(LSplit) < 2 then
    raise ENatsException.Create('Malformed NATS command received (INFO)');

  LArg.InfoStr := LSplit[1];
  Result.Arguments := TValue.From<TNatsArgsINFO>(LArg);
end;

function TNatsParser.ParseMSG(const ACommand: string): TNatsCommand;
var
  LSplit: TArray<string>;
  LArg: TNatsArgsMSG;
begin
  Result.CommandType := TNatsCommandServer.MSG;
  LSplit := ACommand.Split([NatsConstants.SPC]);
  case Length(LSplit) of
    4:
    begin
      LArg.Subject := LSplit[1];
      LArg.Id := LSplit[2].ToInteger;
      LArg.ReplyTo := '';
      LArg.Bytes := LSplit[3].ToInteger;
    end;
    5:
    begin
      LArg.Subject := LSplit[1];
      LArg.Id := LSplit[2].ToInteger;
      LArg.ReplyTo := LSplit[3];
      LArg.Bytes := LSplit[4].ToInteger;
    end;
  else
    raise ENatsException.Create('Malformed NATS command received (MSG)');
  end;
  Result.Arguments := TValue.From<TNatsArgsMSG>(LArg);
end;

function TNatsParser.ParsePayload(const ACommand: TNatsCommand; APayload: string): TNatsCommand;
var
  LArg: TNatsArgsMSG;
begin
  LArg := ACommand.Arguments.AsType<TNatsArgsMSG>;
  LArg.Payload := APayload;
  Result.CommandType := ACommand.CommandType;
  Result.Arguments := TValue.From<TNatsArgsMSG>(LArg);
end;

end.
