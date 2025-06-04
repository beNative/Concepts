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
unit Nats.Entities;

interface

uses
  System.SysUtils, System.JSON.Serializers,

  Nats.Consts;

type
  TNatsServerInfo = record
    server_id: string;
    server_name: string;
    version: string;
    proto: Integer;
    git_commit: string;
    go: string;
    host: string;
    port: Integer;
    headers: Boolean;
    auth_required: Boolean;
    tls_required: Boolean;
    tls_available: Boolean;
    max_payload: Integer;
    jetstream: Boolean;
    client_id: Integer;
    client_ip: string;
    nonce: string;

    class function FromJSONString(const AValue: string): TNatsServerInfo; static;
  end;

  TNatsConnectOptions = record
  public
    verbose: Boolean;
    pedantic: Boolean;
    tls_required: Boolean;
    auth_token: string;
    user: string;
    pass: string;
    name: string;
    lang: string;
    version: string;
    protocol: Integer;
    echo: Boolean;
    sig: string;
    jwt: string;

    function ToJSONString: string;
    class function FromJSONString(const AValue: string): TNatsConnectOptions; static;
  end;

implementation

uses
  System.JSON, REST.Json;

{ TNatsServerInfo }

class function TNatsServerInfo.FromJSONString(const AValue: string): TNatsServerInfo;
var
  LSer: TJsonSerializer;
begin
  LSer := TJsonSerializer.Create;
  try
    Result := LSer.Deserialize<TNatsServerInfo>(AValue);
  finally
    LSer.Free;
  end;
end;

{ TNatsConnectOptions }

class function TNatsConnectOptions.FromJSONString(const AValue: string): TNatsConnectOptions;
var
  LSer: TJsonSerializer;
begin
  LSer := TJsonSerializer.Create;
  try
    Result := LSer.Deserialize<TNatsConnectOptions>(AValue);
  finally
    LSer.Free;
  end;
end;

function TNatsConnectOptions.ToJSONString: string;
var
  LSer: TJsonSerializer;
begin
  LSer := TJsonSerializer.Create;
  try
    Result := LSer.Serialize<TNatsConnectOptions>(Self);
  finally
    LSer.Free;
  end;
end;

end.
