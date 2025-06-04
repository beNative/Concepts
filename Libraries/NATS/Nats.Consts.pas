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
unit Nats.Consts;

interface

uses
  System.SysUtils;

type

  NatsConstants = class
  public const

    VERSION = '0.1.0';

    DEFAULT_PORT = 4222;
    DEFAULT_URI_ = 'nats://localhost';

    // Parser status
    AWAITING_CONTROL = 0;
    AWAITING_MSG_PAYLOAD = 1;

    // Connection status
    OPEN = 0;
    CLOSE = 1;
    RECONNECT = 2;

    // TLS parameters
    DEFAULT_KEYSTORE = './keystore';
    DEFAULT_TRUSTSTORE = './truststore';
    DEFAULT_PASSWORD = 'password';
    TLS_REQUIRED = 'tls_required';
    TLS_VERSION = 'TLSv1.2';

    // Reconnect Parameters, 2 sec wait, 10 tries
    DEFAULT_RECONNECT_TIME_WAIT = 2*1000;
    DEFAULT_MAX_RECONNECT_ATTEMPTS = 3;
    DEFAULT_PING_INTERVAL = 4*1000;

    CR_LF = #13#10;
    TAB = #9;
    CR_LF_LEN = 2;
    EMPTY = '';
    SPC = ' ';
    COL = ':';
    SEP = '.';
    WC = '*';
    ARR = '>';

    public type Protocol = class
    const
      // Protocol
      PUB = 'PUB';
      SUB = 'SUB';
      UNSUB = 'UNSUB';
      CONNECT = 'CONNECT';
      MSG = 'MSG';
      PONG = 'PONG';
      PING = 'PING';
      INFO = 'INFO';
      ERR = '-ERR';
      OK = '+OK';
      UNKNOWN = 'UNKNOWN';
    end;

    class function DEFAULT_URI: string; static;
  end;



implementation

{ NATSConsts }

class function NatsConstants.DEFAULT_URI: string;
begin
  Result := DEFAULT_URI_ + ':' + DEFAULT_PORT.ToString;
end;

end.
