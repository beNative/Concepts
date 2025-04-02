{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2024 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit TestFireDACConnection;

interface

uses
  DB,
  FireDAC.Comp.Client;

type
  // See TestSQLConnection for more info
  {$RTTI EXPLICIT
    METHODS([vcPrivate..vcPublished])
    PROPERTIES(DefaultPropertyRttiVisibility)
    FIELDS(DefaultFieldRttiVisibility)}
  TTestFDConnection = class(TFDConnection)
  public
    function GetConnected: Boolean; override; abstract;
    procedure RegisterClient(Client: TObject;
      Event: TConnectChangeEvent); override; abstract;
    procedure SetConnected(Value: Boolean); override; abstract;
    procedure UnRegisterClient(Client: TObject); override; abstract;
  end;

implementation

end.
