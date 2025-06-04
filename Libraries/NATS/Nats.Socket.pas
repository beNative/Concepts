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
unit Nats.Socket;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  INatsSocket = interface
  ['{B97D9A0E-50CD-40B3-ACA2-CC925FC40FBC}']
    // Property Getters
    function GetConnected: Boolean;
    function GetHost: string;
    function GetPort: Integer;
    function GetTimeout: Cardinal;
    function GetMaxLineLength: Cardinal;
    // Property Setters
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetTimeout(const Value: Cardinal);
    procedure SetMaxLineLength(const Value: Cardinal);
    // Methods
    procedure Open();
    procedure Close();
    procedure SendBytes(const AValue: TBytes);
    procedure SendString(const AValue: string);
    function ReceiveString: string;
    function ReceiveBytes: TBytes;
    // Properties
    property Connected: Boolean read GetConnected;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
    property Timeout: Cardinal read GetTimeout write SetTimeout;
    property MaxLineLength: Cardinal read GetMaxLineLength write SetMaxLineLength;

  end;

  TNatsSocket = class(TInterfacedObject, INatsSocket)
  protected
    function GetConnected: Boolean; virtual; abstract;
    function GetHost: string; virtual; abstract;
    function GetPort: Integer; virtual; abstract;
    function GetTimeout: Cardinal; virtual; abstract;
    function GetMaxLineLength: Cardinal; virtual; abstract;

    procedure SetHost(const Value: string); virtual; abstract;
    procedure SetPort(const Value: Integer); virtual; abstract;
    procedure SetTimeout(const Value: Cardinal); virtual; abstract;
    procedure SetMaxLineLength(const Value: Cardinal); virtual; abstract;
  public
    constructor Create; virtual; abstract;
    procedure Open(); virtual; abstract;
    procedure Close(); virtual; abstract;
    procedure SendBytes(const AValue: TBytes); virtual; abstract;
    procedure SendString(const AValue: string); virtual; abstract;
    function ReceiveString: string; virtual; abstract;
    function ReceiveBytes: TBytes; virtual; abstract;
  end;

  TNatsSocketClass = class of TNatsSocket;

  TNatsSocketRegistry = class(TDictionary<string, TNatsSocketClass>)
  strict private
    class var FInstance: TNatsSocketRegistry;
    class destructor Destroy;
  private
    class function GetInstance: TNatsSocketRegistry; static;
  protected
    FDefaultClass: TNatsSocketClass;
    function GetDefaultClass: TNatsSocketClass;
    procedure SetDefaultClass(AClass: TNatsSocketClass);
  public
    class function Get(const AName: string): INatsSocket; static;
    class procedure Register<T: TNatsSocket>(const AName: string; ADefault: Boolean); static;
  end;


implementation

uses
  Nats.Exceptions;

class destructor TNatsSocketRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TNatsSocketRegistry.GetDefaultClass: TNatsSocketClass;
var
  LClassList: TArray<TPair<string, TNatsSocketClass>>;
begin
  if Self.Count < 1 then
    raise ENatsException.Create('CreateSocket: no socket class registered (add "Nats.Socket.*" unit to the project)');

  if Assigned(FDefaultClass) then
    Exit(FDefaultClass);

  LClassList := Self.ToArray;
  Result := LClassList[0].Value;
end;

class function TNatsSocketRegistry.GetInstance: TNatsSocketRegistry;
var
  LRegistry: TNatsSocketRegistry;
begin
  if FInstance = nil then
  begin
    LRegistry := TNatsSocketRegistry.Create;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LRegistry), nil) <> nil then
      LRegistry.Free
  end;
  Result := FInstance;
end;

class function TNatsSocketRegistry.Get(const AName: string): INatsSocket;
var
  LClass: TNatsSocketClass;
  LSelf: TNatsSocketRegistry;
begin
  LSelf := GetInstance;
  if LSelf.Count < 1 then
    raise ENatsException.Create('Get: no socket class registered (add "Nats.Socket.*" unit to the project)');

  if AName = '' then
    LClass := LSelf.GetDefaultClass()
  else if not LSelf.TryGetValue(AName, LClass) then
    raise ENatsException.CreateFmt('Get: socket class [%s] not registered (add "Nats.Socket.*" unit to the project)', [AName]);

  Result := LClass.Create;
end;

class procedure TNatsSocketRegistry.Register<T>(const AName: string; ADefault: Boolean);
var
  LSelf: TNatsSocketRegistry;
begin
  LSelf := GetInstance;
  LSelf.Add(AName, TNatsSocketClass(T));
  if ADefault then
    LSelf.SetDefaultClass(TNatsSocketClass(T));
end;

procedure TNatsSocketRegistry.SetDefaultClass(AClass: TNatsSocketClass);
begin
  FDefaultClass := AClass;
end;

end.
