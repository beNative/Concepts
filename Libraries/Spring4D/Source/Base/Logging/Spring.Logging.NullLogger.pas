{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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

unit Spring.Logging.NullLogger;

interface

uses
  Rtti,
  SysUtils,
  Spring.Logging;

type
  {$REGION 'TNullLogger'}

  /// <summary>
  ///   Logger that does nothing and does it in fastes way possible.
  /// </summary>
  TNullLogger = class(TInterfacedObject, ILogger)
  private
    class var fGlobalInstance: ILogger;
    class constructor Create;

    function GetEnabled: Boolean;
    function GetEventTypes: TLogEventTypes;
    function GetLevels: TLogLevels;
  public
    function IsEnabled(level: TLogLevel; eventTypes: TLogEventTypes): Boolean; inline;
    function IsFatalEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsWarnEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsTextEnabled: Boolean;
    function IsDebugEnabled: Boolean;
    function IsTraceEnabled: Boolean;

    procedure Log(const event: TLogEvent); overload;

    procedure LogValue(const name: string; const value: TValue); overload;
    procedure LogValue(level: TLogLevel; const name: string;
      const value: TValue); overload;

    procedure Log(const msg: string); overload;
    procedure Log(const msg: string; const e: Exception); overload;
    procedure Log(const fmt: string; const args: array of const); overload;
    procedure Log(const fmt: string;
      const args: array of const; const e: Exception); overload;

    procedure Log(level: TLogLevel; const msg: string); overload;
    procedure Log(level: TLogLevel; const msg: string;
      const e: Exception); overload;
    procedure Log(level: TLogLevel; const fmt: string;
      const args: array of const); overload;
    procedure Log(level: TLogLevel; const fmt: string;
      const args: array of const; const e: Exception); overload;

    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; const e: Exception); overload;
    procedure Fatal(const fmt: string; const args: array of const); overload;
    procedure Fatal(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; const e: Exception); overload;
    procedure Error(const fmt: string; const args: array of const); overload;
    procedure Error(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Warn(const msg: string); overload;
    procedure Warn(const msg: string; const e: Exception); overload;
    procedure Warn(const fmt: string; const args: array of const); overload;
    procedure Warn(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Info(const msg: string); overload;
    procedure Info(const msg: string; const e: Exception); overload;
    procedure Info(const fmt: string; const args: array of const); overload;
    procedure Info(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Text(const msg: string); overload;
    procedure Text(const msg: string; const e: Exception); overload;
    procedure Text(const fmt: string; const args: array of const); overload;
    procedure Text(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Debug(const msg: string); overload;
    procedure Debug(const msg: string; const e: Exception); overload;
    procedure Debug(const fmt: string; const args: array of const); overload;
    procedure Debug(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Trace(const msg: string); overload;
    procedure Trace(const msg: string; const e: Exception); overload;
    procedure Trace(const fmt: string; const args: array of const); overload;
    procedure Trace(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Enter(const methodName: string); overload;
    procedure Enter(const classType: TClass;
      const methodName: string); overload;
    procedure Enter(const instance: TObject;
      const methodName: string); overload;
    procedure Enter(level: TLogLevel; const classType: TClass;
      const methodName: string); overload;

    procedure Leave(const methodName: string); overload;
    procedure Leave(const classType: TClass;
      const methodName: string); overload;
    procedure Leave(const instance: TObject;
      const methodName: string); overload;
    procedure Leave(level: TLogLevel; const classType: TClass;
      const methodName: string); overload;

    function Track(const classType: TClass;
      const methodName: string): IInterface; overload;
    function Track(const instance: TObject;
      const methodName: string): IInterface; overload;
    function Track(level: TLogLevel; const classType: TClass;
      const methodName: string): IInterface; overload;

    class property GlobalInstance: ILogger read fGlobalInstance;
  end;

  {$ENDREGION}


implementation


{$REGION 'TNullLogger'}

class constructor TNullLogger.Create;
begin
  fGlobalInstance := TNullLogger.Create;
end;

procedure TNullLogger.Log(const event: TLogEvent);
begin //FI:W519
end;

procedure TNullLogger.Log(level: TLogLevel; const msg: string);
begin //FI:W519
end;

procedure TNullLogger.Log(level: TLogLevel; const fmt: string;
  const args: array of const);
begin //FI:W519
end;

procedure TNullLogger.Log(level: TLogLevel; const msg: string;
  const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.Debug(const fmt: string; const args: array of const);
begin //FI:W519
end;

procedure TNullLogger.Debug(const fmt: string; const args: array of const;
  const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.Debug(const msg: string);
begin //FI:W519
end;

procedure TNullLogger.Debug(const msg: string; const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.Error(const fmt: string; const args: array of const);
begin //FI:W519
end;

procedure TNullLogger.Enter(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin //FI:W519
end;

procedure TNullLogger.Enter(const methodName: string);
begin //FI:W519
end;

procedure TNullLogger.Enter(const classType: TClass;
  const methodName: string);
begin //FI:W519
end;

procedure TNullLogger.Enter(const instance: TObject;
  const methodName: string);
begin //FI:W519
end;

procedure TNullLogger.Error(const fmt: string; const args: array of const;
  const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.Error(const msg: string);
begin //FI:W519
end;

procedure TNullLogger.Error(const msg: string; const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.Fatal(const fmt: string; const args: array of const);
begin //FI:W519
end;

procedure TNullLogger.Fatal(const fmt: string; const args: array of const;
  const e: Exception);
begin //FI:W519
end;

function TNullLogger.GetEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.GetEventTypes: TLogEventTypes;
begin
  Result := [];
end;

function TNullLogger.GetLevels: TLogLevels;
begin
  Result := [];
end;

procedure TNullLogger.Fatal(const msg: string);
begin //FI:W519
end;

procedure TNullLogger.Fatal(const msg: string; const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.Info(const msg: string; const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.Info(const msg: string);
begin //FI:W519
end;

procedure TNullLogger.Info(const fmt: string; const args: array of const;
  const e: Exception);
begin //FI:W519
end;

function TNullLogger.IsDebugEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.IsEnabled(level: TLogLevel; eventTypes: TLogEventTypes): Boolean; //FI:O804
begin
  Result := False;
end;

function TNullLogger.IsErrorEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.IsFatalEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.IsInfoEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.IsTextEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.IsTraceEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.IsWarnEnabled: Boolean;
begin
  Result := False;
end;

procedure TNullLogger.Info(const fmt: string; const args: array of const);
begin //FI:W519
end;

procedure TNullLogger.Leave(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin //FI:W519
end;

procedure TNullLogger.Leave(const methodName: string);
begin //FI:W519
end;

procedure TNullLogger.Log(const msg: string);
begin //FI:W519
end;

procedure TNullLogger.Log(const msg: string; const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.Log(const fmt: string; const args: array of const);
begin //FI:W519
end;

procedure TNullLogger.Log(const fmt: string; const args: array of const;
  const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.Leave(const instance: TObject;
  const methodName: string);
begin //FI:W519
end;

procedure TNullLogger.Leave(const classType: TClass;
  const methodName: string);
begin //FI:W519
end;

procedure TNullLogger.Log(level: TLogLevel; const fmt: string;
  const args: array of const; const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.LogValue(level: TLogLevel; const name: string;
  const value: TValue);
begin //FI:W519
end;

procedure TNullLogger.LogValue(const name: string; const value: TValue);
begin //FI:W519
end;

procedure TNullLogger.Text(const fmt: string; const args: array of const);
begin //FI:W519
end;

procedure TNullLogger.Text(const fmt: string; const args: array of const;
  const e: Exception);
begin //FI:W519
end;

function TNullLogger.Track(const instance: TObject; const methodName: string): IInterface; //FI:O804
begin
  Result := nil;
end;

function TNullLogger.Track(const classType: TClass; const methodName: string): IInterface; //FI:O804
begin
  Result := nil;
end;

function TNullLogger.Track(level: TLogLevel; const classType: TClass; const methodName: string): IInterface; //FI:O804
begin
  Result := nil;
end;

procedure TNullLogger.Text(const msg: string);
begin //FI:W519
end;

procedure TNullLogger.Text(const msg: string; const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.Trace(const fmt: string; const args: array of const);
begin //FI:W519
end;

procedure TNullLogger.Trace(const fmt: string; const args: array of const;
  const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.Trace(const msg: string);
begin //FI:W519
end;

procedure TNullLogger.Trace(const msg: string; const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.Warn(const fmt: string; const args: array of const);
begin //FI:W519
end;

procedure TNullLogger.Warn(const fmt: string; const args: array of const;
  const e: Exception);
begin //FI:W519
end;

procedure TNullLogger.Warn(const msg: string);
begin //FI:W519
end;

procedure TNullLogger.Warn(const msg: string; const e: Exception);
begin //FI:W519
end;

{$ENDREGION}


end.
