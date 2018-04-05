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

unit Spring.Logging.Loggers;

interface

uses
  SysUtils,
  Rtti,
  Spring,
  Spring.Logging;

type
  {$REGION 'TLoggerBase'}

  {$M+}
  TLoggerBase = class abstract(TInterfacedObject, ILoggerBase, ILoggerProperties)
  private
    fDefaultLevel: TLogLevel;
    fEnabled: Boolean;
    fEventTypes: TLogEventTypes;
    fLevels: TLogLevels;

    function GetDefaultLevel: TLogLevel;
    function GetEnabled: Boolean;
    function GetEventTypes: TLogEventTypes;
    function GetLevels: TLogLevels;

    procedure SetDefaultLevel(value: TLogLevel);
    procedure SetEnabled(value: Boolean);
    procedure SetEventTypes(value: TLogEventTypes);
    procedure SetLevels(value: TLogLevels);
  public
    constructor Create;

    function IsEnabled(level: TLogLevel;
      eventTypes: TLogEventTypes = [TLogEventType.Text]): Boolean; inline;
    function IsFatalEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsWarnEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsTextEnabled: Boolean;
    function IsDebugEnabled: Boolean;
    function IsTraceEnabled: Boolean;
  published
    property DefaultLevel: TLogLevel read GetDefaultLevel write SetDefaultLevel;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property EventTypes: TLogEventTypes read GetEventTypes write SetEventTypes;
    property Levels: TLogLevels read GetLevels write SetLevels;
  end;
  {$M-}

  {$ENDREGION}


  {$REGION 'TAbstractLogger'}

  TAbstractLogger = class abstract(TLoggerBase, ILogger, ILogAppender)
  private
    type
      TLogTracking = class(TInterfacedObject, IInterface)
      private
        fLogger: ILogger;
        fLevel: TLogLevel;
        fClassType: TClass;
        fMethodName: string;
      public
        constructor Create(const logger: ILogger; level: TLogLevel;
          const classType: TClass; const methodName: string);
        destructor Destroy; override;
      end;
  protected
    procedure ILogAppender.Send = Log;
    procedure DoLog(const event: TLogEvent); virtual; abstract;
  public
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
  end;

  {$ENDREGION}


  {$REGION 'TLogger'}

  TLogger = class(TAbstractLogger)
  private
    fController: ILoggerController;
  protected
    procedure DoLog(const event: TLogEvent); override;
  public
    constructor Create(const controller: ILoggerController);
  end;

  {$ENDREGION}


implementation

uses
  Spring.Collections;


{$REGION 'TLoggerBase'}

constructor TLoggerBase.Create;
begin
  inherited Create;
  fDefaultLevel := TLogLevel.Info;
  fEnabled := True;
  fLevels := LOG_BASIC_LEVELS;
  fEventTypes := LOG_BASIC_EVENT_TYPES;
end;

function TLoggerBase.GetDefaultLevel: TLogLevel;
begin
  Result := fDefaultLevel;
end;

function TLoggerBase.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

function TLoggerBase.GetEventTypes: TLogEventTypes;
begin
  Result := fEventTypes;
end;

function TLoggerBase.GetLevels: TLogLevels;
begin
  Result := fLevels;
end;

function TLoggerBase.IsEnabled(level: TLogLevel; eventTypes: TLogEventTypes): Boolean;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckTrue(eventTypes <> [], 'eventTypes');
{$ENDIF}
  Result := fEnabled and (level in fLevels) and (eventTypes * fEventTypes <> []);
end;

function TLoggerBase.IsDebugEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Debug, LOG_ALL_EVENT_TYPES);
end;

function TLoggerBase.IsErrorEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Error, LOG_ALL_EVENT_TYPES);
end;

function TLoggerBase.IsFatalEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Fatal, LOG_ALL_EVENT_TYPES);
end;

function TLoggerBase.IsInfoEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Info, LOG_ALL_EVENT_TYPES);
end;

function TLoggerBase.IsTextEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Text, LOG_ALL_EVENT_TYPES);
end;

function TLoggerBase.IsTraceEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Trace, LOG_ALL_EVENT_TYPES);
end;

function TLoggerBase.IsWarnEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Warn, LOG_ALL_EVENT_TYPES);
end;

procedure TLoggerBase.SetDefaultLevel(value: TLogLevel);
begin
  fDefaultLevel := value;
end;

procedure TLoggerBase.SetEnabled(value: Boolean);
begin
  fEnabled := value;
end;

procedure TLoggerBase.SetEventTypes(value: TLogEventTypes);
begin
  fEventTypes := value;
end;

procedure TLoggerBase.SetLevels(value: TLogLevels);
begin
  fLevels := value;
end;

{$ENDREGION}


{$REGION 'TAbstractLogger'}

procedure TAbstractLogger.Debug(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Debug) then
    DoLog(TLogEvent.Create(TLogLevel.Debug, Format(fmt, args)));
end;

procedure TAbstractLogger.Debug(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Debug) then
    DoLog(TLogEvent.Create(TLogLevel.Debug, Format(fmt, args), e));
end;

procedure TAbstractLogger.Debug(const msg: string);
begin
  if IsEnabled(TLogLevel.Debug) then
    DoLog(TLogEvent.Create(TLogLevel.Debug, msg));
end;

procedure TAbstractLogger.Debug(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Debug) then
    DoLog(TLogEvent.Create(TLogLevel.Debug, msg, e));
end;

procedure TAbstractLogger.Error(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Error) then
    DoLog(TLogEvent.Create(TLogLevel.Error, Format(fmt, args)));
end;

procedure TAbstractLogger.Enter(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin
  if IsEnabled(level, [TLogEventType.Entering]) then
    DoLog(TLogEvent.Create(level, TLogEventType.Entering, methodName, classType));
end;

procedure TAbstractLogger.Enter(const methodName: string);
begin
  Enter(fDefaultLevel, nil, methodName);
end;

procedure TAbstractLogger.Enter(const classType: TClass;
  const methodName: string);
begin
  Enter(fDefaultLevel, classType, methodName);
end;

procedure TAbstractLogger.Enter(const instance: TObject;
  const methodName: string);
begin
  if Assigned(instance) then
    Enter(instance.ClassType, methodName)
  else
    Enter(TClass(nil), methodName)
end;

procedure TAbstractLogger.Error(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Error) then
    DoLog(TLogEvent.Create(TLogLevel.Error, Format(fmt, args), e));
end;

procedure TAbstractLogger.Error(const msg: string);
begin
  if IsEnabled(TLogLevel.Error) then
    DoLog(TLogEvent.Create(TLogLevel.Error, msg));
end;

procedure TAbstractLogger.Error(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Error) then
    DoLog(TLogEvent.Create(TLogLevel.Error, msg, e));
end;

procedure TAbstractLogger.Fatal(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Fatal) then
    DoLog(TLogEvent.Create(TLogLevel.Fatal, Format(fmt, args)));
end;

procedure TAbstractLogger.Fatal(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Fatal) then
    DoLog(TLogEvent.Create(TLogLevel.Fatal, Format(fmt, args), e));
end;

procedure TAbstractLogger.Fatal(const msg: string);
begin
  if IsEnabled(TLogLevel.Fatal) then
    DoLog(TLogEvent.Create(TLogLevel.Fatal, msg));
end;

procedure TAbstractLogger.Fatal(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Fatal) then
    DoLog(TLogEvent.Create(TLogLevel.Fatal, msg, e));
end;

procedure TAbstractLogger.Info(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Info) then
    DoLog(TLogEvent.Create(TLogLevel.Info, msg, e));
end;

procedure TAbstractLogger.Info(const msg: string);
begin
  if IsEnabled(TLogLevel.Info) then
    DoLog(TLogEvent.Create(TLogLevel.Info, msg));
end;

procedure TAbstractLogger.Info(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Info) then
    DoLog(TLogEvent.Create(TLogLevel.Info, Format(fmt, args), e));
end;

procedure TAbstractLogger.Info(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Info) then
    DoLog(TLogEvent.Create(TLogLevel.Info, Format(fmt, args)));
end;

procedure TAbstractLogger.Leave(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin
  if IsEnabled(level, [TLogEventType.Leaving]) then
    DoLog(TLogEvent.Create(level, TLogEventType.Leaving, methodName, classType));
end;

procedure TAbstractLogger.Leave(const methodName: string);
begin
  Leave(fDefaultLevel, nil, methodName);
end;

procedure TAbstractLogger.Leave(const instance: TObject;
  const methodName: string);
begin
  if Assigned(instance) then
    Leave(instance.ClassType, methodName)
  else
    Leave(TClass(nil), methodName)
end;

procedure TAbstractLogger.Leave(const classType: TClass;
  const methodName: string);
begin
  Leave(fDefaultLevel, classType, methodName);
end;

procedure TAbstractLogger.Log(level: TLogLevel; const msg: string;
  const e: Exception);
begin
  if IsEnabled(level) then
    DoLog(TLogEvent.Create(level, msg, e));
end;

procedure TAbstractLogger.Log(level: TLogLevel; const msg: string);
begin
  if IsEnabled(level) then
    DoLog(TLogEvent.Create(level, msg));
end;

procedure TAbstractLogger.Log(level: TLogLevel; const fmt: string;
  const args: array of const);
begin
  if IsEnabled(level) then
    DoLog(TLogEvent.Create(level, Format(fmt, args)));
end;

procedure TAbstractLogger.Log(const msg: string);
begin
  Log(fDefaultLevel, msg);
end;

procedure TAbstractLogger.Log(const msg: string; const e: Exception);
begin
  Log(fDefaultLevel, msg, e);
end;

procedure TAbstractLogger.Log(const fmt: string; const args: array of const);
begin
  Log(fDefaultLevel, fmt, args);
end;

procedure TAbstractLogger.Log(const fmt: string; const args: array of const;
  const e: Exception);
begin
  Log(fDefaultLevel, fmt, args, e);
end;

procedure TAbstractLogger.Log(level: TLogLevel; const fmt: string;
  const args: array of const; const e: Exception);
begin
  if IsEnabled(level) then
    DoLog(TLogEvent.Create(level, Format(fmt, args), e));
end;

procedure TAbstractLogger.LogValue(level: TLogLevel; const name: string;
  const value: TValue);
begin
  if IsEnabled(level, [TLogEventType.Value]) then
    DoLog(TLogEvent.Create(level, TLogEventType.Value, name, nil, value));
end;

procedure TAbstractLogger.LogValue(const name: string; const value: TValue);
begin
  LogValue(fDefaultLevel, name, value);
end;

procedure TAbstractLogger.Log(const event: TLogEvent);
begin
  if IsEnabled(event.Level, [event.EventType]) then
    DoLog(event);
end;

procedure TAbstractLogger.Text(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Text) then
    DoLog(TLogEvent.Create(TLogLevel.Text, Format(fmt, args)));
end;

procedure TAbstractLogger.Text(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Text) then
    DoLog(TLogEvent.Create(TLogLevel.Text, Format(fmt, args), e));
end;

procedure TAbstractLogger.Text(const msg: string);
begin
  if IsEnabled(TLogLevel.Text) then
    DoLog(TLogEvent.Create(TLogLevel.Text, msg));
end;

procedure TAbstractLogger.Text(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Text) then
    DoLog(TLogEvent.Create(TLogLevel.Text, msg, e));
end;

function TAbstractLogger.Track(level: TLogLevel; const classType: TClass;
  const methodName: string): IInterface;
begin
  if IsEnabled(level, [TLogEventType.Entering, TLogEventType.Leaving]) then
    Result := TLogTracking.Create(Self, level, classType, methodName)
  else
    Result := nil;
end;

function TAbstractLogger.Track(const instance: TObject;
  const methodName: string): IInterface;
begin
  if Assigned(instance) then
    Result := Track(instance.ClassType, methodName)
  else
    Result := Track(TClass(nil), methodName)
end;

function TAbstractLogger.Track(const classType: TClass;
  const methodName: string): IInterface;
begin
  Result := Track(fDefaultLevel, classType, methodName);
end;

procedure TAbstractLogger.Trace(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Trace) then
    DoLog(TLogEvent.Create(TLogLevel.Trace, Format(fmt, args)));
end;

procedure TAbstractLogger.Trace(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Trace) then
    DoLog(TLogEvent.Create(TLogLevel.Trace, Format(fmt, args), e));
end;

procedure TAbstractLogger.Trace(const msg: string);
begin
  if IsEnabled(TLogLevel.Trace) then
    DoLog(TLogEvent.Create(TLogLevel.Trace, msg));
end;

procedure TAbstractLogger.Trace(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Trace) then
    DoLog(TLogEvent.Create(TLogLevel.Trace, msg, e));
end;

procedure TAbstractLogger.Warn(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Warn) then
    DoLog(TLogEvent.Create(TLogLevel.Warn, Format(fmt, args)));
end;

procedure TAbstractLogger.Warn(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Warn) then
    DoLog(TLogEvent.Create(TLogLevel.Warn, Format(fmt, args), e));
end;

procedure TAbstractLogger.Warn(const msg: string);
begin
  if IsEnabled(TLogLevel.Warn) then
    DoLog(TLogEvent.Create(TLogLevel.Warn, msg));
end;

procedure TAbstractLogger.Warn(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Warn) then
    DoLog(TLogEvent.Create(TLogLevel.Warn, msg, e));
end;

{$ENDREGION}


{$REGION 'TAbstractLogger.TLogTracking'}

constructor TAbstractLogger.TLogTracking.Create(const logger: ILogger;
  level: TLogLevel; const classType: TClass; const methodName: string);
begin
  inherited Create;
  fLogger := logger;
  fLevel := level;
  fClassType := classType;
  fMethodName := methodName;
  fLogger.Enter(fLevel, fClassType, fMethodName);
end;

destructor TAbstractLogger.TLogTracking.Destroy;
begin
  fLogger.Leave(fLevel, fClassType, fMethodName);
  inherited Destroy;
end;

{$ENDREGION}


{$REGION 'TLogger'}

constructor TLogger.Create(const controller: ILoggerController);
begin
  Guard.CheckNotNull(controller, 'controller');
  inherited Create;

  fController := controller;
end;

procedure TLogger.DoLog(const event: TLogEvent);
begin
  fController.Send(event);
end;

{$ENDREGION}


end.
