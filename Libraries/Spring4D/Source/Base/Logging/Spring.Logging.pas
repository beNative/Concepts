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

unit Spring.Logging;

interface

uses
  Rtti,
{$IF Defined(DELPHIXE2_UP)}
  System.UITypes, // Has minimum dependencies
{$ELSEIF NOT Defined(SPRING_DISABLE_GRAPHICS)}
  Graphics, // Has (unfortunately) VCL dependencies
{$IFEND}
  SysUtils;

{$REGION 'Shadowed Delphi types'}
{$IF Defined(DELPHIXE2_UP)}
type
  TColor = System.UITypes.TColor;
  TColors = System.UITypes.TColors;

const
  clDefault = TColors.SysDefault;
{$ELSEIF NOT Defined(SPRING_DISABLE_GRAPHICS)}
type
  TColor = Graphics.TColor;

const
  clDefault = Graphics.clDefault;
{$ELSE}
type
  TColor = Int32;

const
  clDefault = $20000000;
{$IFEND}
{$ENDREGION}

{$SCOPEDENUMS ON}
type
  {$REGION 'Log Level and formating definitions and constants'}

  TLogLevel = (
    /// <summary>
    ///   Excluded from all states and should never be used!
    /// </summary>
    Unknown,
    Trace,
    Debug,
    Text,
    Info,
    Warn,
    Error,
    Fatal
  );
  TLogLevels = set of TLogLevel;

const
  LOG_ALL_LEVELS = [Low(TLogLevel)..High(TLogLevel)] - [TLogLevel.Unknown];
  LOG_BASIC_LEVELS = [
    TLogLevel.Info,
    TLogLevel.Warn,
    TLogLevel.Error,
    TLogLevel.Fatal
  ];

type
  TLogEventType = (
    /// <summary>
    ///   Is the most basic logging type all loggers should keep enabled
    /// </summary>
    Text,
    Value,
    /// <summary>
    ///   Should only be called if stack is sent to the appender. The appender
    ///   may treat it in a specific way. No one else should use this event
    ///   type. If this event type is not set, callstack logging will be
    ///   disabled completely, this may have significant performance impact on
    ///   some platforms.
    /// </summary>
    CallStack,
    /// <summary>
    ///   Should only be called if serialized data (object, record, etc.) is
    ///   sent to the appender. The appender may treat it in a specific way. No
    ///   one else should use this event type. If this level is not set, data
    ///   serialization logging will be disabled completely.
    /// </summary>
    SerializedData,
    Entering,
    Leaving
  );
  TLogEventTypes = set of TLogEventType;

const
  LOG_ALL_EVENT_TYPES = [Low(TLogEventType)..High(TLogEventType)];
  LOG_BASIC_EVENT_TYPES = [
    TLogEventType.Text,
    TLogEventType.Value,
    TLogEventType.Entering,
    TLogEventType.Leaving
  ];

type
  TLogStyle = (
    Bold,
    Italic,
    Underline,
    /// <summary>
    ///   Text should be printed in monospace font
    /// </summary>
    Monospace,
    /// <summary>
    ///   No escaping should be done on the text of the message by the
    ///   appenders in cases when they need to do so in order to not break the
    ///   output format.
    /// </summary>
    NoEscape
  );
  TLogStyles = set of TLogStyle;

  {$ENDREGION}


  {$REGION 'TLogEvent'}

  TLogEvent = record
  private
    fLevel: TLogLevel;
    fEventType: TLogEventType;
    fMsg: string;
    fTimeStamp: TDateTime;
    fException: Exception;
    /// <summary>
    ///   Leave as default to instruct the appender/viewer to choose the
    ///   default color based on the level or event contents or prescribe the
    ///   color of your choosing (not that some appenders may ignore the
    ///   color).
    /// </summary>
    fColor: TColor;
    /// <summary>
    ///   Similar to Color but defines a text style.
    /// </summary>
    fStyle: TLogStyles;
    fClassType: TClass;
    fAddStack: Boolean;
    /// <summary>
    ///   An arbitrary data that the logger may output to the appenders
    /// </summary>
    fData: TValue;
    /// <summary>
    ///   Additional data anyone can use to extend behavior of their appenders
    /// </summary>
    fTag: NativeInt;
  public
    constructor Create(level: TLogLevel; const msg: string); overload;
    constructor Create(level: TLogLevel; eventType: TLogEventType;
      const msg: string); overload;
    constructor Create(level: TLogLevel; const msg: string;
      const e: Exception); overload;
    constructor Create(level: TLogLevel; eventType: TLogEventType;
      const msg: string; const classType: TClass); overload;
    constructor Create(level: TLogLevel; eventType: TLogEventType;
      const msg: string; const classType: TClass; const data: TValue); overload;
    {constructor Create(level: TLogLevel; const msg: string;
      color: TColor = clDefault; fontStyle: TFontStyles = []; )}

    function SetException(const e: Exception): TLogEvent;
    function SetColor(color: TColor): TLogEvent;
    function SetStyle(style: TLogStyles): TLogEvent;
    function SetClassType(const classType: TClass): TLogEvent;
    function AddStack: TLogEvent;
    function SetData(const Data: TValue): TLogEvent;
    function SetTag(tag: NativeInt): TLogEvent;

    property Level: TLogLevel read fLevel;
    property EventType: TLogEventType read fEventType;
    property Msg: string read fMsg;
    property TimeStamp: TDateTime read fTimeStamp;
    property Exception: Exception read fException;
    property Color: TColor read fColor;
    property Style: TLogStyles read fStyle;
    property ClassType: TClass read FClassType;
    property AddStackValue: Boolean read fAddStack;
    property Data: TValue read fData;
    property Tag: NativeInt read fTag;
  end;

  {$ENDREGION}


  {$REGION 'ILoggerBase'}

  ILoggerBase = interface(IInvokable)
    ['{2CACEE4B-631D-4B31-970C-7B82F49311B4}']
    function GetEnabled: Boolean;
    function GetEventTypes: TLogEventTypes;
    function GetLevels: TLogLevels;

    /// <summary>
    ///   Returns <c>true</c> if level is enabled and any of the <c>eventTypes</c>
    ///    is enabled or <c>false</c> otherwise.
    /// </summary>
    /// <param name="eventTypes">
    ///   Specifies event types to check, <b>must not be empty</b>! Defaults to
    ///   <c>Text</c>.
    /// </param>
    function IsEnabled(level: TLogLevel;
      eventType: TLogEventTypes = [TLogEventType.Text]): Boolean;
    function IsFatalEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsWarnEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsTextEnabled: Boolean;
    function IsDebugEnabled: Boolean;
    function IsTraceEnabled: Boolean;

    property Enabled: Boolean read GetEnabled;
    property EventTypes: TLogEventTypes read GetEventTypes;
    property Levels: TLogLevels read GetLevels;
  end;

  {$ENDREGION}


  {$REGION 'ILogger'}

  ILogger = interface(ILoggerBase)
    ['{8655E906-C12D-4EB3-8291-30CEAB769B26}']
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


  {$REGION 'ILogAppender'}

  ILogAppender = interface(ILoggerBase)
    ['{70DDEB60-3D01-48FB-92CF-A738A8C4BC85}']
    procedure Send(const event: TLogEvent);
  end;

  {$ENDREGION}


  {$REGION 'ILoggerController'}

  ILogEventConverter = interface;

  ILoggerController = interface(ILogAppender)
    ['{6556A795-6F1B-4392-92FC-8E3391E3CB07}']
    procedure AddAppender(const appender: ILogAppender);
    procedure AddEventConverter(const converter: ILogEventConverter);

    /// <summary>
    ///   Returns <c>True</c> if level is enabled and any of the <c>eventTypes</c>
    ///    is enabled in any of the appenders or <c>False</c> otherwise
    /// </summary>
    function IsLoggable(level: TLogLevel; eventTypes: TLogEventTypes): Boolean;

    /// <summary>
    ///   Send the event directly to appenders and skip converters.
    /// </summary>
    procedure SendToAppenders(const event: TLogEvent);
  end;

  {$ENDREGION}


  {$REGION 'ILoggerProperties'}

  /// <summary>
  ///   Interface that can be used to change logger/appender settings during
  ///   runtime. It is hidden within this interface so that it is not widely
  ///   exposed to the consumer.
  /// </summary>
  ILoggerProperties = interface
    ['{6514ADA8-A0A0-4234-A5EE-FBAFE34B58F2}']
    function GetDefaultLevel: TLogLevel;
    function GetEnabled: Boolean;
    function GetEventTypes: TLogEventTypes;
    function GetLevels: TLogLevels;

    procedure SetDefaultLevel(value: TLogLevel);
    procedure SetEnabled(value: Boolean);
    procedure SetEventTypes(value: TLogEventTypes);
    procedure SetLevels(value: TLogLevels);

    property DefaultLevel: TLogLevel read GetDefaultLevel write SetDefaultLevel;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property EventTypes: TLogEventTypes read GetEventTypes write SetEventTypes;
    property Levels: TLogLevels read GetLevels write SetLevels;
  end;

  {$ENDREGION}


  {$REGION 'ILogEventConverter'}

  /// <summary>
  ///   Used by <see cref="Spring.Logging|ILoggerController" /> to convert
  ///   complex logging events (values, stack traces) to simpler ones (text).
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Implementations should use SendToAppenders method of the controller
  ///     to prevent stack overflow.
  ///   </para>
  ///   <para>
  ///     Note that only one converter may consume the event, once <c>
  ///     HandleEvent</c> returns <c>True</c> enumeration of converters is
  ///     stopped.
  ///   </para>
  /// </remarks>
  ILogEventConverter = interface
    ['{80DC67A5-CA3D-4A57-B843-AA1F9E1BB4F2}']
    function GetEventType: TLogEventType;

    /// <summary>
    ///   Consume the event and return <c>True</c> if the event was processed.
    /// </summary>
    /// <param name="owner">
    ///   controller that can be used to output the data
    /// </param>
    /// <param name="event">
    ///   event being processed
    /// </param>
    /// <returns>
    ///   <c>True</c> if the event was processed or <c>False</c> otherwise.
    /// </returns>
    function HandleEvent(const controller: ILoggerController;
      const event: TLogEvent): Boolean;

    property EventType: TLogEventType read GetEventType;
  end;

  {$ENDREGION}


implementation


{$REGION 'TLogEvent'}

constructor TLogEvent.Create(level: TLogLevel; const msg: string);
begin
  fTimeStamp := Now; // Do this ASAP
  fLevel := level;
  fEventType := TLogEventType.Text;
  fMsg := msg;

  // Set default values
  fColor := clDefault;
  fStyle := [];

  // Reset non-managed fields
  fException := nil;
  fAddStack := False;
  fTag := 0;

  Assert(fData.IsEmpty);
end;

constructor TLogEvent.Create(level: TLogLevel; const msg: string;
  const e: Exception);
begin
  Create(level, msg);
  fException := e;
end;

constructor TLogEvent.Create(level: TLogLevel; eventType: TLogEventType;
  const msg: string; const classType: TClass);
begin
  Create(level, msg);
  fClassType := classType;
  fEventType := eventType;
end;

constructor TLogEvent.Create(level: TLogLevel; eventType: TLogEventType;
  const msg: string; const classType: TClass; const data: TValue);
begin
  Create(level, eventType, msg, classType);
  fData := data;
end;

constructor TLogEvent.Create(level: TLogLevel; eventType: TLogEventType;
  const msg: string);
begin
  Create(level, msg);
  fEventType := eventType;
end;

function TLogEvent.SetColor(color: TColor): TLogEvent;
begin
  Result := Self;
  Result.fColor := color;
end;

function TLogEvent.AddStack: TLogEvent;
begin
  Result := Self;
  Result.fAddStack := True;
end;

function TLogEvent.SetClassType(const classType: TClass): TLogEvent;
begin
  Result := Self;
  Result.fClassType := classType;
end;

function TLogEvent.SetData(const Data: TValue): TLogEvent;
begin
  Result := Self;
  Result.fData := Data;
end;

function TLogEvent.SetException(const e: Exception): TLogEvent;
begin
  Result := Self;
  Result.fException := e;
end;

function TLogEvent.SetStyle(style: TLogStyles): TLogEvent;
begin
  Result := Self;
  Result.fStyle := style;
end;

function TLogEvent.SetTag(tag: NativeInt): TLogEvent;
begin
  Result := Self;
  Result.fTag := tag;
end;

{$ENDREGION}


end.
