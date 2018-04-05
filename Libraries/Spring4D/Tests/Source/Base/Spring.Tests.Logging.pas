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

unit Spring.Tests.Logging;

{$I Spring.Tests.inc}
{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes,
  Rtti,
  StrUtils,
  SysConst,
  SysUtils,
  TypInfo,
  TestFramework,
  Spring,
  Spring.Collections,
  Spring.Reflection,
  Spring.Logging,
  Spring.Logging.Appenders,
  Spring.Logging.Appenders.Base,
  Spring.Logging.Controller,
  Spring.Logging.Extensions,
  Spring.Logging.Loggers,
{$IFNDEF DELPHI2010}
  Spring.Mocking,
{$ENDIF}
  Spring.Tests.Logging.Types;

type
  {$REGION 'TTestLoggerController'}

  TTestLoggerController = class(TTestCase)
  private
    fController: ILoggerController;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAppender;
    procedure TestIsLoggable;
    procedure TestSend;
    procedure TestSendDisabled;
    procedure TestAddEventConverter;
    procedure TestFindSerializer;
    procedure TestSendWithSerializer;
{$IFNDEF DELPHI2010}
    procedure TestStackTrace;
{$ENDIF}
  end;

  {$ENDREGION}


  {$REGION 'TTestLogger'}

  TTestLogger = class(TTestCase)
  private
    fController: ILoggerController;
    fLogger: ILogger;
    fException: Exception;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckEvent(enabled: Boolean; level: TLogLevel; const msg: string;
      const e: Exception = nil);
    procedure TestLogEvent(enabled: Boolean); overload;
    procedure TestLog(enabled: Boolean); overload;
    procedure TestFatal(enabled: Boolean); overload;
    procedure TestError(enabled: Boolean); overload;
    procedure TestWarn(enabled: Boolean); overload;
    procedure TestInfo(enabled: Boolean); overload;
    procedure TestText(enabled: Boolean); overload;
    procedure TestDebug(enabled: Boolean); overload;
    procedure TestTrace(enabled: Boolean); overload;
  published
    procedure TestCreate;
    procedure TestLevels;
    procedure TestEnabled;
    procedure TestIsEnabled;

    procedure TestIsFatalEnabled;
    procedure TestIsErrorEnabled;
    procedure TestIsWarnEnabled;
    procedure TestIsInfoEnabled;
    procedure TestIsTextEnabled;
    procedure TestIsDebugEnabled;
    procedure TestIsTraceEnabled;

    procedure TestLoggerProperties;

    procedure TestLogEvent; overload;

    procedure TestLog; overload;
    procedure TestFatal; overload;
    procedure TestError; overload;
    procedure TestWarn; overload;
    procedure TestInfo; overload;
    procedure TestText; overload;
    procedure TestDebug; overload;
    procedure TestTrace; overload;

    procedure TestEntering;
    procedure TestLeaving;
    procedure TestTrack;
  end;

  {$ENDREGION}


  {$REGION 'TAppenderTestCase'}

  TAppenderTestCase = class abstract(TTestCase)
  protected
    fController: ILoggerController;
    fLogger: ILogger;
    fAppender: ILogAppender;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  {$ENDREGION}


  {$REGION 'TTestLogAppenderBase'}

  TTestLogAppenderBase = class(TTestCase)
  published
    procedure TestIsEnabled;
    procedure TestFormatMethodName;
    procedure TestFormatText;
    procedure TestFormatMsg;
  end;

  {$ENDREGION}


  {$REGION 'TTestStreamLogAppender'}

  TTestStreamLogAppender = class(TAppenderTestCase)
  private
    fStream: TStringStream;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWrite;
    procedure TestException;
  end;

  {$ENDREGION}


implementation


{$REGION 'Internal test classes and helpers'}

type
  TTestLoggerHelper = class helper for TTestLogger
  private
    function GetController: TLoggerControllerMock;
    function GetLogger: TLogger;
  public
    property Controller: TLoggerControllerMock read GetController;
    property Logger: TLogger read GetLogger;
  end;

  TLoggerControllerAccess = class(TLoggerController);
  TLogAppenderBaseAccess = class(TLogAppenderBase);

{ TTestLoggerHelper }

function TTestLoggerHelper.GetController: TLoggerControllerMock;
begin
  Result := TObject(fController) as TLoggerControllerMock;
end;

function TTestLoggerHelper.GetLogger: TLogger;
begin
  Result := TObject(fLogger) as TLogger;
end;

{$ENDREGION}


{$REGION 'TTestLoggerController'}

procedure TTestLoggerController.SetUp;
begin
  inherited;
  fController := TLoggerController.Create;
end;

procedure TTestLoggerController.TearDown;
begin
  fController := nil;
  inherited;
end;

procedure TTestLoggerController.TestAddAppender;
var
  appender: TAppenderMock;
  intf: ILogAppender;
  f: TRttiField;
  appenders: IList<ILogAppender>;
begin
  appender := TAppenderMock.Create;
  intf := appender;
  fController.AddAppender(intf);

  f := TType.GetType<TLoggerController>.GetField('fAppenders');
  appenders := f.GetValue(TObject(fController)).AsType<IList<ILogAppender>>;

  CheckEquals(1, appenders.Count);
  CheckSame(intf, appenders[0]);
end;

procedure TTestLoggerController.TestAddEventConverter;
var
  serializer: TTypeSerializerMock;
  intf: ILogEventConverter;
  f: TRttiField;
  serializers: IList<ILogEventConverter>;
begin
  serializer := TTypeSerializerMock.Create;
  intf := serializer;

  fController.AddEventConverter(intf);

  f := TType.GetType<TLoggerController>.GetField('fConverters');
  serializers := f.GetValue(TObject(fController)).AsType<IList<ILogEventConverter>>;

  CheckEquals(1, serializers.Count);
  CheckSame(intf, serializers[0]);
end;

procedure TTestLoggerController.TestFindSerializer;
var
  serializer: TTypeSerializerMock;
  intf: ITypeSerializer;
  controller: ISerializerController;
begin
  serializer := TTypeSerializerMock.Create;
  controller := (fController as ISerializerController);
  fController.AddEventConverter(serializer);

  intf := serializer;
  CheckSame(intf, controller.FindSerializer(TypeInfo(Integer)));
  CheckNull(controller.FindSerializer(TypeInfo(Double)));
end;

procedure TTestLoggerController.TestIsLoggable;
var
  controller: TLoggerControllerAccess;
  appender: TAppenderMock;
begin
  controller := TLoggerControllerAccess(TLoggerController(fController));

  CheckFalse(controller.IsLoggable(TLogLevel.Fatal, [TLogEventType.Text]));

  appender := TAppenderMock.Create;
  appender.Enabled := False;
  controller.AddAppender(appender);
  CheckFalse(controller.IsLoggable(TLogLevel.Fatal, [TLogEventType.Text]));

  appender.Enabled := True;
  appender.Levels := [];
  CheckFalse(controller.IsLoggable(TLogLevel.Fatal, [TLogEventType.Text]));

  appender.Levels := [TLogLevel.Fatal];
  appender.EventTypes := [];
  CheckFalse(controller.IsLoggable(TLogLevel.Fatal, [TLogEventType.Text]));

  appender := TAppenderMock.Create;
  controller.AddAppender(appender);
  appender.Levels := [TLogLevel.Fatal];
  appender.EventTypes := [TLogEventType.Text, TLogEventType.Value];
  CheckTrue(controller.IsLoggable(TLogLevel.Fatal, [TLogEventType.Text]));
  CheckFalse(controller.IsLoggable(TLogLevel.Fatal, [TLogEventType.CallStack]));
  CheckTrue(controller.IsLoggable(TLogLevel.Fatal, [TLogEventType.CallStack,
    TLogEventType.Text]));
end;

procedure TTestLoggerController.TestSend;
const
  MSG = 'test';
var
  appender: TAppenderMock;
  intf: ILogAppender;
begin
  appender := TAppenderMock.Create;
  intf := appender;

  fController.AddAppender(intf);
  fController.Send(TLogEvent.Create(TLogLevel.Fatal, MSG));

  CheckTrue(appender.WriteCalled);
  CheckEquals(Ord(TLogLevel.Fatal), Ord(appender.Event.Level));
  CheckEquals(MSG, appender.Event.Msg);
end;

procedure TTestLoggerController.TestSendDisabled;
const
  MSG = 'test';
var
  appender: TAppenderMock;
  intf: ILogAppender;
begin
  appender := TAppenderMock.Create;
  intf := appender;
  fController.AddAppender(intf);

  TLoggerController(fController).Enabled := False;
  fController.Send(TLogEvent.Create(TLogLevel.Fatal, MSG));
  CheckFalse(appender.WriteCalled);

  TLoggerController(fController).Enabled := True;
  TLoggerController(fController).Levels := [];
  fController.Send(TLogEvent.Create(TLogLevel.Fatal, MSG));
  CheckFalse(appender.WriteCalled);

  TLoggerController(fController).Levels := LOG_ALL_LEVELS - [TLogLevel.Fatal];
  fController.Send(TLogEvent.Create(TLogLevel.Fatal, MSG));
  CheckFalse(appender.WriteCalled);
end;

procedure TTestLoggerController.TestSendWithSerializer;
var
  appender: TAppenderMock;
  intf: ILogAppender;
  serializer: TTypeSerializerMock;
begin
  serializer := TTypeSerializerMock.Create;
  appender := TAppenderMock.Create;
  intf := appender;

  fController.AddAppender(intf);
  fController.AddEventConverter(serializer);

  //Check that we only output the message and not the data if the controller
  //does not have SerializedData level
  appender.Levels := [TLogLevel.Fatal];
  appender.EventTypes := [TLogEventType.Text, TLogEventType.SerializedData];
  (fController as ILoggerProperties).EventTypes := [TLogEventType.Text];
  fController.Send(TLogEvent.Create(TLogLevel.Fatal, TLogEventType.Text, 'test',
    nil, -1));
  CheckEquals(1, appender.WriteCount);
  CheckEquals(1, serializer.HandlesTypeCount);
  CheckEquals(Ord(TLogLevel.Fatal), Ord(appender.Event.Level));
  (fController as ILoggerProperties).EventTypes := LOG_ALL_EVENT_TYPES;

  //Check that we only output the message and not the data if the appender
  //does not have SerializedData level
  appender.Levels := [TLogLevel.Fatal];
  appender.EventTypes := [TLogEventType.Text];
  fController.Send(TLogEvent.Create(TLogLevel.Fatal, TLogEventType.Text, 'test',
    nil, -1));
  CheckEquals(2, appender.WriteCount);
  CheckEquals(2, serializer.HandlesTypeCount);
  CheckEquals(Ord(TLogLevel.Fatal), Ord(appender.Event.Level));

  //... or is disabled
  appender.Enabled := False;
  appender.Levels := [TLogLevel.Fatal];
  appender.EventTypes := [TLogEventType.Text, TLogEventType.SerializedData];
  fController.Send(TLogEvent.Create(TLogLevel.Fatal, TLogEventType.Text, 'test',
    nil, -1));
  CheckEquals(2, appender.WriteCount);
  CheckEquals(3, serializer.HandlesTypeCount);

  //Finally test that we dispatch the call and both messages are logged
  appender.Enabled := True;
  fController.Send(TLogEvent.Create(TLogLevel.Fatal, TLogEventType.Text, 'test',
    nil, -1));
  CheckEquals(4, appender.WriteCount);
  CheckEquals(4, serializer.HandlesTypeCount);
  CheckEquals(Ord(TLogEventType.SerializedData), Ord(appender.Event.EventType));
end;

{$IFNDEF DELPHI2010}
procedure TTestLoggerController.TestStackTrace;
var
  appender: TAppenderMock;
  intf: ILogAppender;
  collectorMock: Mock<TStackTraceCollector>;
  formatterMock: Mock<TStackTraceFormatter>;
  seq: MockSequence;
  event: TLogEvent;
  stack: TArray<Pointer>;
  formatted: TArray<string>;
begin
  appender := TAppenderMock.Create;
  intf := appender;

  collectorMock.Behavior := TMockBehavior.Strict;
  formatterMock.Behavior := TMockBehavior.Strict;

  fController.AddAppender(intf);
  fController.AddEventConverter(TCallStackEventConverter.Create(collectorMock,
    formatterMock));
  try
    event := TLogEvent.Create(TLogLevel.Error, 'Message').AddStack;

    // Disabled by controller
    appender.EventTypes := LOG_ALL_EVENT_TYPES;
    fController.Send(event);
    CheckEquals(1, appender.WriteCount);
    CheckEqualsString(event.Msg, appender.Event.Msg);

    // Disabled by appender
    appender.EventTypes := LOG_BASIC_EVENT_TYPES;
    (fController as ILoggerProperties).EventTypes := LOG_ALL_EVENT_TYPES;
    fController.Send(event);
    CheckEquals(2, appender.WriteCount);
    CheckEqualsString(event.Msg, appender.Event.Msg);
    // No calls to collector and formatter expected

    // Empty stack
    appender.EventTypes := LOG_ALL_EVENT_TYPES;
    collectorMock.Setup(Seq).Returns<TArray<Pointer>>(nil).When.Collect;
    formatterMock.Setup(Seq).Returns<TArray<string>>(nil).When.Format(nil);
    fController.Send(event);
    // No calls to formatter expected
    Check(seq.Completed);
    CheckEquals(4, appender.WriteCount);
    CheckEqualsString('', appender.Event.Msg);
    Check(appender.Event.Level = event.Level);
    Check(appender.Event.EventType = TLogEventType.CallStack);

    // One line stack
    seq.Reset;
    stack := TArray<Pointer>.Create(Pointer($12345678));
    formatted := TArray<string>.Create('$12345678');
    collectorMock.Setup(Seq).Returns<TArray<Pointer>>(stack).When.Collect;
    formatterMock.Setup(Seq).Returns<TArray<string>>(formatted).When.Format(stack);
    fController.Send(event);
    Check(seq.Completed);
    CheckEquals(6, appender.WriteCount);
    CheckEqualsString(formatted[0], appender.Event.Msg);
    Check(appender.Event.Level = event.Level);
    Check(appender.Event.EventType = TLogEventType.CallStack);

    // Multi line stack
    seq.Reset;
    stack := TArray<Pointer>.Create(Pointer(1), Pointer(1));
    formatted := TArray<string>.Create('1', '2');
    collectorMock.Setup(Seq).Returns<TArray<Pointer>>(stack).When.Collect;
    formatterMock.Setup(Seq).Returns<TArray<string>>(formatted).When.Format(stack);
    fController.Send(event);
    Check(seq.Completed);
    CheckEquals(8, appender.WriteCount);
    CheckEqualsString(formatted[0] + sLineBreak + formatted[1],
      appender.Event.Msg);
    Check(appender.Event.Level = event.Level);
    Check(appender.Event.EventType = TLogEventType.CallStack);
  finally
    // Release mock references
    fController := nil;
  end;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TTestLogger'}

procedure TTestLogger.CheckEvent(enabled: Boolean; level: TLogLevel;
  const msg: string; const e: Exception = nil);
begin
  CheckEquals(enabled, Controller.LastEvent.Level <> TLogLevel.Unknown);
  if (not enabled) then
    Exit;

  CheckEquals(Ord(level), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Text), Ord(Controller.LastEvent.EventType));
  CheckEquals(msg, Controller.LastEvent.Msg);
  CheckSame(e, Controller.LastEvent.Exception);
end;

procedure TTestLogger.SetUp;
var logger: TLogger;
begin
  inherited;
  fController := TLoggerControllerMock.Create;
  logger := TLogger.Create(fController);
  logger.Levels := [TLogLevel.Fatal];
  fLogger := logger;
  fException := ENotSupportedException.Create('');
end;

procedure TTestLogger.TearDown;
begin
  inherited;
  fLogger := nil;
  fController := nil;
  FreeAndNil(fException);
end;

procedure TTestLogger.TestCreate;
var logger: TLogger;
begin
  logger := TLogger.Create(fController);
  try
    CheckTrue(logger.Enabled);
    Check(LOG_BASIC_LEVELS = logger.Levels, 'Levels');
  finally
    logger.Free;
  end;
end;

procedure TTestLogger.TestDebug;
begin
  Logger.Levels := [TLogLevel.Debug];
  TestDebug(True);
  Logger.Levels := [];
  TestDebug(False);
end;

procedure TTestLogger.TestDebug(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Debug('D1');
  CheckEvent(enabled, TLogLevel.Debug, 'D1');

  Controller.Reset;
  fLogger.Debug('D2', fException);
  CheckEvent(enabled, TLogLevel.Debug, 'D2', fException);

  Controller.Reset;
  fLogger.Debug('D%d', [3]);
  CheckEvent(enabled, TLogLevel.Debug, 'D3');

  Controller.Reset;
  fLogger.Debug('D%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Debug, 'D4', fException);
end;

procedure TTestLogger.TestEnabled;
begin
  Logger.Enabled := False;
  CheckFalse(Logger.Enabled);

  Logger.Enabled := True;
  CheckTrue(Logger.Enabled);
end;

procedure TTestLogger.TestEntering;
begin
  Controller.Reset;

  Logger.Enabled := False;
  fLogger.Enter(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);

  Logger.Enabled := True;
  Logger.Levels := [TLogLevel.Warn];
  fLogger.Enter(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);

  fLogger.Enter(TLogLevel.Warn, nil, '');
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Entering),  Ord(Controller.LastEvent.EventType));

  Controller.Reset;

  Logger.Enabled := False;
  fLogger.Enter(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);

  Logger.Enabled := True;
  Logger.Levels := [TLogLevel.Warn];
  fLogger.Enter(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);

  fLogger.Enter(TLogLevel.Warn, nil, '');
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Entering),  Ord(Controller.LastEvent.EventType));
  Controller.Reset;

  Logger.DefaultLevel := TLogLevel.Info;
  fLogger.Enter(TClass(nil), '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);

  Logger.DefaultLevel := TLogLevel.Warn;
  fLogger.Enter(TClass(nil), '');
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Entering),  Ord(Controller.LastEvent.EventType));
  Controller.Reset;

  fLogger.Enter(TObject(nil), '');
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Entering),  Ord(Controller.LastEvent.EventType));
  Controller.Reset;

  fLogger.Enter(Self, '');
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Entering),  Ord(Controller.LastEvent.EventType));

  fLogger.Enter('');
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Entering),  Ord(Controller.LastEvent.EventType));
end;

procedure TTestLogger.TestError(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Error('E1');
  CheckEvent(enabled, TLogLevel.Error, 'E1');

  Controller.Reset;
  fLogger.Error('E2', fException);
  CheckEvent(enabled, TLogLevel.Error, 'E2', fException);

  Controller.Reset;
  fLogger.Error('E%d', [3]);
  CheckEvent(enabled, TLogLevel.Error, 'E3');

  Controller.Reset;
  fLogger.Error('E%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Error, 'E4', fException);
end;

procedure TTestLogger.TestError;
begin
  Logger.Levels := [TLogLevel.Error];
  TestError(True);
  Logger.Levels := [];
  TestError(False);
end;

procedure TTestLogger.TestFatal(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Fatal('F1');
  CheckEvent(enabled, TLogLevel.Fatal, 'F1');

  Controller.Reset;
  fLogger.Fatal('F2', fException);
  CheckEvent(enabled, TLogLevel.Fatal, 'F2', fException);

  Controller.Reset;
  fLogger.Fatal('F%d', [3]);
  CheckEvent(enabled, TLogLevel.Fatal, 'F3');

  Controller.Reset;
  fLogger.Fatal('F%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Fatal, 'F4', fException);
end;

procedure TTestLogger.TestFatal;
begin
  Logger.Levels := [TLogLevel.Fatal];
  TestFatal(True);
  Logger.Levels := [];
  TestFatal(False);
end;

procedure TTestLogger.TestInfo(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Info('I1');
  CheckEvent(enabled, TLogLevel.Info, 'I1');

  Controller.Reset;
  fLogger.Info('I2', fException);
  CheckEvent(enabled, TLogLevel.Info, 'I2', fException);

  Controller.Reset;
  fLogger.Info('I%d', [3]);
  CheckEvent(enabled, TLogLevel.Info, 'I3');

  Controller.Reset;
  fLogger.Info('I%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Info, 'I4', fException);
end;

procedure TTestLogger.TestInfo;
begin
  Logger.Levels := [TLogLevel.Info];
  TestInfo(True);
  Logger.Levels := [];
  TestInfo(False);
end;

procedure TTestLogger.TestIsDebugEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Debug];
  CheckFalse(fLogger.IsDebugEnabled);

  Logger.Levels := [TLogLevel.Debug];
  CheckTrue(fLogger.IsDebugEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsDebugEnabled);
end;

procedure TTestLogger.TestIsEnabled;
var l: TLogLevel;
begin
  for l in LOG_ALL_LEVELS do
    CheckEquals(l in Logger.Levels, Logger.IsEnabled(l));
  Logger.Enabled := False;
  for l in LOG_ALL_LEVELS do
    CheckFalse(Logger.IsEnabled(l));

  Logger.Enabled := True;
  Logger.Levels := [];
  CheckFalse(Logger.IsEnabled(TLogLevel.Fatal, [TLogEventType.Text]));

  Logger.Levels := [TLogLevel.Fatal];
  Logger.EventTypes := [];
  CheckFalse(Logger.IsEnabled(TLogLevel.Fatal, [TLogEventType.Text]));

  Logger.Levels := [TLogLevel.Fatal];
  Logger.EventTypes := [TLogEventType.Text, TLogEventType.Value];
  CheckTrue(Logger.IsEnabled(TLogLevel.Fatal, [TLogEventType.Text]));
  CheckFalse(Logger.IsEnabled(TLogLevel.Fatal, [TLogEventType.CallStack]));
  CheckTrue(Logger.IsEnabled(TLogLevel.Fatal, [TLogEventType.CallStack,
    TLogEventType.Text]));

  ExpectedException := EArgumentException;
  Logger.IsEnabled(TLogLevel.Warn, []);
end;

procedure TTestLogger.TestIsErrorEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Error];
  CheckFalse(fLogger.IsErrorEnabled);

  Logger.Levels := [TLogLevel.Error];
  CheckTrue(fLogger.IsErrorEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsErrorEnabled);
end;

procedure TTestLogger.TestIsFatalEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Fatal];
  CheckFalse(fLogger.IsFatalEnabled);

  Logger.Levels := [TLogLevel.Fatal];
  CheckTrue(fLogger.IsFatalEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsFatalEnabled);
end;

procedure TTestLogger.TestIsInfoEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Info];
  CheckFalse(fLogger.IsInfoEnabled);

  Logger.Levels := [TLogLevel.Info];
  CheckTrue(fLogger.IsInfoEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsInfoEnabled);
end;

procedure TTestLogger.TestIsTextEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Text];
  CheckFalse(fLogger.IsTextEnabled);

  Logger.Levels := [TLogLevel.Text];
  CheckTrue(fLogger.IsTextEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsTextEnabled);
end;

procedure TTestLogger.TestIsTraceEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Trace];
  CheckFalse(fLogger.IsTraceEnabled);

  Logger.Levels := [TLogLevel.Trace];
  CheckTrue(fLogger.IsTraceEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsTraceEnabled);
end;

procedure TTestLogger.TestIsWarnEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Warn];
  CheckFalse(fLogger.IsWarnEnabled);

  Logger.Levels := [TLogLevel.Warn];
  CheckTrue(fLogger.IsWarnEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsWarnEnabled);
end;

procedure TTestLogger.TestLeaving;
begin
  Controller.Reset;

  Logger.Enabled := False;
  fLogger.Leave(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);

  Logger.Enabled := True;
  Logger.Levels := [TLogLevel.Warn];
  fLogger.Leave(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);

  fLogger.Leave(TLogLevel.Warn, nil, '');
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Leaving),  Ord(Controller.LastEvent.EventType));
  Controller.Reset;

  Logger.DefaultLevel := TLogLevel.Info;
  fLogger.Leave(TClass(nil), '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);

  Logger.DefaultLevel := TLogLevel.Warn;
  fLogger.Leave(TClass(nil), '');
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Leaving),  Ord(Controller.LastEvent.EventType));
  Controller.Reset;

  fLogger.Leave(TObject(nil), '');
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Leaving),  Ord(Controller.LastEvent.EventType));
  Controller.Reset;

  fLogger.Leave(Self, '');
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Leaving),  Ord(Controller.LastEvent.EventType));

  fLogger.Leave('');
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Leaving),  Ord(Controller.LastEvent.EventType));
end;

procedure TTestLogger.TestLevels;
begin
  Logger.Levels := [];
  Check([] = Logger.Levels, 'Levels');

  Logger.Levels := LOG_ALL_LEVELS;
  Check(LOG_ALL_LEVELS = Logger.Levels, 'Levels');
end;

procedure TTestLogger.TestLogEvent(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Log(TLogEvent.Create(TLogLevel.Fatal, 'L1'));
  CheckEvent(enabled, TLogLevel.Fatal, 'L1');

  Controller.Reset;
  fLogger.Log(TLogEvent.Create(TLogLevel.Error, 'L2').SetException(fException));
  CheckEvent(enabled, TLogLevel.Error, 'L2', fException);
end;

procedure TTestLogger.TestLogEvent;
begin
  Logger.Levels := [TLogLevel.Fatal, TLogLevel.Error];
  TestLogEvent(True);
  Logger.Levels := [];
  TestLogEvent(False);
end;

procedure TTestLogger.TestLoggerProperties;
var
  props: ILoggerProperties;
begin
  props := Logger as ILoggerProperties;

  Logger.Enabled := False;
  CheckFalse(props.Enabled);

  Logger.Enabled := True;
  CheckTrue(props.Enabled);

  props.Enabled := False;
  CheckFalse(Logger.Enabled);

  props.Enabled := True;
  CheckTrue(Logger.Enabled);

  Logger.Levels := [];
  Check([] = props.Levels, 'Levels');

  Logger.Levels := LOG_ALL_LEVELS;
  Check(LOG_ALL_LEVELS = props.Levels, 'Levels');

  props.Levels := [];
  Check([] = Logger.Levels, 'Levels');

  props.Levels := LOG_ALL_LEVELS;
  Check(LOG_ALL_LEVELS = Logger.Levels, 'Levels');
end;

procedure TTestLogger.TestText;
begin
  Logger.Levels := [TLogLevel.Text];
  TestText(True);
  Logger.Levels := [];
  TestText(False);
end;

procedure TTestLogger.TestTrack;
var
  result: IInterface;
begin
  Controller.Reset;

  Logger.Enabled := False;
  result := fLogger.Track(TLogLevel.Info, nil, '');
  CheckNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);

  Logger.Enabled := True;
  Logger.Levels := [TLogLevel.Warn];
  result := fLogger.Track(TLogLevel.Info, nil, '');
  CheckNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);

  result := fLogger.Track(TLogLevel.Warn, nil, '');
  CheckNotNull(result);
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Entering), Ord(Controller.LastEvent.EventType));
  Controller.Reset;
  result := nil;
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Leaving), Ord(Controller.LastEvent.EventType));

  Controller.Reset;

  Logger.EventTypes := [];
  result := fLogger.Track(TLogLevel.Warn, nil, '');
  CheckNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);
  Controller.Reset;

  Logger.EventTypes := [TLogEventType.Entering];
  result := fLogger.Track(TLogLevel.Warn, nil, '');
  CheckNotNull(result);
  CheckTrue(TLogLevel.Warn = Controller.LastEvent.Level);
  CheckEquals(Ord(TLogEventType.Entering), Ord(Controller.LastEvent.EventType));
  result := nil;
  CheckEquals(Ord(TLogEventType.Entering), Ord(Controller.LastEvent.EventType));
  Controller.Reset;

  Logger.EventTypes := [TLogEventType.Leaving];
  result := fLogger.Track(TLogLevel.Warn, nil, '');
  CheckNotNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);
  result := nil;
  CheckTrue(TLogLevel.Warn = Controller.LastEvent.Level);
  CheckEquals(Ord(TLogEventType.Leaving), Ord(Controller.LastEvent.EventType));
  Controller.Reset;

  Logger.EventTypes := [TLogEventType.Entering, TLogEventType.Leaving];
  Logger.Enabled := False;
  result := fLogger.Track(TLogLevel.Info, nil, '');
  CheckNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);

  Logger.Enabled := True;
  Logger.Levels := [TLogLevel.Warn];
  result := fLogger.Track(TLogLevel.Info, nil, '');
  CheckNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEvent.Level);

  result := fLogger.Track(TLogLevel.Warn, nil, '');
  CheckNotNull(result);
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Entering), Ord(Controller.LastEvent.EventType));
  Controller.Reset;
  result := nil;
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Leaving), Ord(Controller.LastEvent.EventType));
  Controller.Reset;

  Logger.DefaultLevel := TLogLevel.Info;
  result := fLogger.Track(TClass(nil), '');
  CheckNull(result);

  Logger.DefaultLevel := TLogLevel.Warn;
  result := fLogger.Track(TClass(nil), '');
  CheckNotNull(result);
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Entering), Ord(Controller.LastEvent.EventType));
  Controller.Reset;
  result := nil;
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Leaving), Ord(Controller.LastEvent.EventType));
  Controller.Reset;

  Logger.DefaultLevel := TLogLevel.Warn;
  result := fLogger.Track(TObject(nil), '');
  CheckNotNull(result);
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Entering), Ord(Controller.LastEvent.EventType));
  Controller.Reset;
  result := nil;
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Leaving), Ord(Controller.LastEvent.EventType));
  Controller.Reset;

  Logger.DefaultLevel := TLogLevel.Warn;
  result := fLogger.Track(Self, '');
  CheckNotNull(result);
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Entering), Ord(Controller.LastEvent.EventType));
  Controller.Reset;
  result := nil;
  CheckEquals(Ord(TLogLevel.Warn), Ord(Controller.LastEvent.Level));
  CheckEquals(Ord(TLogEventType.Leaving), Ord(Controller.LastEvent.EventType));
  Controller.Reset;
end;

procedure TTestLogger.TestText(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Text('T1');
  CheckEvent(enabled, TLogLevel.Text, 'T1');

  Controller.Reset;
  fLogger.Text('T2', fException);
  CheckEvent(enabled, TLogLevel.Text, 'T2', fException);

  Controller.Reset;
  fLogger.Text('T%d', [3]);
  CheckEvent(enabled, TLogLevel.Text, 'T3');

  Controller.Reset;
  fLogger.Text('T%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Text, 'T4', fException);
end;

procedure TTestLogger.TestTrace;
begin
  Logger.Levels := [TLogLevel.Trace];
  TestTrace(True);
  Logger.Levels := [];
  TestTrace(False);
end;

procedure TTestLogger.TestTrace(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Trace('V1');
  CheckEvent(enabled, TLogLevel.Trace, 'V1');

  Controller.Reset;
  fLogger.Trace('V2', fException);
  CheckEvent(enabled, TLogLevel.Trace, 'V2', fException);

  Controller.Reset;
  fLogger.Trace('V%d', [3]);
  CheckEvent(enabled, TLogLevel.Trace, 'V3');

  Controller.Reset;
  fLogger.Trace('V%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Trace, 'V4', fException);
end;

procedure TTestLogger.TestWarn;
begin
  Logger.Levels := [TLogLevel.Warn];
  TestWarn(True);
  Logger.Levels := [];
  TestWarn(False);
end;

procedure TTestLogger.TestWarn(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Warn('W1');
  CheckEvent(enabled, TLogLevel.Warn, 'W1');

  Controller.Reset;
  fLogger.Warn('W2', fException);
  CheckEvent(enabled, TLogLevel.Warn, 'W2', fException);

  Controller.Reset;
  fLogger.Warn('W%d', [3]);
  CheckEvent(enabled, TLogLevel.Warn, 'W3');

  Controller.Reset;
  fLogger.Warn('W%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Warn, 'W4', fException);
end;

procedure TTestLogger.TestLog(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Log(TLogLevel.Fatal, 'l1');
  CheckEvent(enabled, TLogLevel.Fatal, 'l1');

  Controller.Reset;
  fLogger.Log(TLogLevel.Error, 'l2', fException);
  CheckEvent(enabled, TLogLevel.Error, 'l2', fException);

  Controller.Reset;
  fLogger.Log(TLogLevel.Fatal, 'l%d', [3]);
  CheckEvent(enabled, TLogLevel.Fatal, 'l3');

  Controller.Reset;
  fLogger.Log(TLogLevel.Error, 'l%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Error, 'l4', fException);
end;

procedure TTestLogger.TestLog;
begin
  Logger.Levels := [TLogLevel.Fatal, TLogLevel.Error];
  TestLog(True);
  Logger.Levels := [];
  TestLog(False);
end;

{$ENDREGION}


{$REGION 'TAppenderTestCase'}

procedure TAppenderTestCase.SetUp;
begin
  inherited;
  fController := TLoggerController.Create;
  fLogger := TLogger.Create(fController);
end;

procedure TAppenderTestCase.TearDown;
begin
  inherited;
  fLogger := nil;
  fController := nil;
  fAppender := nil;
end;

{$ENDREGION}


{$REGION 'TTestLogAppenderBase'}

procedure TTestLogAppenderBase.TestFormatMethodName;
var
  result: string;
begin
    result := TLogAppenderBase.FormatMethodName(nil, 'MethodName');
    CheckEquals('MethodName', result);

    result := TLogAppenderBase.FormatMethodName(ClassType, 'MethodName');
    CheckEquals('Spring.Tests.Logging.TTestLogAppenderBase.MethodName', result);
end;

{$ENDREGION}


{$REGION 'TTestStreamLogAppender'}

procedure TTestLogAppenderBase.TestFormatMsg;
var
  result, s: string;
  e: Exception;
  len: Integer;
begin
  result := TLogAppenderBase.FormatMsg(TLogEvent.Create(TLogLevel.Unknown,
    'message'));
  CheckEquals('message', result);

  e := Exception.Create('exception');
  try
    SetLength(s, 1024);
    len := ExceptionErrorMessage(e, nil, @s[1], Length(s));
    SetLength(s, len);
    result := TLogAppenderBase.FormatMsg(TLogEvent.Create(TLogLevel.Unknown,
      'message', e));
    CheckEquals('message: ' + s, result);
  finally
    e.Free;
  end;
end;

procedure TTestLogAppenderBase.TestFormatText;
var
  result: string;
begin
  result := TLogAppenderBase. FormatText(TLogEvent.Create(TLogLevel.Unknown,
    'message'));
  CheckEquals('message', result);

  result := TLogAppenderBase. FormatText(TLogEvent.Create(TLogLevel.Unknown,
    TLogEventType.SerializedData, 'message'));
  CheckEquals('message', result);

  result := TLogAppenderBase. FormatText(TLogEvent.Create(TLogLevel.Unknown,
    TLogEventType.CallStack, 'message'));
  CheckEquals('message', result);

  result := TLogAppenderBase.FormatText(TLogEvent.Create(TLogLevel.Unknown,
    TLogEventType.Entering, 'message', nil));
  CheckEquals('Entering: message', result);

  result := TLogAppenderBase.FormatText(TLogEvent.Create(TLogLevel.Unknown,
    TLogEventType.Leaving, 'message', nil));
  CheckEquals('Leaving: message', result);
end;

procedure TTestLogAppenderBase.TestIsEnabled;
var
  appender: TLogAppenderBaseAccess;
begin
  appender := TLogAppenderBaseAccess(TAppenderMock.Create);
  try
    //Just make sure we have the right parent class
    Assert(appender is TLogAppenderBase);

    appender.Enabled := False;
    CheckFalse(appender.IsEnabled(TLogLevel.Fatal, [TLogEventType.Text]));

    appender.Enabled := True;
    appender.Levels := [];
    CheckFalse(appender.IsEnabled(TLogLevel.Fatal, [TLogEventType.Text]));

    appender.Levels := [TLogLevel.Fatal];
    appender.EventTypes := [];
    CheckFalse(appender.IsEnabled(TLogLevel.Fatal, [TLogEventType.Text]));

    appender.Levels := [TLogLevel.Fatal];
    appender.EventTypes := [TLogEventType.Text, TLogEventType.Value];
    CheckTrue(appender.IsEnabled(TLogLevel.Fatal, [TLogEventType.Text]));
    CheckFalse(appender.IsEnabled(TLogLevel.Fatal, [TLogEventType.CallStack]));
  finally
    appender.Free;
  end;
end;

procedure TTestStreamLogAppender.SetUp;
begin
  inherited;
  fStream := TStringStream.Create;
  fAppender := TStreamLogAppender.Create(fStream);
  fController.AddAppender(fAppender);
end;

procedure TTestStreamLogAppender.TearDown;
begin
  inherited;
  fStream := nil;
end;

procedure TTestStreamLogAppender.TestException;
var
  s: string;
  len: Integer;
begin
  try
    raise EAbort.CreateRes(@SOperationAborted) at nil;
  except
    on e: EAbort do
    begin
      fLogger.Error('', e);
      SetLength(s, 1024);
      len := ExceptionErrorMessage(e, nil, @s[1], Length(s));
      SetLength(s, len);
      CheckEquals('[ERROR] ' + s + sLineBreak, Copy(fStream.DataString, 15, MaxInt));
      fStream.Clear;
      fLogger.Error('With message', e);
      CheckEquals('[ERROR] With message: ' + s + sLineBreak,
        Copy(fStream.DataString, 15, MaxInt));
    end;
  end;
end;

procedure TTestStreamLogAppender.TestWrite;
begin
  fLogger.Info('Test');
  CheckTrue(EndsStr('[INFO ] Test' + sLineBreak, fStream.DataString));
end;

{$ENDREGION}


end.

