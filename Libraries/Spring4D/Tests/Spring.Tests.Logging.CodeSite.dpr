program Spring.Tests.Logging.CodeSite;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  Spring.Logging,
  Spring.Logging.Appenders.CodeSite in '..\Source\Base\Logging\Spring.Logging.Appenders.CodeSite.pas',
//  Spring.Logging.Appenders.SmartInspect in '..\Source\Base\Logging\Spring.Logging.Appenders.SmartInspect.pas',
  Spring.Logging.Controller,
  Spring.Logging.Loggers;

function InitLog: ILogger;
var
  appender: ILogAppender;
  controller: ILoggerController;
begin
  appender := TCodeSiteAppender.Create;
//  appender := TSmartInspectAppender.Create;
  (appender as ILoggerProperties).Levels := LOG_ALL_LEVELS;

  controller := TLoggerController.Create;
  controller.AddAppender(appender);
  (controller as ILoggerProperties).Levels := LOG_ALL_LEVELS;

  Result := TLogger.Create(controller);
  (Result as ILoggerProperties).Levels := LOG_ALL_LEVELS;
end;

procedure TestTrack(const log: ILogger);
begin
  log.Track(TLogLevel.Info, TLogger, 'TestTrack');
  log.Warn('Warning text');
  log.Log(TLogEvent.Create(TLogLevel.Info, 'Log message with color')
    .SetColor($5555FF));
end;

procedure TestException(const log: ILogger);
begin
  log.Track(TLogLevel.Info, TLogger, 'TestException');
  try
    Abort;
  except
    on E: EAbort do
    begin
      log.Error('', E);
      log.Error('With message', E);
    end;
  end;
end;

procedure TestValues(const log: ILogger);
var
  c: TComponent;
begin
  log.Track(nil, 'TestValue');
  c := TComponent.Create(nil);
  try
    c.Name := 'SomeName';
    log.LogValue('c', c);
  finally
    c.Free;
  end;

  log.LogValue('integer', 5);
end;

var
  log: ILogger;
begin
  log := InitLog;
  log.Enter(TLogLevel.Info, nil, 'Spring.Tests.Logging.CodeSite');
  log.Fatal('Fatal message');
  log.Error('Error message');
  log.Info('Info message');
  log.Text('Text message');
  log.Debug('Debug message');
  log.Trace('Verbose message');
  TestTrack(log);
  TestException(log);
  TestValues(log);
  log.Leave(TLogLevel.Info, nil, 'Spring.Tests.Logging.CodeSite');
end.
