program Spring.Tests.Logging.CodeSite;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  Spring.Logging,
  Spring.Logging.Controller,
  Spring.Logging.Loggers,
  Spring.Logging.Appenders.CodeSite in '..\Source\Base\Logging\Spring.Logging.Appenders.CodeSite.pas';

function InitLog: ILogger;
var
  controller: TLoggerController;
  appender: TCodeSiteAppender;
begin
  appender := TCodeSiteAppender.Create;
  appender.Levels := LOG_ALL_LEVELS;

  controller := TLoggerController.Create;
  controller.Levels := LOG_ALL_LEVELS;
  controller.AddAppender(appender);

  Result := TLogger.Create(controller);
  (Result as ILoggerProperties).Levels := LOG_ALL_LEVELS;
end;

var
  log: ILogger;

procedure TestTrack;
begin
  log.Track(TLogLevel.Info, TLogger, 'TestTrack');
  log.Warn('Warning text');
  log.Log(TLogEntry.Create(TLogLevel.Info, 'Log message with color')
    .SetColor($5555FF));
end;

procedure TestException;
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

procedure TestValues;
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

begin
  log := InitLog;
  log.Enter(TLogLevel.Info, nil, 'Spring.Tests.Logging.CodeSite');
  log.Fatal('Fatal message');
  log.Error('Error message');
  log.Info('Info message');
  log.Text('Text message');
  log.Debug('Debug message');
  log.Trace('Verbose message');
  TestTrack;
  TestException;
  TestValues;
  log.Leave(TLogLevel.Info, nil, 'Spring.Tests.Logging.CodeSite');
end.
