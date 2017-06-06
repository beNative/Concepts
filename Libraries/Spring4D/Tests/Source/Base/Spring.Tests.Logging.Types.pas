{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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

unit Spring.Tests.Logging.Types;

{$I Spring.Tests.inc}

interface

uses
  Classes,
  Rtti,
  TestFramework,
  TypInfo,
  Spring,
  Spring.Container.Common,
  Spring.Logging,
  Spring.Logging.Appenders.Base,
  Spring.Logging.Controller,
  Spring.Logging.Extensions,
  Spring.Logging.Loggers,
  Spring.Logging.Serializers;

type
  TAppenderMock = class(TLogAppenderBase)
  private
    fWriteCount: Integer;
    fEvent: TLogEvent;
    fSomeFloat: Extended;
    fSomeInt: Integer;
    fSomeEnum: TLogLevel;
    fSomeString: string;
    function GetWriteCalled: Boolean;
  protected
    procedure DoSend(const event: TLogEvent); override;
  public
    property Event: TLogEvent read fEvent;
    property WriteCalled: Boolean read GetWriteCalled;
    property WriteCount: Integer read fWriteCount;
    property SomeFloat: Extended read fSomeFloat write fSomeFloat;
    property SomeInt: Integer read fSomeInt write fSomeInt;
    property SomeEnum: TLogLevel read fSomeEnum write fSomeEnum;
    property SomeString: string read fSomeString write fSomeString;
  end;

  TAppenderMock2 = class(TLogAppenderBase)
  protected
    procedure DoSend(const event: TLogEvent); override;
  end;

  TLoggerControllerMock = class(TLoggerBase, ILoggerController, ILogAppender)
  private
    fLastEvent: TLogEvent;
    fAppnederOnly: Boolean;
  public
    procedure AddAppender(const appender: ILogAppender);
    procedure AddEventConverter(const converter: ILogEventConverter);
    procedure Send(const event: TLogEvent);
    procedure SendToAppenders(const event: TLogEvent);
    procedure Reset;
    function GetEnabled: Boolean;
    function GetLevels: TLogLevels;
    function GetEventTypes: TLogEventTypes;
    function IsLoggable(level: TLogLevel; eventTypes: TLogEventTypes): Boolean;
    property LastEvent: TLogEvent read fLastEvent;
  end;

  TLoggerController2 = class(TLoggerController);

  TLoggerDefault = class(TLogger);
  TLogger1 = class(TLogger);
  TLogger2 = class(TLogger);

  IService = interface
    ['{DFFC820C-120B-4828-8D22-21DDC60E4398}']
  end;

  TImpl = class(TInterfacedObject, IService)
  private
    [Inject]
    fLogger1: ILogger;
    [Inject('logging.logger2')]
    fLogger2: ILogger;
  public
    property Logger1: ILogger read fLogger1;
    property Logger2: ILogger read fLogger2;
  end;

  TObjProc = class
  private
    fLogger: ILogger;
  public
    [Inject]
    procedure SetLoggger(const logger: ILogger);
    property Logger: ILogger read fLogger;
  end;

  TObjCtor = class
  private
    fLogger: ILogger;
  public
    constructor Create(const logger: ILogger);
    property Logger: ILogger read fLogger;
  end;

  TObjLazy = class
  private
    [Inject]
    fLogger1: Lazy<ILogger>;
    [Inject('logging.logger2')]
    fLogger2: Lazy<ILogger>;
  public
    property Logger1: Lazy<ILogger> read fLogger1;
    property Logger2: Lazy<ILogger> read fLogger2;
  end;

  TSomeRecord = record
  end;

  TTypeSerializerMock = class(TSerializerBase, ITypeSerializer)
  private
    fHandlesTypeCount: Integer;
  public
    function CanHandleType(typeInfo: PTypeInfo): Boolean; override;
    function Serialize(const controller: ISerializerController;
      const value: TValue; nestingLevel: Integer = 0): string; override;

    property HandlesTypeCount: Integer read fHandlesTypeCount;
  end;

  TTypeSerializerMock2 = class(TTypeSerializerMock);

  TSampleObject = class(TInterfacedObject, IInterface)
{$IFDEF AUTOREFCOUNT}
  private class var
    fInstances: Integer;
{$ENDIF}
  private
    FROProp: Boolean;
  public
    {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
    fObject: TObject;
    fString: string;
    property PObject: TObject read fObject;
    property PString: string read fString write fString;
    property ROProp: Boolean write fROProp;
{$IFDEF AUTOREFCOUNT}
  public
    class function NewInstance: TObject unsafe; override;
    procedure FreeInstance; override;
    class property Instances: Integer read fInstances;
{$ENDIF}
  end;

  TSampleRecord = record
  public
    fObject: TObject;
    fString: string;
    property PObject: TObject read fObject write fObject;
    property PString: string read fString;
  end;

  TStackTraceCollector = class(TInterfaceBase, IStackTraceCollector)
  public
    function Collect: TArray<Pointer>; virtual; abstract;
  end;

  TStackTraceFormatter = class(TInterfaceBase, IStackTraceFormatter)
  public
    function Format(const stack: TArray<Pointer>): TArray<string>; virtual; abstract;
  end;

implementation


{$REGION 'TAppenderMock'}

procedure TAppenderMock.DoSend(const event: TLogEvent);
begin
  Inc(fWriteCount);
  fEvent := event;
end;

function TAppenderMock.GetWriteCalled: Boolean;
begin
  Result := fWriteCount > 0;
end;

{$ENDREGION}


{$REGION 'TObjProc'}

procedure TObjProc.SetLoggger(const logger: ILogger);
begin
  fLogger := logger;
end;

{$ENDREGION}


{$REGION 'TObjCtor'}

constructor TObjCtor.Create(const logger: ILogger);
begin
  fLogger := logger;
end;

{$ENDREGION}


{$REGION 'TLoggerControllerMock'}

procedure TLoggerControllerMock.AddAppender(const appender: ILogAppender);
begin
  raise ETestError.Create('Should be inaccessible');
end;

procedure TLoggerControllerMock.AddEventConverter(
  const converter: ILogEventConverter);
begin
  raise ETestError.Create('Should be inaccessible');
end;

function TLoggerControllerMock.GetEnabled: Boolean;
begin
  Result := True;
end;

function TLoggerControllerMock.GetEventTypes: TLogEventTypes;
begin
  Result := LOG_ALL_EVENT_TYPES;
end;

function TLoggerControllerMock.GetLevels: TLogLevels;
begin
  Result := LOG_ALL_LEVELS;
end;

function TLoggerControllerMock.IsLoggable(level: TLogLevel;
  eventTypes: TLogEventTypes): Boolean;
begin
  Result := True;
end;

procedure TLoggerControllerMock.Reset;
begin
  fLastEvent := TLogEvent.Create(TLogLevel.Unknown, '');
end;

procedure TLoggerControllerMock.Send(const event: TLogEvent);
begin
  fLastEvent := event;
  fAppnederOnly := False;
end;

procedure TLoggerControllerMock.SendToAppenders(const event: TLogEvent);
begin
  fLastEvent := event;
  fAppnederOnly := True;
end;

{$ENDREGION}


{$REGION 'TAppenderMock2'}

procedure TAppenderMock2.DoSend(const event: TLogEvent);
begin
end;

{$ENDREGION}


{$REGION 'TTypeSerializerMock'}

function TTypeSerializerMock.CanHandleType(typeInfo: PTypeInfo): Boolean;
begin
  Inc(fHandlesTypeCount);
  Result := typeInfo.Kind = tkInteger;
end;

function TTypeSerializerMock.Serialize(const controller: ISerializerController;
  const value: TValue; nestingLevel: Integer): string;
begin
  Result := '';
end;

{$ENDREGION}


{$REGION 'TSampleObject'}

{$IFDEF AUTOREFCOUNT}
procedure TSampleObject.FreeInstance;
begin
  inherited;
  AtomicDecrement(fInstances);
end;

class function TSampleObject.NewInstance: TObject;
begin
  Result := inherited;
  AtomicIncrement(fInstances);
end;
{$ENDIF}

{$ENDREGION}


end.
