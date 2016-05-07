{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2016 Spring4D Team                           }
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
  Spring.Logging.Loggers;

type
  TAppenderMock = class(TLogAppenderBase)
  private
    fWriteCount: Integer;
    fEntry: TLogEntry;
    fSomeFloat: Extended;
    fSomeInt: Integer;
    fSomeEnum: TLogLevel;
    fSomeString: string;
    function GetWriteCalled: Boolean;
  protected
    procedure DoSend(const entry: TLogEntry); override;
  public
    property Entry: TLogEntry read fEntry;
    property WriteCalled: Boolean read GetWriteCalled;
    property WriteCount: Integer read fWriteCount;
    property SomeFloat: Extended read fSomeFloat write fSomeFloat;
    property SomeInt: Integer read fSomeInt write fSomeInt;
    property SomeEnum: TLogLevel read fSomeEnum write fSomeEnum;
    property SomeString: string read fSomeString write fSomeString;
  end;

  TAppenderMock2 = class(TLogAppenderBase)
  protected
    procedure DoSend(const entry: TLogEntry); override;
  end;

  TLoggerControllerMock = class(TInterfacedObject, ILoggerController, ILogAppender)
  private
    fLastEntry: TLogEntry;
  public

    procedure AddAppender(const appedner: ILogAppender);
    procedure Send(const entry: TLogEntry);
    procedure Reset;
    function GetEnabled: Boolean;
    function GetLevels: TLogLevels;
    function GetEntryTypes: TLogEntryTypes;
    property LastEntry: TLogEntry read FLastEntry;
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

  TTypeSerializerMock = class(TInterfacedObject, ITypeSerializer)
  private
    fHandlesTypeCount: Integer;
  public
    function HandlesType(typeInfo: PTypeInfo): Boolean;
    function Serialize(const controller: ISerializerController;
      const value: TValue; nestingLevel: Integer = 0): string;

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

implementation


{$REGION 'TAppenderMock'}

procedure TAppenderMock.DoSend(const entry: TLogEntry);
begin
  Inc(fWriteCount);
  fEntry := entry;
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

procedure TLoggerControllerMock.AddAppender(const appedner: ILogAppender);
begin
  raise ETestError.Create('Should be inaccessible');
end;

function TLoggerControllerMock.GetEnabled: Boolean;
begin
  Result := True;
end;

function TLoggerControllerMock.GetEntryTypes: TLogEntryTypes;
begin
  Result := LOG_ALL_ENTRY_TYPES;
end;

function TLoggerControllerMock.GetLevels: TLogLevels;
begin
  Result := LOG_ALL_LEVELS;
end;

procedure TLoggerControllerMock.Reset;
begin
  fLastEntry := TLogEntry.Create(TLogLevel.Unknown, '');
end;

procedure TLoggerControllerMock.Send(const entry: TLogEntry);
begin
  fLastEntry := entry;
end;

{$ENDREGION}


{$REGION 'TAppenderMock2'}

procedure TAppenderMock2.DoSend(const entry: TLogEntry);
begin
end;

{$ENDREGION}


{$REGION 'TTypeSerializerMock'}

function TTypeSerializerMock.HandlesType(typeInfo: PTypeInfo): Boolean;
begin
  Inc(fHandlesTypeCount);
  Result := typeInfo^.Kind = tkInteger;
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
