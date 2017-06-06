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

{$I Spring.inc}

unit Spring.Logging.Controller;

interface

uses
  TypInfo,
  Spring.Collections,
  Spring.Logging,
  Spring.Logging.Appenders.Base,
  Spring.Logging.Extensions,
  Spring.Logging.Loggers;

type
  {$REGION 'TLoggerController'}

  TLoggerController = class(TLogAppenderBase, ILoggerController, ISerializerController)
  private
    fAppenders: IList<ILogAppender>;
    fConverters: IList<ILogEventConverter>;
  protected
    procedure DoSend(const event: TLogEvent); override;
  public
    constructor Create; overload;
    constructor Create(const appenders: TArray<ILogAppender>); overload;
    constructor Create(const appenders: array of ILogAppender); overload;

    procedure AddAppender(const appender: ILogAppender);
    procedure AddEventConverter(const converter: ILogEventConverter);
    function FindSerializer(typeInfo: PTypeInfo): ITypeSerializer;
    function IsLoggable(level: TLogLevel; eventTypes: TLogEventTypes): Boolean;
    procedure SendToAppenders(const event: TLogEvent);
  end;

  {$ENDREGION}


implementation

uses
  Spring;


{$REGION 'TLoggerController'}

constructor TLoggerController.Create;
begin
  inherited Create;
  fAppenders := TCollections.CreateInterfaceList<ILogAppender>;
  fConverters := TCollections.CreateInterfaceList<ILogEventConverter>;
end;

constructor TLoggerController.Create(const appenders: TArray<ILogAppender>);
begin
  Create;
  fAppenders.AddRange(appenders);
end;

constructor TLoggerController.Create(const appenders: array of ILogAppender);
begin
  Create;
  fAppenders.AddRange(appenders);
end;

procedure TLoggerController.AddAppender(const appender: ILogAppender);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(appender, 'appender');
{$ENDIF}
  fAppenders.Add(appender);
end;

procedure TLoggerController.AddEventConverter(
  const converter: ILogEventConverter);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(converter, 'converter');
{$ENDIF}
  fConverters.Add(converter);
end;

procedure TLoggerController.DoSend(const event: TLogEvent);

  procedure HandleConverters;
  var
    converter: ILogEventConverter;
  begin
    for converter in fConverters do
      if converter.HandleEvent(Self, event) then
        Break;
  end;

begin
  SendToAppenders(event);

  if not fConverters.IsEmpty then
    HandleConverters;
end;

function TLoggerController.FindSerializer(typeInfo: PTypeInfo): ITypeSerializer;
var
  converter: ILogEventConverter;
  serializer: ITypeSerializer;
begin
  for converter in fConverters do
    if converter.QueryInterface(ITypeSerializer, serializer) = S_OK then
      if serializer.CanHandleType(typeInfo) then
        Exit(serializer);

  Result := nil;
end;

function TLoggerController.IsLoggable(level: TLogLevel;
  eventTypes: TLogEventTypes): Boolean;
var
  appender: ILogAppender;
begin
  if IsEnabled(level, eventTypes) then
    for appender in fAppenders do
      if appender.IsEnabled(level, eventTypes) then
        Exit(True);

  Result := False;
end;

procedure TLoggerController.SendToAppenders(const event: TLogEvent);
var
  appender: ILogAppender;
begin
  for appender in fAppenders do
    appender.Send(event);
end;

{$ENDREGION}


end.
