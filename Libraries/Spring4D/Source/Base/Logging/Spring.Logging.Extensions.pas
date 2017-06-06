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

unit Spring.Logging.Extensions;

interface

uses
  Rtti,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Logging;

type
  ISerializerController = interface;

  /// <summary>
  ///   Serializer that can convert some types to string
  /// </summary>
  ITypeSerializer = interface
    ['{CF783059-AD29-468C-8BAF-F2FE0EAE6FE7}']
    function CanHandleType(typeInfo: PTypeInfo): Boolean;
    function Serialize(const controller: ISerializerController;
      const value: TValue; nestingLevel: Integer = 0): string;
  end;

  /// <summary>
  ///   Controller that can be used by nested serializers to convert more
  ///   complex data
  /// </summary>
  ISerializerController = interface
    ['{6390E2C6-C415-4C7A-8FBF-975B331B90B2}']
    function FindSerializer(typeInfo: PTypeInfo): ITypeSerializer;
  end;

  IStackTraceCollector = interface
    ['{59259AF9-2E14-4AC4-B14E-C4CF0AFD0A44}']
    function Collect: TArray<Pointer>;
  end;

  IStackTraceFormatter = interface
    ['{515E564E-5EB4-4B6D-B74E-4080AB0E5D8C}']
    function Format(const stack: TArray<Pointer>): TArray<string>;
  end;

  TLogEventConverterBase = class abstract(TInterfacedObject, ILogEventConverter)
  private
    fEventType: TLogEventType;
    function GetEventType: TLogEventType; inline;
  strict protected
    constructor Create(eventType: TLogEventType);

    function CanHandleEvent(const event: TLogEvent): Boolean; virtual; abstract;
    function Execute(const controller: ILoggerController;
      const event: TLogEvent): string; virtual; abstract;
  public
    function HandleEvent(const controller: ILoggerController;
      const event: TLogEvent): Boolean;
  end;

  TCallStackEventConverter = class(TLogEventConverterBase)
  private
    fCollector: IStackTraceCollector;
    fFormatter: IStackTraceFormatter;
  strict protected
    function CanHandleEvent(const event: TLogEvent): Boolean; override;
    function Execute(const controller: ILoggerController;
      const event: TLogEvent): string; override;
  public
    constructor Create(const collector: IStackTraceCollector;
      const formatter: IStackTraceFormatter);
  end;

implementation


{$REGION 'TLogEventConverterBase'}

constructor TLogEventConverterBase.Create(eventType: TLogEventType);
begin
  inherited Create;
  fEventType := eventType;
end;

function TLogEventConverterBase.GetEventType: TLogEventType;
begin
  Result := fEventType;
end;

function TLogEventConverterBase.HandleEvent(const controller: ILoggerController;
  const event: TLogEvent): Boolean;

  procedure DoExecute;
  var
    msg: string;
    converted: TLogEvent;
  begin
    msg := Execute(controller, event);
    converted := TLogEvent.Create(event.Level, fEventType, msg);
    controller.SendToAppenders(converted);
  end;

begin
  Result := CanHandleEvent(event);
  if Result then
    if controller.IsLoggable(event.Level, [fEventType]) then
      DoExecute;
end;

{$ENDREGION}


{$REGION 'TCallStackEventConverter'}

constructor TCallStackEventConverter.Create(
  const collector: IStackTraceCollector; const formatter: IStackTraceFormatter);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(collector, 'collector');
  Guard.CheckNotNull(formatter, 'formatter');
{$ENDIF}
  inherited Create(TLogEventType.CallStack);
  fCollector := collector;
  fFormatter := formatter;
end;

function TCallStackEventConverter.Execute(const controller: ILoggerController;
  const event: TLogEvent): string;
var
  stack: TArray<Pointer>;
  formatted: TArray<string>;
  s: string;
begin
  stack := fCollector.Collect;
  formatted := fFormatter.Format(stack);
  Result := '';
  for s in formatted do
  begin
    if Result <> '' then
      Result := Result + sLineBreak;
    Result := Result + s;
  end;
end;

function TCallStackEventConverter.CanHandleEvent(const event: TLogEvent): Boolean;
begin
  Result := event.AddStackValue;
end;

{$ENDREGION}


end.
