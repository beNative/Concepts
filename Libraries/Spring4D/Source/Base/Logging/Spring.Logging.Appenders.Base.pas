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

unit Spring.Logging.Appenders.Base;

{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  SysUtils,
  Spring,
  Spring.Logging,
  Spring.Logging.Loggers;

type
  {$REGION 'TLogAppenderBase'}

  TLogAppenderBase = class abstract(TLoggerBase, ILogAppender)
  private
  {$REGION 'Helper constants and functions'}
  protected const
    //May or may not be used by descendants, its here just for convenience
    LEVEL: array[TLogLevel] of string = (
      '[UNKNOWN]',
      '[VERBOSE]',
      '[DEBUG]',
      '[TEXT]',
      '[INFO]',
      '[WARN]',
      '[ERROR]',
      '[FATAL]'
    );
    LEVEL_FIXED: array[TLogLevel] of string = (
      '[UNK  ]',
      '[VERB ]',
      '[DEBUG]',
      '[TEXT ]',
      '[INFO ]',
      '[WARN ]',
      '[ERROR]',
      '[FATAL]'
    );
  public
    class function FormatText(const event: TLogEvent): string; static; inline;
    class function FormatMsg(const event: TLogEvent): string; static; inline;
    class function FormatException(const e: Exception): string; static; //noinline
    class function FormatMethodName(classType: TClass;
      const methodName: string): string; static; inline;
    class function FormatEntering(classType: TClass;
      const methodName: string): string; static; inline;
    class function FormatLeaving(classType: TClass;
      const methodName: string): string; static; inline;
  {$ENDREGION}
  protected
    procedure DoSend(const event: TLogEvent); virtual; abstract;
  public
    procedure Send(const event: TLogEvent);
  end;

  {$ENDREGION}


implementation

uses
  Spring.Logging.ResourceStrings;


{$REGION 'TLogAppenderBase'}

class function TLogAppenderBase.FormatException(const e: Exception): string;
var
  len: Integer;
begin
  SetLength(Result, 1024);
  len := ExceptionErrorMessage(e, ExceptAddr, @Result[1], Length(Result));
  SetLength(Result, len);
end;

class function TLogAppenderBase.FormatMethodName(classType: TClass;
  const methodName: string): string;
begin
  if Assigned(classType) then
    Result := GetQualifiedClassName(classType) + '.' + methodName
  else
    Result := methodName;
end;

class function TLogAppenderBase.FormatEntering(classType: TClass;
  const methodName: string): string;
begin
  Result := SLogEntering + FormatMethodName(classType, methodName);
end;

class function TLogAppenderBase.FormatLeaving(classType: TClass;
  const methodName: string): string;
begin
  Result := SLogLeaving + FormatMethodName(classType, methodName);
end;

class function TLogAppenderBase.FormatText(const event: TLogEvent): string;
begin
  case event.EventType of
    TLogEventType.Text,
    TLogEventType.SerializedData,
    TLogEventType.CallStack:
      Result := event.Msg;

    TLogEventType.Entering:
      Result := FormatEntering(event.ClassType, event.Msg);

    TLogEventType.Leaving:
      Result := FormatLeaving(event.ClassType, event.Msg);
  end;
end;

class function TLogAppenderBase.FormatMsg(const event: TLogEvent): string;
begin
  if event.Exception = nil then
    Result := FormatText(event)
  else
    if event.Msg <> '' then
      Result := FormatText(event) + ': ' + FormatException(event.Exception)
    else
      Result := FormatException(event.Exception);
end;

procedure TLogAppenderBase.Send(const event: TLogEvent);
begin
  if IsEnabled(event.Level, [event.EventType]) then
    DoSend(event);
end;

{$ENDREGION}


end.
