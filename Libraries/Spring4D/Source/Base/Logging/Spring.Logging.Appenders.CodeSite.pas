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

unit Spring.Logging.Appenders.CodeSite;

{$I Spring.inc}

interface

uses
  Spring.Logging,
  Spring.Logging.Appenders.Base;

type
  TCodeSiteAppender = class(TLogAppenderBase)
  protected
    procedure DoSend(const event: TLogEvent); override;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  CodeSiteLogging,
  Spring;


{$REGION 'TCodeSiteAppender'}

procedure TCodeSiteAppender.DoSend(const event: TLogEvent);
begin
  if event.Color = clDefault then
    CodeSite.CategoryColor := $FFFFFF
  else
    CodeSite.CategoryColor := event.Color;

  case event.EventType of
    TLogEventType.Text:
      if Assigned(event.Exception) then
        CodeSite.SendException(TLogAppenderBase.FormatMsg(event), event.Exception)
      else
        case event.Level of
          TLogLevel.Unknown: ;

          TLogLevel.Trace:
            CodeSite.SendNote(event.Msg);

          TLogLevel.Debug,
          TLogLevel.Text:
            CodeSite.SendMsg(event.Msg);

          TLogLevel.Info:
            CodeSite.SendReminder(event.Msg);

          TLogLevel.Warn:
            CodeSite.SendWarning(event.Msg);

          TLogLevel.Error,
          TLogLevel.Fatal:
            CodeSite.SendError(event.Msg);
        end;

    TLogEventType.Value:
      case event.Data.Kind of
        tkClass: CodeSite.Send(event.Msg, event.Data.AsObject);
      else
        CodeSite.Send(event.Msg, event.Data.ToString);
      end;

    TLogEventType.Entering:
      CodeSite.EnterMethod(
        TLogAppenderBase.FormatMethodName(event.ClassType, event.Msg));

    TLogEventType.Leaving:
      CodeSite.ExitMethod(
        TLogAppenderBase.FormatMethodName(event.ClassType, event.Msg));

    TLogEventType.CallStack,
    TLogEventType.SerializedData:
      CodeSite.Send(event.Msg);
  end;
end;

{$ENDREGION}


end.
