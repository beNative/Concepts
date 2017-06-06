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

unit Spring.Logging.Appenders.SmartInspect;

{$I Spring.inc}

interface

uses
  Spring.Logging,
  Spring.Logging.Appenders.Base;

type
  TSmartInspectAppender = class(TLogAppenderBase)
  protected
    procedure DoSend(const event: TLogEvent); override;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  SiAuto,
  SmartInspect,
  Spring;


{$REGION 'TCodeSiteAppender'}

constructor TSmartInspectAppender.Create;
begin
  inherited;
  Si.Enabled := True;
end;

procedure TSmartInspectAppender.DoSend(const event: TLogEvent);
begin
  case event.EventType of
    TLogEventType.Text:
      if Assigned(event.Exception) then
        SiMain.LogException(event.Exception, TLogAppenderBase.FormatMsg(event))
      else
        case event.Level of
          TLogLevel.Unknown: ;

          TLogLevel.Trace:
            SiMain.LogVerbose(event.Msg);

          TLogLevel.Debug:
            SiMain.LogDebug(event.Msg);

          TLogLevel.Text,
          TLogLevel.Info:
            SiMain.LogMessage(event.Msg);

          TLogLevel.Warn:
            SiMain.LogWarning(event.Msg);

          TLogLevel.Error:
            SiMain.LogError(event.Msg);

          TLogLevel.Fatal:
            SiMain.LogFatal(event.Msg);
        end;

    TLogEventType.Value:
      case event.Data.Kind of
        tkInteger: SiMain.LogValue(event.Msg, event.Data.AsInteger);
        tkEnumeration:
          if event.Data.TypeInfo = TypeInfo(Boolean) then
            SiMain.LogValue(event.Msg, event.Data.AsBoolean)
          else
            SiMain.SendCustomLogEntry(
              Format('%s = %s', [event.Msg, event.Data.ToString]),
              ltVariableValue, viTitle);
        tkFloat: SiMain.LogValue(event.Msg, event.Data.AsExtended);
        tkSet: SiMain.SendCustomLogEntry(
          Format('%s = %s', [event.Msg, event.Data.ToString]),
          ltVariableValue, viTitle);
        tkClass: SiMain.LogObject(event.Msg, event.Data.AsObject);
        tkInt64: SiMain.LogValue(event.Msg, event.Data.AsInt64);
        tkPointer: SiMain.LogPointer(event.Msg, event.Data.AsType<Pointer>);
      else
        SiMain.LogValue(event.Msg, event.Data.ToString);
      end;

    TLogEventType.Entering:
      SiMain.EnterMethod(
        TLogAppenderBase.FormatMethodName(event.ClassType, event.Msg));

    TLogEventType.Leaving:
      SiMain.LeaveMethod(
        TLogAppenderBase.FormatMethodName(event.ClassType, event.Msg));

    TLogEventType.CallStack,
    TLogEventType.SerializedData:
      SiMain.LogMessage(event.Msg);
  end;
end;

{$ENDREGION}


end.
