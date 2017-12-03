{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

{ WinIPC appender for the Spring4D logging system. }

unit DDuce.Logging.Appenders.WinIPC;

interface

uses
  Winapi.Windows,
  System.Classes,

  Spring.Logging.Appenders.Base, Spring.Logging.Appenders, Spring.Logging;

type
  TWinIPCAppender = class(TLogAppenderBase, ILogAppender)
  private
    FMemoryStream : TMemoryStream;
    FHwnd         : HWND;

  protected
    procedure DoSend(const ALogEvent: TLogEvent); override;

  public
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;

  end;

implementation

uses
  Winapi.Messages,
  System.Rtti;

const
  MSG_WND_CLASSNAME  = 'FPCMsgWindowCls';
  MSG_WND_WINDOWNAME = 'ipc_log_server';

{$REGION 'construction and destruction'}
procedure TWinIPCAppender.AfterConstruction;
begin
  inherited AfterConstruction;
  FMemoryStream := TMemoryStream.Create;
end;

procedure TWinIPCAppender.BeforeDestruction;
begin
  FMemoryStream.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TWinIPCAppender.DoSend(const ALogEvent: TLogEvent);
const
  ZeroBuf: Integer = 0;
var
  CDS      : TCopyDataStruct;
  TextSize : Integer;
  S        : AnsiString;
  LMT      : Integer;
begin
  case ALogEvent.EventType of
    TLogEventType.Text:
    begin
      case ALogEvent.Level of
        TLogLevel.Unknown : LMT := 0;
        TLogLevel.Trace   : LMT := 0;
        TLogLevel.Debug   : LMT := 0;
        TLogLevel.Text    : LMT := 0;
        TLogLevel.Info    : LMT := 0;
        TLogLevel.Warn    : LMT := 2;
        TLogLevel.Error   : LMT := 1;
        TLogLevel.Fatal   : LMT := 1;
      end;
    end;
    TLogEventType.Value          : LMT := 3;
    TLogEventType.CallStack      : LMT := 9;
    TLogEventType.SerializedData : LMT := 0;
    TLogEventType.Entering       : LMT := 4;
    TLogEventType.Leaving        : LMT := 5;
  end;

  S := AnsiString(ALogEvent.Msg);
  TextSize := Length(S);

  FMemoryStream.Seek(0, soFromBeginning);
  FMemoryStream.WriteBuffer(LMT, SizeOf(Integer));
  FMemoryStream.WriteBuffer(ALogEvent.TimeStamp, SizeOf(TDateTime));
  FMemoryStream.WriteBuffer(TextSize, SizeOf(Integer));
  FMemoryStream.WriteBuffer(S[1], TextSize);
  if not ALogEvent.Data.IsEmpty then
  begin
    FMemoryStream.WriteBuffer(ZeroBuf, SizeOf(Integer)); // necessary?
  end
  else
    FMemoryStream.WriteBuffer(ZeroBuf, SizeOf(Integer)); // necessary?

  if FHWND = 0 then
    FHWND := FindWindow(MSG_WND_CLASSNAME, PChar(MSG_WND_WINDOWNAME));

  if FHWND <> 0 then
  begin
    FMemoryStream.Seek(0, soFromBeginning);
    CDS.lpData := FMemoryStream.Memory;
    CDS.cbData := FMemoryStream.Size;
    Winapi.Windows.SendMessage(FHWnd, WM_COPYDATA, 0, Integer(@CDS));
  end;
  FMemoryStream.Clear;
end;
{$ENDREGION}

end.
