{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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
    procedure DoSend(const entry: TLogEntry); override;

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
procedure TWinIPCAppender.DoSend(const entry: TLogEntry);
const
  ZeroBuf: Integer = 0;
var
  CDS     : TCopyDataStruct;
  TextSize : Integer;
//  DataSize : Integer;
//  P        : Pointer;
  S        : AnsiString;
  LMT      : Integer;
begin
(*
  TLogMessageType = (
    lmtInfo        = 0,
    lmtError       = 1,
    lmtWarning     = 2,
    lmtValue       = 3,
    lmtEnterMethod = 4,
    lmtExitMethod  = 5,
    lmtConditional = 6,
    lmtCheckpoint  = 7,
    lmtStrings     = 8,
    lmtCallStack   = 9,
    lmtObject      = 10,
    lmtException   = 11,
    lmtBitmap      = 12,
    lmtHeapInfo    = 13,
    lmtMemory      = 14,
    lmtCustomData  = 15,
    lmtWatch       = 20,
    lmtCounter     = 21,
    lmtClear       = 100
  );
*)
  case entry.EntryType of
    TLogEntryType.Text:
    begin
      case entry.Level of
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
    TLogEntryType.Value          : LMT := 3;
    TLogEntryType.CallStack      : LMT := 9;
    TLogEntryType.SerializedData : LMT := 0;
    TLogEntryType.Entering       : LMT := 4;
    TLogEntryType.Leaving        : LMT := 5;
  end;

(*
  LOG_ALL_LEVELS = [Low(TLogLevel)..High(TLogLevel)] - [TLogLevel.Unknown];
  LOG_BASIC_LEVELS = [
    TLogLevel.Info,
    TLogLevel.Warn,
    TLogLevel.Error,
    TLogLevel.Fatal
  ];

type
  TLogEntryType = (
    /// <summary>
    ///   Is the most basic logging type all loggers should keep enabled
    /// </summary>
    Text,
    Value,
    /// <summary>
    ///   Should only be called if stack is sent to the appender. The appender
    ///   may treat it in a specific way. No one else should use this entry
    ///   type. If this entry type is not set, callstack logging will be
    ///   disabled completely, this may have significant performance impact on
    ///   some platforms.
    /// </summary>
    CallStack,
    /// <summary>
    ///   Should only be called if serialized data (object, record, etc.) is
    ///   sent to the appender. The appender may treat it in a specific way. No
    ///   one else should use this entry type. If this level is not set, data
    ///   serialization logging will be disabled completely.
    /// </summary>
    SerializedData,
    Entering,
    Leaving
  );
  TLogEntryTypes = set of TLogEntryType;
  *)

  S := AnsiString(entry.Msg);
  TextSize := Length(S);

  FMemoryStream.Seek(0, soFromBeginning);
  //FMemoryStream.WriteBuffer(entry.EntryType, SizeOf(Integer));
  FMemoryStream.WriteBuffer(LMT, SizeOf(Integer));
  FMemoryStream.WriteBuffer(entry.TimeStamp, SizeOf(TDateTime));
  FMemoryStream.WriteBuffer(TextSize, SizeOf(Integer));
  FMemoryStream.WriteBuffer(S[1], TextSize);
  if not entry.Data.IsEmpty then
  begin
    //S := AnsiString(entry.Data.ToString);
    //DataSize := Length(S);

    //entry.Data.DataSize;
    //S := entry.Data.ToString;

    //FMemoryStream.WriteBuffer(DataSize, SizeOf(Integer));
    //FMemoryStream.WriteBuffer(entry.Data.GetReferenceToRawData, DataSize)
    //FMemoryStream.WriteBuffer(S[1], DataSize);
    // WriteLn('[IPCChannel] Size Of Stream: ',DataSize);

    FMemoryStream.WriteBuffer(ZeroBuf, SizeOf(Integer)); // necessary?

    //AMsg.Data.Position := 0;
    //FMemoryStream.CopyFrom( AMsg.Data, DataSize);
    // WriteLn('DataCopied: ',CopyFrom(AMsg.Data,DataSize));

  end
  else
    FMemoryStream.WriteBuffer(ZeroBuf, SizeOf(Integer)); // necessary?

  if FHWND = 0 then
    FHWND := FindWindow(MSG_WND_CLASSNAME, PChar(MSG_WND_WINDOWNAME));

//  if HWND = 0 then
//    raise Exception.Create(Format(SErrServerNotActive, [FServerID]));

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
