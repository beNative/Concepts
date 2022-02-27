{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I .\..\DDuce.inc}

unit DDuce.Logger.Interfaces;

interface

uses
  System.Classes, System.Rtti, System.SysUtils, System.Types, System.UITypes,
  Vcl.Menus, Vcl.Graphics, Vcl.Forms,
  Data.DB,

  Spring, Spring.Collections;

type
  { Remark: Enumerated types with explicitly assigned ordinality don't have RTTI
    generated for them. Enumerated constants without a specific value however
    do have RTTI.}
  TLogMessageType = (
    lmtInfo        = 0,
    lmtError       = 1,
    lmtWarning     = 2,
    lmtValue       = 3,
    lmtEnterMethod = 4,
    lmtLeaveMethod = 5,
    lmtConditional = 6,
    lmtCheckpoint  = 7,
    lmtStrings     = 8,   // TStrings and descendants
    lmtCallStack   = 9,   // not supported yet
    lmtComponent   = 10,
    lmtException   = 11,
    lmtBitmap      = 12,
    lmtHeapInfo    = 13,  // not supported yet
    lmtMemory      = 14,
    lmtCustomData  = 15,  // not supported yet
    lmtObject      = 16,
    lmtInterface   = 17,
    lmtPersistent  = 18,
    lmtReserved    = 19,
    lmtWatch       = 20,
    lmtCounter     = 21,
    lmtColor       = 22,
    lmtAlphaColor  = 23,
    lmtScreenShot  = 24,
    lmtText        = 25,  // arbitrary text with optional highlighter info
    lmtDataSet     = 26,
    lmtAction      = 27,  // TAction execution
    lmtClear       = 99,
    lmtNone        = 100  // can be used as a default value
  );
  TLogMessageTypes  = set of TLogMessageType;

  TLogMessageLevel  = 0..31;
  TLogMessageLevels = set of TLogMessageLevel;

const
  TracingMessages      : TLogMessageTypes =
    [lmtEnterMethod, lmtLeaveMethod];
  NotificationMessages : TLogMessageTypes =
    [lmtInfo, lmtError, lmtWarning, lmtConditional, lmtCheckpoint];
  DataValueMessages    : TLogMessageTypes = [
    lmtValue, lmtStrings, lmtComponent, lmtException, lmtBitmap, lmtObject,
    lmtInterface, lmtPersistent, lmtColor, lmtAlphaColor, lmtScreenShot,
    lmtText, lmtDataSet, lmtAction, lmtMemory
  ];
  StateMessages        : TLogMessageTypes =
    [lmtCounter, lmtWatch];
  CommandMessages      : TLogMessageTypes =
     [lmtClear];
  DiagnosticMessages   : TLogMessageTypes =
    [lmtCallStack, lmtHeapInfo];
  AllMessages          : TLogMessageTypes = [
    lmtEnterMethod, lmtLeaveMethod,
    lmtInfo, lmtError, lmtWarning, lmtConditional,
    lmtValue, lmtStrings, lmtComponent, lmtException, lmtBitmap, lmtObject,
    lmtInterface, lmtPersistent, lmtColor, lmtAlphaColor, lmtScreenShot,
    lmtCustomData,
    lmtText, lmtDataSet, lmtAction, lmtMemory,
    lmtCounter, lmtCheckpoint,
    lmtWatch,
    lmtReserved,
    lmtClear,
    lmtCallStack, lmtHeapInfo,
    lmtNone
  ];
  AllLevels : TLogMessageLevels =
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
     21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31];

type
  ILogger = interface;

  TLogMessage = packed record
    MsgType   : Byte; // TLogMessageType
    LogLevel  : Byte; // TLogMessageLevel
    Reserved1 : Byte;
    Reserved2 : Byte;
    TimeStamp : TDateTime;
    Text      : UTF8String;
    Data      : TStream;
  end;

  TCustomDataCallbackMethod = function(
    ASender     : ILogger;
    AData       : TValue;
    var ADoSend : Boolean
  ): string of object;

  TCustomDataCallbackFunction = function(
    ASender     : ILogger;
    AData       : TValue;
    var ADoSend : Boolean
  ): string;

  ILogChannel = interface
  ['{FDE37401-BB4F-4362-863A-CCCCF9228BD9}']
    {$REGION 'property access methods'}
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetConnected: Boolean;
    function GetAutoConnect: Boolean;
    procedure SetAutoConnect(const Value: Boolean);
    {$ENDREGION}

    function Write(const AMsg: TLogMessage): Boolean;
    function Connect: Boolean;
    function Disconnect: Boolean;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    { True when the channel is connected with the server (or receiving)
      instance. }
    property Connected: Boolean
      read GetConnected;
  end;

  ILogFileChannel = interface(ILogChannel)
  ['{AD38F81B-DC15-4295-B74D-B646CDF9831D}']
    {$REGION 'property access methods'}
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    procedure SetShowTime(const AValue: Boolean);
    procedure SetShowDate(const Value: Boolean);
    function GetShowDate: Boolean;
    function GetShowHeader: Boolean;
    function GetShowPrefix: Boolean;
    function GetShowTime: Boolean;
    procedure SetShowHeader(const Value: Boolean);
    procedure SetShowPrefix(const Value: Boolean);
    {$ENDREGION}

    property FileName: string
      read GetFileName write SetFileName;

    property ShowHeader: Boolean
      read GetShowHeader write SetShowHeader;

    property ShowPrefix: Boolean
      read GetShowPrefix write SetShowPrefix;

    property ShowTime: Boolean
      read GetShowTime write SetShowTime;

    property ShowDate: Boolean
      read GetShowDate write SetShowDate;
  end;

  IZmqChannel = interface(ILogChannel)
  ['{7DC63C0E-1038-4416-84C3-9D8E76C9B929}']
    {$REGION 'property access methods'}
    function GetPort: Integer;
    function GetEndPoint: string;
    procedure SetEndPoint(const Value: string);
    function GetZmqVersion: string;
    {$ENDREGION}

    { Endpoint string consisting of transport, host/IP, port  }
    property EndPoint : string
      read GetEndPoint write SetEndPoint;

    property ZmqVersion: string
      read GetZmqVersion;

    property Port: Integer
      read GetPort;
  end;

  IMqttChannel = interface(ILogChannel)
  ['{5D482099-AB68-462B-8EE8-CCE4DBB60C44}']
    {$REGION 'property access methods'}
    function GetPort: Integer;
    function GetBroker: string;
    procedure SetBroker(const Value: string);
    {$ENDREGION}
    { Hostname/IP of the MQTT broker }
    property Broker: string
      read GetBroker write SetBroker;

    property Port: Integer
      read GetPort;
  end;

  IWinipcChannel = interface(ILogChannel)
  ['{E23B78DA-F62D-4D2B-A0FB-33C3CE106FFA}']
  end;

  TChannelList = IList<ILogChannel>;

  // TODO send TTimeSpan
  //      send stream?
  //      send file

  ILogger = interface
  ['{28E9BADE-6B42-4399-8867-1CA115576E40}']
    {$REGION 'property access methods'}
    function GetChannels: TChannelList;
    function GetLogLevel: Byte;
    procedure SetLogLevel(const Value: Byte);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    {$ENDREGION}

    function Send(const AName: string; const AArgs: array of const): ILogger; overload;

    { These three overloads are here because TValue would cast them implicitely
      to string (and we would lose type information of AValue) }
    function Send(const AName: string; const AValue: AnsiString): ILogger; overload;
    function Send(const AName: string; const AValue: WideString): ILogger; overload;
    function Send(const AName: string; const AValue: ShortString): ILogger; overload;
    function Send(const AName: string; const AValue: string): ILogger; overload;

    { UInt8 = Byte }
    function Send(const AName: string; const AValue: Byte): ILogger; overload;
    { UInt16 = Word }
    function Send(const AName: string; const AValue: Word): ILogger; overload;
    { UInt32 = Cardinal = FixedUInt }
    function Send(const AName: string; const AValue: Cardinal): ILogger; overload;
    { UInt64 }
    function Send(const AName: string; const AValue: UInt64): ILogger; overload;
    { Int8 = ShortInt }
    function Send(const AName: string; const AValue: ShortInt): ILogger; overload;
    { Int16 = SmallInt }
    function Send(const AName: string; const AValue: SmallInt): ILogger; overload;
    { Int32 = Integer = FixedInt }
    function Send(const AName: string; const AValue: FixedInt): ILogger; overload;

    { All types that can implicitely be casted to TValue will be handled
      through this call. }

    { These are (tested):
       Integer
       Single
       Double
       Extended
       Currency
       Int64
       UInt64
       Boolean

       TObject ?
       TClass ?
    }
    function Send(const AName: string; const AValue: TValue): ILogger; overload;
    function Send(const AValue: AnsiString): ILogger; overload;
    function Send(const AValue: WideString): ILogger; overload;
    function Send(const AValue: ShortString): ILogger; overload;
    function Send(const AValue: string): ILogger; overload;

    function Send(const AValue: Byte): ILogger; overload;
    function Send(const AValue: Word): ILogger; overload;
    function Send(const AValue: Cardinal): ILogger; overload;
    function Send(const AValue: UInt64): ILogger; overload;
    function Send(const AValue: ShortInt): ILogger; overload;
    function Send(const AValue: SmallInt): ILogger; overload;
    function Send(const AValue: FixedInt): ILogger; overload;

    function Send(const AValue: TValue): ILogger; overload;

    { Send methods for types that do not have an implicit cast to TValue
      These are equivalent to Send(AName, TValue.From(AValue)); }
    function SendDateTime(const AName: string; AValue: TDateTime): ILogger; overload;
    function SendDateTime(AValue: TDateTime): ILogger; overload;
    function SendDate(const AName: string; AValue: TDate): ILogger; overload;
    function SendDate(AValue: TDate): ILogger; overload;
    function SendTime(const AName: string; AValue: TTime): ILogger; overload;
    function SendTime(AValue: TTime): ILogger; overload;

    { Send methods for types that need a custom representation. }
    function SendColor(const AName: string; AColor: TColor): ILogger; overload;
    function SendColor(AColor: TColor): ILogger; overload;
    function SendAlphaColor(const AName: string; AAlphaColor: TAlphaColor): ILogger; overload;
    function SendAlphaColor(AAlphaColor: TAlphaColor): ILogger; overload;
    function SendObject(const AName: string; AValue: TObject): ILogger; overload;
    function SendObject(AValue: TObject): ILogger; overload;
    { Logs interface properties. }
    function SendInterface(const AName: string; AValue: IInterface): ILogger; overload;
    function SendInterface(AValue: IInterface): ILogger; overload;
    { Logs published properties. }
    function SendPersistent(const AName: string; AValue: TPersistent): ILogger; overload;
    function SendPersistent(AValue: TPersistent): ILogger; overload;
    function SendRect(const AName: string; const AValue: TRect): ILogger; overload;
    function SendRect(const AValue: TRect): ILogger; overload;
    function SendPoint(const AName: string; const AValue: TPoint): ILogger; overload;
    function SendPoint(const AValue: TPoint): ILogger; overload;
    function SendStrings(const AName: string; AValue: TStrings): ILogger; overload;
    function SendStrings(AValue: TStrings): ILogger; overload;
    function SendStrings(const AValue: string): ILogger; overload;
    function SendStrings(const AName: string; AValue: string): ILogger; overload;
    function SendComponent(const AName: string; AValue: TComponent): ILogger; overload;
    function SendComponent(AValue: TComponent): ILogger; overload;
    function SendPointer(const AName: string; AValue: Pointer): ILogger; overload;
    function SendPointer(AValue: Pointer): ILogger; overload;
    function SendException(const AName: string; AValue: Exception): ILogger; overload;
    function SendException(AValue: Exception): ILogger; overload;
    function SendBitmap(const AName: string; AValue: TBitmap; ASendCompressed: Boolean = True): ILogger; overload;
    function SendBitmap(AValue: TBitmap; ASendCompressed: Boolean = True): ILogger; overload;
    function SendScreenShot(const AName: string; AForm: TCustomForm): ILogger; overload;
    function SendScreenShot(AForm: TCustomForm): ILogger; overload;
    function SendDataSet(const AName: string; AValue: TDataSet): ILogger; overload;
    function SendDataSet(AValue: TDataSet): ILogger; overload;
    function SendShortCut(const AName: string; AValue: TShortCut): ILogger; overload;
    function SendShortCut(AValue: TShortCut): ILogger; overload;
    function SendVariant(const AName: string; const AValue: Variant): ILogger; overload;
    function SendVariant(const AValue: Variant): ILogger; overload;

    function SendMemory(
      const AName: string;
      AAddress   : Pointer;
      ASize      : LongWord
    ): ILogger;

    { Send methods for (optionally named) text that optionally can be displayed
      with a dedicated highlighter. }
    function SendText(
      const AName        : string;
      const AText        : string;
      const AHighlighter : string = ''
    ): ILogger; overload;
    function SendText(const AText: string): ILogger; overload;

    function SendSQL(const AName: string; const AValue: string): ILogger; overload;
    function SendSQL(const AValue: string): ILogger; overload;
    function SendXML(const AName: string; const AValue: string): ILogger; overload;
    function SendXML(const AValue: string): ILogger; overload;
    function SendHTML(const AName: string; const AValue: string): ILogger; overload;
    function SendHTML(const AValue: string): ILogger; overload;
    function SendINI(const AName: string; const AValue: string): ILogger; overload;
    function SendINI(const AValue: string): ILogger; overload;
    function SendJSON(const AName: string; const AValue: string): ILogger; overload;
    function SendJSON(const AValue: string): ILogger; overload;

    function IncCounter(const AName: string): ILogger;
    function DecCounter(const AName: string): ILogger;
    function ResetCounter(const AName: string): ILogger;
    function GetCounter(const AName: string): Integer;

    function Enter(const AName: string): ILogger; overload;
    function Enter(const AName: string; const AArgs: array of const): ILogger; overload;
    function Enter(ASender: TObject; const AName: string): ILogger; overload;
    function Enter(ASender: TObject; const AName: string; const AArgs: array of const): ILogger; overload;
    function Leave(const AName: string): ILogger; overload;
    function Leave(const AName: string; const AArgs: array of const): ILogger; overload;
    function Leave(ASender: TObject; const AName: string): ILogger; overload;
    function Leave(ASender: TObject; const AName: string; const AArgs: array of const): ILogger; overload;
    { Track uses an interface variable to replace Enter/Leave calls in the
      scope of the method where it is called. A call to Track will create an
      instance and trigger the Enter method. When the interface variable goes
      out of scope (end of the routine or method) a call to the logger's Leave
      method is triggered. }
    function Track(const AName: string): IInterface; overload;
    function Track(const AName: string; const AArgs: array of const): IInterface; overload;
    function Track(ASender: TObject; const AName: string): IInterface; overload;
    function Track(ASender: TObject; const AName: string; const AArgs: array of const): IInterface; overload;

    function Action(AAction: TBasicAction): ILogger;

    function AddCheckPoint(const AName: string = ''): ILogger;
    function ResetCheckPoint(const AName: string = ''): ILogger;

    { Monitors a named value in the LogViewer application }
    function Watch(const AName: string; const AValue: TValue): ILogger; overload;
    function Watch(const AName: string; const AValue: string): ILogger; overload;
    function Watch(const AName: string; const AValue: ShortString): ILogger; overload;
    function Watch(const AName: string; const AValue: AnsiString): ILogger; overload;
    function Watch(const AName: string; const AValue: WideString): ILogger; overload;

    function Warn(const AText: string): ILogger; overload;
    function Warn(
      const AText : string;
      const AArgs : array of const
    ): ILogger; overload;
    function Error(const AText: string): ILogger; overload;
    function Error(
      const AText : string;
      const AArgs : array of const
    ): ILogger; overload;
    function Info(const AText: string): ILogger; overload;
    function Info(
      const AText: string;
      const AArgs: array of const
    ): ILogger; overload;

    function SendIf(
      const AText : string;
      AExpression : Boolean;
      AIsTrue     : Boolean = True
    ): ILogger;
    { Sends out a dedicated message to clear the logviewer contents. }
    function Clear: ILogger;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property LogLevel: Byte
      read GetLogLevel write SetLogLevel;

    property Channels: TChannelList
      read GetChannels;
  end;

const
  LOG_PREFIXES: array [lmtInfo..lmtAction] of string = (
    'INFO',
    'ERROR',
    'WARNING',
    'VALUE',
    '>>ENTER METHOD',
    '<<LEAVE METHOD',
    'CONDITIONAL',
    'CHECKPOINT',
    'STRINGS',
    'CALL STACK',
    'OBJECT',
    'EXCEPTION',
    'BITMAP',
    'HEAP INFO',
    'MEMORY',
    '',
    '',
    '',
    '',
    '',
    'WATCH',
    'COUNTER',
    'COLOR',
    'ALPHACOLOR',
    'SCREENSHOT',
    'TEXT',
    'DATASET',
    'ACTION'
  );

function LogMessageTypeNameOf(ALogMessageType: TLogMessageType): string;

implementation

{$REGION 'interfaced routines'}
function LogMessageTypeNameOf(ALogMessageType: TLogMessageType): string;
var
  S : string;
begin
  case ALogMessageType of
    lmtInfo        : S := 'lmtInfo';
    lmtError       : S := 'lmtError' ;
    lmtWarning     : S := 'lmtWaring';
    lmtValue       : S := 'lmtValue';
    lmtEnterMethod : S := 'lmtEnterMethod';
    lmtLeaveMethod : S := 'lmtLeaveMethod';
    lmtConditional : S := 'lmtConditional';
    lmtCheckpoint  : S := 'lmtCheckpoint';
    lmtStrings     : S := 'lmtStrings';
    lmtCallStack   : S := 'lmtCallStack';
    lmtComponent   : S := 'lmtComponent';
    lmtException   : S := 'lmtException';
    lmtBitmap      : S := 'lmtBitmap';
    lmtHeapInfo    : S := 'lmtHeapInfo';
    lmtMemory      : S := 'lmtMemory';
    lmtCustomData  : S := 'lmtCustomData';
    lmtObject      : S := 'lmtObject';
    lmtInterface   : S := 'lmtInterface';
    lmtPersistent  : S := 'lmtPersistent';
    lmtReserved    : S := 'lmtReserved';
    lmtWatch       : S := 'lmtWatch';
    lmtCounter     : S := 'lmtCounter';
    lmtColor       : S := 'lmtColor';
    lmtAlphaColor  : S := 'lmtAlphaColor';
    lmtScreenShot  : S := 'lmtScreenShot';
    lmtText        : S := 'lmtText';
    lmtDataSet     : S := 'lmtDataSet';
    lmtAction      : S := 'lmtAction';
    lmtClear       : S := 'lmtClear';
    lmtNone        : S := 'lmtNone';
  else
    S := '';
  end;
  Result := S;
end;
{$ENDREGION}

end.
