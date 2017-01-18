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

unit DDuce.Logger.Interfaces;

//{$I DDuce.inc}

interface

uses
  System.Classes, System.Rtti, System.SysUtils, System.Types, System.UITypes,
  System.UIConsts,
  Vcl.Menus,

  Spring.Collections;

type
  TLogMessageType = (
    lmtInfo        = 0,
    lmtError       = 1,
    lmtWarning     = 2,

    lmtValue       = 3,

    lmtEnterMethod = 4,
    lmtLeaveMethod = 5,

    lmtConditional = 6,

    lmtCheckpoint  = 7,

    lmtStrings     = 8,   // TStringList

    lmtCallStack   = 9,

    lmtObject      = 10,  // should be lmtComponent (dfm stream is sent)

    lmtException   = 11,

    lmtBitmap      = 12,

    lmtHeapInfo    = 13,

    lmtMemory      = 14,
    lmtCustomData  = 15,

    lmtWatch       = 20,
    lmtCounter     = 21,

    lmtColor       = 22,
    lmtAlphaColor  = 23,

    lmtScreenShot  = 24,

    lmtDataSet     = 25,

    lmtClear       = 100
  );

  ILogger = interface;

  TLogMessage = packed record
    MsgType : Integer;
    MsgTime : TDateTime;
    MsgText : AnsiString;
    Data    : TStream;
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
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);

    function Write(const AMsg: TLogMessage): Boolean;
    function Connect: Boolean;
    function Disconnect: Boolean;

    property Active: Boolean
      read GetActive write SetActive;

    { True when the channel is connected with the server (or receiving)
      instance. }
    property Connected: Boolean
      read GetConnected write SetConnected;
  end;

  TChannelList = IList<ILogChannel>;

  ILogger = interface(IInterface)
  ['{28E9BADE-6B42-4399-8867-1CA115576E40}']
    function GetChannels: TChannelList;

    procedure Send(const AName: string; const AArgs: array of const); overload;
    { This overload is used for Variant arguments. }
    procedure Send(const AName: string; const AValue: string = ''); overload;

    { All primary types that are are implicitely ba cast to TValue will be
      handled through this call. }
    procedure Send(const AName: string; const AValue: TValue); overload;

    { Send methods for types that do not have an implicit cast to TValue
      These are equivalent to Send(AName, TValue.From(AValue)); }
    procedure SendDateTime(const AName: string; AValue: TDateTime);
    procedure SendDate(const AName: string; AValue: TDate);
    procedure SendTime(const AName: string; AValue: TTime);

    { Send methods for types that need a custom representation. }
    procedure SendColor(const AName: string; AColor: TColor);
    procedure SendAlphaColor(const AName: string; AAlphaColor: TAlphaColor);
    procedure SendObject(const AName: string; AValue: TObject);
    procedure SendRect(const AName: string; const AValue: TRect);
    procedure SendPoint(const AName: string; const APoint: TPoint);
    procedure SendStrings(const AName: string; AValue: TStrings);
    procedure SendComponent(const AName: string; AValue: TComponent);
    procedure SendPointer(const AName: string; APointer: Pointer);
    procedure SendException(const AName: string; AException: Exception);
    procedure SendMemory(
      const AName: string;
      AAddress   : Pointer;
      ASize      : LongWord
    );
    procedure SendShortCut(const AName: string; AShortCut: TShortCut);

    procedure IncCounter(const AName: string);
    procedure DecCounter(const AName: string);
    procedure ResetCounter(const AName: string);
    function GetCounter(const AName: string): Integer;

    procedure Enter(const AName: string); overload;
    procedure Enter(ASender: TObject; const AName: string); overload;
    procedure Leave(const AName: string); overload;
    procedure Leave(ASender: TObject; const AName: string); overload;

    { Track uses an interface variable to replace Enter/Leave calls in the
      scope of the method where it is called. A call to Track will create an
      instance and trigger the Enter method. When the interface variable goes
      out of scope (end of the routine or method) a call to the logger's Leave
      method is triggered. }
    function Track(const AName: string): IInterface; overload;
    function Track(ASender: TObject; const AName: string): IInterface; overload;

    procedure AddCheckPoint(const AName: string = '');
    procedure ResetCheckPoint(const AName: string = '');

    { Monitors a named value in the LogViewer application }
    procedure Watch(const AName: string; const AValue: TValue); overload;
    procedure Watch(const AName: string; const AValue: string = ''); overload;

    procedure Warn(const AText: string); overload;
    procedure Warn(
      const AText : string;
      const AArgs : array of const
    ); overload;
    procedure Error(const AText: string); overload;
    procedure Error(
      const AText : string;
      const AArgs : array of const
    ); overload;
    procedure Info(const AText: string); overload;
    procedure Info(
      const AText: string;
      const AArgs: array of const
    ); overload;

    procedure SendIf(
      const AText : string;
      AExpression : Boolean;
      AIsTrue     : Boolean = True
    );
    { Sends out a dedicated message to clear the logviewer contents. }
    procedure Clear;

    property Channels: TChannelList
      read GetChannels;
  end;

const
  LOG_PREFIXES: array [lmtInfo..lmtCounter] of string = (
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
    'COUNTER'
  );

implementation

end.
