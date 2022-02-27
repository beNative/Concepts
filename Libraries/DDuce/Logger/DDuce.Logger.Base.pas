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

{
  Although this unit has been almost completely rewritten, the original idea
  was created by Luiz Américo Pereira Câmara in his open source multilog
  project.

  The Initial Developer of the Original Code (multilog.pas) is Luiz Américo
  Pereira Câmara (pascalive@bol.com.br). Portions created by the Initial
  Developer are Copyright (C) 2006. All Rights Reserved. You may obtain a copy
  of the original code at http://code.google.com/p/luipack/
}

{$I .\..\DDuce.inc}

unit DDuce.Logger.Base;

{
  StopWatch support
    StartStopWatch
    StopStopWatch

  SendScreenshot
}

interface

uses
  Winapi.Windows,
  System.Types, System.Classes, System.SysUtils, System.Rtti, System.UITypes,
  Vcl.Graphics, Vcl.Forms,
  Data.DB,

  Spring, Spring.Collections,

  DDuce.Logger.Interfaces;

const
  DEFAULT_MAXSTACKCOUNT = 20;

type
  TLogger = class(TInterfacedObject, ILogger)
  type
    TTrack = class(TInterfacedObject)
    private
      FLogger : ILogger;
      FName   : string;
      FSender : TObject;
    public
      constructor Create(
        const ALogger: ILogger;
        ASender      : TObject;
        const AName  : string
        );
      destructor Destroy; override;
    end;

  var
    FMaxStackCount : Integer;
    FChannels      : IList<ILogChannel>;
    FLogStack      : TStrings;
    FCheckList     : TStringList;
    FCounterList   : IDictionary<string, Int64>;
    FOnCustomData  : TCustomDataCallbackMethod;
    FLogLevel      : Byte;
    FEnabled       : Boolean;

    {$REGION 'property access methods'}
    procedure SetMaxStackCount(const AValue: Integer);
    function GetChannels: TChannelList;
    function GetLogLevel: Byte;
    procedure SetLogLevel(const Value: Byte);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    {$ENDREGION}

  protected
    function StringValueOf(const AValue: TValue): string;

    function InternalSend(
      AMsgType    : TLogMessageType;
      const AText : string = ''
    ): ILogger;
    function InternalSendStream(
      AMsgType    : TLogMessageType;
      const AText : string;
      AStream     : TStream
    ): ILogger;
    function InternalSendBuffer(
      AMsgType    : TLogMessageType;
      const AText : string;
      var ABuffer;
      ACount      : LongWord
    ): ILogger;

    function CalledBy(const AName: string): Boolean;

    { Sends a dedicated message to clear content in the receiver (LogViewer). }
    function Clear: ILogger;

    // Send procedures
    function Send(const AName: string; const AArgs: array of const): ILogger; overload;

    function Send(const AName: string; const AValue: string): ILogger; overload;
    function Send(const AName: string; const AValue: AnsiString): ILogger; overload;
    function Send(const AName: string; const AValue: WideString): ILogger; overload;
    function Send(const AName: string; const AValue: ShortString): ILogger; overload;

    // Overloads for builtin integer types
    function Send(const AName: string; const AValue: Cardinal): ILogger; overload;
    function Send(const AName: string; const AValue: Word): ILogger; overload;
    function Send(const AName: string; const AValue: SmallInt): ILogger; overload;
    function Send(const AName: string; const AValue: Byte): ILogger; overload;
    function Send(const AName: string; const AValue: ShortInt): ILogger; overload;
    function Send(const AName: string; const AValue: UInt64): ILogger; overload;
    function Send(const AName: string; const AValue: FixedInt): ILogger; overload;

    // no need to define overloads which have an implicit cast to TValue
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

    function SendDateTime(const AName: string; AValue: TDateTime): ILogger; overload;
    function SendDateTime(AValue: TDateTime): ILogger; overload;
    function SendDate(const AName: string; AValue: TDate): ILogger; overload;
    function SendDate(AValue: TDate): ILogger; overload;
    function SendTime(const AName: string; AValue: TTime): ILogger; overload;
    function SendTime(AValue: TTime): ILogger; overload;

    { Send methods for types that need a custom representation. }
    function SendColor(const AName: string; AColor: TColor): ILogger; overload;
    function SendColor(AValue: TColor): ILogger; overload;
    function SendAlphaColor(const AName: string; AValue: TAlphaColor): ILogger; overload;
    function SendAlphaColor(AValue: TAlphaColor): ILogger; overload;
    { Will send object data using RTTI information. }
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
    { Will send the component as a dfm-stream. }
    function SendComponent(const AName: string; AValue: TComponent): ILogger; overload;
    function SendComponent(AValue: TComponent): ILogger; overload;
    function SendPointer(const AName: string; AValue: Pointer): ILogger; overload;
    function SendPointer(AValue: Pointer): ILogger; overload;
    function SendException(const AName: string; AValue: Exception): ILogger; overload;
    function SendException(AValue: Exception): ILogger; overload;
    function SendBitmap(const AName: string; AValue: TBitmap; ASendCompressed: Boolean = True): ILogger; overload;
    function SendBitmap(AValue: TBitmap; ASendCompressed: Boolean = True): ILogger; overload;
    function SendGraphic(const AName: string; AValue: TGraphic): ILogger; overload;
    function SendGraphic(AValue: TGraphic): ILogger; overload;
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
      ASize      : UInt32
    ): ILogger;

    { Send methods for text that can be displayed with a dedicated
      highlighter. }
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

    function SendIf(
      const AText : string;
      AExpression : Boolean;
      AIsTrue     : Boolean = True
    ): ILogger;

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
      const AText : string;
      const AArgs : array of const
    ): ILogger; overload;

    { Uses the OnCustomData event as callback. }
    function SendCustomData(
      const AName : string;
      const AData : TValue
    ): ILogger; overload;
    function SendCustomData(
      const AName : string;
      const AData : TValue;
      AFunc       : TCustomDataCallbackMethod
    ): ILogger; overload;
    function SendCustomData(
      const AName : string;
      const AData : TValue;
      AFunc       : TCustomDataCallbackFunction
    ): ILogger; overload;

    { Checkpoints are used to count how many times the application enters a
      certain position in the code. }
    function AddCheckPoint(const AName: string = ''): ILogger;
    function ResetCheckPoint(const AName: string = ''): ILogger;

    { Counter support }
    function IncCounter(const AName: string): ILogger;
    function DecCounter(const AName: string): ILogger;
    function ResetCounter(const AName: string): ILogger;
    function GetCounter(const AName: string): Integer;

    { Method tracing }
    function Enter(const AName: string): ILogger; overload;
    function Enter(const AName: string; const AArgs: array of const): ILogger; overload;
    function Enter(ASender: TObject; const AName: string): ILogger; overload;
    function Enter(ASender: TObject; const AName: string; const AArgs: array of const): ILogger; overload;
    function Leave(const AName: string): ILogger; overload;
    function Leave(const AName: string; const AArgs: array of const): ILogger; overload;
    function Leave(ASender: TObject; const AName: string): ILogger; overload;
    function Leave(ASender: TObject; const AName: string; const AArgs: array of const): ILogger; overload;
    function Track(const AName: string): IInterface; overload;
    function Track(const AName: string; const AArgs: array of const): IInterface; overload;
    function Track(ASender: TObject; const AName: string): IInterface; overload;
    function Track(ASender: TObject; const AName: string; const AArgs: array of const): IInterface; overload;

    function Action(AAction: TBasicAction): ILogger;

    { Watches support }
    function Watch(const AName: string; const AValue: TValue): ILogger; overload;
    function Watch(const AName: string; const AValue: string): ILogger; overload;
    function Watch(const AName: string; const AValue: AnsiString): ILogger; overload;
    function Watch(const AName: string; const AValue: WideString): ILogger; overload;
    function Watch(const AName: string; const AValue: ShortString): ILogger; overload;

    { List of channels where logmessages will be posted to }
    property Channels: TChannelList
      read GetChannels;

    property LogLevel: Byte
      read GetLogLevel write SetLogLevel;

    property LogStack: TStrings
      read FLogStack;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    { not used yet }
    property MaxStackCount: Integer
      read FMaxStackCount write SetMaxStackCount default DEFAULT_MAXSTACKCOUNT;

    property OnCustomData: TCustomDataCallbackMethod
      read FOnCustomData write FOnCustomData;

  public
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;
  end;

implementation

uses
  System.TypInfo, System.UIConsts,
  Vcl.Menus, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg,
  FireDAC.Comp.Client, FireDAC.Stan.Intf, FireDAC.Comp.DataSet,
  FireDAC.Stan.StorageBin,

  Spring.Reflection, Spring.Helpers,

  DDuce.Reflect;

const
  STACKCOUNTLIMIT        = 256;
  DEFAULT_CHECKPOINTNAME = 'CheckPoint';

{$REGION 'non-interfaced routines'}
function GetInterfaceTypeName(AIntf: IInterface): Tuple<string,string>;
var
  O        : TObject;
  LType    : TRttiInterfaceType;
  LContext : TRttiContext;
  LIntf    : IInterface;
begin
  // get the implementing object...
  O := AIntf as TObject;

  // enumerate the object's interfaces, looking for the
  // one that matches the input parameter...
  for LType in (LContext.GetType(O.ClassType) as TRttiInstanceType).GetImplementedInterfaces do
  begin
    if O.GetInterface(LType.GUID, LIntf) then
    begin
      if AIntf = LIntf then
      begin
        Result := [LType.Name, LType.GUID.ToString];
        Exit;
      end;
      LIntf := nil;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'TLogger'}
{$REGION 'construction and destruction'}
procedure TLogger.AfterConstruction;
begin
  inherited AfterConstruction;
  FChannels                  := TCollections.CreateInterfaceList<ILogChannel>;
  FMaxStackCount             := DEFAULT_MAXSTACKCOUNT;
  FLogStack                  := TStringList.Create;
  FCheckList                 := TStringList.Create;
  FCheckList.CaseSensitive   := False;
  FCheckList.Sorted          := True;
  FEnabled                   := True;
  FCounterList               := TCollections.CreateDictionary<string, Int64>;
end;

procedure TLogger.BeforeDestruction;
begin
  FLogStack.Free;
  FCheckList.Free;
  FCounterList := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogger.GetChannels: TChannelList;
begin
  Result := FChannels;
end;

function TLogger.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TLogger.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

function TLogger.GetLogLevel: Byte;
begin
  Result := FLogLevel;
end;

procedure TLogger.SetLogLevel(const Value: Byte);
begin
  FLogLevel := Value;
end;

procedure TLogger.SetMaxStackCount(const AValue: Integer);
begin
  if AValue < STACKCOUNTLIMIT then
    FMaxStackCount := AValue
  else
    FMaxStackCount := STACKCOUNTLIMIT;
end;
{$ENDREGION}

{$REGION 'protected methods'}
{$REGION 'Internal methods'}
function TLogger.StringValueOf(const AValue: TValue): string;
var
  V : TValue;
  S : string;
begin
  case AValue.Kind of
    tkArray, tkDynArray:
    begin
      for V in  AValue.GetArray do
        S := S + StringValueOf(V) + #13#10;
    end;
    tkInterface:
      S := sLineBreak + Reflect.Fields(AValue).ToString;
    tkRecord:
      S := sLineBreak + Reflect.Fields(AValue).ToString;
    else
    begin
      S := AValue.ToString;
    end;
  end;
  Result := S;
end;

function TLogger.InternalSendStream(AMsgType: TLogMessageType; const AText: string;
  AStream: TStream): ILogger;
var
  LM : TLogMessage;
  LC : ILogChannel;
begin
  Result := Self;
  if Enabled then
  begin
    LM := Default(TLogMessage);
    LM.MsgType   := Byte(AMsgType);
    LM.LogLevel  := LogLevel;
    LM.Reserved1 := 0;
    LM.Reserved2 := 0;
    LM.TimeStamp := Now;
    LM.Text      := UTF8String(AText);
    LM.Data      := AStream;
    for LC in Channels do
      if LC.Enabled then
        LC.Write(LM);
  end;
end;

function TLogger.InternalSend(AMsgType: TLogMessageType; const AText: string)
  : ILogger;
begin
  Result := InternalSendStream(AMsgType, AText, nil);
end;

function TLogger.InternalSendBuffer(AMsgType: TLogMessageType; const AText: string;
  var ABuffer; ACount: LongWord): ILogger;
var
  LStream : TStream;
begin
  Result  := Self;
  LStream := nil;
  if ACount > 0 then
  begin
    LStream := TMemoryStream.Create;
    try
      LStream.Write(ABuffer, ACount);
      InternalSendStream(AMsgType, AText, LStream);
    finally
      FreeAndNil(LStream);
    end;
  end
  else
    InternalSendStream(AMsgType, AText, nil);
end;
{$ENDREGION}

function TLogger.CalledBy(const AName: string): Boolean;
begin
  Result := FLogStack.IndexOf(UpperCase(AName)) <> -1;
end;

function TLogger.Clear: ILogger;
var
  I : Integer;
begin
  Result := Self;
  // repeated to compensate for initial message loss in combination with
  // some channels (ZeroMQ)
  for I := 0 to 3 do
  begin
    InternalSend(lmtClear);
    Sleep(100);
  end;
end;

{$REGION 'Notification'}
function TLogger.Info(const AText: string): ILogger;
begin
  Result := InternalSend(lmtInfo, AText);
end;

function TLogger.Info(const AText: string; const AArgs: array of const): ILogger;
begin
  Result := InternalSend(lmtInfo, Format(AText, AArgs));
end;

function TLogger.Warn(const AText: string): ILogger;
begin
  Result := InternalSend(lmtWarning, AText);
end;

function TLogger.Warn(const AText: string; const AArgs: array of const): ILogger;
begin
  Result := InternalSend(lmtWarning, Format(AText, AArgs));
end;

function TLogger.Error(const AText: string): ILogger;
begin
  Result := InternalSend(lmtError, AText);
end;

function TLogger.Error(const AText: string; const AArgs: array of const): ILogger;
begin
  Result := InternalSend(lmtError, Format(AText, AArgs));
end;
{$ENDREGION}

{$REGION 'Send overloads'}
function TLogger.Send(const AName: string; const AValue: string): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.Send(const AName: string; const AValue: ShortString): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.Send(const AName: string; const AValue: WideString): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.Send(const AName: string; const AValue: AnsiString): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.Send(const AName: string; const AValue: TValue): ILogger;
var
  S : string;
begin
  if AValue.TypeInfo <> nil then
  begin
    S := Format('%s (%s) = %s', [
      AName,
      AValue.TypeInfo.TypeName,
      StringValueOf(AValue)
    ]);
  end
  else
  begin
    S := Format('%s = %s', [
      AName,
      StringValueOf(AValue)
    ]);
  end;
  Result := InternalSend(lmtValue, S);
end;

function TLogger.Send(const AName: string; const AArgs: array of const): ILogger;
//type
//  TVarArray = array of TVarRec;
//var
//  VA : array of TValue;
//  I  : Integer;
begin
//  SetLength(VA, Length(AArgs));
//  for I := 0 to High(AArgs) do
//  begin
//    VA[I] := TValue.FromVarRec(AArgs[I]);
//  end;
//  Send(AName, TValue.FromArray(TypeInfo(TVarRec), VA));
  Result := Self;
end;

function TLogger.Send(const AName: string; const AValue: FixedInt): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.Send(const AName: string; const AValue: UInt64): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.Send(const AName: string; const AValue: Byte): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.Send(const AName: string; const AValue: Word): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.Send(const AName: string; const AValue: ShortInt): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.Send(const AName: string; const AValue: Cardinal): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.Send(const AName: string; const AValue: SmallInt): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.Send(const AValue: string): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: Byte): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: Word): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: AnsiString): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: WideString): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: ShortString): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: SmallInt): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: FixedInt): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: TValue): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: Cardinal): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: UInt64): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: ShortInt): ILogger;
begin
  Result := Send('', AValue);
end;
{$ENDREGION}

{$REGION 'Specialized Send methods'}
function TLogger.SendAlphaColor(AValue: TAlphaColor): ILogger;
begin
  Result := SendAlphaColor('', AValue);
end;

function TLogger.SendRect(const AName: string; const AValue: TRect): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.SendScreenShot(const AName: string; AForm: TCustomForm): ILogger;
var
  LBitmap : TBitmap;
  LStream : TMemoryStream;
  LPng    : TPngImage;
begin
  LPng := TPngImage.Create;
  LPng.CompressionLevel := 9;
  Result := Self;
  LStream := TMemoryStream.Create;
  try
   LBitmap := AForm.GetFormImage;
   try
     LPng.Assign(LBitmap);
     LPng.SaveToStream(LStream);
     InternalSendStream(lmtScreenShot, AName, LStream); 
   finally
     LBitmap.Free;
     LPng.Free;
   end;
 finally
    LStream.Free;
  end;
end;

function TLogger.SendShortCut(const AName: string; AValue: TShortCut): ILogger;
begin
  Result := Send(AName, ShortCutToText(AValue));
end;

function TLogger.SendStrings(const AName: string; AValue: TStrings): ILogger;
begin
  Guard.CheckNotNull(AValue, AName);
  Result := InternalSend(
    lmtStrings,
    Format('%s (%s) = ' + sLineBreak + '%s', [AName, AValue.ClassName, AValue.Text])
  );
end;

function TLogger.SendObject(const AName: string; AValue: TObject): ILogger;
begin
  Guard.CheckNotNull(AValue, AName);
  Result := InternalSend(
    lmtObject,
    Format('%s (%s) = ' + sLineBreak + '%s',
    [AName, AValue.ClassName, Reflect.Fields(AValue).ToString
    + #13#10 + #13#10 + Reflect.Properties(AValue).ToString]
    )
  );
end;

function TLogger.SendStrings(const AName: string; AValue: string): ILogger;
begin
  Result := InternalSend(
    lmtStrings,
    Format('%s = ' + sLineBreak + '%s', [AName, AValue])
  );
end;

function TLogger.SendStrings(const AValue: string): ILogger;
begin
  Result := SendStrings('', AValue);
end;

function TLogger.SendDateTime(const AName: string; AValue: TDateTime): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.SendException(AValue: Exception): ILogger;
begin
  Result := SendException('', AValue);
end;

function TLogger.SendGraphic(const AName: string; AValue: TGraphic): ILogger;
var
  LStream  : TMemoryStream;
begin
  Result := Self;
  LStream := TMemoryStream.Create;
  try
    AValue.SaveToStream(LStream);
    InternalSendStream(lmtBitmap, AName, LStream);
  finally
    LStream.Free;
  end;
end;

function TLogger.SendGraphic(AValue: TGraphic): ILogger;
begin
  Result := SendGraphic('', AValue);
end;

function TLogger.SendDataSet(const AName: string; AValue: TDataSet): ILogger;
var
  LFDMemTable : TFDMemTable;
  LStream     : TStream;
begin
  Result := Self;
  LFDMemTable := TFDMemTable.Create(nil);
  try
    LFDMemTable.CopyDataSet(AValue, [coStructure, coRestart, coAppend]);
    LStream := TMemoryStream.Create;
    try
      LFDMemTable.SaveToStream(LStream, sfBinary);
      InternalSendStream(lmtDataSet, AName, LStream);
    finally
      LStream.Free;
    end;
  finally
    LFDMemTable.Free;
  end;
end;

function TLogger.SendDataSet(AValue: TDataSet): ILogger;
begin
  Result := SendDataSet('', AValue);
end;

function TLogger.SendDate(const AName: string; AValue: TDate): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.SendTime(const AName: string; AValue: TTime): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.SendVariant(const AValue: Variant): ILogger;
begin
  Result := SendVariant('', AValue);
end;

function TLogger.SendText(const AName, AText, AHighlighter: string): ILogger;
var
  S : string;
begin
  if AHighlighter.IsEmpty then
    S := Format('%s'#13#10'%s', [AName, AText])
  else
    S := Format('%s (%s)'#13#10'%s', [AName, AHighlighter, AText]);
  Result := InternalSend(lmtText, S);
end;

function TLogger.SendText(const AText: string): ILogger;
var
  S : string;
begin
  S := #13#10 + AText;
  Result := InternalSend(lmtText, S);
end;

function TLogger.SendTime(AValue: TTime): ILogger;
begin
  Result := SendTime('', AValue);
end;

function TLogger.SendVariant(const AName: string; const AValue: Variant): ILogger;
begin
  Result := Send(AName, TValue.FromVariant(AValue));
end;

function TLogger.SendBitmap(const AName: string; AValue: TBitmap;
  ASendCompressed: Boolean): ILogger;
var
  LPngImage : TPngImage;
begin
  if ASendCompressed then
  begin
    LPngImage := TPngImage.Create;
    try
      LPngImage.Assign(AValue);
      Result := SendGraphic(LPngImage);
    finally
      LPngImage.Free;
    end;
  end
  else
    Result := SendGraphic(AValue);
end;

function TLogger.SendPersistent(const AName: string; AValue: TPersistent): ILogger;
begin
  Guard.CheckNotNull(AValue, AName);
  Result := InternalSend(
    lmtPersistent,
    Format('%s (%s) = ' + sLineBreak + '%s',
      [AName, AValue.ClassName, Reflect.PublishedProperties(AValue).ToString]
    )
  );
end;

function TLogger.SendPoint(const AName: string; const AValue: TPoint): ILogger;
begin
  Result := Send(AName, TValue.From(AValue));
end;

function TLogger.SendPointer(const AName: string; AValue: Pointer): ILogger;
begin
  Result := InternalSend(lmtValue, AName + ' = $' + IntToHex(NativeInt(AValue), 8));
end;

function TLogger.SendException(const AName: string; AValue: Exception): ILogger;
var
  S : string;
begin
  if AValue <> nil then
    S := AValue.ClassName + ' - ' + AValue.Message + sLineBreak;
  Result := InternalSendBuffer(lmtException, AName, S[1], Length(S));
end;

function TLogger.SendMemory(const AName: string; AAddress: Pointer;
  ASize: Uint32): ILogger;
begin
  Result := InternalSendBuffer(lmtMemory, AName, AAddress^, ASize);
end;

function TLogger.SendObject(AValue: TObject): ILogger;
begin
  Result := SendObject('', AValue);
end;

function TLogger.SendBitmap(AValue: TBitmap; ASendCompressed: Boolean): ILogger;
begin
  Result := SendBitmap('', AValue, ASendCompressed);
end;

{ SendIf sends a message if it meets a given condition. AText is intended to be
  a textual representation of the given boolean expression. The message is only
  sent if the boolean expression evaluates to the given AIsTrue value (defaults
  to True) }

function TLogger.SendIf(const AText: string; AExpression, AIsTrue: Boolean): ILogger;
begin
  Result := Self;
  if AExpression = AIsTrue then
    InternalSend(lmtConditional, AText);
end;

function TLogger.SendInterface(AValue: IInterface): ILogger;
begin
  Result := SendInterface('', AValue);
end;

function TLogger.SendInterface(const AName: string; AValue: IInterface): ILogger;
var
  O  : TObject;
  S1 : string;
  S2 : string;
begin
  Guard.CheckNotNull(AValue, AName);
  GetInterfaceTypeName(AValue).Unpack(S1, S2);
  O := AValue as TObject;
  Result := InternalSend(
    lmtInterface,
    Format('%s (%s, %s) = ' + sLineBreak + '%s',
    [AName, S1, S2, Reflect.Fields(O).ToString
    + #13#10 + #13#10 + Reflect.Properties(O).ToString]
    )
  );
end;

function TLogger.SendCustomData(const AName: string; const AData: TValue): ILogger;
begin
  Result := SendCustomData(AName, AData, FOnCustomData);
end;

function TLogger.SendCustomData(const AName: string; const AData: TValue;
  AFunc: TCustomDataCallbackMethod): ILogger;
var
  B : Boolean;
  S : string;
begin
  Result := Self;
  if not Assigned(AFunc) then
    Exit;
  B := True;
  S := AFunc(Self, AData, B);
  if B then
    InternalSendBuffer(lmtCustomData, AName, S[1], Length(S));
end;

function TLogger.SendAlphaColor(const AName: string; AValue: TAlphaColor): ILogger;
var
  LHex   : string;
  LColor : string;
  S      : string;
begin
  LHex   := '$' + Integer(AValue).ToHexString;
  LColor := 'cla' + AlphaColorToString(AValue);
  if LHex = LColor then
    S := Format('%s (TAlphaColor) = %s', [AName, LHex])
  else
    S := Format('%s (TAlphaColor) = %s (%s)', [AName, LHex, LColor]);
  Result := InternalSend(lmtAlphaColor, S);
end;

function TLogger.SendColor(const AName: string; AColor: TColor): ILogger;
var
  LHex   : string;
  LColor : string;
  S      : string;
begin
  LHex   := '$' + Integer(AColor).ToHexString;
  LColor := ColorToString(AColor);
  if LHex = LColor then
    S := Format('%s (TColor) = %s', [AName, LHex])
  else
    S := Format('%s (TColor) = %s (%s)', [AName, LHex, LColor]);
  Result := InternalSend(lmtColor, S);
end;

function TLogger.SendComponent(const AName: string; AValue: TComponent): ILogger;
var
  LStream : TStream;
  S       : string;
begin
  Guard.CheckNotNull(AValue, AName);
  Result := Self;
  S := Format('%s (%s) = %s', [
    AName,
    AValue.ClassName,
    AValue.Name
  ]);
  LStream := TMemoryStream.Create;
  try
    LStream.WriteComponent(TComponent(AValue));
    InternalSendStream(lmtComponent, S, LStream);
  finally
    LStream.Free;
  end;
end;

function TLogger.SendCustomData(const AName: string; const AData: TValue;
  AFunc: TCustomDataCallbackFunction): ILogger;
var
  B : Boolean;
  S : string;
begin
  Result := Self;
  if not Assigned(AFunc) then
    Exit;
  B := True;
  S := AFunc(Self, AData, B);
  if B then
    InternalSendBuffer(lmtCustomData, AName, S[1], Length(S));
end;

function TLogger.SendColor(AValue: TColor): ILogger;
begin
  Result := SendColor('', AValue);
end;

function TLogger.SendComponent(AValue: TComponent): ILogger;
begin
  Result := SendComponent('', AValue);
end;

function TLogger.SendPersistent(AValue: TPersistent): ILogger;
begin
  Result := SendPersistent('', AValue);
end;

function TLogger.SendPoint(const AValue: TPoint): ILogger;
begin
  Result := SendPoint('', AValue);
end;

function TLogger.SendPointer(AValue: Pointer): ILogger;
begin
  Result := SendPointer('', AValue);
end;

function TLogger.SendRect(const AValue: TRect): ILogger;
begin
  Result := SendRect('', AValue);
end;

function TLogger.SendScreenShot(AForm: TCustomForm): ILogger;
begin
  Result := SendScreenshot('', AForm);
end;

function TLogger.SendShortCut(AValue: TShortCut): ILogger;
begin
  Result := SendShortCut('', AValue);
end;

function TLogger.SendStrings(AValue: TStrings): ILogger;
begin
  Result := SendStrings('', AValue);
end;

function TLogger.SendDate(AValue: TDate): ILogger;
begin
  Result := SendDate('', AValue);
end;

function TLogger.SendDateTime(AValue: TDateTime): ILogger;
begin
  Result := SendDateTime('', AValue);
end;
{$ENDREGION}

function TLogger.Action(AAction: TBasicAction): ILogger;
begin
  Result := InternalSend(lmtAction, AAction.Name);
end;

{$REGION 'specialized SendText methods'}
function TLogger.SendINI(const AValue: string): ILogger;
begin
  Result := SendINI('', AValue);
end;

function TLogger.SendINI(const AName, AValue: string): ILogger;
begin
  Result := SendText(AName, AValue, 'INI');
end;

function TLogger.SendXML(const AName, AValue: string): ILogger;
begin
  Result := SendText(AName, AValue, 'XML');
end;

function TLogger.SendXML(const AValue: string): ILogger;
begin
  Result := SendXML('', AValue);
end;

function TLogger.SendJSON(const AName, AValue: string): ILogger;
begin
  Result := SendText(AName, AValue, 'JSON');
end;

function TLogger.SendJSON(const AValue: string): ILogger;
begin
  Result := SendJSON('', AValue);
end;

function TLogger.SendHTML(const AName, AValue: string): ILogger;
begin
  Result := SendText(AName, AValue, 'HTML');
end;

function TLogger.SendHTML(const AValue: string): ILogger;
begin
  Result := SendHTML('', AValue);
end;

function TLogger.SendSQL(const AName, AValue: string): ILogger;
begin
  Result := SendText(AName, AValue, 'SQL');
end;

function TLogger.SendSQL(const AValue: string): ILogger;
begin
  Result := SendSQL('', AValue);
end;
{$ENDREGION}

{$REGION 'Counter'}
function TLogger.IncCounter(const AName: string): ILogger;
var
  LValue : Int64;
begin
  if not FCounterList.ContainsKey(AName) then
  begin
    LValue := 1;
    FCounterList.AddOrSetValue(AName, LValue);
  end
  else
  begin
    LValue := FCounterList[AName] + 1;
    FCounterList[AName] := LValue;
  end;
  Result := InternalSend(lmtCounter, AName + ' = ' + IntToStr(LValue));
end;

function TLogger.DecCounter(const AName: string): ILogger;
var
  LValue : Int64;
begin
  if not FCounterList.ContainsKey(AName) then
  begin
    LValue := -1;
    FCounterList.AddOrSetValue(AName, LValue);
  end
  else
  begin
    LValue := FCounterList[AName] - 1;
    FCounterList[AName] := LValue;
  end;
  Result := InternalSend(lmtCounter, AName + ' = ' + IntToStr(LValue));
end;

function TLogger.ResetCounter(const AName: string): ILogger;
begin
  if not FCounterList.ContainsKey(AName) then
  begin
    FCounterList.Add(AName, 0);
  end
  else
  begin
    FCounterList[AName] := 0;
  end;
  Result := InternalSend(lmtCounter, AName + ' = 0');
end;

function TLogger.GetCounter(const AName: string): Integer;
begin
  if not FCounterList.ContainsKey(AName) then
  begin
    Result := 0;
  end
  else
  begin
    Result := FCounterList[AName];
  end;
end;
{$ENDREGION}

{$REGION 'CheckPoint'}
function TLogger.AddCheckPoint(const AName: string): ILogger;
var
  I : Integer;
  J : Integer;
begin
  I := FCheckList.IndexOf(AName);
  if I <> -1 then
  begin
    // Add a custom CheckList
    J := Integer(FCheckList.Objects[I]) + 1;
    FCheckList.Objects[I] := TObject(J);
  end
  else
  begin
    FCheckList.AddObject(AName, TObject(0));
    J := 0;
  end;
  Result := InternalSend(lmtCheckpoint, AName + ' #' + IntToStr(J));
end;

function TLogger.ResetCheckPoint(const AName: string): ILogger;
var
  I : Integer;
  S : string;
begin
  Result := Self;
  if AName = '' then
    S := DEFAULT_CHECKPOINTNAME
  else
    S := AName;
  I := FCheckList.IndexOf(AName);
  if I <> -1 then
  begin
    FCheckList.Objects[I] := TObject(0);
    InternalSend(lmtCheckpoint, S + ' #0');
  end;
end;
{$ENDREGION}

{$REGION 'Tracing methods'}
function TLogger.Enter(const AName: string): ILogger;
begin
  Result := Enter(nil, AName);
end;

function TLogger.Enter(ASender: TObject; const AName: string): ILogger;
begin
  Result := Self;
  FLogStack.Insert(0, UpperCase(AName));
  if ASender <> nil then
  begin
    if ASender is TComponent then
      InternalSend(lmtEnterMethod, TComponent(ASender).Name + '.' + AName)
    else
      InternalSend(lmtEnterMethod, ASender.ClassName + '.' + AName);
  end
  else
    InternalSend(lmtEnterMethod, AName);
end;

function TLogger.Enter(ASender: TObject; const AName: string;
  const AArgs: array of const): ILogger;
begin
  Result := Enter(ASender, Format(AName, AArgs));
end;

function TLogger.Enter(const AName: string;
  const AArgs: array of const): ILogger;
begin
  Result := Enter(nil, AName, AArgs);
end;

function TLogger.Leave(const AName: string): ILogger;
begin
  Result := Leave(nil, AName);
end;

function TLogger.Leave(ASender: TObject; const AName: string): ILogger;
var
  I : Integer;
begin
  Result := Self;
  // ensure that Leave will be called allways if there's a unpaired Enter
  if FLogStack.Count = 0 then
    Exit;
  I := FLogStack.IndexOf(AName);
  if I <> -1 then
    FLogStack.Delete(I)
  else
    Exit;
  if ASender <> nil then
  begin
    if ASender is TComponent then
      InternalSend(lmtLeaveMethod, TComponent(ASender).Name + '.' + AName)
    else
      InternalSend(lmtLeaveMethod, ASender.ClassName + '.' + AName);
  end
  else
    InternalSend(lmtLeaveMethod, AName);
end;

function TLogger.Leave(ASender: TObject; const AName: string;
  const AArgs: array of const): ILogger;
begin
  Result := Leave(ASender, Format(AName, AArgs));
end;

function TLogger.Leave(const AName: string;
  const AArgs: array of const): ILogger;
begin
  Result := Leave(nil, AName, AArgs);
end;

function TLogger.Track(const AName: string): IInterface;
begin
  Result := TTrack.Create(Self, nil, AName);
end;

function TLogger.Track(ASender: TObject; const AName: string): IInterface;
begin
  Result := TTrack.Create(Self, ASender, AName);
end;

function TLogger.Track(const AName: string;
  const AArgs: array of const): IInterface;
begin
  Result := TTrack.Create(Self, nil, Format(AName, AArgs));
end;

function TLogger.Track(ASender: TObject; const AName: string;
  const AArgs: array of const): IInterface;
begin
  Result := TTrack.Create(Self, ASender, Format(AName, AArgs));
end;
{$ENDREGION}

{$REGION 'Watch'}
function TLogger.Watch(const AName, AValue: string): ILogger;
begin
  Result := Watch(AName, TValue.From(AValue));
end;

function TLogger.Watch(const AName: string; const AValue: AnsiString): ILogger;
begin
  Result := Watch(AName, TValue.From(AValue));
end;

function TLogger.Watch(const AName: string; const AValue: WideString): ILogger;
begin
  Result := Watch(AName, TValue.From(AValue));
end;

function TLogger.Watch(const AName: string; const AValue: ShortString): ILogger;
begin
  Result := Watch(AName, TValue.From(AValue));
end;

function TLogger.Watch(const AName: string; const AValue: TValue): ILogger;
var
  S : string;
begin
  if AValue.TypeInfo <> nil then
  begin
    S := Format('%s (%s) = %s', [
      AName,
      AValue.TypeInfo.TypeName,
      StringValueOf(AValue)
    ]);
  end
  else
  begin
    S := Format('%s = %s', [
      AName,
      StringValueOf(AValue)
    ]);
  end;
  Result := InternalSend(lmtWatch, S);
end;
{$ENDREGION}
{$ENDREGION}
{$ENDREGION}

{$REGION 'TLogger.TTrack'}
constructor TLogger.TTrack.Create(const ALogger: ILogger;
  ASender: TObject; const AName: string);
begin
  inherited Create;
  FLogger := ALogger;
  FSender := ASender;
  FName   := AName;
  if not Assigned(FSender) then
    FLogger.Enter(FName)
  else
    FLogger.Enter(FSender, FName);
end;

destructor TLogger.TTrack.Destroy;
begin
  if not Assigned(FSender) then
    FLogger.Leave(FName)
  else
    FLogger.Leave(FSender, FName);
  inherited Destroy;
end;
{$ENDREGION}

end.
