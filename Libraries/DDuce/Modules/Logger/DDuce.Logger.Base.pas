{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I DDuce.inc}

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
    FCounterList   : TStringList;
    FOnCustomData  : TCustomDataCallbackMethod;
    FLogLevel      : Byte;

    {$REGION 'property access methods'}
    procedure SetMaxStackCount(const AValue: Integer);
    function GetChannels: TChannelList;
    function GetLogLevel: Byte;
    procedure SetLogLevel(const Value: Byte);
    {$ENDREGION}

  protected
    function StringValueOf(const AValue: TValue): string;

    procedure InternalSend(
      AMsgType    : TLogMessageType;
      const AText : string = ''
    );
    procedure InternalSendStream(
      AMsgType    : TLogMessageType;
      const AText : string;
      AStream     : TStream
    );
    procedure InternalSendBuffer(
      AMsgType    : TLogMessageType;
      const AText : string;
      var ABuffer;
      ACount      : LongWord
    );

    function CalledBy(const AName: string): Boolean;

    { Sends a dedicated message to clear content in the receiver (LogViewer). }
    procedure Clear;

    // Send procedures
    procedure Send(const AName: string; const AArgs: array of const); overload;

    procedure Send(const AName: string; const AValue: string); overload;
    procedure Send(const AName: string; const AValue: AnsiString); overload;
    procedure Send(const AName: string; const AValue: WideString); overload;
    procedure Send(const AName: string; const AValue: ShortString); overload;

    // Overloads for builtin integer types
    procedure Send(const AName: string; const AValue: Cardinal); overload;
    procedure Send(const AName: string; const AValue: Word); overload;
    procedure Send(const AName: string; const AValue: SmallInt); overload;
    procedure Send(const AName: string; const AValue: Byte); overload;
    procedure Send(const AName: string; const AValue: ShortInt); overload;
    procedure Send(const AName: string; const AValue: UInt64); overload;
    procedure Send(const AName: string; const AValue: FixedInt); overload;

    // no need to define overloads which have an implicit cast to TValue
    procedure Send(const AName: string; const AValue: TValue); overload;

    procedure Send(const AValue: AnsiString); overload;
    procedure Send(const AValue: WideString); overload;
    procedure Send(const AValue: ShortString); overload;
    procedure Send(const AValue: string); overload;

    procedure Send(const AValue: Byte); overload;
    procedure Send(const AValue: Word); overload;
    procedure Send(const AValue: Cardinal); overload;
    procedure Send(const AValue: UInt64); overload;
    procedure Send(const AValue: ShortInt); overload;
    procedure Send(const AValue: SmallInt); overload;
    procedure Send(const AValue: FixedInt); overload;

    procedure Send(const AValue: TValue); overload;

    procedure SendDateTime(const AName: string; AValue: TDateTime); overload;
    procedure SendDateTime(AValue: TDateTime); overload;
    procedure SendDate(const AName: string; AValue: TDate); overload;
    procedure SendDate(AValue: TDate); overload;
    procedure SendTime(const AName: string; AValue: TTime); overload;
    procedure SendTime(AValue: TTime); overload;

    { Send methods for types that need a custom representation. }
    procedure SendColor(const AName: string; AColor: TColor); overload;
    procedure SendColor(AValue: TColor); overload;
    procedure SendAlphaColor(const AName: string; AValue: TAlphaColor); overload;
    procedure SendAlphaColor(AValue: TAlphaColor); overload;
    { Will send object data using RTTI information. }
    procedure SendObject(const AName: string; AValue: TObject); overload;
    procedure SendObject(AValue: TObject); overload;
    { Logs interface properties. }
    procedure SendInterface(const AName: string; AValue: IInterface); overload;
    procedure SendInterface(AValue: IInterface); overload;
    { Logs published properties. }
    procedure SendPersistent(const AName: string; AValue: TPersistent); overload;
    procedure SendPersistent(AValue: TPersistent); overload;
    procedure SendRect(const AName: string; const AValue: TRect); overload;
    procedure SendRect(const AValue: TRect); overload;
    procedure SendPoint(const AName: string; const AValue: TPoint); overload;
    procedure SendPoint(const AValue: TPoint); overload;
    procedure SendStrings(const AName: string; AValue: TStrings); overload;
    procedure SendStrings(AValue: TStrings); overload;
    { Will send the component as a dfm-stream. }
    procedure SendComponent(const AName: string; AValue: TComponent); overload;
    procedure SendComponent(AValue: TComponent); overload;
    procedure SendPointer(const AName: string; AValue: Pointer); overload;
    procedure SendPointer(AValue: Pointer); overload;
    procedure SendException(const AName: string; AValue: Exception); overload;
    procedure SendException(AValue: Exception); overload;
    procedure SendBitmap(const AName: string; AValue: TBitmap); overload;
    procedure SendBitmap(AValue: TBitmap); overload;
    procedure SendScreenShot(const AName: string; AForm: TCustomForm); overload;
    procedure SendScreenShot(AForm: TCustomForm); overload;
    procedure SendDataSet(const AName: string; AValue: TDataSet); overload;
    procedure SendDataSet(AValue: TDataSet); overload;
    procedure SendShortCut(const AName: string; AValue: TShortCut); overload;
    procedure SendShortCut(AValue: TShortCut); overload;
    procedure SendVariant(const AName: string; const AValue: Variant); overload;
    procedure SendVariant(const AValue: Variant); overload;

    procedure SendMemory(
      const AName: string;
      AAddress   : Pointer;
      ASize      : UInt32
    );

    { Send methods for text that can be displayed with a dedicated
      highlighter. }
    procedure SendText(
      const AName        : string;
      const AText        : string;
      const AHighlighter : string = ''
    ); overload;
    procedure SendText(const AText: string); overload;

    procedure SendSQL(const AName: string; const AValue: string); overload;
    procedure SendSQL(const AValue: string); overload;
    procedure SendXML(const AName: string; const AValue: string); overload;
    procedure SendXML(const AValue: string); overload;
    procedure SendHTML(const AName: string; const AValue: string); overload;
    procedure SendHTML(const AValue: string); overload;
    procedure SendINI(const AName: string; const AValue: string); overload;
    procedure SendINI(const AValue: string); overload;
    procedure SendJSON(const AName: string; const AValue: string); overload;
    procedure SendJSON(const AValue: string); overload;

    procedure SendIf(
      const AText : string;
      AExpression : Boolean;
      AIsTrue     : Boolean = True
    );

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
      const AText : string;
      const AArgs : array of const
    ); overload;

    { Uses the OnCustomData event as callback. }
    procedure SendCustomData(
      const AName : string;
      const AData : TValue
    ); overload;
    procedure SendCustomData(
      const AName : string;
      const AData : TValue;
      AFunc       : TCustomDataCallbackMethod
    ); overload;
    procedure SendCustomData(
      const AName : string;
      const AData : TValue;
      AFunc       : TCustomDataCallbackFunction
    ); overload;

    { Checkpoints are used to count how many times the application enters a
      certain position in the code. }
    procedure AddCheckPoint(const AName: string = '');
    procedure ResetCheckPoint(const AName: string = '');

    { Counter support }
    procedure IncCounter(const AName: string);
    procedure DecCounter(const AName: string);
    procedure ResetCounter(const AName: string);
    function GetCounter(const AName: string): Integer;

    { Callstack logging methods }
    procedure Enter(const AName: string); overload;
    procedure Enter(ASender: TObject; const AName: string); overload;
    procedure Leave(const AName: string); overload;
    procedure Leave(ASender: TObject; const AName: string); overload;
    function Track(const AName: string): IInterface; overload;
    function Track(ASender: TObject; const AName: string): IInterface; overload;

    procedure Action(AAction: TBasicAction);

    { Watches support }
    procedure Watch(const AName: string; const AValue: TValue); overload;
    procedure Watch(const AName: string; const AValue: string = ''); overload;
    procedure Watch(const AName: string; const AValue: AnsiString); overload;
    procedure Watch(const AName: string; const AValue: WideString); overload;
    procedure Watch(const AName: string; const AValue: ShortString); overload;

    { List of channels where logmessages will be posted to }
    property Channels: TChannelList
      read GetChannels;

    property LogLevel: Byte
      read GetLogLevel write SetLogLevel;

    property LogStack: TStrings
      read FLogStack;

    { not used yet }
    property MaxStackCount: Integer
      read FMaxStackCount write SetMaxStackCount default DEFAULT_MAXSTACKCOUNT;

    property OnCustomData: TCustomDataCallbackMethod
      read FOnCustomData write FOnCustomData;

  public
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;
  end;

var
  Logger: ILogger = nil;

implementation

uses
  System.TypInfo, System.UIConsts,
  Vcl.Menus,
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
  FCounterList               := TStringList.Create;
  FCounterList.CaseSensitive := False;
  FCounterList.Sorted        := True;
end;

procedure TLogger.BeforeDestruction;
begin
  FLogStack.Free;
  FCheckList.Free;
  FCounterList.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogger.GetChannels: TChannelList;
begin
  Result := FChannels;
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

procedure TLogger.InternalSendStream(AMsgType: TLogMessageType; const AText: string;
  AStream: TStream);
var
  LM : TLogMessage;
  LC : ILogChannel;
begin
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

procedure TLogger.InternalSend(AMsgType: TLogMessageType; const AText: string);
begin
  InternalSendStream(AMsgType, AText, nil);
end;

procedure TLogger.InternalSendBuffer(AMsgType: TLogMessageType; const AText: string;
  var ABuffer; ACount: LongWord);
var
  LStream: TStream;
begin
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

function TLogger.CalledBy(const AName: string): Boolean;
begin
  Result := FLogStack.IndexOf(UpperCase(AName)) <> -1;
end;

procedure TLogger.Clear;
var
  I : Integer;
begin
  // repeated to compensate for initial message loss in combination with
  // some channels (ZeroMQ)
  for I := 0 to 3 do
  begin
    InternalSend(lmtClear);
    Sleep(100);
  end;
end;

procedure TLogger.Info(const AText: string);
begin
  InternalSend(lmtInfo, AText);
end;

procedure TLogger.Info(const AText: string; const AArgs: array of const);
begin
  InternalSend(lmtInfo, Format(AText, AArgs));
end;

procedure TLogger.Send(const AName: string; const AValue: string);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.Send(const AName: string; const AValue: ShortString);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.Send(const AName: string; const AValue: WideString);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.Send(const AName: string; const AValue: AnsiString);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.SendRect(const AName: string; const AValue: TRect);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.SendScreenShot(const AName: string; AForm: TCustomForm);
var
  LBitmap : TBitmap;
  LStream : TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
   LBitmap := AForm.GetFormImage;
   try
     LBitmap.SaveToStream(LStream);
     InternalSendStream(lmtScreenShot, AName, LStream); 
   finally
     LBitmap.Free;
   end;
 finally
    LStream.Free;
  end;
end;

procedure TLogger.SendShortCut(const AName: string; AValue: TShortCut);
begin
  Send(AName, ShortCutToText(AValue));
end;

procedure TLogger.SendStrings(const AName: string; AValue: TStrings);
begin
  Guard.CheckNotNull(AValue, AName);
  InternalSend(
    lmtStrings,
    Format('%s (%s) = ' + sLineBreak + '%s', [AName, AValue.ClassName, AValue.Text])
  );
end;

procedure TLogger.SendObject(const AName: string; AValue: TObject);
begin
  Guard.CheckNotNull(AValue, AName);
  InternalSend(
    lmtObject,
    Format('%s (%s) = ' + sLineBreak + '%s',
    [AName, AValue.ClassName, Reflect.Fields(AValue).ToString
    + #13#10 + #13#10 + Reflect.Properties(AValue).ToString]
    )
  );
end;

procedure TLogger.SendDateTime(const AName: string; AValue: TDateTime);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.SendException(AValue: Exception);
begin
  SendException('', AValue);
end;

procedure TLogger.SendDataSet(const AName: string; AValue: TDataSet);
var
  LFDMemTable : TFDMemTable;
  LStream     : TStream;
begin
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

procedure TLogger.SendDataSet(AValue: TDataSet);
begin
  SendDataSet('', AValue);
end;

procedure TLogger.SendDate(const AName: string; AValue: TDate);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.SendTime(const AName: string; AValue: TTime);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.SendVariant(const AValue: Variant);
begin
  SendVariant('', AValue);
end;

procedure TLogger.SendText(const AName, AText, AHighlighter: string);
var
  S : string;
begin
  if AHighlighter.IsEmpty then
    S := Format('%s'#13#10'%s', [AName, AText])
  else
    S := Format('%s (%s)'#13#10'%s', [AName, AHighlighter, AText]);
  InternalSend(lmtText, S);
end;

procedure TLogger.SendText(const AText: string);
var
  S : string;
begin
  S := #13#10 + AText;
  InternalSend(lmtText, S);
end;

procedure TLogger.SendTime(AValue: TTime);
begin
  SendTime('', AValue);
end;

procedure TLogger.SendVariant(const AName: string; const AValue: Variant);
begin
  Send(AName, TValue.FromVariant(AValue));
end;

procedure TLogger.Send(const AName: string; const AValue: TValue);
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
  InternalSend(lmtValue, S);
end;

procedure TLogger.Send(const AName: string; const AValue: FixedInt);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.Send(const AName: string; const AValue: UInt64);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.Send(const AName: string; const AValue: Byte);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.Send(const AName: string; const AValue: Word);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.Send(const AName: string; const AValue: ShortInt);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.Send(const AName: string; const AValue: Cardinal);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.Send(const AName: string; const AValue: SmallInt);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.Send(const AValue: string);
begin
  Send('', AValue);
end;

procedure TLogger.Send(const AValue: Byte);
begin
  Send('', AValue);
end;

procedure TLogger.Send(const AValue: Word);
begin
  Send('', AValue);
end;

procedure TLogger.Send(const AValue: AnsiString);
begin
  Send('', AValue);
end;

procedure TLogger.Send(const AValue: WideString);
begin
  Send('', AValue);
end;

procedure TLogger.Send(const AValue: ShortString);
begin
  Send('', AValue);
end;

procedure TLogger.Send(const AValue: SmallInt);
begin
  Send('', AValue);
end;

procedure TLogger.Send(const AValue: FixedInt);
begin
  Send('', AValue);
end;

procedure TLogger.Send(const AValue: TValue);
begin
  Send('', AValue);
end;

procedure TLogger.SendAlphaColor(AValue: TAlphaColor);
begin
  SendAlphaColor('', AValue);
end;

procedure TLogger.Send(const AValue: Cardinal);
begin
  Send('', AValue);
end;

procedure TLogger.Send(const AValue: UInt64);
begin
  Send('', AValue);
end;

procedure TLogger.Send(const AValue: ShortInt);
begin
  Send('', AValue);
end;

procedure TLogger.SendBitmap(const AName: string; AValue: TBitmap);
var
  LStream : TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    AValue.SaveToStream(LStream);
    InternalSendStream(lmtBitmap, AName, LStream);
  finally
    LStream.Free;
  end;
end;

procedure TLogger.Send(const AName: string; const AArgs: array of const);
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
end;

procedure TLogger.SendPersistent(const AName: string; AValue: TPersistent);
begin
  Guard.CheckNotNull(AValue, AName);
  InternalSend(
    lmtPersistent,
    Format('%s (%s) = ' + sLineBreak + '%s',
      [AName, AValue.ClassName, Reflect.PublishedProperties(AValue).ToString]
    )
  );
end;

procedure TLogger.SendPoint(const AName: string; const AValue: TPoint);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.SendPointer(const AName: string; AValue: Pointer);
begin
  InternalSend(lmtValue, AName + ' = $' + IntToHex(NativeInt(AValue), 8));
end;

procedure TLogger.SendException(const AName: string; AValue: Exception);
var
  S : string;
begin
  if AValue <> nil then
    S := AValue.ClassName + ' - ' + AValue.Message + sLineBreak;
  InternalSendBuffer(lmtException, AName, S[1], Length(S));
end;

procedure TLogger.SendMemory(const AName: string; AAddress: Pointer;
  ASize: Uint32);
begin
  InternalSendBuffer(lmtMemory, AName, AAddress^, ASize);
end;

procedure TLogger.SendObject(AValue: TObject);
begin
  SendObject('', AValue);
end;

{ SendIf sends a message if it meets a given condition. AText is intended to be
  a textual representation of the given boolean expression. The message is only
  sent if the boolean expression evaluates to the given AIsTrue value (defaults
  to True) }

procedure TLogger.SendIf(const AText: string; AExpression, AIsTrue: Boolean);
begin
  if AExpression = AIsTrue then
    InternalSend(lmtConditional, AText);
end;

procedure TLogger.SendInterface(AValue: IInterface);
begin
  SendInterface('', AValue);
end;

procedure TLogger.SendInterface(const AName: string; AValue: IInterface);
var
  O  : TObject;
  S1 : string;
  S2 : string;
begin
  Guard.CheckNotNull(AValue, AName);
  GetInterfaceTypeName(AValue).Unpack(S1, S2);
  O := AValue as TObject;
  InternalSend(
    lmtInterface,
    Format('%s (%s, %s) = ' + sLineBreak + '%s',
    [AName, S1, S2, Reflect.Fields(O).ToString
    + #13#10 + #13#10 + Reflect.Properties(O).ToString]
    )
  );
end;

procedure TLogger.Warn(const AText: string);
begin
  InternalSend(lmtWarning, AText);
end;

procedure TLogger.Warn(const AText: string; const AArgs: array of const);
begin
  InternalSend(lmtWarning, Format(AText, AArgs));
end;

procedure TLogger.Error(const AText: string);
begin
  InternalSend(lmtError, AText);
end;

procedure TLogger.Error(const AText: string; const AArgs: array of const);
begin
  InternalSend(lmtError, Format(AText, AArgs));
end;

procedure TLogger.SendCustomData(const AName: string; const AData: TValue);
begin
  SendCustomData(AName, AData, FOnCustomData);
end;

procedure TLogger.SendCustomData(const AName: string; const AData: TValue;
  AFunc: TCustomDataCallbackMethod);
var
  B : Boolean;
  S : string;
begin
  if not Assigned(AFunc) then
    Exit;
  B := True;
  S := AFunc(Self, AData, B);
  if B then
    InternalSendBuffer(lmtCustomData, AName, S[1], Length(S));
end;

procedure TLogger.SendAlphaColor(const AName: string; AValue: TAlphaColor);
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
  InternalSend(lmtAlphaColor, S);
end;

procedure TLogger.SendColor(const AName: string; AColor: TColor);
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
  InternalSend(lmtColor, S);
end;

procedure TLogger.SendComponent(const AName: string; AValue: TComponent);
var
  LStream : TStream;
  S       : string;
begin
  Guard.CheckNotNull(AValue, AName);
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

procedure TLogger.SendCustomData(const AName: string; const AData: TValue;
  AFunc: TCustomDataCallbackFunction);
var
  B : Boolean;
  S : string;
begin
  if not Assigned(AFunc) then
    Exit;
  B := True;
  S := AFunc(Self, AData, B);
  if B then
    InternalSendBuffer(lmtCustomData, AName, S[1], Length(S));
end;

procedure TLogger.Action(AAction: TBasicAction);
begin
  InternalSend(lmtAction, AAction.Name);
end;

procedure TLogger.AddCheckPoint(const AName: string);
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
  InternalSend(lmtCheckpoint, AName + ' #' + IntToStr(J));
end;

procedure TLogger.IncCounter(const AName: string);
var
  I : Integer;
  J : Integer;
begin
  I := FCounterList.IndexOf(AName);
  if I <> -1 then
  begin
    J := Integer(FCounterList.Objects[I]) + 1;
    FCounterList.Objects[I] := TObject(J);
  end
  else
  begin
    FCounterList.AddObject(AName, TObject(1));
    J := 1;
  end;
  InternalSend(lmtCounter, AName + '=' + IntToStr(J));
end;

procedure TLogger.DecCounter(const AName: string);
var
  I : Integer;
  J : Integer;
begin
  I := FCounterList.IndexOf(AName);
  if I <> -1 then
  begin
    J := Integer(FCounterList.Objects[I]) - 1;
    FCounterList.Objects[I] := TObject(J);
  end
  else
  begin
    FCounterList.AddObject(AName, TObject(-1));
    J := -1;
  end;
  InternalSend(lmtCounter, AName + '=' + IntToStr(J));
end;

procedure TLogger.ResetCounter(const AName: string);
var
  I : Integer;
begin
  I := FCounterList.IndexOf(AName);
  if I <> -1 then
  begin
    FCounterList.Objects[I] := TObject(0);
    InternalSend(lmtCounter, FCounterList[I] + '= 0');
  end;
end;

function TLogger.GetCounter(const AName: string): Integer;
var
  I: Integer;
begin
  I := FCounterList.IndexOf(AName);
  if I <> -1 then
    Result := Integer(FCounterList.Objects[I])
  else
    Result := 0;
end;

procedure TLogger.ResetCheckPoint(const AName: string);
var
  I: Integer;
  S: string;
begin
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

procedure TLogger.Enter(const AName: string);
begin
  Enter(nil, AName);
end;

procedure TLogger.Enter(ASender: TObject; const AName: string);
begin
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

procedure TLogger.Leave(const AName: string);
begin
  Leave(nil, AName);
end;

procedure TLogger.Leave(ASender: TObject; const AName: string);
var
  I: Integer;
begin
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

function TLogger.Track(const AName: string): IInterface;
begin
  Result := TTrack.Create(Self, nil, AName);
end;

function TLogger.Track(ASender: TObject; const AName: string): IInterface;
begin
  Result := TTrack.Create(Self, ASender, AName);
end;

procedure TLogger.Watch(const AName, AValue: string);
begin
  Watch(AName, TValue.From(AValue));
end;

procedure TLogger.Watch(const AName: string; const AValue: AnsiString);
begin
  Watch(AName, TValue.From(AValue));
end;

procedure TLogger.Watch(const AName: string; const AValue: WideString);
begin
  Watch(AName, TValue.From(AValue));
end;

procedure TLogger.Watch(const AName: string; const AValue: ShortString);
begin
  Watch(AName, TValue.From(AValue));
end;

procedure TLogger.Watch(const AName: string; const AValue: TValue);
var
  S : string;
begin
//  case AValue.Kind of
//    tkEnumeration:
//    begin
//      if AValue.TypeInfo = TypeInfo(Boolean) then
//      begin
//        S := BoolToStr(AValue.AsBoolean, True);
//      end
//      else
//      begin
//        S := AValue.ToString;
//      end;
//    end;
//    tkString, tkLString, tkWString, tkUString:
//      S := AValue.AsString;
//    tkFloat:
//    begin
//      if AValue.TypeInfo = TypeInfo(TDate) then
//      begin
//        S := DateToStr(AValue.AsType<TDate>);
//      end
//      else
//      if AValue.TypeInfo = TypeInfo(TDateTime) then
//      begin
//        S := DateTimeToStr(AValue.AsType<TDateTime>);
//      end
//      else
//      if AValue.TypeInfo = TypeInfo(TTime) then
//      begin
//        S := TimeToStr(AValue.AsType<TTime>);
//      end
//      else
//      begin
//        S := FloatToStr(AValue.AsExtended);
//      end;
//    end;
//    tkInteger:
//      S := AValue.AsInteger.ToString;
//    tkInt64:
//      S := AValue.AsInt64.ToString
//    else
//      S := AValue.ToString;
//  end;
//  InternalSend(lmtWatch, AName + ' = ' + S);
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
  InternalSend(lmtWatch, S);
end;
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

procedure TLogger.SendBitmap(AValue: TBitmap);
begin
  SendBitmap('', AValue);
end;

procedure TLogger.SendColor(AValue: TColor);
begin
  SendColor('', AValue);
end;

procedure TLogger.SendComponent(AValue: TComponent);
begin
  SendComponent('', AValue);
end;

procedure TLogger.SendPersistent(AValue: TPersistent);
begin
  SendPersistent('', AValue);
end;

procedure TLogger.SendPoint(const AValue: TPoint);
begin
  SendPoint('', AValue);
end;

procedure TLogger.SendPointer(AValue: Pointer);
begin
  SendPointer('', AValue);
end;

procedure TLogger.SendRect(const AValue: TRect);
begin
  SendRect('', AValue);
end;

procedure TLogger.SendScreenShot(AForm: TCustomForm);
begin
  SendScreenshot('', AForm);
end;

procedure TLogger.SendShortCut(AValue: TShortCut);
begin
  SendShortCut('', AValue);
end;

procedure TLogger.SendStrings(AValue: TStrings);
begin
  SendStrings('', AValue);
end;

procedure TLogger.SendDate(AValue: TDate);
begin
  SendDate('', AValue);
end;

procedure TLogger.SendDateTime(AValue: TDateTime);
begin
  SendDateTime('', AValue);
end;

{$REGION 'specialized SendText methods'}
procedure TLogger.SendINI(const AValue: string);
begin
  SendINI('', AValue);
end;

procedure TLogger.SendINI(const AName, AValue: string);
begin
  SendText(AName, AValue, 'INI');
end;

procedure TLogger.SendXML(const AName, AValue: string);
begin
  SendText(AName, AValue, 'XML');
end;

procedure TLogger.SendXML(const AValue: string);
begin
  SendXML('', AValue);
end;

procedure TLogger.SendJSON(const AName, AValue: string);
begin
  SendText(AName, AValue, 'JSON');
end;

procedure TLogger.SendJSON(const AValue: string);
begin
  SendJSON('', AValue);
end;

procedure TLogger.SendHTML(const AName, AValue: string);
begin
  SendText(AName, AValue, 'HTML');
end;

procedure TLogger.SendHTML(const AValue: string);
begin
  SendHTML('', AValue);
end;

procedure TLogger.SendSQL(const AName, AValue: string);
begin
  SendText(AName, AValue, 'SQL');
end;

procedure TLogger.SendSQL(const AValue: string);
begin
  SendSQL('', AValue);
end;
{$ENDREGION}

end.
