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

{
  The Original Code is part of the LuiPack library for Freepascal/Lazarus.
  The Initial Developer of the Original Code (multilog.pas) is Luiz Américo
  Pereira Câmara (pascalive@bol.com.br). Portions created by the Initial
  Developer are Copyright (C) 2006. All Rights Reserved. You may obtain a copy
  of the original code at http://code.google.com/p/luipack/
}

unit DDuce.Logger.Base;

//{$I DDuce.inc}

{
TODO Track method (like Spring4D)
}

interface

uses
  System.Types, System.Classes, System.SysUtils, System.Rtti,
  Winapi.Windows,

  Spring.Collections,

  DDuce.Logger.Interfaces;

const
  DEFAULT_MAXSTACKCOUNT = 20;

type
  TLogger = class(TInterfacedObject, ILogger)
  strict private
    FMaxStackCount : Integer;
    FChannels      : IList<ILogChannel>;
    FLogStack      : TStrings;
    FCheckList     : TStringList;
    FCounterList   : TStringList;
    FOnCustomData  : TCustomDataCallbackMethod;

    procedure SetMaxStackCount(const AValue: Integer);
    function GetChannels: TChannelList;

  strict protected
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

    procedure Clear;

    // Send functions
    procedure Send(const AName: string; const AArgs: array of const); overload;
    procedure Send(const AName: string; const AValue: string = ''); overload;
    // no need to define overloads which have an implicit cast to TValue
    procedure Send(const AName: string; const AValue: TValue); overload;

    procedure SendStrings(const AName: string; AValue: TStrings);
    { Will send the component as a dfm-stream. }
    procedure SendComponent(const AName: string; AValue: TComponent);
    { Will send object data using RTTI information. }
    procedure SendObject(const AName: string; AValue: TObject);
    procedure SendDateTime(const AName: string; AValue: TDateTime);
    procedure SendDate(const AName: string; AValue: TDate);
    procedure SendTime(const AName: string; AValue: TTime);
    procedure SendRect(const AName: string; const AValue: TRect);
    procedure SendPoint(const AName: string; const APoint: TPoint);
    procedure SendPointer(const AName: string; APointer: Pointer);
    procedure SendException(const AName: string; AException: Exception);
    procedure SendMemory(const AName: string; AAddress: Pointer; ASize: LongWord);

    //procedure SendColor(

    procedure SendIf(
      const AText : string;
      AExpression : Boolean;
      AIsTrue     : Boolean = True
    );

    procedure SendWarning(const AText: string); overload;
    procedure SendWarning(
      const AText : string;
      const AArgs : array of const
    ); overload;
    procedure SendError(const AText: string); overload;
    procedure SendError(
      const AText : string;
      const AArgs : array of const
    ); overload;
    procedure SendInfo(const AText: string); overload;
    procedure SendInfo(
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

    { Watches support }
    procedure Watch(const AName: string; const AValue: string); overload;
    procedure Watch(const AName: string; AValue: Integer); overload;
    procedure Watch(const AName: string; AValue: Cardinal); overload;
    procedure Watch(const AName: string; AValue: Double); overload;
    procedure Watch(const AName: string; AValue: Boolean); overload;

    { List of channels where logmessages will be sent to }
    property Channels: TChannelList
      read GetChannels;

    property LogStack: TStrings
      read FLogStack;

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
  System.TypInfo, System.StrUtils,
  Vcl.Forms,

  Spring,

  DDuce.Reflect;

const
  STACKCOUNTLIMIT        = 256;
  DEFAULT_CHECKPOINTNAME = 'CheckPoint';

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
procedure TLogger.SetMaxStackCount(const AValue: Integer);
begin
  if AValue < STACKCOUNTLIMIT then
    FMaxStackCount := AValue
  else
    FMaxStackCount := STACKCOUNTLIMIT;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TLogger.InternalSendStream(AMsgType: TLogMessageType; const AText: string;
  AStream: TStream);
var
  LM : TLogMessage;
  LC : ILogChannel;
begin
  LM.MsgType := Integer(AMsgType);
  LM.MsgTime := Now;
  LM.MsgText := AnsiString(AText);
  LM.Data    := AStream;
  for LC in Channels do
    if LC.Active then
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
//var
//  LC: ILogChannel;
begin
  InternalSend(lmtClear);
//  for LC in Channels do
//    if LC.Active then
//       LC.Clear;
end;

procedure TLogger.SendInfo(const AText: string);
begin
  InternalSend(lmtInfo, AText);
end;

procedure TLogger.SendInfo(const AText: string; const AArgs: array of const);
begin
  InternalSend(lmtInfo, Format(AText, AArgs));
end;

procedure TLogger.Send(const AName: string; const AValue: string);
begin
  InternalSend(lmtValue, AName + ' = ' + AValue);
end;

procedure TLogger.SendRect(const AName: string; const AValue: TRect);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.SendStrings(const AName: string; AValue: TStrings);
var
  S       : string;
begin
  Guard.CheckNotNull(AValue, AName);
  Send(AName, AValue.Text);
end;

procedure TLogger.SendObject(const AName: string; AValue: TObject);
begin
  Guard.CheckNotNull(AValue, AName);
  Send(AName, Reflect.Fields(AValue).ToString);
end;

procedure TLogger.SendDateTime(const AName: string; AValue: TDateTime);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.SendDate(const AName: string; AValue: TDate);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.SendTime(const AName: string; AValue: TTime);
begin
  Send(AName, TValue.From(AValue));
end;

procedure TLogger.Send(const AName: string; const AValue: TValue);
begin
  case AValue.Kind of
    tkInteger:
      InternalSend(lmtValue, AName + ' = ' + IntToStr(AValue.AsInteger));
    tkEnumeration:
    begin
      if AValue.TypeInfo = TypeInfo(Boolean) then
      begin
        InternalSend(lmtValue, AName + ' = ' + BoolToStr(AValue.AsBoolean, True));
      end;
    end;
    tkFloat:
    begin
      if AValue.TypeInfo = TypeInfo(TDate) then
      begin
        InternalSend(lmtValue, AName + ' = ' + DateToStr(AValue.AsType<TDate>));
      end
      else
      if AValue.TypeInfo = TypeInfo(TDateTime) then
      begin
        InternalSend(lmtValue, AName + ' = ' + DateTimeToStr(AValue.AsType<TDateTime>));
      end
      else
      if AValue.TypeInfo = TypeInfo(TTime) then
      begin
        InternalSend(lmtValue, AName + ' = ' + TimeToStr(AValue.AsType<TTime>));
      end
      else
      begin
        InternalSend(lmtValue, AName + ' = ' + FloatToStr(AValue.AsExtended));
      end;
    end;
    tkClass:
      SendObject(AName, AValue.AsObject);
    tkInt64:
      InternalSend(lmtValue, AName + ' = ' + IntToStr(AValue.AsInt64));
    tkRecord:
      InternalSend(lmtValue, AName + ' = ' + Reflect.Fields(AValue).ToString);
    tkInterface:
      InternalSend(lmtValue, AName + ' = ' + Reflect.Fields(AValue).ToString);
  else
    InternalSend(lmtValue, AName + ' = ' + AValue.ToString);
  end;
end;

procedure TLogger.Send(const AName: string; const AArgs: array of const);
begin
  Send(Format(AName, AArgs));
end;

procedure TLogger.SendPoint(const AName: string; const APoint: TPoint);
begin

end;

procedure TLogger.SendPointer(const AName: string; APointer: Pointer);
begin
  InternalSend(lmtValue, AName + ' = ' + IntToHex(NativeInt(APointer), 8));
end;

procedure TLogger.SendException(const AName: string; AException: Exception);
var
  S: string;
begin
  if AException <> nil then
    S := AException.ClassName + ' - ' + AException.Message + sLineBreak;
  InternalSendBuffer(lmtException, AName, S[1], Length(S));
end;

procedure TLogger.SendMemory(const AName: string; AAddress: Pointer;
  ASize: LongWord);
begin
  InternalSendBuffer(lmtMemory, AName, AAddress^, ASize);
end;

procedure TLogger.SendIf(const AText: string; AExpression, AIsTrue: Boolean);
begin
  if AExpression = AIsTrue then
    InternalSend(lmtConditional, AText);
end;

procedure TLogger.SendWarning(const AText: string);
begin
  InternalSend(lmtWarning, AText);
end;

procedure TLogger.SendWarning(const AText: string; const AArgs: array of const);
begin
  InternalSend(lmtWarning, Format(AText, AArgs));
end;

procedure TLogger.SendError(const AText: string);
begin
  InternalSend(lmtError, AText);
end;

procedure TLogger.SendError(const AText: string; const AArgs: array of const);
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

procedure TLogger.SendComponent(const AName: string; AValue: TComponent);
var
  S       : string;
  LStream : TStream;
begin
  Guard.CheckNotNull(AValue, AName);
  if AValue <> nil then
  begin
    S := AName + ' (';
    S := S + ('"' + TComponent(AValue).Name + '"/');
    LStream := TMemoryStream.Create;
    try
      LStream.WriteComponent(TComponent(AValue));
      S := S + (AValue.ClassName + '/');
      S := S + ('$' + IntToHex(Integer(AValue),
      SizeOf(Integer) * 2) + ')');
      InternalSendStream(lmtObject, S, LStream);
    finally
      LStream.Free;
    end;
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
    InternalSend(lmtCounter, FCounterList[I] + '=0');
  end;
end;

function TLogger.GetChannels: TChannelList;
begin
  Result := FChannels;
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
  // even if Classes is not Active
  if FLogStack.Count = 0 then
    Exit;
  // todo: see if is necessary to do Uppercase (set case sensitive to false?)
  I := FLogStack.IndexOf(UpperCase(AName));
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

procedure TLogger.Watch(const AName, AValue: string);
begin
  InternalSend(lmtWatch, AName + '=' + AValue);
end;

procedure TLogger.Watch(const AName: string; AValue: Integer);
begin
  InternalSend(lmtWatch, AName + '=' + IntToStr(AValue));
end;

procedure TLogger.Watch(const AName: string; AValue: Cardinal);
begin
  InternalSend(lmtWatch, AName + '=' + IntToStr(AValue));
end;

procedure TLogger.Watch(const AName: string; AValue: Double);
begin
  InternalSend(lmtWatch, AName + '=' + FloatToStr(AValue));
end;

procedure TLogger.Watch(const AName: string; AValue: Boolean);
begin
  InternalSend(lmtWatch, AName + '=' + BoolToStr(AValue));
end;
{$ENDREGION}
{$ENDREGION}

end.
