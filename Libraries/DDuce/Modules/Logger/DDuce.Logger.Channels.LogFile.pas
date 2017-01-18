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

unit DDuce.Logger.Channels.LogFile;

//{$I DDuce.inc}

interface

uses
  System.Classes, System.SysUtils,

  DDuce.Logger.Interfaces, DDuce.Logger.Channels.Base;

type
  TLogFileChannel = class(TCustomLogChannel)
  strict private
    FRelativeIndent : Integer;
    FBaseIndent     : Integer;
    FShowHeader     : Boolean;
    FShowDate       : Boolean;
    FShowTime       : Boolean;
    FShowPrefix     : Boolean;
    FShowStrings    : Boolean;
    FStreamWriter   : TStreamWriter;

    procedure SetShowTime(const AValue: Boolean);
    procedure SetShowDate(const Value: Boolean);

    function Space(ACount: Integer): string;
    procedure UpdateIndentation;
    procedure WriteStrings(AStream: TStream);
    procedure WriteComponent(AStream: TStream);

  public
    constructor Create(const AFileName: string = ''); reintroduce; virtual;

    procedure BeforeDestruction; override;

    function Write(const AMsg: TLogMessage): Boolean; override;

    property ShowHeader: Boolean
      read FShowHeader write FShowHeader default False;

    property ShowPrefix: Boolean
      read FShowPrefix write FShowPrefix default True;

    property ShowTime: Boolean
      read FShowTime write SetShowTime default True;

    property ShowDate: Boolean
      read FShowDate write SetShowDate default False;
  end;

implementation

uses
  System.StrUtils,
  Vcl.Forms;

{$REGION 'construction and destruction'}
constructor TLogFileChannel.Create(const AFileName: string);
var
  S : string;
begin
  inherited Create;
  if AFileName = '' then
  begin
    S := ExtractFilePath(Application.ExeName)
      + FormatDateTime('yyyymmdd hhnnss ', Now)
      + ExtractFileName(ChangeFileExt(Application.ExeName, '.log'));
  end
  else
    S := AFileName;
  FShowPrefix   := True;
  FShowDate     := False;
  FShowTime     := True;
  FShowStrings  := True;
  FShowHeader   := True;
  Active        := True;
  FStreamWriter := TStreamWriter.Create(S, True); // Append
  if FShowHeader then
    FStreamWriter.WriteLine('============|Log Session Started at ' + DateTimeToStr(Now)
      + ' by ' + Application.Title + '|============');
  UpdateIndentation;
end;

procedure TLogFileChannel.BeforeDestruction;
begin
  FStreamWriter.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TLogFileChannel.SetShowDate(const Value: Boolean);
begin
  FShowDate := Value;
  UpdateIndentation;
end;

procedure TLogFileChannel.SetShowTime(const AValue: Boolean);
begin
  FShowTime := AValue;
  UpdateIndentation;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TLogFileChannel.UpdateIndentation;
var
  S : string;
begin
  S := '';
  if ShowDate then
    S := FormatDateTime('yyyy-mm-dd ', Date);
  if ShowTime then
    S := S + FormatDateTime('hh:nn:ss:zzz', Time);
  FBaseIndent := Length(S) + 3;
end;

function TLogFileChannel.Space(ACount: Integer): string;
begin
  Result := DupeString(' ', ACount);
end;

procedure TLogFileChannel.WriteStrings(AStream: TStream);
var
  I  : Integer;
  SL : TStringList;
begin
  if AStream.Size > 0 then
  begin
    SL := TStringList.Create;
    try
      AStream.Position := 0;
      SL.LoadFromStream(AStream);
      for I := 0 to SL.Count - 1 do
        FStreamWriter.WriteLine(
          Space(FRelativeIndent + FBaseIndent) + SL.Strings[I]
        );
    finally
      SL.Free;
    end;
  end;
end;

procedure TLogFileChannel.WriteComponent(AStream: TStream);
begin
  AStream.Seek(0, soFromBeginning);
  ObjectBinaryToText(AStream, FStreamWriter.BaseStream);
end;
{$ENDREGION}

{$REGION 'public methods'}
function TLogFileChannel.Write(const AMsg: TLogMessage): Boolean;
begin
  // Exit method identation must be set before
  if (AMsg.MsgType = Integer(lmtLeaveMethod)) and (FRelativeIndent >= 2) then
    Dec(FRelativeIndent, 2);
  if ShowDate then
    FStreamWriter.Write(FormatDateTime('yyyy-mm-dd', AMsg.MsgTime) + ' ');
  if ShowTime then
    FStreamWriter.Write(FormatDateTime('hh:nn:ss:zzz', AMsg.MsgTime) + ' ');
  FStreamWriter.Write(Space(FRelativeIndent));
  if ShowPrefix then
    FStreamWriter.Write(LOG_PREFIXES[TLogMessageType(AMsg.MsgType)] + ': ');
  FStreamWriter.WriteLine(string(AMsg.MsgText));
  if FShowStrings and (AMsg.Data <> nil) then
  begin
    case TLogMessageType(AMsg.MsgType) of
      lmtStrings, lmtCallStack, lmtHeapInfo, lmtException:
         WriteStrings(AMsg.Data);
      lmtObject:
        WriteComponent(AMsg.Data);
    end;
  end;
  // Update enter method identation
  if TLogMessageType(AMsg.MsgType) = lmtEnterMethod then
    Inc(FRelativeIndent, 2);
  Result := True;
end;
{$ENDREGION}

end.
