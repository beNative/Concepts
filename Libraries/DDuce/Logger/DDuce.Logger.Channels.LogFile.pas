{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Logger.Channels.LogFile;

interface

uses
  System.Classes, System.SysUtils,

  Spring,

  DDuce.Logger.Interfaces, DDuce.Logger.Channels.Base;

type
  TLogFileChannel = class(TCustomLogChannel, ILogChannel, ILogFileChannel)
  private
    FRelativeIndent : Integer;
    FBaseIndent     : Integer;
    FShowHeader     : Boolean;
    FShowDate       : Boolean;
    FShowTime       : Boolean;
    FShowPrefix     : Boolean;
    FShowStrings    : Boolean;
    FStreamWriter   : Lazy<TStreamWriter>;
    FFileName       : string;
    FFileStream     : Lazy<TFileStream>;

    function Space(ACount: Integer): string;
    procedure UpdateIndentation;
    procedure WriteStrings(AStream: TStream);
    procedure WriteComponent(AStream: TStream);

  protected
    {$REGION 'property access methods'}
    procedure SetShowTime(const AValue: Boolean);
    procedure SetShowDate(const Value: Boolean);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetShowDate: Boolean;
    function GetShowHeader: Boolean;
    function GetShowPrefix: Boolean;
    function GetShowTime: Boolean;
    procedure SetShowHeader(const Value: Boolean);
    procedure SetShowPrefix(const Value: Boolean);
    {$ENDREGION}

  public
    constructor Create(const AFileName: string = ''); reintroduce; virtual;

    procedure BeforeDestruction; override;

    function Write(const AMsg: TLogMessage): Boolean; override;

    property ShowHeader: Boolean
      read GetShowHeader write SetShowHeader default False;

    property ShowPrefix: Boolean
      read GetShowPrefix write SetShowPrefix default True;

    property ShowTime: Boolean
      read GetShowTime write SetShowTime default True;

    property ShowDate: Boolean
      read GetShowDate write SetShowDate default False;

    property FileName: string
      read GetFileName write SetFileName;
  end;

implementation

uses
  System.StrUtils,
  Vcl.Forms;

{$REGION 'construction and destruction'}
constructor TLogFileChannel.Create(const AFileName: string);
begin
  inherited Create;
  if AFileName = '' then
  begin
    FFileName := ExtractFilePath(Application.ExeName)
      + FormatDateTime('yyyymmdd hhnnss ', Now)
      + ExtractFileName(ChangeFileExt(Application.ExeName, '.log'));
  end
  else
    FFileName := AFileName;
  FShowPrefix   := True;
  FShowDate     := False;
  FShowTime     := True;
  FShowStrings  := True;
  FShowHeader   := False;
  Enabled       := False;
  FFileStream.Create(function: TFileStream
    begin
      if not FileExists(FFileName) then
        Result := TFileStream.Create(FFileName, fmCreate or fmShareDenyNone)
      else
        Result := TFileStream.Create(FFileName, fmOpenWrite or fmShareDenyNone);
    end
  );

  FStreamWriter.Create(function: TStreamWriter
    begin
      Result := TStreamWriter.Create(FFileStream);
      if FShowHeader then
      begin
        Result.WriteLine(
          '============|Log Session Started at ' +
          DateTimeToStr(Now) +
          ' by ' +
          Application.Title +
          '|============'
        );
      end;
      UpdateIndentation;
    end
  )
end;

procedure TLogFileChannel.BeforeDestruction;
begin
  //FFileStream.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogFileChannel.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TLogFileChannel.SetFileName(const Value: string);
begin
  if Value <> FileName then
  begin
    FFileName := Value;
  end;
end;

function TLogFileChannel.GetShowDate: Boolean;
begin
  Result := FShowDate;
end;

procedure TLogFileChannel.SetShowDate(const Value: Boolean);
begin
  FShowDate := Value;
  UpdateIndentation;
end;

function TLogFileChannel.GetShowHeader: Boolean;
begin
  Result := FShowHeader;
end;

procedure TLogFileChannel.SetShowHeader(const Value: Boolean);
begin
  FShowHeader := Value;
end;

function TLogFileChannel.GetShowPrefix: Boolean;
begin
  Result := FShowPrefix;
end;

procedure TLogFileChannel.SetShowPrefix(const Value: Boolean);
begin
  FShowPrefix := Value;
end;

function TLogFileChannel.GetShowTime: Boolean;
begin
  Result := FShowTime;
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
        FStreamWriter.Value.WriteLine(
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
  ObjectBinaryToText(AStream, FStreamWriter.Value.BaseStream);
end;
{$ENDREGION}

{$REGION 'public methods'}
function TLogFileChannel.Write(const AMsg: TLogMessage): Boolean;
begin
  if (AMsg.MsgType = Integer(lmtLeaveMethod)) and (FRelativeIndent >= 2) then
    Dec(FRelativeIndent, 2);
  if ShowDate then
    FStreamWriter.Value.Write(FormatDateTime('yyyy-mm-dd', AMsg.TimeStamp) + ' ');
  if ShowTime then
    FStreamWriter.Value.Write(FormatDateTime('hh:nn:ss:zzz', AMsg.TimeStamp) + ' ');
  FStreamWriter.Value.Write(Space(FRelativeIndent));
  if ShowPrefix then
    FStreamWriter.Value.Write(LOG_PREFIXES[TLogMessageType(AMsg.MsgType)] + ': ');
  FStreamWriter.Value.WriteLine(string(AMsg.Text));
  if FShowStrings and (AMsg.Data <> nil) then
  begin
    case TLogMessageType(AMsg.MsgType) of
      lmtStrings, lmtCallStack, lmtHeapInfo, lmtException:
         WriteStrings(AMsg.Data);
      lmtComponent:
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
