{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I Concepts.inc}

unit Concepts.Spring.Logging.Form;

{ Demonstrates the Spring Logging system.

  Remark: Stack trace support is not supported yet by Spring4D. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  Spring.Logging, Spring.Logging.Controller, Spring.Logging.Extensions,
  Spring.Logging.Appenders;

type
  TfrmSpringLogging = class(TForm)
    aclMain            : TActionList;
    actTrackThis       : TAction;
    btnTrackThis       : TButton;
    actWarn            : TAction;
    actLog             : TAction;
    grpLogEntry        : TGroupBox;
    rgpLogEntryTypes   : TRadioGroup;
    rgpLogLevel        : TRadioGroup;
    edtMessage         : TEdit;
    lbl1               : TLabel;
    edtData            : TEdit;
    lblData            : TLabel;
    btnLog             : TButton;
    mmoLog             : TMemo;
    chkTraceLogEnabled : TCheckBox;
    chkFileLogEnabled  : TCheckBox;
    chkMemoLogEnabled  : TCheckBox;

    procedure actTrackThisExecute(Sender: TObject);
    procedure actLogExecute(Sender: TObject);

  private
    FController : ILoggerController;
    FLogger     : ILogger;
    FSerializer : ITypeSerializer;

    FFileLogAppender  : ILogAppender;
    FTraceLogAppender : ILogAppender;
    FMemoLogAppender  : ILogAppender;

  protected
    procedure InitializeLogEntryTypes;
    procedure InitializeLogLevels;
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;

    procedure Test; virtual;

  end;

  TMemoLogAppender = class(TStreamLogAppender, ILogAppender)
  private
    FMemo         : TMemo;
    FStringStream : TStringStream;

  public
    constructor Create(AMemo: TMemo);

    procedure Send(const entry: TLogEntry);
  end;

implementation

{$R *.dfm}

uses
  Spring.Logging.Loggers, Spring.Logging.Serializers,

  DDuce.Reflect,

  Concepts.Resources;

{$REGION 'TMemoLogAppender '}
procedure TMemoLogAppender.Send(const entry: TLogEntry);
begin

  DoSend(entry);
  FMemo.Lines.Text := FStringStream.DataString;
  FMemo.SelStart := FMemo.Lines.Text.Length - 1;
  FMemo.SelLength := 1;
end;

constructor TMemoLogAppender.Create(AMemo: TMemo);
begin
  FMemo := AMemo;
  FStringStream := TStringStream.Create;
  inherited Create(FStringStream);

end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmSpringLogging.AfterConstruction;
var
  FLA : TFileLogAppender;
  LP : ILoggerProperties;
begin
  inherited AfterConstruction;
  FController := TLoggerController.Create;
  FSerializer := TReflectionTypeSerializer.Create;
  // create
  FLA  := TFileLogAppender.Create;
  FLA.FileName := 'SpringLog.log';
  FFileLogAppender := FLA;
  FTraceLogAppender := TTraceLogAppender.Create;
  FMemoLogAppender  := TMemoLogAppender.Create(mmoLog);
  // setup
  LP := FController as ILoggerProperties;
  LP.Levels := LOG_ALL_LEVELS;
  LP.EntryTypes := LOG_ALL_ENTRY_TYPES;
  LP := FFileLogAppender as ILoggerProperties;
  LP.EntryTypes := LOG_ALL_ENTRY_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;
  LP := FTraceLogAppender as ILoggerProperties;
  LP.EntryTypes := LOG_ALL_ENTRY_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;
  LP := FMemoLogAppender as ILoggerProperties;
  LP.EntryTypes := LOG_ALL_ENTRY_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;
  FLogger := TLogger.Create(FController);
  LP := (FLogger as ILoggerProperties);
  LP.EntryTypes := LOG_ALL_ENTRY_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;
  FController.AddAppender(FFileLogAppender);
  FController.AddAppender(FTraceLogAppender);
  FController.AddAppender(FMemoLogAppender);
  (FController as ISerializerController).AddSerializer(FSerializer);
  InitializeLogEntryTypes;
  InitializeLogLevels;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmSpringLogging.actTrackThisExecute(Sender: TObject);
begin
  FLogger.Track(Self.ClassType, 'Test');
end;

procedure TfrmSpringLogging.actLogExecute(Sender: TObject);
begin
  FController.Send(
    TLogEntry.Create(
      TLogLevel(rgpLogLevel.ItemIndex),
      TLogEntryType(rgpLogEntryTypes.ItemIndex),
      edtMessage.Text
    )
  );
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmSpringLogging.Test;
begin
  ShowMessage('Beware, the invocation of this message might be tracked!');
end;

procedure TfrmSpringLogging.UpdateActions;
begin
  inherited UpdateActions;
  (FFileLogAppender as ILoggerProperties).Enabled  := chkFileLogEnabled.Checked;
  (FTraceLogAppender as ILoggerProperties).Enabled := chkTraceLogEnabled.Checked;
  (FMemoLogAppender as ILoggerProperties).Enabled  := chkMemoLogEnabled.Checked;
end;

procedure TfrmSpringLogging.InitializeLogEntryTypes;
var
  LET : TLogEntryType;
begin
  for LET :=  Low(TLogEntryType) to High(TLogEntryType) do
  begin
    rgpLogEntryTypes.Items.Add(Reflect.EnumName(LET));
  end;
  rgpLogEntryTypes.ItemIndex := Integer(TLogEntryType.Text);
end;

procedure TfrmSpringLogging.InitializeLogLevels;
var
  LL : TLogLevel;
begin
  for LL := Low(TLogLevel) to High(TLogLevel) do
  begin
    rgpLogLevel.Items.Add(Reflect.EnumName(LL));
  end;
  rgpLogLevel.ItemIndex := Integer(TLogLevel.Trace);
end;
{$ENDREGION}

end.
