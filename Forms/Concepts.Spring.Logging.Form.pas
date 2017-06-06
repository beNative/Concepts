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

{$I Concepts.inc}

unit Concepts.Spring.Logging.Form;

{ Demonstrates the Spring Logging system.
  Shows an example of
    - a custom ILogAppender implementation (TLogTreeAppender).
    - how to log RTTI info using a TReflectionTypeSerializer
    - how properties can be set on the controller, logger and appenders.

  Remark: Stack trace support is not supported yet by Spring4D. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.ImageList,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Tabs, Vcl.ImgList,

  Spring.Logging, Spring.Logging.Controller, Spring.Logging.Extensions,
  Spring.Logging.Appenders,

  DDuce.Logging.Appenders.WinIPC, DDuce.Logging.Appenders.LogTree,

  DDuce.Components.PropertyInspector, DDuce.Components.Logtree;

type
  TfrmSpringLogging = class(TForm)
    aclMain            : TActionList;
    actLog             : TAction;
    actLogValue        : TAction;
    actTrackThis       : TAction;
    actWarn            : TAction;
    btnLog             : TButton;
    btnTrackThis       : TButton;
    chkFileLogEnabled  : TCheckBox;
    chkLogTreeEnabled  : TCheckBox;
    chkTraceLogEnabled : TCheckBox;
    chkWinIPCEnabled   : TCheckBox;
    edtData            : TEdit;
    edtLogFile         : TEdit;
    edtMessage         : TEdit;
    grpAppenders       : TGroupBox;
    grpLogEntry        : TGroupBox;
    imlLogLevels       : TImageList;
    imlMain            : TImageList;
    lblMessage         : TLabel;
    lblData            : TLabel;
    pnlLeft            : TPanel;
    pnlLoggerInspector : TPanel;
    pnlRight           : TPanel;
    rgpLogEntryTypes   : TRadioGroup;
    rgpLogLevel        : TRadioGroup;
    splVertical        : TSplitter;
    tsMain             : TTabSet;

    procedure actTrackThisExecute(Sender: TObject);
    procedure actLogExecute(Sender: TObject);
    procedure actLogValueExecute(Sender: TObject);

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tsMainChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure edtLogFileChange(Sender: TObject);

  private
    FController        : ILoggerController;
    FLogger            : ILogger;
    FPropertyInspector : TPropertyInspector;
    FLogTree           : TLogTree;

    FFileLogAppender  : ILogAppender;
    FTraceLogAppender : ILogAppender;
    FLogTreeAppender  : ILogAppender;
    FWinIPCAppender   : ILogAppender;

  protected
    procedure InitializeControls;

    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.Rtti,

  Spring.Logging.Loggers, Spring.Logging.Serializers,

  DDuce.Reflect, DDuce.Components.Factories;

type
  TLogLevel = Spring.Logging.TLogLevel;

{$REGION 'construction and destruction'}
procedure TfrmSpringLogging.AfterConstruction;
var
  FLA : TFileLogAppender;
  LP  : ILoggerProperties;
  SC  : ISerializerController;
begin
  inherited AfterConstruction;

  FLogTree := TDDuceComponents.CreateLogTree(Self, pnlRight);
  FLogTree.Images := imlMain;

  FController := TLoggerController.Create;

  // create
  FLA  := TFileLogAppender.Create;
  FLA.FileName := edtLogFile.Text;
  FFileLogAppender  := FLA;
  FTraceLogAppender := TTraceLogAppender.Create;
  FLogTreeAppender  := TLogTreeAppender.Create(FLogTree);
  FWinIPCAppender   := TWinIPCAppender.Create;
  // setup
  LP := FController as ILoggerProperties;
  LP.Levels := LOG_ALL_LEVELS;
  LP.EventTypes := LOG_ALL_EVENT_TYPES;

  // All appenders

  LP := FFileLogAppender as ILoggerProperties;
  LP.EventTypes := LOG_ALL_EVENT_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;

  LP := FTraceLogAppender as ILoggerProperties;
  LP.EventTypes := LOG_ALL_EVENT_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;

  LP := FLogTreeAppender as ILoggerProperties;
  LP.EventTypes := LOG_ALL_EVENT_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;

  LP := FWinIPCAppender as ILoggerProperties;
  LP.EventTypes := LOG_ALL_EVENT_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;

  FController.AddAppender(FFileLogAppender);
  FController.AddAppender(FTraceLogAppender);
  FController.AddAppender(FLogTreeAppender);
  FController.AddAppender(FWinIPCAppender);

  SC := FController as ISerializerController;
//  SC.AddSerializer(TReflectionTypeSerializer.Create);
//  SC.AddSerializer(TSimpleTypeSerializer.Create);
//  SC.AddSerializer(TInterfaceSerializer.Create);
//  SC.AddSerializer(TArrayOfValueSerializer.Create);

  FLogger := TLogger.Create(FController);

  LP := FLogger as ILoggerProperties;
  LP.EventTypes := LOG_ALL_EVENT_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;

  FPropertyInspector := TDDuceComponents.CreatePropertyInspector(
    Self,
    pnlLoggerInspector,
    FLogger as TObject
  );

  InitializeControls;
end;

procedure TfrmSpringLogging.edtLogFileChange(Sender: TObject);
var
  FLA : TFileLogAppender;
begin
  chkFileLogEnabled.Checked := False;
  FFileLogAppender := nil;
  FLA  := TFileLogAppender.Create;
  FLA.FileName := edtLogFile.Text;
  FFileLogAppender  := FLA;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmSpringLogging.actLogValueExecute(Sender: TObject);
var
  TS : TStrings;
begin
  TS := TStringList.Create;
  try
    FLogger.LogValue('Form', TS);
  finally
    TS.Free;
  end;
end;

{ Track is the equivalent of an Enter/Leave pair at the beginning/ending of a
  routine or method. }

procedure TfrmSpringLogging.actTrackThisExecute(Sender: TObject);
begin
  FLogger.Track(Self, 'actTrackThisExecute');
end;

procedure TfrmSpringLogging.actLogExecute(Sender: TObject);
begin
  FLogger.Log(
    TLogEvent.Create(
      TLogLevel(rgpLogLevel.ItemIndex),
      TLogEventType(rgpLogEntryTypes.ItemIndex),
      edtMessage.Text
    )
  );
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmSpringLogging.tsMainChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  if NewTab = 0 then
    FPropertyInspector.Objects[0] := FLogger as TLogger
  else
    FPropertyInspector.Objects[0] := FLogTree;
end;

procedure TfrmSpringLogging.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmSpringLogging.UpdateActions;
begin
  inherited UpdateActions;
  (FFileLogAppender as ILoggerProperties).Enabled  := chkFileLogEnabled.Checked;
  (FTraceLogAppender as ILoggerProperties).Enabled := chkTraceLogEnabled.Checked;
  (FLogTreeAppender as ILoggerProperties).Enabled  := chkLogTreeEnabled.Checked;
  (FWinIPCAppender as ILoggerProperties).Enabled   := chkWinIPCEnabled.Checked;
end;

procedure TfrmSpringLogging.InitializeControls;
var
  LET : TLogEventType;
  LL  : TLogLevel;
begin
  for LET :=  Low(TLogEventType) to High(TLogEventType) do
  begin
    rgpLogEntryTypes.Items.Add(Reflect.EnumName(LET));
  end;
  rgpLogEntryTypes.ItemIndex := Integer(TLogEventType.Text);

  for LL := Low(TLogLevel) to High(TLogLevel) do
  begin
    rgpLogLevel.Items.Add(Reflect.EnumName(LL));
  end;
  rgpLogLevel.ItemIndex := Integer(TLogLevel.Trace);
end;
{$ENDREGION}

end.
