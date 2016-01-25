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

{$I Concepts.inc}

unit Concepts.Spring.Logging.Form;

{ Demonstrates the Spring Logging system.
    - shows an example of a custom ILogAppender implementation (TLogTreeAppender).
    - how to log RTTI info using a TReflectionTypeSerializer
    - properties can be set on the controller, logger and appenders.

  Remark: Stack trace support is not supported yet by Spring4D. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ImgList, Vcl.Tabs,

  Spring.Logging, Spring.Logging.Controller, Spring.Logging.Extensions,
  Spring.Logging.Appenders,

  DDuce.Logging.Appenders.WinIPC, DDuce.Logging.Appenders.LogTree,

  DDuce.Components.PropertyInspector, DDuce.Components.Logtree;

type
  TfrmSpringLogging = class(TForm)
    aclMain            : TActionList;
    actLog             : TAction;
    actTrackThis       : TAction;
    actWarn            : TAction;
    actLogValue        : TAction;
    pnlLeft            : TPanel;
    pnlRight           : TPanel;
    grpLogEntry        : TGroupBox;
    lbl1               : TLabel;
    lblData            : TLabel;
    rgpLogEntryTypes   : TRadioGroup;
    rgpLogLevel        : TRadioGroup;
    edtMessage         : TEdit;
    edtData            : TEdit;
    pnlLoggerInspector : TPanel;
    spl1               : TSplitter;
    tsMain             : TTabSet;
    imlMain            : TImageList;
    btnTrackThis       : TButton;
    grpAppenders       : TGroupBox;
    chkTraceLogEnabled : TCheckBox;
    chkFileLogEnabled  : TCheckBox;
    chkLogTreeEnabled  : TCheckBox;
    chkWinIPCEnabled   : TCheckBox;
    btnLog             : TButton;
    edtLogFile         : TEdit;
    imlLogLevels: TImageList;

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

  DDuce.Reflect, DDuce.Components.Factories,

  Concepts.Factories, Concepts.Resources;

type
  TLogLevel = Spring.Logging.TLogLevel;

{$REGION 'construction and destruction'}
procedure TfrmSpringLogging.AfterConstruction;
var
  FLA : TFileLogAppender;
  LP  : ILoggerProperties;
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
  LP.EntryTypes := LOG_ALL_ENTRY_TYPES;

  // All appenders

  LP := FFileLogAppender as ILoggerProperties;
  LP.EntryTypes := LOG_ALL_ENTRY_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;

  LP := FTraceLogAppender as ILoggerProperties;
  LP.EntryTypes := LOG_ALL_ENTRY_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;

  LP := FLogTreeAppender as ILoggerProperties;
  LP.EntryTypes := LOG_ALL_ENTRY_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;

  LP := FWinIPCAppender as ILoggerProperties;
  LP.EntryTypes := LOG_ALL_ENTRY_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;

  FController.AddAppender(FFileLogAppender);
  FController.AddAppender(FTraceLogAppender);
  FController.AddAppender(FLogTreeAppender);
  FController.AddAppender(FWinIPCAppender);
  (FController as ISerializerController).AddSerializer(TReflectionTypeSerializer.Create);
  (FController as ISerializerController).AddSerializer(TSimpleTypeSerializer.Create);
  (FController as ISerializerController).AddSerializer(TInterfaceSerializer.Create);
  (FController as ISerializerController).AddSerializer(TArrayOfValueSerializer.Create);
  FLogger := TLogger.Create(FController);
  LP := (FLogger as ILoggerProperties);
  LP.EntryTypes := LOG_ALL_ENTRY_TYPES;
  LP.Levels     := LOG_ALL_LEVELS;

  FPropertyInspector := TDDuceComponents.CreatePropertyInspector(Self, pnlLoggerInspector, FLogger as TObject);

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
  FLogger.LogValue('Form', TS);
  TS.Free;
end;

procedure TfrmSpringLogging.actTrackThisExecute(Sender: TObject);
begin
  FLogger.Track(Self.ClassType, 'Test');
end;

procedure TfrmSpringLogging.actLogExecute(Sender: TObject);
begin
  FLogger.Log(
    TLogEntry.Create(
      TLogLevel(rgpLogLevel.ItemIndex),
      TLogEntryType(rgpLogEntryTypes.ItemIndex),
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
    FPropertyInspector.Objects[0] :=  FLogger as TLogger
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
  LET : TLogEntryType;
  LL  : TLogLevel;
begin
  for LET :=  Low(TLogEntryType) to High(TLogEntryType) do
  begin
    rgpLogEntryTypes.Items.Add(Reflect.EnumName(LET));
  end;
  rgpLogEntryTypes.ItemIndex := Integer(TLogEntryType.Text);

  for LL := Low(TLogLevel) to High(TLogLevel) do
  begin
    rgpLogLevel.Items.Add(Reflect.EnumName(LL));
  end;
  rgpLogLevel.ItemIndex := Integer(TLogLevel.Trace);
end;
{$ENDREGION}

end.
