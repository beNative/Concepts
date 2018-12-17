{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.MQTT.Form;

interface

{$REGION 'documentation'}
{ The MQTT protocol is based on the principle of publishing messages and
  subscribing to topics, or "pub/sub". Multiple clients connect to a broker
  and subscribe to topics that they are interested in. Clients also connect to
  the broker and publish messages to topics. Many clients may subscribe to the
  same topics and do with the information as they please. The broker and MQTT
  act as a simple, common interface for everything to connect to.
  See https://mosquitto.org/ for more info.
}
{$ENDREGION}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst, Vcl.ComCtrls, Vcl.ToolWin,

  MQTT,

  Spring.SystemUtils,

  VirtualTrees,

  DDuce.Components.LogTree, System.ImageList, Vcl.ImgList;

type
  TfrmMQTTNode = class(TForm)
    aclMain                    : TActionList;
    actAddSubscription         : TAction;
    actClearReceived           : TAction;
    actClearSubscriptions      : TAction;
    actClose                   : TAction;
    actConnect                 : TAction;
    actCreateNew               : TAction;
    actCreateNewPUBNode        : TAction;
    actCreateNewSUBNode        : TAction;
    actCreateNewSubscriberNode : TAction;
    actCreateNewWithNewContext : TAction;
    actDeleteSubscription      : TAction;
    actDisconnect              : TAction;
    actPopulateMemo            : TAction;
    actPublish                 : TAction;
    btnAddSubscription         : TToolButton;
    btnClearReceived           : TButton;
    btnClearSubscriptions      : TToolButton;
    btnConnectToBroker         : TButton;
    btnDeleteSubscription      : TToolButton;
    btnDisconnect              : TButton;
    btnPopulateMemo            : TButton;
    btnSend                    : TButton;
    edtAddress                 : TLabeledEdit;
    edtCounter                 : TLabeledEdit;
    edtPollTimeout             : TLabeledEdit;
    edtPort                    : TLabeledEdit;
    edtPublishTopic            : TLabeledEdit;
    edtTopic                   : TLabeledEdit;
    grpEndPoint                : TGroupBox;
    grpMonitorEvents           : TGroupBox;
    grpPollingSettings         : TGroupBox;
    grpSubscriptions           : TGroupBox;
    imlMain                    : TImageList;
    lbxEvents                  : TCheckListBox;
    lbxTopics                  : TListBox;
    mmoPublish                 : TMemo;
    mmoReceive                 : TMemo;
    pgcMessage                 : TPageControl;
    pnlLogging                 : TPanel;
    tlbSubscriptions           : TToolBar;
    tsEndpoints                : TTabSheet;
    tsPublish                  : TTabSheet;
    tsReceive                  : TTabSheet;
    tsSettings                 : TTabSheet;
    tsSubscriptions            : TTabSheet;

    procedure actPublishExecute(Sender: TObject);
    procedure actSubscribeExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actClearSubscriptionsExecute(Sender: TObject);
    procedure actDeleteSubscriptionExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actAddSubscriptionExecute(Sender: TObject);
    procedure actPopulateMemoExecute(Sender: TObject);
    procedure actClearReceivedExecute(Sender: TObject);

  private
    FMQTT    : TMQTT;
    FLogTree : TLogTree;

    { Called when a message is received by a subscriber. }
    procedure FMQTTOnPublish(
      Sender  : TObject;
      Topic   : UTF8String;
      Payload : UTF8String
    );
    function GetTopics: TStrings;

  protected
    procedure UpdateActions; override;

    function GenerateRandomData(ALineCount: Integer): string;

    property Topics: TStrings
      read GetTopics;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.StrUtils,

  DDuce.Reflect, DDuce.Logger, DDuce.Components.Factories, DDuce.RandomData,
  DDuce.Utils,

  Spring;

{$REGION 'construction and destruction'}
procedure TfrmMQTTNode.AfterConstruction;
begin
  inherited AfterConstruction;
  FLogTree := TDDuceComponents.CreateLogTree(Self, pnlLogging);
  FLogTree.DateTimeFormat     := 'hh:nn:ss.zzz';
  FLogTree.ShowDateColumn     := False;
  FLogTree.AutoLogLevelColors := True;
end;

procedure TfrmMQTTNode.BeforeDestruction;
begin
  FMQTT.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
{ Called on the subscriber. }

procedure TfrmMQTTNode.FMQTTOnPublish(Sender: TObject; Topic,
  Payload: UTF8String);
begin
  mmoReceive.Text := string(Payload);
  FLogTree.LogFmt('Received for topic %s.', [Topic]);
end;

function TfrmMQTTNode.GetTopics: TStrings;
begin
  Result := lbxTopics.Items;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMQTTNode.actAddSubscriptionExecute(Sender: TObject);
begin
  Topics.Add(edtTopic.Text);
  FMQTT.Subscribe(UTF8String(edtTopic.Text), 0);
end;

procedure TfrmMQTTNode.actClearReceivedExecute(Sender: TObject);
begin
  mmoReceive.Clear;
end;

procedure TfrmMQTTNode.actClearSubscriptionsExecute(Sender: TObject);
var
  S : string;
begin
  for S in Topics do
  begin
    FMQTT.Unsubscribe(UTF8String(S));
  end;
end;

procedure TfrmMQTTNode.actConnectExecute(Sender: TObject);
begin
  if Assigned(FMQTT) then
  begin
    FreeAndNil(FMQTT);
  end;
  FMQTT := TMQTT.Create(UTF8String(edtAddress.Text), StrToInt(edtPort.Text));
  FMQTT.OnPublish := FMQTTOnPublish;
  if FMQTT.Connect then
  begin
    FLogTree.Log('Socket connected to broker.', llInfo);
    FLogTree.Header.AutoFitColumns;
  end;
end;

procedure TfrmMQTTNode.actDeleteSubscriptionExecute(Sender: TObject);
begin
  FMQTT.Unsubscribe(UTF8String(lbxTopics.Items[lbxTopics.ItemIndex]));
  Topics.Delete(lbxTopics.ItemIndex);
end;

procedure TfrmMQTTNode.actDisconnectExecute(Sender: TObject);
begin
  if FMQTT.Disconnect then
  begin
    FLogTree.Log('Socket Disconnected.', llInfo);
    FLogTree.Header.AutoFitColumns;
  end;
end;

procedure TfrmMQTTNode.actPopulateMemoExecute(Sender: TObject);
begin
  mmoPublish.Text := GenerateRandomData(1000);
end;

procedure TfrmMQTTNode.actPublishExecute(Sender: TObject);
begin
  if FMQTT.Publish(UTF8String(edtPublishTopic.Text), UTF8String(mmoPublish.Text)) then
  begin
    FLogTree.Log('Published', llInfo);
  end;
end;

procedure TfrmMQTTNode.actSubscribeExecute(Sender: TObject);
begin
  FMQTT.Subscribe('Test', 1);
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TfrmMQTTNode.GenerateRandomData(ALineCount: Integer): string;
var
  I  : Integer;
  SL : IShared<TStringList>;
begin
  SL := Shared<TStringList>.New;
  for I := 0 to ALineCount - 1 do
  begin
    SL.Add(RandomData.FullName);
  end;
  Result := SL.Text;
end;

procedure TfrmMQTTNode.UpdateActions;
var
  B : Boolean;
begin
  B := Assigned(FMQTT);
  actConnect.Enabled := not B or not FMQTT.isConnected;
  actDisconnect.Enabled := not actConnect.Enabled;
  inherited UpdateActions;
end;
{$ENDREGION}

end.
