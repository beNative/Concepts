{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

  MQTT subscription syntax

  In MQTT, the word topic refers to an UTF-8 string that the broker uses to
  filter messages for each connected client. The topic consists of one or
  more topic levels. Each topic level is separated by a forward slash (topic
  level separator).

  Symbols used in topics
    / topic level seperator
    + can be used as a single level wildcard
    # can be used as a multi-level wildcard (can only be specified at the end).

  Best practices
    - Never use a leading forward slash
    - Never use spaces in a topic
    - Use only ASCII characters, avoid non printable characters
    - Embed a unique identifier or the Client Id into the topic
}
{$ENDREGION}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.ImageList,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ImgList, Vcl.Mask,

  MQTT,

  VirtualTrees,

  DDuce.Components.LogTree;

type
  TfrmMQTTNode = class(TForm)
    {$REGION 'designer conntrols'}
    aclMain                    : TActionList;
    actAddSubscription         : TAction;
    actClearMessage            : TAction;
    actClearReceived           : TAction;
    actClearSubscriptions      : TAction;
    actClose                   : TAction;
    actConnect                 : TAction;
    actCreateNew               : TAction;
    actCreateNewWithNewContext : TAction;
    actDeleteSubscription      : TAction;
    actDisconnect              : TAction;
    actPopulateMessage         : TAction;
    actPublishMessage          : TAction;
    actStartBroker             : TAction;
    actSubscribeToAll          : TAction;
    btnAddSubscription         : TButton;
    btnClearPublishList        : TButton;
    btnClearReceived           : TButton;
    btnClearSubscriptions      : TButton;
    btnConnectToBroker         : TButton;
    btnDeleteSubscription      : TButton;
    btnDisconnect              : TButton;
    btnPopulateMemo            : TButton;
    btnSend                    : TButton;
    btnStartBroker             : TButton;
    chkAutoConnect             : TCheckBox;
    chkSubscribeToAllTopics    : TCheckBox;
    edtAddress                 : TLabeledEdit;
    edtCounter                 : TLabeledEdit;
    edtPort                    : TLabeledEdit;
    edtPublishTopic            : TLabeledEdit;
    edtTopic                   : TLabeledEdit;
    imlMain                    : TImageList;
    lbxTopics                  : TListBox;
    mmoPublish                 : TMemo;
    mmoReceive                 : TMemo;
    pgcMessage                 : TPageControl;
    pnlLogging                 : TPanel;
    tsEndpoints                : TTabSheet;
    tsPublish                  : TTabSheet;
    tsReceive                  : TTabSheet;
    tsSubscriptions            : TTabSheet;
    {$ENDREGION}

    procedure actPublishMessageExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actClearSubscriptionsExecute(Sender: TObject);
    procedure actDeleteSubscriptionExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actAddSubscriptionExecute(Sender: TObject);
    procedure actPopulateMessageExecute(Sender: TObject);
    procedure actClearMessageExecute(Sender: TObject);
    procedure actClearReceivedExecute(Sender: TObject);
    procedure actStartBrokerExecute(Sender: TObject);

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chkSubscribeToAllTopicsClick(Sender: TObject);

  private
    FMQTT    : TMQTT;
    FLogTree : TLogTree;

    { Called when a message is received by a subscriber. }
    procedure FMQTTOnPublish(
      Sender  : TObject;
      Topic   : UTF8String;
      Payload : UTF8String
    );

    {$REGION 'property access methods'}
    function GetTopics: TStrings;
    function GetConnected: Boolean;
    procedure SubscribeToAllTopics;
    {$ENDREGION}

  protected
    procedure Connect;
    procedure UpdateActions; override;

    function GenerateRandomData(ALineCount: Integer): string;
    procedure LoadSettings;
    procedure SaveSettings;

    property Topics: TStrings
      read GetTopics;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property Connected: Boolean
      read GetConnected;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Components.Factories, DDuce.RandomData, DDuce.Utils.WinApi,

  Spring,

  Concepts.Settings;

{$REGION 'construction and destruction'}
procedure TfrmMQTTNode.AfterConstruction;
begin
  inherited AfterConstruction;
  FLogTree := TDDuceComponents.CreateLogTree(Self, pnlLogging);
  FLogTree.DateTimeFormat     := 'hh:nn:ss.zzz';
  FLogTree.ShowDateColumn     := False;
  FLogTree.AutoLogLevelColors := True;
  LoadSettings;
  pgcMessage.ActivePageIndex  := 0;
end;

destructor TfrmMQTTNode.Destroy;
begin
  if Assigned(FMQTT) then
  begin
    FMQTT.Disconnect;
    FMQTT.Free;
  end;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMQTTNode.chkSubscribeToAllTopicsClick(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
  begin
    SubscribeToAllTopics;
  end;
end;

{ Called on the subscriber. }

procedure TfrmMQTTNode.FMQTTOnPublish(Sender: TObject; Topic,
  Payload: UTF8String);
begin
  mmoReceive.Text := string(Payload);
  FLogTree.LogFmt('Received for topic %s.', [Topic]);
end;

procedure TfrmMQTTNode.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
  Action := caFree;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMQTTNode.actAddSubscriptionExecute(Sender: TObject);
begin
  if (edtTopic.Text <> '') and (Topics.IndexOf(edtTopic.Text) = -1) then
  begin
    Topics.Add(edtTopic.Text);
    FMQTT.Subscribe(UTF8String(edtTopic.Text), 0);
  end;
end;

procedure TfrmMQTTNode.actClearMessageExecute(Sender: TObject);
begin
  mmoPublish.Clear;
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
  Topics.Clear;
end;

procedure TfrmMQTTNode.actConnectExecute(Sender: TObject);
begin
  Connect;
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

procedure TfrmMQTTNode.actPopulateMessageExecute(Sender: TObject);
begin
  mmoPublish.Text := GenerateRandomData(50);
end;

procedure TfrmMQTTNode.actPublishMessageExecute(Sender: TObject);
begin
  if FMQTT.Publish(UTF8String(edtPublishTopic.Text),UTF8String(mmoPublish.Text)) then
  begin
    FLogTree.Log('Published', llInfo);
  end;
end;

procedure TfrmMQTTNode.actStartBrokerExecute(Sender: TObject);
const
  MOSQUITTO_FILENAME = 'mosquitto.exe';
begin
  if FileExists(MOSQUITTO_FILENAME) then
  begin
    RunApplication('', MOSQUITTO_FILENAME, False);
    ShowMessage('Broker started on localhost:1883.');
  end
  else
  begin
    ShowMessageFmt('''%s'' not found.', [MOSQUITTO_FILENAME]);
  end;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmMQTTNode.GetConnected: Boolean;
begin
  Result := Assigned(FMQTT) and FMQTT.Connected;
end;

function TfrmMQTTNode.GetTopics: TStrings;
begin
  Result := lbxTopics.Items;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMQTTNode.Connect;
begin
  if Assigned(FMQTT) then
  begin
    FreeAndNil(FMQTT);
  end;
  FMQTT := TMQTT.Create(edtAddress.Text, StrToInt(edtPort.Text));
  FMQTT.OnPublish := FMQTTOnPublish;
  FMQTT.WillTopic := 'a'; // required by some brokers
  FMQTT.WillMsg := 'a';   // required by some brokers
  if FMQTT.Connect then
  begin
    FLogTree.Log('Socket connected to broker.', llInfo);
    FLogTree.Header.AutoFitColumns;
    if chkSubscribeToAllTopics.Checked then
      SubscribeToAllTopics;
  end;
end;

procedure TfrmMQTTNode.LoadSettings;
begin
  edtAddress.Text := Settings.ReadString(UnitName, 'BrokerAddress', 'localhost');
  edtPort.Text := Settings.ReadInteger(UnitName, 'BrokerPort', 1883).ToString;
  edtPublishTopic.Text := Settings.ReadString(UnitName, 'PublishTopic');
  chkAutoConnect.Checked := Settings.ReadBool(UnitName, 'AutoConnect', False);
  chkSubscribeToAllTopics.Checked :=
    Settings.ReadBool(UnitName, 'SubscribeToAllTopics', False);
end;

procedure TfrmMQTTNode.SaveSettings;
begin
  Settings.WriteString(UnitName, 'BrokerAddress', edtAddress.Text);
  Settings.WriteString(UnitName, 'PublishTopic', edtPublishTopic.Text);
  Settings.WriteInteger(UnitName, 'BrokerPort', StrToIntDef(edtPort.Text, 1883));
  Settings.WriteBool(UnitName, 'AutoConnect', chkAutoConnect.Checked);
  Settings.WriteBool(UnitName, 'SubscribeToAllTopics', chkSubscribeToAllTopics.Checked);
end;

function TfrmMQTTNode.GenerateRandomData(ALineCount: Integer): string;
var
  I  : Integer;
  SL : IShared<TStringList>;
begin
  SL := Shared<TStringList>.Make;
  for I := 0 to ALineCount - 1 do
  begin
    SL.Add(RandomData.FullName);
  end;
  Result := SL.Text;
end;

procedure TfrmMQTTNode.SubscribeToAllTopics;
begin
  Topics.Clear;
  Topics.Add('#');
  if Connected then
    FMQTT.Subscribe('#', 0);
  UpdateActions;
end;

procedure TfrmMQTTNode.UpdateActions;
var
  B : Boolean;
begin
  actConnect.Enabled        := not Connected;
  actDisconnect.Enabled     := Connected;
  actPublishMessage.Enabled := Connected and (edtPublishTopic.Text <> '');
  B := chkSubscribeToAllTopics.Checked;
  lbxTopics.Enabled             := not B;
  edtTopic.Enabled              := not B;
  actAddSubscription.Enabled    := not B;
  actDeleteSubscription.Enabled := not B;
  actClearSubscriptions.Enabled := not B;
  inherited UpdateActions;
end;
{$ENDREGION}

end.
