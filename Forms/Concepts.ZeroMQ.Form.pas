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

{$I Concepts.inc}

unit Concepts.ZeroMQ.Form;

{ Form demonstrating the ZeroMQ Delphi binding.
             http://github.com/zedalaye/Delphi-ZeroMQ

  A great tutorial on ZeroMQ (and message queuing in general) can be found at:
               http://zguide.zeromq.org/page:all
}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.ImageList, System.SysUtils, System.Variants, System.Actions,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst, Vcl.ComCtrls, Vcl.ImgList,

  ZeroMQ,

  Spring.SystemUtils,

  VirtualTrees,

  DDuce.Components.LogTree, Vcl.Buttons, Vcl.ToolWin;

{ IZeroMQ
    .Start(ZMQSocket) => IZMQPair  Creates a IZMQPair which supports the given
                         socket.
    .Monitor => connect a TZMQEventProc which will be called for the given
                set of events.

  These are the socket combinations that are valid for a connect-bind pair
  (either side can bind):

    PUB and SUB
    REQ and REP
    REQ and ROUTER (take care, REQ inserts an extra null frame)
    DEALER and REP (take care, REP assumes a null frame)
    DEALER and ROUTER
    DEALER and DEALER
    ROUTER and ROUTER
    PUSH and PULL
    PAIR and PAIR
 }

type
  TfrmZMQConcept = class(TForm)
    {$REGION 'designer controls'}
    aclMain                    : TActionList;
    actAddSubscription         : TAction;
    actBind                    : TAction;
    actClearReceived           : TAction;
    actClearSubscriptions      : TAction;
    actClose                   : TAction;
    actConnect                 : TAction;
    actCreateNew               : TAction;
    actCreateNewWithNewContext : TAction;
    actDeleteSubscription      : TAction;
    actPopulateMemo            : TAction;
    actReceive                 : TAction;
    actResetCounter            : TAction;
    actSendCounterValue        : TAction;
    actSendLineByLine          : TAction;
    actSendMemoText            : TAction;
    actSendMessages            : TAction;
    actSubscribeToAll          : TAction;
    btnAddSubscription         : TToolButton;
    btnClearSubscriptions      : TToolButton;
    btnCreateNew               : TButton;
    btnCreateNew1              : TButton;
    btnDeleteSubscription      : TToolButton;
    btnPopulateMemo            : TButton;
    btnReceive                 : TButton;
    btnReceive1                : TButton;
    btnSend                    : TButton;
    btnSend1000Messages        : TButton;
    btnSendCounterValue        : TButton;
    btnSendLineByLine          : TButton;
    btnSubscribeToAll          : TButton;
    edtAddress                 : TLabeledEdit;
    edtCounter                 : TLabeledEdit;
    edtFilter                  : TLabeledEdit;
    edtPort                    : TLabeledEdit;
    edtQuantity                : TEdit;
    grpEndPoint                : TGroupBox;
    grpSubscriptions           : TGroupBox;
    imlMain                    : TImageList;
    lbxSubscriptions           : TListBox;
    mmoIPs                     : TMemo;
    mmoReceive                 : TMemo;
    mmoSend                    : TMemo;
    pgcMessage                 : TPageControl;
    pnlClient                  : TPanel;
    pnlLeft                    : TPanel;
    pnlLog                     : TPanel;
    pnlNodeSettings            : TPanel;
    pnlSocketConfiguration     : TPanel;
    pnlTop                     : TPanel;
    sbrMain                    : TStatusBar;
    tlbSubscriptions           : TToolBar;
    tmrPoll                    : TTimer;
    tsReceive                  : TTabSheet;
    tsSend                     : TTabSheet;
    actShowLastEndPoint: TAction;
    grp1: TGroupBox;
    cbxZMQSocketType: TComboBox;
    tsSettings: TTabSheet;
    grpMonitorEvents: TGroupBox;
    lbxEvents: TCheckListBox;
    grpPollingSettings: TGroupBox;
    edtPollTimeout: TLabeledEdit;
    btnClientConnect: TButton;
    btnServerBind: TButton;
    lbxEndPoints: TListBox;
    cbxTransport: TComboBox;
    lblTransport: TLabel;
    edtConnectionString: TEdit;
    actStart: TAction;
    btnStart: TButton;
    btnClose: TButton;
    actCreateNewSubscriberNode: TAction;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actBindExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actCreateNewExecute(Sender: TObject);
    procedure actReceiveExecute(Sender: TObject);
    procedure actSendMemoTextExecute(Sender: TObject);
    procedure actSendCounterValueExecute(Sender: TObject);
    procedure actResetCounterExecute(Sender: TObject);
    procedure actSendMessagesExecute(Sender: TObject);
    procedure actAddSubscriptionExecute(Sender: TObject);
    procedure actDeleteSubscriptionExecute(Sender: TObject);
    procedure actClearSubscriptionsExecute(Sender: TObject);
    procedure actCreateNewWithNewContextExecute(Sender: TObject);
    procedure actSendLineByLineExecute(Sender: TObject);
    procedure actClearReceivedExecute(Sender: TObject);
    procedure actSubscribeToAllExecute(Sender: TObject);
    procedure actPopulateMemoExecute(Sender: TObject);
    {$ENDREGION}

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lbxEventsClickCheck(Sender: TObject);
    procedure edtCounterExit(Sender: TObject);
    procedure tmrPollTimer(Sender: TObject);
    procedure actShowLastEndPointExecute(Sender: TObject);
    procedure actStartExecute(Sender: TObject);
    procedure actCreateNewSubscriberNodeExecute(Sender: TObject);

  private
    FZMQ             : IZeroMQ;
    FPair            : IZMQPair;
    FEventProc       : TZMQEventProc;
    FEvents          : ZMQEvents;
    FPoll            : IZMQPoll;
    FCounter         : Integer;
    FLogTree         : TLogTree;
    FSubscribedToAll : Boolean;

  protected
    {$REGION 'property access methods'}
    function GetEvents: ZMQEvents;
    procedure SetEvents(const Value: ZMQEvents);
    function GetSocketKind: ZMQSocket;
    procedure SetSocketKind(const Value: ZMQSocket);
    function GetConnectionString: string;
    function GetPort: string;
    procedure SetPort(const Value: string);
    function GetTransport: string;
    procedure SetTransport(const Value: string);
    {$ENDREGION}

    procedure UpdateActions; override;
    procedure UpdateEvents;

    procedure Initialize;
    procedure StartZMQ;
    procedure CloseZMQ;
    procedure Bind;
    procedure Connect;
    procedure ClearSubscriptions;
    procedure UpdateConnectionString;

    function GenerateRandomData(ALineCount: Integer): string;

    property ZMQ: IZeroMQ
      read FZMQ write FZMQ;

    property Pair: IZMQPair
      read FPair write FPair;

    property Poll: IZMQPoll
      read FPoll write FPoll;

    property SocketKind: ZMQSocket
      read GetSocketKind write SetSocketKind;

    property Port: string
      read GetPort write SetPort;

    property Transport: string
      read GetTransport write SetTransport;

    property ConnectionString: string
      read GetConnectionString;

    { Set of all event types to monitor. }
    property Events: ZMQEvents
      read GetEvents write SetEvents;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    constructor Create(
      AOwner      : TComponent;
      ASocketKind : ZMQSocket;
      AZMQ        : IZeroMQ = nil
    ); reintroduce; overload; virtual;

    constructor Create(AOwner: TComponent); overload; override;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Reflect, DDuce.Logger, DDuce.Components.Factories, DDuce.RandomData,
  DDuce.Utils,

  Concepts.ZeroMQ.Data, Concepts.Utils,

  Spring;

{$REGION 'construction and destruction'}
procedure TfrmZMQConcept.AfterConstruction;
begin
  inherited AfterConstruction;
  FLogTree := TDDuceComponents.CreateLogTree(Self, pnlLog);
  FLogTree.DateTimeFormat     := 'hh:nn:ss.zzz';
  FLogTree.ShowDateColumn     := False;
  FLogTree.AutoLogLevelColors := True;
  GetIPAddresses(mmoIPs.Lines);
  Initialize;
  tmrPoll.Enabled := True;
end;

procedure TfrmZMQConcept.BeforeDestruction;
begin
  Pair := nil;
  ZMQ  := nil;
  inherited BeforeDestruction;
end;

{ Used to test the 'inproc' transport protocol. For this both client and server
  need to share the same ZeroMQ context (or IZeroMQ instance). }

constructor TfrmZMQConcept.Create(AOwner: TComponent; ASocketKind : ZMQSocket;
 AZMQ: IZeroMQ);
begin
  inherited Create(AOwner);
  SocketKind := ASocketKind;
  if Assigned(AZMQ) then
    FZMQ := AZMQ;
end;

constructor TfrmZMQConcept.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SocketKind := ZMQSocket.Publisher;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmZMQConcept.GenerateRandomData(ALineCount: Integer): string;
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

function TfrmZMQConcept.GetConnectionString: string;
begin
  Result := edtConnectionString.Text;
end;

function TfrmZMQConcept.GetEvents: ZMQEvents;
begin
  Result := FEvents;
end;

procedure TfrmZMQConcept.SetEvents(const Value: ZMQEvents);
var
  E : ZMQEvent;
begin
  if Value <> Events then
  begin
    FEvents := Value;
    for E := Low(ZMQEvent) to High(ZMQEvent) do
    begin
      lbxEvents.Checked[Ord(E)] := E in FEvents;
    end;
  end;
end;

function TfrmZMQConcept.GetPort: string;
begin
  Result := edtPort.Text;
end;

procedure TfrmZMQConcept.SetPort(const Value: string);
begin
  if Value <> Port then
  begin
    edtPort.Text := Value;
  end;
end;

function TfrmZMQConcept.GetSocketKind: ZMQSocket;
begin
  Result := ZMQSocket(cbxZMQSocketType.ItemIndex);
end;

procedure TfrmZMQConcept.SetSocketKind(const Value: ZMQSocket);
begin
  if Value <> SocketKind then
  begin
    cbxZMQSocketType.ItemIndex := Ord(Value);
  end;
end;

function TfrmZMQConcept.GetTransport: string;
begin
  Result := ZMQTransports[cbxTransport.ItemIndex];
end;

procedure TfrmZMQConcept.SetTransport(const Value: string);
var
  I : Integer;
  B : Boolean;
begin
  if Value <> Transport then
  begin
    I := 0;
    B := False;
    while not B and (I < Length(ZMQTransports)) do
    begin
      if ZMQTransports[I] = Value then
      begin
        B := True;
        cbxTransport.ItemIndex := I;
      end;
      Inc(I);
    end;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmZMQConcept.tmrPollTimer(Sender: TObject);
begin
  if Assigned(FPoll) then
  begin
    while FPoll.PollOnce(StrToInt(edtPollTimeout.Text)) > 0 do
      FPoll.FireEvents;
  end;
end;

procedure TfrmZMQConcept.edtCounterExit(Sender: TObject);
begin
  FCounter := StrToInt(edtCounter.Text);
end;

procedure TfrmZMQConcept.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmZMQConcept.lbxEventsClickCheck(Sender: TObject);
begin
  UpdateEvents;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmZMQConcept.actAddSubscriptionExecute(Sender: TObject);
begin
  if lbxSubscriptions.Items.IndexOf(edtFilter.Text) = -1 then
    lbxSubscriptions.Items.Add(edtFilter.Text);
  Pair.Subscribe(edtFilter.Text);
end;

procedure TfrmZMQConcept.actBindExecute(Sender: TObject);
begin
  Bind;
end;

procedure TfrmZMQConcept.actClearReceivedExecute(Sender: TObject);
begin
  mmoReceive.Clear;
end;

procedure TfrmZMQConcept.actClearSubscriptionsExecute(Sender: TObject);
begin
  ClearSubscriptions;
end;

procedure TfrmZMQConcept.actCloseExecute(Sender: TObject);
begin
  CloseZMQ;
end;

procedure TfrmZMQConcept.actConnectExecute(Sender: TObject);
begin
  Connect;
end;

procedure TfrmZMQConcept.actCreateNewExecute(Sender: TObject);
var
  F : TfrmZMQConcept;
begin
  F := TfrmZMQConcept.Create(Self, ZMQSocket.Subscriber, FZMQ);
  F.Show;
end;

procedure TfrmZMQConcept.actCreateNewSubscriberNodeExecute(Sender: TObject);
begin
//
end;

procedure TfrmZMQConcept.actCreateNewWithNewContextExecute(Sender: TObject);
var
  F : TfrmZMQConcept;
begin
  F := TfrmZMQConcept.Create(Self);
  F.Show;
end;

procedure TfrmZMQConcept.actDeleteSubscriptionExecute(Sender: TObject);
begin
  lbxSubscriptions.Items.Delete(lbxSubscriptions.ItemIndex);
  Pair.UnSubscribe(edtFilter.Text);
end;

procedure TfrmZMQConcept.actPopulateMemoExecute(Sender: TObject);
begin
  mmoSend.Text := GenerateRandomData(1000);
end;

procedure TfrmZMQConcept.actReceiveExecute(Sender: TObject);
begin
  mmoReceive.Text := Pair.ReceiveString(True);
end;

procedure TfrmZMQConcept.actResetCounterExecute(Sender: TObject);
begin
//
end;

procedure TfrmZMQConcept.actSendMessagesExecute(Sender: TObject);
var
  I : Integer;
  S : string;
  N : Integer;
begin
  N := StrToInt(edtQuantity.Text);
  for I := 1 to N do
  begin
    Inc(FCounter);
    S := S + Format('Message %d', [FCounter]);
  end;
  Pair.SendString(S, True);
  edtCounter.Text := FCounter.ToString;
end;

procedure TfrmZMQConcept.actShowLastEndPointExecute(Sender: TObject);
begin
  if Assigned(Pair) then
  begin
    ShowMessage(Pair.LastEndPoint);
  end;

end;

procedure TfrmZMQConcept.actStartExecute(Sender: TObject);
begin
  StartZMQ;
end;

procedure TfrmZMQConcept.actSendCounterValueExecute(Sender: TObject);
begin
  Pair.SendString(FCounter.ToString);
  Inc(FCounter);
  edtCounter.Text := FCounter.ToString;
end;

procedure TfrmZMQConcept.actSendLineByLineExecute(Sender: TObject);
var
  SL : IShared<TStringList>;
begin
  SL := Shared<TStringList>.New;
  SL.Assign(mmoSend.Lines);
  TThread.CreateAnonymousThread(procedure
    var
      S : string;
    begin
      for S in SL do
      begin
        Pair.SendString(S, False);
      end;
    end
  ).Start;
end;

procedure TfrmZMQConcept.actSendMemoTextExecute(Sender: TObject);
begin
  Pair.SendString(mmoSend.Text, True);
end;

procedure TfrmZMQConcept.actSubscribeToAllExecute(Sender: TObject);
begin
  if Assigned(Pair) then
  begin
    ClearSubscriptions;
    Pair.Subscribe('');
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmZMQConcept.CloseZMQ;
begin
  Pair := nil;
  FZMQ := nil;
  Poll := nil;
  lbxSubscriptions.Clear;
  lbxEndPoints.CLear;
  FSubscribedToAll := False;
  edtFilter.Text := '';
  FLogTree.Log('<b>Closed</b>', llDebug);
end;

procedure TfrmZMQConcept.Initialize;
begin
  Events := ZMQAllEvents;
  FEventProc := procedure(Event: ZMQEvents; Value: Integer; const Address: string)
    const
      LOG_MESSAGE1 = '<b>%s</b> <x=100>(Address = %s)</x>';
      LOG_MESSAGE2 = '<b>%s</b>';
    var
      S : string;
      E : ZMQEvent;
      L : TLogLevel;
    begin
      for E := Low(ZMQEvent) to High(ZMQEvent) do
      begin
        if E in Event then
        begin
          S := S + ZMQEventNames[E] + ' ';
          if E in [ZMQEvent.BindFailed, ZMQEvent.CloseFailed, ZMQEvent.AcceptFailed] then
            L := llError
          else if E in [ZMQEvent.Delayed, ZMQEvent.Retried] then
            L := llWarning
          else if E in [ZMQEvent.MonitorStopped, ZMQEvent.Closed, ZMQEvent.Disconnected] then
            L := llNone
          else
            L := llDebug;
        end;
      end;
      TThread.Queue(TThread.Current, procedure
      begin
        if Address <> '' then
          FLogTree.LogFmt(LOG_MESSAGE1, [S, Address], L)
        else
          FLogTree.LogFmt(LOG_MESSAGE2, [S], L);
        FLogTree.Header.AutoFitColumns(False, smaAllColumns);
      end
      );
    end;
end;

procedure TfrmZMQConcept.StartZMQ;
begin
  if not Assigned(FZMQ) then
    FZMQ := TZeroMQ.Create;
  if not Assigned(Pair) then
  begin
    Pair := ZMQ.Start(SocketKind);
    FZMQ.Monitor(
      Pair,
      'Monitor', //edtAddress.Text,
      Events,
      FEventProc
    );
  end;
end;

procedure TfrmZMQConcept.UpdateActions;
var
  B : Boolean;
begin
  inherited UpdateActions;
  B := Assigned(Pair);
  actReceive.Enabled          := B;
  actSendMemoText.Enabled     := B;
  actSendCounterValue.Enabled := B;
  actSendLineByLine.Enabled   := B;
  actClose.Enabled            := B;
  edtAddress.Enabled          := B;
  edtPort.Enabled             := B;
  lblTransport.Enabled        := B;
  cbxTransport.Enabled        := B;
  edtConnectionString.Enabled := B;
  //cbxZMQSocketType.Enabled    := not B;
  actAddSubscription.Enabled    := B and not FSubscribedToAll;
  actDeleteSubscription.Enabled := actAddSubscription.Enabled;
  edtFilter.Enabled             := actAddSubscription.Enabled;
  lbxSubscriptions.Enabled      := actAddSubscription.Enabled;
  actClearSubscriptions.Enabled := actAddSubscription.Enabled;
  actSubscribeToAll.Enabled     := actAddSubscription.Enabled;
  actBind.Enabled    := B;
  actConnect.Enabled := B;
  if Assigned(FZMQ) then
  begin
    sbrMain.SimpleText := Format('Context handle: %d', [Integer(FZMQ)]);
  end;
  UpdateConnectionString;
end;

procedure TfrmZMQConcept.UpdateConnectionString;
var
  A : string;
  S : string;
  T : string;
  P : string;
begin
  T := cbxTransport.Text;
  A := edtAddress.Text;
  S := Format('%s://%s', [T, A]);
  if T = 'tcp' then
  begin
    P := edtPort.Text;
    S := Format('%s:%s', [S, P])
  end;
  edtConnectionString.Text := S;
end;

procedure TfrmZMQConcept.UpdateEvents;
var
  I : Integer;
begin
  for I := 0 to lbxEvents.Count - 1 do
  begin
    if lbxEvents.Checked[I] then
      Include(FEvents, ZMQEvent(I))
    else
      Exclude(FEvents, ZMQEvent(I));
  end;
end;

{  A server node can bind to many endpoints (that is, a combination of protocol
   and address), and it can do this using a single socket. This means it will
   accept connections across different transports:
    zmq_bind (socket, "tcp://*:5555");
    zmq_bind (socket, "tcp://*:9999");
    zmq_bind (socket, "inproc://somename");
}

procedure TfrmZMQConcept.Bind;
begin
  FSubscribedToAll := False;
  if Transport = 'tcp' then
  begin
    edtAddress.Text := '*';
    UpdateConnectionString;
  end;
  if Pair.Bind(ConnectionString) = 0 then
    lbxEndPoints.Items.Add(Pair.LastEndPoint);
end;

procedure TfrmZMQConcept.ClearSubscriptions;
var
  S : string;
begin
  if Assigned(Pair) then
  begin
    FSubscribedToAll := False;
    for S in lbxSubscriptions.Items do
    begin
      Pair.UnSubscribe(S);
    end;
    Pair.UnSubscribe('');
  end;
  edtFilter.Text := '';
  lbxSubscriptions.Items.Clear;
end;

procedure TfrmZMQConcept.Connect;
begin
  mmoReceive.Clear;
  FSubscribedToAll := False;
  StartZMQ;

  if Pair.Connect(ConnectionString) = 0 then
  begin
    Poll := ZMQ.Poller;
    Poll.RegisterPair(Pair, [PollEvent.PollIn],
      procedure(Event: PollEvents)
      begin
        pgcMessage.ActivePageIndex := 1;
        if Assigned(Pair) then
        begin
          mmoReceive.Lines.Add(Pair.ReceiveString);
        end;
      end
    );
    lbxEndPoints.Items.Add(Pair.LastEndPoint);
  end
  else
    FLogTree.Log('Connect failed.', llError);
end;
{$ENDREGION}

end.
