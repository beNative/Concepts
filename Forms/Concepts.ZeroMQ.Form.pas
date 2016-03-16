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

unit Concepts.ZeroMQ.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Actions, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst,

  ZeroMQ,

  Spring.SystemUtils;

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
    aclMain             : TActionList;
    actBind             : TAction;
    actClose            : TAction;
    actConnect          : TAction;
    actCreateNew        : TAction;
    actReceive          : TAction;
    actSendMemoText     : TAction;
    actSubscribe        : TAction;
    btnClientConnect    : TButton;
    btnCreateNew        : TButton;
    btnReceive          : TButton;
    btnSend             : TButton;
    btnServerBind       : TButton;
    btnSubscribe        : TButton;
    edtAddress          : TLabeledEdit;
    edtFilter           : TLabeledEdit;
    edtPort             : TLabeledEdit;
    lbxEvents           : TCheckListBox;
    mmoLog              : TMemo;
    mmoReceive          : TMemo;
    mmoSend             : TMemo;
    pnlClient           : TPanel;
    pnlConnectionString : TPanel;
    rgpTransport        : TRadioGroup;
    rgpZMQSocket        : TRadioGroup;
    edtPollTimeout      : TLabeledEdit;
    actSendCounterValue : TAction;
    actResetCounter     : TAction;
    edtCounter          : TLabeledEdit;
    btnSendCounterValue : TButton;
    mmoIPs              : TMemo;
    {$ENDREGION}

    procedure actBindExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actCreateNewExecute(Sender: TObject);
    procedure actReceiveExecute(Sender: TObject);
    procedure actSendMemoTextExecute(Sender: TObject);
    procedure actSubscribeExecute(Sender: TObject);

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lbxEventsClickCheck(Sender: TObject);
    procedure actSendCounterValueExecute(Sender: TObject);
    procedure actResetCounterExecute(Sender: TObject);
    procedure edtCounterExit(Sender: TObject);

  private
    FZMQ       : IZeroMQ;
    FPair      : IZMQPair;
    FEventProc : TZMQEventProc;
    FEvents    : ZMQEvents;
    FPoll      : IZMQPoll;
    FCounter   : Integer;

  protected
    function GetEvents: ZMQEvents;
    procedure SetEvents(const Value: ZMQEvents);
    function GetSocket: ZMQSocket;
    procedure SetSocket(const Value: ZMQSocket);
    function GetConnectionString: string;
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetTransport: string;
    procedure SetTransport(const Value: string);

    procedure UpdateActions; override;
    procedure UpdateEvents;

    property ZMQ: IZeroMQ
      read FZMQ write FZMQ;

    property Pair: IZMQPair
      read FPair write FPair;

    property Poll: IZMQPoll
      read FPoll write FPoll;

    property Socket: ZMQSocket
      read GetSocket write SetSocket;

    property Port: Integer
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
      AOwner : TComponent;
      AZMQ   : IZeroMQ = nil
    ); reintroduce; virtual;

  end;

implementation

uses
  DDuce.Reflect,

  BCEditor.JsonDataObjects,

  Concepts.Utils, Concepts.ZeroMQ.Data;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmZMQConcept.AfterConstruction;
var
  S : string;
begin
  inherited AfterConstruction;
  FZMQ   := TZeroMQ.Create;
  Events := ZMQAllEvents;
  FEventProc := procedure(Event: ZMQEvents; Value: Integer; const Address: string)
    const
      LOG_MESSAGE = '%s (Value=%d, Address=%s)';
    var
      S : string;
      E : ZMQEvent;
    begin
      for E := Low(ZMQEvent) to High(ZMQEvent) do
      begin
        if E in Event then
          S := S + ZMQEventNames[E] + ' ';
      end;
      mmoLog.Lines.Add(Format(LOG_MESSAGE, [S, Value, Address]));
    end;

  GetIPAddresses(mmoIPs.Lines);
  if GetExternalIP(S) then
    mmoIPs.Lines.Add(S);
end;

procedure TfrmZMQConcept.BeforeDestruction;
begin
  Pair := nil;
  ZMQ  := nil;
  inherited BeforeDestruction;
end;

{ Used to test the 'inproc' transport protocol. For this both client and server
  need to share the same ZeroMQ context (or IZeroMQ instance). }

constructor TfrmZMQConcept.Create(AOwner: TComponent; AZMQ: IZeroMQ);
begin
  inherited Create(AOwner);
  if Assigned(AZMQ) then
    FZMQ := AZMQ;
end;
procedure TfrmZMQConcept.edtCounterExit(Sender: TObject);
begin
  FCounter := StrToInt(edtCounter.Text);
end;

{$ENDREGION}

{$REGION 'property access methods'}
function TfrmZMQConcept.GetConnectionString: string;
const
  CONNECTION_STRING = '%s://%s';
var
  S : string;
begin
  S := Format(CONNECTION_STRING, [GetTransport, edtAddress.Text]);
  if Transport = 'tcp' then
    S := S + ':' + Port.ToString;
  Result := S;
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

function TfrmZMQConcept.GetPort: Integer;
begin
  Result := StrToIntDef(edtPort.Text, 5555);
end;

procedure TfrmZMQConcept.SetPort(const Value: Integer);
begin
  if Value <> Port then
  begin
    edtPort.Text := Value.ToString;
  end;
end;

function TfrmZMQConcept.GetSocket: ZMQSocket;
begin
  Result := ZMQSocket(rgpZMQSocket.ItemIndex);
end;

procedure TfrmZMQConcept.SetSocket(const Value: ZMQSocket);
begin
  if Value <> Socket then
  begin
    rgpZMQSocket.ItemIndex := Ord(Value);
  end;
end;

function TfrmZMQConcept.GetTransport: string;
begin
  Result := ZMQTransports[rgpTransport.ItemIndex];
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
        rgpTransport.ItemIndex := I;
      end;
      Inc(I);
    end;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
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
procedure TfrmZMQConcept.actBindExecute(Sender: TObject);
begin
  if Assigned(Pair) then
    Pair.Close;
  Pair := nil;
  Pair := FZMQ.Start(Socket);
  FZMQ.Monitor(
    Pair,
    edtAddress.Text,
    Events,
    FEventProc
  );
  if Transport = 'tcp' then
    edtAddress.Text := '*';
  Pair.Bind(ConnectionString);
end;

procedure TfrmZMQConcept.actCloseExecute(Sender: TObject);
begin
  Pair.Close;
end;

procedure TfrmZMQConcept.actConnectExecute(Sender: TObject);
var
  S    : string;
begin
  if Assigned(Pair) then
  begin
    Pair.Close;
  end;
  Pair := nil;
  Pair := ZMQ.Start(Socket);
  FZMQ.Monitor(
    Pair,
    edtAddress.Text,
    Events,
    FEventProc
  );
  if Pair.Connect(ConnectionString) = 0 then
  begin
    Poll := ZMQ.Poller;
    Poll.RegisterPair(Pair, [PollEvent.PollIn],
      procedure(Event: PollEvents)
      begin
        mmoReceive.Text := Pair.ReceiveString;
      end
    );
  end
  else
    ShowMessage('Could not connect.');
end;

procedure TfrmZMQConcept.actCreateNewExecute(Sender: TObject);
var
  F : TfrmZMQConcept;
begin
  F := TfrmZMQConcept.Create(Self, FZMQ);
  F.Show;
end;

procedure TfrmZMQConcept.actReceiveExecute(Sender: TObject);
begin
  mmoReceive.Text := Pair.ReceiveString(True);
end;

procedure TfrmZMQConcept.actResetCounterExecute(Sender: TObject);
begin
//
end;

procedure TfrmZMQConcept.actSendCounterValueExecute(Sender: TObject);
begin
  Pair.SendString(FCounter.ToString);
  Inc(FCounter);
  edtCounter.Text := FCounter.ToString;
end;

procedure TfrmZMQConcept.actSendMemoTextExecute(Sender: TObject);
begin
  Pair.SendString(mmoSend.Text, True);
end;

procedure TfrmZMQConcept.actSubscribeExecute(Sender: TObject);
begin
  Pair.Subscribe(edtFilter.Text);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmZMQConcept.UpdateActions;
var
  B : Boolean;
begin
  inherited UpdateActions;
  B := Assigned(Pair);
  actReceive.Enabled   := B;
  actSendMemoText.Enabled      := B;
  actSendCounterValue.Enabled := B;
  actSubscribe.Enabled := B;
  pnlConnectionString.Caption := ConnectionString;
  if Assigned(FPoll) then
  begin
    if FPoll.PollOnce(StrToInt(edtPollTimeout.Text)) > 0 then
      FPoll.FireEvents;
  end;
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
{$ENDREGION}

end.
