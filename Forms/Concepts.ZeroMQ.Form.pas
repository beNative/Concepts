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

unit Concepts.ZeroMQ.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Actions, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst,

  ZeroMQ;

type
  TfrmZMQConcept = class(TForm)
    aclMain                  : TActionList;
    actBind                  : TAction;
    actConnect               : TAction;
    actCreateNew             : TAction;
    actReceive               : TAction;
    actSend                  : TAction;
    actSubscribe             : TAction;
    btnClientConnect         : TButton;
    btnReceive               : TButton;
    btnSend                  : TButton;
    btnServerBind            : TButton;
    edtAddress               : TLabeledEdit;
    edtFilter                : TLabeledEdit;
    edtPort                  : TLabeledEdit;
    lbxEvents                : TCheckListBox;
    mmoLog                   : TMemo;
    mmoReceive               : TMemo;
    mmoSend                  : TMemo;
    pnlClient                : TPanel;
    pnlConnectionString      : TPanel;
    pnlZMQSocket             : TPanel;
    rgpZMQSocket             : TRadioGroup;
    shpDealerRouter          : TShape;
    shpPair                  : TShape;
    shpPublisherSubscriber   : TShape;
    shpPullPush              : TShape;
    shpRequesterResponder    : TShape;
    shpSpacer                : TShape;
    shpStream                : TShape;
    shpXPublisherXSubscriber : TShape;
    btnSubscribe             : TButton;
    rgpTransport             : TRadioGroup;
    btnCreateNew             : TButton;

    procedure actSendExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actBindExecute(Sender: TObject);
    procedure actReceiveExecute(Sender: TObject);
    procedure actSubscribeExecute(Sender: TObject);

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actCreateNewExecute(Sender: TObject);

  private
    FZMQ  : IZeroMQ;
    FPair : IZMQPair;

  protected
    function GetConnectionString: string;
    function GetPort: Integer;
    function GetZMQSocket: ZMQSocket;
    function GetTransport: string;

    procedure UpdateActions; override;

    property ZMQ: IZeroMQ
      read FZMQ write FZMQ;

    property Pair: IZMQPair
      read FPair write FPair;

    property Port: Integer
      read GetPort;

    property ConnectionString: string
      read GetConnectionString;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    constructor Create(
      AOwner : TComponent;
      AZMQ   : IZeroMQ = nil
    ); reintroduce; virtual;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmZMQConcept.AfterConstruction;
begin
  inherited AfterConstruction;
  if not Assigned(FZMQ) then
    FZMQ := TZeroMQ.Create;
end;

procedure TfrmZMQConcept.BeforeDestruction;
begin
  Pair := nil;
  ZMQ := nil;
  inherited BeforeDestruction;
end;

constructor TfrmZMQConcept.Create(AOwner: TComponent; AZMQ: IZeroMQ);
begin
  inherited Create(AOwner);
  if Assigned(AZMQ) then
    FZMQ := AZMQ;
end;

{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmZMQConcept.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmZMQConcept.actConnectExecute(Sender: TObject);
var
  N : Integer;
begin
  if Assigned(Pair) then
  begin
    Pair.Close;
  end;
  Pair := nil;
  Pair := ZMQ.Start(GetZMQSocket);
  N := StrToIntDef(edtPort.Text, 5555);
  FZMQ.Monitor(
    Pair,
    edtAddress.Text,
    [Connected, Delayed, Retried, Listening, BindFailed, Accepted, AcceptFailed,
     Closed, CloseFailed, Disconnected, MonitorStopped],
    procedure(Event: ZMQEvents; Value: Integer; const Address: string)
    begin
      if Connected in Event then
        mmoLog.Lines.Add('Connected');
      if Delayed in Event then
        mmoLog.Lines.Add('Delayed');
      if Retried in Event then
        mmoLog.Lines.Add('Retried');
      if Listening in Event then
        mmoLog.Lines.Add('Listening');
      if BindFailed in Event then
        mmoLog.Lines.Add('BindFailed');
      if Accepted in Event then
        mmoLog.Lines.Add('Accepted');
      if AcceptFailed in Event then
        mmoLog.Lines.Add('AcceptFailed');
      if Closed in Event then
        mmoLog.Lines.Add('Closed');
      if CloseFailed in Event then
        mmoLog.Lines.Add('CloseFailed');
      if Disconnected in Event then
        mmoLog.Lines.Add('Disconnected');
      if MonitorStopped in Event then
        mmoLog.Lines.Add('MonitorStopped');
    end
  );
  Pair.Connect(ConnectionString);
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

procedure TfrmZMQConcept.actSendExecute(Sender: TObject);
begin
  Pair.SendString(mmoSend.Text, True);
end;

procedure TfrmZMQConcept.actSubscribeExecute(Sender: TObject);
begin
  Pair.Subscribe(edtFilter.Text);
end;

procedure TfrmZMQConcept.actBindExecute(Sender: TObject);
begin
  if Assigned(Pair) then
    Pair.Close;
  Pair := nil;
  Pair := FZMQ.Start(GetZMQSocket);
  FZMQ.Monitor(
    Pair,
    edtAddress.Text,
    [   Connected, Delayed, Retried,
    Listening, BindFailed,
    Accepted, AcceptFailed,
    Closed, CloseFailed, Disconnected, MonitorStopped],
    procedure(Event: ZMQEvents; Value: Integer; const Address: string)
    begin
    if Connected in Event then
        mmoLog.Lines.Add('Connected');
      if Delayed in Event then
        mmoLog.Lines.Add('Delayed');
      if Retried in Event then
        mmoLog.Lines.Add('Retried');
      if Listening in Event then
        mmoLog.Lines.Add('Listening');
      if BindFailed in Event then
        mmoLog.Lines.Add('BindFailed');
      if Accepted in Event then
        mmoLog.Lines.Add('Accepted');
      if AcceptFailed in Event then
        mmoLog.Lines.Add('AcceptFailed');
      if Closed in Event then
        mmoLog.Lines.Add('Closed');
      if CloseFailed in Event then
        mmoLog.Lines.Add('CloseFailed');
      if Disconnected in Event then
        mmoLog.Lines.Add('Disconnected');
      if MonitorStopped in Event then
        mmoLog.Lines.Add('MonitorStopped');
    end
  );
if GetTransport = 'tcp' then
    edtAddress.Text := '*';
  Pair.Bind(ConnectionString);
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TfrmZMQConcept.GetConnectionString: string;
const
  CONNECTION_STRING = '%s://%s';
var
  S : string;
begin
  S := Format(CONNECTION_STRING, [GetTransport, edtAddress.Text]);
  if GetTransport = 'tcp' then
    S := S + ':' + Port.ToString;
  Result := S;
end;

function TfrmZMQConcept.GetPort: Integer;
begin
  Result := StrToIntDef(edtPort.Text, 5555);
end;

function TfrmZMQConcept.GetTransport: string;
begin
  case rgpTransport.ItemIndex of
    0: Result := 'tcp';
    1: Result := 'inproc';   // needs to have the same owner TZMQ object
    2: Result := 'ipc';
    3: Result := 'pgm';
    4: Result := 'pgm';
  end;
end;

function TfrmZMQConcept.GetZMQSocket: ZMQSocket;
begin
  Result := ZMQSocket(rgpZMQSocket.ItemIndex);
end;

procedure TfrmZMQConcept.UpdateActions;
var
  B : Boolean;
begin
  inherited UpdateActions;
  B := Assigned(Pair);
  actReceive.Enabled   := B;
  actSend.Enabled      := B;
  actSubscribe.Enabled := B;
  pnlConnectionString.Caption := ConnectionString;
end;

{$ENDREGION}

end.
