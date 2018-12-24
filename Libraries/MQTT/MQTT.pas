unit MQTT;

interface

uses
  SysUtils,
  Types,
  Classes,
  // FMX.Types, // TSI
  Vcl.ExtCtrls,
  Generics.Collections,
  SyncObjs,
  // ==============================================================================
  // HammerOh
  // blcksock,
  IdTCPClient,
  // ==============================================================================

  MQTTHeaders,
  MQTTReadThread;

type
{$IF not declared(TBytes)}
  TBytes = array of Byte;
{$IFEND}

  TMQTT = class
  private
    { Private Declarations }
    FClientID    : UTF8String;
    FHostname    : UTF8String;
    FPort        : Integer;
    FMessageID   : Integer;
    FIsConnected : Boolean;
    FRecvThread  : TMQTTReadThread;
    FCSSock      : TCriticalSection;

    FWillMsg   : UTF8String;
    FWillTopic : UTF8String;
    FUsername  : UTF8String;
    FPassword  : UTF8String;
    // ==============================================================================
    // HammerOh
    // FSocket: TTCPBlockSocket;
    FSocket : TIdTCPClient;
    // ==============================================================================

    FKeepAliveTimer : TTimer;

    // Event Fields
    FConnAckEvent  : TConnAckEvent;
    FPublishEvent  : TPublishEvent;
    FPingRespEvent : TPingRespEvent;
    FPingReqEvent  : TPingReqEvent;
    FSubAckEvent   : TSubAckEvent;
    FUnSubAckEvent : TUnSubAckEvent;
    FPubAckEvent   : TPubAckEvent;
    FPubRelEvent   : TPubRelEvent;
    FPubRecEvent   : TPubRecEvent;
    FPubCompEvent  : TPubCompEvent;

    function WriteData(AData : TBytes) : Boolean;
    function HasWill : Boolean;
    function GetNextMessageId : Integer;
    // ==============================================================================
    // HammerOh
    // function createAndResumeRecvThread(Socket: TTCPBlockSocket): boolean;
    function CreateAndResumeRecvThread(var Socket : TIdTCPClient) : Boolean;
    // ==============================================================================

    // TMQTTMessage Factory Methods.
    function ConnectMessage : TMQTTMessage;
    function DisconnectMessage : TMQTTMessage;
    function PublishMessage : TMQTTMessage;
    function PingReqMessage : TMQTTMessage;
    function SubscribeMessage : TMQTTMessage;
    function UnsubscribeMessage : TMQTTMessage;

    // Our Keep Alive Ping Timer Event
    procedure KeepAliveTimer_Event(Sender : TObject);

    // Recv Thread Event Handling Procedures.
    procedure GotConnAck(
      Sender     : TObject;
      ReturnCode : Integer
      );
    procedure GotPingResp(Sender : TObject);
    procedure GotSubAck(
      Sender     : TObject;
      MessageID  : Integer;
      GrantedQoS : array of Integer
      );
    procedure GotUnSubAck(
      Sender    : TObject;
      MessageID : Integer
      );
    procedure GotPub(
      Sender         : TObject;
      ATopic, payload : UTF8String
      );
    procedure GotPubAck(
      Sender    : TObject;
      MessageID : Integer
      );
    procedure GotPubRec(
      Sender    : TObject;
      MessageID : Integer
      );
    procedure GotPubRel(
      Sender    : TObject;
      MessageID : Integer
      );
    procedure GotPubComp(
      Sender    : TObject;
      MessageID : Integer
      );

  public
    { Public Declarations }

    function Connect : Boolean;
    function Disconnect : Boolean;
    function Publish(
      ATopic   : UTF8String;
      APayload : UTF8String
    ) : Boolean; overload;
    function Publish(
      ATopic   : UTF8String;
      APayload : UTF8String;
      ARetain  : Boolean
    ) : Boolean; overload;
    function Publish(
      ATopic   : UTF8String;
      APayload : UTF8String;
      ARetain  : Boolean;
      QoS      : Integer
    ) : Boolean; overload;
    function Subscribe(
      ATopic      : UTF8String;
      RequestQoS : Integer) : Integer; overload;
    function Subscribe(Topics : TDictionary<UTF8String, Integer>)
      : Integer; overload;
    function Unsubscribe(ATopic : UTF8String) : Integer; overload;
    function Unsubscribe(Topics : TStringList) : Integer; overload;
    function PingReq : Boolean;
    constructor Create(
      hostName : UTF8String;
      port     : Integer);
    destructor Destroy; override;

    property WillTopic : UTF8String
      read FWillTopic write FWillTopic;

    property WillMsg : UTF8String
      read FWillMsg write FWillMsg;

    property Username : UTF8String
      read FUsername write FUsername;

    property Password : UTF8String
      read FPassword write FPassword;
    // Client ID is our Client Identifier.
    property ClientID : UTF8String
      read FClientID write FClientID;

    property IsConnected : Boolean
      read FIsConnected;

    // Event Handlers
    property OnConnAck : TConnAckEvent
      read FConnAckEvent write FConnAckEvent;

    property OnPublish : TPublishEvent
      read FPublishEvent write FPublishEvent;

    property OnPingResp : TPingRespEvent
      read FPingRespEvent write FPingRespEvent;

    property OnPingReq : TPingRespEvent
      read FPingRespEvent write FPingRespEvent;

    property OnSubAck : TSubAckEvent
      read FSubAckEvent write FSubAckEvent;

    property OnUnSubAck : TUnSubAckEvent
      read FUnSubAckEvent write FUnSubAckEvent;

    property OnPubAck : TUnSubAckEvent
      read FUnSubAckEvent write FUnSubAckEvent;

    property OnPubRec : TUnSubAckEvent
      read FUnSubAckEvent write FUnSubAckEvent;

    property OnPubRel : TUnSubAckEvent
      read FUnSubAckEvent write FUnSubAckEvent;

    property OnPubComp : TUnSubAckEvent
      read FUnSubAckEvent write FUnSubAckEvent;
  end;

implementation


{ TMQTTClient }

procedure TMQTT.GotConnAck(
  Sender     : TObject;
  ReturnCode : Integer);
begin
  if Assigned(FConnAckEvent) then
    OnConnAck(Self, ReturnCode);
end;

function TMQTT.Connect : Boolean;
var
  Msg : TMQTTMessage;
begin
  // Create socket and connect.
  // ==============================================================================
  // HammerOh
  // FSocket := TTCPBlockSocket.Create;
  FSocket := TIdTCPClient.Create(nil);
  // ==============================================================================

  try
    // ==============================================================================
    // HammerOh
    // FSocket.Connect(Self.FHostname, IntToStr(Self.FPort));
    FSocket.Host := Self.FHostname;
    FSocket.port := Self.FPort;
    FSocket.Connect;
    // ==============================================================================

    FIsConnected := true;
  except
    // If we encounter an exception upon connection then reraise it, free the socket
    // and reset our isConnected flag.
    on E : Exception do
    begin
      raise;
      FIsConnected := false;
      FSocket.Free;
    end;
  end;

  if FIsConnected then
  begin
    Msg := ConnectMessage;
    try
      Msg.payload.Contents.Add(Self.FClientID);
      (Msg.VariableHeader as TMQTTConnectVarHeader).WillFlag := Ord(HasWill);
      if HasWill then
      begin
        Msg.payload.Contents.Add(Self.FWillTopic);
        Msg.payload.Contents.Add(Self.FWillMsg);
      end;

      if ((Length(FUsername) > 1) and (Length(FPassword) > 1)) then
      begin
        Msg.payload.Contents.Add(FUsername);
        Msg.payload.Contents.Add(FPassword);
      end;

      if WriteData(Msg.ToBytes) then
        Result := true
      else
        Result := false;
      // Start our Receive thread.
      if (Result and CreateAndResumeRecvThread(FSocket)) then
      begin
        // Use the KeepAlive that we just sent to determine our ping timer.
        FKeepAliveTimer.Interval :=
          (Round((Msg.VariableHeader as TMQTTConnectVarHeader).KeepAlive *
              0.80)) * 1000;
        FKeepAliveTimer.Enabled := true;
      end;

    finally
      Msg.Free;
    end;
  end;
end;

function TMQTT.ConnectMessage : TMQTTMessage;
begin
  Result                         := TMQTTMessage.Create;
  Result.VariableHeader          := TMQTTConnectVarHeader.Create;
  Result.payload                 := TMQTTPayload.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.Connect);
  Result.FixedHeader.Retain      := 0;
  Result.FixedHeader.QoSLevel    := 0;
  Result.FixedHeader.Duplicate   := 0;
end;

constructor TMQTT.Create(
  hostName : UTF8String;
  port     : Integer);
begin
  inherited Create;

  Self.FIsConnected := false;
  Self.FHostname    := hostName;
  Self.FPort        := port;
  Self.FMessageID   := 1;
  // Randomise and create a random client id.
  Randomize;
  Self.FClientID := 'TMQTT' + IntToStr(Random(1000) + 1);
  FCSSock        := TCriticalSection.Create;

  // Create the timer responsible for pinging.
  FKeepAliveTimer         := TTimer.Create(nil);
  FKeepAliveTimer.Enabled := false;
  FKeepAliveTimer.OnTimer := KeepAliveTimer_Event;
end;

// ==============================================================================
// HammerOh
// function TMQTT.createAndResumeRecvThread(Socket: TTCPBlockSocket): boolean;
function TMQTT.CreateAndResumeRecvThread(var Socket : TIdTCPClient) : Boolean;
// ==============================================================================
begin
  Result := false;
  try
    FRecvThread := TMQTTReadThread.Create(Socket, FCSSock);

    { Todo: Assign Event Handlers here. }
    FRecvThread.OnConnAck  := Self.GotConnAck;
    FRecvThread.OnPublish  := Self.GotPub;
    FRecvThread.OnPingResp := Self.GotPingResp;
    FRecvThread.OnSubAck   := Self.GotSubAck;
    FRecvThread.OnPubAck   := Self.GotPubAck;
    Result                 := true;
  except
    Result := false;
  end;
end;

destructor TMQTT.Destroy;
begin
  if Assigned(FSocket) then
  begin
    Disconnect;
  end;
  if Assigned(FKeepAliveTimer) then
  begin
    FreeAndNil(FKeepAliveTimer);
  end;
  if Assigned(FRecvThread) then
  begin
    FreeAndNil(FRecvThread);
  end;
  if Assigned(FCSSock) then
  begin
    FreeAndNil(FCSSock);
  end;
  inherited;
end;

function TMQTT.Disconnect : Boolean;
var
  Msg : TMQTTMessage;
begin
  Result := false;
  if IsConnected then
  begin
    FKeepAliveTimer.Enabled := false;
    Msg                     := DisconnectMessage;
    if WriteData(Msg.ToBytes) then
      Result := true
    else
      Result := false;
    Msg.Free;
    // Terminate our socket receive thread.
    FRecvThread.Terminate;
    FRecvThread.WaitFor;

    // Close our socket.
    // ==============================================================================
    // HammerOh
    // FSocket.CloseSocket;
    FSocket.Disconnect;
    // ==============================================================================

    FIsConnected := false;

    // Free everything.
    if Assigned(FRecvThread) then
      FreeAndNil(FRecvThread);
    if Assigned(FSocket) then
      FreeAndNil(FSocket);
  end;
end;

function TMQTT.DisconnectMessage : TMQTTMessage;
begin
  Result                         := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.Disconnect);
end;

function TMQTT.GetNextMessageId : Integer;
begin
  // If we've reached the upper bounds of our 16 bit unsigned message Id then
  // start again. The spec says it typically does but is not required to Inc(MsgId,1).
  if (FMessageID = 65535) then
  begin
    FMessageID := 1;
  end;

  // Return our current message Id
  Result := FMessageID;
  // Increment message Id
  Inc(FMessageID);
end;

function TMQTT.HasWill : Boolean;
begin
  if ((Length(FWillTopic) < 1) and (Length(FWillMsg) < 1)) then
  begin
    Result := false;
  end
  else
    Result := true;
end;

procedure TMQTT.KeepAliveTimer_Event(Sender : TObject);
begin
  if Self.IsConnected then
  begin
    PingReq;
  end;
end;

function TMQTT.PingReq : Boolean;
var
  Msg : TMQTTMessage;
begin
  Result := false;
  if IsConnected then
  begin
    Msg := PingReqMessage;
    if WriteData(Msg.ToBytes) then
      Result := true
    else
      Result := false;
    Msg.Free;
  end;
end;

function TMQTT.PingReqMessage : TMQTTMessage;
begin
  Result                         := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.PingReq);
end;

procedure TMQTT.GotPingResp(Sender : TObject);
begin
  if Assigned(FPingRespEvent) then
    OnPingResp(Self);
end;

function TMQTT.Publish(
  ATopic, APayload : UTF8String;
  ARetain          : Boolean) : Boolean;
begin
  Result := Publish(ATopic, APayload, ARetain, 0);
end;

function TMQTT.Publish(ATopic, APayload : UTF8String) : Boolean;
begin
  Result := Publish(ATopic, APayload, false, 0);
end;

function TMQTT.Publish(
  ATopic, APayload : UTF8String;
  ARetain          : Boolean;
  QoS             : Integer) : Boolean;
var
  Msg : TMQTTMessage;
begin
  if ((QoS > -1) and (QoS <= 3)) then
  begin
    if IsConnected then
    begin
      Msg                      := PublishMessage;
      Msg.FixedHeader.QoSLevel := QoS;
      (Msg.VariableHeader as TMQTTPublishVarHeader).QoSLevel := QoS;
      (Msg.VariableHeader as TMQTTPublishVarHeader).Topic := ATopic;
      if (QoS > 0) then
      begin
        (Msg.VariableHeader as TMQTTPublishVarHeader).MessageID :=
          GetNextMessageId;
      end;
      Msg.payload.Contents.Add(APayload);
      Msg.payload.PublishMessage := true;
      if WriteData(Msg.ToBytes) then
        Result := true
      else
        Result := false;
      Msg.Free;
    end;
  end
  else
    raise EInvalidOp.Create
      ('QoS level can only be equal to or between 0 and 3.');
end;

function TMQTT.PublishMessage : TMQTTMessage;
begin
  Result                         := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.Publish);
  Result.VariableHeader          := TMQTTPublishVarHeader.Create(0);
  Result.payload                 := TMQTTPayload.Create;
end;

procedure TMQTT.GotPubRec(
  Sender    : TObject;
  MessageID : Integer);
begin
  if Assigned(FPubRecEvent) then
    OnPubRec(Self, MessageID);
end;

procedure TMQTT.GotPubRel(
  Sender    : TObject;
  MessageID : Integer);
begin
  if Assigned(FPubRelEvent) then
    OnPubRel(Self, MessageID);
end;

function TMQTT.Subscribe(
  ATopic      : UTF8String;
  RequestQoS : Integer) : Integer;
var
  dTopics : TDictionary<UTF8String, Integer>;
begin
  dTopics := TDictionary<UTF8String, Integer>.Create;
  dTopics.Add(ATopic, RequestQoS);
  Result := Subscribe(dTopics);
  dTopics.Free;
end;

procedure TMQTT.GotSubAck(
  Sender     : TObject;
  MessageID  : Integer;
  GrantedQoS : array of Integer);
begin
  if Assigned(FSubAckEvent) then
    OnSubAck(Self, MessageID, GrantedQoS);
end;

function TMQTT.Subscribe(Topics : TDictionary<UTF8String, Integer>) : Integer;
var
  Msg    : TMQTTMessage;
  MsgId  : Integer;
  sTopic : UTF8String;
  data   : TBytes;
begin
  Result := -1;
  if IsConnected then
  begin
    Msg   := SubscribeMessage;
    MsgId := GetNextMessageId;
    (Msg.VariableHeader as TMQTTSubscribeVarHeader).MessageID := MsgId;

    for sTopic in Topics.Keys do
    begin
      Msg.payload.Contents.Add(sTopic);
      Msg.payload.Contents.Add(IntToStr(Topics.Items[sTopic]))
    end;
    // the subscribe message contains integer literals not encoded as UTF8Strings.
    Msg.payload.ContainsIntLiterals := true;

    data := Msg.ToBytes;
    if WriteData(data) then
      Result := MsgId;

    Msg.Free;
  end;
end;

function TMQTT.SubscribeMessage : TMQTTMessage;
begin
  Result                         := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.Subscribe);
  Result.FixedHeader.QoSLevel    := 0;
  Result.VariableHeader          := TMQTTSubscribeVarHeader.Create;
  Result.payload                 := TMQTTPayload.Create;
end;

function TMQTT.Unsubscribe(ATopic : UTF8String) : Integer;
var
  slTopics : TStringList;
begin
  slTopics := TStringList.Create;
  slTopics.Add(ATopic);
  Result := Unsubscribe(slTopics);
  slTopics.Free;
end;

procedure TMQTT.GotUnSubAck(
  Sender    : TObject;
  MessageID : Integer);
begin
  if Assigned(FUnSubAckEvent) then
    OnUnSubAck(Self, MessageID);
end;

function TMQTT.Unsubscribe(Topics : TStringList) : Integer;
var
  Msg    : TMQTTMessage;
  MsgId  : Integer;
  sTopic : UTF8String;
begin
  Result := -1;
  if IsConnected then
  begin
    Msg   := UnsubscribeMessage;
    MsgId := GetNextMessageId;
    (Msg.VariableHeader as TMQTTUnsubscribeVarHeader).MessageID := MsgId;

    Msg.payload.Contents.AddStrings(Topics);

    if WriteData(Msg.ToBytes) then
      Result := MsgId;

    Msg.Free;
  end;
end;

function TMQTT.UnsubscribeMessage : TMQTTMessage;
var
  Msg : TMQTTMessage;
begin
  Result                         := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.Unsubscribe);
  Result.FixedHeader.QoSLevel    := 1;
  Result.VariableHeader          := TMQTTUnsubscribeVarHeader.Create;
  Result.payload                 := TMQTTPayload.Create;
end;

function TMQTT.WriteData(AData : TBytes) : Boolean;
var
  sentData        : Integer;
  attemptsToWrite : Integer;
  dataStream      : TMemoryStream;
begin
  Result          := false;
  sentData        := 0;
  attemptsToWrite := 1;
  if IsConnected then
  begin
    // ==============================================================================
    // HammerOh
    (*
      repeat
      FCSSock.Acquire;
      try
      if FSocket.CanWrite(500 * attemptsToWrite) then
      begin
      sentData := sentData + FSocket.SendBuffer(Pointer(Copy(AData, sentData - 1, Length(AData) + 1)), Length(AData) - sentData);
      Inc(attemptsToWrite);
      end;
      finally
      FCSSock.Release;
      end;
      until ((attemptsToWrite = 3) or (sentData = Length(AData)));
      if sentData = Length(AData) then
      begin
      Result := True;
      FisConnected := true;
      end
      else
      begin
      Result := False;
      FisConnected := false;
      raise Exception.Create('Error Writing to Socket, it appears to be disconnected');
      end;
    *)
    try
      dataStream          := TMemoryStream.Create;
      dataStream.Position := 0;
      dataStream.WriteBuffer(AData, Length(AData));
      FSocket.IOHandler.Write(dataStream);
      dataStream.Free;
      Result       := true;
      FIsConnected := true;
    except
      Result       := false;
      FIsConnected := false;
    end;
    // ==============================================================================
  end;
end;

procedure TMQTT.GotPub(
  Sender         : TObject;
  ATopic, payload : UTF8String);
begin
  if Assigned(FPublishEvent) then
    OnPublish(Self, ATopic, payload);
end;

procedure TMQTT.GotPubAck(
  Sender    : TObject;
  MessageID : Integer);
begin
  if Assigned(FPubAckEvent) then
    OnPubAck(Self, MessageID);
end;

procedure TMQTT.GotPubComp(
  Sender    : TObject;
  MessageID : Integer);
begin
  if Assigned(FPubCompEvent) then
    OnPubComp(Self, MessageID);
end;

end.
