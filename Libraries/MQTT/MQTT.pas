unit MQTT;

interface

uses
  SysUtils,
  Types,
  Classes,
  //FMX.Types, // TSI
  Vcl.ExtCtrls,
  Generics.Collections,
  SyncObjs,
  //==============================================================================
  // HammerOh
  // blcksock,
  IdTCPClient,
  //==============================================================================


  MQTTHeaders,
  MQTTReadThread;

type
  {$IF not declared(TBytes)}
    TBytes = array of Byte;
  {$IFEND}

  TMQTT = class
    private
    { Private Declarations }
      FClientID: UTF8String;
      FHostname: UTF8String;
      FPort: Integer;
      FMessageID: Integer;
      FisConnected: boolean;
      FRecvThread: TMQTTReadThread;
      FCSSock: TCriticalSection;

      FWillMsg: UTF8String;
      FWillTopic: UTF8String;
      FUsername: UTF8String;
      FPassword: UTF8String;
      //==============================================================================
      // HammerOh
      // FSocket: TTCPBlockSocket;
      FSocket: TIdTCPClient;
      //==============================================================================

      FKeepAliveTimer: TTimer;

      // Event Fields
      FConnAckEvent: TConnAckEvent;
      FPublishEvent: TPublishEvent;
      FPingRespEvent: TPingRespEvent;
      FPingReqEvent: TPingReqEvent;
      FSubAckEvent: TSubAckEvent;
      FUnSubAckEvent: TUnSubAckEvent;
      FPubAckEvent: TPubAckEvent;
      FPubRelEvent: TPubRelEvent;
      FPubRecEvent: TPubRecEvent;
      FPubCompEvent: TPubCompEvent;

      function WriteData(AData: TBytes): boolean;
      function hasWill: boolean;
      function getNextMessageId: integer;
      //==============================================================================
      // HammerOh
      // function createAndResumeRecvThread(Socket: TTCPBlockSocket): boolean;
      function createAndResumeRecvThread(var Socket: TIdTCPClient): boolean;
      //==============================================================================


      // TMQTTMessage Factory Methods.
      function ConnectMessage: TMQTTMessage;
      function DisconnectMessage: TMQTTMessage;
      function PublishMessage: TMQTTMessage;
      function PingReqMessage: TMQTTMessage;
      function SubscribeMessage: TMQTTMessage;
      function UnsubscribeMessage: TMQTTMessage;

      // Our Keep Alive Ping Timer Event
      procedure KeepAliveTimer_Event(sender: TObject);

      // Recv Thread Event Handling Procedures.
      procedure GotConnAck(Sender: TObject; ReturnCode: integer);
      procedure GotPingResp(Sender: TObject);
      procedure GotSubAck(Sender: TObject; MessageID: integer; GrantedQoS: Array of integer);
      procedure GotUnSubAck(Sender: TObject; MessageID: integer);
      procedure GotPub(Sender: TObject; topic, payload: UTF8String);
      procedure GotPubAck(Sender: TObject; MessageID: integer);
      procedure GotPubRec(Sender: TObject; MessageID: integer);
      procedure GotPubRel(Sender: TObject; MessageID: integer);
      procedure GotPubComp(Sender: TObject; MessageID: integer);

    public
    { Public Declarations }

      function Connect: boolean;
      function Disconnect: boolean;
      function Publish(Topic: UTF8String; sPayload: UTF8String): boolean; overload;
      function Publish(Topic: UTF8String; sPayload: UTF8String; Retain: boolean): boolean; overload;
      function Publish(Topic: UTF8String; sPayload: UTF8String; Retain: boolean; QoS: integer): boolean; overload;
      function Subscribe(Topic: UTF8String; RequestQoS: integer): integer; overload;
      function Subscribe(Topics: TDictionary<UTF8String, integer>): integer; overload;
      function Unsubscribe(Topic: UTF8String): integer; overload ;
      function Unsubscribe(Topics: TStringList): integer; overload;
      function PingReq: boolean;
      constructor Create(hostName: UTF8String; port: integer);
      destructor Destroy; override;

      property WillTopic: UTF8String read FWillTopic write FWillTopic;
      property WillMsg: UTF8String read FWillMsg write FWillMsg;

      property Username: UTF8String read FUsername write FUsername;
      property Password: UTF8String read FPassword write FPassword;
      // Client ID is our Client Identifier.
      property ClientID : UTF8String read FClientID write FClientID;
      property isConnected: boolean read FisConnected;

      // Event Handlers
      property OnConnAck : TConnAckEvent read FConnAckEvent write FConnAckEvent;
      property OnPublish : TPublishEvent read FPublishEvent write FPublishEvent;
      property OnPingResp : TPingRespEvent read FPingRespEvent write FPingRespEvent;
      property OnPingReq : TPingRespEvent read FPingRespEvent write FPingRespEvent;
      property OnSubAck : TSubAckEvent read FSubAckEvent write FSubAckEvent;
      property OnUnSubAck : TUnSubAckEvent read FUnSubAckEvent write FUnSubAckEvent;
      property OnPubAck : TUnSubAckEvent read FUnSubAckEvent write FUnSubAckEvent;
      property OnPubRec : TUnSubAckEvent read FUnSubAckEvent write FUnSubAckEvent;
      property OnPubRel : TUnSubAckEvent read FUnSubAckEvent write FUnSubAckEvent;
      property OnPubComp : TUnSubAckEvent read FUnSubAckEvent write FUnSubAckEvent;
  end;

implementation


{ TMQTTClient }

procedure TMQTT.GotConnAck(Sender: TObject; ReturnCode: integer);
begin
  if Assigned(FConnAckEvent) then OnConnAck(Self, ReturnCode);
end;

function TMQTT.Connect: boolean;
var
  Msg: TMQTTMessage;
begin
  // Create socket and connect.
  //==============================================================================
  // HammerOh
  // FSocket := TTCPBlockSocket.Create;
  FSocket := TIdTCPClient.Create(nil);
  //==============================================================================

  try
    //==============================================================================
    // HammerOh
    // FSocket.Connect(Self.FHostname, IntToStr(Self.FPort));
    FSocket.Host := Self.FHostname;
    FSocket.Port := Self.FPort;
    FSocket.Connect;
    //==============================================================================

    FisConnected := true;
  except
    // If we encounter an exception upon connection then reraise it, free the socket
    // and reset our isConnected flag.
    on E: Exception do
      begin
        raise;
        FisConnected := false;
        FSocket.Free;
      end;
  end;

  if FisConnected then
  begin
    Msg := ConnectMessage;
    try
      Msg.Payload.Contents.Add(Self.FClientID);
      (Msg.VariableHeader as TMQTTConnectVarHeader).WillFlag := ord(hasWill);
      if hasWill then
      begin
        Msg.Payload.Contents.Add(Self.FWillTopic);
        Msg.Payload.Contents.Add(Self.FWillMsg);
      end;

      if ((Length(FUsername) > 1) and (Length(FPassword) > 1)) then
      begin
        Msg.Payload.Contents.Add(FUsername);
        Msg.Payload.Contents.Add(FPassword);
      end;


      if WriteData(Msg.ToBytes) then Result := true else Result := false;
      // Start our Receive thread.
      if (Result and createAndResumeRecvThread(FSocket)) then
      begin
        // Use the KeepAlive that we just sent to determine our ping timer.
        FKeepAliveTimer.Interval := (Round((Msg.VariableHeader as TMQTTConnectVarHeader).KeepAlive * 0.80)) * 1000;
        FKeepAliveTimer.Enabled := true;
      end;

    finally
      Msg.Free;
    end;
  end;
end;

function TMQTT.ConnectMessage: TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.VariableHeader := TMQTTConnectVarHeader.Create;
  Result.Payload := TMQTTPayload.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.CONNECT);
  Result.FixedHeader.Retain := 0;
  Result.FixedHeader.QoSLevel := 0;
  Result.FixedHeader.Duplicate := 0;
end;

constructor TMQTT.Create(hostName: UTF8String; port: integer);
begin
  inherited Create;

  Self.FisConnected := false;
  Self.FHostname := Hostname;
  Self.FPort := Port;
  Self.FMessageID := 1;
  // Randomise and create a random client id.
  Randomize;
  Self.FClientID := 'TMQTT' + IntToStr(Random(1000) + 1);
  FCSSock := TCriticalSection.Create;

  // Create the timer responsible for pinging.
  FKeepAliveTimer := TTimer.Create(nil);
  FKeepAliveTimer.Enabled := false;
  FKeepAliveTimer.OnTimer := KeepAliveTimer_Event;
end;
//==============================================================================
// HammerOh
// function TMQTT.createAndResumeRecvThread(Socket: TTCPBlockSocket): boolean;
function TMQTT.createAndResumeRecvThread(var Socket: TIdTCPClient): boolean;
//==============================================================================
begin
  Result := false;
  try
    FRecvThread := TMQTTReadThread.Create(Socket, FCSSock);

    { Todo: Assign Event Handlers here.   }
    FRecvThread.OnConnAck := Self.GotConnAck;
    FRecvThread.OnPublish := Self.GotPub;
    FRecvThread.OnPingResp := Self.GotPingResp;
    FRecvThread.OnSubAck := Self.GotSubAck;
    FRecvThread.OnPubAck := Self.GotPubAck;
    Result := true;
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

function TMQTT.Disconnect: boolean;
var
  Msg: TMQTTMessage;
begin
  Result := false;
  if isConnected then
    begin
      FKeepAliveTimer.Enabled := false;
      Msg := DisconnectMessage;
      if WriteData(Msg.ToBytes) then Result := true else Result := false;
      Msg.Free;
      // Terminate our socket receive thread.
      FRecvThread.Terminate;
      FRecvThread.WaitFor;

      // Close our socket.
      //==============================================================================
      // HammerOh
      // FSocket.CloseSocket;
      FSocket.Disconnect;
      //==============================================================================

      FisConnected := False;

      // Free everything.
	    if Assigned(FRecvThread) then FreeAndNil(FRecvThread);
      if Assigned(FSocket) then FreeAndNil(FSocket);
    end;
end;

function TMQTT.DisconnectMessage: TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.DISCONNECT);
end;

function TMQTT.getNextMessageId: integer;
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

function TMQTT.hasWill: boolean;
begin
  if ((Length(FWillTopic) < 1) and (Length(FWillMsg) < 1)) then
    begin
      Result := false;
    end else Result := true;
end;

procedure TMQTT.KeepAliveTimer_Event(sender: TObject);
begin
  if Self.isConnected then
    begin
      PingReq;
    end;
end;

function TMQTT.PingReq: boolean;
var
  Msg: TMQTTMessage;
begin
  Result := false;
  if isConnected then
  begin
    Msg := PingReqMessage;
    if WriteData(Msg.ToBytes) then Result := true else Result := false;
    Msg.Free;
  end;
end;

function TMQTT.PingReqMessage: TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.PINGREQ);
end;

procedure TMQTT.GotPingResp(Sender: TObject);
begin
  if Assigned(FPingRespEvent) then OnPingResp(Self);
end;

function TMQTT.Publish(Topic, sPayload: UTF8String; Retain: boolean): boolean;
begin
  Result := Publish(Topic, sPayload, Retain, 0);
end;

function TMQTT.Publish(Topic, sPayload: UTF8String): boolean;
begin
  Result := Publish(Topic, sPayload, false, 0);
end;

function TMQTT.Publish(Topic, sPayload: UTF8String; Retain: boolean;
  QoS: integer): boolean;
var
  Msg: TMQTTMessage;
begin
  if ((QoS > -1) and (QoS <= 3)) then
  begin
    if isConnected then
    begin
       Msg := PublishMessage;
       Msg.FixedHeader.QoSLevel := QoS;
       (Msg.VariableHeader as TMQTTPublishVarHeader).QoSLevel := QoS;
       (Msg.VariableHeader as TMQTTPublishVarHeader).Topic := Topic;
       if (QoS > 0) then
        begin
          (Msg.VariableHeader as TMQTTPublishVarHeader).MessageID := getNextMessageId;
        end;
       Msg.Payload.Contents.Add(sPayload);
       Msg.Payload.PublishMessage := true;
       if WriteData(Msg.ToBytes) then Result := true else Result := false;
       Msg.Free;
    end;
  end
  else
    raise EInvalidOp.Create('QoS level can only be equal to or between 0 and 3.');
end;

function TMQTT.PublishMessage: TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.PUBLISH);
  Result.VariableHeader := TMQTTPublishVarHeader.Create(0);
  Result.Payload := TMQTTPayload.Create;
end;

procedure TMQTT.GotPubRec(Sender: TObject; MessageID: integer);
begin
  if Assigned(FPubRecEvent) then OnPubRec(Self, MessageID);
end;

procedure TMQTT.GotPubRel(Sender: TObject; MessageID: integer);
begin
  if Assigned(FPubRelEvent) then OnPubRel(Self, MessageID);
end;

function TMQTT.Subscribe(Topic: UTF8String; RequestQoS: integer): integer;
var
  dTopics: TDictionary<UTF8String, integer>;
begin
  dTopics := TDictionary<UTF8String, integer>.Create;
  dTopics.Add(Topic, RequestQoS);
  Result := Subscribe(dTopics);
  dTopics.Free;
end;

procedure TMQTT.GotSubAck(Sender: TObject; MessageID: integer;
  GrantedQoS: array of integer);
begin
  if Assigned(FSubAckEvent) then OnSubAck(Self, MessageID, GrantedQoS);
end;

function TMQTT.Subscribe(Topics: TDictionary<UTF8String, integer>): integer;
var
  Msg: TMQTTMessage;
  MsgId: Integer;
  sTopic: UTF8String;
  data: TBytes;
begin
  Result := -1;
  if isConnected then
    begin
      Msg := SubscribeMessage;
      MsgId := getNextMessageId;
      (Msg.VariableHeader as TMQTTSubscribeVarHeader).MessageID := MsgId;

      for sTopic in Topics.Keys do
      begin
        Msg.Payload.Contents.Add(sTopic);
        Msg.Payload.Contents.Add(IntToStr(Topics.Items[sTopic]))
      end;
      // the subscribe message contains integer literals not encoded as UTF8Strings.
      Msg.Payload.ContainsIntLiterals := true;

      data := Msg.ToBytes;
      if WriteData(data) then Result := MsgId;

      Msg.Free;
    end;
end;

function TMQTT.SubscribeMessage: TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.SUBSCRIBE);
  Result.FixedHeader.QoSLevel := 0;
  Result.VariableHeader := TMQTTSubscribeVarHeader.Create;
  Result.Payload := TMQTTPayload.Create;
end;

function TMQTT.Unsubscribe(Topic: UTF8String): integer;
var
  slTopics: TStringList;
begin
  slTopics := TStringList.Create;
  slTopics.Add(Topic);
  Result := Unsubscribe(slTopics);
  slTopics.Free;
end;

procedure TMQTT.GotUnSubAck(Sender: TObject; MessageID: integer);
begin
  if Assigned(FUnSubAckEvent) then OnUnSubAck(Self, MessageID);
end;

function TMQTT.Unsubscribe(Topics: TStringList): integer;
var
  Msg: TMQTTMessage;
  MsgId: integer;
  sTopic: UTF8String;
begin
  Result := -1;
  if isConnected then
    begin
      Msg := UnsubscribeMessage;
      MsgId := getNextMessageId;
      (Msg.VariableHeader as TMQTTSubscribeVarHeader).MessageID := MsgId;

      Msg.Payload.Contents.AddStrings(Topics);

      if WriteData(Msg.ToBytes) then Result := MsgId;

      Msg.Free;
    end;
end;

function TMQTT.UnsubscribeMessage: TMQTTMessage;
var
  Msg: TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.UNSUBSCRIBE);
  Result.FixedHeader.QoSLevel := 1;
  Result.VariableHeader := TMQTTUnsubscribeVarHeader.Create;
  Result.Payload := TMQTTPayload.Create;
end;

function TMQTT.WriteData(AData: TBytes): boolean;
var
  sentData: integer;
  attemptsToWrite: integer;
  dataStream: TMemoryStream;
begin
  Result := False;
  sentData := 0;
  attemptsToWrite := 1;
  if isConnected then
  begin
    //==============================================================================
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
      dataStream := TMemoryStream.Create;
      dataStream.Position := 0;
      dataStream.WriteBuffer(AData, Length(AData));
      FSocket.IOHandler.Write(dataStream);
      dataStream.Free;
      Result := True;
      FisConnected := true;
    except
      Result := False;
      FisConnected := false;
    end;
    //==============================================================================
  end;
end;


procedure TMQTT.GotPub(Sender: TObject; topic, payload: UTF8String);
begin
  if Assigned(FPublishEvent) then OnPublish(Self, topic, payload);
end;

procedure TMQTT.GotPubAck(Sender: TObject; MessageID: integer);
begin
  if Assigned(FPubAckEvent) then OnPubAck(Self, MessageID);
end;

procedure TMQTT.GotPubComp(Sender: TObject; MessageID: integer);
begin
  if Assigned(FPubCompEvent) then OnPubComp(Self, MessageID);
end;

end.

