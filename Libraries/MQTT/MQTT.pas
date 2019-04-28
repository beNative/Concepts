unit MQTT;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.SyncObjs,
  System.Generics.Collections,

  Vcl.ExtCtrls,

  // ==============================================================================
  // HammerOh
  // blcksock,
  IdTCPClient,
  // ==============================================================================

  MQTTHeaders, MQTTReadThread;

type
  TMQTT = class
  private
    FClientId   : UTF8String;
    FHostname   : string;
    FPort       : Integer;
    FMessageId  : Integer;
    FConnected  : Boolean;
    FRecvThread : TMQTTReadThread;
    FCSSock     : TCriticalSection;
    FWillMsg    : UTF8String;
    FWillTopic  : UTF8String;
    FUsername   : UTF8String;
    FPassword   : UTF8String;
    // ==============================================================================
    // HammerOh
    // FSocket: TTCPBlockSocket;
    FSocket : TIdTCPClient;
    // ==============================================================================

    FKeepAliveTimer : TTimer;
    FConnAckEvent   : TConnAckEvent;
    FPublishEvent   : TPublishEvent;
    FPingRespEvent  : TPingRespEvent;
    FPingReqEvent   : TPingReqEvent;
    FSubAckEvent    : TSubAckEvent;
    FUnSubAckEvent  : TUnSubAckEvent;
    FPubAckEvent    : TPubAckEvent;
    FPubRelEvent    : TPubRelEvent;
    FPubRecEvent    : TPubRecEvent;
    FPubCompEvent   : TPubCompEvent;

    function WriteData(AData: TBytes): Boolean;
    function HasWill: Boolean;
    function GetNextMessageId: Integer;
    // ==============================================================================
    // HammerOh
    // function createAndResumeRecvThread(Socket: TTCPBlockSocket): boolean;
    function CreateAndResumeRecvThread(var ASocket: TIdTCPClient): Boolean;
    // ==============================================================================

    // TMQTTMessage Factory Methods.
    function ConnectMessage: TMQTTMessage;
    function DisconnectMessage: TMQTTMessage;
    function PublishMessage: TMQTTMessage;
    function PingReqMessage: TMQTTMessage;
    function SubscribeMessage: TMQTTMessage;
    function UnsubscribeMessage: TMQTTMessage;

    procedure FKeepAliveTimerTimer(Sender: TObject);

    // Recv Thread Event Handling Procedures.
    procedure GotConnAck(
      Sender      : TObject;
      AReturnCode : Integer
    );
    procedure GotPingResp(Sender: TObject);
    procedure GotSubAck(
      Sender      : TObject;
      AMessageId  : Integer;
      AGrantedQoS : array of Integer
    );
    procedure GotUnSubAck(
      Sender     : TObject;
      AMessageId : Integer
    );
    procedure GotPub(
      Sender   : TObject;
      ATopic   : UTF8String;
      APayload : UTF8String
    );
    procedure GotPubAck(
      Sender     : TObject;
      AMessageId : Integer
    );
    procedure GotPubRec(
      Sender     : TObject;
      AMessageId : Integer
    );
    procedure GotPubRel(
      Sender     : TObject;
      AMessageId : Integer
    );
    procedure GotPubComp(
      Sender     : TObject;
      AMessageId : Integer
    );

  public
    function Connect: Boolean;
    function Disconnect: Boolean;

    function Publish(
      ATopic   : UTF8String;
      APayload : UTF8String
    ): Boolean; overload;
    function Publish(
      ATopic   : UTF8String;
      APayload : UTF8String;
      ARetain  : Boolean
    ): Boolean; overload;
    function Publish(
      ATopic   : UTF8String;
      APayload : UTF8String;
      ARetain  : Boolean;
      AQoS     : Integer
    ): Boolean; overload;

    function Subscribe(
      ATopic      : UTF8String;
      ARequestQoS : Integer
    ): Integer; overload;
    function Subscribe(ATopics: TDictionary<UTF8String, Integer>): Integer;
      overload;

    function Unsubscribe(ATopic: UTF8String): Integer; overload;
    function Unsubscribe(ATopics: TStringList): Integer; overload;

    function PingReq: Boolean;

    constructor Create(
      const AHostName : string;
      APort           : Integer
    );
    destructor Destroy; override;

    property WillTopic: UTF8String
      read FWillTopic write FWillTopic;

    property WillMsg: UTF8String
      read FWillMsg write FWillMsg;

    property Username: UTF8String
      read FUsername write FUsername;

    property Password: UTF8String
      read FPassword write FPassword;

    property ClientId: UTF8String
      read FClientId write FClientId;

    property Connected: Boolean
      read FConnected;

    property OnConnAck: TConnAckEvent
      read FConnAckEvent write FConnAckEvent;

    property OnPublish: TPublishEvent
      read FPublishEvent write FPublishEvent;

    property OnPingResp: TPingRespEvent
      read FPingRespEvent write FPingRespEvent;

    property OnPingReq: TPingRespEvent
      read FPingRespEvent write FPingRespEvent;

    property OnSubAck: TSubAckEvent
      read FSubAckEvent write FSubAckEvent;

    property OnUnSubAck: TUnSubAckEvent
      read FUnSubAckEvent write FUnSubAckEvent;

    property OnPubAck: TUnSubAckEvent
      read FUnSubAckEvent write FUnSubAckEvent;

    property OnPubRec: TUnSubAckEvent
      read FUnSubAckEvent write FUnSubAckEvent;

    property OnPubRel: TUnSubAckEvent
      read FUnSubAckEvent write FUnSubAckEvent;

    property OnPubComp: TUnSubAckEvent
      read FUnSubAckEvent write FUnSubAckEvent;
  end;

implementation

procedure TMQTT.GotConnAck(Sender: TObject; AReturnCode: Integer);
begin
  if Assigned(FConnAckEvent) then
    OnConnAck(Self, AReturnCode);
end;

function TMQTT.Connect: Boolean;
var
  LMsg : TMQTTMessage;
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
    FSocket.Host := FHostname;
    FSocket.Port := FPort;
    FSocket.Connect;
    // ==============================================================================
    FConnected := True;
  except
    // If we encounter an exception upon connection then reraise it, free the socket
    // and reset our isConnected flag.
    on E: Exception do
    begin
      raise;
      FConnected := False;
      FSocket.Free;
    end;
  end;

  if FConnected then
  begin
    LMsg := ConnectMessage;
    try
      LMsg.Payload.Contents.Add(Self.FClientId);
      (LMsg.VariableHeader as TMQTTConnectVarHeader).WillFlag := Ord(HasWill);
      if HasWill then
      begin
        LMsg.Payload.Contents.Add(Self.FWillTopic);
        LMsg.Payload.Contents.Add(Self.FWillMsg);
      end;

      if (Length(FUsername) > 1) and (Length(FPassword) > 1) then
      begin
        LMsg.Payload.Contents.Add(FUsername);
        LMsg.Payload.Contents.Add(FPassword);
      end;

      if WriteData(LMsg.ToBytes) then
        Result := True
      else
        Result := False;
      // Start our Receive thread.
      if (Result and CreateAndResumeRecvThread(FSocket)) then
      begin
        // Use the KeepAlive that we just sent to determine our ping timer.
        FKeepAliveTimer.Interval :=
          (Round((LMsg.VariableHeader as TMQTTConnectVarHeader).KeepAlive *
              0.80)) * 1000;
        FKeepAliveTimer.Enabled := True;
      end;
    finally
      LMsg.Free;
    end;
  end;
end;

function TMQTT.ConnectMessage: TMQTTMessage;
begin
  Result                         := TMQTTMessage.Create;
  Result.VariableHeader          := TMQTTConnectVarHeader.Create;
  Result.Payload                 := TMQTTPayload.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.Connect);
  Result.FixedHeader.Retain      := 0;
  Result.FixedHeader.QoSLevel    := 0;
  Result.FixedHeader.Duplicate   := 0;
end;

constructor TMQTT.Create(const AHostName: string; APort: Integer);
begin
  inherited Create;

  FConnected := False;
  FHostname  := AHostName;
  FPort      := APort;
  FMessageId := 1;
  // Randomise and create a random client id.
  Randomize;
  FClientId := 'TMQTT' + IntToStr(Random(1000) + 1);
  FCSSock        := TCriticalSection.Create;

  // Create the timer responsible for pinging.
  FKeepAliveTimer         := TTimer.Create(nil);
  FKeepAliveTimer.Enabled := False;
  FKeepAliveTimer.OnTimer := FKeepAliveTimerTimer;
end;

// ==============================================================================
// HammerOh
// function TMQTT.createAndResumeRecvThread(Socket: TTCPBlockSocket): boolean;
function TMQTT.CreateAndResumeRecvThread(var ASocket: TIdTCPClient): Boolean;
// ==============================================================================
begin
  Result := False;
  try
    FRecvThread := TMQTTReadThread.Create(ASocket, FCSSock);

    { Todo: Assign Event Handlers here. }
    FRecvThread.OnConnAck  := GotConnAck;
    FRecvThread.OnPublish  := GotPub;
    FRecvThread.OnPingResp := GotPingResp;
    FRecvThread.OnSubAck   := GotSubAck;
    FRecvThread.OnPubAck   := GotPubAck;
    Result                 := True;
  except
    Result := False;
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
  inherited Destroy;
end;

function TMQTT.Disconnect: Boolean;
var
  LMsg : TMQTTMessage;
begin
  Result := False;
  if Connected then
  begin
    FKeepAliveTimer.Enabled := False;
    LMsg                     := DisconnectMessage;
    if WriteData(LMsg.ToBytes) then
      Result := True
    else
      Result := False;
    LMsg.Free;
    // Terminate our socket receive thread.
    FRecvThread.Terminate;
    FRecvThread.WaitFor;
    // Close our socket.
    // ==============================================================================
    // HammerOh
    // FSocket.CloseSocket;
    FSocket.Disconnect;
    // ==============================================================================
    FConnected := False;
    // Free everything.
    if Assigned(FRecvThread) then
      FreeAndNil(FRecvThread);
    if Assigned(FSocket) then
      FreeAndNil(FSocket);
  end;
end;

function TMQTT.DisconnectMessage: TMQTTMessage;
begin
  Result                         := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.Disconnect);
end;

function TMQTT.GetNextMessageId: Integer;
begin
  // If we've reached the upper bounds of our 16 bit unsigned message Id then
  // start again. The spec says it typically does but is not required to Inc(MsgId,1).
  if FMessageId = 65535 then
  begin
    FMessageId := 1;
  end;

  // Return our current message Id
  Result := FMessageId;
  // Increment message Id
  Inc(FMessageId);
end;

function TMQTT.HasWill: Boolean;
begin
  if (Length(FWillTopic) < 1) and (Length(FWillMsg) < 1) then
  begin
    Result := False;
  end
  else
    Result := True;
end;

procedure TMQTT.FKeepAliveTimerTimer(Sender: TObject);
begin
  if Connected then
  begin
    PingReq;
  end;
end;

function TMQTT.PingReq: Boolean;
var
  Msg : TMQTTMessage;
begin
  Result := False;
  if Connected then
  begin
    Msg := PingReqMessage;
    if WriteData(Msg.ToBytes) then
      Result := True
    else
      Result := False;
    Msg.Free;
  end;
end;

function TMQTT.PingReqMessage: TMQTTMessage;
begin
  Result                         := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.PingReq);
end;

procedure TMQTT.GotPingResp(Sender: TObject);
begin
  if Assigned(FPingRespEvent) then
    OnPingResp(Self);
end;

function TMQTT.Publish(ATopic, APayload: UTF8String; ARetain: Boolean): Boolean;
begin
  Result := Publish(ATopic, APayload, ARetain, 0);
end;

function TMQTT.Publish(ATopic, APayload: UTF8String): Boolean;
begin
  Result := Publish(ATopic, APayload, False, 0);
end;

function TMQTT.Publish(ATopic, APayload: UTF8String; ARetain: Boolean;
  AQoS: Integer): Boolean;
var
  LMsg : TMQTTMessage;
begin
  if (AQoS > -1) and (AQoS <= 3) then
  begin
    if Connected then
    begin
      LMsg                      := PublishMessage;
      LMsg.FixedHeader.QoSLevel := AQoS;
      (LMsg.VariableHeader as TMQTTPublishVarHeader).QoSLevel := AQoS;
      (LMsg.VariableHeader as TMQTTPublishVarHeader).Topic := ATopic;
      if AQoS > 0 then
      begin
        (LMsg.VariableHeader as TMQTTPublishVarHeader).MessageId :=
          GetNextMessageId;
      end;
      LMsg.Payload.Contents.Add(APayload);
      LMsg.Payload.PublishMessage := True;
      if WriteData(LMsg.ToBytes) then
        Result := True
      else
        Result := False;
      LMsg.Free;
    end;
  end
  else
    raise EInvalidOp.Create
      ('QoS level can only be equal to or between 0 and 3.');
end;

function TMQTT.PublishMessage: TMQTTMessage;
begin
  Result                         := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.Publish);
  Result.VariableHeader          := TMQTTPublishVarHeader.Create(0);
  Result.Payload                 := TMQTTPayload.Create;
end;

procedure TMQTT.GotPubRec(Sender: TObject; AMessageId: Integer);
begin
  if Assigned(FPubRecEvent) then
    OnPubRec(Self, AMessageId);
end;

procedure TMQTT.GotPubRel(Sender: TObject; AMessageId: Integer);
begin
  if Assigned(FPubRelEvent) then
    OnPubRel(Self, AMessageId);
end;

function TMQTT.Subscribe(ATopic: UTF8String; ARequestQoS: Integer): Integer;
var
  LTopics : TDictionary<UTF8String, Integer>;
begin
  LTopics := TDictionary<UTF8String, Integer>.Create;
  LTopics.Add(ATopic, ARequestQoS);
  Result := Subscribe(LTopics);
  LTopics.Free;
end;

procedure TMQTT.GotSubAck(Sender: TObject; AMessageId: Integer;
  AGrantedQoS: array of Integer);
begin
  if Assigned(FSubAckEvent) then
    OnSubAck(Self, AMessageId, AGrantedQoS);
end;

function TMQTT.Subscribe(ATopics: TDictionary<UTF8String, Integer>): Integer;
var
  LMsg   : TMQTTMessage;
  LMsgId : Integer;
  LTopic : UTF8String;
  LData  : TBytes;
begin
  Result := -1;
  if Connected then
  begin
    LMsg   := SubscribeMessage;
    LMsgId := GetNextMessageId;
    (LMsg.VariableHeader as TMQTTSubscribeVarHeader).MessageId := LMsgId;

    for LTopic in ATopics.Keys do
    begin
      LMsg.Payload.Contents.Add(LTopic);
      LMsg.Payload.Contents.Add(IntToStr(ATopics.Items[LTopic]))
    end;
    // the subscribe message contains integer literals not encoded as UTF8Strings.
    LMsg.Payload.ContainsIntLiterals := True;

    LData := LMsg.ToBytes;
    if WriteData(LData) then
      Result := LMsgId;

    LMsg.Free;
  end;
end;

function TMQTT.SubscribeMessage: TMQTTMessage;
begin
  Result                         := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.Subscribe);
  Result.FixedHeader.QoSLevel    := 0;
  Result.VariableHeader          := TMQTTSubscribeVarHeader.Create;
  Result.Payload                 := TMQTTPayload.Create;
end;

function TMQTT.Unsubscribe(ATopic: UTF8String): Integer;
var
  LTopics : TStringList;
begin
  LTopics := TStringList.Create;
  LTopics.Add(ATopic);
  Result := Unsubscribe(LTopics);
  LTopics.Free;
end;

procedure TMQTT.GotUnSubAck(Sender: TObject; AMessageId: Integer);
begin
  if Assigned(FUnSubAckEvent) then
    OnUnSubAck(Self, AMessageId);
end;

function TMQTT.Unsubscribe(ATopics: TStringList) : Integer;
var
  LMsg   : TMQTTMessage;
  LMsgId : Integer;
begin
  Result := -1;
  if Connected then
  begin
    LMsg   := UnsubscribeMessage;
    LMsgId := GetNextMessageId;
    (LMsg.VariableHeader as TMQTTSubscribeVarHeader).MessageId := LMsgId;

    LMsg.Payload.Contents.AddStrings(ATopics);

    if WriteData(LMsg.ToBytes) then
      Result := LMsgId;

    LMsg.Free;
  end;
end;

function TMQTT.UnsubscribeMessage: TMQTTMessage;
begin
  Result                         := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.Unsubscribe);
  Result.FixedHeader.QoSLevel    := 1;
  Result.VariableHeader          := TMQTTUnsubscribeVarHeader.Create;
  Result.Payload                 := TMQTTPayload.Create;
end;

function TMQTT.WriteData(AData: TBytes) : Boolean;
var
  LSentData      : Integer;
  LWriteAttempts : Integer;
  LDataStream    : TMemoryStream;
begin
  Result         := False;
  LWriteAttempts := 1;
  if Connected then
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
      LDataStream          := TMemoryStream.Create;
      LDataStream.Position := 0;
      LDataStream.WriteBuffer(AData, Length(AData));
      FSocket.IOHandler.Write(LDataStream);
      LDataStream.Free;
      Result       := True;
      FConnected := True;
    except
      Result       := False;
      FConnected := False;
    end;
    // ==============================================================================
  end;
end;

procedure TMQTT.GotPub(Sender: TObject; ATopic, APayload: UTF8String);
begin
  if Assigned(FPublishEvent) then
    OnPublish(Self, ATopic, APayload);
end;

procedure TMQTT.GotPubAck(Sender: TObject; AMessageId: Integer);
begin
  if Assigned(FPubAckEvent) then
    OnPubAck(Self, AMessageId);
end;

procedure TMQTT.GotPubComp(Sender: TObject; AMessageId: Integer);
begin
  if Assigned(FPubCompEvent) then
    OnPubComp(Self, AMessageId);
end;

end.
