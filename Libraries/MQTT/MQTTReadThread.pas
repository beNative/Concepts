unit MQTTReadThread;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils, System.SyncObjs,

  // ==============================================================================
  // HammerOh
  // blcksock;
  IdTCPClient, IdGlobal,
  // ==============================================================================
  MQTTHeaders;

type
  // ==============================================================================
  // HammerOh
  // PTCPBlockSocket = ^TTCPBlockSocket;
  // ==============================================================================

  TMQTTRecvUtilities = class
  public
    class function MSBLSBToInt(ALengthBytes: TBytes): Integer;
    class function RLBytesToInt(ARlBytes: TBytes): Integer;
  end;

  TUnparsedMsg = record
  public
    FixedHeader : Byte;
    RL          : TBytes;
    Data        : TBytes;
  end;

  TMQTTReadThread = class(TThread)
  private
    { Private declarations }
    // ==============================================================================
    // HammerOh
    // FPSocket: TTCPBlockSocket;
    FPSocket : TIdTCPClient;
    // ==============================================================================

    FCSock      : TCriticalSection;
    FCurrentMsg : TUnparsedMsg;

    FCurrentRecvState : TMQTTRecvState;
    // Events
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

    procedure ProcessMessage;
    function ReadSingleString(
      const ADataStream   : TBytes;
      const AIndexStartAt : Integer;
      var AStringRead     : string
    ) : Integer;
//    function ReadMessageId(
//      const ADataStream   : TBytes;
//      const AIndexStartAt : Integer;
//      var AMessageId      : Integer
//    ) : Integer;
    function ReadStringWithoutPrefix(
      const ADataStream   : TBytes;
      const AIndexStartAt : Integer;
      var AStringRead     : string
    ) : Integer;

  protected
    procedure CleanStart;
    procedure Execute; override;

  public
    // ==============================================================================
    // HammerOh
    // constructor Create(Socket: TTCPBlockSocket; CSSock: TCriticalSection);
    constructor Create(
      var Socket : TIdTCPClient;
      var CSSock : TCriticalSection);
    // ==============================================================================

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



{$REGION 'non-interfaced routines'}
procedure AppendBytes(var ADestArray: TBytes; const ANewBytes: TBytes);
var
  LDestLen : Integer;
begin
  if Length(ANewBytes) > 0 then
  begin
    LDestLen := Length(ADestArray);
    SetLength(ADestArray, LDestLen + Length(ANewBytes));
    Move(ANewBytes[0], ADestArray[LDestLen], Length(ANewBytes));
  end;
end;
{$ENDREGION}

{$REGION 'TMQTTRecvUtilities'}
class function TMQTTRecvUtilities.MSBLSBToInt(ALengthBytes: TBytes): Integer;
begin
  Assert(ALengthBytes <> nil, 'Must not pass nil to this method');
  Assert(Length(ALengthBytes) = 2,
    'The MSB-LSB 2 bytes structure must be 2 Bytes in length');

  Result := ALengthBytes[0] shl 8;
  Result := Result + ALengthBytes[1];
end;

class function TMQTTRecvUtilities.RLBytesToInt(ARlBytes: TBytes): Integer;
var
  LMulti : Integer;
  I      : Integer;
  LDigit : Byte;
begin
  Assert(ARlBytes <> nil, 'Must not pass nil to this method');

  LMulti  := 1;
  I      := 0;
  Result := 0;

  if ((Length(ARlBytes) > 0) and (Length(ARlBytes) <= 4)) then
  begin
    repeat
      LDigit  := ARlBytes[I];
      Result := Result + (LDigit and 127) * LMulti;
      LMulti  := LMulti * 128;
      Inc(I);
    until ((LDigit and 128) = 0);
  end;
end;
{$ENDREGION}

{$REGION 'TMQTTReadThread'}
procedure TMQTTReadThread.CleanStart;
begin
  FCurrentRecvState := TMQTTRecvState.FixedHeaderByte;
end;

// ==============================================================================
// HammerOh
// constructor TMQTTReadThread.Create(Socket: TTCPBlockSocket; CSSock: TCriticalSection);
constructor TMQTTReadThread.Create(var Socket: TIdTCPClient;
  var CSSock: TCriticalSection);
// ==============================================================================
begin
  inherited Create(False);

  FPSocket        := Socket;
  FCSock          := CSSock;
  FreeOnTerminate := False;

  CleanStart;
end;

procedure TMQTTReadThread.Execute;
var
  LCurrentMessage : TUnparsedMsg;
  LRLInt          : Integer;
  LBuffer         : TBytes;
  I               : Integer;
  // ==============================================================================
  // HammerOh
  LSize    : Integer;
  LIdBuffer : TIdBytes;
  LCount   : Integer;
  LIndex   : Integer;
  // ==============================================================================
begin
  LIndex := 0;
  LCount := 0;
  LRLInt := 0;
  while not Terminated do
  begin
    //try
      if FPSocket.IOHandler.CheckForDataOnSource(1000) then
        if not FPSocket.IOHandler.InputBufferIsEmpty then
        begin
          LSize    := FPSocket.IOHandler.InputBuffer.Size;
          LIdBuffer := nil;
          FPSocket.IOHandler.ReadBytes(LIdBuffer, LSize);
          for I := 0 to LSize - 1 do
          begin
            case FCurrentRecvState of
              TMQTTRecvState.FixedHeaderByte :
                begin
                  LCurrentMessage.FixedHeader := 0;
                  LCurrentMessage.FixedHeader := LIdBuffer[I];
                  if (LCurrentMessage.FixedHeader <> 0) then
                    FCurrentRecvState := TMQTTRecvState.RemainingLength;
                end;

              TMQTTRecvState.RemainingLength :
                begin
                  LRLInt := 0;
                  SetLength(LCurrentMessage.RL, 1);
                  SetLength(LBuffer, 1);
                  LCurrentMessage.RL[0] := LIdBuffer[I];
                  if LCurrentMessage.RL[0] = 0 then
                  begin
                    FCurrentMsg := LCurrentMessage;
                    Synchronize(ProcessMessage);
                    LCurrentMessage    := default (TUnparsedMsg);
                    FCurrentRecvState := TMQTTRecvState.FixedHeaderByte;
                  end
                  else
                  begin
                    FCurrentRecvState := TMQTTRecvState.RemainingLength1;
                    LIndex          := 0;
                  end;
                end;

              TMQTTRecvState.RemainingLength1 :
                begin
                  if ((LCurrentMessage.RL[LIndex] and 128) <> 0) then
                  begin
                    LBuffer[0] := LIdBuffer[I];
                    AppendBytes(LCurrentMessage.RL, LBuffer);
                    Inc(LIndex);
                  end
                  else
                  begin
                    LIndex := 5;
                  end;

                  if (LIndex = 4) or (LIndex = 5) then
                  begin
                    LRLInt := TMQTTRecvUtilities.RLBytesToInt(LCurrentMessage.RL);
                    if LRLInt > 0 then
                    begin
                      SetLength(LCurrentMessage.Data, LRLInt);
                      LCount := 0;
                      if LIndex = 5 then
                      begin
                        LCurrentMessage.Data[LCount] := LIdBuffer[I];
                        Inc(LCount);
                      end;
                      FCurrentRecvState := TMQTTRecvState.Data;
                    end
                    else
                    begin
                      FCurrentMsg := LCurrentMessage;
                      Synchronize(ProcessMessage);
                      LCurrentMessage    := default (TUnparsedMsg);
                      FCurrentRecvState := TMQTTRecvState.FixedHeaderByte;
                    end;
                  end;
                end;

              TMQTTRecvState.Data :
                begin
                  LCurrentMessage.Data[LCount] := LIdBuffer[I];
                  Inc(LCount);
                  if LCount = LRLInt then
                  begin
                    FCurrentMsg := LCurrentMessage;
                    Synchronize(ProcessMessage);
                    LCurrentMessage    := default (TUnparsedMsg);
                    FCurrentRecvState := TMQTTRecvState.FixedHeaderByte;
                  end;
                end;
            end;
          end;
        end;
//    except
//
//    end;

    (*
      FCSock.Acquire;
      try
      if FPSocket.WaitingDataEx > 0 then
      begin
      case FCurrentRecvState of
      TMQTTRecvState.FixedHeaderByte:
      begin
      CurrentMessage.FixedHeader := 0;
      CurrentMessage.FixedHeader := FPSocket.RecvByte(1000);
      if ((FPSocket.LastError = 0) and (CurrentMessage.FixedHeader <> 0)) then FCurrentRecvState := TMQTTRecvState.RemainingLength;
      end;
      TMQTTRecvState.RemainingLength:
      begin
      RLInt := 0;

      SetLength(CurrentMessage.RL, 1);
      SetLength(Buffer, 1);
      CurrentMessage.RL[0] := FPSocket.RecvByte(1000);
      for i := 1 to 4 do
      begin
      if (( CurrentMessage.RL[i - 1] and 128) <> 0) then
      begin
      Buffer[0] := FPSocket.PeekByte(1000);
      AppendBytes(CurrentMessage.RL, Buffer);
      end else Break;
      end;

      RLInt := TMQTTRecvUtilities.RLBytesToInt(CurrentMessage.RL);

      if (FPSocket.LastError = 0) then FCurrentRecvState := TMQTTRecvState.Data;
      end;
      TMQTTRecvState.Data:
      begin
      if (RLInt > 0)  then
      begin
      SetLength(CurrentMessage.Data, RLInt);
      RLInt := RLInt - FPSocket.RecvBufferEx(Pointer(CurrentMessage.Data), RLInt, 1000);
      end;

      if ((FPSocket.LastError = 0) and (RLInt = 0)) then
      begin
      FCurrentMsg := CurrentMessage;
      Synchronize(ProcessMessage);
      CurrentMessage := Default(TUnparsedMsg);
      FCurrentRecvState := TMQTTRecvState.FixedHeaderByte;
      end;
      end;
      end;  // end of Recv state case
      end; // end of waitingdata check
      finally
      FCSock.Release;
      end;
    *)
  end;
end;

procedure TMQTTReadThread.ProcessMessage;
var
  LNewMsg     : TUnparsedMsg;
  LFHCode     : Byte;
  LDataCaret  : Integer;
  LGrantedQoS : array of Integer;
  I           : Integer;
  LTopic      : string;
  LPayload    : string;
begin
  LDataCaret := 0;

  LNewMsg := FCurrentMsg;
  LFHCode := LNewMsg.FixedHeader shr 4;
  case LFHCode of
    Ord(TMQTTMessageType.CONNACK) :
      begin
        if Length(LNewMsg.Data) > 0 then
        begin
          if Assigned(FConnAckEvent) then
            OnConnAck(Self, LNewMsg.Data[0]);
        end;
      end;
    Ord(TMQTTMessageType.PINGREQ) :
      begin
        if Assigned(FPingReqEvent) then
          OnPingReq(Self);
      end;
    Ord(TMQTTMessageType.PINGRESP) :
      begin
        if Assigned(FPingRespEvent) then
          OnPingResp(Self);
      end;
    Ord(TMQTTMessageType.PUBLISH) :
      begin
        // Todo: This only applies for QoS level 0 messages.
        LDataCaret := ReadSingleString(LNewMsg.Data, LDataCaret, LTopic);
        ReadStringWithoutPrefix(LNewMsg.Data, LDataCaret, LPayload);
        if Assigned(FPublishEvent) then
          OnPublish(Self, UTF8String(LTopic), UTF8String(LPayload));
      end;
    Ord(TMQTTMessageType.SUBACK) :
      begin
        if (Length(LNewMsg.Data) > 2) then
        begin
          SetLength(LGrantedQoS, Length(LNewMsg.Data) - 2);
          for I := 0 to Length(LNewMsg.Data) - 1 do
          begin
            LGrantedQoS[I] := LNewMsg.Data[I + 2];
          end;
          if Assigned(FSubAckEvent) then
            OnSubAck(Self, TMQTTRecvUtilities.MSBLSBToInt(Copy(LNewMsg.Data, 0,
                  2)), LGrantedQoS);
        end;
      end;
    Ord(TMQTTMessageType.UNSUBACK) :
      begin
        if Length(LNewMsg.Data) = 2 then
        begin
          if Assigned(FUnSubAckEvent) then
            OnUnSubAck(Self, TMQTTRecvUtilities.MSBLSBToInt(LNewMsg.Data))
        end;
      end;
    Ord(TMQTTMessageType.PUBREC) :
      begin
        if Length(LNewMsg.Data) = 2 then
        begin
          if Assigned(FPubRecEvent) then
            OnPubRec(Self, TMQTTRecvUtilities.MSBLSBToInt(LNewMsg.Data))
        end;
      end;
    Ord(TMQTTMessageType.PUBREL) :
      begin
        if Length(LNewMsg.Data) = 2 then
        begin
          if Assigned(FPubRelEvent) then
            OnPubRel(Self, TMQTTRecvUtilities.MSBLSBToInt(LNewMsg.Data))
        end;
      end;
    Ord(TMQTTMessageType.PUBACK) :
      begin
        if Length(LNewMsg.Data) = 2 then
        begin
          if Assigned(FPubAckEvent) then
            OnPubAck(Self, TMQTTRecvUtilities.MSBLSBToInt(LNewMsg.Data))
        end;
      end;
    Ord(TMQTTMessageType.PUBCOMP) :
      begin
        if Length(LNewMsg.Data) = 2 then
        begin
          if Assigned(FPubCompEvent) then
            OnPubComp(Self, TMQTTRecvUtilities.MSBLSBToInt(LNewMsg.Data))
        end;
      end;
  end;
end;

//function TMQTTReadThread.ReadMessageId(const ADataStream: TBytes;
//  const AIndexStartAt: Integer; var AMessageId: Integer) : Integer;
//begin
//  AMessageId := TMQTTRecvUtilities.MSBLSBToInt
//    (Copy(ADataStream, AIndexStartAt, 2));
//  Result := AIndexStartAt + 2;
//end;

function TMQTTReadThread.ReadStringWithoutPrefix(const ADataStream: TBytes;
  const AIndexStartAt : Integer; var AStringRead: string): Integer;
var
  LLength : Integer;
begin
  LLength := Length(ADataStream) - (AIndexStartAt + 1);
  if LLength > 0 then
  begin
    // ==============================================================================
    // HammerOh
    // SetString(StringRead, PChar(@dataStream[indexStartAt + 2]), (strLength -1) );
    AStringRead := TEncoding.UTF8.GetString(ADataStream, AIndexStartAt + 2,
      LLength - 1);
    // ==============================================================================
  end;
  Result := AIndexStartAt + LLength;
end;

function TMQTTReadThread.ReadSingleString(const ADataStream: TBytes;
  const AIndexStartAt: Integer; var AStringRead: string) : Integer;
var
  LLength : Integer;
begin
  LLength := TMQTTRecvUtilities.MSBLSBToInt
    (Copy(ADataStream, AIndexStartAt, 2));
  if LLength > 0 then
  begin
    // ==============================================================================
    // HammerOh
    // SetString(StringRead, PChar(@dataStream[indexStartAt + 2]), strLength);
    AStringRead := TEncoding.UTF8.GetString(ADataStream, AIndexStartAt + 2,
      LLength);
    // ==============================================================================
  end;
  Result := AIndexStartAt + LLength;
end;
{$ENDREGION}

end.
