unit MQTTReadThread;

interface

uses
  Classes,
  SysUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Generics.Collections,
  SyncObjs,


  //==============================================================================
  // HammerOh
  // blcksock;
  IdTCPClient, IdGlobal,
  //==============================================================================
  MQTTHeaders;

type
  //==============================================================================
  // HammerOh
  // PTCPBlockSocket = ^TTCPBlockSocket;
  //==============================================================================


  TMQTTRecvUtilities = class
    public
      class function MSBLSBToInt(ALengthBytes: TBytes): integer;
      class function RLBytesToInt(ARlBytes: TBytes): Integer;
  end;

  TUnparsedMsg = record
  public
    FixedHeader: Byte;
    RL: TBytes;
    Data: TBytes;
  end;

  TMQTTReadThread = class(TThread)
  private
    { Private declarations }
    //==============================================================================
    // HammerOh
    // FPSocket: TTCPBlockSocket;
    FPSocket: TIdTCPClient;
    //==============================================================================




    FCSock: TCriticalSection;
    FCurrentMsg: TUnparsedMsg;

    FCurrentRecvState: TMQTTRecvState;
    // Events
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


    procedure ProcessMessage;
    function readSingleString(const dataStream: TBytes; const indexStartAt: Integer; var StringRead: string): integer;
    function readMessageId(const dataStream: TBytes; const indexStartAt: Integer;  var messageId: integer): integer;
    function readStringWithoutPrefix(const dataStream: TBytes;
      const indexStartAt: Integer; var StringRead: string): integer;
  protected
    procedure CleanStart;
    procedure Execute; override;
  public
    //==============================================================================
    // HammerOh
    // constructor Create(Socket: TTCPBlockSocket; CSSock: TCriticalSection);
    constructor Create(var Socket: TIdTCPClient;var CSSock: TCriticalSection);
    //==============================================================================


    // Event properties.
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

uses
  MQTT;

class function TMQTTRecvUtilities.MSBLSBToInt(ALengthBytes: TBytes): integer;
begin
  Assert(ALengthBytes <> nil, 'Must not pass nil to this method');
  Assert(Length(ALengthBytes) = 2, 'The MSB-LSB 2 bytes structure must be 2 Bytes in length');

  Result := 0;
  Result := ALengthBytes[0] shl 8;
  Result := Result + ALengthBytes[1];
end;

class function TMQTTRecvUtilities.RLBytesToInt(ARlBytes: TBytes): Integer;
var
  multi: integer;
  i: integer;
  digit: Byte;
begin
  Assert(ARlBytes <> nil, 'Must not pass nil to this method');

  multi := 1;
  i := 0;
  Result := 0;

  if ((Length(ARlBytes) > 0) and (Length(ARlBytes) <= 4)) then
  begin
    digit := ARlBytes[i];
    repeat
      digit := ARlBytes[i];
      Result := Result + (digit and 127) * multi;
      multi := multi * 128;
      Inc(i);
    until ((digit and 128) = 0);
  end;
end;

procedure AppendBytes(var DestArray: TBytes;
  const NewBytes: TBytes);
var
  DestLen: Integer;
begin
  if Length(NewBytes) > 0 then
  begin
    DestLen := Length(DestArray);
    SetLength(DestArray, DestLen + Length(NewBytes));
    Move(NewBytes[0], DestArray[DestLen], Length(NewBytes));
  end;
end;

{ TMQTTReadThread }

procedure TMQTTReadThread.CleanStart;
begin
  FCurrentRecvState := TMQTTRecvState.FixedHeaderByte;
end;

//==============================================================================
// HammerOh
// constructor TMQTTReadThread.Create(Socket: TTCPBlockSocket; CSSock: TCriticalSection);
constructor TMQTTReadThread.Create(var Socket: TIdTCPClient; var CSSock: TCriticalSection);
//==============================================================================
begin
  inherited Create(false);

  FPSocket := Socket;
  FCSock := CSSock;
  FreeOnTerminate := false;

  CleanStart;
end;

procedure TMQTTReadThread.Execute;
var
  CurrentMessage: TUnparsedMsg;
  RLInt: Integer;
  Buffer: TBytes;
  i: integer;
  //==============================================================================
  // HammerOh
  intSize: Integer;
  indyBuffer: TIdBytes;
  intCount: Integer;
  intTemp: Integer;
  intIndex: Integer;
  //==============================================================================
begin
  while not Terminated do
    begin
      try
        if FPSocket.IOHandler.CheckForDataOnSource(1000) then
        if not FPSocket.IOHandler.InputBufferIsEmpty then
        begin
          intSize := FPSocket.IOHandler.InputBuffer.Size;
          indyBuffer := nil;
          FPSocket.IOHandler.ReadBytes(indyBuffer, intSize);
          for i := 0 to intSize - 1 do
          begin
            case FCurrentRecvState of
              TMQTTRecvState.FixedHeaderByte:
                begin
                  CurrentMessage.FixedHeader := 0;
                  CurrentMessage.FixedHeader := indyBuffer[i];
                  if (CurrentMessage.FixedHeader <> 0) then FCurrentRecvState := TMQTTRecvState.RemainingLength;
                end;

              TMQTTRecvState.RemainingLength:
                begin
                  RLInt := 0;
                  SetLength(CurrentMessage.RL, 1);
                  SetLength(Buffer, 1);
                  CurrentMessage.RL[0] := indyBuffer[i];
                  if CurrentMessage.RL[0] = 0 then
                  begin
                    FCurrentMsg := CurrentMessage;
                    Synchronize(ProcessMessage);
                    CurrentMessage := Default(TUnparsedMsg);
                    FCurrentRecvState := TMQTTRecvState.FixedHeaderByte;
                  end
                  else
                  begin
                    FCurrentRecvState := TMQTTRecvState.RemainingLength1;
                    intIndex := 0;
                  end;
                end;

              TMQTTRecvState.RemainingLength1:
                begin
                  if ((CurrentMessage.RL[intIndex] and 128) <> 0) then
                  begin
                    Buffer[0] := indyBuffer[i];
                    AppendBytes(CurrentMessage.RL, Buffer);
                    Inc(intIndex);
                  end
                  else
                  begin
                    intIndex := 5;
                  end;

                  if (intIndex = 4) or (intIndex = 5) then
                  begin
                    RLInt := TMQTTRecvUtilities.RLBytesToInt(CurrentMessage.RL);
                    if RLInt > 0 then
                    begin
                      SetLength(CurrentMessage.Data, RLInt);
                      intCount := 0;
                      if intIndex = 5  then
                      begin
                        CurrentMessage.Data[intCount] := indyBuffer[i];
                        Inc(intCount);
                      end;
                      FCurrentRecvState := TMQTTRecvState.Data;
                    end
                    else
                    begin
                      FCurrentMsg := CurrentMessage;
                      Synchronize(ProcessMessage);
                      CurrentMessage := Default(TUnparsedMsg);
                      FCurrentRecvState := TMQTTRecvState.FixedHeaderByte;
                    end;
                  end;
                end;

              TMQTTRecvState.Data:
                begin
                  CurrentMessage.Data[intCount] := indyBuffer[i];
                  Inc(intCount);
                  if intCount = RLInt then
                  begin
                    FCurrentMsg := CurrentMessage;
                    Synchronize(ProcessMessage);
                    CurrentMessage := Default(TUnparsedMsg);
                    FCurrentRecvState := TMQTTRecvState.FixedHeaderByte;
                  end;
                end;
            end;
          end;
        end;
      except

      end;

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
  NewMsg: TUnparsedMsg;
  FHCode: Byte;
  dataCaret: integer;
  grantedQoS: Array of Integer;
  I: Integer;
  strTopic, strPayload: string;
begin
  dataCaret := 0;

  NewMsg := FCurrentMsg;
  FHCode := NewMsg.FixedHeader shr 4;
  case FHCode of
    Ord(TMQTTMessageType.CONNACK):
      begin
        if Length(NewMsg.Data) > 0 then
        begin
          if Assigned(FConnAckEvent) then OnConnAck(Self, NewMsg.Data[0]);
        end;
      end;
    Ord(TMQTTMessageType.PINGREQ):
      begin
        if Assigned(FPingReqEvent) then OnPingReq(Self);
      end;
    Ord(TMQTTMessageType.PINGRESP):
      begin
        if Assigned(FPingRespEvent) then OnPingResp(Self);
      end;
    Ord(TMQTTMessageType.PUBLISH):
      begin
         // Todo: This only applies for QoS level 0 messages.
         dataCaret := 0;
         dataCaret := readSingleString(NewMsg.Data, dataCaret, strTopic);
         dataCaret := readStringWithoutPrefix(NewMsg.Data, dataCaret, strPayload);
         if Assigned(FPublishEvent) then OnPublish(Self, strTopic, strPayload);
      end;
    Ord(TMQTTMessageType.SUBACK):
      begin
        if (Length(NewMsg.Data) > 2) then
        begin
          SetLength(grantedQoS, Length(NewMsg.Data) - 2);
          for I := 0 to Length(NewMsg.Data) - 1 do
            begin
              grantedQoS[i] := NewMsg.Data[i + 2];
            end;
          if Assigned(FSubAckEvent) then OnSubAck(Self, TMQTTRecvUtilities.MSBLSBToInt(Copy(NewMsg.Data, 0, 2)), grantedQoS);
        end;
      end;
    Ord(TMQTTMessageType.UNSUBACK):
      begin
        if Length(NewMsg.Data) = 2 then
          begin
            if Assigned(FUnSubAckEvent) then OnUnSubAck(Self, TMQTTRecvUtilities.MSBLSBToInt(NewMsg.Data))
          end;
      end;
    Ord(TMQTTMessageType.PUBREC):
      begin
        if Length(NewMsg.Data) = 2 then
          begin
            if Assigned(FPubRecEvent) then OnPubRec(Self, TMQTTRecvUtilities.MSBLSBToInt(NewMsg.Data))
          end;
      end;
    Ord(TMQTTMessageType.PUBREL):
      begin
        if Length(NewMsg.Data) = 2 then
          begin
            if Assigned(FPubRelEvent) then OnPubRel(Self, TMQTTRecvUtilities.MSBLSBToInt(NewMsg.Data))
          end;
      end;
    Ord(TMQTTMessageType.PUBACK):
      begin
        if Length(NewMsg.Data) = 2 then
          begin
            if Assigned(FPubAckEvent) then OnPubAck(Self, TMQTTRecvUtilities.MSBLSBToInt(NewMsg.Data))
          end;
      end;
    Ord(TMQTTMessageType.PUBCOMP):
      begin
        if Length(NewMsg.Data) = 2 then
          begin
            if Assigned(FPubCompEvent) then OnPubComp(Self, TMQTTRecvUtilities.MSBLSBToInt(NewMsg.Data))
          end;
      end;
  end;

end;

function TMQTTReadThread.readMessageId(const dataStream: TBytes;
  const indexStartAt: Integer; var messageId: integer): integer;
begin
  messageId := TMQTTRecvUtilities.MSBLSBToInt(Copy(dataStream, indexStartAt, 2));
  Result := indexStartAt + 2;
end;

function TMQTTReadThread.readStringWithoutPrefix(const dataStream: TBytes;
  const indexStartAt: Integer; var StringRead: string): integer;
var
  strLength: integer;
begin
  strLength := Length(dataStream) - (indexStartAt + 1);
  if strLength > 0 then
  begin
    //==============================================================================
    // HammerOh
    // SetString(StringRead, PChar(@dataStream[indexStartAt + 2]), (strLength -1) );
    StringRead := TEncoding.UTF8.GetString(dataStream, indexStartAt + 2, strLength - 1);
    //==============================================================================
  end;
  Result := indexStartAt + strLength;
end;

function TMQTTReadThread.readSingleString(const dataStream: TBytes;
  const indexStartAt: Integer; var StringRead: string): integer;
var
  strLength: integer;
begin
  strLength := TMQTTRecvUtilities.MSBLSBToInt(Copy(dataStream, indexStartAt, 2));
  if strLength > 0 then
  begin
    //==============================================================================
    // HammerOh
    // SetString(StringRead, PChar(@dataStream[indexStartAt + 2]), strLength);
    StringRead := TEncoding.UTF8.GetString(dataStream, indexStartAt + 2, strLength);
    //==============================================================================
  end;
  Result := indexStartAt + strLength;
end;

end.
