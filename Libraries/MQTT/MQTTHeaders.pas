unit MQTTHeaders;

interface

uses
  SysUtils,
  Types,
  Classes;

type

  TMQTTMessageType = (
    Reserved0,   // 0	Reserved
    CONNECT,     // 1	Client request to connect to Broker
    CONNACK,     // 2	Connect Acknowledgment
    PUBLISH,     // 3	Publish message
    PUBACK,      // 4	Publish Acknowledgment
    PUBREC,      // 5	Publish Received (assured delivery part 1)
    PUBREL,      // 6	Publish Release (assured delivery part 2)
    PUBCOMP,     // 7	Publish Complete (assured delivery part 3)
    SUBSCRIBE,   // 8	Client Subscribe request
    SUBACK,      // 9	Subscribe Acknowledgment
    UNSUBSCRIBE, // 10	Client Unsubscribe request
    UNSUBACK,    // 11	Unsubscribe Acknowledgment
    PINGREQ,     // 12	PING Request
    PINGRESP,    // 13	PING Response
    DISCONNECT,  // 14	Client is Disconnecting
    Reserved15   // 15
  );

  TMQTTRecvState = (
    FixedHeaderByte,
    RemainingLength,
    RemainingLength1,
    RemainingLength2,
    RemainingLength3,
    RemainingLength4,
    Data
  );

  {
    bit	    7	6	5	4	      3	        2	1	      0
    byte 1	Message Type	DUP flag	QoS level	RETAIN
    byte 2	Remaining Length
  }
  TMQTTFixedHeader = packed record
  private
    function GetBits(const aIndex : Integer) : Integer;
    procedure SetBits(
      const aIndex : Integer;
      const aValue : Integer
    );
  public
    Flags           : Byte;
    property Retain : Integer index $0001 read GetBits write SetBits;
    // 1 bits at offset 0
    property QoSLevel : Integer index $0102 read GetBits write SetBits;
    // 2 bits at offset 1
    property Duplicate : Integer index $0301 read GetBits write SetBits;
    // 1 bits at offset 3
    property MessageType : Integer index $0404 read GetBits write SetBits;
    // 4 bits at offset 4
  end;

  {
    Description	    7	6	5	4	3	2	1	0
    Connect Flags
    byte 10	        1	1	0	0	1	1	1	x
    Will RETAIN (0)
    Will QoS (01)
    Will flag (1)
    Clean Start (1)
    Username Flag (1)
    Password Flag (1)
  }
  TMQTTConnectFlags = packed record
  private
    function GetBits(const aIndex : Integer) : Integer;
    procedure SetBits(
      const aIndex : Integer;
      const aValue : Integer
    );
  public
    Flags               : Byte;
    property CleanStart : Integer index $0101 read GetBits write SetBits;
    // 1 bit at offset 1
    property WillFlag : Integer index $0201 read GetBits write SetBits;
    // 1 bit at offset 2
    property WillQoS : Integer index $0302 read GetBits write SetBits;
    // 2 bits at offset 3
    property WillRetain : Integer index $0501 read GetBits write SetBits;
    // 1 bit at offset 5
    property Password : Integer index $0601 read GetBits write SetBits;
    // 1 bit at offset 6
    property UserName : Integer index $0701 read GetBits write SetBits;
    // 1 bit at offset 7
  end;

  TConnAckEvent = procedure(
    Sender     : TObject;
    ReturnCode : Integer
  ) of object;
  TPublishEvent = procedure(
    Sender         : TObject;
    topic, payload : UTF8String
  ) of object;
  TPingRespEvent = procedure(Sender : TObject) of object;
  TPingReqEvent  = procedure(Sender : TObject) of object;
  TSubAckEvent   = procedure(
    Sender     : TObject;
    MessageID  : Integer;
    GrantedQoS : array of Integer
  ) of object;
  TUnSubAckEvent = procedure(
    Sender    : TObject;
    MessageID : Integer
  ) of object;
  TPubAckEvent = procedure(
    Sender    : TObject;
    MessageID : Integer
  ) of object;
  TPubRelEvent = procedure(
    Sender    : TObject;
    MessageID : Integer
  ) of object;
  TPubRecEvent = procedure(
    Sender    : TObject;
    MessageID : Integer
  ) of object;
  TPubCompEvent = procedure(
    Sender    : TObject;
    MessageID : Integer
  ) of object;

  TMQTTVariableHeader = class
  private
    FBytes : TBytes;
  protected
    procedure AddField(AByte : Byte); overload;
    procedure AddField(ABytes : TBytes); overload;
    procedure ClearField;
  public
    constructor Create;
    function ToBytes : TBytes; virtual;
  end;

  TMQTTConnectVarHeader = class(TMQTTVariableHeader)
  const
    PROTOCOL_ID  = 'MQIsdp';
    PROTOCOL_VER = 3;
  private
    FConnectFlags : TMQTTConnectFlags;
    FKeepAlive    : Integer;
    function rebuildHeader : boolean;
    procedure setupDefaultValues;
    function get_CleanStart : Integer;
    function get_QoSLevel : Integer;
    function get_Retain : Integer;
    procedure set_CleanStart(const Value : Integer);
    procedure set_QoSLevel(const Value : Integer);
    procedure set_Retain(const Value : Integer);
    function get_WillFlag : Integer;
    procedure set_WillFlag(const Value : Integer);
    function get_Username : Integer;
    procedure set_Username(const Value : Integer);
    function get_Password : Integer;
    procedure set_Password(const Value : Integer);
  public
    constructor Create(AKeepAlive : Integer); overload;
    constructor Create; overload;
    constructor Create(ACleanStart : boolean); overload;
    property KeepAlive : Integer read FKeepAlive write FKeepAlive;
    property CleanStart : Integer read get_CleanStart write set_CleanStart;
    property QoSLevel : Integer read get_QoSLevel write set_QoSLevel;
    property Retain : Integer read get_Retain write set_Retain;
    property UserName : Integer read get_Username write set_Username;
    property Password : Integer read get_Password write set_Password;
    property WillFlag : Integer read get_WillFlag write set_WillFlag;
    function ToBytes : TBytes; override;
  end;

  TMQTTPublishVarHeader = class(TMQTTVariableHeader)
  private
    FTopic     : UTF8String;
    FQoSLevel  : Integer;
    FMessageID : Integer;
    function get_MessageID : Integer;
    function get_QoSLevel : Integer;
    procedure set_MessageID(const Value : Integer);
    procedure set_QoSLevel(const Value : Integer);
    function get_Topic : UTF8String;
    procedure set_Topic(const Value : UTF8String);
    procedure rebuildHeader;
  public
    constructor Create(QoSLevel : Integer); overload;
    property MessageID : Integer read get_MessageID write set_MessageID;
    property QoSLevel : Integer read get_QoSLevel write set_QoSLevel;
    property topic : UTF8String read get_Topic write set_Topic;
    function ToBytes : TBytes; override;
  end;

  TMQTTSubscribeVarHeader = class(TMQTTVariableHeader)
  private
    FMessageID : Integer;
    function get_MessageID : Integer;
    procedure set_MessageID(const Value : Integer);
  public
    constructor Create(MessageID : Integer); overload;
    property MessageID : Integer read get_MessageID write set_MessageID;
    function ToBytes : TBytes; override;
  end;

  TMQTTUnsubscribeVarHeader = class(TMQTTVariableHeader)
  private
    FMessageID : Integer;
    function get_MessageID : Integer;
    procedure set_MessageID(const Value : Integer);
  public
    constructor Create(MessageID : Integer); overload;
    property MessageID : Integer read get_MessageID write set_MessageID;
    function ToBytes : TBytes; override;
  end;

  TMQTTPayload = class
  private
    FContents            : TStringList;
    FContainsIntLiterals : boolean;
    FPublishMessage      : boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function ToBytes : TBytes; overload;
    function ToBytes(WithIntegerLiterals : boolean) : TBytes; overload;
    property Contents : TStringList read FContents;
    property ContainsIntLiterals : boolean read FContainsIntLiterals
      write FContainsIntLiterals;
    property PublishMessage : boolean read FPublishMessage
      write FPublishMessage;
  end;

  TMQTTMessage = class
  private
    FRemainingLength : Integer;
  public
    FixedHeader    : TMQTTFixedHeader;
    VariableHeader : TMQTTVariableHeader;
    payload        : TMQTTPayload;
    constructor Create;
    destructor Destroy; override;
    function ToBytes : TBytes;
    property RemainingLength : Integer read FRemainingLength;
  end;

  TMQTTUtilities = class
  public
    class function UTF8EncodeToBytes(AStrToEncode : UTF8String) : TBytes;
    class function UTF8EncodeToBytesNoLength(AStrToEncode : UTF8String)
      : TBytes;
    class function RLIntToBytes(ARlInt : Integer) : TBytes;
    class function IntToMSBLSB(ANumber : Word) : TBytes;
  end;

implementation

function GetDWordBits(
  const Bits   : Byte;
  const aIndex : Integer) : Integer;
begin
  Result := (Bits shr (aIndex shr 8)) // offset
    and ((1 shl Byte(aIndex)) - 1);   // mask
end;

procedure SetDWordBits(
  var Bits     : Byte;
  const aIndex : Integer;
  const aValue : Integer);
var
  Offset : Byte;
  Mask   : Integer;
begin
  Mask := ((1 shl Byte(aIndex)) - 1);
  Assert(aValue <= Mask);

  Offset := aIndex shr 8;
  Bits   := (Bits and (not(Mask shl Offset)))
    or DWORD(aValue shl Offset);
end;

class function TMQTTUtilities.IntToMSBLSB(ANumber : Word) : TBytes;
begin
  SetLength(Result, 2);
  Result[0] := ANumber div 256;
  Result[1] := ANumber mod 256;
end;

{ MSBLSBToInt is in the MQTTRecvThread unit }

class function TMQTTUtilities.UTF8EncodeToBytes(AStrToEncode
    : UTF8String) : TBytes;
var
  i : Integer;
  // ==============================================================================
  // HammerOh
  temp : TBytes;
  // ==============================================================================
begin
  { This is a UTF-8 hack to give 2 Bytes of Length MSB-LSB followed by a Single-byte
    per character UTF8String. }
  // ==============================================================================
  // HammerOh
  temp := TEncoding.UTF8.GetBytes(AStrToEncode);
  // ==============================================================================

  SetLength(Result, Length(AStrToEncode) + 2);

  Result[0] := Length(AStrToEncode) div 256;
  Result[1] := Length(AStrToEncode) mod 256;
  // ==============================================================================
  // HammerOh
  (* )
    for I := 0 to Length(AStrToEncode) - 1 do
    begin
    Result[i + 2] := Ord(AStrToEncode[i + 1]);
    end;
    ( *)
  for i := 0 to Length(AStrToEncode) - 1 do
  begin
    Result[i + 2] := temp[i];
  end;

  // ==============================================================================
end;

class function TMQTTUtilities.UTF8EncodeToBytesNoLength
  (AStrToEncode : UTF8String) : TBytes;
var
  i : Integer;
  // ==============================================================================
  // HammerOh
  temp : TBytes;
  // ==============================================================================
begin
  // ==============================================================================
  // HammerOh
  temp := TEncoding.UTF8.GetBytes(AStrToEncode);
  // ==============================================================================
  SetLength(Result, Length(AStrToEncode));
  // ==============================================================================
  // HammerOh
  (* )
    for i := 0 to Length(AStrToEncode) - 1 do
    begin
    Result[i] := Ord(AStrToEncode[i + 1]);
    end;
    ( *)
  for i := 0 to Length(AStrToEncode) - 1 do
  begin
    Result[i] := temp[i];
  end;

  // ==============================================================================
end;

procedure AppendToByteArray(
  ASourceBytes     : TBytes;
  var ATargetBytes : TBytes); overload;
var
  iUpperBnd : Integer;
begin
  if Length(ASourceBytes) > 0 then
  begin
    iUpperBnd := Length(ATargetBytes);
    SetLength(ATargetBytes, iUpperBnd + Length(ASourceBytes));
    Move(ASourceBytes[0], ATargetBytes[iUpperBnd], Length(ASourceBytes));
  end;
end;

procedure AppendToByteArray(
  ASourceByte      : Byte;
  var ATargetBytes : TBytes); overload;
var
  iUpperBnd : Integer;
begin
  iUpperBnd := Length(ATargetBytes);
  SetLength(ATargetBytes, iUpperBnd + 1);
  Move(ASourceByte, ATargetBytes[iUpperBnd], 1);
end;

class function TMQTTUtilities.RLIntToBytes(ARlInt : Integer) : TBytes;
var
  byteindex : Integer;
  digit     : Integer;
begin
  SetLength(Result, 1);
  byteindex := 0;
  while (ARlInt > 0) do
  begin
    digit  := ARlInt mod 128;
    ARlInt := ARlInt div 128;
    if ARlInt > 0 then
    begin
      digit := digit or $80;
    end;
    Result[byteindex] := digit;
    if ARlInt > 0 then
    begin
      inc(byteindex);
      SetLength(Result, Length(Result) + 1);
    end;
  end;
end;

{ TMQTTFixedHeader }

function TMQTTFixedHeader.GetBits(const aIndex : Integer) : Integer;
begin
  Result := GetDWordBits(Flags, aIndex);
end;

procedure TMQTTFixedHeader.SetBits(const aIndex, aValue : Integer);
begin
  SetDWordBits(Flags, aIndex, aValue);
end;

{ TMQTTMessage }

{ TMQTTVariableHeader }

procedure TMQTTVariableHeader.AddField(AByte : Byte);
var
  DestUpperBnd : Integer;
begin
  DestUpperBnd := Length(FBytes);
  SetLength(FBytes, DestUpperBnd + SizeOf(AByte));
  Move(AByte, FBytes[DestUpperBnd], SizeOf(AByte));
end;

procedure TMQTTVariableHeader.AddField(ABytes : TBytes);
var
  DestUpperBnd : Integer;
begin
  DestUpperBnd := Length(FBytes);
  SetLength(FBytes, DestUpperBnd + Length(ABytes));
  Move(ABytes[0], FBytes[DestUpperBnd], Length(ABytes));
end;

procedure TMQTTVariableHeader.ClearField;
begin
  SetLength(FBytes, 0);
end;

constructor TMQTTVariableHeader.Create;
begin
end;

function TMQTTVariableHeader.ToBytes : TBytes;
begin
  Result := FBytes;
end;

{ TMQTTConnectVarHeader }

constructor TMQTTConnectVarHeader.Create(ACleanStart : boolean);
begin
  inherited Create;
  setupDefaultValues;
  Self.FConnectFlags.CleanStart := Ord(ACleanStart);
end;

function TMQTTConnectVarHeader.get_CleanStart : Integer;
begin
  Result := Self.FConnectFlags.CleanStart;
end;

function TMQTTConnectVarHeader.get_Password : Integer;
begin
  Result := Self.FConnectFlags.Password;
end;

function TMQTTConnectVarHeader.get_QoSLevel : Integer;
begin
  Result := Self.FConnectFlags.WillQoS;
end;

function TMQTTConnectVarHeader.get_Retain : Integer;
begin
  Result := Self.FConnectFlags.WillRetain;
end;

function TMQTTConnectVarHeader.get_Username : Integer;
begin
  Result := Self.FConnectFlags.UserName;
end;

function TMQTTConnectVarHeader.get_WillFlag : Integer;
begin
  Result := Self.FConnectFlags.WillFlag;
end;

constructor TMQTTConnectVarHeader.Create(AKeepAlive : Integer);
begin
  inherited Create;
  setupDefaultValues;
  Self.FKeepAlive := AKeepAlive;
end;

constructor TMQTTConnectVarHeader.Create;
begin
  inherited Create;
  setupDefaultValues;
end;

function TMQTTConnectVarHeader.rebuildHeader : boolean;
begin
  try
    ClearField;
    AddField(TMQTTUtilities.UTF8EncodeToBytes(Self.PROTOCOL_ID));
    AddField(Byte(Self.PROTOCOL_VER));
    AddField(FConnectFlags.Flags);
    AddField(TMQTTUtilities.IntToMSBLSB(FKeepAlive));
  except
    Result := false;
  end;
  Result := true;
end;

procedure TMQTTConnectVarHeader.setupDefaultValues;
begin
  Self.FConnectFlags.Flags      := 0;
  Self.FConnectFlags.CleanStart := 1;
  Self.FConnectFlags.WillQoS    := 1;
  Self.FConnectFlags.WillRetain := 0;
  Self.FConnectFlags.WillFlag   := 1;
  Self.FConnectFlags.UserName   := 0;
  Self.FConnectFlags.Password   := 0;
  Self.FKeepAlive               := 10;
end;

procedure TMQTTConnectVarHeader.set_CleanStart(const Value : Integer);
begin
  Self.FConnectFlags.CleanStart := Value;
end;

procedure TMQTTConnectVarHeader.set_Password(const Value : Integer);
begin
  Self.FConnectFlags.UserName := Value;
end;

procedure TMQTTConnectVarHeader.set_QoSLevel(const Value : Integer);
begin
  Self.FConnectFlags.WillQoS := Value;
end;

procedure TMQTTConnectVarHeader.set_Retain(const Value : Integer);
begin
  Self.FConnectFlags.WillRetain := Value;
end;

procedure TMQTTConnectVarHeader.set_Username(const Value : Integer);
begin
  Self.FConnectFlags.Password := Value;
end;

procedure TMQTTConnectVarHeader.set_WillFlag(const Value : Integer);
begin
  Self.FConnectFlags.WillFlag := Value;
end;

function TMQTTConnectVarHeader.ToBytes : TBytes;
begin
  Self.rebuildHeader;
  Result := FBytes;
end;

{ TMQTTConnectFlags }

function TMQTTConnectFlags.GetBits(const aIndex : Integer) : Integer;
begin
  Result := GetDWordBits(Flags, aIndex);
end;

procedure TMQTTConnectFlags.SetBits(const aIndex, aValue : Integer);
begin
  SetDWordBits(Flags, aIndex, aValue);
end;

{ TMQTTPayload }

constructor TMQTTPayload.Create;
begin
  FContents            := TStringList.Create();
  FContainsIntLiterals := false;
  FPublishMessage      := false;
end;

destructor TMQTTPayload.Destroy;
begin
  FContents.Free;
  inherited;
end;

function TMQTTPayload.ToBytes(WithIntegerLiterals : boolean) : TBytes;
var
  line        : UTF8String;
  lineAsBytes : TBytes;
  lineAsInt   : Integer;
begin
  SetLength(Result, 0);
  for line in FContents do
  begin
    // This is really nasty and needs refactoring into subclasses
    if PublishMessage then
    begin
      lineAsBytes := TMQTTUtilities.UTF8EncodeToBytesNoLength(line);
      AppendToByteArray(lineAsBytes, Result);
    end
    else
    begin
      if (WithIntegerLiterals and TryStrToInt(line, lineAsInt)) then
      begin
        AppendToByteArray(Lo(lineAsInt), Result);
      end
      else
      begin
        lineAsBytes := TMQTTUtilities.UTF8EncodeToBytes(line);
        AppendToByteArray(lineAsBytes, Result);
      end;
    end;
  end;
end;

function TMQTTPayload.ToBytes : TBytes;
begin
  Result := ToBytes(FContainsIntLiterals);
end;

{ TMQTTMessage }

constructor TMQTTMessage.Create;
begin
  inherited;
  // Fill our Fixed Header with Zeros to wipe any unintended noise.
  // FillChar(FixedHeader, SizeOf(FixedHeader), #0);
end;

destructor TMQTTMessage.Destroy;
begin
  if Assigned(VariableHeader) then
    VariableHeader.Free;
  if Assigned(payload) then
    payload.Free;
  inherited;
end;

function TMQTTMessage.ToBytes : TBytes;
var
  iRemainingLength     : Integer;
  bytesRemainingLength : TBytes;
  i                    : Integer;
begin

  try
    iRemainingLength := 0;
    if Assigned(VariableHeader) then
      iRemainingLength := iRemainingLength + Length(VariableHeader.ToBytes);
    if Assigned(payload) then
      iRemainingLength := iRemainingLength + Length(payload.ToBytes);

    FRemainingLength     := iRemainingLength;
    bytesRemainingLength := TMQTTUtilities.RLIntToBytes(FRemainingLength);

    AppendToByteArray(FixedHeader.Flags, Result);
    AppendToByteArray(bytesRemainingLength, Result);
    if Assigned(VariableHeader) then
      AppendToByteArray(VariableHeader.ToBytes, Result);
    if Assigned(payload) then
      AppendToByteArray(payload.ToBytes, Result);

  except
    // on E:Exception do

  end;
end;

{ TMQTTPublishVarHeader }

constructor TMQTTPublishVarHeader.Create(QoSLevel : Integer);
begin
  inherited Create;
  FQoSLevel := QoSLevel;
end;

function TMQTTPublishVarHeader.get_MessageID : Integer;
begin
  Result := FMessageID;
end;

function TMQTTPublishVarHeader.get_QoSLevel : Integer;
begin
  Result := FQoSLevel;
end;

function TMQTTPublishVarHeader.get_Topic : UTF8String;
begin
  Result := FTopic;
end;

procedure TMQTTPublishVarHeader.rebuildHeader;
begin
  ClearField;
  AddField(TMQTTUtilities.UTF8EncodeToBytes(FTopic));
  if (FQoSLevel > 0) then
  begin
    AddField(TMQTTUtilities.IntToMSBLSB(FMessageID));
  end;
end;

procedure TMQTTPublishVarHeader.set_MessageID(const Value : Integer);
begin
  FMessageID := Value;
end;

procedure TMQTTPublishVarHeader.set_QoSLevel(const Value : Integer);
begin
  FQoSLevel := Value;
end;

procedure TMQTTPublishVarHeader.set_Topic(const Value : UTF8String);
begin
  FTopic := Value;
end;

function TMQTTPublishVarHeader.ToBytes : TBytes;
begin
  Self.rebuildHeader;
  Result := Self.FBytes;
end;

{ TMQTTSubscribeVarHeader }

constructor TMQTTSubscribeVarHeader.Create(MessageID : Integer);
begin
  inherited Create;
  FMessageID := MessageID;
end;

function TMQTTSubscribeVarHeader.get_MessageID : Integer;
begin
  Result := FMessageID;
end;

procedure TMQTTSubscribeVarHeader.set_MessageID(const Value : Integer);
begin
  FMessageID := Value;
end;

function TMQTTSubscribeVarHeader.ToBytes : TBytes;
begin
  ClearField;
  AddField(TMQTTUtilities.IntToMSBLSB(FMessageID));
  Result := FBytes;
end;

{ TMQTTUnsubscribeVarHeader }

constructor TMQTTUnsubscribeVarHeader.Create(MessageID : Integer);
begin
  inherited Create;
  FMessageID := MessageID;
end;

function TMQTTUnsubscribeVarHeader.get_MessageID : Integer;
begin
  Result := FMessageID;
end;

procedure TMQTTUnsubscribeVarHeader.set_MessageID(const Value : Integer);
begin
  FMessageID := Value;
end;

function TMQTTUnsubscribeVarHeader.ToBytes : TBytes;
begin
  ClearField;
  AddField(TMQTTUtilities.IntToMSBLSB(FMessageID));
  Result := FBytes;
end;

end.
