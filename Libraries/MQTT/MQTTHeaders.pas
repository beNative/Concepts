unit MQTTHeaders;

interface

uses
  System.SysUtils, System.Types, System.Classes;

type
  TMQTTMessageType = (
    Reserved0,   // 0	 Reserved
    CONNECT,     // 1	 Client request to connect to Broker
    CONNACK,     // 2	 Connect Acknowledgment
    PUBLISH,     // 3	 Publish message
    PUBACK,      // 4	 Publish Acknowledgment
    PUBREC,      // 5	 Publish Received (assured delivery part 1)
    PUBREL,      // 6	 Publish Release (assured delivery part 2)
    PUBCOMP,     // 7	 Publish Complete (assured delivery part 3)
    SUBSCRIBE,   // 8	 Client Subscribe request
    SUBACK,      // 9	 Subscribe Acknowledgment
    UNSUBSCRIBE, // 10 Client Unsubscribe request
    UNSUBACK,    // 11 Unsubscribe Acknowledgment
    PINGREQ,     // 12 PING Request
    PINGRESP,    // 13 PING Response
    DISCONNECT,  // 14 Client is Disconnecting
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
    function GetBits(const AIndex : Integer) : Integer;
    procedure SetBits(
      const AIndex : Integer;
      const AValue : Integer
    );

  public
    Flags           : Byte;

    property Retain : Integer
      index $0001 read GetBits write SetBits;
    // 1 bits at offset 0
    property QoSLevel : Integer
      index $0102 read GetBits write SetBits;
    // 2 bits at offset 1
    property Duplicate : Integer
      index $0301 read GetBits write SetBits;
    // 1 bits at offset 3
    property MessageType : Integer
      index $0404 read GetBits write SetBits;
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
    function GetBits(const AIndex: Integer): Integer;
    procedure SetBits(const AIndex: Integer; const AValue: Integer);

  public
    Flags : Byte;

    property CleanStart : Integer
      index $0101 read GetBits write SetBits;
    // 1 bit at offset 1
    property WillFlag : Integer
      index $0201 read GetBits write SetBits;
    // 1 bit at offset 2
    property WillQoS : Integer
      index $0302 read GetBits write SetBits;
    // 2 bits at offset 3
    property WillRetain : Integer
      index $0501 read GetBits write SetBits;
    // 1 bit at offset 5
    property Password : Integer
      index $0601 read GetBits write SetBits;
    // 1 bit at offset 6
    property UserName : Integer
      index $0701 read GetBits write SetBits;
    // 1 bit at offset 7
  end;

  TConnAckEvent = procedure(
    Sender     : TObject;
    ReturnCode : Integer
  ) of object;

  TPublishEvent = procedure(
    Sender   : TObject;
    ATopic   : UTF8String;
    APayload : UTF8String
  ) of object;

  TPingRespEvent = procedure(Sender: TObject) of object;
  TPingReqEvent  = procedure(Sender: TObject) of object;

  TSubAckEvent   = procedure(
    Sender      : TObject;
    AMessageId  : Integer;
    AGrantedQoS : array of Integer
  ) of object;

  TUnSubAckEvent = procedure(
    Sender     : TObject;
    AMessageId : Integer
  ) of object;

  TPubAckEvent = procedure(
    Sender     : TObject;
    AMessageId : Integer
  ) of object;

  TPubRelEvent = procedure(
    Sender     : TObject;
    AMessageId : Integer
  ) of object;

  TPubRecEvent = procedure(
    Sender     : TObject;
    AMessageId : Integer
  ) of object;

  TPubCompEvent = procedure(
    Sender     : TObject;
    AMessageId : Integer
  ) of object;

  TMQTTVariableHeader = class
  private
    FBytes : TBytes;

  protected
    procedure AddField(AByte: Byte); overload;
    procedure AddField(ABytes: TBytes); overload;
    procedure ClearField;

  public
    function ToBytes: TBytes; virtual;

  end;

  TMQTTConnectVarHeader = class(TMQTTVariableHeader)
  const
    PROTOCOL_ID  = 'MQIsdp';
    PROTOCOL_VER = 3;

  private
    FConnectFlags : TMQTTConnectFlags;
    FKeepAlive    : Integer;

    function RebuildHeader: Boolean;
    procedure SetupDefaultValues;
    function GetCleanStart: Integer;
    function GetQoSLevel: Integer;
    function GetRetain: Integer;
    procedure SetCleanStart(const Value: Integer);
    procedure SetQoSLevel(const Value: Integer);
    procedure SetRetain(const Value: Integer);
    function GetWillFlag: Integer;
    procedure SetWillFlag(const Value: Integer);
    function GetUserName: Integer;
    procedure SetUsername(const Value: Integer);
    function GetPassword: Integer;
    procedure SetPassword(const Value: Integer);

  public
    constructor Create(AKeepAlive: Integer); overload;
    constructor Create; overload;
    constructor Create(ACleanStart: Boolean); overload;

    property KeepAlive: Integer
      read FKeepAlive write FKeepAlive;

    property CleanStart: Integer
      read GetCleanStart write SetCleanStart;

    property QoSLevel: Integer
      read GetQoSLevel write SetQoSLevel;

    property Retain: Integer
      read GetRetain write SetRetain;

    property UserName: Integer
      read GetUserName write SetUsername;

    property Password: Integer
      read GetPassword write SetPassword;

    property WillFlag: Integer
      read GetWillFlag write SetWillFlag;

    function ToBytes: TBytes; override;
  end;

  TMQTTPublishVarHeader = class(TMQTTVariableHeader)
  private
    FTopic     : UTF8String;
    FQoSLevel  : Integer;
    FMessageId : Integer;

    function GetMessageId: Integer;
    function GetQoSLevel: Integer;
    procedure SetMessageId(const Value: Integer);
    procedure SetQoSLevel(const Value: Integer);
    function GetTopic : UTF8String;
    procedure SetTopic(const Value: UTF8String);
    procedure RebuildHeader;

  public
    constructor Create(AQoSLevel: Integer); overload;

    property MessageId: Integer
      read GetMessageId write SetMessageId;

    property QoSLevel: Integer
      read GetQoSLevel write SetQoSLevel;

    property Topic: UTF8String
      read GetTopic write SetTopic;

    function ToBytes: TBytes; override;
  end;

  TMQTTSubscribeVarHeader = class(TMQTTVariableHeader)
  private
    FMessageId: Integer;

    function GetMessageId: Integer;
    procedure SetMessageId(const Value: Integer);

  public
    constructor Create(AMessageId: Integer); overload;

    property MessageId: Integer
      read GetMessageId write SetMessageId;

    function ToBytes: TBytes; override;
  end;

  TMQTTUnsubscribeVarHeader = class(TMQTTVariableHeader)
  private
    FMessageId : Integer;

    function GetMessageId: Integer;
    procedure SetMessageId(const Value: Integer);

  public
    constructor Create(AMessageId: Integer); overload;

    property MessageId : Integer
      read GetMessageId write SetMessageId;

    function ToBytes: TBytes; override;
  end;

  TMQTTPayload = class
  private
    FContents            : TStringList;
    FContainsIntLiterals : Boolean;
    FPublishMessage      : Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function ToBytes: TBytes; overload;
    function ToBytes(AWithIntegerLiterals: Boolean): TBytes; overload;

    property Contents: TStringList
      read FContents;

    property ContainsIntLiterals: Boolean
      read FContainsIntLiterals write FContainsIntLiterals;

    property PublishMessage: Boolean
      read FPublishMessage write FPublishMessage;
  end;

  TMQTTMessage = class
  private
    FRemainingLength : Integer;

  public
    FixedHeader    : TMQTTFixedHeader;
    VariableHeader : TMQTTVariableHeader;
    Payload        : TMQTTPayload;

    constructor Create;
    destructor Destroy; override;
    function ToBytes: TBytes;

    property RemainingLength: Integer
      read FRemainingLength;
  end;

  TMQTTUtilities = class
  public
    class function UTF8EncodeToBytes(AStrToEncode: UTF8String): TBytes;
    class function UTF8EncodeToBytesNoLength(AStrToEncode: UTF8String): TBytes;
    class function RLIntToBytes(ARlInt: Integer): TBytes;
    class function IntToMSBLSB(ANumber: Word): TBytes;
  end;

implementation

{$REGION 'non-interfaced routines'}
function GetDWordBits(const ABits: Byte; const AIndex: Integer): Integer;
begin
  Result := (ABits shr (AIndex shr 8)) // offset
    and ((1 shl Byte(AIndex)) - 1);   // mask
end;

procedure SetDWordBits(var ABits: Byte; const AIndex: Integer;
  const AValue: Integer);
var
  LOffset : Byte;
  LMask   : Integer;
begin
  LMask := (1 shl Byte(AIndex)) - 1;
  Assert(AValue <= LMask);

  LOffset := AIndex shr 8;
  ABits   := (ABits and (not(LMask shl LOffset))) or DWORD(AValue shl LOffset);
end;

procedure AppendToByteArray(ASourceBytes: TBytes; var ATargetBytes: TBytes);
  overload;
var
  LUpperBnd : Integer;
begin
  if Length(ASourceBytes) > 0 then
  begin
    LUpperBnd := Length(ATargetBytes);
    SetLength(ATargetBytes, LUpperBnd + Length(ASourceBytes));
    Move(ASourceBytes[0], ATargetBytes[LUpperBnd], Length(ASourceBytes));
  end;
end;

procedure AppendToByteArray(ASourceByte: Byte; var ATargetBytes: TBytes);
  overload;
var
  LUpperBnd : Integer;
begin
  LUpperBnd := Length(ATargetBytes);
  SetLength(ATargetBytes, LUpperBnd + 1);
  Move(ASourceByte, ATargetBytes[LUpperBnd], 1);
end;
{$ENDREGION}

{$REGION 'TMQTTUtilities'}
class function TMQTTUtilities.IntToMSBLSB(ANumber: Word): TBytes;
begin
  SetLength(Result, 2);
  Result[0] := ANumber div 256;
  Result[1] := ANumber mod 256;
end;

{ MSBLSBToInt is in the MQTTRecvThread unit }

class function TMQTTUtilities.UTF8EncodeToBytes(AStrToEncode: UTF8String): TBytes;
var
  I : Integer;
  // ==============================================================================
  // HammerOh
  LTemp : TBytes;
  // ==============================================================================
begin
  { This is a UTF-8 hack to give 2 Bytes of Length MSB-LSB followed by a Single-byte
    per character UTF8String. }
  // ==============================================================================
  // HammerOh
  LTemp := TEncoding.UTF8.GetBytes(string(AStrToEncode));
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
  for I := 0 to Length(AStrToEncode) - 1 do
  begin
    Result[I + 2] := LTemp[I];
  end;

  // ==============================================================================
end;

class function TMQTTUtilities.UTF8EncodeToBytesNoLength(
  AStrToEncode: UTF8String): TBytes;
var
  I : Integer;
  // ==============================================================================
  // HammerOh
  LTemp : TBytes;
  // ==============================================================================
begin
  // ==============================================================================
  // HammerOh
  LTemp := TEncoding.UTF8.GetBytes(string(AStrToEncode));
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
  for I := 0 to Length(AStrToEncode) - 1 do
  begin
    Result[I] := LTemp[I];
  end;

  // ==============================================================================
end;

class function TMQTTUtilities.RLIntToBytes(ARlInt: Integer): TBytes;
var
  LByteIndex : Integer;
  LDigit     : Integer;
begin
  SetLength(Result, 1);
  LByteIndex := 0;
  while ARlInt > 0 do
  begin
    LDigit := ARlInt mod 128;
    ARlInt := ARlInt div 128;
    if ARlInt > 0 then
    begin
      LDigit := LDigit or $80;
    end;
    Result[LByteIndex] := LDigit;
    if ARlInt > 0 then
    begin
      inc(LByteIndex);
      SetLength(Result, Length(Result) + 1);
    end;
  end;
end;
{$ENDREGION}

{$REGION 'TMQTTFixedHeader'}
function TMQTTFixedHeader.GetBits(const AIndex: Integer): Integer;
begin
  Result := GetDWordBits(Flags, AIndex);
end;

procedure TMQTTFixedHeader.SetBits(const AIndex, AValue: Integer);
begin
  SetDWordBits(Flags, AIndex, AValue);
end;
{$ENDREGION}

{$REGION 'TMQTTVariableHeader'}
procedure TMQTTVariableHeader.AddField(AByte: Byte);
var
  LDestUpperBnd : Integer;
begin
  LDestUpperBnd := Length(FBytes);
  SetLength(FBytes, LDestUpperBnd + SizeOf(AByte));
  Move(AByte, FBytes[LDestUpperBnd], SizeOf(AByte));
end;

procedure TMQTTVariableHeader.AddField(ABytes: TBytes);
var
  LDestUpperBnd : Integer;
begin
  LDestUpperBnd := Length(FBytes);
  SetLength(FBytes, LDestUpperBnd + Length(ABytes));
  Move(ABytes[0], FBytes[LDestUpperBnd], Length(ABytes));
end;

procedure TMQTTVariableHeader.ClearField;
begin
  SetLength(FBytes, 0);
end;

function TMQTTVariableHeader.ToBytes: TBytes;
begin
  Result := FBytes;
end;
{$ENDREGION}

{$REGION 'TMQTTConnectVarHeader'}
constructor TMQTTConnectVarHeader.Create(ACleanStart: Boolean);
begin
  inherited Create;
  SetupDefaultValues;
  FConnectFlags.CleanStart := Ord(ACleanStart);
end;

function TMQTTConnectVarHeader.GetCleanStart: Integer;
begin
  Result := FConnectFlags.CleanStart;
end;

function TMQTTConnectVarHeader.GetPassword: Integer;
begin
  Result := FConnectFlags.Password;
end;

function TMQTTConnectVarHeader.GetQoSLevel: Integer;
begin
  Result := FConnectFlags.WillQoS;
end;

function TMQTTConnectVarHeader.GetRetain: Integer;
begin
  Result := FConnectFlags.WillRetain;
end;

function TMQTTConnectVarHeader.GetUserName: Integer;
begin
  Result := FConnectFlags.UserName;
end;

function TMQTTConnectVarHeader.GetWillFlag: Integer;
begin
  Result := FConnectFlags.WillFlag;
end;

constructor TMQTTConnectVarHeader.Create(AKeepAlive: Integer);
begin
  inherited Create;
  SetupDefaultValues;
  Self.FKeepAlive := AKeepAlive;
end;

constructor TMQTTConnectVarHeader.Create;
begin
  inherited Create;
  SetupDefaultValues;
end;

function TMQTTConnectVarHeader.RebuildHeader : Boolean;
begin
  try
    ClearField;
    AddField(TMQTTUtilities.UTF8EncodeToBytes(Self.PROTOCOL_ID));
    AddField(Byte(Self.PROTOCOL_VER));
    AddField(FConnectFlags.Flags);
    AddField(TMQTTUtilities.IntToMSBLSB(FKeepAlive));
  except
    Result := False;
  end;
  Result := True;
end;

procedure TMQTTConnectVarHeader.SetupDefaultValues;
begin
  FConnectFlags.Flags      := 0;
  FConnectFlags.CleanStart := 1;
  FConnectFlags.WillRetain := 0;
  FConnectFlags.WillQoS    := 1;
  FConnectFlags.WillFlag   := 1;
  FConnectFlags.UserName   := 0;
  FConnectFlags.Password   := 0;
  FKeepAlive               := 10;
end;

procedure TMQTTConnectVarHeader.SetCleanStart(const Value: Integer);
begin
  FConnectFlags.CleanStart := Value;
end;

procedure TMQTTConnectVarHeader.SetPassword(const Value: Integer);
begin
  FConnectFlags.UserName := Value;
end;

procedure TMQTTConnectVarHeader.SetQoSLevel(const Value: Integer);
begin
  FConnectFlags.WillQoS := Value;
end;

procedure TMQTTConnectVarHeader.SetRetain(const Value: Integer);
begin
  FConnectFlags.WillRetain := Value;
end;

procedure TMQTTConnectVarHeader.SetUsername(const Value: Integer);
begin
  FConnectFlags.Password := Value;
end;

procedure TMQTTConnectVarHeader.SetWillFlag(const Value: Integer);
begin
  FConnectFlags.WillFlag := Value;
end;

function TMQTTConnectVarHeader.ToBytes: TBytes;
begin
  RebuildHeader;
  Result := FBytes;
end;
{$ENDREGION}

{$REGION 'TMQTTConnectFlags'}
function TMQTTConnectFlags.GetBits(const AIndex: Integer): Integer;
begin
  Result := GetDWordBits(Flags, AIndex);
end;

procedure TMQTTConnectFlags.SetBits(const AIndex, AValue: Integer);
begin
  SetDWordBits(Flags, AIndex, AValue);
end;
{$ENDREGION}

{$REGION 'TMQTTPayload'}
constructor TMQTTPayload.Create;
begin
  FContents            := TStringList.Create();
  FContainsIntLiterals := False;
  FPublishMessage      := False;
end;

destructor TMQTTPayload.Destroy;
begin
  FContents.Free;
  inherited;
end;

function TMQTTPayload.ToBytes(AWithIntegerLiterals: Boolean): TBytes;
var
  LLine        : UTF8String;
  LLineAsBytes : TBytes;
  LLineAsInt   : Integer;
begin
  SetLength(Result, 0);
  for LLine in FContents do
  begin
    // This is really nasty and needs refactoring into subclasses
    if PublishMessage then
    begin
      LLineAsBytes := TMQTTUtilities.UTF8EncodeToBytesNoLength(LLine);
      AppendToByteArray(LLineAsBytes, Result);
    end
    else
    begin
      if AWithIntegerLiterals and TryStrToInt(LLine, LLineAsInt) then
      begin
        AppendToByteArray(Lo(LLineAsInt), Result);
      end
      else
      begin
        LLineAsBytes := TMQTTUtilities.UTF8EncodeToBytes(LLine);
        AppendToByteArray(LLineAsBytes, Result);
      end;
    end;
  end;
end;

function TMQTTPayload.ToBytes : TBytes;
begin
  Result := ToBytes(FContainsIntLiterals);
end;
{$ENDREGION}

{$REGION 'TMQTTMessage'}
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
  if Assigned(Payload) then
    Payload.Free;
  inherited;
end;

function TMQTTMessage.ToBytes: TBytes;
var
  LRemainingLength      : Integer;
  LBytesRemainingLength : TBytes;
  I                     : Integer;
begin
  try
    LRemainingLength := 0;
    if Assigned(VariableHeader) then
      LRemainingLength := LRemainingLength + Length(VariableHeader.ToBytes);
    if Assigned(Payload) then
      LRemainingLength := LRemainingLength + Length(Payload.ToBytes);

    FRemainingLength     := LRemainingLength;
    LBytesRemainingLength := TMQTTUtilities.RLIntToBytes(FRemainingLength);

    AppendToByteArray(FixedHeader.Flags, Result);
    AppendToByteArray(LBytesRemainingLength, Result);
    if Assigned(VariableHeader) then
      AppendToByteArray(VariableHeader.ToBytes, Result);
    if Assigned(Payload) then
      AppendToByteArray(Payload.ToBytes, Result);
  except
    // on E:Exception do
  end;
end;
{$ENDREGION}

{$REGION 'TMQTTPublishVarHeader'}
constructor TMQTTPublishVarHeader.Create(AQoSLevel: Integer);
begin
  inherited Create;
  FQoSLevel := AQoSLevel;
end;

function TMQTTPublishVarHeader.GetMessageId: Integer;
begin
  Result := FMessageId;
end;

function TMQTTPublishVarHeader.GetQoSLevel: Integer;
begin
  Result := FQoSLevel;
end;

function TMQTTPublishVarHeader.GetTopic: UTF8String;
begin
  Result := FTopic;
end;

procedure TMQTTPublishVarHeader.RebuildHeader;
begin
  ClearField;
  AddField(TMQTTUtilities.UTF8EncodeToBytes(FTopic));
  if (FQoSLevel > 0) then
  begin
    AddField(TMQTTUtilities.IntToMSBLSB(FMessageId));
  end;
end;

procedure TMQTTPublishVarHeader.SetMessageId(const Value: Integer);
begin
  FMessageId := Value;
end;

procedure TMQTTPublishVarHeader.SetQoSLevel(const Value: Integer);
begin
  FQoSLevel := Value;
end;

procedure TMQTTPublishVarHeader.SetTopic(const Value: UTF8String);
begin
  FTopic := Value;
end;

function TMQTTPublishVarHeader.ToBytes: TBytes;
begin
  Self.RebuildHeader;
  Result := Self.FBytes;
end;
{$ENDREGION}

{$REGION 'TMQTTSubscribeVarHeader'}
constructor TMQTTSubscribeVarHeader.Create(AMessageId: Integer);
begin
  inherited Create;
  FMessageId := AMessageId;
end;

function TMQTTSubscribeVarHeader.GetMessageId: Integer;
begin
  Result := FMessageId;
end;

procedure TMQTTSubscribeVarHeader.SetMessageId(const Value: Integer);
begin
  FMessageId := Value;
end;

function TMQTTSubscribeVarHeader.ToBytes: TBytes;
begin
  ClearField;
  AddField(TMQTTUtilities.IntToMSBLSB(FMessageId));
  Result := FBytes;
end;
{$ENDREGION}

{$REGION 'TMQTTUnsubscribeVarHeader'}
constructor TMQTTUnsubscribeVarHeader.Create(AMessageId: Integer);
begin
  inherited Create;
  FMessageId := AMessageId;
end;

function TMQTTUnsubscribeVarHeader.GetMessageId: Integer;
begin
  Result := FMessageId;
end;

procedure TMQTTUnsubscribeVarHeader.SetMessageId(const Value: Integer);
begin
  FMessageId := Value;
end;

function TMQTTUnsubscribeVarHeader.ToBytes: TBytes;
begin
  ClearField;
  AddField(TMQTTUtilities.IntToMSBLSB(FMessageId));
  Result := FBytes;
end;
{$ENDREGION}

end.
