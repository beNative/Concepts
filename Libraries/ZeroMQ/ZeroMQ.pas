(*
    Unit owner :
       Pierre Yager <pierre.y@gmail.com>

    Added by Tim Sinaeve
      - IZMQPair.UnSubscribe method
      - IZMQPair.LastEndPoint method
*)

unit ZeroMQ;

interface

uses
  System.Classes, System.Generics.Collections,
  ZeroMQ.API;

type
  ZMQSocket = (
    Pair,
    Publisher, Subscriber,
    Requester, Responder,
    Dealer, Router,
    Pull, Push,
    XPublisher, XSubscriber,
    Stream
  );

  ZMQEvent = (
    Connected, Delayed, Retried,
    Listening, BindFailed,
    Accepted, AcceptFailed,
    Closed, CloseFailed, Disconnected, MonitorStopped
  );
  ZMQEvents = set of ZMQEvent;

  TZMQEventProc = reference to procedure(Event: ZMQEvents; Value: Integer; const Address: string);

  MessageFlag = (DontWait, SendMore);
  MessageFlags = set of MessageFlag;

  IZMQPair = interface
  ['{7F6D7BE5-7182-4972-96E1-4B5798608DDE}']
    { When manually closing the socket is needed }
    function Close: Boolean;
    { Server pair }
    function Bind(const Address: string): Integer;
    { Client pair }
    function Connect(const Address: string): Integer;
    { Socket Options }
    function SocketType: ZMQSocket;
    { Required for ZMQ.Subscriber pair }
    function Subscribe(const Filter: string): Integer; overload;
    function UnSubscribe(const Filter: string): Integer; overload;

    function Subscribe(const ABuffer: RawByteString): Integer; overload;
    function UnSubscribe(const ABuffer: RawByteString): Integer; overload;

    function HaveMore: Boolean;
    { Raw messages }
    function SendMessage(var Msg: TZmqMsg; Flags: MessageFlags): Integer;
    function ReceiveMessage(var Msg: TZmqMsg; Flags: MessageFlags): Integer;
    { Simple string message }
    function SendString(const Data: string; Flags: MessageFlags): Integer; overload;
    function SendString(const Data: string; DontWait: Boolean = False): Integer; overload;
    function SendBytesStream(AStream: TBytesStream): Integer;
    function ReceiveString(DontWait: Boolean = False): string;
    { Multipart string message }
    function SendStrings(const Data: array of string; DontWait: Boolean = False): Integer;
    function ReceiveStrings(const DontWait: Boolean = False): TArray<string>;
    { High Level Algorithms - Forward message to another pair }
    procedure ForwardMessage(Pair: IZMQPair);
    function LastEndPoint: string;
  end;

  PollEvent = (PollIn, PollOut, PollErr);
  PollEvents = set of PollEvent;

  TZMQPollEvent = reference to procedure(Events: PollEvents);

  IZMQPoll = interface
    procedure RegisterPair(const Pair: IZMQPair; Events: PollEvents = []; const Proc: TZMQPollEvent = nil);
    function PollOnce(Timeout: Integer = -1): Integer;
    procedure FireEvents;
  end;

  IZeroMQ = interface
    ['{593FC079-23AD-451E-8877-11584E93D80E}']
    function Start(Kind: ZMQSocket): IZMQPair;
    function StartProxy(Frontend, Backend: IZMQPair; Capture: IZMQPair = nil): Integer;
    function Poller: IZMQPoll;
    function InitMessage(var Msg: TZmqMsg; Size: Integer = 0): Integer;
    function CloseMessage(var Msg: TZmqMsg): Integer;
    function Monitor(Socket: IZMQPair; const Name: string; Events: ZMQEvents; const OnEvent: TZMQEventProc): Boolean;
    procedure Sleep(Seconds: Integer);
  end;

  TZeroMQ = class(TInterfacedObject, IZeroMQ)
  private
    FContext: Pointer;
    FPairs: TList<IZMQPair>;
  public
    constructor Create;
    destructor Destroy; override;
    function Start(Kind: ZMQSocket): IZMQPair;
    function StartProxy(Frontend, Backend: IZMQPair; Capture: IZMQPair = nil): Integer;
    function Poller: IZMQPoll;
    function InitMessage(var Msg: TZmqMsg; Size: Integer = 0): Integer;
    function CloseMessage(var Msg: TZmqMsg): Integer;
    function Monitor(Socket: IZMQPair; const Name: string; Events: ZMQEvents; const OnEvent: TZMQEventProc): Boolean;
    procedure Sleep(Seconds: Integer);
  end;

const
  ZMQAllEvents = [Connected..MonitorStopped];

implementation

uses
  System.SysUtils;

type
  TZMQPair = class(TInterfacedObject, IZMQPair)
  private
    FSocket  : Pointer;
    FContext : TZeroMQ;

  public
    constructor Create(Context: TZeroMQ; Socket: Pointer);
    destructor Destroy; override;
    { When manually closing the socket is needed }
    function Close: Boolean;
    { Server pair }
    function Bind(const Address: string): Integer;
    { Client pair }
    function Connect(const Address: string): Integer;
    { Required for ZMQ.Subscriber pair }
    function Subscribe(const Filter: string): Integer; overload;
    function UnSubscribe(const Filter: string): Integer; overload;
    { Subscribe to raw data (bytestring) }
    function Subscribe(const ABuffer: RawByteString): Integer; overload;
    function UnSubscribe(const ABuffer: RawByteString): Integer; overload;

    function HaveMore: Boolean;
    { Socket Options }
    function SocketType: ZMQSocket;
    { Raw messages }
    function SendMessage(var Msg: TZmqMsg; Flags: MessageFlags): Integer;
    function ReceiveMessage(var Msg: TZmqMsg; Flags: MessageFlags): Integer;
    { Simple string message }
    function SendString(const Data: string; Flags: MessageFlags): Integer; overload;
    function SendString(const Data: string; DontWait: Boolean = False): Integer; overload;
    { Send bytestream }
    function SendBytesStream(AStream: TBytesStream): Integer;
    function ReceiveString(DontWait: Boolean = False): string;
    { Multipart string message }
    function SendStrings(const Data: array of string; DontWait: Boolean = False): Integer;
    function ReceiveStrings(const DontWait: Boolean = False): TArray<string>;
    { High Level Algorithms - Forward message to another pair }
    procedure ForwardMessage(Pair: IZMQPair);

    function LastEndPoint: string;

  end;

  TZMQPoll = class(TInterfacedObject, IZMQPoll)
  private
    FPollItems: TArray<TZmqPollItem>;
    FPollEvents: TArray<TZMQPollEvent>;
  public
    procedure RegisterPair(const Pair: IZMQPair; Events: PollEvents = []; const Event: TZMQPollEvent = nil);
    function PollOnce(Timeout: Integer = -1): Integer;
    procedure FireEvents;
  end;

{ TZeroMQ }

constructor TZeroMQ.Create;
begin
  inherited Create;
  FPairs := TList<IZMQPair>.Create;
  FContext := zmq_ctx_new;

  // TS
  //zmq_ctx_set(FContext, ZMQ_IO_THREADS, 1);
end;

destructor TZeroMQ.Destroy;
begin
  FPairs.Free;
  zmq_ctx_term(FContext);
  inherited;
end;

procedure TZeroMQ.Sleep(Seconds: Integer);
begin
  zmq_sleep(Seconds);
end;

function TZeroMQ.Start(Kind: ZMQSocket): IZMQPair;
var
  LHWM: Integer;
begin
  Result := TZMQPair.Create(Self, zmq_socket(FContext, Ord(Kind)));
//  LHWM := 200000;
   // TS
//  zmq_setsockopt ((Result as TZMQPair).FSocket, ZMQ_SNDHWM, @LHWM, SizeOf(LHWM));
//zmq_setsockopt ((Result as TZMQPair).FSocket, ZMQ_RCVHWM, @LHWM, SizeOf(LHWM));

  FPairs.Add(Result);
end;

function TZeroMQ.StartProxy(Frontend, Backend, Capture: IZMQPair): Integer;
begin
  if Capture = nil then
    Result := zmq_proxy((Frontend as TZMQPair).FSocket, (Backend as TZMQPair).FSocket, nil)
  else
    Result := zmq_proxy((Frontend as TZMQPair).FSocket, (Backend as TZMQPair).FSocket, (Capture as TZMQPair).FSocket)
end;

function TZeroMQ.InitMessage(var Msg: TZmqMsg; Size: Integer): Integer;
begin
  if Size = 0 then
    Result := zmq_msg_init(@Msg)
  else
    Result := zmq_msg_init_size(@Msg, Size)
end;

function TZeroMQ.Poller: IZMQPoll;
begin
  Result := TZMQPoll.Create;
end;

function TZeroMQ.CloseMessage(var Msg: TZmqMsg): Integer;
begin
  Result := zmq_msg_close(@Msg);
end;

function TZeroMQ.Monitor(Socket: IZMQPair; const Name: string; Events: ZMQEvents;
  const OnEvent: TZMQEventProc): Boolean;
var
  MonitorAddress: string;
  rc: Integer;
begin
  MonitorAddress := 'inproc://' + Name;

  rc := zmq_socket_monitor((Socket as TZMQPair).FSocket, PAnsiChar(AnsiString(MonitorAddress)), Word(Events));
  if rc = 0 then
  begin
    TThread.CreateAnonymousThread(
      procedure

        function ReadMonitorMessage(P: IZMQPair; var Event: TZmqEventData; var EP: RawByteString): Boolean;
        var
          m1, m2: TZmqMsg;
          rc: Integer;
          data: PZmqEventData;
        begin
          { binary part }
          InitMessage(m1);
          rc := P.ReceiveMessage(m1, []);
          if (rc = -1) and (zmq_errno = ETERM) then
            Exit(False);
          Assert(rc <> -1);
          Assert(zmq_msg_more(@m1) <> 0);

          { address part }
          InitMessage(m2);
          rc := P.ReceiveMessage(m2, []);
          if (rc = -1) and (zmq_errno = ETERM) then
            Exit(False);
          Assert(rc <> -1);
          Assert(zmq_msg_more(@m2) = 0);

          { copy binary data to event struct }
          data := PZmqEventData(zmq_msg_data(@m1));
          Event := data^;

          { copy address part }
          SetString(EP, PAnsiChar(zmq_msg_data(@m2)), zmq_msg_size(@m2));

          zmq_msg_close(@m2);
          zmq_msg_close(@m1);

          Result := True;
        end;

      var
        event: TZmqEventData;
        addr: RawByteString;
        P: IZMQPair;
      begin
        P := Start(ZMQSocket.Pair);
        P.Connect(MonitorAddress);
        while ReadMonitorMessage(P, event, addr) do
          OnEvent(ZMQEvents(event.event), event.value, string(addr));
      end
    ).Start;
    Result := True;
  end
  else
    Result := False;
end;

{ TZMQPair }

constructor TZMQPair.Create(Context: TZeroMQ; Socket: Pointer);
begin
  inherited Create;
  FContext := Context;
  FSocket := Socket;
end;

destructor TZMQPair.Destroy;
begin
  if FSocket <> nil then
    zmq_close(FSocket);
  inherited;
end;

function TZMQPair.Close: Boolean;
begin
  FContext.FPairs.Remove(Self);
  Result := zmq_close(FSocket) = 0;
  FSocket := nil;
end;

function TZMQPair.Bind(const Address: string): Integer;
begin
  Result := zmq_bind(FSocket, PAnsiChar(AnsiString(Address)));
end;

function TZMQPair.Connect(const Address: string): Integer;
begin
  Result := zmq_connect(FSocket, PAnsiChar(AnsiString(Address)));
end;

function TZMQPair.Subscribe(const Filter: string): Integer;
var
  str: UTF8String;
begin
  str := UTF8String(Filter);
  Result := zmq_setsockopt(FSocket, ZMQ_SUBSCRIBE, PAnsiChar(str), Length(str));
end;

function TZMQPair.Subscribe(const ABuffer: RawByteString): Integer;
begin
  Result := zmq_setsockopt(FSocket, ZMQ_SUBSCRIBE, PAnsiChar(ABuffer), Length(ABuffer));
end;

function TZMQPair.UnSubscribe(const Filter: string): Integer;
var
  str: UTF8String;
begin
  str := UTF8String(Filter);
  Result := zmq_setsockopt(FSocket, ZMQ_UNSUBSCRIBE, PAnsiChar(str), Length(str));
end;

function TZMQPair.UnSubscribe(const ABuffer: RawByteString): Integer;
begin
  Result := zmq_setsockopt(FSocket, ZMQ_UNSUBSCRIBE, PAnsiChar(ABuffer), Length(ABuffer));
end;

function TZMQPair.HaveMore: Boolean;
var
  more: Integer;
  more_size: Cardinal;
begin
  more_size := SizeOf(more);
  zmq_getsockopt(FSocket, ZMQ_RCVMORE, @more, @more_size);
  Result := more > 0;
end;

function TZMQPair.ReceiveMessage(var Msg: TZmqMsg;
  Flags: MessageFlags): Integer;
begin
  Result := zmq_recvmsg(FSocket, @Msg, Byte(Flags));
end;

function TZMQPair.ReceiveString(DontWait: Boolean): string;
var
  msg: TZmqMsg;
  str: UTF8String;
  len: Cardinal;
begin
  zmq_msg_init(@msg);
  if zmq_recvmsg(FSocket, @msg, Ord(DontWait)) = 0 then
    Exit('');

  len := zmq_msg_size(@msg);
  SetLength(str, len);
  Move(zmq_msg_data(@msg)^, PAnsiChar(str)^, len);
  zmq_msg_close(@msg);
  Result := string(str);
end;

function TZMQPair.ReceiveStrings(const DontWait: Boolean): TArray<string>;
var
  L: TList<string>;
begin
  L := TList<string>.Create;
  try
    repeat
      L.Add(ReceiveString(DontWait));
    until not HaveMore;
    Result := L.ToArray;
  finally
    L.Free;
  end;
end;

function TZMQPair.SendBytesStream(AStream: TBytesStream): Integer;
var
  msg: TZmqMsg;
  len: NativeUInt;
begin
  len := NativeUInt(AStream.Size);
  Result := zmq_msg_init_size(@msg, len);
  if Result = 0 then
  begin
    Move(AStream.Memory, zmq_msg_data(@msg)^, len);
    Result := SendMessage(msg, [DontWait]);
    zmq_msg_close(@msg);
  end;
end;

function TZMQPair.SendMessage(var Msg: TZmqMsg; Flags: MessageFlags): Integer;
begin
  Result := zmq_sendmsg(FSocket, @Msg, Byte(Flags));
end;

function TZMQPair.SendString(const Data: string; Flags: MessageFlags): Integer;
var
  msg: TZmqMsg;
  str: UTF8String;
  len: Integer;
begin
  str := UTF8String(Data);
  len := Length(str);
  Result := zmq_msg_init_size(@msg, len);
  if Result = 0 then
  begin
    Move(PAnsiChar(str)^, zmq_msg_data(@msg)^, len);
    Result := SendMessage(msg, Flags);
    zmq_msg_close(@msg);
  end;
end;

function TZMQPair.SendString(const Data: string; DontWait: Boolean): Integer;
begin
  Result := SendString(Data, MessageFlags(Ord(DontWait)));
end;

function TZMQPair.SendStrings(const Data: array of string;
  DontWait: Boolean): Integer;
var
  I: Integer;
  Flags: MessageFlags;
begin
  Result := 0;
  if Length(Data) = 1 then
    Result := SendString(Data[0], DontWait)
  else
  begin
    Flags := [MessageFlag.SendMore] + MessageFlags(Ord(DontWait));
    for I := Low(Data) to High(Data) do
    begin
      if I = High(Data) then
        Exclude(Flags, MessageFlag.SendMore);
      Result := SendString(Data[I], Flags);
      if Result < 0 then
        Break;
    end;
  end;
end;

function TZMQPair.SocketType: ZMQSocket;
var
  RawType: Integer;
  OptionSize: Integer;
begin
  RawType := 0;
  OptionSize := SizeOf(RawType);
  zmq_getsockopt(FSocket, ZMQ_TYPE, @RawType, @OptionSize);
  Result := ZMQSocket(RawType)
end;

procedure TZMQPair.ForwardMessage(Pair: IZMQPair);
const
  SEND_FLAGS: array[Boolean] of Byte = (0, ZMQ_SNDMORE);
var
  msg: TZmqMsg;
  flag: Byte;
begin
  repeat
    zmq_msg_init(@msg);
    zmq_recvmsg(FSocket, @msg, 0);
    flag := SEND_FLAGS[HaveMore];
    zmq_sendmsg((Pair as TZMQPair).FSocket, @msg, flag);
    zmq_msg_close(@msg);
  until flag = 0;
end;

function TZMQPair.LastEndPoint: string;
var
  S       : ShortString;
  LLength : Cardinal;
begin
  LLength := 255;
  zmq_getsockopt(FSocket, ZMQ_LAST_ENDPOINT, @S[1], @LLength);
  SetLength(S, LLength - 1);
  Result := string(S);
end;

{ TZMQPoll }

procedure TZMQPoll.RegisterPair(const Pair: IZMQPair; Events: PollEvents;
  const Event: TZMQPollEvent);
var
  P: PZmqPollItem;
begin
  SetLength(FPollItems, Length(FPollItems) + 1);
  P := @FPollItems[Length(FPollItems) - 1];
  P.socket := (Pair as TZMQPair).FSocket;
  P.fd := 0;
  P.events := Byte(Events);
  P.revents := 0;

  SetLength(FPollEvents, Length(FPollEvents) + 1);
  FPollEvents[Length(FPollEvents) - 1] := Event;
end;

function TZMQPoll.PollOnce(Timeout: Integer): Integer;
begin
  Result := zmq_poll(@FPollItems[0], Length(FPollItems), Timeout);
end;

procedure TZMQPoll.FireEvents;
var
  I: Integer;
begin
  for I := 0 to Length(FPollItems) - 1 do
    if (FPollEvents[I] <> nil) and ((FPollItems[I].revents and FPollItems[I].events) <> 0) then
      FPollEvents[I](PollEvents(Byte(FPollItems[I].revents)));
end;

end.
