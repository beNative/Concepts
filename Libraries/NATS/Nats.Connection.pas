{******************************************************************************}
{                                                                              }
{  NATS.Delphi: Delphi Client Library for NATS                                 }
{  Copyright (c) 2022 Paolo Rossi                                              }
{  https://github.com/paolo-rossi/nats.delphi                                  }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit NATS.Connection;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, System.Rtti, System.SyncObjs,
  System.Generics.Defaults, System.Generics.Collections,

  Nats.Classes,
  Nats.Entities,
  Nats.Parser,
  Nats.Socket;

type
  (*
  INatsConnection = interface
  ['{8630DB26-6324-4E33-8342-85BF42A34FC2}']
    procedure Publish(const ASubject, AMessage: string);
    procedure Subscribe(const ASubject: string);
  end;
  *)

  TNatsConnection = class;


  /// <summary>
  ///   Simple Id generator for subscription and inbox
  /// </summary>
  TNatsGenerator = class
  private
    FSubId: Cardinal;
    FInboxId: Cardinal;
  public
    function GetSubNextId: Cardinal;
    function GetNewInbox: string;
  end;

  /// <summary>
  ///   Worker thread for reading incoming messages from the socket channel
  /// </summary>
  TNatsReader = class(TNatsThread)
  private
    FConnection: TNatsConnection;
    FChannel: INatsSocket;
    FParser: TNatsParser;
    FQueue: TNatsCommandQueue;
    FError: string;
    procedure DoExecute;
  protected
    procedure Execute; override;
  public
    constructor Create(AConnection: TNatsConnection);
    destructor Destroy; override;
    property Error: string read FError write FError;
  end;

  /// <summary>
  ///   Worker thread for processing incoming and outgoing messages
  /// </summary>
  TNatsConsumer = class(TNatsThread)
  private
    FConnection: TNatsConnection;
    FQueue: TNatsCommandQueue;
    FError: string;
    procedure DoExecute;
  protected
    procedure Execute; override;
  public
    constructor Create(const AConnection: TNatsConnection);
    property Error: string read FError write FError;

  end;

  /// <summary>
  ///   Structure holding subscription metadata
  /// </summary>
  TNatsSubscription = class
    Id: Integer;
    Subject: string;
    Handler: TNatsMsgHandler;
    Queue: string;
    Received: Integer;
    Expected: Integer;
    Remaining: Integer;

    constructor Create(AId: Integer; const ASubject: string; AHandler: TNatsMsgHandler); overload;
  end;
  TNatsSubscriptionPair = TPair<Integer, TNatsSubscription>;
  TNatsSubscriptions = TObjectDictionary<Integer, TNatsSubscription>;

  /// <summary>
  ///   TNatsConnection represents a bidirectional channel to the NATS server.
  ///   Message handler may be attached to each operation which is invoked when
  ///   the operation is processed by the server
  /// </summary>
  TNatsConnection = class
  private
    FConnectOptions: TNatsConnectOptions;
    FChannel: INatsSocket;
    FGenerator: TNatsGenerator;
    FSubscriptions: TNatsSubscriptions;
    FName: string;
    FReader: TNatsReader;
    FConsumer: TNatsConsumer;
    FReadQueue: TNatsCommandQueue;
    FConnectHandler: TNatsConnectHandler;
    FDisconnectHandler: TNatsDisconnectHandler;
    procedure SendPing;
    procedure SendPong;
    procedure SendConnect;
    procedure SendCommand(const ACommand: string); overload;
    procedure SendCommand(const ACommand: TBytes; APriority: Boolean); overload;
  private
    procedure SendSubscribe(const ASubscription: TNatsSubscription);
    function GetConnected: Boolean;
    procedure EndThreads;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function SetChannel(const AHost: string; APort, ATimeout: Integer): TNatsConnection;
    procedure Open(AConnectHandler: TNatsConnectHandler; ADisconnectHandler: TNatsDisconnectHandler = nil); overload;
    procedure Close();

    procedure Ping();
    procedure Connect(AOptions: TNatsConnectOptions); overload;

    procedure Publish(const ASubject, AMessage: string; const AReplyTo: string = '');

    function Request(const ASubject: string; AHandler: TNatsMsgHandler): Integer; overload;
    function Request(const ASubject, AMessage: string; AHandler: TNatsMsgHandler): Integer; overload;

    function Subscribe(const ASubject: string; AHandler: TNatsMsgHandler): Integer; overload;
    function Subscribe(const ASubject, AQueue: string; AHandler: TNatsMsgHandler): Integer; overload;

    procedure Unsubscribe(AId: Cardinal; AMaxMsg: Cardinal = 0); overload;
    procedure Unsubscribe(const ASubject: string; AMaxMsg: Cardinal = 0); overload;

    function GetSubscriptionList: TArray<TNatsSubscriptionPair>;
  public
    property Name: string read FName write FName;
    property Connected: Boolean read GetConnected;
    property ConnectOptions: TNatsConnectOptions read FConnectOptions write FConnectOptions;
  end;

  TNatsNetwork = class(TObjectDictionary<string, TNatsConnection>)
  public
    function NewConnection(const AName: string): TNatsConnection;
  end;

implementation

uses
  Nats.Consts,
  Nats.Exceptions;

procedure TNatsConnection.Connect(AOptions: TNatsConnectOptions);
begin
  { TODO -opaolo -c : 07/06/2022 21:23:54 }
end;

procedure TNatsConnection.Open(AConnectHandler: TNatsConnectHandler;
    ADisconnectHandler: TNatsDisconnectHandler = nil);
begin
  FConnectHandler := AConnectHandler;
  FDisconnectHandler := ADisconnectHandler;
  FChannel.Open;

  FReader := TNatsReader.Create(Self);
  FReader.Start;

  FConsumer := TNatsConsumer.Create(Self);
  FConsumer.Start;
end;

constructor TNatsConnection.Create;
begin
  FReadQueue := TNatsCommandQueue.Create;
  FGenerator := TNatsGenerator.Create;
  FSubscriptions := TNatsSubscriptions.Create([doOwnsValues]);

  { TODO -opaolo -c : Remove the default behavior 31/05/2022 18:17:27 }
  FChannel := TNatsSocketRegistry.Get(String.Empty);
end;

destructor TNatsConnection.Destroy;
begin
  Close();

  FSubscriptions.Free;
  FGenerator.Free;
  FReadQueue.Free;
  inherited;
end;

function TNatsConnection.GetConnected: Boolean;
begin
  Result := FChannel.Connected;
end;

function TNatsConnection.GetSubscriptionList: TArray<TNatsSubscriptionPair>;
begin
  TMonitor.Enter(FSubscriptions);
  try
    Result := FSubscriptions.ToArray;
  finally
    TMonitor.Exit(FSubscriptions);
  end;
end;

procedure TNatsConnection.Close();
begin
  EndThreads;
  FChannel.Close;
end;

procedure TNatsConnection.Ping;
begin
  SendPing;
end;

procedure TNatsConnection.Publish(const ASubject, AMessage: string; const AReplyTo: string = '');
var
  LMessageBytes: TBytes;
  LSub: string;
begin
  if ASubject.IsEmpty then
    Exit;

  LMessageBytes := TEncoding.UTF8.GetBytes(AMessage);
  if AReplyTo.IsEmpty then
    LSub := Format('pub %s %d', [ASubject, Length(LMessageBytes)])
  else
    LSub := Format('pub %s %s %d', [ASubject, AReplyTo, Length(LMessageBytes)]);

  FChannel.SendString(LSub);
  FChannel.SendBytes(LMessageBytes);
end;

function TNatsConnection.Request(const ASubject: string; AHandler: TNatsMsgHandler): Integer;
begin
  Result := Request(ASubject, String.Empty, AHandler);
end;

function TNatsConnection.Request(const ASubject, AMessage: string; AHandler: TNatsMsgHandler): Integer;
var
  LInbox: string;
begin
  LInbox := FGenerator.GetNewInbox;
  Result := Subscribe(LInbox, AHandler);
  Publish(ASubject, AMessage, LInbox);
end;

procedure TNatsConnection.Unsubscribe(AId: Cardinal; AMaxMsg: Cardinal = 0);
var
  LSub: TNatsSubscription;
begin
  if not FSubscriptions.TryGetValue(AId, LSub) then
    Exit; // Nothing to do here!

  if AMaxMsg = 0 then
    FChannel.SendString(Format('%s %d', [NatsConstants.Protocol.UNSUB, AId]))
  else
    FChannel.SendString(Format('%s %d %d', [NatsConstants.Protocol.UNSUB, AId, AMaxMsg]));

  if AMaxMsg = 0 then
  begin
    FSubscriptions.Remove(AId);
    Exit;
  end;

  LSub.Remaining := AMaxMsg;
end;

procedure TNatsConnection.SendCommand(const ACommand: TBytes; APriority: Boolean);
begin
  FChannel.SendBytes(ACommand);
end;

procedure TNatsConnection.SendConnect;
begin
  FChannel.SendString(Format('%s %s', [NatsConstants.Protocol.CONNECT, FConnectOptions.ToJSONString]));
end;

procedure TNatsConnection.SendPing;
begin
  FChannel.SendString(NatsConstants.Protocol.PING);
end;

procedure TNatsConnection.SendPong;
begin
  FChannel.SendString(NatsConstants.Protocol.PONG);
end;

procedure TNatsConnection.SendSubscribe(const ASubscription: TNatsSubscription);
begin
  if ASubscription.Queue.IsEmpty then
    FChannel.SendString(Format('%s %s %d',
       [NatsConstants.Protocol.SUB, ASubscription.Subject, ASubscription.Id]))
  else
    FChannel.SendString(Format('%s %s %s %d',
      [NatsConstants.Protocol.SUB, ASubscription.Subject, ASubscription.Queue, ASubscription.Id]));
end;

function TNatsConnection.SetChannel(const AHost: string; APort, ATimeout: Integer): TNatsConnection;
begin
  FChannel.Host := AHost;
  FChannel.Port := APort;
  FChannel.Timeout := ATimeout;
  Result := Self;
end;

function TNatsConnection.Subscribe(const ASubject: string; AHandler: TNatsMsgHandler): Integer;
var
  LSub: TNatsSubscription;
begin
  LSub := TNatsSubscription.Create(FGenerator.GetSubNextId, ASubject, AHandler);
  FSubscriptions.Add(LSub.Id, LSub);
  SendSubscribe(LSub);
  Result := LSub.Id;
end;

function TNatsConnection.Subscribe(const ASubject, AQueue: string; AHandler: TNatsMsgHandler): Integer;
var
  LSub: TNatsSubscription;
begin
  LSub := TNatsSubscription.Create(FGenerator.GetSubNextId, ASubject, AHandler);
  LSub.Queue := AQueue;
  FSubscriptions.Add(LSub.Id, LSub);
  Result := LSub.Id;
end;

procedure TNatsConnection.Unsubscribe(const ASubject: string; AMaxMsg: Cardinal);
var
  LPair: TNatsSubscriptionPair;
  LId: Integer;
begin
  LId := -1;
  for LPair in FSubscriptions do
    if LPair.Value.Subject = ASubject then
    begin
      LId := LPair.Value.Id;
      Break;
    end;

  if LId > -1 then
    Unsubscribe(LId, AMaxMsg)
  else
    raise ENatsException.CreateFmt('Subscription [%s] not found in the subscription list', [ASubject]);
end;

procedure TNatsConnection.EndThreads;
begin
  if (FReader = nil) or (FConsumer = nil) then
    Exit;

  FReader.Stop;
  FConsumer.Stop;

  FReader.WaitFor;
  FreeAndNil(FReader);

  FConsumer.WaitFor;
  FreeAndNil(FConsumer);
end;

procedure TNatsConnection.SendCommand(const ACommand: string);
begin
  SendCommand(TEncoding.UTF8.GetBytes(ACommand), False);
end;

{ TNatsReader }

constructor TNatsReader.Create(AConnection: TNatsConnection);
begin
  inherited Create;
  FParser := TNatsParser.Create;
  FConnection := AConnection;
  FChannel := AConnection.FChannel;
  FQueue := AConnection.FReadQueue;
end;

destructor TNatsReader.Destroy;
begin
  FParser.Free;
  inherited;
end;

procedure TNatsReader.DoExecute;
var
  LRead: string;
  LCommand: TNatsCommand;
  LStep: Integer;
begin
  while not Terminated do
  begin
    if not FChannel.Connected then
    begin
      if FStopEvent.WaitFor(1000) = wrSignaled then
        Break;

      Continue;
    end;

    try
      LRead := FChannel.ReceiveString;
    except
      on E: Exception do
      begin
        LRead := '';
        FError := E.Message;
      end;
    end;

    if LRead.IsEmpty then
      Continue;

    LCommand := FParser.Parse(LRead);

    if LCommand.CommandType = TNatsCommandServer.MSG then
    begin
      LRead := FChannel.ReceiveString;
      LCommand := FParser.ParsePayload(LCommand, LRead);
    end;

    TMonitor.Enter(FQueue);
    try
      FQueue.Enqueue(LCommand);
    finally
      TMonitor.Exit(FQueue);
    end;
  end;
end;

procedure TNatsReader.Execute;
begin
  NameThreadForDebugging(FConnection.Name + ' Reader');
  try
    DoExecute;
  except
    on E: Exception do
      Error := E.Message;
  end;
end;

function TNatsGenerator.GetNewInbox: string;
begin
  TMonitor.Enter(Self);
  try
    Inc(FInboxId);
    Result := 'inbox__' + FInboxId.ToString;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TNatsGenerator.GetSubNextId: Cardinal;
begin
  TMonitor.Enter(Self);
  try
    Inc(FSubId);
    Result := FSubId;
  finally
    TMonitor.Exit(Self);
  end;
end;

{ TNatsNetwork }

function TNatsNetwork.NewConnection(const AName: string): TNatsConnection;
begin
  Result := TNatsConnection.Create;
  Result.Name := AName;
  Self.Add(AName, Result);
end;

{ TNatsConsumer }

constructor TNatsConsumer.Create(const AConnection: TNatsConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FQueue := AConnection.FReadQueue;
end;

procedure TNatsConsumer.DoExecute;
var
  LProcess: Boolean;
  LCommand: TNatsCommand;
  LSub: TNatsSubscription;
begin
  while not Terminated do
  begin
    TMonitor.Enter(FQueue);
    try
      LProcess := FQueue.Count > 0;
      if LProcess then
        LCommand := FQueue.Dequeue;
    finally
      TMonitor.Exit(FQueue);
    end;

    if LProcess then
    case LCommand.CommandType of
      TNatsCommandServer.INFO:
      begin
        { TODO -opaolo -c : read the TLSs parameters and (if) upgrade the connection 23/06/2022 11:00:44 }
        if Assigned(FConnection.FConnectHandler) then
          FConnection.FConnectHandler(LCommand.GetArgAsInfo.Info, FConnection.FConnectOptions);

        if (FConnection.FChannel.MaxLineLength>0) then
          FConnection.FChannel.MaxLineLength := LCommand.GetArgAsInfo.Info.max_payload * 2;

        { Send CONNECT message to NATS }
        FConnection.SendConnect;
      end;

      TNatsCommandServer.PING:
      begin
        FConnection.SendPong;
      end;

      TNatsCommandServer.PONG:
      begin
        { TODO -opaolo -c : Manage an handler set on the Ping? 23/06/2022 11:03:35 }
      end;

      TNatsCommandServer.MSG:
      begin
        if FConnection.FSubscriptions.TryGetValue(LCommand.GetArgAsMsg.Id, LSub) then
        begin
          LSub.Received := LSub.Received + 1;
          if Assigned(LSub.Handler) then
            LSub.Handler(LCommand.GetArgAsMsg);

          if LSub.Remaining > -1 then
            LSub.Remaining := LSub.Remaining - 1;

          if LSub.Remaining = 0 then
            FConnection.FSubscriptions.Remove(LCommand.GetArgAsMsg.Id);
        end;
      end;

      TNatsCommandServer.OK:
      begin
        // Nothing to do here!
      end;

      TNatsCommandServer.ERR:
      begin
        { TODO -opaolo -c : After an ERR the server will disconnect: Reconnect 23/06/2022 10:58:11 }
      end;
    end
    else
      Sleep(100);
  end;
end;

procedure TNatsConsumer.Execute;
begin
  NameThreadForDebugging(FConnection.Name + ' Consumer');
  try
    DoExecute;
  except
    on E: Exception do
      Error := E.Message;
  end;
end;

{ TSubscription }

constructor TNatsSubscription.Create(AId: Integer; const ASubject: string; AHandler: TNatsMsgHandler);
begin
  Received := 0;
  Expected := -1;
  Remaining := -1;

  Id := AId;
  Subject := ASubject;
  Handler := AHandler;
end;

end.
