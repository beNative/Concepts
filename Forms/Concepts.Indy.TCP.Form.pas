{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.Indy.TCP.Form;

{ Demonstrates how to create a TCP socket connection using Indy 10. }

{
  REMARKS:
   - AFAICC commandhandlers cannot be used for communication that does not
     involve welcome messages and replies consisting of less than 3 characters,
     unless you write your own reply class.
}

interface

uses
  System.SysUtils, System.Actions, System.Classes, System.ImageList,
  Vcl.Dialogs, Vcl.ActnList, Vcl.ImgList, Vcl.Menus, Vcl.Controls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.Forms, Vcl.ComCtrls,

  Spring.Collections,

  DDuce.Components.LogTree, DDuce.Components.PropertyInspector, DDuce.Logger,

  zObjInspector,

  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdCmdTCPClient,
  IdContext, IdIntercept, IdGlobal, IdIOHandler, IdIOHandlerSocket,
  IdIOHandlerStack, IdCommandHandlers;

type
  TfrmIndyTCP = class(TForm)
    {$REGION 'designer controls'}
    aclMain                : TActionList;
    actClearReceived       : TAction;
    actClearSent           : TAction;
    actConnect             : TAction;
    actDisconnect          : TAction;
    actSave                : TAction;
    actSend                : TAction;
    actSendCommand         : TAction;
    btnClearReceived       : TSpeedButton;
    btnClearSent           : TSpeedButton;
    btnConnect             : TButton;
    btnDisconnect          : TButton;
    btnSendString          : TButton;
    cbxSent                : TComboBox;
    dlgSave                : TSaveDialog;
    edtPort                : TEdit;
    edtServer              : TEdit;
    IdConnectionIntercept  : TIdConnectionIntercept;
    ilMain                 : TImageList;
    mmoReceivedText        : TMemo;
    mmoSentText            : TMemo;
    mniClearReceivedText   : TMenuItem;
    mniSave                : TMenuItem;
    pgcReceived            : TPageControl;
    pgcSend                : TPageControl;
    pgcSent                : TPageControl;
    pnlCommands            : TGridPanel;
    pnlLeft                : TPanel;
    pnlLeftBottom          : TPanel;
    pnlLeftTop             : TPanel;
    pnlLeftTopTop          : TPanel;
    pnlReceived            : TPanel;
    pnlRight               : TPanel;
    pnlRightBottom         : TPanel;
    pnlRightTop            : TPanel;
    pnlSend                : TPanel;
    pnlSent                : TPanel;
    ppmReceivedText        : TPopupMenu;
    sbrMain                : TStatusBar;
    splLeftHorizontal      : TSplitter;
    splRightHorizontal     : TSplitter;
    splVertical            : TSplitter;
    tsCommands             : TTabSheet;
    tsReceivedLog          : TTabSheet;
    tsReceivedText         : TTabSheet;
    tsSentLog              : TTabSheet;
    tsSentText             : TTabSheet;
    IdTCPClient: TIdTCPClient;
    {$ENDREGION}

    procedure actClearReceivedExecute(Sender: TObject);
    procedure actClearSentExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSendExecute(Sender: TObject);

    procedure InspectorModified(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edtServerChange(Sender: TObject);
    procedure edtPortChange(Sender: TObject);
    procedure IdTCPClientStatus(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure IdConnectionInterceptReceive(ASender: TIdConnectionIntercept;
      var ABuffer: TIdBytes);


  private
    FLogIn     : TLogTree;
    FLogOut    : TLogTree;
    FInspector : TzObjectInspector;
    FUpdate    : Boolean;
    FPort      : Integer;
    FCommands  : IList<TContainedAction>;
    FServer    : string;

    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    function GetClient: TIdTCPClient;
    procedure SetServer(const Value: string);

    procedure FCommandExecute(Sender: TObject);
    function FInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

    procedure CreateCommandControls;

  protected
    function MakeLogString(const AString: string): string;

    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;

    procedure DoStringReceived(const AString: RawByteString); virtual;
    procedure UpdateActions; override;
    procedure UpdateControls; virtual;
    procedure SendString(const AString: RawByteString); virtual;

    procedure Modified;

    property Port: Integer
      read GetPort write SetPort;

    property Connected: Boolean
      read GetConnected write SetConnected;

    property LogIn: TLogTree
      read FLogIn;

    property LogOut: TLogTree
      read FLogIn;

    property Server: string
      read FServer write SetServer;

    property Client: TIdTCPClient
      read GetClient;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.AnsiStrings, System.Rtti,
  Vcl.Graphics,

  DDuce.Components.Factories,

  VirtualTrees,

  Spring.Cryptography,

  Concepts.Factories, Concepts.Settings;

const
  // conversion from a low-level control Char to its corresponding text
  // - contains at least all used LIS1-A control chars
  CTRL_TO_TEXT: array[#0..#31] of RawByteString = (
    'NUL',
    'SOH',
    'STX',
    'ETX',
    'EOT',
    'ENQ',
    'ACK',
    'BEL',
    'BS',
    'TAB',
    'LF',
    'VT',
    'FF',
    'CR',
    'SO',
    'SI',
    'DLE',
    'DC1',
    'DC2',
    'DC3',
    'DC4',
    'NAK',
    'SYN',
    'ETB',
    'CAN',
    'EM',
    'SUB',
    'ESC',
    'FS',
    'GS',
    'RS',
    'US'
  );
  SON  = '<font-color=clBlack><b>ON</b></font-color>';
  SOFF = '<font-color=clRed><b>OFF</b></font-color>';

{$REGION 'construction and destruction'}
procedure TfrmIndyTCP.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommands := TCollections.CreateObjectList<TContainedAction>(False);
  CreateCommandControls;
  LoadSettings;
  FInspector := TConceptFactories.CreatezObjectInspector(
    Self,
    pnlLeftTop,
    idTCPClient
  );
  FInspector.OnBeforeAddItem := FInspectorBeforeAddItem;
  FLogIn                 :=  TDDuceComponents.CreateLogTree(Self, tsReceivedLog);
  FLogIn.DateTimeFormat  := 'hh:nn:ss.zzz';
  FLogIn.Images          := ilMain;
  FLogOut                := TDDuceComponents.CreateLogTree(Self, tsSentLog);
  FLogOut.Images         := ilMain;
  FLogOut.DateTimeFormat := 'hh:nn:ss.zzz';
  Modified;
end;

procedure TfrmIndyTCP.BeforeDestruction;
begin
  FInspector.Component := nil;
  IdTCPClient.Intercept.OnReceive := nil;
  IdTCPClient.OnStatus := nil;
  IdTCPClient.Intercept.Disconnect;
  IdTCPClient.Disconnect;

  SaveSettings;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmIndyTCP.GetPort: Integer;
begin
  Result := FPort;
end;

procedure TfrmIndyTCP.SetPort(const Value: Integer);
begin
  if Value <> Port then
  begin
    FPort := Value;
    Client.Port := Value;
    edtPort.Text := Value.ToString;
    Modified;
  end;
end;

procedure TfrmIndyTCP.SetServer(const Value: string);
begin
  FServer := Value;
  Client.Host := FServer;
  edtServer.Text := Value;
end;

function TfrmIndyTCP.GetClient: TIdTCPClient;
begin
  Result := IdTCPClient;
end;

function TfrmIndyTCP.GetConnected: Boolean;
begin
  Result := Client.Connected;
end;

procedure TfrmIndyTCP.SetConnected(const Value: Boolean);
begin
  if Value then
  begin
    // Will connect with the default port settings.
    Client.Connect;
  end
  else
  begin
    Client.Disconnect(False);
  end;
  Modified;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmIndyTCP.actClearReceivedExecute(Sender: TObject);
begin
  FLogIn.Clear;
  mmoReceivedText.Lines.Clear;
end;

procedure TfrmIndyTCP.actClearSentExecute(Sender: TObject);
begin
  FLogOut.Clear;
  cbxSent.Items.Clear;
  cbxSent.Text := '';
  mmoSentText.Lines.Clear;
end;

procedure TfrmIndyTCP.actConnectExecute(Sender: TObject);
begin
  Port := StrToInt(edtPort.Text);
  try
    Connected := True;
    Modified;
  except
    on E: Exception do
    begin
      FLogIn.Log(E.Message, llError);
    end;
  end;
end;

procedure TfrmIndyTCP.actDisconnectExecute(Sender: TObject);
begin
  Connected := False;
  Modified;
end;

procedure TfrmIndyTCP.actSaveExecute(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    mmoSentText.Lines.SaveToFile(dlgSave.FileName);
  end;
end;

procedure TfrmIndyTCP.actSendExecute(Sender: TObject);
begin
  SendString(cbxSent.Text);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmIndyTCP.IdConnectionInterceptReceive(
  ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
begin
  //ToHex()
  //DoStringReceived(RawByteString(PAnsiChar(ABuffer)));
  DoStringReceived(ToHex(ABuffer));
  UpdateControls;
  Logger.Send('InterceptReceive', PAnsiChar(ABuffer));
end;

procedure TfrmIndyTCP.IdTCPClientStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: string);
begin
  Logger.Track('IdTCPClientStatus');
  if Assigned(FLogin) then
    FLogIn.Log(AStatusText);
 Logger.Send('ClientStatus', AStatusText);
end;

procedure TfrmIndyTCP.InspectorModified(Sender: TObject);
begin
  Modified;
end;

procedure TfrmIndyTCP.FCommandExecute(Sender: TObject);
begin
  SendString((Sender as TContainedAction).Caption);
end;

function TfrmIndyTCP.FInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not PItem.Name.Contains('ComObject');
  Result := Result and (not (PItem.Prop.PropertyType is TRttiMethodType));
end;

procedure TfrmIndyTCP.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmIndyTCP.edtPortChange(Sender: TObject);
begin
  Port := StrToInt(edtPort.Text);
end;

procedure TfrmIndyTCP.edtServerChange(Sender: TObject);
begin
  Server := edtServer.Text;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmIndyTCP.Modified;
begin
  FUpdate := True;
  UpdateActions;
end;

procedure TfrmIndyTCP.CreateCommandControls;
var
  I  : Integer;
  CA : TContainedAction;
  B  : TButton;
begin
  for I := 1 to 18 do
  begin
    CA := TControlAction.Create(aclMain);
    CA.Name := Format('actCommand%d', [I]);
    CA.OnExecute := FCommandExecute;
    FCommands.Add(CA);
    B := TButton.Create(Self);
    B.Action := CA;
    pnlCommands.ControlCollection.AddControl(B);
    B.Align := alClient;
    B.AlignWithMargins := True;
    B.Font.Style := [fsBold];
    B.Parent := pnlCommands;
  end;
end;

procedure TfrmIndyTCP.DoStringReceived(const AString: RawByteString);
begin
  if Assigned(FLogin) then
    FLogIn.Log(AString);


    //MakeLogString(string(AString)));
  mmoReceivedText.DisableAlign;
  mmoReceivedText.Text :=
    mmoReceivedText.Text + AdjustLineBreaks(AString, tlbsCRLF);
  mmoReceivedText.EnableAlign;
  // scroll to last entry
  mmoReceivedText.SelStart := Length(mmoReceivedText.Text) - 1;
  mmoReceivedText.SelLength := 1;
end;

function TfrmIndyTCP.MakeLogString(const AString: string): string;
const
  LOG_FORMAT =
    '<font-color=clSilver>' +
    '<font-family=Consolas>' +
    '[%s]' +
    '</font-family>' +
    '</font-color>';
var
  S : RawByteString;
  I : Integer;
  N : Integer;
  C : AnsiChar;
  K : RawByteString;
  R : RawByteString;
begin
  N := Length(AString);
  if N > 0 then
  begin
    SetLength(S, N);
    for I := 1 to N do
    begin
      C := AnsiChar(Byte(AString[I]));
      if Integer(C) <= $7B then // filter non-readable chars (checksum)
        S[I] := C
      else
        S[I] := '#';
    end;
  end;

  for C := Low(CTRL_TO_TEXT) to High(CTRL_TO_TEXT) do
  begin
    K := C;
    R := System.AnsiStrings.Format(LOG_FORMAT, [CTRL_TO_TEXT[C]]);
    S := System.AnsiStrings.StringReplace(S, K, R, [rfReplaceAll]);
  end;

  S := System.AnsiStrings.StringReplace(
    S, '#', System.AnsiStrings.Format(LOG_FORMAT, ['{BCC}']), [rfReplaceAll]
  );
  S := System.AnsiStrings.Format('<font-family=Terminal_Ctrl+Hex><font-size=9>%s</font-size></font-family>', [S]);
  Result := string(S);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmIndyTCP.UpdateControls;
var
  CA : TContainedAction;
begin
  actConnect.Enabled    := not Connected;
  actDisconnect.Enabled := Connected;
  actSend.Enabled       := Connected;
  for CA in FCommands do
    CA.Enabled := Connected;

  if Assigned(FInspector) then
    FInspector.UpdateProperties;
end;

procedure TfrmIndyTCP.UpdateActions;
begin
  inherited UpdateActions;

  if FUpdate then
  begin
    UpdateControls;
    FUpdate := False;
  end;
end;

procedure TfrmIndyTCP.LoadSettings;
var
  I : Integer;
begin
  Server := Settings.ReadString(UnitName, 'Server', '');
  Port   := Settings.ReadInteger(UnitName, 'Port', 0);
  for I := 0 to FCommands.Count - 1 do
  begin
    FCommands[I].Caption := Settings.ReadString(
      UnitName,
      Format('Command%d', [I + 1]),
      ''
    );
  end;
end;

procedure TfrmIndyTCP.SaveSettings;
var
  I : Integer;
begin
  Settings.WriteString(UnitName, 'Server', Server);
  Settings.WriteInteger(UnitName, 'Port', Port);
  for I := 0 to FCommands.Count - 1 do
  begin
    Settings.WriteString(
      UnitName,
      Format('Command%d', [I + 1]),
      FCommands[I].Caption
    );
  end;
end;

procedure TfrmIndyTCP.SendString(const AString: RawByteString);
var
  S  : string;
begin
  Client.IOHandler.Write(AString);

  //DoStringReceived(Client.IOHandler.AllData);
  FLogOut.Log(AString);
  S := Trim(AString);
  if cbxSent.Items.IndexOf(S) = -1 then
    cbxSent.Items.Add(S);
  mmoSentText.Lines.Add(string(AString));

  if Client.IOHandler.CheckForDataOnSource then
  begin
    Client.IOHandler.WaitFor(#0, True, False, IndyTextEncoding_ASCII, 200);
  end;
    //DoStringReceived();


end;
{$ENDREGION}

end.
