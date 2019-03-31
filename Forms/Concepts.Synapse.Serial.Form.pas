{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.Synapse.Serial.Form;

{ Serial connection using the synaser unit from the Ararat Synapse open source
  project. }

interface

uses
  System.SysUtils, System.Actions, System.Classes, System.ImageList,
  Vcl.Dialogs, Vcl.ActnList, Vcl.ImgList, Vcl.Menus, Vcl.Controls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.Forms, Vcl.ComCtrls,

  Spring.Collections,

  DDuce.Components.LogTree, DDuce.Components.PropertyInspector,

  zObjInspector,

  synaser;

{ TODO
  Adjustable settings:

   - Linebuffer poll interval
   - Autoscroll

   - Raw data, delimited data (use special delimiter for log visualisation)

   Receive options
     - Receive terminated
         - terminate character (used to seperate incoming data)
     - Receive fixed
}

type
  TSynapseSerial = class(TBlockSerial)
    function GetDTR: Boolean;
    function GetRTS: Boolean;
    procedure SetDTRF(Value: Boolean); override;
    procedure SetRTSF(Value: Boolean); override;

    property RTS: Boolean
      read GetRTS write SetRTSF;

    { Use this property to set the value of the DTR signal.}
    property DTR: Boolean
      read GetDTR write SetDTRF;
  end;

type
  TfrmSynapseSerial = class(TForm)
    {$REGION 'designer controls'}
    aclMain                : TActionList;
    actAssignCommand       : TAction;
    actClearCommand        : TAction;
    actClearReceived       : TAction;
    actClearSent           : TAction;
    actConnect              : TAction;
    actConnectEvents       : TAction;
    actDisconnect          : TAction;
    actDisconnectEvents    : TAction;
    actSave                : TAction;
    actSend                : TAction;
    actSendCommand         : TAction;
    actSendMultiLine       : TAction;
    btnClearReceived       : TSpeedButton;
    btnClearSent           : TSpeedButton;
    btnConnect             : TButton;
    btnDisconnect          : TButton;
    btnSendString          : TButton;
    cbxBaudRate            : TComboBox;
    cbxCOMPort             : TComboBox;
    cbxSent                : TComboBox;
    chkHardwareHandshaking : TCheckBox;
    chkSoftwareHandshaking : TCheckBox;
    dlgSave                : TSaveDialog;
    grpFlowControl         : TGroupBox;
    ilMain                 : TImageList;
    lblCOMPort             : TLabel;
    lblSpeed               : TLabel;
    mmoReceivedText        : TMemo;
    mmoSend                : TMemo;
    mmoSentText            : TMemo;
    mniAssignCommand       : TMenuItem;
    mniClearCommand        : TMenuItem;
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
    ppmCommands            : TPopupMenu;
    ppmReceivedText        : TPopupMenu;
    sbrMain                : TStatusBar;
    splLeftHorizontal      : TSplitter;
    splRightHorizontal     : TSplitter;
    splVertical            : TSplitter;
    tmrPoll                : TTimer;
    tsCommands             : TTabSheet;
    tsMemo                 : TTabSheet;
    tsReceivedLog          : TTabSheet;
    tsReceivedText         : TTabSheet;
    tsSentLog              : TTabSheet;
    tsSentText             : TTabSheet;
    pnlIndicators          : TGridPanel;
    pnlRTS                 : TPanel;
    pnlCTS                 : TPanel;
    pnlDTR                 : TPanel;
    pnlDSR                 : TPanel;
    pnlCarrier             : TPanel;
    pnlRing                : TPanel;
    {$ENDREGION}

    procedure actClearReceivedExecute(Sender: TObject);
    procedure actClearSentExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSendExecute(Sender: TObject);
    procedure actAssignCommandExecute(Sender: TObject);
    procedure actClearCommandExecute(Sender: TObject);

    procedure InspectorModified(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbxCOMPortDropDown(Sender: TObject);
    procedure cbxCOMPortChange(Sender: TObject);
    procedure actSendMultiLineExecute(Sender: TObject);
    procedure tmrPollTimer(Sender: TObject);
    procedure cbxBaudRateChange(Sender: TObject);

  private
    FLogIn     : TLogTree;
    FLogOut    : TLogTree;
    FInspector : TzObjectInspector;
    FComPort   : TSynapseSerial;
    FUpdate    : Boolean;
    FPort      : string;
    FCommands  : IList<TContainedAction>;

    function GetPort: string;
    procedure SetPort(const Value: string);
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    function GetBaudRate: Integer;
    procedure SetBaudRate(const Value: Integer);

    procedure ComPortStatus(
      Sender      : TObject;
      Reason      : THookSerialReason;
      const Value : string
    );
    procedure FCommandExecute(Sender: TObject);

    function CreateComPort: TSynapseSerial;
    procedure CreateCommandControls;

    function MakeLogString(const AString: string): string;
    procedure AssignCommandToAction(
      ASender        : TObject;
      const ACommand : string = ''
    );

  protected
    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;

    procedure DoStringReceived(const AString: RawByteString); virtual;
    procedure UpdateActions; override;
    procedure UpdateControls; virtual;
    procedure UpdateIndicators;
    procedure SendString(const AString: RawByteString); virtual;

    procedure Modified;

    property ComPort: TSynapseSerial
      read FComPort;

    property Port: string
      read GetPort write SetPort;

    property BaudRate: Integer
      read GetBaudRate write SetBaudRate;

    property Connected: Boolean
      read GetConnected write SetConnected;

    property LogIn: TLogTree
      read FLogIn;

    property LogOut: TLogTree
      read FLogIn;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.AnsiStrings,
  Vcl.Graphics,

  DDuce.Components.Factories, DDuce.Factories.zObjInspector,

  VirtualTrees,

  Concepts.Factories, Concepts.Settings;

const
  /// conversion from a low-level control Char to its corresponding text
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
  SON  = '<fc=clBlack><b>ON</b></fc>';
  SOFF = '<fc=clRed><b>OFF</b></fc>';

  ALPHA_NUM = 'ABCDEFGHIJKLMN' + //#13#10 +
              'OPQRSTUVWXYZ  ' + //#13#10 +
              'abcdefghijklmn' + //#13#10 +
              'opqrstuvwxyz  ' + //#13#10 +
              '0123456789    ' + #13#10;

{$REGION 'construction and destruction'}
procedure TfrmSynapseSerial.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommands := TCollections.CreateObjectList<TContainedAction>(False);
  CreateCommandControls;
  FComPort                   := CreateComPort;
  cbxCOMPort.Items.CommaText := GetSerialPortNames;
  LoadSettings;
  FInspector                 := TzObjectInspectorFactory.Create(
    Self,
    pnlLeftTop,
    FComPort
  );
  FLogIn                 := TDDuceComponents.CreateLogTree(Self, tsReceivedLog);
  FLogIn.DateTimeFormat  := 'hh:nn:ss.zzz';
  FLogIn.Images          := ilMain;
  FLogOut                := TDDuceComponents.CreateLogTree(Self, tsSentLog);
  FLogOut.Images         := ilMain;
  FLogOut.DateTimeFormat := 'hh:nn:ss.zzz';
  Modified;
  mmoSend.Lines.Text := ALPHA_NUM;
end;

procedure TfrmSynapseSerial.BeforeDestruction;
begin
  SaveSettings;
  FComPort.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmSynapseSerial.GetPort: string;
begin
  Result := FPort;
end;

procedure TfrmSynapseSerial.SetPort(const Value: string);
begin
  if Value <> Port then
  begin
    FPort := Value;
    cbxCOMPort.ItemIndex := cbxCOMPort.Items.IndexOf(Value);
    Modified;
  end;
end;

function TfrmSynapseSerial.GetBaudRate: Integer;
begin
  Result := StrToIntDef(cbxBaudRate.Text, 9600);
end;

procedure TfrmSynapseSerial.SetBaudRate(const Value: Integer);
begin
  cbxBaudRate.Text := Value.ToString;
end;

function TfrmSynapseSerial.GetConnected: Boolean;
begin
  Result := FComPort.InstanceActive;
end;

procedure TfrmSynapseSerial.SetConnected(const Value: Boolean);
begin
  if Value then
  begin
    // Will connect with the default port settings.
    FComPort.Connect(Port);
    FComPort.Config(
      StrToIntDef(cbxBaudRate.Text, 9600),
      8,
      'N',
 { It's very common for devices to ignore parity errors, so this setting does
   not matter. }
      0,
 { (E)USART hardware automatically adds 1 stop bit. Receiving data from a device
   that is sending 2 stop bits is exactly the same as receiving data with one
   stop bit and a little extra time between bytes.
   The UART will just wait however long it needs to until the next start bit. }
      chkSoftwareHandshaking.Checked,
      chkHardwareHandshaking.Checked
    );
  end
  else
  begin
    FComPort.CloseSocket;
  end;
  tmrPoll.Enabled := FComPort.InstanceActive;
  Modified;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmSynapseSerial.actAssignCommandExecute(Sender: TObject);
begin
  AssignCommandToAction(Sender, cbxSent.Text);
end;

procedure TfrmSynapseSerial.actClearCommandExecute(Sender: TObject);
begin
  AssignCommandToAction(Sender);
end;

procedure TfrmSynapseSerial.actClearReceivedExecute(Sender: TObject);
begin
  FLogIn.Clear;
  mmoReceivedText.Lines.Clear;
end;

procedure TfrmSynapseSerial.actClearSentExecute(Sender: TObject);
begin
  FLogOut.Clear;
  cbxSent.Items.Clear;
  cbxSent.Text := '';
  mmoSentText.Lines.Clear;
end;

procedure TfrmSynapseSerial.actConnectExecute(Sender: TObject);
begin
  Port := cbxCOMPort.Text;
  try
    Connected := True;
  except
    on E: Exception do
    begin
      FLogIn.Log(E.Message, llError);
    end;
  end;
end;

procedure TfrmSynapseSerial.actDisconnectExecute(Sender: TObject);
begin
  Connected := False;
end;

procedure TfrmSynapseSerial.actSaveExecute(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    mmoSentText.Lines.SaveToFile(dlgSave.FileName);
  end;
end;

procedure TfrmSynapseSerial.actSendExecute(Sender: TObject);
begin
  SendString(RawByteString(Trim(cbxSent.Text) + #13#10));
end;

procedure TfrmSynapseSerial.actSendMultiLineExecute(Sender: TObject);
begin
  SendString(RawByteString(mmoSend.Text));
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmSynapseSerial.cbxBaudRateChange(Sender: TObject);
begin
    FComPort.Config(
      StrToIntDef(cbxBaudRate.Text, 9600),
      8,
      'N',
 { It's very common for devices to ignore parity errors, so this setting does
   not matter. }
      0,
 { (E)USART hardware automatically adds 1 stop bit. Receiving data from a device
   that is sending 2 stop bits is exactly the same as receiving data with one
   stop bit and a little extra time between bytes.
   The UART will just wait however long it needs to until the next start bit. }
      chkSoftwareHandshaking.Checked,
      chkHardwareHandshaking.Checked
    );
end;

procedure TfrmSynapseSerial.cbxCOMPortChange(Sender: TObject);
begin
  Port := cbxCOMPort.Text;
  Modified;
end;

procedure TfrmSynapseSerial.cbxCOMPortDropDown(Sender: TObject);
begin
  cbxCOMPort.Items.CommaText := GetSerialPortNames;
end;

procedure TfrmSynapseSerial.InspectorModified(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSynapseSerial.FCommandExecute(Sender: TObject);
begin
  SendString(RawByteString((Sender as TContainedAction).Caption + #13#10));
  Modified;
end;

procedure TfrmSynapseSerial.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmSynapseSerial.tmrPollTimer(Sender: TObject);
begin
  while FComPort.WaitingDataEx <> 0 do
  begin
    DoStringReceived(FComPort.RecvPacket(50));
    Modified;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmSynapseSerial.AssignCommandToAction(ASender: TObject;
  const ACommand: string);
var
  LActionComponent : TComponent;
  LAction          : TContainedAction;
begin
  LActionComponent := (ASender as TContainedAction).ActionComponent;
  if LActionComponent is TMenuItem then
  begin
    LAction := (((TPopupMenu(
      TMenuItem(LActionComponent)
        .GetParentMenu)
          .PopupComponent) as TButton)
            .Action as TContainedAction);
    LAction.Caption := ACommand;
  end;
end;

procedure TfrmSynapseSerial.Modified;
begin
  FUpdate := True;
end;

procedure TfrmSynapseSerial.ComPortStatus(Sender: TObject;
  Reason: THookSerialReason; const Value: string);
var
  S : string;
begin
  case Reason of
    HR_SerialClose: S := 'Serial Close';
    HR_Connect:     S := 'Connect';
    HR_CanRead:     S := 'CanRead';
    HR_CanWrite:    S := 'CanWrite';
    HR_ReadCount:   S := 'ReadCount';
    HR_WriteCount:  S := 'WriteCount';
    HR_Wait:        S := 'Wait';
  end;
  if S <> '' then
  begin
    sbrMain.SimpleText := Format('%s %s', [S, Value]);
  end;
end;

{
  baud
    connection speed. Baud rate can be from 50 to 4000000 bits per second.
    (it depends on your hardware!)
  bits
    Number of bits in communication.
  parity
    communication parity (N - None, O - Odd, E - Even, M - Mark or S - Space).
  stop
    number of stopbits. Use constants SB1, SB1andHalf and SB2.
  softflow
    Enable XON/XOFF handshake.
  hardflow
    Enable CTS/RTS handshake.
}

function TfrmSynapseSerial.CreateComPort: TSynapseSerial;
begin
  Result := TSynapseSerial.Create;
  Result.OnStatus           := ComPortStatus;
  Result.ConvertLineEnd     := False;
  Result.RaiseExcept        := False;
  Result.InterPacketTimeout := True;
  Result.EnableRTSToggle(True);
end;

procedure TfrmSynapseSerial.CreateCommandControls;
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
    B                  := TButton.Create(Self);
    B.Action           := CA;
    B.Align            := alClient;
    B.AlignWithMargins := True;
    B.Font.Name        := 'Consolas';
    B.Font.Style       := [fsBold];
    B.Parent           := pnlCommands;
    B.PopupMenu        := ppmCommands;
    pnlCommands.ControlCollection.AddControl(B);
  end;
end;

procedure TfrmSynapseSerial.DoStringReceived(const AString: RawByteString);
begin
  FLogIn.Log(MakeLogString(string(AString)));
  mmoReceivedText.Lines.BeginUpdate;
  mmoReceivedText.Text :=
    mmoReceivedText.Text + AdjustLineBreaks(string(AString), tlbsCRLF);
  mmoReceivedText.Lines.EndUpdate;
end;

function TfrmSynapseSerial.MakeLogString(const AString: string): string;
const
  LOG_FORMAT =
    '<fc=clBlack>' +
    '<f=Consolas>' +
    '[%s]' +
    '</f>' +
    '</fc>';
var
  S : RawByteString;
  I : Integer;
  N : Integer;
  C : AnsiChar;
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

//  for C := Low(CTRL_TO_TEXT) to High(CTRL_TO_TEXT) do
//  begin
//    K := C;
//    R := System.AnsiStrings.Format(LOG_FORMAT, [CTRL_TO_TEXT[C]]);
//    S := System.AnsiStrings.StringReplace(S, K, R, [rfReplaceAll]);
//  end;

  S := System.AnsiStrings.StringReplace(
    S, '#', System.AnsiStrings.Format(LOG_FORMAT, ['{BCC}']), [rfReplaceAll]
  );

  S := System.AnsiStrings.Format('<fc=clBlack><f=Terminal_Ctrl+Hex><fs=9>%s</fs></f></fc>', [S]);
  Result := string(S);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmSynapseSerial.UpdateControls;
var
  CA : TContainedAction;
begin
  actConnect.Enabled       := not Connected;
  actDisconnect.Enabled    := Connected;
  actSend.Enabled          := Connected;
  actSendMultiLine.Enabled := Connected;
  for CA in FCommands do
    CA.Enabled := Connected;

  FInspector.UpdateProperties;
  UpdateIndicators;
end;

procedure TfrmSynapseSerial.UpdateIndicators;

  procedure ColorPanel(APanel : TPanel; AActive: Boolean);
  begin
    if AActive then
      APanel.Color := clLime
    else
      APanel.Color := clBtnFace;
  end;

begin
  if Connected then
  begin
    //ColorPanel(pnlRTS, FComPort.RTS);
    ColorPanel(pnlCTS, FComPort.CTS);
    //ColorPanel(pnlDTR, FComPort.DTR);
    ColorPanel(pnlDSR, FComPort.DSR);
    ColorPanel(pnlCarrier, FComPort.Carrier);
    ColorPanel(pnlRing, FComPort.Ring);
  end;
end;

procedure TfrmSynapseSerial.UpdateActions;
begin
  inherited UpdateActions;
  UpdateIndicators;
  if FUpdate then
  begin
    UpdateControls;
      // scroll to last entry
    mmoReceivedText.SelStart := Length(mmoReceivedText.Text) - 1;
    mmoReceivedText.SelLength := 1;
    FUpdate := False;
  end;
end;

procedure TfrmSynapseSerial.LoadSettings;
var
  I : Integer;
begin
  Port     := Settings.ReadString(UnitName, 'Port', '');
  BaudRate := Settings.ReadInteger(UnitName, 'BaudRate', 9600);
  for I    := 0 to FCommands.Count - 1 do
  begin
    FCommands[I].Caption := Settings.ReadString(
      UnitName,
      Format('Command%d', [I + 1]),
      ''
    );
  end;
end;

procedure TfrmSynapseSerial.SaveSettings;
var
  I : Integer;
begin
  Settings.WriteString(UnitName, 'Port', Port);
  Settings.WriteInteger(UnitName, 'BaudRate', BaudRate);
  for I := 0 to FCommands.Count - 1 do
  begin
    Settings.WriteString(
      UnitName,
      Format('Command%d', [I + 1]),
      FCommands[I].Caption
    );
  end;
end;

procedure TfrmSynapseSerial.SendString(const AString: RawByteString);
var
  S : string;
begin
  FComPort.SendString(AString);
  FLogOut.Log(MakeLogString(string(AString)));
  S := Trim(string(AString));
  if cbxSent.Items.IndexOf(S) = -1 then
    cbxSent.Items.Add(S);
  mmoSentText.Lines.Add(string(AString));
end;
{$ENDREGION}

{$REGION 'TSynapseSerial'}
function TSynapseSerial.GetDTR: Boolean;
begin
  Result := False;
end;

function TSynapseSerial.GetRTS: Boolean;
begin
  Result := False;
end;

procedure TSynapseSerial.SetDTRF(Value: Boolean);
begin
  inherited SetDTRF(Value);
end;

procedure TSynapseSerial.SetRTSF(Value: Boolean);
begin
  inherited SetRTSF(Value);
end;
{$ENDREGION}

end.
