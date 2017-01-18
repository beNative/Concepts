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

    {:Use this property to set the value of the DTR signal.}
    property DTR: Boolean
      read GetDTR write SetDTRF;
  end;

type
  TfrmSynapseSerial = class(TForm)
    {$REGION 'designer controls'}
    aclMain                : TActionList;
    actClearReceived       : TAction;
    actClearSent           : TAction;
    actConnect             : TAction;
    actConnectEvents       : TAction;
    actDisconnect          : TAction;
    actDisconnectEvents    : TAction;
    actSave                : TAction;
    actSend                : TAction;
    actSendMultiLine       : TAction;
    btnClearReceived       : TSpeedButton;
    btnClearSent           : TSpeedButton;
    btnConnect             : TButton;
    btnDisconnect          : TButton;
    btnSendString          : TButton;
    cbxCOMPort             : TComboBox;
    cbxSent                : TComboBox;
    dlgSave                : TSaveDialog;
    ilMain                 : TImageList;
    lblCOMPort             : TLabel;
    mmoReceivedText        : TMemo;
    mmoSentText            : TMemo;
    mniClearReceivedText   : TMenuItem;
    mniSave                : TMenuItem;
    pgcReceived            : TPageControl;
    pgcSent                : TPageControl;
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
    splLeftHorizontal      : TSplitter;
    splRightHorizontal     : TSplitter;
    splVertical            : TSplitter;
    tsReceivedLog          : TTabSheet;
    tsReceivedText         : TTabSheet;
    tsSentLog              : TTabSheet;
    tsSentText             : TTabSheet;
    cbxBaudRate            : TComboBox;
    grpFlowControl         : TGroupBox;
    chkSoftwareHandshaking : TCheckBox;
    chkHardwareHandshaking : TCheckBox;
    cbxParity              : TComboBox;
    lblSpeed               : TLabel;
    lblParity              : TLabel;
    tmrPoll                : TTimer;
    pnlIndicators          : TGridPanel;
    pnlRTS                 : TPanel;
    pnlCTS                 : TPanel;
    pnlDTR                 : TPanel;
    pnlDSR                 : TPanel;
    pnlCarrier             : TPanel;
    pnlRing                : TPanel;
    pgcSend                : TPageControl;
    tsMemo                 : TTabSheet;
    tsCommands             : TTabSheet;
    pnlCommands            : TGridPanel;
    mmoSend                : TMemo;
    sbrMain: TStatusBar;
    {$ENDREGION}

    procedure actClearReceivedExecute(Sender: TObject);
    procedure actClearSentExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSendExecute(Sender: TObject);

    procedure InspectorModified(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbxCOMPortDropDown(Sender: TObject);
    procedure cbxCOMPortChange(Sender: TObject);
    procedure actSendMultiLineExecute(Sender: TObject);
    procedure tmrPollTimer(Sender: TObject);

  private
    FLogIn     : TLogTree;
    FLogOut    : TLogTree;
    FInspector : TzObjectInspector;
    FComPort   : TSynapseSerial;
    FUpdate    : Boolean;
    FPort      : string;
    FButtons   : IList<TButton>;
    FCommands  : ILIst<TContainedAction>;

    function GetPort: string;
    procedure SetPort(const Value: string);
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    function GetBaudRate: Integer;
    procedure SetBaudRate(const Value: Integer);
    function GetDataBits: Integer;
    procedure SetDataBits(const Value: Integer);
    function GetStopBits: Integer;
    procedure SetStopBits(const Value: Integer);

    procedure ComPortStatus(
      Sender      : TObject;
      Reason      : THookSerialReason;
      const Value : string
    );
    procedure FCommandExecute(Sender: TObject);

    function CreateComPort: TSynapseSerial;
    procedure CreateControls;

    function MakeLogString(const AString: string): string;

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

    property DataBits: Integer
      read GetDataBits write SetDataBits;

    property StopBits: Integer
      read GetStopBits write SetStopBits;

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

  DDuce.Components.Factories,

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
  SON  = '<font-color=clBlack><b>ON</b></font-color>';
  SOFF = '<font-color=clRed><b>OFF</b></font-color>';

  ALPHA_NUM = 'ABCDEFGHIJKLMN' + //#13#10 +
              'OPQRSTUVWXYZ  ' + //#13#10 +
              'abcdefghijklmn' + //#13#10 +
              'opqrstuvwxyz  ' + //#13#10 +
              '0123456789    ' + #13#10;

{$REGION 'construction and destruction'}
procedure TfrmSynapseSerial.AfterConstruction;
var
  I : Integer;
begin
  inherited AfterConstruction;
  FCommands := TCollections.CreateObjectList<TContainedAction>(False);
  CreateControls;

  FComPort                   := CreateComPort;
  cbxCOMPort.Items.CommaText := GetSerialPortNames;
  LoadSettings;
  FInspector                 := TConceptFactories.CreatezObjectInspector(
    Self,
    pnlLeftTop,
    FComPort
  );
  FLogIn                     := TDDuceComponents.CreateLogTree(Self, tsReceivedLog);
  FLogIn.DateTimeFormat      := 'hh:nn:ss.zzz';
  FLogIn.Images              := ilMain;
  FLogOut                    := TDDuceComponents.CreateLogTree(Self, tsSentLog);
  FLogOut.Images             := ilMain;
  FLogOut.DateTimeFormat     := 'hh:nn:ss.zzz';
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

function TfrmSynapseSerial.GetStopBits: Integer;
begin
//
end;

procedure TfrmSynapseSerial.SetStopBits(const Value: Integer);
begin
//
end;

function TfrmSynapseSerial.GetBaudRate: Integer;
begin
  Result := StrToIntDef(cbxBaudRate.Text, 9600);
end;

procedure TfrmSynapseSerial.SetBaudRate(const Value: Integer);
begin
  cbxBaudRate.Text := Value.ToString;
end;

function TfrmSynapseSerial.GetDataBits: Integer;
begin
//
end;

procedure TfrmSynapseSerial.SetDataBits(const Value: Integer);
begin

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
      0,
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
  SendString(Trim(cbxSent.Text) + #13#10);
end;

procedure TfrmSynapseSerial.actSendMultiLineExecute(Sender: TObject);
begin
  SendString(mmoSend.Text);
end;

{$ENDREGION}

{$REGION 'event handlers'}
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
  SendString((Sender as TContainedAction).Caption + #13#10);
end;

procedure TfrmSynapseSerial.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmSynapseSerial.tmrPollTimer(Sender: TObject);
begin
  while FComPort.WaitingDataEx <> 0 do
  begin
    //DoStringReceived(FComPort.RecvTerminated(50, #13#10));
    DoStringReceived(FComPort.RecvPacket(50));
    UpdateControls;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmSynapseSerial.Modified;
begin
  FUpdate := True;
end;

procedure TfrmSynapseSerial.ComPortStatus(Sender: TObject;
  Reason: THookSerialReason; const Value: string);
var
  S: string;
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
//    FLogIn.LogFmt(
//      '%s %s',
//      [S, Value]
//    );
  end;
end;


{
baud
    Define connection speed. Baud rate can be from 50 to 4000000 bits per second. (it depends on your hardware!)
bits
    Number of bits in communication.
parity
    Define communication parity (N - None, O - Odd, E - Even, M - Mark or S - Space).
stop
    Define number of stopbits. Use constants SB1, SB1andHalf and SB2.
softflow
    Enable XON/XOFF handshake.
hardflow
    Enable CTS/RTS handshake.   }

function TfrmSynapseSerial.CreateComPort: TSynapseSerial;
begin
  Result := TSynapseSerial.Create;
  Result.OnStatus := ComPortStatus;
  Result.EnableRTSToggle(True);
  Result.ConvertLineEnd := False;
  Result.RaiseExcept := False;
  Result.InterPacketTimeout := True;
end;

procedure TfrmSynapseSerial.CreateControls;
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

procedure TfrmSynapseSerial.DoStringReceived(const AString: RawByteString);
begin
  FLogIn.Log(MakeLogString(string(AString)));
  mmoReceivedText.DisableAlign;
  mmoReceivedText.Text :=
    mmoReceivedText.Text + AdjustLineBreaks(AString, tlbsCRLF);
  mmoReceivedText.EnableAlign;
  // scroll to last entry
  mmoReceivedText.SelStart := Length(mmoReceivedText.Text) - 1;
  mmoReceivedText.SelLength := 1;
end;

function TfrmSynapseSerial.MakeLogString(const AString: string): string;
const
  LOG_FORMAT =
    '<font-color=clSilver>' +
    '<font-family=Consolas>' +
    '[%s]' +
    '</font-family>' +
    '</font-color>';
var
  S  : RawByteString;
  I  : Integer;
  N  : Integer;
  C  : AnsiChar;
  K  : RawByteString;
  R  : RawByteString;
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

  (*
  for C := Low(CTRL_TO_TEXT) to High(CTRL_TO_TEXT) do
  begin
    K := C;
    R := System.AnsiStrings.Format(LOG_FORMAT, [CTRL_TO_TEXT[C]]);
    S := System.AnsiStrings.StringReplace(S, K, R, [rfReplaceAll]);
  end;

  S := System.AnsiStrings.StringReplace(
    S, '#', System.AnsiStrings.Format(LOG_FORMAT, ['{BCC}']), [rfReplaceAll]
  );
  *)
  S := System.AnsiStrings.Format('<font-family=Terminal_Ctrl+Hex><font-size=9>%s</font-size></font-family>', [S]);
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
    FUpdate := False;
  end;
end;

procedure TfrmSynapseSerial.LoadSettings;
var
  I  : Integer;
begin
  Port := Settings.ReadString(UnitName, 'Port', '');
  BaudRate := Settings.ReadInteger(UnitName, 'BaudRate', 9600);
  for I := 0 to FCommands.Count - 1 do
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
  I  : Integer;
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

{ TSynapseSerial }

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

end.
