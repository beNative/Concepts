unit CIMTools.SynapseSerial.Form;

{ Serial connection using the synaser unit from the Ararat Synapse open source
  project. }

interface

uses
  System.SysUtils, System.Actions, System.Classes,
  Vcl.Dialogs, Vcl.ActnList, Vcl.ImgList, Vcl.Menus, Vcl.Controls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.Forms, Vcl.ComCtrls,

  DDuce.Components.LogTree, DDuce.Components.PropertyInspector,

  synaser;

type
  TfrmSynapseSerial = class(TForm)
    {$REGION 'designer controls'}
    aclMain              : TActionList;
    actClearReceived     : TAction;
    actClearSent         : TAction;
    actConnect           : TAction;
    actConnectEvents     : TAction;
    actDisconnect        : TAction;
    actDisconnectEvents  : TAction;
    actSend              : TAction;
    btnConnect           : TButton;
    btnDisconnect        : TButton;
    cbxCOMPort           : TComboBox;
    ilMain               : TImageList;
    lblCOMPort           : TLabel;
    pnlLeft              : TPanel;
    pnlLeftBottom        : TPanel;
    pnlLeftTop           : TPanel;
    pnlLeftTopTop        : TPanel;
    splLeftHorizontal    : TSplitter;
    splVertical          : TSplitter;
    actSave              : TAction;
    dlgSave              : TSaveDialog;
    ppmReceivedText      : TPopupMenu;
    mniClearReceivedText : TMenuItem;
    mniSave              : TMenuItem;
    pnlRight             : TPanel;
    splRightHorizontal   : TSplitter;
    pnlRightTop          : TPanel;
    pnlSent              : TPanel;
    btnClearSent         : TSpeedButton;
    pnlSend              : TPanel;
    cbxSent              : TComboBox;
    btnSendString        : TButton;
    pgcSent              : TPageControl;
    tsSentLog            : TTabSheet;
    tsSentText           : TTabSheet;
    mmoSentText          : TMemo;
    pnlRightBottom       : TPanel;
    pnlReceived          : TPanel;
    btnClearReceived     : TSpeedButton;
    pgcReceived          : TPageControl;
    tsReceivedLog        : TTabSheet;
    tsReceivedText       : TTabSheet;
    mmoReceivedText      : TMemo;
    mmoSend: TMemo;
    actSendMultiLine: TAction;
    btnSendMultiLine: TButton;
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

  private
    FLogIn     : TLogTree;
    FLogOut    : TLogTree;
    FInspector : TPropertyInspector;
    FComPort   : TBlockSerial;
    FUpdate    : Boolean;
    FPort      : string;

    function GetPort: string;
    procedure SetPort(const Value: string);
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);

    procedure ComPortStatus(
            Sender : TObject;
            Reason : THookSerialReason;
      const Value  : string
    );

    function CreateComPort: TBlockSerial;
    function MakeLogString(const AString: string): string;

  protected
    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;

    procedure DoStringReceived(const AString: RawByteString); virtual;
    procedure UpdateActions; override;
    procedure UpdateControls; virtual;
    procedure SendString(const AString: RawByteString); virtual;

    procedure Modified;

    property ComPort: TBlockSerial
      read FComPort;

    property Port: string
      read GetPort write SetPort;

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

  CIMTools.Helpers, CIMTools.Settings, CIMTools.Resources;

{$REGION 'construction and destruction'}
procedure TfrmSynapseSerial.AfterConstruction;
begin
  inherited;
  FComPort                   := CreateComPort;
  cbxCOMPort.ItemIndex       := cbxCOMPort.Items.IndexOf(FPort);
  cbxCOMPort.Items.CommaText := GetSerialPortNames;
  FInspector                 := CreateInspector(Self, pnlLeftTop, FComPort);
  FInspector.OnModified      := InspectorModified;
  FLogIn                     := CreateLogTree(Self, tsReceivedLog);
  FLogIn.DateTimeFormat      := 'hh:nn:ss.zzz';
  FLogIn.Images              := ilMain;
  FLogOut                    := CreateLogTree(Self, tsSentLog);
  FLogOut.Images             := ilMain;
  FLogOut.DateTimeFormat     := 'hh:nn:ss.zzz';
  Modified;
  LoadSettings;
end;

procedure TfrmSynapseSerial.BeforeDestruction;
begin
  SaveSettings;
  FComPort.Free;
  inherited;
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
  end
  else
  begin
    FComPort.CloseSocket;
  end;
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

procedure TfrmSynapseSerial.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
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
//    HR_ReadCount:   S := 'ReadCount';
//    HR_WriteCount:  S := 'WriteCount';
    HR_Wait:        S := 'Wait';
  end;
  if S <> '' then
  begin
    FLogIn.LogFmt(
      '%s %s',
      [S, Value]
    );
  end;
end;

function TfrmSynapseSerial.CreateComPort: TBlockSerial;
begin
  Result := TBlockSerial.Create;
  Result.OnStatus := ComPortStatus;
  Result.EnableRTSToggle(True);
end;

procedure TfrmSynapseSerial.DoStringReceived(const AString: RawByteString);
begin
  FLogIn.Log(MakeLogString(string(AString)));
  mmoReceivedText.DisableAlign;
  mmoReceivedText.Text := mmoReceivedText.Text + AString;
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

  for C := Low(CTRL_TO_TEXT) to High(CTRL_TO_TEXT) do
  begin
    K := C;
    R := System.AnsiStrings.Format(LOG_FORMAT, [CTRL_TO_TEXT[C]]);
    S := System.AnsiStrings.StringReplace(S, K, R, [rfReplaceAll]);
  end;

  S := System.AnsiStrings.StringReplace(
    S, '#', System.AnsiStrings.Format(LOG_FORMAT, ['{BCC}']), [rfReplaceAll]
  );
  S := System.AnsiStrings.Format('<b>%s</b>', [S]);
  Result := string(S);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmSynapseSerial.UpdateControls;
begin
  actConnect.Enabled    := not Connected;
  actDisconnect.Enabled := Connected;
  actSend.Enabled       := Connected;
  FInspector.UpdateItems;
end;

procedure TfrmSynapseSerial.UpdateActions;
begin
  inherited;
  if FUpdate then
  begin
    UpdateControls;
    FUpdate := False;
  end;
  if FComPort.WaitingData <> 0 then
  begin
    DoStringReceived(FComPort.RecvPacket(10));
    UpdateControls;
  end;
end;

procedure TfrmSynapseSerial.LoadSettings;
begin
  Port := Settings.ReadString(UnitName, 'Port', '');
end;

procedure TfrmSynapseSerial.SaveSettings;
begin
  Settings.WriteString(UnitName, 'Port', Port);
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

end.
