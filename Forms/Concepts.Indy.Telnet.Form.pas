{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.Indy.Telnet.Form;

{ Demonstrates a Telnet terminal using Indy 10. }

interface

uses
  System.SysUtils, System.Actions, System.Classes, System.ImageList,
  Vcl.Dialogs, Vcl.ActnList, Vcl.Menus, Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, Vcl.Forms, Vcl.ComCtrls, Vcl.ImgList,

  Spring.Collections,

  DDuce.Components.LogTree, DDuce.Logger,

  zObjInspector, zObjInspTypes,

  IdComponent, IdTCPConnection, IdTCPClient, IdGlobal, IdIOHandler, IdTelnet,
  IdIOHandlerStack, IdBaseComponent;

type
  TfrmIndyTelnet = class(TForm)
    {$REGION 'designer controls'}
    aclMain                : TActionList;
    actAssignCommand       : TAction;
    actClearCommand        : TAction;
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
    IdTelnet               : TIdTelnet;
    ilMain                 : TImageList;
    mmoReceivedText        : TMemo;
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
    tsCommands             : TTabSheet;
    tsReceivedLog          : TTabSheet;
    tsReceivedText         : TTabSheet;
    tsSentLog              : TTabSheet;
    tsSentText             : TTabSheet;
    {$ENDREGION}

    procedure actClearReceivedExecute(Sender: TObject);
    procedure actClearSentExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSendExecute(Sender: TObject);
    procedure actAssignCommandExecute(Sender: TObject);
    procedure actClearCommandExecute(Sender: TObject);
    procedure actSendCommandExecute(Sender: TObject);

    procedure InspectorModified(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edtServerChange(Sender: TObject);
    procedure edtPortChange(Sender: TObject);
    procedure IdTelnetDataAvailable(Sender: TIdTelnet; const Buffer: TIdBytes);
    procedure IdTelnetStatus(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure IdTelnetTelnetCommand(Sender: TIdTelnet;
      Status: TIdTelnetCommand);
    procedure IdTelnetConnected(Sender: TObject);
    procedure IdTelnetDisconnected(Sender: TObject);
    procedure IdTelnetWork(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
    procedure IdTelnetWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure IdTelnetWorkEnd(ASender: TObject; AWorkMode: TWorkMode);

  private
    FLogIn     : TLogTree;
    FLogOut    : TLogTree;
    FInspector : TzObjectInspector;
    FUpdate    : Boolean;
    FPort      : Integer;
    FCommands  : IList<TContainedAction>;
    FServer    : string;

    {$REGION 'property access methods'}
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    function GetClient: TIdTelnet;
    procedure SetServer(const Value: string);
    {$ENDREGION}

    procedure FCommandExecute(Sender: TObject);
    function FInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

    procedure CreateCommandControls;
    procedure AssignCommandToAction(
      ASender        : TObject;
      const ACommand : string = ''
    );

  protected
    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;

    procedure DoStringReceived(const AString: string); virtual;
    procedure UpdateActions; override;
    procedure UpdateControls; virtual;
    procedure SendString(const AString: string); virtual;

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

    property Client: TIdTelnet
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

  DDuce.Components.Factories, DDuce.Factories.zObjInspector,

  VirtualTrees,

  Concepts.Settings;

{$REGION 'construction and destruction'}
procedure TfrmIndyTelnet.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommands := TCollections.CreateObjectList<TContainedAction>(False);
  CreateCommandControls;
  LoadSettings;
  FInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlLeftTop,
    Client
  );
  FInspector.OnBeforeAddItem := FInspectorBeforeAddItem;
  FLogIn                     :=  TDDuceComponents.CreateLogTree(Self, tsReceivedLog);
  FLogIn.DateTimeFormat      := 'hh:nn:ss.zzz';
  FLogIn.AutoLogLevelColors  := True;
  FLogOut                    := TDDuceComponents.CreateLogTree(Self, tsSentLog);
  FLogOut.DateTimeFormat     := 'hh:nn:ss.zzz';
  FLogOut.AutoLogLevelColors := True;
  Modified;
end;

procedure TfrmIndyTelnet.BeforeDestruction;
begin
  FInspector.Component := nil;
  Client.OnStatus := nil;
  Client.Disconnect;

  SaveSettings;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmIndyTelnet.GetPort: Integer;
begin
  Result := FPort;
end;

procedure TfrmIndyTelnet.SetPort(const Value: Integer);
begin
  if Value <> Port then
  begin
    FPort := Value;
    Client.Port := Value;
    edtPort.Text := Value.ToString;
    Modified;
  end;
end;

procedure TfrmIndyTelnet.SetServer(const Value: string);
begin
  FServer := Value;
  Client.Host := FServer;
  edtServer.Text := Value;
end;

function TfrmIndyTelnet.GetClient: TIdTelnet;
begin
  Result := IdTelnet;
end;

function TfrmIndyTelnet.GetConnected: Boolean;
begin
  Result := Client.Connected;
end;

procedure TfrmIndyTelnet.SetConnected(const Value: Boolean);
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
procedure TfrmIndyTelnet.actAssignCommandExecute(Sender: TObject);
begin
  AssignCommandToAction(Sender, cbxSent.Text);
end;

procedure TfrmIndyTelnet.actClearCommandExecute(Sender: TObject);
begin
  AssignCommandToAction(Sender);
end;

procedure TfrmIndyTelnet.actClearReceivedExecute(Sender: TObject);
begin
  FLogIn.Clear;
  mmoReceivedText.Lines.Clear;
end;

procedure TfrmIndyTelnet.actClearSentExecute(Sender: TObject);
begin
  FLogOut.Clear;
  cbxSent.Items.Clear;
  cbxSent.Text := '';
  mmoSentText.Lines.Clear;
end;

procedure TfrmIndyTelnet.actConnectExecute(Sender: TObject);
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

procedure TfrmIndyTelnet.actDisconnectExecute(Sender: TObject);
begin
  Connected := False;
  Modified;
end;

procedure TfrmIndyTelnet.actSaveExecute(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    mmoSentText.Lines.SaveToFile(dlgSave.FileName);
  end;
end;

procedure TfrmIndyTelnet.actSendCommandExecute(Sender: TObject);
begin
//
end;

procedure TfrmIndyTelnet.actSendExecute(Sender: TObject);
begin
  SendString(cbxSent.Text);
  cbxSent.Text := '';
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmIndyTelnet.IdTelnetConnected(Sender: TObject);
begin
  Logger.Info('Client connected.');
end;

procedure TfrmIndyTelnet.IdTelnetDataAvailable(Sender: TIdTelnet;
  const Buffer: TIdBytes);
begin
  DoStringReceived(BytesToString(Buffer));
  UpdateControls;
end;

procedure TfrmIndyTelnet.IdTelnetDisconnected(Sender: TObject);
begin
  Logger.Info('Client disconnected');
end;

procedure TfrmIndyTelnet.IdTelnetStatus(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
begin
  if Assigned(FLogin) then
    FLogIn.Log(AStatusText);
  Logger.Info(AStatusText);
end;

procedure TfrmIndyTelnet.IdTelnetTelnetCommand(Sender: TIdTelnet;
  Status: TIdTelnetCommand);
begin
  Logger.Track(Self, 'IdTelnetTelnetCommand');
  Logger.Send('Status', TValue.From(Status));
end;

procedure TfrmIndyTelnet.IdTelnetWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  Logger.Track(Self, 'IdTelnetWork');
  Logger.Send('AWorkMode', TValue.From(AWorkMode));
  Logger.Send('AWorkCount', AWorkCount);
end;

procedure TfrmIndyTelnet.IdTelnetWorkBegin(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  Logger.Track(Self, 'IdTelnetWorkBegin');
  Logger.Send('AWorkMode', TValue.From(AWorkMode));
  Logger.Send('AWorkCountMax', AWorkCountMax);
end;

procedure TfrmIndyTelnet.IdTelnetWorkEnd(ASender: TObject;
  AWorkMode: TWorkMode);
begin
  Logger.Track(Self, 'IdTelnetWorkEnd');
  Logger.Send('AWorkMode', TValue.From(AWorkMode));
end;

procedure TfrmIndyTelnet.InspectorModified(Sender: TObject);
begin
  Modified;
end;

procedure TfrmIndyTelnet.FCommandExecute(Sender: TObject);
begin
  SendString((Sender as TContainedAction).Caption);
  Modified;
end;

function TfrmIndyTelnet.FInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not PItem.Name.Contains('ComObject');
  Result := Result and (not (PItem.Prop.PropertyType is TRttiMethodType));
end;

procedure TfrmIndyTelnet.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmIndyTelnet.edtPortChange(Sender: TObject);
begin
  Port := StrToInt(edtPort.Text);
end;

procedure TfrmIndyTelnet.edtServerChange(Sender: TObject);
begin
  Server := edtServer.Text;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmIndyTelnet.AssignCommandToAction(ASender: TObject;
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

procedure TfrmIndyTelnet.Modified;
begin
  FUpdate := True;
  UpdateActions;
end;

procedure TfrmIndyTelnet.CreateCommandControls;
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
    B.Font.Name  := 'Consolas';
    B.Font.Style := [fsBold];
    B.Parent := pnlCommands;
    B.PopupMenu := ppmCommands;
  end;
end;

procedure TfrmIndyTelnet.DoStringReceived(const AString: string);
begin
  Logger.SendText(AString);
  if Assigned(FLogin) then
    FLogIn.Log(AString);
  mmoReceivedText.DisableAlign;
  mmoReceivedText.Text :=
    mmoReceivedText.Text + AdjustLineBreaks(AString, tlbsCRLF);
  mmoReceivedText.EnableAlign;
  // scroll to last entry
  mmoReceivedText.SelStart := Length(mmoReceivedText.Text) - 1;
  mmoReceivedText.SelLength := 1;
  Modified;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmIndyTelnet.UpdateControls;
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

procedure TfrmIndyTelnet.UpdateActions;
begin
  inherited UpdateActions;

  if FUpdate then
  begin
    UpdateControls;
    FUpdate := False;
  end;
end;

procedure TfrmIndyTelnet.LoadSettings;
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

procedure TfrmIndyTelnet.SaveSettings;
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

procedure TfrmIndyTelnet.SendString(const AString: string);
begin
  // TODO: make this a setting
  FLogIn.Clear;
  mmoReceivedText.Lines.Clear;

  Client.SendString(AString + EOL);

  FLogOut.Log(AString);
  if cbxSent.Items.IndexOf(AString) = -1 then
    cbxSent.Items.Add(AString);
  mmoSentText.Lines.Add(AString);
end;
{$ENDREGION}

end.
