{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kmessagebox; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Classes, Controls, Forms, KFunctions, KEdits
  {$IFDEF USE_THEMES}
  , Themes
   {$IFNDEF FPC}
  , UxTheme
   {$ENDIF}
  {$ENDIF}
  ;

type
  TKMsgBoxButton = (mbYes, mbNo, mbOK, mbCancel, mbClose, mbAbort, mbRetry, mbIgnore,
    mbAll, mbNoToAll, mbYesToAll, mbHelp);
  TKMsgBoxIcon = (miNone, miInformation, miQuestion, miWarning, miStop);

function CreateMsgBox(const Caption, Text: string;
  const Buttons: array of TKMsgBoxButton; Icon: TKMsgBoxIcon = miNone;
  Def: integer = 0): TCustomForm;
function CreateMsgBoxEx(const Caption, Text: string; const Btns: array of string;
  Icon: TKMsgBoxIcon = miNone; Def: integer = 0): TCustomForm;
procedure FreeMsgBox(AMsgBox: TCustomForm);

function KMsgBox(const Caption, Text: string; const Buttons: array of TKMsgBoxButton;
  Icon: TKMsgBoxIcon = miNone; Def: integer = 0): integer;
function KMsgBoxEx(const Caption, Text: string; const Buttons: array of string;
  Icon: TKMsgBoxIcon = miNone; Def: integer = 0): integer;

function KInputBox(const Caption, Prompt: string; var Text: string): TModalResult;

function KNumberInputBox(const ACaption, APrompt: string; var AValue: double;
  AMin, AMax: double; AFormats: TKNumberEditAcceptedFormats = [neafDec]): TModalResult;

//Win32 API message box
type
  TKMsgBoxButtons = (mbAbortRetryIgnore, mbOkOnly, mbOkCancel,
    mbRetryCancel, mbYesNo, mbYesNoCancel);

function MsgBox(const Caption, Text: string; const Buttons: TKMsgBoxButtons;
  Icon: TKMsgBoxIcon = miNone): integer;
function AppMsgBox(const Caption, Text: string; Flags: integer): integer;

implementation

uses
  Math, StdCtrls, ExtCtrls, SysUtils, KGraphics, KControls, KRes;

type
  TMsgBoxForm = class(TCustomForm)
  private
    FCloseResult: integer;
    procedure SetCloseResult(const Value: integer);
    procedure WMCloseQuery(var M: TLMessage); message LM_CLOSEQUERY;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    property CloseResult: integer read FCloseResult write SetCloseResult;
  end;

function StdButtons(ButtonType: TKMsgBoxButton): string;
begin
  case ButtonType of
    mbYes: Result := sMsgBoxYes;
    mbNo: Result := sMsgBoxNo;
    mbOK: Result := sMsgBoxOk;
    mbCancel: Result := sMsgBoxCancel;
    mbClose: Result := sMsgBoxClose;
    mbAbort: Result := sMsgBoxAbort;
    mbRetry: Result := sMsgBoxRetry;
    mbIgnore: Result := sMsgBoxIgnore;
    mbAll: Result := sMsgBoxAll;
    mbNoToAll: Result := sMsgBoxNoToAll;
    mbYesToAll: Result := sMsgBoxYesToAll;
    mbHelp: Result := sMsgBoxHelp;
  end;
end;

{ TMsgBoxForm }

constructor TMsgBoxForm.Create(AOwner: TComponent);
begin
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner);
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

procedure TMsgBoxForm.SetCloseResult(const Value: integer);
begin
  FCloseResult := Value;
end;

procedure TMsgBoxForm.WMCloseQuery(var M: TLMessage);
begin
  // we need new behavior here
  ModalResult := FCloseResult;
end;

function CreateMsgBox(const Caption, Text: string;
  const Buttons: array of TKMsgBoxButton; Icon: TKMsgBoxIcon = miNone;
  Def: integer = 0): TCustomForm;
var
  Btns: array of string;
  I: integer;
begin
  SetLength(Btns, Length(Buttons));
  for I := 0 to High(Buttons) do
    Btns[I] := StdButtons(Buttons[I]);
  Result := CreateMsgBoxEx(Caption, Text, Btns, Icon, Def);
  Btns := nil;
end;

function CreateMsgBoxEx(const Caption, Text: string; const Btns: array of string;
  Icon: TKMsgBoxIcon = miNone; Def: integer = 0): TCustomForm;

  function FindBtn(const ACaption: string): integer;
  var
    I: integer;
  begin
    Result := -1;
    for I := Low(Btns) to High(Btns) do
      if Btns[I] = ACaption then
      begin
        Result := I;
        Exit;
      end;
  end;

var
  F: TMsgBoxForm;
  B: TButton;
  La: TLabel;
  TB: TKTextBox;
  I, L, L1, L2, W, H, H1: integer;
  ICancel, INo: integer;
  IsCancel, IsFirstBtn: boolean;
{$IFDEF USE_PNG_SUPPORT}
  Png: TKPngImage;
  Im: TImage;
  S: string;
{$ENDIF}
begin
  F := TMsgBoxForm.Create(Application);
  try
    IsFirstBtn := True;
    IsCancel := False;
    F.BorderStyle := bsDialog;
    F.Position := poScreenCenter;
    F.Caption := Caption;
    La := TLabel.Create(F);
    TB := TKTextBox.Create;
    try
      TB.Text := Text;
      TB.Attributes := [taLineBreak];
      TB.Measure(F.Canvas, CreateEmptyRect, W, H);
    finally
      TB.Free;
    end;
    La.Caption := Text;
    La.AutoSize := False;
    La.Width := W + 20;
    La.Height := H + 10;
    La.Parent := F;
    L := 20;
    H1 := 0;
  {$IFDEF USE_PNG_SUPPORT}
    if Icon <> miNone then
    begin
      Im := TImage.Create(F);
      Png := TKPngImage.Create;
      try
        case Icon of
          miInformation: S := 'KMESSAGEBOX_INFO';
          miQuestion: S := 'KMESSAGEBOX_QUESTION';
          miWarning: S := 'KMESSAGEBOX_WARNING';
          miStop: S := 'KMESSAGEBOX_STOP';
        end;
      {$IFDEF FPC}
        Png.LoadFromLazarusResource(S);
      {$ELSE}
        Png.LoadFromResourceName(HInstance, S);
      {$ENDIF}
        Im.Picture.Assign(Png);
        Im.Width := Png.Width;
        Im.Height := Png.Height;
      finally
        Png.Free;
      end;
      Im.Transparent := True;
      Im.Parent := F;
      Im.Left := L;
      Inc(L, Im.Width + 15);
      H1 := Im.Height;
      Im.Top := 15 + Max(0, (La.Height - H1) div 2);
    end;
  {$ENDIF}
    La.Left := L;
    La.Top := 15 + Max(0, (H1 - La.Height) div 2);
    L1 := 20;
    for I := Low(Btns) to High(Btns) do
      Inc(L1, Max(F.Canvas.TextWidth(Btns[I]) + 30, 90));
    Dec(L1, 10);
    L2 := L + LA.Width;
    H1 := 30 + Max(La.Height, H1);
    H := H1;
    if L2 > L1 then
      L := (L2 - L1) div 2
    else
      L := 0;
    Inc(L, 20);
    ICancel := FindBtn(sMsgBoxCancel);
    INo := FindBtn(sMsgBoxNo);
    for I := Low(Btns) to High(Btns) do
    begin
      W := Max(F.Canvas.TextWidth(Btns[I]) + 20, 80);
      B := TButton.Create(F);
      B.Parent := F;
      B.Caption := Btns[I];
      B.Left := L;
      B.Top := H;
      B.Width := W;
      if IsFirstBtn then
      begin
        Inc(H1, B.Height);
        IsFirstBtn := False;
      end;
      if Def = I then
        F.ActiveControl := B;
      if not IsCancel and ((Length(Btns) = 1) or (ICancel = I) or
        (INo = I) and (ICancel < 0)) then
      begin
        B.Cancel := True;
        F.CloseResult := I + 1;
        IsCancel := True;
      end;
      B.ModalResult := TModalResult(I + 1);
      Inc(L, W + 10);
    end;
    F.ClientWidth := Max(L1, L2) + 20;
    F.ClientHeight := H1 + 15;
  {$IFNDEF FPC}
    // this just disables the cancel box in system menu, not absolutely needed
    if not IsCancel then
      EnableMenuItem(GetSystemMenu(F.Handle, False), SC_CLOSE, MF_DISABLED);
  {$ENDIF}
  except
    FreeAndNil(F);
  end;
  Result := TCustomForm(F);
end;

procedure FreeMsgBox(AMsgBox: TCustomForm);
begin
  AMsgBox.Free;
end;

function KMsgBox(const Caption, Text: string; const Buttons: array of TKMsgBoxButton;
  Icon: TKMsgBoxIcon = miNone; Def: integer = 0): integer;
var
  F: TCustomForm;
begin
  F := CreateMsgBox(Caption, Text, Buttons, Icon, Def);
  try
    if F <> nil then
    begin
      DPIScaleControl(F);
      Result := F.ShowModal
    end else
      Result := -1;
  finally
    F.Free;
  end;
end;

function KMsgBoxEx(const Caption, Text: string; const Buttons: array of string;
  Icon: TKMsgBoxIcon = miNone; Def: integer = 0): integer;
var
  F: TCustomForm;
begin
  F := CreateMsgBoxEx(Caption, Text, Buttons, Icon, Def);
  try
    if F <> nil then
      Result := F.ShowModal
    else
      Result := -1;
  finally
    F.Free;
  end;
end;

function KInputBox(const Caption, Prompt: string; var Text: string): TModalResult;
var
  F: TForm;
  L: TLabel;
  E: TEdit;
  BUOk, BUCancel: TButton;
  W, I: integer;
begin
  F := TForm.Create(Application);
  if F <> nil then
  begin
    F.Caption := Caption;
    F.BorderStyle := bsDialog;
    F.Position := poScreenCenter;
    L := TLabel.Create(F);
    L.Parent := F;
    E := TEdit.Create(F);
    E.Parent := F;
    BUOk := TButton.Create(F);
    BUOk.Parent := F;
    BUCancel := TButton.Create(F);
    BUCancel.Parent := F;
    try
      L.Caption := Prompt;
      W := Max(160, L.Canvas.TextWidth(Prompt));
      F.Width := W + 60;
      F.Height := 140;
      I := (F.Width - F.ClientWidth) div 2;
      L.Left := 30 - I;
      L.Top := 15;
      L.FocusControl := E;
      E.Left := L.Left;
      E.Top := L.Top + 18;
      E.Width := Min(160, W);
      E.Text := Text;
      BUOk.Left := (F.Width - BUOk.Width - BUCancel.Width - 10) div 2 - I;
      BUOk.Top := E.Top + 35;
      BUOk.Caption := sMsgBoxOK;
      BUOk.Default := True;
      BUOk.ModalResult := mrOk;
      BUCancel.Left := BUOk.Left + BUOk.Width + 10;
      BUCancel.Top := BUOk.Top;
      BUCancel.Caption := sMsgBoxCancel;
      BUCancel.Cancel := True;
      BUCancel.ModalResult := mrCancel;
      CenterWindowOnScreen(F.Handle);
      Result := F.ShowModal;
      if Result = mrOk then
        Text := E.Text;
    finally
      F.Free;
    end;
  end
  else
    Result := mrCancel;
end;

function KNumberInputBox(const ACaption, APrompt: string; var AValue: double;
  AMin, AMax: double; AFormats: TKNumberEditAcceptedFormats = [neafDec]): TModalResult;
var
  F: TForm;
  L: TLabel;
  E: TKNumberEdit;
  BUOk, BUCancel: TButton;
  W, I: integer;
begin
  F := TForm.Create(Application);
  if F <> nil then
  begin
    F.Caption := ACaption;
    F.BorderStyle := bsDialog;
    L := TLabel.Create(F);
    L.Parent := F;
    E := TKNumberEdit.Create(F);
    E.Parent := F;
    BUOk := TButton.Create(F);
    BUOk.Parent := F;
    BUCancel := TButton.Create(F);
    BUCancel.Parent := F;
    try
      L.Caption := APrompt;
      W := Max(160, L.Canvas.TextWidth(APrompt));
      F.Width := W + 60;
      F.Height := 140;
      I := (F.Width - F.ClientWidth) div 2;
      L.Left := 30 - I;
      L.Top := 15;
      L.FocusControl := E;
      E.Left := L.Left;
      E.Top := L.Top + 18;
      E.Width := Min(160, W);
      E.Options := E.Options - [neoUseUpDown, neoUseLabel];
      E.AcceptedFormats := AFormats;
      E.Value := AValue;
      E.Min := AMin;
      E.Max := AMax;
      BUOk.Left := (F.Width - BUOk.Width - BUCancel.Width - 10) div 2 - I;
      BUOk.Top := E.Top + 35;
      BUOk.Caption := sMsgBoxOK;
      BUOk.Default := True;
      BUOk.ModalResult := mrOk;
      BUCancel.Left := BUOk.Left + BUOk.Width + 10;
      BUCancel.Top := BUOk.Top;
      BUCancel.Caption := sMsgBoxCancel;
      BUCancel.Cancel := True;
      BUCancel.ModalResult := mrCancel;
      CenterWindowOnScreen(F.Handle);
      Result := F.ShowModal;
      if Result = mrOk then
        AValue := E.Value;
    finally
      F.Free;
    end;
  end
  else
    Result := mrCancel;
end;

function MsgBox(const Caption, Text: string; const Buttons: TKMsgBoxButtons;
  Icon: TKMsgBoxIcon): integer;
const
  WinButtons: array[TKMsgBoxButtons] of integer = (MB_ABORTRETRYIGNORE, MB_OK,
    MB_OKCANCEL, MB_RETRYCANCEL, MB_YESNO, MB_YESNOCANCEL);
  WinIcon: array[TKMsgBoxIcon] of integer = (0, MB_ICONINFORMATION, MB_ICONQUESTION,
    MB_ICONEXCLAMATION, MB_ICONSTOP);
begin
  Result := MessageBox(Application.MainForm.Handle, PChar(Text), PChar(Caption),
    WinButtons[Buttons] or WinIcon[Icon]);
end;

function AppMsgBox(const Caption, Text: string; Flags: integer): integer;
begin
  Result := Application.MessageBox(PChar(Text), PChar(Caption), Flags);
end;

{$IFDEF FPC}
initialization
  {$i kmessagebox.lrs}
{$ELSE}
  {$R kmessagebox.res}
{$ENDIF}
end.
