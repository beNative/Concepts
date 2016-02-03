unit BCCommon.Messages;

interface

uses
  System.UITypes;

function AskYesOrNo(Msg: string): Boolean;
function AskYesOrNoAll(Msg: string): Integer;
function MessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer; overload;
function MessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; Captions: array of string): Integer; overload;
function SaveChanges(IncludeCancel: Boolean = True): Integer;
procedure MessageBeep;
procedure ShowErrorMessage(Msg: string);
procedure ShowMessage(Msg: string);
procedure ShowWarningMessage(Msg: string);

implementation

uses
  Winapi.Windows, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, BCCommon.Language.Strings;

procedure MessageBeep;
begin
  Winapi.Windows.MessageBeep(MB_ICONASTERISK);
end;

function MessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer;
begin
  with CreateMessageDialog(Msg, DlgType, Buttons) do
  try
    HelpContext := 0;
    HelpFile := '';
    Position := poMainFormCenter;
    Result := ShowModal;
  finally
    Free;
  end;
end;

function MessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; Captions: array of string): Integer;
var
  i: Integer;
  dlgButton: TButton;
  CaptionIndex: Integer;
begin
  with CreateMessageDialog(Msg, DlgType, Buttons) do
  try
    HelpContext := 0;
    HelpFile := '';
    CaptionIndex := 0;
    { Loop through Objects in Dialog }
    for i := 0 to ComponentCount - 1 do
    begin
     { If the object is of type TButton, then }
      if (Components[i] is TButton) then
      begin
        dlgButton := TButton(Components[i]);
        if CaptionIndex > High(Captions) then
          Break;
        { Give a new caption from our Captions array}
        dlgButton.Caption := Captions[CaptionIndex];
        Inc(CaptionIndex);
      end;
    end;
    Position := poMainFormCenter;
    Result := ShowModal;
  finally
    Free;
  end;
end;

function AskYesOrNo(Msg: string): Boolean;
begin
  Result := MessageDialog(Msg, mtConfirmation, [mbYes, mbNo]) = mrYes;
end;

function AskYesOrNoAll(Msg: string): Integer;
begin
  Result := MessageDialog(Msg, mtConfirmation, [mbYes, mbYesToAll, mbNo, mbNoToAll]);
end;

function SaveChanges(IncludeCancel: Boolean): Integer;
var
  Buttons: TMsgDlgButtons;
begin
  Buttons := [mbYes, mbNO];
  if IncludeCancel then
    Buttons := Buttons + [mbCancel];

  Result := MessageDialog(LanguageDataModule.GetYesOrNoMessage('SaveChanges'), mtConfirmation, Buttons);
end;

procedure ShowMessage(Msg: string);
begin
  MessageDialog(Msg, mtInformation, [mbOK]);
end;

procedure ShowErrorMessage(Msg: string);
begin
  MessageDialog(Msg, mtError, [mbOK]);
end;

procedure ShowWarningMessage(Msg: string);
begin
  MessageDialog(Msg, mtWarning, [mbOK]);
end;

end.
