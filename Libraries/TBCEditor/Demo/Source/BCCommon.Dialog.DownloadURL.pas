unit BCCommon.Dialog.DownloadURL;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ActnList, Vcl.StdCtrls,
  BCControl.ProgressBar, Vcl.ExtActns, BCCommon.Dialog.Base, BCControl.Panel,
  Vcl.Dialogs, sDialogs, sGauge, System.Actions, Vcl.ExtCtrls, sPanel;

type
  TDownloadURLDialog = class(TBCBaseDialog)
    ActionCancel: TAction;
    ActionList: TActionList;
    ActionOK: TAction;
    ButtonCancel: TButton;
    LabelInformation: TLabel;
    PanelProgress: TBCPanel;
    PanelTop: TBCPanel;
    ProgressBar: TBCProgressBar;
    SaveDialog: TsSaveDialog;
    procedure ActionCancelExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionOKExecute(Sender: TObject);
  private
    FCancel: Boolean;
    procedure OnURLDownloadProgress(Sender: TDownLoadURL; Progress, ProgressMax: Cardinal;
      StatusCode: TURLDownloadStatus; StatusText: string; var Cancel: Boolean);
    procedure SetInformationText(Value: string);
  public
    function Open(DefaultFileName: string; DownloadURL: string): string;
  end;

procedure CheckForUpdates(AppName: string; AboutVersion: string);

function DownloadURLDialog: TDownloadURLDialog;

implementation

{$R *.dfm}

uses
  Winapi.Windows, Winapi.WinInet, Winapi.ShellApi, System.StrUtils, BCCommon.Messages,
  BCCommon.Language.Strings, BCCommon.Consts;

var
  FDownloadURLDialog: TDownloadURLDialog;

function DownloadURLDialog: TDownloadURLDialog;
begin
  if not Assigned(FDownloadURLDialog) then
    Application.CreateForm(TDownloadURLDialog, FDownloadURLDialog);
  Result := FDownloadURLDialog;
end;

procedure TDownloadURLDialog.ActionCancelExecute(Sender: TObject);
begin
  FCancel := True;
  LabelInformation.Caption := LanguageDataModule.GetConstant('DownloadCancelling');
  Repaint;
  Application.ProcessMessages;
  Close;
end;

procedure TDownloadURLDialog.FormDestroy(Sender: TObject);
begin
  FDownloadURLDialog := nil;
end;

procedure TDownloadURLDialog.SetInformationText(Value: string);
begin
  LabelInformation.Caption := Value;
  Invalidate;
  Application.ProcessMessages;
end;

function TDownloadURLDialog.Open(DefaultFileName: string; DownloadURL: string): string;
begin
  FCancel := False;
  Result := '';
  ButtonCancel.Action := ActionCancel;
  Application.ProcessMessages;
  SaveDialog.Filter := Trim(StringReplace(LanguageDataModule.GetFileTypes('Zip'), '|', #0, [rfReplaceAll])) + #0#0;
  SaveDialog.Title := LanguageDataModule.GetConstant('SaveAs');
  SaveDialog.FileName := DefaultFileName;
  SaveDialog.DefaultExt := 'zip';
  {if BCCommon.Dialog.SaveFile(Handle, '', Trim(StringReplace(LanguageDataModule.GetFileTypes('Zip')
        , '|', #0, [rfReplaceAll])) + #0#0,
        LanguageDataModule.GetConstant('SaveAs'), FilterIndex, DefaultFileName, 'zip') then  }
  if SaveDialog.Execute(Handle) then
  begin
    SetInformationText(DownloadURL);
    Application.ProcessMessages;
    with TDownloadURL.Create(Self) do
    try
      URL := DownloadURL;
      FileName := SaveDialog.Files[0];
      Result := FileName;
      OnDownloadProgress := OnURLDownloadProgress;
      ExecuteTarget(nil);
    finally
      Free;
    end;
  end
  else
    Close;
  SetInformationText(LanguageDataModule.GetConstant('DownloadDone'));
  ButtonCancel.Action := ActionOK;
end;

procedure TDownloadURLDialog.ActionOKExecute(Sender: TObject);
begin
  Close;
end;

procedure TDownloadURLDialog.OnURLDownloadProgress;
begin
  ProgressBar.Count := ProgressMax;
  ProgressBar.Progress := Progress;
  Invalidate;
  Cancel := FCancel;
  Application.ProcessMessages;
end;

function GetAppVersion(const Url:string):string;
const
  BuffSize = 64*1024;
  TitleTagBegin = '<p>';
  TitleTagEnd = '</p>';
var
  hInter: HINTERNET;
  UrlHandle: HINTERNET;
  BytesRead: Cardinal;
  Buffer: Pointer;
  i,f: Integer;
begin
  Result:='';
  hInter := InternetOpen('', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if Assigned(hInter) then
  begin
    GetMem(Buffer,BuffSize);
    try
       UrlHandle := InternetOpenUrl(hInter, PChar(Url), nil, 0, INTERNET_FLAG_RELOAD,0);
       try
        if Assigned(UrlHandle) then
        begin
          InternetReadFile(UrlHandle, Buffer, BuffSize, BytesRead);
          if BytesRead > 0 then
          begin
            SetString(Result, PAnsiChar(Buffer), BytesRead);
            i := Pos(TitleTagBegin,Result);
            if i > 0 then
            begin
              f := PosEx(TitleTagEnd,Result,i+Length(TitleTagBegin));
              Result := Copy(Result,i+Length(TitleTagBegin),f-i-Length(TitleTagBegin));
            end;
          end;
        end;
       finally
         InternetCloseHandle(UrlHandle);
       end;
    finally
      FreeMem(Buffer);
    end;
    InternetCloseHandle(hInter);
  end
end;

procedure CheckForUpdates(AppName: string; AboutVersion: string);
var
  Version: string;
  FileName: string;
begin
  try
    try
      Screen.Cursor := crHourGlass;
      Version := GetAppVersion(Format('%s/newversioncheck.php?a=%s&v=%s', [BONECODE_URL, LowerCase(AppName), AboutVersion]));
    finally
      Screen.Cursor := crDefault;
    end;

    if (Trim(Version) <> '') and (Version <> AboutVersion) then
    begin
      if AskYesOrNo(Format(LanguageDataModule.GetYesOrNoMessage('NewVersion'), [Version, AppName, CHR_DOUBLE_ENTER])) then
      begin
        {$IFDEF WIN64}
        AppName := AppName + '64';
        {$ENDIF}
        FileName := DownloadURLDialog.Open(Format('%s.zip', [AppName]), Format('%s/downloads/%s.zip', [BONECODE_URL, AppName]));
        ShellExecute(Application.Handle, PChar('explore'), nil, nil, PChar(ExtractFilePath(FileName)), SW_SHOWNORMAL);
      end;
    end
    else
      ShowMessage(LanguageDataModule.GetMessage('LatestVersion'));
  except
    on E: Exception do
      ShowErrorMessage(E.Message);
  end;
end;

end.
