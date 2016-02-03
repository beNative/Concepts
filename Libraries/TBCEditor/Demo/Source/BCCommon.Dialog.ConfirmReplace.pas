unit BCCommon.Dialog.ConfirmReplace;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,
  BCCommon.Dialog.Base, BCControl.Panel, sPanel;

type
  TConfirmReplaceDialog = class(TBCBaseDialog)
    ButtonCancel: TButton;
    ButtonNo: TButton;
    ButtonYes: TButton;
    ButtonYesToAll: TButton;
    Image: TImage;
    LabelConfirmation: TLabel;
    PanelBottom: TBCPanel;
    PanelClient: TBCPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure ReadIniFile;
    procedure WriteIniFile;
  public
    class function ClassShowModal(AOwner: TComponent; AConfirmText: string): Integer;
  end;

implementation

{$R *.DFM}

uses
  System.IniFiles, BCCommon.FileUtils, BCCommon.Utils;

class function TConfirmReplaceDialog.ClassShowModal(AOwner: TComponent; AConfirmText: string): Integer;
begin
  with TConfirmReplaceDialog.Create(AOwner) do
  try
    LabelConfirmation.Caption := AConfirmText;
    Result := ShowModal;
  finally
    Free;
  end;
end;

procedure TConfirmReplaceDialog.FormCreate(Sender: TObject);
begin
  Image.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
end;

procedure TConfirmReplaceDialog.FormDestroy(Sender: TObject);
begin
  WriteIniFile;
  inherited;
end;

procedure TConfirmReplaceDialog.FormShow(Sender: TObject);
begin
  inherited;
  ReadIniFile;
end;

procedure TConfirmReplaceDialog.ReadIniFile;
begin
  with TMemIniFile.Create(GetIniFilename) do
  try
    Left := ReadInteger('ConfirmReplacePosition', 'Left', (Screen.Width - Width) div 2);
    Top := ReadInteger('ConfirmReplacePosition', 'Top', (Screen.Height - Height) div 2);
    { Check if the form is outside the workarea }
    Left := SetFormInsideWorkArea(Left, Width);
  finally
    Free;
  end;
end;

procedure TConfirmReplaceDialog.WriteIniFile;
begin
  with TMemIniFile.Create(GetIniFilename) do
  try
    WriteInteger('ConfirmReplacePosition', 'Left', Left);
    WriteInteger('ConfirmReplacePosition', 'Top', Top);
  finally
    UpdateFile;
    Free;
  end;
end;

end.
