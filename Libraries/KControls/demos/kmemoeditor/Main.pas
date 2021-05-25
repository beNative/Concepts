unit Main;

interface

uses
{$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
{$ELSE}
  Windows,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, KMemoFrm;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FFrame: TKMemoFrame;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FFrame := TKMemoFrame.Create(Self);
  FFrame.Align := alClient;
  FFrame.Parent := Self;
  FFrame.OpenFile('../../../../help/kmemo/manual/kmemo_manual.rtf');
  Menu := FFrame.MainMenu;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FFrame.SaveFile(False, True);
end;

end.
