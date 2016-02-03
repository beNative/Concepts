unit BCCommon.Dialog.Base;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms;

type
  TDialogType = (dtOpen, dtEdit);

  TBCBaseDialog = class(TForm)
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FOrigHeight: Integer;
    FOrigWidth: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    property OrigHeight: Integer read FOrigHeight write FOrigHeight;
    property OrigWidth: Integer read FOrigWidth write FOrigWidth;
  end;

implementation

{$R *.dfm}

{$IFDEF EDITBONE}
uses
  BCCommon.Language.Utils;
{$ENDIF}

constructor TBCBaseDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrigWidth := Width;
  FOrigHeight := Height;
end;

procedure TBCBaseDialog.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    Key := #0; { no beep }
    Close;
  end;
end;

procedure TBCBaseDialog.FormShow(Sender: TObject);
begin
  {$IFDEF EDITBONE}
  BCCommon.Language.Utils.UpdateLanguage(Self);
  {$ENDIF}
end;

end.
