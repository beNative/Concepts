unit kpictureeditor;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TKPictureEditForm = class(TForm)
    BULoad: TButton;
    BUSave: TButton;
    BUOK: TButton;
    ODMain: TOpenDialog;
    SDMain: TSaveDialog;
    BUCancel: TButton;
    PNMain: TPanel;
    IMMain: TImage;
    CBStretch: TCheckBox;
    BUClear: TButton;
    procedure BULoadClick(Sender: TObject);
    procedure BUSaveClick(Sender: TObject);
    procedure CBStretchClick(Sender: TObject);
    procedure BUClearClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TKPictureEditForm.BUClearClick(Sender: TObject);
begin
  IMMain.Picture.Graphic := nil;
end;

procedure TKPictureEditForm.BULoadClick(Sender: TObject);
begin
  if ODMain.Execute then
    IMMain.Picture.LoadFromFile(ODMain.FileName);
end;

procedure TKPictureEditForm.BUSaveClick(Sender: TObject);
begin
  if SDMain.Execute then
    IMMain.Picture.SaveToFile(SDMain.FileName);
end;

procedure TKPictureEditForm.CBStretchClick(Sender: TObject);
begin
  IMMain.Stretch := CBStretch.Checked;
end;

end.