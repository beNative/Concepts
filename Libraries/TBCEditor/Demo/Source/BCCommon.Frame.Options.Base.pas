unit BCCommon.Frame.Options.Base;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, sFrameAdapter, BCControl.Panel;

type
  TBCOptionsBaseFrame = class(TFrame)
    FrameAdapter: TsFrameAdapter;
  private
    FOwnerForm: TForm;
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
  protected
    procedure Init; virtual;
    procedure GetData; virtual;
    procedure PutData; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowFrame;
  end;

implementation

{$R *.dfm}

uses
  BCCommon.Language.Utils, Vcl.ExtCtrls, BCControl.ScrollBox;

constructor TBCOptionsBaseFrame.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  FOwnerForm := AOwner as TForm;
  { find the scroll box }
  for i := 0 to FOwnerForm.ComponentCount - 1 do
    if FOwnerForm.Components[i] is TBCScrollBox then
    begin
       Parent := FOwnerForm.Components[i] as TWinControl;
       Break;
    end;
  {$ifdef EDITBONE}
  UpdateLanguage(TForm(Self), GetSelectedLanguage);
  {$endif}
  Init;
  GetData;
end;

procedure TBCOptionsBaseFrame.WMDestroy(var Msg: TWMDestroy);
begin
  if (csDestroying in ComponentState) then
    if FOwnerForm.ModalResult = mrOk then
      PutData;
  inherited;
end;

procedure TBCOptionsBaseFrame.Init;
begin
  // not abstract because this is not always implemented
end;

procedure TBCOptionsBaseFrame.GetData;
begin
  // not abstract because this is not always implemented
end;

procedure TBCOptionsBaseFrame.PutData;
begin
  // not abstract because this is not always implemented
end;

procedure TBCOptionsBaseFrame.ShowFrame;
var
  i: Integer;
begin
  Show;
  { Autosize panels. This is stupid but TPanel can't be autosized before it's visible. }
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TBCPanel then
      (Components[i] as TBCPanel).Autosize := True;
end;

end.
