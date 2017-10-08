unit Concepts.zObjectInspector.StringsDialog.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ActnList,

  zObjInspector;

type
  TfrmStringsDialog = class(TzInspDialog)
    btnOk       : TButton;
    btnCancel   : TButton;
    mmoMain     : TMemo;
    aclMain     : TActionList;
    actOK       : TAction;
    actCancel   : TAction;

    procedure actOKExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);

  protected
     procedure InitDialog; override;

  end;

implementation

{$R *.dfm}

{$REGION 'protected methods'}
procedure TfrmStringsDialog.InitDialog;
begin
  inherited InitDialog;
  if PropItem.Value.IsObject then
  begin
    mmoMain.Lines.Assign(TStrings(PropItem.Value.AsObject));
  end
  else
  begin
    mmoMain.Lines.Text := PropItem.Value.AsString;
  end;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmStringsDialog.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrNone;
  Close;
end;

procedure TfrmStringsDialog.actOKExecute(Sender: TObject);
begin
  if PropItem.Value.IsObject then
  begin
    TStrings(PropItem.Value.AsObject).Assign(mmoMain.Lines);
  end
  else
  begin
    PropItem.Prop.SetValue(PropItem.Instance, mmoMain.Lines.Text);
  end;
  Close;
  ModalResult := mrOk;
end;
{$ENDREGION}

end.
