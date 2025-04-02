{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.zObjectInspector.StringsDialog.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ActnList,

  zObjInspector;

type
  TfrmStringsDialog = class(TzInspDialog)
    btnOk     : TButton;
    btnCancel : TButton;
    mmoMain   : TMemo;
    aclMain   : TActionList;
    actOK     : TAction;
    actCancel : TAction;

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
