{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.DDetours.Form;

{ Shows how to use DDetours library.
  https://github.com/mahdisafsafi/delphi-detours-library

  instructions on how to use:
  https://github.com/MahdiSafsafi/delphi-detours-library/blob/wiki/DDetours.md
}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdCtrls,

  DDetours;

type
  TfrmDDetours = class(TForm)
    aclMain   : TActionList;
    actHook   : TAction;
    actUnhook : TAction;
    btnHook   : TButton;
    btnUnhook : TButton;
    lblHook: TLabel;

    procedure actHookExecute(Sender: TObject);
    procedure actUnhookExecute(Sender: TObject);
  private

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

var
  GDoCreate: procedure = nil;
  S : string = '';

procedure DoCreate_Hooked;
begin
  GDoCreate;
  S := S + ' Hooked';

end;

{$REGION 'construction and destruction'}
procedure TfrmDDetours.AfterConstruction;
begin
  inherited AfterConstruction;


end;

{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmDDetours.actHookExecute(Sender: TObject);
begin
  @GDoCreate := InterceptCreate(@TForm.DoCreate, @DoCreate_Hooked);
end;

procedure TfrmDDetours.actUnhookExecute(Sender: TObject);
begin
  if Assigned(GDoCreate) then
  begin
    InterceptRemove(@GDoCreate);
    GDoCreate := nil;
  end;

end;

procedure TfrmDDetours.UpdateActions;
begin
  inherited;
  lblHook.Caption := S;
end;

{$ENDREGION}

end.
