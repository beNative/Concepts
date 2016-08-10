{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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
          http://github.com/mahdisafsafi/delphi-detours-library

  Instructions on how to use:
  http://github.com/MahdiSafsafi/delphi-detours-library/blob/wiki/DDetours.md
}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls,

  DDetours;

type
  TModifiedForm = class(TForm)
  type
    TDTProc = procedure;

  private class var
    FDetour1 : IDetours<TDTProc>;
    FDetour2 : IDetours<TDTProc>;

  private
    procedure AddPanel(
      const ACaption : string;
      const AColor   : TColor
    );
  public
    class constructor Create;

    class property Detour1 : IDetours<TDTProc>
      read FDetour1;

    class property Detour2 : IDetours<TDTProc>
      read FDetour2;

    procedure DetourMethod1;
    procedure DetourMethod2;
  end;

type
  TfrmDDetours = class(TForm)
    aclMain               : TActionList;
    actCallDetouredMethod : TAction;
    btnUnitName           : TButton;
    chkDetour1Enabled     : TCheckBox;
    chkDetour2Enabled     : TCheckBox;

    procedure actCallDetouredMethodExecute(Sender: TObject);

    procedure chkDetour1EnabledClick(Sender: TObject);
    procedure chkDetour2EnabledClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

{$REGION 'TModifiedForm'}
class constructor TModifiedForm.Create;
begin
  FDetour1 := TDetours<TDTProc>.Create(
    @TForm.Show,
    @TModifiedForm.DetourMethod1
  );
  FDetour2 := TDetours<TDTProc>.Create(
    @TForm.Show,
    @TModifiedForm.DetourMethod2
  );
end;

procedure TModifiedForm.AddPanel(const ACaption: string; const AColor: TColor);
var
  P : TPanel;
begin
  P := TPanel.Create(Self);
  P.Parent           := Self;
  P.Align            := alTop;
  P.Height           := 15;
  P.ParentBackground := False;
  P.Color            := AColor;
  P.Font.Color       := clWhite;
  P.Font.Style       := [fsBold];
  P.BevelOuter       := bvNone;
  P.Caption          := ACaption;
end;

procedure TModifiedForm.DetourMethod1;
begin
  FDetour1.Disable;
  try
    Show;
  finally
    FDetour1.Enable;
  end;
  AddPanel('Hacked by DetourMethod1', clBlack);
end;

procedure TModifiedForm.DetourMethod2;
begin
  FDetour2.Disable;
  try
    Show;
  finally
    FDetour2.Enable;
  end;
  AddPanel('Hacked by DetourMethod2', clBlue);
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmDDetours.AfterConstruction;
begin
  inherited AfterConstruction;
  TModifiedForm.ClassName;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmDDetours.actCallDetouredMethodExecute(Sender: TObject);
var
  F : TForm;
begin
  F := TForm.Create(Self);
  F.Show;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmDDetours.chkDetour1EnabledClick(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
    TModifiedForm.Detour1.Enable
  else
    TModifiedForm.Detour1.Disable
end;

procedure TfrmDDetours.chkDetour2EnabledClick(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
    TModifiedForm.Detour2.Enable
  else
    TModifiedForm.Detour2.Disable
end;

procedure TfrmDDetours.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TModifiedForm.Detour1.Disable;
  TModifiedForm.Detour2.Disable;
  Action := caFree;
end;
{$ENDREGION}

end.
