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

{$I Concepts.inc}

unit Concepts.System.Variants.Form;

{ Form demonstrating some operations and conversions on Delphi Variants }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ActnList, Vcl.ExtCtrls;

type
  TfrmVariants = class(TForm)
    {$REGION 'designer controls'}
    aclMain                    : TActionList;
    actAssignEmptyParam        : TAction;
    actAssignNull              : TAction;
    actAssignUnassigned        : TAction;
    actShowAsString            : TAction;
    actToggleNullStrictConvert : TAction;
    btnEmptyParam              : TButton;
    btnNull                    : TButton;
    btnShowAsString            : TButton;
    btnUnassigned              : TButton;
    chkNullStrictConvert       : TCheckBox;
    pnlHeader                  : TPanel;
    lblHeader                  : TLabel;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actToggleNullStrictConvertExecute(Sender: TObject);
    procedure actAssignUnassignedExecute(Sender: TObject);
    procedure actAssignNullExecute(Sender: TObject);
    procedure actAssignEmptyParamExecute(Sender: TObject);
    procedure actShowAsStringExecute(Sender: TObject);
    {$ENDREGION}

  private
    FVariant : Variant;

  end;

implementation

{$R *.dfm}

{$REGION 'action handlers'}
procedure TfrmVariants.actAssignEmptyParamExecute(Sender: TObject);
begin
  FVariant := EmptyParam;
end;

procedure TfrmVariants.actAssignNullExecute(Sender: TObject);
begin
  FVariant := Null;
end;

procedure TfrmVariants.actAssignUnassignedExecute(Sender: TObject);
begin
  FVariant := Unassigned;
end;

procedure TfrmVariants.actShowAsStringExecute(Sender: TObject);
begin
  ShowMessage(FVariant);
end;

procedure TfrmVariants.actToggleNullStrictConvertExecute(Sender: TObject);
begin
  actToggleNullStrictConvert.Checked := not actToggleNullStrictConvert.Checked;
  NullStrictConvert := actToggleNullStrictConvert.Checked;
end;
{$ENDREGION}

end.

