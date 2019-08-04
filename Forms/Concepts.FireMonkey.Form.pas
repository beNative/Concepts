{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.FireMonkey.Form;

{ Example form with some common FireMonkey controls. }

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Effects,
  FMX.Colors, FMX.TabControl, FMX.Menus, FMX.Filter.Effects, FMX.Edit,
  FMX.ExtCtrls, FMX.SpinBox, FMX.ListBox, FMX.Layouts, FMX.EditBox,
  FMX.Controls.Presentation, FMX.DateTimeCtrls, FMX.Calendar, FMX.MagnifierGlass;

type
  TFireMonkeyForm = class(TForm)
    {$REGION 'designer controls'}
    btnCornerExample : TCornerButton;
    btnExample       : TButton;
    calExample       : TCalendar;
    cbxColor         : TColorComboBox;
    cbxColorBox      : TColorBox;
    chkExample       : TCheckBox;
    cmbExample       : TComboBox;
    edtDate          : TDateEdit;
    edtExample       : TEdit;
    grpExample       : TGroupBox;
    lblExample       : TLabel;
    lbxColors        : TColorListBox;
    lstExample       : TListBox;
    MenuItem1        : TMenuItem;
    MenuItem10       : TMenuItem;
    MenuItem11       : TMenuItem;
    MenuItem12       : TMenuItem;
    MenuItem2        : TMenuItem;
    MenuItem3        : TMenuItem;
    MenuItem4        : TMenuItem;
    MenuItem5        : TMenuItem;
    MenuItem6        : TMenuItem;
    MenuItem7        : TMenuItem;
    MenuItem8        : TMenuItem;
    MenuItem9        : TMenuItem;
    mnuMenuBar       : TMenuBar;
    pbrExample       : TProgressBar;
    rbtRadioButton   : TRadioButton;
    spbExample       : TSpinBox;
    swExample        : TSwitch;
    tbcExample       : TTabControl;
    tbiExample1      : TTabItem;
    tbiExample2      : TTabItem;
    trbExample       : TTrackBar;
    {$ENDREGION}

    procedure trbExampleChange(Sender: TObject);
    procedure actButtonPressedExecute(Sender: TObject);

  end;

implementation

{$R *.fmx}

{$REGION 'event handlers'}
procedure TFireMonkeyForm.actButtonPressedExecute(Sender: TObject);
begin
  ShowMessage('Button pressed!');
end;

procedure TFireMonkeyForm.trbExampleChange(Sender: TObject);
begin
  pbrExample.Value := trbExample.Value;
end;
{$ENDREGION}

end.
