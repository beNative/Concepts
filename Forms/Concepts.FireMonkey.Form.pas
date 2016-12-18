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

{$I Concepts.inc}

unit Concepts.FireMonkey.Form;

{ Example form with some common FireMonkey controls. }

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Effects,
  FMX.Colors, FMX.TabControl, FMX.Menus, FMX.Filter.Effects, FMX.Edit,
  FMX.ExtCtrls, FMX.EditBox, FMX.SpinBox, FMX.ListBox, FMX.Layouts,
  FMX.Controls.Presentation;

type
  TFireMonkeyForm = class(TForm)
    chkExample  : TCheckBox;
    cmbExample  : TComboBox;
    grpExample  : TGroupBox;
    lblExample  : TLabel;
    lstExample  : TListBox;
    pbrExample  : TProgressBar;
    sbrExample  : TStatusBar;
    spbExample  : TSpinBox;
    swExample   : TSwitch;
    tbcExample  : TTabControl;
    tbiExample1 : TTabItem;
    tbiExample2 : TTabItem;
    tbrExample  : TToolBar;
    trbExample  : TTrackBar;
    edtExample  : TEdit;
    btnExample  : TButton;
  end;

implementation

{$R *.fmx}

end.
