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

unit FMXForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Effects,
  FMX.Colors, FMX.TabControl, FMX.Menus, FMX.Filter.Effects, FMX.Edit,
  FMX.Controls.Presentation, FMX.ExtCtrls, FMX.EditBox, FMX.SpinBox;

type
  TFireMonkeyForm = class(TForm)
    Button1: TButton;
    GroupBox1: TGroupBox;
    Button2: TButton;
    Edit1: TEdit;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    AlphaTrackBar1: TAlphaTrackBar;
    TabItem2: TTabItem;
    Switch1: TSwitch;
    trb1: TTrackBar;
    spnbx1: TSpinBox;
    pltgrd1: TPlotGrid;
    procedure ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFireMonkeyForm.ButtonClick(Sender: TObject);
begin
  MessageDlg('Hello from ' + (Sender as TComponent).Name, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

end.
