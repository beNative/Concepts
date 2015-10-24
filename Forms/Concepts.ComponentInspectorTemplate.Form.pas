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

unit Concepts.ComponentInspectorTemplate.Form;

{$I ..\Source\DDuce.inc}

interface

uses
  System.Classes,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  Vcl.Forms,

  DDuce.Components.PropertyInspector;

type
  TfrmPropertyInspector = class(TForm)
    {$REGION 'designer controls'}
    pnlMain       : TPanel;
    pnlLeft       : TPanel;
    pnlRight      : TPanel;
    chkCheckBox   : TCheckBox;
    cbxControls   : TComboBox;
    sbrStatusBar  : TStatusBar;
    splSplitter   : TSplitter;
    lblLabel      : TLabel;
    {$ENDREGION}

    procedure cbxControlsChange(Sender: TObject);

  private
    FPropertyInspector: TPropertyInspector;

  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  Concepts.Factories;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmPropertyInspector.AfterConstruction;
var
  I: Integer;
  C: TWinControl;
begin
  inherited;
  FPropertyInspector := TConceptFactories.CreatePropertyInspector(
    Self,
    pnlLeft,
    chkCheckBox
  );
  FPropertyInspector.Name := 'PropertyInspector';
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TWinControl then
    begin
      C := TWinControl(Components[I]);
      cbxControls.AddItem(C.Name, C);
    end;
  end;
  cbxControls.ItemIndex := 0;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmPropertyInspector.cbxControlsChange(Sender: TObject);
var
  C: TWinControl;
begin
  C := cbxControls.Items.Objects[cbxControls.ItemIndex] as TWinControl;
  FPropertyInspector.Objects[0] := C;
end;
{$ENDREGION}

end.
