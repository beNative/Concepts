{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.Vcl.GridPanel.Form;

{ Form demonstrating the use of the VCL TGridPanel component and a reorganize
  algorithm used when visibility of one or more child controls changes at
  runtime. }

interface

uses
  System.Classes,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls, Vcl.Forms;

type
  TfrmGridPanel = class(TForm)
    {$REGION 'designer controls'}
    pnlGrid         : TGridPanel;
    pnl1            : TPanel;
    pnl2            : TPanel;
    pnl3            : TPanel;
    pnl4            : TPanel;
    pnl5            : TPanel;
    pnl6            : TPanel;
    pnl7            : TPanel;
    pnl8            : TPanel;
    chk1            : TCheckBox;
    chk2            : TCheckBox;
    chk3            : TCheckBox;
    chk4            : TCheckBox;
    chk5            : TCheckBox;
    chk6            : TCheckBox;
    chk7            : TCheckBox;
    chk8            : TCheckBox;
    btnUpdateLayout : TButton;
    pnlHeader       : TPanel;
    lblHeader       : TLabel;
    {$ENDREGION}

    procedure btnUpdateLayoutClick(Sender: TObject);

  protected
    procedure UpdatePanels(AGridPanel: TGridPanel);
    procedure UpdateActions; override;

  end;

implementation

{$R *.dfm}

{$REGION 'event handlers'}
procedure TfrmGridPanel.btnUpdateLayoutClick(Sender: TObject);
begin
  UpdatePanels(pnlGrid);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmGridPanel.UpdateActions;
begin
  inherited UpdateActions;
  pnl1.Visible := chk1.Checked;
  pnl2.Visible := chk2.Checked;
  pnl3.Visible := chk3.Checked;
  pnl4.Visible := chk4.Checked;
  pnl5.Visible := chk5.Checked;
  pnl6.Visible := chk6.Checked;
  pnl7.Visible := chk7.Checked;
  pnl8.Visible := chk8.Checked;
end;

procedure TfrmGridPanel.UpdatePanels(AGridPanel: TGridPanel);
var
  Row      : Integer;
  Col      : Integer;
  Y        : Integer;
  ColCount : Integer;
  RowCount : Integer;
begin
  ColCount := AGridPanel.ColumnCollection.Count;
  RowCount := AGridPanel.RowCollection.Count;

  for Col := 0 to ColCount - 1 do
  begin
    for Row := 0 to RowCount - 1 do
    begin
      Y := Row;
      while (Y < RowCount)
        and (not AGridPanel.ControlCollection.Controls[Col, Y].Visible) do
        Inc(Y);
      if Y < RowCount then
        AGridPanel.ControlCollection.ControlItems[Col, Y].Row := Row;
    end;
  end;
end;
{$ENDREGION}

end.

