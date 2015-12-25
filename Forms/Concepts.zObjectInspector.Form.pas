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

unit Concepts.zObjectInspector.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.ButtonGroup, Vcl.StdCtrls,

  zObjInspector;

type
  TfrmzObjectInspector = class(TForm)
    pnlMain         : TPanel;
    splSplitter     : TSplitter;
    pnlLeft         : TPanel;
    cbxControls     : TComboBox;
    pnlRight        : TPanel;
    lblLabel        : TLabel;
    btnButton       : TButton;
    chkCheckBox     : TCheckBox;
    edtEdit         : TEdit;
    bgMain          : TButtonGroup;
    trbTrackBar     : TTrackBar;
    edtButtonedEdit : TButtonedEdit;
    sbrStatusBar    : TStatusBar;

  private
    FObjectInspector : TzObjectInspector;
    FObjectHost      : TzObjectHost;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmzObjectInspector.AfterConstruction;
var
  I : Integer;
  C : TComponent;
begin
  inherited AfterConstruction;
  FObjectInspector                  := TzObjectInspector.Create(Self);
  FObjectInspector.Parent           := pnlLeft;
  FObjectInspector.Align            := alClient;
  FObjectInspector.AlignWithMargins := True;
  FObjectInspector.Name             := 'FObjectInspector';
  FObjectHost := TzObjectHost.Create;
  for I := 0 to ComponentCount - 1 do
  begin
    C := Components[I];
    FObjectHost.AddObject(C, C.Name);
  end;
  FObjectInspector.Component := FObjectHost;
  FObjectInspector.SplitterPos := FObjectInspector.Width div 2;
end;

procedure TfrmzObjectInspector.BeforeDestruction;
begin
  FObjectHost.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

end.
