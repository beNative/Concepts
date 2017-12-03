{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.zObjectInspector.Form;

{ This form demonstrates the TzObjectInspector component.

   Official sources can be found at:
           http://github.com/MahdiSafsafi/zcontrols
}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.ButtonGroup, Vcl.StdCtrls, Vcl.ActnList,

  Concepts.Types.Contact,

  zObjInspector, zObjInspTypes;

type
  TfrmzObjectInspector = class(TForm)
    {$REGION 'designer controls'}
    bgMain          : TButtonGroup;
    btnButton       : TButton;
    cbxControls     : TComboBox;
    chkCheckBox     : TCheckBox;
    edtButtonedEdit : TButtonedEdit;
    edtEdit         : TEdit;
    pnlLeft         : TPanel;
    pnlMain         : TPanel;
    pnlRight        : TPanel;
    sbrStatusBar    : TStatusBar;
    splSplitter     : TSplitter;
    trbTrackBar     : TTrackBar;
    pnlHeader       : TPanel;
    lblHeader       : TLabel;
    aclMain         : TActionList;
    actTest1        : TAction;
    actTest2        : TAction;
    {$ENDREGION}

    procedure cbxControlsChange(Sender: TObject);

  private
    FObjectInspector : TzObjectInspector;
    FObjectHost      : TzObjectHost;
    FContact         : TContact;

    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.Rtti, System.TypInfo,

  DDuce.Logger,

  Concepts.Resources, Concepts.Factories,
  Concepts.zObjectInspector.ValueManager;

{$REGION 'construction and destruction'}
procedure TfrmzObjectInspector.AfterConstruction;
var
  I : Integer;
  C : TComponent;
begin
  inherited AfterConstruction;
  FContact := TConceptFactories.CreateRandomContact;
  FObjectInspector                  := TzObjectInspector.Create(Self);
  FObjectInspector.Parent           := pnlLeft;
  FObjectInspector.Align            := alClient;
  FObjectInspector.AlignWithMargins := True;
  FObjectInspector.Name             := 'FObjectInspector';
  FObjectInspector.OnBeforeAddItem  := FObjectInspectorBeforeAddItem;
  aclMain.Images   := dmResources.imlMain;
  btnButton.Images := dmResources.imlMain;

  FObjectHost := TzObjectHost.Create;
  for I := 0 to ComponentCount - 1 do
  begin
    C := Components[I];
    FObjectHost.AddObject(C, C.Name);
    cbxControls.AddItem(Format('%s: %s', [C.Name, C.ClassName]), C);
  end;
  for I := 0 to bgMain.Images.Count - 1 do
    with bgMain.Items.Add do
      ImageIndex := I;
  FObjectHost.AddObject(FObjectInspector, 'ObjectInspector');
  FObjectHost.AddObject(FContact, 'FContact');

  FObjectInspector.Component      := FObjectHost;
  FObjectInspector.SplitterPos    := FObjectInspector.Width div 2;
  FObjectInspector.SortByCategory := False;
end;

procedure TfrmzObjectInspector.BeforeDestruction;
begin
  FContact.Free;
  cbxControls.Clear;
  if Assigned(FObjectHost) then
    FObjectHost.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmzObjectInspector.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not (PItem.Prop.PropertyType is TRttiMethodType);
end;

procedure TfrmzObjectInspector.cbxControlsChange(Sender: TObject);
var
  CBX : TComboBox;
begin
  CBX := Sender as TComboBox;
  if CBX.ItemIndex > -1 then
  begin
    // this assignment will destroy the assigned ObjectHost object!!
    FObjectInspector.Component := CBX.Items.Objects[CBX.ItemIndex];
    FObjectHost := nil;
    FObjectInspector.SortByCategory := False;
  end;
end;
{$ENDREGION}

end.
