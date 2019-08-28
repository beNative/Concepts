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
  Vcl.ExtCtrls, Vcl.ButtonGroup, Vcl.StdCtrls, Vcl.ActnList, Vcl.ExtActns,

  Concepts.Types.Contact,

  zObjInspector, zObjInspTypes;

type
  TfrmzObjectInspector = class(TForm)
    {$REGION 'designer controls'}
    aclMain              : TActionList;
    actInternetBrowseURL : TBrowseURL;
    actTest1             : TAction;
    actTest2             : TAction;
    bgMain               : TButtonGroup;
    btnButton            : TButton;
    cbxControls          : TComboBox;
    chkCheckBox          : TCheckBox;
    edtButtonedEdit      : TButtonedEdit;
    edtEdit              : TEdit;
    lblHeader            : TLabel;
    lblLink              : TLinkLabel;
    pnlHeader            : TPanel;
    pnlLeft              : TPanel;
    pnlMain              : TPanel;
    pnlRight             : TPanel;
    sbrStatusBar         : TStatusBar;
    splSplitter          : TSplitter;
    trbTrackBar          : TTrackBar;
    {$ENDREGION}

    procedure cbxControlsChange(Sender: TObject);
    procedure lblLinkLinkClick(
      Sender     : TObject;
      const Link : string;
      LinkType   : TSysLinkType
    );

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
  System.Rtti,

  DDuce.Logger,

  Concepts.Resources, Concepts.Factories;

{$REGION 'construction and destruction'}
procedure TfrmzObjectInspector.AfterConstruction;
var
  I : Integer;
  C : TComponent;
begin
  inherited AfterConstruction;
  FContact := TConceptFactories.CreateRandomContact;
  FObjectInspector                        := TzObjectInspector.Create(Self);
  FObjectInspector.Parent                 := pnlLeft;
  FObjectInspector.Align                  := alClient;
  FObjectInspector.AlignWithMargins       := True;
  FObjectInspector.Name                   := 'FObjectInspector';
  FObjectInspector.ShowReadOnlyProperties := False;
  FObjectInspector.ShowEventProperties    := False;
  FObjectInspector.OnBeforeAddItem        := FObjectInspectorBeforeAddItem;
  aclMain.Images   := dmResources.imlMain;
  btnButton.Images := dmResources.imlMain;

  FObjectHost := TzObjectHost.Create;
  for I := 0 to ComponentCount - 1 do
  begin
    C := Components[I];
    //FObjectHost.AddObject(C, C.Name);
    cbxControls.AddItem(Format('%s: %s', [C.Name, C.ClassName]), C);
  end;
  for I := 0 to bgMain.Images.Count - 1 do
    with bgMain.Items.Add do
      ImageIndex := I;
//  FObjectHost.AddObject(FObjectInspector, 'ObjectInspector');
//  FObjectHost.AddObject(FContact, 'FContact');

  FObjectInspector.Component      := FObjectInspector;
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
  Result := True;
  if PItem.Component = FContact then
    Logger.Send('PItem', TValue.From(PItem^));
end;

procedure TfrmzObjectInspector.lblLinkLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  actInternetBrowseURL.URL := Link;
  actInternetBrowseURL.Execute;
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
    //FObjectHost := nil;
    FObjectInspector.SortByCategory := False;
  end;
end;
{$ENDREGION}

end.
