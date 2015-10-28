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

{$I Concepts.inc}

unit Concepts.System.LiveBindings.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Rtti,
  System.Bindings.Outputs,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Bind.DBEngExt, Vcl.Bind.Editors, Vcl.ExtCtrls,
  Data.Bind.EngExt, Data.Bind.Components,

  DDuce.Components.PropertyInspector,

  Concepts.Types.Contact;

type
  TfrmLiveBindings = class(TForm)
    pnlLeft            : TPanel;
    pnlRight           : TPanel;
    splVertical        : TSplitter;
    cbxControls        : TComboBox;
    lstBindings        : TBindingsList;
    edtButtonCaption   : TEdit;
    btnButton          : TButton;
    lnkCaption         : TLinkControlToProperty;
    trbTrackBar        : TTrackBar;
    lblLabel           : TLabel;
    bxpLabel           : TBindExpression;
    pbProgressBar      : TProgressBar;
    bxpProgressBar     : TBindExpression;
    bxiExpressionItems : TBindExprItems;
    trbMulti           : TTrackBar;
    pbMulti1           : TProgressBar;
    pbMulti2           : TProgressBar;
    sbrMain            : TStatusBar;
    bxpStatusbar       : TBindExpression;
    edt1: TEdit;
    edt2: TEdit;
    btn1: TButton;
    btn2: TButton;

    procedure cbxControlsChange(Sender: TObject);
    procedure trbTrackBarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);

  private
    FPropertyInspector : TPropertyInspector;
    FBindScope         : TBindScope;
    FContact           : TContact;

    procedure AddComponents;

  public
    procedure AfterConstruction; override;


    procedure CreateObjectBindings;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  Concepts.Factories, Concepts.Utils;

{$REGION 'construction and destruction'}
procedure TfrmLiveBindings.AfterConstruction;
begin
  inherited AfterConstruction;
  AddComponents;
  FPropertyInspector :=
    TConceptFactories.CreatePropertyInspector(Self, pnlLeft, Self);
  FPropertyInspector.UpdateItems;
  FBindScope := TBindScope.Create(Self);
  FContact   := TConceptFactories.CreateRandomContact;
  FBindScope.DataObject := FContact;
  FBindScope.Active := True;
  CreateObjectBindings;
end;

procedure TfrmLiveBindings.BeforeDestruction;
begin
  FreeAndNil(FContact);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'private methods'}
{ Add all components of the form. }

procedure TfrmLiveBindings.AddComponents;
var
  I : Integer;
  C : TComponent;
begin
  cbxControls.Items.Clear;
  for I := 0 to ComponentCount - 1 do
  begin
    C := Components[I];
    cbxControls.AddItem(C.Name, C);
  end;
  cbxControls.ItemIndex := 0;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmLiveBindings.cbxControlsChange(Sender: TObject);
var
  C: TComponent;
begin
  C := cbxControls.Items.Objects[cbxControls.ItemIndex] as TComponent;
  FPropertyInspector.Objects[0] := C;
end;

procedure TfrmLiveBindings.FormResize(Sender: TObject);
begin
  lstBindings.Notify(pnlRight, '');
end;

procedure TfrmLiveBindings.trbTrackBarChange(Sender: TObject);
begin
  lstBindings.Notify(Sender, '');
end;
{$ENDREGION}

procedure TfrmLiveBindings.CreateObjectBindings;
var
  BE : TBindExpression;
begin
  BE := TBindExpression.Create(Self);
  BE.ControlComponent  := edt1;
  BE.ControlExpression := 'Text';
  BE.SourceComponent := FBindScope;
  BE.SourceExpression := 'FirstName';
  BE.Direction       := TExpressionDirection.dirBidirectional;
  BE.BindingsList := lstBindings;
end;

procedure TfrmLiveBindings.btn1Click(Sender: TObject);
var
 CBC: TContainedBindComponent;
begin
  FreeAndNil(FContact);
  FContact := TConceptFactories.CreateRandomContact;
  FBindScope.DataObject := FContact;
  //lstBindings.Notify(edt1, '');
//  for CBC in lstBindings do
//    CBC.



end;

procedure TfrmLiveBindings.btn2Click(Sender: TObject);
begin
  ShowMessage(AsPropString(FContact));
end;

end.
