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

unit Concepts.System.LiveBindings.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Rtti, System.Actions,
  System.Bindings.Outputs, System.Bindings.Helper,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Bind.DBEngExt, Vcl.Bind.Editors, Vcl.ExtCtrls, Vcl.ActnList,
  Vcl.Bind.Grid, Data.Bind.Grid, Vcl.Grids, Vcl.Buttons, Vcl.Bind.Navigator,
  Data.Bind.EngExt, Data.Bind.Components, Data.Bind.GenData,
  Data.Bind.ObjectScope, Data.Bind.Controls, Data.Bind.DBScope,

  zObjInspector,

  Concepts.Types.Contact;

type
  TfrmLiveBindings = class(TForm)
    {$REGION 'designer controls'}
    absMain                   : TAdapterBindSource;
    aclMain                   : TActionList;
    actAlterContactCompany    : TAction;
    actGenerateContact        : TAction;
    bndnvgtrNavigatorabsMain1 : TBindNavigator;
    btnAlterContactCompany    : TButton;
    btnButton                 : TButton;
    btnGenerateContact        : TButton;
    bxiExpressionItems        : TBindExprItems;
    bxpLabel                  : TBindExpression;
    bxpProgressBar            : TBindExpression;
    bxpStatusbar              : TBindExpression;
    cbxControls               : TComboBox;
    dgaMain                   : TDataGeneratorAdapter;
    edtButtonCaption          : TEdit;
    edtCompanyName            : TEdit;
    edtFirstName              : TEdit;
    edtLastName               : TEdit;
    grdMain                   : TStringGrid;
    hntMain                   : TBalloonHint;
    lblButtonCaption          : TLabel;
    lblCompanyName            : TLabel;
    lblFirstName              : TLabel;
    lblHeader                 : TLabel;
    lblLabel                  : TLabel;
    lblLastName               : TLabel;
    lnkCaption                : TLinkControlToProperty;
    lnkgrdtdtsrcabsMain       : TLinkGridToDataSource;
    lstBindings               : TBindingsList;
    pbProgressBar             : TProgressBar;
    pnlHeader                 : TPanel;
    pnlLeft                   : TPanel;
    pnlRight                  : TPanel;
    sbrMain                   : TStatusBar;
    splVertical               : TSplitter;
    trbTrackBar               : TTrackBar;
    {$ENDREGION}

    procedure cbxControlsChange(Sender: TObject);
    procedure trbTrackBarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);

    procedure actGenerateContactExecute(Sender: TObject);
    procedure actAlterContactCompanyExecute(Sender: TObject);

  private
    FObjectInspector   : TzObjectInspector;
    FBindScope         : TBindScope;
    FContact           : TContact;

    procedure AddComponents;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure CreateObjectBindings;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Components.Factories, DDuce.Factories.zObjInspector,

  Concepts.Factories;

{$REGION 'construction and destruction'}
procedure TfrmLiveBindings.AfterConstruction;
begin
  inherited AfterConstruction;
  AddComponents;
  FObjectInspector := TzObjectInspectorFactory.Create(Self, pnlLeft);
  FObjectInspector.Component := lstBindings;
  FObjectInspector.ExpandAll;
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
procedure TfrmLiveBindings.CreateObjectBindings;
begin
  FBindScope := TBindingsFactory.CreateBindScope(FContact, Self);
  TBindingsFactory.CreateEditBinding(
    FBindScope,
    'CompanyName',
    edtCompanyName
  );
  TBindingsFactory.CreateEditBinding(
    FBindScope,
    'FirstName',
    edtFirstName
  );
  TBindingsFactory.CreateEditBinding(
    FBindScope,
    'LastName',
    edtLastName
  );
end;

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
  FObjectInspector.Component := C;
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

{$REGION 'action handlers'}
procedure TfrmLiveBindings.actAlterContactCompanyExecute(Sender: TObject);
begin
  FContact.CompanyName := '';
  TBindings.Notify(FContact);
end;

procedure TfrmLiveBindings.actGenerateContactExecute(Sender: TObject);
begin
  FreeAndNil(FContact);
  FContact := TConceptFactories.CreateRandomContact;
  FBindScope.DataObject := FContact;
end;
{$ENDREGION}

end.
