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

unit Concepts.DSharp.Bindings.Form;

{ Demonstrates how to use DSharp bindings with custom validation rules. }

interface

uses
  WinApi.Windows, WinApi.Messages,
  System.Actions, System.SysUtils, System.Variants, System.Classes, System.Rtti,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ActnList,

  Spring,

  DSharp.Bindings.VCLControls, DSharp.Bindings, DSharp.Core.Validations,

  Concepts.Types.Contact;

type
  TfrmBindings = class(TForm)
    aclMain          : TActionList;
    actValidate      : TAction;
    btnValidate      : TButton;
    edtAddress       : TLabeledEdit;
    edtCompanyName   : TLabeledEdit;
    edtEmail         : TLabeledEdit;
    edtFirstname     : TLabeledEdit;
    edtLastname      : TLabeledEdit;
    edtNumber        : TLabeledEdit;
    lblContactObject : TLabel;
    lblDoubleNumber  : TLabel;
    lblProp          : TLabel;
    sbrMain          : TStatusBar;

    procedure actValidateExecute(Sender: TObject);

  private
    FContact      : TContact;
    FBindingGroup : TBindingGroup;
    FRule         : IValidationRule;

    procedure BindingPropertyChanged(
            ASender   : TObject;
      const EventArgs : IPropertyChangedEventArgs
    );

  protected
    procedure UpdateActions; override;

    procedure AddEditBinding(
      const APropName : string;
            AEdit     : TCustomEdit
    );

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{$R *.dfm}

uses
  DDuce.RandomData,

  Concepts.Utils, Concepts.Types.ValidationRules;

{$REGION 'construction and destruction'}
procedure TfrmBindings.AfterConstruction;
begin
  inherited AfterConstruction;
  FBindingGroup := TBindingGroup.Create(Self);
  FContact      := TContact.Create;
  FRule         := TRequiredRule.Create;
  with FContact do
  begin
    Firstname   := RandomData.FirstName(gnMale);
    Lastname    := RandomData.LastName;
    CompanyName := RandomData.CompanyName;
    Email       := RandomData.Email(Firstname, Lastname);
    Address     := RandomData.Address;
    Number      := RandomData.Number(100);
  end;
  AddEditBinding('Firstname', edtFirstname);
  AddEditBinding('Lastname', edtLastname);
  AddEditBinding('Address', edtAddress);
  AddEditBinding('CompanyName', edtCompanyName);
  AddEditBinding('Email', edtEmail);
  AddEditBinding('Country', edtEmail);
  AddEditBinding('Number', edtNumber);
  with FBindingGroup.AddBinding(FContact, 'Number', lblDoubleNumber, 'Caption') do
  begin

  end;
end;

procedure TfrmBindings.BeforeDestruction;
begin
  FreeAndNil(FContact);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
{TODO -oTS -cGeneral : Check why this does not work anymore}
procedure TfrmBindings.actValidateExecute(Sender: TObject);
var
  VE: IValidationResult;
begin
  FBindingGroup.Validate;
  for VE in FBindingGroup.ValidationErrors do
  begin
    ShowMessage(VE.ErrorContent);
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmBindings.AddEditBinding(const APropName: string; AEdit: TCustomEdit);
begin
  with FBindingGroup.Bindings.Add do
  begin
    Source                := FContact;
    SourcePropertyName    := APropName;
    Target                := AEdit;
    TargetPropertyName    := 'Text';
    BindingMode           := bmTwoWay;
    NotifyOnSourceUpdated := True;
    NotifyOnTargetUpdated := True;
    OnSourceUpdated       := BindingPropertyChanged;
    ValidationRules.Add(FRule);
  end;
end;

procedure TfrmBindings.BindingPropertyChanged(ASender: TObject;
  const EventArgs : IPropertyChangedEventArgs);
begin
   // using DSharp.Core.Reflection
//  if ASender.HasProperty('Name') then
//    sbrMain.SimpleText := ASender.GetProperty('Name').GetValue(ASender).AsString;
end;

procedure TfrmBindings.UpdateActions;
begin
  inherited;
  lblProp.Caption := AsPropString(FContact);
end;
{$ENDREGION}

end.

