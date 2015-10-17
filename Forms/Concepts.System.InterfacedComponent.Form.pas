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

unit Concepts.System.InterfacedComponent.Form;

{ Not working! }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ActnList,
  System.Actions,
  System.InterfacedComponent;

type
  TfrmInterfacedComponent = class(TForm)
    aclMain                   : TActionList;
    actCreateNormal           : TAction;
    actCreateReferenceCounted : TAction;
    btnCreateNormal           : TButton;
    btnCreateReferenceCounted : TButton;
    actCreateBoth             : TAction;
    btnCreateBoth             : TButton;

    procedure actCreateNormalExecute(Sender: TObject);
    procedure actCreateReferenceCountedExecute(Sender: TObject);
    procedure actCreateBothExecute(Sender: TObject);

  private
    FEdit                      : IInterface;

  public
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

procedure TfrmInterfacedComponent.actCreateBothExecute(Sender: TObject);
begin
//  if Assigned(FReferenceCountedComponent) then
//    FreeAndNil(FReferenceCountedComponent);

  FEdit := TEdit.Create(nil);
//  (FEdit. as IGet GetComponent as TEdit).Parent := Self;
//  (FEdit.GetComponent as TEdit).Top := 50;
end;

procedure TfrmInterfacedComponent.actCreateNormalExecute(Sender: TObject);
begin
  FEdit := TEdit.Create(nil);
  (FEdit as IInterfaceComponentReference) .GetComponent.EnableRefCount;
//  (FEdit.GetComponent as TEdit).Parent := Self;
//  (FEdit.GetComponent as TEdit).Top := 80;
end;

procedure TfrmInterfacedComponent.actCreateReferenceCountedExecute(
  Sender: TObject);
begin
//  FReference := nil;
//
//
//  if Assigned(FForm) then
//  begin
//    FreeAndNil(FForm);
//  end;

  //FReference := TComponent.Create(Self);
  //FReferenceCountedComponent.EnableRefCount;
//  FForm := TfrmTest.Create(Self);
//  FForm.Show;
//  FForm.EnableRefCount;
//  FReference := FForm;

end;

procedure TfrmInterfacedComponent.BeforeDestruction;
begin
  FEdit := nil;
  inherited;
end;

//


end.
