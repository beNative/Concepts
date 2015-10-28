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

unit Concepts.Spring.Collections.Form;

{ Demonstrates the use of Spring collections. }

{
References:
  http://delphisorcery.blogspot.be/search?q=Generics+and+variance

}

interface

uses
  System.Actions, System.Classes, System.SysUtils,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.Controls, Vcl.Forms, Vcl.ComCtrls,

  Spring, Spring.Collections,

  Concepts.Types.Contact;

type
  TfrmCollections = class(TForm)
    aclMain            : TActionList;
    actCreateList      : TAction;
    actEnumerate       : TAction;
    actFirstNameIs     : TAction;
    actLastNameIs      : TAction;
    btnCreateList      : TButton;
    btnExecuteQuery    : TButton;
    btnLastNameRoberts : TButton;
    mmoList            : TMemo;
    edtFirstName       : TEdit;
    edtLastName        : TEdit;
    actBoth            : TAction;
    btnBoth            : TButton;
    trbRecordCount     : TTrackBar;

    procedure actCreateListExecute(Sender: TObject);
    procedure actFirstNameIsExecute(Sender: TObject);
    procedure actLastNameIsExecute(Sender: TObject);
    procedure actBothExecute(Sender: TObject);

  private
    FList        : IList<TContact>;
    FFirstNameIs : TPredicate<TContact>;
    FLastNameIs  : TPredicate<TContact>;

  protected
    procedure UpdateActions; override;

  public
    procedure FillList;

    procedure HourGlass(AProc: TProc);

    procedure AfterConstruction; override;
    procedure DefinePredicates;

  end;

implementation

{$R *.dfm}

uses
  Vcl.Dialogs,

  DDuce.RandomData,

  Concepts.Factories, Concepts.Resources;

resourcestring
  SFindContactsWithFirstName = 'Find all contacts with first name = %s';
  SFindContactsWithLastName  = 'Find all contacts with last name = %s';

{$REGION 'construction and destruction'}
procedure TfrmCollections.AfterConstruction;
begin
  inherited AfterConstruction;
  FList := TCollections.CreateObjectList<TContact>;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmCollections.actCreateListExecute(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    FList.Clear;
    TConceptFactories.FillListWithContacts(FList as IObjectList, trbRecordCount.Position);
    ShowMessage('Contactlist has been created.');
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmCollections.actFirstNameIsExecute(Sender: TObject);
var
  SL : TStringList;
  C  : TContact;
begin
  DefinePredicates;
  SL := TStringList.Create;
  try
    for C in FList.Where(FFirstNameIs) do
    begin
      SL.Add(C.Firstname + ' ' + C.Lastname + ' ' + C.Address);
    end;
    mmoList.Text := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TfrmCollections.actLastNameIsExecute(Sender: TObject);
begin
  HourGlass(procedure
    var
      SL         : TStringList;
      C          : TContact;
    begin
      DefinePredicates;
      SL := TStringList.Create;
      try
        for C in FList.Where(FLastNameIs) do
        begin
          SL.Add(C.Firstname + ' ' + C.Lastname + ' ' + C.Address);
        end;
        mmoList.Text := SL.Text;
      finally
        SL.Free;
      end;
    end
  );
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmCollections.FillList;
var
  I : Integer;
  C : TContact;
begin
  FList.Clear;
  for I := 0 to trbRecordCount.Position do
  begin
    C := TContact.Create;
    with C do
    begin
      Firstname   := RandomData.FirstName(gnMale);
      Lastname    := RandomData.LastName;
      CompanyName := RandomData.CompanyName;
      Email       := RandomData.Email(Firstname, Lastname);
      Address     := RandomData.Address;
      Number      := RandomData.Number(100);
    end;
    FList.Add(C);
  end;
end;
{$ENDREGION}

procedure TfrmCollections.DefinePredicates;
begin
  FFirstNameIs := function(const AC: TContact): Boolean
  begin
    Result := AC.Firstname = edtFirstName.Text;
  end;
  FLastNameIs := function(const AC: TContact): Boolean
  begin
    Result := AC.Lastname = edtLastName.Text;
  end;
end;

procedure TfrmCollections.actBothExecute(Sender: TObject);
begin
  HourGlass(procedure
    var
      SL : TStringList;
      C  : TContact;
    begin
      DefinePredicates;
      SL := TStringList.Create;
      try
        for C in FList.Where(FLastNameIs).Where(FFirstNameIs) do
        begin
          SL.Add(C.Firstname + ' ' + C.Lastname + ' ' + C.Address);
        end;
        mmoList.Text := SL.Text;
      finally
        SL.Free;
      end;
    end
  );
end;

procedure TfrmCollections.HourGlass(AProc: TProc);
begin
  Screen.Cursor := crHourGlass;
  try
    AProc();
  finally
    Screen.Cursor := crDefault;
  end;
end;

{$REGION 'protected methods'}
procedure TfrmCollections.UpdateActions;
begin
  actFirstNameIs.Enabled := not FList.IsEmpty;
  actLastNameIs.Enabled  := not FList.IsEmpty;
  actBoth.Enabled        := not FList.IsEmpty;
  actFirstNameIs.Caption := Format(SFindContactsWithFirstName, [edtFirstName.Text]);
  actLastNameIs.Caption  := Format(SFindContactsWithLastName, [edtLastName.Text]);
  inherited UpdateActions;
end;
{$ENDREGION}

end.
