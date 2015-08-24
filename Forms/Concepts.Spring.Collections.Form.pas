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

unit Concepts.Spring.Collections.Form;

{ Demonstrates the use of Spring collections. }

interface

uses
  System.Actions, System.Classes,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.Controls, Vcl.Forms,

  Spring.Collections,

  Concepts.Types.Contact;

type
  TfrmCollections = class(TForm)
    aclMain            : TActionList;
    actCreateList      : TAction;
    actEnumerate       : TAction;
    actFirstNameJohn   : TAction;
    actLastNameRoberts : TAction;
    btnCreateList      : TButton;
    btnExecuteQuery    : TButton;
    btnLastNameRoberts : TButton;
    mmoList            : TMemo;

    procedure actCreateListExecute(Sender: TObject);
    procedure actFirstNameJohnExecute(Sender: TObject);
    procedure actLastNameRobertsExecute(Sender: TObject);

  private
    FList : IList<TContact>;

  public
    procedure FillList;
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  Vcl.Dialogs,

  DDuce.RandomData;

{$REGION 'construction and destruction'}
procedure TfrmCollections.AfterConstruction;
begin
  inherited;
  FList := TCollections.CreateObjectList<TContact>;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmCollections.actCreateListExecute(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    FList.Clear;
    FillList;
    ShowMessage('Contactlist has been created.');
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmCollections.actFirstNameJohnExecute(Sender: TObject);
var
  SL              : TStringList;
  C               : TContact;
  FirstNameIsJohn : Spring.TPredicate<TContact>;
begin
  SL := TStringList.Create;
  try
    FirstNameIsJohn := function(const AC: TContact): Boolean
    begin
      Result := AC.Firstname = 'John';
    end;

    for C in FList.Where(FirstNameIsJohn) do
    begin
      SL.Add(C.Firstname + ' ' + C.Lastname + ' ' + C.Address);
    end;
    mmoList.Text := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TfrmCollections.actLastNameRobertsExecute(Sender: TObject);
var
  SL                : TStringList;
  C                 : TContact;
  LastNameIsRoberts : Spring.TPredicate<TContact>;
begin
  SL := TStringList.Create;
  try
    LastNameIsRoberts := function(const AC: TContact): Boolean
    begin
      Result := AC.Lastname = 'Roberts';
    end;

    for C in FList.Where(LastNameIsRoberts) do
    begin
      SL.Add(C.Firstname + ' ' + C.Lastname + ' ' + C.Address);
    end;
    mmoList.Text := SL.Text;
  finally
    SL.Free;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmCollections.FillList;
var
  I : Integer;
  C : TContact;
begin
  FList.Clear;
  for I := 0 to 10000 do
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

end.
