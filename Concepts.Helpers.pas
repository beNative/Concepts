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

unit Concepts.Helpers;

{ Collection of helper routines. }

interface

uses
  Classes, Controls,

  Spring.Collections;

procedure FillListWithContacts(
  AList  : IList;
  ACount : Integer
);

implementation

uses
  ImgList, Forms, Graphics,

  DDuce.RandomData,

  Concepts.Types.Contact;

procedure FillListWithContacts(AList: IList; ACount: Integer);
var
  C : TContact;
  I : Integer;
begin
  if Assigned(AList) then
  begin
    AList.Clear;
    for I := 0 to ACount - 1 do
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
        BirthDate   := RandomData.BirthDate(1920, 1988);
      end;
      AList.Add(C);
    end;
  end;
end;

end.
