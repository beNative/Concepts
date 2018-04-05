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

unit Concepts.Types.Contact;

{ Sample model object that typically represents a database entity. }

interface

uses
  System.Classes;

type
  TContact = class(TPersistent)
  private
    FLastName    : string;
    FFirstName   : string;
    FCompanyName : string;
    FEmail       : string;
    FAddress     : string;
    FNumber      : Integer;
    FBirthDate   : TDate;
    FActive      : Boolean;

  published
    property FirstName: string
      read FFirstName write FFirstName;

    property Lastname: string
      read FLastName write FLastName;

    property Email: string
      read FEmail write FEmail;

    property CompanyName: string
      read FCompanyName write FCompanyName;

    property Address: string
      read FAddress write FAddress;

    property Number: Integer
      read FNumber write FNumber;

    property BirthDate: TDate
      read FBirthDate write FBirthDate;

    property Active: Boolean
      read FActive write FActive;
  end;

implementation

end.
