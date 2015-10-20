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

unit Concepts.Types.Contact;

{ Sample model object that typically represents a database entity object that
  can be persisted. }

interface

uses
  Spring,
  DSharp.Core.PropertyChangedBase, // TPropertyChangedBase
  DSharp.Core.Properties         // IDataErrorInfo
  ;

type
  TContact = class(TPropertyChangedBase, INotifyPropertyChanged)
  private
    FLastname    : string;
    FFirstname   : string;
    FCompanyName : string;
    FEmail       : string;
    FAddress     : string;
    FNumber      : Integer;
    FBirthDate   : TDate;

    // property access methods
    procedure SetFirstname(const Value: string);
    procedure SetLastname(const Value: string);
    procedure SetCompanyName(const Value: string);
    procedure SetEmail(const Value: string);
    procedure SetAddress(const Value: string);
    procedure SetNumber(const Value: Integer);
    procedure SetBirthDate(const Value: TDate);

  published


    property Firstname: string
      read FFirstname write SetFirstname;

    property Lastname: string
      read FLastname write SetLastname;

    property Email: string
      read FEmail write SetEmail;

    property CompanyName: string
      read FCompanyName write SetCompanyName;

    property Address: string
      read FAddress write SetAddress;

    property Number: Integer
      read FNumber write SetNumber;

    property BirthDate: TDate
      read FBirthDate write SetBirthDate;
  end;

implementation

uses
  System.Classes,
  Vcl.Dialogs;

procedure TContact.SetAddress(const Value: string);
begin
  FAddress := Value;
  NotifyOfPropertyChange('Address');
end;

procedure TContact.SetBirthDate(const Value: TDate);
begin
  FBirthDate := Value;
  NotifyOfPropertyChange('BirthDate');
end;

procedure TContact.SetCompanyName(const Value: string);
begin
  FCompanyName := Value;
  NotifyOfPropertyChange('CompanyName');
end;

procedure TContact.SetEmail(const Value: string);
begin
  FEmail := Value;
  NotifyOfPropertyChange('Email');
end;

procedure TContact.SetFirstname(const Value: string);
begin
  FFirstname := Value;
  NotifyOfPropertyChange('Firstname');
end;

procedure TContact.SetLastname(const Value: string);
begin
  FLastname := Value;
  NotifyOfPropertyChange('Lastname');
end;

procedure TContact.SetNumber(const Value: Integer);
begin
  FNumber := Value;
  NotifyOfPropertyChange('Number');
end;

initialization
  RegisterClass(TContact);

end.
