{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ORM.Chinook.Employee;

interface

uses
  System.Classes,

  Spring,
  Spring.Persistence.Mapping.Attributes, Spring.Persistence.Core.Graphics;

type
  [Entity]
  [Table('Employee', '')]
  TEmployee = class(TPersistent)
  private
    FEmployeeId : Integer;
    FLastName   : string;
    FFirstName  : string;
    FTitle      : Nullable<string>;
    FReportsTo  : Nullable<Integer>;
    FBirthDate  : Nullable<TDateTime>;
    FHireDate   : Nullable<TDateTime>;
    FAddress    : Nullable<string>;
    FCity       : Nullable<string>;
    FState      : Nullable<string>;
    FCountry    : Nullable<string>;
    FPostalCode : Nullable<string>;
    FPhone      : Nullable<string>;
    FFax        : Nullable<string>;
    FEmail      : Nullable<string>;

  published
    [Column('EmployeeId', [cpRequired, cpPrimaryKey, cpNotNull], 9, 0)]
    property EmployeeId: Integer
      read FEmployeeId write FEmployeeId;

    [Column('LastName', [cpRequired, cpNotNull], 20)]
    property LastName: string
      read FLastName write FLastName;

    [Column('FirstName', [cpRequired, cpNotNull], 20)]
    property FirstName: string
      read FFirstName write FFirstName;

    [Column('Title', [], 30)]
    property Title: Nullable<string>
      read FTitle write FTitle;

    [Column('ReportsTo', [], 9, 0)]
    property ReportsTo: Nullable<Integer>
      read FReportsTo write FReportsTo;

    [Column('BirthDate', [])]
    property BirthDate: Nullable<TDateTime>
      read FBirthDate write FBirthDate;

    [Column('HireDate', [])]
    property HireDate: Nullable<TDateTime>
      read FHireDate write FHireDate;

    [Column('Address', [], 70)]
    property Address: Nullable<string>
      read FAddress write FAddress;

    [Column('City', [], 40)]
    property City: Nullable<string>
      read FCity write FCity;

    [Column('State', [], 40)]
    property State: Nullable<string>
      read FState write FState;

    [Column('Country', [], 40)]
    property Country: Nullable<string>
      read FCountry write FCountry;

    [Column('PostalCode', [], 10)]
    property PostalCode: Nullable<string>
      read FPostalCode write FPostalCode;

    [Column('Phone', [], 24)]
    property Phone: Nullable<string>
      read FPhone write FPhone;

    [Column('Fax', [], 24)]
    property Fax: Nullable<string>
      read FFax write FFax;

    [Column('Email', [], 60)]
    property Email: Nullable<string>
      read FEmail write FEmail;
  end;

implementation


end.
