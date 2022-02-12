{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ORM.Chinook.Customer;

interface

uses
  System.Classes,

  Spring,
  Spring.Persistence.Mapping.Attributes, Spring.Persistence.Core.Graphics,

  ORM.Chinook.Employee;

type
  [Entity]
  [Table('Customer', '')]
  TCustomer = class(TPersistent)
  private
    FCustomerId   : Integer;
    FFirstName    : string;
    FLastName     : string;
    FCompany      : Nullable<string>;
    FAddress      : Nullable<string>;
    FCity         : Nullable<string>;
    FState        : Nullable<string>;
    FCountry      : Nullable<string>;
    FPostalCode   : Nullable<string>;
    FPhone        : Nullable<string>;
    FFax          : Nullable<string>;
    FEmail        : string;
    FSupportRepId : Nullable<Integer>;
    FSupportRep   : TEmployee;

  public
    [Column('CustomerId', [cpRequired, cpPrimaryKey, cpNotNull], 9, 0)]
    property CustomerId: Integer
      read FCustomerId write FCustomerId;

    [Column('FirstName', [cpRequired, cpNotNull], 40)]
    property FirstName: string
      read FFirstName write FFirstName;

    [Column('LastName', [cpRequired, cpNotNull], 20)]
    property LastName: string
      read FLastName write FLastName;

    [Column('Company', [], 80)]
    property Company: Nullable<string>
      read FCompany write FCompany;

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

    [Column('Email', [cpRequired, cpNotNull], 60)]
    property Email: string
      read FEmail write FEmail;

    [Column('SupportRepId', [], 9, 0)]
    property SupportRepId: Nullable<Integer>
      read FSupportRepId write FSupportRepId;

    [ManyToOne(True, [], 'SupportRepId')]
    property SupportRep: TEmployee
      read FSupportRep write FSupportRep;
  end;

implementation


end.
