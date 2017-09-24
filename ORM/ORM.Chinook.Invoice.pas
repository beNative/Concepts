{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ORM.Chinook.Invoice;

interface

uses
  System.Classes,

  Spring,
  Spring.Persistence.Mapping.Attributes, Spring.Persistence.Core.Graphics;

type
  [Entity]
  [Table('Invoice', '')]
  TInvoice = class(TPersistent)
  private
    FInvoiceId         : Integer;
    FCustomerId        : Integer;
    FInvoiceDate       : TDateTime;
    FBillingAddress    : Nullable<string>;
    FBillingCity       : Nullable<string>;
    FBillingState      : Nullable<string>;
    FBillingCountry    : Nullable<string>;
    FBillingPostalCode : Nullable<string>;
    FTotal             : Double;

  published
    [Column('InvoiceId', [cpRequired, cpPrimaryKey, cpNotNull], 9, 0)]
    property InvoiceId: Integer
      read FInvoiceId write FInvoiceId;

    [Column('CustomerId', [cpRequired, cpNotNull], 9, 0)]
    property CustomerId: Integer
      read FCustomerId write FCustomerId;

    [Column('InvoiceDate', [cpRequired, cpNotNull])]
    property InvoiceDate: TDateTime
      read FInvoiceDate write FInvoiceDate;

    [Column('BillingAddress', [], 70)]
    property BillingAddress: Nullable<string>
      read FBillingAddress write FBillingAddress;

    [Column('BillingCity', [], 40)]
    property BillingCity: Nullable<string>
      read FBillingCity write FBillingCity;

    [Column('BillingState', [], 40)]
    property BillingState: Nullable<string>
      read FBillingState write FBillingState;

    [Column('BillingCountry', [], 40)]
    property BillingCountry: Nullable<string>
      read FBillingCountry write FBillingCountry;

    [Column('BillingPostalCode', [], 10)]
    property BillingPostalCode: Nullable<string>
      read FBillingPostalCode write FBillingPostalCode;

    [Column('Total', [cpRequired, cpNotNull], 2, 0)]
    property Total: Double
      read FTotal write FTotal;
  end;

implementation


end.
