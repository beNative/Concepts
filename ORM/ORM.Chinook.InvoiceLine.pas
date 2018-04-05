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

unit ORM.Chinook.InvoiceLine;

interface

uses
  System.Classes,

  Spring.Persistence.Mapping.Attributes, Spring.Persistence.Core.Graphics,

  ORM.Chinook.Invoice, ORM.Chinook.Track;

type
  [Entity]
  [Table('InvoiceLine', '')]
  TInvoiceLine = class(TPersistent)
  private
    FInvoiceLineId : Integer;
    FInvoiceId     : Integer;
    FTrackId       : Integer;
    FUnitPrice     : Double;
    FQuantity      : Integer;
    FInvoice       : TInvoice;
    FTrack         : TTrack;

  public
    procedure BeforeDestruction; override;

  published
    [Column('InvoiceLineId', [cpRequired, cpPrimaryKey, cpNotNull], 9, 0)]
    property InvoiceLineId: Integer
      read FInvoiceLineId write FInvoiceLineId;

    [Column('TrackId', [cpRequired, cpNotNull], 9, 0)]
    property TrackId: Integer
      read FTrackId write FTrackId;

    [Column('UnitPrice', [cpRequired, cpNotNull], 2, 0)]
    property UnitPrice: Double
      read FUnitPrice write FUnitPrice;

    [Column('Quantity', [cpRequired, cpNotNull], 9, 0)]
    property Quantity: Integer
      read FQuantity write FQuantity;

    [Column('InvoiceId', [cpRequired, cpNotNull], 9, 0)]
    property InvoiceId: Integer
      read FInvoiceId write FInvoiceId;

    [ManyToOne(True, [], 'InvoiceId')]
    property Invoice: TInvoice
      read FInvoice write FInvoice;

    [ManyToOne(True, [], 'TrackId')]
    property Track: TTrack
      read FTrack write FTrack;
  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
procedure TInvoiceLine.BeforeDestruction;
begin
  FreeAndNil(FInvoice);
  FreeAndNil(FTrack);
  inherited BeforeDestruction;
end;
{$ENDREGION}

end.
