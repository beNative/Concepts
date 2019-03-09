{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ORM.Chinook.Track;

interface

uses
  System.Classes,

  Spring,
  Spring.Persistence.Mapping.Attributes, Spring.Persistence.Core.Graphics,

  ORM.Chinook.Genre, ORM.Chinook.Album, ORM.Chinook.MediaType;

type
  [Entity]
  [Table('Track', '')]
  TTrack = class(TPersistent)
  private
    FTrackId      : Integer;
    FName         : string;
    FComposer     : Nullable<string>;
    FMilliseconds : Integer;
    FBytes        : Nullable<Integer>;
    //FUnitPrice    : Double;
    FAlbumId      : Nullable<Integer>;
    FMediaTypeId  : Integer;
    FGenreId      : Nullable<Integer>;
    FAlbum        : TAlbum;
    FMediaType    : TMediaType;
    FGenre        : TGenre;

  public
    procedure BeforeDestruction; override;

  published
    [Column('TrackId', [cpRequired, cpPrimaryKey, cpNotNull], 9, 0)]
    property TrackId: Integer
      read FTrackId write FTrackId;

    [Column('Name', [cpRequired, cpNotNull], 200)]
    property name: string
      read FName write FName;

    [Column('Composer', [], 220)]
    property Composer: Nullable<string>
      read FComposer write FComposer;

    [Column('Milliseconds', [cpRequired, cpNotNull], 9, 0)]
    property Milliseconds: Integer
      read FMilliseconds write FMilliseconds;

    [Column('Bytes', [], 9, 0)]
    property Bytes: Nullable<Integer>
      read FBytes write FBytes;

//    [Column('UnitPrice', [cpRequired, cpNotNull], 0, 5, 2)]
//    property UnitPrice: Double
//      read FUnitPrice write FUnitPrice;

    [Column('AlbumId', [], 9, 0)]
    property AlbumId: Nullable<Integer>
      read FAlbumId write FAlbumId;

    [Column('MediaTypeId', [cpRequired, cpNotNull], 9, 0)]
    property MediaTypeId: Integer
      read FMediaTypeId write FMediaTypeId;

    [Column('GenreId', [], 9, 0)]
    property GenreId: Nullable<Integer>
      read FGenreId write FGenreId;

    [ManyToOne(True, [], 'MediaTypeId')]
    property Genre: TGenre
      read FGenre write FGenre;

    [ManyToOne(True, [], 'AlbumId')]
    property Album: TAlbum
      read FAlbum write FAlbum;

    [ManyToOne(True, [], 'MediaTypeId')]
    property MediaType: TMediaType
      read FMediaType write FMediaType;
  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
procedure TTrack.BeforeDestruction;
begin
  FreeAndNil(FMediaType);
  FreeAndNil(FAlbum);
  FreeAndNil(FGenre);
  inherited BeforeDestruction;
end;
{$ENDREGION}

end.
