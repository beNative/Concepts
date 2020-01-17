{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ORM.Chinook.Album;

interface

uses
  System.Classes,

  Spring,
  Spring.Persistence.Mapping.Attributes, Spring.Persistence.Core.Graphics,

  ORM.Chinook.Artist;

type
  [Entity]
  [Table('Album', '')]
  TAlbum = class(TPersistent)
  private
    FAlbumId  : Integer;
    FTitle    : string;
    FArtistId : Integer;
    FArtist   : TArtist;

  public
    procedure BeforeDestruction; override;

  published
    [Column('AlbumId', [cpRequired, cpPrimaryKey, cpNotNull], 9, 0)]
    property AlbumId: Integer
      read FAlbumId write FAlbumId;

    [Column('Title', [cpRequired, cpNotNull], 160)]
    property Title: string
      read FTitle write FTitle;

    [Column('ArtistId', [cpRequired, cpNotNull], 9, 0)]
    property ArtistId: Integer
      read FArtistId write FArtistId;

    [ManyToOne(True, [], 'ArtistId')]
    property Artist: TArtist
      read FArtist write FArtist;
  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
procedure TAlbum.BeforeDestruction;
begin
  FreeAndNil(FArtist);
  inherited BeforeDestruction;
end;
{$ENDREGION}

end.
