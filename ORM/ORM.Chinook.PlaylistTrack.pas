{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ORM.Chinook.PlaylistTrack;

interface

uses
  System.Classes,

  Spring,
  Spring.Persistence.Mapping.Attributes, Spring.Persistence.Core.Graphics,

  ORM.Chinook.Track, ORM.Chinook.Playlist;

type
  [Entity]
  [Table('PlaylistTrack', '')]
  TPlaylistTrack = class(TPersistent)
  private
    FPlaylistId : Integer;
    FTrackId    : Integer;
    FPlayList   : TPlaylist;
    FTrack      : TTrack;

  public
    procedure BeforeDestruction; override;

  published
    [Column('PlaylistId', [cpRequired, cpPrimaryKey, cpNotNull], 9, 0)]
    property PlaylistId: Integer
      read FPlaylistId write FPlaylistId;

    [Column('TrackId', [cpRequired, cpPrimaryKey, cpNotNull], 9, 0)]
    property TrackId: Integer
      read FTrackId write FTrackId;

    [ManyToOne(True, [], 'PlayListId')]
    property Playlist: TPlaylist
      read FPlaylist write FPlaylist;

    [ManyToOne(True, [], 'TrackId')]
    property Track: TTrack
      read FTrack write FTrack;

  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
procedure TPlaylistTrack.BeforeDestruction;
begin
  FreeAndNil(FPlayList);
  FreeAndNil(FTrack);
  inherited BeforeDestruction;
end;
{$ENDREGION}

end.
