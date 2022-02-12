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

{$I Concepts.inc}

unit Concepts.Spring.Persistence.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.DBGrids,
  Data.DB,

  SQLiteTable3,

  Spring, Spring.Collections,
  Spring.Persistence.Core.Interfaces, Spring.Persistence.Core.Session,
  Spring.Data.ObjectDataSet,

  ORM.Chinook.Artist, ORM.Chinook.Album, ORM.Chinook.Track;

type
  TfrmSpringPersistence = class(TForm)
    dscMain : TDataSource;

  private
    FConnection    : IDBConnection;
    FDatabase      : TSQLiteDatabase;
    FSession       : TSession;
    FObjectDataSet : TObjectDataSet;
    FDBGrid        : TDBGrid;

    FArtists : IList<TArtist>;
    FTracks  : IList<TTrack>;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  Spring.Persistence.Adapters.SQLite,

  Concepts.Factories, Concepts.Utils;

{$REGION 'construction and destruction'}
procedure TfrmSpringPersistence.AfterConstruction;
begin
  inherited AfterConstruction;
  FDatabase := TSQLiteDatabase.Create(Self);
  FDatabase.Filename := '..\..\..\Data\Chinook_Sqlite.sqlite';
  FConnection := TSQLiteConnectionAdapter.Create(FDatabase);

  FConnection.AutoFreeConnection := True;
  FConnection.Connect;
  FSession := TSession.Create(FConnection);
  FArtists := FSession.FindAll<TArtist>;
  FTracks  := FSession.FindAll<TTrack>;

  FObjectDataSet          := TObjectDataset.Create(Self);
  FObjectDataSet.DataList := FTracks as IObjectList;
  FObjectDataSet.Active := True;
  AutoSizeDisplayWidths(FObjectDataSet);

  dscMain.DataSet := FObjectDataSet;
  FDBGrid := TConceptFactories.CreateDBGrid(Self, Self, dscMain);

  ShowMessage(FTracks.First.Album.Artist.Name.Value);
end;

procedure TfrmSpringPersistence.BeforeDestruction;
begin
  FSession.Free;
  FObjectDataSet.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

end.
