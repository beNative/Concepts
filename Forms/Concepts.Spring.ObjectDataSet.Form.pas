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

unit Concepts.Spring.ObjectDataSet.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  Spring.Persistence.ObjectDataSet,

  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.Session,
  Spring.Collections,
  SQLiteTable3;

type
  TfrmSpringObjectDataSet = class(TForm)
  private
    FConnection : IDBConnection;
    FDatabase   : TSQLiteDatabase;
    //FProducts   : IList<TProduct>;
    FObjectDataSet : TObjectDataset;
    FSession    : TSession;
  public
    procedure CreateDBObjects;
  end;

implementation

{$R *.dfm}

uses
  Spring.Persistence.Core.DatabaseManager,
  Spring.Persistence.Core.ConnectionFactory,

  Spring.Persistence.Adapters.SQLite;

procedure TfrmSpringObjectDataSet.CreateDBObjects;
begin
  FDatabase := TSQLiteDatabase.Create(Self);
  FDatabase.Filename := 'chinook.db3';
  FConnection := TSQLiteConnectionAdapter.Create(FDatabase);

  FConnection.AutoFreeConnection := True;
  FConnection.Connect;
  FSession := TSession.Create(FConnection);
  FObjectDataSet := TObjectDataSet.Create(Self);

  //FObjectDataSet.DataList :=

  //FDatabase.GetUniTableIntf()


  //FObjectDataSet.SetDataList<TContact>(FList as IList<TContact>);

  //FProducts := TCollections.CreateObjectList<TProduct>(True);
end;

end.
