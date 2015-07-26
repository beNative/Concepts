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

unit Concepts.DevExpress.cxDragAndDrop.Form;

interface

uses
  System.Actions, System.Classes,
  Vcl.Controls, Vcl.ActnList, Vcl.Forms,
  Data.DB, Datasnap.DBClient,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxDBData,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView,
  cxClasses, cxGridLevel, cxGrid, cxNavigator;

type
  TfrmcxDragAndDrop = class(TForm)
    grdMain    : TcxGrid;
    aclMain    : TActionList;
    dscMain    : TDataSource;
    dsContacts : TClientDataSet;
    lvlMaster  : TcxGridLevel;
    tvwMaster  : TcxGridDBTableView;

  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
