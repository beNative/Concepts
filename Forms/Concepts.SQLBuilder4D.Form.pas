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

{$I Concepts.inc}

unit Concepts.SQLBuilder4D.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Actions, Vcl.ActnList, Vcl.ExtCtrls;

type
  TfrmSQLBuilder4D = class(TForm)
    mmoMain   : TMemo;
    aclMain   : TActionList;
    actTest1  : TAction;
    actTest2  : TAction;
    btnTest1  : TButton;
    btnTest2  : TButton;
    pnlHeader : TPanel;
    lblHeader : TLabel;

    procedure actTest1Execute(Sender: TObject);
    procedure actTest2Execute(Sender: TObject);
  public

  end;

implementation

{$R *.dfm}

uses
  SQLBuilder4D;

const
  NAME        = 'Name';
  ID          = 'Id';
  ITEM        = 'Item';
  DESCRIPTION = 'Description';
  PERSON      = 'Person';
  FIRSTNAME   = 'FirstName';

{$REGION 'action handlers'}
procedure TfrmSQLBuilder4D.actTest1Execute(Sender: TObject);
begin
  mmoMain.Text := SQL.Select
    .Distinct
    .Column('Id')
    .Column('Name').&As('CustomerName')
    .From('Customer')
    .Where('Name').Equal('John')
    .&And('City').Like('New York', loContaining)
    .ToString;
end;

procedure TfrmSQLBuilder4D.actTest2Execute(Sender: TObject);
begin
  mmoMain.Text := SQL.Select
    .Column(ID)
    .Column(NAME)
    .Column(DESCRIPTION)
    .From(ITEM)
    .Join(PERSON, 'Id = Id').ToString;
end;
{$ENDREGION}

end.
