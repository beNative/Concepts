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

unit Concepts.FireDAC.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ActnList,
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Stan.Param,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Phys,
  FireDAC.VCLUI.Wait,
  FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet,
  FireDAC.Phys.ADS, FireDAC.Phys.FB, FireDAC.Phys.IB, FireDAC.Phys.MSAcc,
  FireDAC.Phys.MySQL, FireDAC.Phys.PG, FireDAC.Phys.SQLite;
//  FireDAC.Phys.Oracle, FireDAC.Phys.MSSQL, FireDAC.Phys.ASA, FireDAC.Phys.DB2,
//  FireDAC.Phys.DS, FireDAC.Phys.Infx, FireDAC.Phys.MongoDB, FireDAC.Phys.ODBC,
//  FireDAC.Phys.TData;

type
  TfrmFireDAC = class(TForm)
    grpConnectionSettings       : TGroupBox;
    lblDriverID                 : TLabel;
    lblDatabase                 : TLabel;
    lblCatalog                  : TLabel;
    lblConnectionDefinitionName : TLabel;
    cbxDrivers                  : TComboBox;
    btnConnectionString         : TButton;
    edtDatabase                 : TButtonedEdit;
    edtCatalog                  : TButtonedEdit;
    grpDBMSUserLogin            : TGroupBox;
    chkOSAuthent                : TCheckBox;
    pnlLogin                    : TGridPanel;
    edtUserName                 : TEdit;
    lblPassword                 : TLabel;
    edtPassword                 : TEdit;
    lblUserName                 : TLabel;
    cbxConnectionDefs           : TComboBox;
    grpClientSettings           : TGroupBox;
    lblPacketrecords            : TLabel;
    edtPacketRecords            : TEdit;
    chkFetchOnDemand            : TCheckBox;
    chkAutoReconnect            : TCheckBox;
    chkMultipleResultSets       : TCheckBox;
    chkReadOnlyResultSets       : TCheckBox;
    chkDisconnectedMode         : TCheckBox;
    btnTestConnection           : TButton;
    conMain                     : TFDConnection;
    qryMain                     : TFDQuery;
    aclMain: TActionList;
    actEditConnectionDefinition: TAction;
    procedure actEditConnectionDefinitionExecute(Sender: TObject);
  private

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;


  end;

implementation

uses
  FireDAC.VCLUI.ConnEdit, FireDAC.Stan.Consts;

{$R *.dfm}

function ExecuteFDConnectionDialog(AConnDef: IFDStanConnectionDef;
  const ACaption: string): Boolean;
var
  LConn : TFDCustomConnection;
  LName : string;
begin
  LConn := TFDCustomConnection.Create(nil);
  try
    LConn.Temporary := True;
    LConn.Params.SetStrings(AConnDef.Params);
    LConn.ConnectionDefName := AConnDef.Name;
    AConnDef.ReadOptions(LConn.FormatOptions, LConn.UpdateOptions,
      LConn.FetchOptions, LConn.ResourceOptions);
    LName := AConnDef.Name;
    Result := TfrmFDGUIxFormsConnEdit.Execute(LConn, ACaption, nil);
    if Result then
    begin
      AConnDef.Params.SetStrings(LConn.Params);
      AConnDef.WriteOptions(LConn.FormatOptions, LConn.UpdateOptions,
        LConn.FetchOptions, LConn.ResourceOptions);
      AConnDef.Name := LName;
    end;
  finally
    LConn.Free;
  end;
end;

{$REGION 'construction and destruction'}
procedure TfrmFireDAC.actEditConnectionDefinitionExecute(Sender: TObject);
begin
  //ExecuteFDConnectionDialog()
end;

procedure TfrmFireDAC.AfterConstruction;
begin
  inherited AfterConstruction;
  FDManager.GetDriverNames(cbxDrivers.Items);
  FDManager.GetConnectionDefNames(cbxConnectionDefs.Items);
end;

procedure TfrmFireDAC.BeforeDestruction;
begin
  inherited BeforeDestruction;

end;
{$ENDREGION}

end.
