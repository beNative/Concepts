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

{$I Concepts.inc}

unit Concepts.Spring.ObjectDataSet.Form;

{ Demonstrates how TObjectDataSet can represent content of a Spring collection:
//    - DSharp bindings
    - Spring ObjectDataSet
}

interface

uses
  System.Actions, System.Classes,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Mask, Vcl.DBCtrls, Vcl.StdCtrls,
  Vcl.Controls, Vcl.DBGrids, Vcl.Forms,
  Data.DB,

  VirtualTrees,

  Spring, Spring.Collections, Spring.Persistence.ObjectDataSet,

  DSharp.Windows.TreeViewPresenter,

  Concepts.Types.Contact, Concepts.Resources;

type
  TfrmObjectDataSet = class(TForm)
    {$REGION 'designer controls'}
    aclMain                : TActionList;
    actConnectDataSet      : TAction;
    actConnectPresenter    : TAction;
    actDisconnectDataSet   : TAction;
    actDisconnectPresenter : TAction;
    actFillList            : TAction;
    btnConnectPresenter    : TButton;
    btnDisconnectPresenter : TButton;
    btnExecute             : TButton;
    btnExecute1            : TButton;
    btnExecute2            : TButton;
    dscMain                : TDataSource;
    edtAddress             : TLabeledEdit;
    edtCompanyName         : TLabeledEdit;
    edtDBAddress           : TDBEdit;
    edtDBCompanyName       : TDBEdit;
    edtDBEmail             : TDBEdit;
    edtDBFirstname         : TDBEdit;
    edtDBLastname          : TDBEdit;
    edtDBNumber            : TDBEdit;
    edtEmail               : TLabeledEdit;
    edtFirstname           : TLabeledEdit;
    edtLastname            : TLabeledEdit;
    edtNumber              : TLabeledEdit;
    edtRecordCount         : TEdit;
    lblRecordCount         : TLabel;
    navDataSet             : TDBNavigator;
    pnlClient              : TPanel;
    pnlDataAware           : TPanel;
    pnlLeft                : TPanel;
    pnlLeftFooter          : TPanel;
    pnlLeftHeader          : TPanel;
    pnlPresenter           : TPanel;
    pnlRight               : TPanel;
    pnlRightFooter         : TPanel;
    pnlRightHeader         : TPanel;
    pnlTop                 : TPanel;
    sbrMain                : TStatusBar;
    splVertical            : TSplitter;
    pnlDataAwareControls   : TPanel;
    pnlVCLControls         : TPanel;
    lblFirstname           : TLabel;
    lblLastname            : TLabel;
    lblEmail               : TLabel;
    lblCompanyName         : TLabel;
    lblAddress             : TLabel;
    lblNumber              : TLabel;
    {$ENDREGION}

    procedure actFillListExecute(Sender: TObject);
    procedure actConnectDataSetExecute(Sender: TObject);
    procedure actDisconnectDataSetExecute(Sender: TObject);
    procedure actDisconnectPresenterExecute(Sender: TObject);
    procedure actConnectPresenterExecute(Sender: TObject);

    procedure FormResize(Sender: TObject);
    procedure dscMainUpdateData(Sender: TObject);

  private
    FList          : IList<TContact>;
    FVST           : TVirtualStringTree;
    FDBG           : TDBGrid;
    FTVP           : TTreeViewPresenter;
    FObjectDataSet : TObjectDataset;

    function GetDataSet: TDataSet;
    function GetDataSetEnabled: Boolean;
    function GetPresenterEnabled: Boolean;
    procedure SetDataSetEnabled(const Value: Boolean);
    procedure SetPresenterEnabled(const Value: Boolean);

    procedure FillList;
    procedure DisconnectPresenter;
    procedure DisconnectDataSet;
    procedure ConnectPresenter;
    procedure ConnectDataSet;

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property DataSet: TDataSet
      read GetDataSet;

    property DataSetEnabled: Boolean
      read GetDataSetEnabled write SetDataSetEnabled;

    property PresenterEnabled: Boolean
      read GetPresenterEnabled write SetPresenterEnabled;
  end;

implementation

{$R *.dfm}

uses
  System.SysUtils,

  Concepts.Factories, Concepts.Utils;

{$REGION 'construction and destruction'}
procedure TfrmObjectDataSet.AfterConstruction;
begin
  inherited AfterConstruction;
  FList := TConceptFactories.CreateContactList;
  FVST  := TConceptFactories.CreateVirtualStringTree(Self, pnlRight);
  FTVP  := TConceptFactories.CreateTreeViewPresenter(Self, FVST, FList as IObjectList);
  FDBG  := TConceptFactories.CreateDBGrid(Self, pnlLeft, dscMain);
  FObjectDataSet          := TObjectDataset.Create(Self);
  FObjectDataSet.DataList := FList as IObjectList;
end;

procedure TfrmObjectDataSet.BeforeDestruction;
begin
  DisconnectPresenter;
  FList := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmObjectDataSet.actConnectDataSetExecute(Sender: TObject);
begin
  ConnectDataSet;
end;

procedure TfrmObjectDataSet.actConnectPresenterExecute(Sender: TObject);
begin
  ConnectPresenter;
end;

procedure TfrmObjectDataSet.actDisconnectDataSetExecute(Sender: TObject);
begin
  DisconnectDataSet;
end;

procedure TfrmObjectDataSet.actDisconnectPresenterExecute(Sender: TObject);
begin
  DisconnectPresenter;
end;

procedure TfrmObjectDataSet.actFillListExecute(Sender: TObject);
begin
  DisconnectPresenter;
  DisconnectDataSet;
  FillList;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmObjectDataSet.FormResize(Sender: TObject);
begin
  pnlLeft.Width := ClientWidth div 2;
end;

procedure TfrmObjectDataSet.dscMainUpdateData(Sender: TObject);
begin
  FVST.Invalidate;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmObjectDataSet.GetDataSet: TDataSet;
begin
  Result := FObjectDataSet;
end;

function TfrmObjectDataSet.GetDataSetEnabled: Boolean;
begin
  Result := DataSet.Active;
end;

procedure TfrmObjectDataSet.SetDataSetEnabled(const Value: Boolean);
begin
  if Value <> DataSetEnabled then
  begin
    DataSet.Active := Value;
  end;
end;

function TfrmObjectDataSet.GetPresenterEnabled: Boolean;
begin
  Result := Assigned(FTVP);
end;

procedure TfrmObjectDataSet.SetPresenterEnabled(const Value: Boolean);
begin
  if Value <> PresenterEnabled then
  begin
    if Value then
      ConnectPresenter
    else
      DisconnectPresenter;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmObjectDataSet.FillList;
begin
  TConceptFactories.FillListWithContacts(FList as IObjectList,
  StrToInt(edtRecordCount.Text));
end;

procedure TfrmObjectDataSet.ConnectDataSet;
begin
  FObjectDataSet.DataList := FList as IObjectList;
  DataSet.Active := True;
  AutoSizeDisplayWidths(DataSet);
  dscMain.DataSet := DataSet;
end;

procedure TfrmObjectDataSet.ConnectPresenter;
begin
  if not Assigned(FTVP) then
  begin
    FTVP := TConceptFactories.CreateTreeViewPresenter(Self, FVST, FList as IObjectList);
    FVST.Header.AutoFitColumns;
//    AddControlBinding(FBG, FTVP, 'View.CurrentItem.Firstname', edtFirstname);
//    AddControlBinding(FBG, FTVP, 'View.CurrentItem.Lastname', edtLastname);
//    AddControlBinding(FBG, FTVP, 'View.CurrentItem.Address', edtAddress);
//    AddControlBinding(FBG, FTVP, 'View.CurrentItem.CompanyName', edtCompanyName);
//    AddControlBinding(FBG, FTVP, 'View.CurrentItem.Email', edtEmail);
//    AddControlBinding(FBG, FTVP, 'View.CurrentItem.Country', edtCountry);
//    AddControlBinding(FBG, FTVP, 'View.CurrentItem.Number', edtNumber);
  end;
end;

procedure TfrmObjectDataSet.DisconnectDataSet;
begin
  DataSet.Active := False;
  dscMain.DataSet := nil;
end;

procedure TfrmObjectDataSet.DisconnectPresenter;
begin
  FVST.Clear;
  FreeAndNil(FTVP);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmObjectDataSet.UpdateActions;
begin
  inherited;
  actConnectDataSet.Enabled         := not DataSetEnabled;
  actDisconnectDataSet.Enabled      := DataSetEnabled;
  actConnectPresenter.Enabled       := not PresenterEnabled;
  actDisconnectPresenter.Enabled    := PresenterEnabled;
end;
{$ENDREGION}

end.
