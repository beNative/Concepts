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

unit Concepts.Spring.ObjectDataSet.Form;

{ Demonstrates how TObjectDataSet can represent content of a Spring collection:
    - Spring ObjectDataSet
    - DSharp TreeViewPresenter
}

{
  REMARKS:
    - TObjectDataSet issues an overflow error when compiled with overflow checks.
}

interface

uses
  System.Actions, System.Classes,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.StdCtrls, Vcl.Mask,
  Vcl.Controls, Vcl.DBGrids, Vcl.Forms,
  Data.DB, Data.Bind.Components,

  VirtualTrees,

  Spring, Spring.Collections, Spring.Data.ObjectDataSet,

  DSharp.Windows.TreeViewPresenter,

  Concepts.Types.Contact;

type
  TfrmObjectDataSet = class(TForm)
    {$REGION 'designer controls'}
    aclMain                 : TActionList;
    actConnectDataSet       : TAction;
    actConnectPresenter     : TAction;
    actDisconnectDataSet    : TAction;
    actDisconnectPresenter  : TAction;
    actFillList             : TAction;
    actInspectObjectDataSet : TAction;
    actInspectPresenter     : TAction;
    btnConnectPresenter     : TButton;
    btnDisconnectPresenter  : TButton;
    btnExecute              : TButton;
    btnExecute1             : TButton;
    btnExecute2             : TButton;
    btnInspectObjectDataSet : TButton;
    btnInspectPresenter     : TButton;
    dscMain                 : TDataSource;
    edtAddress              : TLabeledEdit;
    edtCompanyName          : TLabeledEdit;
    edtDBAddress            : TDBEdit;
    edtDBCompanyName        : TDBEdit;
    edtDBEmail              : TDBEdit;
    edtDBFirstname          : TDBEdit;
    edtDBLastname           : TDBEdit;
    edtDBNumber             : TDBEdit;
    edtEmail                : TLabeledEdit;
    edtFirstname            : TLabeledEdit;
    edtLastname             : TLabeledEdit;
    edtNumber               : TLabeledEdit;
    edtRecordCount          : TEdit;
    lblAddress              : TLabel;
    lblCompanyName          : TLabel;
    lblEmail                : TLabel;
    lblFirstname            : TLabel;
    lblLastname             : TLabel;
    lblNumber               : TLabel;
    lblRecordCount          : TLabel;
    navDataSet              : TDBNavigator;
    pnlClient               : TPanel;
    pnlDataAware            : TPanel;
    pnlDataAwareControls    : TPanel;
    pnlLeft                 : TPanel;
    pnlLeftFooter           : TPanel;
    pnlLeftHeader           : TPanel;
    pnlPresenter            : TPanel;
    pnlRight                : TPanel;
    pnlRightFooter          : TPanel;
    pnlRightHeader          : TPanel;
    pnlTop                  : TPanel;
    pnlVCLControls          : TPanel;
    sbrMain                 : TStatusBar;
    splVertical             : TSplitter;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actFillListExecute(Sender: TObject);
    procedure actConnectDataSetExecute(Sender: TObject);
    procedure actDisconnectDataSetExecute(Sender: TObject);
    procedure actDisconnectPresenterExecute(Sender: TObject);
    procedure actConnectPresenterExecute(Sender: TObject);
    procedure actInspectObjectDataSetExecute(Sender: TObject);
    procedure actInspectPresenterExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure FormResize(Sender: TObject);
    procedure dscMainUpdateData(Sender: TObject);
    procedure FTVPSelectionChanged(Sender: TObject);
    {$ENDREGION}

  private
    FList          : IList<TContact>;
    FVST           : TVirtualStringTree;
    FDBG           : TDBGrid;
    FTVP           : TTreeViewPresenter;
    FObjectDataSet : TObjectDataset;
    FBindScope     : TBindScope;

    {$REGION 'property access methods'}
    function GetDataSet: TDataSet;
    function GetDataSetEnabled: Boolean;
    function GetPresenterEnabled: Boolean;
    procedure SetDataSetEnabled(const Value: Boolean);
    procedure SetPresenterEnabled(const Value: Boolean);
    {$ENDREGION}

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
  System.SysUtils, System.Bindings.Helper,

  DDuce.ObjectInspector.zObjectInspector, DDuce.Utils,

  DSharp.Windows.ColumnDefinitions,

  Concepts.Factories;

{$REGION 'construction and destruction'}
procedure TfrmObjectDataSet.AfterConstruction;
begin
  inherited AfterConstruction;
  FList := TConceptFactories.CreateContactList;
  FVST  := TConceptFactories.CreateVirtualStringTree(Self, pnlRight);
  FTVP  := TConceptFactories.CreateTreeViewPresenter(
    Self,
    FVST,
    FList as IObjectList
  );
  FTVP.OnSelectionChanged := FTVPSelectionChanged;
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
  HourGlass(ConnectDataSet);
end;

procedure TfrmObjectDataSet.actConnectPresenterExecute(Sender: TObject);
begin
  HourGlass(ConnectPresenter);
end;

procedure TfrmObjectDataSet.actDisconnectDataSetExecute(Sender: TObject);
begin
  HourGlass(DisconnectDataSet);
end;

procedure TfrmObjectDataSet.actDisconnectPresenterExecute(Sender: TObject);
begin
  HourGlass(DisconnectPresenter);
end;

procedure TfrmObjectDataSet.actFillListExecute(Sender: TObject);
begin
  DisconnectPresenter;
  DisconnectDataSet;
  HourGlass(FillList);
end;

procedure TfrmObjectDataSet.actInspectObjectDataSetExecute(Sender: TObject);
begin
  InspectComponents(FObjectDataSet);
end;

procedure TfrmObjectDataSet.actInspectPresenterExecute(Sender: TObject);
begin
  InspectComponent(FTVP);
  InspectObject(FTVP.ColumnDefinitions as TColumnDefinitions);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmObjectDataSet.FormResize(Sender: TObject);
begin
  pnlLeft.Width := ClientWidth div 2;
end;

procedure TfrmObjectDataSet.FTVPSelectionChanged(Sender: TObject);
begin
  TBindings.Notify(FTVP);
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
  Result := Assigned(FTVP.View.ItemsSource);
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
  TConceptFactories.FillListWithContacts(
    FList as IObjectList,
    StrToInt(edtRecordCount.Text)
  );
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
  FTVP.View.ItemsSource := FList as IObjectList;
  FBindScope := TBindingsFactory.CreateBindScope(FTVP.SelectedItem as TContact, Self);
  FBindScope.AutoActivate := True;
  TBindingsFactory.CreateEditBinding(
    FBindScope,
    'FirstName',
    edtFirstname
  ).Active := True;
  TBindingsFactory.CreateEditBinding(
    FBindScope,
    'LastName',
    edtLastname
  ).Active := True;
  TBindingsFactory.CreateEditBinding(
    FBindScope,
    'CompanyName',
    edtCompanyName
  ).Active := True;
  TBindingsFactory.CreateEditBinding(
    FBindScope,
    'Email',
    edtEmail
  ).Active := True;
  TBindingsFactory.CreateEditBinding(
    FBindScope,
    'Number',
    edtNumber
  ).Active := True;
  TBindingsFactory.CreateEditBinding(
    FBindScope,
    'Address',
    edtAddress
  ).Active := True;
  FBindScope.Active := True;
end;

procedure TfrmObjectDataSet.DisconnectDataSet;
begin
  DataSet.Active := False;
  dscMain.DataSet := nil;
end;

procedure TfrmObjectDataSet.DisconnectPresenter;
begin
  FTVP.View.ItemsSource := nil;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmObjectDataSet.UpdateActions;
begin
  inherited UpdateActions;
  actConnectDataSet.Enabled      := not DataSetEnabled;
  actDisconnectDataSet.Enabled   := DataSetEnabled;
  actConnectPresenter.Enabled    := not PresenterEnabled;
  actDisconnectPresenter.Enabled := PresenterEnabled;
end;
{$ENDREGION}

end.
