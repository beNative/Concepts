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

{$I ..\Source\DDuce.inc}

{ Demonstrates TListDataSet<T> and the following components:
    - TDBGridView - DDuce.Components.DBGridView
    - DSharp bindings
    - Spring ObjectDataSet
}

interface

uses
  System.Actions, System.Classes,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Mask, Vcl.DBCtrls, Vcl.StdCtrls,
  Vcl.Controls,
  Data.DB,

  VirtualTrees,

  Spring, Spring.Collections, Spring.Persistence.ObjectDataSet,

  DSharp.Bindings, DSharp.Bindings.VCLControls,
  DSharp.Windows.TreeViewPresenter,

  DDuce.Components.ListDataSet, DDuce.Components.GridView,
  DDuce.Components.DBGridView,

  Concepts.Types.Contact;

type
  TfrmListDataSet = class(TForm)
    {$REGION 'designer controls'}
    aclMain                   : TActionList;
    actConnectDataSet         : TAction;
    actConnectPresenter       : TAction;
    actDisconnectDataSet      : TAction;
    actDisconnectPresenter    : TAction;
    actFillList               : TAction;
    actInspectComponents      : TAction;
    btnConnectPresenter       : TButton;
    btnDisconnectPresenter    : TButton;
    btnExecute                : TButton;
    btnExecute1               : TButton;
    btnExecute2               : TButton;
    dscMain                   : TDataSource;
    edtAddress                : TLabeledEdit;
    edtCompanyName            : TLabeledEdit;
    edtCountry                : TLabeledEdit;
    edtDBAddress              : TDBEdit;
    edtDBCompanyName          : TDBEdit;
    edtDBCountry              : TDBEdit;
    edtDBEmail                : TDBEdit;
    edtDBFirstname            : TDBEdit;
    edtDBLastname             : TDBEdit;
    edtDBNumber               : TDBEdit;
    edtEmail                  : TLabeledEdit;
    edtFirstname              : TLabeledEdit;
    edtLastname               : TLabeledEdit;
    edtNumber                 : TLabeledEdit;
    edtRecordCount            : TEdit;
    lblRecordCount            : TLabel;
    navDataSet                : TDBNavigator;
    pnlClient                 : TPanel;
    pnlDataAware              : TPanel;
    pnlLeft                   : TPanel;
    pnlLeftFooter             : TPanel;
    pnlLeftHeader             : TPanel;
    pnlPresenter              : TPanel;
    pnlRight                  : TPanel;
    pnlRightFooter            : TPanel;
    pnlRightHeader            : TPanel;
    pnlTop                    : TPanel;
    sbrMain                   : TStatusBar;
    splVertical               : TSplitter;
    pnlDataAwareControls      : TPanel;
    pnlVCLControls            : TPanel;
    lblFirstname              : TLabel;
    lblLastname               : TLabel;
    lblEmail                  : TLabel;
    lblCompanyName            : TLabel;
    lblAddress                : TLabel;
    lblCountry                : TLabel;
    lblNumber                 : TLabel;
    {$ENDREGION}

    procedure actFillListExecute(Sender: TObject);
    procedure actConnectDataSetExecute(Sender: TObject);
    procedure actDisconnectDataSetExecute(Sender: TObject);
    procedure actInspectComponentsExecute(Sender: TObject);
    procedure actDisconnectPresenterExecute(Sender: TObject);
    procedure actConnectPresenterExecute(Sender: TObject);

    procedure FormResize(Sender: TObject);
    procedure dscMainUpdateData(Sender: TObject);

  private
    FList          : IList<TContact>;
    FVST           : TVirtualStringTree;
    FDBGV          : TDBGridView;
    FTVP           : TTreeViewPresenter;
    FBG            : TBindingGroup;
    FListDataSet   : TListDataSet<TContact>;
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
    procedure FDBGVHeaderClick(Sender: TObject; Section: TGridHeaderSection);

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
  Vcl.Forms,

  Concepts.Factories, Concepts.Helpers;

{$REGION 'construction and destruction'}
procedure TfrmListDataSet.AfterConstruction;
begin
  inherited AfterConstruction;
  //FList               := TConceptFactories.CreateContactList;
  FVST                := TConceptFactories.CreateVST(Self, pnlRight);
  FDBGV               := TConceptFactories.CreateDBGridView(Self, pnlLeft, dscMain);
  FListDataSet        := TListDataset<TContact>.Create(Self, FList);
  FObjectDataSet      := TObjectDataset.Create(Self);
  FBG                 := TBindingGroup.Create(Self);
  FDBGV.OnHeaderClick := FDBGVHeaderClick;
  //FDBGV.OnGetSortDirection := FDBGVGe
end;

procedure TfrmListDataSet.BeforeDestruction;
begin
  DisconnectPresenter;
  FList := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmListDataSet.actConnectDataSetExecute(Sender: TObject);
begin
  ConnectDataSet;
end;

procedure TfrmListDataSet.actConnectPresenterExecute(Sender: TObject);
begin
  ConnectPresenter;
end;

procedure TfrmListDataSet.actDisconnectDataSetExecute(Sender: TObject);
begin
  DisconnectDataSet;
end;

procedure TfrmListDataSet.actDisconnectPresenterExecute(Sender: TObject);
begin
  DisconnectPresenter;
end;

procedure TfrmListDataSet.actFillListExecute(Sender: TObject);
begin
  DisconnectPresenter;
  DisconnectDataSet;
  FillList;
end;

procedure TfrmListDataSet.actInspectComponentsExecute(Sender: TObject);
begin
  //InspectComponents([DataSet, FVST, FDBGV, FTVP]);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmListDataSet.FormResize(Sender: TObject);
begin
  pnlLeft.Width := ClientWidth div 2;
end;

procedure TfrmListDataSet.dscMainUpdateData(Sender: TObject);
begin
  FVST.Invalidate;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmListDataSet.GetDataSet: TDataSet;
begin
  Result := FListDataSet;
//  Result := FObjectDataSet;
end;

function TfrmListDataSet.GetDataSetEnabled: Boolean;
begin
  Result := DataSet.Active;
end;

procedure TfrmListDataSet.SetDataSetEnabled(const Value: Boolean);
begin
  if Value <> DataSetEnabled then
  begin
    DataSet.Active := Value;
  end;
end;

function TfrmListDataSet.GetPresenterEnabled: Boolean;
begin
  Result := False;
  Result := Assigned(FTVP);
end;

procedure TfrmListDataSet.SetPresenterEnabled(const Value: Boolean);
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
procedure TfrmListDataSet.FDBGVHeaderClick(Sender: TObject;
  Section: TGridHeaderSection);
var
//  bDesc : Boolean;
  Field : TField;
begin
  Screen.Cursor := crSQLWait;
  try
    Field := FDBGV.Columns[Section.ColumnIndex].Field;
    if Assigned(Field) and (Field.FieldKind = fkData) then
    begin
//      FList.Sort(
//        function(const Left, Right: TContact): Integer
//        var
//          V1 : TValue;
//          V2 : TValue;
//        begin
//          V1 := Left.GetProperty(Field.FieldName).GetValue(Left);
//          V2 := Right.GetProperty(Field.FieldName).GetValue(Right);
//          if V1.IsOrdinal and V2.IsOrdinal then
//          begin
//            Result := Math.CompareValue(V1.AsOrdinal, V2.AsOrdinal);
//          end else
//          if V1.IsFloat and V2.IsFloat then
//          begin
//            Result := Math.CompareValue(V1.AsFloat, V2.AsFloat);
//          end else
//          if V1.IsString and V2.IsString then
//          begin
//            Result := SysUtils.CompareStr(V1.AsString, V2.AsString);
//          end else
//          begin
//            Result := 0;
//          end;
//        end);
    DataSet.Refresh;
//      FSortedFieldName := Field.FieldName;
//      if SortDataSet(FSortedFieldName, bDesc) then
//        if bDesc then
//          FSortDirection := gsDescending
//        else
//          FSortDirection := gsAscending
//      else
//      begin
//        FSortDirection   := gsNone;
//        FSortedFieldName := '';
//      end;
//      if GotoFirstAfterSort then
//        DataSet.First;
//    end
//    else
//    begin
//      FSortDirection   := gsNone;
//      FSortedFieldName := '';
//    end;
//    // TODO: should maybe only be triggered after a successful sort operation.
   end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmListDataSet.FillList;
begin
  //TConceptFactories.FillListWithContacts(FList, StrToInt(edtRecordCount.Text));
end;

procedure TfrmListDataSet.ConnectDataSet;
begin
  //FObjectDataSet.SetDataList<TContact>(FList as IList<TContact>);
  DataSet.Active := True;

  dscMain.DataSet := DataSet;
  FDBGV.AutoSizeCols;
end;

procedure TfrmListDataSet.ConnectPresenter;
begin
  if not Assigned(FTVP) then
  begin
    FTVP := TConceptFactories.CreateTVP(Self, FVST, FList as IObjectList);
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

procedure TfrmListDataSet.DisconnectDataSet;
begin
  DataSet.Active := False;
  dscMain.DataSet := nil;
end;

procedure TfrmListDataSet.DisconnectPresenter;
begin
  FVST.Clear;
  FBG.Bindings.Clear;
  FreeAndNil(FTVP);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmListDataSet.UpdateActions;
begin
  inherited;
  actConnectDataSet.Enabled         := not DataSetEnabled;
  actDisconnectDataSet.Enabled      := DataSetEnabled;
  actConnectPresenter.Enabled       := not PresenterEnabled;
  actDisconnectPresenter.Enabled    := PresenterEnabled;
end;
{$ENDREGION}

end.
