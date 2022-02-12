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

unit Concepts.DevExpress.cxGridViewPresenter.Form;

interface

{$IFDEF DEVEXPRESS}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.ActnList, Vcl.StdCtrls,

  cxControls, cxGridLevel, cxClasses, cxGridCustomTableView,
  cxGridTableView, cxGrid, cxTL, cxTLData, cxGraphics,

  Spring.Collections,

  DSharp.Windows.CustomPresenter, DSharp.Windows.ColumnDefinitions,
  DSharp.Windows.TreeViewPresenter, DSharp.DevExpress.GridViewPresenter,
  DSharp.DevExpress.TreeListPresenter,

  VirtualTrees,

  Concepts.Types.Contact;

type
  TfrmcxGridViewPresenter = class(TForm)
    {$REGION 'designer controls'}
    aclMain                     : TActionList;
    actFillList                 : TAction;
    actInspectGridViewPresenter : TAction;
    actInspectTreeListPresenter : TAction;
    btnFillList                 : TButton;
    btnInspectGridViewPresenter : TButton;
    btnInspectTreeListPresenter : TButton;
    grdMain                     : TcxGrid;
    grlMain                     : TcxGridLevel;
    lstMain                     : TcxVirtualTreeList;
    pgcMain                     : TPageControl;
    pnlMain                     : TPanel;
    sbrMain                     : TStatusBar;
    tlcMainColumn1              : TcxTreeListColumn;
    tlcMainColumn2              : TcxTreeListColumn;
    tlcMainColumn3              : TcxTreeListColumn;
    tlcMainColumn4              : TcxTreeListColumn;
    tlcMainColumn5              : TcxTreeListColumn;
    tlcMainColumn6              : TcxTreeListColumn;
    tlcMainColumn7              : TcxTreeListColumn;
    tsGridView                  : TTabSheet;
    tsTreelist                  : TTabSheet;
    tsVirtualTree               : TTabSheet;
    tvwMain                     : TcxGridTableView;
    vstMain                     : TVirtualStringTree;
    {$ENDREGION}

    procedure actFillListExecute(Sender: TObject);
    procedure actInspectGridViewPresenterExecute(Sender: TObject);
    procedure actInspectTreeListPresenterExecute(Sender: TObject);

  private
    FList        : IList<TContact>;
    FGVPresenter : TGridViewPresenter;
    FTLPresenter : TTreeListPresenter;
    FTVPresenter : TTreeViewPresenter;

  public
    procedure AfterConstruction; override;

  end;

{$ENDIF}

implementation

{$IFDEF DEVEXPRESS}

{$R *.dfm}

uses
  DDuce.ObjectInspector.zObjectInspector,

  Concepts.Factories;

{$REGION 'construction and destruction'}
procedure TfrmcxGridViewPresenter.actInspectGridViewPresenterExecute(
  Sender: TObject);
begin
  InspectComponent(FGVPresenter);
  InspectObject(FGVPresenter.ColumnDefinitions as TColumnDefinitions);
end;

procedure TfrmcxGridViewPresenter.actInspectTreeListPresenterExecute(
  Sender: TObject);
begin
  InspectComponent(tvwMain);
  InspectComponent(FTLPresenter);
  InspectObject(FTLPresenter.ColumnDefinitions as TColumnDefinitions);
end;

procedure TfrmcxGridViewPresenter.AfterConstruction;
var
  OL : IObjectList;
begin
  inherited AfterConstruction;
  FList := TConceptFactories.CreateContactList(5000);
  OL := FList as IObjectList;
  FGVPresenter := TConceptFactories.CreateGridViewPresenter(Self, tvwMain, OL);
  FTLPresenter := TConceptFactories.CreateTreeListPresenter(Self, lstMain, OL);
  FTVPresenter := TConceptFactories.CreateTreeViewPresenter(Self, vstMain, OL);
  tvwMain.ApplyBestFit;
  lstMain.ApplyBestFit;
  vstMain.Header.AutoFitColumns(False);
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmcxGridViewPresenter.actFillListExecute(Sender: TObject);
begin
  FGVPresenter.BeginUpdate;
  FTLPresenter.BeginUpdate;
  FTVPresenter.BeginUpdate;
  TConceptFactories.FillListWithContacts(FList as IObjectList, 1000);
  FGVPresenter.EndUpdate;
  FTLPresenter.EndUpdate;
  FTVPresenter.EndUpdate;
end;
{$ENDREGION}

{$ENDIF} // DEVEXPRESS

end.
