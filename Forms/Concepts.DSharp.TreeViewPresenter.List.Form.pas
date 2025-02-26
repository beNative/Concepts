{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.DSharp.TreeViewPresenter.List.Form;

{ Form demonstrating the usage of the DSharp TTreeViewPresenter which simplifies
  the process of representing data in a TVirtualStringTree control.
  This demo shows also how some custom drawing can be performed. }

{$REGION 'documentation'}
{
  In this example we will create a list of contacts (IList<TContact>) and
  visualize its contents using a TTreeViewPresenter and a TVirtualStringTree.
  The presenter does its magic by assigning the list as a IObjectList and by
  default it wil automatically provides the published properties of TContact as
  columns in the virtual treeview.
  As IObjectList = interface(IList<TObject>), we can cast our typed list to
  IObjectList.
}
{$ENDREGION}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions, System.Rtti,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.ActnList,

  VirtualTrees,

  zObjInspector, zObjInspTypes,

  DSharp.Windows.ColumnDefinitions, DSharp.Windows.TreeViewPresenter,
  DSharp.Bindings, DSharp.Windows.CustomPresenter,

  Spring.Collections,

  Concepts.Types.Contact;

type
  TfrmTreeViewPresenterList = class(TForm)
    {$REGION 'designer controls'}
    aclMain                   : TActionList;
    pnlBottom                 : TPanel;
    pnlLeft                   : TPanel;
    pnlLeftBottom             : TPanel;
    pnlLeftTop                : TPanel;
    pnlTop                    : TPanel;
    pnlTreeviewPresenter      : TPanel;
    pnlVirtualStringTreeTitle : TPanel;
    splHorizontal             : TSplitter;
    splVertical               : TSplitter;
    {$ENDREGION}

  private
    FList       : IObjectList;
    FOI         : TzObjectInspector;
    FVST        : TVirtualStringTree;
    FTVP        : TTreeViewPresenter;
    FVSTColumns : TVirtualStringTree;
    FTVPColumns : TTreeViewPresenter;

    procedure FTVPColumnsSelectionChanged(Sender: TObject);
    procedure FTVPSelectionChanged(Sender: TObject);

    function FTVPColumnDefinitionsCustomDraw(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ): Boolean;

    function FOIItemSetValue(
      Sender      : TControl;
      PItem       : PPropItem;
      var NewValue: TValue
    ): Boolean;
    function FOIBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

    procedure CreateColumnDefinitionsView;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  DSharp.Windows.ColumnDefinitions.ControlTemplate,
  DSharp.Windows.ControlTemplates,

  DDuce.Factories.zObjInspector,

  Concepts.Factories;

{$REGION 'construction and destruction'}
procedure TfrmTreeViewPresenterList.AfterConstruction;
begin
  inherited AfterConstruction;
  FList := TConceptFactories.CreateContactList(10000) as IObjectList;
  FVST  := TConceptFactories.CreateVirtualStringTree(Self, pnlTop);
  FTVP  := TConceptFactories.CreateTreeViewPresenter(
    Self,
    FVST,
    FList,
    nil,
    nil,
    FTVPColumnDefinitionsCustomDraw
  );
  FTVP.View.ItemTemplate :=
    TColumnDefinitionsControlTemplate.Create(FTVP.ColumnDefinitions);
  FOI := TzObjectInspectorFactory.Create(Self, pnlLeftTop);
  FOI.OnBeforeAddItem := FOIBeforeAddItem;
  FOI.OnItemSetValue  := FOIItemSetValue;
  FOI.Component       := FTVP;
  CreateColumnDefinitionsView;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmTreeViewPresenterList.FOIBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not (PItem.Prop.PropertyType is TRttiMethodType);
end;

function TfrmTreeViewPresenterList.FOIItemSetValue(Sender: TControl;
  PItem: PPropItem; var NewValue: TValue): Boolean;
begin
  Result := True; // allow assigning NewValue to the property.
  FTVP.TreeView.Invalidate;
end;

function TfrmTreeViewPresenterList.FTVPColumnDefinitionsCustomDraw(
  Sender: TObject; ColumnDefinition: TColumnDefinition; Item: TObject;
  TargetCanvas: TCanvas; CellRect: TRect; ImageList: TCustomImageList;
  DrawMode: TDrawMode; Selected: Boolean): Boolean;
begin
  { dmBeforeCellPaint
      Called before any painting is performed. This is commonly used to paint
      the cell background or custom cell borders.
    dmPaintText
      Called just before the celltext is painted. This is commonly used to
      setup custom font styles or colors on the target canvas.
    dmAfterCellPaint
      Called after cell text is drawn. Used to draw additional stuff in the
      cell.
  }
//  if ColumnDefinition.DisplayName = 'FirstName' then
//  begin
//    if DrawMode = dmBeforeCellPaint then
//    begin
//      TargetCanvas.Brush.Color := clWebPaleGoldenrod;
//      TargetCanvas.FillRect(CellRect);
//    end
//    else if DrawMode = dmPaintText then
//    begin
//      TargetCanvas.Font.Color := clRed;
//    end
//    else if DrawMode = dmAfterCellPaint then
//    begin
//      TargetCanvas.Pen.Width := 2;
//      TargetCanvas.Pen.Color := clBlue;
//      TargetCanvas.Chord(16, 16, 4, 4, 8, 8, 16, 16);
//    end;
//  end;
  // intended just to report that some custom drawing occured. This value is
  // not used by the presenter component.
  Result := True;
end;

procedure TfrmTreeViewPresenterList.FTVPColumnsSelectionChanged(Sender: TObject);
begin
  if Assigned(FTVPColumns.SelectedItem) then
    FOI.Component := FTVPColumns.SelectedItem;
end;

procedure TfrmTreeViewPresenterList.FTVPSelectionChanged(Sender: TObject);
begin
  if Assigned(FTVP) then
    FOI.Component := FTVP;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmTreeViewPresenterList.CreateColumnDefinitionsView;
var
  CDList : IObjectList;
  C      : TColumnDefinition;
  I      : Integer;
begin
  CDList := TCollections.CreateObjectList<TColumnDefinition> as IObjectList;
  for I := 0 to FTVP.ColumnDefinitions.Count - 1 do
  begin
    C := FTVP.ColumnDefinitions[I];
    CDList.Add(C);
  end;
  FVSTColumns := TConceptFactories.CreateVirtualStringTree(Self, pnlLeftBottom);
  FTVPColumns :=
    TConceptFactories.CreateTreeViewPresenter(Self, FVSTColumns, CDList);
  FTVPColumns.SelectionMode := smSingle;
  FTVPColumns.OnSelectionChanged := FTVPColumnsSelectionChanged;
  FTVP.OnSelectionChanged := FTVPSelectionChanged;
end;
{$ENDREGION}

end.
