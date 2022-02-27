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

unit DDuce.Editor.ViewList.ToolView;

interface

uses
  System.Classes, System.SysUtils, System.Actions,
  Vcl.Forms, Vcl.ActnList, Vcl.Menus, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Controls,

  Spring.Collections,

  VirtualTrees,

  DSharp.Windows.TreeViewPresenter,

  DDuce.Editor.Interfaces, DDuce.Editor.ToolView.Base;

type
  TfrmViewList = class(TCustomEditorToolView, IEditorToolView)
    aclMain               : TActionList;
    actClose              : TAction;
    actCloseSelectedViews : TAction;
    btnClose              : TButton;
    mniClose              : TMenuItem;
    pnlBottom             : TPanel;
    pnlVST                : TPanel;
    ppmMain               : TPopupMenu;

    {$REGION 'action handlers'}
    procedure actCloseExecute(Sender: TObject);
    procedure actCloseSelectedViewsExecute(Sender: TObject);
    {$ENDREGION}

  private
    FVST      : TVirtualStringTree;
    FItemList : IObjectList;
    FTVP      : TTreeViewPresenter;

    procedure FTVPSelectionChanged(Sender: TObject);

  protected
    procedure UpdateView; override;
    procedure UpdateActions; override;
    procedure Refresh;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  DSharp.Windows.ColumnDefinitions,

  DDuce.Factories.VirtualTrees, DDuce.Editor.ViewList.Data;

resourcestring
  SFileName    = 'Filename';
  SHighlighter = 'Highlighter';
  SModified    = 'Modified';
  SPath        = 'Path';

{$REGION 'construction and destruction'}
procedure TfrmViewList.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := TVirtualStringTreeFactory.CreateGrid(Self, pnlVST);
  FTVP := TTreeViewPresenter.Create(Self);
  //FTVP.MultiSelect := True;
  with FTVP.ColumnDefinitions.Add(SFileName, 200) do
  begin

  end;
  FTVP.ColumnDefinitions.Add(SHighlighter, 80);
  with FTVP.ColumnDefinitions.Add(SModified, 80) do
  begin
    ColumnType := TColumnType.ctCheckBox;
  end;
  with FTVP.ColumnDefinitions.Add(SPath, 100) do
  begin
  end;
  FItemList := TCollections.CreateObjectList<TEditorViewInfo> as IObjectList;
  Refresh;
  FTVP.View.ItemsSource   := FItemList;
  //FTVP.View.ItemTemplate :=
  FTVP.PopupMenu          := ppmMain;
  FTVP.TreeView           := FVST;
  FTVP.OnSelectionChanged := FTVPSelectionChanged;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmViewList.actCloseExecute(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TfrmViewList.actCloseSelectedViewsExecute(Sender: TObject);
var
  V : IEditorView;
  I : Integer;
begin
  for I := 0 to FTVP.SelectedItems.Count - 1 do
  begin
    V := TEditorViewInfo(FTVP.SelectedItems[I]).View;
    Views.Delete(V);
  end;
  Refresh;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmViewList.FTVPSelectionChanged(Sender: TObject);
var
  V: IEditorView;
begin
  if Assigned(FTVP.SelectedItem) then
  begin
    V := (FTVP.SelectedItem as TEditorViewInfo).View;
    Manager.ActiveView := V;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmViewList.UpdateView;
begin
  FVST.Invalidate;
end;

procedure TfrmViewList.UpdateActions;
begin
  if FItemList.Count <> Views.Count then
  begin
    Refresh;
  end;
  inherited UpdateActions;
end;

procedure TfrmViewList.Refresh;
var
  I: Integer;
begin
  FItemList.Clear;
  for I := 0 to Views.Count - 1 do
  begin
    FItemList.Add(TEditorViewInfo.Create(Views[I]));
  end;
  FTVP.Refresh;
end;
{$ENDREGION}

end.

