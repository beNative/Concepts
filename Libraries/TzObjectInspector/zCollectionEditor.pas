{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit zCollectionEditor;

interface

uses
  System.SysUtils, System.Classes, System.TypInfo, System.ImageList,
  System.Actions, System.Rtti,
  Winapi.Windows,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ActnList, Vcl.ImgList, Vcl.ToolWin,
  Vcl.Controls, Vcl.Forms, Vcl.Menus,

  zObjInspector;

type
  TzCollectionEditorDialog = class(TzInspDialog)
    aclMain           : TActionList;
    actAdd            : TAction;
    actDelete         : TAction;
    actDeleteAll      : TAction;
    actDown           : TAction;
    actSelectAll      : TAction;
    actUp             : TAction;
    btnAdd            : TToolButton;
    btnDelete         : TToolButton;
    btnDown           : TToolButton;
    btnSeperator      : TToolButton;
    btnUp             : TToolButton;
    imlMain           : TImageList;
    lvCollectionItems : TListView;
    mniAdd            : TMenuItem;
    mniDelete         : TMenuItem;
    mniDeleteAll      : TMenuItem;
    mniSelectAll      : TMenuItem;
    N1                : TMenuItem;
    pnlLeft           : TPanel;
    pnlRight          : TPanel;
    ppmMain           : TPopupMenu;
    splVertical       : TSplitter;
    tlbMain           : TToolBar;

    procedure actAddExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actUpExecute(Sender: TObject);
    procedure actDownExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);

    procedure lvCollectionItemsDragDrop(
      Sender : TObject;
      Source : TObject;
      X      : Integer;
      Y      : Integer
    );
    procedure lvCollectionItemsDragOver(
      Sender     : TObject;
      Source     : TObject;
      X          : Integer;
      Y          : Integer;
      State      : TDragState;
      var Accept : Boolean
    );
    procedure lvCollectionItemsSelectItem(
      Sender   : TObject;
      Item     : TListItem;
      Selected : Boolean
    );

    function FInspectorItemSetValue(
      Sender       : TControl;
      PItem        : PPropItem;
      var NewValue : TValue
    ) : Boolean;

  private
    FCollection : TCollection;
    FInspector  : TzObjectInspector;
    FContext    : TRttiContext;

    function GetActiveItem: TCollectionItem;

  protected
    procedure UpdateItems;
    procedure UpdateInspector;
    procedure UpdateActions; override;
    procedure InitDialog; override;

  public
    property ActiveItem : TCollectionItem
      read GetActiveItem;
  end;

implementation

{$R *.dfm}

uses
  DDuce.Reflect, DDuce.Logger;

{$REGION 'construction and destruction'}
{$ENDREGION}

{$REGION 'property access methods'}
function TzCollectionEditorDialog.GetActiveItem: TCollectionItem;
var
  I : Integer;
begin
  if lvCollectionItems.ItemIndex = -1 then
    I := 0
  else
    I := lvCollectionItems.ItemIndex;

  Result := TCollectionItem(lvCollectionItems.Items[I].Data);
end;
procedure TzCollectionEditorDialog.InitDialog;
begin
  inherited InitDialog;
  FCollection := TCollection(PropItem.Value.AsObject);
  FInspector  := TzObjectInspector.Create(Self);
  FInspector.Parent := pnlRight;
  FInspector.Align  := alClient;
  FInspector.SplitterPos := FInspector.ClientWidth div 2;
  FInspector.OnItemSetValue := FInspectorItemSetValue;
  UpdateItems;
end;

{$ENDREGION}

{$REGION 'action handlers'}
procedure TzCollectionEditorDialog.actAddExecute(Sender: TObject);
begin
  FCollection.Add;
  UpdateItems;
end;

procedure TzCollectionEditorDialog.actDeleteExecute(Sender: TObject);
begin
  if FCollection.Count > 0 then
  begin
    FCollection.Delete(ActiveItem.Index);
    UpdateItems;
  end;
end;

procedure TzCollectionEditorDialog.actSelectAllExecute(Sender: TObject);
begin
  lvCollectionItems.SelectAll;
end;

procedure TzCollectionEditorDialog.actUpExecute(Sender: TObject);
var
  I : Integer;
begin
  if ActiveItem.Index > 0 then
  begin
    ActiveItem.Index := ActiveItem.Index - 1;
    I := ActiveItem.Index;
    UpdateItems;
    lvCollectionItems.ItemIndex := I;
  end;
end;

procedure TzCollectionEditorDialog.actDownExecute(Sender: TObject);
var
  I : Integer;
begin
  if ActiveItem.Index < FCollection.Count - 1 then
  begin
    ActiveItem.Index := ActiveItem.Index + 1;
    I := ActiveItem.Index;
    UpdateItems;
    lvCollectionItems.ItemIndex := I;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TzCollectionEditorDialog.lvCollectionItemsDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  if not Assigned(Sender) or not Assigned(Source) or
     not Assigned(lvCollectionItems.DropTarget) or
     not Assigned(lvCollectionItems.Selected) or
     (lvCollectionItems.Selected.Index = lvCollectionItems.DropTarget.Index)
  then
    Exit;

  FCollection.Items[lvCollectionItems.Selected.Index].Index :=
    lvCollectionItems.DropTarget.Index;
  UpdateItems;
end;

procedure TzCollectionEditorDialog.lvCollectionItemsDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TzCollectionEditorDialog.lvCollectionItemsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    UpdateInspector;
end;

function TzCollectionEditorDialog.FInspectorItemSetValue(Sender: TControl;
  PItem: PPropItem; var NewValue: TValue): Boolean;
var
  I  : Integer;
  S  : string;
  V  : TValue;
  O  : TObject;
begin
  for I := 0 to lvCollectionItems.Items.Count - 1 do
  begin
    if lvCollectionItems.Items[I].Selected then
    begin
      O := FCollection.Items[I];


      // TODO : does not work anymore!
      //Reflect.Properties<TCollectionItem>(FCollection.Items[I]).Values[PItem.Name] := NewValue;
      //.Values[PItem.Name] := NewValue;

    end;
  end;
  Result := True;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TzCollectionEditorDialog.UpdateActions;
var
  B: Boolean;
begin
  inherited;
  B := FCollection.Count > 1;
  actDelete.Enabled := FCollection.Count > 0;
  actUp.Enabled := B and (lvCollectionItems.ItemIndex > 0);
  actDown.Enabled := B and (lvCollectionItems.ItemIndex < FCollection.Count - 1);
end;

procedure TzCollectionEditorDialog.UpdateInspector;
var
  I : Integer;
begin
  FInspector.BeginUpdate;
  try
    if Assigned(lvCollectionItems.Items[lvCollectionItems.ItemIndex].Data) then
    begin
      FInspector.Component :=
        TPersistent(lvCollectionItems.Items[lvCollectionItems.ItemIndex].Data);
//      FInspector.Add(
//        TPersistent(lvCollectionItems.Items[lvCollectionItems.ItemIndex].Data)
//      );

      for I := 0 to FInspector.Items.Count - 1 do
      begin
//        if FInspector.Items[I].Expandable = mieYes then
//          FInspector.Items[I].Expand;
      end;
    end;
  finally
    FInspector.EndUpdate;
    UpdateActions;
  end;
end;

procedure TzCollectionEditorDialog.UpdateItems;
var
  S  : string;
  I  : Integer;
  LI : TListItem;
begin
  lvCollectionItems.Clear;
  for I := 0 to FCollection.Count - 1 do
  begin
    S := Format('%s', [FCollection.Items[I].DisplayName]);
    LI := lvCollectionItems.Items.Add;
    LI.Caption := IntToStr(I);
    LI.SubItems.Add(S);
    LI.Data := FCollection.Items[I];
  end;
end;
{$ENDREGION}

end.

