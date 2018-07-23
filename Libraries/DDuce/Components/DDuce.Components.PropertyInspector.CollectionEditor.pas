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

{$I DDuce.inc}

unit DDuce.Components.PropertyInspector.CollectionEditor;

interface

uses
  System.SysUtils, System.Classes, System.TypInfo, System.ImageList,
  System.Actions,
  Winapi.Windows,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ActnList, Vcl.ImgList, Vcl.ToolWin,
  Vcl.Controls, Vcl.Forms, Vcl.Menus,

  DDuce.Components.PropertyInspector;

type
  TfrmCollectionEditor = class(TForm)
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
    procedure FInspectorModified(Sender: TObject);

  private
    FCollection : TCollection;
    FInspector  : TPropertyInspector;

    function GetActiveItem: TCollectionItem;

  protected
    procedure UpdateItems;
    procedure UpdateInspector;
    procedure UpdateActions; override;

  public
    constructor Create(
      AOwner      : TComponent;
      ACollection : TCollection
    ); reintroduce;

    property ActiveItem : TCollectionItem
      read GetActiveItem;
  end;

procedure ExecuteCollectionEditor(ACollection : TCollection);

implementation

uses
  System.Rtti,

  DDuce.Reflect;

{$R *.dfm}

{$REGION 'interfaced routines'}
procedure ExecuteCollectionEditor(ACollection : TCollection);
var
  Form : TfrmCollectionEditor;
begin
  Form := TfrmCollectionEditor.Create(Application, ACollection);
  try
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
constructor TfrmCollectionEditor.Create(AOwner: TComponent;
  ACollection: TCollection);
begin
  inherited Create(AOwner);
  FCollection := ACollection;
  FInspector  := TPropertyInspector.Create(Self);
  FInspector.Parent := pnlRight;
  FInspector.Align  := alClient;
  FInspector.Splitter := FInspector.ClientWidth div 2;
  FInspector.OnModified := FInspectorModified;
  UpdateItems;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmCollectionEditor.GetActiveItem: TCollectionItem;
var
  I : Integer;
begin
  if lvCollectionItems.ItemIndex = -1 then
    I := 0
  else
    I := lvCollectionItems.ItemIndex;

  Result := TCollectionItem(lvCollectionItems.Items[I].Data);
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmCollectionEditor.actAddExecute(Sender: TObject);
begin
  FCollection.Add;
  UpdateItems;
end;

procedure TfrmCollectionEditor.actDeleteExecute(Sender: TObject);
begin
  if FCollection.Count > 0 then
  begin
    FCollection.Delete(ActiveItem.Index);
    UpdateItems;
  end;
end;

procedure TfrmCollectionEditor.actSelectAllExecute(Sender: TObject);
begin
  lvCollectionItems.SelectAll;
end;

procedure TfrmCollectionEditor.actUpExecute(Sender: TObject);
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

procedure TfrmCollectionEditor.actDownExecute(Sender: TObject);
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
procedure TfrmCollectionEditor.lvCollectionItemsDragDrop(Sender,
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

procedure TfrmCollectionEditor.lvCollectionItemsDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TfrmCollectionEditor.lvCollectionItemsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    UpdateInspector;
end;

procedure TfrmCollectionEditor.FInspectorModified(Sender: TObject);
var
  S  : string;
  V  : TValue;
  AI : TPropsPageItem;
  PI : TPropsPageItem;
  SL : TStringList;
  O  : TObject;
begin
  SL := TStringList.Create;
  try
    AI := FInspector.ActiveItem;
    O  := FInspector.Objects[0];
    S  := AI.Caption;
    V  := Reflect.Properties(O).Values[S];
    PI := AI;
    while Assigned(PI) do
    begin
      SL.Add(PI.Caption);
      PI := PI.Parent;
    end;
    // todo TS fix this!
//    for I := 0 to lvCollectionItems.Items.Count - 1 do
//    begin
//      if lvCollectionItems.Items[I].Selected then
//      begin
//        O := FCollection.Items[I];
//        for J := SL.Count -1 downto 1 do
//          O := GetObjectProp(O, SL[J]);
//        Reflect.Properties(O).Values[S] := V;
//      end;
//    end;
  finally
    SL.Free;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmCollectionEditor.UpdateActions;
var
  B: Boolean;
begin
  inherited;
  B := FCollection.Count > 1;
  actDelete.Enabled := FCollection.Count > 0;
  actUp.Enabled := B and (lvCollectionItems.ItemIndex > 0);
  actDown.Enabled := B and (lvCollectionItems.ItemIndex < FCollection.Count - 1);
end;

procedure TfrmCollectionEditor.UpdateInspector;
var
  I : Integer;
begin
  FInspector.BeginUpdate;
  try
    FInspector.Clear;
    if Assigned(lvCollectionItems.Items[lvCollectionItems.ItemIndex].Data) then
    begin
      FInspector.Add(
        TPersistent(lvCollectionItems.Items[lvCollectionItems.ItemIndex].Data)
      );

      for I := 0 to FInspector.Items.Count - 1 do
      begin
        if FInspector.Items[I].Expandable = mieYes then
          FInspector.Items[I].Expand;
      end;
    end;
  finally
    FInspector.EndUpdate;
    UpdateActions;
  end;
end;

procedure TfrmCollectionEditor.UpdateItems;
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
