(*
  Copyright (c) 2011, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.DevExpress.TreeListPresenter;

interface

uses
  Classes,
  cxGraphics,
  cxTL,
  cxTLData,
  DSharp.DevExpress.PresenterDataSource,
  DSharp.Windows.CustomPresenter,
  ImgList,
  Spring.Collections;

type
  TTreeListPresenter = class(TCustomPresenter)
  private
    FDataSource: TTreeListPresenterDataSource;
    FTreeList: TcxVirtualTreeList;

    procedure DoCustomDrawDataCell(Sender: TcxCustomTreeList;
      Canvas: TcxCanvas; ViewInfo: TcxTreeListEditCellViewInfo;
      var Done: Boolean);
    procedure DoFocusedNodeChanged(Sender: TcxCustomTreeList;
      PrevFocusedNode, FocusedNode: TcxTreeListNode);
    procedure DoGetNodeImageIndex(Sender: TcxCustomTreeList;
      Node: TcxTreeListNode; IndexType: TcxTreeListImageIndexType;
      var Index: TImageIndex);

    procedure SetTreeList(const Value: TcxVirtualTreeList);
  protected
    procedure DoDblClick(Sender: TObject); override;
    function GetCurrentItem: TObject; override;
    procedure InitColumns; override;
    procedure InitEvents; override;
    procedure InitProperties; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCurrentItem(const Value: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Refresh; override;
  published
    property TreeList: TcxVirtualTreeList read FTreeList write SetTreeList;
  end;

implementation

uses
  DSharp.Core.DataTemplates,
  DSharp.Windows.ControlTemplates,
  SysUtils;

{ TTreeListPresenter }

constructor TTreeListPresenter.Create(AOwner: TComponent);
begin
  inherited;
  FDataSource := TTreeListPresenterDataSource.Create(Self);
end;

destructor TTreeListPresenter.Destroy;
begin
  FDataSource.Free();
  inherited;
end;

procedure TTreeListPresenter.DoCustomDrawDataCell(Sender: TcxCustomTreeList;
  Canvas: TcxCanvas; ViewInfo: TcxTreeListEditCellViewInfo;
  var Done: Boolean);
var
  LItem: TObject;
  LItemTemplate: IControlTemplate;
begin
  LItem := TcxVirtualTreeListNode(ViewInfo.Node).RecordHandle;
  if Supports(GetItemTemplate(LItem), IControlTemplate, LItemTemplate) then
  begin
    Done := LItemTemplate.CustomDraw(LItem, ViewInfo.Column.ItemIndex,
      Canvas.Canvas, ViewInfo.VisibleRect, ImageList, dmAfterCellPaint,
      ViewInfo.Selected);
  end;
end;

procedure TTreeListPresenter.DoDblClick(Sender: TObject);
begin
  if FTreeList.HitTest.HitAtColumn and not FTreeList.IsEditing then
  begin
    inherited;
  end;
end;

procedure TTreeListPresenter.DoFocusedNodeChanged(Sender: TcxCustomTreeList;
  PrevFocusedNode, FocusedNode: TcxTreeListNode);
begin
  if Assigned(FocusedNode) then
  begin
    View.ItemIndex := FocusedNode.Index;
  end
  else
  begin
    View.ItemIndex := -1;
  end;

  DoPropertyChanged('View');
end;

procedure TTreeListPresenter.DoGetNodeImageIndex(Sender: TcxCustomTreeList;
  Node: TcxTreeListNode; IndexType: TcxTreeListImageIndexType;
  var Index: TImageIndex);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  Index := -1;

  LItem := TcxVirtualTreeListNode(Node).RecordHandle;
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    Index := LItemTemplate.GetImageIndex(LItem, 0);
  end;
end;

function TTreeListPresenter.GetCurrentItem: TObject;
begin
  if Assigned(FTreeList) and (View.ItemIndex > -1) then
  begin
    Result := TcxVirtualTreeListNode(FTreeList.FocusedNode).RecordHandle;
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TTreeListPresenter.InitColumns;
var
  i: Integer;
begin
  if Assigned(FTreeList) and UseColumnDefinitions then
  begin
    if Assigned(ColumnDefinitions) then
    begin
      for i := 0 to Pred(ColumnDefinitions.Count) do
      begin
        if i < FTreeList.ColumnCount then
        begin
          FTreeList.Columns[i].Caption.Text := ColumnDefinitions[i].Caption;
          FTreeList.Columns[i].Visible := ColumnDefinitions[i].Visible;
          FTreeList.Columns[i].Width := ColumnDefinitions[i].Width;
        end;
      end;
    end;
  end;
end;

procedure TTreeListPresenter.InitEvents;
begin
  if Assigned(FTreeList) then
  begin
    FTreeList.OnCustomDrawDataCell := DoCustomDrawDataCell;
    FTreeList.OnDblClick := DoDblClick;
    FTreeList.OnFocusedNodeChanged := DoFocusedNodeChanged;
    FTreeList.OnGetNodeImageIndex := DoGetNodeImageIndex;
  end;
end;

procedure TTreeListPresenter.InitProperties;
begin
  if Assigned(FTreeList) then
  begin
    FTreeList.Images := ImageList;
    FTreeList.PopupMenu := PopupMenu;
    FTreeList.OptionsData.SmartLoad := True;
    FTreeList.CustomDataSource := FDataSource;
  end;
end;

procedure TTreeListPresenter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FTreeList then
      FTreeList := nil;
  end;
end;

procedure TTreeListPresenter.Refresh;
begin
  if Assigned(FTreeList) and ([csLoading, csDesigning] * ComponentState = []) then
  begin
    FDataSource.DataChanged;
  end;
end;

procedure TTreeListPresenter.SetCurrentItem(const Value: TObject);
begin
  if Assigned(FTreeList) then
  begin
    FDataSource.DataController.FocusedRecordIndex := FDataSource.GetRecordIndexByHandle(Value);
  end;
end;

procedure TTreeListPresenter.SetTreeList(const Value: TcxVirtualTreeList);
begin
  if FTreeList <> Value then
  begin
    if Assigned(FTreeList) then
    begin
      FTreeList.RemoveFreeNotification(Self);
    end;

    FTreeList := Value;

    if Assigned(FTreeList) then
    begin
      FTreeList.FreeNotification(Self);
    end;
    InitControl();
  end;
end;

end.
