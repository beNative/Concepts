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

unit DSharp.DevExpress.GridViewPresenter;

interface

uses
  Classes,
  Controls,
  cxCustomData,
  cxGraphics,
  cxGridCardView,
  cxGridCustomTableView,
  cxGridCustomView,
  cxGridTableView,
  DSharp.DevExpress.PresenterDataSource,
  DSharp.Windows.CustomPresenter,
  DSharp.WIndows.CustomPresenter.Types,
  Spring.Collections;

{$I 'DSharp.Windows.CustomPresenter.Types.inc'}

type
  TGridViewPresenter = class(TCustomPresenter)
  private
    FCheckedItems: IList<TObject>;
    FDataSource: TGridViewPresenterDataSource;
    FGridView: TcxCustomGridView;
    FSelectedItems: IList<TObject>;
    procedure DoCellDblClick(Sender: TcxCustomGridTableView;
      CellViewInfo: TcxGridTableDataCellViewInfo; Button: TMouseButton;
      Shift: TShiftState; var Handled: Boolean);
    procedure DoFocusedRecordChanged(Sender: TcxCustomGridTableView;
      PrevFocusedRecord, FocusedRecord: TcxCustomGridRecord;
      NewItemRecordFocusingChanged: Boolean);
    procedure DoSelectionChanged(AView: TcxCustomGridTableView);

    function GetAutoCellHeight: Boolean;
    function GetCellImageIndex(AViewInfo: TcxGridTableDataCellViewInfo): Integer;
    function GetItemAtPosition(AGrid: TcxGridSite; X, Y: Integer): TObject;
    function GetRecordItem(ARecord: TcxCustomGridRecord): TObject; inline;
    function GetReservedImageHeight: Integer;
    function GetReservedImageWidth: Integer;
    function GetSelectedItem: TObject;
    procedure SetGridView(const Value: TcxCustomGridView);
    property AutoCellHeight: Boolean read GetAutoCellHeight;
  protected
    procedure ApplyAllowMove; override;
    procedure ApplyCheckSupport; override;
    procedure ApplySelectionMode; override;
    procedure ApplyShowHeader; override;
    function CalcCellContentHeight(
      AViewInfo: TcxGridTableDataCellViewInfo): Integer;

    procedure DoCheckedItemsChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
    procedure DoSourceCollectionChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction); reintroduce;
    procedure DoSelectedItemsChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);

    procedure HandleDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure HandleDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure HandleStartDrag(Sender: TObject; var DragObject: TDragObject);

    procedure HandleCanFocusRecord(Sender: TcxCustomGridTableView;
      ARecord: TcxCustomGridRecord; var AAllow: Boolean);
    procedure HandleDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure HandleEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure HandleFilterRecord(ADataController: TcxCustomDataController;
      ARecordIndex: Integer; var Accept: Boolean);
    procedure HandleGetCellHeight(Sender: TcxCustomGridTableView;
      ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      ACellViewInfo: TcxGridTableDataCellViewInfo; var AHeight: Integer);

    function IsCustomDataSourceSupported(AGridView: TcxCustomGridView): Boolean;

    function GetCurrentItem: TObject; override;
    procedure InitCardViewColumns(ACardView: TcxGridCardView);
    procedure InitTableViewColumns(ATableView: TcxGridTableView);
    procedure InitColumns; override;
    procedure InitEvents; override;
    procedure InitProperties; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCurrentItem(const Value: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyFilter; override;
    procedure Refresh; override;

    property CheckedItems: IList<TObject> read FCheckedItems;
    property SelectedItem: TObject read GetSelectedItem;
    property SelectedItems: IList<TObject> read FSelectedItems;
  published
    property AllowMove default False;
    property GridView: TcxCustomGridView read FGridView write SetGridView;
  end;

implementation

uses
  cxCheckBox,
  cxControls,
  cxProgressBar,
  cxTextEdit,
  dxCore,
  DSharp.Core.DataTemplates,
  DSharp.Windows.ColumnDefinitions,
  DSharp.Windows.ControlTemplates,
  Spring.Collections.Lists,
  SysUtils;

const
  CIconSpacing = 2;

type
  TcxDataControllerAccessor = class(TcxCustomDataController)
  end;

  TcxCellPainter = class(TcxGridTableDataCellPainter)
  end;

  TcxCellViewInfoAccessor = class(TcxGridTableDataCellViewInfo)
  end;

  TcxProviderAccessor = class(TcxCustomDataProvider)
  end;


procedure UpdateList(const Target, Source: IList<TObject>);
var
  i: Integer;
begin
  i := 0;
  while i < Target.Count do
  begin
    if Source.Contains(Target[i]) then
    begin
      Inc(i);
    end
    else
    begin
      Target.Delete(i);
    end;
  end;
  for i := 0 to Pred(Source.Count) do
  begin
    if not Target.Contains(Source[i]) then
    begin
      Target.Add(Source[i]);
    end;
  end;
end;

{ TGridViewPresenter }

constructor TGridViewPresenter.Create(AOwner: TComponent);
begin
  inherited;
  FCheckedItems := TList<TObject>.Create();
  FCheckedItems.OnChanged.Add(DoCheckedItemsChanged);
  FDataSource := TGridViewPresenterDataSource.Create(Self.View, FCheckedItems);
  FSelectedItems := TList<TObject>.Create();
  FSelectedItems.OnChanged.Add(DoSelectedItemsChanged);
  AllowMove := False;
  ShowHeader := True;
  View.OnCollectionChanged.Add(DoSourceCollectionChanged);
end;

destructor TGridViewPresenter.Destroy;
begin
  FCheckedItems.OnChanged.Remove(DoCheckedItemsChanged);
  FSelectedItems.OnChanged.Remove(DoSelectedItemsChanged);
  View.OnCollectionChanged.Remove(DoSourceCollectionChanged);
  FDataSource.Free();
  inherited;
end;

procedure TGridViewPresenter.ApplyAllowMove;
begin
  if Assigned(FGridView) then
  begin
    if AllowMove then
    begin
      FGridView.DragMode := dmAutomatic;
    end
    else
    begin
      FGridView.DragMode := dmManual;
    end;
  end;
end;

procedure TGridViewPresenter.ApplyCheckSupport;
begin
  inherited;
  if CheckSupport = csNone then
  begin
    FCheckedItems.Clear;
    FDataSource.CheckColumnIndex := -1;
  end
  else
  begin
    FDataSource.CheckColumnIndex := 0;
  end;
  if Assigned(FGridView) then
  begin
    InitColumns();
  end;
end;

procedure TGridViewPresenter.ApplyFilter;
begin
  inherited;
  FDataSource.DataChanged();
end;

procedure TGridViewPresenter.ApplySelectionMode;
begin
  if FGridView is TcxCustomGridTableView then
  begin
    TcxCustomGridTableView(FGridView).OptionsSelection.MultiSelect := SelectionMode = smMulti;
  end;
end;

procedure TGridViewPresenter.ApplyShowHeader;
var
  LTable: TcxGridTableView;
begin
  if Assigned(FGridView) and (FGridView is TcxGridTableView) then
  begin
    LTable := TcxGridTableView(FGridView);
    LTable.OptionsView.Header := ShowHeader;
  end;
end;

procedure TGridViewPresenter.DoCellDblClick(Sender: TcxCustomGridTableView;
  CellViewInfo: TcxGridTableDataCellViewInfo; Button: TMouseButton;
  Shift: TShiftState; var Handled: Boolean);
begin
  DoDblClick(Sender);
end;

procedure TGridViewPresenter.DoCheckedItemsChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
begin
  case Action of
    caAdded, caRemoved, caExtracted:
    begin
      FDataSource.DataChanged();
    end;
  end;
end;

procedure TGridViewPresenter.DoFocusedRecordChanged(
  Sender: TcxCustomGridTableView; PrevFocusedRecord,
  FocusedRecord: TcxCustomGridRecord; NewItemRecordFocusingChanged: Boolean);
begin
  if Assigned(FocusedRecord) then
  begin
    View.ItemIndex := FocusedRecord.RecordIndex;
  end
  else
  begin
    View.ItemIndex := -1;
  end;
  if not Sender.OptionsSelection.MultiSelect then
  begin
    DoSelectionChanged(Sender);
  end;

  DoPropertyChanged('View');
end;

procedure TGridViewPresenter.DoSelectedItemsChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  i: Integer;
  LTableView: TcxCustomGridTableView;
begin
  if FCollectionUpdateLock > 0 then
    Exit;

  LTableView := TcxCustomGridTableView(FGridView);
  case Action of
    caAdded:
    begin
      for i := 0 to LTableView.ViewData.RecordCount - 1 do
      begin
        if Item = GetRecordItem(LTableView.ViewData.Records[i]) then
        begin
          LTableView.ViewData.Records[i].Selected := True;
          LTableView.ViewData.Records[i].Focused := True;
          Break;
        end;
      end;
    end;
    caRemoved, caExtracted:
    begin
      for i := 0 to LTableView.ViewData.RecordCount - 1 do
      begin
        if Item = GetRecordItem(LTableView.ViewData.Records[i]) then
        begin
          LTableView.ViewData.Records[i].Selected := False;
          LTableView.ViewData.Records[i].Focused := False;
          Break;
        end;
      end;
    end;
//    caReplace: ;
//    caMove: ;
//    caReset: ;
  end;
end;

procedure TGridViewPresenter.DoSelectionChanged(AView: TcxCustomGridTableView);
var
  i: Integer;
  LSelectedItems: IList<TObject>;
begin
  Inc(FCollectionUpdateLock);
//  BeginInternalCollectionUpdate();
  try
    LSelectedItems := TList<TObject>.Create();
    for i := 0 to AView.Controller.SelectedRecordCount - 1 do
    begin
      LSelectedItems.Add(GetRecordItem(AView.Controller.SelectedRecords[i]));
    end;
    UpdateList(FSelectedItems, LSelectedItems);
  finally
    Dec(FCollectionUpdateLock);
//    EndInternalCollectionUpdate();
  end;

  if Assigned(OnSelectionChanged) then
  begin
    OnSelectionChanged(Self);
  end;
end;

procedure TGridViewPresenter.DoSourceCollectionChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
begin
  case Action of
    caRemoved, caExtracted:
    begin
      FSelectedItems.Remove(Item);
      FCheckedItems.Remove(Item);
    end;
  end;
end;

function TGridViewPresenter.GetCurrentItem: TObject;
begin
  if Assigned(FGridView) and (View.ItemIndex > -1) then
  begin
    Result := FDataSource.GetRecordHandleByIndex(View.ItemIndex);
  end
  else
  begin
    Result := nil;
  end;
end;

function TGridViewPresenter.CalcCellContentHeight(
  AViewInfo: TcxGridTableDataCellViewInfo): Integer;
var
  LCanvas: TcxCanvas;
  LWidth: Integer;
  LRect: TRect;
  LText: string;
  LLines: TSTringList;
  LLine: string;
  LRows: Integer;
begin
  Result := 0;
  LWidth := AViewInfo.ClientBounds.Right - AViewInfo.ClientBounds.Left -
    AViewInfo.TextWidthWithOffset;
  if GetCellImageIndex(AViewInfo) > -1 then
  begin
    LWidth := LWidth - GetReservedImageWidth();
  end;

  LCanvas := TcxCellViewInfoAccessor(AViewInfo).Canvas;
  LRect := Rect(0, 0, LWidth, 50);
  LText := AViewInfo.Value;
  LLines := TStringList.Create();
  try
    cxGetTextLines(LText, LCanvas, LRect, LLines);
    for LLine in LLines do
    begin
      LRows := cxTextWidth(LCanvas.Font, LLine) div LWidth;
      if (cxTextWidth(LCanvas.Font, LLine) mod LWidth) > 0 then
      begin
        Inc(LRows);
      end;
      Inc(Result, LRows * cxTextHeight(LCanvas.Font));
    end;
    Result := Result + AViewInfo.TextHeightWithOffset;
  finally
    LLines.Free;
  end;
end;

function TGridViewPresenter.GetItemAtPosition(AGrid: TcxGridSite;
  X, Y: Integer): TObject;
var
  LHitTest: TcxCustomGridHitTest;
begin
  LHitTest := AGrid.ViewInfo.GetHitTest(X, Y);
  if LHitTest is TcxGridRecordHitTest then
  begin
    Result := GetRecordItem(TcxGridRecordHitTest(LHitTest).GridRecord);
  end
  else
  begin
    Result := nil;
  end;
end;

function TGridViewPresenter.GetAutoCellHeight: Boolean;
begin
  Result := (FGridView is TcxGridTableView)
    and TcxGridTableView(FGridView).OptionsView.CellAutoHeight;
end;

function TGridViewPresenter.GetCellImageIndex(
  AViewInfo: TcxGridTableDataCellViewInfo): Integer;
var
  LColumn: TcxGridColumn;
  LItem: TObject;
  LTemplate: IDataTemplate;
begin
  LColumn := TcxGridColumn(AViewInfo.Item);
  LItem := GetRecordItem(AViewInfo.GridRecord);
  LTemplate := View.ItemTemplate.GetItemTemplate(LItem);
  if Assigned(LTemplate) then
  begin
    Result := LTemplate.GetImageIndex(
      LItem, FDataSource.RelativeItemIndex[LColumn.Index]);
  end
  else
  begin
    Result := -1;
  end;
end;

function TGridViewPresenter.GetRecordItem(ARecord: TcxCustomGridRecord): TObject;
begin
  Result := FDataSource.RecordHandle[ARecord.RecordIndex];
end;

function TGridViewPresenter.GetReservedImageHeight: Integer;
begin
  Result := 0;
  if Assigned(ImageList) then
  begin
    Result := ImageList.Height + CIconSpacing * 2;
  end;
end;

function TGridViewPresenter.GetReservedImageWidth: Integer;
begin
  Result := 0;
  if Assigned(ImageList) then
  begin
    Result := ImageList.Width + CIconSpacing * 2;
  end;
end;

function TGridViewPresenter.GetSelectedItem: TObject;
begin
  if FSelectedItems.Count > 0 then
  begin
    Result := FSelectedItems[0];
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TGridViewPresenter.HandleCanFocusRecord(
  Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  var AAllow: Boolean);
var
  LItem: TObject;
begin
  if Sender.DataController.FocusedRecordIndex <> ARecord.RecordIndex then
  begin
    if Assigned(OnSelectionChanging) then
    begin
      LItem := GetRecordItem(ARecord);
      OnSelectionChanging(Sender, LItem, AAllow);
    end;
  end;
end;

procedure TGridViewPresenter.HandleDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  LDropMode: TDropMode;
  LHandled: Boolean;
  LSource: TObject;
begin
  LDropMode := dmBelow;
  LHandled := False;
  if Assigned(OnDragDrop) then
  begin
    LSource := Source;
    if LSource is TcxDragControlObject then
    begin
      LSource := TcxDragControlObject(LSource).Control;
    end;
    OnDragDrop(Sender, LSource, GetItemAtPosition(TcxGridSite(Sender), X, Y),
      doMove, LDropMode, LHandled);
  end;
end;

procedure TGridViewPresenter.HandleDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  LSource: TObject;
begin
  if Assigned(OnDragOver) then
  begin
    LSource := Source;
    if LSource is TcxDragControlObject then
    begin
      LSource := TcxDragControlObject(LSource).Control;
    end;
    OnDragOver(Sender, LSource, GetItemAtPosition(TcxGridSite(Sender), X, Y), Accept);
  end
  else
  begin
    Accept := True;
  end;
end;

procedure TGridViewPresenter.HandleDrawCell(Sender: TcxCustomGridTableView;
  ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
  var ADone: Boolean);
var
  LIndex: Integer;
  LPainter: TcxCellPainter;
  LEditView: TcxCustomTextEditViewInfo;
  LTop, LHeight: Integer;
begin
  if Assigned(ImageList) and (AViewInfo.Item is TcxGridColumn) then
  begin
    LPainter := TcxCellPainter(TcxCellViewInfoAccessor(
      AViewInfo).GetPainterClass.Create(ACanvas, AViewInfo));
    try
      // paint image if available
      LIndex := GetCellImageIndex(AViewInfo);
      if LIndex >= 0 then
      begin
        // adjust existing text
        if AViewInfo.EditViewInfo is TcxCustomTextEditViewInfo then
        begin
          LEditView := TcxCustomTextEditViewInfo(AViewInfo.EditViewInfo);
          LEditView.TextRect.Left := LEditView.TextRect.Left + ImageList.Width + CIconSpacing * 2;
        end;
        LPainter.DrawBackground();
        LPainter.DrawContent();
        // calculate Top-Offset to center Icon vertically
        LHeight := AViewInfo.Bounds.Bottom - AViewInfo.Bounds.Top;
        LTop := (LHeight - ImageList.Height) div 2;
        ImageList.Draw(ACanvas.Canvas, AViewInfo.Bounds.Left + CIconSpacing,
          AViewInfo.Bounds.Top + LTop, LIndex);
        ADone := True;
      end;
    finally
      LPainter.Free();
    end;
  end;
end;

procedure TGridViewPresenter.HandleEditValueChanged(
  Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem);
begin
  if AItem.Index = FDataSource.CheckColumnIndex then
  begin
    Sender.DataController.PostEditingData();
  end;
end;

procedure TGridViewPresenter.HandleFilterRecord(
  ADataController: TcxCustomDataController; ARecordIndex: Integer;
  var Accept: Boolean);
var
  LItem: TObject;
begin
  LItem := FDataSource.RecordHandle[ARecordIndex];
  DoFilterItem(LItem, Accept);
end;

procedure TGridViewPresenter.HandleGetCellHeight(Sender: TcxCustomGridTableView;
  ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
  ACellViewInfo: TcxGridTableDataCellViewInfo; var AHeight: Integer);
var
  LHeight, LContentHeight: Integer;
begin
  if Assigned(ImageList) then
  begin
    LHeight := GetReservedImageHeight();
    if AutoCellHeight then
    begin
      LContentHeight := CalcCellContentHeight(ACellViewInfo);
    end
    else
    begin
      LContentHeight := 0;
    end;

    if not AutoCellHeight or (LHeight > LContentHeight) then
    begin
      AHeight := LHeight;
    end
    else
    begin
      AHeight := LContentHeight;
    end;
  end;
end;

procedure TGridViewPresenter.HandleStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  LDragItem: TObject;
  LAllow: Boolean;
begin
  LDragItem := SelectedItem;
  LAllow := False;
  if Assigned(OnDragBegin) then
  begin
    OnDragBegin(Self, LDragItem, LAllow);
  end;
end;

procedure TGridViewPresenter.InitCardViewColumns(ACardView: TcxGridCardView);
var
  i: Integer;
  LRow: TcxGridCardViewRow;
  LColumnDefinition: TColumnDefinition;
begin
  ACardView.ClearItems();
  if FDataSource.CheckColumnIndex <> -1 then
  begin
    LRow := ACardView.CreateRow();
    LRow.Options.Moving := False;
    LRow.PropertiesClass := TcxCheckBoxProperties;
  end;
  for i := 0 to Pred(ColumnDefinitions.Count) do
  begin
    LColumnDefinition := ColumnDefinitions[i];
    LRow := ACardView.CreateRow();
    LRow.Caption := LColumnDefinition.Caption;
    LRow.Options.Editing := LColumnDefinition.AllowEdit;
    LRow.Visible := LColumnDefinition.Visible;

    // apply sort order
    case LColumnDefinition.SortingDirection of
      sdAscending: LRow.SortOrder := soAscending;
      sdDescending: LRow.SortOrder := soDescending;
    else
      LRow.SortOrder := soNone;
    end;

    // apply sizing etc options
    LRow.Options.Moving := coDraggable in LColumnDefinition.ColumnOptions;

    // apply column types
    case LColumnDefinition.ColumnType of
      ctCheckBox: LRow.PropertiesClass := TcxCheckBoxProperties;
      ctProgressBar:
      begin
        LRow.PropertiesClass := TcxProgressBarProperties;
        TcxProgressBarProperties(LRow.Properties).ShowText := False;
      end;
    end;
  end;
end;

procedure TGridViewPresenter.InitColumns;
begin
  if Assigned(FGridView) and UseColumnDefinitions then
  begin
    if Assigned(ColumnDefinitions) then
    begin
      if FGridView is TcxGridTableView then
      begin
        InitTableViewColumns(TcxGridTableView(FGridView));
      end else
      if FGridView is TcxGridCardView then
      begin
        InitCardViewColumns(TcxGridCardView(FGridView));
      end;
    end;
  end;
end;

procedure TGridViewPresenter.InitEvents;
var
  LTableView: TcxCustomGridTableView;
begin
  if Assigned(FGridView) then
  begin
    if FGridView is TcxCustomGridTableView then
    begin
      LTableView := TcxCustomGridTableView(FGridView);
      LTableView.DataController.OnFilterRecord := HandleFilterRecord;
      LTableView.OnCanFocusRecord := HandleCanFocusRecord;
      LTableView.OnCellDblClick := DoCellDblClick;
      LTableView.OnCustomDrawCell := HandleDrawCell;
      LTableView.OnDragOver := HandleDragOver;
      LTableView.OnDragDrop := HandleDragDrop;
      LTableView.OnEditValueChanged := HandleEditValueChanged;
      LTableView.OnFocusedRecordChanged := DoFocusedRecordChanged;
      if Assigned(ImageList) then
      begin
        LTableView.OnGetCellHeight := HandleGetCellHeight;
      end
      else
      begin
        LTableView.OnGetCellHeight := nil;
      end;
      LTableView.OnSelectionChanged := DoSelectionChanged;
      LTableView.OnStartDrag := HandleStartDrag;
    end;
  end;
end;


procedure TGridViewPresenter.InitProperties;
begin
  if Assigned(FGridView) then
  begin
    FGridView.PopupMenu := PopupMenu;
    FGridView.DataController.CustomDataSource := FDataSource;
    ApplyAllowMove();
    ApplySelectionMode();
    ApplyShowHeader();
  end;
end;


procedure TGridViewPresenter.InitTableViewColumns(ATableView: TcxGridTableView);
var
  i: Integer;
  LColumn: TcxGridColumn;
  LColumnDefinition: TColumnDefinition;
begin
  ATableView.ClearItems();
  if FDataSource.CheckColumnIndex > -1 then
  begin
    LColumn := ATableView.CreateColumn();
    LColumn.Options.Filtering := False;
    LColumn.Options.HorzSizing := False;
    LColumn.Options.Moving := False;
    LColumn.PropertiesClass := TcxCheckBoxProperties;
    LColumn.Width := 20;
  end;
  for i := 0 to Pred(ColumnDefinitions.Count) do
  begin
    LColumnDefinition := ColumnDefinitions[i];
    LColumn := ATableView.CreateColumn();
    LColumn.Caption := LColumnDefinition.Caption;
    LColumn.MinWidth := LColumnDefinition.MinWidth + GetReservedImageWidth();
    LColumn.Options.Editing := LColumnDefinition.AllowEdit;
    LColumn.Visible := LColumnDefinition.Visible;
    LColumn.Width := LColumnDefinition.Width + GetReservedImageWidth();

    // apply sort order
    case LColumnDefinition.SortingDirection of
      sdAscending: LColumn.SortOrder := soAscending;
      sdDescending: LColumn.SortOrder := soDescending;
    else
      LColumn.SortOrder := soNone;
    end;

    // apply sizing etc options
    LColumn.Options.HorzSizing := coResizable in LColumnDefinition.ColumnOptions;
    LColumn.Options.Moving := coDraggable in LColumnDefinition.ColumnOptions;
    LColumn.Options.Sorting := coSortable in LColumnDefinition.ColumnOptions;

    // apply column type
    case LColumnDefinition.ColumnType of
      ctCheckBox: LColumn.PropertiesClass := TcxCheckBoxProperties;
      ctProgressBar:
      begin
        LColumn.PropertiesClass := TcxProgressBarProperties;
        TcxProgressBarProperties(LColumn.Properties).ShowText := False;
      end;
    end;
  end;
end;

function TGridViewPresenter.IsCustomDataSourceSupported(
  AGridView: TcxCustomGridView): Boolean;
begin
  Result := Assigned(AGridView) and TcxProviderAccessor(TcxDataControllerAccessor(
    AGridView.DataController).Provider).IsCustomDataSourceSupported;
end;

procedure TGridViewPresenter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FGridView then
      FGridView := nil;
  end;
end;

procedure TGridViewPresenter.Refresh;
begin
  if Assigned(FGridView) and ([csLoading, csDesigning] * ComponentState = []) then
  begin
    FDataSource.DataChanged;
  end;
end;

procedure TGridViewPresenter.SetCurrentItem(const Value: TObject);
begin
  if Assigned(FGridView) then
  begin
    FDataSource.DataController.FocusedRecordIndex :=
      FDataSource.GetRecordIndexByHandle(Value);
  end;
end;

procedure TGridViewPresenter.SetGridView(const Value: TcxCustomGridView);
begin
  if IsCustomDataSourceSupported(Value) then
  begin
    if FGridView <> Value then
    begin
      if Assigned(FGridView) then
      begin
        FGridView.RemoveFreeNotification(Self);
      end;

      FGridView := Value;

      if Assigned(FGridView) then
      begin
        FGridView.FreeNotification(Self);
      end;
      InitControl();
    end;
  end
  else
  begin
    raise Exception.CreateFmt('GridView does not support custom data source: "%s".', [Value.ClassName]);
  end;
end;

end.
