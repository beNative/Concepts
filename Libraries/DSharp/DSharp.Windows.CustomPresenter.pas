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

unit DSharp.Windows.CustomPresenter;

interface

uses
  Classes,
  DSharp.Bindings.Collections,
  DSharp.Bindings.CollectionView,
  DSharp.Bindings.Notifications,
  DSharp.Core.DataTemplates,
  DSharp.Windows.ColumnDefinitions,
  DSharp.Windows.CustomPresenter.Types,
  Menus,
  Spring,
  Spring.Collections,
  SysUtils;

{$Include DSharp.Windows.CustomPresenter.Types.inc}
{$HINTS OFF}

type
  TCustomPresenter = class(TComponent, ICollectionViewNavigation,
    ICollectionView, INotifyPropertyChanged)
  private
    FAction: TBasicAction;
    FAllowMove: Boolean;
    FCheckSupport: TCheckSupport;
    FColumnDefinitions: IColumnDefinitions;
    FImageList: TCustomImageList;
    FNotifyPropertyChanged: TNotifyPropertyChanged;
    FOnDoubleClick: TNotifyEvent;
    FOnDragDrop: TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    FOnDragBegin: TDragBeginEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnSelectionChanging: TSelectionChangingEvent;
    FPopupMenu: TPopupMenu;
    FShowHeader: Boolean;
    FSelectionMode: TSelectionMode;
    FUseColumnDefinitions: Boolean;
    FView: TCollectionView;
    procedure DoColumnDefinitionsChanged(Sender: TObject; const Item: TColumnDefinition;
      Action: TCollectionNotification);
    procedure ReadColumnDefinitions(Reader: TReader);
    procedure SetAction(const Value: TBasicAction);
    procedure SetAllowMove(const Value: Boolean);
    procedure SetCheckSupport(const Value: TCheckSupport);
    procedure SetColumnDefinitions(const Value: IColumnDefinitions);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetSelectionMode(const Value: TSelectionMode);
    procedure SetShowHeader(const Value: Boolean);
    property NotifyPropertyChanged: TNotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
    procedure WriteColumnDefinitions(Writer: TWriter);
  protected
    FCollectionUpdateLock: Integer;
    FUpdateCount: Integer;
    procedure ApplyAllowMove; virtual;
    procedure ApplyCheckSupport; virtual;
    procedure ApplySelectionMode; virtual;
    procedure ApplyShowHeader; virtual;
    procedure DoFilterItem(const AItem: TObject; var AAccepted: Boolean); virtual;
    function GetCanMoveCurrentToNext: Boolean; virtual;
    function GetCanMoveCurrentToPrevious: Boolean; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoDblClick(Sender: TObject); virtual;
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure DoSourceCollectionChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction); virtual;
    function GetCurrentItem: TObject; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure InitColumns; virtual;
    procedure InitControl; virtual;
    procedure InitEvents; virtual;
    procedure InitProperties; virtual;
    procedure Loaded; override;
    procedure MoveCurrentToFirst; virtual;
    procedure MoveCurrentToLast; virtual;
    procedure MoveCurrentToNext; virtual;
    procedure MoveCurrentToPrevious; virtual;
    procedure SetCurrentItem(const Value: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyFilter; virtual;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    function GetItemTemplate(const Item: TObject): IDataTemplate; overload;
    procedure Refresh; virtual;

    property ColumnDefinitions: IColumnDefinitions
      read FColumnDefinitions write SetColumnDefinitions;
    property View: TCollectionView read FView
      implements ICollectionView, ICollectionViewNavigation;
  published
    property Action: TBasicAction read FAction write SetAction;
    property AllowMove: Boolean read FAllowMove write SetAllowMove default True;
    property CheckSupport: TCheckSupport read FCheckSupport write SetCheckSupport default csNone;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property OnDoubleClick: TNotifyEvent read FOnDoubleClick write FOnDoubleClick;
    property OnDragBegin: TDragBeginEvent read FOnDragBegin write FOnDragBegin;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnSelectionChanged: TNotifyEvent
      read FOnSelectionChanged write FOnSelectionChanged;
    property OnSelectionChanging: TSelectionChangingEvent
      read FOnSelectionChanging write FOnSelectionChanging;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property SelectionMode: TSelectionMode read FSelectionMode write SetSelectionMode default smSingle;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
    property UseColumnDefinitions: Boolean
      read FUseColumnDefinitions write FUseColumnDefinitions default True;
  end;

  TCollectionViewPresenterAdapter = class(TCollectionView)
  private
    FPresenter: TCustomPresenter;
  protected
    procedure DoFilterChanged(Sender: TObject); override;
    procedure DoItemPropertyChanged(ASender: TObject; APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged); override;
    procedure DoSourceCollectionChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction); override;
    function GetCanMoveCurrentToNext: Boolean; override;
    function GetCanMoveCurrentToPrevious: Boolean; override;
    function GetCurrentItem: TObject; override;
    procedure SetCurrentItem(const Value: TObject); override;
    procedure UpdateItems(AClearItems: Boolean = False); override;
  public
    constructor Create(Presenter: TCustomPresenter);

    procedure MoveCurrentToFirst; override;
    procedure MoveCurrentToLast; override;
    procedure MoveCurrentToNext; override;
    procedure MoveCurrentToPrevious; override;
  end;

  TPresenterColumnDefinitions = class(TColumnDefinitions);

implementation

uses
  DSharp.Windows.ColumnDefinitions.ControlTemplate;

{ TCustomPresenter }

constructor TCustomPresenter.Create(AOwner: TComponent);
begin
  inherited;
  FAllowMove := True;
  FShowHeader := True;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
  FView := TCollectionViewPresenterAdapter.Create(Self);

  FColumnDefinitions := TPresenterColumnDefinitions.Create(Self);
  FColumnDefinitions.OnNotify.Add(DoColumnDefinitionsChanged);
  FUseColumnDefinitions := True;
  FView.ItemTemplate := TColumnDefinitionsControlTemplate.Create(FColumnDefinitions);
end;

destructor TCustomPresenter.Destroy;
begin
  if Assigned(FColumnDefinitions) then
  begin
    FColumnDefinitions.OnNotify.Remove(DoColumnDefinitionsChanged);
  end;
  FView.Free();
  inherited;
end;

procedure TCustomPresenter.ApplyAllowMove;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.ApplyCheckSupport;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.ApplyFilter;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.ApplySelectionMode;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.ApplyShowHeader;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomPresenter.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ColumnDefinitions', ReadColumnDefinitions, WriteColumnDefinitions, True);
end;

procedure TCustomPresenter.DoColumnDefinitionsChanged(Sender: TObject;
  const Item: TColumnDefinition; Action: TCollectionNotification);
begin
  if not (csDesigning in ComponentState) then
  begin
    InitColumns();
  end;
end;

procedure TCustomPresenter.DoDblClick(Sender: TObject);
var
  LItemTemplate: IDataTemplate;
begin
  if Assigned(FOnDoubleClick) and Assigned(FAction)
    and not DelegatesEqual(@FOnDoubleClick, @FAction.OnExecute) then
  begin
    FOnDoubleClick(Self);
  end else
  if not (csDesigning in ComponentState) and Assigned(FAction) then
  begin
    FAction.Execute();
  end else
  if Assigned(FOnDoubleClick) then
  begin
    FOnDoubleClick(Self);
  end else
  begin
    LItemTemplate := GetItemTemplate(View.CurrentItem);
    if Assigned(LItemTemplate) and Assigned(LItemTemplate.Action) then
      LItemTemplate.Action.Execute;
  end;
end;

procedure TCustomPresenter.DoFilterItem(const AItem: TObject;
  var AAccepted: Boolean);
var
  i: Integer;
begin
//  View.Filter.Invoke(AItem, AAccepted);

  if Assigned(ColumnDefinitions) then
  begin
    for i := 0 to Pred(ColumnDefinitions.Count) do
    begin
      if Assigned(ColumnDefinitions[i].Filter) then
      begin
        AAccepted := AAccepted and not ColumnDefinitions[i].Filter(AItem);
      end;
    end;
  end;
end;

procedure TCustomPresenter.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FNotifyPropertyChanged.NotifyOfPropertyChange(APropertyName, AUpdateTrigger);
end;

procedure TCustomPresenter.DoSourceCollectionChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
begin
  if FUpdateCount = 0 then
  begin
    Refresh();
  end;
end;

procedure TCustomPresenter.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      Refresh;
    end;
  end;
end;

function TCustomPresenter.GetCanMoveCurrentToNext: Boolean;
begin
  Result := False; // implemented by descendants
end;

function TCustomPresenter.GetCanMoveCurrentToPrevious: Boolean;
begin
  Result := False; // implemented by descendants
end;

function TCustomPresenter.GetCurrentItem: TObject;
begin
  Result := nil; // implemented by descendants
end;

function TCustomPresenter.GetItemTemplate(const Item: TObject): IDataTemplate;
begin
  Result := nil;

  if Assigned(FView.ItemTemplate) then
  begin
    Result := FView.ItemTemplate.GetItemTemplate(Item);
  end
end;

procedure TCustomPresenter.InitColumns;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.InitControl;
begin
  if [csDesigning, csDestroying] * ComponentState = [] then
  begin
    InitColumns();
    InitEvents();
    InitProperties();
    Refresh();
  end;
end;

procedure TCustomPresenter.InitEvents;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.InitProperties;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    InitColumns();
    Refresh();
  end;
end;

procedure TCustomPresenter.MoveCurrentToFirst;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.MoveCurrentToLast;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.MoveCurrentToNext;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.MoveCurrentToPrevious;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FAction then
    begin
      SetAction(nil);
    end;
    if AComponent = FImageList then
    begin
      SetImageList(nil);
    end;
    if AComponent = FPopupMenu then
    begin
      SetPopupMenu(nil);
    end;
  end;
end;

procedure TCustomPresenter.ReadColumnDefinitions(Reader: TReader);
begin
  Reader.ReadValue();
  Reader.ReadCollection(FColumnDefinitions as TCollection);
end;

procedure TCustomPresenter.Refresh;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.SetAction(const Value: TBasicAction);
begin
  if FAction <> Value then
  begin
    if Assigned(FAction) then
    begin
      FAction.RemoveFreeNotification(Self);
    end;

    FAction := Value;

    if Assigned(FAction) then
    begin
      FAction.FreeNotification(Self);
    end;
  end;
end;

procedure TCustomPresenter.SetAllowMove(const Value: Boolean);
begin
  if FAllowMove <> Value then
  begin
    FAllowMove := Value;
    if not (csDesigning in ComponentState) then
      ApplyAllowMove();
  end;
end;

procedure TCustomPresenter.SetCheckSupport(const Value: TCheckSupport);
begin
  if FCheckSupport <> Value then
  begin
    FCheckSupport := Value;
    if not (csDesigning in ComponentState) then
      ApplyCheckSupport();
  end;
end;

procedure TCustomPresenter.SetColumnDefinitions(
  const Value: IColumnDefinitions);
begin
  if Assigned(FColumnDefinitions) then
  begin
    FColumnDefinitions.OnNotify.Remove(DoColumnDefinitionsChanged);
    if FColumnDefinitions.Owner = Self then
    begin
      if (FView.ItemTemplate is TColumnDefinitionsControlTemplate)
        and ((FView.ItemTemplate as TColumnDefinitionsControlTemplate).ColumnDefinitions = FColumnDefinitions) then
      begin
        (FView.ItemTemplate as TColumnDefinitionsControlTemplate).ColumnDefinitions := Value;
      end;
    end;
  end;
  FColumnDefinitions := Value;
  if Assigned(FColumnDefinitions) then
  begin
    FColumnDefinitions.OnNotify.Add(DoColumnDefinitionsChanged);
  end;
  InitColumns();
end;

procedure TCustomPresenter.SetCurrentItem(const Value: TObject);
begin
  DoPropertyChanged('View');
end;

procedure TCustomPresenter.SetImageList(const Value: TCustomImageList);
begin
  if FImageList <> Value then
  begin
    if Assigned(FImageList) then
    begin
      FImageList.RemoveFreeNotification(Self);
    end;

    FImageList := Value;

    if Assigned(FImageList) then
    begin
      FImageList.FreeNotification(Self);
    end;

    InitControl();
  end;
end;

procedure TCustomPresenter.SetPopupMenu(const Value: TPopupMenu);
begin
  if FPopupMenu <> Value then
  begin
    if Assigned(FPopupMenu) then
    begin
      FPopupMenu.RemoveFreeNotification(Self);
    end;

    FPopupMenu := Value;

    if Assigned(FPopupMenu) then
    begin
      FPopupMenu.FreeNotification(Self);
    end;

    InitControl();
  end;
end;

procedure TCustomPresenter.SetSelectionMode(const Value: TSelectionMode);
begin
  if FSelectionMode <> Value then
  begin
    FSelectionMode := Value;
    if not (csDesigning in ComponentState) then
      ApplySelectionMode();
  end;
end;

procedure TCustomPresenter.SetShowHeader(const Value: Boolean);
begin
  if FShowHeader <> Value then
  begin
    FShowHeader := Value;
    if not (csDesigning in ComponentState) then
      ApplyShowHeader();
  end;
end;

procedure TCustomPresenter.WriteColumnDefinitions(Writer: TWriter);
begin
  Writer.WriteCollection(FColumnDefinitions as TColumnDefinitions);
end;

{ TCollectionViewPresenterAdapter }

constructor TCollectionViewPresenterAdapter.Create(Presenter: TCustomPresenter);
begin
  inherited Create;
  FPresenter := Presenter;
end;

procedure TCollectionViewPresenterAdapter.DoFilterChanged(Sender: TObject);
begin
  FPresenter.ApplyFilter;

  NotifyPropertyChanged(FPresenter, Self, 'View');
end;

procedure TCollectionViewPresenterAdapter.DoItemPropertyChanged(
  ASender: TObject; APropertyName: string; AUpdateTrigger: TUpdateTrigger);
begin
  inherited;

  NotifyPropertyChanged(FPresenter, Self, 'View');
end;

procedure TCollectionViewPresenterAdapter.DoSourceCollectionChanged(
  Sender: TObject; const Item: TObject; Action: TCollectionChangedAction);
begin
  inherited;

  FPresenter.DoSourceCollectionChanged(Sender, Item, Action);

  NotifyPropertyChanged(FPresenter, Self, 'View');
end;

function TCollectionViewPresenterAdapter.GetCanMoveCurrentToNext: Boolean;
begin
  Result := FPresenter.GetCanMoveCurrentToNext;
end;

function TCollectionViewPresenterAdapter.GetCanMoveCurrentToPrevious: Boolean;
begin
  Result := FPresenter.GetCanMoveCurrentToPrevious;
end;

function TCollectionViewPresenterAdapter.GetCurrentItem: TObject;
begin
  Result := FPresenter.GetCurrentItem();
end;

procedure TCollectionViewPresenterAdapter.MoveCurrentToFirst;
begin
  FPresenter.MoveCurrentToFirst;
end;

procedure TCollectionViewPresenterAdapter.MoveCurrentToLast;
begin
  FPresenter.MoveCurrentToLast;
end;

procedure TCollectionViewPresenterAdapter.MoveCurrentToNext;
begin
  FPresenter.MoveCurrentToNext;
end;

procedure TCollectionViewPresenterAdapter.MoveCurrentToPrevious;
begin
  FPresenter.MoveCurrentToPrevious;
end;

procedure TCollectionViewPresenterAdapter.SetCurrentItem(const Value: TObject);
begin
  FPresenter.SetCurrentItem(Value);
end;

procedure TCollectionViewPresenterAdapter.UpdateItems(AClearItems: Boolean);
begin
  inherited;

  if not (csDestroying in FPresenter.ComponentState) then
  begin
    FPresenter.Refresh();

    NotifyPropertyChanged(FPresenter, Self, 'View');
  end;
end;

end.
