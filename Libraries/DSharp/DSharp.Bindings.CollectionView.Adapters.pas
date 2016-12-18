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

unit DSharp.Bindings.CollectionView.Adapters;

interface

uses
  Classes,
  DSharp.Bindings.CollectionView,
  DSharp.Bindings.Notifications,
  Spring.Collections;

type
  TCollectionViewAdapter = class(TCollectionView)
  protected
    FOwner: TPersistent;
    function AddDisplayItem: NativeInt; virtual;
    procedure ClearDisplayItems; virtual;
    function FindDisplayItem(AItem: TObject): NativeInt; virtual;
    function GetDisplayItemsCount: NativeInt; virtual;
    procedure RemoveDisplayItem(AIndex: NativeInt); virtual;
    procedure UpdateDisplayItem(AIndex: NativeInt; AItem: TObject); virtual;

    procedure DoItemPropertyChanged(ASender: TObject; APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged); override;
    procedure DoSourceCollectionChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction); override;
    function GetOwner: TPersistent; override;
    procedure SetItemsSource(const Value: IObjectList); override;
    procedure UpdateItemIndex(ACurrentItem: TObject);
    procedure UpdateItems(AClearItems: Boolean = False); override;
  public
    constructor Create(AOwner: TPersistent);
  end;

  TCollectionViewStringsAdapter = class(TCollectionViewAdapter)
  private
    FItems: TStrings;
  protected
    function AddDisplayItem: NativeInt; override;
    procedure ClearDisplayItems; override;
    function FindDisplayItem(AItem: TObject): NativeInt; override;
    function GetDisplayItemsCount: NativeInt; override;
    procedure RemoveDisplayItem(AIndex: NativeInt); override;
    procedure UpdateDisplayItem(AIndex: NativeInt; AItem: TObject); override;

    function GetCurrentItem: TObject; override;
    procedure SetItemIndex(const Value: NativeInt); override;
    procedure UpdateItems(AClearItems: Boolean = False); override;
  public
    constructor Create(AOwner: TPersistent; AItems: TStrings);
    destructor Destroy; override;
  end;

implementation

uses
  DSharp.Core.DataTemplates,
  DSharp.Core.Reflection,
  Rtti,
  TypInfo;

{ TCollectionViewAdapter }

constructor TCollectionViewAdapter.Create(AOwner: TPersistent);
begin
  inherited Create();
  FOwner := AOwner;
end;

procedure TCollectionViewAdapter.DoItemPropertyChanged(ASender: TObject;
  APropertyName: string; AUpdateTrigger: TUpdateTrigger);
var
  LIndex: NativeInt;
begin
  LIndex := FindDisplayItem(ASender);

  if not IsFiltered(ASender) then
  begin
    if LIndex = -1 then
    begin
      LIndex := AddDisplayItem();
    end;

    if not Updating then
    begin
      UpdateDisplayItem(LIndex, ASender);
    end;
  end
  else
  begin
    if LIndex > -1 then
    begin
      RemoveDisplayItem(LIndex);
    end;
  end;

  NotifyPropertyChanged(FOwner, Self, 'View');
end;

function TCollectionViewAdapter.AddDisplayItem: NativeInt;
begin
  Result := -1;
end;

procedure TCollectionViewAdapter.ClearDisplayItems;
begin

end;

procedure TCollectionViewAdapter.DoSourceCollectionChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LIndex: NativeInt;
  LItem: TObject;
begin
  LItem := Item;

  if FItemsSource.ElementType.Kind = tkInterface then
    LItem := IInterface(Pointer(Item)) as TObject;

  inherited DoSourceCollectionChanged(Sender, LItem, Action);

  case Action of
    caAdded:
    begin
      if not IsFiltered(LItem) then
      begin
        LIndex := AddDisplayItem();
        UpdateDisplayItem(LIndex, LItem);
      end;
    end;
    caRemoved:
    begin
      LIndex := FindDisplayItem(LItem);
      if LIndex > -1 then
      begin
        RemoveDisplayItem(LIndex);
      end;
    end;
  end;

  NotifyPropertyChanged(FOwner, Self, 'View');
  FOnCollectionChanged.Invoke(FOwner, LItem, Action);
end;

function TCollectionViewAdapter.FindDisplayItem(AItem: TObject): NativeInt;
begin
  Result := -1;
end;

function TCollectionViewAdapter.GetDisplayItemsCount: NativeInt;
begin
  Result := 0;
end;

function TCollectionViewAdapter.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TCollectionViewAdapter.RemoveDisplayItem(AIndex: NativeInt);
begin

end;

procedure TCollectionViewAdapter.SetItemsSource(const Value: IObjectList);
begin
  inherited;
  NotifyPropertyChanged(FOwner, Self, 'View');
end;

procedure TCollectionViewAdapter.UpdateDisplayItem(AIndex: NativeInt;
  AItem: TObject);
begin

end;

procedure TCollectionViewAdapter.UpdateItemIndex(ACurrentItem: TObject);
begin
  if Assigned(ACurrentItem) then
  begin
    ItemIndex := FindDisplayItem(ACurrentItem);
  end
  else
  begin
    while not (FItemIndex < GetDisplayItemsCount) do
    begin
      Dec(FItemIndex);
    end;
    ItemIndex := FItemIndex;
  end;
end;

procedure TCollectionViewAdapter.UpdateItems(AClearItems: Boolean);
var
  i: Integer;
  LCurrentItem: TObject;
  LIndex: NativeInt;
  LItem: TValue;
begin
  LCurrentItem := CurrentItem;

  if AClearItems then
  begin
    ClearDisplayItems;
  end;

  if Assigned(FItemsSource) then
  begin
    for i := 0 to Pred(ItemTemplate.GetItemCount(ItemsSource as TObject)) do
    begin
      LItem := ItemTemplate.GetItem(ItemsSource as TObject, i);
      LIndex := FindDisplayItem(LItem.ToObject);
      if not IsFiltered(LItem.ToObject) then
      begin
        if LIndex = -1 then
        begin
          LIndex := AddDisplayItem();
        end;

        UpdateDisplayItem(LIndex, LItem.ToObject);
      end;
    end;
  end;

  UpdateItemIndex(LCurrentItem);

  NotifyPropertyChanged(FOwner, Self, 'View');
end;

{ TCollectionViewStringsAdapter }

constructor TCollectionViewStringsAdapter.Create(AOwner: TPersistent; AItems: TStrings);
begin
  inherited Create(AOwner);
  FItems := AItems;
  FOwner := AOwner;
end;

destructor TCollectionViewStringsAdapter.Destroy;
begin
  FItems := nil;
  inherited;
end;

function TCollectionViewStringsAdapter.AddDisplayItem: NativeInt;
begin
  Result := FItems.AddObject('', nil);
end;

procedure TCollectionViewStringsAdapter.ClearDisplayItems;
begin
  FItems.Clear();
end;

function TCollectionViewStringsAdapter.FindDisplayItem(AItem: TObject): NativeInt;
begin
  Result := FItems.IndexOfObject(AItem);
end;

function TCollectionViewStringsAdapter.GetCurrentItem: TObject;
begin
  if Assigned(FItems) and (FItemIndex > -1) then
  begin
    Result := FItems.Objects[FItemIndex];
  end
  else
  begin
    Result := nil;
  end;
end;

function TCollectionViewStringsAdapter.GetDisplayItemsCount: NativeInt;
begin
  Result := FItems.Count;
end;

procedure TCollectionViewStringsAdapter.RemoveDisplayItem(AIndex: NativeInt);
begin
  if AIndex = ItemIndex then
  begin
    FItems.Delete(AIndex);
    UpdateItemIndex(nil);
    NotifyPropertyChanged(FOwner, Self, 'View');
  end
  else
  begin
    FItems.Delete(AIndex);
  end;
end;

procedure TCollectionViewStringsAdapter.SetItemIndex(const Value: NativeInt);
var
  LProperty: TRttiProperty;
begin
  inherited;

  if FOwner.TryGetProperty('ItemIndex', LProperty) then
  begin
    LProperty.SetValue(FOwner, Value);
  end;

  NotifyPropertyChanged(FOwner, Self, 'View');
end;

procedure TCollectionViewStringsAdapter.UpdateDisplayItem(AIndex: NativeInt;
  AItem: TObject);
begin
  FItems[AIndex] := ItemTemplate.GetText(AItem, -1);
  FItems.Objects[AIndex] := AItem;
end;

procedure TCollectionViewStringsAdapter.UpdateItems(AClearItems: Boolean);
begin
  if Assigned(FItems) then
  begin
    inherited;
  end;
end;

end.
