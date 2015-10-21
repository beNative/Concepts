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

unit DSharp.DevExpress.PresenterDataSource;

interface

uses
  cxTLData,
  cxCustomData,
  DSharp.Bindings.CollectionView,
  DSharp.Windows.CustomPresenter,
  Spring.Collections;

type
  TGridViewPresenterDataSource = class(TcxCustomDataSource)
  private
    FCheckColumnIndex: Integer;
    FCheckedItems: IList<TObject>;
    FView: TCollectionView;
  protected
    function GetRelativeItemIndex(AIndex: Integer): Integer;

    // Load all records mode
    function GetRecordCount: Integer; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle; override;

    // for correct tracking when changing column order
    function GetItemHandle(AItemIndex: Integer): TcxDataItemHandle; override;

    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
  public
    constructor Create(const AView: TCollectionView; const ACheckedItems: IList<TObject>);

    property CheckColumnIndex: Integer read FCheckColumnIndex write FCheckColumnIndex;
    property RecordHandle[ARecordIndex: Integer]: TcxDataRecordHandle
      read GetRecordHandle;
    property RelativeItemIndex[AIndex: Integer]: Integer read GetRelativeItemIndex;
  end;

  TTreeListPresenterDataSource = class(TcxTreeListCustomDataSource)
  private
    FPresenter: TCustomPresenter;
  protected
    // Smart load mode (used in tree)
    function GetChildCount(AParentHandle: TcxDataRecordHandle): Integer; override;
    function GetChildRecordHandle(AParentHandle: TcxDataRecordHandle;
      AChildIndex: Integer): TcxDataRecordHandle; override;
    function GetRootRecordHandle: TcxDataRecordHandle; override;

    // Load all records mode
    function GetRecordCount: Integer; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle; override;

    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
  public
    constructor Create(APresenter: TCustomPresenter);
  end;

implementation

uses
  cxGridCustomTableView,
  DSharp.Bindings.Notifications,
  DSharp.Core.DataTemplates,
  Rtti,
  TypInfo,
  Variants;

{ TGridViewPresenterDataSource }

constructor TGridViewPresenterDataSource.Create(const AView: TCollectionView;
  const ACheckedItems: IList<TObject>);
begin
  inherited Create();
  FCheckColumnIndex := -1;
  FCheckedItems := ACheckedItems;
  FView := AView;
end;

function TGridViewPresenterDataSource.GetItemHandle(
  AItemIndex: Integer): TcxDataItemHandle;
var
  LGridColumn: TcxCustomGridTableItem;
begin
  LGridColumn := TcxCustomGridTableItem(DataController.GetItem(AItemIndex));
  Result := TcxDataItemHandle(LGridColumn.ID);
end;

function TGridViewPresenterDataSource.GetRecordCount: Integer;
begin
  Result := 0;
  if Assigned(FView.ItemsSource) then
  begin
    Result := FView.ItemTemplate.GetItemCount(FView.ItemsSource.AsObject);
  end;
end;

function TGridViewPresenterDataSource.GetRecordHandle(
  ARecordIndex: Integer): TcxDataRecordHandle;
begin
  Result := nil;
  if Assigned(FView.ItemsSource) then
  begin
    Result := FView.ItemTemplate.GetItem(FView.ItemsSource.AsObject, ARecordIndex);
  end;
end;

function TGridViewPresenterDataSource.GetRelativeItemIndex(
  AIndex: Integer): Integer;
begin
  if (FCheckColumnIndex > -1) and (AIndex >= FCheckColumnIndex) then
  begin
    Result := AIndex - 1;
  end
  else
  begin
    Result := AIndex;
  end;
end;

function TGridViewPresenterDataSource.GetValue(
  ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant;

  // TODO: implement in TValueHelper
  function ValueToVariant(AValue: TValue): Variant;
  begin
    case AValue.Kind of
      tkInteger: Result := AValue.AsInteger;
      tkChar: Result := AValue.AsString;
      tkEnumeration:
      begin
        if AValue.TypeInfo = TypeInfo(Boolean) then
        begin
          Result := AValue.AsBoolean;
        end
        else
        begin
          Result := AValue.AsOrdinal;
        end;
      end;
      tkFloat: Result := AValue.AsExtended;
      tkString: Result := AValue.AsString;
  //    tkSet: ;
  //    tkClass: Result := AValue.AsClass;
  //    tkMethod: ;
      tkWChar: Result := AValue.AsString;
      tkLString: Result := AValue.AsString;
      tkWString: Result := AValue.AsString;
  //    tkVariant: ;
  //    tkArray: ;
  //    tkRecord: ;
      tkInterface: Result := AValue.AsInterface;
      tkInt64: Result := AValue.AsInt64;
  //    tkDynArray: ;
      tkUString: Result := AValue.AsString;
  //    tkClassRef: ;
  //    tkPointer: Result := AValue.as;
  //    tkProcedure: ;
    else
      //may crash
      try
        Result := AValue.AsVariant;
      except
        Result := Null;
      end;
    end;
  end;

var
  LValue: TValue;
  LTemplate: IDataTemplate;
begin
  if Integer(AItemHandle) = FCheckColumnIndex then
  begin
    Result := FCheckedItems.Contains(ARecordHandle);
  end
  else
  begin
    LTemplate := FView.ItemTemplate.GetItemTemplate(ARecordHandle);
    if Assigned(LTemplate) then
    begin
      LValue := LTemplate.GetValue(ARecordHandle, GetRelativeItemIndex(Integer(AItemHandle)));
      Result := ValueToVariant(LValue);
    end
    else
    begin
      Result := Null;
    end;
  end;
end;

procedure TGridViewPresenterDataSource.SetValue(
  ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle;
  const AValue: Variant);
var
  LTemplate: IDataTemplate;
begin
  if Integer(AItemHandle) = FCheckColumnIndex then
  begin
    if AValue and not FCheckedItems.Contains(ARecordHandle) then
    begin
      FCheckedItems.Add(ARecordHandle);
    end
    else
    begin
      FCheckedItems.Remove(ARecordHandle);
    end;
  end
  else
  begin
    LTemplate := FView.ItemTemplate.GetItemTemplate(ARecordHandle);
    if Assigned(LTemplate) then
    begin
      LTemplate.SetValue(ARecordHandle, GetRelativeItemIndex(Integer(AItemHandle)), TValue.FromVariant(AValue));
    end;
  end;
end;

{ TTreeListPresenterDataSource }

//{$IFDEF USE_TREELIST}
constructor TTreeListPresenterDataSource.Create(APresenter: TCustomPresenter);
begin
  inherited Create();
  FPresenter := APresenter;
end;

function TTreeListPresenterDataSource.GetChildCount(
  AParentHandle: TcxDataRecordHandle): Integer;
var
  LItemTemplate: IDataTemplate;
begin
  Result := 0;

  LItemTemplate := FPresenter.GetItemTemplate(AParentHandle);
  if Assigned(LItemTemplate) then
  begin
    Result := LItemTemplate.GetItemCount(AParentHandle);
  end;
end;

function TTreeListPresenterDataSource.GetChildRecordHandle(AParentHandle: TcxDataRecordHandle;
  AChildIndex: Integer): TcxDataRecordHandle;
var
  LItemTemplate: IDataTemplate;
begin
  Result := nil;

  LItemTemplate := FPresenter.GetItemTemplate(AParentHandle);
  if Assigned(LItemTemplate) then
  begin
    Result := LItemTemplate.GetItem(AParentHandle, AChildIndex);
  end;
end;

function TTreeListPresenterDataSource.GetRecordCount: Integer;
begin
  Result := GetChildCount(GetRootRecordHandle);
end;

function TTreeListPresenterDataSource.GetRecordHandle(
  ARecordIndex: Integer): TcxDataRecordHandle;
begin
  Result := GetChildRecordHandle(GetRootRecordHandle, ARecordIndex);
end;

function TTreeListPresenterDataSource.GetRootRecordHandle: TcxDataRecordHandle;
begin
  Result := TcxDataRecordHandle(FPresenter.View.ItemsSource as TObject);
end;

function TTreeListPresenterDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var
  LItemTemplate: IDataTemplate;
begin
  LItemTemplate := FPresenter.GetItemTemplate(ARecordHandle);
  if Assigned(LItemTemplate) then
  begin
    Result := LItemTemplate.GetText(ARecordHandle, Integer(AItemHandle));
  end
  else
  begin
    Result := Null;
  end;
end;

procedure TTreeListPresenterDataSource.SetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle; const AValue: Variant);
var
  LItemTemplate: IDataTemplate;
begin
  LItemTemplate := FPresenter.GetItemTemplate(ARecordHandle);
  if Assigned(LItemTemplate) then
  begin
    LItemTemplate.SetValue(ARecordHandle, Integer(AItemHandle), TValue.FromVariant(AValue));
  end;
end;

end.
