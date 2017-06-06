{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Data.IndexList;

interface

uses
  Spring.Collections;

type
  TIndexList = class
  private type
    TIndexItem = record
      Index: Integer;
      Item: TObject;
    end;
    TComparison = function(const left, right: TObject): Integer of object;
  private
    fDataList: IObjectList;
    fIndexes: IList<TIndexItem>;
    fIsChanging: Boolean;
    function GetCount: Integer;
    function GetIndex(index: Integer): TIndexItem;
    function GetItem(index: Integer): TObject;
    procedure SetDataList(const value: IObjectList);
    procedure SetItem(index: Integer; const value: TObject);

    procedure FixIndexes(startIndex: Integer);
  public
    constructor Create;

    function AddIndex(index: Integer): Integer;
    procedure DeleteIndex(index: Integer);

    function AddItem(const item: TObject): Integer;
    procedure DeleteItem(index: Integer);
    procedure InsertItem(const item: TObject; index: Integer);

    function Contains(const item: TObject): Boolean;
    function IndexOf(const item: TObject): Integer;

    procedure Clear;
    procedure Rebuild;

    procedure InsertionSort(startIndex: Integer; const comparer: TComparison);
    procedure MergeSort(const comparer: TComparison);

    property Count: Integer read GetCount;
    property DataList: IObjectList read fDataList write SetDataList;
    property Indexes[index: Integer]: TIndexItem read GetIndex;
    property IsChanging: Boolean read fIsChanging;
    property Items[index: Integer]: TObject read GetItem write SetItem; default;
  end;

implementation


{$REGION 'TIndexList'}

constructor TIndexList.Create;
begin
  inherited Create;
  fIndexes := TCollections.CreateList<TIndexItem>;
end;

function TIndexList.AddIndex(index: Integer): Integer;
var
  indexItem: TIndexItem;
begin
  indexItem.Index := index;
  indexItem.Item := fDataList[index];
  Result := fIndexes.Add(indexItem);
end;

function TIndexList.AddItem(const item: TObject): Integer;
begin
  fIsChanging := True;
  try
    Result := AddIndex(fDataList.Add(item));
  finally
    fIsChanging := False;
  end;
end;

procedure TIndexList.Clear;
begin
  fIndexes.Clear;
end;

function TIndexList.Contains(const item: TObject): Boolean;
begin
  Result := IndexOf(item) <> -1;
end;

procedure TIndexList.DeleteIndex(index: Integer);
begin
  fIndexes.Delete(index);
end;

procedure TIndexList.DeleteItem(index: Integer);
var
  fixIndex: Integer;
begin
  fixIndex := fIndexes[index].Index;
  fIsChanging := True;
  try
    fDataList.Delete(fixIndex);
    DeleteIndex(index);
    FixIndexes(fixIndex);
  finally
    fIsChanging := False;
  end;
end;

procedure TIndexList.FixIndexes(startIndex: Integer);
var
  i: Integer;
  indexItem: TIndexItem;
begin
  for i := 0 to Count - 1 do
  begin
    indexItem := fIndexes[i];
    if indexItem.Index > startIndex then
    begin
      Dec(indexItem.Index);
      fIndexes[i] := indexItem;
    end;
  end;
end;

function TIndexList.GetCount: Integer;
begin
  Result := fIndexes.Count;
end;

function TIndexList.GetIndex(index: Integer): TIndexItem;
begin
  Result := fIndexes[index];
end;

function TIndexList.GetItem(index: Integer): TObject;
begin
  Result := fIndexes[index].Item;
end;

function TIndexList.IndexOf(const item: TObject): Integer;
begin
  if item = nil then
    Exit(-1);

  for Result := 0 to Count - 1 do
    if Items[Result] = item then
      Exit;
  Result := -1;
end;

procedure TIndexList.InsertItem(const item: TObject; index: Integer);
var
  indexItem: TIndexItem;
begin
  fIsChanging := True;
  try
    indexItem.Index := fDataList.Add(item);
    indexItem.Item := item;
    fIndexes.Insert(index, indexItem);
  finally
    fIsChanging := False;
  end;
end;

procedure TIndexList.InsertionSort(startIndex: Integer;
  const comparer: TComparison);
var
  i, j : Integer;
  temp: TObject;
begin
  startIndex := startIndex - 1;
  if startIndex < 0 then
    startIndex := 0;

  for i := startIndex + 1 to Count - 1 Do
  begin
    temp := Items[i];
    j := i;
    while (j > 0) and (comparer(Items[j - 1], temp) > 0) do
    begin
      Items[j] := Items[j - 1];
      Dec(j);
    end;
    Items[j] := temp;
  end;
end;

procedure TIndexList.MergeSort(const comparer: TComparison);
var
  cache: TArray<TObject>;

  procedure Merge(low, mid, high: Integer);
  var
    i, j, k: Integer;
  begin
    for i := low to high do
      cache[i] := Items[i];
    i := low;
    j := mid + 1;
    k := low;
    while (i <= mid) and (j <= high) do
    begin
      if comparer(cache[i], cache[j]) <= 0 then
      begin
        Items[k] := cache[i];
        Inc(i);
      end
      else
      begin
        Items[k] := cache[j];
        Inc(j);
      end;
      Inc(k);
    end;

    while i <= mid do
    begin
      Items[k] := cache[i];
      Inc(k);
      Inc(i);
    end;
  end;

  procedure PerformMergeSort(low, high: Integer);
  var
    mid: Integer;
  begin
    if low < high then
    begin
      mid := (high + low) div 2;
      PerformMergeSort(low, mid);
      PerformMergeSort(mid + 1, high);
      Merge(low, mid, high);
    end;
  end;

begin
  SetLength(cache, Count);
  PerformMergeSort(0, Count - 1);
end;

procedure TIndexList.Rebuild;
var
  i: Integer;
begin
  Clear;
  if Assigned(fDataList) then
    for i := 0 to fDataList.Count - 1 do
      AddIndex(i);
end;

procedure TIndexList.SetDataList(const value: IObjectList);
begin
  fDataList := value;
  Rebuild;
end;

procedure TIndexList.SetItem(index: Integer; const value: TObject);
var
  indexItem: TIndexItem;
begin
  indexItem := fIndexes[index];
  indexItem.Item := value;
  fIndexes[index] := indexItem;
end;

{$ENDREGION}


end.
