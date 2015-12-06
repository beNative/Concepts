{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2015 Spring4D Team                           }
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

unit Spring.Persistence.ObjectDataset.Algorithms.Sort;

interface

uses
  Spring.Collections,
  Spring.Reflection,
  Spring.Persistence.ObjectDataset.IndexList,
  Rtti,
  DB
  ;

const
  MIN_MERGE = 32;
  MIN_GALLOP = 7;
  INITIAL_TMP_STORAGE_LENGTH = 256;
  STACK_LENGTH = 85;

type
  TIndexFieldInfo = record
    Field: TField;
    RttiProperty: TRttiProperty;
    Descending: Boolean;
    CaseInsensitive: Boolean;
  end;

  TCompareRecords = function(const Item1, Item2: TValue; AIndexFieldList: IList<TIndexFieldInfo>): Integer of object;

  TTimSort = class sealed
  private
    class var
      FMinGallop: Integer;
      stackSize: Integer;  // Number of pending runs on stack
      FDataList: IList;
      FComparator: TCompareRecords;
      FIndexFields: IList<TIndexFieldInfo>;
      tmp: TArray<TValue>;
      runBase: TArray<Integer>;
      runLen: TArray<Integer>;
  private
    class procedure ArrayCopy(SrcPos, DestPos, ALength: Integer);
    class procedure ArrayCopyToTemp(SrcPos, DestPos, ALength: Integer);
    class procedure ArrayCopyFromTemp(SrcPos, DestPos, ALength: Integer);
    class procedure RangeCheck(AArrayLength, AFromIndex, AToIndex: Integer);
    class function CountRunAndMakeAscending(Lo, Hi: Integer): Integer;
    class procedure ReverseRange(ALo, AHi: Integer);
    class procedure BinarySort(Lo, Hi, Start: Integer);
    class function gallopLeft(const key: TValue; base, len, hint: Integer; ATmp: Boolean = False): Integer;
    class function gallopRight(const key: TValue; base, len, hint: Integer; ATmp: Boolean = False): Integer;

    class procedure MergeCollapse;
    class procedure mergeForceCollapse;
    class procedure MergeAt(i: Integer);
    class procedure mergeLo(base1, len1, base2, len2: Integer);
    class procedure mergeHi(base1, len1, base2, len2: Integer);
    class function MinRunLength(n: Integer): Integer;
    class procedure PushRun(ARunBase, ARunLen: Integer);
  public
    class procedure Sort(ADataList: IList; AComparator: TCompareRecords; AIndexFields: IList<TIndexFieldInfo>);
  end;

  TQuickSort = class sealed
  private
    class var
      FDataList: IList;
      FFilteredIndexes: IList<Integer>;
      FFiltered: Boolean;
  private
    class procedure QuickSort(ALow, AHigh: Integer; Compare: TCompareRecords; AIndexFieldList: IList<TIndexFieldInfo>);
  public
    class procedure Sort(ADataList: IList; AComparator: TCompareRecords; AIndexFields: IList<TIndexFieldInfo>
      ; AFilteredIndexes: IList<Integer>; AFiltered: Boolean);
  end;

  TMergeSort = class sealed
  private
    class var
      FDataList: TODIndexList;
  private
    class procedure MergeSort(ALow, AHigh: Integer; Compare: TCompareRecords; AIndexFieldList: IList<TIndexFieldInfo>);
  public
    class procedure Sort(ADataList: TODIndexList; AComparator: TCompareRecords; AIndexFields: IList<TIndexFieldInfo>);
  end;

  TInsertionSort = class sealed
  private
    class var
      FDataList: TODIndexList;
  private
    class procedure InsertionSort(ALow, AHigh: Integer; Compare: TCompareRecords; AIndexFieldList: IList<TIndexFieldInfo>);
  public
    class procedure Sort(AStartIndex: Integer; ADataList: TODIndexList; AComparator: TCompareRecords; AIndexFields: IList<TIndexFieldInfo>);
  end;

  TBinaryInsertionSort = class sealed
  private
    class var
      FDataList: IList;
      FFilteredIndexes: IList<Integer>;
      FFiltered: Boolean;
  private
    class function BinarySearch(ALow, AHigh: Integer; const AKey: TValue; Compare: TCompareRecords; AIndexFieldList: IList<TIndexFieldInfo>): Integer;
    class procedure BinaryInsertionSort(ALow, AHigh: Integer; Compare: TCompareRecords; AIndexFieldList: IList<TIndexFieldInfo>);
  public
    class procedure Sort(ADataList: IList; AComparator: TCompareRecords; AIndexFields: IList<TIndexFieldInfo>
      ; AFilteredIndexes: IList<Integer>; AFiltered: Boolean);
  end;

implementation

uses
  SysUtils
  ;




{ TTimSort }

class procedure TTimSort.ArrayCopy(SrcPos, DestPos, ALength: Integer);
var
  i: Integer;
begin
  for i := ALength downto 1 do
  begin
    FDataList[DestPos + i] := FDataList[SrcPos + i];
  end;
end;

class procedure TTimSort.ArrayCopyFromTemp(SrcPos, DestPos, ALength: Integer);
var
  i: Integer;
begin
  for i := ALength downto 1 do
  begin
    FDataList[DestPos + i] := tmp[SrcPos + i];
  end;
end;

class procedure TTimSort.ArrayCopyToTemp(SrcPos, DestPos, ALength: Integer);
var
  i: Integer;
begin
  for i := ALength downto 1 do
  begin
    tmp[DestPos + i] := FDataList[SrcPos + i];
  end;
end;

class procedure TTimSort.BinarySort(Lo, Hi, Start: Integer);
var
  i, left, right, mid, n: Integer;
  pivot: TValue;
begin
  Assert((Lo <= Start) and (Start <= Hi));
  if Start = Lo then
    Inc(Start);

  for i := Start to Hi - 1 do
  begin
    Start := i;

    pivot := FDataList[Start];
    left := Lo;
    right := Start;
    Assert(left <= right);

    while (left < right) do
    begin
      mid := (left + right) shr 1;
      if (FComparator(pivot, FDataList[mid], FIndexFields) < 0) then
        right := mid
      else
        left := mid + 1;
    end;

    Assert(left = right);

    n := Start - left;
    ArrayCopy(left, left + 1, n);
    FDataList[left] := pivot;
  end;
end;

class function TTimSort.CountRunAndMakeAscending(Lo, Hi: Integer): Integer;
var
  runHi: Integer;
begin
  Assert(Lo < Hi);
  runHi := Lo + 1;
  if runHi = Hi then
    Exit(1);

  Inc(runHi);

  if (FComparator(FDataList[runHi], FDataList[Lo], FIndexFields) < 0) then
  begin
    while (runHi < Hi) and (FComparator(FDataList[runHi], FDataList[runHi - 1], FIndexFields) < 0) do
    begin
      Inc(runHi);
    end;
    ReverseRange(Lo, runHi);
  end
  else
  begin
    while (runHi < Hi) and (FComparator(FDataList[runHi], FDataList[runHi - 1], FIndexFields) >= 0) do
    begin
      Inc(runHi);
    end;
  end;

  Result := runHi - Lo;
end;

class function TTimSort.gallopLeft(const key: TValue; base, len, hint: Integer; ATmp: Boolean): Integer;
var
  lastOfs, ofs, maxOfs, Ltmp, m: Integer;
  LValue: TValue;
begin
  Assert( (len > 0) and (hint >= 0) and (hint < len) );
  lastOfs := 0;
  ofs := 1;

  if ATmp then
    LValue := tmp[base + hint]
  else
    LValue := FDataList[base + hint];

  if (FComparator(key, LValue, FIndexFields) > 0) then
  begin
    maxOfs := len - hint;

    if ATmp then
      LValue := tmp[base + hint + ofs]
    else
      LValue := FDataList[base + hint + ofs];

    while (ofs < maxofs) and (FComparator(key, LValue, FIndexFields) > 0) do
    begin
      lastOfs := ofs;
      ofs := (ofs shl 1) + 1;
      if (ofs <= 0) then
        ofs := maxOfs;
    end;

    if (ofs > maxOfs) then
      ofs := maxOfs;

    lastOfs := lastOfs + hint;
    ofs := ofs + hint;
  end
  else
  begin
    maxOfs := hint + 1;

    if ATmp then
      LValue := tmp[base + hint - ofs]
    else
      LValue := FDataList[base + hint - ofs];

    while (ofs < maxofs) and (FComparator(key, LValue, FIndexFields) <= 0) do
    begin
      lastOfs := ofs;
      ofs := (ofs shl 1) + 1;
      if (ofs <= 0) then
        ofs := maxOfs;
    end;

    if (ofs > maxOfs) then
      ofs := maxOfs;

    Ltmp := lastOfs;
    lastOfs := hint - ofs;
    ofs := hint - Ltmp;
  end;
  Assert( (-1 <= lastOfs) and ( lastOfs < ofs) and ( ofs <= len) );

  Inc(lastOfs);

  while (lastOfs < ofs) do
  begin
    m := lastOfs + ((ofs - lastOfs) shr 1);

    if ATmp then
      LValue := tmp[base + m]
    else
      LValue := FDataList[base + m];

    if (FComparator(key, LValue, FIndexFields) > 0) then
      lastOfs := m + 1
    else
      ofs := m;
  end;

  Assert( lastOfs = ofs);    // so a[base + ofs - 1] < key <= a[base + ofs]
  Result := ofs;
end;

class function TTimSort.gallopRight(const key: TValue; base, len, hint: Integer; ATmp: Boolean): Integer;
var
  lastOfs, ofs, maxOfs, Ltmp, m: Integer;
  LValue: TValue;
begin
  Assert( (len > 0) and (hint >= 0) and (hint < len));
  ofs := 1;
  lastOfs := 0;

  if ATmp then
    LValue := tmp[base + hint]
  else
    LValue := FDataList[base + hint];

  if (FComparator(key, LValue, FIndexFields) < 0) then
  begin
    maxOfs := hint + 1;

    if ATmp then
      LValue := tmp[base + hint - ofs]
    else
      LValue := FDataList[base + hint - ofs];

    while (ofs < maxOfs) and (FComparator(key, LValue, FIndexFields) < 0) do
    begin
      lastOfs := ofs;
      ofs := (ofs shl 1) + 1;
      if (ofs <= 0) then
        ofs := maxOfs;
    end;

    if (ofs > maxOfs) then
      ofs := maxOfs;

    Ltmp := lastOfs;
    lastOfs := hint - ofs;
    ofs := hint - Ltmp;
  end
  else
  begin
    maxOfs := len - hint;
    if ATmp then
      LValue := tmp[base + hint + ofs]
    else
      LValue := FDataList[base + hint + ofs];

    while (ofs < maxOfs) and (FComparator(key, LValue, FIndexFields) >= 0) do
    begin
      lastOfs := ofs;
      ofs := (ofs shl 1) + 1;
      if (ofs <= 0) then
        ofs := maxOfs;
    end;
    if (ofs > maxOfs) then
      ofs := maxOfs;

    lastOfs := lastOfs + hint;
    ofs := ofs + hint;
  end;
  Assert( (-1 <= lastOfs) and (lastOfs < ofs) and (ofs <= len));

  Inc(lastOfs);

  while (lastOfs < ofs) do
  begin
    m := lastOfs + ((ofs - lastOfs) shr 1);
    if ATmp then
      LValue := tmp[base + m]
    else
      LValue := FDataList[base + m];

    if (FComparator(key, LValue, FIndexFields) < 0) then
      ofs := m
    else
      lastOfs := m + 1;
  end;
  Assert( lastOfs = ofs);    // so a[b + ofs - 1] <= key < a[b + ofs]
  Result := ofs;
end;

class procedure TTimSort.MergeAt(i: Integer);
var
  base1, base2, len1, len2, k: Integer;
begin
  Assert(StackSize >= 2);
  Assert(i >= 0);
  Assert((i = stackSize - 2) or (i = stackSize - 3));

  base1 := runBase[i];
  len1 := runLen[i];
  base2 := runBase[i + 1];
  len2 := runLen[i + 1];
  Assert((len1 > 0) and (len2 > 0));
  Assert( base1 + len1 = base2);

  runLen[i] := len1 + len2;
  if (i = stackSize - 3) then
  begin
    runBase[i + 1] := runBase[i + 2];
    runLen[i + 1] := runLen[i + 2];
  end;
  Dec(stackSize);

  k := gallopRight(FDataList[base2], base1, len1, 0);

  Assert(k >= 0);
  base1 := base1 + k;
  len1 := len1 - k;
  if len1 = 0 then
    Exit;

  len2 := gallopLeft(FDataList[base1 + len1 - 1], base2, len2, len2 - 1);
  Assert(len2 >= 0);
  if len2 = 0 then
    Exit;

  if (len1 <= len2) then
    mergeLo(base1, len1, base2, len2)
  else
    mergeHi(base1, len1, base2, len2);

end;

class procedure TTimSort.MergeCollapse;
var
  n: Integer;
begin
  while (stackSize > 1) do
  begin
    n := stackSize - 2;
    if (n > 0) and (runLen[n - 1] <= runLen[n] + runLen[n+1]) then
    begin
      if (runLen[n-1] < runLen[n+1]) then
      begin
        Dec(n);
      end;
      MergeAt(n);
    end
    else if (runLen[n] <= runLen[n+1]) then
    begin
      MergeAt(n);
    end
    else
    begin
      Break;
    end;
  end;
end;

class procedure TTimSort.mergeForceCollapse;
var
  n: Integer;
begin
  while (stackSize > 1)  do
  begin
    n := stackSize - 2;
    if (n > 0) and (runLen[n - 1] < runLen[n + 1]) then
    begin
      Dec(n);
    end;
    MergeAt(n);
  end;
end;

class procedure TTimSort.mergeHi(base1, len1, base2, len2: Integer);
var
  cursor1, cursor2, dest, minGallop, count1, count2: Integer;
  label outer;
begin
  Assert( (len1 > 0) and (len2 > 0) and (base1 + len1 = base2));
  ArrayCopyToTemp(base2, 0, len2);
  cursor1 := base1 + len1 - 1;
  cursor2 := len2 - 1;
  dest := base2 + len2 - 1;
  Dec(dest);
  Dec(cursor1);
  FDataList[dest] := FDataList[cursor1];
  Dec(len1);
  if (len1 = 0) then
  begin
    ArrayCopyFromTemp(0, dest - (len2 - 1), len2);
    Exit;
  end;
  if (len2 = 1) then
  begin
    dest := dest - len1;
    cursor1 := cursor1 - len1;
    ArrayCopy(cursor1 + 1, dest + 1, len1);
    FDataList[dest] := tmp[cursor2];
    Exit;
  end;

  minGallop := FMinGallop;

  outer:
  begin
    while (True) do
    begin
      count1 := 0;
      count2 := 0;

      repeat
        Assert( (len1 > 0) and (len2 > 1));

        if (FComparator(tmp[cursor2], FDataList[cursor1], FIndexFields) < 0) then
        begin
          Dec(dest);
          Dec(cursor1);
          FDataList[dest] := FDataList[cursor1];
          Inc(count1);
          count2 := 0;
          Dec(len1);
          if len1 = 0 then
            goto outer;
        end
        else
        begin
          Dec(dest);
          Dec(cursor2);
          FDataList[dest] := tmp[cursor2];
          Inc(count2);
          count1 := 0;
          Dec(len2);
          if len2 = 1 then
            goto outer;
        end;
      until (((count1 or count2) < minGallop));
    end;


    repeat
      Assert( (len1 > 0) and (len2 > 1));
      count1 := len1 - gallopRight(tmp[cursor2], base1, len1, len1 - 1);
      if count1 <> 0 then
      begin
        dest := dest - count1;
        cursor1 := cursor1 - count1;
        len1 := len1 - count1;
        ArrayCopy(cursor1 + 1, dest + 1, count1);
        if len1 = 0 then
          goto outer;
      end;
      Dec(dest);
      Dec(cursor2);
      FDataList[dest] := tmp[cursor2];
      Dec(len2);
      if (len2 = 1) then
        goto outer;

      count2 := len2 - gallopLeft(FDataList[cursor1], 0, len2, len2 - 1, True);
      if (count2 <> 0) then
      begin
        dest := dest - count2;
        cursor2 := cursor2 - count2;
        len2 := len2 - count2;
        ArrayCopyFromTemp(cursor2 + 1, dest + 1, count2);
        if len2 <= 1 then
          goto outer;
      end;

      Dec(dest);
      Dec(cursor1);

      FDataList[dest] := FDataList[cursor1];
      Dec(len1);
      if (len1 = 0) then
        goto outer;

      Dec(minGallop);

    until (((count1 >= MIN_GALLOP) or (count2 >= MIN_GALLOP)));

    if (minGallop < 0) then
      minGallop := 0;

    minGallop := minGallop + 2;
  end;

  if minGallop < 1 then
    FMinGallop := 1
  else
    FMinGallop := minGallop;

  if (len1 = 1) then
  begin
    Assert( len2 > 0);
    dest := dest - len1;
    cursor1 := cursor1 - len1;
    ArrayCopy(cursor1 + 1, dest + 1, len1);
    FDataList[dest] := tmp[cursor2];
  end
  else if len2 = 0 then
  begin
    raise EArgumentOutOfRangeException.Create('Comparison method violates its general contract!');
  end
  else
  begin
    assert( len1 = 0);
    assert( len2 > 0);
    ArrayCopyFromTemp(0, dest - (len2 - 1), len2);
  end;
end;



class procedure TTimSort.mergeLo(base1, len1, base2, len2: Integer);
var
  cursor1, cursor2, dest, minGallop, count1, count2: Integer;
  label outer;
begin
  Assert( (len1 > 0) and (len2 > 0) and (base1 + len1 = base2));
  ArrayCopyToTemp(base1, 0, len1);
  cursor1 := 0;
  cursor2 := base2;
  dest := base1;
  Inc(dest);
  Inc(cursor2);
  FDataList[dest] := FDataList[cursor2];
  Dec(len2);
  if (len2 = 0) then
  begin
    ArrayCopyFromTemp(cursor1, dest, len1);
    Exit;
  end;
  if (len1 = 1) then
  begin
    ArrayCopy(cursor2, dest, len2);
    FDataList[dest + len2] := tmp[cursor1];
    Exit;
  end;

  minGallop := FMinGallop;

  outer:
  begin
    while (True) do
    begin
      count1 := 0;
      count2 := 0;

      repeat
        Assert( (len1 > 1) and (len2 > 0));

        if (FComparator(FDataList[cursor2], tmp[cursor1], FIndexFields) < 0) then
        begin
          Inc(dest);
          Inc(cursor2);
          FDataList[dest] := FDataList[cursor2];
          Inc(count2);
          count1 := 0;
          Dec(len2);
          if len2 = 0 then
            goto outer;
        end
        else
        begin
          Inc(dest);
          Inc(cursor1);
          FDataList[dest] := tmp[cursor1];
          Inc(count1);
          count2 := 0;
          Dec(len1);
          if len1 = 1 then
            goto outer;
        end;
      until (((count1 or count2) < minGallop));
    end;


    repeat
      Assert( (len1 > 1) and (len2 > 0));
      count1 := gallopRight(FDataList[cursor2], cursor1, len1, 0, True);
      if count1 <> 0 then
      begin
        ArrayCopyFromTemp(cursor1, dest, count1);
        dest := dest + count1;
        cursor1 := cursor1 + count1;
        len1 := len1 - count1;
        if (len1 <= 1) then // len1 == 1 || len1 == 0
            goto outer;
      end;
      Inc(dest);
      Inc(cursor2);
      FDataList[dest] := FDataList[cursor2];
      Dec(len2);
      if (len2 = 0) then
        goto outer;

      count2 := gallopLeft(tmp[cursor1], cursor2, len2, 0);
      if (count2 <> 0) then
      begin
        ArrayCopy(cursor2, dest, count2);
        dest := dest + count2;
        cursor2 := cursor2 + count2;
        len2 := len2 - count2;
        if len2 = 0 then
          goto outer;
      end;

      Inc(dest);
      Inc(cursor1);

      FDataList[dest] := tmp[cursor1];
      Dec(len1);
      if (len1 = 1) then
        goto outer;

      Dec(minGallop);

    until (((count1 >= MIN_GALLOP) or (count2 >= MIN_GALLOP)));

    if (minGallop < 0) then
      minGallop := 0;

    minGallop := minGallop + 2;
  end;

  if minGallop < 1 then
    FMinGallop := 1
  else
    FMinGallop := minGallop;

  if (len1 = 1) then
  begin
    Assert( len2 > 0);
    ArrayCopy(cursor2, dest, len2);
    FDataList[dest + len2] := tmp[cursor1];
  end
  else if len1 = 0 then
  begin
    raise EArgumentOutOfRangeException.Create('Comparison method violates its general contract!');
  end
  else
  begin
    assert( len2 = 0);
    assert( len1 > 1);
    ArrayCopyFromTemp(cursor1, dest, len1);
  end;
end;

class function TTimSort.MinRunLength(n: Integer): Integer;
var
  r: Integer;
begin
  Assert(n >= 0);

  r := 0;
  while (n >= MIN_MERGE) do
  begin
    r := r or (n and 1);
    n := n shr 1;
  end;
  Result := n + r;
end;

class procedure TTimSort.PushRun(ARunBase, ARunLen: Integer);
begin
  runBase[stackSize] := ARunBase;
  runLen[stackSize] := ARunLen;
  Inc(stackSize);
end;

class procedure TTimSort.RangeCheck(AArrayLength, AFromIndex, AToIndex: Integer);
begin
  if AFromIndex > AToIndex then
    raise EArgumentOutOfRangeException.Create('fromIndex(' + IntToStr(AFromIndex) +
      ') > toIndex(' + IntToStr(AToIndex)+')');

  if AFromIndex < 0 then
    raise EArgumentOutOfRangeException.Create('From index less than 0');

  if AToIndex > AArrayLength then
    raise EArgumentOutOfRangeException.Create('AToIndex > AArrayLength');
end;

class procedure TTimSort.ReverseRange(ALo, AHi: Integer);
var
  LItem: TValue;
begin
  Dec(AHi);
  while (ALo < AHi) do
  begin
    LItem := FDataList[ALo];
    Inc(ALo);
    FDataList[ALo] := FDataList[AHi];
    Dec(AHi);
    FDataList[AHi] := LItem;
  end;
end;

class procedure TTimSort.Sort(ADataList: IList; AComparator: TCompareRecords; AIndexFields: IList<TIndexFieldInfo>);
var
  nRemaining, initRunLen, minRun, LrunLen, lo, hi, force: Integer;
begin
  FDataList := ADataList;
  FComparator := AComparator;
  FIndexFields := AIndexFields;
  FMinGallop := MIN_GALLOP;
  stackSize := 0;

  lo := 0;
  hi := FDataList.Count - 1;

  SetLength(tmp, FDataList.Count);  //initialize to max capacity
  SetLength(runBase, STACK_LENGTH);
  SetLength(runLen, STACK_LENGTH);

  RangeCheck(FDataList.Count, lo, hi);

  nRemaining := hi - lo;
  if nRemaining < 2 then
    Exit;

  if nRemaining < MIN_MERGE then
  begin
    initRunLen := CountRunAndMakeAscending(lo, hi);
    BinarySort(lo, hi, lo + initRunLen);
    Exit;
  end;

  minRun := MinRunLength(nRemaining);
  repeat
    LrunLen := countRunAndMakeAscending(lo, hi);
    if (LrunLen < minRun) then
    begin
      if nRemaining <= minRun then
        force := nRemaining
      else
        force := minRun;

      BinarySort(lo, lo + force, lo + LrunLen);
      LrunLen := force;

      PushRun(lo, LrunLen);

      MergeCollapse;
      lo := lo + LrunLen;
      nRemaining := nRemaining - LrunLen;
    end;
  until (nRemaining <> 0);

  Assert( lo = hi);
  mergeForceCollapse;
  Assert(stackSize = 1);
end;

{ TQuickSort }

class procedure TQuickSort.QuickSort(ALow, AHigh: Integer; Compare: TCompareRecords;
  AIndexFieldList: IList<TIndexFieldInfo>);
var
  LLow, LHigh: Integer;
  iPivot: Integer;
  LPivot: TValue;
begin
  if (FDataList.Count = 0) or ( (AHigh - ALow) <= 0 ) then
    Exit;
  repeat
    LLow := ALow;
    LHigh := AHigh;
    iPivot := (ALow + (AHigh - ALow) shr 1);
    LPivot := FDataList[iPivot];

    repeat
      while Compare(FDataList[LLow], LPivot, AIndexFieldList) < 0 do
        Inc(LLow);

      while Compare(FDataList[LHigh], LPivot, AIndexFieldList) > 0 do
        Dec(LHigh);

      if LLow <= LHigh then
      begin
        if LLow <> LHigh then
        begin
          FDataList.Exchange(LLow, LHigh);
          if FFiltered then
            FFilteredIndexes.Exchange(LLow, LHigh);
        end;

        Inc(LLow);
        Dec(LHigh);
      end;
    until LLow > LHigh;

    if ALow < LHigh then
      QuickSort(ALow, LHigh, Compare, AIndexFieldList);
    ALow := LLow;
  until LLow >= AHigh;

end;

class procedure TQuickSort.Sort(ADataList: IList; AComparator: TCompareRecords; AIndexFields: IList<TIndexFieldInfo>
  ; AFilteredIndexes: IList<Integer>; AFiltered: Boolean);
begin
  FDataList := ADataList;
  FFilteredIndexes := AFilteredIndexes;
  FFiltered := AFiltered;
  QuickSort(0, ADataList.Count - 1, AComparator, AIndexFields);
end;

{ TMergeSort }

class procedure TMergeSort.MergeSort(ALow, AHigh: Integer; Compare: TCompareRecords;
  AIndexFieldList: IList<TIndexFieldInfo>);
var
  LCache: TArray<TValue>;

  procedure Merge(Low, Mid, High: Integer);
  var
    i, j, k: Integer;
  begin
    for i := Low to High do
    begin
      LCache[i] := FDataList.GetModel(i);
    end;
    i := Low;
    j := Mid + 1;
    k := Low;
    while (i <= Mid) and (j <= High) do
    begin
      if (Compare(LCache[i], LCache[j], AIndexFieldList) <= 0) then
      begin
        FDataList.SetModel(k, LCache[i]);
        Inc(i);
      end
      else
      begin
        FDataList.SetModel(k, LCache[j]);
        Inc(j);
      end;
      Inc(k);
    end;

    while (i <= Mid) do
    begin
      FDataList.SetModel(k, LCache[i]);
      Inc(k);
      Inc(i);
    end;
  end;

  procedure PerformMergeSort(ALowIndex, AHighIndex: Integer; CompareMethod: TCompareRecords;
    AIndexFields: IList<TIndexFieldInfo>);
  var
    iMid: Integer;
  begin
    if ALowIndex < AHighIndex then
    begin
      iMid:= (AHighIndex + ALowIndex) div 2;
      PerformMergeSort( ALowIndex, iMid, CompareMethod, AIndexFields );
      PerformMergeSort( iMid+1, AHighIndex, CompareMethod, AIndexFields );
      Merge(ALowIndex, iMid, AHighIndex);
    end;
  end;

begin
  SetLength(LCache, FDataList.Count);
  PerformMergeSort(ALow, AHigh, Compare, AIndexFieldList);
end;

class procedure TMergeSort.Sort(ADataList: TODIndexList; AComparator: TCompareRecords; AIndexFields: IList<TIndexFieldInfo>);
begin
  FDataList := ADataList;
  MergeSort(0, FDataList.Count - 1, AComparator, AIndexFields);
end;

{ TInsertionSort }

class procedure TInsertionSort.InsertionSort(ALow, AHigh: Integer; Compare: TCompareRecords;
  AIndexFieldList: IList<TIndexFieldInfo>);
var
  i, j : Integer;
  LTemp: TValue;
Begin
  for i:= ALow + 1 to AHigh Do
  begin
    LTemp := FDataList.GetModel(i);
    j := i;
    while (j > 0) and (Compare(FDataList.GetModel(j-1), LTemp, AIndexFieldList) > 0) do
    begin
      FDataList.SetModel(j, FDataList.GetModel(j-1));
      Dec(j);
    end;
    FDataList.SetModel(j, LTemp);
  End;
end;

class procedure TInsertionSort.Sort(AStartIndex: Integer; ADataList: TODIndexList; AComparator: TCompareRecords;
  AIndexFields: IList<TIndexFieldInfo>);
begin
  FDataList := ADataList;
  AStartIndex := AStartIndex - 1;
  if AStartIndex < 0 then
    AStartIndex := 0;
  InsertionSort(AStartIndex, FDataList.Count - 1, AComparator, AIndexFields);
end;

{ TBinaryInsertionSort }

class procedure TBinaryInsertionSort.BinaryInsertionSort(ALow, AHigh: Integer; Compare: TCompareRecords;
  AIndexFieldList: IList<TIndexFieldInfo>);
var
  i, j, ins: Integer;
  LTemp: TValue;
begin
  for i := 1 to AHigh do
  begin
    ins := BinarySearch(0, i, FDataList[i], Compare, AIndexFieldList);
    LTemp := FDataList[i];

    for j := i downto ins + 1 do
    begin
      FDataList[j + 1] := FDataList[j];
    end;

    FDataList[ins] := LTemp;
  end;
end;

class function TBinaryInsertionSort.BinarySearch(ALow, AHigh: Integer; const AKey: TValue; Compare: TCompareRecords; AIndexFieldList: IList<TIndexFieldInfo>): Integer;
var
  iMid: Integer;
begin
  if ALow = AHigh then
    Exit(ALow);

  iMid := ALow + ((AHigh - ALow) div 2);
  if (Compare(AKey, FDataList[iMid], AIndexFieldList) > 0) then
  begin
    Result := BinarySearch(iMid + 1, AHigh, AKey, Compare, AIndexFieldList);
  end
  else if (Compare(AKey, FDataList[iMid], AIndexFieldList) < 0) then
  begin
    Result := BinarySearch(ALow, iMid, AKey, Compare, AIndexFieldList);
  end
  else
  begin
    Result := iMid;
  end;
end;

class procedure TBinaryInsertionSort.Sort(ADataList: IList; AComparator: TCompareRecords;
  AIndexFields: IList<TIndexFieldInfo>; AFilteredIndexes: IList<Integer>; AFiltered: Boolean);
begin
  FDataList := ADataList;
  FFilteredIndexes := AFilteredIndexes;
  FFiltered := AFiltered;

  BinaryInsertionSort(0, FDataList.Count - 1, AComparator, AIndexFields);
end;

end.
