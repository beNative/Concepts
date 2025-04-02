{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2024 Spring4D Team                           }
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

unit Spring.Collections.Queues;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Events;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  TAbstractQueue<T> = class abstract(TCircularArrayBuffer<T>)
  public
  {$REGION 'Implements IQueue<T>'}
    function Dequeue: T;
    function Extract: T;
    function Peek: T;
    function PeekOrDefault: T;
    function TryDequeue(var item: T): Boolean;
    function TryExtract(var item: T): Boolean;
    function TryPeek(var item: T): Boolean;
  {$ENDREGION}
  end;

  /// <summary>
  ///   Represents a first-in, first-out collection of items which grows
  ///   automatically.
  /// </summary>
  TQueue<T> = class(TAbstractQueue<T>, IInterface, IEnumerable<T>, IQueue<T>)
  private
    procedure Grow;
  public
    constructor Create(const values: array of T); overload;
    constructor Create(const values: IEnumerable<T>); overload;

  {$REGION 'Implements IQueue<T>'}
    function Enqueue(const item: T): Boolean;
  {$ENDREGION}
  end;

  /// <summary>
  ///   Represents a first-in, first-out collection of items which has a fixed
  ///   size and prevents adding items when full.
  /// </summary>
  TBoundedQueue<T> = class(TAbstractQueue<T>, IInterface, IEnumerable<T>, IQueue<T>)
  public
  {$REGION 'Implements IQueue<T>'}
    function Enqueue(const item: T): Boolean;
  {$ENDREGION}
  end;

  /// <summary>
  ///   Represents a first-in, first-out collection of items which has a fixed
  ///   size and removes the oldest item when full.
  /// </summary>
  TEvictingQueue<T> = class(TAbstractQueue<T>, IInterface, IEnumerable<T>, IQueue<T>)
  public
  {$REGION 'Implements IQueue<T>'}
    function Enqueue(const item: T): Boolean;
  {$ENDREGION}
  end;

  TAbstractDeque<T> = class(TCircularArrayBuffer<T>)
  public
  {$REGION 'Implements IDeque<T>'}
    function RemoveFirst: T;
    function RemoveLast: T;
    function ExtractFirst: T;
    function ExtractLast: T;

    function TryRemoveFirst(var item: T): Boolean;
    function TryRemoveLast(var item: T): Boolean;
    function TryExtractFirst(var item: T): Boolean;
    function TryExtractLast(var item: T): Boolean;
  {$ENDREGION}
  end;

  /// <summary>
  ///   Represents a double-ended queue of items which grows automatically.
  /// </summary>
  TDeque<T> = class(TAbstractDeque<T>, IInterface, IEnumerable<T>, IDeque<T>)
  private
    procedure Grow;
  public
    constructor Create(const values: array of T); overload;
    constructor Create(const values: IEnumerable<T>); overload;

  {$REGION 'Implements IDeque<T>'}
    function AddFirst(const item: T): Boolean;
    function AddLast(const item: T): Boolean;
  {$ENDREGION}
  end;

  /// <summary>
  ///   Represents a double-ended queue of items which has a fixed size and
  ///   prevents adding items when full.
  /// </summary>
  TBoundedDeque<T> = class(TAbstractDeque<T>, IInterface, IEnumerable<T>, IDeque<T>)
  public
  {$REGION 'Implements IDeque<T>'}
    function AddFirst(const item: T): Boolean;
    function AddLast(const item: T): Boolean;
  {$ENDREGION}
  end;

  /// <summary>
  ///   Represents a double-ended queue of items which has a fixed size and
  ///   and removes items from the opposite end when full.
  /// </summary>
  TEvictingDeque<T> = class(TAbstractDeque<T>, IInterface, IEnumerable<T>, IDeque<T>)
  public
  {$REGION 'Implements IDeque<T>'}
    function AddFirst(const item: T): Boolean;
    function AddLast(const item: T): Boolean;
  {$ENDREGION}
  end;

  TFoldedQueue<T> = class(TQueue<T>)
  private
    fElementType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(const elementType: PTypeInfo;
      const comparer: IComparer<T>; ownsObjects: Boolean = False);
  end;

implementation

uses
  TypInfo,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TAbstractQueue<T>'}

function TAbstractQueue<T>.Dequeue: T; //FI:W521
begin
  if Count > 0 then
  begin
    if OwnsObjects then
      Result := Default(T)
    else
      Result := Items[Head];
    DeleteFromHead(caRemoved);
  end
  else
  begin
    RaiseHelper.NoElements;
    __SuppressWarning(Result);
  end;
end;

function TAbstractQueue<T>.Extract: T; //FI:W521
begin
  if Count > 0 then
  begin
    Result := Items[Head];
    DeleteFromHead(caExtracted);
  end
  else
  begin
    RaiseHelper.NoElements;
    __SuppressWarning(Result);
  end;
end;

function TAbstractQueue<T>.Peek: T; //FI:W521
begin
  if Count > 0 then
    Exit(Items[Head]);
  RaiseHelper.NoElements;
  __SuppressWarning(Result);
end;

function TAbstractQueue<T>.PeekOrDefault: T;
begin
  if Count > 0 then
    Result := Items[Head]
  else
    Result := Default(T);
end;

function TAbstractQueue<T>.TryDequeue(var item: T): Boolean;
begin
  if Count > 0 then
  begin
    if OwnsObjects then
      item := Default(T)
    else
      item := Items[Head];
    DeleteFromHead(caRemoved);
    Exit(True);
  end;
  item := Default(T);
  Result := False;
end;

function TAbstractQueue<T>.TryExtract(var item: T): Boolean;
begin
  if Count > 0 then
  begin
    item := Items[Head];
    DeleteFromHead(caExtracted);
    Exit(True);
  end;
  item := Default(T);
  Result := False;
end;

function TAbstractQueue<T>.TryPeek(var item: T): Boolean;
begin
  if Count > 0 then
  begin
    item := Items[Head];
    Exit(True);
  end;
  item := Default(T);
  Result := False;
end;

{$ENDREGION}


{$REGION 'TQueue<T>'}

constructor TQueue<T>.Create(const values: array of T);
var
  i: Integer;
begin
  SetCapacity(Length(values));
  for i := 0 to High(values) do
    Enqueue(values[i]);
end;

constructor TQueue<T>.Create(const values: IEnumerable<T>);
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    Enqueue(item);
  end;
end;

function TQueue<T>.Enqueue(const item: T): Boolean;
begin
  if Count = Capacity then
    Grow;
  AddToTail(item);
  Result := True;
end;

procedure TQueue<T>.Grow;
begin
  SetCapacity(GrowCapacity(Capacity));
end;

{$ENDREGION}


{$REGION 'TBoundedQueue<T>'}

function TBoundedQueue<T>.Enqueue(const item: T): Boolean;
begin
  if Count <> Capacity then
  begin
    AddToTail(item);
    Exit(True);
  end;
  Result := False;
end;

{$ENDREGION}


{$REGION 'TEvictingQueue<T>'}

function TEvictingQueue<T>.Enqueue(const item: T): Boolean;
begin
  if Count = Capacity then
    DeleteFromHead(caRemoved);
  AddToTail(item);
  Result := True;
end;

{$ENDREGION}


{$REGION 'TAbstractDeque<T>'}

function TAbstractDeque<T>.RemoveFirst: T; //FI:W521
begin
  if Count > 0 then
  begin
    if OwnsObjects then
      Result := Default(T)
    else
      Result := Items[Head];
    DeleteFromHead(caRemoved);
  end
  else
  begin
    RaiseHelper.NoElements;
    __SuppressWarning(Result);
  end;
end;

function TAbstractDeque<T>.RemoveLast: T; //FI:W521
begin
  if Count > 0 then
  begin
    if OwnsObjects then
      Result := Default(T)
    else
      Result := Items[Tail];
    DeleteFromTail(caRemoved);
  end
  else
  begin
    RaiseHelper.NoElements;
    __SuppressWarning(Result);
  end;
end;

function TAbstractDeque<T>.ExtractFirst: T; //FI:W521
begin
  if Count > 0 then
  begin
    Result := Items[Head];
    DeleteFromHead(caExtracted);
  end
  else
  begin
    RaiseHelper.NoElements;
    __SuppressWarning(Result);
  end;
end;

function TAbstractDeque<T>.ExtractLast: T; //FI:W521
begin
  if Count > 0 then
  begin
    Result := Items[Tail];
    DeleteFromTail(caExtracted);
  end
  else
  begin
    RaiseHelper.NoElements;
    __SuppressWarning(Result);
  end;
end;

function TAbstractDeque<T>.TryRemoveFirst(var item: T): Boolean;
begin
  if Count > 0 then
  begin
    if OwnsObjects then
      item := Default(T)
    else
      item := Items[Head];
    DeleteFromHead(caRemoved);
    Exit(True);
  end;
  item := Default(T);
  Result := False;
end;

function TAbstractDeque<T>.TryRemoveLast(var item: T): Boolean;
begin
  if Count > 0 then
  begin
    if OwnsObjects then
      item := Default(T)
    else
      item := Items[Tail];
    DeleteFromTail(caRemoved);
    Exit(True);
  end;
  item := Default(T);
  Result := False;
end;

function TAbstractDeque<T>.TryExtractFirst(var item: T): Boolean;
begin
  if Count > 0 then
  begin
    item := Items[Head];
    DeleteFromHead(caExtracted);
    Exit(True);
  end;
  item := Default(T);
  Result := False;
end;

function TAbstractDeque<T>.TryExtractLast(var item: T): Boolean;
begin
  if Count > 0 then
  begin
    item := Items[Tail];
    DeleteFromTail(caExtracted);
    Exit(True);
  end;
  item := Default(T);
  Result := False;
end;

{$ENDREGION}


{$REGION 'TDeque<T>'}

constructor TDeque<T>.Create(const values: array of T);
var
  i: Integer;
begin
  SetCapacity(Length(values));
  for i := 0 to High(values) do
    AddLast(values[i]);
end;

constructor TDeque<T>.Create(const values: IEnumerable<T>);
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    AddLast(item);
  end;
end;

function TDeque<T>.AddFirst(const item: T): Boolean;
begin
  if Count = Capacity then
    Grow;
  AddToHead(item);
  Result := True;
end;

function TDeque<T>.AddLast(const item: T): Boolean;
begin
  if Count = Capacity then
    Grow;
  AddToTail(item);
  Result := True;
end;

procedure TDeque<T>.Grow;
begin
  SetCapacity(GrowCapacity(Capacity));
end;

{$ENDREGION}


{$REGION 'TBoundedDeque<T>'}

function TBoundedDeque<T>.AddFirst(const item: T): Boolean;
begin
  if Count = Capacity then
    Exit(False);
  AddToHead(item);
  Result := True;
end;

function TBoundedDeque<T>.AddLast(const item: T): Boolean;
begin
  if Count = Capacity then
    Exit(False);
  AddToTail(item);
  Result := True;
end;

{$ENDREGION}


{$REGION 'TEvictingDeque<T>'}

function TEvictingDeque<T>.AddFirst(const item: T): Boolean;
begin
  if Count = Capacity then
    DeleteFromTail(caRemoved);
  AddToHead(item);
  Result := True;
end;

function TEvictingDeque<T>.AddLast(const item: T): Boolean;
begin
  if Count = Capacity then
    DeleteFromHead(caRemoved);
  AddToTail(item);
  Result := True;
end;

{$ENDREGION}


{$REGION 'TFoldedQueue<T>'}

constructor TFoldedQueue<T>.Create(const elementType: PTypeInfo;
  const comparer: IComparer<T>; ownsObjects: Boolean);
begin
  fComparer := comparer;
  fElementType := elementType;
  SetOwnsObjects(ownsObjects);
end;

function TFoldedQueue<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


end.
