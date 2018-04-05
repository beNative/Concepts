{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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
  Spring.Collections,
  Spring.Collections.Base;

type
  /// <summary>
  ///   Represents a first-in, first-out (FIFO) collection of items.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the type of elements in the queue.
  /// </typeparam>
  TQueue<T> = class(TEnumerableBase<T>, IQueue<T>, INotifyCollectionChanged<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fQueue: TQueue<T>;
        fIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const queue: TQueue<T>);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
      end;
      TArrayManager = TArrayManager<T>;
  private
    fHead: Integer;
    fTail: Integer;
    fCount: Integer;
    fItems: TArray<T>;
    fVersion: Integer;
    fOnChanged: ICollectionChangedEvent<T>;
    procedure Grow;
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer; override;
    function GetOnChanged: ICollectionChangedEvent<T>;
    procedure SetCapacity(const value: Integer);
  {$ENDREGION}

    procedure Changed(const item: T; action: TCollectionChangedAction);
    function DequeueInternal(notification: TCollectionChangedAction): T; virtual;
  public
    constructor Create; overload; override;
    constructor Create(const values: array of T); overload;
    constructor Create(const collection: IEnumerable<T>); overload;
    destructor Destroy; override;

    function GetEnumerator: IEnumerator<T>; override;

    procedure Clear;
    procedure Enqueue(const item: T);
    function Dequeue: T;
    function Extract: T;
    function Peek: T;
    function PeekOrDefault: T;
    function TryDequeue(out item: T): Boolean;
    function TryExtract(out item: T): Boolean;
    function TryPeek(out item: T): Boolean;

    procedure TrimExcess;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  TObjectQueue<T: class> = class(TQueue<T>, ICollectionOwnership)
  private
    fOwnsObjects: Boolean;
  {$REGION 'Property Accessors'}
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}
  protected
    function DequeueInternal(notification: TCollectionChangedAction): T; override;
  public
    constructor Create; override;
    constructor Create(ownsObjects: Boolean); overload;
    constructor Create(const comparer: IComparer<T>; ownsObjects: Boolean = True); overload;

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

implementation

uses
  Classes,
  RTLConsts,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Collections.Events,
  Spring.ResourceStrings;


{$REGION 'TQueue<T>'}

constructor TQueue<T>.Create;
begin
  inherited Create;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
end;

constructor TQueue<T>.Create(const values: array of T);
var
  i: Integer;
begin
  Create;
  for i := Low(values) to High(values) do
    Enqueue(values[i]);
end;

constructor TQueue<T>.Create(const collection: IEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in collection do
    Enqueue(item);
end;

destructor TQueue<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TQueue<T>.Changed(const item: T; action: TCollectionChangedAction);
begin
  if fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, action);
end;

procedure TQueue<T>.Clear;
begin
  while fCount > 0 do
    Dequeue;
  fHead := 0;
  fTail := 0;
  fCount := 0;
end;

function TQueue<T>.Dequeue: T;
begin
  Result := DequeueInternal(caRemoved);
end;

function TQueue<T>.DequeueInternal(notification: TCollectionChangedAction): T;
begin
  if fCount = 0 then
    raise EListError.CreateRes(@SUnbalancedOperation);
  Result := fItems[fTail];

  IncUnchecked(fVersion);
  fItems[fTail] := Default(T);
  fTail := (fTail + 1) mod Length(fItems);
  Dec(fCount);

  Changed(Result, notification);
end;

procedure TQueue<T>.Enqueue(const item: T);
begin
  if fCount = Length(fItems) then
    Grow;

  IncUnchecked(fVersion);
  fItems[fHead] := item;
  fHead := (fHead + 1) mod Length(fItems);
  Inc(fCount);

  Changed(item, caAdded);
end;

function TQueue<T>.Extract: T;
begin
  Result := DequeueInternal(caExtracted);
end;

function TQueue<T>.GetCapacity: Integer;
begin
  Result := Length(fItems);
end;

function TQueue<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TQueue<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TQueue<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

procedure TQueue<T>.Grow;
var
  newCapacity: Integer;
begin
  newCapacity := Length(fItems) * 2;
  if newCapacity = 0 then
    newCapacity := 4
  else if newCapacity < 0 then
    OutOfMemoryError;
  SetCapacity(newCapacity);
end;

function TQueue<T>.Peek: T;
begin
  if fCount = 0 then
    raise EListError.CreateRes(@SUnbalancedOperation);
  Result := fItems[fTail];
end;

function TQueue<T>.PeekOrDefault: T;
begin
  if fCount = 0 then
    Result := Default(T)
  else
    Result := fItems[fTail];
end;

procedure TQueue<T>.SetCapacity(const value: Integer);
var
  tailCount, offset: Integer;
begin
  offset := value - Length(fItems);
  if offset = 0 then
    Exit;

  if (fHead < fTail) or ((fHead = fTail) and (fCount > 0)) then
    tailCount := Length(fItems) - fTail
  else
    tailCount := 0;

  if offset > 0 then
    SetLength(fItems, value);
  if tailCount > 0 then
  begin
    TArrayManager.Move(fItems, fItems, fTail, fTail + offset, tailCount);
    if offset > 0 then
      TArrayManager.Finalize(fItems, fTail, offset)
    else
      if offset < 0 then
        TArrayManager.Finalize(fItems, fCount, -offset);
    Inc(fTail, offset);
  end
  else
    if fTail > 0 then
    begin
      if fCount > 0 then
      begin
        TArrayManager.Move(fItems, fItems, fTail, 0, fCount);
        TArrayManager.Finalize(fItems, fCount, fTail);
      end;
      Dec(fHead, fTail);
      fTail := 0;
    end;
  if offset < 0 then
  begin
    SetLength(fItems, value);
    if value = 0 then
      fHead := 0
    else
      fHead := fHead mod Length(fItems);
  end;
end;

procedure TQueue<T>.TrimExcess;
begin
  SetCapacity(fCount);
end;

function TQueue<T>.TryDequeue(out item: T): Boolean;
begin
  Result := fCount > 0;
  if Result then
    item := Dequeue
  else
    item := Default(T);
end;

function TQueue<T>.TryExtract(out item: T): Boolean;
begin
  Result := fCount > 0;
  if Result then
    item := Extract
  else
    item := Default(T);
end;

function TQueue<T>.TryPeek(out item: T): Boolean;
begin
  Result := fCount > 0;
  if Result then
    item := Peek
  else
    item := Default(T);
end;

{$ENDREGION}


{$REGION 'TQueue<T>.TEnumerator'}

constructor TQueue<T>.TEnumerator.Create(const queue: TQueue<T>);
begin
  inherited Create;
  fQueue := queue;
  fQueue._AddRef;
  fVersion := fQueue.fVersion;
end;

destructor TQueue<T>.TEnumerator.Destroy;
begin
  fQueue._Release;
  inherited Destroy;
end;

function TQueue<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TQueue<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if fVersion <> fQueue.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  if fIndex < fQueue.fCount then
  begin
    fCurrent := fQueue.fItems[(fQueue.fTail + fIndex) mod Length(fQueue.fItems)];
    Inc(fIndex);
    Result := True;
  end
  else
    fCurrent := Default(T);
end;

{$ENDREGION}


{$REGION 'TObjectQueue<T>'}

constructor TObjectQueue<T>.Create;
begin
  inherited Create;
  fOwnsObjects := True;
end;

constructor TObjectQueue<T>.Create(ownsObjects: Boolean);
begin
  inherited Create;
  fOwnsObjects := ownsObjects;
end;

constructor TObjectQueue<T>.Create(const comparer: IComparer<T>;
  ownsObjects: Boolean);
begin
  inherited Create(comparer);
  fOwnsObjects := ownsObjects;
end;

function TObjectQueue<T>.DequeueInternal(
  notification: TCollectionChangedAction): T;
begin
  Result := inherited DequeueInternal(notification);
  if fOwnsObjects and (notification = caRemoved) then
  begin
{$IFNDEF AUTOREFCOUNT}
    Result.Free;
{$ELSE}
    Result.DisposeOf;
{$ENDIF}
    Result := nil;
  end;
end;

function TObjectQueue<T>.GetOwnsObjects: Boolean;
begin
  Result := fOwnsObjects;
end;

procedure TObjectQueue<T>.SetOwnsObjects(const value: Boolean);
begin
  fOwnsObjects := value;
end;

{$ENDREGION}


end.
