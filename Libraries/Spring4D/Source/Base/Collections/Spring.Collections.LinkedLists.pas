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

unit Spring.Collections.LinkedLists;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type

  /// <summary>
  ///   Represents a doubly linked list.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the element type of the linked list.
  /// </typeparam>
  TLinkedList<T> = class(TCollectionBase<T>, IEnumerable<T>,
    ICollection<T>, IReadOnlyCollection<T>, ILinkedList<T>)
  private
    type
      TEnumerator = class(TRefCountedObject, IEnumerator<T>)
      private
        fList: TLinkedList<T>;
        fVersion: Integer;
        fNode: TLinkedListNode<T>;
        fCurrent: T;
        function GetCurrent: T;
      public
        constructor Create(const list: TLinkedList<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
  private
    fFirstFree: TLinkedListNode<T>;
    fCount: Integer;
    fVersion: Integer;
    function EnsureNode(const value: T): TLinkedListNode<T>;
    procedure InternalInsertNodeBefore(const node: TLinkedListNode<T>;
      const newNode: TLinkedListNode<T>);
    procedure InternalInsertNodeToEmptyList(const newNode: TLinkedListNode<T>);
    procedure InternalRemoveNode(const node: TLinkedListNode<T>);
    procedure InvalidateNode(const node: TLinkedListNode<T>);
    procedure ValidateNewNode(const node: TLinkedListNode<T>);
    procedure ValidateNode(const node: TLinkedListNode<T>);
  protected
    fHead: TLinkedListNode<T>;
  {$REGION 'Property Accessors'}
    function GetCount: Integer; 
    function GetFirst: TLinkedListNode<T>;
    function GetLast: TLinkedListNode<T>;
    function GetNonEnumeratedCount: Integer;
  {$ENDREGION}
    function TryGetFirst(var value: T): Boolean; overload;
    function TryGetLast(var value: T): Boolean; overload;
  public
    procedure BeforeDestruction; override;

    function GetEnumerator: IEnumerator<T>;

    function Add(const item: T): Boolean;
    function Remove(const item: T): Boolean; overload;
    function Extract(const item: T): T;

    procedure AddAfter(const node: TLinkedListNode<T>; const newNode: TLinkedListNode<T>); overload;
    function AddAfter(const node: TLinkedListNode<T>; const value: T): TLinkedListNode<T>; overload;
    procedure AddBefore(const node: TLinkedListNode<T>; const newNode: TLinkedListNode<T>); overload;
    function AddBefore(const node: TLinkedListNode<T>; const value: T): TLinkedListNode<T>; overload;
    procedure AddFirst(const node: TLinkedListNode<T>); overload;
    function AddFirst(const value: T): TLinkedListNode<T>; overload;
    procedure AddLast(const node: TLinkedListNode<T>); overload;
    function AddLast(const value: T): TLinkedListNode<T>; overload;

    procedure Clear;

    function Find(const value: T): TLinkedListNode<T>;
    function FindLast(const value: T): TLinkedListNode<T>;

    procedure Remove(const node: TLinkedListNode<T>); overload;
    procedure RemoveFirst;
    procedure RemoveLast;
  end;

implementation

uses
  Generics.Defaults,
  Spring.Collections.Events,
  Spring.Comparers,
  Spring.ResourceStrings;


{$REGION 'TLinkedList<T>'}

procedure TLinkedList<T>.BeforeDestruction;
begin
  Clear;
  inherited BeforeDestruction;
end;

function TLinkedList<T>.Add(const item: T): Boolean;
begin
  AddLast(item);
  Result := True;
end;

procedure TLinkedList<T>.AddAfter(const node, newNode: TLinkedListNode<T>);
begin
  ValidateNode(node);
  ValidateNewNode(newNode);
  InternalInsertNodeBefore(node.fNext, newNode);
end;

function TLinkedList<T>.AddAfter(const node: TLinkedListNode<T>;
  const value: T): TLinkedListNode<T>;
begin
  ValidateNode(node);
  Result := EnsureNode(value);
  InternalInsertNodeBefore(node.fNext, Result);
end;

procedure TLinkedList<T>.AddBefore(const node, newNode: TLinkedListNode<T>);
begin
  ValidateNode(node);
  ValidateNewNode(newNode);
  InternalInsertNodeBefore(node, newNode);
  if node = fHead then
    fHead := newNode;
end;

function TLinkedList<T>.AddBefore(const node: TLinkedListNode<T>;
  const value: T): TLinkedListNode<T>;
begin
  ValidateNode(node);
  Result := EnsureNode(value);
  InternalInsertNodeBefore(node, Result);
  if node = fHead then
    fHead := Result;
end;

procedure TLinkedList<T>.AddFirst(const node: TLinkedListNode<T>);
begin
  ValidateNewNode(node);
  if not Assigned(fHead) then
    InternalInsertNodeToEmptyList(node)
  else
  begin
    InternalInsertNodeBefore(fHead, node);
    fHead := node;
  end;
end;

function TLinkedList<T>.AddFirst(const value: T): TLinkedListNode<T>;
begin
  Result := EnsureNode(value);
  if not Assigned(fHead) then
    InternalInsertNodeToEmptyList(Result)
  else
  begin
    InternalInsertNodeBefore(fHead, Result);
    fHead := Result;
  end;
end;

procedure TLinkedList<T>.AddLast(const node: TLinkedListNode<T>);
begin
  ValidateNewNode(node);
  if not Assigned(fHead) then
    InternalInsertNodeToEmptyList(node)
  else
    InternalInsertNodeBefore(fHead, node);
end;

function TLinkedList<T>.AddLast(const value: T): TLinkedListNode<T>;
begin
  Result := EnsureNode(value);
  if not Assigned(fHead) then
    InternalInsertNodeToEmptyList(Result)
  else
    InternalInsertNodeBefore(fHead, Result);
end;

procedure TLinkedList<T>.Clear;
var
  oldItems: array of T;
  i: Integer;
  node1, node2: TLinkedListNode<T>;
begin
  SetLength(oldItems, fCount);
  i := 0;

  IncUnchecked(fVersion);
  node1 := fHead;
  while Assigned(node1) do
  begin
    oldItems[i] := node1.fItem;
    Inc(i);

    node2 := node1;
    node1 := node1.Next;
    node2.Free;
  end;
  fHead := nil;
  fCount := 0;

  while Assigned(fFirstFree) do
  begin
    node1 := fFirstFree;
    fFirstFree := fFirstFree.fNext;
    node1.Free;
  end;

  for i := 0 to DynArrayHigh(oldItems) do
    Changed(oldItems[i], caRemoved);
end;

function TLinkedList<T>.EnsureNode(const value: T): TLinkedListNode<T>;
begin
  if not Assigned(fFirstFree) then
    Result := TLinkedListNode<T>.Create(value)
  else
  begin
    Result := fFirstFree;
    Result.fItem := value;
    fFirstFree := fFirstFree.fNext;
  end;
end;

function TLinkedList<T>.Extract(const item: T): T;
var
  node: TLinkedListNode<T>;
begin
  node := Find(item);
  if not Assigned(node) then
    Result := Default(T)
  else
  begin
    Result := node.fItem;
    InternalRemoveNode(node);
  end;
end;

function TLinkedList<T>.Find(const value: T): TLinkedListNode<T>;
var
  node: TLinkedListNode<T>;
  comparer: Pointer;
begin
  Result := nil;
  node := fHead;
  if Assigned(node) then
  begin
    comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
    while not IEqualityComparer<T>(comparer).Equals(node.fItem, value) do
    begin
      node := node.fNext;
      if node = fHead then
        Exit;
    end;
    Result := node;
  end;
end;

function TLinkedList<T>.FindLast(const value: T): TLinkedListNode<T>; //FI:W521
var
  node1, node2: TLinkedListNode<T>;
  comparer: Pointer;
begin
  if not Assigned(fHead) then
    Exit(nil);
  node1 := fHead.fPrev;
  node2 := node1;
  if Assigned(node2) then
  begin
    comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
    while not IEqualityComparer<T>(comparer).Equals(node2.fItem, value) do
    begin
      node2 := node2.fPrev;
      if node2 = node1 then
        Exit(nil);
    end;
  end;
  Result := node2;
end;

function TLinkedList<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TLinkedList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TLinkedList<T>.GetFirst: TLinkedListNode<T>;
begin
  Result := fHead;
end;

function TLinkedList<T>.GetLast: TLinkedListNode<T>;
begin
  if Assigned(fHead) then
    Result := fHead.fPrev
  else
    Result := nil;
end;

function TLinkedList<T>.GetNonEnumeratedCount: Integer;
begin
  Result := fCount;
end;

procedure TLinkedList<T>.InternalInsertNodeBefore(
  const node: TLinkedListNode<T>; const newNode: TLinkedListNode<T>);
begin
  IncUnchecked(fVersion);
  newNode.fList := Self;
  newNode.fNext := node;
  newNode.fPrev := node.fPrev;
  node.fPrev.fNext := newNode;
  node.fPrev := newNode;
  Inc(fCount);
  Changed(newNode.Value, caAdded);
end;

procedure TLinkedList<T>.InternalInsertNodeToEmptyList(
  const newNode: TLinkedListNode<T>);
begin
  IncUnchecked(fVersion);
  newNode.fList := Self;
  newNode.fNext := newNode;
  newNode.fPrev := newNode;
  fHead := newNode;
  Inc(fCount);
  Changed(newNode.Value, caAdded);
end;

procedure TLinkedList<T>.InternalRemoveNode(const node: TLinkedListNode<T>);
var
  item: T;
begin
  IncUnchecked(fVersion);
  item := node.Value;
  if node.fNext = node then
    fHead := nil
  else
  begin
    node.fNext.fPrev := node.fPrev;
    node.fPrev.fNext := node.fNext;
    if fHead = node then
      fHead := node.fNext;
  end;
  InvalidateNode(node);
  Dec(fCount);
  Changed(item, caRemoved);
end;

procedure TLinkedList<T>.InvalidateNode(const node: TLinkedListNode<T>);
begin
  node.fList := nil;
  node.fPrev := nil;
  node.fNext := fFirstFree;
  node.fItem := Default(T);
  fFirstFree := node;
end;

procedure TLinkedList<T>.Remove(const node: TLinkedListNode<T>);
begin
  ValidateNode(node);
  InternalRemoveNode(node);
end;

procedure TLinkedList<T>.RemoveFirst;
begin
  if not Assigned(fHead) then
    RaiseHelper.NoElements;
  InternalRemoveNode(fHead);
end;

procedure TLinkedList<T>.RemoveLast;
begin
  if not Assigned(fHead) then
    RaiseHelper.NoElements;
  InternalRemoveNode(fHead.fPrev);
end;

function TLinkedList<T>.Remove(const item: T): Boolean;
var
  node: TLinkedListNode<T>;
begin
  node := Find(item);
  Result := Assigned(node);
  if Result then
    InternalRemoveNode(node);
end;

function TLinkedList<T>.TryGetFirst(var value: T): Boolean;
begin
  Result := Assigned(fHead);
  if Result then
    value := fHead.fItem
  else
    value := Default(T);
end;

function TLinkedList<T>.TryGetLast(var value: T): Boolean;
begin
  Result := Assigned(fHead);
  if Result then
    value := fHead.fPrev.fItem
  else
    value := Default(T);
end;

procedure TLinkedList<T>.ValidateNewNode(const node: TLinkedListNode<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(node), 'node');
{$ENDIF}

  if Assigned(node.fList) then
    raise EInvalidOperationException.CreateRes(@SLinkedListNodeIsAttached);
end;

procedure TLinkedList<T>.ValidateNode(const node: TLinkedListNode<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(node), 'node');
{$ENDIF}

  if node.fList <> Pointer(Self) then
    raise EInvalidOperationException.CreateRes(@SLinkedListNodeIsAttached);
end;

{$ENDREGION}


{$REGION 'TLinkedList<T>.TEnumerator'}

constructor TLinkedList<T>.TEnumerator.Create(const list: TLinkedList<T>);
begin
  fList := list;
  fList._AddRef;
  fVersion := fList.fVersion;
  fNode := fList.fHead;
end;

destructor TLinkedList<T>.TEnumerator.Destroy; //FI:W504
begin
  fList._Release;
end;

function TLinkedList<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TLinkedList<T>.TEnumerator.MoveNext: Boolean;
begin
  if fVersion = fList.fVersion then
  begin
    if Assigned(fNode) then
    begin
      fCurrent := fNode.fItem;
      fNode := fNode.Next;
      Result := True;
    end
    else
      Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


end.
