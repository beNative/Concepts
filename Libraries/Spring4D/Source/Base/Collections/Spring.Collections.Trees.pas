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

unit Spring.Collections.Trees;

{$I Spring.inc}
// node records must not be packed because when they get stored in an array the
// pointers must be at least 2 byte aligned to leave the least significant bit 0
// aligning them by 2 byte causes at most wasting 1 byte of space for each
{$A2}

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

const
  ColorMask = IntPtr(1);
  PointerMask = not ColorMask;
  BlockSizeBits = 6;
  BlockSize = 1 shl BlockSizeBits;

type
  TNodeColor = (Black, Red);

  TBlockAllocatedArray = record
  private
    fItems: Pointer;
    fCapacity: NativeInt;
    procedure Grow(arrayType: PTypeInfo);
    procedure SetCapacity(const value: NativeInt; arrayType: PTypeInfo);
  end;

  TBlockAllocatedArray<T> = record
  strict private type
    PT = ^T;
  strict private
    fItems: TArray<TArray<T>>;
    fCapacity: NativeInt;
    function GetItem(index: Integer): PT; inline;
    procedure SetCapacity(const value: NativeInt); inline;
  public
    procedure Grow; inline;
    property Capacity: NativeInt read fCapacity write SetCapacity;
    property Items[index: Integer]: PT read GetItem; default;
  end;

  TTraverseMode = (tmInOrder, tmPreOrder, tmPostOrder);

  TBinaryTree = class abstract(TRefCountedObject)
  private type
    PNode = ^TNode;

    TEnumeratorVtable = array[0..4] of Pointer;

    TEnumerator = record
    private
      fRoot: PNode;
      fMode: TTraverseMode;
      fCurrent: PNode;
    strict private
      function MoveNextInOrder: Boolean;
      function MoveNextPreOrder: Boolean;
      function MoveNextPostOrder: Boolean;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function MoveNext: Boolean;
      property Current: PNode read fCurrent;
    end;

    TNode = record
    strict private
      fParent, fLeft, fRight: PNode;
      function GetParent: PNode; inline;
      function GetLeftMost: PNode;
      function GetRightMost: PNode;
      function GetNext: PNode;
      function GetPrev: PNode;
      function GetHeight: NativeInt;
    public
      property Parent: PNode read GetParent;
      property Left: PNode read fLeft;
      property Right: PNode read fRight;

      property LeftMost: PNode read GetLeftMost;
      property RightMost: PNode read GetRightMost;
      property Next: PNode read GetNext;
      property Prev: PNode read GetPrev;

      property Height: NativeInt read GetHeight;
    end;
  protected
    fRoot: PNode;
    fCount: Integer;
    fComparer: IInterface;

  {$REGION 'Property Accessors'}
    function GetCount: Integer; inline;
    function GetHeight: Integer;
    function GetRoot: PNode; inline;
  {$ENDREGION}
  public
    property Comparer: IInterface read fComparer;
    property Count: Integer read fCount;
    property Root: PNode read fRoot;
  end;

  TRedBlackTree = class abstract(TBinaryTree)
  private type
    PNode = ^TNode;
    TChildNodes = array[0..1] of PNode;
    TNode = record
    strict private
      function GetColor: TNodeColor; inline;
      function GetParent: PNode; inline;
    private
      fParent: PNode;
      fChilds: TChildNodes;
    public
      property Color: TNodeColor read GetColor;
      property Parent: PNode read GetParent;
      property Left: PNode read fChilds[0];
      property Right: PNode read fChilds[1];
    end;
  private
    fFreeNode: PNode;
    procedure Rotate(node: PNode; direction: Integer);

    procedure FixupAfterInsert(node: PNode);
    procedure FixupAfterDelete(node: PNode);
    procedure Delete(node: PNode);
  public
    destructor Destroy; override;

    procedure Clear; virtual;
  end;

  PBinaryTreeNode = TBinaryTree.PNode;
  PRedBlackTreeNode = TRedBlackTree.PNode;

  TNodes<T> = record
  strict private type
    PNode = ^TNode;

    TKeyEnumerator = record
    private
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: T; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function MoveNext: Boolean; inline;
      property Current: T read GetCurrent;
    end;

    TKeyEnumerable = record
    private
      fRoot: PNode;
      fMode: TTraverseMode;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function GetEnumerator: TKeyEnumerator; inline;
    end;

    TEnumerator = record
    private
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: PNode; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function MoveNext: Boolean; inline;
      property Current: PNode read GetCurrent;
    end;

    TEnumerable = record
    private
      fRoot: PNode;
      fMode: TTraverseMode;
      function GetKeys: TKeyEnumerable; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function GetEnumerator: TEnumerator; inline;
      property Keys: TKeyEnumerable read GetKeys;
    end;

    TNode = record
    strict private
      function GetLeftMost: PNode; inline;
      function GetRightMost: PNode; inline;
      function GetNext: PNode; inline;
      function GetPrev: PNode; inline;

      function GetColor: TNodeColor; inline;
      function GetParent: PNode; inline;
    private
      fParent, fLeft, fRight: PNode;
      fKey: T;
      function GetEnumerable(mode: TTraverseMode): TEnumerable; inline;
    public
      function GetEnumerator: TEnumerator; inline;

      property LeftMost: PNode read GetLeftMost;
      property RightMost: PNode read GetRightMost;
      property Next: PNode read GetNext;
      property Prev: PNode read GetPrev;

      property Color: TNodeColor read GetColor;
      property Parent: PNode read GetParent;
      property Left: PNode read fLeft;
      property Right: PNode read fRight;
      property Key: T read fKey;

      property InOrder: TEnumerable index tmInOrder read GetEnumerable;
      property PreOrder: TEnumerable index tmPreOrder read GetEnumerable;
      property PostOrder: TEnumerable index tmPostOrder read GetEnumerable;
    end;
  public type
    TRedBlackTreeNode = TNode;
    PRedBlackTreeNode = PNode;
  end;

  TNodes<TKey, TValue> = record
  strict private type
    PNode = ^TNode;

    TKeyEnumerator = record
    private
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: TKey; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function MoveNext: Boolean; inline;
      property Current: TKey read GetCurrent;
    end;

    TKeyEnumerable = record
    private
      fRoot: PNode;
      fMode: TTraverseMode;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function GetEnumerator: TKeyEnumerator; inline;
    end;

    TValueEnumerator = record
    private
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: TValue; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function MoveNext: Boolean; inline;
      property Current: TValue read GetCurrent;
    end;

    TValueEnumerable = record
    private
      fRoot: PNode;
      fMode: TTraverseMode;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function GetEnumerator: TValueEnumerator; inline;
    end;

    TEnumerator = record
    private
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: PNode; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function MoveNext: Boolean; inline;
      property Current: PNode read GetCurrent;
    end;

    TEnumerable = record
    private
      fRoot: PNode;
      fMode: TTraverseMode;
      function GetKeys: TKeyEnumerable; inline;
      function GetValues: TValueEnumerable; inline;
    public
      constructor Create(const root: PNode; mode: TTraverseMode);
      function GetEnumerator: TEnumerator; inline;
      property Keys: TKeyEnumerable read GetKeys;
      property Values: TValueEnumerable read GetValues;
    end;

    TNode = record
    strict private
      function GetLeftMost: PNode; inline;
      function GetRightMost: PNode; inline;
      function GetNext: PNode; inline;
      function GetPrev: PNode; inline;

      function GetColor: TNodeColor; inline;
      function GetParent: PNode; inline;
    private
      fParent, fLeft, fRight: PNode;
      fPair: TPair<TKey, TValue>;
      function GetEnumerable(mode: TTraverseMode): TEnumerable; inline;
    public
      function GetEnumerator: TEnumerator; inline;

      property LeftMost: PNode read GetLeftMost;
      property RightMost: PNode read GetRightMost;
      property Next: PNode read GetNext;
      property Prev: PNode read GetPrev;

      property Color: TNodeColor read GetColor;
      property Parent: PNode read GetParent;
      property Left: PNode read fLeft;
      property Right: PNode read fRight;
      property Pair: TPair<TKey, TValue> read fPair;

      property InOrder: TEnumerable index tmInOrder read GetEnumerable;
      property PreOrder: TEnumerable index tmPreOrder read GetEnumerable;
      property PostOrder: TEnumerable index tmPostOrder read GetEnumerable;
    end;
  public type
    TRedBlackTreeNode = TNode;
    PRedBlackTreeNode = PNode;
  end;

  IBinaryTree = interface
    ['{2A6DBEEA-FFBA-40DB-9274-5057238CDFAB}']
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetHeight: Integer;
    function GetRoot: PBinaryTreeNode;
  {$ENDREGION}

    property Count: Integer read GetCount;
    property Height: Integer read GetHeight;
    property Root: PBinaryTreeNode read GetRoot;
  end;

  IBinaryTree<T> = interface(IBinaryTree) //FI:W524
    ['{06E837A5-29B7-4F33-AC5C-46BC82F00D15}']
  {$REGION 'Property Accessors'}
    function GetRoot: TNodes<T>.PRedBlackTreeNode;
  {$ENDREGION}

    function Add(const key: T): Boolean;
    function Delete(const key: T): Boolean;
    function Exists(const key: T): Boolean;
    function Find(const key: T; var value: T): Boolean;
    procedure Clear;

    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;

    property Root: TNodes<T>.PRedBlackTreeNode read GetRoot;
  end;

  IBinaryTree<TKey, TValue> = interface(IBinaryTree) //FI:W524
    ['{7F554520-BD51-4B53-953B-61B43ED6D59E}']
  {$REGION 'Property Accessors'}
    function GetRoot: TNodes<TKey, TValue>.PRedBlackTreeNode;
  {$ENDREGION}

    function Add(const key: TKey; const value: TValue): Boolean;
    function AddOrSet(const key: TKey; const value: TValue): Boolean;
    function Delete(const key: TKey): Boolean;
    function Exists(const key: TKey): Boolean;
    function Find(const key: TKey; var foundValue: TValue): Boolean;
    procedure Clear;

    function GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
    function ToArray: TArray<TPair<TKey,TValue>>;

    property Root: TNodes<TKey, TValue>.PRedBlackTreeNode read GetRoot;
  end;

  IRedBlackTree<T> = interface(IBinaryTree<T>) //FI:W524
    ['{59BB2B37-D85F-4092-8E80-1EFEE1D2E8F8}']
    function FindNode(const value: T): TNodes<T>.PRedBlackTreeNode;
    procedure DeleteNode(node: TNodes<T>.PRedBlackTreeNode);
  end;

  IRedBlackTree<TKey, TValue> = interface(IBinaryTree<TKey, TValue>) //FI:W524
    ['{8C6F6C1A-92C1-4F4A-A1A9-DD5EA70921CB}']
    function FindNode(const key: TKey): TNodes<TKey, TValue>.PRedBlackTreeNode;
    procedure DeleteNode(node: TNodes<TKey, TValue>.PRedBlackTreeNode);
  end;

  TRedBlackTreeBase<T> = class(TRedBlackTree)
  private type
    TNode = TNodes<T>.TRedBlackTreeNode;
    PNode = TNodes<T>.PRedBlackTreeNode;
  private
    fStorage: TBlockAllocatedArray<TNode>;
    function GetCapacity: Integer;
    procedure SetCapacity(value: Integer);
  protected
    function AddNode(const key: T): PNode;
    function CreateNode(const key: T; parent: PNode): PNode;
    function FindNode(const key: T): PNode;
    procedure DeleteNode(node: PNode);
  public
    constructor Create; overload;
    constructor Create(const comparer: IComparer<T>); overload;

    procedure Clear; override;

    procedure TrimExcess;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
  end;

  TRedBlackTree<T> = class(TRedBlackTreeBase<T>, IInterface, IBinaryTree<T>, IRedBlackTree<T>)
  private type
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
    PNode = TRedBlackTreeBase<T>.PNode;
    function GetRoot: PNode; overload; inline;
  public
    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;

  {$REGION 'Implements IBinaryTree<T>'}
    function Add(const key: T): Boolean;
    function Delete(const key: T): Boolean;
    function Exists(const key: T): Boolean;
    function Find(const key: T; var value: T): Boolean;
  {$ENDREGION}
  end;

  TRedBlackTreeBase<TKey, TValue> = class(TRedBlackTree)
  private type
    TNode = TNodes<TKey, TValue>.TRedBlackTreeNode;
    PNode = TNodes<TKey, TValue>.PRedBlackTreeNode;
  private
    fStorage: TBlockAllocatedArray<TNode>;
    function GetCapacity: Integer;
    procedure SetCapacity(value: Integer);
  protected
    // IMPORTANT: When returning an existing node the lowest bit is set
    function AddNode(const key: TKey; allowReplace: Boolean = False): PNode;
    function CreateNode(const key: TKey; parent: PNode): PNode;
    function FindNode(const key: TKey): PNode;
    procedure DeleteNode(node: PNode);
  public
    constructor Create; overload;
    constructor Create(const comparer: IComparer<TKey>); overload;

    procedure Clear; override;

    procedure TrimExcess;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
  end;

  TRedBlackTree<TKey, TValue> = class(TRedBlackTreeBase<TKey, TValue>,
    IInterface, IBinaryTree<TKey, TValue>, IRedBlackTree<TKey, TValue>)
  private type
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fEnumerator: TBinaryTree.TEnumerator;
      function GetCurrent: TPair<TKey,TValue>;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
    PNode = TRedBlackTreeBase<TKey, TValue>.PNode;
    function GetRoot: PNode; overload; inline;
  public
    function GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
    function ToArray: TArray<TPair<TKey,TValue>>;

  {$REGION 'Implements IBinaryTree<TKey, TValue>'}
    function Add(const key: TKey; const value: TValue): Boolean;
    function AddOrSet(const key: TKey; const value: TValue): Boolean;
    function Delete(const key: TKey): Boolean;
    function Exists(const key: TKey): Boolean;
    function Find(const key: TKey; var foundValue: TValue): Boolean;
  {$ENDREGION}
  end;

implementation

uses
  {$IF not declared(TTypeKind)}
  TypInfo,
  {$IFEND}
  Spring.Collections.Base,
  Spring.Comparers;


{$REGION 'TBlockAllocatedArray<T>'}

procedure TBlockAllocatedArray.Grow(arrayType: PTypeInfo);
var
  n, blockLength: NativeInt;
  elemType: PTypeInfo;
begin
  n := DynArrayLength(fItems) + 1;
  Inc(fCapacity, BlockSize);
  DynArraySetLength(fItems, arrayType, 1, @n);
  elemType := Pointer(PDynArrayTypeInfo(PByte(arrayType) + Byte(PDynArrayTypeInfo(arrayType).name)).elType^);
  blockLength := BlockSize;
  DynArraySetLength(TArray<Pointer>(fItems)[n-1], elemType, 1, @blockLength);
end;

procedure TBlockAllocatedArray.SetCapacity(const value: NativeInt; arrayType: PTypeInfo);
var
  oldLength, newLength, i, blockLength: NativeInt;
  elemType: PTypeInfo;
begin
  oldLength := DynArrayLength(fItems);
  newLength := value shr BlockSizeBits;
  if value and (BlockSize - 1) > 0 then
    Inc(newLength);
  fCapacity := newLength * BlockSize;
  DynArraySetLength(fItems, arrayType, 1, @newLength);
  elemType := Pointer(PDynArrayTypeInfo(PByte(arrayType) + Byte(PDynArrayTypeInfo(arrayType).name)).elType^);
  blockLength := BlockSize;
  for i := oldLength to newLength - 1 do
    DynArraySetLength(TArray<Pointer>(fItems)[i], elemType, 1, @blockLength);
end;


{$REGION 'TBlockAllocatedArray<T>'}

function TBlockAllocatedArray<T>.GetItem(index: Integer): PT;
begin
  Result := @fItems[index shr BlockSizeBits, index and (BlockSize - 1)];
end;

procedure TBlockAllocatedArray<T>.Grow;
begin
  TBlockAllocatedArray(Self).Grow(TypeInfo(TArray<TArray<T>>));
end;

procedure TBlockAllocatedArray<T>.SetCapacity(const value: NativeInt);
begin
  TBlockAllocatedArray(Self).SetCapacity(value, TypeInfo(TArray<TArray<T>>));
end;

{$ENDREGION}


{$REGION 'TBinaryTree'}

function TBinaryTree.GetCount: Integer;
begin
  Result := fCount;
end;

function TBinaryTree.GetHeight: Integer;
begin
  Result := PBinaryTreeNode(fRoot).Height;
end;

function TBinaryTree.GetRoot: PBinaryTreeNode;
begin
  Result := fRoot;
end;

{$ENDREGION}


{$REGION 'TBinaryTree.TNode'}

function TBinaryTree.TNode.GetParent: PNode;
begin
  Result := Pointer(IntPtr(fParent) and PointerMask);
end;

function TBinaryTree.TNode.GetHeight: NativeInt;
var
  height: NativeInt;
begin
  Result := NativeInt(@Self);
  if Result > 0 then
  begin
    height := fLeft.Height;
    Result := fRight.Height;
    if height >= Result then
      Result := height;
    Inc(Result);
  end;
end;

function TBinaryTree.TNode.GetLeftMost: PNode;
begin
  Result := @Self;
  if Result = nil then Exit;
  while Assigned(Result.Left) do
    Result := Result.Left;
end;

function TBinaryTree.TNode.GetRightMost: PNode;
begin
  Result := @Self;
  if Result = nil then Exit;
  while Assigned(Result.Right) do
    Result := Result.Right;
end;

function TBinaryTree.TNode.GetNext: PNode;
var
  node: PNode;
begin
  Result := @Self;
  if Result = nil then Exit;
  if Assigned(Result.Right) then
  begin
    Result := Result.Right;
    while Assigned(Result.Left) do
      Result := Result.Left;
    Exit;
  end;
  repeat
    node := Result;
    Result := Result.Parent;
  until (Result = nil) or (node <> Result.Right);
end;

function TBinaryTree.TNode.GetPrev: PNode;
var
  node: PNode;
begin
  Result := @Self;
  if Result = nil then Exit;
  if Assigned(Result.Left) then
  begin
    Result := Result.Left;
    while Assigned(Result.Right) do
      Result := Result.Right;
    Exit;
  end;
  repeat
    node := Result;
    Result := Result.Parent;
  until (Result = nil) or (node <> Result.Left);
end;

{$ENDREGION}


{$REGION 'TBinaryTree.TEnumerator'}

constructor TBinaryTree.TEnumerator.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fRoot := root;
  fCurrent := nil;
  fMode := mode;
end;

function TBinaryTree.TEnumerator.MoveNext: Boolean;
begin
  if fMode = tmInOrder then
    Result := MoveNextInOrder
  else if fMode = tmPreOrder then
    Result := MoveNextPreOrder
  else
    Result := MoveNextPostOrder;
end;

function TBinaryTree.TEnumerator.MoveNextInOrder: Boolean;
begin
  if not Assigned(fCurrent) then
    fCurrent := fRoot.LeftMost
  else
    fCurrent := fCurrent.Next;

  Result := Assigned(fCurrent);
  if not Result then
    fRoot := nil;
end;

function TBinaryTree.TEnumerator.MoveNextPreOrder: Boolean;
var
  sibling: PBinaryTreeNode;
begin
  if not Assigned(fCurrent) then
    fCurrent := fRoot
  else if Assigned(fCurrent.Left) then // walk down left
    fCurrent := fCurrent.Left
  else if Assigned(fCurrent.Right) then // walk down right
    fCurrent := fCurrent.Right
  else
  begin
    while Assigned(fCurrent.Parent) and (fCurrent <> fRoot) do // walk up ...
    begin
      sibling := fCurrent.Parent.Right;
      if Assigned(sibling) and (sibling <> fCurrent) then // ... and down right
      begin
        fCurrent := sibling;
        Exit(True);
      end;
      fCurrent := fCurrent.Parent;
    end;
    fCurrent := nil;
  end;

  Result := Assigned(fCurrent);
  if not Result then
    fRoot := nil;
end;

function TBinaryTree.TEnumerator.MoveNextPostOrder: Boolean;
var
  sibling: PBinaryTreeNode;
begin
  if not Assigned(fCurrent) then
    fCurrent := fRoot
  else if Assigned(fCurrent.Parent) and (fCurrent <> fRoot) then // walk up ...
  begin
    sibling := fCurrent.Parent.Right;
    if Assigned(sibling) and (sibling <> fCurrent) then // ... and down right
      fCurrent := sibling
    else
    begin
      fCurrent := fCurrent.Parent;
      Exit(True);
    end;
  end
  else
    fCurrent := nil;

  while Assigned(fCurrent) do // walk down to leftmost leaf
    if Assigned(fCurrent.Left) then
      fCurrent := fCurrent.Left
    else if Assigned(fCurrent.Right) then
      fCurrent := fCurrent.Right
    else
      Break;

  Result := Assigned(fCurrent);
  if not Result then
    fRoot := nil;
end;

{$ENDREGION}


{$REGION 'TRedBlackTree'}

destructor TRedBlackTree.Destroy; //FI:W504
begin
  Clear;
end;

procedure TRedBlackTree.Clear;
begin
  fRoot := nil;
  fCount := 0;
end;

procedure TRedBlackTree.Delete(node: PNode);
var
  child, parent, freeNode: PNode;
  nodeSide: Integer;
begin
  child := node.fChilds[Ord(Assigned(node.Right))];
  parent := Pointer(IntPtr(node.fParent) and PointerMask);

  // node has a child
  if Assigned(child) then
  begin
    if Assigned(parent) then
    begin
      // the side of node in the childs of its parent - 0 = left, 1 = right
      nodeSide := Ord(parent.Right = node);
      parent.fChilds[nodeSide] := child;
      IntPtr(child.fParent) := IntPtr(parent) or IntPtr(child.fParent) and ColorMask;
    end
    else
    begin
      fRoot := Pointer(child);
      nodeSide := Ord(node.Right = child);
      node.fChilds[nodeSide] := parent;
      child.fParent := parent;
    end;

    if not Odd(IntPtr(node.fParent)) then
      FixupAfterDelete(child);
  end else
  // node is not the root
  if Assigned(parent) then
  begin
    if not Odd(IntPtr(node.fParent)) then
      FixupAfterDelete(node);

    // unlink the node which could not be done before
    // because FixupAfterDelete requires Parent to be set
    nodeSide := Ord(parent.Right = node);
    parent.fChilds[nodeSide] := child;
    node.fParent := child;
  end
  else
    fRoot := Pointer(child);

  Dec(fCount);
  freeNode := fFreeNode;
  fFreeNode := node;
  node.fParent := freeNode;
  node.fChilds[0] := nil;
  node.fChilds[1] := nil;
end;

procedure TRedBlackTree.FixupAfterDelete(node: PNode);
var
  parent, sibling, child: PNode;
  direction, parentColor: Integer;
begin
  if (Pointer(node) <> fRoot) and not Odd(IntPtr(node.fParent)) then
  repeat
    parent := Pointer(IntPtr(node.fParent) and PointerMask);

    direction := Ord(node = parent.Left);   // 0 => right child, 1 => left child
    sibling := parent.fChilds[direction];   // left = 0, right = 1

    // case 1: sibling is red
    if Assigned(sibling) and Odd(IntPtr(sibling.fParent)) then
    begin
      IntPtr(sibling.fParent) := IntPtr(sibling.fParent) and PointerMask;
      IntPtr(parent.fParent) := IntPtr(parent.fParent) or 1;
      Rotate(parent, direction xor 1);
      parent := Pointer(IntPtr(node.fParent) and PointerMask);
      sibling := parent.fChilds[direction];
    end;

    // case 2: both of siblings children are black
    if not (Assigned(sibling.Left) and Odd(IntPtr(sibling.Left.fParent)))
      and not (Assigned(sibling.Right) and Odd(IntPtr(sibling.Right.fParent))) then
    begin
      IntPtr(sibling.fParent) := IntPtr(sibling.fParent) or 1;
      node := parent;
    end else
    begin
      // case 3: siblings right/left child is black
      child := sibling.fChilds[direction];
      if not Assigned(child) or not Odd(IntPtr(child.fParent)) then
      begin
        child := sibling.fChilds[direction xor 1];
        IntPtr(child.fParent) := IntPtr(child.fParent) and PointerMask;
        IntPtr(sibling.fParent) := IntPtr(sibling.fParent) or 1;
        Rotate(sibling, direction);
        parent := Pointer(IntPtr(node.fParent) and PointerMask);
        sibling := parent.fChilds[direction];
      end;

      // case 4: siblings right/left child is red
      parentColor := IntPtr(parent.fParent) and ColorMask;
      IntPtr(sibling.fParent) := IntPtr(sibling.fParent) and PointerMask or parentColor;
      IntPtr(parent.fParent) := IntPtr(parent.fParent) and PointerMask;
      child := sibling.fChilds[direction];
      IntPtr(child.fParent) := IntPtr(child.fParent) and PointerMask;
      Rotate(parent, direction xor 1);
      node := Pointer(fRoot);
    end;
  until (Pointer(node) = fRoot) or Odd(IntPtr(node.fParent));

  IntPtr(node.fParent) := IntPtr(node.fParent) and PointerMask;
end;

procedure TRedBlackTree.FixupAfterInsert(node: PNode);
var
  parent, grandParent, uncle: PNode;
  direction: Integer;
begin
  parent := Pointer(IntPtr(node.fParent) and PointerMask);

  // if node is not the root and its parent is red
  if Assigned(parent) and Odd(IntPtr(parent.fParent)) then
  repeat
    grandParent := Pointer(IntPtr(parent.fParent) and PointerMask);
    direction := Ord(parent = grandParent.Left);                        // 0 => right child, 1 => left child
    uncle := grandParent.fChilds[direction];                            // left = 0, right = 1

    // case 1: uncle is red
    if Assigned(uncle) and Odd(IntPtr(uncle.fParent)) then
    begin
      IntPtr(parent.fParent) := IntPtr(parent.fParent) and PointerMask; // parent.Color := Black;
      IntPtr(uncle.fParent) := IntPtr(uncle.fParent) and PointerMask;   // uncle.Color := Black;
      IntPtr(grandParent.fParent) := IntPtr(grandParent.fParent) or 1;  // grandParent.Color := Red;
      node := grandParent;
    end else
    // case 2: uncle is black and node is a right/left child
    if node = parent.fChilds[direction] then
    begin
      node := parent;
      Rotate(node, direction xor 1);                                    // if parent = grandParent.Left -> RotateLeft
    end else
    // case 3: uncle is black and node is a left/right child
    begin
      IntPtr(parent.fParent) := IntPtr(parent.fParent) and PointerMask; // parent.Color := Black;
      IntPtr(grandParent.fParent) := IntPtr(grandParent.fParent) or 1;  // grandParent.Color := Red;
      Rotate(grandParent, direction);                                   // if parent = grandParent.Left -> RotateRight
    end;

    if not Assigned(node) then Break;
    if Pointer(node) = fRoot then Break;
    parent := Pointer(IntPtr(node.fParent) and PointerMask);
    if not Assigned(parent) then Break;
    if not Odd(IntPtr(parent.fParent)) then Break;
  until False;

  IntPtr(PNode(fRoot).fParent) := IntPtr(PNode(fRoot).fParent) and PointerMask;
end;

procedure TRedBlackTree.Rotate(node: PNode; direction: Integer); // direction = 0  means left rotate
var
  newRoot, relocatedChild, parent: PRedBlackTreeNode;
  nodeSide: Integer;
begin
  newRoot := node.fChilds[direction xor 1]; // newRoot is the child of node that gets moved up - 0 = right, 1 = left

  relocatedChild := newRoot.fChilds[direction];  // the child of newRoot that is the replacement child of node
  node.fChilds[direction xor 1] := relocatedChild;
  if Assigned(relocatedChild) then
    IntPtr(relocatedChild.fParent) := IntPtr(node) or IntPtr(relocatedChild.fParent) and ColorMask;

  parent := Pointer(IntPtr(node.fParent) and PointerMask);
  IntPtr(newRoot.fParent) := IntPtr(parent) or IntPtr(newRoot.fParent) and ColorMask;
  if Assigned(parent) then
  begin
    nodeSide := Ord(parent.Right = node); // the side of node in the childs of its parent - 0 = left, 1 = right
    parent.fChilds[nodeSide] := newRoot;
  end
  else
    fRoot := Pointer(newRoot);

  newRoot.fChilds[direction] := node;
  if Assigned(node) then
    IntPtr(node.fParent) := IntPtr(newRoot) or IntPtr(node.fParent) and ColorMask;
end;

{$ENDREGION}


{$REGION 'TRedBlackTree.TNode'}

function TRedBlackTree.TNode.GetColor: TNodeColor;
begin
  Result := TNodeColor(IntPtr(fParent) and ColorMask);
end;

function TRedBlackTree.TNode.GetParent: PNode;
begin
  Result := Pointer(IntPtr(fParent) and PointerMask);
end;

{$ENDREGION}


{$REGION 'TNodes<T>.TNode'}

function TNodes<T>.TNode.GetColor: TNodeColor;
begin
  Result := PRedBlackTreeNode(@fParent).Color;
end;

function TNodes<T>.TNode.GetEnumerable(mode: TTraverseMode): TEnumerable;
begin
  Result := TEnumerable.Create(@fParent, mode);
end;

function TNodes<T>.TNode.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(@fParent, tmInOrder);
end;

function TNodes<T>.TNode.GetLeftMost: PNode;
begin
  Result := PNode(PBinaryTreeNode(@fParent).LeftMost);
end;

function TNodes<T>.TNode.GetNext: PNode;
begin
  Result := PNode(PBinaryTreeNode(@fParent).Next);
end;

function TNodes<T>.TNode.GetParent: PNode;
begin
  Result := PNode(PBinaryTreeNode(@fParent).Parent);
end;

function TNodes<T>.TNode.GetPrev: PNode;
begin
  Result := PNode(PBinaryTreeNode(@fParent).Prev);
end;

function TNodes<T>.TNode.GetRightMost: PNode;
begin
  Result := PNode(PBinaryTreeNode(@fParent).RightMost);
end;

{$ENDREGION}


{$REGION 'TNodes<T>.TKeyEnumerator'}

constructor TNodes<T>.TKeyEnumerator.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fEnumerator := TBinaryTree.TEnumerator.Create(Pointer(root), mode);
end;

function TNodes<T>.TKeyEnumerator.GetCurrent: T;
begin
  Result := PNode(fEnumerator.fCurrent).Key;
end;

function TNodes<T>.TKeyEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TNodes<T>.TKeyEnumerable'}

constructor TNodes<T>.TKeyEnumerable.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fRoot := root;
  fMode := mode;
end;

function TNodes<T>.TKeyEnumerable.GetEnumerator: TKeyEnumerator;
begin
  Result := TKeyEnumerator.Create(fRoot, fMode);
end;

{$ENDREGION}


{$REGION 'TNodes<T>.TEnumerator'}

constructor TNodes<T>.TEnumerator.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fEnumerator := TBinaryTree.TEnumerator.Create(Pointer(root), mode);
end;

function TNodes<T>.TEnumerator.GetCurrent: PNode;
begin
  Result := PNode(fEnumerator.fCurrent);
end;

function TNodes<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TNodes<T>.TEnumerable'}

constructor TNodes<T>.TEnumerable.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fRoot := root;
  fMode := mode;
end;

function TNodes<T>.TEnumerable.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(fRoot, fMode);
end;

function TNodes<T>.TEnumerable.GetKeys: TKeyEnumerable;
begin
  Result := TKeyEnumerable.Create(fRoot, fMode);
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TNode'}

function TNodes<TKey, TValue>.TNode.GetColor: TNodeColor;
begin
  Result := PRedBlackTreeNode(@fParent).Color;
end;

function TNodes<TKey, TValue>.TNode.GetEnumerable(
  mode: TTraverseMode): TEnumerable;
begin
  Result := TEnumerable.Create(@fParent, mode);
end;

function TNodes<TKey, TValue>.TNode.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(@fParent, tmInOrder);
end;

function TNodes<TKey, TValue>.TNode.GetLeftMost: PNode;
begin
  Result := PNode(PBinaryTreeNode(@fParent).LeftMost);
end;

function TNodes<TKey, TValue>.TNode.GetNext: PNode;
begin
  Result := PNode(PBinaryTreeNode(@fParent).Next);
end;

function TNodes<TKey, TValue>.TNode.GetParent: PNode;
begin
  Result := PNode(PBinaryTreeNode(@fParent).Parent);
end;

function TNodes<TKey, TValue>.TNode.GetPrev: PNode;
begin
  Result := PNode(PBinaryTreeNode(@fParent).Prev);
end;

function TNodes<TKey, TValue>.TNode.GetRightMost: PNode;
begin
  Result := PNode(PBinaryTreeNode(@fParent).RightMost);
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TKeyEnumerator'}

constructor TNodes<TKey, TValue>.TKeyEnumerator.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fEnumerator := TBinaryTree.TEnumerator.Create(Pointer(root), mode);
end;

function TNodes<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result := PNode(fEnumerator.fCurrent).fPair.Key;
end;

function TNodes<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TKeyEnumerable'}

constructor TNodes<TKey, TValue>.TKeyEnumerable.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fRoot := root;
  fMode := mode;
end;

function TNodes<TKey, TValue>.TKeyEnumerable.GetEnumerator: TKeyEnumerator;
begin
  Result := TKeyEnumerator.Create(fRoot, fMode);
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TValueEnumerator'}

constructor TNodes<TKey, TValue>.TValueEnumerator.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fEnumerator := TBinaryTree.TEnumerator.Create(Pointer(root), mode);
end;

function TNodes<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := PNode(fEnumerator.fCurrent).fPair.Value;
end;

function TNodes<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TValueEnumerable'}

constructor TNodes<TKey, TValue>.TValueEnumerable.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fRoot := root;
  fMode := mode;
end;

function TNodes<TKey, TValue>.TValueEnumerable.GetEnumerator: TValueEnumerator;
begin
  Result := TValueEnumerator.Create(fRoot, fMode);
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TEnumerator'}

constructor TNodes<TKey, TValue>.TEnumerator.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fEnumerator := TBinaryTree.TEnumerator.Create(Pointer(root), mode);
end;

function TNodes<TKey, TValue>.TEnumerator.GetCurrent: PNode;
begin
  Result := PNode(fEnumerator.fCurrent);
end;

function TNodes<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TNodes<TKey, TValue>.TEnumerable'}

constructor TNodes<TKey, TValue>.TEnumerable.Create(const root: PNode;
  mode: TTraverseMode);
begin
  fRoot := root;
  fMode := mode;
end;

function TNodes<TKey, TValue>.TEnumerable.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(fRoot, fMode);
end;

function TNodes<TKey, TValue>.TEnumerable.GetKeys: TKeyEnumerable;
begin
  Result := TKeyEnumerable.Create(fRoot, fMode);
end;

function TNodes<TKey, TValue>.TEnumerable.GetValues: TValueEnumerable;
begin
  Result := TValueEnumerable.Create(fRoot, fMode);
end;

{$ENDREGION}


{$REGION 'TRedBlackTreeBase<T>'}

constructor TRedBlackTreeBase<T>.Create;
begin
  Create(nil);
end;

constructor TRedBlackTreeBase<T>.Create(const comparer: IComparer<T>);
begin
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := IComparer<T>(_LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T)));
end;

function TRedBlackTreeBase<T>.CreateNode(const key: T; parent: PNode): PNode;
begin
  if fCount = fStorage.Capacity then
    fStorage.Grow;

  if Assigned(fFreeNode) then
  begin
    Result := PNode(fFreeNode);
    fFreeNode := fFreeNode.fParent;
  end
  else
    Result := PNode(fStorage[fCount]);
  IntPtr(Result.fParent) := IntPtr(parent) or Ord(Assigned(parent)); // mark node red when it has parent
  Result.fKey := key;
  Inc(fCount);
end;

function TRedBlackTreeBase<T>.AddNode(const key: T): PNode;
var
  node: PNode;
  newNode, child: Pointer;
  compareResult, i: Integer;
begin
  if Assigned(fRoot) then
  begin
    child := fRoot;
    repeat
      node := child;
      compareResult := IComparer<T>(fComparer).Compare(node.Key, key);
      if compareResult = 0 then
        Exit(Pointer(compareResult)); //FI:W541
      i := compareResult shr 31; // left -> 0, right -> 1
      child := PRedBlackTreeNode(node).fChilds[i];
    until not Assigned(child);

    newNode := CreateNode(key, node);
    PRedBlackTreeNode(node).fChilds[i] := newNode;
    FixupAfterInsert(newNode);
    Exit(newNode);
  end
  else
  begin
    Result := CreateNode(key, nil);
    fRoot := Pointer(Result);
  end;
end;

procedure TRedBlackTreeBase<T>.Clear;
begin
  inherited Clear;
  fStorage.Capacity := 0;
end;

procedure TRedBlackTreeBase<T>.DeleteNode(node: PNode);
var
  next: PNode;
begin
  if Assigned(node.Left) and Assigned(node.Right) then
  begin
    // inline node.Next -> we know that it will be node.Right.LeftMost
    next := node.Right;
    while Assigned(next.Left) do
      next := next.Left;
    node.fKey := next.Key;
    node := next;
  end;
  node.fKey := Default(T);
  inherited Delete(PRedBlackTreeNode(node));
end;

function TRedBlackTreeBase<T>.FindNode(const key: T): PNode;
var
  root, node: PNode;
  compareResult, i: Integer;
begin
  root := Pointer(fRoot);
  if Assigned(root) then
  begin
    node := root;
    repeat
      compareResult := IComparer<T>(fComparer).Compare(node.Key, key);
      if compareResult = 0 then Break;
      i := compareResult shr 31; // left -> 0, right -> 1
      node := PNode(PRedBlackTreeNode(node).fChilds[i]);
    until not Assigned(node);
    Result := node;
  end
  else
    Result := root;
end;

function TRedBlackTreeBase<T>.GetCapacity: Integer;
begin
  Result := fStorage.Capacity;
end;

procedure TRedBlackTreeBase<T>.SetCapacity(value: Integer);
begin
  if value >= fCount then
    fStorage.Capacity := value
  else
    RaiseHelper.ArgumentOutOfRange(ExceptionArgument.value, ExceptionResource.ArgumentOutOfRange_Capacity);
end;

procedure TRedBlackTreeBase<T>.TrimExcess;
begin
  SetCapacity(fCount);
end;

{$ENDREGION}


{$REGION 'TRedBlackTree<T>'}

function TRedBlackTree<T>.Add(const key: T): Boolean;
begin
  Result := Assigned(AddNode(key));
end;

function TRedBlackTree<T>.Delete(const key: T): Boolean;
var
  node: PNode;
begin
  if fCount = 0 then
    Exit(False);
  node := FindNode(key);
  Result := Assigned(node);
  if Result then
    DeleteNode(node);
end;

function TRedBlackTree<T>.Exists(const key: T): Boolean;
var
  node: PNode;
begin
  node := FindNode(key);
  Result := Assigned(node);
end;

function TRedBlackTree<T>.Find(const key: T; var value: T): Boolean;
var
  node: PNode;
begin
  if fCount = 0 then
    Exit(False);
  node := FindNode(key);
  Result := Assigned(node);
  if Result then
    value := node.Key
  else
    value := Default(T);
end;

function TRedBlackTree<T>.GetEnumerator: IEnumerator<T>; //FI:W521
begin
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
    fEnumerator := TBinaryTree.TEnumerator.Create(fRoot, tmInOrder);
end;

function TRedBlackTree<T>.GetRoot: PNode;
begin
  Result := Pointer(fRoot);
end;

function TRedBlackTree<T>.ToArray: TArray<T>;
var
  node: PBinaryTreeNode;
  i: Integer;
begin
  SetLength(Result, fCount);
  if fCount > 0 then
  begin
    node := PBinaryTreeNode(fRoot).LeftMost;
    for i := 0 to fCount - 1 do
    begin
      Result[i] := PNode(node).Key;
      node := node.Next;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TRedBlackTree<T>.TEnumerator'}

function TRedBlackTree<T>.TEnumerator.GetCurrent: T;
begin
  Result := PNode(fEnumerator.fCurrent).Key;
end;

function TRedBlackTree<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TRedBlackTreeBase<TKey, TValue>'}

constructor TRedBlackTreeBase<TKey, TValue>.Create;
begin
  Create(nil);
end;

constructor TRedBlackTreeBase<TKey, TValue>.Create(const comparer: IComparer<TKey>);
begin
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := IComparer<TKey>(_LookupVtableInfo(giComparer, TypeInfo(TKey), SizeOf(TKey)));
end;

function TRedBlackTreeBase<TKey, TValue>.CreateNode(const key: TKey; parent: PNode): PNode;
begin
  if fCount = fStorage.Capacity then
    fStorage.Grow;

  if Assigned(fFreeNode) then
  begin
    Result := PNode(fFreeNode);
    fFreeNode := fFreeNode.fParent;
  end
  else
    Result := PNode(fStorage[fCount]);
  IntPtr(Result.fParent) := IntPtr(parent) or Ord(Assigned(parent)); // mark node red when it has parent
  Result.fPair.Key := key;
  Inc(fCount);
end;

function TRedBlackTreeBase<TKey, TValue>.AddNode(const key: TKey; allowReplace: Boolean): PNode;
var
  node: PNode;
  newNode: Pointer;
  compareResult, i: Integer;
begin
  if Assigned(fRoot) then
  begin
    node := Pointer(fRoot);
    repeat
      compareResult := IComparer<TKey>(fComparer).Compare(node.fPair.Key, key);
      if compareResult = 0 then Break;
      i := compareResult shr 31; // left -> 0, right -> 1
      if Assigned(PRedBlackTreeNode(node).fChilds[i]) then
        node := PNode(PRedBlackTreeNode(node).fChilds[i])
      else
      begin
        newNode := CreateNode(key, node);
        PRedBlackTreeNode(node).fChilds[i] := newNode;
        FixupAfterInsert(newNode);
        Exit(newNode);
      end;
    until False;
    if allowReplace then
      // to indicate that an existing node was returned the lowest bit is set
      // the caller needs to zero it again to work with the pointer
      Exit(Pointer(IntPtr(node) or 1))
    else
      Exit(nil);
  end
  else
  begin
    Result := CreateNode(key, nil);
    fRoot := Pointer(Result);
  end;
end;

procedure TRedBlackTreeBase<TKey, TValue>.Clear;
begin
  inherited Clear;
  fStorage.Capacity := 0;
end;

procedure TRedBlackTreeBase<TKey, TValue>.DeleteNode(node: PNode);
var
  next: PNode;
begin
  if Assigned(node.Left) and Assigned(node.Right) then
  begin
    // inline node.Next -> we know that it will be node.Right.LeftMost
    next := node.Right;
    while Assigned(next.Left) do
      next := next.Left;
    node.fPair.Key := next.fPair.Key;
    node.fPair.Value := next.fPair.Value;
    node := next;
  end;
  node.fPair.Key := Default(TKey);
  node.fPair.Value := Default(TValue);
  inherited Delete(PRedBlackTreeNode(node));
end;

function TRedBlackTreeBase<TKey, TValue>.FindNode(const key: TKey): PNode;
var
  root, node: PNode;
  compareResult, i: Integer;
begin
  root := Pointer(fRoot);
  if Assigned(root) then
  begin
    node := root;
    repeat
      compareResult := IComparer<TKey>(fComparer).Compare(node.fPair.Key, key);
      if compareResult = 0 then Break;
      i := compareResult shr 31; // left -> 0, right -> 1
      node := PNode(PRedBlackTreeNode(node).fChilds[i]);
    until not Assigned(node);
    Result := node;
  end
  else
    Result := root;
end;

function TRedBlackTreeBase<TKey, TValue>.GetCapacity: Integer;
begin
  Result := fStorage.Capacity;
end;

procedure TRedBlackTreeBase<TKey, TValue>.SetCapacity(value: Integer);
begin
  if value >= fCount then
    fStorage.Capacity := value
  else
    RaiseHelper.ArgumentOutOfRange(ExceptionArgument.value, ExceptionResource.ArgumentOutOfRange_Capacity);
end;

procedure TRedBlackTreeBase<TKey, TValue>.TrimExcess;
begin
  SetCapacity(fCount);
end;

{$ENDREGION}


{$REGION 'TRedBlackTree<TKey, TValue>'}

function TRedBlackTree<TKey, TValue>.Add(const key: TKey; const value: TValue): Boolean;
var
  node: PNode;
begin
  node := AddNode(key, False);
  if node = nil then Exit(Boolean(Pointer(node)));
  node.fPair.Value := value;
  Result := True;
end;

function TRedBlackTree<TKey, TValue>.AddOrSet(const key: TKey; const value: TValue): Boolean;
var
  node: PNode;
begin
  node := AddNode(key, True);
  node := Pointer(IntPtr(node) and not 1);
  node.fPair.Value := value;
  Result := True;
end;

function TRedBlackTree<TKey, TValue>.Delete(const key: TKey): Boolean;
var
  node: PNode;
begin
  if fCount = 0 then
    Exit(False);
  node := FindNode(key);
  Result := Assigned(node);
  if Result then
    DeleteNode(node);
end;

function TRedBlackTree<TKey, TValue>.Exists(const key: TKey): Boolean;
var
  node: PNode;
begin
  node := FindNode(key);
  Result := Assigned(node);
end;

function TRedBlackTree<TKey, TValue>.Find(const key: TKey; var foundValue: TValue): Boolean;
var
  node: PNode;
begin
  if fCount = 0 then
    Exit(False);
  node := FindNode(key);
  Result := Assigned(node);
  if Result then
    foundValue := node.fPair.Value
  else
    foundValue := Default(TValue);
end;

function TRedBlackTree<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>; //FI:W521
begin
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
    fEnumerator := TBinaryTree.TEnumerator.Create(fRoot, tmInOrder);
end;

function TRedBlackTree<TKey, TValue>.GetRoot: PNode;
begin
  Result := Pointer(fRoot);
end;

function TRedBlackTree<TKey, TValue>.ToArray: TArray<TPair<TKey, TValue>>;
var
  node: PBinaryTreeNode;
  i: Integer;
begin
  SetLength(Result, fCount);
  if fCount > 0 then
  begin
    node := fRoot.LeftMost;
    for i := 0 to fCount - 1 do
    begin
      Result[i].Key := PNode(node).fPair.Key;
      Result[i].Value := PNode(node).fPair.Value;
      node := node.Next;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TRedBlackTree<TKey, TValue>.TEnumerator'}

function TRedBlackTree<TKey, TValue>.TEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  Result.Key := PNode(fEnumerator.fCurrent).fPair.Key;
  Result.Value := PNode(fEnumerator.fCurrent).fPair.Value;
end;

function TRedBlackTree<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


end.
