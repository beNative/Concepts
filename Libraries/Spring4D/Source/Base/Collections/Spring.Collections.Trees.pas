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

unit Spring.Collections.Trees;

{$I Spring.inc}

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Spring.Collections,
  Spring.Collections.Base;

type
  TNodeColor = (Black, Red);

  TBucketIndex = record
    Row, Pos: NativeUInt;
  end;

  PBinaryTreeNode = ^TBinaryTreeNode;
  TBinaryTreeNode = record
  strict private
    fLeft, fParent, fRight: PBinaryTreeNode;
    function GetLeftMost: PBinaryTreeNode;
    function GetRightMost: PBinaryTreeNode;
    function GetNext: PBinaryTreeNode;
    function GetPrev: PBinaryTreeNode;
    function GetHeight: Integer;
  public
    property Left: PBinaryTreeNode read fLeft;
    property Parent: PBinaryTreeNode read fParent;
    property Right: PBinaryTreeNode read fRight;

    property LeftMost: PBinaryTreeNode read GetLeftMost;
    property RightMost: PBinaryTreeNode read GetRightMost;
    property Next: PBinaryTreeNode read GetNext;
    property Prev: PBinaryTreeNode read GetPrev;

    property Height: Integer read GetHeight;
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

  IBinaryTree<T> = interface(IBinaryTree)
    ['{06E837A5-29B7-4F33-AC5C-46BC82F00D15}']
    function Add(const key: T): Boolean;
    function Delete(const key: T): Boolean;
    function Exists(const ley: T): Boolean;
    function Find(const key: T; out value: T): Boolean;
    procedure Clear;

    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;
  end;

  IBinaryTree<TKey, TValue> = interface(IBinaryTree)
    ['{7F554520-BD51-4B53-953B-61B43ED6D59E}']
    function Add(const key: TKey; const value: TValue): Boolean;
    function AddOrSet(const key: TKey; const value: TValue): Boolean;
    function Delete(const key: TKey): Boolean;
    function Exists(const key: TKey): Boolean;
    function Find(const key: TKey; out foundValue: TValue): Boolean;
    procedure Clear;

    function GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
    function ToArray: TArray<TPair<TKey,TValue>>;
  end;

  TBinaryTree = class abstract(TInterfacedObject)
  protected
    fRoot: Pointer;
    fCount: Integer;

  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetHeight: Integer;
    function GetRoot: PBinaryTreeNode;
  {$ENDREGION}

    function GetBucketIndex(index: NativeUInt): TBucketIndex;
  end;

  TRedBlackTree = class abstract(TBinaryTree)
  private
    type
      PRedBlackTreeNode = ^TRedBlackTreeNode;
      TRedBlackTreeNode = record
      private
        fLeft, fParent, fRight: PRedBlackTreeNode;
        fColor: TNodeColor;
        function GetIsBlack: Boolean; inline;
        procedure SetLeft(const value: PRedBlackTreeNode);
        procedure SetParent(const value: PRedBlackTreeNode);
        procedure SetRight(const value: PRedBlackTreeNode);
      public
        property Left: PRedBlackTreeNode read fLeft write SetLeft;
        property Parent: PRedBlackTreeNode read fParent write SetParent;
        property Right: PRedBlackTreeNode read fRight write SetRight;
        property IsBlack: Boolean read GetIsBlack;
      end;
  private
    procedure InsertLeft(node, newNode: PRedBlackTreeNode);
    procedure InsertRight(node, newNode: PRedBlackTreeNode);
    procedure RotateLeft(node: PRedBlackTreeNode);
    procedure RotateRight(node: PRedBlackTreeNode);
    procedure FixupAfterInsert(node: PRedBlackTreeNode);
    procedure FixupAfterDelete(node: PRedBlackTreeNode);
    procedure Delete(node: PRedBlackTreeNode);
    procedure SetRoot(value: PRedBlackTreeNode);
    property Root: PRedBlackTreeNode write SetRoot;
  protected
    procedure FreeNode(node: Pointer); virtual; abstract;
  public
    destructor Destroy; override;

    procedure Clear; virtual;
  end;

  PRedBlackTreeNode = TRedBlackTree.PRedBlackTreeNode;

  TRedBlackTreeNode<T> = record
  private
    fLeft, fParent, fRight: PRedBlackTreeNode;
    fColor: TNodeColor;
    fKey: T;
  public
    property Left: PRedBlackTreeNode read fLeft;
    property Parent: PRedBlackTreeNode read fParent;
    property Right: PRedBlackTreeNode read fRight;
    property Color: TNodeColor read fColor;
    property Key: T read fKey;
  end;

  TRedBlackTreeNode<TKey, TValue> = record
  private
    fLeft, fParent, fRight: PRedBlackTreeNode;
    fColor: TNodeColor;
    fKey: TKey;
    fValue: TValue;
  public
    property Left: PRedBlackTreeNode read fLeft;
    property Parent: PRedBlackTreeNode read fParent;
    property Right: PRedBlackTreeNode read fRight;
    property Color: TNodeColor read fColor;
    property Key: TKey read fKey;
    property Value: TValue read fValue write fValue;
  end;

  TRedBlackTreeNodeHelper<T> = record
  type
    TNode = TRedBlackTreeNode<T>;
    PNode = ^TNode;
  end;

  TRedBlackTreeNodeHelper<TKey, TValue> = record
  type
    TNode = TRedBlackTreeNode<TKey, TValue>;
    PNode = ^TNode;
  end;

  IRedBlackTree<T> = interface(IBinaryTree<T>)
    ['{59BB2B37-D85F-4092-8E80-1EFEE1D2E8F8}']
    function FindNode(const value: T): TRedBlackTreeNodeHelper<T>.PNode;
    procedure DeleteNode(node: TRedBlackTreeNodeHelper<T>.PNode);
  end;

  IRedBlackTree<TKey, TValue> = interface(IBinaryTree<TKey, TValue>)
    ['{8C6F6C1A-92C1-4F4A-A1A9-DD5EA70921CB}']
    function FindNode(const key: TKey): TRedBlackTreeNodeHelper<TKey, TValue>.PNode;
    procedure DeleteNode(node: TRedBlackTreeNodeHelper<TKey, TValue>.PNode);
  end;

  TRedBlackTree<T> = class(TRedBlackTree, IBinaryTree<T>, IRedBlackTree<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fTree: TRedBlackTree;
        fCurrentNode: PBinaryTreeNode;
        fFinished: Boolean;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const tree: TRedBlackTree);
        function MoveNext: Boolean; override;
      end;
      PNode = TRedBlackTreeNodeHelper<T>.PNode;
  private
    fStorage: TArray<TArray<TRedBlackTreeNode<T>>>;
    procedure Grow;
    procedure DestroyNode(node: PNode);
  protected
    fComparer: IComparer<T>;
    function CreateNode(const key: T): PNode;
    function FindNode(const key: T): PNode;
    procedure DeleteNode(node: PNode);
    procedure FreeNode(node: Pointer); override;
  public
    constructor Create; overload;
    constructor Create(const comparer: IComparer<T>); overload;

    procedure Clear; override;

    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;

  {$REGION 'Implements IBinaryTree<T>'}
    function Add(const key: T): Boolean;
    function Delete(const key: T): Boolean;
    function Exists(const key: T): Boolean;
    function Find(const key: T; out value: T): Boolean;
  {$ENDREGION}
  end;

  TRedBlackTree<TKey, TValue> = class(TRedBlackTree,
    IBinaryTree<TKey, TValue>, IRedBlackTree<TKey, TValue>)
  private
    type
      TEnumerator = class(TEnumeratorBase<TPair<TKey,TValue>>)
      private
        fTree: TRedBlackTree;
        fCurrentNode: PBinaryTreeNode;
        fFinished: Boolean;
      protected
        function GetCurrent: TPair<TKey,TValue>; override;
      public
        constructor Create(const tree: TRedBlackTree);
        function MoveNext: Boolean; override;
      end;
      PNode = TRedBlackTreeNodeHelper<TKey,TValue>.PNode;
  private
    fStorage: TArray<TArray<TRedBlackTreeNode<TKey, TValue>>>;
    function InternalAdd(const key: TKey; const value: TValue; allowReplace: Boolean): Boolean;
    procedure Grow;
    procedure DestroyNode(node: PNode);
  protected
    fComparer: IComparer<TKey>;
    function CreateNode(const key: TKey; const value: TValue): PNode;
    function FindNode(const key: TKey): PNode;
    procedure DeleteNode(node: PNode);
    procedure FreeNode(node: Pointer); override;
  public
    constructor Create; overload;
    constructor Create(const comparer: IComparer<TKey>); overload;

    procedure Clear; override;

    function GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
    function ToArray: TArray<TPair<TKey,TValue>>;

  {$REGION 'Implements IBinaryTree<TKey, TValue>'}
    function Add(const key: TKey; const value: TValue): Boolean;
    function AddOrSet(const key: TKey; const value: TValue): Boolean;
    function Delete(const key: TKey): Boolean;
    function Exists(const key: TKey): Boolean;
    function Find(const key: TKey; out foundValue: TValue): Boolean;
  {$ENDREGION}
  end;

  TTest = TRedBlackTree<Integer,string>;

implementation

uses
  Math;

const
  DefaultBucketSize = 64;


{$REGION 'TBinaryTreeNode'}

function TBinaryTreeNode.GetHeight: Integer;
begin
  if Assigned(@Self) then
    Result := Max(fLeft.Height, fRight.Height) + 1
  else
    Result := 0;
end;

function TBinaryTreeNode.GetLeftMost: PBinaryTreeNode;
begin
  Result := @Self;
  if Result = nil then Exit;
  while Assigned(Result.Left) do
    Result := Result.Left;
end;

function TBinaryTreeNode.GetRightMost: PBinaryTreeNode;
begin
  Result := @Self;
  if Result = nil then Exit;
  while Assigned(Result.Right) do
    Result := Result.Right;
end;

function TBinaryTreeNode.GetNext: PBinaryTreeNode;
var
  node: PBinaryTreeNode;
begin
  if Assigned(Right) then
    Exit(Right.GetLeftMost);
  Result := Parent;
  node := @Self;
  while Assigned(Result) and (node = Result.Right) do
  begin
    node := Result;
    Result := Result.Parent;
  end;
end;

function TBinaryTreeNode.GetPrev: PBinaryTreeNode;
var
  node: PBinaryTreeNode;
begin
  if Assigned(Left) then
    Exit(Left.GetRightMost);
  Result := Parent;
  node := @Self;
  while Assigned(Result) and (node = Result.Left) do
  begin
    node := Result;
    Result := Result.Parent;
  end;
end;

{$ENDREGION}


{$REGION 'TRedBlackTree.TRedBlackTreeNode'}

function TRedBlackTree.TRedBlackTreeNode.GetIsBlack: Boolean;
begin
  Result := (@Self = nil) or (fColor = Black);
end;

procedure TRedBlackTree.TRedBlackTreeNode.SetLeft(const value: PRedBlackTreeNode);
begin
  fLeft := value;
  if Assigned(value) then
    value.fParent := @Self;
end;

procedure TRedBlackTree.TRedBlackTreeNode.SetParent(const value: PRedBlackTreeNode);
begin
  Assert(not Assigned(value));
  if Assigned(fParent) then
    if fParent.fLeft = @Self then
      fParent.fLeft := nil
    else if fParent.fRight = @Self then
      fParent.fRight := nil;
  fParent := nil;
end;

procedure TRedBlackTree.TRedBlackTreeNode.SetRight(const value: PRedBlackTreeNode);
begin
  fRight := value;
  if Assigned(value) then
    value.fParent := @Self;
end;

{$ENDREGION}


{$REGION 'TBinaryTree'}

function TBinaryTree.GetBucketIndex(index: NativeUInt): TBucketIndex;
begin
  Result.Row := index div NativeUInt(DefaultBucketSize);
  Result.Pos := index mod NativeUInt(DefaultBucketSize);
end;

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


{$REGION 'TRedBlackTree'}

destructor TRedBlackTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TRedBlackTree.Clear;
begin
  fRoot := nil;
  fCount := 0;
end;

procedure TRedBlackTree.Delete(node: PRedBlackTreeNode);
var
  child: PRedBlackTreeNode;
begin
  try
    if Assigned(node.Left) then
      child := node.Left
    else
      child := node.Right;

    // node has a child
    if Assigned(child) then
    begin
      if not Assigned(node.Parent) then
        Root := child
      else if node.Parent.Left = node then
        node.Parent.Left := child
      else
        node.Parent.Right := child;

      if node.IsBlack then
        FixupAfterDelete(child);
    end else
    // node is the root
    if not Assigned(node.Parent) then
      fRoot := nil
    else
    begin
      if node.IsBlack then
        FixupAfterDelete(node);
      // unlink the node which could not be done before
      // because DeleteFixUp requires Parent to be set
      node.Parent := nil;
    end;
  finally
    FreeNode(node);
  end;
end;

procedure TRedBlackTree.FixupAfterDelete(node: PRedBlackTreeNode);
var
  sibling: PRedBlackTreeNode;
begin
  while (node <> fRoot) and node.IsBlack do
  begin
    // node is a left child
    if node = node.Parent.Left then
    begin
      sibling := node.Parent.Right;

      // case 1: sibling is red
      if not sibling.IsBlack then
      begin
        sibling.fColor := Black;
        node.Parent.fColor := Red;
        RotateLeft(node.Parent);
        sibling := node.Parent.Right;
      end;

      // case 2: both of siblings children are black
      if sibling.Left.IsBlack and sibling.Right.IsBlack then
      begin
        sibling.fColor := Red;
        node := node.Parent;
      end else
      begin
        // case 3: siblings right child is black
        if sibling.Right.IsBlack then
        begin
          sibling.Left.fColor := Black;
          sibling.fColor := Red;
          RotateRight(sibling);
          sibling := node.Parent.Right;
        end;

        // case 4: siblings right child is red
        sibling.fColor := node.Parent.fColor;
        node.Parent.fColor := Black;
        sibling.Right.fColor := Black;
        RotateLeft(node.Parent);
        node := fRoot;
      end;
    end else
    // node is a right child
    begin
      sibling := node.Parent.Left;

      // case 1: sibling is red
      if not sibling.IsBlack then
      begin
        sibling.fColor := Black;
        node.Parent.fColor := Red;
        RotateRight(node.Parent);
        sibling := node.Parent.Left;
      end;

      // case 2: both of siblings children are black
      if sibling.Right.IsBlack and sibling.Left.IsBlack then
      begin
        sibling.fColor := Red;
        node := node.Parent;
      end else
      begin
        // case 3: siblings left child is black
        if sibling.Left.IsBlack then
        begin
          sibling.Right.fColor := Black;
          sibling.fColor := Red;
          RotateLeft(sibling);
          sibling := node.Parent.Left;
        end;

        // case 4: siblings left child is red
        sibling.fColor := node.Parent.fColor;
        node.Parent.fColor := Black;
        sibling.Left.fColor := Black;
        RotateRight(node.Parent);
        node := fRoot;
      end;
    end;
  end;

  node.fColor := Black;
end;

procedure TRedBlackTree.FixupAfterInsert(node: PRedBlackTreeNode);
var
  uncle: PRedBlackTreeNode;
begin
  node.fColor := Red;

  while Assigned(node) and (node <> fRoot) and not node.Parent.IsBlack do
  begin
    // if nodes parent is the left child of its parent
    if node.Parent = node.Parent.Parent.Left then
    begin
      uncle := node.Parent.Parent.Right;

      // case 1: uncle is red
      if not uncle.IsBlack then
      begin
        node.Parent.fColor := Black;
        uncle.fColor := Black;
        node.Parent.Parent.fColor := Red;
        node := node.Parent.Parent;
      end else
      // case 2: uncle is black and node is a right child
      if node = node.Parent.Right then
      begin
        node := node.Parent;
        RotateLeft(node);
      end else
      // case 3: uncle is black and node is a left child
      begin
        node.Parent.fColor := Black;
        node.Parent.Parent.fColor := Red;
        RotateRight(node.Parent.Parent);
      end;
    end else
    // if nodes parent is the right child of its parent
    begin
      uncle := node.Parent.Parent.Left;

      // case 1: uncle is red
      if not uncle.IsBlack then
      begin
        node.Parent.fColor := Black;
        uncle.fColor := Black;
        node.Parent.Parent.fColor := Red;
        node := node.Parent.Parent;
      end else
      // case 2: uncle is black and node is a left child
      if node = node.Parent.Left then
      begin
        node := node.Parent;
        RotateRight(node);
      end else
      // case 3: uncle is black and node is right child
      begin
        node.Parent.fColor := Black;
        node.Parent.Parent.fColor := Red;
        RotateLeft(node.Parent.Parent);
      end
    end
  end;

  PRedBlackTreeNode(fRoot).fColor := Black;
end;

procedure TRedBlackTree.InsertLeft(node, newNode: PRedBlackTreeNode);
begin
  Assert(not Assigned(node.Left));
  node.Left := newNode;
  FixupAfterInsert(newNode);
end;

procedure TRedBlackTree.InsertRight(node, newNode: PRedBlackTreeNode);
begin
  Assert(not Assigned(node.Right));
  node.Right := newNode;
  FixupAfterInsert(newNode);
end;

procedure TRedBlackTree.RotateLeft(node: PRedBlackTreeNode);
var
  right: PRedBlackTreeNode;
begin
  right := node.Right;
  node.Right := right.Left;

  if not Assigned(node.Parent) then
    Root := right
  else if node.Parent.Left = node then
    node.Parent.Left := right
  else
    node.Parent.Right := right;

  right.Left := node;
end;

procedure TRedBlackTree.RotateRight(node: PRedBlackTreeNode);
var
  left: PRedBlackTreeNode;
begin
  left := node.Left;
  node.Left := left.Right;

  if not Assigned(node.Parent) then
    Root := left
  else if node.Parent.Right = node then
    node.Parent.Right := left
  else
    node.Parent.Left := left;

  left.Right := node;
end;

procedure TRedBlackTree.SetRoot(value: PRedBlackTreeNode);
begin
  fRoot := value;
  if Assigned(value) then
    value.Parent := nil;
end;

{$ENDREGION}


{$REGION 'TRedBlackTree<T>'}

constructor TRedBlackTree<T>.Create;
begin
  Create(nil);
end;

constructor TRedBlackTree<T>.Create(const comparer: IComparer<T>);
begin
  inherited Create;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := TComparer<T>.Default;
end;

function TRedBlackTree<T>.CreateNode(const key: T): PNode;
var
  index: TBucketIndex;
begin
  index := GetBucketIndex(fCount);
  if index.Pos = 0 then
    Grow;

  Result := @fStorage[index.Row, index.Pos];
  Result.fKey := key;
  Inc(fCount);
end;

procedure TRedBlackTree<T>.DestroyNode(node: PNode);
var
  index: TBucketIndex;
  lastNode: PNode;
begin
  if fCount > 1 then
  begin
    index := GetBucketIndex(fCount - 1);
    lastNode := @fStorage[index.Row, index.Pos];

    if lastNode <> node then
    begin
      node^ := lastNode^;
      if Assigned(node.fLeft) then
        node.fLeft.fParent := PRedBlackTreeNode(node);
      if Assigned(node.fRight) then
        node.fRight.fParent := PRedBlackTreeNode(node);
      if Assigned(node.fParent) then
      begin
        if node.fParent.fLeft = PRedBlackTreeNode(lastNode) then
          node.fParent.fLeft := PRedBlackTreeNode(node)
        else if node.fParent.fRight = PRedBlackTreeNode(lastNode) then
          node.fParent.fRight := PRedBlackTreeNode(node)
      end
      else
        fRoot := PRedBlackTreeNode(node);
    end;

    node := lastNode;
  end;
  Dec(fCount);
  node.fLeft := nil;
  node.fParent := nil;
  node.fRight := nil;
  node.fColor := Black;
  node.fKey := Default(T);
end;

procedure TRedBlackTree<T>.FreeNode(node: Pointer);
begin
  DestroyNode(PNode(node));
end;

function TRedBlackTree<T>.Add(const key: T): Boolean;
var
  node: PRedBlackTreeNode;
  compareResult: Integer;
begin
  if not Assigned(fRoot) then
  begin
    fRoot := PRedBlackTreeNode(CreateNode(key));
    Exit(True);
  end;

  node := fRoot;
  while True do
  begin
    compareResult := fComparer.Compare(key, PNode(node).Key);

    if compareResult > 0 then
      if Assigned(node.Right) then
        node := node.Right
      else
      begin
        InsertRight(node, PRedBlackTreeNode(CreateNode(key)));
        Exit(True);
      end
    else if compareResult < 0 then
      if Assigned(node.Left) then
        node := node.Left
      else
      begin
        InsertLeft(node, PRedBlackTreeNode(CreateNode(key)));
        Exit(True);
      end
    else
      Exit(False);
  end;
end;

procedure TRedBlackTree<T>.Clear;
begin
  inherited Clear;
  SetLength(fStorage, 0);
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

procedure TRedBlackTree<T>.DeleteNode(node: PNode);
var
  next: PNode;
begin
  if Assigned(node.Left) and Assigned(node.Right) then
  begin
    next := PNode(PBinaryTreeNode(node).Next);
    node.fKey := next.Key;
    node := next;
  end;
  inherited Delete(PRedBlackTreeNode(node));
end;

function TRedBlackTree<T>.Exists(const key: T): Boolean;
begin
  Result := (fCount > 0) and Assigned(FindNode(key));
end;

function TRedBlackTree<T>.Find(const key: T; out value: T): Boolean;
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

function TRedBlackTree<T>.FindNode(const key: T): PNode;
var
  compareResult: Integer;
begin
  Result := PNode(fRoot);
  while Assigned(Result) do
  begin
    compareResult := fComparer.Compare(key, Result.Key);

    if compareResult < 0 then
      Result := PNode(Result.Left)
    else if compareResult > 0 then
      Result := PNode(Result.Right)
    else
      Exit;
  end;
end;

function TRedBlackTree<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TRedBlackTree<T>.Grow;
var
  index: TBucketIndex;
begin
  index := GetBucketIndex(fCount);
  SetLength(fStorage, index.Row + 1);
  SetLength(fStorage[index.Row], DefaultBucketSize);
end;

function TRedBlackTree<T>.ToArray: TArray<T>;
var
  node: PBinaryTreeNode;
  i: Integer;
begin
  SetLength(Result, fCount);
  if fCount > 0 then
    node := PBinaryTreeNode(fRoot).LeftMost;
  for i := 0 to fCount - 1 do
  begin
    Result[i] := PNode(node).Key;
    node := node.Next;
  end;
end;

{$ENDREGION}


{$REGION 'TRedBlackTree<T>.TEnumerator'}

constructor TRedBlackTree<T>.TEnumerator.Create(
  const tree: TRedBlackTree);
begin
  inherited Create;
  fTree := tree;
end;

function TRedBlackTree<T>.TEnumerator.GetCurrent: T;
begin
  Result := PNode(fCurrentNode).Key;
end;

function TRedBlackTree<T>.TEnumerator.MoveNext: Boolean;
begin
  if (fTree.fCount = 0) or fFinished then
    Exit(False);
  if not Assigned(fCurrentNode) then
    fCurrentNode := PBinaryTreeNode(fTree.fRoot).LeftMost
  else
    fCurrentNode := fCurrentNode.Next;
  Result := Assigned(fCurrentNode);
  fFinished := not Result;
end;

{$ENDREGION}


{$REGION 'TRedBlackTree<TKey, TValue>'}

constructor TRedBlackTree<TKey, TValue>.Create;
begin
  Create(nil);
end;

constructor TRedBlackTree<TKey, TValue>.Create(const comparer: IComparer<TKey>);
begin
  inherited Create;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := TComparer<TKey>.Default;
end;

function TRedBlackTree<TKey, TValue>.CreateNode(const key: TKey;
  const value: TValue): PNode;
var
  index: TBucketIndex;
begin
  index := GetBucketIndex(fCount);
  if index.Pos = 0 then
    Grow;

  Result := @fStorage[index.Row, index.Pos];
  Result.fKey := key;
  Result.fValue := value;
  Inc(fCount);
end;

procedure TRedBlackTree<TKey, TValue>.DestroyNode(node: PNode);
var
  index: TBucketIndex;
  lastNode: PNode;
begin
  if fCount > 1 then
  begin
    index := GetBucketIndex(fCount - 1);
    lastNode := @fStorage[index.Row, index.Pos];

    if lastNode <> node then
    begin
      node^ := lastNode^;
      if Assigned(node.fLeft) then
        node.fLeft.fParent := PRedBlackTreeNode(node);
      if Assigned(node.fRight) then
        node.fRight.fParent := PRedBlackTreeNode(node);
      if Assigned(node.fParent) then
      begin
        if node.fParent.fLeft = PRedBlackTreeNode(lastNode) then
          node.fParent.fLeft := PRedBlackTreeNode(node)
        else if node.fParent.fRight = PRedBlackTreeNode(lastNode) then
          node.fParent.fRight := PRedBlackTreeNode(node)
      end
      else
        fRoot := PRedBlackTreeNode(node);
    end;

    node := lastNode;
  end;
  Dec(fCount);
  node.fLeft := nil;
  node.fParent := nil;
  node.fRight := nil;
  node.fColor := Black;
  node.fKey := Default(TKey);
  node.fValue := Default(TValue);
end;

procedure TRedBlackTree<TKey, TValue>.FreeNode(node: Pointer);
begin
  DestroyNode(PNode(node));
end;

function TRedBlackTree<TKey, TValue>.Add(const key: TKey;
  const value: TValue): Boolean;
begin
  Result := InternalAdd(key, value, False);
end;

function TRedBlackTree<TKey, TValue>.AddOrSet(const key: TKey;
  const value: TValue): Boolean;
begin
  Result := InternalAdd(key, value, True);
end;

procedure TRedBlackTree<TKey, TValue>.Clear;
begin
  inherited Clear;
  SetLength(fStorage, 0);
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

procedure TRedBlackTree<TKey, TValue>.DeleteNode(node: PNode);
var
  next: PNode;
begin
  if Assigned(node.Left) and Assigned(node.Right) then
  begin
    next := PNode(PBinaryTreeNode(node).Next);
    node.fKey := next.Key;
    node.fValue := next.Value;
    node := next;
  end;
  inherited Delete(PRedBlackTreeNode(node));
end;

function TRedBlackTree<TKey, TValue>.Exists(const key: TKey): Boolean;
begin
  Result := (fCount > 0) and Assigned(FindNode(key));
end;

function TRedBlackTree<TKey, TValue>.Find(const key: TKey;
  out foundValue: TValue): Boolean;
var
  node: PNode;
begin
  if fCount = 0 then
    Exit(False);
  node := FindNode(key);
  Result := Assigned(node);
  if Result then
    foundValue := node.Value
  else
    foundValue := Default(TValue);
end;

function TRedBlackTree<TKey, TValue>.FindNode(const key: TKey): PNode;
var
  compareResult: Integer;
begin
  Result := PNode(fRoot);
  while Assigned(Result) do
  begin
    compareResult := fComparer.Compare(key, Result.Key);

    if compareResult < 0 then
      Result := PNode(Result.Left)
    else if compareResult > 0 then
      Result := PNode(Result.Right)
    else
      Exit;
  end;
end;

function TRedBlackTree<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TRedBlackTree<TKey, TValue>.Grow;
var
  index: TBucketIndex;
begin
  index := GetBucketIndex(fCount);
  SetLength(fStorage, index.Row + 1);
  SetLength(fStorage[index.Row], DefaultBucketSize);
end;

function TRedBlackTree<TKey, TValue>.InternalAdd(const key: TKey;
  const value: TValue; allowReplace: Boolean): Boolean;
var
  node: PRedBlackTreeNode;
  compareResult: Integer;
begin
  if not Assigned(fRoot) then
  begin
    fRoot := PRedBlackTreeNode(CreateNode(key, value));
    Exit(True);
  end;

  node := fRoot;
  while True do
  begin
    compareResult := fComparer.Compare(key, PNode(node).Key);

    if compareResult > 0 then
      if Assigned(node.Right) then
        node := node.Right
      else
      begin
        InsertRight(node, PRedBlackTreeNode(CreateNode(key, value)));
        Exit(True);
      end
    else if compareResult < 0 then
      if Assigned(node.Left) then
        node := node.Left
      else
      begin
        InsertLeft(node, PRedBlackTreeNode(CreateNode(key, value)));
        Exit(True);
      end
    else
      if allowReplace then
      begin
        PNode(node).Value := value;
        Exit(True);
      end
      else
        Exit(False);
  end;
end;

function TRedBlackTree<TKey, TValue>.ToArray: TArray<TPair<TKey, TValue>>;
var
  node: PBinaryTreeNode;
  i: Integer;
begin
  SetLength(Result, fCount);
  if fCount > 0 then
    node := PBinaryTreeNode(fRoot).LeftMost;
  for i := 0 to fCount - 1 do
  begin
    Result[i].Key := PNode(node).Key;
    Result[i].Value := PNode(node).Value;
    node := node.Next;
  end;
end;

{$ENDREGION}


{$REGION 'TRedBlackTree<TKey, TValue>.TEnumerator'}

constructor TRedBlackTree<TKey, TValue>.TEnumerator.Create(
  const tree: TRedBlackTree);
begin
  inherited Create;
  fTree := tree;
end;

function TRedBlackTree<TKey, TValue>.TEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  Result.Key := PNode(fCurrentNode).Key;
  Result.Value := PNode(fCurrentNode).Value;
end;

function TRedBlackTree<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  if (fTree.fCount = 0) or fFinished then
    Exit(False);
  if not Assigned(fCurrentNode) then
    fCurrentNode := PBinaryTreeNode(fTree.fRoot).LeftMost
  else
    fCurrentNode := fCurrentNode.Next;
  Result := Assigned(fCurrentNode);
  fFinished := not Result;
end;

{$ENDREGION}


end.
