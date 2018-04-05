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

unit Spring.Collections.Sets;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Spring.Collections,
{$IFNDEF DELPHI2010}
  Spring.Collections.Trees,
{$ENDIF}
  Spring.Collections.Base;

type
  /// <summary>
  ///   The abstract base class for all set implementations.
  /// </summary>
  TSetBase<T> = class abstract(TCollectionBase<T>)
  protected
    class function CreateSet: ISet<T>; virtual; abstract;
  public
    procedure ExceptWith(const other: IEnumerable<T>);
    procedure IntersectWith(const other: IEnumerable<T>);
    procedure UnionWith(const other: IEnumerable<T>);
    function IsSubsetOf(const other: IEnumerable<T>): Boolean;
    function IsSupersetOf(const other: IEnumerable<T>): Boolean;
    function SetEquals(const other: IEnumerable<T>): Boolean;
    function Overlaps(const other: IEnumerable<T>): Boolean;
  end;

  /// <summary>
  ///   Represents a set of values.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of elements in the hash set.
  /// </typeparam>
  THashSet<T> = class(TSetBase<T>, ISet<T>)
  private
    type
      TGenericDictionary = Generics.Collections.TDictionary<T, Integer>;
  private
    fDictionary: TGenericDictionary;
    procedure DoKeyNotify(Sender: TObject; const Item: T;
      Action: TCollectionNotification);
  protected
    class function CreateSet: ISet<T>; override;
    procedure AddInternal(const item: T); override;
    function GetCount: Integer; override;
  public
    constructor Create; overload; override;
    constructor Create(const comparer: IEqualityComparer<T>); overload;
    destructor Destroy; override;

    function GetEnumerator: IEnumerator<T>; override;

    function Add(const item: T): Boolean;
    function Remove(const item: T): Boolean; override;
    function Extract(const item: T): T; override;
    procedure Clear; override;

    function Contains(const item: T): Boolean; override;
  end;

  TOrderedSet<T> = class(THashSet<T>, IOrderedSet<T>)
  private
    fKeys: IList<T>;
  {$REGION 'Property Accessors'}
    function GetItem(index: Integer): T;
  {$ENDREGION}
  protected
    procedure Changed(const item: T; action: TCollectionChangedAction); override;
  public
    constructor Create; overload; override;
    constructor Create(const comparer: IEqualityComparer<T>); overload;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>; override;
    function ToArray: TArray<T>; override;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    procedure Clear; override;
  {$ENDREGION}

  {$REGION 'Implements IOrderedSet<T>'}
    function IndexOf(const key: T): Integer;
  {$ENDREGION}
  end;

{$IFNDEF DELPHI2010}
  TSortedSet<T> = class(TSetBase<T>, ISet<T>)
  private
    fTree: IRedBlackTree<T>;
    type
      PNode = TRedBlackTreeNodeHelper<T>.PNode;
  protected
    class function CreateSet: ISet<T>; override;
    procedure AddInternal(const item: T); override;
    function GetCount: Integer; override;
  public
    constructor Create; overload; override;
    constructor Create(const comparer: IComparer<T>); overload;

    function GetEnumerator: IEnumerator<T>; override;

    function Add(const item: T): Boolean;
    function Remove(const item: T): Boolean; override;
    function Extract(const item: T): T; override;
    procedure Clear; override;

    function Contains(const item: T): Boolean; override;
  end;
{$ENDIF}

implementation

uses
  Spring,
  Spring.Collections.Extensions,
  Spring.Collections.Lists;


{$REGION 'TSetBase<T>'}

procedure TSetBase<T>.ExceptWith(const other: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    Remove(item);
end;

procedure TSetBase<T>.IntersectWith(const other: IEnumerable<T>);
var
  item: T;
  list: IList<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

{$IFNDEF DELPHI2010}
  list := TCollections.CreateList<T>;
{$ELSE}
  list := TList<T>.Create;
{$ENDIF}
  for item in Self do
    if not other.Contains(item) then
      list.Add(item);

  for item in list do
    Remove(item);
end;

function TSetBase<T>.IsSubsetOf(const other: IEnumerable<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in Self do
    if not other.Contains(item) then
      Exit(False);

  Result := True;
end;

function TSetBase<T>.IsSupersetOf(const other: IEnumerable<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    if not Contains(item) then
      Exit(False);

  Result := True;
end;

function TSetBase<T>.Overlaps(const other: IEnumerable<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    if Contains(item) then
      Exit(True);

  Result := False;
end;

function TSetBase<T>.SetEquals(const other: IEnumerable<T>): Boolean;
var
  item: T;
  localSet: ISet<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  localSet := CreateSet;

  for item in other do
  begin
    localSet.Add(item);
    if not Contains(item) then
      Exit(False);
  end;

  for item in Self do
    if not localSet.Contains(item) then
      Exit(False);

  Result := True;
end;

procedure TSetBase<T>.UnionWith(const other: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    Add(item);
end;

{$ENDREGION}


{$REGION 'THashSet<T>'}

constructor THashSet<T>.Create;
var
  // use variable to pass nil because of codegen bug in XE2 and XE3 in x64
  comparer: IEqualityComparer<T>;
begin
  Create(comparer);
end;

constructor THashSet<T>.Create(const comparer: IEqualityComparer<T>);
begin
  inherited Create;
  fDictionary := TGenericDictionary.Create(comparer);
  fDictionary.OnKeyNotify := DoKeyNotify;
end;

destructor THashSet<T>.Destroy;
begin
  fDictionary.Free;
  inherited Destroy;
end;

class function THashSet<T>.CreateSet: ISet<T>;
begin
  Result := THashSet<T>.Create;
end;

function THashSet<T>.Add(const item: T): Boolean;
begin
  Result := not fDictionary.ContainsKey(item);
  if Result then
    inherited Add(item);
end;

procedure THashSet<T>.AddInternal(const item: T);
begin
  fDictionary.AddOrSetValue(item, 0);
end;

procedure THashSet<T>.Clear;
begin
  fDictionary.Clear;
end;

function THashSet<T>.Contains(const item: T): Boolean;
begin
  Result := fDictionary.ContainsKey(item);
end;

procedure THashSet<T>.DoKeyNotify(Sender: TObject; const Item: T;
  Action: TCollectionNotification);
begin
  Changed(Item, TCollectionChangedAction(Action));
end;

function THashSet<T>.Extract(const item: T): T;
begin
  if fDictionary.ContainsKey(item) then
{$IFDEF DELPHIXE2_UP}
    Result := fDictionary.ExtractPair(item).Key
{$ELSE}
  begin
    Result := item;
    fDictionary.ExtractPair(item);
  end
{$ENDIF}
  else
    Result := Default(T);
end;

function THashSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(fDictionary.Keys);
end;

function THashSet<T>.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

function THashSet<T>.Remove(const item: T): Boolean;
begin
  Result := fDictionary.ContainsKey(item);
  if Result then
    fDictionary.Remove(item);
end;

{$ENDREGION}


{$REGION 'TOrderedSet<T>'}

constructor TOrderedSet<T>.Create;
begin
  inherited Create;
  fKeys := TKeyList<T>.Create(nil);
end;

constructor TOrderedSet<T>.Create(const comparer: IEqualityComparer<T>);
begin
  inherited Create(comparer);
  fKeys := TKeyList<T>.Create(comparer);
end;

procedure TOrderedSet<T>.Clear;
begin
  fKeys.Clear;
  inherited Clear;
end;

procedure TOrderedSet<T>.Changed(const item: T;
  action: TCollectionChangedAction);
begin
  inherited Changed(item, action);
  case action of
    caAdded: fKeys.Add(item);
    caRemoved, caExtracted: fKeys.Remove(item);
  end;
end;

function TOrderedSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := fKeys.GetEnumerator;
end;

function TOrderedSet<T>.GetItem(index: Integer): T;
begin
  Result := fKeys[index];
end;

function TOrderedSet<T>.IndexOf(const key: T): Integer;
begin
  Result := fKeys.IndexOf(key);
end;

function TOrderedSet<T>.ToArray: TArray<T>;
begin
  Result := fKeys.ToArray;
end;

{$ENDREGION}


{$REGION 'TSortedSet<T>'}

{$IFNDEF DELPHI2010}
constructor TSortedSet<T>.Create;
var
  // use variable to pass nil because of codegen bug in XE2 and XE3 in x64
  comparer: IComparer<T>;
begin
  Create(comparer);
end;

constructor TSortedSet<T>.Create(const comparer: IComparer<T>);
begin
  inherited Create;
  fTree := TRedBlackTree<T>.Create(comparer);
end;

class function TSortedSet<T>.CreateSet: ISet<T>;
begin
  Result := TSortedSet<T>.Create;
end;

function TSortedSet<T>.Add(const item: T): Boolean;
begin
  Result := fTree.Add(item);
  if Result then
    Changed(item, caAdded);
end;

procedure TSortedSet<T>.AddInternal(const item: T);
begin
  fTree.Add(item);
  Changed(item, caAdded);
end;

procedure TSortedSet<T>.Clear;
var
  node: PBinaryTreeNode;
begin
  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    Changed(PNode(node).Key, caRemoved);
    node := node.Next;
  end;

  fTree.Clear;
end;

function TSortedSet<T>.Contains(const item: T): Boolean;
begin
  Result := Assigned(fTree.FindNode(item));
end;

function TSortedSet<T>.Extract(const item: T): T;
var
  node: PNode;
begin
  node := fTree.FindNode(item);
  if Assigned(node) then
  begin
    Result := node.Key;
    Changed(Result, caExtracted);
  end
  else
    Result := Default(T);
end;

function TSortedSet<T>.GetCount: Integer;
begin
  Result := fTree.Count;
end;

function TSortedSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := fTree.GetEnumerator;
end;

function TSortedSet<T>.Remove(const item: T): Boolean;
begin
  Result := fTree.Delete(item);
  if Result then
    Changed(item, caRemoved);
end;
{$ENDIF}

{$ENDREGION}


end.
