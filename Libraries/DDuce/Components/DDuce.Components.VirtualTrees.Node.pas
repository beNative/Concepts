{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

{$I DDuce.inc}

unit DDuce.Components.VirtualTrees.Node;

interface

uses
  VirtualTrees;

{
  Documentation
    TVTNode is a type designed to be used as the data structure where each
    treenode in a treeview is pointing to.

    For any treenode (of type PVirtualNode) this can be obtained by the following
    method defined in TBaseVirtualStringTree:
          function GetNodeData<T>(pNode: PVirtualNode): T;

    type
      TMyData = class
        ...
      end;
}
type
  TVTNode<T: class> = class;

  TVTNode<T: class> = class
  public type
    TVTNodeEnumerator<K: T> = record
    strict private
      FCurrent : TVTNode<K>;
      FFirst   : Boolean;
    public
      constructor Create(AVTNode: TVTNode<K>);

      function GetCurrent: TVTNode<K>;

      function MoveNext: Boolean;

      property Current: TVTNode<K>
        read GetCurrent;
    end;

  private
    FTree       : TCustomVirtualStringTree;
    FVNode      : PVirtualNode;
    FData       : T;
    FText       : string;
    FHint       : string;
    FImageIndex : Integer;
    FCheckState : TCheckState;
    FCheckType  : TCheckType;
    FOwnsObject : Boolean;

  private
    function SearchTree(ANode: TVTNode<T>; const AData: T): TVTNode<T>;

  protected
    {$REGION 'property access methods'}
    function GetItem(AIndex: UInt32): TVTNode<T>;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    function GetLevel: Integer;
    function GetIndex: Integer;
    function GetImageIndex: Integer;
    procedure SetImageIndex(const Value: Integer);
    function GetHint: string;
    procedure SetHint(const Value: string);
    function GetCheckState: TCheckState;
    procedure SetCheckState(const Value: TCheckState);
    function GetCheckType: TCheckType;
    procedure SetCheckType(const Value: TCheckType);
    function GetVNode: PVirtualNode;
    procedure SetVNode(const Value: PVirtualNode);
    function GetData: T;
    procedure SetData(const Value: T);
    function GetChildCount: UInt32;
    function GetText: string; virtual;
    procedure SetText(const Value: string); virtual;
    function GetOwnsObject: Boolean;
    procedure SetOwnsObject(AValue: Boolean);
    function GetTree: TCustomVirtualStringTree;
    function GetFocused: Boolean;
    procedure SetFocused(const Value: Boolean);
    function GetSelected: Boolean;
    procedure SetSelected(const Value: Boolean);
    function GetExpanded: Boolean;
    procedure SetExpanded(const Value: Boolean);
    {$ENDREGION}

  public
    constructor Create(
      ATree        : TCustomVirtualStringTree;
      const AData  : T;
      AOwnsObject  : Boolean = True;
      AParentVNode : PVirtualNode = nil;
      const AText  : string = ''
    ); overload; virtual;
    constructor Create(
      ATree        : TCustomVirtualStringTree;
      AOwnsObject  : Boolean = True;
      AParentVNode : PVirtualNode = nil;
      const AText  : string = ''
    ); overload; virtual;
    destructor Destroy; override;

    function GetEnumerator: TVTNodeEnumerator<T>;

    function Add(const AData: T; AOwnsObject: Boolean = True): TVTNode<T>;
    function Find(const AData: T): TVTNode<T>;
    procedure Select(
      AScrollIntoView   : Boolean = True;
      ACenterVertical   : Boolean = True;
      ACenterHorizontal : Boolean = True;
      AExpandNode       : Boolean = True
    );

    property CheckState: TCheckState
      read GetCheckState write SetCheckState;

    property CheckType: TCheckType
      read GetCheckType write SetCheckType;

    property ChildCount: UInt32
      read GetChildCount;

    { User defined data that is associated with the current node. }
    property Data: T
      read GetData write SetData;

    property Expanded: Boolean
      read GetExpanded write SetExpanded;

    property Focused: Boolean
      read GetFocused write SetFocused;

    property Hint: string
      read GetHint write SetHint;

    property ImageIndex: Integer
      read GetImageIndex write SetImageIndex;

    property Index: Integer
      read GetIndex;

    property Items[AIndex: UInt32]: TVTNode<T>
      read GetItem; default;

    property Level: Integer
      read GetLevel;

    { If Data is of a class type, this determines if Data is freed when TVTNode
    instance is freed. }
    property OwnsObject: Boolean
      read GetOwnsObject write SetOwnsObject;

    property Selected: Boolean
      read GetSelected write SetSelected;

    property Text: string
      read GetText write SetText;

    property Tree: TCustomVirtualStringTree
      read GetTree;

    property Visible: Boolean
      read GetVisible write SetVisible;

    { Points to the corresponding node of the virtual treeview. }
    property VNode: PVirtualNode
      read GetVNode write SetVNode;


  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
constructor TVTNode<T>.Create(ATree: TCustomVirtualStringTree; const AData: T;
  AOwnsObject: Boolean; AParentVNode: PVirtualNode; const AText: string);
begin
  FTree       := ATree;
  FData       := AData;
  FOwnsObject := AOwnsObject;
  FText       := AText;
  FImageIndex := -1;
  if not Assigned(AParentVNode) then // create rootnode
    FVNode := FTree.AddChild(nil, Self);
end;

constructor TVTNode<T>.Create(ATree: TCustomVirtualStringTree;
  AOwnsObject: Boolean; AParentVNode: PVirtualNode; const AText: string);
begin
  FTree       := ATree;
  FData       := Default(T);
  FOwnsObject := AOwnsObject;
  FText       := AText;
  if not Assigned(AParentVNode) then // create rootnode
    FVNode := FTree.AddChild(nil, Self);
end;

destructor TVTNode<T>.Destroy;
begin
  if (GetTypekind(T) = tkClass) and OwnsObject then
    TObject(Pointer(@FData)^).Free;
  FTree  := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TVTNode<T>.GetCheckState: TCheckState;
begin
  if Assigned(VNode) then
    FCheckState := VNode.CheckState;
  Result := FCheckState;
end;

procedure TVTNode<T>.SetCheckState(const Value: TCheckState);
begin
  FCheckState := Value;
  if Assigned(VNode) then
    VNode.CheckState := Value;
end;

function TVTNode<T>.GetCheckType: TCheckType;
begin
  if Assigned(VNode) then
    FCheckType := VNode.CheckType;
  Result := FCheckType;
end;

procedure TVTNode<T>.SetCheckType(const Value: TCheckType);
begin
  FCheckType := Value;
  if Assigned(VNode) then
    VNode.CheckType := Value;
end;

function TVTNode<T>.GetChildCount: UInt32;
begin
  if Assigned(VNode) then
    Result := VNode.ChildCount
  else
    Result := 0;
end;

function TVTNode<T>.GetData: T;
begin
  Result := FData;
end;

procedure TVTNode<T>.SetData(const Value: T);
begin
  FData := Value;
end;

function TVTNode<T>.GetExpanded: Boolean;
begin
  Result := vsExpanded in VNode.States;
end;

procedure TVTNode<T>.SetExpanded(const Value: Boolean);
begin
  if Value then
    VNode.States := VNode.States + [vsExpanded]
  else
    VNode.States := VNode.States - [vsExpanded];
end;

function TVTNode<T>.GetFocused: Boolean;
begin
  if Assigned(FTree) then
  begin
    Result := FTree.FocusedNode = FVNode;
  end
  else
  begin
    Result := False;
  end;
end;

procedure TVTNode<T>.SetFocused(const Value: Boolean);
begin
  if Assigned(FTree) then
  begin
    FTree.FocusedNode := FVNode;
  end;
end;

function TVTNode<T>.GetHint: string;
begin
  Result := FHint;
end;

procedure TVTNode<T>.SetHint(const Value: string);
begin
  FHint := Value;
end;

function TVTNode<T>.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;

procedure TVTNode<T>.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
end;

function TVTNode<T>.GetOwnsObject: Boolean;
begin
  Result := FOwnsObject;
end;

procedure TVTNode<T>.SetOwnsObject(AValue: Boolean);
begin
  FOwnsObject := AValue;
end;

function TVTNode<T>.GetSelected: Boolean;
begin
  if Assigned(FTree) then
  begin
    Result := FTree.Selected[FVNode];
  end
  else
  begin
    Result := False;
  end;
  //Result := vsSelected in VNode.States;
end;

procedure TVTNode<T>.SetSelected(const Value: Boolean);
begin
  if Assigned(FTree) then
  begin
    FTree.Selected[FVNode] := not FTree.Selected[FVNode];
  end;
  // remark: We do not add vsSelected to the States of VNode because
  // it will select the node regardless of the tree's MultiSelect setting.
//  if Value then
//    VNode.States := VNode.States + [vsSelected]
//  else
//    VNode.States := VNode.States - [vsSelected];

end;

function TVTNode<T>.GetIndex: Integer;
begin
  if Assigned(VNode) then
    Result := VNode.Index
  else
    Result := 0;
end;

function TVTNode<T>.GetItem(AIndex: UInt32): TVTNode<T>;
var
  I	 : UInt32;
	VN : PVirtualNode;
begin
  //Guard.CheckIndex(VNode.ChildCount, AIndex);
	VN := VNode.FirstChild;
  if AIndex > 0 then
  begin
    for I := 0 to AIndex - 1 do
    begin
      //Guard.CheckNotNull(VN, 'VN');
      VN := VN.NextSibling;
    end;
  end;
	Result := FTree.GetNodeData<TVTNode<T>>(VN);
end;

function TVTNode<T>.GetLevel: Integer;
begin
  Result := FTree.GetNodeLevel(VNode);
end;

function TVTNode<T>.GetText: string;
begin
  Result := FText;
end;

procedure TVTNode<T>.SetText(const Value: string);
begin
  FText := Value;
end;

function TVTNode<T>.GetTree: TCustomVirtualStringTree;
begin
  Result := FTree;
end;

function TVTNode<T>.GetVisible: Boolean;
begin
  if Assigned(FTree) and Assigned(FVNode) then
  begin
    Result := FTree.IsVisible[VNode];
  end
  else
    Result := False;
end;

procedure TVTNode<T>.SetVisible(const Value: Boolean);
begin
  if Assigned(FTree) and Assigned(FVNode) then
  begin
    FTree.IsVisible[VNode] := Value;
  end;
end;

function TVTNode<T>.GetVNode: PVirtualNode;
begin
  Result := FVNode;
end;

procedure TVTNode<T>.SetVNode(const Value: PVirtualNode);
begin
  if Value <> VNode then
  begin
    FVNode := Value;
    if Assigned(FVNode) then
    begin
      FVNode.CheckState := FCheckState;
      FVNode.CheckType  := FCheckType;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
{ Search with recursion. }

function TVTNode<T>.SearchTree(ANode: TVTNode<T>; const AData: T): TVTNode<T>;
var
  I      : Integer;
  LFound : Boolean;
  LItem  : TVTNode<T>;
begin
  I      := 0;
  LFound := False;
  Result := nil;
  while (I < ANode.ChildCount) and not LFound do
  begin
    LItem := ANode.Items[I];
    if LItem.Data = AData then
    begin
      Result := LItem;
      LFound := True;
    end
    else
    begin
      LItem  := SearchTree(LItem, AData);
      if Assigned(LItem) and (LItem.Data = AData) then
      begin
        Result := LItem;
        LFound := True;
      end
    end;
    Inc(I);
  end;
end;

procedure TVTNode<T>.Select(AScrollIntoView, ACenterVertical, ACenterHorizontal,
  AExpandNode: Boolean);
begin
//
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TVTNode<T>.GetEnumerator: TVTNodeEnumerator<T>;
begin
  Result := TVTNodeEnumerator<T>.Create(
    FTree.GetNodeData<TVTNode<T>>(VNode.FirstChild)
  );
end;
{$ENDREGION}

{$REGION 'public methods'}
{ Adds a new child node with the given data. }

function TVTNode<T>.Add(const AData: T; AOwnsObject: Boolean): TVTNode<T>;
var
  LVTNode : TVTNode<T>;
  LVNode  : PVirtualNode;
begin
  if not Assigned(VNode) then // create root node if it does not exist
  begin
    VNode := FTree.AddChild(nil, Self);
  end;
  LVTNode       := TVTNode<T>.Create(FTree, AData, AOwnsObject, VNode);
  LVNode        := FTree.AddChild(VNode, LVTNode);
  LVTNode.VNode := LVNode;
  Result        := LVTNode;
end;

function TVTNode<T>.Find(const AData: T): TVTNode<T>;
begin
  Result := SearchTree(Self, AData);
end;
{$ENDREGION}

{$REGION 'TVTNodeEnumerator<T>'}
constructor TVTNode<T>.TVTNodeEnumerator<K>.Create(AVTNode: TVTNode<K>);
begin
  FCurrent := AVTNode;
  FFirst   := True;
end;

function TVTNode<T>.TVTNodeEnumerator<K>.GetCurrent: TVTNode<K>;
begin
  Result := FCurrent;
end;

function TVTNode<T>.TVTNodeEnumerator<K>.MoveNext: Boolean;
var
  LTree : TCustomVirtualStringTree;
begin
  if Assigned(FCurrent) then
  begin
    if FFirst then
    begin
      FFirst := False;
    end
    else
    begin
      LTree := FCurrent.FTree;
      FCurrent := LTree.GetNodeData<TVTNode<K>>(FCurrent.VNode.NextSibling);
    end;
  end;
  Result := Assigned(FCurrent);
end;
{$ENDREGION}

end.
