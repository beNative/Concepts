{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Components.JsonTree;

interface

uses
  System.JSON,
  Vcl.Graphics,

  VirtualTrees.Types, VirtualTrees.Header, VirtualTrees,

  DDuce.Components.SectionTree, DDuce.Components.VirtualTrees.Node;

type
  TJsonNode = TVTNode<TJSONAncestor>;

type
  TJsonTree = class(TSectionTree)
  private
    FRootNode : TJsonNode;
    FJson     : TJSONValue;

  protected
    procedure BuildTree; override;
    procedure ParseNode(ANode: TJsonNode);
    function GetNode(const AVNode: PVirtualNode): TJsonNode;

    {$REGION 'property access methods'}
    function GetJsonString: string;
    procedure SetJsonString(const Value: string);
    {$ENDREGION}

    {$REGION 'event dispatch methods'}
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;
    procedure DoGetBackColor(
      ANode          : PVirtualNode;
      var ABackColor : TColor
    ); override;
    {$ENDREGION}

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property JsonString: string
      read GetJsonString write SetJsonString;
  end;

implementation

uses
  System.SysUtils;

{$REGION 'property access methods'}
procedure TJsonTree.AfterConstruction;
begin
  inherited AfterConstruction;
  Header.Options := Header.Options + [hoAutoResize];
  with Header.Columns.Add do
  begin
    Color    := clWhite;
    MaxWidth := 1200;
    MinWidth := 100;
    Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
      coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus
      {coEditable}];
    Position := 0;
    Width    := 200;
    Text := 'Node';
  end;
  with Header.Columns.Add do
  begin
    MaxWidth := 1200;
    MinWidth := 100;
    Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
      coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
      coSmartResize, coAllowFocus{, coEditable}];
    Position := 1;
    Width    := 400;
    Text := 'Value';
  end;
  Header.AutoSizeIndex := 1;
end;

destructor TJsonTree.Destroy;
begin
  FreeAndNil(FJson);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TJsonTree.DoFreeNode(Node: PVirtualNode);
begin
  inherited DoFreeNode(Node);
  GetNode(Node).Free;
end;

procedure TJsonTree.DoGetBackColor(ANode: PVirtualNode; var ABackColor: TColor);
var
  LNode : TJsonNode;
begin
  LNode := GetNode(ANode);
  if Assigned(LNode) then
  begin
    if LNode.HasChildren then
      ABackColor := $00EBEBEB
    else
      ABackColor := $00F8F8F8;
  end;
  inherited;
end;

procedure TJsonTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
var
  LNode  : TJsonNode;
  LValue : TJSONAncestor;
  S      : string;
begin
  with pEventArgs do
  begin
    LNode := GetNode(Node);
    LValue := LNode.Data;
    if LValue is TJSONPair then
    begin
      if Column = 0 then
      begin
        S := (LValue as TJSONPair).JsonString.Value;
        if LNode.HasChildren then
        begin
          CellText := Format('%s {%d}', [S, LNode.ChildCount]);
        end
        else
        begin
          CellText := S;
        end;
      end
      else
      begin
        S := (LValue as TJSONPair).JsonValue.Value;
        CellText := S;
      end;
    end
    else
    begin
      if Column = 0 then
      begin
        S := LValue.Value;
        if S.IsEmpty then
          S := LNode.Index.ToString;
        if LNode.HasChildren then
        begin
          CellText := Format('%s [%d]', [S, LNode.ChildCount]);
        end
        else
        begin
          CellText := S;
        end;
      end
      else
        CellText := '';
    end;
  end;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TJsonTree.GetJsonString: string;
begin
  if Assigned(FJson) then
    Result := FJson.Format(2)
  else
    Result := '';
end;

procedure TJsonTree.SetJsonString(const Value: string);
begin
  if Assigned(FJson) then
  begin
    FreeAndNil(FJson);
    Clear;
    FRootNode := nil;
  end;
  FJson := TJSONObject.ParseJSONValue(Value);
  if Assigned(FJson) then
    BuildTree;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TJsonTree.BuildTree;
begin
  BeginUpdate;
  try
    FRootNode := TJsonNode.Create(Self, FJson, False);
    ParseNode(FRootNode);
    FRootNode.Expand;
  finally
    EndUpdate;
  end;
end;

function TJsonTree.GetNode(const AVNode: PVirtualNode): TJsonNode;
begin
  Result := GetNodeData<TJsonNode>(AVNode);
end;

{ Parses a JSON object and its children to instrument the tree structure. }

procedure TJsonTree.ParseNode(ANode: TJsonNode);
var
  LValue : TJSONAncestor;
  LNode  : TJsonNode;
  LData  : TJSONAncestor;
begin
  LData := ANode.Data;
  ANode.VNode.States := ANode.VNode.States + [vsMultiline];
  if LData is TJSONPair then
  begin
    LValue := (LData as TJSONPair).JsonValue;
  end
  else
    LValue := ANode.Data;
  if LValue is TJSONObject then
  begin
    var LObject := LValue as TJSONObject;
    for var LPair in LObject do
    begin
      LNode := ANode.Add(LPair, False);
      if (LPair.JsonValue is TJSONArray) or (LPair.JsonValue is TJSONObject) then
      begin
        ParseNode(LNode);
      end
    end;
  end
  else if LValue is TJSONArray then
  begin
    var LArray := LValue as TJSONArray;
    for var LItem in LArray do
    begin
      LNode := ANode.Add(LItem, False);
      if (LItem is TJSONArray) or (LItem is TJSONObject) then
      begin
        ParseNode(LNode);
      end
    end;
  end;
end;
{$ENDREGION}

end.
