{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Components.ValueList.Node;

interface

uses
  System.Rtti,

  VirtualTrees,

  Spring.Collections,

  DDuce.DynamicRecord, DDuce.Logger;

type
  TValueListNode = class
  private
    FVTNode : PVirtualNode;
    FField  : IDynamicField;
    FNodes  : IList<TValueListNode>;
    FName   : string;
    FValue  : TValue;

  protected
    {$REGION 'property access methods'}
    function GetCount: Integer;
    function GetName: string;
    function GetValue: TValue;
    procedure SetName(const Value: string);
    procedure SetValue(const Value: TValue);
    function GetVTNode: PVirtualNode;
    procedure SetVTNode(const Value: PVirtualNode);
    function GetField: IDynamicField;
    procedure SetField(const Value: IDynamicField);
    function GetNodes: IList<TValueListNode>;
    {$ENDREGION}

    procedure UpdateNodes;

  public
    property Field: IDynamicField
      read GetField write SetField;

    property Name: string
      read GetName write SetName;

    property Value: TValue
      read GetValue write SetValue;

    property VTNode: PVirtualNode
      read GetVTNode write SetVTNode;

    property Count: Integer
      read GetCount;

    property Nodes: IList<TValueListNode>
      read GetNodes;
  end;

implementation

uses
  Spring,

  System.SysUtils;

{$REGION 'property access methods'}
function TValueListNode.GetCount: Integer;
begin
  if Assigned(FNodes) then
    Result := FNodes.Count
  else
    Result := 0;
end;

function TValueListNode.GetField: IDynamicField;
begin
  Result := FField;
end;

procedure TValueListNode.SetField(const Value: IDynamicField);
begin
  if Value <> FField then
  begin
    FField := Value;
    if Assigned(FField) then
    begin
      FValue := TValue.Empty;
    end;
    UpdateNodes;
  end;
end;

function TValueListNode.GetName: string;
begin
  if Assigned(FField) then
    Result := FField.Name
  else
    Result := FName;
end;

procedure TValueListNode.SetName(const Value: string);
begin
  if Assigned(FField) then
  begin
    FField.Name := Value
  end
  else
    FName := Value;
end;

function TValueListNode.GetNodes: IList<TValueListNode>;
begin
  if not Assigned(FNodes) then
    FNodes := TCollections.CreateObjectList<TValueListNode>;
  Result := FNodes;
end;

function TValueListNode.GetValue: TValue;
begin
  if Assigned(FField) then
    Result := FField.Value
  else
    Result := FValue;
end;

procedure TValueListNode.SetValue(const Value: TValue);
begin
  if Assigned(FField) then
    FField.Value := Value
  else
    FValue := Value;
end;

function TValueListNode.GetVTNode: PVirtualNode;
begin
  Result := FVTNode;
end;

procedure TValueListNode.SetVTNode(const Value: PVirtualNode);
begin
  FVTNode := Value;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TValueListNode.UpdateNodes;
var
  S : string;
  A : TArray<string>;
  N : TValueListNode;
begin
  if Assigned(FNodes) then
    FNodes.Clear;
  S := FField.Value.ToString;
  if (not S.IsEmpty) and (S[1] = '[') and (S[S.Length] = ']') then
  begin
    S := Copy(S, 2, S.Length - 2);
    A := S.Split([',']);
    for S in A do
    begin
      N := TValueListNode.Create;
      N.Name  := S;
      N.Value := True;
      Nodes.Add(N);
    end;
  end;
end;
{$ENDREGION}

end.
