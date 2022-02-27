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

unit DDuce.Components.IniTree;

interface

uses
  System.IniFiles, System.Classes, System.Generics.Collections, System.Rtti,
  Vcl.Graphics,

  VirtualTrees.Types, VirtualTrees.Header, VirtualTrees,

  DDuce.DynamicRecord, DDuce.Components.SectionTree,
  DDuce.Components.VirtualTrees.Node;

type
  TIniData = TPair<string, TValue>;
  TIniNode = TVTNode<TIniData>;

type
  TIniTree = class(TSectionTree)
  private
    FIniFile   : TMemIniFile;
    FIniStream : TStringStream;
    FIniString : string;
    FRootNode  : TIniNode;

  protected
    function GetIniString: string;
    procedure SetIniString(const Value: string);

    procedure BuildTree; override;
    function GetNode(const AVNode: PVirtualNode): TIniNode;

    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property IniString: string
      read GetIniString write SetIniString;
  end;

implementation

uses
  System.SysUtils,

  Spring;

{$REGION 'construction and destruction'}
procedure TIniTree.AfterConstruction;
begin
  inherited AfterConstruction;
  FIniStream := TStringStream.Create;
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

destructor TIniTree.Destroy;
begin
  FIniStream.Free;
  FreeAndNil(FIniFile);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TIniTree.GetIniString: string;
begin
  Result := FIniString;
end;

procedure TIniTree.SetIniString(const Value: string);
begin
   if Value <> IniString then
   begin
     Clear;
     FreeAndNil(FIniFile);
     FIniString := Value;
     FIniStream.Clear;
     FIniStream.WriteString(Value);
     FIniFile := TMemIniFile.Create(FIniStream);
     BuildTree;
   end;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TIniTree.DoFreeNode(Node: PVirtualNode);
begin
  inherited DoFreeNode(Node);
  GetNode(Node).Free;
end;

procedure TIniTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
var
  LNode  : TIniNode;
  LValue : TIniData;
  S      : string;
begin
  with pEventArgs do
  begin
    LNode := GetNode(Node);
    LValue := LNode.Data;
    if Column = 0 then
    begin
      S := LValue.Key;
    end
    else
    begin
      S := LValue.Value.ToString;
    end;
    CellText := S;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TIniTree.BuildTree;
var
  LSections      : IShared<TStringList>;
  LSection       : string;
  LSectionValues : IShared<TStringList>;
  LSectionNode   : TIniNode;
begin
  FRootNode := TIniNode.Create(Self, TIniData.Create('', ''), False);
  LSections := Shared.Make(TStringList.Create);
  FIniFile.ReadSections(LSections);
  for LSection in LSections do
  begin
    LSectionNode := TIniNode.Create(Self, TIniData.Create(LSection, ''), False);
    LSectionValues := Shared.Make(TStringList.Create);
    FIniFile.ReadSectionValues(LSection, LSectionValues);
    for var I := 0 to LSectionValues.Count - 1 do
    begin
      LSectionNode.Add(TIniData.Create(
        LSectionValues.Names[I], LSectionValues.ValueFromIndex[I])
      );
    end;
  end;
end;

function TIniTree.GetNode(const AVNode: PVirtualNode): TIniNode;
begin
  Result := GetNodeData<TIniNode>(AVNode);
end;
{$ENDREGION}

end.
