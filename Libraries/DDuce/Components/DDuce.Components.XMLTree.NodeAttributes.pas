{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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
unit DDuce.Components.XMLTree.NodeAttributes;

{$I ..\DDuce.inc}

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Graphics,

  DSharp.Core.Collections;

type
  TNodeType = (
    ntUnknown,   // Error
    ntRoot,     // DocumentElement, NODE_DOCUMENT, NODE_PROCESSING_INSTRUCTION
    ntComment,  // NODE_COMMENT
    ntText,     // NODE_TEXT, NODE_CDATA_SECTION
    ntAttribute,// NODE_ATTRIBUTE
    ntElement,  // NODE_ELEMENT without ChildNodes
    ntNode      // NODE_ELEMENT with ChildNodes
  );

type
  TNodeAttributesItem = class(TCollectionItem)
  private
    FBackGroundColor : TColor;
    FFont            : TFont;
    FName            : string;
    FNodeType        : TNodeType;

    procedure SetBackGroundColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetNodeType(AValue: TNodeType);
  protected
    function GetDisplayName: string; override;

  public
    constructor Create(ACollection: System.Classes.TCollection); override;
    procedure BeforeDestruction; override;

  published
    property Name: string
      read FName write FName;

    property NodeType: TNodeType
      read FNodeType write SetNodeType;

    property BackGroundColor: TColor
      read FBackGroundColor write SetBackGroundColor;

    property Font: TFont
      read FFont write SetFont;
  end;

type
  TNodeAttributes = class(TOwnedCollection<TNodeAttributesItem>)
  private
    function GetItemByType(Index: TNodeType): TNodeAttributesItem;

  public
    property ItemByType[Index: TNodeType]: TNodeAttributesItem
      read GetItemByType;
  end;

implementation

{$REGION 'TNodeAttributesItem'}
{$REGION 'construction and destruction'}
constructor TNodeAttributesItem.Create(ACollection: System.Classes.TCollection);
begin
  inherited Create(ACollection);
  FFont := TFont.Create;
  FFont.Size := 8;
end;

function TNodeAttributesItem.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TNodeAttributesItem.BeforeDestruction;
begin
  FFont.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
procedure TNodeAttributesItem.SetBackGroundColor(AValue: TColor);
begin
  if AValue <> BackGroundColor then
  begin
    FBackGroundColor := AValue;
    Changed(False);
  end;
end;

procedure TNodeAttributesItem.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  Changed(False);
end;

procedure TNodeAttributesItem.SetNodeType(AValue: TNodeType);
begin
  if AValue <> NodeType then
  begin;
    FNodeType := AValue;
    Changed(False);
  end;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TNodeAttributes'}
{$REGION 'property access mehods'}
function TNodeAttributes.GetItemByType(Index: TNodeType): TNodeAttributesItem;
var
  I : Integer;
  B : Boolean;
begin
  I := 0;
  B := False;
  Result := nil;
  while (I < Count) and not B do
  begin
    if Items[I].NodeType = Index then
    begin
      Result := Items[I];
      B := True;
    end;
    Inc(I);
  end;
end;
{$ENDREGION}
{$ENDREGION}
end.

