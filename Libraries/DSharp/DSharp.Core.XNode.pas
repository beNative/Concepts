(*
  Copyright (c) 2011, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.Core.XNode;

interface

uses
  DSharp.Collections.ObservableCollection,
  DSharp.Core.PropertyChangedBase,
  Spring.Collections,
  xmldom;

type
  TXNode = class;

  IXNodeList = IList<TXNode>;

  TXNode = class(TPropertyChangedBase)
  private
    FAttributes: IXNodeList;
    FChildNodes: IXNodeList;
    FDOMNode: IDOMNode;
    function GetAttributes: IXNodeList;
    function GetChildNodes: IXNodeList;
    function GetName: string;
    function GetTextNode(ANode: IDOMNode): IDOMNode;
    function GetValue: string;
    procedure SetValue(const Value: string);
  public
    constructor Create(ANode: IDOMNode);
    destructor Destroy; override;

    function SelectElement(const XPath: string): IDOMNode; overload;
    function SelectElements(const XPath: string): IDOMNodeList; overload;
    class function SelectElement(Node: IDOMNode; const XPath: string): IDOMNode; overload;
    class function SelectElements(Node: IDOMNode; const XPath: string): IDOMNodeList; overload;
    function SelectValue(const XPath: string = ''): string;

    property Attributes: IXNodeList read GetAttributes;
    property ChildNodes: IXNodeList read GetChildNodes;
    property DOMNode: IDOMNode read FDOMNode;
    property Name: string read GetName;
    property Value: string read GetValue write SetValue;
  end;

  TXNodeList = class(TObservableCollection<TXNode>, IXNodeList)
  private
    FDOMNode: IDOMNode;
  public
    constructor Create(ANode: IDOMNode = nil);
  end;

implementation

uses
  SysUtils;

{ TXNode }

constructor TXNode.Create(ANode: IDOMNode);
begin
  FDOMNode := ANode;
  FAttributes := TXNodeList.Create(FDOMNode);
  FChildNodes := TXNodeList.Create(FDOMNode);
end;

destructor TXNode.Destroy;
begin
  inherited;
end;

function TXNode.GetAttributes: IXNodeList;
var
  i: Integer;
begin
  if FAttributes.Count = 0 then
  begin
    for i := 0 to Pred(FDOMNode.attributes.length) do
    begin
      if FDOMNode.attributes[i].nodeType = ATTRIBUTE_NODE then
      begin
        FAttributes.Add(TXNode.Create(FDOMNode.attributes[i]));
      end;
    end;
  end;

  Result := FAttributes;
end;

function TXNode.GetChildNodes: IXNodeList;
var
  i: Integer;
begin
  // do it the easy way for now
  if FChildNodes.Count = 0 then
  begin
    for i := 0 to Pred(FDOMNode.childNodes.length) do
    begin
      if FDOMNode.childNodes[i].nodeType = ELEMENT_NODE then
      begin
        FChildNodes.Add(TXNode.Create(FDOMNode.childNodes[i]));
      end;
    end;
  end;

  Result := FChildNodes;
end;

function TXNode.GetName: string;
begin
  Result := FDOMNode.nodeName;
end;

function TXNode.GetTextNode(ANode: IDOMNode): IDOMNode;
begin
  if Assigned(ANode) then
  begin
    if ANode.nodeType = ELEMENT_NODE then
    begin
      if ANode.hasChildNodes then
      begin
        Result := ANode.childNodes[0];
      end
      else
      begin
        Result := ANode.appendChild(ANode.ownerDocument.createTextNode(''));
      end;
    end
    else
    begin
      Result := ANode;
    end;
  end
  else
  begin
    Result := nil;
  end;
end;

function TXNode.GetValue: string;
begin
  Result := Trim(GetTextNode(FDOMNode).nodeValue);
end;

function TXNode.SelectElement(const XPath: string): IDOMNode;
begin
  Result := SelectElement(FDOMNode, XPath);
end;

class function TXNode.SelectElement(Node: IDOMNode; const XPath: string): IDOMNode;
var
  LNodeSelect: IDOMNodeSelect;
begin
  Result := nil;
  if Supports(Node, IDomNodeSelect, LNodeSelect) then
  begin
    Result := LNodeSelect.selectNode(XPath);
  end;
end;

function TXNode.SelectElements(const XPath: string): IDOMNodeList;
begin
  Result := SelectElements(FDOMNode, XPath);
end;

class function TXNode.SelectElements(Node: IDOMNode; const XPath: string): IDOMNodeList;
var
  LNodeSelect: IDOMNodeSelect;
begin
  Result := nil;
  if Supports(Node, IDomNodeSelect, LNodeSelect) then
  begin
    Result := LNodeSelect.selectNodes(XPath);
  end;
end;

function TXNode.SelectValue(const XPath: string): string;
var
  LNode: IDOMNode;
begin
  Result := '';

  if XPath = '' then
    LNode := FDOMNode
  else
    LNode := SelectElement(XPath);

  LNode := GetTextNode(LNode);

  if Assigned(LNode) then
    Result := Trim(LNode.nodeValue);
end;

procedure TXNode.SetValue(const Value: string);
begin
  GetTextNode(FDOMNode).nodeValue := Value;
  NotifyOfPropertyChange('Value');
end;

{ TXNodeList }

constructor TXNodeList.Create(ANode: IDOMNode);
begin
  inherited Create();
  FDOMNode := ANode;
end;

end.
