unit DDuce.Components.XmlTree;

interface

uses
  System.Classes, System.SysUtils, System.Types,
  Vcl.Graphics,
  Xml.XMLDoc, Xml.XMLIntf,

  Spring,

  VirtualTrees.Types, VirtualTrees.Header, VirtualTrees.BaseTree, VirtualTrees,

  DDuce.Components.SectionTree, DDuce.Settings.TextFormat,
  DDuce.Components.VirtualTrees.Node;

type
  TXmlNode = TVTNode<IXMLNode>;

type
  TXmlTree = class(TSectionTree)
  private type
    TColorSettings = class(TPersistent)
    private
      FElement   : TTextFormatSettings;
      FAttribute : TTextFormatSettings;
      FTextNode  : TTextFormatSettings;
      FComment   : TTextFormatSettings;
      FOnChanged : Event<TNotifyEvent>;

    protected
      function GetOnChanged: IEvent<TNotifyEvent>;
      procedure InitializeObjects;
      procedure Changed;
      procedure FormatSettingsChanged(Sender: TObject);

    public
      procedure AfterConstruction; override;
      destructor Destroy; override;

      property Element: TTextFormatSettings read FElement;
      property Attribute: TTextFormatSettings read FAttribute;
      property TextNode: TTextFormatSettings read FTextNode;
      property Comment: TTextFormatSettings read FComment;
      property OnChanged: IEvent<TNotifyEvent> read GetOnChanged;
    end;

  private
    FXmlDocument   : IXMLDocument;
    FColorSettings : TColorSettings;

  protected
    procedure BuildTree; override;
    function GetNode(const AVNode: PVirtualNode): TXmlNode;
    procedure ParseNode(AParentNode: TXmlNode; AXmlNode: IXMLNode);

    {$REGION 'Event dispatch methods'}
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
      Column: TColumnIndex; TextType: TVSTTextType); override;
    {$ENDREGION}

    {$REGION 'Property access methods'}
    function GetXmlString: string;
    procedure SetXmlString(const Value: string);
    {$ENDREGION}

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property ColorSettings: TColorSettings
      read FColorSettings;

    property XmlString: string
      read GetXmlString write SetXmlString;
  end;

implementation

uses
  System.Variants;

{$REGION 'TXmlTree.TColorSettings'}
procedure TXmlTree.TColorSettings.AfterConstruction;
begin
  inherited;
  InitializeObjects;
end;

destructor TXmlTree.TColorSettings.Destroy;
begin
  FreeAndNil(FElement);
  FreeAndNil(FAttribute);
  FreeAndNil(FTextNode);
  FreeAndNil(FComment);
  inherited;
end;

procedure TXmlTree.TColorSettings.InitializeObjects;
begin
  FElement := TTextFormatSettings.Create;
  FElement.FontColor := clNavy;
  FElement.FontStyle := [fsBold];
  FElement.OnChanged.Add(FormatSettingsChanged);

  FAttribute := TTextFormatSettings.Create;
  FAttribute.FontColor := clPurple;
  FAttribute.OnChanged.Add(FormatSettingsChanged);

  FTextNode := TTextFormatSettings.Create;
  FTextNode.FontColor := clGreen;
  FTextNode.OnChanged.Add(FormatSettingsChanged);

  FComment := TTextFormatSettings.Create;
  FComment.FontColor := clGray;
  FComment.OnChanged.Add(FormatSettingsChanged);
end;

function TXmlTree.TColorSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

procedure TXmlTree.TColorSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;

procedure TXmlTree.TColorSettings.FormatSettingsChanged(Sender: TObject);
begin
  Changed;
end;
{$ENDREGION}

{$REGION 'TXmlTree'}
procedure TXmlTree.AfterConstruction;
begin
  inherited;
  FColorSettings := TColorSettings.Create;
  with Header.Columns.Add do begin
    Text := 'Name';
    Width := 200;
  end;
  with Header.Columns.Add do begin
    Text := 'Value';
    Width := 400;
  end;
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toVariableNodeHeight];
end;

destructor TXmlTree.Destroy;
begin
  FreeAndNil(FColorSettings);
  inherited;
end;

procedure TXmlTree.BuildTree;
begin
  BeginUpdate;
  try
    if Assigned(FXmlDocument) and Assigned(FXmlDocument.DocumentElement) then
      ParseNode(nil, FXmlDocument.DocumentElement);
    Header.AutoFitColumns;
  finally
    EndUpdate;
  end;
end;

procedure TXmlTree.ParseNode(AParentNode: TXmlNode; AXmlNode: IXMLNode);
var
  LNode: TXmlNode;
  LParentVNode: PVirtualNode;
  I: Integer;
begin
  // Determine parent VNode
  if Assigned(AParentNode) then
    LParentVNode := AParentNode.VNode
  else
    LParentVNode := nil;

  // Create node for current XML node
  LNode := TXmlNode.Create(Self, AXmlNode, False, LParentVNode);

  // Add attributes as child nodes
  for I := 0 to AXmlNode.AttributeNodes.Count - 1 do
    TXmlNode.Create(Self, AXmlNode.AttributeNodes[I], False, LNode.VNode);

  // Recursively add child nodes
  for I := 0 to AXmlNode.ChildNodes.Count - 1 do
    ParseNode(LNode, AXmlNode.ChildNodes[I]);
end;

function TXmlTree.GetNode(const AVNode: PVirtualNode): TXmlNode;
begin
  Result := GetNodeData<TXmlNode>(AVNode);
end;

procedure TXmlTree.DoFreeNode(Node: PVirtualNode);
begin
  GetNode(Node).Free;
  inherited;
end;

procedure TXmlTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
var
  LNode: TXmlNode;
  LXmlNode: IXMLNode;
begin
  LNode := GetNode(pEventArgs.Node);
  LXmlNode := LNode.Data;

  case LXmlNode.NodeType of
    ntElement:
      begin
        pEventArgs.CellText := LXmlNode.NodeName;
        if pEventArgs.Column = 1 then
          pEventArgs.CellText := LXmlNode.Text;
      end;
    ntAttribute:
      begin
        if pEventArgs.Column = 0 then
          pEventArgs.CellText := '@' + LXmlNode.NodeName
        else
          pEventArgs.CellText := LXmlNode.NodeValue;
      end;
    ntText:
      begin
        if pEventArgs.Column = 0 then
          pEventArgs.CellText := '#text'
        else
          pEventArgs.CellText := LXmlNode.Text;
      end;
    ntComment:
      begin
        if pEventArgs.Column = 0 then
          pEventArgs.CellText := '#comment'
        else
          pEventArgs.CellText := LXmlNode.Text;
      end;
  end;
end;

procedure TXmlTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
  Column: TColumnIndex; TextType: TVSTTextType);
var
  LNode: TXmlNode;
begin
  inherited;
  LNode := GetNode(Node);
  case LNode.Data.NodeType of
    ntElement:    Canvas.Font.Assign(ColorSettings.Element.Font);
    ntAttribute:  Canvas.Font.Assign(ColorSettings.Attribute.Font);
    ntText:       Canvas.Font.Assign(ColorSettings.TextNode.Font);
    ntComment:    Canvas.Font.Assign(ColorSettings.Comment.Font);
  end;
end;

function TXmlTree.GetXmlString: string;
begin
  if Assigned(FXmlDocument) then
    Result := FXmlDocument.XML.Text
  else
    Result := '';
end;

procedure TXmlTree.SetXmlString(const Value: string);
begin
  if Value <> XmlString then begin
    Clear;
    FXmlDocument := LoadXMLData(Value);
//    FXmlDocument := TXMLDocument.Create(nil);
//    FXmlDocument.LoadFromXML(Value);
    BuildTree;
  end;
end;
{$ENDREGION}

end.
