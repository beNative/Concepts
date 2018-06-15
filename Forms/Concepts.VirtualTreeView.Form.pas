unit Concepts.VirtualTreeView.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.StdCtrls,

  zObjInspector, zObjInspTypes,

  VirtualTrees;

  {
     This form demonstrates how to set up a simple TVirtualStringTree.

     Required for a simple setup:
       - OnInitNode : attach data to each node
       - OnGetText  : obtain cell content for each column based on the node data
       - OnFreeNode : cleanup attached data for each node (if this is required)
  }

type
  TfrmVirtualTreeView = class(TForm)
    {$REGION 'designer controls'}
    splVertical : TSplitter;
    sbrMain     : TStatusBar;
    pnlLeft     : TPanel;
    pnlMain     : TPanel;
    pnlHeader   : TPanel;
    lblHeader   : TLabel;
    {$ENDREGION}

  private
    FObjectInspector   : TzObjectInspector;
    FVirtualStringTree : TVirtualStringTree;

    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

    procedure FVirtualStringTreeAfterCellPaint(
      Sender       : TBaseVirtualTree;
      TargetCanvas : TCanvas;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      CellRect     : TRect
    );
    procedure FVirtualStringTreeChange(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure FVirtualStringTreeDblClick(Sender : TObject);
    procedure FVirtualStringTreeExpanded(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure FVirtualStringTreeExpanding(
      Sender      : TBaseVirtualTree;
      Node        : PVirtualNode;
      var Allowed : Boolean
    );
    procedure FVirtualStringTreeFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );
    procedure FVirtualStringTreeFocusChanging(
      Sender : TBaseVirtualTree;
      OldNode,
      NewNode : PVirtualNode;
      OldColumn, NewColumn : TColumnIndex;
      var Allowed          : Boolean
    );
    procedure FVirtualStringTreeFreeNode(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure FVirtualStringTreeGetHint(
      Sender             : TBaseVirtualTree;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      var LineBreakStyle : TVTTooltipLineBreakStyle;
      var HintText       : string
    );
    procedure FVirtualStringTreeGetImageIndex(
      Sender         : TBaseVirtualTree;
      Node           : PVirtualNode;
      Kind           : TVTImageKind;
      Column         : TColumnIndex;
      var Ghosted    : Boolean;
      var ImageIndex : TImageIndex
    );
    procedure FVirtualStringTreeGetNodeDataSize(
      Sender           : TBaseVirtualTree;
      var NodeDataSize : Integer
    );
    procedure FVirtualStringTreeGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );
    procedure FVirtualStringTreeInitChildren(
      Sender         : TBaseVirtualTree;
      Node           : PVirtualNode;
      var ChildCount : Cardinal
    );
    procedure FVirtualStringTreeInitNode(
      Sender : TBaseVirtualTree;
      ParentNode,
      Node : PVirtualNode;
      var InitialStates : TVirtualNodeInitStates
    );
    procedure FVirtualStringTreeMouseUp(
      Sender : TObject;
      Button : TMouseButton;
      Shift  : TShiftState;
      X, Y   : Integer
    );
    procedure FVirtualStringTreeHeaderDrawQueryElements(
      Sender        : TVTHeader;
      var PaintInfo : THeaderPaintInfo;
      var Elements  : THeaderPaintElements
    );
    procedure FVirtualStringTreeHeaderDraw(
      Sender         : TVTHeader;
      HeaderCanvas   : TCanvas;
      Column         : TVirtualTreeColumn;
      R              : TRect;
      Hover, Pressed : Boolean;
      DropMark       : TVTDropMarkMode
    );

  public
    procedure AfterConstruction; override;

    procedure InitializeTree;
  end;

implementation

uses
  System.TypInfo, System.Rtti,

  DDuce.Logger, DDuce.Factories.VirtualTrees, DDuce.Factories.zObjInspector,

  Concepts.Types.Contact, Concepts.Factories, Concepts.Utils;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmVirtualTreeView.AfterConstruction;
begin
  inherited AfterConstruction;
  FVirtualStringTree := TVirtualStringTreeFactory.CreateList(Self, pnlMain);
  InitializeTree;
  FObjectInspector := TzObjectInspectorFactory.Create(Self, pnlLeft);
  FObjectInspector.OnBeforeAddItem := FObjectInspectorBeforeAddItem;
  FObjectInspector.ObjectVisibility := mvPublic;
  FObjectInspector.Component := FVirtualStringTree;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmVirtualTreeView.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not PItem.Name.Contains('ComObject');
end;

{$REGION 'FVirtualStringTree'}
procedure TfrmVirtualTreeView.FVirtualStringTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node.SetData(TConceptFactories.CreateRandomContact);
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  LContact: TContact;
begin
  LContact := Sender.GetNodeData<TContact>(Node);

  if Column = 0 then
    CellText := LContact.FirstName
  else if Column = 1 then
    CellText := LContact.Lastname
  else if Column = 2 then
    CellText := LContact.Email
  else if Column = 3 then
    CellText := LContact.CompanyName
  else if Column = 4 then
    CellText := LContact.Address
  else if Column = 5 then
    CellText := LContact.Number.ToString
  else
  begin
    CellText := '';
  end;
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Sender.GetNodeData<TContact>(Node).Free;
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
begin
  //
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Logger.Track(Sender, 'OnChange');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeDblClick(Sender: TObject);
begin
  Logger.Track(Sender, 'OnDblClick');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Logger.Track(Sender, 'OnExpanded');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
begin
  Logger.Track(Sender, 'OnExpanding');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  Logger.Track(Sender, 'OnFocusChanged');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  Logger.Track(Sender, 'OnFocusChanging');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
begin
  Logger.Track(Sender, 'OnGetHint');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  Logger.Track(Sender, 'OnGetImageIndex');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  Logger.Track(Sender, 'OnGetNodeDataSize');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeHeaderDraw(Sender: TVTHeader;
  HeaderCanvas: TCanvas; Column: TVirtualTreeColumn; R: TRect; Hover,
  Pressed: Boolean; DropMark: TVTDropMarkMode);
begin
    //
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Elements := [hpeBackground];
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
//
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//
end;

{$ENDREGION}
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmVirtualTreeView.InitializeTree;
begin
  with FVirtualStringTree do
  begin
    OnAfterCellPaint          := FVirtualStringTreeAfterCellPaint;
    OnChange                  := FVirtualStringTreeChange;
    OnDblClick                := FVirtualStringTreeDblClick;
    OnExpanded                := FVirtualStringTreeExpanded;
    OnExpanding               := FVirtualStringTreeExpanding;
    OnFocusChanged            := FVirtualStringTreeFocusChanged;
    OnFocusChanging           := FVirtualStringTreeFocusChanging;
    OnFreeNode                := FVirtualStringTreeFreeNode;
    OnGetText                 := FVirtualStringTreeGetText;
    OnGetImageIndex           := FVirtualStringTreeGetImageIndex;
    OnGetHint                 := FVirtualStringTreeGetHint;
    OnGetNodeDataSize         := FVirtualStringTreeGetNodeDataSize;
    OnHeaderDraw              := FVirtualStringTreeHeaderDraw;
    OnHeaderDrawQueryElements := FVirtualStringTreeHeaderDrawQueryElements;
    OnInitChildren            := FVirtualStringTreeInitChildren;
    OnInitNode                := FVirtualStringTreeInitNode;
    OnMouseUp                 := FVirtualStringTreeMouseUp;
    with Header.Columns.Add do
    begin
      Color    := clWhite;
      MaxWidth := 200;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus,
        coEditable];
      Position := 0;
      Width    := 200;
      Text     := 'FirstName';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 200;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 1;
      Width    := 100;
      Text     := 'LastName';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 200;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 2;
      Width    := 100;
      Text     := 'Email';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 400;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 3;
      Width    := 100;
      Text     := 'CompanyName';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 400;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 4;
      Width    := 100;
      Text     := 'Address';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 200;
      MinWidth := 70;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 5;
      Width    := 70;
      Text     := 'Number';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 200;
      MinWidth := 70;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 6;
      Width    := 200;
      Text     := 'Active';
    end;
    RootNodeCount := 1000;
    Header.AutoFitColumns;
    Header.Options := Header.Options + [hoOwnerDraw];
  end;
end;

{$ENDREGION}

end.
