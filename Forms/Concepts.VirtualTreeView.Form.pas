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
     Required for a simple setup:
       - OnInitNode : attach data to each node
       - OnGetText : obtain cell content for each column based on the node data
       - OnFreeNode : cleanup attached data for each node (if this is required)
  }

type
  TfrmVirtualTreeView = class(TForm)
    {$REGION 'designer controls'}
    splVertical : TSplitter;
    sbrMain     : TStatusBar;
    pnlLeft     : TPanel;
    pnlMain     : TPanel;
    vstMain     : TVirtualStringTree;
    pnlHeader   : TPanel;
    lblHeader   : TLabel;
    {$ENDREGION}

    procedure vstMainAfterCellPaint(
      Sender       : TBaseVirtualTree;
      TargetCanvas : TCanvas;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      CellRect     : TRect
    );
    procedure vstMainChange(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure vstMainDblClick(Sender : TObject);
    procedure vstMainExpanded(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure vstMainExpanding(
      Sender      : TBaseVirtualTree;
      Node        : PVirtualNode;
      var Allowed : Boolean
    );
    procedure vstMainFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );
    procedure vstMainFocusChanging(
      Sender : TBaseVirtualTree;
      OldNode,
      NewNode : PVirtualNode;
      OldColumn, NewColumn : TColumnIndex;
      var Allowed          : Boolean
    );
    procedure vstMainFreeNode(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure vstMainGetHint(
      Sender             : TBaseVirtualTree;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      var LineBreakStyle : TVTTooltipLineBreakStyle;
      var HintText       : string
    );
    procedure vstMainGetImageIndex(
      Sender         : TBaseVirtualTree;
      Node           : PVirtualNode;
      Kind           : TVTImageKind;
      Column         : TColumnIndex;
      var Ghosted    : Boolean;
      var ImageIndex : TImageIndex
    );
    procedure vstMainGetNodeDataSize(
      Sender           : TBaseVirtualTree;
      var NodeDataSize : Integer
    );
    procedure vstMainGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );
    procedure vstMainInitChildren(
      Sender         : TBaseVirtualTree;
      Node           : PVirtualNode;
      var ChildCount : Cardinal
    );
    procedure vstMainInitNode(
      Sender : TBaseVirtualTree;
      ParentNode,
      Node : PVirtualNode;
      var InitialStates : TVirtualNodeInitStates
    );
    procedure vstMainMouseUp(
      Sender : TObject;
      Button : TMouseButton;
      Shift  : TShiftState;
      X, Y   : Integer
    );
    procedure vstMainHeaderDrawQueryElements(
      Sender        : TVTHeader;
      var PaintInfo : THeaderPaintInfo;
      var Elements  : THeaderPaintElements
    );
    procedure vstMainHeaderDraw(
      Sender         : TVTHeader;
      HeaderCanvas   : TCanvas;
      Column         : TVirtualTreeColumn;
      R              : TRect;
      Hover, Pressed : Boolean;
      DropMark       : TVTDropMarkMode
    );

  private
    FObjectInspector : TzObjectInspector;

    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  System.TypInfo, System.Rtti,

  DDuce.Components.Factories, DDuce.Logger,

  Concepts.Types.Contact, Concepts.Factories, Concepts.Utils;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmVirtualTreeView.AfterConstruction;
begin
  inherited AfterConstruction;
  FObjectInspector := TConceptFactories.CreatezObjectInspector(
    Self,
    pnlLeft
  );
  FObjectInspector.OnBeforeAddItem := FObjectInspectorBeforeAddItem;
  FObjectInspector.ObjectVisibility := mvPublic;
  FObjectInspector.Component := vstMain;
  vstMain.RootNodeCount := 100000;
  vstMain.Indent := 0;
  vstMain.Header.AutoFitColumns;
  vstMain.Header.Options := vstMain.Header.Options + [hoOwnerDraw];
end;

procedure TfrmVirtualTreeView.BeforeDestruction;
begin

  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmVirtualTreeView.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not PItem.Name.Contains('ComObject');
  Result := Result and (not (PItem.Prop.PropertyType is TRttiMethodType));
end;

{$REGION 'vstMain'}


procedure TfrmVirtualTreeView.vstMainAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
begin

//
end;

procedure TfrmVirtualTreeView.vstMainChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
//
end;

procedure TfrmVirtualTreeView.vstMainDblClick(Sender: TObject);
begin
//
end;

procedure TfrmVirtualTreeView.vstMainExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
//
end;

procedure TfrmVirtualTreeView.vstMainExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
begin
//
end;

procedure TfrmVirtualTreeView.vstMainFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
//
end;

procedure TfrmVirtualTreeView.vstMainFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
//
end;

procedure TfrmVirtualTreeView.vstMainFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Sender.GetNodeData<TContact>(Node).Free;
end;

procedure TfrmVirtualTreeView.vstMainGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
begin
//
end;

procedure TfrmVirtualTreeView.vstMainGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
//
end;

procedure TfrmVirtualTreeView.vstMainGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
//
end;

procedure TfrmVirtualTreeView.vstMainGetText(Sender: TBaseVirtualTree;
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
//    CellText := '';
  end;
end;

procedure TfrmVirtualTreeView.vstMainHeaderDraw(Sender: TVTHeader;
  HeaderCanvas: TCanvas; Column: TVirtualTreeColumn; R: TRect; Hover,
  Pressed: Boolean; DropMark: TVTDropMarkMode);
begin
    //
end;

procedure TfrmVirtualTreeView.vstMainHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Elements := [hpeBackground];
end;

procedure TfrmVirtualTreeView.vstMainInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
//
end;

procedure TfrmVirtualTreeView.vstMainInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node.SetData(TConceptFactories.CreateRandomContact);
end;

procedure TfrmVirtualTreeView.vstMainMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//
end;

{$ENDREGION}
{$ENDREGION}

end.
