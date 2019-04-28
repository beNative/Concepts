unit Concepts.VirtualTreeView.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.StdCtrls,

  zObjInspector, zObjInspTypes,

  VirtualTrees, System.Actions, Vcl.ActnList;

  {
     Required for a simple setup:
       - OnInitNode : attach data to each node
       - OnGetText : obtain cell content for each column based on the node data
       - OnFreeNode : cleanup attached data for each node (if this is required)
  }

type
  TfrmVirtualTreeView = class(TForm)
    {$REGION 'designer controls'}
    splVertical       : TSplitter;
    sbrMain           : TStatusBar;
    pnlLeft           : TPanel;
    pnlMain           : TPanel;
    pnlHeader         : TPanel;
    lblHeader         : TLabel;
    pnlColumnSettings : TGridPanel;
    pnlCol0           : TPanel;
    pnlCol1           : TPanel;
    pnlCol2           : TPanel;
    pnlCol3           : TPanel;
    pnlCol4           : TPanel;
    pnlCol5           : TPanel;
    pnlCol6: TPanel;
    splHorizontal: TSplitter;
    pnlTreeView: TPanel;
    aclMain: TActionList;
    actAutoSizeColumns: TAction;
    btnAutoSizeColumns: TButton;
    lblFocusedNode: TLabel;
    procedure FormResize(Sender: TObject);
    procedure actAutoSizeColumnsExecute(Sender: TObject);
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

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;

    procedure InitializeTree;
    procedure CreateInspectors;
  end;

implementation

uses
  System.TypInfo, System.Rtti,

  DDuce.Logger, DDuce.Factories.VirtualTrees, DDuce.Factories.zObjInspector,

  Concepts.Types.Contact, Concepts.Factories, Concepts.Utils;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmVirtualTreeView.actAutoSizeColumnsExecute(Sender: TObject);
begin
  FVirtualStringTree.Header.AutoFitColumns(
    True,
    smaUseColumnOption
  );
end;

procedure TfrmVirtualTreeView.AfterConstruction;
begin
  inherited AfterConstruction;
  FVirtualStringTree := TVirtualStringTreeFactory.CreateList(Self, pnlTreeView);
  InitializeTree;
  FObjectInspector := TzObjectInspectorFactory.Create(Self, pnlLeft);
  FObjectInspector.OnBeforeAddItem := FObjectInspectorBeforeAddItem;
  FObjectInspector.ObjectVisibility := mvPublished;
  FObjectInspector.Component := FVirtualStringTree;
  CreateInspectors;
end;

procedure TfrmVirtualTreeView.CreateInspectors;
var
  I  : Integer;
  OI : TzObjectInspector;
  P  : TPanel;
begin
  for I := 0 to FVirtualStringTree.Header.Columns.Count - 1 do
  begin
    OI := TzObjectInspectorFactory.Create(
      Self,
      pnlColumnSettings,
      FVirtualStringTree.Header.Columns[I]
    );
    OI.ExpandAll;
    pnlColumnSettings.ControlCollection.Controls[I, 1] := OI;
    P := pnlColumnSettings.ControlCollection.Controls[I, 0] as TPanel;
    P.Caption := FVirtualStringTree.Header.Columns[I].Text;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmVirtualTreeView.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not PItem.Name.Contains('ComObject');
  Result := Result and (not (PItem.Prop.PropertyType is TRttiMethodType));
end;

procedure TfrmVirtualTreeView.FormResize(Sender: TObject);
var
  I  : Integer;
  OI : TzObjectInspector;
begin
  for I := 0 to pnlColumnSettings.ColumnCollection.Count - 1 do
  begin
    OI := pnlColumnSettings.ControlCollection.Controls[I, 1] as TzObjectInspector;
    OI.SplitterPos := OI.Width div 2;
  end;
end;

{$REGION 'FVirtualStringTree'}
procedure TfrmVirtualTreeView.FVirtualStringTreeAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
begin
  //Logger.IncCounter('OnAfterCellPaint');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Logger.AddCheckPoint('OnChange');
  Logger.IncCounter('OnChange');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeDblClick(Sender: TObject);
begin
  Logger.Track(Self, 'FVirtualStringTreeDblClick');
  Logger.IncCounter('OnDblClick');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Logger.Track(Self, 'FVirtualStringTreeExpanded');
  Logger.IncCounter('OnExpanded');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
begin
  Logger.Track(Self, 'FVirtualStringTreeExpanding');
  Logger.IncCounter('OnExpanding');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  Logger.AddCheckPoint('OnFocusChanged');
  Logger.IncCounter('OnFocusChanged');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  Logger.AddCheckPoint('OnFocusChanging');
  Logger.IncCounter('OnFocusChanging');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Logger.IncCounter('OnFreeNode');
  Sender.GetNodeData<TContact>(Node).Free;
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
begin
  Logger.AddCheckPoint('OnGetHint');
  Logger.IncCounter('OnGetHint');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
//  Logger.IncCounter('OnGetImageIndex');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  Logger.IncCounter('OnGetNodeDataSize');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  LContact: TContact;
begin
  //Logger.IncCounter('OnGetText');       -> slows logger down
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

procedure TfrmVirtualTreeView.FVirtualStringTreeHeaderDraw(Sender: TVTHeader;
  HeaderCanvas: TCanvas; Column: TVirtualTreeColumn; R: TRect; Hover,
  Pressed: Boolean; DropMark: TVTDropMarkMode);
begin
  Logger.IncCounter('OnHeaderDraw');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Logger.IncCounter('OnHeaderDrawQueryElements');
  Elements := [hpeBackground];
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  Logger.IncCounter('OnInitChildren');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Logger.IncCounter('OnInitNode');
  Node.SetData(TConceptFactories.CreateRandomContact);
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Logger.IncCounter('OnMouseUp');
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
      Text := 'FirstName';
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
      Text := 'LastName';
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
      Text := 'Email';
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
      Text := 'CompanyName';
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
      Text := 'Address';
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
      Text := 'Number';
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
      Text := 'Active';
    end;
    RootNodeCount := 10;
    Header.AutoFitColumns;
    Header.Options := Header.Options + [hoOwnerDraw];
  end;
end;
procedure TfrmVirtualTreeView.UpdateActions;
begin
  inherited UpdateActions;
  if Assigned(FVirtualStringTree) and Assigned(FVirtualStringTree.FocusedNode) then
  begin
    lblFocusedNode.Caption := FVirtualStringTree.FocusedNode.Index.ToString;
  end
  else
  begin
    lblFocusedNode.Caption := 'None';
  end;

end;

{$ENDREGION}

end.
