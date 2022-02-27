unit Concepts.VirtualTreeView.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ActnList,

  zObjInspector, zObjInspTypes,

  VirtualTrees;

{$REGION 'documentation'}
{
   Required events to handle for a simple setup:
     - OnInitNode : attach data to each node
     - OnGetText  : obtain cell content for each column based on the node data
     - OnFreeNode : cleanup attached data for each node (if this is required)

   Run this form in combination with LogViewer to see a comprehensive overview
   of the sequence of events triggered when drawing the treeview.
}
{$ENDREGION}

type
  TfrmVirtualTreeView = class(TForm)
    {$REGION 'designer controls'}
    aclMain            : TActionList;
    actAutoSizeColumns : TAction;
    btnAutoSizeColumns : TButton;
    lblFocusedNode     : TLabel;
    lblHeader          : TLabel;
    pnlCol0            : TPanel;
    pnlCol1            : TPanel;
    pnlCol2            : TPanel;
    pnlCol3            : TPanel;
    pnlCol4            : TPanel;
    pnlCol5            : TPanel;
    pnlCol6            : TPanel;
    pnlColumnSettings  : TGridPanel;
    pnlHeader          : TPanel;
    pnlLeft            : TPanel;
    pnlMain            : TPanel;
    pnlTreeView        : TPanel;
    sbrMain            : TStatusBar;
    splHorizontal      : TSplitter;
    splVertical        : TSplitter;
    {$ENDREGION}

    procedure FormResize(Sender: TObject);

    procedure actAutoSizeColumnsExecute(Sender: TObject);

  private
    FObjectInspector   : TzObjectInspector;
    FVirtualStringTree : TVirtualStringTree;

    {$REGION 'event handlers'}
    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

    procedure FVirtualStringTreeStateChange(
      Sender : TBaseVirtualTree;
      Enter  : TVirtualTreeStates;
      Leave  : TVirtualTreeStates
    );

    procedure FVirtualStringTreeBeforePaint(
      Sender       : TBaseVirtualTree;
      TargetCanvas : TCanvas
    );
    procedure FVirtualStringTreeAfterPaint(
      Sender       : TBaseVirtualTree;
      TargetCanvas : TCanvas
    );
    procedure FVirtualStringTreeBeforeItemErase(
      Sender          : TBaseVirtualTree;
      TargetCanvas    : TCanvas;
      Node            : PVirtualNode;
      ItemRect        : TRect;
      var ItemColor   : TColor;
      var EraseAction : TItemEraseAction
    );
    procedure FVirtualStringTreeAfterItemErase(
      Sender       : TBaseVirtualTree;
      TargetCanvas : TCanvas;
      Node         : PVirtualNode;
      ItemRect     : TRect
    );
    procedure FVirtualStringTreeBeforeItemPaint(
      Sender         : TBaseVirtualTree;
      TargetCanvas   : TCanvas;
      Node           : PVirtualNode;
      ItemRect       : TRect;
      var CustomDraw : Boolean
    );
    procedure FVirtualStringTreeAfterItemPaint(
      Sender       : TBaseVirtualTree;
      TargetCanvas : TCanvas;
      Node         : PVirtualNode;
      ItemRect     : TRect
    );
    procedure FVirtualStringTreeBeforeCellPaint(
      Sender          : TBaseVirtualTree;
      TargetCanvas    : TCanvas;
      Node            : PVirtualNode;
      Column          : TColumnIndex;
      CellPaintMode   : TVTCellPaintMode;
      CellRect        : TRect;
      var ContentRect : TRect
    );
    procedure FVirtualStringTreeAfterCellPaint(
      Sender       : TBaseVirtualTree;
      TargetCanvas : TCanvas;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      CellRect     : TRect
    );

    procedure FVirtualStringTreeBeforeDrawTreeLine(
      Sender   : TBaseVirtualTree;
      Node     : PVirtualNode;
      Level    : Integer;
      var PosX : Integer
    );

    procedure FVirtualStringTreeChange(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );

    procedure FVirtualStringTreeDblClick(Sender : TObject);
    procedure FVirtualStringTreeExpanding(
      Sender      : TBaseVirtualTree;
      Node        : PVirtualNode;
      var Allowed : Boolean
    );
    procedure FVirtualStringTreeExpanded(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure FVirtualStringTreeFocusChanging(
      Sender : TBaseVirtualTree;
      OldNode,
      NewNode : PVirtualNode;
      OldColumn, NewColumn : TColumnIndex;
      var Allowed          : Boolean
    );
    procedure FVirtualStringTreeFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
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
    procedure FVirtualStringTreeFreeNode(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
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
    procedure FVirtualStringTreePaintText(
      Sender             : TBaseVirtualTree;
      const TargetCanvas : TCanvas;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      TextType           : TVSTTextType
    );
    procedure FVirtualStringTreeMeasureItem(
      Sender         : TBaseVirtualTree;
      TargetCanvas   : TCanvas;
      Node           : PVirtualNode;
      var NodeHeight : Integer
    );
    {$ENDREGION}

  protected
    procedure ConnectEventHandlers;
    procedure CreateInspectors;
    procedure InitializeTree;
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.TypInfo, System.Rtti, System.StrUtils,

  VirtualTrees.Types, VirtualTrees.Header,

  DDuce.Logger, DDuce.Factories.VirtualTrees, DDuce.Factories.zObjInspector,

  Concepts.Types.Contact, Concepts.Factories;

const
  VisibleProperties : array of string = [
    'Color',
    'Colors',
    'DefaultNodeHeight',
    'DefaultText',
    'DragImageKind',
    'DragKind',
    'DragMode',
    'DragOperations',
    'DragType',
    'DragWidth',
    'DrawSelectionMode',
    'EmptyListMessage',
    'Enabled',
    'Font',
    'Header',
    'Hint',
    'HintMode',
    'Indent',
    'LineMode',
    'LineStyle',
    'Margin',
    'NodeAlignment',
    'ShowHint',
    'TextMargin',
    'TreeOptions',
    'Visible'
  ];

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
var
  LName : string;
begin
  LName := PItem.QualifiedName;
  LName := LName.Split(['.'], 2)[1];
  Result := not LName.Contains('ComObject')
    and (not (PItem.Prop.PropertyType is TRttiMethodType))
    and MatchText(LName, VisibleProperties);
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
procedure TfrmVirtualTreeView.FVirtualStringTreeBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  if CellPaintMode = cpmPaint then
  begin
    Logger.AddCheckPoint('OnBeforeCellPaint');
    Logger.IncCounter('OnBeforeCellPaint');
  end;
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
begin
  Logger.AddCheckPoint('OnAfterCellPaint');
  Logger.IncCounter('OnAfterCellPaint');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeBeforeDrawTreeLine(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Level: Integer;
  var PosX: Integer);
begin
  Logger.AddCheckPoint('OnBeforeDrawLineImage');
  Logger.IncCounter('OnBeforeDrawLineImage');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeBeforeItemErase(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
begin
  Logger.AddCheckPoint('OnBeforeItemErase');
  Logger.IncCounter('OnBeforeItemErase');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeAfterItemErase(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect);
begin
  Logger.AddCheckPoint('OnAfterItemErase');
  Logger.IncCounter('OnAfterItemErase');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeBeforeItemPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect; var CustomDraw: Boolean);
begin
  Logger.AddCheckPoint('OnBeforeItemPaint');
  Logger.IncCounter('OnBeforeItemPaint');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeAfterItemPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect);
begin
  Logger.AddCheckPoint('OnAfterItemPaint');
  Logger.IncCounter('OnAfterItemPaint');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeBeforePaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
begin
  Logger.AddCheckPoint('OnBeforePaint');
  Logger.IncCounter('OnBeforePaint');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeAfterPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
begin
  Logger.AddCheckPoint('OnAfterPaint');
  Logger.IncCounter('OnAfterPaint');
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

procedure TfrmVirtualTreeView.FVirtualStringTreeExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
begin
  Logger.Track(Self, 'FVirtualStringTreeExpanding');
  Logger.IncCounter('OnExpanding');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Logger.Track(Self, 'FVirtualStringTreeExpanded');
  Logger.IncCounter('OnExpanded');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  Logger.AddCheckPoint('OnFocusChanging');
  Logger.IncCounter('OnFocusChanging');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  Logger.AddCheckPoint('OnFocusChanged');
  Logger.IncCounter('OnFocusChanged');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Logger.IncCounter('OnInitNode');
  Node.SetData(TConceptFactories.CreateRandomContact);
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
  Logger.IncCounter('OnGetHint');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  Logger.IncCounter('OnGetImageIndex');
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
  Logger.IncCounter('OnGetText');
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

procedure TfrmVirtualTreeView.FVirtualStringTreeMeasureItem(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  var NodeHeight: Integer);
begin
  Logger.IncCounter('OnMeasureItem');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Logger.IncCounter('OnMouseUp');
end;

procedure TfrmVirtualTreeView.FVirtualStringTreePaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  if TextType = ttNormal then
  begin
    if Column = 0 then
      TargetCanvas.Font.Style := [fsBold];
    Logger.IncCounter('OnPaintText');
  end;
end;

procedure TfrmVirtualTreeView.FVirtualStringTreeStateChange(
  Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
begin
  Logger.Watch('Enter', TValue.From(Enter));
  Logger.Watch('Leave', TValue.From(Leave));
  if Enter <> [] then
  begin
    Logger.Send('Enter', TValue.From(Enter));
  end;
  if Leave <> [] then
  begin
    Logger.Send('Leave', TValue.From(Leave));
  end;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmVirtualTreeView.ConnectEventHandlers;
begin
  with FVirtualStringTree do
  begin
    OnChange                  := FVirtualStringTreeChange;
    OnStateChange             := FVirtualStringTreeStateChange;

    OnFocusChanging           := FVirtualStringTreeFocusChanging;
    OnFocusChanged            := FVirtualStringTreeFocusChanged;

    OnExpanding               := FVirtualStringTreeExpanding;
    OnExpanded                := FVirtualStringTreeExpanded;

    OnDblClick                := FVirtualStringTreeDblClick;
    OnMouseUp                 := FVirtualStringTreeMouseUp;

    OnGetText                 := FVirtualStringTreeGetText;
    OnGetImageIndex           := FVirtualStringTreeGetImageIndex;
    OnGetHint                 := FVirtualStringTreeGetHint;
    OnGetNodeDataSize         := FVirtualStringTreeGetNodeDataSize;

    OnHeaderDraw              := FVirtualStringTreeHeaderDraw;
    OnHeaderDrawQueryElements := FVirtualStringTreeHeaderDrawQueryElements;

    OnInitChildren            := FVirtualStringTreeInitChildren;

    OnInitNode                := FVirtualStringTreeInitNode;
    OnFreeNode                := FVirtualStringTreeFreeNode;

    OnBeforePaint             := FVirtualStringTreeBeforePaint;
    OnAfterPaint              := FVirtualStringTreeAfterPaint;

    OnBeforeItemPaint         := FVirtualStringTreeBeforeItemPaint;
    OnAfterItemPaint          := FVirtualStringTreeAfterItemPaint;
    OnBeforeCellPaint         := FVirtualStringTreeBeforeCellPaint;
    OnAfterCellPaint          := FVirtualStringTreeAfterCellPaint;
    OnBeforeItemErase         := FVirtualStringTreeBeforeItemErase;
    OnAfterItemErase          := FVirtualStringTreeAfterItemErase;

    OnBeforeDrawTreeLine      := FVirtualStringTreeBeforeDrawTreeLine;

    OnPaintText               := FVirtualStringTreePaintText;

    OnMeasureItem             := FVirtualStringTreeMeasureItem;
  end;
end;

procedure TfrmVirtualTreeView.InitializeTree;
begin
  with FVirtualStringTree do
  begin
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
    ConnectEventHandlers;
    RootNodeCount := 2;
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
