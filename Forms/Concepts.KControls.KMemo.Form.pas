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

{$I Concepts.inc}

unit Concepts.KControls.KMemo.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ActnList, Vcl.ImgList, Vcl.Menus,

  VirtualTrees, zObjInspector,

  DDuce.Components.VirtualTrees.Node, DDuce.ObjectInspector.zObjectInspector,

  kcontrols, kmemo, kgraphics, kpagecontrol, SynEdit;

type
  TBlockNode = TVTNode<TKMemoBlock>;

type
  TfrmKMemo = class(TForm)
    {$REGION 'designer controls'}
    aclMain           : TActionList;
    actAddTextBlock   : TAction;
    actClear          : TAction;
    actAddParagraph   : TAction;
    actAddImageBlock  : TAction;
    actAddTable       : TAction;
    actAddHyperLink   : TAction;
    actAddContainer   : TAction;
    actDeleteBlock    : TAction;
    actRebuildTree    : TAction;
    pnlRichEditor     : TPanel;
    pnlTree           : TPanel;
    splVertical       : TSplitter;
    imlMain           : TImageList;
    ppmMain           : TPopupMenu;
    mniAddTextBlock   : TMenuItem;
    mniAddContainer   : TMenuItem;
    mniAddHyperLink   : TMenuItem;
    mniAddImageBlock  : TMenuItem;
    mniAddParagraph   : TMenuItem;
    mniAddTable       : TMenuItem;
    pnlButtons        : TPanel;
    btnDeleteObject   : TButton;
    btnClear          : TButton;
    btnRebuildTree    : TButton;
    btnAddTable       : TButton;
    btnAddParagraph   : TButton;
    btnAddImageBlock  : TButton;
    btnAddHyperlink   : TButton;
    btnAddContainer   : TButton;
    btnAddTextBlock   : TButton;
    ppmTree           : TPopupMenu;
    mniDeleteBlock    : TMenuItem;
    mniN1             : TMenuItem;
    mniAddContainer1  : TMenuItem;
    mniAddHyperLink1  : TMenuItem;
    mniAddImageBlock1 : TMenuItem;
    mniAddParagraph1  : TMenuItem;
    mniAddTable1      : TMenuItem;
    mniAddTextBlock1  : TMenuItem;
    mniAdd            : TMenuItem;
    mniN1x11          : TMenuItem;
    mniN2x11          : TMenuItem;
    mniN3x11          : TMenuItem;
    mniN4x11          : TMenuItem;
    mniN2x12          : TMenuItem;
    mniN3x12          : TMenuItem;
    mniN4x12          : TMenuItem;
    pnlInspectors     : TPanel;
    pnlTop            : TPanel;
    pnlBottom         : TPanel;
    spl1              : TSplitter;
    btn1: TButton;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    actOpenFile: TAction;
    dlgOpenFile: TFileOpenDialog;
    btnOpenFile: TButton;
    dlgSaveFile: TFileSaveDialog;
    btnSaveFile: TButton;
    actSaveFile: TAction;
    pgcKMemo: TKPageControl;
    tsKMemo: TKTabSheet;
    tsRTF: TKTabSheet;
    KMemo: TKMemo;
    seRTF: TSynEdit;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAddTextBlockExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actAddParagraphExecute(Sender: TObject);
    procedure actAddImageBlockExecute(Sender: TObject);
    procedure actAddHyperLinkExecute(Sender: TObject);
    procedure actAddContainerExecute(Sender: TObject);
    procedure actDeleteBlockExecute(Sender: TObject);
    procedure actRebuildTreeExecute(Sender: TObject);
    procedure actAddTableExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure KMemoBlockClick(
      Sender     : TObject;
      ABlock     : TKMemoBlock;
      var Result : Boolean
    );
    procedure KMemoChange(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure tsRTFEnter(Sender: TObject);
    {$ENDREGION}

  private
    FTree            : TVirtualStringTree;
    FRootNode        : TBlockNode;
    FRootData        : TKMemoBlock;
    FBlockInspector  : TzObjectInspector;
    FBlocksInspector : TzObjectInspector;
    FUpdate          : Boolean;

    {$REGION 'property access methods'}
    function GetKMemoNotifier: IKMemoNotifier;
    function GetSelectedBlock: TKMemoBlock;
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure FTreeFreeNode(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure FTreeGetHint(
      Sender             : TBaseVirtualTree;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      var LineBreakStyle : TVTTooltipLineBreakStyle;
      var HintText       : string
    );
    procedure FTreeGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );
    procedure FTreeFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );
    procedure FTreeGetImageIndex(
      Sender         : TBaseVirtualTree;
      Node           : PVirtualNode;
      Kind           : TVTImageKind;
      Column         : TColumnIndex;
      var Ghosted    : Boolean;
      var ImageIndex : TImageIndex
    );
    {$ENDREGION}

  protected
    function AddBlockToTree(
      ABlock  : TKMemoBlock;
      AParent : TKMemoBlock = nil
    ): TBlockNode;

//    function AddBlockToTree(
//      ABlock      : TKMemoBlock;
//      AParentNode : TBlockNode = nil
//    ): TBlockNode;
    procedure BuildTreeView(AKMemo: TKMemo);
    function AddNodes(AKMemoBlock: TKMemoContainer): Boolean;

    function AddTextBlock: TKMemoTextBlock;
    function AddParagraphBlock: TKMemoParagraph;
    function AddHyperLinkBlock: TKMemoHyperlink;
    function AddContainerBlock: TKMemoContainer;
    function AddImageBlock: TKMemoImageBlock;
    function AddTable: TKMemoTable;

    procedure CreateTreeView;
    procedure CreateInspectors;
    procedure ClearTree;
    procedure DeleteBlock;

    procedure Modified;
    function ActiveBlocks: TKMemoBlocks;
    function ParentBlock: TKMemoBlock;
    procedure UpdateActions; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property SelectedBlock: TKMemoBlock
      read GetSelectedBlock;

    property KMemoNotifier: IKMemoNotifier
      read GetKMemoNotifier;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Logger, DDuce.Factories.zObjInspector;

{$REGION 'construction and destruction'}
constructor TfrmKMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateTreeView;
  //CreateInspectors;
end;

destructor TfrmKMemo.Destroy;
begin
  FRootData.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmKMemo.GetKMemoNotifier: IKMemoNotifier;
begin
  Result := KMemo as IKMemoNotifier;
end;

function TfrmKMemo.GetSelectedBlock: TKMemoBlock;
begin
  Result := KMemoNotifier.GetSelectedBlock;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmKMemo.actAddContainerExecute(Sender: TObject);
begin
  AddContainerBlock;
end;

procedure TfrmKMemo.actAddHyperLinkExecute(Sender: TObject);
begin
  AddHyperLinkBlock;
end;

procedure TfrmKMemo.actAddImageBlockExecute(Sender: TObject);
begin
//  AddImageBlock;
end;

procedure TfrmKMemo.actAddParagraphExecute(Sender: TObject);
begin
  AddParagraphBlock;
end;

procedure TfrmKMemo.actAddTableExecute(Sender: TObject);
begin
  AddTable;
end;

procedure TfrmKMemo.actAddTextBlockExecute(Sender: TObject);
begin
  AddTextBlock;
end;

procedure TfrmKMemo.actClearExecute(Sender: TObject);
begin
  ClearTree;
end;

procedure TfrmKMemo.actDeleteBlockExecute(Sender: TObject);
begin
  DeleteBlock;
end;

procedure TfrmKMemo.actOpenFileExecute(Sender: TObject);
begin
  if dlgOpenFile.Execute then
  begin
    KMemo.LoadFromFile(dlgOpenFile.FileName);
  end;
end;

procedure TfrmKMemo.actRebuildTreeExecute(Sender: TObject);
begin
  BuildTreeView(KMemo);
end;

procedure TfrmKMemo.actSaveFileExecute(Sender: TObject);
begin
  if dlgSaveFile.Execute then
  begin
    KMemo.SaveToFile(dlgSaveFile.FileName);
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
{$REGION 'FTree'}
procedure TfrmKMemo.FTreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LNode : TBlockNode;
begin
  if Assigned(Node) then
  begin
    LNode := Sender.GetNodeData<TBlockNode>(Node);
    if Assigned(LNode) and (LNode <> FRootNode) and Assigned(LNode.Data) then
    begin
      (KMemo as IKMemoNotifier).SelectBlock(LNode.Data, sgpNone);

//      FBlockInspector.Component  := LNode.Data;
//      FBlocksInspector.Component := LNode.Data.ParentBlocks;
    end
    else
    begin
      KMemo.Select(0,0);
    end;
  end;
end;

procedure TfrmKMemo.FTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  LNode : TBlockNode;
begin
  LNode := Sender.GetNodeData<TBlockNode>(Node);
  LNode.Free;
end;

procedure TfrmKMemo.FTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: string);
var
  LNode : TBlockNode;
begin
  //Logger.Track(Self, 'FTreeGetHint');
  LNode  := TBlockNode(Sender.GetNodeData(Node)^);
  if LNode.Text.IsEmpty then
  begin
    HintText := LNode.Data.Text;
  end
end;

procedure TfrmKMemo.FTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  LNode : TBlockNode;
begin
  if Kind in [ikNormal, ikSelected] then
  begin
    LNode := Sender.GetNodeData<TBlockNode>(Node);
    if Assigned(LNode) and Assigned(LNode.Data) then
      ImageIndex := LNode.ImageIndex;
  end;
end;

procedure TfrmKMemo.FTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  LNode : TBlockNode;
begin
  LNode  := TBlockNode(Sender.GetNodeData(Node)^);
  if LNode.Text.IsEmpty then
  begin
    CellText := LNode.Data.ClassName;
  end
  else
  begin
    CellText := LNode.Text;
  end;
end;



{$ENDREGION}

{$REGION 'KMemo'}
procedure TfrmKMemo.KMemoBlockClick(Sender: TObject; ABlock: TKMemoBlock;
  var Result: Boolean);
var
  LNode : TBlockNode;
begin
  LNode := FRootNode.Find(ABlock);
  if Assigned(LNode) and Assigned(LNode.VNode) then
  begin
    FTree.Selected[LNode.VNode] := True;
    if Assigned(LNode.Data) then
    begin
//      FBlockInspector.Component  := LNode.Data;
//      FBlocksInspector.Component := LNode.Data.ActiveBlocks;
    end;
  end;
end;

procedure TfrmKMemo.KMemoChange(Sender: TObject);
begin
  if KMemo.Focused then
    Modified;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmKMemo.Modified;
begin
  FUpdate := True;
end;

function TfrmKMemo.ParentBlock: TKMemoBlock;
begin
//  if KMemo.SelectedBlock is TKMemoTableCell then
//    Result := (KMemo.SelectedBlock as TKMemoTableCell).ParentRow
//  else if KMemo.SelectedBlock is TKMemoTableRow then
//    Result := (KMemo.SelectedBlock as TKMemoTableRow).ParentTable


end;

procedure TfrmKMemo.tsRTFEnter(Sender: TObject);
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create;
  try
    //KMemo.ActiveBlocks := KMemo.Blocks;
    KMemo.SaveToRTFStream(LStream, False, True);
    seRTF.Lines.Text := LStream.DataString;
  finally
    LStream.Free;
  end;
end;

procedure TfrmKMemo.UpdateActions;
begin
  inherited UpdateActions;
  if FUpdate then
  begin
    BuildTreeView(KMemo);
    FUpdate := False;
  end;
end;

function TfrmKMemo.ActiveBlocks: TKMemoBlocks;
begin
  if Assigned(KMemo.SelectedBlock) then
  begin
    Result := KMemo.SelectedBlock.ActiveBlocks;
  end
  else
  begin
    Result := KMemo.Blocks;
  end;
end;

{ Creates a new treeview node and attaches it to the node associated with
  the given parent block, or simply adds it to the root if it AParent is nil. }

function TfrmKMemo.AddBlockToTree(ABlock, AParent: TKMemoBlock): TBlockNode;
var
  LNode : TBlockNode;
  LNew  : TBlockNode;
begin
  LNode := nil;
  if Assigned(AParent) then
  begin
    Logger.SendObject('AParent', AParent);
    LNode := FRootNode.Find(AParent);
    if Assigned(LNode) then
      Logger.SendObject('LNode', LNode.Data);
  end;
  if not Assigned(LNode) then
  begin
    LNode := FRootNode;
  end;
  LNew := LNode.Add(ABlock, False);
  if ABlock is TKMemoParagraph then
  begin
    LNew.ImageIndex := 0;
  end
  else if ABlock is TKMemoHyperlink then
  begin
    LNew.ImageIndex := 3;
  end
  else if ABlock is TKMemoTextBlock then
  begin
    LNew.ImageIndex := 1;
  end
  else if ABlock is TKMemoImageBlock then
  begin
    LNew.ImageIndex := 2;
  end
  else if ABlock is TKMemoTable then
  begin
    LNew.ImageIndex := 5;
  end
  else if ABlock is TKMemoTableRow then
  begin
    LNew.ImageIndex := 6;
  end
  else if ABlock is TKMemoTableCell then
  begin
    LNew.ImageIndex := 7;
  end
  else if ABlock is TKMemoContainer then
  begin
    LNew.ImageIndex := 4;
  end;
  //FRootNode.Expanded := True;
  Result := LNew;
end;

//function TfrmKMemo.AddBlockToTree(ABlock: TKMemoBlock;
//  AParentNode: TBlockNode): TBlockNode;
//begin
//  if Assigned(AParentNode) then
//  begin
//    AParentNode.Data.ActiveBlocks.A
//  end;
//end;

function TfrmKMemo.AddContainerBlock: TKMemoContainer;
var
  CO: TKMemoContainer;
begin
  CO := ActiveBlocks.AddContainer;
  CO.Position := mbpAbsolute;
  AddBlockToTree(CO, ActiveBlocks.Parent).Expanded := True;
  CO.FixedWidth := True;
  AddBlockToTree(CO.Blocks.AddTextBlock('Text in a container!'), CO);
  Result := CO;
end;

function TfrmKMemo.AddHyperLinkBlock: TKMemoHyperlink;
var
  HL : TKMemoHyperlink;
begin
  HL := ActiveBlocks.AddHyperlink('testlink', 'www.test.com');
  AddBlockToTree(HL, ActiveBlocks.Parent);
  Result := HL;
end;

function TfrmKMemo.AddImageBlock: TKMemoImageBlock;
var
  LBitmap     : TBitmap;
  LPicture    : TPicture;
  LImageBlock : TKMemoImageBlock;
begin
  LPicture := TPicture.Create;
  LBitmap := GetFormImage;
  LBitmap.Transparent := False;
  LBitmap.TransparentColor := clBlack;
  LPicture.Assign(LBitmap);

  LBitmap.SetSize(100, 100);
  LBitmap.Canvas.Brush.Color := clYellow;
  LBitmap.Transparent := False;

  LBitmap.Canvas.Pen.Color := clBlack;
  LBitmap.Canvas.Pen.Style := psSolid;
  LBitmap.Canvas.Pen.Width := 2;

  LBitmap.Canvas.MoveTo(0, 0);
  LBitmap.Canvas.LineTo(100, 100);
  LBitmap.Canvas.MoveTo(100, 0);
  LBitmap.Canvas.LineTo(0, 100);
  try
    LImageBlock := ActiveBlocks.AddImageBlock(LPicture);
    AddBlockToTree(LImageBlock);
  finally
    LPicture.Free;
  end;
  Result := LImageBlock;
end;

{ Recursively attaches new nodes for every child block in the given
  Container block. }

function TfrmKMemo.AddNodes(AKMemoBlock: TKMemoContainer): Boolean;
var
  I      : Integer;
  LBlock : TKMemoBlock;
  LTable : TKMemoTable;
  LRow   : TKMemoTableRow;
  LCell  : TKMemoTableCell;
begin
  Result := True;
  Logger.Send('AKMemoBlock', AKMemoBlock.Text);
  if AKMemoBlock is TKMemoTable then
  begin
    LTable := AKMemoBlock as TKMemoTable;
    for I := 0 to LTable.RowCount - 1 do
    begin
      LRow := LTable.Rows[I];
      AddBlockToTree(LRow, LTable);
      AddNodes(LRow);
    end;
  end
  else if AKMemoBlock is TKMemoTableRow then
  begin
    LRow := AKMemoBlock as TKMemoTableRow;
    for I := 0 to LRow.CellCount - 1 do
    begin
      LCell := LRow.Cells[I];
      AddBlockToTree(LCell, LRow);
      AddNodes(LCell);
    end;
  end
  else if AKMemoBlock is TKMemoTableCell then
  begin
    LCell := AKMemoBlock as TKMemoTableCell;
    for I := 0 to LCell.Blocks.Count - 1 do
    begin
      LBlock := LCell.Blocks[I];
      AddBlockToTree(LBlock, LCell);
      if LBlock is TKMemoContainer then
        AddNodes(LBlock as TKMemoContainer);
    end;
  end
  else
  begin
    for I := 0 to AKMemoBlock.Blocks.Count - 1 do
    begin
      LBlock := AKMemoBlock.Blocks.Items[I];
      AddBlockToTree(LBlock, AKMemoBlock);
      if LBlock is TKMemoContainer then
        AddNodes(LBlock as TKMemoContainer);
    end;
  end;
end;

function TfrmKMemo.AddParagraphBlock: TKMemoParagraph;
var
  TB : TKMemoTextBlock;
  PA : TKMemoParagraph;
begin
  PA := ActiveBlocks.AddParagraph;
  PA.ParaStyle.HAlign := halLeft;
  PA.ParaStyle.BottomPadding := 20;
  AddBlockToTree(PA, ActiveBlocks.Parent);
  Result := PA;
end;

function TfrmKMemo.AddTable: TKMemoTable;
var
  LTable     : TKMemoTable;
  X          : Integer;
  Y          : Integer;
  LTextBlock : TKMemoTextBlock;
  LNode      : TBlockNode;
begin
  LTable := ActiveBlocks.AddTable;
  LNode := AddBlockToTree(LTable, ActiveBlocks.Parent);
  LTable.ColCount := 4;
  LTable.RowCount := 4;
  for Y := 0 to LTable.RowCount - 1 do
  begin
    AddBlockToTree(LTable.Rows[Y], LTable);
    for X := 0 to LTable.ColCount - 1 do
    begin
      AddBlockToTree(LTable.Cells[X, Y], LTable.Rows[Y]);
      LTextBlock :=
        LTable.Cells[X, Y].Blocks.AddTextBlock(Format('Cell(%d, %d)', [X, Y]));
      AddBlockToTree(LTextBlock, LTable.Cells[X, Y]);
    end;
  end;
  LTable.CellStyle.BorderWidth := 2;
  LTable.ApplyDefaultCellStyle;
  Result := LTable;
end;

function TfrmKMemo.AddTextBlock: TKMemoTextBlock;
var
  LTextBlock : TKMemoTextBlock;
begin
  LTextBlock := ActiveBlocks.AddTextBlock('Hello world!' + #13#10);
  LTextBlock.TextStyle.Font.Name := 'Arial';
  LTextBlock.TextStyle.Font.Color := clRed;
  LTextBlock.TextStyle.Font.Style := [fsBold];
  AddBlockToTree(LTextBlock, ActiveBlocks.Parent);
  Result := LTextBlock;
end;

{ (Re)builds the treeview structure based on the contents of the given
  TKMemo control. }

procedure TfrmKMemo.BuildTreeView(AKMemo: TKMemo);
var
  I      : Integer;
  LBlock : TKMemoBlock;
begin
  Logger.Track(Self, 'BuildTreeView');
  KMemo.LockUpdate;
  try
    ClearTree;
    if AKMemo.Blocks.Count > 0 then
    begin
      for I := 0 to AKMemo.Blocks.Count - 1 do
      begin
        LBlock := AKMemo.Blocks.Items[I];
        AddBlockToTree(LBlock);
        if LBlock is TKMemoContainer then
          AddNodes(LBlock as TKMemoContainer);
      end;
    end;
    FTree.FullExpand;
  finally
    KMemo.UnlockUpdate;
  end;
end;

{ Clears all nodes in the treeview and creates a new root node holding the
  root data. It does not touch the content of the KMemo control. }

procedure TfrmKMemo.ClearTree;
begin
  FTree.Clear;
  FRootNode := TBlockNode.Create(FTree, FRootData, False);
  FRootNode.Text := 'Document root';
end;

procedure TfrmKMemo.CreateInspectors;
begin
  FBlocksInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlTop,
    nil
  );
  FBlockInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlBottom,
    nil
  );
end;

{ Creates the TVirtualStringTree component and root node, which is attached to
  the tree. }

procedure TfrmKMemo.CreateTreeView;
begin
  Logger.Track(Self, 'CreateTreeView');
  FTree := TVirtualStringTree.Create(Self);
  FTree.Parent          := pnlTree;
  FTree.Align           := alClient;
  FTree.ShowHint        := True;
  FTree.BorderStyle     := bsNone;
  FTree.OnFreeNode      := FTreeFreeNode;
  FTree.OnGetText       := FTreeGetText;
  FTree.OnGetHint       := FTreeGetHint;
  FTree.OnGetImageIndex := FTreeGetImageIndex;
  FTree.OnFocusChanged  := FTreeFocusChanged;
  FTree.Images          := imlMain;
  FTree.PopupMenu       := ppmTree;
  FRootData             := TKMemoBlock.Create;
  FRootNode             := TBlockNode.Create(FTree, FRootData, False);
  FRootNode.Text        := 'Document root';
end;

procedure TfrmKMemo.DeleteBlock;
begin
  KMemo.DeleteSelectedBlock;
  FTree.DeleteNode(FTree.FocusedNode);
end;
{$ENDREGION}

end.
