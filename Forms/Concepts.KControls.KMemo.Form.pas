{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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
  Winapi.Windows, Winapi.Messages, Winapi.ActiveX,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ActnList, Vcl.ImgList, Vcl.Menus,

  VirtualTrees, zObjInspector,

  DDuce.Components.VirtualTrees.Node, DDuce.ObjectInspector.zObjectInspector,

  kcontrols, kmemo, kgraphics, kpagecontrol;

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
    spl1              : TSplitter;
    btn1              : TButton;
    Panel1            : TPanel;
    Button1           : TButton;
    Button2           : TButton;
    Button3           : TButton;
    Button4           : TButton;
    Button5           : TButton;
    Button6           : TButton;
    Button7           : TButton;
    Button8           : TButton;
    Button9           : TButton;
    Button10          : TButton;
    actOpenFile       : TAction;
    dlgOpenFile       : TFileOpenDialog;
    btnOpenFile       : TButton;
    dlgSaveFile       : TFileSaveDialog;
    btnSaveFile       : TButton;
    actSaveFile       : TAction;
    KMemo             : TKMemo;
    Button11: TButton;
    actAddBulletList: TAction;
    pnlGrid: TGridPanel;
    pnlInspector00: TPanel;
    pnlSelectedBlockTitle: TPanel;
    pnlInspector01: TPanel;
    pnlActiveBlockTitle: TPanel;
    pnlInspector10: TPanel;
    pnlBlocksTitle: TPanel;
    pnlInspector11: TPanel;
    pnlActiveBlocksTitle: TPanel;
    pnlInspector20: TPanel;
    pnlTextStyleTitle: TPanel;
    pnlInspector21: TPanel;
    pnlParaStyleTitle: TPanel;
    pnlInspector02: TPanel;
    pnlActiveInnerBlockTitle: TPanel;
    pnlInspector12: TPanel;
    pnlActiveInnerBlocksTitle: TPanel;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
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
    procedure actOpenFileExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure actAddBulletListExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure KMemoBlockClick(
      Sender     : TObject;
      ABlock     : TKMemoBlock;
      var Result : Boolean
    );
    procedure KMemoChange(Sender: TObject);
    procedure tsRTFEnter(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    {$ENDREGION}

  private
    FTree                       : TVirtualStringTree;
    FRootNode                   : TBlockNode;
    FRootData                   : TKMemoBlock;
    FSelectedBlockInspector     : TzObjectInspector;
    FActiveBlockInspector       : TzObjectInspector;
    FActiveInnerBlockInspector  : TzObjectInspector;
    FBlocksInspector            : TzObjectInspector;
    FActiveBlocksInspector      : TzObjectInspector;
    FActiveInnerBlocksInspector : TzObjectInspector;
    FTextStyleInspector         : TzObjectInspector;
    FParaStyleInspector         : TzObjectInspector;
    FUpdate                     : Boolean;
    //FDraggedNode                : TBlockNode;

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
    procedure FTreeDragAllowed(
      Sender      : TBaseVirtualTree;
      Node        : PVirtualNode;
      Column      : TColumnIndex;
      var Allowed : Boolean
    );
    procedure FTreeDragDrop(
      Sender     : TBaseVirtualTree;
      Source     : TObject;
      DataObject : IDataObject;
      Formats    : TFormatArray;
      Shift      : TShiftState;
      Pt         : TPoint;
      var Effect : Integer;
      Mode       : TDropMode
    );
    procedure FTreeDragOver(
      Sender     : TBaseVirtualTree;
      Source     : TObject;
      Shift      : TShiftState;
      State      : TDragState;
      Pt         : TPoint;
      Mode       : TDropMode;
      var Effect : Integer;
      var Accept : Boolean
    );

    function GetFocusedNode: TBlockNode;
    {$ENDREGION}

  protected
    function AddBlockToTree(
      ABlock  : TKMemoBlock;
      AParent : TKMemoBlock = nil
    ): TBlockNode;
    procedure BuildTreeView;
    function AddNodes(AKMemoBlock: TKMemoContainer): Boolean;

    function AddBlockWithAutoParagraph(
      ABlocks   : TKMemoBlocks;
      ABlock    : TKMemoBlock;
      AIndex    : TKMemoBlockIndex = -1
    ): TKMemoBlockIndex;

    function AddTextBlock: TKMemoTextBlock;
    function AddParagraphBlock: TKMemoParagraph;
    function AddBulletList: TKMemoParagraph;
    function AddHyperLinkBlock: TKMemoHyperlink;
    function AddContainerBlock: TKMemoContainer;
    function AddImageBlock: TKMemoImageBlock;
    function AddTable: TKMemoTable;

    procedure CreateTreeView;
    procedure CreateInspectors;
    procedure ClearTree;
    procedure DeleteBlock;
    procedure CutBlock;
    procedure CopyBlock;
    procedure PasteBlock;

    procedure Modified;
    function ActiveBlocks: TKMemoBlocks;

    procedure UpdateInspectors;
    procedure UpdateActions; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

     property SelectedBlock: TKMemoBlock
      read GetSelectedBlock;

    property FocusedNode: TBlockNode
      read GetFocusedNode;

    property KMemoNotifier: IKMemoNotifier
      read GetKMemoNotifier;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Logger, DDuce.Factories.zObjInspector, Vcl.Imaging.pngimage,

  keditcommon;

//procedure ConvertStringToKMemoTable(tableStr: string; memo: TKMemo);
//var
//  rows: TStringList;
//  i, j: Integer;
//  cells: TStringList;
//begin
//  rows := TStringList.Create;
//  cells := TStringList.Create;
//  try
//    rows.Text := tableStr;
//    for i := 0 to rows.Count - 1 do
//    begin
//      cells.Clear;
//      ExtractStrings(['\t'], [], PChar(rows[i]), cells);
//      for j := 0 to cells.Count - 1 do
//      begin
//        if j = 0 then
//          memo.AppendTableCell(cells[j])
//        else
//          memo.AppendTableCell(cells[j], False);
//      end;
//      memo.AppendTableRow;
//    end;
//  finally
//    rows.Free;
//    cells.Free;
//  end;
//
//var
//  LTable     : TKMemoTable;
//  X          : Integer;
//  Y          : Integer;
//  LTextBlock : TKMemoTextBlock;
//  LNode      : TBlockNode;
//begin
//  LTable := ActiveBlocks.AddTable;
//  LNode := AddBlockToTree(LTable, ActiveBlocks.Parent);
//  LTable.ColCount := 4;
//  LTable.RowCount := 4;
//  for Y := 0 to LTable.RowCount - 1 do
//  begin
//    AddBlockToTree(LTable.Rows[Y], LTable);
//    for X := 0 to LTable.ColCount - 1 do
//    begin
//      AddBlockToTree(LTable.Cells[X, Y], LTable.Rows[Y]);
//      LTextBlock :=
//        LTable.Cells[X, Y].Blocks.AddTextBlock(Format('Cell(%d, %d)', [X, Y]));
//      AddBlockToTree(LTextBlock, LTable.Cells[X, Y]);
//    end;
//  end;
//  LTable.CellStyle.BorderWidth := 2;
//  LTable.ApplyDefaultCellStyle;
//  Result := LTable;
//end;


{$REGION 'construction and destruction'}
constructor TfrmKMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateTreeView;
  CreateInspectors;
  KMemo.OnChange     := KMemoChange;
  KMemo.OnBlockClick := KMemoBlockClick;
end;

destructor TfrmKMemo.Destroy;
begin
  FRootData.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmKMemo.GetFocusedNode: TBlockNode;
begin
  Result := FTree.GetNodeData<TBlockNode>(FTree.FocusedNode);
end;

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
procedure TfrmKMemo.actAddBulletListExecute(Sender: TObject);
begin
  AddBulletList;
  KMemo.SetFocus;
end;

procedure TfrmKMemo.actAddContainerExecute(Sender: TObject);
begin
  AddContainerBlock;
  KMemo.SetFocus;
end;

procedure TfrmKMemo.actAddHyperLinkExecute(Sender: TObject);
begin
  AddHyperLinkBlock;
  KMemo.SetFocus;
end;

procedure TfrmKMemo.actAddImageBlockExecute(Sender: TObject);
begin
  AddImageBlock;
  KMemo.SetFocus;
end;

procedure TfrmKMemo.actAddParagraphExecute(Sender: TObject);
begin
  AddParagraphBlock;
  KMemo.SetFocus;
end;

procedure TfrmKMemo.actAddTableExecute(Sender: TObject);
begin
  AddTable;
  KMemo.SetFocus;
end;

procedure TfrmKMemo.actAddTextBlockExecute(Sender: TObject);
begin
  AddTextBlock;
  KMemo.SetFocus;
end;

procedure TfrmKMemo.actClearExecute(Sender: TObject);
begin
  ClearTree;
end;

procedure TfrmKMemo.actCopyExecute(Sender: TObject);
begin
//
end;

procedure TfrmKMemo.actCutExecute(Sender: TObject);
begin
  CutBlock;
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

procedure TfrmKMemo.actPasteExecute(Sender: TObject);
begin
  PasteBlock;
end;

procedure TfrmKMemo.actRebuildTreeExecute(Sender: TObject);
begin
  BuildTreeView;
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
procedure TfrmKMemo.FTreeDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
//  FDraggedNode := Sender.GetNodeData<TBlockNode>(Node);

  Allowed := True;
end;

procedure TfrmKMemo.FTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
    Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
//  LAttachMode : TVTNodeAttachMode;
  LNode       : TBlockNode;
begin
  LNode := Sender.GetNodeData<TBlockNode>(Sender.GetNodeAt(Pt.x, Pt.y));

 if Assigned(LNode) and (LNode <> FRootNode) and Assigned(LNode.Data) then
  begin
    KMemo.ExecuteCommand(ecCut);
    (KMemo as IKMemoNotifier).SelectBlock(LNode.Data, sgpNone);
    KMemo.SelectedBlock.Select(KMemo.SelectedBlock.SelEnd, 0, False);
//    KMemo.Select(KMemo.SelectedBlock.SelEnd, 0, False);
//      Sender.FocusedNode := LNode.VNode;
     KMemo.ExecuteCommand(ecPaste);
     Modified;
  end;

//  if Mode = dmOnNode then
//    LAttachMode := amInsertBefore
//  else if Mode = dmAbove then
//    LAttachMode := amInsertBefore
//  else if Mode = dmBelow then
//    LAttachMode := amInsertAfter
//  else
//    LAttachMode := amAddChildLast;
end;

procedure TfrmKMemo.FTreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
    Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := True;
end;

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
      if Sender.Focused then
        (KMemo as IKMemoNotifier).SelectBlock(LNode.Data, sgpNone);

      if Assigned(LNode.Data) then
      begin
        FSelectedBlockInspector.Component  := LNode.Data;
        if Assigned(LNode.Data.ParentBlocks) then
          FActiveBlocksInspector.Component := LNode.Data.ParentBlocks;
      end;
    end
    else
    begin
      if Sender.Focused then
        KMemo.Select(0,0);
    end;
  end;
  UpdateInspectors;
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
  LIndex : TKMemoBlockIndex;
begin
  LNode  := TBlockNode(Sender.GetNodeData(Node)^);
  if LNode.Text.IsEmpty then
  begin

    LIndex := KMemo.Blocks.BlockToIndex(LNode.Data);
    CellText := Format('[%4d] %s', [LIndex, LNode.Data.ClassName]);

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
//      FActiveBlocksInspector.Component := LNode.Data.ActiveBlocks;
    end;
  end;
  UpdateInspectors;
end;

procedure TfrmKMemo.KMemoChange(Sender: TObject);
begin
  if KMemo.Focused then
    Modified;
end;
{$ENDREGION}
procedure TfrmKMemo.tsRTFEnter(Sender: TObject);
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create;
  try
    //KMemo.ActiveBlocks := KMemo.Blocks;
    KMemo.SaveToRTFStream(LStream, False, True);
    //seRTF.Lines.Text := LStream.DataString;
  finally
    LStream.Free;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmKMemo.Modified;
begin
  FUpdate := True;
end;

procedure TfrmKMemo.PasteBlock;
begin

end;

procedure TfrmKMemo.UpdateActions;
begin
  inherited UpdateActions;
  if FUpdate then
  begin
    BuildTreeView;
    UpdateInspectors;
    FUpdate := False;
  end;
end;

procedure TfrmKMemo.UpdateInspectors;
  procedure UpdateCaption(APanel: TPanel; AObject: TObject; const ACaption: string);
  var
    S : string;
  begin
    if Assigned(AObject) then
    begin
      S := Format('%s(%s: %d)', [ACaption, AObject.ClassName, Integer(AObject)]);
    end
    else
    begin
      S := ACaption;
    end;
    APanel.Caption := S;
  end;

begin
  if not KMemo.Focused then
    Exit;
{
  if Assigned(KMemo.SelectedBlock) then
    FSelectedBlockInspector.Component     := KMemo.SelectedBlock;
  if Assigned(KMemo.ActiveBlock) then
    FActiveBlockInspector.Component       := KMemo.ActiveBlock;
  if Assigned(KMemo.ActiveInnerBlock) then
    FActiveInnerBlockInspector.Component  := KMemo.ActiveInnerBlock;

  if Assigned(KMemo.Blocks) then
    FBlocksInspector.Component            := KMemo.Blocks;
  FActiveBlocksInspector.Component      := KMemo.ActiveBlocks;
  FActiveInnerBlocksInspector.Component := KMemo.ActiveInnerBlocks;

  if Assigned(KMemo.ActiveInnerBlocks) then
  begin
    FParaStyleInspector.Component := KMemo.ActiveInnerBlocks.SelectionParaStyle;
    FTextStyleInspector.Component := KMemo.ActiveInnerBlocks.SelectionTextStyle;
  end
  else if Assigned(KMemo.ActiveBlocks) then
  begin
    FParaStyleInspector.Component := KMemo.ActiveBlocks.SelectionParaStyle;
    FTextStyleInspector.Component := KMemo.ActiveBlocks.SelectionTextStyle;
  end
  else
  begin
    FParaStyleInspector.Component := nil;
    FTextStyleInspector.Component := nil;
  end;
  UpdateCaption(pnlSelectedBlockTitle, FSelectedBlockInspector.Component, 'SB');
  UpdateCaption(pnlActiveBlockTitle, FActiveBlockInspector.Component, 'AB');
  UpdateCaption(pnlActiveInnerBlockTitle, FActiveInnerBlockInspector.Component, 'AIB');

  UpdateCaption(pnlBlocksTitle, FBlocksInspector.Component, 'Bs');
  UpdateCaption(pnlActiveBlocksTitle, FActiveBlocksInspector.Component, 'ABs');
  UpdateCaption(pnlActiveInnerBlocksTitle, FActiveInnerBlocksInspector.Component, 'AIBs');

  UpdateCaption(pnlTextStyleTitle, FTextStyleInspector.Component, 'TS');
  UpdateCaption(pnlParaStyleTitle, FParaStyleInspector.Component, 'PS');

  FSelectedBlockInspector.UpdateProperties(True);
  FActiveBlockInspector.UpdateProperties(True);
  FBlocksInspector.UpdateProperties(True);
  FActiveInnerBlockInspector.UpdateProperties(True);
  FActiveBlocksInspector.UpdateProperties(True);
  FActiveInnerBlocksInspector.UpdateProperties(True);
  FParaStyleInspector.UpdateProperties(True);
  FTextStyleInspector.UpdateProperties(True);
  }
end;

function TfrmKMemo.ActiveBlocks: TKMemoBlocks;
begin
  if Assigned(SelectedBlock) then
  begin
    if SelectedBlock is TKMemoContainer then
    begin
      Result := (SelectedBlock as TKMemoContainer).Blocks;
      Logger.Info('ActiveBlocks=SelectedBlock.Blocks');
    end
    else
    begin
      Result := SelectedBlock.ParentBlocks;
      Logger.Info('ActiveBlocks=SelectedBlock.ParentBlocks');
    end;
  end
  else
  begin
    Result := KMemo.ActiveBlocks;
    Logger.Info('KMemo.ActiveBlocks');
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
  if Assigned(ABlock) then
  begin
    Logger.Send('ABlock', ABlock.ClassName);
    if Assigned(AParent) then
    begin
    //  Logger.SendObject('AParent', AParent);
      LNode := FRootNode.Find(AParent);
  //    if Assigned(LNode) then
  //      Logger.SendObject('LNode', LNode.Data);
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
   // FRootNode.Expanded := True;
    Result := LNew;
  end;
end;

function TfrmKMemo.AddBlockWithAutoParagraph(ABlocks: TKMemoBlocks;
  ABlock: TKMemoBlock; AIndex: TKMemoBlockIndex): TKMemoBlockIndex;
//var
//  InsertIndex  : TKMemoBlockIndex;
//  NextBlock    : TKMemoBlock;
//  NewParagraph : TKMemoParagraph;
begin
//  if AIndex = -1 then
//    InsertIndex := ABlocks.Las
//
//
//  ActiveBlocks.InsertNewLine(AIndex);
////  (KMemo as IKMemoNotifier).SelectBlock(LTable, sgpNone);
//
//
////  InsertIndex := ABlocks.AddAt(ABlock, AIndex);
//
//
//  InsertIndex := AIndex + 1;
//  // If the next item after InsertIndex is NOT a paragraph, add one
//  if InsertIndex < ABlocks.Count - 1 then
//    NextBlock := ABlocks[InsertIndex + 1]
//  else
//    NextBlock := nil;
//
//  if not (NextBlock is TKMemoParagraph) then
//  begin
//  //  NewParagraph := ABlocks.AddParagraph(InsertIndex + 1);
//    // Now place the caret in that new paragraph
////    KMemoNotifier.SelectBlock(NextBlock, sgpNone);
//  end
//  else
//  begin
//    // The next block is a paragraph, so just place caret there
//    KMemoNotifier.SelectBlock(NextBlock, sgpNone);
//  end;
//
//  Result := InsertIndex;

end;

function TfrmKMemo.AddBulletList: TKMemoParagraph;
var
  PA : TKMemoParagraph;
begin
  PA := ActiveBlocks.AddParagraph;
  PA.Numbering := pnuBullets;
  PA.ParaStyle.LineSpacingMode := lsmValue;
  PA.ParaStyle.HAlign := halLeft;
  ActiveBlocks.Add(PA);
  Result := PA;
  AddParagraphBlock;
  Modified;
end;

function TfrmKMemo.AddContainerBlock: TKMemoContainer;
var
  CO : TKMemoContainer;
  TB : TKMemoTextBlock;
  PA : TKMemoParagraph;
begin
  CO := ActiveBlocks.AddContainer;
  CO.Position := mbpText;
  CO.BlockStyle.ContentPadding.All := 10;
  CO.BlockStyle.BorderWidth := 2;
  CO.BlockStyle.Brush.Color := clYellow;
  CO.FixedWidth := False;
  PA := CO.Blocks.AddParagraph;
  PA.ParaStyle.HAlign := halCenter;
  TB := CO.Blocks.AddTextBlock('Text in a container!');
  TB.TextStyle.Font.Style := [fsBold];
  TB.TextStyle.Font.Color := clGreen;
   Result := CO;
  AddParagraphBlock;
  Modified;
end;

function TfrmKMemo.AddHyperLinkBlock: TKMemoHyperlink;
var
  HL : TKMemoHyperlink;
begin
  HL := ActiveBlocks.AddHyperlink('testlink', 'www.test.com');
  ActiveBlocks.AddParagraph;
  Result := HL;
end;

function TfrmKMemo.AddImageBlock: TKMemoImageBlock;
var
  LBitmap     : TBitmap;
  LPicture    : TPicture;
  LPngImage   : TPngImage;
  LImageBlock : TKMemoImageBlock;
  LSize       : Integer;
begin
  ActiveBlocks.AddParagraph;
  LPicture := TPicture.Create;
  try
    LBitmap := TBitmap.Create;
    try
      LSize := 100;
      LBitmap.SetSize(LSize, LSize);
      LBitmap.Transparent := True;
      LBitmap.Canvas.Brush.Color := clBlue;
      LBitmap.Canvas.Brush.Style := bsSolid;
      LBitmap.Canvas.FillRect(Rect(0, 0, LSize, LSize));
      LBitmap.Canvas.Pen.Color := clWhite;
      LBitmap.Canvas.Pen.Style := psSolid;
      LBitmap.Canvas.Pen.Width := 2;
      LBitmap.Canvas.MoveTo(0, 0);
      LBitmap.Canvas.LineTo(LSize, LSize);
      LBitmap.Canvas.MoveTo(LSize, 0);
      LBitmap.Canvas.LineTo(0, LSize);
      LPngImage := TPngImage.Create;
      try
        LPngImage.Assign(LBitmap);
        LPicture.Assign(LPngImage);
        LPicture.Assign(LPicture);
      finally
       LPngImage.Free;
      end;
      LImageBlock := ActiveBlocks.AddImageBlock(LPicture);
    finally
      LBitmap.Free;
    end;
  finally
    LPicture.Free;
  end;
  AddParagraphBlock;
  Result := LImageBlock;
  Modified;
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
  Logger.SendObject('AKMemoBlock', AKMemoBlock);
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
  PA : TKMemoParagraph;
begin
  PA := ActiveBlocks.AddParagraph;
  Result := PA;
  KMemoNotifier.SelectBlock(PA, sgpNone);
//  KMemo.SelectedBlock := PA;
  Modified;
end;

function TfrmKMemo.AddTable: TKMemoTable;
var
  LTable     : TKMemoTable;
  X          : Integer;
  Y          : Integer;
  LTextBlock : TKMemoTextBlock;
  LIndex     : TKMemoBlockIndex;
  LPara      : TKMemoParagraph;
begin
  LTable := ActiveBlocks.AddTable;
  LTable.ColCount := 4;
  LTable.RowCount := 4;
  for Y := 0 to LTable.RowCount - 1 do
  begin
    for X := 0 to LTable.ColCount - 1 do
    begin
//      LTextBlock :=
//        LTable.Cells[X, Y].Blocks.AddTextBlock(Format('Cell(%d, %d)', [X, Y]));
    end;
  end;
  LTable.CellStyle.BorderWidth := 2;
  LTable.ApplyDefaultCellStyle;
//  AddParagraphBlock;
  Result := LTable;
 // LIndex := ActiveBlocks.BlockToIndex(LTable);
//  LPara := ActiveBlocks.AddParagraph(-1);
//  KMemoNotifier.SelectBlock(Lpara, sgpNone);
  LPara := LTable.ParentBlocks.AddParagraph;
  KMemoNotifier.SelectBlock(Lpara, sgpNone);
//  AddBlockWithAutoParagraph(ActiveBlocks, LTable);
//  AddParagraphBlock;
  BuildTreeView;
end;

function TfrmKMemo.AddTextBlock: TKMemoTextBlock;
var
  LTextBlock : TKMemoTextBlock;
begin
  LTextBlock := ActiveBlocks.AddTextBlock('Red Bold Arial text.');
  LTextBlock.TextStyle.Font.Name := 'Arial';
  LTextBlock.TextStyle.Font.Color := clRed;
  LTextBlock.TextStyle.Font.Style := [fsBold];
  AddBlockWithAutoParagraph(ActiveBlocks, LTextBlock);
  Result := LTextBlock;

//  AddParagraphBlock;
  Modified;
end;

{ (Re)builds the treeview structure based on the contents of the given
  TKMemo control. }

procedure TfrmKMemo.BuildTreeView;
var
  I      : Integer;
  LBlock : TKMemoBlock;
  N      : TBlockNode;
begin
  Logger.Track(Self, 'BuildTreeView');
  KMemo.LockUpdate;
  try
    ClearTree;
    if KMemo.Blocks.Count > 0 then
    begin
      for I := 0 to KMemo.Blocks.Count - 1 do
      begin
        LBlock := KMemo.Blocks.Items[I];
        AddBlockToTree(LBlock);
        if LBlock is TKMemoContainer then
          AddNodes(LBlock as TKMemoContainer);
      end;
      LBlock := SelectedBlock;
      if not Assigned(LBlock) then
        LBlock := KMemo.ActiveBlock;
        //NearestParagraph;
      N := FRootNode.Find(LBlock);
      if Assigned(N) then
      begin
        N.Selected := True;
      end;
    end;
    //FRootNode.Expand;

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

procedure TfrmKMemo.CopyBlock;
begin

end;

procedure TfrmKMemo.CreateInspectors;
begin
  FSelectedBlockInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlInspector00,
    nil
  );
  FActiveBlockInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlInspector01,
    nil
  );
  FActiveInnerBlockInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlInspector02,
    nil
  );
  FBlocksInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlInspector10,
    nil
  );
  FActiveBlocksInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlInspector11,
    nil
  );
  FActiveInnerBlocksInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlInspector12,
    nil
  );
  FTextStyleInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlInspector20,
    nil
  );
  FParaStyleInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlInspector21,
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
  FTree.OnDragAllowed   := FTreeDragAllowed;
  FTree.OnDragDrop      := FTreeDragDrop;
  FTree.OnDragOver      := FTreeDragOver;
  FTree.Images          := imlMain;
  FTree.PopupMenu       := ppmTree;
  FRootData             := TKMemoBlock.Create;
  FRootNode             := TBlockNode.Create(FTree, FRootData, False);
  FRootNode.Text        := 'Document root';
end;

procedure TfrmKMemo.CutBlock;
var
  LNextNode : TBlockNode;
begin
  if Assigned(FocusedNode) and (FocusedNode <> FRootNode) then
  begin
    LNextNode := FocusedNode.NextNode;
    FTree.DeleteNode(FTree.FocusedNode);
    KMemo.ExecuteCommand(ecCut);
//    KMemo.DeleteSelectedBlock;
    if Assigned(LNextNode) then
    begin
      LNextNode.SetFocus;
      LNextNode.Select;
    end;
  end;
end;

procedure TfrmKMemo.DeleteBlock;
var
  LNextNode : TBlockNode;
begin
  if Assigned(FocusedNode) and (FocusedNode <> FRootNode) then
  begin
    LNextNode := FocusedNode.NextNode;
    FTree.DeleteNode(FTree.FocusedNode);
    KMemo.DeleteSelectedBlock;
    if Assigned(LNextNode) then
    begin
      LNextNode.SetFocus;
      LNextNode.Select;
    end;
  end;
end;
{$ENDREGION}

end.
