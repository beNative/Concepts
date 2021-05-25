{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ActnList,

  VirtualTrees,

  DDuce.Components.VirtualTrees.Node,

  kcontrols, kmemo, kgraphics;

type
  TfrmKMemo = class(TForm)
    aclMain               : TActionList;
    actAddTextBlock       : TAction;
    actClear              : TAction;
    actAddParagraph       : TAction;
    actAddImageBlock      : TAction;
    actAddTable           : TAction;
    actAddHyperLink       : TAction;
    actAddContainer       : TAction;
    actDeleteObject       : TAction;
    actRebuildTree        : TAction;
    actInspectKMemo       : TAction;
    btnAddTextBlock       : TButton;
    btnClear              : TButton;
    btnAddContainer       : TButton;
    btnAddHyperlink       : TButton;
    btnAddImageBlock      : TButton;
    btnAddParagraph       : TButton;
    btnAddTable           : TButton;
    btnDeleteObject       : TButton;
    pnlTreeView           : TPanel;
    btnactCnPrefixWizard  : TButton;
    btnactCnPrefixWizard1 : TButton;
    pnlRichEditor         : TPanel;
    KMemo                 : TKMemo;

    procedure actAddTextBlockExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actAddParagraphExecute(Sender: TObject);
    procedure actAddImageBlockExecute(Sender: TObject);
    procedure actAddTableExecute(Sender: TObject);
    procedure actAddHyperLinkExecute(Sender: TObject);
    procedure actAddContainerExecute(Sender: TObject);
    procedure actDeleteObjectExecute(Sender: TObject);
    procedure actRebuildTreeExecute(Sender: TObject);
    procedure actInspectKMemoExecute(Sender: TObject);
    procedure FTreeFreeNode(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
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

  private
    FTree              : TVirtualStringTree;
    FVTRoot            : TVTNode<TKMemoBlock>;
    FRootData          : TKMemoBlock;

  protected
    procedure AddBlock(
      ABlock  : TKMemoBlock;
      AParent : TKMemoBlock = nil
    );

    procedure BuildTreeView(AKMemo: TKMemo);
    procedure Clear;

  public
    procedure CreateTreeView;
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Logger;

{$REGION 'action handlers'}
procedure TfrmKMemo.actAddContainerExecute(Sender: TObject);
var
  CO: TKMemoContainer;
begin
  CO := KMemo.Blocks.AddContainer;
  CO.Position := mbpAbsolute;
  AddBlock(CO);

  //CO.LeftOffset := 50;
  //CO.TopOffset := 20;
  CO.FixedWidth := True;
  //CO.FixedHeight := True;
  //CO.RequiredHeight := 400;
  //CO.RequiredWidth := 300;
  //CO.BlockStyle.Brush.Color := clLime;
  AddBlock(CO.Blocks.AddTextBlock('Text in a container!'), CO);
end;

procedure TfrmKMemo.actAddHyperLinkExecute(Sender: TObject);
var
  HL : TKMemoHyperlink;
begin
  HL := KMemo.Blocks.AddHyperlink('testlink', 'www.test.com');
  AddBlock(HL);
//
end;

procedure TfrmKMemo.actAddImageBlockExecute(Sender: TObject);
begin
//
end;

procedure TfrmKMemo.actAddParagraphExecute(Sender: TObject);
var
  TB : TKMemoTextBlock;
  PA : TKMemoParagraph;
begin
  PA := KMemo.ActiveBlocks.AddParagraph;
  PA.ParaStyle.HAlign := halLeft;
  PA.ParaStyle.BottomPadding := 20;
  AddBlock(PA, KMemo.ActiveBlock);
end;

procedure TfrmKMemo.actAddTableExecute(Sender: TObject);
var
  TBL : TKMemoTable;
begin
  TBL := KMemo.Blocks.AddTable;
  TBL.ColCount := 2;
  TBL.RowCount := 2;
  TBL.Cells[0, 0].Blocks.AddTextBlock('Table text 1');
  TBL.Cells[0, 1].Blocks.AddTextBlock('Table text 2');
  TBL.Cells[1, 0].Blocks.AddTextBlock('Table text 3');
  TBL.Cells[1, 1].Blocks.AddTextBlock('Table text 4');
  TBL.CellStyle.BorderWidth := 1;
  TBL.ApplyDefaultCellStyle;
  AddBlock(TBL);
end;

procedure TfrmKMemo.actAddTextBlockExecute(Sender: TObject);
var
  TB : TKMemoTextBlock;
begin
  if Assigned(KMemo.ActiveBlocks) then
  begin
    TB := KMemo.ActiveBlocks.AddTextBlock('Hello nested world!' + #13#10);
  end
  else
  begin
    TB := KMemo.Blocks.AddTextBlock('Hello world!' + #13#10);
  end;

  TB.TextStyle.Font.Name := 'Arial';
  TB.TextStyle.Font.Color := clRed;
  TB.TextStyle.Font.Style := [fsBold];
  AddBlock(TB);
end;

procedure TfrmKMemo.actClearExecute(Sender: TObject);
begin
  Clear;
end;

procedure TfrmKMemo.actDeleteObjectExecute(Sender: TObject);
begin
  FTree.DeleteNode(FTree.FocusedNode);
  KMemo.DeleteSelectedBlock;
end;

procedure TfrmKMemo.actInspectKMemoExecute(Sender: TObject);
begin
//
end;

procedure TfrmKMemo.actRebuildTreeExecute(Sender: TObject);
begin
  BuildTreeView(KMemo);
end;

procedure TfrmKMemo.AddBlock(ABlock, AParent: TKMemoBlock);
var
  LVTNode: TVTNode<TKMemoBlock>;
begin
  Logger.Enter(Self, 'AddBlock');
  LVTNode := nil;
  if Assigned(AParent) then
  begin
    //LVTNode := FVTRoot.Find(AParent);
  end;
  if not Assigned(LVTNode) then
  begin
    LVTNode := FVTRoot;
  end;
  LVTNode.Add(ABlock, False);
  FTree.FullExpand;
  //if Assigned(FTree.FocusedNode) then
  //begin
  //  LVTNode  := TVTNode<TKMemoBlock>(FTree.GetNodeData(FTree.FocusedNode)^);
  //  if Assigned(LVTNode) then
  //    LVTNode.Add(ABlock, False);
  //end
  //else
  Logger.Leave(Self, 'AddBlock');
end;

procedure TfrmKMemo.BuildTreeView(AKMemo: TKMemo);
var
  I     : Integer;
  LData : TKMemoBlock;
begin
  Logger.Enter(Self, 'BuildTreeView');
  Clear;

  if AKMemo.Blocks.Count > 0 then
  begin
    for I := 0 to AKMemo.Blocks.Count - 1 do
    begin
      LData := AKMemo.Blocks.Items[I];
      AddBlock(LData);
    end;
  end;
  FTree.FullExpand;
  Logger.Leave(Self, 'BuildTreeView');

end;

procedure TfrmKMemo.Clear;
begin
  Logger.Enter(Self, 'Clear');
  FTree.Clear;
  FVTRoot := TVTNode<TKMemoBlock>.Create(FTree, FRootData, False);
  Logger.Leave(Self, 'Clear');
end;

constructor TfrmKMemo.Create(AOwner: TComponent);
begin
  inherited;
  CreateTreeView;
end;

procedure TfrmKMemo.CreateTreeView;
begin
  Logger.Enter(Self, 'CreateTreeView');
  FTree := TVirtualStringTree.Create(Self);
  FTree.Parent         := pnlTreeView;
  FTree.Align          := alClient;
  FTree.ShowHint       := True;
  FTree.OnFreeNode     := FTreeFreeNode;
  FTree.OnGetText      := FTreeGetText;
  FTree.OnFocusChanged := FTreeFocusChanged;
  FRootData            := TKMemoBlock.Create;
  FVTRoot              := TVTNode<TKMemoBlock>.Create(FTree, FRootData, False);
  FVTRoot.Text := 'Root';
  Logger.Leave(Self, 'CreateTreeView');
end;

procedure TfrmKMemo.FTreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LVTNode : TVTNode<TKMemoBlock>;
begin
  Logger.Enter(Self, 'FTreeFocusChanged');
  if Assigned(Node) then
  begin
    LVTNode := TVTNode<TKMemoBlock>(Sender.GetNodeData(Node)^);
    if Assigned(LVTNode) and Assigned(LVTNode.Data) then
    begin
      IKMemoNotifier(KMemo).SelectBlock(LVTNode.Data, sgpNone);
    end;
  end;
  Logger.Leave(Self, 'FTreeFocusChanged');
end;

procedure TfrmKMemo.FTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  LVTNode : TVTNode<TKMemoBlock>;
begin
  Logger.Enter(Self, 'FTreeFreeNode');
  LVTNode := TVTNode<TKMemoBlock>(Sender.GetNodeData(Node)^);
  LVTNode.Free;
  Logger.Leave(Self, 'FTreeFreeNode');
end;

procedure TfrmKMemo.FTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  LVTNode : TVTNode<TKMemoBlock>;
begin
  LVTNode  := TVTNode<TKMemoBlock>(Sender.GetNodeData(Node)^);
  if LVTNode.Text.IsEmpty then
  begin
    //CellText := LVTNode.Data.Text;
    CellText := LVTNode.Data.ClassName ;
  end
  else
  begin
    CellText := LVTNode.Text;
  end;
end;

{$ENDREGION}

end.
