unit BCCommon.Dialog.Popup.Highlighter;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.ExtCtrls, BCControl.ButtonedEdit, sSkinProvider,
  System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList, sPanel, BCControl.Panel;

type
  TSelectHighlighterEvent = procedure(AHighlighterName: string) of object;

  TPopupHighlighterDialog = class(TForm)
    VirtualDrawTree: TVirtualDrawTree;
    SkinProvider: TsSkinProvider;
    ButtonedEdit: TBCButtonedEdit;
    ActionList: TActionList;
    ActionClear: TAction;
    ActionSearch: TAction;
    ImageList: TImageList;
    procedure FormShow(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure VirtualDrawTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure VirtualDrawTreeDblClick(Sender: TObject);
    procedure VirtualDrawTreeDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
    procedure VirtualDrawTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VirtualDrawTreeGetNodeWidth(Sender: TBaseVirtualTree; HintCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; var NodeWidth: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSelectHighlighter: TSelectHighlighterEvent;
    procedure SetVisibleRows;
    procedure WMActivate(var AMessage: TWMActivate); message WM_ACTIVATE;
  public
    procedure Execute(AHighlighters: TStrings; ASelectedHighlighter: string);
    property OnSelectHighlighter: TSelectHighlighterEvent read FSelectHighlighter write FSelectHighlighter;
  end;

implementation

{$R *.dfm}

uses
  System.Types, BCControl.Utils, sGraphUtils, sVclUtils, sDefaults, System.Math;

type
  PSearchRec = ^TSearchRec;
  TSearchRec = packed record
    Name: string;
  end;

procedure TPopupHighlighterDialog.FormCreate(Sender: TObject);
var
  SysImageList: THandle;
begin
  VirtualDrawTree.NodeDataSize := SizeOf(TSearchRec);
  VirtualDrawTree.Images := TImageList.Create(Self);
  SysImageList := GetSysImageList;
  if SysImageList <> 0 then
  begin
    VirtualDrawTree.Images.Handle := SysImageList;
    VirtualDrawTree.Images.BkColor := clNone;
    VirtualDrawTree.Images.ShareImages := True;
  end;
end;

procedure TPopupHighlighterDialog.Execute(AHighlighters: TStrings; ASelectedHighlighter: string);
var
  i: Integer;
  LNode, LSelectedNode: PVirtualNode;
  LNodeData: PSearchRec;
  LHighlighterName: string;
  LWidth, LMaxWidth: Integer;
begin
  LMaxWidth := 0;
  LSelectedNode := nil;
  for i := 0 to AHighlighters.Count - 1 do
  begin
    LNode := VirtualDrawTree.AddChild(nil);
    LNodeData := VirtualDrawTree.GetNodeData(LNode);
    LHighlighterName := AHighlighters[i];

    LWidth := VirtualDrawTree.Canvas.TextWidth(LHighlighterName);
    if LWidth > LMaxWidth then
      LMaxWidth := LWidth;

    LNodeData.Name := LHighlighterName;

    if ASelectedHighlighter = LHighlighterName then
    begin
      VirtualDrawTree.Selected[LNode] := True;
      LSelectedNode := LNode;
    end;
  end;
  VirtualDrawTree.Sort(nil, 0, sdAscending, False);
  VirtualDrawTree.Invalidate;

  if Assigned(LSelectedNode) then
    VirtualDrawTree.ScrollIntoView(LSelectedNode, True);

  Width := LMaxWidth + 80;
  Height := Min(Integer(VirtualDrawTree.DefaultNodeHeight) * AHighlighters.Count + ButtonedEdit.Height +
    VirtualDrawTree.BorderWidth * 2 + 2, TForm(Self.PopupParent).Height - GetSystemMetrics(SM_CYCAPTION) - 10);

  SetWindowPos(Handle, HWND_TOPMOST, Left, Top, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);

  Visible := True;
end;

procedure TPopupHighlighterDialog.FormDestroy(Sender: TObject);
begin
  VirtualDrawTree.Images.Free;
end;

procedure TPopupHighlighterDialog.FormShow(Sender: TObject);
begin
  ButtonedEdit.SetFocus;
end;

procedure TPopupHighlighterDialog.VirtualDrawTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PSearchRec;
begin
  if Result = 0 then
  begin
    Data1 := VirtualDrawTree.GetNodeData(Node1);
    Data2 := VirtualDrawTree.GetNodeData(Node2);

    Result := -1;

    if not Assigned(Data1) or not Assigned(Data2) then
      Exit;

    Result := AnsiCompareText(Data1.Name, Data2.Name);
  end;
end;

procedure TPopupHighlighterDialog.VirtualDrawTreeDblClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PSearchRec;
begin
  Node := VirtualDrawTree.GetFirstSelected;
  Data := VirtualDrawTree.GetNodeData(Node);
  if Assigned(Data) then
    if Assigned(FSelectHighlighter) then
      FSelectHighlighter(Data.Name);
end;

procedure TPopupHighlighterDialog.VirtualDrawTreeDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
var
  Data: PSearchRec;
  S: string;
  R: TRect;
  Format: Cardinal;
begin
  with Sender as TVirtualDrawTree, PaintInfo do
  begin
    Data := Sender.GetNodeData(Node);

    if not Assigned(Data) then
      Exit;

    Canvas.Font.Color := SkinProvider.SkinData.SkinManager.GetActiveEditFontColor;

    if vsSelected in PaintInfo.Node.States then
    begin
      Canvas.Brush.Color := SkinProvider.SkinData.SkinManager.GetHighLightColor;
      Canvas.Font.Color := SkinProvider.SkinData.SkinManager.GetHighLightFontColor
    end;

    SetBKMode(Canvas.Handle, TRANSPARENT);

    R := ContentRect;
    InflateRect(R, -TextMargin, 0);
    Dec(R.Right);
    Dec(R.Bottom);

    S := Data.Name;

    if Length(S) > 0 then
    begin
      Format := DT_TOP or DT_LEFT or DT_VCENTER or DT_SINGLELINE;

      DrawText(Canvas.Handle, S, Length(S), R, Format);
    end;
  end;
end;

procedure TPopupHighlighterDialog.WMActivate(var AMessage: TWMActivate);
begin
  if AMessage.Active <> WA_INACTIVE then
    SendMessage(Self.PopupParent.Handle, WM_NCACTIVATE, WPARAM(True), -1);

  inherited;

  if AMessage.Active = WA_INACTIVE then
    Release;
end;

procedure TPopupHighlighterDialog.ActionClearExecute(Sender: TObject);
begin
  ButtonedEdit.Text := '';
end;

procedure TPopupHighlighterDialog.ActionSearchExecute(Sender: TObject);
begin
  ButtonedEdit.RightButton.Visible := Trim(ButtonedEdit.Text) <> '';
  SetVisibleRows;
end;

procedure TPopupHighlighterDialog.SetVisibleRows;
var
  CurNode: PVirtualNode;
  Data: PSearchRec;
begin
  with VirtualDrawTree do
  begin
    CurNode := GetFirst;
    while Assigned(CurNode) do
    begin
      Data := GetNodeData(CurNode);
      IsVisible[CurNode] := (Pos(UpperCase(ButtonedEdit.Text), UpperCase(Data.Name)) <> 0) or (ButtonedEdit.Text = '');
      CurNode := CurNode.NextSibling;
    end;
  end;
end;

procedure TPopupHighlighterDialog.VirtualDrawTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSearchRec;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
  inherited;
end;

procedure TPopupHighlighterDialog.VirtualDrawTreeGetNodeWidth(Sender: TBaseVirtualTree; HintCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; var NodeWidth: Integer);
var
  Data: PSearchRec;
  AMargin: Integer;
begin
  with Sender as TVirtualDrawTree do
  begin
    AMargin := TextMargin;
    Data := GetNodeData(Node);
    if Assigned(Data) then
      NodeWidth := Canvas.TextWidth(Data.Name) + 2 * AMargin;
  end;
end;

end.
