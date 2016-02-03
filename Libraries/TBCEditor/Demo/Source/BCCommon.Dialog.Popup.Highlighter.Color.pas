unit BCCommon.Dialog.Popup.Highlighter.Color;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.ExtCtrls, BCControl.ButtonedEdit, sSkinProvider,
  System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList, sPanel, BCControl.Panel;

type
  TSelectHighlighterColorEvent = procedure(AHighlighterColorName: string) of object;

  TPopupHighlighterColorDialog = class(TForm)
    VirtualDrawTree: TVirtualDrawTree;
    SkinProvider: TsSkinProvider;
    procedure VirtualDrawTreeDblClick(Sender: TObject);
    procedure VirtualDrawTreeDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
    procedure VirtualDrawTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VirtualDrawTreeGetNodeWidth(Sender: TBaseVirtualTree; HintCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; var NodeWidth: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSelectHighlighterColor: TSelectHighlighterColorEvent;
    procedure WMActivate(var AMessage: TWMActivate); message WM_ACTIVATE;
  public
    procedure Execute(AHighlighterColors: TStrings; ASelectedHighlighterName: string);
    property OnSelectHighlighterColor: TSelectHighlighterColorEvent read FSelectHighlighterColor write FSelectHighlighterColor;
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

procedure TPopupHighlighterColorDialog.FormCreate(Sender: TObject);
begin
  VirtualDrawTree.NodeDataSize := SizeOf(TSearchRec);
end;

procedure TPopupHighlighterColorDialog.FormShow(Sender: TObject);
begin
   VirtualDrawTree.SetFocus;
end;

procedure TPopupHighlighterColorDialog.Execute(AHighlighterColors: TStrings; ASelectedHighlighterName: string);
var
  i: Integer;
  LNode: PVirtualNode;
  LNodeData: PSearchRec;
  LHighlighterColorName: string;
  LWidth, LMaxWidth: Integer;

begin
  LMaxWidth := 0;

  for i := 0 to AHighlighterColors.Count - 1 do
  begin
    LNode := VirtualDrawTree.AddChild(nil);
    LNodeData := VirtualDrawTree.GetNodeData(LNode);
    LHighlighterColorName := AHighlighterColors[i];

    LWidth := VirtualDrawTree.Canvas.TextWidth(LHighlighterColorName);
    if LWidth > LMaxWidth then
      LMaxWidth := LWidth;

    LNodeData.Name := LHighlighterColorName;
    VirtualDrawTree.Selected[LNode] := ASelectedHighlighterName = LHighlighterColorName;
  end;

  VirtualDrawTree.Invalidate;

  Width := LMaxWidth + 80;
  Height := Min(Integer(VirtualDrawTree.DefaultNodeHeight) * AHighlighterColors.Count + VirtualDrawTree.BorderWidth * 2 + 2, TForm(Self.PopupParent).Height);

  SetWindowPos(Handle, HWND_TOPMOST, Left, Top, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);

  Visible := True;
end;

procedure TPopupHighlighterColorDialog.VirtualDrawTreeDblClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PSearchRec;
begin
  Node := VirtualDrawTree.GetFirstSelected;
  Data := VirtualDrawTree.GetNodeData(Node);
  if Assigned(Data) then
    if Assigned(FSelectHighlighterColor) then
      FSelectHighlighterColor(Data.Name);
end;

procedure TPopupHighlighterColorDialog.VirtualDrawTreeDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
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

procedure TPopupHighlighterColorDialog.WMActivate(var AMessage: TWMActivate);
begin
  if AMessage.Active <> WA_INACTIVE then
    SendMessage(Self.PopupParent.Handle, WM_NCACTIVATE, WPARAM(True), -1);

  inherited;

  if AMessage.Active = WA_INACTIVE then
    Release;
end;

procedure TPopupHighlighterColorDialog.VirtualDrawTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSearchRec;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
  inherited;
end;

procedure TPopupHighlighterColorDialog.VirtualDrawTreeGetNodeWidth(Sender: TBaseVirtualTree; HintCanvas: TCanvas;
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
