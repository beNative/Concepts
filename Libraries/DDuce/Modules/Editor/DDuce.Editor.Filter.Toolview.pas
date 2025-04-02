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

unit DDuce.Editor.Filter.ToolView;

interface

{
  This is an attempt to make a reusable filter view using a virtual treeview and
  a presenter.

  sequence that we need to follow:

  - create VST (Owner, Parent)
  - assign VST events
  - create TVP (Owner)
  - create TVP datatemplate (takes columndefinitions as argument)
  - assign TVP.ItemTemplate
  - add columndefinitions for TVP
  - assign TVP events (filter event)
  - assign TVP.ItemsSource property
  - assign TVP.Treeview property
}

uses
  System.Classes, System.SysUtils, System.Actions,
  Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ActnList,

  VirtualTrees,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,
  DSharp.Core.DataTemplates, DSharp.Windows.ControlTemplates,

  DDuce.Editor.ToolView.Base, DDuce.Editor.Filter.Data;

type
  TfrmFilter = class(TCustomEditorToolView)
    aclMain            : TActionList;
    actFocusFilterText : TAction;
    edtFilter          : TEdit;
    pnlView            : TPanel;
    pnlHeader          : TPanel;
    pnlMain            : TPanel;
    sbrMain            : TStatusBar;


    {$REGION 'action handlers'}
    procedure actFocusFilterTextExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    function CCustomDraw(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ) : Boolean;

    procedure edtFilterChange(Sender: TObject);
    procedure edtFilterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtFilterKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FTVPFilter(Item: TObject; var Accepted: Boolean);
    procedure FVSTKeyPress(Sender: TObject; var Key: char);
    procedure FVSTKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FTVPSelectionChanged(Sender: TObject);
    {$ENDREGION}

  private
    FVST : TVirtualStringTree;
    FTVP : TTreeViewPresenter;

    FVKPressed        : Boolean;
    FUpdate           : Boolean;
    FUpdateEditorView : Boolean;
    FLines            : IList<TLine>;
//    FTextStyle        : TTextFormat;

    {$REGION 'property access methods'}
    function GetFilter: string;
    procedure SetFilter(AValue: string);
    {$ENDREGION}

    function IsMatch(const AString : string): Boolean; overload; inline;
    function IsMatch(
      const AString : string; // Search string
        var AMatch  : string; // Matching string
        var APos    : Integer // Character position where the match starts
    ): Boolean; overload; inline;

    procedure CalcMatchRect(
      const ASource           : string;   // String to search for a match
      const AMatch            : string;   // The found match
      const AOffset           : Integer;  // Character offset in Source to Match
            ACanvas           : TCanvas;
            AColumnDefinition : TColumnDefinition;
        var ARect             : TRect
    ); inline;
    procedure DrawMatchRect(
            ACanvas : TCanvas;
      const ARect   : TRect
    ); inline;

    procedure InitializeComponents;

  protected
    procedure SetVisible(Value: Boolean); override;

    procedure ApplyFilter;
    procedure Modified; override;
    procedure UpdateView; override;
    procedure FillList(AStrings: TStrings);
    procedure UpdateActions; override;
    procedure UpdateStatusDisplay;

  public
    procedure AfterConstruction; override;

    property Filter: string
      read GetFilter write SetFilter;

  end;

implementation

{$R *.dfm}

uses
  Winapi.Windows, Winapi.Messages,
  System.Variants, System.UITypes,

  DSharp.Windows.ColumnDefinitions.ControlTemplate,

  DDuce.Logger, DDuce.Factories.TreeViewPresenter, DDuce.Factories.VirtualTrees,
  DDuce.Editor.Utils;

resourcestring
  SOneLineWithMatchFound = '1 line with match found.';
  SLinesWithMatchFound   = '%d lines with match found.';

type
  TVKSet = set of Byte;

var
  VK_EDIT_KEYS : TVKSet = [
    VK_DELETE,
    VK_BACK,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END,
    VK_SPACE,
    Ord('0')..Ord('9'),
    Ord('A')..Ord('Z'),
    VK_OEM_1..VK_OEM_102,
    VK_NUMPAD0..VK_DIVIDE
  ];

  VK_CTRL_EDIT_KEYS : TVKSet = [
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END,
    Ord('C'),
    Ord('X'),
    Ord('V'),
    Ord('Z')
  ];

  VK_SHIFT_EDIT_KEYS : TVKSet = [
    VK_CONTROL,
    VK_SHIFT,
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END
  ];

{$REGION 'construction and destruction'}
procedure TfrmFilter.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := TVirtualStringTreeFactory.CreateTree(Self, pnlView);
  FVST.OnKeyPress := FVSTKeyPress;
  FVST.OnKeyUp := FVSTKeyUp;
  FTVP := TFactories.CreateTreeViewPresenter(Self, FVST);
  FLines := TCollections.CreateObjectList<TLine>;
  FTVP.View.ItemsSource := FLines as IObjectList;
  with FTVP.ColumnDefinitions.Add('Index', 50) do
  begin
    ValuePropertyName := 'Index';
  end;
  with FTVP.ColumnDefinitions.Add('Text', 600) do
  begin
    ValuePropertyName := 'Text';
    Font.Assign(View.Editor.Fonts.Text);
    Font.Size := 8;
  end;
  InitializeComponents;

//  FTextStyle.SingleLine := True;
//  FTextStyle.Opaque     := False;
//  FTextStyle.ExpandTabs := False;
//  FTextStyle.Wordbreak  := False;
//  FTextStyle.ShowPrefix := True;
//  FTextStyle.Clipping   := False;
//  FTextStyle.SystemFont := False;
//  FTextStyle.Alignment  := taLeftJustify;
//  FTextStyle.Layout     := tlCenter;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmFilter.edtFilterChange(Sender: TObject);
begin
  FTVP.ApplyFilter;
end;

function TfrmFilter.CCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
var
  S      : string;
  R      : TRect;
  Offset : Integer;
  Match  : string;
begin
  if DrawMode = dmBeforeCellPaint then
  begin
    S := FTVP.View.ItemTemplate.GetText(Item, ColumnDefinition.Index);
    if IsMatch(S, Match, Offset) then
    begin
      R := CellRect;
      CalcMatchRect(S, Match, Offset, TargetCanvas, ColumnDefinition, R);
      DrawMatchRect(TargetCanvas, R);
    end;
  end;
  Result := True;
end;

procedure TfrmFilter.actFocusFilterTextExecute(Sender: TObject);
begin
  edtFilter.SetFocus;
  edtFilter.SelectAll;
end;

procedure TfrmFilter.edtFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  A : Boolean;
  B : Boolean;
  C : Boolean;
  D : Boolean;
  E : Boolean;
  F : Boolean;
  G : Boolean;
begin
  // SHIFTED and ALTED keycombinations
  A := (ssAlt in Shift) or (ssShift in Shift);
  { Single keys that need to be handled by the edit control like all displayable
    characters but also HOME and END }
  B := (Key in VK_EDIT_KEYS) and (Shift = []);
  { CTRL-keycombinations that need to be handled by the edit control like
    CTRL-C for clipboard copy. }
  C := (Key in VK_CTRL_EDIT_KEYS) and (Shift = [ssCtrl]);
  { SHIFT-keycombinations that need to be handled by the edit control for
    uppercase characters but also eg. SHIFT-HOME for selections. }
  D := (Key in VK_SHIFT_EDIT_KEYS) and (Shift = [ssShift]);
  { Only CTRL key is pressed. }
  E := (Key = VK_CONTROL) and (Shift = [ssCtrl]);
  { Only SHIFT key is pressed. }
  F := (Key = VK_SHIFT) and (Shift = [ssShift]);
  { Only (left) ALT key is pressed. }
  G := (Key = VK_MENU) and (Shift = [ssAlt]);
  if not (A or B or C or D or E or F or G) then
  begin
    FVKPressed := True;
    Key := 0;
  end
  { Prevents jumping to the application's main menu which happens by default
    if ALT is pressed. }
  else if G then
  begin
    Key := 0;
  end;
end;

procedure TfrmFilter.edtFilterKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FVKPressed and FVST.Enabled then
  begin
    FVST.Perform(WM_KEYDOWN, Key, 0);
    if Visible and FVST.CanFocus then
      FVST.SetFocus;
  end;
  FVKPressed := False;
end;

procedure TfrmFilter.FillList(AStrings: TStrings);
var
  I : Integer;
begin
  FTVP.BeginUpdate;
  FLines.Clear;
  for I := 0 to AStrings.Count - 1 do
  begin
    FLines.Add(TLine.Create(I, AStrings[I]));
  end;
  FTVP.EndUpdate;
end;

procedure TfrmFilter.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Filter := '';
end;

procedure TfrmFilter.FormShow(Sender: TObject);
begin
  UpdateView;
  FTVP.ApplyFilter;
end;

procedure TfrmFilter.FTVPFilter(Item: TObject; var Accepted: Boolean);
var
  B : Boolean;
  S : string;
  I : Integer;
  C : TColumnDefinition;
begin
  B := False;
  for I := 0 to FTVP.ColumnDefinitions.Count - 1 do
  begin
    C := FTVP.ColumnDefinitions[I];
    S := FTVP.View.ItemTemplate.GetText(Item, C.Index);
    B := B or IsMatch(S);
  end;
  Accepted := B;
end;

procedure TfrmFilter.FTVPSelectionChanged(Sender: TObject);
begin
  FUpdateEditorView := True;
end;

procedure TfrmFilter.FVSTKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_RETURN then
  begin
    Close;
  end
  else if Ord(Key) = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Close;
  end
  else if not edtFilter.Focused then
  begin
    edtFilter.SetFocus;
    PostMessage(edtFilter.Handle, WM_CHAR, Ord(Key), 0);
    edtFilter.SelStart := Length(Filter);
    // required to prevent the invocation of accSISelerator keys!
    Key := #0;
  end;
end;

procedure TfrmFilter.FVSTKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_MENU) and (Shift = []) then // only ALT pressed
  begin
    Key := 0;
  end;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmFilter.GetFilter: string;
begin
  Result := edtFilter.Text;
end;

procedure TfrmFilter.SetFilter(AValue: string);
begin
  if AValue <> Filter then
  begin
    edtFilter.Text := AValue;
  end;
end;

procedure TfrmFilter.SetVisible(Value: Boolean);
begin
  if Value then
  begin
    // check properties
    if FTVP.View.ItemsSource = nil then
      Exception.Create('ItemSource property not assigned');
    InitializeComponents;
  end;
  inherited SetVisible(Value);
end;
{$ENDREGION}

{$REGION 'private methods'}
function TfrmFilter.IsMatch(const AString: string): Boolean;
begin
  if Filter = '' then
    Result := True
  else
    Result := StrPos(Filter, AString, False) > 0;
end;

function TfrmFilter.IsMatch(const AString: string; var AMatch: string;
  var APos: Integer): Boolean;
begin
  APos   := -1;
  AMatch := '';
  Result := False;
  if Filter <> '' then
  begin
    APos   := StrPos(Filter, AString, False);
    AMatch := System.Copy(AString, APos, Length(Filter));
    Result := APos > 0;
  end;
end;

procedure TfrmFilter.Modified;
begin
  FUpdate := True;
end;

procedure TfrmFilter.InitializeComponents;
var
  C : TColumnDefinition;
  I : Integer;
begin
  // connect custom draw events
  for I := 0 to FTVP.ColumnDefinitions.Count - 1 do
  begin
    C := FTVP.ColumnDefinitions[I];
    C.OnCustomDraw := CCustomDraw;
  end;

  FTVP.View.Filter.Add(FTVPFilter);
  FTVP.OnSelectionChanged := FTVPSelectionChanged;
  FTVP.TreeView := FVST;
end;

procedure TfrmFilter.CalcMatchRect(const ASource: string; const AMatch: string;
  const AOffset: Integer; ACanvas: TCanvas;
  AColumnDefinition: TColumnDefinition; var ARect: TRect);
var
  Margin: Integer;
begin
  // calculate the rectangle to draw around the matching text
  Margin := 8;
  ARect.Left := ARect.Left + Margin +
    ACanvas.TextWidth(System.Copy(ASource, 1, AOffset - 1));
  ARect.Right := ARect.Left + ACanvas.TextWidth(AMatch);
end;

procedure TfrmFilter.DrawMatchRect(ACanvas: TCanvas; const ARect: TRect);
var
  C : TColor;
  B : TBrush;
  P : TPen;
begin
  B := TBrush.Create;
  try
    B.Assign(ACanvas.Brush); // copy original brush
    P := TPen.Create;
    try
      P.Assign(ACanvas.Pen); // copy original pen
      C := ColorToRGB(ACanvas.Brush.Color);
      if C <> clWhite then
      begin
//        C := MixColors(
//          C,
//          Manager.Settings.Colors.HighlightAllColor.Background,
//          Manager.Settings.Colors.HighlightAllColor.BackAlpha
//        )
      end
      else
      begin
//        C := ColorAdjustLuma(
//          Manager.Settings.Colors.HighlightAllColor.Background,
//          70,
//          False
//        );
      end;
//      ACanvas.Pen.Color := Manager.Settings.Colors.HighlightAllColor.FrameColor;
      ACanvas.Pen.Width := 1;
      ACanvas.Brush.Color := C;
      ACanvas.Rectangle(ARect);
      ACanvas.Pen.Assign(P); // restore original pen
      ACanvas.Brush.Assign(B); // restore original brush
    finally
      P.Free;
    end;
  finally
    B.Free;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmFilter.ApplyFilter;
var
  B: Boolean;
begin
  B := True;
//  if RegEx and (Filter <> '') then
//  begin
//    try
//      FRegExpr.Expression := Filter;
//      FRegExpr.ModifierI := not MatchCase;
//      FRegExpr.ModifierM := True;
//      FRegExpr.Compile;
//      FIsCompiled := True;
//    except
//      B := False;
//      sbrMain.SimpleText := FRegExpr.ErrorMsg(FRegExpr.LastError);
//    end;
//  end;
  if B then
  begin
    sbrMain.SimpleText := '';
    FTVP.ApplyFilter;
    UpdateStatusDisplay;
  end;
end;

procedure TfrmFilter.UpdateActions;
var
  L: TLine;
begin
  inherited UpdateActions;
  if FUpdate then
  begin
    UpdateView;
    ApplyFilter;
    FUpdate := False;
  end;
  if FUpdateEditorView then // update position in the editorview
  begin
    L := TLine(FTVP.SelectedItem);
    if Assigned(L) then
      //View.SearchAndSelectLine(L.Index, L.Text);
      View.Editor.GoToLineAndSetPosition(L.Index);
    FUpdateEditorView := False;
  end;
end;

procedure TfrmFilter.UpdateStatusDisplay;
var
  N : Cardinal;
begin
  N := FTVP.TreeView.VisibleCount;
  if N = 1 then
    sbrMain.SimpleText := SOneLineWithMatchFound
  else
    sbrMain.SimpleText := Format(SLinesWithMatchFound, [N]);
end;

procedure TfrmFilter.UpdateView;
begin
  FillList(View.Lines);
  FTVP.Refresh;
  UpdateStatusDisplay;
end;
{$ENDREGION}
end.
