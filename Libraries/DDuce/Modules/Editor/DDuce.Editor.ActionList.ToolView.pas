{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.ActionList.ToolView;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls, Vcl.ActnList, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ImgList, Vcl.Graphics,

  Spring.Collections,

  BCEditor.Editor.KeyCommands,

  VirtualTrees,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,
  DSharp.Core.DataTemplates,

  DDuce.Editor.Interfaces;

type

  TfrmActionListView = class(TForm, IEditorToolView)
    edtFilterActions : TEdit;
    pnlActions       : TPanel;
    pgcMain          : TPageControl;
    pnlEditorList    : TPanel;
    tsCommands       : TTabSheet;
    tsActions        : TTabSheet;

    procedure edtFilterActionsChange(Sender: TObject);
    procedure edtFilterActionsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtFilterActionsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    function FTVPActionsCustomDraw(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ): Boolean;
    procedure FTVPActionsFilter(Item: TObject; var Accepted: Boolean);
    procedure FVSTActionsKeyPress(Sender: TObject; var Key: Char);

  private
    FVSTActions      : TVirtualStringTree;
    FVSTCommands     : TVirtualStringTree;
    FTVPActions      : TTreeViewPresenter;
    FTVPCommands     : TTreeViewPresenter;
    FActionItems     : IObjectList;
    FCommandItems    : IObjectList;
    FVKPressed       : Boolean;
    //FTextStyle       : TTextStyle;

    function GetFilter: string;
    procedure SetFilter(AValue: string);
    function GetForm: TForm;
    function GetManager: IEditorManager;
    function GetName: string;

  protected
    procedure CreateActionsView;
    procedure CreateCommandsView;
    procedure UpdateLists;
    property Manager: IEditorManager
      read GetManager;

    function IsMatch(const AString : string): Boolean; overload; inline;
    function IsMatch(
      const AString : string;
        var AMatch  : string;
        var APos    : Integer
    ): Boolean; overload; inline;

    { IEditorToolView }
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    procedure UpdateView;

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;

    property Filter: string
      read GetFilter write SetFilter;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  Winapi.Windows, Winapi.Messages,
  System.TypInfo, System.StrUtils, System.Variants, System.Actions, System.Rtti,
  System.UITypes,
  Vcl.GraphUtil, Vcl.Menus,

  DSharp.Windows.ControlTemplates,

  DDuce.Editor.Utils, DDuce.Editor.ActionList.Templates,
  DDuce.Factories;

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
    VK_SHIFT,
    VK_CONTROL,
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
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END
  ];

{$REGION 'TfrmActionListView'}
{$REGION 'construction and destruction'}
procedure TfrmActionListView.AfterConstruction;
begin
  inherited AfterConstruction;
  FActionItems    := TCollections.CreateList<TContainedAction>(False) as IObjectList;
  FCommandItems := TCollections.CreateList<TBCEditorKeyCommand>(False) as IObjectList;
  CreateActionsView;
  CreateCommandsView;

//  FTextStyle.SingleLine := True;
//  FTextStyle.Opaque := False;
//  FTextStyle.ExpandTabs := False;
//  FTextStyle.Wordbreak := False;
//  FTextStyle.ShowPrefix := True;
//  FTextStyle.Clipping := False;
//  FTextStyle.SystemFont := False;
//  FTextStyle.Alignment := taLeftJustify;
//  FTextStyle.Layout := tlCenter;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmActionListView.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TfrmActionListView.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmActionListView.GetFilter: string;
begin
  Result := edtFilterActions.Text;
end;

function TfrmActionListView.GetName: string;
begin
  Result := inherited Name;
end;

procedure TfrmActionListView.SetFilter(AValue: string);
begin
  if AValue <> Filter then
  begin
    edtFilterActions.Text := AValue;
  end;
end;

function TfrmActionListView.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TfrmActionListView.SetVisible(AValue: Boolean);
begin
  inherited Visible := AValue;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmActionListView.FormShow(Sender: TObject);
begin
  UpdateLists;
  edtFilterActions.SetFocus;
end;

function TfrmActionListView.FTVPActionsCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
var
  A : TAction;
  Match : string;
  Margin : Integer;
  Offset : Integer;
  R      : TRect;
  S      : string;
  C      : TColor;
  OC     : TColor;
begin
  Result := True;
  if DrawMode = dmAfterCellPaint then
  begin
    A := TAction(Item);
    if ColumnDefinition.DisplayName = 'Name' then
    begin
      S := A.Name;
    end
    else if ColumnDefinition.DisplayName = 'Caption' then
    begin
      S := A.Caption;
    end;
    Margin := 4;
    Result := False;
    R := CellRect;
    TargetCanvas.FillRect(R);
    if IsMatch(S, Match, Offset) then
    begin
      R.Left := Margin + R.Left + TargetCanvas.TextWidth(System.Copy(S, 1, Offset - 1));
      R.Right := R.Left + TargetCanvas.TextWidth(Match);
      //TargetCanvas.Pen.Color := Manager.Settings.Colors.HighlightAllColor.FrameColor;
      TargetCanvas.Pen.Width := 1;
      C := ColorToRGB(TargetCanvas.Brush.Color);
      if C <> clWhite then
      begin
//        C := MixColors(
//          C,
//          Manager.Settings.Colors.HighlightAllColor.Background,
//          Manager.Settings.Colors.HighlightAllColor.BackAlpha
//        );
      end
      else
      begin
//        C := ColorAdjustLuma(
//          Manager.Settings.Colors.HighlightAllColor.Background,
//          50,
//          False
//        );
      end;
      OC := TargetCanvas.Brush.Color;
      TargetCanvas.Brush.Color := C;
      TargetCanvas.Rectangle(R);
      TargetCanvas.Brush.Color := OC;
    end;
    R := CellRect;
    //TargetCanvas.TextRect(R, R.Left + Margin, R.Top, S, FTextStyle);
  end;
end;

procedure TfrmActionListView.FTVPActionsFilter(Item: TObject;
  var Accepted: Boolean);
var
  A: TAction;
begin
  A := TAction(Item);
  Accepted := IsMatch(A.Name) or IsMatch(A.Caption);
end;

procedure TfrmActionListView.edtFilterActionsChange(Sender: TObject);
begin
  FTVPActions.ApplyFilter;
end;

procedure TfrmActionListView.edtFilterActionsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  A : Boolean;
  B : Boolean;
  C : Boolean;
  D : Boolean;
  E : Boolean;
  F : Boolean;
  G : Boolean;
  H : Boolean;
begin
  // SHIFTED and ALTED keycombinations
  A := (ssAlt in Shift) or (ssShift in Shift);
  { Single keys that need to be handled by the edit control like all displayable
    characters but also HOME and END }
  B := (Key in VK_EDIT_KEYS) and (Shift = []);
  { CTRL-keycombinations that need to be handled by the edit control like
    CTRL-C for clipboard copy. }
  C := (Key in VK_CTRL_EDIT_KEYS) {and (Shift = [ssCtrlOS])};
  { SHIFT-keycombinations that need to be handled by the edit control for
    uppercase characters but also eg. SHIFT-HOME for selections. }
  D := (Key in VK_SHIFT_EDIT_KEYS) and (Shift = [ssShift]);
  { Only CTRL key is pressed. }
  E := (Key = VK_CONTROL) {and (Shift = [ssCtrlOS])};
  { Only SHIFT key is pressed. }
  F := (Key = VK_SHIFT) and (Shift = [ssShift]);
  { Only (left) ALT key is pressed. }
  G := (Key = VK_MENU) and (Shift = [ssAlt]);
  { ESCAPE }
  H := Key = VK_ESCAPE;
  if not (A or B or C or D or E or F or G or H) then
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

procedure TfrmActionListView.edtFilterActionsKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if FVKPressed and FVSTActions.Enabled then
  begin
    FVSTActions.Perform(WM_KEYDOWN, Key, 0);
    if Visible and FVSTActions.CanFocus then
      FVSTActions.SetFocus;
  end;
  FVKPressed := False;
end;

{ Handled when KeyPreview = True. }

procedure TfrmActionListView.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
  end
  else
    inherited;
end;

procedure TfrmActionListView.FVSTActionsKeyPress(Sender: TObject; var Key: char
  );
begin
  if Ord(Key) = VK_RETURN then
  begin
    ModalResult := mrOK;
  end
  else if Ord(Key) = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
  end
  else if not edtFilterActions.Focused then
  begin
    edtFilterActions.SetFocus;
    PostMessage(edtFilterActions.Handle, WM_CHAR, Ord(Key), 0);
    edtFilterActions.SelStart := Length(Filter);
    // required to prevent the invocation of accelerator keys!
    Key := #0;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmActionListView.CreateActionsView;
var
  CD : IColumnDefinitions;
begin
  FVSTActions := TFactories.CreateVirtualStringTree(Self, pnlActions);
  FVSTActions.OnKeyPress := FVSTActionsKeyPress;
  FTVPActions := TTreeViewPresenter.Create(Self);
  FTVPActions.ListMode := True;
  FTVPActions.AllowMove := False;
  FTVPActions.SyncMode := True;
  FTVPActions.ImageList := Manager.Actions.ActionList.Images as TImageList;
  CD := FTVPActions.ColumnDefinitions;
  FTVPActions.View.ItemTemplate := TActionListTemplate.Create(CD);
  with CD.Add(SCategory) do
  begin
    ValuePropertyName := 'Category';
  end;
  with CD.Add(SName, 150) do
  begin
    ValuePropertyName := 'Name';
//    OnCustomDraw := FTVPActionsCustomDraw;
  end;
  with CD.Add('', 24) do
  begin
    ImageIndexPropertyName := 'ImageIndex';
  end;
  with CD.Add(SCaption, 120) do
  begin
     ValuePropertyName := 'Caption';
//    OnCustomDraw := FTVPActionsCustomDraw;
  end;
  with CD.Add(SShortcut,100) do
  begin
    AllowEdit := False;
  end;
  with CD.Add(SHint, 200) do
  begin
    ValuePropertyName := 'Hint';
    AllowEdit := True;
  end;
  with CD.Add(SVisible, 50) do
  begin
    ValuePropertyName := 'Visible';
    ColumnType := TColumnType.ctCheckBox;
    AllowEdit := True;
  end;
  with CD.Add(SEnabled, 55) do
  begin
    ValuePropertyName := 'Enabled';
    ColumnType := TColumnType.ctCheckBox;
    AllowEdit := True;
  end;
  FTVPActions.View.Filter.Add(FTVPActionsFilter);
  FTVPActions.View.ItemsSource := FActionItems;
  FTVPActions.TreeView    := FVSTActions;
end;

procedure TfrmActionListView.CreateCommandsView;
var
  CD : IColumnDefinitions;
begin
  FVSTCommands := TFactories.CreateVirtualStringTree(Self, tsCommands);
  FTVPCommands := TTreeViewPresenter.Create(Self);
  FTVPCommands.ListMode := True;
  CD := FTVPCommands.ColumnDefinitions;
  FTVPCommands.View.ItemTemplate := TKeyCommandTemplate.Create(CD);
  CD.Add(SCommand, 200);
  CD.Add(SShortcut, 120);
  CD.Add(SShortcut2, 120);
  FTVPCommands.View.ItemsSource := FCommandItems;
  FTVPCommands.TreeView    := FVSTCommands;
end;

procedure TfrmActionListView.UpdateView;
begin
  FVSTActions.Invalidate;
  FVSTCommands.Invalidate;
end;

procedure TfrmActionListView.UpdateLists;
var
  K: TCollectionItem;
  A: TContainedAction;
begin
  FActionItems.Clear;
  FCommandItems.Clear;
  for K in Manager.KeyCommands do
    FCommandItems.Add(K);
  for A in Manager.Actions.ActionList do
    FActionItems.Add(A);
  FTVPActions.Refresh;
  FTVPCommands.Refresh;
end;

function TfrmActionListView.IsMatch(const AString: string): Boolean;
begin
  if Filter = '' then
    Result := True
  else
    Result := StrPos(Filter, AString, False) > 0;
end;

function TfrmActionListView.IsMatch(const AString: string; var AMatch: string;
  var APos: Integer): Boolean;
var
  S : string;
begin
  APos   := 0;
  AMatch := '';
  Result := False;
  if Filter <> '' then
  begin
    // remove accelerator token
    S := StringReplace(AString, '&', '', [rfReplaceAll]);
    APos   := StrPos(Filter, S, False);
    AMatch := System.Copy(S, APos, Length(Filter));
    Result := APos > 0;
  end;
end;
{$ENDREGION}
{$ENDREGION}

end.
