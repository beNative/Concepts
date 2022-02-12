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

unit Concepts.MainForm;

{ Shows a list of all registered concepts which can be started. }

interface

uses
  System.Actions, System.Classes, System.Win.TaskbarCore,
  Vcl.ActnList, Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Taskbar,

  VirtualTrees,

  DSharp.Windows.ColumnDefinitions, DSharp.Windows.TreeViewPresenter;

type
  TfrmMain = class(TForm)
    {$REGION 'designer controls'}
    aclMain           : TActionList;
    actCenterMainForm : TAction;
    actClose          : TAction;
    actExecute        : TAction;
    actExecuteModal   : TAction;
    btnClose          : TButton;
    btnExecute        : TButton;
    btnExecuteModal   : TButton;
    edtFilter         : TEdit;
    pnlButtons        : TGridPanel;
    pnlVST            : TPanel;
    sbrMain           : TStatusBar;
    tbrMain           : TTaskbar;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actExecuteExecute(Sender: TObject);
    procedure actExecuteModalExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCenterMainFormExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure edtFilterKeyDown(
      Sender  : TObject;
      var Key : Word;
      Shift   : TShiftState
    );
    procedure edtFilterKeyUp(
      Sender  : TObject;
      var Key : Word;
      Shift   : TShiftState
    );
    procedure edtFilterChange(Sender: TObject);
    {$ENDREGION}

  private
    FVKPressed : Boolean;
    FVST       : TVirtualStringTree;
    FTVP       : TTreeViewPresenter;

    {$REGION 'event handlers'}
    function FTVPColumnDefinitionsCustomDrawColumn(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ): Boolean;
    procedure FTVPFilter(Item: TObject; var Accepted: Boolean);
    procedure FTVPDoubleClick(Sender: TObject);
    procedure FVSTKeyPress(Sender: TObject; var Key: Char);
    {$ENDREGION}

    procedure InitializePresenter;
    procedure ApplyFilter;

    procedure LoadSettings;
    procedure SaveSettings;

  public
    procedure AfterConstruction; override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  Winapi.Windows, Winapi.Messages,
  System.StrUtils, System.SysUtils, System.UITypes,
  Vcl.Graphics,

  Spring.Collections,

  Concepts.Factories, Concepts.Manager, Concepts.Settings;

resourcestring
  SConceptsLoaded = '%d concepts loaded.';

{$REGION 'Virtual Key sets'}
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
    Byte('0')..Byte('Z'),
    VK_OEM_1..VK_OEM_102,
    VK_MULTIPLY..VK_DIVIDE
  ];

  VK_CTRL_EDIT_KEYS : TVKSet = [
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END,
    Byte('C'),
    Byte('X'),
    Byte('V'),
    Byte('Z')
  ];

  VK_SHIFT_EDIT_KEYS : TVKSet = [
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END
  ];
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := TConceptFactories.CreateVirtualStringTree(Self, pnlVST);
  FVST.Header.Font.Style := FVST.Header.Font.Style + [fsBold];
  FTVP := TConceptFactories.CreateTreeViewPresenter(Self);
  FTVP.Sorting := False;
  InitializePresenter;
  sbrMain.SimpleText := Format(SConceptsLoaded, [ConceptManager.ItemList.Count]);
  LoadSettings;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMain.actExecuteExecute(Sender: TObject);
var
  O : TObject;
begin
  for O in FTVP.SelectedItems do
  begin
    ConceptManager.Execute(O, False);
  end;
end;

procedure TfrmMain.actExecuteModalExecute(Sender: TObject);
begin
  ConceptManager.Execute(FTVP.SelectedItem);
end;

procedure TfrmMain.actCenterMainFormExecute(Sender: TObject);
begin
  Position := poScreenCenter;
  SaveSettings;
  LoadSettings;
end;

procedure TfrmMain.actCloseExecute(Sender: TObject);
begin
  SaveSettings;
  Close;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMain.edtFilterChange(Sender: TObject);
begin
  ApplyFilter;
  FVST.FocusedNode := FVST.GetFirstVisible;
  FVST.Selected[FVST.FocusedNode] := True;
end;

procedure TfrmMain.edtFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  A : Boolean;
  B : Boolean;
  C : Boolean;
  D : Boolean;
begin
  A := (ssAlt in Shift) or (ssShift in Shift);
  B := (Key in VK_EDIT_KEYS) and (Shift = []);
  C := (Key in VK_CTRL_EDIT_KEYS) and (Shift = [ssCtrl]);
  D := (Key in VK_SHIFT_EDIT_KEYS) and (Shift = [ssShift]);
  if not (A or B or C or D) then
  begin
    FVKPressed := True;
    Key := 0;
  end;
end;

procedure TfrmMain.edtFilterKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FVKPressed and FVST.Enabled then
  begin
    PostMessage(FVST.Handle, WM_KEYDOWN, Key, 0);
    if Visible and FVST.CanFocus then
      FVST.SetFocus;
  end;
  FVKPressed := False;
end;

function TfrmMain.FTVPColumnDefinitionsCustomDrawColumn(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
var
  C : TConcept;
begin
  C := Item as TConcept;
  if ColumnDefinition.Index < 2 then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
  if DrawMode = TDrawMode.dmBeforeCellPaint then
  begin
    TargetCanvas.Brush.Color := C.Color;
    TargetCanvas.FillRect(CellRect);
  end;
  Result := True;
end;

procedure TfrmMain.FTVPDoubleClick(Sender: TObject);
begin
  ConceptManager.Execute(FTVP.SelectedItem, False);
end;

procedure TfrmMain.FTVPFilter(Item: TObject; var Accepted: Boolean);
var
  C : TConcept;
begin
  if edtFilter.Text <> '' then
  begin
    C := TConcept(Item);
    Accepted :=
      ContainsText(C.Name, edtFilter.Text)
      or ContainsText(C.SourceFilename, edtFilter.Text)
      or ContainsText(C.Category, edtFilter.Text)
      or ContainsText(C.Description, edtFilter.Text)
  end
  else
    Accepted := True;
end;

procedure TfrmMain.FVSTKeyPress(Sender: TObject; var Key: Char);
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
    edtFilter.SelStart := Length(edtFilter.Text);
    // required to prevent the invocation of accelerator keys!
    Key := #0;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmMain.InitializePresenter;
begin
  with FTVP.ColumnDefinitions.Add('Category') do
  begin
    ValuePropertyName := 'Category';
    Alignment         := taCenter;
    OnCustomDraw      := FTVPColumnDefinitionsCustomDrawColumn;
    AutoSize          := True;
  end;
  with FTVP.ColumnDefinitions.Add('Name') do
  begin
    ValuePropertyName := 'Name';
    Alignment         := taLeftJustify;
    OnCustomDraw      := FTVPColumnDefinitionsCustomDrawColumn;
    AutoSize          := True;
  end;
  with FTVP.ColumnDefinitions.Add('Description') do
  begin
    ValuePropertyName := 'Description';
    Alignment         := taLeftJustify;
    OnCustomDraw      := FTVPColumnDefinitionsCustomDrawColumn;
    AutoSize          := True;
  end;
  with FTVP.ColumnDefinitions.Add('SourceFilename') do
  begin
    ValuePropertyName := 'SourceFilename';
    Alignment         := taLeftJustify;
    OnCustomDraw      := FTVPColumnDefinitionsCustomDrawColumn;
    AutoSize          := True;
  end;
  FTVP.View.ItemsSource := ConceptManager.ItemList as IObjectList;
  FTVP.TreeView         := FVST;
  FTVP.OnDoubleClick    := FTVPDoubleClick;
  FTVP.SelectionMode    := smMulti;
  FTVP.View.Filter.Add(FTVPFilter);
  FTVP.TreeView.OnKeyPress := FVSTKeyPress;
  FTVP.TreeView.Header.AutoFitColumns;
  FTVP.Refresh;
end;

procedure TfrmMain.LoadSettings;
begin
  Position := poScreenCenter;
  Left   := Settings.ReadInteger(UnitName, 'Left', Left);
  Top    := Settings.ReadInteger(UnitName, 'Top', Top);
  Width  := Settings.ReadInteger(UnitName, 'Width', Width);
  Height := Settings.ReadInteger(UnitName, 'Height', Height);
end;

procedure TfrmMain.SaveSettings;
begin
  Settings.WriteInteger(UnitName, 'Left', Left);
  Settings.WriteInteger(UnitName, 'Top', Top);
  Settings.WriteInteger(UnitName, 'Width', Width);
  Settings.WriteInteger(UnitName, 'Height', Height);
end;

procedure TfrmMain.ApplyFilter;
begin
  FTVP.ApplyFilter;
end;
{$ENDREGION}

end.
