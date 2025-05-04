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

unit Concepts.TextEditor.Form;

{ Form demonstrating the TTextEditor component.

  The official sources are based at:
          https://github.com/TextEditorPro/TTextEditor
}

interface

uses
  System.Classes, System.Actions, System.SysUtils,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms,
  Vcl.ActnList, Vcl.Graphics, Vcl.ToolWin,

  DDuce.Components.Gridview,

  Spring.Collections,

  zObjInspector, zObjInspTypes,

  TextEditor, TextEditor.Highlighter;

type
  TfrmTextEditor = class(TForm)
    {$REGION 'designer controls'}
    aclMain               : TActionList;
    actCollapseAll        : TAction;
    actExpandAll          : TAction;
    actSaveTheme          : TAction;
    actSaveHighlighter    : TAction;
    actTest               : TAction;
    btnCollapseAll        : TToolButton;
    btnExpandAll          : TToolButton;
    btnSaveColorMap       : TToolButton;
    btnSaveHighlighter    : TToolButton;
    pgcLeftBottoù         : TPageControl;
    pgcMain               : TPageControl;
    pnlCMLeft             : TPanel;
    pnlCMRight            : TPanel;
    pnlCMRightRight       : TPanel;
    pnlColors             : TPanel;
    pnlExampleCodeHeader  : TPanel;
    pnlHighlighter        : TPanel;
    pnlHLLeft             : TPanel;
    pnlHLRight            : TPanel;
    pnlLeft               : TPanel;
    pnlLeftBottom         : TPanel;
    pnlMain               : TPanel;
    pnlRight              : TPanel;
    pnlRightBottom        : TPanel;
    pnlRightTop           : TPanel;
    sbrStatusBar          : TStatusBar;
    spl1                  : TSplitter;
    splLeftHorizontal     : TSplitter;
    splSplitter           : TSplitter;
    splVerticalRight      : TSplitter;
    tlbColors             : TToolBar;
    tlbComponentInspector : TToolBar;
    tlbHighlighter        : TToolBar;
    tsThemes              : TTabSheet;
    tsHighlighter         : TTabSheet;
    tsSampleCode          : TTabSheet;
    pnlHeader             : TPanel;
    lblHeader             : TLabel;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actSaveHighlighterExecute(Sender: TObject);
    procedure actSaveThemeExecute(Sender: TObject);
    procedure actCollapseAllExecute(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
    procedure actTestExecute(Sender: TObject);
    {$ENDREGION}

  private
    FObjectInspector   : TzObjectInspector;
    FTextEditor        : TTextEditor;
    FHighlighterEditor : TTextEditor;
    FThemeEditor       : TTextEditor;
    FExampleCodeEditor : TTextEditor;
    FHighlighters      : IList<string>;
    FThemes            : IList<string>;
    FHighlightersGrid  : TGridView;
    FThemesGrid        : TGridView;
    FColorGrid         : TGridView;

    {$REGION 'event handlers'}
    procedure FColorGridCellText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );
    procedure FColorGridCellColors(
      Sender : TObject;
      Cell   : TGridCell;
      Canvas : TCanvas
    );

    procedure FHighlightersGridCellText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );
    procedure FHighlightersGridChange(
      Sender   : TObject;
      Cell     : TGridCell;
      Selected : Boolean
    );
    procedure FHighlightersGridChanging(
      Sender       : TObject;
      var Cell     : TGridCell;
      var Selected : Boolean
    );

    procedure FThemesGridCellText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );

    procedure FThemesGridChange(
      Sender   : TObject;
      Cell     : TGridCell;
      Selected : Boolean
    );
    procedure FThemesGridChanging(
      Sender       : TObject;
      var Cell     : TGridCell;
      var Selected : Boolean
    );

    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;
    {$ENDREGION}

    procedure LoadHighlighters;
    procedure LoadThemes;

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  System.IOUtils, System.Types, System.Rtti, System.TypInfo,
  Vcl.GraphUtil, Vcl.Dialogs,

  DDuce.Factories.GridView, DDuce.Factories.zObjInspector, DDuce.Logger,
  DDuce.Utils,

  Concepts.Factories;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmTextEditor.AfterConstruction;
begin
  inherited AfterConstruction;

  FColorGrid := TGridViewFactory.CreateGridView(Self, pnlCMRightRight);
  FColorGrid.GridLines := False;
  with FColorGrid.Columns.Add do
  begin
    Caption := 'Name';
    Width := 150;
  end;
  with FColorGrid.Columns.Add do
  begin
    Caption := 'Color';
    Width := 50;
  end;
  FColorGrid.OnGetCellText   := FColorGridCellText;
  FColorGrid.OnGetCellColors := FColorGridCellColors;
  FColorGrid.Rows.Count := WebNamedColorsCount;

  FHighlightersGrid := TGridViewFactory.CreateGridView(Self, pnlHLLeft);
  FHighlightersGrid.Header.Font.Style := [fsBold];
  FHighlightersGrid.GridLines := False;
  with FHighlightersGrid.Columns.Add do
  begin
    Caption := 'Highlighters';
    Width := 100;
  end;
  FHighlightersGrid.OnGetCellText := FHighlightersGridCellText;
  FHighlightersGrid.OnChange      := FHighlightersGridChange;
  FHighlightersGrid.OnChanging    := FHighlightersGridChanging;

  FThemesGrid := TGridViewFactory.CreateGridView(Self, pnlCMLeft);
  FThemesGrid.Header.Font.Style := [fsBold];
  FThemesGrid.GridLines := False;
  with FThemesGrid.Columns.Add do
  begin
    Caption := 'Themes';
    Width := 100;
  end;
  FThemesGrid.OnGetCellText := FThemesGridCellText;
  FThemesGrid.OnChanging    := FThemesGridChanging;
  FThemesGrid.OnChange      := FThemesGridChange;
  FTextEditor := TConceptFactories.CreateTextEditor(
    Self,
    pnlRightBottom,
    '',
    'Object Pascal',
    'tsColors'
  );
  FTextEditor.Fonts.Text.Size := 11;
  FHighlighterEditor := TConceptFactories.CreateTextEditor(
    Self,
    pnlHLRight,
    //FTextEditor.GetHighlighterFileName(FTextEditor.Highlighter.FileName),
    '',
    'JSON',
    'tsColors'
  );
  FHighlighterEditor.Fonts.Text.Size := 11;
  FThemeEditor := TConceptFactories.CreateTextEditor(
    Self,
    tsThemes,
//    FTextEditor.GetColorsFileName(FTextEditor.Highlighter.Colors.FileName),
    '',
    'JSON',
    'tsColors'
  );
  FThemeEditor.Fonts.Text.Size := 11;
  FExampleCodeEditor := TConceptFactories.CreateTextEditor(
    Self,
    tsSampleCode,
    '',
    'Object Pascal',
    'tsColors'
  );
  FExampleCodeEditor.Fonts.Text.Size := 11;

  FObjectInspector := TzObjectInspectorFactory.Create(Self, pnlLeft);

  FObjectInspector.OnBeforeAddItem := FObjectInspectorBeforeAddItem;
  FObjectInspector.ObjectVisibility := mvPublic;
  FObjectInspector.Component := FTextEditor;
  FObjectInspector.ExpandAll;
  FHighlighters := TCollections.CreateList<string>;
  FThemes       := TCollections.CreateList<string>;
  LoadHighlighters;
  LoadThemes;

  WindowState := wsMaximized;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmTextEditor.actCollapseAllExecute(Sender: TObject);
begin
  FObjectInspector.CollapseAll;
end;

procedure TfrmTextEditor.actExpandAllExecute(Sender: TObject);
begin
  FObjectInspector.ExpandAll;
end;

procedure TfrmTextEditor.actSaveThemeExecute(Sender: TObject);
var
  S : string;
begin
  S := FThemes[FThemesGrid.CellFocused.Row];
  FThemeEditor.SaveToFile(S);
  FTextEditor.Highlighter.Colors.LoadFromFile(S);
end;

procedure TfrmTextEditor.actSaveHighlighterExecute(Sender: TObject);
var
  S : string;
begin
  S := FHighlighters[FHighlightersGrid.CellFocused.Row];
  FHighlighterEditor.SaveToFile(S);
  FTextEditor.Highlighter.LoadFromFile(S);
end;

procedure TfrmTextEditor.actTestExecute(Sender: TObject);
begin
  ShowMessage('Works');
end;

{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmTextEditor.FThemesGridCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
begin
  Value := TPath.GetFileNameWithoutExtension(FThemes[Cell.Row]);
end;

procedure TfrmTextEditor.FThemesGridChange(Sender: TObject; Cell: TGridCell;
  Selected: Boolean);
var
  S : string;
begin
  if not FThemeEditor.Modified then
  begin
    S := FThemes[Cell.Row];
    FThemeEditor.LoadFromFile(S);
    FTextEditor.Highlighter.Colors.LoadFromFile(S);
    FExampleCodeEditor.Highlighter.Colors.LoadFromFile(S);
    FThemesGrid.SetFocus;
  end;
end;

procedure TfrmTextEditor.FThemesGridChanging(Sender: TObject; var Cell: TGridCell;
  var Selected: Boolean);
begin
  if FThemeEditor.Modified then
  begin
    if AskConfirmation('Save?') then
    begin
      actSaveTheme.Execute;
    end
    else
    begin
      Cell := FThemesGrid.CellFocused;
    end;
  end;
end;

procedure TfrmTextEditor.FColorGridCellColors(Sender: TObject; Cell: TGridCell;
  Canvas: TCanvas);
begin
  if Cell.Col = 1 then
  begin
    Canvas.Brush.Color := WebNamedColors[Cell.Row].Value;
  end;
end;

procedure TfrmTextEditor.FColorGridCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
begin
  if Cell.Col = 0 then
  begin
    Value := WebNamedColors[Cell.Row].Name;
  end;
end;

procedure TfrmTextEditor.FHighlightersGridCellText(Sender: TObject;
  Cell: TGridCell; var Value: string);
begin
  Value := TPath.GetFileNameWithoutExtension(FHighlighters[Cell.Row]);
end;

procedure TfrmTextEditor.FHighlightersGridChange(Sender: TObject;
  Cell: TGridCell; Selected: Boolean);
var
  S : string;
begin
  S := FHighlighters[Cell.Row];
  FHighlighterEditor.LoadFromFile(S);
  FTextEditor.Highlighter.LoadFromFile(S);
  FExampleCodeEditor.Highlighter.LoadFromFile(S);
  FExampleCodeEditor.Text := FTextEditor.Highlighter.Sample;
  FHighlightersGrid.SetFocus;
end;

procedure TfrmTextEditor.FHighlightersGridChanging(Sender: TObject;
  var Cell: TGridCell; var Selected: Boolean);
begin
  if FHighlighterEditor.Modified then
  begin
    if AskConfirmation('Save?') then
    begin
      actSaveHighlighter.Execute;
    end
    else
    begin
      Cell := FHighlightersGrid.CellFocused;
    end;
  end;
end;

function TfrmTextEditor.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not PItem.Name.Contains('ComObject');
  Result := Result and (not (PItem.Prop.PropertyType is TRttiMethodType));
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmTextEditor.LoadThemes;
begin
  FThemes.AddRange(TDirectory.GetFiles('.\Themes', '*.json'));
  FThemesGrid.Rows.Count := FThemes.Count;
  FThemesGrid.Refresh;
  FThemesGrid.AutoSizeCols;
end;

procedure TfrmTextEditor.LoadHighlighters;
begin
  FHighlighters.AddRange(TDirectory.GetFiles('.\Highlighters', '*.json'));
  FHighlightersGrid.Rows.Count := FHighlighters.Count;
  FHighlightersGrid.Refresh;
  FHighlightersGrid.AutoSizeCols;
end;

procedure TfrmTextEditor.UpdateActions;
begin
  inherited UpdateActions;
  actSaveHighlighter.Enabled :=
    tsHighlighter.Visible and FHighlighterEditor.Modified;
  actSaveTheme.Enabled       :=
    tsThemes.Visible and FThemeEditor.Modified;
end;
{$ENDREGION}

end.
