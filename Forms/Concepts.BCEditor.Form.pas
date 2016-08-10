{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.BCEditor.Form;

{ Form demonstrating the TBCEditor component.

  The official sources of these component can be found at:
          http://github.com/bonecode/TBCEditor
}

interface

uses
  System.Classes, System.Actions, System.SysUtils,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms,
  Vcl.ActnList, Vcl.Graphics, Vcl.ToolWin,

  DDuce.Components.Gridview,

  Spring.Collections,

  zObjInspector,

  BCEditor.Editor.Base, BCEditor.Editor, BCEditor.Highlighter;

type
  TfrmBCEditor = class(TForm)
    {$REGION 'designer controls'}
    aclMain               : TActionList;
    actCollapseAll        : TAction;
    actExpandAll          : TAction;
    actSaveColorMap       : TAction;
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
    tsColors              : TTabSheet;
    tsHighlighter         : TTabSheet;
    tsSampleCode          : TTabSheet;
    {$ENDREGION}

    procedure actSaveHighlighterExecute(Sender: TObject);
    procedure actSaveColorMapExecute(Sender: TObject);
    procedure actCollapseAllExecute(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
    procedure actTestExecute(Sender: TObject);

  private
    FObjectInspector   : TzObjectInspector;
    FBCEditor          : TBCEditor;
    FHighlighterEditor : TBCEditor;
    FColorSchemeEditor : TBCEditor;
    FExampleCodeEditor : TBCEditor;
    FHighlighters      : IList<string>;
    FColorMaps         : IList<string>;
    FHLGrid            : TGridView;
    FCMGrid            : TGridView;
    FColorGrid         : TGridView;

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

    procedure FHLGridCellText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );

    procedure FHLGridChange(
      Sender   : TObject;
      Cell     : TGridCell;
      Selected : Boolean
    );

    procedure FHLGridChanging(
      Sender       : TObject;
      var Cell     : TGridCell;
      var Selected : Boolean
    );

    procedure FCMGridCellText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );

    procedure FCMGridChange(
      Sender   : TObject;
      Cell     : TGridCell;
      Selected : Boolean
    );

    procedure FCMGridChanging(
      Sender       : TObject;
      var Cell     : TGridCell;
      var Selected : Boolean
    );

    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

    procedure LoadHighlighters;
    procedure LoadColorMaps;

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  System.IOUtils, System.Types, System.Rtti, System.TypInfo,
  Vcl.GraphUtil, Vcl.Dialogs,

  DDuce.Components.Factories, DDuce.Logger,

  Concepts.Factories, Concepts.Utils;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmBCEditor.AfterConstruction;
begin
  inherited AfterConstruction;

  FColorGrid := TDDuceComponents.CreateGridView(Self, pnlCMRightRight);
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

  FHLGrid := TDDuceComponents.CreateGridView(Self, pnlHLLeft);
  FHLGrid.Header.Font.Style := [fsBold];
  FHLGrid.GridLines := False;
  with FHLGrid.Columns.Add do
  begin
    Caption := 'Highlighters';
    Width := 100;
  end;
  FHLGrid.OnGetCellText := FHLGridCellText;
  FHLGrid.OnChange      := FHLGridChange;
  FHLGrid.OnChanging    := FHLGridChanging;

  FCMGrid := TDDuceComponents.CreateGridView(Self, pnlCMLeft);
  FCMGrid.Header.Font.Style := [fsBold];
  FCMGrid.GridLines := False;
  with FCMGrid.Columns.Add do
  begin
    Caption := 'Colormaps';
    Width := 100;
  end;
  FCMGrid.OnGetCellText := FCMGridCellText;
  FCMGrid.OnChanging    := FCMGridChanging;
  FCMGrid.OnChange      := FCMGridChange;

  FBCEditor := TConceptFactories.CreateBCEditor(
    Self,
    pnlRightBottom,
    '',
    'Object Pascal',
    'tsColors'
  );
  FHighlighterEditor := TConceptFactories.CreateBCEditor(
    Self,
    pnlHLRight,
    FBCEditor.GetHighlighterFileName(FBCEditor.Highlighter.FileName),
    'JSON',
    'tsColors'
  );
  FColorSchemeEditor := TConceptFactories.CreateBCEditor(
    Self,
    tsColors,
    FBCEditor.GetColorsFileName(FBCEditor.Highlighter.Colors.FileName),
    'JSON',
    'tsColors'
  );
  FExampleCodeEditor := TConceptFactories.CreateBCEditor(
    Self,
    tsSampleCode,
    '',
    'Object Pascal',
    'tsColors'
  );

  FObjectInspector := TConceptFactories.CreatezObjectInspector(
    Self,
    pnlLeft
  );

  FObjectInspector.OnBeforeAddItem := FObjectInspectorBeforeAddItem;
  FObjectInspector.ObjectVisibility := mvPublic;
  FObjectInspector.Component := FBCEditor;
  FObjectInspector.ExpandAll;
  FHighlighters := TCollections.CreateList<string>;
  FColorMaps    := TCollections.CreateList<string>;
  LoadHighlighters;
  LoadColorMaps;

  WindowState := wsMaximized;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmBCEditor.actCollapseAllExecute(Sender: TObject);
begin
  FObjectInspector.CollapseAll;
end;

procedure TfrmBCEditor.actExpandAllExecute(Sender: TObject);
begin
  FObjectInspector.ExpandAll;
end;

procedure TfrmBCEditor.actSaveColorMapExecute(Sender: TObject);
var
  S : string;
begin
  S := FBCEditor.Highlighter.Colors.FileName;
  FColorSchemeEditor.SaveToFile(S);
  FBCEditor.Highlighter.Colors.LoadFromFile(S);
end;

procedure TfrmBCEditor.actSaveHighlighterExecute(Sender: TObject);
var
  S : string;
begin
  S := FBCEditor.Highlighter.FileName;
  FHighlighterEditor.SaveToFile(S);
  FBCEditor.Highlighter.LoadFromFile(S);
end;

procedure TfrmBCEditor.actTestExecute(Sender: TObject);
begin
  ShowMessage('Works');
end;

{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmBCEditor.FCMGridCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
begin
  Value := TPath.GetFileNameWithoutExtension(FColorMaps[Cell.Row]);//
end;

procedure TfrmBCEditor.FCMGridChange(Sender: TObject; Cell: TGridCell;
  Selected: Boolean);
var
  S : string;
begin
  if not FColorSchemeEditor.Modified then
  begin
    S := FColorMaps[Cell.Row];
    S := FBCEditor.GetColorsFileName(S);
    FColorSchemeEditor.LoadFromFile(S);
    FBCEditor.Highlighter.Colors.LoadFromFile(S);
    FExampleCodeEditor.Highlighter.Colors.LoadFromFile(S);
    FCMGrid.SetFocus;
  end;
end;

procedure TfrmBCEditor.FCMGridChanging(Sender: TObject; var Cell: TGridCell;
  var Selected: Boolean);
begin
  if FColorSchemeEditor.Modified then
  begin
    if AskConfirmation('Save?') then
    begin
      actSaveColorMap.Execute;
    end
    else
    begin
      Cell := FCMGrid.CellFocused;
    end;
  end;
end;

procedure TfrmBCEditor.FColorGridCellColors(Sender: TObject; Cell: TGridCell;
  Canvas: TCanvas);
begin
  if Cell.Col = 1 then
  begin
    Canvas.Brush.Color := WebNamedColors[Cell.Row].Value;
  end;
end;

procedure TfrmBCEditor.FColorGridCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
begin
  if Cell.Col = 0 then
  begin
    Value := WebNamedColors[Cell.Row].Name;
  end;
end;

procedure TfrmBCEditor.FHLGridCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
begin
  Value := TPath.GetFileNameWithoutExtension(FHighlighters[Cell.Row]);
end;

procedure TfrmBCEditor.FHLGridChange(Sender: TObject; Cell: TGridCell;
  Selected: Boolean);
var
  S : string;
begin
  S := FHighlighters[Cell.Row];
  S := FBCEditor.GetHighlighterFileName(S);
  FHighlighterEditor.LoadFromFile(S);
  FBCEditor.Highlighter.LoadFromFile(S);
  FExampleCodeEditor.Highlighter.LoadFromFile(S);
  FExampleCodeEditor.Text := FBCEditor.Highlighter.Info.General.Sample;
  FHLGrid.SetFocus;
end;

procedure TfrmBCEditor.FHLGridChanging(Sender: TObject; var Cell: TGridCell;
  var Selected: Boolean);
begin
  if FHighlighterEditor.Modified then
  begin
    if AskConfirmation('Save?') then
    begin
      actSaveHighlighter.Execute;
    end
    else
    begin
      Cell := FHLGrid.CellFocused;
    end;
  end;
end;

function TfrmBCEditor.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not PItem.Name.Contains('ComObject');
  Result := Result and (not (PItem.Prop.PropertyType is TRttiMethodType));
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmBCEditor.LoadColorMaps;
begin
  FColorMaps.AddRange(TDirectory.GetFiles(FBCEditor.Directories.Colors, '*.json'));
  FCMGrid.Rows.Count := FColorMaps.Count;
  FCMGrid.Refresh;
  FCMGrid.AutoSizeCols;
end;

procedure TfrmBCEditor.LoadHighlighters;
begin
  FHighlighters.AddRange(TDirectory.GetFiles(FBCEditor.Directories.Highlighters, '*.json'));
  FHLGrid.Rows.Count := FHighlighters.Count;
  FHLGrid.Refresh;
  FHLGrid.AutoSizeCols;
end;

procedure TfrmBCEditor.UpdateActions;
begin
  inherited UpdateActions;
  actSaveHighlighter.Enabled :=
    tsHighlighter.Visible and FHighlighterEditor.Modified;
  actSaveColorMap.Enabled    :=
    tsColors.Visible and FColorSchemeEditor.Modified;
end;
{$ENDREGION}

end.
