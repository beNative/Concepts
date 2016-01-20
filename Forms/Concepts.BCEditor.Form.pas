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

interface

uses
  System.Classes, System.Actions,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms,
  Vcl.ActnList, Vcl.ToolWin,

  DDuce.Components.PropertyInspector, DDuce.Components.Gridview,

  Spring.Collections,

  BCEditor.Editor.Base, BCEditor.Editor, BCEditor.Highlighter;

type
  TfrmBCEditor = class(TForm)
    {$REGION 'designer controls'}
    pnlMain            : TPanel;
    pnlLeft            : TPanel;
    pnlRight           : TPanel;
    cbxControls        : TComboBox;
    sbrStatusBar       : TStatusBar;
    splSplitter        : TSplitter;
    pnlRightTop        : TPanel;
    pnlRightBottom     : TPanel;
    spl1               : TSplitter;
    pgcMain            : TPageControl;
    tsHighlighter      : TTabSheet;
    tsColors           : TTabSheet;
    aclMain            : TActionList;
    actSaveHighlighter : TAction;
    actSaveColorMap    : TAction;
    tlbHighlighter     : TToolBar;
    tlbColors          : TToolBar;
    btnSaveHighlighter : TToolButton;
    btnSaveColorMap    : TToolButton;
    pnlHighlighter     : TPanel;
    pnlColors          : TPanel;
    tsHighlighters     : TTabSheet;
    pnlHLLeft          : TPanel;
    pnlHLRight         : TPanel;
    pnlCMLeft          : TPanel;
    pnlCMRight         : TPanel;
    {$ENDREGION}

    procedure cbxControlsChange(Sender: TObject);

    procedure actSaveHighlighterExecute(Sender: TObject);
    procedure actSaveColorMapExecute(Sender: TObject);

  private
    FPropertyInspector : TPropertyInspector;
    FBCEditor          : TBCEditor;
    FHighlighterEditor : TBCEditor;
    FColorSchemeEditor : TBCEditor;
    FHighlighters      : IList<string>;
    FColorMaps         : IList<string>;
    FHLGrid            : TGridView;
    FCMGrid            : TGridView;

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

  public
    procedure AfterConstruction; override;

    procedure LoadHighlighters;
    procedure LoadColorMaps;

  end;

implementation

uses
  System.IOUtils, Vcl.Graphics,

  DDuce.Components.Factories, DDuce.Logger,

  Concepts.Factories;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmBCEditor.AfterConstruction;
begin
  inherited AfterConstruction;
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

  FCMGrid := TDDuceComponents.CreateGridView(Self, pnlCMLeft);
  FCMGrid.Header.Font.Style := [fsBold];
  FCMGrid.GridLines := False;
  with FCMGrid.Columns.Add do
  begin
    Caption := 'Colormaps';
    Width := 100;
  end;
  FCMGrid.OnGetCellText := FCMGridCellText;
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
  FPropertyInspector := TDDuceComponents.CreatePropertyInspector(
    Self,
    pnlLeft,
    FBCEditor
  );
  FPropertyInspector.Name := 'PropertyInspector';

//  FObjectInspector := TConceptFactories.CreatezObjectInspector(
//    Self,
//    pnlLeft,
//    FBCEditor
//  );

  cbxControls.AddItem(FBCEditor.Name, FBCEditor);
  cbxControls.AddItem(FBCEditor.Highlighter.FileName, FBCEditor.Highlighter);
  cbxControls.ItemIndex := 0;
  FHighlighters := TCollections.CreateList<string>;
  FColorMaps    := TCollections.CreateList<string>;
  LoadHighlighters;
  LoadColorMaps;

  WindowState := wsMaximized;
end;
{$ENDREGION}

{$REGION 'action handlers'}
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
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmBCEditor.cbxControlsChange(Sender: TObject);
var
  O: TObject;
begin
  O := cbxControls.Items.Objects[cbxControls.ItemIndex] as TObject;
end;

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
  S := FColorMaps[Cell.Row];
  S := FBCEditor.GetColorsFileName(S);
  FColorSchemeEditor.LoadFromFile(S);
  FBCEditor.Highlighter.Colors.LoadFromFile(S);
  FCMGrid.SetFocus;
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
  FBCEditor.Text := FBCEditor.Highlighter.Info.General.Sample;
  FHLGrid.SetFocus;
end;

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
{$ENDREGION}

end.
