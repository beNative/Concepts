{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  Vcl.Forms, Vcl.ActnList, Vcl.ToolWin,

  DDuce.Components.PropertyInspector,

  BCEditor.Editor.Base, BCEditor.Editor;

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
    {$ENDREGION}

    procedure cbxControlsChange(Sender: TObject);
    procedure actSaveHighlighterExecute(Sender: TObject);
    procedure actSaveColorMapExecute(Sender: TObject);

  private
    FPropertyInspector : TPropertyInspector;
    FBCEditor          : TBCEditor;
    FHighlighterEditor : TBCEditor;
    FColorSchemeEditor : TBCEditor;

  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  DDuce.Components.Factories,

  Concepts.Factories;

{$R *.dfm}

{$REGION 'construction and destruction'}
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

procedure TfrmBCEditor.AfterConstruction;
var
  I: Integer;
  C: TWinControl;
begin
  inherited;
  FBCEditor := TConceptFactories.CreateBCEditor(
    Self,
    pnlRightBottom,
    '',
    'JSON',
    'Default'
  );

  FHighlighterEditor := TConceptFactories.CreateBCEditor(
    Self,
    tsHighlighter,
    FBCEditor.Highlighter.FileName,
    'JSON',
    'Default'
  );
  FColorSchemeEditor := TConceptFactories.CreateBCEditor(
    Self,
    tsColors,
    FBCEditor.Highlighter.Colors.FileName,
    'JSON',
    'Default'
  );
  FPropertyInspector := TDDuceComponents.CreatePropertyInspector(
    Self,
    pnlLeft,
    FBCEditor
  );
  FPropertyInspector.Name := 'PropertyInspector';
//  for I := 0 to ComponentCount - 1 do
//  begin
//    if Components[I] is TWinControl then
//    begin
//      C := TWinControl(Components[I]);
//      cbxControls.AddItem(C.Name, C);
//    end;
//  end;

   //FBCEditor.Highlighter.FileName




  cbxControls.AddItem(FBCEditor.Name, FBCEditor);
  cbxControls.AddItem(FBCEditor.Highlighter.FileName, FBCEditor.Highlighter);
  cbxControls.ItemIndex := 0;

end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmBCEditor.cbxControlsChange(Sender: TObject);
var
  O: TObject;
begin
  O := cbxControls.Items.Objects[cbxControls.ItemIndex] as TObject;
  FPropertyInspector.Objects[0] := O;
end;
{$ENDREGION}

end.
