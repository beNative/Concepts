unit BCCommon.Frame.Options.Editor.Options;

interface

uses
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  BCControl.Edit, BCCommon.Options.Container, BCCommon.Frame.Options.Base,
  BCControl.Panel, acSlider, sLabel, sEdit, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsEditorOptionsFrame = class(TBCOptionsBaseFrame)
    EditLineSpacing: TBCEdit;
    Panel: TBCPanel;
    StickyLabelAutoIndent: TsStickyLabel;
    SliderAutoIndent: TsSlider;
    SliderAutoSave: TsSlider;
    StickyLabelAutoSave: TsStickyLabel;
    StickyLabelDragDropEditing: TsStickyLabel;
    SliderDragDropEditing: TsSlider;
    SliderDropFiles: TsSlider;
    StickyLabelDropFiles: TsStickyLabel;
    StickyLabelGroupUndo: TsStickyLabel;
    SliderGroupUndo: TsSlider;
    StickyLabelTrimTrailingSpaces: TsStickyLabel;
    SliderTrimTrailingSpaces: TsSlider;
    StickyLabelUndoAfterSave: TsStickyLabel;
    SliderUndoAfterSave: TsSlider;
  protected
    procedure GetData; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsEditorOptionsFrame(AOwner: TComponent): TOptionsEditorOptionsFrame;

implementation

{$R *.dfm}

uses
  System.SysUtils, BCCommon.Utils;

var
  FOptionsEditorOptionsFrame: TOptionsEditorOptionsFrame;

function OptionsEditorOptionsFrame(AOwner: TComponent): TOptionsEditorOptionsFrame;
begin
  if not Assigned(FOptionsEditorOptionsFrame) then
    FOptionsEditorOptionsFrame := TOptionsEditorOptionsFrame.Create(AOwner);
  Result := FOptionsEditorOptionsFrame;
  AlignSliders(Result.Panel);
end;

destructor TOptionsEditorOptionsFrame.Destroy;
begin
  inherited;
  FOptionsEditorOptionsFrame := nil;
end;

procedure TOptionsEditorOptionsFrame.PutData;
begin
  OptionsContainer.AutoIndent := SliderAutoIndent.SliderOn;
  OptionsContainer.AutoSave := SliderAutoSave.SliderOn;
  OptionsContainer.DragDropEditing := SliderDragDropEditing.SliderOn;
  OptionsContainer.DropFiles := SliderDropFiles.SliderOn;
  OptionsContainer.GroupUndo := SliderGroupUndo.SliderOn;
  OptionsContainer.TrimTrailingSpaces := SliderTrimTrailingSpaces.SliderOn;
  OptionsContainer.UndoAfterSave := SliderUndoAfterSave.SliderOn;
  OptionsContainer.LineSpacing := StrToIntDef(EditLineSpacing.Text, 1);
end;

procedure TOptionsEditorOptionsFrame.GetData;
begin
  SliderAutoIndent.SliderOn := OptionsContainer.AutoIndent;
  SliderAutoSave.SliderOn := OptionsContainer.AutoSave;
  SliderDragDropEditing.SliderOn := OptionsContainer.DragDropEditing;
  SliderDropFiles.SliderOn := OptionsContainer.DropFiles;
  SliderGroupUndo.SliderOn := OptionsContainer.GroupUndo;
  SliderTrimTrailingSpaces.SliderOn := OptionsContainer.TrimTrailingSpaces;
  SliderUndoAfterSave.SliderOn := OptionsContainer.UndoAfterSave;
  EditLineSpacing.Text := IntToStr(OptionsContainer.LineSpacing);
end;

end.
