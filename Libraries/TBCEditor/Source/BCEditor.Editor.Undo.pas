unit BCEditor.Editor.Undo;

interface

uses
  System.Classes, BCEditor.Types;

type
  TBCEditorUndo = class(TPersistent)
  strict private
    FOptions: TBCEditorUndoOptions;
    procedure SetOptions(const AValue: TBCEditorUndoOptions);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorUndoOption; const AEnabled: Boolean);
  published
    property Options: TBCEditorUndoOptions read FOptions write SetOptions default [uoGroupUndo];
  end;

implementation

constructor TBCEditorUndo.Create;
begin
  inherited;

  FOptions := [uoGroupUndo];
end;

procedure TBCEditorUndo.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorUndo then
  with ASource as TBCEditorUndo do
    Self.FOptions := FOptions
  else
    inherited Assign(ASource);
end;

procedure TBCEditorUndo.SetOption(const AOption: TBCEditorUndoOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorUndo.SetOptions(const AValue: TBCEditorUndoOptions);
begin
  if FOptions <> AValue then
    FOptions := AValue;
end;

end.
