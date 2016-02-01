unit BCEditor.Editor.Undo;

interface

uses
  System.Classes, BCEditor.Consts, BCEditor.Types;

type
  TBCEditorUndo = class(TPersistent)
  strict private
    FMaxActions: Integer;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorUndoOptions;
    procedure DoChange;
    procedure SetMaxActions(AValue: Integer);
    procedure SetOptions(const AValue: TBCEditorUndoOptions);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property MaxActions: Integer read FMaxActions write SetMaxActions default BCEDITOR_UNDO_MAX_ACTIONS;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorUndoOptions read FOptions write SetOptions default [uoGroupUndo];
  end;

implementation

constructor TBCEditorUndo.Create;
begin
  inherited;

  FMaxActions := BCEDITOR_UNDO_MAX_ACTIONS;
  FOptions := [uoGroupUndo];
end;

procedure TBCEditorUndo.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorUndo.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorUndo then
  with ASource as TBCEditorUndo do
  begin
    Self.FMaxActions := FMaxActions;
    Self.FOptions := FOptions;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorUndo.SetMaxActions(AValue: Integer);
begin
  if FMaxActions <> AValue then
  begin
    FMaxActions := AValue;
    DoChange;
  end;
end;

procedure TBCEditorUndo.SetOptions(const AValue: TBCEditorUndoOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange;
  end;
end;

end.
