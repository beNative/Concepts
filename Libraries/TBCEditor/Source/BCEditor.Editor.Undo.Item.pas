unit BCEditor.Editor.Undo.Item;

interface

uses
  System.Classes, BCEditor.Types;

type
  TBCEditorUndoItem = class(TPersistent)
  protected
    FChangeBlockNumber: Integer;
    FChangeCaretPosition: TBCEditorTextPosition;
    FChangeData: Pointer;
    FChangeEndPosition: TBCEditorTextPosition;
    FChangeReason: TBCEditorChangeReason;
    FChangeSelectionMode: TBCEditorSelectionMode;
    FChangeBeginPosition: TBCEditorTextPosition;
    FChangeString: string;
  public
    procedure Assign(ASource: TPersistent); override;

    property ChangeBlockNumber: Integer read FChangeBlockNumber write FChangeBlockNumber;
    property ChangeCaretPosition: TBCEditorTextPosition read FChangeCaretPosition write FChangeCaretPosition;
    property ChangeData: Pointer read FChangeData write FChangeData;
    property ChangeEndPosition: TBCEditorTextPosition read FChangeEndPosition write FChangeEndPosition;
    property ChangeReason: TBCEditorChangeReason read FChangeReason write FChangeReason;
    property ChangeSelectionMode: TBCEditorSelectionMode read FChangeSelectionMode write FChangeSelectionMode;
    property ChangeBeginPosition: TBCEditorTextPosition read FChangeBeginPosition write FChangeBeginPosition;
    property ChangeString: string read FChangeString write FChangeString;
  end;

implementation

{ TBCEditorUndoItem }

procedure TBCEditorUndoItem.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorUndoItem) then
  with ASource as TBCEditorUndoItem do
  begin
    Self.FChangeBlockNumber := FChangeBlockNumber;
    Self.FChangeCaretPosition := FChangeCaretPosition;
    Self.FChangeData := FChangeData;
    Self.FChangeReason := FChangeReason;
    Self.FChangeSelectionMode := FChangeSelectionMode;
    Self.FChangeBeginPosition := FChangeBeginPosition;
    Self.FChangeEndPosition := FChangeEndPosition;
    Self.FChangeString := FChangeString;
  end
  else
    inherited Assign(ASource);
end;

end.
