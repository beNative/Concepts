unit BCEditor.Editor.Undo.List;

interface

uses
  System.Classes, BCEditor.Editor.Undo.Item, BCEditor.Types, BCEditor.Consts;

type
  TBCEditorUndoList = class(TPersistent)
  protected
    FBlockCount: Integer;
    FBlockNumber: Integer;
    FChangeBlockNumber: Integer;
    FInsideRedo: Boolean;
    FInsideUndoBlock: Boolean;
    FItems: TList;
    FLockCount: Integer;
    FOnAddedUndo: TNotifyEvent;
    function GetCanUndo: Boolean;
    function GetItemCount: Integer;
    function GetItems(AIndex: Integer): TBCEditorUndoItem;
    procedure SetItems(AIndex: Integer; const AValue: TBCEditorUndoItem);
  public
    constructor Create;
    destructor Destroy; override;

    function PeekItem: TBCEditorUndoItem;
    function PopItem: TBCEditorUndoItem;
    function LastChangeBlockNumber: Integer;
    function LastChangeReason: TBCEditorChangeReason;
    function LastChangeString: string;
    procedure AddChange(AReason: TBCEditorChangeReason; const ACaretPosition, ASelectionBeginPosition, ASelectionEndPosition: TBCEditorTextPosition;
      const ChangeText: string; SelectionMode: TBCEditorSelectionMode; AChangeBlockNumber: Integer = 0);
    procedure BeginBlock(AChangeBlockNumber: Integer = 0);
    procedure Clear;
    procedure EndBlock;
    procedure Lock;
    procedure PushItem(AItem: TBCEditorUndoItem);
    procedure Unlock;
  public
    procedure AddGroupBreak;
    procedure Assign(ASource: TPersistent); override;
    property BlockCount: Integer read FBlockCount;
    property CanUndo: Boolean read GetCanUndo;
    property InsideRedo: Boolean read FInsideRedo write FInsideRedo default False;
    property InsideUndoBlock: Boolean read FInsideUndoBlock write FInsideUndoBlock default False;
    property ItemCount: Integer read GetItemCount;
    property Items[AIndex: Integer]: TBCEditorUndoItem read GetItems write SetItems;
    property OnAddedUndo: TNotifyEvent read FOnAddedUndo write FOnAddedUndo;
  end;

implementation

{ TBCEditorUndoList }

constructor TBCEditorUndoList.Create;
begin
  inherited;

  FItems := TList.Create;
  FInsideRedo := False;
  FInsideUndoBlock := False;
  FBlockNumber := BCEDITOR_UNDO_BLOCK_NUMBER_START;
end;

destructor TBCEditorUndoList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TBCEditorUndoList.Assign(ASource: TPersistent);
var
  i: Integer;
  LUndoItem: TBCEditorUndoItem;
begin
  if Assigned(ASource) and (ASource is TBCEditorUndoList) then
  with ASource as TBCEditorUndoList do
  begin
    Self.Clear;
    for i := 0 to (ASource as TBCEditorUndoList).FItems.Count - 1 do
    begin
      LUndoItem := TBCEditorUndoItem.Create;
      LUndoItem.Assign(FItems[i]);
      Self.FItems.Add(LUndoItem);
    end;
    Self.FInsideUndoBlock := FInsideUndoBlock;
    Self.FBlockCount := FBlockCount;
    Self.FChangeBlockNumber := FChangeBlockNumber;
    Self.FLockCount := FLockCount;
    Self.FInsideRedo := FInsideRedo;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorUndoList.AddChange(AReason: TBCEditorChangeReason;
  const ACaretPosition, ASelectionBeginPosition, ASelectionEndPosition: TBCEditorTextPosition;
  const ChangeText: string; SelectionMode: TBCEditorSelectionMode; AChangeBlockNumber: Integer = 0);
var
  LNewItem: TBCEditorUndoItem;
begin
  if FLockCount = 0 then
  begin
    LNewItem := TBCEditorUndoItem.Create;
    with LNewItem do
    begin
      if AChangeBlockNumber <> 0 then
        ChangeBlockNumber := AChangeBlockNumber
      else
      if FInsideUndoBlock then
        ChangeBlockNumber := FChangeBlockNumber
      else
        ChangeBlockNumber := 0;
      ChangeReason := AReason;
      ChangeSelectionMode := SelectionMode;
      ChangeCaretPosition := ACaretPosition;
      ChangeBeginPosition := ASelectionBeginPosition;
      ChangeEndPosition := ASelectionEndPosition;
      ChangeString := ChangeText;
    end;
    PushItem(LNewItem);
  end;
end;

procedure TBCEditorUndoList.BeginBlock(AChangeBlockNumber: Integer = 0);
begin
  Inc(FBlockCount);
  if AChangeBlockNumber = 0 then
  begin
    Inc(FBlockNumber);
    FChangeBlockNumber := FBlockNumber;
  end
  else
    FChangeBlockNumber := AChangeBlockNumber;
  FInsideUndoBlock := True;
end;

procedure TBCEditorUndoList.Clear;
var
  i: Integer;
begin
  FBlockCount := 0;
  for i := 0 to FItems.Count - 1 do
    TBCEditorUndoItem(FItems[i]).Free;
  FItems.Clear;
end;

procedure TBCEditorUndoList.EndBlock;
begin
  Assert(FBlockCount > 0);
  Dec(FBlockCount);
  FInsideUndoBlock := False;
end;

function TBCEditorUndoList.GetCanUndo: Boolean;
begin
  Result := FItems.Count > 0;
end;

function TBCEditorUndoList.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TBCEditorUndoList.Lock;
begin
  Inc(FLockCount);
end;

function TBCEditorUndoList.PeekItem: TBCEditorUndoItem;
var
  i: Integer;
begin
  Result := nil;
  i := FItems.Count - 1;
  if i >= 0 then
    Result := FItems[i];
end;

function TBCEditorUndoList.PopItem: TBCEditorUndoItem;
var
  i: Integer;
begin
  Result := nil;
  i := FItems.Count - 1;
  if i >= 0 then
  begin
    Result := FItems[i];
    FItems.Delete(i);
  end;
end;

procedure TBCEditorUndoList.PushItem(AItem: TBCEditorUndoItem);
begin
  if Assigned(AItem) then
  begin
    FItems.Add(AItem);
    if (AItem.ChangeReason <> crGroupBreak) and Assigned(OnAddedUndo) then
      OnAddedUndo(Self);
  end;
end;

procedure TBCEditorUndoList.Unlock;
begin
  if FLockCount > 0 then
    Dec(FLockCount);
end;

function TBCEditorUndoList.LastChangeReason: TBCEditorChangeReason;
begin
  if FItems.Count = 0 then
    Result := crNothing
  else
    Result := TBCEditorUndoItem(FItems[FItems.Count - 1]).ChangeReason;
end;

function TBCEditorUndoList.LastChangeBlockNumber: Integer;
begin
  if FItems.Count = 0 then
    Result := 0
  else
    Result := TBCEditorUndoItem(FItems[FItems.Count - 1]).ChangeBlockNumber;
end;

function TBCEditorUndoList.LastChangeString: string;
begin
  if FItems.Count = 0 then
    Result := ''
  else
    Result := TBCEditorUndoItem(FItems[FItems.Count - 1]).ChangeString;
end;

procedure TBCEditorUndoList.AddGroupBreak;
var
  vDummy: TBCEditorTextPosition;
begin
  if LastChangeReason <> crGroupBreak then
    AddChange(crGroupBreak, vDummy, vDummy, vDummy, '', smNormal);
end;

function TBCEditorUndoList.GetItems(AIndex: Integer): TBCEditorUndoItem;
begin
  Result := TBCEditorUndoItem(FItems[AIndex]);
end;

procedure TBCEditorUndoList.SetItems(AIndex: Integer; const AValue: TBCEditorUndoItem);
begin
  FItems[AIndex] := AValue;
end;

{procedure TBCEditorUndoList.DeleteItem(AIndex: Integer);
begin
  TBCEditorUndoItem(FItems[AIndex]).Free;
  FItems.Delete(AIndex);
end; }

end.
