unit BCEditor.Editor.Undo.List;

interface

uses
  System.Classes, BCEditor.Editor.Undo.Item, BCEditor.Types, BCEditor.Consts;

type
  TBCEditorUndoList = class(TPersistent)
  protected
    FBlockCount: Integer;
    FFullUndoImposible: Boolean;
    FInsideRedo: Boolean;
    FInsideUndoBlock: Boolean;
    FItems: TList;
    FLockCount: Integer;
    FMaxUndoActions: Integer;
    FOnAddedUndo: TNotifyEvent;
    function GetCanUndo: Boolean;
    function GetItemCount: Integer;
    function GetItems(AIndex: Integer): TBCEditorUndoItem;
    procedure EnsureMaxEntries;
    procedure SetMaxUndoActions(AValue: Integer);
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
    procedure BeginBlock;
    procedure Clear;
    procedure EndBlock;
    procedure Lock;
    procedure PushItem(AItem: TBCEditorUndoItem);
    procedure Unlock;
  public
    procedure AddGroupBreak;
    procedure Assign(ASource: TPersistent); override;
    procedure DeleteItem(AIndex: Integer);
    property BlockCount: Integer read FBlockCount;
    property CanUndo: Boolean read GetCanUndo;
    property FullUndoImpossible: Boolean read FFullUndoImposible;
    property InsideRedo: Boolean read FInsideRedo write FInsideRedo default False;
    property InsideUndoBlock: Boolean read FInsideUndoBlock write FInsideUndoBlock default False;
    property ItemCount: Integer read GetItemCount;
    property Items[AIndex: Integer]: TBCEditorUndoItem read GetItems write SetItems;
    property MaxUndoActions: Integer read FMaxUndoActions write SetMaxUndoActions default BCEDITOR_MAX_UNDO_ACTIONS;
    property OnAddedUndo: TNotifyEvent read FOnAddedUndo write FOnAddedUndo;
  end;

implementation

{ TBCEditorUndoList }

constructor TBCEditorUndoList.Create;
begin
  inherited;

  FItems := TList.Create;
  FMaxUndoActions := BCEDITOR_MAX_UNDO_ACTIONS;
  FInsideRedo := False;
  FInsideUndoBlock := False;
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
    Self.FFullUndoImposible := FFullUndoImposible;
    Self.FLockCount := FLockCount;
    Self.FMaxUndoActions := FMaxUndoActions;
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
        ChangeBlockNumber := FBlockCount
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

procedure TBCEditorUndoList.BeginBlock;
begin
  Inc(FBlockCount);
  FInsideUndoBlock := True;
end;

procedure TBCEditorUndoList.Clear;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    TBCEditorUndoItem(FItems[i]).Free;
  FItems.Clear;
  FFullUndoImposible := False;
end;

procedure TBCEditorUndoList.EndBlock;
begin
  Assert(FBlockCount > 0);
  Dec(FBlockCount);
  FInsideUndoBlock := False;
end;

procedure TBCEditorUndoList.EnsureMaxEntries;
var
  LItem: TBCEditorUndoItem;
begin
  if FItems.Count > FMaxUndoActions then
  begin
    FFullUndoImposible := True;
    while FItems.Count > FMaxUndoActions do
    begin
      LItem := FItems[0];
      LItem.Free;
      FItems.Delete(0);
    end;
  end;
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
    EnsureMaxEntries;
    if (AItem.ChangeReason <> crGroupBreak) and Assigned(OnAddedUndo) then
      OnAddedUndo(Self);
  end;
end;

procedure TBCEditorUndoList.SetMaxUndoActions(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FMaxUndoActions <> AValue then
  begin
    FMaxUndoActions := AValue;
    EnsureMaxEntries;
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

procedure TBCEditorUndoList.DeleteItem(AIndex: Integer);
begin
  TBCEditorUndoItem(FItems[AIndex]).Free;
  FItems.Delete(AIndex);
end;

end.
