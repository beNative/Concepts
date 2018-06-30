unit BCEditor.Editor.CodeFolding.Ranges;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, BCEditor.Editor.CodeFolding.Regions;

type
  TBCEditorCodeFoldingRange = class;
  TBCEditorAllCodeFoldingRanges = class;

  TBCEditorCodeFoldingRanges = class(TPersistent)
  strict private
    FList: TList;
    function GetItem(AIndex: Integer): TBCEditorCodeFoldingRange;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(AAllCodeFoldingRanges: TBCEditorAllCodeFoldingRanges; AFromLine, AIndentLevel, AFoldRangeLevel: Integer;
      ARegionItem: TBCEditorCodeFoldingRegionItem; AToLine: Integer = 0): TBCEditorCodeFoldingRange;
    procedure Clear;

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TBCEditorCodeFoldingRange read GetItem; default;
  end;

  TBCEditorAllCodeFoldingRanges = class(TBCEditorCodeFoldingRanges)
  strict private
    FList: TList;
    function GetAllCount: Integer;
    function GetItem(AIndex: Integer): TBCEditorCodeFoldingRange;
    procedure SetItem(AIndex: Integer; Value: TBCEditorCodeFoldingRange);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ClearAll;
    procedure Delete(FoldRange: TBCEditorCodeFoldingRange); overload;
    procedure Delete(AIndex: Integer); overload;
    procedure UpdateFoldRanges;
    procedure SetParentCollapsedOfSubCodeFoldingRanges(AFoldRange: TBCEditorCodeFoldingRange);

    property AllCount: Integer read GetAllCount;
    property Items[AIndex: Integer]: TBCEditorCodeFoldingRange read GetItem write SetItem; default;
    property List: TList read FList;
  end;

  TBCEditorCodeFoldingRange = class
  strict private
    FAllCodeFoldingRanges: TBCEditorAllCodeFoldingRanges;
    FCollapsed: Boolean;
    FCollapsedBy: Integer;
    FCollapseMarkRect: TRect;
    FFoldRangeLevel: Integer;
    FFromLine: Integer;
    FIndentLevel: Integer;
    FIsExtraTokenFound: Boolean;
    FParentCollapsed: Boolean;
    FRegionItem: TBCEditorCodeFoldingRegionItem;
    FSubCodeFoldingRanges: TBCEditorCodeFoldingRanges;
    FToLine: Integer;
    FUndoListed: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Collapsable: Boolean;
    procedure MoveBy(LineCount: Integer);
    procedure MoveChildren(By: Integer);
    procedure SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed: Boolean; ACollapsedBy: Integer);
    procedure Widen(LineCount: Integer);
    property AllCodeFoldingRanges: TBCEditorAllCodeFoldingRanges read FAllCodeFoldingRanges write FAllCodeFoldingRanges;
    property Collapsed: Boolean read FCollapsed write FCollapsed default False;
    property CollapsedBy: Integer read FCollapsedBy write FCollapsedBy;
    property CollapseMarkRect: TRect read FCollapseMarkRect write FCollapseMarkRect;
    property FoldRangeLevel: Integer read FFoldRangeLevel write FFoldRangeLevel;
    property FromLine: Integer read FFromLine write FFromLine;
    property IndentLevel: Integer read FIndentLevel write FIndentLevel;
    property IsExtraTokenFound: Boolean read FIsExtraTokenFound write FIsExtraTokenFound default False;
    property ParentCollapsed: Boolean read FParentCollapsed write FParentCollapsed;
    property RegionItem: TBCEditorCodeFoldingRegionItem read FRegionItem write FRegionItem;
    property SubCodeFoldingRanges: TBCEditorCodeFoldingRanges read FSubCodeFoldingRanges;
    property ToLine: Integer read FToLine write FToLine;
    property UndoListed: Boolean read FUndoListed write FUndoListed default False;
  end;

implementation

uses
  BCEditor.Utils;

{ TBCEditorAllCodeFoldingRanges }

constructor TBCEditorAllCodeFoldingRanges.Create;
begin
  inherited;

  FList := TList.Create;
end;

destructor TBCEditorAllCodeFoldingRanges.Destroy;
begin
  FreeList(FList);

  inherited;
end;

procedure TBCEditorAllCodeFoldingRanges.ClearAll;
begin
  Clear;
  ClearList(FList);
end;

procedure TBCEditorAllCodeFoldingRanges.Delete(FoldRange: TBCEditorCodeFoldingRange);
var
  LIndex: Integer;
begin
  for LIndex := 0 to FList.Count - 1 do
  if FList[LIndex] = FoldRange then
  begin
    TBCEditorCodeFoldingRange(FList[LIndex]).Free;
    FList[LIndex] := nil;
    FList.Delete(LIndex);
    Break;
  end;
end;

procedure TBCEditorAllCodeFoldingRanges.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

function TBCEditorAllCodeFoldingRanges.GetAllCount: Integer;
begin
  Result := FList.Count;
end;

function TBCEditorAllCodeFoldingRanges.GetItem(AIndex: Integer): TBCEditorCodeFoldingRange;
begin
  if Cardinal(AIndex) < Cardinal(FList.Count) then
    Result := FList.List[AIndex]
  else
    Result := nil;
end;

procedure TBCEditorAllCodeFoldingRanges.SetItem(AIndex: Integer; Value: TBCEditorCodeFoldingRange);
begin
  FList[AIndex] := Value;
end;

procedure TBCEditorAllCodeFoldingRanges.SetParentCollapsedOfSubCodeFoldingRanges(AFoldRange: TBCEditorCodeFoldingRange);
var
	LIndex: Integer;
  LFoldRange: TBCEditorCodeFoldingRange;
begin
  for LIndex := 0 to AllCount - 1 do
  begin
    LFoldRange := GetItem(LIndex);
    if LFoldRange = AFoldRange then
      Continue;
    if LFoldRange.FromLine > AFoldRange.ToLine then
      Break;
    if (LFoldRange.FromLine > AFoldRange.FromLine) and (LFoldRange.FromLine <> AFoldRange.ToLine) then
      LFoldRange.ParentCollapsed := True;
  end;
end;

procedure TBCEditorAllCodeFoldingRanges.UpdateFoldRanges;
var
  LIndex: Integer;
  LFoldRange: TBCEditorCodeFoldingRange;
begin
  for LIndex := 0 to AllCount - 1 do
  begin
    LFoldRange := GetItem(LIndex);
    if Assigned(LFoldRange) then
      LFoldRange.ParentCollapsed := False;
  end;
  for LIndex := 0 to AllCount - 1 do
  begin
    LFoldRange := GetItem(LIndex);
    if Assigned(LFoldRange) and not LFoldRange.ParentCollapsed then
      SetParentCollapsedOfSubCodeFoldingRanges(LFoldRange);
  end;
end;

{ TBCEditorCodeFoldingRanges }

constructor TBCEditorCodeFoldingRanges.Create;
begin
  inherited;

  FList := TList.Create;
end;

destructor TBCEditorCodeFoldingRanges.Destroy;
begin
  FList.Clear;
  FList.Free;
  FList := nil;

  inherited;
end;

function TBCEditorCodeFoldingRanges.Add(AAllCodeFoldingRanges: TBCEditorAllCodeFoldingRanges; AFromLine, AIndentLevel, AFoldRangeLevel: Integer;
  ARegionItem: TBCEditorCodeFoldingRegionItem; AToLine: Integer): TBCEditorCodeFoldingRange;
begin
  Result := TBCEditorCodeFoldingRange.Create;
  with Result do
  begin
    FromLine := AFromLine;
    ToLine := AToLine;
    IndentLevel := AIndentLevel;
    FoldRangeLevel := AFoldRangeLevel;
    AllCodeFoldingRanges := AAllCodeFoldingRanges;
    RegionItem := ARegionItem;
  end;
  FList.Add(Result);
  AAllCodeFoldingRanges.List.Add(Result);
end;

procedure TBCEditorCodeFoldingRanges.Clear;
begin
  FList.Clear;
end;

function TBCEditorCodeFoldingRanges.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBCEditorCodeFoldingRanges.GetItem(AIndex: Integer): TBCEditorCodeFoldingRange;
begin
  Result := FList[AIndex];
end;

{ TBCEditorCodeFoldingRange }

function TBCEditorCodeFoldingRange.Collapsable: Boolean;
begin
  Result := (FFromLine < FToLine) or RegionItem.TokenEndIsPreviousLine and (FFromLine = FToLine);
end;

constructor TBCEditorCodeFoldingRange.Create;
begin
  inherited;

  FSubCodeFoldingRanges := TBCEditorCodeFoldingRanges.Create;
  FCollapsed := False;
  FCollapsedBy := -1;
  FIsExtraTokenFound := False;
  FUndoListed := False;
end;

destructor TBCEditorCodeFoldingRange.Destroy;
begin;
  FSubCodeFoldingRanges.Clear;
  FSubCodeFoldingRanges.Free;
  FSubCodeFoldingRanges := nil;

  inherited;
end;

procedure TBCEditorCodeFoldingRange.MoveBy(LineCount: Integer);
begin
  Inc(FFromLine, LineCount);
  Inc(FToLine, LineCount);
end;

procedure TBCEditorCodeFoldingRange.MoveChildren(By: Integer);
var
  LIndex: Integer;
  LCodeFoldingRange: TBCEditorCodeFoldingRange;
begin
  for LIndex := 0 to FSubCodeFoldingRanges.Count - 1 do
  begin
    LCodeFoldingRange := FSubCodeFoldingRanges[LIndex];
    if Assigned(LCodeFoldingRange) then
    begin
      LCodeFoldingRange.MoveChildren(By);

      with FAllCodeFoldingRanges.List do
      if LCodeFoldingRange.FParentCollapsed then
        Move(IndexOf(LCodeFoldingRange), IndexOf(LCodeFoldingRange) + By);
    end;
  end;
end;

procedure TBCEditorCodeFoldingRange.SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed: Boolean; ACollapsedBy: Integer);
var
  LIndex: Integer;
  LCodeFoldingRange: TBCEditorCodeFoldingRange;
begin
  if Assigned(FSubCodeFoldingRanges) then
  for LIndex := 0 to FSubCodeFoldingRanges.Count - 1 do
  begin
    LCodeFoldingRange := FSubCodeFoldingRanges[LIndex];
    LCodeFoldingRange.SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed, ACollapsedBy);

    if (LCodeFoldingRange.FCollapsedBy = -1) or (LCodeFoldingRange.FCollapsedBy = ACollapsedBy) then
    begin
      LCodeFoldingRange.FParentCollapsed := AParentCollapsed;

      if not AParentCollapsed then
        LCodeFoldingRange.FCollapsedBy := -1
      else
        LCodeFoldingRange.FCollapsedBy := ACollapsedBy;
    end;
  end;
end;

procedure TBCEditorCodeFoldingRange.Widen(LineCount: Integer);
begin
  Inc(FToLine, LineCount);
end;

end.
