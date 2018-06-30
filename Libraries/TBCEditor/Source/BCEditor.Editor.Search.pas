unit BCEditor.Editor.Search;

interface

uses
  System.Classes, Vcl.Controls, BCEditor.Editor.Search.Map, BCEditor.Types, BCEditor.Editor.Search.Highlighter,
  BCEditor.Editor.Search.InSelection;

const
  BCEDITOR_SEARCH_OPTIONS = [soHighlightResults, soSearchOnTyping, soBeepIfStringNotFound, soShowSearchMatchNotFound];

type
  TBCEditorSearch = class(TPersistent)
  strict private
    FEnabled: Boolean;
    FEngine: TBCEditorSearchEngine;
    FHighlighter: TBCEditorSearchHighlighter;
    FInSelection: TBCEditorSearchInSelection;
    FLines: TList;
    FMap: TBCEditorSearchMap;
    FOnChange: TBCEditorSearchChangeEvent;
    FOptions: TBCEditorSearchOptions;
    FSearchText: string;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetEngine(const AValue: TBCEditorSearchEngine);
    procedure SetHighlighter(const AValue: TBCEditorSearchHighlighter);
    procedure SetInSelection(const AValue: TBCEditorSearchInSelection);
    procedure SetMap(const AValue: TBCEditorSearchMap);
    procedure SetOnChange(const AValue: TBCEditorSearchChangeEvent);
    procedure SetSearchText(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    function GetNextSearchItemIndex(const ATextPosition: TBCEditorTextPosition): Integer;
    function GetPreviousSearchItemIndex(const ATextPosition: TBCEditorTextPosition): Integer;
    procedure Assign(ASource: TPersistent); override;
    procedure ClearLines;
    procedure SetOption(const AOption: TBCEditorSearchOption; const AEnabled: Boolean);
    property Visible: Boolean read FVisible write FVisible;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Engine: TBCEditorSearchEngine read FEngine write SetEngine default seNormal;
    property Highlighter: TBCEditorSearchHighlighter read FHighlighter write SetHighlighter;
    property InSelection: TBCEditorSearchInSelection read FInSelection write SetInSelection;
    property Lines: TList read FLines write FLines;
    property Map: TBCEditorSearchMap read FMap write SetMap;
    property OnChange: TBCEditorSearchChangeEvent read FOnChange write SetOnChange;
    property Options: TBCEditorSearchOptions read FOptions write FOptions default BCEDITOR_SEARCH_OPTIONS;
    property SearchText: string read FSearchText write SetSearchText;
  end;

implementation

constructor TBCEditorSearch.Create;
begin
  inherited;

  FSearchText := '';
  FEngine := seNormal;
  FMap := TBCEditorSearchMap.Create;
  FLines := TList.Create;
  FHighlighter := TBCEditorSearchHighlighter.Create;
  FInSelection := TBCEditorSearchInSelection.Create;
  FOptions := BCEDITOR_SEARCH_OPTIONS;
  FEnabled := True;
end;

destructor TBCEditorSearch.Destroy;
begin
  FMap.Free;
  FHighlighter.Free;
  FInSelection.Free;
  ClearLines;
  FLines.Free;
  inherited;
end;

procedure TBCEditorSearch.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSearch) then
  with ASource as TBCEditorSearch do
  begin
    Self.FEnabled := FEnabled;
    Self.FSearchText := FSearchText;
    Self.FEngine := FEngine;
    Self.FOptions := FOptions;
    Self.FMap.Assign(FMap);
    Self.FHighlighter.Assign(FHighlighter);
    Self.FInSelection.Assign(FInSelection);
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(scRefresh);
end;

procedure TBCEditorSearch.SetOption(const AOption: TBCEditorSearchOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorSearch.SetOnChange(const AValue: TBCEditorSearchChangeEvent);
begin
  FOnChange := AValue;
  FMap.OnChange := FOnChange;
  FHighlighter.OnChange := FOnChange;
  FInSelection.OnChange := FOnChange;
end;

procedure TBCEditorSearch.SetEngine(const AValue: TBCEditorSearchEngine);
begin
  if FEngine <> AValue then
  begin
    FEngine := AValue;
    if Assigned(FOnChange) then
      FOnChange(scEngineUpdate);
  end;
end;

procedure TBCEditorSearch.SetSearchText(const AValue: string);
begin
  FSearchText := AValue;
  if Assigned(FOnChange) then
    FOnChange(scSearch);
end;

procedure TBCEditorSearch.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if Assigned(FOnChange) then
      FOnChange(scSearch);
  end;
end;

procedure TBCEditorSearch.SetHighlighter(const AValue: TBCEditorSearchHighlighter);
begin
  FHighlighter.Assign(AValue);
end;

procedure TBCEditorSearch.SetInSelection(const AValue: TBCEditorSearchInSelection);
begin
  FInSelection.Assign(AValue);
end;

procedure TBCEditorSearch.SetMap(const AValue: TBCEditorSearchMap);
begin
  FMap.Assign(AValue);
end;

procedure TBCEditorSearch.ClearLines;
var
  LIndex: Integer;
begin
  for LIndex := FLines.Count - 1 downto 0 do
    Dispose(PBCEditorSearchItem(FLines.Items[LIndex]));
  FLines.Clear;
end;

function TBCEditorSearch.GetPreviousSearchItemIndex(const ATextPosition: TBCEditorTextPosition): Integer;
var
  LLow, LHigh, LMiddle: Integer;
  LSearchItem: PBCEditorSearchItem;

  function IsTextPositionBetweenSearchItems: Boolean;
  var
    LNextSearchItem: PBCEditorSearchItem;
  begin
    LNextSearchItem := PBCEditorSearchItem(FLines.Items[LMiddle + 1]);

    Result :=
      ( (LSearchItem^.EndTextPosition.Line < ATextPosition.Line) or
        (LSearchItem^.EndTextPosition.Line = ATextPosition.Line) and (LSearchItem^.EndTextPosition.Char <= ATextPosition.Char) )
      and
      ( (LNextSearchItem^.EndTextPosition.Line > ATextPosition.Line) or
        (LNextSearchItem^.EndTextPosition.Line = ATextPosition.Line) and (LNextSearchItem^.EndTextPosition.Char > ATextPosition.Char) )
  end;

  function IsSearchItemGreaterThanTextPosition: Boolean;
  begin
    Result := (LSearchItem^.EndTextPosition.Line > ATextPosition.Line) or
      (LSearchItem^.EndTextPosition.Line = ATextPosition.Line) and (LSearchItem^.EndTextPosition.Char > ATextPosition.Char)
  end;

  function IsSearchItemLowerThanTextPosition: Boolean;
  begin
    Result := (LSearchItem^.EndTextPosition.Line < ATextPosition.Line) or
      (LSearchItem^.EndTextPosition.Line = ATextPosition.Line) and (LSearchItem^.EndTextPosition.Char <= ATextPosition.Char)
  end;

begin
  Result := -1;

  if FLines.Count = 0 then
    Exit;

  LSearchItem := PBCEditorSearchItem(FLines.Items[0]);
  if IsSearchItemGreaterThanTextPosition then
    Exit;

  LHigh := FLines.Count - 1;

  LSearchItem := PBCEditorSearchItem(FLines.Items[LHigh]);
  if IsSearchItemLowerThanTextPosition then
    Exit(LHigh);

  LLow := 0;
  Dec(LHigh);

  while LLow <= LHigh do
  begin
    LMiddle := (LLow + LHigh) div 2;

    LSearchItem := PBCEditorSearchItem(FLines.Items[LMiddle]);

    if IsTextPositionBetweenSearchItems then
      Exit(LMiddle)
    else
    if IsSearchItemGreaterThanTextPosition then
      LHigh := LMiddle - 1
    else
    if IsSearchItemLowerThanTextPosition then
      LLow := LMiddle + 1
  end;
end;

function TBCEditorSearch.GetNextSearchItemIndex(const ATextPosition: TBCEditorTextPosition): Integer;
var
  LLow, LHigh, LMiddle: Integer;
  LSearchItem: PBCEditorSearchItem;

  function IsTextPositionBetweenSearchItems: Boolean;
  var
    LPreviousSearchItem: PBCEditorSearchItem;
  begin
    LPreviousSearchItem := PBCEditorSearchItem(FLines.Items[LMiddle - 1]);

    Result :=
      ( (LPreviousSearchItem^.BeginTextPosition.Line < ATextPosition.Line) or
        (LPreviousSearchItem^.BeginTextPosition.Line = ATextPosition.Line) and (LPreviousSearchItem^.BeginTextPosition.Char < ATextPosition.Char) )
      and
      ( (LSearchItem^.BeginTextPosition.Line > ATextPosition.Line) or
        (LSearchItem^.BeginTextPosition.Line = ATextPosition.Line) and (LSearchItem^.BeginTextPosition.Char >= ATextPosition.Char) );
  end;

  function IsSearchItemGreaterThanTextPosition: Boolean;
  begin
    Result := (LSearchItem^.BeginTextPosition.Line > ATextPosition.Line) or
      (LSearchItem^.BeginTextPosition.Line = ATextPosition.Line) and (LSearchItem^.BeginTextPosition.Char >= ATextPosition.Char)
  end;

  function IsSearchItemLowerThanTextPosition: Boolean;
  begin
    Result := (LSearchItem^.BeginTextPosition.Line < ATextPosition.Line) or
      (LSearchItem^.BeginTextPosition.Line = ATextPosition.Line) and (LSearchItem^.BeginTextPosition.Char < ATextPosition.Char)
  end;

begin
  Result := -1;

  if FLines.Count = 0 then
    Exit;

  LSearchItem := PBCEditorSearchItem(FLines.Items[0]);
  if IsSearchItemGreaterThanTextPosition then
    Exit(0);

  LHigh := FLines.Count - 1;

  LSearchItem := PBCEditorSearchItem(FLines.Items[LHigh]);
  if IsSearchItemLowerThanTextPosition then
    Exit;

  LLow := 1;

  while LLow <= LHigh do
  begin
    LMiddle := (LLow + LHigh) div 2;

    LSearchItem := PBCEditorSearchItem(FLines.Items[LMiddle]);

    if IsTextPositionBetweenSearchItems then
      Exit(LMiddle)
    else
    if IsSearchItemGreaterThanTextPosition then
      LHigh := LMiddle - 1
    else
    if IsSearchItemLowerThanTextPosition then
      LLow := LMiddle + 1
  end;
end;

end.
