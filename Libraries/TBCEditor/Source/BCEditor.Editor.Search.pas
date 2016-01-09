unit BCEditor.Editor.Search;

interface

uses
  System.Classes, Vcl.Controls, BCEditor.Editor.Search.Map, BCEditor.Types,
  BCEditor.Editor.Search.Highlighter;

const
  BCEDITOR_SEARCH_OPTIONS = [soHighlightResults, soSearchOnTyping, soBeepIfStringNotFound, soShowSearchMatchNotFound];

type
  TBCEditorSearch = class(TPersistent)
  strict private
    FEnabled: Boolean;
    FEngine: TBCEditorSearchEngine;
    FHighlighter: TBCEditorSearchHighlighter;
    FMap: TBCEditorSearchMap;
    FOnChange: TBCEditorSearchChangeEvent;
    FOptions: TBCEditorSearchOptions;
    FSearchText: string;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetEngine(const AValue: TBCEditorSearchEngine);
    procedure SetHighlighter(const AValue: TBCEditorSearchHighlighter);
    procedure SetMap(const AValue: TBCEditorSearchMap);
    procedure SetOnChange(const AValue: TBCEditorSearchChangeEvent);
    procedure SetSearchText(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    property Visible: Boolean read FVisible write FVisible;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Engine: TBCEditorSearchEngine read FEngine write SetEngine default seNormal;
    property Highlighter: TBCEditorSearchHighlighter read FHighlighter write SetHighlighter;
    property Map: TBCEditorSearchMap read FMap write SetMap;
    property OnChange: TBCEditorSearchChangeEvent read FOnChange write SetOnChange;
    property Options: TBCEditorSearchOptions read FOptions write FOptions default BCEDITOR_SEARCH_OPTIONS;
    property SearchText: string read FSearchText write SetSearchText;
  end;

implementation

{ TBCEditorSearchPanel }

constructor TBCEditorSearch.Create;
begin
  inherited;

  FSearchText := '';
  FEngine := seNormal;
  FMap := TBCEditorSearchMap.Create;
  FHighlighter := TBCEditorSearchHighlighter.Create;
  FOptions := BCEDITOR_SEARCH_OPTIONS;
  FEnabled := True;
end;

destructor TBCEditorSearch.Destroy;
begin
  FMap.Free;
  FHighlighter.Free;
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

procedure TBCEditorSearch.SetOnChange(const AValue: TBCEditorSearchChangeEvent);
begin
  FOnChange := AValue;
  FMap.OnChange := FOnChange;
  FHighlighter.OnChange := FOnChange;
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

procedure TBCEditorSearch.SetMap(const AValue: TBCEditorSearchMap);
begin
  FMap.Assign(AValue);
end;

end.
