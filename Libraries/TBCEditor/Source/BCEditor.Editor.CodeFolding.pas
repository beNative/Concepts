unit BCEditor.Editor.CodeFolding;

interface

uses
  System.Classes, System.SysUtils, Vcl.Graphics, BCEditor.Types, BCEditor.Editor.CodeFolding.Colors,
  BCEditor.Editor.CodeFolding.Hint;

const
  BCEDITOR_CODE_FOLDING_DEFAULT_OPTIONS = [cfoAutoPadding, cfoAutoWidth, cfoHighlightIndentGuides, cfoHighlightMatchingPair, cfoShowIndentGuides,
    cfoShowTreeLine, cfoUncollapseByHintClick];

type
  TBCEditorCodeFolding = class(TPersistent)
  strict private
    FColors: TBCEditorCodeFoldingColors;
    FHint: TBCEditorCodeFoldingHint;
    FMarkStyle: TBCEditorCodeFoldingMarkStyle;
    FMouseOverHint: Boolean;
    FOnChange: TBCEditorCodeFoldingChangeEvent;
    FOptions: TBCEditorCodeFoldingOptions;
    FPadding: Integer;
    FWidth: Integer;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetColors(const AValue: TBCEditorCodeFoldingColors);
    procedure SetHint(AValue: TBCEditorCodeFoldingHint);
    procedure SetMarkStyle(const AValue: TBCEditorCodeFoldingMarkStyle);
    procedure SetOnChange(AValue: TBCEditorCodeFoldingChangeEvent);
    procedure SetOptions(AValue: TBCEditorCodeFoldingOptions);
    procedure SetPadding(const AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function GetWidth: Integer;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorCodeFoldingOption; const AEnabled: Boolean);
    property MouseOverHint: Boolean read FMouseOverHint write FMouseOverHint;
  published
    property Colors: TBCEditorCodeFoldingColors read FColors write SetColors;
    property Hint: TBCEditorCodeFoldingHint read FHint write SetHint;
    property MarkStyle: TBCEditorCodeFoldingMarkStyle read FMarkStyle write SetMarkStyle default msSquare;
    property OnChange: TBCEditorCodeFoldingChangeEvent read FOnChange write SetOnChange;
    property Options: TBCEditorCodeFoldingOptions read FOptions write SetOptions default BCEDITOR_CODE_FOLDING_DEFAULT_OPTIONS;
    property Padding: Integer read FPadding write SetPadding default 2;
    property Width: Integer read FWidth write SetWidth default 14;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

implementation

uses
  System.Math;

constructor TBCEditorCodeFolding.Create;
begin
  inherited;

  FVisible := False;
  FOptions := BCEDITOR_CODE_FOLDING_DEFAULT_OPTIONS;
  FMarkStyle := msSquare;
  FColors := TBCEditorCodeFoldingColors.Create;
  FHint := TBCEditorCodeFoldingHint.Create;
  FPadding := 2;
  FWidth := 14;

  FMouseOverHint := False;
end;

destructor TBCEditorCodeFolding.Destroy;
begin
  FColors.Free;
  FHint.Free;

  inherited;
end;

procedure TBCEditorCodeFolding.SetOnChange(AValue: TBCEditorCodeFoldingChangeEvent);
begin
  FOnChange := AValue;
  FColors.OnChange := AValue;
end;

procedure TBCEditorCodeFolding.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCodeFolding then
  with ASource as TBCEditorCodeFolding do
  begin
    Self.FVisible := FVisible;
    Self.FOptions := FOptions;
    Self.FColors.Assign(FColors);
    Self.FHint.Assign(FHint);
    Self.FWidth := FWidth;
    if Assigned(Self.OnChange) then
      Self.OnChange(fcRescan);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCodeFolding.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(fcRefresh);
end;

procedure TBCEditorCodeFolding.SetMarkStyle(const AValue: TBCEditorCodeFoldingMarkStyle);
begin
  if FMarkStyle <> AValue then
  begin
    FMarkStyle := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFolding.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    if Assigned(FOnChange) then
      FOnChange(fcEnabled);
  end;
end;

procedure TBCEditorCodeFolding.SetOptions(AValue: TBCEditorCodeFoldingOptions);
var
  LRescan: Boolean;
begin
  LRescan := not (cfoFoldMultilineComments in FOptions) and (cfoFoldMultilineComments in AValue) or
    (cfoFoldMultilineComments in FOptions) and not (cfoFoldMultilineComments in AValue);

  FOptions := AValue;
  if LRescan then
    FOnChange(fcRescan)
  else
    DoChange;
end;

procedure TBCEditorCodeFolding.SetOption(const AOption: TBCEditorCodeFoldingOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorCodeFolding.SetColors(const AValue: TBCEditorCodeFoldingColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorCodeFolding.SetHint(AValue: TBCEditorCodeFoldingHint);
begin
  FHint.Assign(AValue);
end;

procedure TBCEditorCodeFolding.SetPadding(const AValue: Integer);
begin
  if FPadding <> AValue then
  begin
    FPadding := AValue;
    DoChange;
  end;
end;

function TBCEditorCodeFolding.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

procedure TBCEditorCodeFolding.SetWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    DoChange;
  end;
end;

end.
