unit BCEditor.Editor.CodeFolding.Hint.Indicator;

interface

uses
  System.Classes, Vcl.Controls, BCEditor.Editor.CodeFolding.Hint.Indicator.Colors, BCEditor.Editor.Glyph,
  BCEditor.Types;

const
  BCEDITOR_CODE_FOLDING_HINT_INDICATOR_DEFAULT_OPTIONS = [hioShowBorder, hioShowMark];

type
  TBCEditorCodeFoldingHintIndicator = class(TPersistent)
  strict private
    FColors: TBCEditorCodeFoldingHintIndicatorColors;
    FGlyph: TBCEditorGlyph;
    FMarkStyle: TBCEditorCodeFoldingHintIndicatorMarkStyle;
    FOptions: TBCEditorCodeFoldingHintIndicatorOptions;
    FPadding: TBCEditorCodeFoldingHintIndicatorPadding;
    FVisible: Boolean;
    FWidth: Integer;
    procedure SetGlyph(const AValue: TBCEditorGlyph);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Colors: TBCEditorCodeFoldingHintIndicatorColors read FColors write FColors;
    property Glyph: TBCEditorGlyph read FGlyph write SetGlyph;
    property MarkStyle: TBCEditorCodeFoldingHintIndicatorMarkStyle read FMarkStyle write FMarkStyle default imsThreeDots;
    property Options: TBCEditorCodeFoldingHintIndicatorOptions read FOptions write FOptions default BCEDITOR_CODE_FOLDING_HINT_INDICATOR_DEFAULT_OPTIONS;
    property Padding: TBCEditorCodeFoldingHintIndicatorPadding read FPadding write FPadding;
    property Visible: Boolean read FVisible write FVisible default True;
    property Width: Integer read FWidth write FWidth default 26;
  end;

implementation

constructor TBCEditorCodeFoldingHintIndicator.Create;
begin
  inherited;

  FColors := TBCEditorCodeFoldingHintIndicatorColors.Create;
  FGlyph := TBCEditorGlyph.Create;
  FPadding := TBCEditorCodeFoldingHintIndicatorPadding.Create(nil);
  FGlyph.Visible := False;
  FMarkStyle := imsThreeDots;
  FVisible := True;
  FOptions := BCEDITOR_CODE_FOLDING_HINT_INDICATOR_DEFAULT_OPTIONS;
  FWidth := 26;
end;

destructor TBCEditorCodeFoldingHintIndicator.Destroy;
begin
  FColors.Free;
  FGlyph.Free;
  FPadding.Free;

  inherited;
end;

procedure TBCEditorCodeFoldingHintIndicator.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCodeFoldingHintIndicator) then
  with ASource as TBCEditorCodeFoldingHintIndicator do
  begin
    Self.FVisible := FVisible;
    Self.FMarkStyle := FMarkStyle;
    Self.FWidth := FWidth;
    Self.FColors.Assign(FColors);
    Self.FGlyph.Assign(FGlyph);
    Self.FPadding.Assign(FPadding);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCodeFoldingHintIndicator.SetGlyph(const AValue: TBCEditorGlyph);
begin
  FGlyph.Assign(AValue);
end;

end.
