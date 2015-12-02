unit BCEditor.Editor.MatchingPair;

interface

uses
  System.Classes, BCEditor.Editor.MatchingPair.Colors, BCEditor.Types;

type
  TBCEditorMatchingPair = class(TPersistent)
  strict private
    FColors: TBCEditorMatchingPairColors;
    FEnabled: Boolean;
    FOptions: TBCEditorMatchingPairOptions;
    procedure SetColors(const AValue: TBCEditorMatchingPairColors);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Colors: TBCEditorMatchingPairColors read FColors write SetColors;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Options: TBCEditorMatchingPairOptions read FOptions write FOptions default [mpoHighlightAfterToken, mpoUseMatchedColor];
  end;

implementation

{ TBCEditorMatchingPair }

constructor TBCEditorMatchingPair.Create;
begin
  inherited;

  FColors := TBCEditorMatchingPairColors.Create;
  FEnabled := True;
  FOptions := [mpoHighlightAfterToken, mpoUseMatchedColor];
end;

destructor TBCEditorMatchingPair.Destroy;
begin
  FColors.Free;

  inherited;
end;

procedure TBCEditorMatchingPair.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorMatchingPair) then
  with ASource as TBCEditorMatchingPair do
  begin
    Self.FEnabled := FEnabled;
    Self.FColors.Assign(FColors);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorMatchingPair.SetColors(const AValue: TBCEditorMatchingPairColors);
begin
  FColors.Assign(AValue);
end;

end.
