unit BCEditor.Editor.MatchingPair.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts;

type
  TBCEditorMatchingPairColors = class(TPersistent)
  strict private
    FMatched: TColor;
    FUnderline: TColor;
    FUnmatched: TColor;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Matched: TColor read FMatched write FMatched default clAqua;
    property Underline: TColor read FUnderline write FUnderline default clMatchingPairUnderline;
    property Unmatched: TColor read FUnmatched write FUnmatched default clYellow;
  end;

implementation

constructor TBCEditorMatchingPairColors.Create;
begin
  inherited;

  FMatched := clAqua;
  FUnderline := clMatchingPairUnderline;
  FUnmatched := clYellow;
end;

procedure TBCEditorMatchingPairColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorMatchingPairColors then
  with ASource as TBCEditorMatchingPairColors do
  begin
    Self.FMatched := FMatched;
    Self.FUnmatched := FUnmatched;
  end
  else
    inherited Assign(ASource);
end;

end.
