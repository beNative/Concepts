unit BCEditor.Editor.CodeFolding.Hint.Indicator.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts;

type
  TBCEditorCodeFoldingHintIndicatorColors = class(TPersistent)
  strict private
    FBackground: TColor;
    FBorder: TColor;
    FMark: TColor;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Background: TColor read FBackground write FBackground default clLeftMarginBackground;
    property Border: TColor read FBorder write FBorder default clLeftMarginFontForeground;
    property Mark: TColor read FMark write FMark default clLeftMarginFontForeground;
  end;

implementation

constructor TBCEditorCodeFoldingHintIndicatorColors.Create;
begin
  inherited;

  FBackground := clLeftMarginBackground;
  FBorder := clLeftMarginFontForeground;
  FMark := clLeftMarginFontForeground;
end;

procedure TBCEditorCodeFoldingHintIndicatorColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCodeFoldingHintIndicatorColors then
  with ASource as TBCEditorCodeFoldingHintIndicatorColors do
  begin
    Self.FBackground := FBackground;
    Self.FBorder := FBorder;
    Self.FMark := FMark;
  end
  else
    inherited Assign(ASource);
end;

end.
