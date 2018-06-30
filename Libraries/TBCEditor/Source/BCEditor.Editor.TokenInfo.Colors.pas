unit BCEditor.Editor.TokenInfo.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorTokenInfoColors = class(TPersistent)
  strict private
    FBackground: TColor;
    FReference: TColor;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Background: TColor read FBackground write FBackground default clWindow;
    property Reference: TColor read FReference write FReference default clBlue;
  end;

implementation

constructor TBCEditorTokenInfoColors.Create;
begin
  inherited;

  FBackground := clWindow;
  FReference := clBlue;
end;

procedure TBCEditorTokenInfoColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorTokenInfoColors then
  with ASource as TBCEditorTokenInfoColors do
  begin
    Self.FBackground := FBackground;
    Self.FReference := FReference;
  end
  else
    inherited Assign(ASource);
end;

end.
