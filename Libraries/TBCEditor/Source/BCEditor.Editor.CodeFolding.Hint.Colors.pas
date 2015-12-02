unit BCEditor.Editor.CodeFolding.Hint.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorCodeFoldingHintColors = class(TPersistent)
  strict private
    FBackground: TColor;
    FBorder: TColor;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Background: TColor read FBackground write FBackground default clWindow;
    property Border: TColor read FBorder write FBorder default clBtnFace;
  end;

implementation

{ TBCEditorCompletionProposalColors }

constructor TBCEditorCodeFoldingHintColors.Create;
begin
  inherited;

  FBackground := clWindow;
  FBorder := clBtnFace;
end;

procedure TBCEditorCodeFoldingHintColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCodeFoldingHintColors then
  with ASource as TBCEditorCodeFoldingHintColors do
  begin
    Self.FBackground := FBackground;
    Self.FBorder := FBorder;
  end
  else
    inherited Assign(ASource);
end;

end.
