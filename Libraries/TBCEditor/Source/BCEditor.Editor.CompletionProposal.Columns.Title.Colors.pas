unit BCEditor.Editor.CompletionProposal.Columns.Title.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorCompletionProposalColumnTitleColors = class(TPersistent)
  strict private
    FBackground: TColor;
    FBottomBorder: TColor;
    FRightBorder: TColor;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Background: TColor read FBackground write FBackground default clWindow;
    property BottomBorder: TColor read FBottomBorder write FBottomBorder default clBtnFace;
    property RightBorder: TColor read FRightBorder write FRightBorder default clBtnFace;
  end;

implementation

constructor TBCEditorCompletionProposalColumnTitleColors.Create;
begin
  inherited;

  FBackground := clWindow;
  FBottomBorder := clBtnFace;
  FRightBorder := clBtnFace;
end;

procedure TBCEditorCompletionProposalColumnTitleColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposalColumnTitleColors then
  with ASource as TBCEditorCompletionProposalColumnTitleColors do
  begin
    Self.FBackground := FBackground;
    Self.FBottomBorder := FBottomBorder;
    Self.FRightBorder := FRightBorder;
  end
  else
    inherited Assign(ASource);
end;

end.
