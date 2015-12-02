unit BCEditor.Editor.CompletionProposal.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorCompletionProposalColors = class(TPersistent)
  strict private
    FBackground: TColor;
    FForeground: TColor;
    FSelectedBackground: TColor;
    FSelectedText: TColor;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Background: TColor read FBackground write FBackground default clWindow;
    property Foreground: TColor read FForeground write FForeground default clWindowText;
    property SelectedBackground: TColor read FSelectedBackground write FSelectedBackground default clHighlight;
    property SelectedText: TColor read FSelectedText write FSelectedText default clHighlightText;
  end;

implementation

{ TBCEditorCompletionProposalColors }

constructor TBCEditorCompletionProposalColors.Create;
begin
  inherited;

  FBackground := clWindow;
  FForeground := clWindowText;
  FSelectedBackground := clHighlight;
  FSelectedText := clHighlightText;
end;

procedure TBCEditorCompletionProposalColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposalColors then
  with ASource as TBCEditorCompletionProposalColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
    Self.FSelectedBackground := FSelectedBackground;
    Self.FSelectedText := FSelectedText;
  end
  else
    inherited Assign(ASource);
end;

end.
