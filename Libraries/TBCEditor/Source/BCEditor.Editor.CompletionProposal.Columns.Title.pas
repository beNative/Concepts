unit BCEditor.Editor.CompletionProposal.Columns.Title;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Editor.CompletionProposal.Columns.Title.Colors;

type
  TBCEditorCompletionProposalColumnTitle = class(TPersistent)
  strict private
    FCaption: string;
    FColors: TBCEditorCompletionProposalColumnTitleColors;
    FFont: TFont;
    FVisible: Boolean;
    procedure SetFont(const AValue: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Caption: string read FCaption write FCaption;
    property Colors: TBCEditorCompletionProposalColumnTitleColors read FColors write FColors;
    property Font: TFont read FFont write SetFont;
    property Visible: Boolean read FVisible write FVisible default False;
  end;

implementation

constructor TBCEditorCompletionProposalColumnTitle.Create;
begin
  inherited;

  FColors := TBCEditorCompletionProposalColumnTitleColors.Create;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FVisible := False;
end;

destructor TBCEditorCompletionProposalColumnTitle.Destroy;
begin
  FColors.Free;
  FFont.Free;

  inherited;
end;

procedure TBCEditorCompletionProposalColumnTitle.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposalColumnTitle then
  with ASource as TBCEditorCompletionProposalColumnTitle do
  begin
    Self.FCaption := FCaption;
    Self.FColors.Assign(FColors);
    Self.FFont.Assign(FFont);
    Self.FVisible := FVisible;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCompletionProposalColumnTitle.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

end.
