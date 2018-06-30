unit BCEditor.Editor.CompletionProposal;

interface

uses
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.ImgList, BCEditor.Editor.CompletionProposal.Colors,
  BCEditor.Editor.CompletionProposal.Columns, BCEditor.Editor.CompletionProposal.Trigger, BCEditor.Types;

const
  BCEDITOR_COMPLETION_PROPOSAL_DEFAULT_OPTIONS = [cpoAutoConstraints, cpoAddHighlighterKeywords, cpoFiltered,
    cpoParseItemsFromText, cpoUseHighlighterColumnFont];

type
  TBCEditorCompletionProposal = class(TPersistent)
  strict private
    FCloseChars: string;
    FColors: TBCEditorCompletionProposalColors;
    FColumns: TBCEditorCompletionProposalColumns;
    FCompletionColumnIndex: Integer;
    FConstraints: TSizeConstraints;
    FEnabled: Boolean;
    FImages: TCustomImageList;
    FOptions: TBCEditorCompletionProposalOptions;
    FOwner: TComponent;
    FSecondaryShortCut: TShortCut;
    FShortCut: TShortCut;
    FTrigger: TBCEditorCompletionProposalTrigger;
    FVisibleLines: Integer;
    FWidth: Integer;
    procedure SetImages(const AValue: TCustomImageList);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorCompletionProposalOption; const AEnabled: Boolean);
  published
    property CloseChars: string read FCloseChars write FCloseChars;
    property Colors: TBCEditorCompletionProposalColors read FColors write FColors;
    property Columns: TBCEditorCompletionProposalColumns read FColumns write FColumns;
    property CompletionColumnIndex: Integer read FCompletionColumnIndex write FCompletionColumnIndex default 0;
    property Constraints: TSizeConstraints read FConstraints write FConstraints;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Images: TCustomImageList read FImages write SetImages;
    property Options: TBCEditorCompletionProposalOptions read FOptions write FOptions default BCEDITOR_COMPLETION_PROPOSAL_DEFAULT_OPTIONS;
    property SecondaryShortCut: TShortCut read FSecondaryShortCut write FSecondaryShortCut;
    property ShortCut: TShortCut read FShortCut write FShortCut;
    property Trigger: TBCEditorCompletionProposalTrigger read FTrigger write FTrigger;
    property VisibleLines: Integer read FVisibleLines write FVisibleLines default 8;
    property Width: Integer read FWidth write FWidth default 260;
  end;

implementation

uses
  Vcl.Menus;

constructor TBCEditorCompletionProposal.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FCloseChars := '()[]. ';
  FColors := TBCEditorCompletionProposalColors.Create;
  FColumns := TBCEditorCompletionProposalColumns.Create(Self, TBCEditorCompletionProposalColumn);
  FColumns.Add; { default column }
  FCompletionColumnIndex := 0;
  FEnabled := True;
  FOptions := BCEDITOR_COMPLETION_PROPOSAL_DEFAULT_OPTIONS;
  FShortCut := Vcl.Menus.ShortCut(Ord(' '), [ssCtrl]);
  FTrigger := TBCEditorCompletionProposalTrigger.Create;
  FVisibleLines := 8;
  FWidth := 260;
  FConstraints := TSizeConstraints.Create(nil);
end;

destructor TBCEditorCompletionProposal.Destroy;
begin
  FColors.Free;
  FTrigger.Free;
  FColumns.Free;
  FConstraints.Free;

  inherited;
end;

procedure TBCEditorCompletionProposal.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposal then
  with ASource as TBCEditorCompletionProposal do
  begin
    Self.FCloseChars := FCloseChars;
    Self.FColors.Assign(FColors);
    Self.FColumns.Assign(FColumns);
    Self.FEnabled := FEnabled;
    Self.FImages := FImages;
    Self.FOptions := FOptions;
    Self.FSecondaryShortCut := FSecondaryShortCut;
    Self.FShortCut := FShortCut;
    Self.FTrigger.Assign(FTrigger);
    Self.FVisibleLines := FVisibleLines;
    Self.FWidth := FWidth;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCompletionProposal.SetOption(const AOption: TBCEditorCompletionProposalOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

function TBCEditorCompletionProposal.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TBCEditorCompletionProposal.SetImages(const AValue: TCustomImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;
    if Assigned(FImages) then
      FImages.FreeNotification(FOwner);
  end;
end;

end.
