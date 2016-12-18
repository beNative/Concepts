unit BCEditor.Editor.CompletionProposal.Trigger;

interface

uses
  System.Classes;

type
  TBCEditorCompletionProposalTrigger = class(TPersistent)
  strict private
    FChars: string;
    FEnabled: Boolean;
    FInterval: Integer;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Chars: string read FChars write FChars;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Interval: Integer read FInterval write FInterval default 1000;
  end;

implementation

constructor TBCEditorCompletionProposalTrigger.Create;
begin
  inherited;

  FChars := '.';
  FEnabled := False;
  FInterval := 1000;
end;

procedure TBCEditorCompletionProposalTrigger.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposalTrigger then
  with ASource as TBCEditorCompletionProposalTrigger do
  begin
    Self.FChars := FChars;
    Self.FEnabled := FEnabled;
    Self.FInterval := FInterval;
  end
  else
    inherited Assign(ASource);
end;

end.
