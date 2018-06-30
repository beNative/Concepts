unit BCEditor.Editor.CompletionProposal.Columns.Items;

interface

uses
  System.Classes;

type
  TBCEditorCompletionProposalColumnItem = class(TCollectionItem)
  strict private
    FImageIndex: Integer;
    FValue: string;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(ASource: TPersistent); override;
  published
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
    property Value: string read FValue write FValue;
  end;

  TBCEditorCompletionProposalColumnItems = class(TCollection)
  strict private
    FOwner: TPersistent;
    function GetItem(AIndex: Integer): TBCEditorCompletionProposalColumnItem;
    procedure SetItem(AIndex: Integer; AValue: TBCEditorCompletionProposalColumnItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    function Add: TBCEditorCompletionProposalColumnItem;
    function FindItemID(AID: Integer): TBCEditorCompletionProposalColumnItem;
    function Insert(AIndex: Integer): TBCEditorCompletionProposalColumnItem;
    property Items[AIndex: Integer]: TBCEditorCompletionProposalColumnItem read GetItem write SetItem; default;
  end;

implementation

{ TBCEditorCompletionProposalColumnItem }

constructor TBCEditorCompletionProposalColumnItem.Create(ACollection: TCollection);
begin
  inherited;

  FImageIndex := -1;
end;

procedure TBCEditorCompletionProposalColumnItem.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposalColumnItem then
  with ASource as TBCEditorCompletionProposalColumnItem do
  begin
    Self.FImageIndex := FImageIndex;
    Self.FValue := FValue;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCompletionProposalColumnItems }

constructor TBCEditorCompletionProposalColumnItems.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);

  FOwner := AOwner;
end;

function TBCEditorCompletionProposalColumnItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TBCEditorCompletionProposalColumnItems.GetItem(AIndex: Integer): TBCEditorCompletionProposalColumnItem;
begin
  Result := inherited GetItem(AIndex) as TBCEditorCompletionProposalColumnItem;
end;

procedure TBCEditorCompletionProposalColumnItems.SetItem(AIndex: Integer; AValue: TBCEditorCompletionProposalColumnItem);
begin
  inherited SetItem(AIndex, AValue);
end;

function TBCEditorCompletionProposalColumnItems.Add: TBCEditorCompletionProposalColumnItem;
begin
  Result := inherited Add as TBCEditorCompletionProposalColumnItem;
end;

function TBCEditorCompletionProposalColumnItems.FindItemID(AID: Integer): TBCEditorCompletionProposalColumnItem;
begin
  Result := inherited FindItemID(AID) as TBCEditorCompletionProposalColumnItem;
end;

function TBCEditorCompletionProposalColumnItems.Insert(AIndex: Integer): TBCEditorCompletionProposalColumnItem;
begin
  Result := inherited Insert(AIndex) as TBCEditorCompletionProposalColumnItem;
end;

end.
