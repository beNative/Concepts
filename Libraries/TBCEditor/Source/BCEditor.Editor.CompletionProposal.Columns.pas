unit BCEditor.Editor.CompletionProposal.Columns;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Editor.CompletionProposal.Columns.Items,
  BCEditor.Editor.CompletionProposal.Columns.Title;

type
  TBCEditorCompletionProposalColumn = class(TCollectionItem)
  strict private
    FAutoWidth: Boolean;
    FFont: TFont;
    FItems: TBCEditorCompletionProposalColumnItems;
    FTitle: TBCEditorCompletionProposalColumnTitle;
    FWidth: Integer;
    procedure SetFont(const AValue: TFont);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property AutoWidth: Boolean read FAutoWidth write FAutoWidth default True;
    property Font: TFont read FFont write SetFont;
    property Items: TBCEditorCompletionProposalColumnItems read FItems write FItems;
    property Title: TBCEditorCompletionProposalColumnTitle read FTitle write FTitle;
    property Width: Integer read FWidth write FWidth default 0;
  end;

  TBCEditorCompletionProposalColumns = class(TCollection)
  strict private
    FOwner: TPersistent;
    function GetItem(AIndex: Integer): TBCEditorCompletionProposalColumn;
    procedure SetItem(AIndex: Integer; AValue: TBCEditorCompletionProposalColumn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    function Add: TBCEditorCompletionProposalColumn;
    function FindItemID(AID: Integer): TBCEditorCompletionProposalColumn;
    function Insert(AIndex: Integer): TBCEditorCompletionProposalColumn;
    property Items[AIndex: Integer]: TBCEditorCompletionProposalColumn read GetItem write SetItem; default;
  end;

implementation

{ TBCEditorCompletionProposalColumn }

constructor TBCEditorCompletionProposalColumn.Create(ACollection: TCollection);
begin
  inherited;

  FAutoWidth := True;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FItems := TBCEditorCompletionProposalColumnItems.Create(Self, TBCEditorCompletionProposalColumnItem);
  FTitle := TBCEditorCompletionProposalColumnTitle.Create;
  FWidth := 0;
end;

destructor TBCEditorCompletionProposalColumn.Destroy;
begin
  FFont.Free;
  FItems.Free;
  FTitle.Free;

  inherited;
end;

procedure TBCEditorCompletionProposalColumn.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposalColumn then
  with ASource as TBCEditorCompletionProposalColumn do
  begin
    Self.FAutoWidth := FAutoWidth;
    Self.FFont.Assign(FFont);
    Self.FItems.Assign(FItems);
    Self.FTitle.Assign(FTitle);
    Self.FWidth := FWidth;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCompletionProposalColumn.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TBCEditorCompletionProposalColumns }

constructor TBCEditorCompletionProposalColumns.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);

  FOwner := AOwner;
end;

function TBCEditorCompletionProposalColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TBCEditorCompletionProposalColumns.GetItem(AIndex: Integer): TBCEditorCompletionProposalColumn;
begin
  Result := inherited GetItem(AIndex) as TBCEditorCompletionProposalColumn;
end;

procedure TBCEditorCompletionProposalColumns.SetItem(AIndex: Integer; AValue: TBCEditorCompletionProposalColumn);
begin
  inherited SetItem(AIndex, AValue);
end;

function TBCEditorCompletionProposalColumns.Add: TBCEditorCompletionProposalColumn;
begin
  Result := inherited Add as TBCEditorCompletionProposalColumn;
end;

function TBCEditorCompletionProposalColumns.FindItemID(AID: Integer): TBCEditorCompletionProposalColumn;
begin
  Result := inherited FindItemID(AID) as TBCEditorCompletionProposalColumn;
end;

function TBCEditorCompletionProposalColumns.Insert(AIndex: Integer): TBCEditorCompletionProposalColumn;
begin
  Result := inherited Insert(AIndex) as TBCEditorCompletionProposalColumn;
end;

end.
