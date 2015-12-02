unit BCEditor.Editor.CompletionProposal.Columns;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorProposalColumn = class(TCollectionItem)
  strict private
    FAutoWidth: Boolean;
    FItemList: TStrings;
    FWidth: Integer;
    procedure SetItemList(const AValue: TStrings);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property AutoWidth: Boolean read FAutoWidth write FAutoWidth default True;
    property ItemList: TStrings read FItemList write SetItemList;
    property Width: Integer read FWidth write FWidth default 0;
  end;

  TBCEditorProposalColumns = class(TCollection)
  strict private
    FOwner: TPersistent;
    function GetItem(AIndex: Integer): TBCEditorProposalColumn;
    procedure SetItem(AIndex: Integer; AValue: TBCEditorProposalColumn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    function Add: TBCEditorProposalColumn;
    function FindItemID(AID: Integer): TBCEditorProposalColumn;
    function Insert(AIndex: Integer): TBCEditorProposalColumn;
    property Items[AIndex: Integer]: TBCEditorProposalColumn read GetItem write SetItem; default;
  end;

implementation

{ TBCEditorProposalColumn }

constructor TBCEditorProposalColumn.Create(ACollection: TCollection);
begin
  inherited;
  FItemList := TStringList.Create;
  FAutoWidth := True;
  FWidth := 0;
end;

destructor TBCEditorProposalColumn.Destroy;
begin
  FItemList.Free;

  inherited;
end;

procedure TBCEditorProposalColumn.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorProposalColumn then
  with ASource as TBCEditorProposalColumn do
    Self.FItemList.Assign(FItemList)
  else
    inherited Assign(ASource);
end;

procedure TBCEditorProposalColumn.SetItemList(const AValue: TStrings);
begin
  FItemList.Assign(AValue);
end;

{ TBCEditorProposalColumns }

constructor TBCEditorProposalColumns.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);

  FOwner := AOwner;
end;

function TBCEditorProposalColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TBCEditorProposalColumns.GetItem(AIndex: Integer): TBCEditorProposalColumn;
begin
  Result := inherited GetItem(AIndex) as TBCEditorProposalColumn;
end;

procedure TBCEditorProposalColumns.SetItem(AIndex: Integer; AValue: TBCEditorProposalColumn);
begin
  inherited SetItem(AIndex, AValue);
end;

function TBCEditorProposalColumns.Add: TBCEditorProposalColumn;
begin
  Result := inherited Add as TBCEditorProposalColumn;
end;

function TBCEditorProposalColumns.FindItemID(AID: Integer): TBCEditorProposalColumn;
begin
  Result := inherited FindItemID(AID) as TBCEditorProposalColumn;
end;

function TBCEditorProposalColumns.Insert(AIndex: Integer): TBCEditorProposalColumn;
begin
  Result := inherited Insert(AIndex) as TBCEditorProposalColumn;
end;

end.
