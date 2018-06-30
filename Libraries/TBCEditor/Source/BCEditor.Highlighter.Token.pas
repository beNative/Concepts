unit BCEditor.Highlighter.Token;

interface

uses
  System.Classes, BCEditor.Types, BCEditor.Highlighter.Attributes;

type
  TBCEditorAbstractRule = class(TObject)
  strict private
    FTokenType: TBCEditorRangeType;
  public
    property TokenType: TBCEditorRangeType read FTokenType write FTokenType;
  end;

  TBCEditorAbstractToken = class(TObject)
  strict private
    FAttribute: TBCEditorHighlighterAttribute;
    FBreakType: TBCEditorBreakType;
    FOpenRule: TBCEditorAbstractRule;
  public
    constructor Create; reintroduce; overload;
    constructor Create(const AHighlighterAttribute: TBCEditorHighlighterAttribute); reintroduce; overload;
    constructor Create(const AToken: TBCEditorAbstractToken); reintroduce; overload;

    procedure Clear;
    property Attribute: TBCEditorHighlighterAttribute read FAttribute write FAttribute;
    property BreakType: TBCEditorBreakType read FBreakType write FBreakType;
    property OpenRule: TBCEditorAbstractRule read FOpenRule write FOpenRule;
  end;

  TBCEditorMultiToken = class(TBCEditorAbstractToken)
  strict private
    FSymbols: TStringList;
    function GetSymbol(const AIndex: Integer): string;
    procedure SetSymbol(const AIndex: Integer; const ASymbol: string);
  public
    constructor Create; reintroduce; overload;
    constructor Create(const AHighlighterAttribute: TBCEditorHighlighterAttribute); reintroduce; overload;
    constructor Create(const AMultiToken: TBCEditorMultiToken); reintroduce; overload;
    destructor Destroy; override;

    function AddSymbol(const ASymbol: string): Integer;
    function SymbolCount: Integer;
    procedure Clear;
    procedure DeleteSymbol(const AIndex: Integer);
    property Symbols[const AIndex: Integer]: string read GetSymbol write SetSymbol;
  end;

  TBCEditorToken = class(TBCEditorAbstractToken)
  strict private
    FClosingToken: TBCEditorToken;
    FSymbol: string;
    FTemporary: Boolean;
  public
    constructor Create; overload;
    constructor Create(const AHighlighterAttribute: TBCEditorHighlighterAttribute); overload;
    constructor Create(const AToken: TBCEditorToken); overload;
    constructor Create(const AMultiToken: TBCEditorMultiToken; const AIndex: Integer); overload;

    procedure Clear;
    property Symbol: string read FSymbol write FSymbol;
    property ClosingToken: TBCEditorToken read FClosingToken write FClosingToken;
    property Temporary: Boolean read FTemporary write FTemporary;
  end;

  TBCEditorTokenNodeList = class;

  TBCEditorTokenNode = class(TObject)
  strict private
    FChar: Char;
    FBreakType: TBCEditorBreakType;
    FNextNodes: TBCEditorTokenNodeList;
    FToken: TBCEditorToken;
  public
    constructor Create(const AChar: Char; const AToken: TBCEditorToken; const ABreakType: TBCEditorBreakType); overload;
    constructor Create(const AChar: Char); overload;
    destructor Destroy; override;

    property Char: Char read FChar write FChar;
    property BreakType: TBCEditorBreakType read FBreakType write FBreakType;
    property NextNodes: TBCEditorTokenNodeList read FNextNodes write FNextNodes;
    property Token: TBCEditorToken read FToken write FToken;
  end;

  TBCEditorTokenNodeList = class(TObject)
  strict private
    FNodeList: TList;
  public
    constructor Create;
    destructor Destroy; override;

    function FindNode(const AChar: Char): TBCEditorTokenNode;
    function GetCount: Integer;
    function GetNode(const AIndex: Integer): TBCEditorTokenNode;
    procedure AddNode(const ANode: TBCEditorTokenNode);
    procedure SetNode(const AIndex: Integer; const AValue: TBCEditorTokenNode);
    property Count: Integer read GetCount;
    property Nodes[const Aindex: Integer]: TBCEditorTokenNode read GetNode write SetNode;
  end;

implementation

uses
  System.SysUtils, BCEditor.Utils;

{ TBCEditorAbstractToken }

constructor TBCEditorAbstractToken.Create;
begin
  inherited;

  FAttribute := nil;
  FOpenRule := nil;
  FBreakType := btUnspecified;
end;

constructor TBCEditorAbstractToken.Create(const AHighlighterAttribute: TBCEditorHighlighterAttribute);
begin
  Create;
  FAttribute := AHighlighterAttribute;
end;

constructor TBCEditorAbstractToken.Create(const AToken: TBCEditorAbstractToken);
begin
  inherited Create;
  FAttribute := AToken.Attribute;
  FBreakType := AToken.BreakType;
end;

procedure TBCEditorAbstractToken.Clear;
begin
  FBreakType := btUnspecified;
end;

{ TBCEditorMultiToken }

constructor TBCEditorMultiToken.Create;
begin
  inherited;

  FSymbols := TStringList.Create;
  BreakType := btUnspecified;
end;

constructor TBCEditorMultiToken.Create(const AHighlighterAttribute: TBCEditorHighlighterAttribute);
begin
  inherited;

  Create;
end;

constructor TBCEditorMultiToken.Create(const AMultiToken: TBCEditorMultiToken);
begin
  inherited Create(AMultiToken as TBCEditorAbstractToken);

  Create;
end;

destructor TBCEditorMultiToken.Destroy;
begin
  FSymbols.Free;
  FSymbols := nil;
  inherited;
end;

function TBCEditorMultiToken.AddSymbol(const ASymbol: string): Integer;
begin
  Result := FSymbols.Add(ASymbol);
end;

procedure TBCEditorMultiToken.Clear;
begin
  FSymbols.Clear;
end;

procedure TBCEditorMultiToken.DeleteSymbol(const AIndex: Integer);
begin
  if (AIndex > -1) and (AIndex < FSymbols.Count) then
    FSymbols.Delete(AIndex)
end;

function TBCEditorMultiToken.GetSymbol(const AIndex: Integer): string;
begin
  Result := '';
  if (AIndex > -1) and (AIndex < FSymbols.Count) then
    Result := FSymbols[AIndex]
end;

procedure TBCEditorMultiToken.SetSymbol(const AIndex: Integer; const ASymbol: string);
begin
  if (AIndex > -1) and (AIndex < FSymbols.Count) then
    FSymbols[AIndex] := ASymbol
end;

function TBCEditorMultiToken.SymbolCount: Integer;
begin
  Result := FSymbols.Count;
end;

constructor TBCEditorToken.Create;
begin
  inherited Create;

  Symbol := '';
  FTemporary := False;
end;

constructor TBCEditorToken.Create(const AHighlighterAttribute: TBCEditorHighlighterAttribute);
begin
  inherited Create(AHighlighterAttribute);
  Symbol := '';
end;

constructor TBCEditorToken.Create(const AToken: TBCEditorToken);
begin
  inherited Create(AToken as TBCEditorAbstractToken);

  Symbol := AToken.Symbol;
end;

constructor TBCEditorToken.Create(const AMultiToken: TBCEditorMultiToken; const AIndex: Integer);
begin
  inherited Create(AMultiToken as TBCEditorAbstractToken);

  Symbol := AMultiToken.Symbols[AIndex];
end;

procedure TBCEditorToken.Clear;
begin
  Symbol := '';
end;

{ TBCEditorTokenNode }

constructor TBCEditorTokenNode.Create(const AChar: Char);
begin
  inherited Create;

  FChar := AChar;
  FNextNodes := TBCEditorTokenNodeList.Create;
  FToken := nil;
end;

constructor TBCEditorTokenNode.Create(const AChar: Char; const AToken: TBCEditorToken; const ABreakType: TBCEditorBreakType);
begin
  Create(AChar);
  FBreakType := ABreakType;
  FToken := AToken;
end;

destructor TBCEditorTokenNode.Destroy;
begin
  FNextNodes.Free;
  FNextNodes := nil;
  inherited;
end;

{ TBCEditorTokenNodeList }

constructor TBCEditorTokenNodeList.Create;
begin
  inherited;

  FNodeList := TList.Create;
end;

destructor TBCEditorTokenNodeList.Destroy;
begin
  FreeList(FNodeList);
  inherited;
end;

procedure TBCEditorTokenNodeList.AddNode(const ANode: TBCEditorTokenNode);
begin
  FNodeList.Add(ANode);
end;

function TBCEditorTokenNodeList.FindNode(const AChar: Char): TBCEditorTokenNode;
var
  LIndex: Integer;
  LTokenNode: TBCEditorTokenNode;
begin
  Result := nil;
  for LIndex := FNodeList.Count - 1 downto 0 do
  begin
    LTokenNode := TBCEditorTokenNode(FNodeList.List[LIndex]);
    if LTokenNode.Char = AChar then
      Exit(LTokenNode);
  end;
end;

function TBCEditorTokenNodeList.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

function TBCEditorTokenNodeList.GetNode(const AIndex: Integer): TBCEditorTokenNode;
begin
  Result := TBCEditorTokenNode(FNodeList[AIndex]);
end;

procedure TBCEditorTokenNodeList.SetNode(const AIndex: Integer; const AValue: TBCEditorTokenNode);
begin
  if AIndex < FNodeList.Count then
    TBCEditorTokenNode(FNodeList[AIndex]).Free;
  FNodeList[AIndex] := AValue;
end;

end.
