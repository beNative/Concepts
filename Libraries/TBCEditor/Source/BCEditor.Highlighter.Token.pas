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
    constructor Create(AHighlighterAttribute: TBCEditorHighlighterAttribute); reintroduce; overload;
    constructor Create(AToken: TBCEditorAbstractToken); reintroduce; overload;

    procedure Clear;
    property Attribute: TBCEditorHighlighterAttribute read FAttribute write FAttribute;
    property BreakType: TBCEditorBreakType read FBreakType write FBreakType;
    property OpenRule: TBCEditorAbstractRule read FOpenRule write FOpenRule;
  end;

  TBCEditorMultiToken = class(TBCEditorAbstractToken)
  strict private
    FSymbols: TStringList;
    function GetSymbol(AIndex: Integer): string;
    procedure SetSymbol(AIndex: Integer; const ASymbol: string);
  public
    constructor Create; reintroduce; overload;
    constructor Create(AHighlighterAttribute: TBCEditorHighlighterAttribute); reintroduce; overload;
    constructor Create(AMultiToken: TBCEditorMultiToken); reintroduce; overload;
    destructor Destroy; override;

    function AddSymbol(const ASymbol: string): Integer;
    function SymbolCount: Integer;
    procedure Clear;
    procedure DeleteSymbol(AIndex: Integer);
    property Symbols[Aindex: Integer]: string read GetSymbol write SetSymbol;
  end;

  TBCEditorToken = class(TBCEditorAbstractToken)
  strict private
    FClosingToken: TBCEditorToken;
    FSymbol: string;
    FTemporary: Boolean;
    function GetSymbol: string;
  public
    constructor Create; overload;
    constructor Create(AHighlighterAttribute: TBCEditorHighlighterAttribute); overload;
    constructor Create(AToken: TBCEditorToken); overload;
    constructor Create(AMultiToken: TBCEditorMultiToken; AIndex: Integer); overload;

    procedure Clear;
    property Symbol: string read GetSymbol write FSymbol;
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
    constructor Create(AChar: Char; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType); overload; // virtual;
    constructor Create(AChar: Char); overload;
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

    function FindNode(AChar: Char): TBCEditorTokenNode;
    function GetCount: Integer;
    function GetNode(AIndex: Integer): TBCEditorTokenNode;
    procedure AddNode(Node: TBCEditorTokenNode);
    procedure SetNode(AIndex: Integer; Value: TBCEditorTokenNode);
    property Count: Integer read GetCount;
    property Nodes[Aindex: Integer]: TBCEditorTokenNode read GetNode write SetNode;
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

constructor TBCEditorAbstractToken.Create(AHighlighterAttribute: TBCEditorHighlighterAttribute);
begin
  Create;
  FAttribute := AHighlighterAttribute;
end;

constructor TBCEditorAbstractToken.Create(AToken: TBCEditorAbstractToken);
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

constructor TBCEditorMultiToken.Create(AHighlighterAttribute: TBCEditorHighlighterAttribute);
begin
  inherited;

  Create;
end;

constructor TBCEditorMultiToken.Create(AMultiToken: TBCEditorMultiToken);
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

procedure TBCEditorMultiToken.DeleteSymbol(AIndex: Integer);
begin
  if (AIndex > -1) and (AIndex < FSymbols.Count) then
    FSymbols.Delete(AIndex)
end;

function TBCEditorMultiToken.GetSymbol(AIndex: Integer): string;
begin
  Result := '';
  if (AIndex > -1) and (AIndex < FSymbols.Count) then
    Result := FSymbols[AIndex]
end;

procedure TBCEditorMultiToken.SetSymbol(AIndex: Integer; const ASymbol: string);
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

constructor TBCEditorToken.Create(AHighlighterAttribute: TBCEditorHighlighterAttribute);
begin
  inherited Create(AHighlighterAttribute);
  Symbol := '';
end;

constructor TBCEditorToken.Create(AToken: TBCEditorToken);
begin
  inherited Create(AToken as TBCEditorAbstractToken);
  Symbol := AToken.Symbol;
end;

constructor TBCEditorToken.Create(AMultiToken: TBCEditorMultiToken; AIndex: Integer);
begin
  inherited Create(AMultiToken as TBCEditorAbstractToken);
  Symbol := AMultiToken.Symbols[AIndex];
end;

function TBCEditorToken.GetSymbol: string;
begin
  Result := FSymbol;
end;

procedure TBCEditorToken.Clear;
begin
  Symbol := '';
end;

{ TBCEditorTokenNode }

constructor TBCEditorTokenNode.Create(AChar: Char);
begin
  inherited Create;
  FChar := AChar;
  FNextNodes := TBCEditorTokenNodeList.Create;
  FToken := nil;
end;

constructor TBCEditorTokenNode.Create(AChar: Char; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType);
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

procedure TBCEditorTokenNodeList.AddNode(Node: TBCEditorTokenNode);
begin
  FNodeList.Add(Node);
end;

function TBCEditorTokenNodeList.FindNode(AChar: Char): TBCEditorTokenNode;
var
  i: Integer;
begin
  for i := 0 to FNodeList.Count - 1 do
  begin
    Result := TBCEditorTokenNode(FNodeList.List[i]);
    if Result.Char = AChar then
      exit;
  end;
  Result := nil;
end;

function TBCEditorTokenNodeList.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

function TBCEditorTokenNodeList.GetNode(AIndex: Integer): TBCEditorTokenNode;
begin
  Result := TBCEditorTokenNode(FNodeList[AIndex]);
end;

procedure TBCEditorTokenNodeList.SetNode(AIndex: Integer; Value: TBCEditorTokenNode);
begin
  if AIndex < FNodeList.Count then
    TBCEditorTokenNode(FNodeList[AIndex]).Free;
  FNodeList[AIndex] := Value;
end;

end.
