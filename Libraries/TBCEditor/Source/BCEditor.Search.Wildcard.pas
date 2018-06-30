unit BCEditor.Search.Wildcard;

interface

uses
  System.Classes, BCEditor.Search.RegularExpressions;

type
  TBCEditorWildcardSearch = class(TBCEditorRegexSearch)
  strict private
    FPattern: string;
  protected
    function GetPattern: string; override;
    function WildCardToRegExpr(const AWildCard: string): string;
    procedure SetPattern(const AValue: string); override;
  public
    constructor Create;
  end;

implementation

{ TBCEditorWildcardSearch }

constructor TBCEditorWildcardSearch.Create;
begin
  inherited Create;
  FPattern := '';
end;

function TBCEditorWildcardSearch.GetPattern: string;
begin
  Result := FPattern;
end;

procedure TBCEditorWildcardSearch.SetPattern(const AValue: string);
begin
  FPattern := AValue;

  inherited SetPattern(WildCardToRegExpr(AValue));
end;

function TBCEditorWildcardSearch.WildCardToRegExpr(const AWildCard: string): string;
var
  LIndex: Integer;
begin
  Result := '';

  for LIndex := 1 to Length(AWildCard) do
    case AWildCard[LIndex] of
      '*':
        Result := Result + '.*';
      '?':
        Result := Result + '.?';
    else
      Result := Result + AWildCard[LIndex];
    end;
end;

end.
