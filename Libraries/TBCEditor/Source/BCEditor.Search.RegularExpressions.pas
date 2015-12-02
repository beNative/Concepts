unit BCEditor.Search.RegularExpressions;

interface

uses
  System.Classes, System.RegularExpressions, BCEditor.Search;

type
  TBCEditorRegexSearch = class(TBCEditorSearchCustom)
  strict private
    FLengths: TList;
    FOptions: TRegexOptions;
    FPattern: string;
    FPositions: TList;
  protected
    function GetLength(AIndex: Integer): Integer; override;
    function GetPattern: string; override;
    function GetResult(AIndex: Integer): Integer; override;
    function GetResultCount: Integer; override;
    procedure SetPattern(const AValue: string); override;
  public
    constructor Create;
    destructor Destroy; override;

    function FindAll(const AInput: string): Integer; override;
    function Replace(const AInput, AReplacement: string): string; override;
    procedure Clear; override;
  end;

implementation

{ TBCEditorRegexSearch }

constructor TBCEditorRegexSearch.Create;
begin
  inherited Create;
  {$if CompilerVersion > 26}
  FOptions := [roNotEmpty];
  {$endif}
  FPositions := TList.Create;
  FLengths := TList.Create;
end;

destructor TBCEditorRegexSearch.Destroy;
begin
  inherited;
  FPositions.Free;
  FLengths.Free;
end;

function TBCEditorRegexSearch.FindAll(const AInput: string): Integer;

  procedure AddResult(const aPos, aLength: Integer);
  begin
    FPositions.Add(Pointer(aPos));
    FLengths.Add(Pointer(aLength));
  end;

var
  Regex: TRegEx;
  Match: TMatch;
begin
  Result := 0;
  Clear;
  Regex := TRegEx.Create(FPattern, FOptions);
  Match := Regex.Match(AInput);
  while Match.Success do
  begin
    AddResult(Match.Index, Match.Length);
    Match := Match.NextMatch;
  end;
end;

function TBCEditorRegexSearch.Replace(const AInput, AReplacement: string): string;
var
  Regex: TRegEx;
begin
  Regex := TRegEx.Create(FPattern, FOptions);
  Regex.Replace(AInput, AReplacement);
  Result := AReplacement;
end;

procedure TBCEditorRegexSearch.Clear;
begin
  FPositions.Clear;
  FLengths.Clear;
end;

function TBCEditorRegexSearch.GetLength(AIndex: Integer): Integer;
begin
  Result := Integer(FLengths[AIndex]);
end;

function TBCEditorRegexSearch.GetPattern: string;
begin
  Result := FPattern;
end;

function TBCEditorRegexSearch.GetResult(AIndex: Integer): Integer;
begin
  Result := Integer(FPositions[AIndex]);
end;

function TBCEditorRegexSearch.GetResultCount: Integer;
begin
  Result := FPositions.Count;
end;

procedure TBCEditorRegexSearch.SetPattern(const AValue: string);
begin
  FPattern := AValue;
end;

end.
