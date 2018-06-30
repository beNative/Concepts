unit BCEditor.Search.RegularExpressions;

interface

uses
  System.Classes, System.RegularExpressions, BCEditor.Search, BCEditor.Lines;

type
  TBCEditorRegexSearch = class(TBCEditorSearchBase)
  strict private
    FLengths: TList;
    FOptions: TRegexOptions;
    FPattern: string;
    FPositions: TList;
  protected
    function GetLength(const AIndex: Integer): Integer; override;
    function GetPattern: string; override;
    function GetResult(const AIndex: Integer): Integer; override;
    function GetResultCount: Integer; override;
    procedure CaseSensitiveChanged; override;
    procedure SetPattern(const AValue: string); override;
  public
    constructor Create;
    destructor Destroy; override;
    function SearchAll(const ALines: TBCEditorLines): Integer; override;
    procedure Clear; override;
  end;

implementation

uses
  System.SysUtils;

constructor TBCEditorRegexSearch.Create;
begin
  inherited Create;

  FOptions := [roMultiLine];
  {$if CompilerVersion > 26}
  Include(FOptions, roNotEmpty);
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

procedure TBCEditorRegexSearch.CaseSensitiveChanged;
begin
  if CaseSensitive then
    Exclude(FOptions, roIgnoreCase)
  else
    Include(FOptions, roIgnoreCase);
end;

function TBCEditorRegexSearch.SearchAll(const ALines: TBCEditorLines): Integer;

  procedure AddResult(const APos, ALength: Integer);
  begin
    FPositions.Add(Pointer(APos));
    FLengths.Add(Pointer(ALength));
  end;

var
  LRegex: TRegEx;
  LMatch: TMatch;
begin
  Result := 0;
  Clear;
  Status := '';
  try
    LRegex := TRegEx.Create(FPattern, FOptions);
    LMatch := LRegex.Match(ALines.Text);
    while LMatch.Success do
    begin
      AddResult(LMatch.Index, LMatch.Length);
      LMatch := LMatch.NextMatch;
      Inc(Result);
    end;
  except
    on E: Exception do
      Status := E.Message;
  end;
end;

procedure TBCEditorRegexSearch.Clear;
begin
  FPositions.Clear;
  FLengths.Clear;
end;

function TBCEditorRegexSearch.GetLength(const AIndex: Integer): Integer;
begin
  Result := Integer(FLengths[AIndex]);
end;

function TBCEditorRegexSearch.GetPattern: string;
begin
  Result := FPattern;
end;

function TBCEditorRegexSearch.GetResult(const AIndex: Integer): Integer;
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
