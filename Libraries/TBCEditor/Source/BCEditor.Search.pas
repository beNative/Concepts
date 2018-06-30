unit BCEditor.Search;

interface

uses
  System.Classes, BCEditor.Lines;

type
  TBCEditorSearchBase = class
  strict private
    FCaseSensitive: Boolean;
    FStatus: string;
    FWholeWordsOnly: Boolean;
    procedure SetCaseSensitive(const AValue: Boolean);
  protected
    function GetLength(const AIndex: Integer): Integer; virtual; abstract;
    function GetPattern: string; virtual; abstract;
    function GetResult(const AIndex: Integer): Integer; virtual; abstract;
    function GetResultCount: Integer; virtual; abstract;
    procedure CaseSensitiveChanged; virtual; abstract;
    procedure SetPattern(const AValue: string); virtual; abstract;
  public
    constructor Create;
    function SearchAll(const ALines: TBCEditorLines): Integer; virtual; abstract;
    procedure Clear; virtual; abstract;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive default False;
    property Lengths[const AIndex: Integer]: Integer read GetLength;
    property Pattern: string read GetPattern write SetPattern;
    property ResultCount: Integer read GetResultCount;
    property Results[const AIndex: Integer]: Integer read GetResult;
    property Status: string read FStatus write FStatus;
    property WholeWordsOnly: Boolean read FWholeWordsOnly write FWholeWordsOnly default False;
  end;

implementation

constructor TBCEditorSearchBase.Create;
begin
  inherited;

  FCaseSensitive := False;
  FWholeWordsOnly := False;
end;

procedure TBCEditorSearchBase.SetCaseSensitive(const AValue: Boolean);
begin
  FCaseSensitive := AValue;
  CaseSensitiveChanged;
end;

end.

