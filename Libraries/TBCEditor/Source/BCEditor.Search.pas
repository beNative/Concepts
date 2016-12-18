unit BCEditor.Search;

interface

uses
  System.Classes;

type
  TBCEditorSearchBase = class
  strict private
    FCaseSensitive: Boolean;
    FStatus: string;
    FWholeWordsOnly: Boolean;
    procedure SetCaseSensitive(const AValue: Boolean);
  protected
    function GetLength(AIndex: Integer): Integer; virtual; abstract;
    function GetPattern: string; virtual; abstract;
    function GetResult(AIndex: Integer): Integer; virtual; abstract;
    function GetResultCount: Integer; virtual; abstract;
    procedure CaseSensitiveChanged; virtual; abstract;
    procedure SetPattern(const AValue: string); virtual; abstract;
  public
    constructor Create;
    function SearchAll(const AText: string): Integer; virtual; abstract;
    procedure Clear; virtual; abstract;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive default False;
    property Lengths[AIndex: Integer]: Integer read GetLength;
    property Pattern: string read GetPattern write SetPattern;
    property ResultCount: Integer read GetResultCount;
    property Results[AIndex: Integer]: Integer read GetResult;
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

