unit BCEditor.Editor.CodeFolding.Regions;

interface

uses
  System.Classes, System.SysUtils, BCEditor.Editor.SkipRegions, BCEditor.Types, BCEditor.Consts;

type
  TBCEditorCodeFoldingRegion = class;

  TBCEditorCodeFoldingRegionItem = class(TCollectionItem)
  strict private
    FBreakIfNotFoundBeforeNextRegion: string;
    FCloseAtNextToken: Boolean;
    FCloseToken: string;
    FCloseTokenBeginningOfLine: Boolean;
    FCloseTokenLength: Integer;
    FNoSubs: Boolean;
    FOpenIsClose: Boolean;
    FOpenToken: string;
    FOpenTokenBeginningOfLine: Boolean;
    FOpenTokenCanBeFollowedBy: string;
    FOpenTokenEnd: string;
    FOpenTokenLength: Integer;
    FParentRegionItem: TBCEditorCodeFoldingRegionItem;
    FSharedClose: Boolean;
    FShowGuideLine: Boolean;
    FSkipIfFoundAfterOpenTokenArray: TBCEditorArrayOfString;
    FSkipIfFoundAfterOpenTokenArrayCount: Integer;
    FTokenEndIsPreviousLine: Boolean;
    procedure SetSkipIfFoundAfterOpenTokenArrayCount(const AValue: Integer);
  public
    constructor Create(ACollection: TCollection); override;
    property BreakIfNotFoundBeforeNextRegion: string read FBreakIfNotFoundBeforeNextRegion write FBreakIfNotFoundBeforeNextRegion;
    property CloseAtNextToken: Boolean read FCloseAtNextToken write FCloseAtNextToken;
    property CloseToken: string read FCloseToken write FCloseToken;
    property CloseTokenBeginningOfLine: Boolean read FCloseTokenBeginningOfLine write FCloseTokenBeginningOfLine default False;
    property CloseTokenLength: Integer read FCloseTokenLength write FCloseTokenLength;
    property NoSubs: Boolean read FNoSubs write FNoSubs default False;
    property OpenIsClose: Boolean read FOpenIsClose write FOpenIsClose default False;
    property OpenToken: string read FOpenToken write FOpenToken;
    property OpenTokenBeginningOfLine: Boolean read FOpenTokenBeginningOfLine write FOpenTokenBeginningOfLine default False;
    property OpenTokenCanBeFollowedBy: string read FOpenTokenCanBeFollowedBy write FOpenTokenCanBeFollowedBy;
    property OpenTokenEnd: string read FOpenTokenEnd write FOpenTokenEnd;
    property OpenTokenLength: Integer read FOpenTokenLength write FOpenTokenLength;
    property ParentRegionItem: TBCEditorCodeFoldingRegionItem read FParentRegionItem write FParentRegionItem;
    property SharedClose: Boolean read FSharedClose write FSharedClose default False;
    property ShowGuideLine: Boolean read FShowGuideLine write FShowGuideLine default True;
    property SkipIfFoundAfterOpenTokenArray: TBCEditorArrayOfString read FSkipIfFoundAfterOpenTokenArray write FSkipIfFoundAfterOpenTokenArray;
    property SkipIfFoundAfterOpenTokenArrayCount: Integer read FSkipIfFoundAfterOpenTokenArrayCount write SetSkipIfFoundAfterOpenTokenArrayCount;
    property TokenEndIsPreviousLine: Boolean read FTokenEndIsPreviousLine write FTokenEndIsPreviousLine;
  end;

  TBCEditorCodeFoldingRegion = class(TCollection)
  strict private
    FCloseToken: string;
    FOpenToken: string;
    FSkipRegions: TBCEditorSkipRegions;
    FEscapeChar: Char;
    FStringEscapeChar: Char;
    function GetItem(AIndex: Integer): TBCEditorCodeFoldingRegionItem;
  public
    constructor Create(AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add(const AOpenToken: string; const ACloseToken: string): TBCEditorCodeFoldingRegionItem;
    property CloseToken: string read FCloseToken write FCloseToken;
    function Contains(const AOpenToken, ACloseToken: string): Boolean;
    property EscapeChar: Char read FEscapeChar write FEscapeChar default BCEDITOR_NONE_CHAR;
    property Items[AIndex: Integer]: TBCEditorCodeFoldingRegionItem read GetItem; default;
    property OpenToken: string read FOpenToken write FOpenToken;
    property SkipRegions: TBCEditorSkipRegions read FSkipRegions;
    property StringEscapeChar: Char read FStringEscapeChar write FStringEscapeChar default BCEDITOR_NONE_CHAR;
  end;

  TBCEditorCodeFoldingRegions = array of TBCEditorCodeFoldingRegion;

implementation

{ TBCEditorCodeFoldingRegionItem }

constructor TBCEditorCodeFoldingRegionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FSkipIfFoundAfterOpenTokenArrayCount := 0;
end;

procedure TBCEditorCodeFoldingRegionItem.SetSkipIfFoundAfterOpenTokenArrayCount(const AValue: Integer);
begin
  FSkipIfFoundAfterOpenTokenArrayCount := AValue;
  SetLength(FSkipIfFoundAfterOpenTokenArray, AValue);
end;

{ TBCEditorCodeFoldingRegions }

function TBCEditorCodeFoldingRegion.Add(const AOpenToken: string; const ACloseToken: string): TBCEditorCodeFoldingRegionItem;
begin
  Result := TBCEditorCodeFoldingRegionItem(inherited Add);
  with Result do
  begin
    BreakIfNotFoundBeforeNextRegion := '';
    CloseToken := ACloseToken;
    CloseTokenBeginningOfLine := False;
    CloseTokenLength := Length(ACloseToken);
    NoSubs := False;
    OpenIsClose := False;
    OpenToken := AOpenToken;
    OpenTokenBeginningOfLine := False;
    OpenTokenLength := Length(AOpenToken);
    SharedClose := False;
  end;
end;

constructor TBCEditorCodeFoldingRegion.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  FSkipRegions := TBCEditorSkipRegions.Create(TBCEditorSkipRegionItem);
  FEscapeChar := BCEDITOR_NONE_CHAR;
  FStringEscapeChar := BCEDITOR_NONE_CHAR;
end;

destructor TBCEditorCodeFoldingRegion.Destroy;
begin
  FSkipRegions.Free;

  inherited;
end;

function TBCEditorCodeFoldingRegion.Contains(const AOpenToken, ACloseToken: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if (Items[i].OpenToken = AOpenToken) and (Items[i].CloseToken = ACloseToken) then
      Exit(True);
end;

function TBCEditorCodeFoldingRegion.GetItem(AIndex: Integer): TBCEditorCodeFoldingRegionItem;
begin
  Result := TBCEditorCodeFoldingRegionItem(inherited Items[AIndex]);
end;

end.
