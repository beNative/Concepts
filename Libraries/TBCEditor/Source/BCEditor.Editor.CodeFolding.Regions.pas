unit BCEditor.Editor.CodeFolding.Regions;

interface

uses
  System.Classes, System.SysUtils, BCEditor.Editor.SkipRegions, BCEditor.Types, BCEditor.Consts;

type
  TBCEditorCodeFoldingRegion = class;

  TBCEditorCodeFoldingRegionItem = class(TCollectionItem)
  strict private
    FBeginWithBreakChar: Boolean;
    FBreakCharFollows: Boolean;
    FBreakIfNotFoundBeforeNextRegion: string;
    FCloseAtNextToken: Boolean;
    FCloseToken: string;
    FCloseTokenBeginningOfLine: Boolean;
    FCloseTokenLength: Integer;
    FNoSubs: Boolean;
    FOpenIsClose: Boolean;
    FOpenToken: string;
    FOpenTokenBeginningOfLine: Boolean;
    FOpenTokenBreaksLine: Boolean;
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
    property BeginWithBreakChar: Boolean read FBeginWithBreakChar write FBeginWithBreakChar;
    property BreakCharFollows: Boolean read FBreakCharFollows write FBreakCharFollows default True;
    property BreakIfNotFoundBeforeNextRegion: string read FBreakIfNotFoundBeforeNextRegion write FBreakIfNotFoundBeforeNextRegion;
    property CloseAtNextToken: Boolean read FCloseAtNextToken write FCloseAtNextToken;
    property CloseToken: string read FCloseToken write FCloseToken;
    property CloseTokenBeginningOfLine: Boolean read FCloseTokenBeginningOfLine write FCloseTokenBeginningOfLine default False;
    property CloseTokenLength: Integer read FCloseTokenLength write FCloseTokenLength;
    property NoSubs: Boolean read FNoSubs write FNoSubs default False;
    property OpenIsClose: Boolean read FOpenIsClose write FOpenIsClose default False;
    property OpenToken: string read FOpenToken write FOpenToken;
    property OpenTokenBeginningOfLine: Boolean read FOpenTokenBeginningOfLine write FOpenTokenBeginningOfLine default False;
    property OpenTokenBreaksLine: Boolean read FOpenTokenBreaksLine write FOpenTokenBreaksLine default False;
    property OpenTokenCanBeFollowedBy: string read FOpenTokenCanBeFollowedBy write FOpenTokenCanBeFollowedBy;
    property OpenTokenEnd: string read FOpenTokenEnd write FOpenTokenEnd;
    property OpenTokenLength: Integer read FOpenTokenLength write FOpenTokenLength;
    property ParentRegionItem: TBCEditorCodeFoldingRegionItem read FParentRegionItem write FParentRegionItem;
    property SharedClose: Boolean read FSharedClose write FSharedClose default False;
    property ShowGuideLine: Boolean read FShowGuideLine write FShowGuideLine default True;
    property SkipIfFoundAfterOpenTokenArray: TBCEditorArrayOfString read FSkipIfFoundAfterOpenTokenArray write FSkipIfFoundAfterOpenTokenArray;
    property SkipIfFoundAfterOpenTokenArrayCount: Integer read FSkipIfFoundAfterOpenTokenArrayCount write SetSkipIfFoundAfterOpenTokenArrayCount;
    property TokenEndIsPreviousLine: Boolean read FTokenEndIsPreviousLine write FTokenEndIsPreviousLine default False;
  end;

  TBCEditorCodeFoldingRegion = class(TCollection)
  strict private
    FCloseToken: string;
    FEscapeChar: Char;
    FFoldTags: Boolean;
    FOpenToken: string;
    FSkipRegions: TBCEditorSkipRegions;
    FStringEscapeChar: Char;
    function GetItem(AIndex: Integer): TBCEditorCodeFoldingRegionItem;
  public
    constructor Create(AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add(const AOpenToken: string; const ACloseToken: string): TBCEditorCodeFoldingRegionItem;
    property CloseToken: string read FCloseToken write FCloseToken;
    function Contains(const AOpenToken: string; const ACloseToken: string): Boolean;
    property EscapeChar: Char read FEscapeChar write FEscapeChar default BCEDITOR_NONE_CHAR;
    property FoldTags: Boolean read FFoldTags write FFoldTags default False;
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
  FBreakIfNotFoundBeforeNextRegion := '';
  FCloseTokenBeginningOfLine := False;
  FNoSubs := False;
  FOpenIsClose := False;
  FOpenTokenBeginningOfLine := False;
  FOpenTokenBreaksLine := False;
  FSharedClose := False;
  FBreakCharFollows := True;
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
    OpenToken := AOpenToken;
    OpenTokenLength := Length(AOpenToken);
    CloseToken := ACloseToken;
    CloseTokenLength := Length(ACloseToken);
  end;
end;

constructor TBCEditorCodeFoldingRegion.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);

  FSkipRegions := TBCEditorSkipRegions.Create(TBCEditorSkipRegionItem);
  FEscapeChar := BCEDITOR_NONE_CHAR;
  FStringEscapeChar := BCEDITOR_NONE_CHAR;
  FFoldTags := False;
end;

destructor TBCEditorCodeFoldingRegion.Destroy;
begin
  FSkipRegions.Free;

  inherited;
end;

function TBCEditorCodeFoldingRegion.Contains(const AOpenToken: string; const ACloseToken: string): Boolean;
var
  LIndex: Integer;
  LItem: TBCEditorCodeFoldingRegionItem;
begin
  Result := False;
  for LIndex := 0 to Count - 1 do
  begin
    LItem := Items[LIndex];
    if (LItem.OpenToken = AOpenToken) and (LItem.CloseToken = ACloseToken) then
      Exit(True);
  end;
end;

function TBCEditorCodeFoldingRegion.GetItem(AIndex: Integer): TBCEditorCodeFoldingRegionItem;
begin
  Result := TBCEditorCodeFoldingRegionItem(inherited Items[AIndex]);
end;

end.
