unit BCEditor.Editor.SkipRegions;

interface

uses
  System.Classes, System.SysUtils, BCEditor.Consts;

type
  TBCEditorSkipRegionItemType = (ritUnspecified, ritMultiLineString, ritSingleLineString, ritMultiLineComment, ritSingleLineComment);

  TBCEditorSkipRegionItem = class(TCollectionItem)
  strict private
    FCloseToken: string;
    FOpenToken: string;
    FRegionType: TBCEditorSkipRegionItemType;
    FSkipEmptyChars: Boolean;
    FSkipIfNextCharIsNot: Char;
  public
    property OpenToken: string read FOpenToken write FOpenToken;
    property CloseToken: string read FCloseToken write FCloseToken;
    property RegionType: TBCEditorSkipRegionItemType read FRegionType write FRegionType;
    property SkipEmptyChars: Boolean read FSkipEmptyChars write FSkipEmptyChars;
    property SkipIfNextCharIsNot: Char read FSkipIfNextCharIsNot write FSkipIfNextCharIsNot default BCEDITOR_NONE_CHAR;
  end;

  TBCEditorSkipRegions = class(TCollection)
  strict private
    function GetSkipRegionItem(AIndex: Integer): TBCEditorSkipRegionItem;
  public
    function Add(const AOpenToken, ACloseToken: string): TBCEditorSkipRegionItem;
    function Contains(const AOpenToken, ACloseToken: string): Boolean;
    property SkipRegionItems[AIndex: Integer]: TBCEditorSkipRegionItem read GetSkipRegionItem; default;
  end;

implementation

uses
  Winapi.Windows;

{ TBCEditorSkipRegions }

function TBCEditorSkipRegions.Add(const AOpenToken, ACloseToken: string): TBCEditorSkipRegionItem;
begin
  Result := TBCEditorSkipRegionItem(inherited Add);
  with Result do
  begin
    OpenToken := AOpenToken;
    CloseToken := ACloseToken;
  end;
end;

function TBCEditorSkipRegions.Contains(const AOpenToken, ACloseToken: string): Boolean;
var
  LIndex: Integer;
  LSkipRegion: TBCEditorSkipRegionItem;
begin
  Result := False;
  for LIndex := 0 to Count - 1 do
  begin
    LSkipRegion := SkipRegionItems[LIndex];
    if (LSkipRegion.OpenToken = AOpenToken) and (LSkipRegion.CloseToken = ACloseToken) then
      Exit(True);
  end;
end;

function TBCEditorSkipRegions.GetSkipRegionItem(AIndex: Integer): TBCEditorSkipRegionItem;
begin
  Result := TBCEditorSkipRegionItem(inherited Items[AIndex]);
end;

end.
