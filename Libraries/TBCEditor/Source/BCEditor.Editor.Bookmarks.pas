unit BCEditor.Editor.Bookmarks;

interface

uses
  Vcl.Controls, System.Classes, System.Contnrs, BCEditor.Consts;

type
  TBCEditorBookmark = class
  protected
    FChar: Integer;
    FData: Pointer;
    FEditor: TCustomControl;
    FImageIndex: Integer;
    FIndex: Integer;
    FInternalImage: Boolean;
    FLine: Integer;
    FVisible: Boolean;
    function GetIsBookmark: Boolean;
    procedure Invalidate;
    procedure SetChar(const AValue: Integer); virtual;
    procedure SetImageIndex(const AValue: Integer); virtual;
    procedure SetInternalImage(const AValue: Boolean);
    procedure SetLine(const AValue: Integer); virtual;
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create(AOwner: TCustomControl);

    property Char: Integer read FChar write SetChar;
    property Data: Pointer read FData write FData;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Index: Integer read FIndex write FIndex;
    property InternalImage: Boolean read FInternalImage write SetInternalImage;
    property IsBookmark: Boolean read GetIsBookmark;
    property Line: Integer read FLine write SetLine;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TBCEditorBookmarkEvent = procedure(ASender: TObject; var AMark: TBCEditorBookmark) of object;
  TBCEditorBookmarks = array [1 .. BCEDITOR_MAX_BOOKMARKS] of TBCEditorBookmark;

  TBCEditorBookmarkList = class(TObjectList)
  protected
    FEditor: TCustomControl;
    FOnChange: TNotifyEvent;
    function GetItem(AIndex: Integer): TBCEditorBookmark;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure SetItem(AIndex: Integer; AItem: TBCEditorBookmark);
    property OwnsObjects;
  public
    constructor Create(AOwner: TCustomControl);

    function Extract(AItem: TBCEditorBookmark): TBCEditorBookmark;
    function First: TBCEditorBookmark;
    function Last: TBCEditorBookmark;
    procedure ClearLine(ALine: Integer);
    procedure GetMarksForLine(ALine: Integer; var AMarks: TBCEditorBookmarks);
    procedure Place(AMark: TBCEditorBookmark);
  public
    property Items[AIndex: Integer]: TBCEditorBookmark read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  BCEditor.Editor.Base, System.Types;

{ TBCEditorBookmark }

constructor TBCEditorBookmark.Create(AOwner: TCustomControl);
begin
  inherited Create;

  FIndex := -1;
  FEditor := AOwner;
end;

function TBCEditorBookmark.GetIsBookmark: Boolean;
begin
  Result := FIndex >= 0;
end;

procedure TBCEditorBookmark.SetChar(const AValue: Integer);
begin
  FChar := AValue;
end;

procedure TBCEditorBookmark.Invalidate;
begin
  if FVisible then
    if Assigned(FEditor) and (FEditor is TBCBaseEditor) then
     (FEditor as TBCBaseEditor).Invalidate;
end;

procedure TBCEditorBookmark.SetImageIndex(const AValue: Integer);
begin
  FImageIndex := AValue;
  Invalidate;
end;

procedure TBCEditorBookmark.SetInternalImage(const AValue: Boolean);
begin
  FInternalImage := AValue;
  Invalidate;
end;

procedure TBCEditorBookmark.SetLine(const AValue: Integer);
begin
  if FVisible and Assigned(FEditor) then
  begin
    if FLine > 0 then
      Invalidate;
    FLine := AValue;
    Invalidate;
  end
  else
    FLine := AValue;
end;

procedure TBCEditorBookmark.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Invalidate;
  end;
end;

{ TBCEditorBookmarkList }

procedure TBCEditorBookmarkList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TBCEditorBookmarkList.GetItem(AIndex: Integer): TBCEditorBookmark;
begin
  Result := TBCEditorBookmark(inherited GetItem(AIndex));
end;

procedure TBCEditorBookmarkList.SetItem(AIndex: Integer; AItem: TBCEditorBookmark);
begin
  inherited SetItem(AIndex, AItem);
end;

constructor TBCEditorBookmarkList.Create(AOwner: TCustomControl);
begin
  inherited Create;
  FEditor := AOwner;
end;

function TBCEditorBookmarkList.First: TBCEditorBookmark;
begin
  Result := TBCEditorBookmark(inherited First);
end;

function TBCEditorBookmarkList.Last: TBCEditorBookmark;
begin
  Result := TBCEditorBookmark(inherited Last);
end;

function TBCEditorBookmarkList.Extract(AItem: TBCEditorBookmark): TBCEditorBookmark;
begin
  Result := TBCEditorBookmark(inherited Extract(AItem));
end;

procedure TBCEditorBookmarkList.ClearLine(ALine: Integer);
var
  i: Integer;
  LMark: TBCEditorBookmark;
begin
  for i := Count - 1 downto 0 do
  begin
    LMark := Items[i];
    if not LMark.IsBookmark and (LMark.Line = ALine) then
      Delete(i);
  end;
end;

procedure TBCEditorBookmarkList.GetMarksForLine(ALine: Integer; var AMarks: TBCEditorBookmarks);
var
  i, j: Integer;
  LMark: TBCEditorBookmark;
begin
  FillChar(AMarks, SizeOf(AMarks), 0);
  j := 0;
  for i := 0 to Count - 1 do
  begin
    LMark := Items[i];
    if LMark.Line = ALine then
    begin
      Inc(j);
      AMarks[j] := LMark;
      if j = BCEDITOR_MAX_BOOKMARKS then
        Break;
    end;
  end;
end;

procedure TBCEditorBookmarkList.Place(AMark: TBCEditorBookmark);
var
  LEditor: TBCBaseEditor;
begin
  LEditor := nil;
  if Assigned(FEditor) and (FEditor is TBCBaseEditor) then
    LEditor := FEditor as TBCBaseEditor;
  if Assigned(LEditor) then
    if Assigned(LEditor.OnBeforeBookmarkPlaced) then
      LEditor.OnBeforeBookmarkPlaced(FEditor, AMark);
  if Assigned(AMark) then
    Add(AMark);
  if Assigned(LEditor) then
    if Assigned(LEditor.OnAfterBookmarkPlaced) then
      LEditor.OnAfterBookmarkPlaced(FEditor);
end;

end.
