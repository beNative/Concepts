unit BCEditor.Editor.Minimap.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts;

type
  TBCEditorMinimapColors = class(TPersistent)
  strict private
    FBookmark: TColor;
    FVisibleLines: TColor;
    FOnChange: TNotifyEvent;
    procedure SetBookmark(const AValue: TColor);
    procedure SetVisibleLines(const AValue: TColor);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property VisibleLines: TColor read FVisibleLines write SetVisibleLines default clMinimapVisibleLines;
    property Bookmark: TColor read FBookmark write SetBookmark default clMinimapBookmark;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorMinimapColors }

constructor TBCEditorMinimapColors.Create;
begin
  inherited;

  FBookmark := clMinimapBookmark;
  FVisibleLines := clMinimapVisibleLines;
end;

procedure TBCEditorMinimapColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorMinimapColors then
  with ASource as TBCEditorMinimapColors do
  begin
    Self.FBookmark := FBookmark;
    Self.FVisibleLines := FVisibleLines;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorMinimapColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorMinimapColors.SetBookmark(const AValue: TColor);
begin
  if FBookmark <> AValue then
  begin
    FBookmark := AValue;
    DoChange;
  end;
end;

procedure TBCEditorMinimapColors.SetVisibleLines(const AValue: TColor);
begin
  if FVisibleLines <> AValue then
  begin
    FVisibleLines := AValue;
    DoChange;
  end;
end;

end.
