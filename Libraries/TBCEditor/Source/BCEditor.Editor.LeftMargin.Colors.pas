unit BCEditor.Editor.LeftMargin.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts;

type
  TBCEditorLeftMarginColors = class(TPersistent)
  strict private
    FActiveLineBackground: TColor;
    FBackground: TColor;
    FBookmarkBackground: TColor;
    FBookmarkPanelBackground: TColor;
    FBorder: TColor;
    FLineNumberLine: TColor;
    FLineStateModified: TColor;
    FLineStateNormal: TColor;
    FMarkDefaultBackground: TColor;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property ActiveLineBackground: TColor read FActiveLineBackground write FActiveLineBackground default clActiveLineBackground;
    property Background: TColor read FBackground write FBackground default clLeftMarginBackground;
    property BookmarkBackground: TColor read FBookmarkBackground write FBookmarkBackground default clNone;
    property BookmarkPanelBackground: TColor read FBookmarkPanelBackground write FBookmarkPanelBackground default clLeftMarginBackground;
    property Border: TColor read FBorder write FBorder default clLeftMarginBackground;
    property LineNumberLine: TColor read FLineNumberLine write FLineNumberLine default clLeftMarginFontForeground;
    property LineStateModified: TColor read FLineStateModified write FLineStateModified default clYellow;
    property LineStateNormal: TColor read FLineStateNormal write FLineStateNormal default clLime;
    property MarkDefaultBackground: TColor read FMarkDefaultBackground write FMarkDefaultBackground default clNone;
  end;

implementation

constructor TBCEditorLeftMarginColors.Create;
begin
  inherited;

  FActiveLineBackground := clActiveLineBackground;
  FBackground := clLeftMarginBackground;
  FBookmarkBackground := clNone;
  FBookmarkPanelBackground := clLeftMarginBackground;
  FBorder := clLeftMarginBackground;
  FLineNumberLine := clLeftMarginFontForeground;
  FLineStateModified := clYellow;
  FLineStateNormal := clLime;
  FMarkDefaultBackground := clNone;
end;

procedure TBCEditorLeftMarginColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorLeftMarginColors then
  with ASource as TBCEditorLeftMarginColors do
  begin
    Self.FActiveLineBackground := FActiveLineBackground;
    Self.FBackground := FBackground;
    Self.FBookmarkPanelBackground := FBookmarkPanelBackground;
    Self.FBorder := FBorder;
    Self.FLineNumberLine := FLineNumberLine;
    Self.FLineStateModified := FLineStateModified;
    Self.FLineStateNormal := FLineStateNormal;
    Self.FMarkDefaultBackground := FMarkDefaultBackground;
  end
  else
    inherited Assign(ASource);
end;

end.
