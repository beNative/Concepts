unit BCEditor.Editor.LeftMargin.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts;

type
  TBCEditorLeftMarginColors = class(TPersistent)
  strict private
    FActiveLineBackground: TColor;
    FBackground: TColor;
    FBookmarkPanelBackground: TColor;
    FBorder: TColor;
    FLineNumberLine: TColor;
    FLineStateModified: TColor;
    FLineStateNormal: TColor;
    FOnChange: TNotifyEvent;
    procedure SetActiveLineBackground(const AValue: TColor);
    procedure SetBackground(const AValue: TColor);
    procedure SetBookmarkPanelBackground(const AValue: TColor);
    procedure SetBorder(const AValue: TColor);
    procedure SetLineNumberLine(const AValue: TColor);
    procedure SetLineStateModified(const AValue: TColor);
    procedure SetLineStateNormal(const AValue: TColor);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property ActiveLineBackground: TColor read FActiveLineBackground write SetActiveLineBackground default clActiveLineBackground;
    property Background: TColor read FBackground write SetBackground default clLeftMarginBackground;
    property BookmarkPanelBackground: TColor read FBookmarkPanelBackground write SetBookmarkPanelBackground default clLeftMarginBackground;
    property Border: TColor read FBorder write SetBorder default clLeftMarginBackground;
    property LineNumberLine: TColor read FLineNumberLine write SetLineNumberLine default clLeftMarginFontForeground;
    property LineStateModified: TColor read FLineStateModified write SetLineStateModified default clYellow;
    property LineStateNormal: TColor read FLineStateNormal write SetLineStateNormal default clLime;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorLeftMarginColors }

constructor TBCEditorLeftMarginColors.Create;
begin
  inherited;

  FActiveLineBackground := clActiveLineBackground;
  FBackground := clLeftMarginBackground;
  FBookmarkPanelBackground := clLeftMarginBackground;
  FBorder := clLeftMarginBackground;
  FLineNumberLine := clLeftMarginFontForeground;
  FLineStateModified := clYellow;
  FLineStateNormal := clLime;
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
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMarginColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginColors.SetActiveLineBackground(const AValue: TColor);
begin
  if FActiveLineBackground <> AValue then
  begin
    FActiveLineBackground := AValue;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginColors.SetBackground(const AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginColors.SetBookmarkPanelBackground(const AValue: TColor);
begin
  if FBookmarkPanelBackground <> AValue then
  begin
    FBookmarkPanelBackground := AValue;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginColors.SetBorder(const AValue: TColor);
begin
  if FBorder <> AValue then
  begin
    FBorder := AValue;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginColors.SetLineNumberLine(const AValue: TColor);
begin
  if FLineNumberLine <> AValue then
  begin
    FLineNumberLine := AValue;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginColors.SetLineStateModified(const AValue: TColor);
begin
  if FLineStateModified <> AValue then
  begin
    FLineStateModified := AValue;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginColors.SetLineStateNormal(const AValue: TColor);
begin
  if FLineStateNormal <> AValue then
  begin
    FLineStateNormal := AValue;
    DoChange;
  end;
end;

end.
