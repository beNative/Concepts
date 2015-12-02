unit BCEditor.Editor.Search.Map.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts, BCEditor.Types;

type
  TBCEditorSearchMapColors = class(TPersistent)
  strict private
    FActiveLine: TColor;
    FBackground: TColor;
    FForeground: TColor;
    FOnChange: TBCEditorSearchChangeEvent;
    procedure SetActiveLine(AValue: TColor);
    procedure SetBackground(AValue: TColor);
    procedure SetForeground(AValue: TColor);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property ActiveLine: TColor read FActiveLine write SetActiveLine default clLeftMarginBookmarkBackground;
    property Background: TColor read FBackground write SetBackground default clLeftMarginBackground;
    property Foreground: TColor read FForeground write SetForeground default clSearchHighlighter;
    property OnChange: TBCEditorSearchChangeEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorSelectedColor }

constructor TBCEditorSearchMapColors.Create;
begin
  inherited;

  FActiveLine := clLeftMarginBookmarkBackground;
  FBackground := clLeftMarginBackground;
  FForeground := clSearchHighlighter;
end;

procedure TBCEditorSearchMapColors.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSearchMapColors) then
  with ASource as TBCEditorSearchMapColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
    Self.FActiveLine := FActiveLine;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(scRefresh);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearchMapColors.SetActiveLine(AValue: TColor);
begin
  if FActiveLine <> AValue then
  begin
    FActiveLine := AValue;
    if Assigned(FOnChange) then
      FOnChange(scRefresh);
  end;
end;

procedure TBCEditorSearchMapColors.SetBackground(AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    if Assigned(FOnChange) then
      FOnChange(scRefresh);
  end;
end;

procedure TBCEditorSearchMapColors.SetForeground(AValue: TColor);
begin
  if FForeground <> AValue then
  begin
    FForeground := AValue;
    if Assigned(FOnChange) then
      FOnChange(scRefresh);
  end;
end;

end.
