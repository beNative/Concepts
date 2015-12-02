unit BCEditor.Editor.CodeFolding.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts, BCEditor.Types;

type
  TBCEditorCodeFoldingColors = class(TPersistent)
  strict private
    FActiveLineBackground: TColor;
    FBackground: TColor;
    FCollapsedLine: TColor;
    FFoldingLine: TColor;
    FFoldingLineHighlight: TColor;
    FIndent: TColor;
    FIndentHighlight: TColor;
    FOnChange: TBCEditorCodeFoldingChangeEvent;
    procedure SetActiveLineBackground(const AValue: TColor);
    procedure SetBackground(const AValue: TColor);
    procedure SetCollapsedLine(const AValue: TColor);
    procedure SetFoldingLine(const AValue: TColor);
    procedure SetFoldingLineHighlight(const AValue: TColor);
    procedure SetIndent(const AValue: TColor);
    procedure SetIndentHighlight(const AValue: TColor);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property ActiveLineBackground: TColor read FActiveLineBackground write SetActiveLineBackground default clActiveLineBackground;
    property CollapsedLine: TColor read FCollapsedLine write SetCollapsedLine default clLeftMarginFontForeground;
    property Background: TColor read FBackground write SetBackground default clLeftMarginBackground;
    property FoldingLine: TColor read FFoldingLine write SetFoldingLine default clLeftMarginFontForeground;
    property FoldingLineHighlight: TColor read FFoldingLineHighlight write SetFoldingLineHighlight default clLeftMarginFontForeground;
    property Indent: TColor read FIndent write SetIndent default clIndent;
    property IndentHighlight: TColor read FIndentHighlight write SetIndentHighlight default clIndentHighlight;
    property OnChange: TBCEditorCodeFoldingChangeEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorCodeFoldingColors }

constructor TBCEditorCodeFoldingColors.Create;
begin
  inherited;

  FActiveLineBackground := clActiveLineBackground;
  FCollapsedLine := clLeftMarginFontForeground;
  FBackground := clLeftMarginBackground;
  FFoldingLine := clLeftMarginFontForeground;
  FFoldingLineHighlight := clLeftMarginFontForeground;
  FIndentHighlight := clIndentHighlight;
end;

procedure TBCEditorCodeFoldingColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCodeFoldingColors then
  with ASource as TBCEditorCodeFoldingColors do
  begin
    Self.FActiveLineBackground := FActiveLineBackground;
    Self.FCollapsedLine := FCollapsedLine;
    Self.FBackground := FBackground;
    Self.FFoldingLine := FFoldingLine;
    Self.FFoldingLineHighlight := FFoldingLineHighlight;
    Self.FIndentHighlight := FIndentHighlight;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCodeFoldingColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(fcRefresh);
end;

procedure TBCEditorCodeFoldingColors.SetActiveLineBackground(const AValue: TColor);
begin
  if FActiveLineBackground <> AValue then
  begin
    FActiveLineBackground := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFoldingColors.SetBackground(const AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFoldingColors.SetFoldingLine(const AValue: TColor);
begin
  if FFoldingLine <> AValue then
  begin
    FFoldingLine := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFoldingColors.SetFoldingLineHighlight(const AValue: TColor);
begin
  if FFoldingLineHighlight <> AValue then
  begin
    FFoldingLineHighlight := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFoldingColors.SetCollapsedLine(const AValue: TColor);
begin
  if FCollapsedLine <> AValue then
  begin
    FCollapsedLine := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFoldingColors.SetIndent(const AValue: TColor);
begin
  if FIndent <> AValue then
  begin
    FIndent := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFoldingColors.SetIndentHighlight(const AValue: TColor);
begin
  if FIndentHighlight <> AValue then
  begin
    FIndentHighlight := AValue;
    DoChange;
  end;
end;

end.
