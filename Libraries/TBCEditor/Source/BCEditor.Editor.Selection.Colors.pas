unit BCEditor.Editor.Selection.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts;

type
  TBCEditorSelectedColor = class(TPersistent)
  strict private
    FBackground: TColor;
    FForeground: TColor;
    FOnChange: TNotifyEvent;
    procedure SetBackground(AValue: TColor);
    procedure SetForeground(AValue: TColor);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Background: TColor read FBackground write SetBackground default clSelectionColor;
    property Foreground: TColor read FForeground write SetForeground default clHighLightText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorSelectedColor }

constructor TBCEditorSelectedColor.Create;
begin
  inherited;

  FBackground := clSelectionColor;
  FForeground := clHighLightText;
end;

procedure TBCEditorSelectedColor.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSelectedColor) then
  with ASource as TBCEditorSelectedColor do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSelectedColor.SetBackground(AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TBCEditorSelectedColor.SetForeground(AValue: TColor);
begin
  if FForeground <> AValue then
  begin
    FForeground := AValue;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

end.
