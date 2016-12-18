unit BCEditor.Highlighter.Attributes;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Graphics, BCEditor.Consts;

type
  TBCEditorHighlighterAttribute = class(TPersistent)
  strict private
    FBackground: TColor;
    FBackgroundDefault: TColor;
    FElement: string;
    FEscapeChar: Char;
    FForeground: TColor;
    FForegroundDefault: TColor;
    FName: string;
    FOnChange: TNotifyEvent;
    FParentBackground: Boolean;
    FParentForeground: Boolean;
    FFontStyles: TFontStyles;
    FFontStylesDefault: TFontStyles;
    function GetBackgroundColorStored: Boolean;
    function GetFontStylesStored: Boolean;
    function GetForegroundColorStored: Boolean;
    procedure Changed; virtual;
    procedure SetBackground(AValue: TColor);
    procedure SetForeground(AValue: TColor);
    procedure SetFontStyles(AValue: TFontStyles);
  public
    constructor Create(const AttributeName: string);
    procedure Assign(ASource: TPersistent); override;
    procedure AssignColorAndStyle(ASource: TBCEditorHighlighterAttribute);
    procedure InternalSaveDefaultValues;
  public
    property Name: string read FName write FName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Background: TColor read FBackground write SetBackground stored GetBackgroundColorStored;
    property Element: string read FElement write FElement;
    property EscapeChar: Char read FEscapeChar write FEscapeChar default BCEDITOR_NONE_CHAR;
    property Foreground: TColor read FForeground write SetForeground stored GetForegroundColorStored;
    property ParentForeground: Boolean read FParentForeground write FParentForeground;
    property ParentBackground: Boolean read FParentBackground write FParentBackground;
    property FontStyles: TFontStyles read FFontStyles write SetFontStyles stored GetFontStylesStored;
  end;

implementation

uses
  System.SysUtils;

constructor TBCEditorHighlighterAttribute.Create(const AttributeName: string);
begin
  inherited Create;

  FBackground := clNone;
  FForeground := clNone;
  FName := AttributeName;
  FEscapeChar := BCEDITOR_NONE_CHAR;
end;

constructor TBCEditorHighlighterAttribute.Create(const AttributeName: string);
begin
  inherited Create;

  FBackground := clNone;
  FForeground := clNone;
  FName := AttributeName;
  FEscapeChar := BCEDITOR_NONE_CHAR;
end;

procedure TBCEditorHighlighterAttribute.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorHighlighterAttribute) then
    with ASource as TBCEditorHighlighterAttribute do
    begin
      Self.FName := FName;
      Self.AssignColorAndStyle(ASource as TBCEditorHighlighterAttribute);
    end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorHighlighterAttribute.AssignColorAndStyle(ASource: TBCEditorHighlighterAttribute);
var
  IsChanged: Boolean;
begin
  IsChanged := False;
  if FBackground <> ASource.FBackground then
  begin
    FBackground := ASource.FBackground;
    IsChanged := True;
  end;
  if FForeground <> ASource.FForeground then
  begin
    FForeground := ASource.FForeground;
    IsChanged := True;
  end;
  if FFontStyles <> ASource.FFontStyles then
  begin
    FFontStyles := ASource.FFontStyles;
    IsChanged := True;
  end;
  FParentForeground := ASource.ParentForeground;
  FParentBackground := ASource.ParentBackground;
  if IsChanged then
    Changed;
end;

procedure TBCEditorHighlighterAttribute.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TBCEditorHighlighterAttribute.GetBackgroundColorStored: Boolean;
begin
  Result := FBackground <> FBackgroundDefault;
end;

function TBCEditorHighlighterAttribute.GetForegroundColorStored: Boolean;
begin
  Result := FForeground <> FForegroundDefault;
end;

function TBCEditorHighlighterAttribute.GetFontStylesStored: Boolean;
begin
  Result := FFontStyles <> FFontStylesDefault;
end;

procedure TBCEditorHighlighterAttribute.InternalSaveDefaultValues;
begin
  FForegroundDefault := FForeground;
  FBackgroundDefault := FBackground;
  FFontStylesDefault := FFontStyles;
end;

procedure TBCEditorHighlighterAttribute.SetBackground(AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    Changed;
  end;
end;

procedure TBCEditorHighlighterAttribute.SetForeground(AValue: TColor);
begin
  if FForeground <> AValue then
  begin
    FForeground := AValue;
    Changed;
  end;
end;

procedure TBCEditorHighlighterAttribute.SetFontStyles(AValue: TFontStyles);
begin
  if FFontStyles <> AValue then
  begin
    FFontStyles := AValue;
    Changed;
  end;
end;

end.
