unit BCEditor.Editor.Search.Highlighter;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Editor.Search.Highlighter.Colors, BCEditor.Types;

type
  TBCEditorSearchHighlighter = class(TPersistent)
  strict private
    FAlphaBlending: Byte;
    FColors: TBCEditorSearchColors;
    FOnChange: TBCEditorSearchChangeEvent;
    procedure SetAlphaBlending(const AValue: Byte);
    procedure SetColors(const AValue: TBCEditorSearchColors);
    procedure DoChange;
    procedure SetOnChange(AValue: TBCEditorSearchChangeEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property AlphaBlending: Byte read FAlphaBlending write SetAlphaBlending default 255;
    property Colors: TBCEditorSearchColors read FColors write SetColors;
    property OnChange: TBCEditorSearchChangeEvent read FOnChange write SetOnChange;
  end;

implementation

{ TBCEditorSearchHighlighter }

constructor TBCEditorSearchHighlighter.Create;
begin
  inherited;

  FAlphaBlending := 255;
  FColors := TBCEditorSearchColors.Create;
end;

destructor TBCEditorSearchHighlighter.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TBCEditorSearchHighlighter.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSearchHighlighter) then
  with ASource as TBCEditorSearchHighlighter do
  begin
    Self.FAlphaBlending := FAlphaBlending;
    Self.FColors.Assign(Colors);
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearchHighlighter.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(scRefresh);
end;

procedure TBCEditorSearchHighlighter.SetOnChange(AValue: TBCEditorSearchChangeEvent);
begin
  FOnChange := AValue;
  FColors.OnChange := FOnChange;
end;

procedure TBCEditorSearchHighlighter.SetColors(const AValue: TBCEditorSearchColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorSearchHighlighter.SetAlphaBlending(const AValue: Byte);
begin
  if FAlphaBlending <> AValue then
  begin
    FAlphaBlending := AValue;
    DoChange;
  end;
end;

end.
