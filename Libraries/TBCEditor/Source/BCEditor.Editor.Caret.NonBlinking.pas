unit BCEditor.Editor.Caret.NonBlinking;

interface

uses
  System.Classes, BCEditor.Editor.Caret.NonBlinking.Colors;

type
  TBCEditorCaretNonBlinking = class(TPersistent)
  strict private
    FColors: TBCEditorCaretNonBlinkingColors;
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    procedure DoChange;
    procedure SetColors(AValue: TBCEditorCaretNonBlinkingColors);
    procedure SetEnabled(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Colors: TBCEditorCaretNonBlinkingColors read FColors write SetColors;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

constructor TBCEditorCaretNonBlinking.Create;
begin
  inherited;

  FColors := TBCEditorCaretNonBlinkingColors.Create;
  FEnabled := False;
end;

destructor TBCEditorCaretNonBlinking.Destroy;
begin
  FColors.Free;

  inherited;
end;

procedure TBCEditorCaretNonBlinking.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaretNonBlinking) then
  with ASource as TBCEditorCaretNonBlinking do
  begin
    Self.FColors.Assign(FColors);
    Self.FEnabled := FEnabled;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaretNonBlinking.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorCaretNonBlinking.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCaretNonBlinking.SetColors(AValue: TBCEditorCaretNonBlinkingColors);
begin
  FColors.Assign(AValue);
end;

end.
