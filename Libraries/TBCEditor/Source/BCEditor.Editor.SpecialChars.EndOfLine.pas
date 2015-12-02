unit BCEditor.Editor.SpecialChars.EndOfLine;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Types;

type
  TBCEditorSpecialCharsEndOfLine = class(TPersistent)
  strict private
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FStyle: TBCEditorSpecialCharsEndOfLineStyle;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetColor(const AValue: TColor);
    procedure SetStyle(const AValue: TBCEditorSpecialCharsEndOfLineStyle);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Style: TBCEditorSpecialCharsEndOfLineStyle read FStyle write SetStyle default eolArrow;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

implementation

{ TBCEditorSpecialCharsEndOfLine }

constructor TBCEditorSpecialCharsEndOfLine.Create;
begin
  inherited;

  FColor := clBlack;
  FStyle := eolArrow;
  FVisible := False;
end;

procedure TBCEditorSpecialCharsEndOfLine.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSpecialCharsEndOfLine) then
  with ASource as TBCEditorSpecialCharsEndOfLine do
  begin
    Self.FColor := FColor;
    Self.FStyle := FStyle;
    Self.FVisible := FVisible;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSpecialCharsEndOfLine.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorSpecialCharsEndOfLine.SetColor(const AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSpecialCharsEndOfLine.SetStyle(const AValue: TBCEditorSpecialCharsEndOfLineStyle);
begin
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSpecialCharsEndOfLine.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

end.
