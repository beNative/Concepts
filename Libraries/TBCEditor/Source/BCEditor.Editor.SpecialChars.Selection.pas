unit BCEditor.Editor.SpecialChars.Selection;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorSpecialCharsSelection = class(TPersistent)
  strict private
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetColor(const AValue: TColor);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

implementation

{ TBCEditorSpecialCharsSelection }

constructor TBCEditorSpecialCharsSelection.Create;
begin
  inherited;

  FColor := clBlack;
  FVisible := False;
end;

procedure TBCEditorSpecialCharsSelection.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSpecialCharsSelection) then
  with ASource as TBCEditorSpecialCharsSelection do
  begin
    Self.FColor := FColor;
    Self.FVisible := FVisible;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSpecialCharsSelection.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorSpecialCharsSelection.SetColor(const AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSpecialCharsSelection.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

end.
