unit BCEditor.Editor.Scroll.Shadow;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorScrollShadow = class(TPersistent)
  strict private
    FAlphaBlending: Byte;
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    FWidth: Integer;
    procedure DoChange;
    procedure SetAlphaBlending(const AValue: Byte);
    procedure SetColor(const AValue: TColor);
    procedure SetVisible(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property AlphaBlending: Byte read FAlphaBlending write SetAlphaBlending default 96;
    property Color: TColor read FColor write SetColor default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read FWidth write SetWidth default 8;
  end;

implementation

constructor TBCEditorScrollShadow.Create;
begin
  inherited;

  FAlphaBlending := 96;
  FColor := clBlack;
  FVisible := True;
  FWidth := 8;
end;

procedure TBCEditorScrollShadow.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorScrollShadow) then
  with ASource as TBCEditorScrollShadow do
  begin
    Self.FAlphaBlending := FAlphaBlending;
    Self.FColor := FColor;
    Self.FVisible := FVisible;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorScrollShadow.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorScrollShadow.SetAlphaBlending(const AValue: Byte);
begin
  if FAlphaBlending <> AValue then
  begin
    FAlphaBlending := AValue;
    DoChange;
  end;
end;

procedure TBCEditorScrollShadow.SetColor(const AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    DoChange;
  end;
end;

procedure TBCEditorScrollShadow.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

procedure TBCEditorScrollShadow.SetWidth(const AValue: Integer);
begin
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    DoChange;
  end;
end;

end.
