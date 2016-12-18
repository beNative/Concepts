unit BCEditor.Editor.RightMargin.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorRightMarginColors = class(TPersistent)
  strict private
    FEdge: TColor;
    FMovingEdge: TColor;
    FOnChange: TNotifyEvent;
    procedure SetEdge(AValue: TColor);
    procedure SetMovingEdge(AValue: TColor);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Edge: TColor read FEdge write SetEdge default clSilver;
    property MovingEdge: TColor read FMovingEdge write SetMovingEdge default clSilver;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

constructor TBCEditorRightMarginColors.Create;
begin
  inherited;

  FEdge := clSilver;
  FMovingEdge := clSilver;
end;

procedure TBCEditorRightMarginColors.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorRightMarginColors) then
  with ASource as TBCEditorRightMarginColors do
  begin
    Self.FEdge := FEdge;
    Self.FMovingEdge := FMovingEdge;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorRightMarginColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorRightMarginColors.SetEdge(AValue: TColor);
begin
  if FEdge <> AValue then
  begin
    FEdge := AValue;
    DoChange;
  end;
end;

procedure TBCEditorRightMarginColors.SetMovingEdge(AValue: TColor);
begin
  if FMovingEdge <> AValue then
  begin
    FMovingEdge := AValue;
    DoChange;
  end;
end;

end.
