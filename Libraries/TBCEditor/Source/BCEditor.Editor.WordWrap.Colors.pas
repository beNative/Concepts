unit BCEditor.Editor.WordWrap.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts;

type
  TBCEditorWordWrapColors = class(TPersistent)
  strict private
    FArrow: TColor;
    FLines: TColor;
    FOnChange: TNotifyEvent;
    procedure SetArrow(const AValue: TColor);
    procedure SetLines(const AValue: TColor);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Arrow: TColor read FArrow write SetArrow default clWordWrapIndicatorArrow;
    property Lines: TColor read FLines write SetLines default clWordWrapIndicatorLines;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorCodeFoldingColors }

constructor TBCEditorWordWrapColors.Create;
begin
  inherited;

  FArrow := clWordWrapIndicatorArrow;
  FLines := clWordWrapIndicatorLines;
end;

procedure TBCEditorWordWrapColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorWordWrapColors then
  with ASource as TBCEditorWordWrapColors do
  begin
    Self.FArrow := FArrow;
    Self.FLines := FLines;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorWordWrapColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorWordWrapColors.SetArrow(const AValue: TColor);
begin
  if FArrow <> AValue then
  begin
    FArrow := AValue;
    DoChange;
  end;
end;

procedure TBCEditorWordWrapColors.SetLines(const AValue: TColor);
begin
  if FLines <> AValue then
  begin
    FLines := AValue;
    DoChange;
  end;
end;

end.
