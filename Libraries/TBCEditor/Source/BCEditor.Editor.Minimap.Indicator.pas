unit BCEditor.Editor.Minimap.Indicator;

interface

uses
  System.Classes, BCEditor.Types;

type
  TBCEditorMinimapIndicator = class(TPersistent)
  strict private
    FAlphaBlending: Byte;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorMinimapIndicatorOptions;
    procedure DoChange;
    procedure SetAlphaBlending(const AValue: Byte);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorMinimapIndicatorOption; const AEnabled: Boolean);
  published
    property AlphaBlending: Byte read FAlphaBlending write SetAlphaBlending default 96;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorMinimapIndicatorOptions read FOptions write FOptions default [];
  end;

implementation

constructor TBCEditorMinimapIndicator.Create;
begin
  inherited;

  FAlphaBlending := 96;
  FOptions := [];
end;

procedure TBCEditorMinimapIndicator.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorMinimapIndicator) then
  with ASource as TBCEditorMinimapIndicator do
  begin
    Self.FAlphaBlending := FAlphaBlending;
    Self.FOptions := FOptions;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorMinimapIndicator.SetOption(const AOption: TBCEditorMinimapIndicatorOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;


procedure TBCEditorMinimapIndicator.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorMinimapIndicator.SetAlphaBlending(const AValue: Byte);
begin
  if FAlphaBlending <> AValue then
  begin
    FAlphaBlending := AValue;
    DoChange;
  end;
end;

end.
