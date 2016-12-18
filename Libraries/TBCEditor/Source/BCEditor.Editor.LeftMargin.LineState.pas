unit BCEditor.Editor.LeftMargin.LineState;

interface

uses
  System.Classes;

type
  TBCEditorLeftMarginLineState = class(TPersistent)
  strict private
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    FWidth: Integer;
    procedure DoChange;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetWidth(const AValue: Integer);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Width: Integer read FWidth write SetWidth default 2;
  end;

implementation

constructor TBCEditorLeftMarginLineState.Create;
begin
  inherited;

  FEnabled := True;
  FWidth := 2;
end;

procedure TBCEditorLeftMarginLineState.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMarginLineState) then
  with ASource as TBCEditorLeftMarginLineState do
  begin
    Self.FEnabled := FEnabled;
    Self.FWidth := FWidth;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMarginLineState.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

procedure TBCEditorLeftMarginLineState.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginLineState.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginLineState.SetWidth(const AValue: Integer);
begin
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    DoChange
  end;
end;

end.
