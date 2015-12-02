unit BCEditor.Editor.ActiveLine;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Editor.Glyph, BCEditor.Consts;

type
  TBCEditorActiveLine = class(TPersistent)
  strict private
    FColor: TColor;
    FIndicator: TBCEditorGlyph;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    procedure DoChange(Sender: TObject);
    procedure SetColor(const AValue: TColor);
    procedure SetIndicator(const AValue: TBCEditorGlyph);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clActiveLineBackground;
    property Indicator: TBCEditorGlyph read FIndicator write SetIndicator;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

{ TBCEditorActiveLine }

constructor TBCEditorActiveLine.Create;
begin
  inherited;

  FColor := clActiveLineBackground;
  FIndicator := TBCEditorGlyph.Create(HINSTANCE, 'BCEDITORACTIVELINE', clFuchsia);
  FIndicator.Visible := False;
  FVisible := True;
end;

destructor TBCEditorActiveLine.Destroy;
begin
  FIndicator.Free;

  inherited;
end;

procedure TBCEditorActiveLine.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorActiveLine) then
  with ASource as TBCEditorActiveLine do
  begin
    Self.FColor := FColor;
    Self.FVisible := FVisible;
    Self.FIndicator.Assign(FIndicator);
    Self.DoChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorActiveLine.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FIndicator.OnChange := AValue;
end;

procedure TBCEditorActiveLine.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

procedure TBCEditorActiveLine.SetColor(const AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    DoChange(Self);
  end;
end;

procedure TBCEditorActiveLine.SetIndicator(const AValue: TBCEditorGlyph);
begin
  FIndicator.Assign(AValue);
end;

procedure TBCEditorActiveLine.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange(Self);
  end;
end;

end.
