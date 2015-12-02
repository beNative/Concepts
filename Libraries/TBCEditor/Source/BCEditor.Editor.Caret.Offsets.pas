unit BCEditor.Editor.Caret.Offsets;

interface

uses
  System.Classes;

type
  TBCEditorCaretOffsets = class(TPersistent)
  strict private
    FOnChange: TNotifyEvent;
    FX: Integer;
    FY: Integer;
    procedure DoChange(Sender: TObject);
    procedure SetX(AValue: Integer);
    procedure SetY(AValue: Integer);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property X: Integer read FX write SetX default 0;
    property Y: Integer read FY write SetY default 0;
  end;

implementation

{ TBCEditorCaretOffsets }

constructor TBCEditorCaretOffsets.Create;
begin
  inherited;

  FX := 0;
  FY := 0;
end;

procedure TBCEditorCaretOffsets.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaretOffsets) then
  with ASource as TBCEditorCaretOffsets do
  begin
    Self.FX := FX;
    Self.FY := FY;
    Self.DoChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaretOffsets.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

procedure TBCEditorCaretOffsets.SetX(AValue: Integer);
begin
  if FX <> AValue then
  begin
    FX := AValue;
    DoChange(Self);
  end;
end;

procedure TBCEditorCaretOffsets.SetY(AValue: Integer);
begin
  if FY <> AValue then
  begin
    FY := AValue;
    DoChange(Self);
  end;
end;

end.
