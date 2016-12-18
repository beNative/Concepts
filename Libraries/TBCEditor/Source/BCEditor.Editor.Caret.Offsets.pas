unit BCEditor.Editor.Caret.Offsets;

interface

uses
  System.Classes;

type
  TBCEditorCaretOffsets = class(TPersistent)
  strict private
    FOnChange: TNotifyEvent;
    FLeft: Integer;
    FTop: Integer;
    procedure DoChange(ASender: TObject);
    procedure SetLeft(const AValue: Integer);
    procedure SetTop(const AValue: Integer);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Left: Integer read FLeft write SetLeft default 0;
    property Top: Integer read FTop write SetTop default 0;
  end;

implementation

constructor TBCEditorCaretOffsets.Create;
begin
  inherited;

  FLeft := 0;
  FTop := 0;
end;

procedure TBCEditorCaretOffsets.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaretOffsets) then
  with ASource as TBCEditorCaretOffsets do
  begin
    Self.FLeft := FLeft;
    Self.FTop := FTop;
    Self.DoChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaretOffsets.DoChange(ASender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(ASender);
end;

procedure TBCEditorCaretOffsets.SetLeft(const AValue: Integer);
begin
  if FLeft <> AValue then
  begin
    FLeft := AValue;
    DoChange(Self);
  end;
end;

procedure TBCEditorCaretOffsets.SetTop(const AValue: Integer);
begin
  if FTop <> AValue then
  begin
    FTop := AValue;
    DoChange(Self);
  end;
end;

end.
