unit BCEditor.Editor.Caret.Styles;

interface

uses
  System.Classes, BCEditor.Types;

type
  TBCEditorCaretStyles = class(TPersistent)
  strict private
    FInsert: TBCEditorCaretStyle;
    FOnChange: TNotifyEvent;
    FOverwrite: TBCEditorCaretStyle;
    procedure DoChange;
    procedure SetInsert(const AValue: TBCEditorCaretStyle);
    procedure SetOverwrite(const AValue: TBCEditorCaretStyle);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Insert: TBCEditorCaretStyle read FInsert write SetInsert default csThinVerticalLine;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Overwrite: TBCEditorCaretStyle read FOverwrite write SetOverwrite default csThinVerticalLine;
  end;

implementation

{ TBCEditorCaretStyle }

constructor TBCEditorCaretStyles.Create;
begin
  inherited;

  FInsert := csThinVerticalLine;
  FOverwrite := csThinVerticalLine;
end;

procedure TBCEditorCaretStyles.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaretStyles) then
  with ASource as TBCEditorCaretStyles do
  begin
    Self.FOverwrite := FOverwrite;
    Self.FInsert := FInsert;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaretStyles.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorCaretStyles.SetInsert(const AValue: TBCEditorCaretStyle);
begin
  if FInsert <> AValue then
  begin
    FInsert := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCaretStyles.SetOverwrite(const AValue: TBCEditorCaretStyle);
begin
  if FOverwrite <> AValue then
  begin
    FOverwrite := AValue;
    DoChange;
  end;
end;

end.
