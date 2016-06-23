unit BCEditor.Editor.Caret.MultiEdit;

interface

uses
  System.Classes, BCEditor.Editor.Caret.MultiEdit.Colors, BCEditor.Types;

type
  TBCEditorCaretMultiEdit = class(TPersistent)
  strict private
    FColors: TBCEditorCaretMultiEditColors;
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorCaretMultiEditOptions;
    FStyle: TBCEditorCaretStyle;
    procedure DoChange;
    procedure SetColors(AValue: TBCEditorCaretMultiEditColors);
    procedure SetEnabled(AValue: Boolean);
    procedure SetOptions(const AValue: TBCEditorCaretMultiEditOptions);
    procedure SetStyle(const AValue: TBCEditorCaretStyle);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Colors: TBCEditorCaretMultiEditColors read FColors write SetColors;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorCaretMultiEditOptions read FOptions write SetOptions default [meoShowActiveLine, meoShowGhost];
    property Style: TBCEditorCaretStyle read FStyle write SetStyle default csThinVerticalLine;
  end;

implementation

constructor TBCEditorCaretMultiEdit.Create;
begin
  inherited;

  FColors := TBCEditorCaretMultiEditColors.Create;
  FEnabled := True;
  FStyle := csThinVerticalLine;
  FOptions := [meoShowActiveLine, meoShowGhost];
end;

destructor TBCEditorCaretMultiEdit.Destroy;
begin
  FColors.Free;

  inherited;
end;

procedure TBCEditorCaretMultiEdit.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaretMultiEdit) then
  with ASource as TBCEditorCaretMultiEdit do
  begin
    Self.FColors.Assign(FColors);
    Self.FEnabled := FEnabled;
    Self.FOptions := FOptions;
    Self.FStyle := FStyle;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaretMultiEdit.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorCaretMultiEdit.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCaretMultiEdit.SetStyle(const AValue: TBCEditorCaretStyle);
begin
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCaretMultiEdit.SetColors(AValue: TBCEditorCaretMultiEditColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorCaretMultiEdit.SetOptions(const AValue: TBCEditorCaretMultiEditOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange;
  end;
end;

end.
