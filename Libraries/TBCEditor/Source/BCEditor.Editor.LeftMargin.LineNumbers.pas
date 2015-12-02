unit BCEditor.Editor.LeftMargin.LineNumbers;

interface

uses
  System.Classes, BCEditor.Types;

type
  TBCEditorLeftMarginLineNumbers = class(TPersistent)
  strict private
    FAutosizeDigitCount: Integer;
    FDigitCount: Integer;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorLeftMarginLineNumberOptions;
    FStartFrom: Integer;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetDigitCount(AValue: Integer);
    procedure SetOptions(const AValue: TBCEditorLeftMarginLineNumberOptions);
    procedure SetStartFrom(const AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
    property AutosizeDigitCount: Integer read FAutosizeDigitCount write FAutosizeDigitCount;
  published
    property DigitCount: Integer read FDigitCount write SetDigitCount default 4;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorLeftMarginLineNumberOptions read FOptions write SetOptions default [lnoIntens];
    property StartFrom: Integer read FStartFrom write SetStartFrom default 1;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

uses
  BCEditor.Utils;

{ TBCEditorLeftMarginLineNumbers }

constructor TBCEditorLeftMarginLineNumbers.Create;
begin
  inherited;

  FAutosizeDigitCount := 4;
  FDigitCount := 4;
  FOptions := [lnoIntens];
  FStartFrom := 1;
  FVisible := True;
end;

procedure TBCEditorLeftMarginLineNumbers.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMarginLineNumbers) then
  with ASource as TBCEditorLeftMarginLineNumbers do
  begin
    Self.FAutosizeDigitCount := FAutosizeDigitCount;
    Self.FDigitCount := FDigitCount;
    Self.FOptions := FOptions;
    Self.FStartFrom := FStartFrom;
    Self.FVisible := FVisible;

    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMarginLineNumbers.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginLineNumbers.SetDigitCount(AValue: Integer);
begin
  AValue := MinMax(AValue, 2, 12);
  if FDigitCount <> AValue then
  begin
    FDigitCount := AValue;
    FAutosizeDigitCount := FDigitCount;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginLineNumbers.SetOptions(const AValue: TBCEditorLeftMarginLineNumberOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginLineNumbers.SetStartFrom(const AValue: Integer);
begin
  if FStartFrom <> AValue then
  begin
    FStartFrom := AValue;
    if FStartFrom < 0 then
      FStartFrom := 0;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginLineNumbers.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange
  end;
end;

end.
