unit BCEditor.Editor.SpecialChars;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Editor.SpecialChars.Selection, BCEditor.Editor.SpecialChars.EndOfLine,
  BCEditor.Types;

type
  TBCEditorSpecialChars = class(TPersistent)
  strict private
    FColor: TColor;
    FEndOfLine: TBCEditorSpecialCharsEndOfLine;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorSpecialCharsOptions;
    FSelection: TBCEditorSpecialCharsSelection;
    FStyle: TBCEditorSpecialCharsStyle;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetColor(const AValue: TColor);
    procedure SetEndOfLine(const AValue: TBCEditorSpecialCharsEndOfLine);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOptions(const AValue: TBCEditorSpecialCharsOptions);
    procedure SetSelection(const AValue: TBCEditorSpecialCharsSelection);
    procedure SetStyle(const AValue: TBCEditorSpecialCharsStyle);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property EndOfLine: TBCEditorSpecialCharsEndOfLine read FEndOfLine write SetEndOfLine;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Options: TBCEditorSpecialCharsOptions read FOptions write SetOptions default [scoUseTextColor];
    property Selection: TBCEditorSpecialCharsSelection read FSelection write SetSelection;
    property Style: TBCEditorSpecialCharsStyle read FStyle write SetStyle;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

implementation

{ TBCEditorSpecialChars }

constructor TBCEditorSpecialChars.Create;
begin
  inherited;

  FColor := clBlack;
  FEndOfLine := TBCEditorSpecialCharsEndOfLine.Create;
  FSelection := TBCEditorSpecialCharsSelection.Create;
  FVisible := False;
  FOptions := [scoUseTextColor];
end;

destructor TBCEditorSpecialChars.Destroy;
begin
  FEndOfLine.Free;
  FSelection.Free;
  inherited Destroy;
end;

procedure TBCEditorSpecialChars.SetOnChange(const AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FEndOfLine.OnChange := FOnChange;
  FSelection.OnChange := FOnChange;
end;

procedure TBCEditorSpecialChars.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSpecialChars) then
  with ASource as TBCEditorSpecialChars do
  begin
    Self.FColor := FColor;
    Self.FEndOfLine.Assign(FEndOfLine);
    Self.FOptions := FOptions;
    Self.FSelection.Assign(FSelection);
    Self.FVisible := FVisible;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSpecialChars.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorSpecialChars.SetColor(const AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSpecialChars.SetEndOfLine(const AValue: TBCEditorSpecialCharsEndOfLine);
begin
  FEndOfLine.Assign(AValue);
end;

procedure TBCEditorSpecialChars.SetSelection(const AValue: TBCEditorSpecialCharsSelection);
begin
  FSelection.Assign(AValue);
end;

procedure TBCEditorSpecialChars.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSpecialChars.SetStyle(const AValue: TBCEditorSpecialCharsStyle);
begin
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSpecialChars.SetOptions(const AValue: TBCEditorSpecialCharsOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange;
  end;
end;

end.
