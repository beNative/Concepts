unit BCEditor.Editor.CodeFolding.Hint;

interface

uses
  System.Classes, System.UITypes, Vcl.Graphics, BCEditor.Editor.CodeFolding.Hint.Colors,
  BCEditor.Editor.CodeFolding.Hint.Indicator;

type
  TBCEditorCodeFoldingHint = class(TPersistent)
  strict private
    FColors: TBCEditorCodeFoldingHintColors;
    FCursor: TCursor;
    FFont: TFont;
    FIndicator: TBCEditorCodeFoldingHintIndicator;
    FRowCount: Integer;
    FVisible: Boolean;
    procedure SetFont(const AValue: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Colors: TBCEditorCodeFoldingHintColors read FColors write FColors;
    property Cursor: TCursor read FCursor write FCursor default crHelp;
    property Font: TFont read FFont write SetFont;
    property Indicator: TBCEditorCodeFoldingHintIndicator read FIndicator write FIndicator;
    property RowCount: Integer read FRowCount write FRowCount default 40;
    property Visible: Boolean read FVisible write FVisible default True;
  end;

implementation

constructor TBCEditorCodeFoldingHint.Create;
begin
  inherited;

  FColors := TBCEditorCodeFoldingHintColors.Create;
  FIndicator := TBCEditorCodeFoldingHintIndicator.Create;
  FCursor := crHelp;
  FRowCount := 40;
  FVisible := True;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
end;

destructor TBCEditorCodeFoldingHint.Destroy;
begin
  FColors.Free;
  FIndicator.Free;
  FFont.Free;

  inherited;
end;

procedure TBCEditorCodeFoldingHint.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCodeFoldingHint then
  with ASource as TBCEditorCodeFoldingHint do
  begin
    Self.FColors.Assign(FColors);
    Self.FIndicator.Assign(FIndicator);
    Self.FCursor := FCursor;
    Self.FFont.Assign(FFont);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCodeFoldingHint.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

end.
