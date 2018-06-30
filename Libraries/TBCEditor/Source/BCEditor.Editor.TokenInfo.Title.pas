unit BCEditor.Editor.TokenInfo.Title;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Editor.TokenInfo.Title.Colors;

type
  TBCEditorTokenInfoTitle = class(TPersistent)
  strict private
    FColors: TBCEditorTokenInfoTitleColors;
    FFont: TFont;
    FVisible: Boolean;
    procedure SetFont(const AValue: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Colors: TBCEditorTokenInfoTitleColors read FColors write FColors;
    property Font: TFont read FFont write SetFont;
    property Visible: Boolean read FVisible write FVisible default False;
  end;

implementation

constructor TBCEditorTokenInfoTitle.Create;
begin
  inherited;

  FColors := TBCEditorTokenInfoTitleColors.Create;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FVisible := False;
end;

destructor TBCEditorTokenInfoTitle.Destroy;
begin
  FColors.Free;
  FFont.Free;

  inherited;
end;

procedure TBCEditorTokenInfoTitle.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorTokenInfoTitle then
  with ASource as TBCEditorTokenInfoTitle do
  begin
    Self.FColors.Assign(FColors);
    Self.FFont.Assign(FFont);
    Self.FVisible := FVisible;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorTokenInfoTitle.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

end.
