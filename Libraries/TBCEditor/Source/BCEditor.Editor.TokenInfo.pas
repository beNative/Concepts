unit BCEditor.Editor.TokenInfo;

interface

uses
  System.Classes, Vcl.Controls, Vcl.Graphics, BCEditor.Types, BCEditor.Editor.TokenInfo.Colors,
  BCEditor.Editor.TokenInfo.Title;

const
  BCEDITOR_TOKEN_INFO_DEFAULT_OPTIONS = [tioAutoSize];

type
  TBCEditorTokenInfo = class(TPersistent)
  strict private
    FColors: TBCEditorTokenInfoColors;
    FDelayInterval: Cardinal;
    FEnabled: Boolean;
    FFont: TFont;
    FHeight: Integer;
    FOptions: TBCEditorTokenInfoOptions;
    FTitle: TBCEditorTokenInfoTitle;
    FWidth: Integer;
    procedure SetFont(const AValue: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Colors: TBCEditorTokenInfoColors read FColors write FColors;
    property DelayInterval: Cardinal read FDelayInterval write FDelayInterval default 300;
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property Font: TFont read FFont write SetFont;
    property Height: Integer read FHeight write FHeight default 0;
    property Options: TBCEditorTokenInfoOptions read FOptions write FOptions default BCEDITOR_TOKEN_INFO_DEFAULT_OPTIONS;
    property Title: TBCEditorTokenInfoTitle read FTitle write FTitle;
    property Width: Integer read FWidth write FWidth default 0;
  end;

implementation

constructor TBCEditorTokenInfo.Create;
begin
  inherited Create;

  FColors := TBCEditorTokenInfoColors.Create;
  FDelayInterval := 300;
  FEnabled := False;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FHeight := 0;
  FOptions := BCEDITOR_TOKEN_INFO_DEFAULT_OPTIONS;
  FTitle := TBCEditorTokenInfoTitle.Create;
  FWidth := 0;
end;

destructor TBCEditorTokenInfo.Destroy;
begin
  FColors.Free;
  FFont.Free;
  FTitle.Free;

  inherited;
end;

procedure TBCEditorTokenInfo.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorTokenInfo then
  with ASource as TBCEditorTokenInfo do
  begin
    Self.FColors.Assign(FColors);
    Self.FDelayInterval := FDelayInterval;
    Self.FEnabled := FEnabled;
    Self.FFont.Assign(FFont);
    Self.FHeight := FHeight;
    Self.FOptions := FOptions;
    Self.FTitle.Assign(FTitle);
    Self.FWidth := FWidth;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorTokenInfo.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

end.
