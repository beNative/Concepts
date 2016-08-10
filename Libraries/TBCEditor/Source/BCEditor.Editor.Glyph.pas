unit BCEditor.Editor.Glyph;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorGlyph = class(TPersistent)
  strict private
    FGlyph: TBitmap;
    FInternalGlyph: TBitmap;
    FInternalMaskColor: TColor;
    FLeft: Integer;
    FMaskColor: TColor;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure GlyphChange(ASender: TObject);
    procedure SetGlyph(AValue: TBitmap);
    procedure SetLeft(AValue: Integer);
    procedure SetMaskColor(AValue: TColor);
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create(AModule: THandle; const AName: string; AMaskColor: TColor);
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; X, Y: Integer; ALineHeight: Integer = 0);
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  published
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Left: Integer read FLeft write SetLeft default 2;
    property MaskColor: TColor read FMaskColor write SetMaskColor default clNone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

uses
  Winapi.Windows, System.SysUtils;

{ TBCEditorGlyph }

constructor TBCEditorGlyph.Create(AModule: THandle; const AName: string; AMaskColor: TColor);
begin
  inherited Create;

  if AName <> '' then
  begin
    FInternalGlyph := Vcl.Graphics.TBitmap.Create;
    FInternalGlyph.Handle := LoadBitmap(AModule, PChar(AName));
    FInternalMaskColor := AMaskColor;
  end
  else
    FInternalMaskColor := clNone;

  FVisible := True;
  FGlyph := Vcl.Graphics.TBitmap.Create;
  FGlyph.OnChange := GlyphChange;
  FMaskColor := clNone;
  FLeft := 2;
end;

destructor TBCEditorGlyph.Destroy;
begin
  if Assigned(FInternalGlyph) then
  begin
    FInternalGlyph.Free;
    FInternalGlyph := nil;
  end;
  FGlyph.Free;

  inherited Destroy;
end;

procedure TBCEditorGlyph.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorGlyph) then
  with ASource as TBCEditorGlyph do
  begin
    if Assigned(FInternalGlyph) then
      Self.FInternalGlyph.Assign(FInternalGlyph);
    Self.FInternalMaskColor := FInternalMaskColor;
    Self.FVisible := FVisible;
    Self.FGlyph.Assign(FGlyph);
    Self.FMaskColor := FMaskColor;
    Self.FLeft := FLeft;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorGlyph.Draw(ACanvas: TCanvas; X, Y: Integer; ALineHeight: Integer = 0);
var
  LGlyphBitmap: Vcl.Graphics.TBitmap;
  LMaskColor: TColor;
begin
  if not FGlyph.Empty then
  begin
    LGlyphBitmap := FGlyph;
    LMaskColor := FMaskColor;
  end
  else
  if Assigned(FInternalGlyph) then
  begin
    LGlyphBitmap := FInternalGlyph;
    LMaskColor := FInternalMaskColor;
  end
  else
    Exit;

  if ALineHeight <> 0 then
    Inc(Y, Abs(LGlyphBitmap.Height - ALineHeight) div 2);

  LGlyphBitmap.Transparent := True;
  LGlyphBitmap.TransparentMode := tmFixed;
  LGlyphBitmap.TransparentColor := LMaskColor;
  ACanvas.Draw(X, Y, LGlyphBitmap);
end;

procedure TBCEditorGlyph.SetGlyph(AValue: Vcl.Graphics.TBitmap);
begin
  FGlyph.Assign(AValue);
end;

procedure TBCEditorGlyph.GlyphChange(ASender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorGlyph.SetMaskColor(AValue: TColor);
begin
  if FMaskColor <> AValue then
  begin
    FMaskColor := AValue;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TBCEditorGlyph.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TBCEditorGlyph.SetLeft(AValue: Integer);
begin
  if FLeft <> AValue then
  begin
    FLeft := AValue;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

function TBCEditorGlyph.GetWidth: Integer;
begin
  if not FGlyph.Empty then
    Result := FGlyph.Width
  else
  if Assigned(FInternalGlyph) then
    Result := FInternalGlyph.Width
  else
    Result := 0;
end;

function TBCEditorGlyph.GetHeight: Integer;
begin
  if not FGlyph.Empty then
    Result := FGlyph.Height
  else
  if Assigned(FInternalGlyph) then
    Result := FInternalGlyph.Height
  else
    Result := 0;
end;

end.
