unit BCEditor.Editor.InternalImage;

interface

uses
  Vcl.Graphics;

type
  TBCEditorInternalImage = class(TObject)
  strict private
    FCount: Integer;
    FHeight: Integer;
    FImages: Vcl.Graphics.TBitmap;
    FWidth: Integer;
    function CreateBitmapFromInternalList(AModule: THandle; const AName: string): Vcl.Graphics.TBitmap;
    procedure FreeBitmapFromInternalList;
  public
    constructor Create(AModule: THandle; const AName: string; const ACount: Integer);
    destructor Destroy; override;

    procedure Draw(ACanvas: TCanvas; const ANumber: Integer; const X: Integer; const Y: Integer;
      const ALineHeight: Integer; const ATransparentColor: TColor = clNone);
  end;

implementation

uses
  Winapi.Windows, System.Classes, System.Types, System.SysUtils;

type
  TInternalResource = class(TObject)
  public
    UsageCount: Integer;
    Name: string;
    Bitmap: Vcl.Graphics.TBitmap;
  end;

var
  GInternalResources: TList;

{ TBCEditorInternalImage }

constructor TBCEditorInternalImage.Create(AModule: THandle; const AName: string; const ACount: Integer);
begin
  inherited Create;
  FImages := CreateBitmapFromInternalList(AModule, AName);
  FWidth := (FImages.Width + ACount shr 1) div ACount;
  FHeight := FImages.Height;
  FCount := ACount;
end;

destructor TBCEditorInternalImage.Destroy;
begin
  FreeBitmapFromInternalList;

  inherited Destroy;
end;

function TBCEditorInternalImage.CreateBitmapFromInternalList(AModule: THandle; const AName: string): Vcl.Graphics.TBitmap;
var
  i: Integer;
  LInternalResource: TInternalResource;
begin
  for i := 0 to GInternalResources.Count - 1 do
    if TInternalResource(GInternalResources[i]).Name = UpperCase(AName) then
      with TInternalResource(GInternalResources[i]) do
      begin
        UsageCount := UsageCount + 1;
        Result := Bitmap;
        Exit;
      end;

  Result := Vcl.Graphics.TBitmap.Create;
  Result.Handle := LoadBitmap(AModule, PChar(AName));

  LInternalResource := TInternalResource.Create;
  with LInternalResource do
  begin
    UsageCount := 1;
    Name := UpperCase(AName);
    Bitmap := Result;
  end;
  GInternalResources.Add(LInternalResource);
end;

procedure TBCEditorInternalImage.FreeBitmapFromInternalList;
var
  i: Integer;
  LInternalResource: TInternalResource;

  function FindImageIndex: Integer;
  begin
    for Result := 0 to GInternalResources.Count - 1 do
      if TInternalResource(GInternalResources[Result]).Bitmap = FImages then
        Exit;
    Result := -1;
  end;

begin
  i := FindImageIndex;
  if i = -1 then
    Exit;

  LInternalResource := TInternalResource(GInternalResources[i]);
  with LInternalResource do
  begin
    UsageCount := UsageCount - 1;
    if UsageCount = 0 then
    begin
      Bitmap.Free;
      Bitmap := nil;
      GInternalResources.Delete(i);
      LInternalResource.Free;
    end;
  end;
end;

procedure TBCEditorInternalImage.Draw(ACanvas: TCanvas; const ANumber: Integer; const X: Integer; const Y: Integer;
  const ALineHeight: Integer; const ATransparentColor: TColor = clNone);
var
  LSourceRect, LDestinationRect: TRect;
  LY: Integer;
begin
  if (ANumber >= 0) and (ANumber < FCount) then
  begin
    LY := Y;
    if ALineHeight >= FHeight then
    begin
      LSourceRect := Rect(ANumber * FWidth, 0, (ANumber + 1) * FWidth, FHeight);
      Inc(LY, (ALineHeight - FHeight) div 2);
      LDestinationRect := Rect(X, LY, X + FWidth, LY + FHeight);
    end
    else
    begin
      LDestinationRect := Rect(X, LY, X + FWidth, LY + ALineHeight);
      LY := (FHeight - ALineHeight) div 2;
      LSourceRect := Rect(ANumber * FWidth, LY, (ANumber + 1) * FWidth, LY + ALineHeight);
    end;
    if ATransparentColor = clNone then
      ACanvas.CopyRect(LDestinationRect, FImages.Canvas, LSourceRect)
    else
      ACanvas.BrushCopy(LDestinationRect, FImages, LSourceRect, ATransparentColor);
  end;
end;

initialization

  GInternalResources := TList.Create;

finalization

  GInternalResources.Free;
  GInternalResources := nil;

end.
