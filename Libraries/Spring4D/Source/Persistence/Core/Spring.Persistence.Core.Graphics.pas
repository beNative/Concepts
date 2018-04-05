{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Persistence.Core.Graphics;

interface

uses
{$IFDEF FMX}
  {$IFDEF DELPHIXE5_UP}
  Fmx.Graphics
  {$ELSE}
  Fmx.Types
  {$ENDIF}
{$ELSE}
  {$IFDEF MSWINDOWS}
  {$IFDEF HAS_UNITSCOPE}
  Vcl.Graphics
  {$ELSE}
  Graphics
  {$ENDIF}
  {$ELSE}
  SysUtils,
  Classes
  {$ENDIF}
{$ENDIF}
  ;

type

{$REGION 'Type overlays'}

{$IFDEF FMX}
  {$IFDEF DELPHIXE5_UP}
  TBitmap = Fmx.Graphics.TBitmap;
  {$ELSE}
  TBitmap = Fmx.Types.TBitmap;
  {$ENDIF}
  TPicture = TBitmap;
  TGraphic = TBitmap;
  TGraphicClass = class of TBitmap;
{$ELSE}
  {$IFDEF MSWINDOWS}
  {$IFDEF HAS_UNITSCOPE}
  TBitmap = Vcl.Graphics.TBitmap;
  TPicture = Vcl.Graphics.TPicture;
  TGraphic = Vcl.Graphics.TGraphic;
  TGraphicClass = Vcl.Graphics.TGraphicClass;
  {$ELSE}
  TBitmap = Graphics.TBitmap;
  TPicture = Graphics.TPicture;
  TGraphic = Graphics.TGraphic;
  TGraphicClass = Graphics.TGraphicClass;
  {$ENDIF}
  {$ELSE}
  // hack to compile code on Linux - semi functional
  TBitmapType = (Unknown, BMP, PNG);
  TBitmap = class(TPersistent)
  private
    fBitmapType: TBitmapType;
    fData: TBytes;
    fHeight, fWidth: Integer;
  public
    procedure Assign(source: TPersistent); override;
    procedure LoadFromFile(const fileName: string);
    procedure LoadFromStream(const stream: TStream);
    procedure SaveToFile(const fileName: string);
    procedure SaveToStream(const stream: TStream);

    property Height: Integer read fHeight;
    property Width: Integer read fWidth;
    property BitmapType: TBitmapType read fBitmapType;
  end;
  TPicture = TBitmap;
  TGraphic = TBitmap;
  TGraphicClass = class of TBitmap;
  EInvalidBitmap = class(Exception);
  {$ENDIF}

{$ENDIF}

{$ENDREGION}


{$REGION 'Helpers'}

{$IFNDEF MSWINDOWS}
type
  TPictureHelper = class helper for TPicture
  private
    function GetGraphic: TBitmap; inline;
  public
    property Graphic: TBitmap read GetGraphic;
  end;
{$ENDIF}

{$ENDREGION}


implementation


{$REGION 'TPictureHelper'}

{$IFNDEF MSWINDOWS}
function TPictureHelper.GetGraphic: TBitmap;
begin
  Result := Self;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TBitmap'}

{$IFDEF LINUX}
type
  PBitmapFileHeader = ^TBitmapFileHeader;
  TBitmapFileHeader = packed record
    bfType: Word;
    bfSize: Cardinal;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: Cardinal;
  end;

  PBitmapInfoHeader = ^TBitmapInfoHeader;
  TBitmapInfoHeader = packed record
    biSize: Cardinal;
    biWidth: Integer;
    biHeight: Integer;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: Cardinal;
    biSizeImage: Cardinal;
    biXPelsPerMeter: Integer;
    biYPelsPerMeter: Integer;
    biClrUsed: Cardinal;
    biClrImportant: Cardinal;
  end;

  TPngCrc = Cardinal;
  TPngInt = packed array[0..3] of Byte;

  PPngIhdr = ^TPngIhdr;
  TPngIhdr = packed record
    Width, Height: TPngInt;
    BitDepth,
    ColorType,
    CompressionMethod,
    FilterMethod,
    InterlaceMethod: Byte;
  end;

const
  PNG_HEADER: array[0..7] of Byte = ($89, $50, $4E, $47, $0D, $0A, $1A, $0A);
  PNG_IHDR: array[0..7] of Byte = (0, 0, 0, $D, $49, $48, $44, $52);

procedure TBitmap.Assign(source: TPersistent);
begin
  if not Assigned(source) then
  begin
    fData := nil;
    fHeight := 0;
    fWidth := 0;
    fBitmapType := TBitmapType.Unknown;
  end
  else if source is TBitmap then
  begin
    fData := TBitmap(source).fData;
    fHeight := TBitmap(source).fHeight;
    fWidth := TBitmap(source).fWidth;
    fBitmapType := TBitmap(source).fBitmapType;
  end
  else
    inherited;
end;

procedure TBitmap.LoadFromFile(const fileName: string);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(fileName, fmOpenRead);
  LoadFromStream(stream);
end;

procedure TBitmap.LoadFromStream(const stream: TStream);

  procedure LoadAsBitmap;
  var
    fileHdr: PBitmapFileHeader;
    infoHdr: PBitmapInfoHeader;
  begin
    if Length(fData) < SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader) then
      EInvalidBitmap.Create('Invalid bitmap size');
    fileHdr := @fData[0];
    {if fileHdr.bfType <> $4D42 then
      raise EInvalidBitmap.Create('Invalid bitmap type');}
    if fileHdr.bfSize <> Length(fData) then
      raise EInvalidBitmap.Create('Invalid bitmap header');
    infoHdr := @fData[SizeOf(TBitmapFileHeader)];
    if infoHdr.biSize <> SizeOf(TBitmapInfoHeader) then
      raise EInvalidBitmap.Create('Invalid bitmap info header');
    fWidth := infoHdr.biWidth;
    fHeight := infoHdr.biHeight;
    fBitmapType := TBitmapType.BMP;
  end;

  function PngIntToInt(const value: TPngInt): Integer; inline;
  begin
    Result := value[0] shl 24 or value[1] shl 16 or value[2] shl 8 or value[3];
  end;

  procedure LoadAsPng;
  var
    ihdr: PPngIhdr;
  begin
    if Length(fData) < SizeOf(PNG_HEADER) + SizeOf(PNG_IHDR)
      + SizeOf(TPngIhdr) + SizeOf(TPngCrc) then
      raise EInvalidBitmap.Create('Invalid PNG header');
    if not CompareMem(@fData[SizeOf(PNG_HEADER)], @PNG_IHDR, SizeOf(PNG_IHDR)) then
      raise EInvalidBitmap.Create('Invalid PNG header');
    ihdr := @fData[SizeOf(PNG_HEADER) + SizeOf(PNG_IHDR)];
    fWidth := PngIntToInt(ihdr.Width);
    fHeight := PngIntToInt(ihdr.Height);
    fBitmapType := TBitmapType.PNG;
  end;

begin
  SetLength(fData, stream.Size - stream.Position);
  stream.ReadBuffer(fData, Length(fData));
  if (Length(fData) > 2) and (fData[0] = $42) and (fData[0] = $4D) then
    LoadAsBitmap
  else if (Length(fData) > SizeOf(PNG_HEADER))
    and CompareMem(@fData[0], @PNG_HEADER, SizeOf(PNG_HEADER)) then
    LoadAsPng
  else
    raise EInvalidBitmap.Create('Unsupported bitmap type');
end;

procedure TBitmap.SaveToFile(const fileName: string);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(fileName, fmCreate);
  SaveToStream(stream);
end;

procedure TBitmap.SaveToStream(const stream: TStream);
begin
  stream.WriteBuffer(fData, Length(fData));
end;

{$ENDIF}

{$ENDREGION}


end.
