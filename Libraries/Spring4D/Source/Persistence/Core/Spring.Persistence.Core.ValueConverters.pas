{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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

unit Spring.Persistence.Core.ValueConverters;

interface

uses
  Classes,
  Spring,
  Spring.Persistence.Core.Graphics,
  Spring.ValueConverters;

type
  TStreamToVariantConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  TPictureToVariantConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  TStreamToPictureConverter = class(TValueConverter)
  private
    function TryLoadFromStreamSmart(const stream: TStream;
      const picture: TPicture): Boolean;
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

implementation

uses
{$IF Defined(DELPHIXE4_UP) AND NOT Defined(NEXTGEN)}
  AnsiStrings,
{$ELSE}
  SysUtils,
{$IFEND}
{$IFDEF MSWINDOWS}
  GIFImg,
  Graphics,
  jpeg,
  pngimage,
{$ELSE}
  {$IFNDEF LINUX}
  FMX.Graphics,
  {$ENDIF}
{$ENDIF}
  Variants;

procedure RegisterConverters;
begin
  TValueConverterFactory.RegisterConverter(TypeInfo(TStream), TypeInfo(Variant),
    TStreamToVariantConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TPicture), TypeInfo(Variant),
    TPictureToVariantConverter);
  TValueConverterFactory.RegisterConverter(TypeInfo(TStream), TypeInfo(TPicture),
    TStreamToPictureConverter);
end;

{$REGION 'TStreamToVariantConverter'}

function TStreamToVariantConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  stream: TStream;
begin
  stream := TStream(value.AsObject);
  Result := TValue.From<Variant>(StreamToVariant(stream));
end;

{$ENDREGION}


{$REGION 'TPictureToVariantConverter'}

function TPictureToVariantConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  stream: TStream;
begin
  stream := TMemoryStream.Create;
  try
    TPicture(value.AsObject).Graphic.SaveToStream(stream);
    stream.Position := 0;
    Result := TValue.From<Variant>(StreamToVariant(stream));
  finally
    stream.Free;
  end;
end;

{$ENDREGION}


{$REGION 'TStreamToPictureConverter'}

function TStreamToPictureConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  pic: TPicture;
  stream: TStream;
begin
  pic := TPicture.Create;
  stream := TStream(value.AsObject);
  if TryLoadFromStreamSmart(stream, pic) then
    Result := pic
  else
  begin
    pic.Free;
    Result := TValue.Empty;
  end;
end;

{$IFDEF MSWINDOWS}
function FindGraphicClass(const Buffer; const BufferSize: Int64;
  out GraphicClass: TGraphicClass): Boolean; overload;
const
  MinGraphicSize = 44; //we may test up to & including the 11th longword
var
  LongWords: array[Byte] of LongWord absolute Buffer;
  Words: array[Byte] of Word absolute Buffer;
begin
  GraphicClass := nil;
  Result := False;
  if BufferSize < MinGraphicSize then Exit;
  case Words[0] of
    $4D42: GraphicClass := TBitmap;
    $D8FF: GraphicClass := TJPEGImage;
    $4949: if Words[1] = $002A then GraphicClass := TWicImage; //i.e., TIFF
    $4D4D: if Words[1] = $2A00 then GraphicClass := TWicImage; //i.e., TIFF
  else
    if Int64(Buffer) = $A1A0A0D474E5089 then
      GraphicClass := TPNGImage
    else if LongWords[0] = $9AC6CDD7 then
      GraphicClass := TMetafile
    else if (LongWords[0] = 1) and (LongWords[10] = $464D4520) then
      GraphicClass := TMetafile
    else if {$IFDEF DELPHIXE4_UP}AnsiStrings.{$ENDIF}StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
      GraphicClass := TGIFImage
    else if Words[1] = 1 then
      GraphicClass := TIcon;
  end;
  Result := (GraphicClass <> nil);
end;
{$ENDIF}

function TStreamToPictureConverter.TryLoadFromStreamSmart(const stream: TStream;
  const picture: TPicture): Boolean;
var
  LGraphic: TGraphic;
  LGraphicClass: TGraphicClass;
  LStream: TMemoryStream;
begin
  LGraphic := nil;
  LStream := TMemoryStream.Create;
  try
    stream.Position := 0;
    LStream.CopyFrom(stream, stream.Size);
    if LStream.Size = 0 then
    begin
      picture.Assign(nil);
      Exit(True);
    end;
{$IFDEF MSWINDOWS}
    if not FindGraphicClass(LStream.Memory^, LStream.Size, LGraphicClass) then
      Exit(False);
{$ELSE}
    LGraphicClass := TBitmap;
{$ENDIF}
     // raise EInvalidGraphic.Create(SInvalidImage);
    LGraphic := LGraphicClass.Create;
    LStream.Position := 0;
    LGraphic.LoadFromStream(LStream);
    picture.Assign(LGraphic);
    Result := True;
  finally
    LStream.Free;
    LGraphic.Free;
  end;
end;

{$ENDREGION}


initialization
  RegisterConverters;

end.
