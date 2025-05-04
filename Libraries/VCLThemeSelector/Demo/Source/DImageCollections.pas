{******************************************************************************}
{  ModernAppDemo by Carlo Barazzetta                                           }
{  A full example of an HighDPI - VCL Themed enabled application               }
{  See how to select the application Theme using VCLThemeSelector Form         }
{                                                                              }
{       Copyright (c) 2020-2024 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       https://github.com/EtheaDev/VCLThemeSelector                           }
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit DImageCollections;

interface

uses
  System.SysUtils, System.Classes, IconFontsImageCollection,
  Vcl.BaseImageCollection, //If you are compiling with a versione older than 10.3 remove this line
  SVGIconImageCollection;

type
  TIconsType = (itIconFonts, itSVGIcons);

  TImageCollectionDataModule = class(TDataModule)
    SVGIconImageCollection: TSVGIconImageCollection;
    IconFontsImageCollection: TIconFontsImageCollection;
    IconFontsImageCollectionMono: TIconFontsImageCollection;
  private
    FIconsType: TIconsType;
    procedure SetIconsType(const Value: TIconsType);
  public
    property IconsType: TIconsType read FIconsType write SetIconsType;
  end;

var
  ImageCollectionDataModule: TImageCollectionDataModule;

implementation

{$R *.dfm}

{ TImageCollectionDataModule }

procedure TImageCollectionDataModule.SetIconsType(const Value: TIconsType);
begin
  FIconsType := Value;
end;

end.
