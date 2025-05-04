{******************************************************************************}
{  VCLThemeSelector Launcher by Carlo Barazzetta                               }
{  A simple example to launch VCLThemeSelector                                 }
{                                                                              }
{       Copyright (c) 2020-2023 (Ethea S.r.l.)                                 }
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
unit LauncherForm;

interface

uses
  Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , System.Variants
  , System.Classes
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.Themes
  , Vcl.Imaging.pngimage
  , Vcl.Mask;

const
  COMPANY_NAME = 'Ethea';

type
  TLauncher = class(TForm)
    ChangeThemeButton: TButton;
    ExcludeWindowsCkeckBox: TCheckBox;
    MaxRowsEdit: TLabeledEdit;
    MaxColsEdit: TLabeledEdit;
    EtheaImage: TImage;
    Label1: TLabel;
    Label2: TLabel;
    procedure ChangeThemeButtonClick(Sender: TObject);
  private
    procedure TrySetStyle(const AStyleName: string);
  protected
    procedure Loaded; override;
  public
    { Public declarations }
  end;

var
  Launcher: TLauncher;

implementation

{$R *.dfm}

uses
  FVCLThemeSelector
  ;

procedure TLauncher.TrySetStyle(const AStyleName: string);
begin
  try
    TStyleManager.SetStyle(AStyleName);
  except
    ; //ignore
  end;
end;

procedure TLauncher.ChangeThemeButtonClick(Sender: TObject);
var
  LStyleName: string;
begin
  LStyleName := TStyleManager.ActiveStyle.Name;

  if ShowVCLThemeSelector(LStyleName,
      ExcludeWindowsCkeckBox.Checked,
      StrToInt(MaxRowsEdit.Text),
      StrToInt(MaxColsEdit.Text)) then
  begin
    TrySetStyle(LStyleName);
    WriteAppStyleToReg(COMPANY_NAME, ExtractFileName(Application.ExeName), LStyleName);
  end;
end;

procedure TLauncher.Loaded;
var
  LStyleName: string;
begin
  //Acquire system font and size (eg. for windows 10 Segoe UI and 14 at 96 DPI)
  //but without using Assign!
  Font.Name := Screen.IconFont.Name;
  //If you want to use system font Height:
  Font.Height := Muldiv(Screen.IconFont.Height, 96, Screen.IconFont.PixelsPerInch);

  //Read Style stored into Registry
  LStyleName := ReadAppStyleFromReg(COMPANY_NAME,  ExtractFileName(Application.ExeName));
  TrySetStyle(LStyleName);

  inherited;
end;

initialization
  //Example: how to add a new style not present into VCLThemeSelectorLauncher.InitDefaultThemesAttributes
  //download 'Radiant VCL Premium Style' from GetIt Package Manager and add to the
  //appearance section of this application
  RegisterThemeAttributes('Radiant', ttLight, clWebLightYellow, clWebLightgrey);

end.
