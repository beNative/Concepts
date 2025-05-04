{******************************************************************************}
{                                                                              }
{       CBVCLStylePreviewForm: Example Preview of a VCL Style                  }
{       based on: VCLStylePreview Vcl.Styles.Ext                               }
{       https://github.com/RRUZ/vcl-styles-utils/                              }
{                                                                              }
{       Copyright (c) 2020-2024 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/VCLThemeSelector                           }
{                                                                              }
{******************************************************************************}
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
unit CBVCLStylePreviewForm;

{$Include VCLThemeSelector.inc}

interface

Uses
  System.Classes,
  System.Sysutils,
  System.Generics.Collections,
  System.Types,
  Winapi.Windows,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Forms,
  vcl.Menus,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls;

const
  PREVIEW_HEIGHT = 190; //At 96 DPI
  PREVIEW_WIDTH = 300;  //At 96 DPI

type
  TCBVCLPreviewForm = class(TForm)
    MainMenu: TMainMenu;
    FMenu1: TMenuItem;
    FMenu2: TMenuItem;
    FMenu3: TMenuItem;
    FMenu4: TMenuItem;
    TabControl: TTabControl;
    FNormalTextEdit: TEdit;
    FButtonNormal: TButton;
    FButtonDisabled: TButton;
    FRequiredTextEdit: TEdit;
    ScrollBar: TScrollBar;
    FReadOnlyTextEdit: TEdit;
    CheckBox: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FButtonNormalMouseEnter(Sender: TObject);
    procedure FButtonNormalMouseLeave(Sender: TObject);
    procedure FButtonNormalMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FButtonNormalMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FButtonNormalCaption: string;
    FButtonHotCaption: string;
    FButtonPressedCaption: string;
    FCustomStyle: TCustomStyleServices;
    { Private declarations }
  public
    procedure SetCaptions(const ACaptions: string);
    property CustomStyle: TCustomStyleServices read FCustomStyle Write FCustomStyle;
  end;

implementation

{$R *.dfm}

{ TCBVCLPreviewForm }

procedure TCBVCLPreviewForm.FButtonNormalMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FButtonNormal.Caption := FButtonPressedCaption;
end;

procedure TCBVCLPreviewForm.FButtonNormalMouseEnter(Sender: TObject);
begin
  FButtonNormal.Caption := FButtonHotCaption;
end;

procedure TCBVCLPreviewForm.FButtonNormalMouseLeave(Sender: TObject);
begin
  FButtonNormal.Caption := FButtonNormalCaption;
end;

procedure TCBVCLPreviewForm.FButtonNormalMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FButtonNormal.Caption := FButtonHotCaption;
end;

procedure TCBVCLPreviewForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := False;
end;

procedure TCBVCLPreviewForm.FormCreate(Sender: TObject);
begin
  ;
end;

procedure TCBVCLPreviewForm.FormShow(Sender: TObject);
begin
  Self.StyleName := FCustomStyle.Name;
  Visible := True;
end;

procedure TCBVCLPreviewForm.SetCaptions(const ACaptions: string);
var
  LCaptions: TStringList;
begin
  LCaptions := TStringList.Create;
  LCaptions.Text := ACaptions;
  try
    if LCaptions.Count  > 0 then //File
      FMenu1.Caption := LCaptions.Strings[0];

    if LCaptions.Count  > 1 then //Edit
      FMenu2.Caption := LCaptions.Strings[1];

    if LCaptions.Count  > 2 then //View
      FMenu3.Caption := LCaptions.Strings[2];

    if LCaptions.Count  > 3 then //Help
      FMenu4.Caption := LCaptions.Strings[3];

    if LCaptions.Count  > 4 then //Text editor
      FNormalTextEdit.Text := LCaptions.Strings[4];

    if LCaptions.Count  > 5 then //Normal
    begin
      FButtonNormalCaption := LCaptions.Strings[5];
      FButtonNormal.Caption := FButtonNormalCaption;
    end;

    if LCaptions.Count  > 6 then //Hot
      FButtonHotCaption := LCaptions.Strings[6];

    if LCaptions.Count  > 7 then //Pressed
      FButtonPressedCaption := LCaptions.Strings[7];

    if LCaptions.Count  > 8 then //Disabled
      FButtonDisabled.Caption := LCaptions.Strings[8];

    if LCaptions.Count  > 9 then //Required
      FRequiredTextEdit.Text := LCaptions.Strings[9];

    if LCaptions.Count  > 10 then //Readonly
      FReadOnlyTextEdit.Text := LCaptions.Strings[10];

    if LCaptions.Count  > 11 then //Check
      CheckBox.Caption := LCaptions.Strings[11];

    if LCaptions.Count  > 12 then //Page 1
      Tabcontrol.Tabs[0] := LCaptions.Strings[12];

    if LCaptions.Count  > 13 then //Page 2
      Tabcontrol.Tabs[1] := LCaptions.Strings[13];

    if LCaptions.Count  > 14 then //Page 3
      Tabcontrol.Tabs[2] := LCaptions.Strings[14];

  finally
    LCaptions.Free;
  end;
end;

end.
