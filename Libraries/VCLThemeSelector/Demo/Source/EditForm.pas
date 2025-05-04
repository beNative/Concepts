{******************************************************************************}
{  ModernAppDemo by Carlo Barazzetta                                           }
{  A full example of an HighDPI - VCL Themed enabled application               }
{  See how to select the application Theme using VCLThemeSelector Form         }
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
unit EditForm;

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
  , Vcl.Mask
  , Vcl.DBCtrls
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.ComCtrls
  , Vcl.Menus
  , System.ImageList
  , Vcl.ImgList
  , SVGIconImageListBase      //If don't compile you must before download and installa SVGIconImageList components
  , SVGIconVirtualImageList   //https://github.com/EtheaDev/SVGIconImageList
  , IconFontsImageListBase    //If don't compile you must before download and installa IconFontsImageList components
  , IconFontsVirtualImageList //https://github.com/EtheaDev/IconFontsImageList
  , DImageCollections, Vcl.ToolWin
  {$IFDEF STYLEDCOMPONENTS}
  , Vcl.StyledComponentsHooks
  {$ENDIF}
  ;

type
  TFmEdit = class(TForm)
    PageControl1: TPageControl;
    tsParentFont: TTabSheet;
    NameEdit: TEdit;
    SurNameEdit: TEdit;
    LabeledEdit: TLabeledEdit;
    Label1: TLabel;
    Label2: TLabel;
    tsNoParent: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    LabeledEdit2: TLabeledEdit;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    SettingsMenuitem: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    IconFontsVirtualImageList: TIconFontsVirtualImageList;
    SVGIconVirtualImageList: TSVGIconVirtualImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure FormShow(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    procedure UpdateFontAttributes;
  protected
  public
  end;

var
  FmEdit: TFmEdit;

implementation

{$R *.dfm}

procedure TFmEdit.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TFmEdit.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  if ParentFont and (Application.MainForm.Monitor.Handle <> Self.Monitor.Handle) then
    Font.Height := MulDiv(Font.Height, NewDPI, OldDPI);
end;

procedure TFmEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FmEdit := nil;
end;

procedure TFmEdit.FormShow(Sender: TObject);
begin
  if ImageCollectionDataModule.IconsType = itIconFonts then
    FmEdit.MainMenu.Images := IconFontsVirtualImageList
  else
    FmEdit.MainMenu.Images := SVGIconVirtualImageList;
  UpdateFontAttributes;
end;

procedure TFmEdit.UpdateFontAttributes;
begin
  NameEdit.ParentFont := True;
  NameEdit.Font.Style := [fsBold];
  SurNameEdit.ParentFont := True;
  SurNameEdit.Font.Style := [fsBold];
  LabeledEdit.ParentFont := True;
  LabeledEdit.Font.Style := [fsBold];
end;

end.
