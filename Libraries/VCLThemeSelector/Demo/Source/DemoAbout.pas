{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: Demo  About Form                                 }
{       (Help Viewer and Help Interfaces for Markdown files)                   }
{                                                                              }
{       Copyright (c) 2023-2024 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/MarkdownHelpViewer                         }
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
unit DemoAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls,
  {$IFDEF STYLEDCOMPONENTS}Vcl.StyledComponentsHooks,{$ENDIF}
  SVGIconImage;

const
  HELP_URL = 'https://github.com/EtheaDev/VCLThemeSelector';
type
  TFrmAbout = class(TForm)
    Panel1: TPanel;
    btnOK: TButton;
    TitleLabel: TLabel;
    MemoCopyRights: TMemo;
    ButtonHelp: TButton;
    SVGIconImage2: TSVGIconImage;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonHelpClick(Sender: TObject);
  private
    FTitle: string;
    procedure SetTitle(const Value: string);
  public
    property Title: string read FTitle write SetTitle;
  end;

procedure ShowAboutForm(const ATitle: string);

implementation

uses
  ShellApi;

{$R *.dfm}

procedure ShowAboutForm(const ATitle: string);
var
  LFrm: TFrmAbout;
begin
  LFrm := TFrmAbout.Create(nil);
  try
    LFrm.Title := ATitle;
    LFrm.ShowModal;
  finally
    LFrm.Free;
  end;
end;

procedure TFrmAbout.btnOKClick(Sender: TObject);
begin
  Close();
end;

procedure TFrmAbout.ButtonHelpClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(HELP_URL), nil, nil, SW_SHOW);
end;

procedure TFrmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
  TitleLabel.Font.Height := Round(TitleLabel.Font.Height * 1.6);
end;

procedure TFrmAbout.SetTitle(const Value: string);
begin
  FTitle := Value;
  Caption := FTitle;
  TitleLabel.Caption := Value;
end;

end.
