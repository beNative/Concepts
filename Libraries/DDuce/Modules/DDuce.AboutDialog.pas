{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit DDuce.AboutDialog;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  DDuce.Components.ValueList;

type
  TfrmAboutDialog = class(TForm)
    pnlVersionInfo: TPanel;

  private
    FVersionInfoList : TValueList;

    procedure UpdateVersionInfoDisplay;

  public
    procedure AfterConstruction; override;

  end;

procedure ShowAboutDialog;

implementation

{$R *.dfm}

uses
  DDuce.DynamicRecord,

  Spring.Utils, Spring;

{$REGION 'interfaced routines'}
procedure ShowAboutDialog;
var
  F : TfrmAboutDialog;
begin
  F := TfrmAboutDialog.Create(Application);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmAboutDialog.AfterConstruction;
begin
  inherited AfterConstruction;
  FVersionInfoList            := TValueList.Create(Self);
  FVersionInfoList.Parent     := pnlVersionInfo;
  FVersionInfoList.Align      := alClient;
  FVersionInfoList.Editable   := False;
  FVersionInfoList.ShowHeader := False;

  UpdateVersionInfoDisplay;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmAboutDialog.UpdateVersionInfoDisplay;
var
  R : DynamicRecord;
begin
  R.Data.ProductName      := TEnvironment.ApplicationVersionInfo.ProductName;
  R.Data.ProductVersion   := TEnvironment.ApplicationVersionInfo.ProductVersion;
  R.Data.LegalCopyright   := TEnvironment.ApplicationVersionInfo.LegalCopyright;
  R.Data.FileName         := TEnvironment.ApplicationVersionInfo.FileName;
  R.Data.OriginalFilename := TEnvironment.ApplicationVersionInfo.OriginalFilename;
  R.Data.FileDescription  := TEnvironment.ApplicationVersionInfo.FileDescription;
  R.Data.Language         := TEnvironment.ApplicationVersionInfo.Language;
  R.Data.IsDebug          := TEnvironment.ApplicationVersionInfo.IsDebug;

  R.Data.ApplicationPath          := TEnvironment.ApplicationPath;
  R.Data.ApplicationVersionString := TEnvironment.ApplicationVersionString;
  R.Data.UserDomainName           := TEnvironment.UserDomainName;
  R.Data.UserName                 := TEnvironment.UserName;
  R.Data.UserInteractive          := TEnvironment.UserInteractive;
  R.Data.IsAdmin                  := TEnvironment.IsAdmin;
  R.Data.MachineName              := TEnvironment.MachineName;
  R.Data.ProcessorCount           := TEnvironment.ProcessorCount;
  R.Data.SystemDirectory          := TEnvironment.SystemDirectory;
  R.Data.TickCount                := TEnvironment.TickCount;
  R.Data.ProcessorArchitecture    := TEnum.GetName(TEnvironment.ProcessorArchitecture);

  FVersionInfoList.Data := R;
end;
{$ENDREGION}

end.
