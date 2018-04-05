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

unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ExtCtrls, CheckLst, BuildEngine;

type
  TfrmMain = class(TForm)
    btnBuild: TButton;
    mmoDetails: TMemo;
    lblDetails: TLabel;
    grpTargets: TGroupBox;
    lbTargets: TCheckListBox;
    lblHomepage: TLinkLabel;
    BalloonHint1: TBalloonHint;
    btnClean: TButton;
    chkRunTests: TCheckBox;
    grpBuildOptions: TGroupBox;
    chkModifyDelphiRegistrySettings: TCheckBox;
    chkPauseAfterEachStep: TCheckBox;
    PopupMenu1: TPopupMenu;
    mniCheckAll: TMenuItem;
    mniUncheckAll: TMenuItem;
    grpBuildConfigurations: TGroupBox;
    chkDebug: TCheckBox;
    chkRelease: TCheckBox;
    chkRunTestsAsConsole: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
    procedure btnCleanClick(Sender: TObject);
    procedure lbTargetsClickCheck(Sender: TObject);
    procedure lblHomepageLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure chkRunTestsClick(Sender: TObject);
    procedure chkModifyDelphiRegistrySettingsClick(Sender: TObject);
    procedure chkPauseAfterEachStepClick(Sender: TObject);
    procedure mniCheckAllClick(Sender: TObject);
    procedure mniUncheckAllClick(Sender: TObject);
    procedure chkDebugClick(Sender: TObject);
    procedure chkReleaseClick(Sender: TObject);
    procedure chkRunTestsAsConsoleClick(Sender: TObject);
  private
    fBuildEngine: TBuildEngine;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  ShellAPI,
  Spring.Utils;

const
  CCompilerSettingsFileName = 'Build.Settings.Compilers.ini';
  CBuildSettingsFileName = 'Build.Settings.ini';

procedure TfrmMain.FormCreate(Sender: TObject);
var
  task: TBuildTask;
  index: Integer;
begin
  fBuildEngine := TBuildEngine.Create;
  fBuildEngine.ConfigureCompilers(ApplicationPath + CCompilerSettingsFileName);
  fBuildEngine.LoadSettings(ApplicationPath + CBuildSettingsFileName);
  chkDebug.Checked := TBuildConfig.Debug in fBuildEngine.BuildConfigs;
  chkRelease.Checked := TBuildConfig.Release in fBuildEngine.BuildConfigs;
  chkPauseAfterEachStep.Checked := fBuildEngine.PauseAfterEachStep;
  chkRunTests.Checked := fBuildEngine.RunTests;
  chkRunTestsAsConsole.Checked := fBuildEngine.RunTestsAsConsole;
  chkModifyDelphiRegistrySettings.Checked := fBuildEngine.ModifyDelphiRegistrySettings;

  lbTargets.Clear;
  for task in fBuildEngine.Tasks do
  begin
    if fBuildEngine.OnlyShowInstalledVersions and not task.CanBuild then
      Continue;
    index := lbTargets.Items.AddObject(task.Name, task);
    lbTargets.ItemEnabled[index] := task.CanBuild;
    lbTargets.Checked[index] := fBuildEngine.SelectedTasks.Contains(task);
  end;

  if FileExists('Build.md') then
    mmoDetails.Lines.LoadFromFile('Build.md');
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fBuildEngine.SaveSettings(ApplicationPath + CBuildSettingsFileName);
  fBuildEngine.Free;
end;

procedure TfrmMain.lblHomepageLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(Handle, 'open', PChar(Link), nil, nil, SW_NORMAL);
end;

procedure TfrmMain.lbTargetsClickCheck(Sender: TObject);
var
  task: TBuildTask;
  i: Integer;
begin
  fBuildEngine.SelectedTasks.Clear;
  for i := 0 to lbTargets.Count - 1 do
  begin
    if lbTargets.Checked[i] then
    begin
      task := TBuildTask(lbTargets.Items.Objects[i]);
      fBuildEngine.SelectedTasks.Add(task);
    end;
  end;
  btnBuild.Enabled := fBuildEngine.SelectedTasks.Any;
end;

procedure TfrmMain.mniUncheckAllClick(Sender: TObject);
begin
  lbTargets.CheckAll(cbUnchecked);
  lbTargetsClickCheck(lbTargets);
end;

procedure TfrmMain.btnBuildClick(Sender: TObject);
begin
  fBuildEngine.BuildAll;
end;

procedure TfrmMain.btnCleanClick(Sender: TObject);
begin
  fBuildEngine.CleanUp;
end;

procedure TfrmMain.mniCheckAllClick(Sender: TObject);
begin
  lbTargets.CheckAll(cbChecked, False, False);
  lbTargetsClickCheck(lbTargets);
end;

procedure TfrmMain.chkReleaseClick(Sender: TObject);
begin
  if chkRelease.Checked then
    fBuildEngine.BuildConfigs := fBuildEngine.BuildConfigs + [TBuildConfig.Release]
  else
    fBuildEngine.BuildConfigs := fBuildEngine.BuildConfigs - [TBuildConfig.Release];
end;

procedure TfrmMain.chkRunTestsAsConsoleClick(Sender: TObject);
begin
  fBuildEngine.RunTestsAsConsole := chkRunTestsAsConsole.Checked;
end;

procedure TfrmMain.chkRunTestsClick(Sender: TObject);
begin
  fBuildEngine.RunTests := chkRunTests.Checked;
  chkRunTestsAsConsole.Enabled := chkRunTests.Checked;
end;

procedure TfrmMain.chkDebugClick(Sender: TObject);
begin
  if chkDebug.Checked then
    fBuildEngine.BuildConfigs := fBuildEngine.BuildConfigs + [TBuildConfig.Debug]
  else
    fBuildEngine.BuildConfigs := fBuildEngine.BuildConfigs - [TBuildConfig.Debug];
end;

procedure TfrmMain.chkModifyDelphiRegistrySettingsClick(Sender: TObject);
begin
  fBuildEngine.ModifyDelphiRegistrySettings := chkModifyDelphiRegistrySettings.Checked;
end;

procedure TfrmMain.chkPauseAfterEachStepClick(Sender: TObject);
begin
  fBuildEngine.PauseAfterEachStep := chkPauseAfterEachStep.Checked;
end;

end.
