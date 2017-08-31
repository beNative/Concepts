{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I Concepts.inc}

unit Concepts.Spring.Utils.Form;

{ Demonstrates the Spring.Utils unit. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  Spring.Utils;

type
  TfrmSpringUtils = class(TForm)
    aclMain                    : TActionList;
    actApplicationVersion      : TAction;
    actApplicationVersionInfo  : TAction;
    actEnvironment             : TAction;
    actGetCommandLineArgs      : TAction;
    actGetEnvironmentVariables : TAction;
    actGetLogicalDrives        : TAction;
    actOperatingSystem         : TAction;
    btnApplicationVersion      : TButton;
    btnEnvironment             : TButton;
    btnFileVersionInfo         : TButton;
    btnGetCommandLineArgs      : TButton;
    btnGetEnvironmentVariables : TButton;
    btnGetLogicalDrives        : TButton;
    btnOperatingSystem         : TButton;
    mmoMain                    : TMemo;
    pnlTop                     : TGridPanel;

    procedure actOperatingSystemExecute(Sender: TObject);
    procedure actApplicationVersionInfoExecute(Sender: TObject);
    procedure actApplicationVersionExecute(Sender: TObject);
    procedure actEnvironmentExecute(Sender: TObject);
    procedure actGetEnvironmentVariablesExecute(Sender: TObject);
    procedure actGetCommandLineArgsExecute(Sender: TObject);
    procedure actGetLogicalDrivesExecute(Sender: TObject);

  end;

implementation

{$R *.dfm}

uses
  DDuce.Reflect, DDuce.DynamicRecord, DDuce.ScopedReference;

{$REGION 'action handlers'}
procedure TfrmSpringUtils.actApplicationVersionExecute(Sender: TObject);
begin
  mmoMain.Text := Reflect.Fields(TEnvironment.ApplicationVersion).ToString;
end;

procedure TfrmSpringUtils.actEnvironmentExecute(Sender: TObject);
var
  R : TRecord;
begin
  R.Data.ApplicationPath          := TEnvironment.ApplicationPath;
  R.Data.ApplicationVersionString := TEnvironment.ApplicationVersionString;
  R.Data.CommandLine              := TEnvironment.CommandLine;
  R.Data.CurrentDirectory         := TEnvironment.CurrentDirectory;
  R.Data.IsAdmin                  := TEnvironment.IsAdmin;
  R.Data.MachineName              := TEnvironment.MachineName;
  R.Data.ProcessorCount           := TEnvironment.ProcessorCount;
  R.Data.RegisteredOrganization   := TEnvironment.RegisteredOrganization;
  R.Data.RegisteredOwner          := TEnvironment.RegisteredOwner;
  R.Data.SystemDirectory          := TEnvironment.SystemDirectory;
  R.Data.TickCount                := TEnvironment.TickCount;
  R.Data.UserDomainName           := TEnvironment.UserDomainName;
  R.Data.UserName                 := TEnvironment.UserName;
  R.Data.UserInteractive          := TEnvironment.UserInteractive;
  R.Data.ProcessorArchitecture    :=
    Reflect.EnumName(TEnvironment.ProcessorArchitecture);
  R.Data.NewLine                  := TEnvironment.NewLine;
  mmoMain.Text := R.ToString;
end;

procedure TfrmSpringUtils.actGetCommandLineArgsExecute(Sender: TObject);
var
  S  : string;
begin
  mmoMain.Clear;
  for S in TEnvironment.GetCommandLineArgs do
    mmoMain.Lines.Add(S);
end;

procedure TfrmSpringUtils.actGetEnvironmentVariablesExecute(Sender: TObject);
var
  SL : Scoped<TStringList>;
  R  : TRecord;
begin
  TEnvironment.GetEnvironmentVariables(SL);
  R.FromStrings(SL);
  mmoMain.Text := R.ToString;
end;

procedure TfrmSpringUtils.actGetLogicalDrivesExecute(Sender: TObject);
var
  S  : string;
begin
  mmoMain.Clear;
  for S in TEnvironment.GetLogicalDrives do
    mmoMain.Lines.Add(S);
end;

procedure TfrmSpringUtils.actApplicationVersionInfoExecute(Sender: TObject);
begin
  mmoMain.Text := Reflect.Fields(TEnvironment.ApplicationVersionInfo).ToString;
end;

procedure TfrmSpringUtils.actOperatingSystemExecute(Sender: TObject);
begin
  mmoMain.Text := Reflect.Properties(TEnvironment.OperatingSystem).ToString;
end;
{$ENDREGION}

end.
