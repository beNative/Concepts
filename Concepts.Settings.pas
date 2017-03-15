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

unit Concepts.Settings;

{ Provides access to application settings.  }

interface

uses
  System.Classes, System.IniFiles;

type
  ISettings = interface
  ['{2C05F3B9-2416-4E4E-B336-13213554457C}']
    function ReadString(const Section, Ident, Default: string): string;
    procedure WriteString(const Section, Ident, Value: string);
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint;
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
  end;

function Settings: ISettings;

implementation

uses
  System.SysUtils,
  Vcl.Forms;

type
  TSettings = class(TInterfacedObject, ISettings)
  private
    FIniFile : TIniFile;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function ReadString(const Section, Ident, Default: string): string;
    procedure WriteString(const Section, Ident, Value: string);
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint;
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
  end;

var
  FSettings: ISettings;

function Settings: ISettings;
begin
  if not Assigned(FSettings) then
    FSettings := TSettings.Create;
  Result := FSettings;
end;

{$REGION 'construction and destruction'}
procedure TSettings.AfterConstruction;
var
  S : string;
begin
  inherited;
  S := ExtractFilePath(Application.ExeName) +
    ExtractFileName(ChangeFileExt(Application.ExeName, '.ini'));
  FIniFile := TIniFile.Create(S);
end;

procedure TSettings.BeforeDestruction;
begin
  FIniFile.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TSettings.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  Result := FIniFile.ReadBool(Section, Ident, Default);
end;

function TSettings.ReadInteger(const Section, Ident: string;
  Default: Integer): Longint;
begin
  Result := FIniFile.ReadInteger(Section, Ident, Default);
end;

function TSettings.ReadString(const Section, Ident, Default: string): string;
begin
  Result := FIniFile.ReadString(Section, Ident, Default);
end;

procedure TSettings.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  FIniFile.WriteBool(Section, Ident, Value);
end;

procedure TSettings.WriteInteger(const Section, Ident: string; Value: Integer);
begin
  FIniFile.WriteInteger(Section, Ident, Value);
end;

procedure TSettings.WriteString(const Section, Ident, Value: string);
begin
  FIniFile.WriteString(Section, Ident, Value);
end;
{$ENDREGION}

end.
