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

{$I Concepts.inc}

unit Concepts.Settings;

{ Provides access to application settings.  }

interface

uses
  System.Classes, System.IniFiles;

type
  ISettings = interface
  ['{2C05F3B9-2416-4E4E-B336-13213554457C}']
    function ReadString(
      const ASection : string;
      const AIdent   : string;
      const ADefault : string = ''
    ): string;
    procedure WriteString(
      const ASection : string;
      const AIdent   : string;
      const AValue   : string);
    function ReadInteger(
      const ASection : string;
      const AIdent   : string;
      const ADefault : Integer = 0
    ) : Integer;
    procedure WriteInteger(
      const ASection : string;
      const AIdent   : string;
      const AValue   : Integer
    );
    function ReadBool(
      const ASection : string;
      const AIdent   : string;
      const ADefault : Boolean = False
    ) : Boolean;
    procedure WriteBool(
      const ASection : string;
      const AIdent   : string;
      const AValue   : Boolean
    );
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

    function ReadString(
      const ASection : string;
      const AIdent   : string;
      const ADefault : string = ''
    ): string;
    procedure WriteString(
      const ASection : string;
      const AIdent   : string;
      const AValue   : string);
    function ReadInteger(
      const ASection : string;
      const AIdent   : string;
      const ADefault : Integer = 0
    ) : Integer;
    procedure WriteInteger(
      const ASection : string;
      const AIdent   : string;
      const AValue   : Integer
    );
    function ReadBool(
      const ASection : string;
      const AIdent   : string;
      const ADefault : Boolean = False
    ) : Boolean;
    procedure WriteBool(
      const ASection : string;
      const AIdent   : string;
      const AValue   : Boolean
    );
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
  inherited AfterConstruction;
  S := ExtractFilePath(Application.ExeName) +
    ExtractFileName(ChangeFileExt(Application.ExeName, '.ini'));
  FIniFile := TIniFile.Create(S);
end;

procedure TSettings.BeforeDestruction;
begin
  FIniFile.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TSettings.ReadBool(const ASection, AIdent: string;
  const ADefault: Boolean): Boolean;
begin
  Result := FIniFile.ReadBool(ASection, AIdent, ADefault);
end;

function TSettings.ReadInteger(const ASection, AIdent: string;
  const ADefault: Integer): Longint;
begin
  Result := FIniFile.ReadInteger(ASection, AIdent, ADefault);
end;

function TSettings.ReadString(const ASection, AIdent, ADefault: string): string;
begin
  Result := FIniFile.ReadString(ASection, AIdent, ADefault);
end;

procedure TSettings.WriteBool(const ASection, AIdent: string; const AValue: Boolean);
begin
  FIniFile.WriteBool(ASection, AIdent, AValue);
end;

procedure TSettings.WriteInteger(const ASection, AIdent: string; const AValue: Integer);
begin
  FIniFile.WriteInteger(ASection, AIdent, AValue);
end;

procedure TSettings.WriteString(const ASection, AIdent, AValue: string);
begin
  FIniFile.WriteString(ASection, AIdent, AValue);
end;
{$ENDREGION}

end.
