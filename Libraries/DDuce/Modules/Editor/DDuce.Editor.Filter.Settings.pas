{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.Filter.Settings;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.ActnList,

  DDuce.Settings.Form;

const
  DEFAULT_WIDTH = 400;

type
  TFilterSettings = class(TComponent)
  private
    FFormSettings: TFormSettings;

    procedure SetFormSettings(AValue: TFormSettings);

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;


    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property FormSettings: TFormSettings
      read FFormSettings write SetFormSettings;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TFilterSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FFormSettings := TFormSettings.Create;
end;

destructor TFilterSettings.Destroy;
begin
  FFormSettings.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TFilterSettings.SetFormSettings(AValue: TFormSettings);
begin
  if FormSettings <> AValue then
    FFormSettings.Assign(AValue);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TFilterSettings.AssignTo(Dest: TPersistent);
var
  S: TFilterSettings;
begin
  if Dest is TFilterSettings then
  begin
    S := TFilterSettings(Dest);
    S.AssignTo(Dest);
  end
  else
    inherited AssignTo(Dest);
end;

procedure TFilterSettings.Assign(Source: TPersistent);
var
  S: TFilterSettings;
begin
  if Source is TFilterSettings then
  begin
    S := TFilterSettings(Source);
    S.Assign(Source);
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

initialization
  RegisterClass(TFilterSettings);

end.
