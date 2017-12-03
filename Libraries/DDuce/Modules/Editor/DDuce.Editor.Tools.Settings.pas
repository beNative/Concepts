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

unit DDuce.Editor.Tools.Settings;

interface

uses
  System.Classes, System.SysUtils;

type
  TEditorToolSettings = class(TComponent)
  private
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TComponent;
    function GetItemsByClass(AClass: TComponentClass): TComponent;
    function GetItemsByName(const AName: string): TComponent;

  public
    function RegisterSettings(
      ASettingsClass : TComponentClass;
      const AName    : string
    ) : TComponent;

  public
    property Count: Integer
      read GetCount;

    property Items[AIndex: Integer]: TComponent
      read GetItems;

    property ItemsByClass[AClass: TComponentClass]: TComponent
      read GetItemsByClass;

    property ItemsByName[const AName: string]: TComponent
      read GetItemsByName; default;
  end;

implementation

uses
  DDuce.Logger;

{$REGION 'property access mehods'}
function TEditorToolSettings.GetItemsByClass(
  AClass: TComponentClass): TComponent;
var
  I : Integer;
  B : Boolean;
begin
  Result := nil;
  I := 0;
  B := False;
  if ComponentCount > 0 then
  begin
    while (I < ComponentCount) and not B do
    begin
      B := Components[I] is AClass;
      if not B then
        Inc(I);
    end;
    if B then
      Result := Components[I];
  end;
end;

function TEditorToolSettings.GetCount: Integer;
begin
  Result := ComponentCount;
end;

function TEditorToolSettings.GetItems(AIndex: Integer): TComponent;
begin
  Result := Components[AIndex];
end;

function TEditorToolSettings.GetItemsByName(
  const AName: string): TComponent;
var
  I : Integer;
  B : Boolean;
begin
  Result := nil;
  I := 0;
  B := False;
  if ComponentCount > 0 then
  begin
    while (I < ComponentCount) and not B do
    begin
      B := Components[I].Name = AName;
      if not B then
        Inc(I);
    end;
    if B then
      Result := Components[I];
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TEditorToolSettings.RegisterSettings(
  ASettingsClass: TComponentClass; const AName: string): TComponent;
var
  ESI : TComponent;
begin
  ESI := ItemsByClass[ASettingsClass];
  if not Assigned(ESI) then
  begin
    ESI := ASettingsClass.Create(Self);
    ESI.Name := AName;
  end;
  Result := ESI;
end;
{$ENDREGION}

initialization
  RegisterClass(TEditorToolSettings);

end.
