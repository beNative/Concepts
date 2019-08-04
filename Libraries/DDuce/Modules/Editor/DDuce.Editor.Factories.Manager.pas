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
unit DDuce.Editor.Factories.Manager;

interface

uses
  System.Classes, System.SysUtils,

  DDuce.Editor.Interfaces;

type
  TEditorManagerFactory = class(TInterfacedObject, IEditorManagerFactory)
    function CreateInstance(
      AOwner                  : TComponent = nil;
      APersistSettings        : Boolean = False;
      const ASettingsFileName : string = ''
    ): IEditorManager; overload;

    function CreateInstance(
      AOwner    : TComponent;
      ASettings : IEditorSettings
    ): IEditorManager; overload;
  end;

implementation

uses
  Vcl.Forms,

  DDuce.Editor.Manager;

function TEditorManagerFactory.CreateInstance(AOwner: TComponent;
  APersistSettings: Boolean; const ASettingsFileName: string): IEditorManager;
var
  O : TComponent;
  S : string;
begin
  if not Assigned(AOwner) then
    O := Application
  else
    O := AOwner;
  Result := TdmEditorManager.Create(O, nil);
  Result.PersistSettings := APersistSettings;
  if APersistSettings then
  begin
//    if ASettingsFileName = '' then
//      S := ApplicationName + '.xml'
//    else
//      S := ASettingsFileName;
    Result.Settings.FileName := S;
  end;
end;

function TEditorManagerFactory.CreateInstance(AOwner: TComponent;
  ASettings: IEditorSettings): IEditorManager;
begin
  Result := TdmEditorManager.Create(AOwner, ASettings);
end;

end.
