{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.ActionList.Templates;

interface

uses
  System.Classes, System.SysUtils, System.Actions, System.Rtti,
  Vcl.ActnList,

  BCEditor.Editor.KeyCommands,

  Spring.Collections,

  DSharp.Windows.ColumnDefinitions,
  DSharp.Core.DataTemplates, DSharp.Windows.ControlTemplates,
  DSharp.Windows.ColumnDefinitions.ControlTemplate;

{$REGION 'TActionListTemplate'}
type
  TActionListTemplate = class(TColumnDefinitionsControlTemplate)
    function GetValue(
      const Item        : TObject;
      const ColumnIndex : Integer
    ) : TValue; override;
  end;
{$ENDREGION}

{$REGION 'TKeyStrokeTemplate'}
type
  TKeyCommandTemplate = class(TColumnDefinitionsControlTemplate)
    function GetValue(
      const Item        : TObject;
      const ColumnIndex : Integer
    ) : TValue; override;
  end;
{$ENDREGION}

resourcestring
  SName       = 'Name';
  SCategory   = 'Category';
  SCaption    = 'Caption';
  SShortcut   = 'Shortcut';
  SShortcut2  = 'Shortcut2';
  SHint       = 'Hint';
  SVisible    = 'Visible';
  SEnabled    = 'Enabled';
  SCommand    = 'Command';
  SButton     = 'Button';
  SShift      = 'Shift';
  SShiftMask  = 'ShiftMask';
  SMoveCaret  = 'MoveCaret';

implementation

uses
  Vcl.Menus;

{$REGION 'TActionListTemplate'}
function TActionListTemplate.GetValue(const Item: TObject;
  const ColumnIndex: Integer): TValue;
var
  CA: TContainedAction;
  CD: TColumnDefinition;
begin
  CA := Item as TContainedAction;
  CD := TColumnDefinition(ColumnDefinitions[ColumnIndex]);
  if CD.Caption = SShortcut then
    Result := ShortCutToText(CA.Shortcut)
  else
    Result := inherited GetValue(Item, ColumnIndex);
end;
{$ENDREGION}

{$REGION 'TKeyCommandTemplate'}
function TKeyCommandTemplate.GetValue(const Item: TObject;
  const ColumnIndex: Integer): TValue;
var
  KC: TBCEditorKeyCommand;
  CD: TColumnDefinition;
  S : string;
begin
  KC := Item as TBCEditorKeyCommand;
  CD := TColumnDefinition(ColumnDefinitions[ColumnIndex]);
  if CD.Caption = SCommand then
  begin
    if not EditorCommandToIdent(KC.Command, S) then
      S := IntToStr(KC.Command);
    Result := S;
  end
  else if CD.Caption = SShortcut then
  begin
     Result := ShortCutToText(KC.ShortCut);
  end
  else if CD.Caption = SShortcut2 then
  begin
    Result := ShortCutToText(KC.SecondaryShortCut);
  end
  else
    Result := inherited GetValue(Item, ColumnIndex);
end;
{$ENDREGION}

end.
