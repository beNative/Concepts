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

unit DDuce.Editor.Factories.Menus;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Menus, Vcl.ActnList,

  DDuce.Editor.Interfaces, DDuce.Editor.Resources;

type
  TEditorMenusFactory = class(TInterfacedObject, IEditorMenusFactory)
  strict private
    FActions : IEditorActions;
    FMenus   : IEditorMenus;

    function CreateMenuItem(
      AParent : TMenuItem;
      AAction : TBasicAction = nil
    ): TMenuItem; overload;
    function CreateMenuItem(
            AParent     : TMenuItem;
      const AActionName : string
    ): TMenuItem; overload;

  public
    constructor Create(
      AActions  : IEditorActions;
      AMenus    : IEditorMenus
    );
    procedure BeforeDestruction; override;

    function CreateFileMenu(AMenu : TMenu): TMenuItem;
    function CreateEditMenu(AMenu : TMenu): TMenuItem;
    function CreateSelectionMenu(AMenu : TMenu): TMenuItem;
    function CreateInsertMenu(AMenu: TMenu): TMenuItem;
    function CreateSearchMenu(AMenu: TMenu): TMenuItem;
    function CreateToolsMenu(AMenu: TMenu): TMenuItem;
    function CreateViewsMenu(AMenu: TMenu): TMenuItem;
    function CreateSettingsMenu(AMenu: TMenu): TMenuItem;
    function CreateHighlightersMenu(AMenu: TMenu): TMenuItem;
    function CreateHelpMenu(AMenu: TMenu): TMenuItem;
    function CreateMainMenu(AOwner: TComponent): TMainMenu;
  end;

implementation

uses
  Spring;

{$REGION 'construction and destruction'}
constructor TEditorMenusFactory.Create(AActions: IEditorActions;
  AMenus: IEditorMenus);
begin
  inherited Create;
  FActions := AActions;
  FMenus   := AMenus;
end;

procedure TEditorMenusFactory.BeforeDestruction;
begin
  FActions := nil;
  FMenus   := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'private methods'}
function TEditorMenusFactory.CreateMenuItem(AParent: TMenuItem;
  AAction: TBasicAction): TMenuItem;
var
  MI: TMenuItem;
begin
  Guard.CheckNotNull(AParent, 'AParent');
  if not Assigned(AAction) then
  begin
    MI := TMenuItem.Create(AParent.Owner);
    MI.Caption := cLineCaption;
    AParent.Add(MI);
  end
  else
  begin
    MI := TMenuItem.Create(AParent.Owner);
    MI.Action := AAction;
    if (AAction is TAction) and (TAction(AAction).GroupIndex > 0) then
    begin
      MI.RadioItem := True;
    end;
    AParent.Add(MI);
  end;
  Result := MI;
end;

function TEditorMenusFactory.CreateMenuItem(AParent: TMenuItem;
  const AActionName: string): TMenuItem;
var
  A : TBasicAction;
begin
  A := FActions[AActionName];
  Guard.CheckNotNull(A, AActionName);
  Result := CreateMenuItem(AParent, A)
end;
{$ENDREGION}

{$REGION 'public methods'}
function TEditorMenusFactory.CreateFileMenu(AMenu: TMenu): TMenuItem;
var
  M   : TMenuItem;
  MI  : TMenuItem;
  SMI : TMenuItem;
begin
  Guard.CheckNotNull(AMenu, 'AMenu');
  MI := TMenuItem.Create(AMenu.Owner);
  MI.Caption := SFileMenuCaption;
  AMenu.Items.Add(MI);
  CreateMenuItem(MI, 'actNew');
  CreateMenuItem(MI, 'actOpen');
  CreateMenuItem(MI, 'actSave');
  CreateMenuItem(MI, 'actSaveAs');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actReload');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actCreateDesktopLink');
  CreateMenuItem(MI);
  SMI := CreateMenuItem(MI, 'actEncodingMenu');
  for M in FMenus.EncodingPopupMenu.Items do
  begin
    CreateMenuItem(SMI, M.Action.Name);
  end;
  SMI := CreateMenuItem(MI, 'actLineBreakStyleMenu');
  for M in FMenus.LineBreakStylePopupMenu.Items do
  begin
    CreateMenuItem(SMI, M.Action.Name);
  end;
  CreateMenuItem(MI, 'actClose');
  CreateMenuItem(MI, 'actCloseOthers');
  CreateMenuItem(MI, 'actExit');
  Result := MI;
end;

function TEditorMenusFactory.CreateEditMenu(AMenu: TMenu): TMenuItem;
var
  MI : TMenuItem;
begin
  Guard.CheckNotNull(AMenu, 'AMenu');
  MI := TMenuItem.Create(AMenu.Owner);
  MI.Caption := '&Edit';
  AMenu.Items.Add(MI);
  CreateMenuItem(MI, 'actUndo');
  CreateMenuItem(MI, 'actRedo');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actCut');
  CreateMenuItem(MI, 'actCopy');
  CreateMenuItem(MI, 'actPaste');
  CreateMenuItem(MI, 'actSelectAll');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actCopyFileName');
  CreateMenuItem(MI, 'actCopyFilePath');
  CreateMenuItem(MI, 'actCopyFullPath');
  Result := MI;
end;

function TEditorMenusFactory.CreateSelectionMenu(AMenu: TMenu): TMenuItem;
var
  MI  : TMenuItem;
  SMI : TMenuItem;
begin
  Guard.CheckNotNull(AMenu, 'AMenu');
  MI := TMenuItem.Create(AMenu.Owner);
  MI.Caption := SSeLectionMenuCaption;
  AMenu.Items.Add(MI);
  CreateMenuItem(MI, 'actAlignSelection');
  CreateMenuItem(MI, 'actSortSelectedLines');
  CreateMenuItem(MI, 'actSyncEdit');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actIndent');
  CreateMenuItem(MI, 'actUnindent');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actLowerCaseSelection');
  CreateMenuItem(MI, 'actUpperCaseSelection');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actConvertTabsToSpaces');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actQuoteSelection');
  CreateMenuItem(MI, 'actDequoteSelection');
  CreateMenuItem(MI, 'actQuoteLines');
  CreateMenuItem(MI, 'actQuoteLinesAndDelimit');
  CreateMenuItem(MI, 'actDequoteLines');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actToggleBlockCommentSelection');
  CreateMenuItem(MI, 'actPascalStringOfSelection');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actStripFirstChar');
  CreateMenuItem(MI, 'actStripLastChar');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actStripComments');
  CreateMenuItem(MI, 'actStripMarkup');
  CreateMenuItem(MI, 'actCompressSpace');
  CreateMenuItem(MI, 'actCompressWhitespace');
  CreateMenuItem(MI, 'actMergeBlankLines');
  CreateMenuItem(MI);
  SMI := CreateMenuItem(MI, 'actSelectionEncodeMenu');
  CreateMenuItem(SMI, 'actEncodeBase64');
  CreateMenuItem(SMI, 'actEncodeURL');
  CreateMenuItem(SMI, 'actEncodeXML');
  SMI := CreateMenuItem(MI, 'actSelectionDecodeMenu');
  CreateMenuItem(SMI, 'actDecodeBase64');
  CreateMenuItem(SMI, 'actDecodeURL');
  CreateMenuItem(SMI, 'actDecodeXML');
  Result := MI;
end;

function TEditorMenusFactory.CreateInsertMenu(AMenu: TMenu): TMenuItem;
var
  MI: TMenuItem;
begin
  Guard.CheckNotNull(AMenu, 'AMenu');
  MI := TMenuItem.Create(AMenu.Owner);
  MI.Caption := SInsertMenuCaption;
  AMenu.Items.Add(MI);
  CreateMenuItem(MI, 'actInsertColorValue');
  CreateMenuItem(MI, 'actInsertGUID');
  Result := MI;
end;

function TEditorMenusFactory.CreateSearchMenu(AMenu: TMenu): TMenuItem;
var
  MI: TMenuItem;
begin
  Guard.CheckNotNull(AMenu, 'AMenu');
  MI := TMenuItem.Create(AMenu.Owner);
  MI.Caption := SSearchMenuCaption;
  AMenu.Items.Add(MI);
  CreateMenuItem(MI, 'actSearch');
  CreateMenuItem(MI, 'actFindAllOccurences');
  CreateMenuItem(MI, 'actSearchReplace');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actFindNext');
  CreateMenuItem(MI, 'actFindPrevious');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actFindNextWord');
  CreateMenuItem(MI, 'actFindPrevWord');
  Result := MI;
end;

function TEditorMenusFactory.CreateToolsMenu(AMenu: TMenu): TMenuItem;
var
  MI: TMenuItem;
begin
  Guard.CheckNotNull(AMenu, 'AMenu');
  MI := TMenuItem.Create(AMenu.Owner);
  MI.Caption := SToolsMenuCaption;
  AMenu.Items.Add(MI);
  CreateMenuItem(MI, 'actShowCodeShaper');
  CreateMenuItem(MI, 'actShowCodeFilter');
  CreateMenuItem(MI, 'actShowCharacterMap');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actSmartSelect');
  CreateMenuItem(MI, 'actFormat');
  CreateMenuItem(MI, 'actAutoGuessHighlighter');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actMonitorChanges');
  Result := MI;
end;

function TEditorMenusFactory.CreateViewsMenu(AMenu: TMenu): TMenuItem;
var
  MI: TMenuItem;
begin
  Guard.CheckNotNull(AMenu, 'AMenu');
  MI := TMenuItem.Create(AMenu.Owner);
  MI.Caption := SViewMenuCaption;
  AMenu.Items.Add(MI);
  CreateMenuItem(MI, 'actShowViews');
  CreateMenuItem(MI, 'actShowActions');
//  CreateMenuItem(MI, 'actShowHTMLViewer');
//  CreateMenuItem(MI, 'actShowStructureViewer');
//  CreateMenuItem(MI, 'actShowHexEditor');
//  CreateMenuItem(MI, 'actShowScriptEditor');
  Result := MI;
end;

function TEditorMenusFactory.CreateSettingsMenu(AMenu: TMenu): TMenuItem;
var
  MI: TMenuItem;
begin
  Guard.CheckNotNull(AMenu, 'AMenu');
  MI := TMenuItem.Create(AMenu.Owner);
  MI.Caption := SSettingsMenuCaption;
  AMenu.Items.Add(MI);
  CreateMenuItem(MI, 'actSettings');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actShowMinimap');
  CreateMenuItem(MI, 'actShowSearchmap');
  CreateMenuItem(MI, 'actShowSpecialCharacters');
  CreateMenuItem(MI, 'actShowIndentGuides');
  CreateMenuItem(MI, 'actToggleWordWrap');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actIncFontSize');
  CreateMenuItem(MI, 'actDecFontSize');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actStayOnTop');
  CreateMenuItem(MI, 'actToggleMaximized');
  CreateMenuItem(MI);
  CreateMenuItem(MI, 'actSingleInstance');
  Result := MI;
end;

function TEditorMenusFactory.CreateHighlightersMenu(AMenu: TMenu): TMenuItem;
var
  MI : TMenuItem;
  M  : TMenuItem;
begin
  Guard.CheckNotNull(AMenu, 'AMenu');
  MI := TMenuItem.Create(AMenu.Owner);
  MI.Caption := SHighlightersMenuCaption;
  AMenu.Items.Add(MI);
  for M in FMenus.HighlighterPopupMenu.Items do
  begin
    CreateMenuItem(MI, M.Action.Name);
  end;
  Result := MI;
end;

function TEditorMenusFactory.CreateHelpMenu(AMenu: TMenu): TMenuItem;
var
  MI: TMenuItem;
begin
  Guard.CheckNotNull(AMenu, 'AMenu');
  MI := TMenuItem.Create(AMenu.Owner);
  MI.Caption := SHelpMenuCaption;
  AMenu.Items.Add(MI);
  //CreateMenuItem(MI, 'actAbout');
  Result := MI;
end;

function TEditorMenusFactory.CreateMainMenu(AOwner: TComponent): TMainMenu;
var
  MM : TMainMenu;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  MM := TMainMenu.Create(AOwner);
  MM.Images := FActions.ActionList.Images;
  CreateFileMenu(MM);
  CreateEditMenu(MM);
  CreateSelectionMenu(MM);
  CreateSearchMenu(MM);
  CreateToolsMenu(MM);
  CreateSettingsMenu(MM);
  CreateViewsMenu(MM);
  CreateHighlightersMenu(MM);
  //CreateHelpMenu(MM);
  Result := MM;
end;
{$ENDREGION}

end.

