{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.Manager;

{$REGION'documentation'}
{
  Datamodule holding common actions, menu's to manage one or more IEditorView
  instances.

  REMARKS:
    - CTRL-ALT-SHIFT keycombinations are used for experimental features

  TODO:
   - move out creation of dependant modules and use a factory to wire those
     objects => eases creation of unit tests.
   - wire Actions to the active view's commands which are stored in the
     Keystrokes collection property of TSynEdit.
   - add bookmark actions and map shortcuts to corresponding editor commands.
   - macro support
   - spellcheck support
   - apply consistent casing for word under cursor/all words ? => dangerous for strings
   - customizable shortcuts for actions.
   - show a hintwindow of the selection with the proposed operation!
   - list of all supported actions, category, shortcut, description (treeview)
   - goto line
   - goto position

  KNOWN ISSUES
   - copy to clipboard
      - as HTML object has a incomplete HTML closing tag
      - as RTF object does the same as copy as RTF text

  Adding actions:
    - if checkable then AutoCheck should be set to True
    - the OnExecute should be handled in this unit
    - if the action alters application settings, the Settings instance should
      be adjusted in the action handler. When Settings change, a (multicast)
      notification will be dispatched which can be handled by any module that
      needs to be notified (observer pattern).

  Use a IEditorManager instance to create IEditorView instances. In most cases
  you only need a single manager instance.
    - EditorManager.OpenFile(<<filename>>)
    - EditorManager.NewFile(<<filename>>)

    - When a new view is added to the list of editorviews, the OnAddEditorView
      event is dispatched. The application can handle this event for example to
      dock the view.
    - A view can also be added directly to the list using:
      EditorManager.Views.Add(..)

      EditorManager.Actions   : IEditorActions
                   .Commands  : IEditorCommands
                   .Events    : IEditorEvents
                   .Menus     : IEditorMenus
                   .Settings  : IEditorSettings
                   .ToolViews : IEditorToolViews
                   .Views     : IEditorViews

      IEditorCommands
        Set of commands that can be executed on the active editor view.
}
{$ENDREGION}

interface

uses
  System.Classes, System.SysUtils, System.Variants, System.Actions,
  System.ImageList,
  Vcl.Controls, Vcl.ActnList, Vcl.Menus, Vcl.Dialogs, Vcl.Forms, Vcl.ImgList,
  Vcl.ActnPopup,

  Spring.Collections,

  BCEditor.Editor, BCEditor.Editor.KeyCommands,

  DDuce.Editor.Types, DDuce.Editor.Interfaces, DDuce.Editor.Resources,
  DDuce.Editor.View, DDuce.Editor.Highlighters,

  DDuce.Logger;

type
  TPopupMenu = DDuce.Editor.Interfaces.TPopupMenu;

type
  TdmEditorManager = class(TDataModule, IEditorManager,
                                        IEditorActions,
                                        IEditorView,   // active view
                                        IEditorViews,
                                        IEditorEvents,
                                        IEditorCommands,
                                        IEditorMenus,
                                        IEditorSettings,
                                        IEditorSearchEngine
  )
    {$REGION'designer controls'}
    aclActions                        : TActionList;
    actAbout                          : TAction;
    actAlignSelection                 : TAction;
    actAutoFormatXML                  : TAction;
    actAutoGuessHighlighter           : TAction;
    actClear                          : TAction;
    actClipboardMenu                  : TAction;
    actClose                          : TAction;
    actCloseOthers                    : TAction;
    actCompressSpace                  : TAction;
    actCompressWhitespace             : TAction;
    actConvertTabsToSpaces            : TAction;
    actCopy                           : TAction;
    actCopyFileName                   : TAction;
    actCopyFilePath                   : TAction;
    actCopyFullPath                   : TAction;
    actCopyHTMLTextToClipboard        : TAction;
    actCopyRTFTextToClipboard         : TAction;
    actCopyRTFToClipboard             : TAction;
    actCopytHTMLToClipboard           : TAction;
    actCopyToClipboard                : TAction;
    actCopyWikiTextToClipboard        : TAction;
    actCopyWikiToClipboard            : TAction;
    actCreateDesktopLink              : TAction;
    actCut                            : TAction;
    actDecFontSize                    : TAction;
    actDecodeBase64                   : TAction;
    actDecodeURL                      : TAction;
    actDecodeXML                      : TAction;
    actDelete                         : TAction;
    actDequoteLines                   : TAction;
    actDequoteSelection               : TAction;
    actEncodeBase64                   : TAction;
    actEncodeURL                      : TAction;
    actEncodeXML                      : TAction;
    actEncodingMenu                   : TAction;
    actExecuteScriptOnSelection       : TAction;
    actExit                           : TAction;
    actExportMenu                     : TAction;
    actExportToHTML                   : TAction;
    actExportToRTF                    : TAction;
    actExportToWiki                   : TAction;
    actFileMenu                       : TAction;
    actFindAllOccurences              : TAction;
    actFindNext                       : TAction;
    actFindNextWord                   : TAction;
    actFindPrevious                   : TAction;
    actFindPrevWord                   : TAction;
    actFoldLevel0                     : TAction;
    actFoldLevel1                     : TAction;
    actFoldLevel10                    : TAction;
    actFoldLevel2                     : TAction;
    actFoldLevel3                     : TAction;
    actFoldLevel4                     : TAction;
    actFoldLevel5                     : TAction;
    actFoldLevel6                     : TAction;
    actFoldLevel7                     : TAction;
    actFoldLevel8                     : TAction;
    actFoldLevel9                     : TAction;
    actFoldMenu                       : TAction;
    actFormat                         : TAction;
    actHelp                           : TAction;
    actHighlighter                    : TAction;
    actHighlighterMenu                : TAction;
    actIncFontSize                    : TAction;
    actIndent                         : TAction;
    actInsertColorValue               : TAction;
    actInsertGUID                     : TAction;
    actInsertMenu                     : TAction;
    actInspect                        : TAction;
    actLineBreakStyleMenu             : TAction;
    actLowerCaseSelection             : TAction;
    actMergeBlankLines                : TAction;
    actMonitorChanges                 : TAction;
    actNew                            : TAction;
    actOpen                           : TAction;
    actOpenFileAtCursor               : TAction;
    actPageSetup                      : TAction;
    actPascalStringOfSelection        : TAction;
    actPaste                          : TAction;
    actPlaybackMacro                  : TAction;
    actPrint                          : TAction;
    actPrintPreview                   : TAction;
    actQuoteLines                     : TAction;
    actQuoteLinesAndDelimit           : TAction;
    actQuoteSelection                 : TAction;
    actRecordMacro                    : TAction;
    actRedo                           : TAction;
    actReload                         : TAction;
    actSave                           : TAction;
    actSaveAll                        : TAction;
    actSaveAs                         : TAction;
    actSearch                         : TAction;
    actSearchMenu                     : TAction;
    actSearchReplace                  : TAction;
    actSelectAll                      : TAction;
    actSelectionDecodeMenu            : TAction;
    actSelectionEncodeMenu            : TAction;
    actSelectionInfo                  : TAction;
    actSelectionMenu                  : TAction;
    actSelectionModeMenu              : TAction;
    actSelectMenu                     : TAction;
    actSettings                       : TAction;
    actSettingsMenu                   : TAction;
    actShowActions                    : TAction;
    actShowCharacterMap               : TAction;
    actShowCodeFilter                 : TAction;
    actShowCodeShaper                 : TAction;
    actShowFilterTest                 : TAction;
    actShowHexEditor                  : TAction;
    actShowHTMLViewer                 : TAction;
    actShowIndentGuides               : TAction;
    actShowPreview                    : TAction;
    actShowScriptEditor               : TAction;
    actShowSpecialCharacters          : TAction;
    actShowStructureViewer            : TAction;
    actShowTest                       : TAction;
    actShowViews                      : TAction;
    actSingleInstance                 : TAction;
    actSmartSelect                    : TAction;
    actSortSelectedLines              : TAction;
    actStayOnTop                      : TAction;
    actStripComments                  : TAction;
    actStripFirstChar                 : TAction;
    actStripLastChar                  : TAction;
    actStripMarkup                    : TAction;
    actSyncEdit                       : TAction;
    actToggleBlockCommentSelection    : TAction;
    actToggleComment                  : TAction;
    actToggleFoldLevel                : TAction;
    actToggleHighlighter              : TAction;
    actToggleMaximized                : TAction;
    actShowMinimap                    : TAction;
    actToggleWordWrap                 : TAction;
    actUndo                           : TAction;
    actUnindent                       : TAction;
    actUpperCaseSelection             : TAction;
    dlgColor                          : TColorDialog;
    dlgOpen                           : TOpenDialog;
    dlgSave                           : TSaveDialog;
    imlMain                           : TImageList;
    MenuItem1                         : TMenuItem;
    MenuItem43                        : TMenuItem;
    MenuItem44                        : TMenuItem;
    MenuItem45                        : TMenuItem;
    MenuItem46                        : TMenuItem;
    MenuItem47                        : TMenuItem;
    MenuItem48                        : TMenuItem;
    MenuItem49                        : TMenuItem;
    MenuItem50                        : TMenuItem;
    MenuItem51                        : TMenuItem;
    MenuItem52                        : TMenuItem;
    ppmClipboard                      : TPopupMenu;
    ppmEditor                         : TPopupMenu;
    ppmEncoding                       : TPopupMenu;
    ppmExport                         : TPopupMenu;
    ppmFile                           : TPopupMenu;
    ppmFold                           : TPopupMenu;
    ppmHighLighters                   : TPopupMenu;
    ppmInsert                         : TPopupMenu;
    ppmLineBreakStyle                 : TPopupMenu;
    ppmSearch                         : TPopupMenu;
    ppmSelect                         : TPopupMenu;
    ppmSelection                      : TPopupMenu;
    ppmSelectionDecode                : TPopupMenu;
    ppmSelectionEncode                : TPopupMenu;
    ppmSelectionMode                  : TPopupMenu;
    ppmSettings                       : TPopupMenu;
    actShowSearchmap: TAction;
    {$ENDREGION}

    {$REGION'action handlers'}
    procedure aclActionsExecute(AAction: TBasicAction; var Handled: Boolean);
    procedure actAboutExecute(Sender: TObject);
    procedure actAlignAndSortSelectionExecute(Sender: TObject);
    procedure actAlignSelectionExecute(Sender: TObject);
    procedure actAssociateFilesExecute(Sender: TObject);
    procedure actAutoFormatXMLExecute(Sender: TObject);
    procedure actAutoGuessHighlighterExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCloseOthersExecute(Sender: TObject);
    procedure actCompressSpaceExecute(Sender: TObject);
    procedure actCompressWhitespaceExecute(Sender: TObject);
    procedure actConvertTabsToSpacesExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCopyFileNameExecute(Sender: TObject);
    procedure actCopyFilePathExecute(Sender: TObject);
    procedure actCopyFullPathExecute(Sender: TObject);
    procedure actCopyHTMLTextToClipboardExecute(Sender: TObject);
    procedure actCopyRTFTextToClipboardExecute(Sender: TObject);
    procedure actCopyRTFToClipboardExecute(Sender: TObject);
    procedure actCopytHTMLToClipboardExecute(Sender: TObject);
    procedure actCopyToClipboardExecute(Sender: TObject);
    procedure actCopyWikiTextToClipboardExecute(Sender: TObject);
    procedure actCopyWikiToClipboardExecute(Sender: TObject);
    procedure actCreateDesktopLinkExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actDecFontSizeExecute(Sender: TObject);
    procedure actDecodeBase64Execute(Sender: TObject);
    procedure actDecodeURLExecute(Sender: TObject);
    procedure actDecodeXMLExecute(Sender: TObject);
    procedure actDequoteLinesExecute(Sender: TObject);
    procedure actDequoteSelectionExecute(Sender: TObject);
    procedure actEncodeBase64Execute(Sender: TObject);
    procedure actEncodeURLExecute(Sender: TObject);
    procedure actEncodeXMLExecute(Sender: TObject);
    procedure actEncodingExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actExportToHTMLExecute(Sender: TObject);
    procedure actExportToRTFExecute(Sender: TObject);
    procedure actExportToWikiExecute(Sender: TObject);
    procedure actFindAllOccurencesExecute(Sender: TObject);
    procedure actFindNextExecute(Sender: TObject);
    procedure actFindNextWordExecute(Sender: TObject);
    procedure actFindPreviousExecute(Sender: TObject);
    procedure actFindPrevWordExecute(Sender: TObject);
    procedure actFoldLevel0Execute(Sender: TObject);
    procedure actFoldLevel10Execute(Sender: TObject);
    procedure actFoldLevel1Execute(Sender: TObject);
    procedure actFoldLevel2Execute(Sender: TObject);
    procedure actFoldLevel3Execute(Sender: TObject);
    procedure actFoldLevel4Execute(Sender: TObject);
    procedure actFoldLevel5Execute(Sender: TObject);
    procedure actFoldLevel6Execute(Sender: TObject);
    procedure actFoldLevel7Execute(Sender: TObject);
    procedure actFoldLevel8Execute(Sender: TObject);
    procedure actFoldLevel9Execute(Sender: TObject);
    procedure actFormatExecute(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure actHighlighterExecute(Sender: TObject);
    procedure actIncFontSizeExecute(Sender: TObject);
    procedure actIndentExecute(Sender: TObject);
    procedure actInsertColorValueExecute(Sender: TObject);
    procedure actInsertGUIDExecute(Sender: TObject);
    procedure actInspectExecute(Sender: TObject);
    procedure actLineBreakStyleExecute(Sender: TObject);
    procedure actLowerCaseSelectionExecute(Sender: TObject);
    procedure actMergeBlankLinesExecute(Sender: TObject);
    procedure actMonitorChangesExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actOpenFileAtCursorExecute(Sender: TObject);
    procedure actPageSetupExecute(Sender: TObject);
    procedure actPascalStringOfSelectionExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actPlaybackMacroExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actPrintPreviewExecute(Sender: TObject);
    procedure actQuoteLinesAndDelimitExecute(Sender: TObject);
    procedure actQuoteLinesExecute(Sender: TObject);
    procedure actQuoteSelectionExecute(Sender: TObject);
    procedure actRecordMacroExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actReloadExecute(Sender: TObject);
    procedure actSaveAllExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSearchExecute(Sender: TObject);
    procedure actSearchReplaceExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectionInfoExecute(Sender: TObject);
    procedure actSelectionModeExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actShowActionsExecute(Sender: TObject);
    procedure actShowCharacterMapExecute(Sender: TObject);
    procedure actShowCodeFilterExecute(Sender: TObject);
    procedure actShowCodeShaperExecute(Sender: TObject);
    procedure actShowFilterTestExecute(Sender: TObject);
    procedure actShowHexEditorExecute(Sender: TObject);
    procedure actShowHTMLViewerExecute(Sender: TObject);
    procedure actShowPreviewExecute(Sender: TObject);
    procedure actShowScriptEditorExecute(Sender: TObject);
    procedure actShowSpecialCharactersExecute(Sender: TObject);
    procedure actShowStructureViewerExecute(Sender: TObject);
    procedure actShowTestExecute(Sender: TObject);
    procedure actShowViewsExecute(Sender: TObject);
    procedure actSingleInstanceExecute(Sender: TObject);
    procedure actSmartSelectExecute(Sender: TObject);
    procedure actSortSelectedLinesExecute(Sender: TObject);
    procedure actStayOnTopExecute(Sender: TObject);
    procedure actStripCommentsExecute(Sender: TObject);
    procedure actStripFirstCharExecute(Sender: TObject);
    procedure actStripLastCharExecute(Sender: TObject);
    procedure actStripMarkupExecute(Sender: TObject);
    procedure actSyncEditExecute(Sender: TObject);
    procedure actTestFormExecute(Sender: TObject);
    procedure actToggleBlockCommentSelectionExecute(Sender: TObject);
    procedure actToggleCommentExecute(Sender: TObject);
    procedure actToggleFoldLevelExecute(Sender: TObject);
    procedure actToggleHighlighterExecute(Sender: TObject);
    procedure actToggleMaximizedExecute(Sender: TObject);
    procedure actShowMinimapExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actUnindentExecute(Sender: TObject);
    procedure actUpperCaseSelectionExecute(Sender: TObject);
    procedure actToggleWordWrapExecute(Sender: TObject);
    procedure actShowIndentGuidesExecute(Sender: TObject);
    procedure actShowSearchmapExecute(Sender: TObject);
    {$ENDREGION}

  private
    FChanged          : Boolean;
    FPersistSettings  : Boolean;
    FToolViews        : IEditorToolViews;
    FEvents           : IEditorEvents;
    FCommands         : IEditorCommands;
    FSettings         : IEditorSettings;
    FActiveView       : IEditorView;
    FViewList         : IList<IEditorView>;
    FSearchEngine     : IEditorSearchEngine;

    {$REGION'property access methods'}
    function GetActionList: TActionList;
    function GetActions: IEditorActions;
    function GetClipboardPopupMenu: TPopupMenu;
    function GetCommands: IEditorCommands;
    function GetEditorPopupMenu: TPopupMenu;
    function GetEncodingPopupMenu: TPopupMenu;
    function GetEvents: IEditorEvents;
    function GetExportPopupMenu: TPopupMenu;
    function GetFilePopupMenu: TPopupMenu;
    function GetFoldPopupMenu: TPopupMenu;
    function GetHighlighterPopupMenu: TPopupMenu;
    function GetInsertPopupMenu: TPopupMenu;
    function GetItem(AName: string): TCustomAction;
    function GetKeyCommands: TBCEditorKeyCommands;
    function GetLineBreakStylePopupMenu: TPopupMenu;
    function GetMenus: IEditorMenus;
    function GetPersistSettings: Boolean;
    function GetSearchEngine: IEditorSearchEngine;
    function GetSearchPopupMenu: TPopupMenu;
    function GetSelectionDecodePopupMenu: TPopupMenu;
    function GetSelectionEncodePopupMenu: TPopupMenu;
    function GetSelectionModePopupMenu: TPopupMenu;
    function GetSelectionPopupMenu: TPopupMenu;
    function GetSelectPopupMenu: TPopupMenu;
    function GetSettings: IEditorSettings;
    function GetActiveView: IEditorView;
    function GetHighlighters: THighlighters;
    function GetSettingsPopupMenu: TPopupMenu;
    function GetToolViews: IEditorToolViews;
    function GetView(AIndex: Integer): IEditorView;
    function GetViewByFileName(AFileName: string): IEditorView;
    function GetViewByName(AName: string): IEditorView;
    function GetViewCount: Integer;
    function GetViewList: IList<IEditorView>;
    function GetViews: IEditorViews;
    procedure SetActiveView(AValue: IEditorView);
    procedure SetPersistSettings(const AValue: Boolean);
    {$ENDREGION}

    function AddMenuItem(
      AParent : TMenuItem;
      AAction : TBasicAction = nil
    ): TMenuItem; overload;
    function AddMenuItem(
      AParent : TMenuItem;
      AMenu   : TMenu
    ): TMenuItem; overload;

    // event handlers
    procedure EditorSettingsChanged(ASender: TObject);

    procedure InitializePopupMenus;
    procedure CreateActions;
    procedure RegisterToolViews;

    procedure BuildClipboardPopupMenu;
    procedure BuildEncodingPopupMenu;
    procedure BuildLineBreakStylePopupMenu;
    procedure BuildFilePopupMenu;
    procedure BuildHighlighterPopupMenu;
    procedure BuildInsertPopupMenu;
    procedure BuildSearchPopupMenu;
    procedure BuildSelectPopupMenu;
    procedure BuildSelectionPopupMenu;
    procedure BuildSelectionEncodePopupMenu;
    procedure BuildSelectionDecodePopupMenu;
    procedure BuildSelectionModePopupMenu;
    procedure BuildSettingsPopupMenu;
    procedure BuildFoldPopupMenu;
    procedure BuildEditorPopupMenu;
    procedure BuildExportPopupMenu;

  protected
    procedure ActiveViewChanged;

    function SaveFile(
      const AFileName   : string = '';
            AShowDialog : Boolean = False
    ): Boolean;
    function ViewsModified: Boolean;

    procedure ExportLines(
      AFormat       : string;
      AToClipBoard  : Boolean = True;
      ANativeFormat : Boolean = True
    );

    {$REGION'IEditorViews'}
    function IEditorViews.Add = AddView;
    function IEditorViews.GetCount = GetViewCount;
    function IEditorViews.Delete = DeleteView;
    procedure IEditorViews.Clear = ClearViews;
    function IEditorViews.GetEnumerator = GetViewsEnumerator;

    function AddView(
      const AName        : string = '';
      const AFileName    : string = '';
      const AHighlighter : string = ''
    ): IEditorView;

    function DeleteView(AIndex: Integer): Boolean; overload;
    function DeleteView(AView: IEditorView): Boolean; overload;
    function DeleteView(const AName: string): Boolean; overload;
    procedure ClearViews(AExceptActive: Boolean = False);
    function GetViewsEnumerator: TEditorViewListEnumerator;
    {$ENDREGION}

    procedure ShowToolView(
       const AName      : string;
             AShowModal : Boolean;
             ASetFocus  : Boolean
    );

    // TComponent overrides
    procedure Notification(
      AComponent : TComponent;
      Operation  : TOperation
    ); override;

    procedure UpdateActions;
    procedure UpdateEncodingActions;
    procedure UpdateLineBreakStyleActions;
    procedure UpdateSelectionModeActions;
    procedure UpdateHighLighterActions;
    procedure UpdateFoldingActions;
    procedure UpdateFileActions;

    {$REGION'IEditorManager'}
    function ActivateView(const AName: string): Boolean;
    function OpenFile(const AFileName: string): IEditorView;
    function NewFile(
      const AFileName  : string;
      const AText      : string = ''
    ): IEditorView;
    {$ENDREGION}

    // properties
    property ActionList: TActionList
      read GetActionList;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    {$REGION'IEditorMenus'}
    property ClipboardPopupMenu: TPopupMenu
      read GetClipboardPopupMenu;

    property EditorPopupMenu: TPopupMenu
      read GetEditorPopupMenu;

    property EncodingPopupMenu: TPopupMenu
      read GetEncodingPopupMenu;

    property LineBreakStylePopupMenu: TPopupMenu
      read GetLineBreakStylePopupMenu;

    property ExportPopupMenu: TPopupMenu
      read GetExportPopupMenu;

    property FilePopupMenu: TPopupMenu
      read GetFilePopupMenu;

    property FoldPopupMenu: TPopupMenu
      read GetFoldPopupMenu;

    property HighlighterPopupMenu: TPopupMenu
      read GetHighlighterPopupMenu;

    property InsertPopupMenu: TPopupMenu
      read GetInsertPopupMenu;

    property SearchPopupMenu: TPopupMenu
      read GetSearchPopupMenu;

    property SelectPopupMenu: TPopupMenu
      read GetSelectPopupMenu;

    property SelectionEncodePopupMenu: TPopupMenu
      read GetSelectionEncodePopupMenu;

    property SelectionDecodePopupMenu: TPopupMenu
      read GetSelectionDecodePopupMenu;

    property SelectionPopupMenu: TPopupMenu
      read GetSelectionPopupMenu;

    property SelectionModePopupMenu: TPopupMenu
      read GetSelectionModePopupMenu;

    property SettingsPopupMenu: TPopupMenu
      read GetSettingsPopupMenu;
    {$ENDREGION}

    {$REGION'IEditorManager'}
    property PersistSettings: Boolean
      read GetPersistSettings write SetPersistSettings;

    property Highlighters: THighlighters
      read GetHighlighters;

    { Set/get the reference to the active view. }
    property ActiveView: IEditorView
      read GetActiveView write SetActiveView;

    { Delegates the implementation of IEditorEvents to an internal object. }
    property Events: IEditorEvents
      read GetEvents implements IEditorEvents;

    property ToolViews: IEditorToolViews
      read GetToolViews;

    property Menus: IEditorMenus
      read GetMenus;

    property Actions: IEditorActions
      read GetActions;

    { Delegates the implementation of IEditorCommands to an internal object. }
    property Commands: IEditorCommands
      read GetCommands implements IEditorCommands;

    property KeyCommands: TBCEditorKeyCommands
      read GetKeyCommands;

    { Delegates the implementation of IEditorSettings to an internal object. }
    property Settings: IEditorSettings
      read GetSettings implements IEditorSettings;

    { Delegates the implementation of IEditorSearchEngine to an internal object. }
    property SearchEngine: IEditorSearchEngine
      read GetSearchEngine implements IEditorSearchEngine;

    { Delegates the implementation of IEditorView to the active editor view. }
    property View: IEditorView
      read GetActiveView implements IEditorView;
    {$ENDREGION}

    {$REGION'IEditorViews'}
    property Views[AIndex: Integer]: IEditorView
      read GetView;

    { Looks for the view with the given component name. If the view is not found
      the active view is returned. }
    property ViewByName[AName: string]: IEditorView
      read GetViewByName;

    { Looks for the view with the given filename. If the view is not found nil
      is returned. }
    property ViewByFileName[AFileName: string]: IEditorView
      read GetViewByFileName;

    property ViewList: IList<IEditorView>
      read GetViewList;

    property ViewCount: Integer
      read GetViewCount;
    {$ENDREGION}

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : IEditorSettings
    ); reintroduce; virtual;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{$R *.dfm}

uses
  Winapi.Windows,
  System.StrUtils, System.TypInfo,
  Vcl.Clipbrd,

  Spring,

  DDuce.Editor.Settings, DDuce.Editor.Utils,

  DDuce.Editor.ToolView.Manager,

  DDuce.Editor.Filter.ToolView,
  DDuce.Editor.Search.ToolView,
  DDuce.Editor.SortStrings.ToolView,
  DDuce.Editor.ViewList.ToolView,
  DDuce.Editor.ActionList.ToolView,
  DDuce.Editor.CharacterMap.ToolView,
  DDuce.Editor.AlignLines.ToolView,
  DDuce.Editor.Test.ToolView,
  DDuce.Editor.SelectionInfo.ToolView,

  DDuce.Editor.Filter.Settings,
  DDuce.Editor.Search.Engine.Settings,
  DDuce.Editor.SortStrings.Settings,
  DDuce.Editor.Search.Engine,

  DDuce.Editor.Events, DDuce.Editor.Commands;
const
  // prefixes used for naming dynamically created actions.
  ACTION_PREFIX_ENCODING       = 'actEncoding';
  ACTION_PREFIX_HIGHLIGHTER    = 'actHighlighter';
  ACTION_PREFIX_SELECTIONMODE  = 'actSelectionMode';
  ACTION_PREFIX_LINEBREAKSTYLE = 'actLineBreakStyle';

{$REGION'construction and destruction'}
constructor TdmEditorManager.Create(AOwner: TComponent;
  ASettings: IEditorSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
  if not Assigned(FSettings) then
    FSettings := TEditorSettings.Create(Self);
end;

procedure TdmEditorManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FPersistSettings  := False;
  { TODO -oTS : Move the construction of these objects to the outside and pass
   the instances to the constructor (dependency injection). This will allow to
   create unit tests for each module. }
  FToolViews        := TToolViews.Create(Self);
  FEvents           := TEditorEvents.Create(Self);
  FCommands         := TEditorCommands.Create(Self);
  FViewList         := TCollections.CreateInterfaceList<IEditorView>;;
  FSearchEngine     := TSearchEngine.Create(Self);
  FSettings.OnChanged.Add(EditorSettingsChanged);
  RegisterToolViews;
  CreateActions;
  InitializePopupMenus;
end;

procedure TdmEditorManager.BeforeDestruction;
begin
  if PersistSettings then
    FSettings.Save;
  FSettings.OnChanged.Remove(EditorSettingsChanged);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION'property access methods'}
function TdmEditorManager.GetEditorPopupMenu: TPopupMenu;
begin
  Result := ppmEditor;
end;

function TdmEditorManager.GetEncodingPopupMenu: TPopupMenu;
begin
  Result := ppmEncoding;
end;

function TdmEditorManager.GetEvents: IEditorEvents;
begin
  Result := FEvents;
end;

function TdmEditorManager.GetExportPopupMenu: TPopupMenu;
begin
  Result := ppmExport;
end;

function TdmEditorManager.GetFilePopupMenu: TPopupMenu;
begin
  Result := ppmFile;
end;

function TdmEditorManager.GetFoldPopupMenu: TPopupMenu;
begin
  Result := ppmFold;
end;

function TdmEditorManager.GetHighlighterPopupMenu: TPopupMenu;
begin
  Result := ppmHighLighters;
end;

function TdmEditorManager.GetInsertPopupMenu: TPopupMenu;
begin
  Result := ppmInsert;
end;

function TdmEditorManager.GetActionList: TActionList;
begin
  Result := aclActions;
end;

function TdmEditorManager.GetActions: IEditorActions;
begin
  Result := Self as IEditorActions;
end;

function TdmEditorManager.GetClipboardPopupMenu: TPopupMenu;
begin
  Result := ppmClipboard;
end;

function TdmEditorManager.GetCommands: IEditorCommands;
begin
  Result := FCommands as IEditorCommands;
end;

function TdmEditorManager.GetItem(AName: string): TCustomAction;
var
  A  : TCustomAction;
  CA : TContainedAction;
begin
  Result := nil;
  for CA in aclActions do
  begin
    if CA.Name = AName then
    begin
      A := CA as TCustomAction;
      Result := A;
      Break;
    end;
  end;
end;

function TdmEditorManager.GetKeyCommands: TBCEditorKeyCommands;
begin
  Result := ActiveView.Editor.KeyCommands;
end;

function TdmEditorManager.GetLineBreakStylePopupMenu: TPopupMenu;
begin
  Result := ppmLineBreakStyle;
end;

function TdmEditorManager.GetMenus: IEditorMenus;
begin
  Result := Self as IEditorMenus;
end;

function TdmEditorManager.GetPersistSettings: Boolean;
begin
  Result := FPersistSettings;
end;

procedure TdmEditorManager.SetPersistSettings(const AValue: Boolean);
begin
  if AValue <> PersistSettings then
  begin
    FPersistSettings := AValue;
  end;
end;

function TdmEditorManager.GetSearchEngine: IEditorSearchEngine;
begin
  Result := FSearchEngine;
end;

function TdmEditorManager.GetSearchPopupMenu: TPopupMenu;
begin
  Result := ppmSearch;
end;

function TdmEditorManager.GetSelectionDecodePopupMenu: TPopupMenu;
begin
  Result := ppmSelectionDecode;
end;

function TdmEditorManager.GetSelectionEncodePopupMenu: TPopupMenu;
begin
  Result := ppmSelectionEncode;
end;

function TdmEditorManager.GetSelectionModePopupMenu: TPopupMenu;
begin
  Result := ppmSelectionMode;
end;

function TdmEditorManager.GetSelectionPopupMenu: TPopupMenu;
begin
  Result := ppmSelection;
end;

function TdmEditorManager.GetSelectPopupMenu: TPopupMenu;
begin
  Result := ppmSelect;
end;

function TdmEditorManager.GetSettings: IEditorSettings;
begin
  Result := FSettings;
end;

function TdmEditorManager.GetActiveView: IEditorView;
begin
  Result := FActiveView;
end;

procedure TdmEditorManager.SetActiveView(AValue: IEditorView);
begin
  if Assigned(AValue) and (AValue <> FActiveView) then
  begin
    FActiveView := AValue;
    Events.DoActiveViewChange;
    ActiveViewChanged;
  end;
end;

function TdmEditorManager.GetHighlighters: THighlighters;
begin
  Result := Settings.Highlighters;
end;

function TdmEditorManager.GetSettingsPopupMenu: TPopupMenu;
begin
  Result := ppmSettings;
end;

function TdmEditorManager.GetToolViews: IEditorToolViews;
begin
  Result := FToolViews as IEditorToolViews;
end;

function TdmEditorManager.GetView(AIndex: Integer): IEditorView;
begin
  if (AIndex > -1) and (AIndex < FViewList.Count) then
  begin
    Result := FViewList[AIndex] as IEditorView;
  end
  else
    Result := nil;
end;

{ Looks for the view with the given filename. If the view is not found nil is
  returned. }

function TdmEditorManager.GetViewByFileName(AFileName: string): IEditorView;
var
  I: Integer;
  B: Boolean;
begin
  I := 0;
  B := False;
  while (I < FViewList.Count) and not B do
  begin
    B := SameFileName(Views[I].FileName, AFileName);
    if not B then
      Inc(I);
  end;
  if B then
    Result := FViewList[I] as IEditorView
  else
    Result := nil;
end;

{ Looks for the view with the given component name. If the view is not found
  the active view is returned. }

function TdmEditorManager.GetViewByName(AName: string): IEditorView;
var
  I: Integer;
  B: Boolean;
begin
  I := 0;
  B := False;
  while (I < FViewList.Count) and not B do
  begin
    B := Views[I].Name = AName;
    if not B then
      Inc(I);
  end;
  if B then
    Result := FViewList[I] as IEditorView
  else
    Result := ActiveView;
end;

function TdmEditorManager.GetViewCount: Integer;
begin
  if Assigned(FViewList) then
    Result := FViewList.Count
  else
    Result := 0;
end;

function TdmEditorManager.GetViewList: IList<IEditorView>;
begin
  Result := FViewList;
end;

function TdmEditorManager.GetViews: IEditorViews;
begin
  Result := Self as IEditorViews;
end;
{$ENDREGION}

{$REGION'action handlers'}
procedure TdmEditorManager.actSortSelectedLinesExecute(Sender: TObject);
begin
  Commands.SortSelectedLines;
end;

procedure TdmEditorManager.actToggleCommentExecute(Sender: TObject);
begin
  Commands.ToggleLineComment;
end;

procedure TdmEditorManager.actToggleHighlighterExecute(Sender: TObject);
begin
  Commands.ToggleHighlighter;
end;

procedure TdmEditorManager.actToggleMaximizedExecute(Sender: TObject);
var
  A : TAction;
begin
  A := Sender as TAction;
  if A.Checked then
    Settings.FormSettings.WindowState := wsMaximized
  else
    Settings.FormSettings.WindowState := wsNormal;
end;

procedure TdmEditorManager.actUndoExecute(Sender: TObject);
begin
  ActiveView.Undo;
end;

procedure TdmEditorManager.actUpperCaseSelectionExecute(Sender: TObject);
begin
  Commands.UpperCaseSelection;
end;

procedure TdmEditorManager.actOpenExecute(Sender: TObject);
begin
  if Assigned(ActiveView) and Assigned(ActiveView.Editor.Highlighter)
    and Assigned(ActiveView.HighlighterItem) then
    dlgOpen.Filter := ActiveView.HighlighterItem.DefaultFilter;
  if dlgOpen.Execute then
  begin
    OpenFile(dlgOpen.FileName);
  end;
end;

procedure TdmEditorManager.actPascalStringOfSelectionExecute(Sender: TObject);
begin
  Commands.PascalStringFromSelection;
end;

procedure TdmEditorManager.actPasteExecute(Sender: TObject);
var
  B : Boolean;
begin
  if ActiveView.Focused then
  begin
    B := (ActiveView.Text = '') and Settings.AutoGuessHighlighterType;
    ActiveView.Paste;
    if B then
      Commands.GuessHighlighterType;
  end;
end;

procedure TdmEditorManager.actQuoteLinesAndDelimitExecute(Sender: TObject);
begin
  Commands.QuoteLinesInSelection(True);
end;

procedure TdmEditorManager.actQuoteLinesExecute(Sender: TObject);
begin
  Commands.QuoteLinesInSelection;
end;

procedure TdmEditorManager.actSaveExecute(Sender: TObject);
begin
  Commands.Save;
end;

procedure TdmEditorManager.actSaveAsExecute(Sender: TObject);
begin
  SaveFile(ActiveView.FileName, True);
end;

procedure TdmEditorManager.actSearchExecute(Sender: TObject);
begin
  if ActiveView.SelectionAvailable then
    SearchEngine.SearchText := ActiveView.SelectedText;
  ShowToolView('Search', False, True);
end;

procedure TdmEditorManager.actSearchReplaceExecute(Sender: TObject);
begin
  if ActiveView.SelectionAvailable then
    ActiveView.Editor.SearchString := ActiveView.SelectedText;
  ShowToolView('Search', False, True);
end;

procedure TdmEditorManager.actShowCodeShaperExecute(Sender: TObject);
begin
//  ShowToolView('CodeShaper', False, True);
end;

procedure TdmEditorManager.actShowCharacterMapExecute(Sender: TObject);
begin
//  ShowToolView('CharacterMap', False, False);
end;

procedure TdmEditorManager.actAlignSelectionExecute(Sender: TObject);
begin
  ShowToolView('AlignLines', False, True);
end;

procedure TdmEditorManager.actAssociateFilesExecute(Sender: TObject);
//var
//  F : TForm;
begin
//  F := TfrmOptionsAssociate.Create(Self);
//  try
//    F.ShowModal;
//  finally
//    F.Free;
//  end;
end;

procedure TdmEditorManager.actSelectionInfoExecute(Sender: TObject);
begin
  ShowToolView('SelectionInfo', False, False);
end;

procedure TdmEditorManager.actShowPreviewExecute(Sender: TObject);
begin
  ShowToolView('Preview', False, False);
end;

procedure TdmEditorManager.actShowTestExecute(Sender: TObject);
begin
  ShowToolView('Test', False, False);
end;

procedure TdmEditorManager.actShowViewsExecute(Sender: TObject);
begin
  ShowToolView('ViewList', True, True);
end;

procedure TdmEditorManager.actShowActionsExecute(Sender: TObject);
begin
  ShowToolView('ActionListView', True, True);
end;

procedure TdmEditorManager.actShowHexEditorExecute(Sender: TObject);
begin
  ShowToolView('HexEditor', False, True);
end;

procedure TdmEditorManager.actShowHTMLViewerExecute(Sender: TObject);
begin
  ShowToolView('HTMLView', False, False);
end;

procedure TdmEditorManager.actShowIndentGuidesExecute(Sender: TObject);
begin
  Settings.EditorOptions.ShowIndentGuides :=
    not Settings.EditorOptions.ShowIndentGuides;
end;

procedure TdmEditorManager.actShowMinimapExecute(Sender: TObject);
begin
  Settings.EditorOptions.ShowMinimap :=
    not Settings.EditorOptions.ShowMinimap;
end;

procedure TdmEditorManager.actToggleWordWrapExecute(Sender: TObject);
begin
  Settings.EditorOptions.WordWrapEnabled :=
    not Settings.EditorOptions.WordWrapEnabled;
end;

procedure TdmEditorManager.actShowScriptEditorExecute(Sender: TObject);
begin
  ShowToolView('ScriptEditor', False, False);
end;

procedure TdmEditorManager.actShowSearchmapExecute(Sender: TObject);
begin
  Settings.EditorOptions.ShowSearchmap :=
    not Settings.EditorOptions.ShowSearchmap;
end;

procedure TdmEditorManager.actShowStructureViewerExecute(Sender: TObject);
begin
  ShowToolView('Structure', False, False);
end;

procedure TdmEditorManager.actTestFormExecute(Sender: TObject);
begin
  ShowToolView('Test', False, False);
end;

procedure TdmEditorManager.actSelectAllExecute(Sender: TObject);
begin
  ActiveView.SelectAll;
end;

procedure TdmEditorManager.actExportToHTMLExecute(Sender: TObject);
begin
  ExportLines('HTML', False, False);
end;

procedure TdmEditorManager.actExportToWikiExecute(Sender: TObject);
begin
  ExportLines('WIKI', False, False);
end;

procedure TdmEditorManager.actLowerCaseSelectionExecute(Sender: TObject);
begin
  Commands.LowerCaseSelection;
end;

procedure TdmEditorManager.actExportToRTFExecute(Sender: TObject);
begin
  ExportLines(HL_RTF, False, False);
end;

procedure TdmEditorManager.actCopytHTMLToClipboardExecute(Sender: TObject);
begin
  ExportLines('HTML');
end;

procedure TdmEditorManager.actCopyWikiToClipboardExecute(Sender: TObject);
begin
  ExportLines('WIKI');
end;

procedure TdmEditorManager.actCopyRTFToClipboardExecute(Sender: TObject);
begin
  ExportLines(HL_RTF);
end;

procedure TdmEditorManager.actCreateDesktopLinkExecute(Sender: TObject);
begin
  Commands.CreateDesktopLink;
end;

procedure TdmEditorManager.actCopyRTFTextToClipboardExecute(Sender: TObject);
begin
  ExportLines(HL_RTF, True, False);
end;

procedure TdmEditorManager.actCopyWikiTextToClipboardExecute(Sender: TObject);
begin
  ExportLines('WIKI', True, False);
end;

procedure TdmEditorManager.actCopyHTMLTextToClipboardExecute(Sender: TObject);
begin
  ExportLines('HTML', True, False);
end;

procedure TdmEditorManager.actCopyToClipboardExecute(Sender: TObject);
begin
  Commands.CopyToClipboard;
end;

procedure TdmEditorManager.actCutExecute(Sender: TObject);
begin
  if ActiveView.Focused then
    ActiveView.Cut;
end;

procedure TdmEditorManager.actShowCodeFilterExecute(Sender: TObject);
var
  ETV: IEditorToolView;
begin
  ETV := ToolViews.ViewByName['CodeFilter'];
  if Assigned(ETV) then
  begin
    if not ETV.Visible then
    begin
      ETV.Visible := True;
      ETV.UpdateView;
    end;
    ETV.SetFocus;
    FChanged := True;
  end;
end;

procedure TdmEditorManager.actHelpExecute(Sender: TObject);
begin
//  ShortCuts.Show;
end;

procedure TdmEditorManager.actInsertColorValueExecute(Sender: TObject);
begin
  dlgColor.Execute;
  Commands.InsertTextAtCaret(IntToStr(Integer(dlgColor.Color)));
end;

procedure TdmEditorManager.actInspectExecute(Sender: TObject);
begin
//  InspectComponents([
//    HighlighterPopupMenu.Items,
//    HighlighterPopupMenu.Items[0],
//    HighlighterPopupMenu.Items[1],
//    ActiveView.Editor,
//    ActiveView.Editor.Gutter.ChangesPart,
//    ActiveView.Editor.Gutter.LineNumberPart,
//    ActiveView.Editor.Gutter.CodeFoldPart,
//    ActiveView.Editor.Gutter.SeparatorPart
//  ]);
end;

procedure TdmEditorManager.actNewExecute(Sender: TObject);
var
  S : string;
  V : IEditorView;
begin
  if Assigned(ActiveView) then
    S := ActiveView.SelectedText;
  V := NewFile(SNewEditorViewFileName, S);
  if V.CanFocus then
    V.SetFocus;
end;

procedure TdmEditorManager.actReloadExecute(Sender: TObject);
begin
  ActiveView.Load;
end;

procedure TdmEditorManager.actSmartSelectExecute(Sender: TObject);
begin
  Commands.SmartSelect;
end;

procedure TdmEditorManager.actStripFirstCharExecute(Sender: TObject);
begin
  Commands.StripCharsFromSelection(True, False);
end;

procedure TdmEditorManager.actStripMarkupExecute(Sender: TObject);
begin
  Commands.StripMarkupFromSelection;
end;

procedure TdmEditorManager.actStripCommentsExecute(Sender: TObject);
begin
  Commands.StripCommentsFromSelection;
end;

procedure TdmEditorManager.actMergeBlankLinesExecute(Sender: TObject);
begin
  Commands.MergeBlankLinesInSelection;
end;

procedure TdmEditorManager.actStripLastCharExecute(Sender: TObject);
begin
  Commands.StripCharsFromSelection(False, True);
end;

procedure TdmEditorManager.actSyncEditExecute(Sender: TObject);
begin
  Commands.SyncEditSelection;
end;

procedure TdmEditorManager.actToggleFoldLevelExecute(Sender: TObject);
begin
  ActiveView.FoldLevel := (ActiveView.FoldLevel + 1) mod 11;
end;

procedure TdmEditorManager.actFindNextExecute(Sender: TObject);
begin
  Commands.FindNext;
end;

procedure TdmEditorManager.actFindPreviousExecute(Sender: TObject);
begin
  Commands.FindPrevious;
end;

procedure TdmEditorManager.actFoldLevel0Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 0;
end;

procedure TdmEditorManager.actFoldLevel1Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 1;
end;

procedure TdmEditorManager.actFoldLevel2Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 2;
end;

procedure TdmEditorManager.actFoldLevel3Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 3;
end;

procedure TdmEditorManager.actFoldLevel4Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 4;
end;

procedure TdmEditorManager.actFoldLevel5Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 5;
end;

procedure TdmEditorManager.actFoldLevel6Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 6;
end;

procedure TdmEditorManager.actFoldLevel7Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 7;
end;

procedure TdmEditorManager.actFoldLevel8Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 8;
end;

procedure TdmEditorManager.actFoldLevel9Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 9;
end;

procedure TdmEditorManager.actFoldLevel10Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 10;
end;

procedure TdmEditorManager.actFormatExecute(Sender: TObject);
begin
  Commands.FormatCode;
end;

procedure TdmEditorManager.actIncFontSizeExecute(Sender: TObject);
begin
  Commands.AdjustFontSize(1);
end;

procedure TdmEditorManager.actAutoFormatXMLExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := not (Sender as TAction).Checked;
  Settings.AutoFormatXML := (Sender as TAction).Checked;
end;

procedure TdmEditorManager.actMonitorChangesExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := not (Sender as TAction).Checked;
  ActiveView.MonitorChanges := (Sender as TAction).Checked;
end;

procedure TdmEditorManager.actOpenFileAtCursorExecute(Sender: TObject);
begin
  Commands.OpenFileAtCursor;
end;

procedure TdmEditorManager.actQuoteSelectionExecute(Sender: TObject);
begin
  Commands.QuoteSelection;
end;

procedure TdmEditorManager.actRedoExecute(Sender: TObject);
begin
  ActiveView.Redo;
end;

procedure TdmEditorManager.actSettingsExecute(Sender: TObject);
begin
  ShowMessage(SNotImplementedYet);
{  with TEditorSettingsDialog.Create(Self) do
  begin
    ShowModal;
  end;         }
//  ExecuteSettingsDialog(Self);
end;

procedure TdmEditorManager.actHighlighterExecute(Sender: TObject);
var
  A: TAction;
begin
  A := Sender as TAction;
  { TODO -oTS: Don't use Caption of the action to associate with the highlighter }
  Commands.AssignHighlighter(A.Caption);
end;

procedure TdmEditorManager.actDequoteLinesExecute(Sender: TObject);
begin
  Commands.DequoteLinesInSelection;
end;

procedure TdmEditorManager.actAutoGuessHighlighterExecute(Sender: TObject);
begin
  Commands.GuessHighlighterType;
end;

procedure TdmEditorManager.actCompressSpaceExecute(Sender: TObject);
begin
  Commands.CompressSpace;
end;

procedure TdmEditorManager.actCompressWhitespaceExecute(Sender: TObject);
begin
  Commands.CompressWhitespace;
end;

procedure TdmEditorManager.actConvertTabsToSpacesExecute(
  Sender: TObject);
begin
  Commands.ConvertTabsToSpacesInSelection;
end;

procedure TdmEditorManager.actDecodeURLExecute(Sender: TObject);
begin
  Commands.URLFromSelection(True);
end;

procedure TdmEditorManager.actDecodeXMLExecute(Sender: TObject);
begin
  Commands.XMLFromSelection(True);
end;

procedure TdmEditorManager.actEncodeURLExecute(Sender: TObject);
begin
  Commands.URLFromSelection;
end;

procedure TdmEditorManager.actEncodeXMLExecute(Sender: TObject);
begin
  Commands.XMLFromSelection;
end;

procedure TdmEditorManager.actPageSetupExecute(Sender: TObject);
begin
  ShowMessage(SNotImplementedYet);
end;

procedure TdmEditorManager.actPlaybackMacroExecute(Sender: TObject);
begin
  ShowMessage(SNotImplementedYet);
end;

procedure TdmEditorManager.actPrintExecute(Sender: TObject);
begin
  ShowMessage(SNotImplementedYet);
end;

procedure TdmEditorManager.actPrintPreviewExecute(Sender: TObject);
begin
  ShowMessage(SNotImplementedYet);
end;

procedure TdmEditorManager.actRecordMacroExecute(Sender: TObject);
begin
  ShowMessage(SNotImplementedYet);
end;

procedure TdmEditorManager.actSaveAllExecute(Sender: TObject);
begin
  Commands.SaveAll;
end;

{ Only used for tests. }
procedure TdmEditorManager.actShowFilterTestExecute(Sender: TObject);
//var
//  F  : TfrmFilter;
//  OL : TObjectList;
//  A  : TContainedAction;
begin
//  OL := TObjectList.Create(False);
//  try
//    for A in ActionList do
//    begin
//      OL.Add(A);
//      if OL.Count = 5  then
//        Break;
//    end;
//    F := (ToolViews['Filter'].Form as TfrmFilter);
//    F.ColumnDefinitions.AddColumn('Name', 'Name');
//    F.ColumnDefinitions.AddColumn('Caption', 'Caption');
//    //F.ItemTemplate  := TActionCategoryTemplate.Create(F.ColumnDefinitions);
//    F.ItemsSource := OL;
//    ShowToolView('Filter', True, True);
//  finally
//    OL.Free;
//  end;
  // CTRL-ALT-SHIFT-F5
end;

procedure TdmEditorManager.actUnindentExecute(Sender: TObject);
begin
  Commands.Unindent;
end;

procedure TdmEditorManager.actFindAllOccurencesExecute(Sender: TObject);
begin
//  SearchEngine.SearchText := ActiveView.CurrentWord;
//  SearchEngine.Execute;
end;

procedure TdmEditorManager.actIndentExecute(Sender: TObject);
begin
  Commands.Indent;
end;

procedure TdmEditorManager.actInsertGUIDExecute(Sender: TObject);
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Commands.InsertTextAtCaret(GUIDToString(GUID));
end;

procedure TdmEditorManager.actSelectionModeExecute(Sender: TObject);
begin
//  ActiveView.SelectionMode := TSynSelectionMode((Sender as TCustomAction).Tag);
end;

procedure TdmEditorManager.actSingleInstanceExecute(Sender: TObject);
begin
  Settings.SingleInstance := not Settings.SingleInstance;
end;

procedure TdmEditorManager.actStayOnTopExecute(Sender: TObject);
var
  A : TAction;
begin
  A := Sender as TAction;
  if A.Checked then
    Settings.FormSettings.FormStyle := fsStayOnTop
  else
    Settings.FormSettings.FormStyle := fsNormal;
end;

procedure TdmEditorManager.actToggleBlockCommentSelectionExecute(Sender: TObject);
begin
  Commands.ToggleBlockComment;
end;

procedure TdmEditorManager.actClearExecute(Sender: TObject);
begin
  ActiveView.Clear;
end;

procedure TdmEditorManager.actAboutExecute(Sender: TObject);
begin
  ShowMessage(SNotImplementedYet);
end;

procedure TdmEditorManager.aclActionsExecute(AAction: TBasicAction;
  var Handled: Boolean);
begin
  Events.DoActionExecute(AAction, Handled);
end;

procedure TdmEditorManager.actAlignAndSortSelectionExecute(Sender: TObject);
begin
  { TODO -oTS : Implement as a shortcut which takes default settings from the
  dedicated toolview. }
  ShowMessage(SNotImplementedYet);
end;

procedure TdmEditorManager.actCloseExecute(Sender: TObject);
begin
  if ViewCount > 1 then
    DeleteView(ActiveView);
end;

procedure TdmEditorManager.actCloseOthersExecute(Sender: TObject);
begin
  ClearViews(True);
end;

procedure TdmEditorManager.actCopyFileNameExecute(Sender: TObject);
begin
  Clipboard.AsText := ExtractFileName(ActiveView.FileName);
end;

procedure TdmEditorManager.actCopyFilePathExecute(Sender: TObject);
begin
  Clipboard.AsText := ExtractFilePath(ActiveView.FileName);
end;

procedure TdmEditorManager.actCopyFullPathExecute(Sender: TObject);
begin
  Clipboard.AsText := ActiveView.FileName;
end;

procedure TdmEditorManager.actDequoteSelectionExecute(Sender: TObject);
begin
  Commands.DequoteSelection;
end;

procedure TdmEditorManager.actEncodeBase64Execute(Sender: TObject);
begin
  Commands.Base64FromSelection;
end;

procedure TdmEditorManager.actExitExecute(Sender: TObject);
begin
  ClearViews;
  Application.Terminate;
end;

procedure TdmEditorManager.actFindNextWordExecute(Sender: TObject);
begin
  ActiveView.FindNextWordOccurrence(True);
end;

procedure TdmEditorManager.actFindPrevWordExecute(Sender: TObject);
begin
  ActiveView.FindNextWordOccurrence(False);
end;

procedure TdmEditorManager.actCopyExecute(Sender: TObject);
begin
  if ActiveView.Focused then
    ActiveView.Copy;
end;

procedure TdmEditorManager.actDecFontSizeExecute(Sender: TObject);
begin
  Commands.AdjustFontSize(-1);
end;

procedure TdmEditorManager.actDecodeBase64Execute(Sender: TObject);
begin
  Commands.Base64FromSelection(True);
end;

procedure TdmEditorManager.actShowSpecialCharactersExecute(Sender: TObject);
begin
  Settings.EditorOptions.ShowSpecialCharacters := (Sender as TAction).Checked;
end;

procedure TdmEditorManager.actEncodingExecute(Sender: TObject);
begin
  ShowMessage(SNotImplementedYet);
  //ActiveView.Encoding := (Sender as TAction).Caption;
end;

procedure TdmEditorManager.actLineBreakStyleExecute(Sender: TObject);
begin
  ActiveView.LineBreakStyle := (Sender as TAction).Caption;
end;
{$ENDREGION}

{$REGION'event handlers'}
procedure TdmEditorManager.EditorSettingsChanged(ASender: TObject);
begin
  if Assigned(ActiveView) then
    ActiveView.Editor.Refresh;
end;
{$ENDREGION}

{$REGION'private methods'}
{$REGION'Helpers'}
function TdmEditorManager.AddMenuItem(AParent: TMenuItem; AAction: TBasicAction
  ): TMenuItem;
var
  MI: TMenuItem;
begin
  if not Assigned(AAction) then
  begin
    MI := TMenuItem.Create(AParent.Owner);
    MI.Caption := ('-');
    AParent.Add(MI);
    Result := nil;
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
    Result := MI;
  end;
end;

function TdmEditorManager.AddMenuItem(AParent: TMenuItem; AMenu: TMenu
  ): TMenuItem;
var
  MI  : TMenuItem;
  M   : TMenuItem;
  SM  : TMenuItem;
  SMI : TMenuItem;
  I   : Integer;
begin
  MI := TMenuItem.Create(AMenu);
  MI.Action := AMenu.Items.Action;
  AParent.Add(MI);
  for M in AMenu.Items do
  begin
    SMI := AddMenuItem(MI, M.Action);
    // add submenu(s)
    if M.Count > 0 then
    begin
      for I := 0 to M.Count - 1 do
      begin
        SM := M.Items[I];
        AddMenuItem(SMI, SM.Action);
      end;
    end;
  end;
  MI.Enabled := True;
  Result := MI;
end;
{$ENDREGION}

{$REGION'Initialization'}
procedure TdmEditorManager.InitializePopupMenus;
begin
  BuildHighlighterPopupMenu;
  BuildClipboardPopupMenu;
  BuildEncodingPopupMenu;
  BuildFoldPopupMenu;
  BuildExportPopupMenu;
  BuildInsertPopupMenu;
  BuildLineBreakStylePopupMenu;
  BuildSearchPopupMenu;
  BuildSelectionModePopupMenu;
  BuildSelectionEncodePopupMenu;
  BuildSelectionDecodePopupMenu;
  BuildSelectionPopupMenu;
  BuildSelectPopupMenu;
  BuildSettingsPopupMenu;
  BuildFilePopupMenu;
  BuildEditorPopupMenu;
end;

procedure TdmEditorManager.CreateActions;
var
  A  : TAction;
  SL : TStringList;
  S  : string;
  HI : THighlighterItem;
begin
  SL := TStringList.Create;
  try
    //GetSupportedEncodings(SL);
    for S in SL do
    begin
      A := TAction.Create(ActionList);
      A.ActionList := ActionList;
      A.Caption := S;
      //A.Name    := ACTION_PREFIX_ENCODING + DelChars(S, '-');
      A.AutoCheck := True;
      A.GroupIndex := 3;
      A.Category   := actEncodingMenu.Category;
      A.OnExecute  := actEncodingExecute;
    end;
    for S in ALineBreakStyles do
    begin
      A := TAction.Create(aclActions);
      A.ActionList := ActionList;
      A.Caption := S;
      A.Name    := ACTION_PREFIX_LINEBREAKSTYLE + S;
      A.AutoCheck := True;
      A.GroupIndex := 4;
      A.Category := actLineBreakStyleMenu.Category;
      A.OnExecute  := actLineBreakStyleExecute;
    end;
    for HI in Highlighters do
    begin
      A := TAction.Create(ActionList);
      A.ActionList := ActionList;
      A.Caption := HI.Name;
      A.Hint := HI.Description;
      A.Name := ACTION_PREFIX_HIGHLIGHTER + HI.Name;
      A.AutoCheck := True;
      A.Category := actHighlighterMenu.Category;
      A.OnExecute := actHighlighterExecute;
      A.GroupIndex := 5;
    end;
//    for SM := Low(TSynSelectionMode) to High(TSynSelectionMode) do
//    begin
//      A := TAction.Create(ActionList);
//      A.ActionList := ActionList;
//      A.Tag := Ord(SM);
//      S := GetEnumName(TypeInfo(TSynSelectionMode), A.Tag);
//      S := System.Copy(S, 3, Length(S));
//      A.Caption := S;
//      A.Name := ACTION_PREFIX_SELECTIONMODE + S;
//      A.AutoCheck := True;
//      A.GroupIndex := 6;
//      A.Category := actSelectionModeMenu.Category;
//      A.OnExecute := actSelectionModeExecute;
//    end;
  finally
    FreeAndNil(SL);
  end;
end;
{$ENDREGION}

{$REGION'Build popup menus'}
procedure TdmEditorManager.BuildClipboardPopupMenu;
var
  MI: TMenuItem;
begin
  MI := ClipboardPopupMenu.Items;
  MI.Clear;
  MI.Action := actClipboardMenu;
  actClipboardMenu.DisableIfNoHandler := False;
  AddMenuItem(MI, actCopyFileName);
  AddMenuItem(MI, actCopyFilePath);
  AddMenuItem(MI, actCopyFullPath);
  AddMenuItem(MI);
  AddMenuItem(MI, actCopyHTMLTextToClipboard);
  AddMenuItem(MI, actCopyRTFTextToClipboard);
  AddMenuItem(MI, actCopyWikiTextToClipboard);
  AddMenuItem(MI);
  AddMenuItem(MI, actCopytHTMLToClipboard);
  AddMenuItem(MI, actCopyRTFToClipboard);
  AddMenuItem(MI, actCopyWikiToClipboard);
end;


procedure TdmEditorManager.BuildEncodingPopupMenu;
var
  SL : TStringList;
  S  : string;
  A  : TCustomAction;
  MI : TMenuItem;
  i  :  integer;
begin
  SL := TStringList.Create;
  try
    MI := EncodingPopupMenu.Items;
    MI.Clear;
    MI.Action := actEncodingMenu;
    actEncodingMenu.DisableIfNoHandler := False;
    //GetSupportedEncodings(SL);
    for i:=0 to SL.Count-1 do //change for ... in loop by esvignolo
    begin
      S := SL[i];
//      S := ACTION_PREFIX_ENCODING + DelChars(S, '-');
      A := Items[S];
      if Assigned(A) then
      begin
        AddMenuItem(MI, A);
      end;
    end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TdmEditorManager.BuildLineBreakStylePopupMenu;
var
  MI : TMenuItem;
  S  : string;
  A  : TCustomAction;
  T  : TTextLineBreakStyle;
begin
  actLineBreakStyleMenu.DisableIfNoHandler := False;
  MI := LineBreakStylePopupMenu.Items;
  MI.Clear;
  MI.Action := actLineBreakStyleMenu;
  for T := Low(TTextLineBreakStyle) to High(TTextLineBreakStyle) do
  begin
    S:= ALineBreakStyles[t];
    MI := TMenuItem.Create(LineBreakStylePopupMenu);
    S := ACTION_PREFIX_LINEBREAKSTYLE +  S;
    A := Items[S];
    if Assigned(A) then
    begin
      MI.Action     := A;
      MI.Caption    := A.Caption;
      MI.AutoCheck  := A.AutoCheck;
      MI.RadioItem  := True;
      MI.GroupIndex := A.GroupIndex;
    end;
    LineBreakStylePopupMenu.Items.Add(MI);
  end;
end;

procedure TdmEditorManager.BuildFilePopupMenu;
var
  MI: TMenuItem;
begin
  actFileMenu.DisableIfNoHandler := False;
  MI := FilePopupMenu.Items;
  MI.Clear;
  MI.Action := actFileMenu;
  AddMenuItem(MI, actNew);
  AddMenuItem(MI, actOpen);
  AddMenuItem(MI, actSave);
  AddMenuItem(MI, actSaveAs);
  AddMenuItem(MI);
  AddMenuItem(MI, actMonitorChanges);
  AddMenuItem(MI, actCreateDesktopLink);
  AddMenuItem(MI);
  AddMenuItem(MI, actReload);
  AddMenuItem(MI, EncodingPopupMenu);
  AddMenuItem(MI, LineBreakStylePopupMenu);
  AddMenuItem(MI);
  AddMenuItem(MI, actPageSetup);
  AddMenuItem(MI, actPrintPreview);
  AddMenuItem(MI, actPrint);
end;

procedure TdmEditorManager.BuildHighlighterPopupMenu;
var
  MI : TMenuItem;
  S  : string;
  HI : THighlighterItem;
  A  : TCustomAction;
begin
  actHighlighterMenu.DisableIfNoHandler := False;
  HighlighterPopupMenu.Items.Action := actHighlighterMenu;
  HighlighterPopupMenu.Items.Clear;
  for HI in Highlighters do
  begin
    MI := TMenuItem.Create(HighlighterPopupMenu);
    S  := ACTION_PREFIX_HIGHLIGHTER + HI.Name;
    A  := Items[S];
    if Assigned(A) then
    begin
      MI.Action     := A;
      MI.Hint       := HI.Description;
      MI.Caption    := A.Caption;
      MI.AutoCheck  := A.AutoCheck;
      MI.RadioItem  := False;
    end;
    HighlighterPopupMenu.Items.Add(MI);
  end;
end;

procedure TdmEditorManager.BuildSearchPopupMenu;
var
  MI : TMenuItem;
begin
  MI := SearchPopupMenu.Items;
  MI.Clear;
  MI.Action := actSearchMenu;
  actSearchMenu.DisableIfNoHandler := False;
  AddMenuItem(MI, actSearch);
  AddMenuItem(MI, actSearchReplace);
  AddMenuItem(MI, actFindAllOccurences);
  AddMenuItem(MI);
  AddMenuItem(MI, actFindNext);
  AddMenuItem(MI, actFindPrevious);
  AddMenuItem(MI);
  AddMenuItem(MI, actFindNextWord);
  AddMenuItem(MI, actFindPrevWord);
end;

procedure TdmEditorManager.BuildSelectPopupMenu;
var
  MI : TMenuItem;
begin
  MI := SelectPopupMenu.Items;
  MI.Clear;
  MI.Action := actSelectMenu;
  actSelectMenu.DisableIfNoHandler := False;
  AddMenuItem(MI, actSelectAll);
  AddMenuItem(MI);
  AddMenuItem(MI, actClear);
  AddMenuItem(MI);
  AddMenuItem(MI, actSmartSelect);
end;

procedure TdmEditorManager.BuildSelectionPopupMenu;
var
  MI : TMenuItem;
begin
  MI := SelectionPopupMenu.Items;
  MI.Clear;
  MI.Action := actSelectionMenu;
  actSelectionMenu.DisableIfNoHandler := False;
  AddMenuItem(MI, actSyncEdit);
  AddMenuItem(MI, actAlignSelection);
  AddMenuItem(MI, actSortSelectedLines);
  AddMenuItem(MI);
  AddMenuItem(MI, actIndent);
  AddMenuItem(MI, actUnindent);
  AddMenuItem(MI);
  AddMenuItem(MI, actUpperCaseSelection);
  AddMenuItem(MI, actLowerCaseSelection);
  AddMenuItem(MI);
  AddMenuItem(MI, actStripFirstChar);
  AddMenuItem(MI, actStripLastChar);
  AddMenuItem(MI, actStripComments);
  AddMenuItem(MI, actStripMarkup);
  AddMenuItem(MI, actCompressSpace);
  AddMenuItem(MI, actCompressWhitespace);
  AddMenuItem(MI, actMergeBlankLines);
  AddMenuItem(MI);
  AddMenuItem(MI, actToggleComment);
  AddMenuItem(MI, actToggleBlockCommentSelection);
  AddMenuItem(MI);
  AddMenuItem(MI, actQuoteSelection);
  AddMenuItem(MI, actDequoteSelection);
  AddMenuItem(MI, actQuoteLinesAndDelimit);
  AddMenuItem(MI, actQuoteLines);
  AddMenuItem(MI, actDequoteLines);
  AddMenuItem(MI);
  AddMenuItem(MI, SelectionEncodePopupMenu);
  AddMenuItem(MI, SelectionDecodePopupMenu);
  AddMenuItem(MI);
  AddMenuItem(MI, actConvertTabsToSpaces);
  AddMenuItem(MI);
  AddMenuItem(MI, actPascalStringOfSelection);
end;

procedure TdmEditorManager.BuildSelectionEncodePopupMenu;
var
  MI : TMenuItem;
begin
  MI := SelectionEncodePopupMenu.Items;
  MI.Clear;
  MI.Action := actSelectionEncodeMenu;
  actSelectionEncodeMenu.DisableIfNoHandler := False;
  AddMenuItem(MI, actEncodeBase64);
  AddMenuItem(MI, actEncodeURL);
  AddMenuItem(MI, actEncodeXML);
end;

procedure TdmEditorManager.BuildSelectionDecodePopupMenu;
var
  MI : TMenuItem;
begin
  MI := SelectionDecodePopupMenu.Items;
  MI.Clear;
  MI.Action := actSelectionDecodeMenu;
  AddMenuItem(MI, actDecodeBase64);
  AddMenuItem(MI, actDecodeURL);
  AddMenuItem(MI, actDecodeXML);
end;

procedure TdmEditorManager.BuildInsertPopupMenu;
var
  MI : TMenuItem;
begin
  actInsertMenu.DisableIfNoHandler := False;
  MI := InsertPopupMenu.Items;
  MI.Clear;
  MI.Action := actInsertMenu;
  AddMenuItem(MI, actShowCharacterMap);
  AddMenuItem(MI, actInsertColorValue);
  AddMenuItem(MI, actInsertGUID);
end;

procedure TdmEditorManager.BuildSelectionModePopupMenu;
//var
//  SM : TSynSelectionMode;
//  MI : TMenuItem;
//  S  : string;
//  A  : TCustomAction;
begin
  actSelectionModeMenu.DisableIfNoHandler := False;
  SelectionModePopupMenu.Items.Clear;
  SelectionModePopupMenu.Items.Action := actSelectionModeMenu;
//  for SM := Low(TSynSelectionMode) to High(TSynSelectionMode) do
//  begin
//    MI := TMenuItem.Create(SelectionModePopupMenu);
//    S := GetEnumName(TypeInfo(TSynSelectionMode), Ord(SM));
//    S := System.Copy(S, 3, Length(S));
//    S := ACTION_PREFIX_SELECTIONMODE + S;
//    A := Items[S];
//    if Assigned(A) then
//    begin
//      MI.Action     := A;
//      MI.Hint       := A.Hint;
//      MI.Caption    := A.Caption;
//      MI.AutoCheck  := A.AutoCheck;
//      MI.RadioItem  := True;
//      {$IFDEF LCLGTK2}
//      MI.RadioItem  := False;
//      {$ENDIF}
//      MI.GroupIndex := A.GroupIndex;
//    end;
//    SelectionModePopupMenu.Items.Add(MI);
//  end;
end;

procedure TdmEditorManager.BuildSettingsPopupMenu;
var
  MI : TMenuItem;
begin
  actSettingsMenu.DisableIfNoHandler := False;
  MI := SettingsPopupMenu.Items;
  MI.Clear;
  MI.Action := actSettingsMenu;
  AddMenuItem(MI, actSettings);
  AddMenuItem(MI);
  AddMenuItem(MI, actShowSpecialCharacters);
  AddMenuItem(MI, actShowMinimap);
  AddMenuItem(MI, actShowSearchmap);
  AddMenuItem(MI, actShowIndentGuides);
  AddMenuItem(MI, actToggleWordWrap);
  AddMenuItem(MI);
  AddMenuItem(MI, actIncFontSize);
  AddMenuItem(MI, actDecFontSize);
  AddMenuItem(MI);
  AddMenuItem(MI, actStayOnTop);
  AddMenuItem(MI, actSingleInstance);
end;

procedure TdmEditorManager.BuildFoldPopupMenu;
begin
  actFoldMenu.DisableIfNoHandler := False;
  FoldPopupMenu.Items.Action := actFoldMenu;
end;

procedure TdmEditorManager.BuildExportPopupMenu;
var
  MI : TMenuItem;
begin
  actExportMenu.DisableIfNoHandler := False;
  MI := ExportPopupMenu.Items;
  MI.Clear;
  MI.Action := actExportMenu;
  AddMenuItem(MI, actExportToHTML);
  AddMenuItem(MI, actExportToRTF);
  AddMenuItem(MI, actExportToWiki);
end;

procedure TdmEditorManager.BuildEditorPopupMenu;
var
  MI : TMenuItem;
begin
  MI := EditorPopupMenu.Items;
  MI.Clear;
  AddMenuItem(MI, actCut);
  AddMenuItem(MI, actCopy);
  AddMenuItem(MI, actPaste);
  AddMenuItem(MI);
  AddMenuItem(MI, actUndo);
  AddMenuItem(MI, actRedo);
  AddMenuItem(MI);
  AddMenuItem(MI, FilePopupMenu);
  AddMenuItem(MI, SettingsPopupMenu);
  AddMenuItem(MI, SearchPopupMenu);
  AddMenuItem(MI, SelectPopupMenu);
  AddMenuItem(MI, SelectionPopupMenu);
  AddMenuItem(MI, InsertPopupMenu);
  AddMenuItem(MI, ClipboardPopupMenu);
  AddMenuItem(MI, HighlighterPopupMenu);
  AddMenuItem(MI, ExportPopupMenu);
  AddMenuItem(MI, FoldPopupMenu);
  AddMenuItem(MI);
  AddMenuItem(MI, actClose);
  AddMenuItem(MI, actCloseOthers);
end;
{$ENDREGION}

{$REGION'Registration'}
procedure TdmEditorManager.RegisterToolViews;
begin
  //ToolViews.Register(TfrmAlignLines, TAlignLinesSettings, 'AlignLines');
  ToolViews.Register(TfrmFilter, TFilterSettings, 'CodeFilter');
//ToolViews.Register(TfrmHTMLView, THTMLViewSettings, 'HTMLView');
  //ToolViews.Register(TfrmCodeShaper, TCodeShaperSettings, 'CodeShaper');
//  ToolViews.Register(TfrmStructure, nil, 'Structure');
//  ToolViews.Register(TfrmScriptEditor, nil, 'ScriptEditor');
//  ToolViews.Register(TfrmFilter, nil, 'Filter');
  ToolViews.Register(TfrmSortStrings, TSortStringsSettings, 'SortStrings');
  ToolViews.Register(TfrmSearchForm, TSearchEngineSettings, 'Search');
  ToolViews.Register(TfrmViewList, nil, 'ViewList');
  ToolViews.Register(TfrmActionListView, nil, 'ActionListView');
  ToolViews.Register(TfrmTest, nil,  'Test');
  ToolViews.Register(TfrmSelectionInfo, nil, 'SelectionInfo');
  ToolViews.Register(TfrmCharacterMap, nil, 'CharacterMap');
end;
{$ENDREGION}
{$ENDREGION}

{$REGION'protected methods'}
{ Called when the active view is set to another view in the list. }
procedure TdmEditorManager.ActiveViewChanged;
begin
  UpdateHighLighterActions;
  UpdateEncodingActions;
  UpdateLineBreakStyleActions;
  UpdateFileActions;
  UpdateFoldingActions;
end;

function TdmEditorManager.ViewsModified: Boolean;
var
  V : IEditorView;
begin
  Result := False;
  for V in (Self as IEditorViews) do
  begin
    if V.Modified then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TdmEditorManager.GetViewsEnumerator: TEditorViewListEnumerator;
begin
  Result := TEditorViewListEnumerator.Create(FViewList);
end;

{ TODO -oTS : Check out if this is appropriate. }
procedure TdmEditorManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Supports(AComponent, IEditorView) and (Operation = opRemove) then
  begin
    DeleteView(AComponent as IEditorView);
    Logger.Watch('ViewCount', ViewCount);
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TdmEditorManager.ShowToolView(const AName: string;
  AShowModal: Boolean; ASetFocus: Boolean);
var
  ETV : IEditorToolView;
  TV  : IEditorToolView;
begin
  ETV := ToolViews[AName];
  Guard.CheckNotNull(ETV, AName);
  for TV in ToolViews do
  begin
    if TV <> ETV then
      TV.Visible := False;
  end;
  if not ETV.Visible then
  begin
    if not AShowModal then
    begin
      { This for example can allow the owner to dock the toolview in the main
        application workspace. }
      Events.DoShowToolView(ETV);
      ETV.Visible := True;
    end
    else
    begin
      ETV.Form.ShowModal;
    end;
  end;
  ETV.UpdateView;
  if ASetFocus then
    ETV.SetFocus;
end;

{$REGION'IEditorActions'}
function TdmEditorManager.AddView(const AName: string; const AFileName: string;
  const AHighlighter: string): IEditorView;
var
  V : IEditorView;
begin
  Logger.Enter(Self, 'AddView');
  V := TEditorView.Create(Self);
  // if no name is provided, the view will get an automatically generated one.
  { TODO -oTS : Needs to be refactored. }
  if AName <> '' then
    V.Name := AName;
  V.FileName := AFileName;
  if FileExists(V.FileName) then
    V.Load;
  if AHighlighter <> '' then
    V.HighlighterName := AHighlighter
  else
    V.HighlighterName := 'TXT';
  V.Form.Caption := '';
  ViewList.Add(V);
  Events.DoAddEditorView(V);
  V.Activate;
  Result := V;
  Logger.Watch('ViewCount', ViewCount);
  Logger.Leave(Self, 'AddView');
end;

function TdmEditorManager.DeleteView(AIndex: Integer): Boolean;
var
  I : Integer;
  V : IEditorView;
begin
  Logger.Enter(Self, 'DeleteView(AIndex)');
  if (AIndex > -1) and (AIndex < ViewCount) {and (ViewCount > 1)} then
  begin
    I := ViewList.IndexOf(ActiveView);
    if I = AIndex then // select a new active view
    begin
      V := Views[I];
      V.Activate
    end;
    Views[AIndex].Close;
    ViewList.Delete(AIndex);
    Result := True;
  end
  else
    Result := False;
  Logger.Watch('ViewCount', ViewCount);
  Logger.Leave(Self, 'DeleteView(AIndex)');
end;

{ 1. Removes the given instance from the list
  2. Closes the instance (which will free it)
  3. Sets the active view to another view if we were closing the active view
}

function TdmEditorManager.DeleteView(AView: IEditorView): Boolean;
var
  I : Integer;
begin
  Result := False;
  Logger.Enter(Self, 'DeleteView(AView)');
  if Assigned(AView) and Assigned(ViewList) then
  begin
    I := ViewList.IndexOf(AView);
    if (ViewCount > 1) and (I > -1) then
    begin
      if AView = ActiveView then
      begin
        AView.Close;
        ViewList.Delete(I);
        Views[0].Activate;
        Result := True;
      end
      else
      begin
        AView.Close;
        ViewList.Delete(I);
      end;
    end
    else
    begin
      Result := False;
    end;
  end
  else
    Result := False;
  Logger.Watch('ViewCount', ViewCount);
  Logger.Leave(Self, 'DeleteView(AView)');
end;

function TdmEditorManager.DeleteView(const AName: string): Boolean;
begin
  Result := DeleteView(ViewByName[AName]);
end;

{ Closes and clears all views in the list (except for the active view when
  AExceptActive is True).

  TODO: Does not work when AExeptActive is False}

procedure TdmEditorManager.ClearViews(AExceptActive: Boolean);
var
  I : Integer;
  V : IEditorView;
begin
  if AExceptActive then
  begin
    V := ActiveView;
    I := ViewList.IndexOf(V);
    ViewList.Delete(I);
  end;
  while ViewCount > 0 do
    DeleteView(0);
  ViewList.Clear;
  if AExceptActive then
    ViewList.Add(V);
end;
{$ENDREGION}

{$REGION'IEditorCommands'}
procedure TdmEditorManager.ExportLines(AFormat: string; AToClipBoard: Boolean;
  ANativeFormat: Boolean);
//var
//  S  : string;
//  SL : TStringList;
begin
//  SL := TStringList.Create;
//  try
//    if AFormat = 'HTML' then
//    begin
//      SynExporterHTML.Highlighter := ActiveView.Editor.Highlighter;
//      SynExporterHTML.ExportAsText := not ANativeFormat;
//      SynExporterHTML.Font.Assign(ActiveView.Editor.Font);
//      if ActiveView.SelAvail then
//        SL.Text := ActiveView.SelText
//      else
//        SL.Text := ActiveView.Text;
//      SynExporterHTML.ExportAll(SL);
//      if AToClipboard then
//        SynExporterHTML.CopyToClipboard
//      else
//      begin
//        S := dlgSave.Filter;
//        dlgSave.Filter := SynExporterHTML.DefaultFilter;
//        dlgSave.FileName := ExtractFileNameWithoutExt(Settings.FileName) + '.html';
//        if dlgSave.Execute then
//          SynExporterHTML.SaveToFile(dlgSave.FileName);
//        dlgSave.Filter := S;
//      end;
//    end
//    else if AFormat = HL_RTF then
//    begin
//      FSynExporterRTF.Highlighter := ActiveView.Editor.Highlighter;
//      FSynExporterRTF.ExportAsText := not ANativeFormat;
//      FSynExporterRTF.Font.Assign(ActiveView.Editor.Font);
//      if ActiveView.SelAvail then
//        SL.Text := ActiveView.SelText
//      else
//        SL.Text := ActiveView.Text;
//      FSynExporterRTF.ExportAll(SL);
//      if AToClipboard then
//        FSynExporterRTF.CopyToClipboard
//      else
//      begin
//        S := dlgSave.Filter;
//        dlgSave.Filter := FSynExporterRTF.DefaultFilter;
//        dlgSave.FileName := ExtractFileNameWithoutExt(Settings.FileName) + '.rtf';
//        if dlgSave.Execute then
//          FSynExporterRTF.SaveToFile(dlgSave.FileName);
//        dlgSave.Filter := S;
//      end;
//    end
    //else if AFormat = 'WIKI' then
    //begin
    //  SynExporterWiki.Highlighter := ActiveView.Editor.Highlighter;
    //  SynExporterWiki.ExportAsText := not ANativeFormat;
    //  if ActiveView.SelAvail then
    //    SL.Text := ActiveView.SelText
    //  else
    //    SL.Text := ActiveView.Text;
    //  SynExporterWiki.ExportAll(SL);
    //  if AToClipboard then
    //    SynExporterWiki.CopyToClipboard
    //  else
    //  begin
    //    S := dlgSave.Filter;
    //    dlgSave.Filter := SynExporterWiki.DefaultFilter;
    //    dlgSave.FileName := ExtractFileNameWithoutExt(Settings.FileName) + '.txt';
    //    if dlgSave.Execute then
    //      SynExporterWiki.SaveToFile(dlgSave.FileName);
    //    dlgSave.Filter := S;
    //  end;
    //end;
//  finally
//    SL.Free;
//  end;
end;

{ Saves the content of the active editorview to the given filename. If the
  given filename does not exist or is empty, the user is prompted to enter a
  name with the save file dialog. }

function TdmEditorManager.SaveFile(const AFileName: string;
  AShowDialog: Boolean): Boolean;
var
  B : Boolean;
  S : string;
begin
  S := AFileName;
  B := AShowDialog or not FileExists(AFileName);
  if B then // show dialog
  begin
    if Assigned(ActiveView.Editor.Highlighter) then
    begin
      dlgSave.Filter := ActiveView.HighlighterItem.DefaultFilter;
      dlgSave.FilterIndex := ActiveView.HighlighterItem.Index + 1;
    end;
    dlgSave.FileName := S;
    if dlgSave.Execute then
    begin
      ActiveView.FileName := dlgSave.FileName;
      ActiveView.Save(dlgSave.FileName);
      Result := True;
    end
    else
      Result := False;
  end
  else
  begin
    ActiveView.Save(S);
    Result := True;
  end;
end;

function TdmEditorManager.ActivateView(const AName: string): Boolean;
var
  V: IEditorView;
begin
  V := ViewByName[AName];
  if  Assigned(V) then
  begin
    ViewByName[AName].Activate;
    Result := True;
  end
  else
    Result := False;
end;
{$ENDREGION}

{$REGION'UpdateActions'}
{ Gets called from the active editor view. }

procedure TdmEditorManager.UpdateActions;
var
  B  : Boolean;
  V  : IEditorView;
begin
  V := ActiveView;
  if Assigned(V) then
  begin
    B := ActiveView.Focused;
    actCut.Enabled   := actCut.Visible and B;
    actCopy.Enabled  := actCopy.Visible and B;
    actPaste.Enabled := actPaste.Visible and ActiveView.CanPaste and B;

    if Assigned(Settings) and V.Focused {and FChanged} then
    begin
      B := V.SelectionAvailable {and not Settings.ReadOnly};
      actDequoteSelection.Enabled            := B;
      actLowerCaseSelection.Enabled          := B;
      actToggleBlockCommentSelection.Enabled := B;
      actPascalStringOfSelection.Enabled     := B;
      actStripMarkup.Enabled                 := B;
      actQuoteSelection.Enabled              := B;
      actQuoteLinesAndDelimit.Enabled        := B;
      actSortSelectedLines.Enabled           := B;
      actUpperCaseSelection.Enabled          := B;
      actStripComments.Enabled               := B;
      actStripFirstChar.Enabled              := B;
      actStripLastChar.Enabled               := B;
      actMergeBlankLines.Enabled             := B;
      actQuoteLines.Enabled                  := B;
      actDequoteLines.Enabled                := B;
      actSelectionEncodeMenu.Enabled         := B;
      actSelectionDecodeMenu.Enabled         := B;
      actEncodeBase64.Enabled                := B;
      actDecodeBase64.Enabled                := B;
      actEncodeURL.Enabled                   := B;
      actDecodeURL.Enabled                   := B;
      actEncodeXML.Enabled                   := B;
      actDecodeXML.Enabled                   := B;
      actConvertTabsToSpaces.Enabled         := B;
      actExecuteScriptOnSelection.Enabled    := B;
      actSyncEdit.Enabled                    := B;
      actCompressSpace.Enabled               := B;
      actCompressWhitespace.Enabled          := B;
      actIndent.Enabled                      := B;
      actUnindent.Enabled                    := B;

      B := not Settings.ReadOnly;
      actAlignSelection.Visible          := B;
      actCut.Visible                     := B;
      actDelete.Visible                  := B;
      actDequoteLines.Visible            := B;
      actDequoteSelection.Visible        := B;
      actFormat.Visible                  := B;
      actShowCharacterMap.Visible        := actShowCharacterMap.Visible and B;
      actPascalStringOfSelection.Visible := B;
      actPaste.Visible                   := B;
      actQuoteSelection.Visible          := B;
      actQuoteLines.Visible              := B;
      actQuoteLinesAndDelimit.Visible    := B;
      actShowCodeShaper.Visible          := actShowCodeShaper.Visible and B;
      actToggleComment.Visible           := B;
      actStripLastChar.Visible           := B;
      actStripFirstChar.Visible          := B;
      actUpperCaseSelection.Visible      := B;
      actLowerCaseSelection.Visible      := B;
      actSyncEdit.Visible                := B;

      actRedo.Enabled := B and V.CanRedo;
      actUndo.Enabled := B and V.CanUndo;

      B := True;
      actToggleFoldLevel.Enabled := B;
      actFoldLevel0.Enabled      := B;
      actFoldLevel1.Enabled      := B;
      actFoldLevel2.Enabled      := B;
      actFoldLevel3.Enabled      := B;
      actFoldLevel4.Enabled      := B;
      actFoldLevel5.Enabled      := B;
      actFoldLevel6.Enabled      := B;
      actFoldLevel7.Enabled      := B;
      actFoldLevel8.Enabled      := B;
      actFoldLevel9.Enabled      := B;
      actFoldLevel10.Enabled     := B;

      actToggleFoldLevel.ImageIndex    := 59 + V.FoldLevel;
      actShowSpecialCharacters.Enabled := True;
      actShowSpecialCharacters.Checked :=
        Settings.EditorOptions.ShowSpecialCharacters;
      actShowIndentGuides.Checked :=
        Settings.EditorOptions.ShowIndentGuides;
      actToggleWordWrap.Checked :=
        Settings.EditorOptions.WordWrapEnabled;
      actShowMinimap.Checked :=
        Settings.EditorOptions.ShowMinimap;
      actShowSearchmap.Checked :=
        Settings.EditorOptions.ShowSearchmap;

      actSave.Enabled := ActiveView.Modified;
      actCloseOthers.Visible := ViewCount > 1;

      actToggleMaximized.Checked :=
        Settings.FormSettings.WindowState = wsMaximized;
      actStayOnTop.Checked := Settings.FormSettings.FormStyle = fsStayOnTop;
      actSingleInstance.Checked := Settings.SingleInstance;

      actSaveAll.Enabled := ViewsModified;

      FChanged := False;
    end;
    // Hide close view actions when there is only one editor view
    B := ViewCount = 1;
    actClose.Enabled       := not B;
    actCloseOthers.Visible := not B;
    { TODO -oTS : Cleanup popup menu (hide orphaned seperators) }

//    if EditorPopupMenu.Items.Count > 0 then
//    begin
//      MI := EditorPopupMenu.Items[EditorPopupMenu.Items.Count - 1];
//      if MI.Caption = '-' then
//        MI.Visible := False;
//    end;
  end;
  actHighlighterMenu.Enabled := True;
end;

procedure TdmEditorManager.UpdateEncodingActions;
var
  S : string;
  A : TCustomAction;
begin
  S := '';
  if Assigned(ActiveView) then
  begin
//    S := ACTION_PREFIX_ENCODING + DelChars(ActiveView.Encoding, '-');
    A := Items[S];
    if Assigned(A) then
      A.Checked := True;
  end;
end;

procedure TdmEditorManager.UpdateLineBreakStyleActions;
//var
//  S : string;
//  A : TCustomAction;
begin
//  S := '';
//  if Assigned(ActiveView) then
//  begin
//    S := ACTION_PREFIX_LINEBREAKSTYLE + ActiveView.LineBreakStyle;
//    A := Items[S];
//    if Assigned(A) then
//      A.Checked := True;
//  end;
end;

procedure TdmEditorManager.UpdateSelectionModeActions;
var
  S : string;
  A : TCustomAction;
begin
  S := '';
  if Assigned(ActiveView) then
  begin
//    S := GetEnumName(TypeInfo(TSynSelectionMode), Ord(ActiveView.SelectionMode));
    S := System.Copy(S, 3, Length(S));
    S := ACTION_PREFIX_SELECTIONMODE + S;
    A := Items[S];
    if Assigned(A) then
      A.Checked := True;
  end;
end;

procedure TdmEditorManager.UpdateFileActions;
var
  B : Boolean;
begin
  B := FileExists(ActiveView.FileName);
  Logger.Send('Filexists', ActiveView.FileName);
  Logger.Send('Filexists', B);
  actCreateDesktopLink.Enabled := B;
  actCopyFileName.Enabled      := B;
  actCopyFilePath.Enabled      := B;
  actCopyFullPath.Enabled      := B;
  actReload.Enabled            := B;
  actMonitorChanges.Enabled    := B;
  actMonitorChanges.Checked    := B and ActiveView.MonitorChanges;
end;

{ TODO: causes problems for an unknown reason when no highlighters are
  registered. }

procedure TdmEditorManager.UpdateHighLighterActions;
var
  S : string;
  A : TCustomAction;
begin
  S := '';
  if Assigned(ActiveView) and Assigned(ActiveView.HighlighterItem) then
  begin
    S := ACTION_PREFIX_HIGHLIGHTER + ActiveView.HighlighterItem.Name;
    A := Items[S];
    // TS TODO!!!
    if Assigned(A) then
      A.Checked := True;
  end;
end;

procedure TdmEditorManager.UpdateFoldingActions;
//var
//  I : Integer;
begin
  // Assign fold shortcuts
//  I := View.Editor.KeyCommands.FindCommand(ecFoldLevel0);
//  actFoldLevel0.ShortCut := View.Editor.KeyCommands[I].ShortCut;
//  I := View.Editor.KeyCommands.FindCommand(ecFoldLevel1);
//  actFoldLevel1.ShortCut := View.Editor.KeyCommands[I].ShortCut;
//  I := View.Editor.KeyCommands.FindCommand(ecFoldLevel2);
//  actFoldLevel2.ShortCut := View.Editor.KeyCommands[I].ShortCut;
//  I := View.Editor.KeyCommands.FindCommand(ecFoldLevel3);
//  actFoldLevel3.ShortCut := View.Editor.KeyCommands[I].ShortCut;
//  I := View.Editor.KeyCommands.FindCommand(ecFoldLevel4);
//  actFoldLevel4.ShortCut := View.Editor.KeyCommands[I].ShortCut;
//  I := View.Editor.KeyCommands.FindCommand(ecFoldLevel5);
//  actFoldLevel5.ShortCut := View.Editor.KeyCommands[I].ShortCut;
//  I := View.Editor.KeyCommands.FindCommand(ecFoldLevel6);
//  actFoldLevel6.ShortCut := View.Editor.KeyCommands[I].ShortCut;
//  I := View.Editor.KeyCommands.FindCommand(ecFoldLevel7);
//  actFoldLevel7.ShortCut := View.Editor.KeyCommands[I].ShortCut;
//  I := View.Editor.KeyCommands.FindCommand(ecFoldLevel8);
//  actFoldLevel8.ShortCut := View.Editor.KeyCommands[I].ShortCut;
//  I := View.Editor.KeyCommands.FindCommand(ecFoldLevel9);
//  actFoldLevel9.ShortCut := View.Editor.KeyCommands[I].ShortCut;
end;

{ Called by actOpen. }

function TdmEditorManager.OpenFile(const AFileName: string): IEditorView;
var
  V : IEditorView;
begin
  Events.DoOpen(AFileName);
  { Check if the file is already opened in a view. }
  V := ViewByFileName[AFileName];
  if Assigned(V) then
    Result := V
  else
  begin
    if FileExists(AFileName) then
    begin
      V := AddView;
      V.Load(AFileName);
    end;
    Result := V;
  end;
end;

{ Called by actNew. }

function TdmEditorManager.NewFile(const AFileName: string;
  const AText: string): IEditorView;
var
  V : IEditorView;
  S : string;
begin
  S := '';
  Events.DoNew(AFileName, AText);
  if Assigned(ActiveView) then
    S := ActiveView.HighlighterName;
  V := AddView('', AFileName, S);
  V.Text := AText;
  Result := V;
end;
{$ENDREGION}
{$ENDREGION}

end.
