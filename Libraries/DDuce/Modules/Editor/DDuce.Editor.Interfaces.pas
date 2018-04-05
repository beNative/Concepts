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

unit DDuce.Editor.Interfaces;

{ This unit hosts all interfaces of the editor module.
  To make an editor instance you need to use this unit in the interface section
  and DDuce.Editor.Factories in the implementation part to let you create
  instances supporting one or more of these interfaces. }

interface

uses
  System.Classes, System.Contnrs, System.Types, System.SysUtils,
  Vcl.ActnList, Vcl.Controls, Vcl.Forms, Vcl.Menus, Vcl.Graphics, Vcl.ComCtrls,
  Vcl.ActnPopup,

  Spring, Spring.Collections,

  BCEditor.Editor, BCEditor.Types, BCEditor.Editor.KeyCommands,

  DDuce.FormSettings,

  DDuce.Editor.Search.Data,

  DDuce.Editor.Tools.Settings, DDuce.Editor.Options.Settings,
  DDuce.Editor.Colors.Settings, DDuce.Editor.Highlighters,

  DDuce.Editor.Types,

  DDuce.Editor.AlignLines.Settings, DDuce.Editor.SortStrings.Settings;
  //DDuce.Editor.CodeShaper.Settings,
  //DDuce.Editor.CodeFilter.Settings,
  //DDuce.Editor.HTMLView.Settings,
  //DDuce.Editor.HexEditor.Settings,

// TPopupMenu does not work with Vcl styles! This interposer class is a work-
// around for this issue.
// See: https://theroadtodelphi.wordpress.com/2012/03/06/adding-vcl-styles-support-to-a-tpopupmenu-in-2-lines-of-code/

type
  TPopupMenu = class(Vcl.ActnPopup.TPopupActionBar);

type
  // forward declarations
  IEditorView                   = interface;
  IEditorToolView               = interface;
  IEditorActions                = interface;
  IEditorSettings               = interface;
  TEditorViewListEnumerator     = class;
  TEditorToolViewListEnumerator = class;

  TEditorViewEvent = procedure(
    Sender      : TObject;
    AEditorView : IEditorView
  ) of object;

  TEditorToolViewEvent = procedure(
    Sender          : TObject;
    AEditorToolView : IEditorToolView
  ) of object;

  IControl = interface
  ['{303F3DE1-81F5-473B-812B-7DD4C306725B}']
    function GetName: string;
    function GetParent: TWinControl;
    function GetPopupMenu: TPopupMenu;
    function GetVisible: Boolean;
    procedure SetName(AValue: string);
    procedure SetParent(AValue: TWinControl);
    procedure SetPopupMenu(AValue: TPopupMenu);
    procedure SetVisible(const AValue: Boolean);

    function Focused: Boolean;
    procedure SetFocus;
    function Canfocus: Boolean;

    property Parent: TWinControl
      read GetParent write SetParent;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property Name: string
      read GetName write SetName;

    property Visible: Boolean
      read GetVisible write SetVisible;
  end;

  { Handles display view of the editor. }

  { IEditorView }

  IEditorView = interface(IControl)
  ['{94689213-B046-45F6-922B-FAE91C02A3FF}']
    {$REGION 'property access methods'}
    function GetActions: IEditorActions;
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretX: Integer;
    function GetCaretXY: TPoint;
    function GetCaretY: Integer;
    function GetCurrentChar: WideChar;
    function GetCurrentWord: string;
    function GetEditor: TBCEditor;
    function GetEditorFont: TFont;
    function GetEncoding: TEncoding;
    function GetFileName: string;
    function GetFindHistory: TStrings;
    function GetFoldLevel: Integer;
    function GetForm: TCustomForm;
    function GetHighlighterItem: THighlighterItem;
    function GetHighlighterName: string;
    function GetInsertMode: Boolean;
    function GetIsFile: Boolean;
    function GetLineBreakStyle: string;
    function GetLines: TStrings;
    function GetLinesInWindow: Integer;
    function GetLineText: string;
    function GetLogicalCaretXY: TPoint;
    function GetModified: Boolean;
    function GetMonitorChanges: Boolean;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TBCEditorDropFilesEvent;
    function GetReplaceHistory: TStrings;
    function GetSearchText: string;
    function GetSelectionAvailable: Boolean;
    function GetSelectionLength: Integer;
    function GetSelectionMode: TBCEditorSelectionMode;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelectedText: string;
    function GetSettings: IEditorSettings;
    function GetShowSpecialChars: Boolean;
    function GetText: string;
    function GetTextBetween(AStartPos, AEndPos: TPoint): string;
    function GetTextSize: Integer;
    function GetTopLine: Integer;
    procedure SetBlockBegin(const AValue: TPoint);
    procedure SetBlockEnd(const AValue: TPoint);
    procedure SetCaretX(const AValue: Integer);
    procedure SetCaretXY(const AValue: TPoint);
    procedure SetCaretY(const AValue: Integer);
    procedure SetEditorFont(AValue: TFont);
    procedure SetFileName(const AValue: string);
    procedure SetFoldLevel(const AValue: Integer);
    procedure SetHighlighterItem(const AValue: THighlighterItem);
    procedure SetHighlighterName(AValue: string);
    procedure SetInsertMode(AValue: Boolean);
    procedure SetIsFile(AValue: Boolean);
    procedure SetLineBreakStyle(const AValue: string);
    procedure SetLineText(const AValue: string);
    procedure SetLogicalCaretXY(const AValue: TPoint);
    procedure SetModified(const AValue: Boolean);
    procedure SetMonitorChanges(const AValue: Boolean);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TBCEditorDropFilesEvent);
    procedure SetSearchText(const AValue: string);
    procedure SetSelectionMode(AValue: TBCEditorSelectionMode);
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelectedText(const AValue: string);
    procedure SetShowSpecialChars(const AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetTextBetween(AStartPos, AEndPos: TPoint; const Value: string);
    procedure SetTopLine(const AValue: Integer);
    {$ENDREGION}

    // information retrieval
    function GetWordAtPosition(const APosition: TPoint): string;
    function GetWordFromCaret(const ACaretPos: TPoint): string;

    // lock updates
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure BeginUndoBlock;
    procedure EndUndoBlock;

    // make current view the active one if more than one view is managed.
    procedure Activate;
    procedure Clear;
    procedure Close;

    // search
//    procedure SearchAndSelectLine(ALineIndex: Integer; const ALine: string);
//    procedure SearchAndSelectText(const AText: string);
    procedure FindNextWordOccurrence(DirectionForward: Boolean);

    // load and save
    procedure Load(const AStorageName: string = '');
    procedure Save(const AStorageName: string = '');
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

     // selection
    procedure SelectAll;
    procedure SelectWord;

    // other commands

    // clipboard commands
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;

    // properties
    property FindHistory: TStrings
      read GetFindHistory;

    property ReplaceHistory: TStrings
      read GetReplaceHistory;

    property Editor: TBCEditor
      read GetEditor;

    property Form: TCustomForm
      read GetForm;

    { Settings shared by all edit instances managed by IEditorActions instance. }
    property Settings: IEditorSettings
      read GetSettings;

    { Reference to the main 'Actions' instance .}
    property Actions: IEditorActions
      read GetActions;

    // Selection properties
    property BlockBegin: TPoint
      read GetBlockBegin write SetBlockBegin;

    property BlockEnd: TPoint
      read GetBlockEnd write SetBlockEnd;

    property SelectedText: string
      read GetSelectedText write SetSelectedText;

    property SelectionLength: Integer
      read GetSelectionLength;

    property SelStart: Integer
      read GetSelStart write SetSelStart;

    property SelEnd: Integer
      read GetSelEnd write SetSelEnd;

    property InsertMode: Boolean
      read GetInsertMode write SetInsertMode;

    property HighlighterItem: THighlighterItem
      read GetHighlighterItem write SetHighlighterItem;

    { Sets the current (default) selection mode.  }
    property SelectionMode: TBCEditorSelectionMode
      read GetSelectionMode write SetSelectionMode;

    property SelectionAvailable: Boolean
      read GetSelectionAvailable;

    property SearchText: string
      read GetSearchText write SetSearchText;

    // Status information (readonly)
    property CanPaste: Boolean
      read GetCanPaste;

    property CanRedo: Boolean
      read GetCanRedo;

    property CanUndo: Boolean
      read GetCanUndo;

    property TextSize: Integer
      read GetTextSize;

    property LinesInWindow: Integer
      read GetLinesInWindow;

    property CurrentChar: WideChar
      read GetCurrentChar;

    property CurrentWord: string
      read GetCurrentWord;
    //------------------------------------------
    property Lines: TStrings
      read GetLines;

    property Text: string
      read GetText write SetText;

    //---| Cursor properties |-------------------------------------------------
    { current X-coordinate of the caret. }
    property CaretX: Integer
      read GetCaretX write SetCaretX;

    { current Y-coordinate of the caret. }
    property CaretY: Integer
      read GetCaretY write SetCaretY;

    { TPoint representation of current caret position }
    property CaretXY: TPoint
      read GetCaretXY write SetCaretXY;

    property LogicalCaretXY: TPoint
      read GetLogicalCaretXY write SetLogicalCaretXY;

    //---| Folding support |---------------------------------------------------
    property FoldLevel: Integer
      read GetFoldLevel write SetFoldLevel;

    { If IsFile=True then this property holds the content's filename. In the
      other case it is a name that corresponds to content provided by the owning
      application. This name can eg. be associated with a database resource to
      load the content from. }
    property FileName: string
      read GetFileName write SetFileName;

    { Component name of the editor view }
    property Name: string
      read GetName write SetName;

    { Determines if the view content is stored in a file. If True the content
      will be laoded and saved to a file using the FileName property. If False
      this is delegated to the owning instance that can provide the content
      from another resource like eg. a database. }
    property IsFile: Boolean
      read GetIsFile write SetIsFile;

    property LineText: string
      read GetLineText write SetLineText;

    //---| Properties |--------------------------------------------------------

    property MonitorChanges: Boolean
      read GetMonitorChanges write SetMonitorChanges;

    property ShowSpecialChars: Boolean
      read GetShowSpecialChars write SetShowSpecialChars;

    property TextBetween[AStartPos: TPoint; AEndPos: TPoint]: string
      read GetTextBetween write SetTextBetween;

    property TopLine: Integer
      read GetTopLine write SetTopLine;

    { Set when content has changed since initial load or last save. }
    property Modified: Boolean
      read GetModified write SetModified;

    property Encoding: TEncoding
      read GetEncoding;

    property LineBreakStyle: string
      read GetLineBreakStyle write SetLineBreakStyle;

    property HighlighterItem: THighlighterItem
      read GetHighlighterItem write SetHighlighterItem;

    property HighlighterName: string
      read GetHighlighterName write SetHighlighterName;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    // events
    property OnDropFiles: TBCEditorDropFilesEvent
      read GetOnDropFiles write SetOnDropFiles;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;
  end;

  { IEditorSearchEngine }

  IEditorSearchEngine = interface
  ['{5403336C-3E81-4A1B-B2BB-170CF0EF0B84}']
    function GetCurrentIndex: Integer;
    function GetItemGroups: IList<TSearchResultGroup>;
    function GetItemList: IList<TSearchResult>;
    function GetOptions: TBCEditorSearchOptions;
    function GetReplaceText: string;
    function GetSearchAllViews: Boolean;
    function GetSearchText: string;
    procedure SetCurrentIndex(AValue: Integer);
    procedure SetOptions(AValue: TBCEditorSearchOptions);
    procedure SetReplaceText(AValue: string);
    procedure SetSearchAllViews(AValue: Boolean);
    procedure SetSearchText(AValue: string);
    function GetOnChange: IEvent<TNotifyEvent>;
    function GetOnExecute: IEvent<TNotifyEvent>;

    procedure Execute;
    procedure Replace;
    procedure ReplaceAll;
    procedure FindNext;
    procedure FindPrevious;

    property CurrentIndex: Integer
      read GetCurrentIndex write SetCurrentIndex;

    property Options: TBCEditorSearchOptions
      read GetOptions write SetOptions;

    property SearchText : string
      read GetSearchText write SetSearchText;

    property ReplaceText: string
      read GetReplaceText write SetReplaceText;

    property SearchAllViews: Boolean
      read GetSearchAllViews write SetSearchAllViews;

    property ItemList: IList<TSearchResult>
      read GetItemList;

    property ItemGroups: IList<TSearchResultGroup>
      read GetItemGroups;

    property OnChange: IEvent<TNotifyEvent>
      read GetOnChange;

    property OnExecute: IEvent<TNotifyEvent>
      read GetOnExecute;
  end;

  { IEditorEvents }

  { TODO: make a seperation of the events and the dispatcher methods? }

  IEditorEvents = interface
  ['{D078C92D-16DF-4727-A18F-4C76E07D37A2}']
    {$REGION 'property access methods'}
    function GetOnAddEditorView: IEvent<TEditorViewEvent>;
    function GetOnActiveViewChange: IEvent<TNotifyEvent>;
    function GetOnChange: IEvent<TNotifyEvent>;
    function GetOnModified: IEvent<TNotifyEvent>;
    function GetOnAfterSave: IEvent<TStorageEvent>;
    function GetOnBeforeSave: IEvent<TStorageEvent>;
    function GetOnHideEditorToolView: IEvent<TEditorToolViewEvent>;
    function GetOnLoad: IEvent<TStorageEvent>;
    function GetOnOpen: IEvent<TStorageEvent>;
    function GetOnOpenOtherInstance: IEvent<TOpenOtherInstanceEvent>;
    function GetOnSave: IEvent<TStorageEvent>;
    function GetOnShowEditorToolView: IEvent<TEditorToolViewEvent>;
    function GetOnNew: IEvent<TNewEvent>;
//    function GetOnStatusChange: TStatusChangeEvent;
      function GetOnActionExecute: IEvent<TActionExecuteEvent>;
      function GetOnCaretPositionChange: IEvent<TCaretPositionEvent>;
    {$ENDREGION}

    // event dispatch methods
    procedure DoCaretPositionChange;
    procedure DoActiveViewChange;
    procedure DoActionExecute(
          AAction  : TBasicAction;
      var AHandled : Boolean
    );
    procedure DoHighlighterChange;
    procedure DoOpenOtherInstance(const AParams: array of string);
    procedure DoAddEditorView(AEditorView: IEditorView);
    procedure DoStatusMessage(AText: string);
//    procedure DoStatusChange(AChanges: TSynStatusChanges);
    procedure DoShowToolView(AToolView: IEditorToolView);
    procedure DoHideToolView(AToolView: IEditorToolView);
    procedure DoChange;
    procedure DoModified;
    procedure DoOpen(const AName: string);
    procedure DoLoad(const AName: string);
    procedure DoNew(
      const AFileName : string = '';
      const AText     : string = ''
    );

    // events
    property OnAddEditorView: IEvent<TEditorViewEvent>
      read GetOnAddEditorView;

    property OnShowEditorToolView: IEvent<TEditorToolViewEvent>
      read GetOnShowEditorToolView;

    property OnHideEditorToolView: IEvent<TEditorToolViewEvent>
      read GetOnHideEditorToolView;

    property OnChange: IEvent<TNotifyEvent>
      read GetOnChange;

    property OnModified: IEvent<TNotifyEvent>
      read GetOnModified;

    property OnActiveViewChange: IEvent<TNotifyEvent>
      read GetOnActiveViewChange;

//    property OnStatusChange: TStatusChangeEvent
//      read GetOnStatusChange write SetOnStatusChange;

    property OnLoad: IEvent<TStorageEvent>
      read GetOnLoad;

    property OnNew: IEvent<TNewEvent>
      read GetOnNew;

    property OnOpen: IEvent<TStorageEvent>
      read GetOnOpen;

    property OnBeforeSave: IEvent<TStorageEvent>
      read GetOnBeforeSave;

    property OnAfterSave: IEvent<TStorageEvent>
      read GetOnAfterSave;

    property OnOpenOtherInstance: IEvent<TOpenOtherInstanceEvent>
      read GetOnOpenOtherInstance;

    property OnActionExecute: IEvent<TActionExecuteEvent>
      read GetOnActionExecute;

    property OnCaretPositionChange: IEvent<TCaretPositionEvent>
      read GetOnCaretPositionChange;
  end;

  { Settings we should store if we would like to restore the editor to its
    current state }

  { IEditorSettings }

  IEditorSettings = interface
  ['{CDB18A45-54AA-49F2-82C7-15D68C952197}']
    {$REGION 'property access methods'}
    function GetAutoFormatXML: Boolean;
    function GetAutoGuessHighlighterType: Boolean;
    function GetCloseWithESC: Boolean;
    function GetColors: TEditorColorSettings;
    function GetDebugMode: Boolean;
    function GetDimInactiveView: Boolean;
    function GetEditorFont: TFont;
    function GetEditorOptions: TEditorOptionsSettings;
    function GetFileName: string;
    function GetFormSettings: TFormSettings;
//    function GetHighlighterAttributes: THighlighterAttributes;
    function GetHighlighters: THighlighters;
    function GetHighlighterType: string;
    function GetLanguageCode: string;
    function GetReadOnly: Boolean;
    function GetSingleInstance: Boolean;
    function GetToolSettings: TEditorToolSettings;
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetXML: string;
    procedure SetAutoFormatXML(const AValue: Boolean);
    procedure SetAutoGuessHighlighterType(const AValue: Boolean);
    procedure SetCloseWithESC(const AValue: Boolean);
    procedure SetColors(AValue: TEditorColorSettings);
    procedure SetDebugMode(AValue: Boolean);
    procedure SetDimInactiveView(const AValue: Boolean);
    procedure SetEditorFont(AValue: TFont);
    procedure SetEditorOptions(AValue: TEditorOptionsSettings);
    procedure SetFileName(const AValue: string);
    procedure SetFormSettings(const AValue: TFormSettings);
//    procedure SetHighlighterAttributes(AValue: THighlighterAttributes);
    procedure SetHighlighterType(const AValue: string);
    procedure SetLanguageCode(AValue: string);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetSingleInstance(AValue: Boolean);
    procedure SetToolSettings(AValue: TEditorToolSettings);
    {$ENDREGION}

    procedure Load;
    procedure Save;
    procedure Apply;

    property Colors: TEditorColorSettings
      read GetColors write SetColors;

    property ToolSettings:  TEditorToolSettings
      read GetToolSettings write SetToolSettings;

    property FileName: string
      read GetFileName write SetFileName;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    property DimInactiveView: Boolean
      read GetDimInactiveView write SetDimInactiveView;

    property HighlighterType: string
      read GetHighlighterType write SetHighlighterType;

    { Locale to be used by the application }
    property LanguageCode: string
      read GetLanguageCode write SetLanguageCode;

    property ReadOnly: Boolean
      read GetReadOnly write SetReadOnly;

    property AutoFormatXML: Boolean
      read GetAutoFormatXML write SetAutoFormatXML;

    property CloseWithESC: Boolean
      read GetCloseWithESC write SetCloseWithESC;

    property AutoGuessHighlighterType: Boolean
      read GetAutoGuessHighlighterType write SetAutoGuessHighlighterType;

    property Highlighters: THighlighters
      read GetHighlighters;

    property FormSettings: TFormSettings
      read GetFormSettings write SetFormSettings;

    property DebugMode: Boolean
      read GetDebugMode write SetDebugMode;

    property SingleInstance: Boolean
      read GetSingleInstance write SetSingleInstance;

    property XML: string
      read GetXML;

    // Editor options
    property EditorOptions: TEditorOptionsSettings
      read GetEditorOptions write SetEditorOptions;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;
  end;

  IEditorViews = interface
  ['{FBFB8DC6-7663-4EA4-935D-5B9F3CD7C753}']
    function GetView(AIndex: Integer): IEditorView;
    function GetViewByFileName(AFileName: string): IEditorView;
    function GetViewByName(AName: string): IEditorView;
    function GetCount: Integer;
    function GetViewList: IList<IEditorView>;

    function Add(
      const AName        : string = '';
      const AFileName    : string = '';
      const AHighlighter : string = ''
    ): IEditorView;

    //function Delete(AIndex: Integer): Boolean; overload;
    function Delete(AView: IEditorView): Boolean; overload;
    //function Delete(const AName: string): Boolean; overload;
    procedure Clear(AExceptActive: Boolean = False);

    function GetEnumerator: TEditorViewListEnumerator;

    property Views[AIndex: Integer]: IEditorView
      read GetView; default;

    property ViewByName[AName: string]: IEditorView
      read GetViewByName;

    property ViewByFileName[AFileName: string]: IEditorView
      read GetViewByFileName;

    property ViewList: IList<IEditorView>
      read GetViewList;

    property Count: Integer
      read GetCount;
  end;

  IEditorToolView = interface
  ['{F6BEE8F6-BA4D-4B38-8FB0-79088B615DF5}']
    function GetForm: TForm;
    function GetName: string;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    { Lets the view respond to changes. }
    procedure UpdateView;

    //procedure Refresh; TODO: refresh all items
    procedure SetFocus;
    function Focused: Boolean;

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;
  end;

  IEditorToolViews = interface
  ['{A5575878-A189-4F3C-9008-61899B739DA1}']
    function GetView(AIndex: Integer): IEditorToolView;
    function GetViewByName(AName: string): IEditorToolView;
    function GetCount: Integer;

    function GetEnumerator: TEditorToolViewListEnumerator;

    function Register(
            AFormClass     : TComponentClass;
            ASettingsClass : TComponentClass;
      const AName          : string = ''
    ): Boolean;
    procedure Hide;

    property Views[AIndex: Integer]: IEditorToolView
      read GetView;

    property ViewByName[AName: string]: IEditorToolView
      read GetViewByName; default;

    property Count: Integer
      read GetCount;
  end;

  IEditorCommands = interface
  ['{5CB77731-425D-44FD-93BA-2137875F76B5}']
    procedure AdjustFontSize(AOffset: Integer);
    procedure AlignSelection(
      const AToken                  : string;
            ACompressWS             : Boolean;
            AInsertSpaceBeforeToken : Boolean;
            AInsertSpaceAfterToken  : Boolean;
            AAlignInParagraphs      : Boolean
    );
    procedure AssignHighlighter(const AName: string);
    procedure Base64FromSelection(ADecode: Boolean = False);
    procedure CompressSpace;
    procedure CompressWhitespace;
    procedure ConvertTabsToSpacesInSelection;
    procedure CopyToClipboard;
    procedure CreateDesktopLink;
    procedure DequoteLinesInSelection;
    procedure DequoteSelection;
    procedure FindNext;
    procedure FindPrevious;
    procedure FormatCode;
    procedure GuessHighlighterType;
    procedure Indent;
    procedure InsertTextAtCaret(const AText: string);
    procedure LowerCaseSelection;
    procedure OpenFileAtCursor;
    procedure PascalStringFromSelection;
    procedure QuoteLinesInSelection(ADelimit : Boolean = False);
    procedure QuoteSelection;
    procedure SortSelectedLines;
    procedure SyncEditSelection;
    procedure ToggleBlockComment;
    procedure ToggleHighlighter;
    procedure ToggleLineComment;
    procedure UpperCaseSelection;
    procedure URLFromSelection(ADecode: Boolean = False);
    procedure XMLFromSelection(ADecode: Boolean = False);
    procedure MergeBlankLinesInSelection;
    procedure Save;
    procedure SaveAll;
    function SelectBlockAroundCursor(
      const AStartTag  : string;
      const AEndTag    : string;
      AIncludeStartTag : Boolean;
      AIncludeEndTag   : Boolean
    ): Boolean;
    procedure SmartSelect;
    procedure StripCommentsFromSelection;
    procedure StripMarkupFromSelection;
    procedure StripCharsFromSelection(
      AFirst : Boolean;
      ALast  : Boolean
    );
    procedure UnIndent;
  end;

  { IEditorMenus }

  IEditorMenus = interface
  ['{4B6F6B6A-8A72-478B-B3AF-089E72E23CDF}']
    {$REGION 'property access methods'}
    function GetClipboardPopupMenu: TPopupMenu;
    function GetEditorPopupMenu: TPopupMenu;
    function GetEncodingPopupMenu: TPopupMenu;
    function GetExportPopupMenu: TPopupMenu;
    function GetFilePopupMenu: TPopupMenu;
    function GetFoldPopupMenu: TPopupMenu;
    function GetHighlighterPopupMenu: TPopupMenu;
    function GetInsertPopupMenu: TPopupMenu;
    function GetLineBreakStylePopupMenu: TPopupMenu;
    function GetSearchPopupMenu: TPopupMenu;
    function GetSelectionDecodePopupMenu: TPopupMenu;
    function GetSelectionEncodePopupMenu: TPopupMenu;
    function GetSelectionModePopupMenu: TPopupMenu;
    function GetSelectionPopupMenu: TPopupMenu;
    function GetSelectPopupMenu: TPopupMenu;
    function GetSettingsPopupMenu: TPopupMenu;
    {$ENDREGION}

    property ClipboardPopupMenu: TPopupMenu
      read GetClipboardPopupMenu;

    property EditorPopupMenu: TPopupMenu
      read GetEditorPopupMenu;

    property EncodingPopupMenu: TPopupMenu
      read GetEncodingPopupMenu;

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

    property LineBreakStylePopupMenu: TPopupMenu
      read GetLineBreakStylePopupMenu;

    property SearchPopupMenu: TPopupMenu
      read GetSearchPopupMenu;

    property SelectPopupMenu: TPopupMenu
      read GetSelectPopupMenu;

    property SelectionPopupMenu: TPopupMenu
      read GetSelectionPopupMenu;

    property SelectionEncodePopupMenu: TPopupMenu
      read GetSelectionEncodePopupMenu;

    property SelectionDecodePopupMenu: TPopupMenu
      read GetSelectionDecodePopupMenu;

    property SelectionModePopupMenu: TPopupMenu
      read GetSelectionModePopupMenu;

    property SettingsPopupMenu: TPopupMenu
      read GetSettingsPopupMenu;
  end;

  { All supported actions by the editor views. }

  IEditorActions = interface
  ['{E42EF2E3-A7A0-4847-B299-3C35699DC708}']
    function GetActionList: TActionList;
    function GetItem(AName: string): TCustomAction;

    procedure UpdateActions;
    procedure UpdateHighLighterActions;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    property ActionList: TActionList
      read GetActionList;

    { TODO -oTS : Declare all actions as properties? }
  end;

  { IEditorManager }

  IEditorManager = interface
  ['{631A126F-1693-4E25-B691-CD2487BCB820}']
    {$REGION 'property access methods'}
    function GetActions: IEditorActions;
    function GetCommands: IEditorCommands;
    function GetEvents: IEditorEvents;
    function GetMenus: IEditorMenus;
    function GetSearchEngine: IEditorSearchEngine;
    function GetSettings: IEditorSettings;
    function GetToolViews: IEditorToolViews;
    function GetViews: IEditorViews;
    function GetPersistSettings: Boolean;
    procedure SetPersistSettings(const AValue: Boolean);
    function GetActiveView: IEditorView;
    procedure SetActiveView(AValue: IEditorView);
    function GetHighlighters: THighlighters;
    function GetKeyCommands: TBCEditorKeyCommands;
    {$ENDREGION}

    procedure UpdateActions;
    function ActivateView(const AName: string): Boolean;
    function OpenFile(const AFileName: string): IEditorView;
    function NewFile(
      const AFileName  : string;
      const AText      : string = ''
    ): IEditorView;
    function SaveFile(
      const AFileName : string = '';
      AShowDialog     : Boolean = False
    ): Boolean;

    property PersistSettings: Boolean
      read GetPersistSettings write SetPersistSettings;

    property Highlighters: THighlighters
      read GetHighlighters;

    property ActiveView: IEditorView
      read GetActiveView write SetActiveView;

    property Events: IEditorEvents
      read GetEvents;

    property Views: IEditorViews
      read GetViews;

    property ToolViews: IEditorToolViews
      read GetToolViews;

    property Menus: IEditorMenus
      read GetMenus;

    property Actions: IEditorActions
      read GetActions;

    property Commands: IEditorCommands
      read GetCommands;

    property KeyCommands: TBCEditorKeyCommands
      read GetKeyCommands;

    property Settings: IEditorSettings
      read GetSettings;

    property SearchEngine: IEditorSearchEngine
      read GetSearchEngine;
  end;

  TEditorViewListEnumerator = class
  strict private
    FIndex : Integer;
    FList  : IList<IEditorView>;

  public
    constructor Create(AList: IList<IEditorView>);
    function GetCurrent: IEditorView;
    function MoveNext: Boolean;
    property Current: IEditorView
      read GetCurrent;
  end;

  { TEditorToolViewListEnumerator }

  TEditorToolViewListEnumerator = class
  strict private
    FIndex : Integer;
    FList  : IEditorToolViews;

  public
    constructor Create(AList: IEditorToolViews);
    procedure BeforeDestruction; override;
    function GetCurrent: IEditorToolView;
    function MoveNext: Boolean;
    property Current: IEditorToolView
      read GetCurrent;
  end;

  { IEditorManagerFactory }

  IEditorManagerFactory = interface
  ['{BE85A08D-936E-4F76-BBE1-A1999DE882B9}']
    function CreateInstance(
            AOwner            : TComponent = nil;
            APersistSettings  : Boolean = False;
      const ASettingsFileName : string = ''
    ): IEditorManager; overload;

    function CreateInstance(
      AOwner    : TComponent;
      ASettings : IEditorSettings
    ): IEditorManager; overload;
  end;

  IEditorViewFactory = interface
  ['{CAEF28D5-0E70-4D4E-AEC7-07BD6E743945}']
    function CreateInstance(
      AParent            : TWinControl;
      AManager           : IEditorManager;
      const AName        : string = '';
      const AFileName    : string = '';
      const AHighlighter : string = 'TXT'
    ): IEditorView;
  end;

  IEditorSettingsFactory = interface
  ['{6479785C-A7C0-40D9-9036-D39BEE780CA2}']
    function CreateInstance(
      AOwner          : TComponent = nil;
      const AFileName : string = ''
    ): IEditorSettings;
  end;

  IEditorMenusFactory = interface
  ['{99676D13-D72A-4C2F-B96B-84FC290DDE12}']
    function CreateMainMenu(
      AOwner: TComponent
    ): TMainMenu;
  end;

  IEditorToolbarsFactory = interface
  ['{0E1F34F3-E5AF-4A59-8B13-0F9B4D11D69D}']
    function CreateMainToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;

    function CreateSelectionToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;

    function CreateRightToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;
  end;

implementation

{$REGION 'TEditorViewListEnumerator'}
constructor TEditorViewListEnumerator.Create(AList: IList<IEditorView>);
begin
  FList := AList;
  FIndex := -1;
end;

function TEditorViewListEnumerator.GetCurrent: IEditorView;
begin
  if FList.Count > 0 then
    Result := FList[FIndex] as IEditorView;
end;

function TEditorViewListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < (FList.Count - 1);
  if Result then
    Inc(FIndex);
end;
{$ENDREGION}

{$REGION 'TEditorToolViewListEnumerator'}
constructor TEditorToolViewListEnumerator.Create(AList: IEditorToolViews);
begin
  FList := AList;
  FIndex := -1;
end;

procedure TEditorToolViewListEnumerator.BeforeDestruction;
begin
  FList := nil;
  inherited BeforeDestruction;
end;

function TEditorToolViewListEnumerator.GetCurrent: IEditorToolView;
begin
  Result := FList.Views[FIndex] as IEditorToolView;
end;

function TEditorToolViewListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < (FList.Count - 1);
  if Result then
    Inc(FIndex);
end;
{$ENDREGION}

end.
