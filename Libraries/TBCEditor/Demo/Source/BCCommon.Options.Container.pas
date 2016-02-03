unit BCCommon.Options.Container;

interface

uses
  System.Classes, Vcl.Forms, BCCommon.FileUtils, IniPersist;

type
  TOptionsContainer = class(TPersistent)
  private
    { Caret }
    FShowCaret: Boolean;
    FRightMouseClickMovesCaret: Boolean;
    FShowNonblinkingCaret: Boolean;
    FNonblinkingCaretBackgroundColor: string;
    FNonblinkingCaretForegroundColor: string;
    FInsertCaret: Integer;
    FOverwriteCaret: Integer;
    { Code folding }
    FShowCodeFolding: Boolean;
    FFoldMultilineComments: Boolean;
    FHighlightIndentGuides: Boolean;
    FHighlightMatchingPair: Boolean;
    FShowCollapsedCodeHint: Boolean;
    FShowCollapsedLine: Boolean;
    FShowIndentGuides: Boolean;
    FUncollapseByHintClick: Boolean;
    FCodeFoldingMarkStyle: Integer;
    FCodeFoldingHintRowCount: Integer;
    { Completion proposal }
    FCompletionProposalCaseSensitive: Boolean;
    FCompletionProposalEnabled: Boolean;
    FCompletionProposalAutoInvoke: Boolean;
    FCompletionProposalShortcut: string;
    { Left margin }
    FLeftMarginVisible: Boolean;
    FLeftMarginAutosize: Boolean;
    FLeftMarginShowBookmarks: Boolean;
    FLeftMarginShowBookmarkPanel: Boolean;
    FLeftMarginShowLineState: Boolean;
    FLeftMarginLineNumbersShowInTens: Boolean;
    FLeftMarginLineNumbersShowLeadingZeros: Boolean;
    FLeftMarginLineNumbersShowAfterLastLine: Boolean;
    FLeftMarginLineNumbersStartFrom: Integer;
    FLeftMarginWidth: Integer;
    FLeftMarginBookmarkPanelWidth: Integer;
    { Matching pair }
    FMatchingPairEnabled: Boolean;
    FMatchingPairHighlightAfterToken: Boolean;
    FMatchingPairHighlightUnmatched: Boolean;
    { Minimap }
    FMinimapVisible: Boolean;
    FMinimapShowBookmarks: Boolean;
    FMinimapShowIndentGuides: Boolean;
    FMinimapWidth: Integer;
    FMinimapAlign: Integer;
    { Editor }
    FAutoIndent: Boolean;
    FAutoSave: Boolean;
    FDragDropEditing: Boolean;
    FDropFiles: Boolean;
    FGroupUndo: Boolean;
    FTrimTrailingSpaces: Boolean;
    FUndoAfterSave: Boolean;
    FLineSpacing: Integer;
    { Right margin }
    FRightMarginVisible: Boolean;
    FRightMarginMouseMove: Boolean;
    FRightMarginShowMovingHint: Boolean;
    FRightMarginPosition: Integer;
    { Scroll }
    FScrollAutosizeMaxWidth: Boolean;
    FScrollHalfPage: Boolean;
    FScrollHintFollows: Boolean;
    FScrollPastEndOfFile: Boolean;
    FScrollPastEndOfLineMarker: Boolean;
    FScrollShowHint: Boolean;
    { Search }
    FSearchVisible: Boolean;
    FDocumentSpecificSearch: Boolean;
    FDocumentSpecificSearchText: string;
    FShowSearchMap: Boolean;
    { Selection }
    FSelectionVisible: Boolean;
    FALTSetsColumnMode: Boolean;
    FHighlightSimilarTerms: Boolean;
    FSelectionFromEndOfLine: Boolean;
    FSelectionToEndOfLine: Boolean;
    FSelectionToEndOfLastLine: Boolean;
    FTripleClickRowSelect: Boolean;
    { Special chars }
    FSpecialCharsUseTextColor: Boolean;
    FSpecialCharsStyle: Integer;
    FSpecialEndOfLineVisible: Boolean;
    FSpecialEndOfLineColor: string;
    FSpecialCharsEndOfLineStyle: Integer;
    FSpecialCharsSelectionVisible: Boolean;
    FSpecialSelectionColor: string;
    { Tabs }
    FSelectedBlockIndent: Boolean;
    FTabsToSpaces: Boolean;
    FTabWidth: Integer;
    { Compare }
    FCompareIgnoreBlanks: Boolean;
    FCompareIgnoreCase: Boolean;
    { Main menu }
    FMainMenuFontName: string;
    FMainMenuFontSize: Integer;
    FMainMenuSystemFontName: string;
    FMainMenuSystemFontSize: Integer;
    FMainMenuUseSystemFont: Boolean;
    { Output }
    FOutputIndent: Integer;
    FOutputShowTreeLines: Boolean;
    FOutputShowCheckBox: Boolean;
    { Print }
    FPrintDateTime: Integer;
    FPrintDocumentName: Integer;
    FPrintPageNumber: Integer;
    FPrintPrintedBy: Integer;
    FPrintShowFooterLine: Boolean;
    FPrintShowHeaderLine: Boolean;
    FPrintShowLineNumbers: Boolean;
    FPrintWordWrapLine: Boolean;
    { Statusbar }
    FStatusBarFontName: string;
    FStatusBarFontSize: Integer;
    FStatusBarUseSystemFont: Boolean;
    FStatusBarShowMacro: Boolean;
    FStatusBarShowCaretPosition: Boolean;
    FStatusBarShowKeyState: Boolean;
    FStatusBarShowModified: Boolean;
    { View }
    FEnableLineNumbers: Boolean;
    FEnableSelectionMode: Boolean;
    FEnableSpecialChars: Boolean;
    FEnableWordWrap: Boolean;
  public
    procedure AssignTo(Dest: TPersistent); override;
    procedure ReadIniFile; virtual;
    procedure WriteIniFile; virtual;
  published
    { Caret }
    [IniValue('Options', 'ShowCaret', 'True')]
    property ShowCaret: Boolean read FShowCaret write FShowCaret;
    [IniValue('Options', 'RightMouseClickMovesCaret', 'False')]
    property RightMouseClickMovesCaret: Boolean read FRightMouseClickMovesCaret write FRightMouseClickMovesCaret;
    [IniValue('Options', 'ShowNonblinkingCaret', 'False')]
    property ShowNonblinkingCaret: Boolean read FShowNonblinkingCaret write FShowNonblinkingCaret;
    [IniValue('Options', 'NonblinkingCaretBackgroundColor', 'clBlack')]
    property NonblinkingCaretBackgroundColor: string read FNonblinkingCaretBackgroundColor write FNonblinkingCaretBackgroundColor;
    [IniValue('Options', 'NonblinkingCaretForegroundColor', 'clWhite')]
    property NonblinkingCaretForegroundColor: string read FNonblinkingCaretForegroundColor write FNonblinkingCaretForegroundColor;
    [IniValue('Options', 'InsertCaret', '1')]
    property InsertCaret: Integer read FInsertCaret write FInsertCaret;
    [IniValue('Options', 'OverwriteCaret', '1')]
    property OverwriteCaret: Integer read FOverwriteCaret write FOverwriteCaret;
    { Code folding }
    [IniValue('Options', 'ShowCodeFolding', 'True')]
    property ShowCodeFolding: Boolean read FShowCodeFolding write FShowCodeFolding;
    [IniValue('Options', 'FoldMultilineComments', 'False')]
    property FoldMultilineComments: Boolean read FFoldMultilineComments write FFoldMultilineComments;
    [IniValue('Options', 'HighlightIndentGuides', 'True')]
    property HighlightIndentGuides: Boolean read FHighlightIndentGuides write FHighlightIndentGuides;
    [IniValue('Options', 'HighlightMatchingPair', 'True')]
    property HighlightMatchingPair: Boolean read FHighlightMatchingPair write FHighlightMatchingPair;
    [IniValue('Options', 'ShowCollapsedCodeHint', 'True')]
    property ShowCollapsedCodeHint: Boolean read FShowCollapsedCodeHint write FShowCollapsedCodeHint;
    [IniValue('Options', 'ShowCollapsedLine', 'False')]
    property ShowCollapsedLine: Boolean read FShowCollapsedLine write FShowCollapsedLine;
    [IniValue('Options', 'ShowIndentGuides', 'True')]
    property ShowIndentGuides: Boolean read FShowIndentGuides write FShowIndentGuides;
    [IniValue('Options', 'UncollapseByHintClick', 'True')]
    property UncollapseByHintClick: Boolean read FUncollapseByHintClick write FUncollapseByHintClick;
    [IniValue('Options', 'CodeFoldingMarkStyle', '0')]
    property CodeFoldingMarkStyle: Integer read FCodeFoldingMarkStyle write FCodeFoldingMarkStyle;
    [IniValue('Options', 'CodeFoldingHintRowCount', '40')]
    property CodeFoldingHintRowCount: Integer read FCodeFoldingHintRowCount write FCodeFoldingHintRowCount;
    { Completion proposal }
    [IniValue('Options', 'CompletionProposalCaseSensitive', 'True')]
    property CompletionProposalCaseSensitive: Boolean read FCompletionProposalCaseSensitive write FCompletionProposalCaseSensitive;
    [IniValue('Options', 'CompletionProposalEnabled', 'True')]
    property CompletionProposalEnabled: Boolean read FCompletionProposalEnabled write FCompletionProposalEnabled;
    [IniValue('Options', 'CompletionProposalAutoInvoke', 'False')]
    property CompletionProposalAutoInvoke: Boolean read FCompletionProposalAutoInvoke write FCompletionProposalAutoInvoke;
    [IniValue('Options', 'CompletionProposalShortcut', 'Ctrl+Space')]
    property CompletionProposalShortcut: string read FCompletionProposalShortcut write FCompletionProposalShortcut;
    { Left margin }
    [IniValue('Options', 'LeftMarginVisible', 'True')]
    property LeftMarginVisible: Boolean read FLeftMarginVisible write FLeftMarginVisible;
    [IniValue('Options', 'LeftMarginAutosize', 'True')]
    property LeftMarginAutosize: Boolean read FLeftMarginAutosize write FLeftMarginAutosize;
    [IniValue('Options', 'LeftMarginShowBookmarks', 'True')]
    property LeftMarginShowBookmarks: Boolean read FLeftMarginShowBookmarks write FLeftMarginShowBookmarks;
    [IniValue('Options', 'LeftMarginShowBookmarkPanel', 'True')]
    property LeftMarginShowBookmarkPanel: Boolean read FLeftMarginShowBookmarkPanel write FLeftMarginShowBookmarkPanel;
    [IniValue('Options', 'LeftMarginShowLineState', 'True')]
    property LeftMarginShowLineState: Boolean read FLeftMarginShowLineState write FLeftMarginShowLineState;
    [IniValue('Options', 'LeftMarginLineNumbersShowInTens', 'True')]
    property LeftMarginLineNumbersShowInTens: Boolean read FLeftMarginLineNumbersShowInTens write FLeftMarginLineNumbersShowInTens;
    [IniValue('Options', 'LeftMarginLineNumbersShowLeadingZeros', 'False')]
    property LeftMarginLineNumbersShowLeadingZeros: Boolean read FLeftMarginLineNumbersShowLeadingZeros write FLeftMarginLineNumbersShowLeadingZeros;
    [IniValue('Options', 'LeftMarginLineNumbersShowAfterLastLine', 'False')]
    property LeftMarginLineNumbersShowAfterLastLine: Boolean read FLeftMarginLineNumbersShowAfterLastLine write FLeftMarginLineNumbersShowAfterLastLine;
    [IniValue('Options', 'LeftMarginLineNumbersStartFrom', '1')]
    property LeftMarginLineNumbersStartFrom: Integer read FLeftMarginLineNumbersStartFrom write FLeftMarginLineNumbersStartFrom;
    [IniValue('Options', 'LeftMarginWidth', '57')]
    property LeftMarginWidth: Integer read FLeftMarginWidth write FLeftMarginWidth;
    [IniValue('Options', 'LeftMarginBookmarkPanelWidth', '20')]
    property LeftMarginBookmarkPanelWidth: Integer read FLeftMarginBookmarkPanelWidth write FLeftMarginBookmarkPanelWidth;
    { Matching pair }
    [IniValue('Options', 'MatchingPairEnabled', 'True')]
    property MatchingPairEnabled: Boolean read FMatchingPairEnabled write FMatchingPairEnabled;
    [IniValue('Options', 'MatchingPairHighlightAfterToken', 'False')]
    property MatchingPairHighlightAfterToken: Boolean read FMatchingPairHighlightAfterToken write FMatchingPairHighlightAfterToken;
    [IniValue('Options', 'MatchingPairHighlightUnmatched', 'True')]
    property MatchingPairHighlightUnmatched: Boolean read FMatchingPairHighlightUnmatched write FMatchingPairHighlightUnmatched;
    { Minimap }
    [IniValue('Options', 'MinimapChecked', 'False')]
    property MinimapVisible: Boolean read FMinimapVisible write FMinimapVisible;
    [IniValue('Options', 'MinimapShowBookmarks', 'False')]
    property MinimapShowBookmarks: Boolean read FMinimapShowBookmarks write FMinimapShowBookmarks;
    [IniValue('Options', 'MinimapShowIndentGuides', 'False')]
    property MinimapShowIndentGuides: Boolean read FMinimapShowIndentGuides write FMinimapShowIndentGuides;
    [IniValue('Options', 'MinimapWidth', '100')]
    property MinimapWidth: Integer read FMinimapWidth write FMinimapWidth;
    [IniValue('Options', 'MinimapAlign', '1')]
    property MinimapAlign: Integer read FMinimapAlign write FMinimapAlign;
    { Editor }
    [IniValue('Options', 'AutoIndent', 'True')]
    property AutoIndent: Boolean read FAutoIndent write FAutoIndent;
    [IniValue('Options', 'AutoSave', 'False')]
    property AutoSave: Boolean read FAutoSave write FAutoSave;
    [IniValue('Options', 'DragDropEditing', 'True')]
    property DragDropEditing: Boolean read FDragDropEditing write FDragDropEditing;
    [IniValue('Options', 'DropFiles', 'True')]
    property DropFiles: Boolean read FDropFiles write FDropFiles;
    [IniValue('Options', 'GroupUndo', 'True')]
    property GroupUndo: Boolean read FGroupUndo write FGroupUndo;
    [IniValue('Options', 'TrimTrailingSpaces', 'True')]
    property TrimTrailingSpaces: Boolean read FTrimTrailingSpaces write FTrimTrailingSpaces;
    [IniValue('Options', 'UndoAfterSave', 'False')]
    property UndoAfterSave: Boolean read FUndoAfterSave write FUndoAfterSave;
    [IniValue('Options', 'LineSpacing', '1')]
    property LineSpacing: Integer read FLineSpacing write FLineSpacing;
    { Right margin }
    [IniValue('Options', 'RightMarginVisible', 'True')]
    property RightMarginVisible: Boolean read FRightMarginVisible write FRightMarginVisible;
    [IniValue('Options', 'RightMarginMouseMove', 'True')]
    property RightMarginMouseMove: Boolean read FRightMarginMouseMove write FRightMarginMouseMove;
    [IniValue('Options', 'RightMarginShowMovingHint', 'True')]
    property RightMarginShowMovingHint: Boolean read FRightMarginShowMovingHint write FRightMarginShowMovingHint;
    [IniValue('Options', 'RightMarginPosition', '80')]
    property RightMarginPosition: Integer read FRightMarginPosition write FRightMarginPosition;
    { Scroll }
    [IniValue('Options', 'ScrollAutosizeMaxWidth', 'False')]
    property ScrollAutosizeMaxWidth: Boolean read FScrollAutosizeMaxWidth write FScrollAutosizeMaxWidth;
    [IniValue('Options', 'ScrollHalfPage', 'False')]
    property ScrollHalfPage: Boolean read FScrollHalfPage write FScrollHalfPage;
    [IniValue('Options', 'ScrollHintFollows', 'False')]
    property ScrollHintFollows: Boolean read FScrollHintFollows write FScrollHintFollows;
    [IniValue('Options', 'ScrollPastEndOfFile', 'False')]
    property ScrollPastEndOfFile: Boolean read FScrollPastEndOfFile write FScrollPastEndOfFile;
    [IniValue('Options', 'ScrollPastEndOfLineMarker', 'True')]
    property ScrollPastEndOfLineMarker: Boolean read FScrollPastEndOfLineMarker write FScrollPastEndOfLineMarker;
    [IniValue('Options', 'ScrollShowHint', 'True')]
    property ScrollShowHint: Boolean read FScrollShowHint write FScrollShowHint;
    { Search }
    [IniValue('Options', 'SearchVisible', 'False')]
    property SearchVisible: Boolean read FSearchVisible write FSearchVisible;
    [IniValue('Options', 'DocumentSpecificSearch', 'False')]
    property DocumentSpecificSearch: Boolean read FDocumentSpecificSearch write FDocumentSpecificSearch;
    property DocumentSpecificSearchText: string read FDocumentSpecificSearchText write FDocumentSpecificSearchText;
    [IniValue('Options', 'ShowSearchMap', 'True')]
    property ShowSearchMap: Boolean read FShowSearchMap write FShowSearchMap;
    { Selection }
    [IniValue('Options', 'SelectionVisible', 'True')]
    property SelectionVisible: Boolean read FSelectionVisible write FSelectionVisible;
    [IniValue('Options', 'ALTSetsColumnMode', 'True')]
    property ALTSetsColumnMode: Boolean read FALTSetsColumnMode write FALTSetsColumnMode;
    [IniValue('Options', 'HighlightSimilarTerms', 'True')]
    property HighlightSimilarTerms: Boolean read FHighlightSimilarTerms write FHighlightSimilarTerms;
    [IniValue('Options', 'SelectionFromEndOfLine', 'False')]
    property SelectionFromEndOfLine: Boolean read FSelectionFromEndOfLine write FSelectionFromEndOfLine;
    [IniValue('Options', 'SelectionToEndOfLine', 'False')]
    property SelectionToEndOfLine: Boolean read FSelectionToEndOfLine write FSelectionToEndOfLine;
    [IniValue('Options', 'SelectionToEndOfLastLine', 'False')]
    property SelectionToEndOfLastLine: Boolean read FSelectionToEndOfLastLine write FSelectionToEndOfLastLine;
    [IniValue('Options', 'TripleClickRowSelect', 'False')]
    property TripleClickRowSelect: Boolean read FTripleClickRowSelect write FTripleClickRowSelect;
    { Special chars }
    [IniValue('Options', 'SpecialCharsUseTextColor', 'True')]
    property SpecialCharsUseTextColor: Boolean read FSpecialCharsUseTextColor write FSpecialCharsUseTextColor;
    [IniValue('Options', 'SpecialCharsStyle', '0')]
    property SpecialCharsStyle: Integer read FSpecialCharsStyle write FSpecialCharsStyle;
    [IniValue('Options', 'SpecialEndOfLineVisible', 'True')]
    property SpecialEndOfLineVisible: Boolean read FSpecialEndOfLineVisible write FSpecialEndOfLineVisible;
    [IniValue('Options', 'SpecialEndOfLineColor', 'clBlack')]
    property SpecialEndOfLineColor: string read FSpecialEndOfLineColor write FSpecialEndOfLineColor;
    [IniValue('Options', 'SpecialCharsEndOfLineStyle', '0')]
    property SpecialCharsEndOfLineStyle: Integer read FSpecialCharsEndOfLineStyle write FSpecialCharsEndOfLineStyle;
    [IniValue('Options', 'SpecialCharsSelectionVisible', 'False')]
    property SpecialCharsSelectionVisible: Boolean read FSpecialCharsSelectionVisible write FSpecialCharsSelectionVisible;
    [IniValue('Options', 'SpecialSelectionColor', 'clBlack')]
    property SpecialSelectionColor: string read FSpecialSelectionColor write FSpecialSelectionColor;
    { Tabs }
    [IniValue('Options', 'SelectedBlockIndent', 'True')]
    property SelectedBlockIndent: Boolean read FSelectedBlockIndent write FSelectedBlockIndent;
    [IniValue('Options', 'TabsToSpaces', 'False')]
    property TabsToSpaces: Boolean read FTabsToSpaces write FTabsToSpaces;
    [IniValue('Options', 'TabWidth', '2')]
    property TabWidth: Integer read FTabWidth write FTabWidth;
    { View }
    [IniValue('Options', 'EnableLineNumbers', 'True')]
    property EnableLineNumbers: Boolean read FEnableLineNumbers write FEnableLineNumbers;
    [IniValue('Options', 'EnableSelectionMode', 'False')]
    property EnableSelectionMode: Boolean read FEnableSelectionMode write FEnableSelectionMode;
    [IniValue('Options', 'EnableSpecialChars', 'False')]
    property EnableSpecialChars: Boolean read FEnableSpecialChars write FEnableSpecialChars;
    [IniValue('Options', 'EnableWordWrap', 'False')]
    property EnableWordWrap: Boolean read FEnableWordWrap write FEnableWordWrap;
    { Compare }
    [IniValue('Options', 'CompareIgnoreBlanks', 'True')]
    property CompareIgnoreBlanks: Boolean read FCompareIgnoreBlanks write FCompareIgnoreBlanks;
    [IniValue('Options', 'CompareIgnoreCase', 'True')]
    property CompareIgnoreCase: Boolean read FCompareIgnoreCase write FCompareIgnoreCase;
    { Main menu }
    [IniValue('Options', 'MainMenuFontName', 'Segoe UI')]
    property MainMenuFontName: string read FMainMenuFontName write FMainMenuFontName;
    [IniValue('Options', 'MainMenuFontSize', '8')]
    property MainMenuFontSize: Integer read FMainMenuFontSize write FMainMenuFontSize;
    property MainMenuSystemFontName: string read FMainMenuSystemFontName write FMainMenuSystemFontName;
    property MainMenuSystemFontSize: Integer read FMainMenuSystemFontSize write FMainMenuSystemFontSize;
    [IniValue('Options', 'MainMenuUseSystemFont', 'False')]
    property MainMenuUseSystemFont: Boolean read FMainMenuUseSystemFont write FMainMenuUseSystemFont;
    { Output }
    [IniValue('Options', 'OutputIndent', '20')]
    property OutputIndent: Integer read FOutputIndent write FOutputIndent;
    [IniValue('Options', 'OutputShowTreeLines', 'False')]
    property OutputShowTreeLines: Boolean read FOutputShowTreeLines write FOutputShowTreeLines;
    [IniValue('Options', 'OutputShowCheckBox', 'True')]
    property OutputShowCheckBox: Boolean read FOutputShowCheckBox write FOutputShowCheckBox;
    { Print }
    [IniValue('Options', 'PrintDateTime', '1')]
    property PrintDateTime: Integer read FPrintDateTime write FPrintDateTime;
    [IniValue('Options', 'PrintDocumentName', '2')]
    property PrintDocumentName: Integer read FPrintDocumentName write FPrintDocumentName;
    [IniValue('Options', 'PrintPageNumber', '3')]
    property PrintPageNumber: Integer read FPrintPageNumber write FPrintPageNumber;
    [IniValue('Options', 'PrintPrintedBy', '0')]
    property PrintPrintedBy: Integer read FPrintPrintedBy write FPrintPrintedBy;
    [IniValue('Options', 'PrintShowFooterLine', 'True')]
    property PrintShowFooterLine: Boolean read FPrintShowFooterLine write FPrintShowFooterLine;
    [IniValue('Options', 'PrintShowHeaderLine', 'True')]
    property PrintShowHeaderLine: Boolean read FPrintShowHeaderLine write FPrintShowHeaderLine;
    [IniValue('Options', 'PrintShowLineNumbers', 'False')]
    property PrintShowLineNumbers: Boolean read FPrintShowLineNumbers write FPrintShowLineNumbers;
    [IniValue('Options', 'PrintWordWrapLine', 'False')]
    property PrintWordWrapLine: Boolean read FPrintWordWrapLine write FPrintWordWrapLine;
    { Statusbar }
    [IniValue('Options', 'StatusBarFontName', 'Segoe UI')]
    property StatusBarFontName: string read FStatusBarFontName write FStatusBarFontName;
    [IniValue('Options', 'StatusBarFontSize', '8')]
    property StatusBarFontSize: Integer read FStatusBarFontSize write FStatusBarFontSize;
    [IniValue('Options', 'StatusBarUseSystemFont', 'False')]
    property StatusBarUseSystemFont: Boolean read FStatusBarUseSystemFont write FStatusBarUseSystemFont;
    [IniValue('Options', 'StatusBarShowMacro', 'True')]
    property StatusBarShowMacro: Boolean read FStatusBarShowMacro write FStatusBarShowMacro;
    [IniValue('Options', 'StatusBarShowCaretPosition', 'True')]
    property StatusBarShowCaretPosition: Boolean read FStatusBarShowCaretPosition write FStatusBarShowCaretPosition;
    [IniValue('Options', 'StatusBarShowKeyState', 'True')]
    property StatusBarShowKeyState: Boolean read FStatusBarShowKeyState write FStatusBarShowKeyState;
    [IniValue('Options', 'StatusBarShowModified', 'True')]
    property StatusBarShowModified: Boolean read FStatusBarShowModified write FStatusBarShowModified;
  end;

  {$ifdef ORABONE}
  TOraBoneOptionsContainer = class(TOptionsContainer)
  private
    FConnectionCloseTabByDblClick: Boolean;
    FConnectionCloseTabByMiddleClick: Boolean;
    FConnectionMultiLine: Boolean;
    FConnectionRightClickSelect: Boolean;
    FConnectionShowCloseButton: Boolean;
    FConnectionShowImage: Boolean;
    FDateFormat: string;
    FEditorCloseTabByDblClick: Boolean;
    FEditorCloseTabByMiddleClick: Boolean;
    FEditorMultiLine: Boolean;
    FEditorRightClickSelect: Boolean;
    FEditorShowCloseButton: Boolean;
    FEditorShowImage: Boolean;
    FFilterOnTyping: Boolean;
    FObjectFrameAlign: string;
    FOutputCloseTabByDblClick: Boolean;
    FOutputCloseTabByMiddleClick: Boolean;
    FOutputMultiLine: Boolean;
    FOutputRightClickSelect: Boolean;
    FOutputShowCloseButton: Boolean;
    FOutputShowImage: Boolean;
    FPollingInterval: Integer;
    FSchemaBrowserAlign: string;
    FSchemaBrowserIndent: Integer;
    FSchemaBrowserShowTreeLines: Boolean;
    FShowDataSearchPanel: Boolean;
    FShowObjectCreationAndModificationTimestamp: Boolean;
    FTimeFormat: string;
  public
    procedure WriteIniFile; override;
  published
    [IniValue('Options', 'ConnectionCloseTabByDblClick', 'False')]
    property ConnectionCloseTabByDblClick: Boolean read FConnectionCloseTabByDblClick write FConnectionCloseTabByDblClick;
    [IniValue('Options', 'ConnectionCloseTabByMiddleClick', 'False')]
    property ConnectionCloseTabByMiddleClick: Boolean read FConnectionCloseTabByMiddleClick write FConnectionCloseTabByMiddleClick;
    [IniValue('Options', 'ConnectionMultiLine', 'False')]
    property ConnectionMultiLine: Boolean read FConnectionMultiLine write FConnectionMultiLine;
    [IniValue('Options', 'ConnectionRightClickSelect', 'True')]
    property ConnectionRightClickSelect: Boolean read FConnectionRightClickSelect write FConnectionRightClickSelect;
    [IniValue('Options', 'ConnectionShowCloseButton', 'False')]
    property ConnectionShowCloseButton: Boolean read FConnectionShowCloseButton write FConnectionShowCloseButton;
    [IniValue('Options', 'ConnectionShowImage', 'True')]
    property ConnectionShowImage: Boolean read FConnectionShowImage write FConnectionShowImage;
    [IniValue('Options', 'DateFormat', 'DD.MM.YYYY')]
    property DateFormat: string read FDateFormat write FDateFormat;
    [IniValue('Options', 'EditorCloseTabByDblClick', 'False')]
    property EditorCloseTabByDblClick: Boolean read FEditorCloseTabByDblClick write FEditorCloseTabByDblClick;
    [IniValue('Options', 'EditorCloseTabByMiddleClick', 'False')]
    property EditorCloseTabByMiddleClick: Boolean read FEditorCloseTabByMiddleClick write FEditorCloseTabByMiddleClick;
    [IniValue('Options', 'EditorMultiLine', 'False')]
    property EditorMultiLine: Boolean read FEditorMultiLine write FEditorMultiLine;
    [IniValue('Options', 'EditorRightClickSelect', 'True')]
    property EditorRightClickSelect: Boolean read FEditorRightClickSelect write FEditorRightClickSelect;
    [IniValue('Options', 'EditorShowCloseButton', 'False')]
    property EditorShowCloseButton: Boolean read FEditorShowCloseButton write FEditorShowCloseButton;
    [IniValue('Options', 'EditorShowImage', 'True')]
    property EditorShowImage: Boolean read FEditorShowImage write FEditorShowImage;
    [IniValue('Options', 'FilterOnTyping', 'True')]
    property FilterOnTyping: Boolean read FFilterOnTyping write FFilterOnTyping;
    [IniValue('Options', 'ObjectFrameAlign', 'Bottom')]
    property ObjectFrameAlign: string read FObjectFrameAlign write FObjectFrameAlign;
    [IniValue('Options', 'OutputCloseTabByDblClick', 'False')]
    property OutputCloseTabByDblClick: Boolean read FOutputCloseTabByDblClick write FOutputCloseTabByDblClick;
    [IniValue('Options', 'OutputCloseTabByMiddleClick', 'False')]
    property OutputCloseTabByMiddleClick: Boolean read FOutputCloseTabByMiddleClick write FOutputCloseTabByMiddleClick;
    [IniValue('Options', 'OutputMultiLine', 'False')]
    property OutputMultiLine: Boolean read FOutputMultiLine write FOutputMultiLine;
    [IniValue('Options', 'OutputRightClickSelect', 'True')]
    property OutputRightClickSelect: Boolean read FOutputRightClickSelect write FOutputRightClickSelect;
    [IniValue('Options', 'OutputShowCloseButton', 'False')]
    property OutputShowCloseButton: Boolean read FOutputShowCloseButton write FOutputShowCloseButton;
    [IniValue('Options', 'OutputShowImage', 'True')]
    property OutputShowImage: Boolean read FOutputShowImage write FOutputShowImage;
    [IniValue('Options', 'PollingInterval', '1')]
    property PollingInterval: Integer read FPollingInterval write FPollingInterval;
    [IniValue('Options', 'SchemaBrowserAlign', 'Bottom')]
    property SchemaBrowserAlign: string read FSchemaBrowserAlign write FSchemaBrowserAlign;
    [IniValue('Options', 'SchemaBrowserIndent', '16')]
    property SchemaBrowserIndent: Integer read FSchemaBrowserIndent write FSchemaBrowserIndent;
    [IniValue('Options', 'SchemaBrowserShowTreeLines', 'False')]
    property SchemaBrowserShowTreeLines: Boolean read FSchemaBrowserShowTreeLines write FSchemaBrowserShowTreeLines;
    [IniValue('Options', 'ShowDataSearchPanel', 'True')]
    property ShowDataSearchPanel: Boolean read FShowDataSearchPanel write FShowDataSearchPanel;
    [IniValue('Options', 'ShowObjectCreationAndModificationTimestamp', 'False')]
    property ShowObjectCreationAndModificationTimestamp: Boolean read FShowObjectCreationAndModificationTimestamp write FShowObjectCreationAndModificationTimestamp;
    [IniValue('Options', 'TimeFormat', 'HH24:MI:SS')]
    property TimeFormat: string read FTimeFormat write FTimeFormat;
  end;

  function OptionsContainer: TOraBoneOptionsContainer;
  {$endif}

  {$ifdef EDITBONE}
  TEditBoneOptionsContainer = class(TOptionsContainer)
  private
    FDefaultColor: string;
    FHighlighterColorStrings: TStrings;
    FDefaultEncoding: Integer;
    FDefaultHighlighter: string;
    FDefaultHighlighterColor: string;
    FDefaultSQLHighlighter: string;
    FDefaultBrowser: string;
    FDirAutoHide: Boolean;
    FDirCloseTabByDblClick: Boolean;
    FDirCloseTabByMiddleClick: Boolean;
    FDirIndent: Integer;
    FDirMultiLine: Boolean;
    FDirRightClickSelect: Boolean;
    FDirSaveTabs: Boolean;
    FDirShowArchiveFiles: Boolean;
    FDirShowCloseButton: Boolean;
    FDirShowHiddenFiles: Boolean;
    FDirShowImage: Boolean;
    FDirShowOpenDirectoryButton: Boolean;
    FDirShowSystemFiles: Boolean;
    FDirShowTreeLines: Boolean;
    FDirShowOverlayIcons: Boolean;
    FDirAlign: Integer;
    FDocCloseTabByDblClick: Boolean;
    FDocCloseTabByMiddleClick: Boolean;
    FDocMultiLine: Boolean;
    FDocRightClickSelect: Boolean;
    FDocSaveTabs: Boolean;
    FDocShowCloseButton: Boolean;
    FDocShowNewDocumentButton: Boolean;
    FDocShowImage: Boolean;
    FFileTypes: TStrings;
    FHighlighterStrings: TStrings;
    FOutputCloseTabByDblClick: Boolean;
    FOutputCloseTabByMiddleClick: Boolean;
    FOutputMultiLine: Boolean;
    FOutputRightClickSelect: Boolean;
    FOutputSaveTabs: Boolean;
    FOutputShowCloseButton: Boolean;
    FOutputShowFindInFilesButton: Boolean;
    FOutputShowImage: Boolean;
    FShowXMLTree: Boolean;
    FSupportedFileExtensions: string;
    FToolbarVisible: Boolean;
    FToolbarIconSizeSmall: Boolean;
    FMenuBarVisible: Boolean;
    FMainMenuVisible: Boolean;
    { Skin colors }
    FSkinActiveLineBackground: Boolean;
    FSkinBackground: Boolean;
    FSkinBookmarkPanelBackground: Boolean;
    FSkinCodeFoldingBackground: Boolean;
    FSkinCodeFoldingHintBackground: Boolean;
    FSkinCompletionProposalBackground: Boolean;
    FSkinCompletionProposalSelectionBackground: Boolean;
    FSkinForeground: Boolean;
    FSkinLeftMarginBackground: Boolean;
    FSkinSelectionBackground: Boolean;
    FSkinSelectionForeground: Boolean;
    function GetExtensions: string;
    function GetFilterCount: Cardinal;
    function GetFilters: string;
  public
    destructor Destroy; override;
    //function FileType(FileType: TFileType): string;
    function GetFilterExt(FilterIndex: Cardinal): string;
    function GetFilterIndex(FileExt: string): Cardinal;
    function SupportedFileExtensions(Refresh: Boolean = False): string;
   // procedure AssignTo(Dest: TPersistent); override;
    procedure ReadIniFile; override;
  published
    { Defaults }
    [IniValue('Options', 'DefaultColor', 'Default')]
    property DefaultColor: string read FDefaultColor write FDefaultColor;
    [IniValue('Options', 'DefaultEncoding', '0')]
    property DefaultEncoding: Integer read FDefaultEncoding write FDefaultEncoding;
    [IniValue('Options', 'DefaultHighlighter', 'Text')]
    property DefaultHighlighter: string read FDefaultHighlighter write FDefaultHighlighter;
    [IniValue('Options', 'DefaultHighlighterColor', 'Default')]
    property DefaultHighlighterColor: string read FDefaultHighlighterColor write FDefaultHighlighterColor;
    [IniValue('Options', 'DefaultSQLHighlighter', 'SQL - Standard')]
    property DefaultSQLHighlighter: string read FDefaultSQLHighlighter write FDefaultSQLHighlighter;
    [IniValue('Options', 'DefaultBrowser', '')]
    property DefaultBrowser: string read FDefaultBrowser write FDefaultBrowser;
    { Strings }
    property HighlighterColorStrings: TStrings read FHighlighterColorStrings write FHighlighterColorStrings;
    property HighlighterStrings: TStrings read FHighlighterStrings write FHighlighterStrings;
    { Directory }
    [IniValue('Options', 'DirAutoHide', 'True')]
    property DirAutoHide: Boolean read FDirAutoHide write FDirAutoHide;
    [IniValue('Options', 'DirCloseTabByDblClick', 'False')]
    property DirCloseTabByDblClick: Boolean read FDirCloseTabByDblClick write FDirCloseTabByDblClick;
    [IniValue('Options', 'DirCloseTabByMiddleClick', 'False')]
    property DirCloseTabByMiddleClick: Boolean read FDirCloseTabByMiddleClick write FDirCloseTabByMiddleClick;
    [IniValue('Options', 'DirIndent', '20')]
    property DirIndent: Integer read FDirIndent write FDirIndent;
    [IniValue('Options', 'DirMultiLine', 'False')]
    property DirMultiLine: Boolean read FDirMultiLine write FDirMultiLine;
    [IniValue('Options', 'DirRightClickSelect', 'True')]
    property DirRightClickSelect: Boolean read FDirRightClickSelect write FDirRightClickSelect;
    [IniValue('Options', 'DirSaveTabs', 'True')]
    property DirSaveTabs: Boolean read FDirSaveTabs write FDirSaveTabs;
    [IniValue('Options', 'DirShowArchiveFiles', 'True')]
    property DirShowArchiveFiles: Boolean read FDirShowArchiveFiles write FDirShowArchiveFiles;
    [IniValue('Options', 'DirShowCloseButton', 'False')]
    property DirShowCloseButton: Boolean read FDirShowCloseButton write FDirShowCloseButton;
    [IniValue('Options', 'DirShowHiddenFiles', 'False')]
    property DirShowHiddenFiles: Boolean read FDirShowHiddenFiles write FDirShowHiddenFiles;
    [IniValue('Options', 'DirShowImage', 'True')]
    property DirShowImage: Boolean read FDirShowImage write FDirShowImage;
    [IniValue('Options', 'DirShowOpenDirectoryButton', 'True')]
    property DirShowOpenDirectoryButton: Boolean read FDirShowOpenDirectoryButton write FDirShowOpenDirectoryButton;
    [IniValue('Options', 'DirShowSystemFiles', 'False')]
    property DirShowSystemFiles: Boolean read FDirShowSystemFiles write FDirShowSystemFiles;
    [IniValue('Options', 'DirShowTreeLines', 'False')]
    property DirShowTreeLines: Boolean read FDirShowTreeLines write FDirShowTreeLines;
    [IniValue('Options', 'DirShowOverlayIcons', 'True')]
    property DirShowOverlayIcons: Boolean read FDirShowOverlayIcons write FDirShowOverlayIcons;
    [IniValue('Options', 'DirAlign', '0')]
    property DirAlign: Integer read FDirAlign write FDirAlign;
    { Document }
    [IniValue('Options', 'DocCloseTabByDblClick', 'False')]
    property DocCloseTabByDblClick: Boolean read FDocCloseTabByDblClick write FDocCloseTabByDblClick;
    [IniValue('Options', 'DocCloseTabByMiddleClick', 'False')]
    property DocCloseTabByMiddleClick: Boolean read FDocCloseTabByMiddleClick write FDocCloseTabByMiddleClick;
    [IniValue('Options', 'DocMultiLine', 'False')]
    property DocMultiLine: Boolean read FDocMultiLine write FDocMultiLine;
    [IniValue('Options', 'DocRightClickSelect', 'True')]
    property DocRightClickSelect: Boolean read FDocRightClickSelect write FDocRightClickSelect;
    [IniValue('Options', 'DocSaveTabs', 'True')]
    property DocSaveTabs: Boolean read FDocSaveTabs write FDocSaveTabs;
    [IniValue('Options', 'DocShowCloseButton', 'True')]
    property DocShowCloseButton: Boolean read FDocShowCloseButton write FDocShowCloseButton;
    [IniValue('Options', 'DocShowNewDocumentButton', 'True')]
    property DocShowNewDocumentButton: Boolean read FDocShowNewDocumentButton write FDocShowNewDocumentButton;
    [IniValue('Options', 'DocShowImage', 'True')]
    property DocShowImage: Boolean read FDocShowImage write FDocShowImage;
    { Output }
    [IniValue('Options', 'OutputCloseTabByDblClick', 'False')]
    property OutputCloseTabByDblClick: Boolean read FOutputCloseTabByDblClick write FOutputCloseTabByDblClick;
    [IniValue('Options', 'OutputCloseTabByMiddleClick', 'False')]
    property OutputCloseTabByMiddleClick: Boolean read FOutputCloseTabByMiddleClick write FOutputCloseTabByMiddleClick;
    [IniValue('Options', 'OutputMultiLine', 'False')]
    property OutputMultiLine: Boolean read FOutputMultiLine write FOutputMultiLine;
    [IniValue('Options', 'OutputRightClickSelect', 'True')]
    property OutputRightClickSelect: Boolean read FOutputRightClickSelect write FOutputRightClickSelect;
    [IniValue('Options', 'OutputSaveTabs', 'True')]
    property OutputSaveTabs: Boolean read FOutputSaveTabs write FOutputSaveTabs;
    [IniValue('Options', 'OutputShowCloseButton', 'True')]
    property OutputShowCloseButton: Boolean read FOutputShowCloseButton write FOutputShowCloseButton;
    [IniValue('Options', 'OutputShowFindInFilesButton', 'False')]
    property OutputShowFindInFilesButton: Boolean read FOutputShowFindInFilesButton write FOutputShowFindInFilesButton;
    [IniValue('Options', 'OutputShowImage', 'True')]
    property OutputShowImage: Boolean read FOutputShowImage write FOutputShowImage;
    { View }
    [IniValue('Options', 'ShowXMLTree', 'True')]
    property ShowXMLTree: Boolean read FShowXMLTree write FShowXMLTree;
    [IniValue('Options', 'ToolbarVisible', 'False')]
    property ToolbarVisible: Boolean read FToolbarVisible write FToolbarVisible;
    [IniValue('Options', 'ToolbarIconSizeSmall', 'True')]
    property ToolbarIconSizeSmall: Boolean read FToolbarIconSizeSmall write FToolbarIconSizeSmall;
    [IniValue('Options', 'MenuBarVisible', 'True')]
    property MenuBarVisible: Boolean read FMenuBarVisible write FMenuBarVisible;
    [IniValue('Options', 'MainMenuVisible', 'False')]
    property MainMenuVisible: Boolean read FMainMenuVisible write FMainMenuVisible;
    { Skin colors }
    [IniValue('Options', 'SkinActiveLineBackground', 'False')]
    property SkinActiveLineBackground: Boolean read FSkinActiveLineBackground write FSkinActiveLineBackground;
    [IniValue('Options', 'SkinBackground', 'False')]
    property SkinBackground: Boolean read FSkinBackground write FSkinBackground;
    [IniValue('Options', 'SkinBookmarkPanelBackground', 'False')]
    property SkinBookmarkPanelBackground: Boolean read FSkinBookmarkPanelBackground write FSkinBookmarkPanelBackground;
    [IniValue('Options', 'SkinCodeFoldingBackground', 'False')]
    property SkinCodeFoldingBackground: Boolean read FSkinCodeFoldingBackground write FSkinCodeFoldingBackground;
    [IniValue('Options', 'SkinCodeFoldingHintBackground', 'False')]
    property SkinCodeFoldingHintBackground: Boolean read FSkinCodeFoldingHintBackground write FSkinCodeFoldingHintBackground;
    [IniValue('Options', 'SkinCompletionProposalBackground', 'False')]
    property SkinCompletionProposalBackground: Boolean read FSkinCompletionProposalBackground write FSkinCompletionProposalBackground;
    [IniValue('Options', 'SkinCompletionProposalSelectionBackground', 'False')]
    property SkinCompletionProposalSelectionBackground: Boolean read FSkinCompletionProposalSelectionBackground write FSkinCompletionProposalSelectionBackground;
    [IniValue('Options', 'SkinForeground', 'False')]
    property SkinForeground: Boolean read FSkinForeground write FSkinForeground;
    [IniValue('Options', 'SkinLeftMarginBackground', 'False')]
    property SkinLeftMarginBackground: Boolean read FSkinLeftMarginBackground write FSkinLeftMarginBackground;
    [IniValue('Options', 'SkinSelectionBackground', 'False')]
    property SkinSelectionBackground: Boolean read FSkinSelectionBackground write FSkinSelectionBackground;
    [IniValue('Options', 'SkinSelectionForeground', 'False')]
    property SkinSelectionForeground: Boolean read FSkinSelectionForeground write FSkinSelectionForeground;
    { Other }
    property Extensions: string read GetExtensions;
    property FileTypes: TStrings read FFileTypes write FFileTypes;
    property FilterCount: Cardinal read GetFilterCount;
    property Filters: string read GetFilters;
  end;

  function OptionsContainer: TEditBoneOptionsContainer;
  {$endif}

implementation

uses
  System.SysUtils, Vcl.ComCtrls, Vcl.Graphics, Vcl.Menus, BCCommon.StringUtils, BCCommon.Language.Strings, BigIni,
  BCEditor.Editor, BCEditor.Types, BCControl.Utils, BCControl.Statusbar;

{$ifdef ORABONE}
var
  FOptionsContainer: TOraBoneOptionsContainer;

function OptionsContainer: TOraBoneOptionsContainer;
begin
  if not Assigned(FOptionsContainer) then
    FOptionsContainer := TOraBoneOptionsContainer.Create;
  Result := FOptionsContainer;
end;
{$endif}
{$ifdef EDITBONE}
var
  FOptionsContainer: TEditBoneOptionsContainer;

function OptionsContainer: TEditBoneOptionsContainer;
begin
  if not Assigned(FOptionsContainer) then
    FOptionsContainer := TEditBoneOptionsContainer.Create;
  Result := FOptionsContainer;
end;
{$endif}

procedure TOptionsContainer.ReadIniFile;
begin
  TIniPersist.Load(GetIniFilename, Self);

  with TBigIniFile.Create(GetIniFilename) do
  try
    { OS-dependent options }
    FMainMenuSystemFontName := ReadString('Options', 'MainMenuSystemFontName', Screen.MenuFont.Name);
    FMainMenuSystemFontSize := StrToInt(ReadString('Options', 'MainMenuSystemFontSize', IntToStr(Screen.MenuFont.Size)));
  finally
    Free;
  end;
end;

procedure TOptionsContainer.WriteIniFile;
begin
  TIniPersist.Save(GetIniFilename, Self);
end;

procedure TOptionsContainer.AssignTo(Dest: TPersistent);
begin
  if Assigned(Dest) and (Dest is TBCEditor) then
  with Dest as TBCEditor do
  begin
    { Caret }
    Caret.Visible := FShowCaret;
    if FRightMouseClickMovesCaret then
      Caret.Options := Caret.Options + [coRightMouseClickMovesCaret]
    else
      Caret.Options := Caret.Options - [coRightMouseClickMovesCaret];
    Caret.NonBlinking.Enabled := FShowNonblinkingCaret;
    Caret.NonBlinking.Colors.Background := StringToColor(FNonblinkingCaretBackgroundColor);
    Caret.NonBlinking.Colors.Foreground := StringToColor(FNonblinkingCaretForegroundColor);
    Caret.Styles.Insert := TBCEditorCaretStyle(FInsertCaret);
    Caret.Styles.Overwrite := TBCEditorCaretStyle(FOverwriteCaret);
    { Code folding }
    if Highlighter.CodeFoldingRangeCount > 0 then
    begin
      CodeFolding.Visible := FShowCodeFolding;
      if FFoldMultilineComments then
        CodeFolding.Options := CodeFolding.Options + [cfoFoldMultilineComments]
      else
        CodeFolding.Options := CodeFolding.Options - [cfoFoldMultilineComments];
      if FHighlightIndentGuides then
        CodeFolding.Options := CodeFolding.Options + [cfoHighlightIndentGuides]
      else
        CodeFolding.Options := CodeFolding.Options - [cfoHighlightIndentGuides];
      if FHighlightMatchingPair then
        CodeFolding.Options := CodeFolding.Options + [cfoHighlightMatchingPair]
      else
        CodeFolding.Options := CodeFolding.Options - [cfoHighlightMatchingPair];
      if FShowCollapsedCodeHint then
        CodeFolding.Options := CodeFolding.Options + [cfoShowCollapsedCodeHint]
      else
        CodeFolding.Options := CodeFolding.Options - [cfoShowCollapsedCodeHint];
      if FShowCollapsedLine then
        CodeFolding.Options := CodeFolding.Options + [cfoShowCollapsedLine]
      else
        CodeFolding.Options := CodeFolding.Options - [cfoShowCollapsedLine];
      if FShowIndentGuides then
        CodeFolding.Options := CodeFolding.Options + [cfoShowIndentGuides]
      else
        CodeFolding.Options := CodeFolding.Options - [cfoShowIndentGuides];
      if FUncollapseByHintClick then
        CodeFolding.Options := CodeFolding.Options + [cfoUncollapseByHintClick]
      else
        CodeFolding.Options := CodeFolding.Options - [cfoUncollapseByHintClick];
      CodeFolding.MarkStyle := TBCEditorCodeFoldingMarkStyle(FCodeFoldingMarkStyle);
      CodeFolding.Hint.RowCount := FCodeFoldingHintRowCount;
    end;
    { Completion proposal }
    if not FCompletionProposalEnabled then
      CompletionProposal.ShortCut := TextToShortCut('')
    else
      CompletionProposal.ShortCut := TextToShortCut(FCompletionProposalShortcut);
    if FCompletionProposalCaseSensitive then
      CompletionProposal.Options := CompletionProposal.Options + [cpoCaseSensitive]
    else
      CompletionProposal.Options := CompletionProposal.Options - [cpoCaseSensitive];
    if FCompletionProposalAutoInvoke then
      CompletionProposal.Options := CompletionProposal.Options + [cpoAutoInvoke]
    else
      CompletionProposal.Options := CompletionProposal.Options - [cpoAutoInvoke];
    { Left margin }
    LeftMargin.Visible := FLeftMarginVisible;
    LeftMargin.Autosize := FLeftMarginAutosize;
    LeftMargin.Bookmarks.Visible := FLeftMarginShowBookmarks;
    LeftMargin.Bookmarks.Panel.Visible := FLeftMarginShowBookmarkPanel;
    LeftMargin.LineState.Enabled := FLeftMarginShowLineState;
    LeftMargin.LineNumbers.Visible := FEnableLineNumbers;
    if FLeftMarginLineNumbersShowInTens then
      LeftMargin.LineNumbers.Options := LeftMargin.LineNumbers.Options + [lnoIntens]
    else
      LeftMargin.LineNumbers.Options := LeftMargin.LineNumbers.Options - [lnoIntens];
    if FLeftMarginLineNumbersShowLeadingZeros then
      LeftMargin.LineNumbers.Options := LeftMargin.LineNumbers.Options + [lnoLeadingZeros]
    else
      LeftMargin.LineNumbers.Options := LeftMargin.LineNumbers.Options - [lnoLeadingZeros];
    if FLeftMarginLineNumbersShowAfterLastLine then
      LeftMargin.LineNumbers.Options := LeftMargin.LineNumbers.Options + [lnoAfterLastLine]
    else
      LeftMargin.LineNumbers.Options := LeftMargin.LineNumbers.Options - [lnoAfterLastLine];
    LeftMargin.LineNumbers.StartFrom := FLeftMarginLineNumbersStartFrom;
    LeftMargin.Width := FLeftMarginWidth;
    LeftMargin.Bookmarks.Panel.Width := FLeftMarginBookmarkPanelWidth;
    { Matching pair }
    MatchingPair.Enabled := FMatchingPairEnabled;
    if FMatchingPairHighlightAfterToken then
      MatchingPair.Options := MatchingPair.Options + [mpoHighlightAfterToken]
    else
      MatchingPair.Options := MatchingPair.Options - [mpoHighlightAfterToken];
    if FMatchingPairHighlightUnmatched then
      MatchingPair.Options := MatchingPair.Options + [mpoHighlightUnmatched]
    else
      MatchingPair.Options := MatchingPair.Options - [mpoHighlightUnmatched];
    { Minimap }
    Minimap.Visible := Minimap.Visible or FMinimapVisible;
    if FMinimapShowBookmarks then
      Minimap.Options := Minimap.Options + [moShowBookmarks]
    else
      Minimap.Options := Minimap.Options - [moShowBookmarks];
    if FMinimapShowIndentGuides then
      Minimap.Options := Minimap.Options + [moShowIndentGuides]
    else
      Minimap.Options := Minimap.Options - [moShowIndentGuides];
    Minimap.Width := FMinimapWidth;
    Minimap.Align := TBCEditorMinimapAlign(FMinimapAlign);
    { Editor }
    if FAutoIndent then
      Options := Options + [eoAutoIndent]
    else
      Options := Options - [eoAutoIndent];
    if FDragDropEditing then
      Options := Options + [eoDragDropEditing]
    else
      Options := Options - [eoDragDropEditing];
    if FDropFiles then
      Options := Options + [eoDropFiles]
    else
      Options := Options - [eoDropFiles];
    if FGroupUndo then
      Undo.Options := Undo.Options + [uoGroupUndo]
    else
      Undo.Options := Undo.Options - [uoGroupUndo];
     if FTrimTrailingSpaces then
      Options := Options + [eoTrimTrailingSpaces]
    else
      Options := Options - [eoTrimTrailingSpaces];
    LineSpacing.Spacing := FLineSpacing;
    { Right margin }
    RightMargin.Visible := FRightMarginVisible;
    if FRightMarginMouseMove then
      RightMargin.Options := RightMargin.Options + [rmoMouseMove]
    else
      RightMargin.Options := RightMargin.Options - [rmoMouseMove];
    if FRightMarginShowMovingHint then
      RightMargin.Options := RightMargin.Options + [rmoShowMovingHint]
    else
      RightMargin.Options := RightMargin.Options - [rmoShowMovingHint];
    RightMargin.Position := FRightMarginPosition;
    { Scroll }
    if FScrollAutosizeMaxWidth then
      Scroll.Options := Scroll.Options + [soAutosizeMaxWidth]
    else
      Scroll.Options := Scroll.Options - [soAutosizeMaxWidth];
    if FScrollHalfPage then
      Scroll.Options := Scroll.Options + [soHalfPage]
    else
      Scroll.Options := Scroll.Options - [soHalfPage];
    if FScrollHintFollows then
      Scroll.Options := Scroll.Options + [soHintFollows]
    else
      Scroll.Options := Scroll.Options - [soHintFollows];
    if FScrollPastEndOfFile then
      Scroll.Options := Scroll.Options + [soPastEndOfFileMarker]
    else
      Scroll.Options := Scroll.Options - [soPastEndOfFileMarker];
    if FScrollPastEndOfLineMarker then
      Scroll.Options := Scroll.Options + [soPastEndOfLine]
    else
      Scroll.Options := Scroll.Options - [soPastEndOfLine];
    if FScrollShowHint then
      Scroll.Options := Scroll.Options + [soShowHint]
    else
      Scroll.Options := Scroll.Options - [soShowHint];
    { Search }
    Search.Map.Visible := FShowSearchMap;
    { Selection }
    Selection.Visible := FSelectionVisible;
    if FALTSetsColumnMode then
      Selection.Options := Selection.Options + [soALTSetsColumnMode]
    else
      Selection.Options := Selection.Options - [soALTSetsColumnMode];
    if FSelectionFromEndOfLine then
      Selection.Options := Selection.Options + [soFromEndOfLine]
    else
      Selection.Options := Selection.Options - [soFromEndOfLine];
    if FSelectionToEndOfLine then
      Selection.Options := Selection.Options + [soToEndOfLine]
    else
      Selection.Options := Selection.Options - [soToEndOfLine];
    if FSelectionToEndOfLastLine then
      Selection.Options := Selection.Options + [soToEndOfLastLine]
    else
      Selection.Options := Selection.Options - [soToEndOfLastLine];
    if FHighlightSimilarTerms then
      Selection.Options := Selection.Options + [soHighlightSimilarTerms]
    else
      Selection.Options := Selection.Options - [soHighlightSimilarTerms];
    if FTripleClickRowSelect then
      Selection.Options := Selection.Options + [soTripleClickRowSelect]
    else
      Selection.Options := Selection.Options - [soTripleClickRowSelect];
    if FEnableSelectionMode then
      Selection.Mode := smColumn
    else
      Selection.Mode := smNormal;
    { Special chars }
    SpecialChars.Visible := FEnableSpecialChars;
    if FSpecialCharsUseTextColor then
      SpecialChars.Options := SpecialChars.Options + [scoUseTextColor]
    else
      SpecialChars.Options := SpecialChars.Options - [scoUseTextColor];
    SpecialChars.Style := TBCEditorSpecialCharsStyle(FSpecialCharsStyle);
    SpecialChars.EndOfLine.Visible := FSpecialEndOfLineVisible;
    SpecialChars.EndOfLine.Color := StringToColor(FSpecialEndOfLineColor);
    SpecialChars.EndOfLine.Style := TBCEditorSpecialCharsEndOfLineStyle(FSpecialCharsEndOfLineStyle);
    SpecialChars.Selection.Visible := FSpecialCharsSelectionVisible;
    SpecialChars.Selection.Color := StringToColor(FSpecialSelectionColor);
    { Tabs }
    if FSelectedBlockIndent then
      Tabs.Options := Tabs.Options + [toSelectedBlockIndent]
    else
      Tabs.Options := Tabs.Options - [toSelectedBlockIndent];
    if FTabsToSpaces then
      Tabs.Options := Tabs.Options + [toTabsToSpaces]
    else
      Tabs.Options := Tabs.Options - [toTabsToSpaces];
    TabWidth := FTabWidth;
    { Word wrap }
    WordWrap.Enabled := FEnableWordWrap;
  end
  else
  if Assigned(Dest) and (Dest is TBCStatusBar) then
  begin
    TBCStatusBar(Dest).UseSystemFont := FStatusBarUseSystemFont;
    if not FStatusBarUseSystemFont then
    begin
      TBCStatusBar(Dest).Font.Name := FStatusBarFontName;
      TBCStatusBar(Dest).Font.Size := FStatusBarFontSize;
      TBCStatusBar(Dest).Height := FStatusBarFontSize + 14;
    end;
  end
  else
    inherited;
end;

{ TEditBoneOptionsContainer }

{$ifdef EDITBONE}
destructor TEditBoneOptionsContainer.Destroy;
begin
  if Assigned(FHighlighterColorStrings) then
    FHighlighterColorStrings.Free;
  if Assigned(FHighlighterStrings) then
    FHighlighterStrings.Free;
  if Assigned(FileTypes) then
    FileTypes.Free;
  inherited;
end;

procedure TEditBoneOptionsContainer.ReadIniFile;
var
  i, j: Integer;
  LFileTypes: TStrings;
begin
  inherited;

  FFileTypes := TStringList.Create;
  for i := 0 to LanguageDataModule.MultiStringHolderFileTypes.MultipleStrings.Count - 1 - 2 do // TODO -2 Macro and Language...
    FFileTypes.Add(LanguageDataModule.MultiStringHolderFileTypes.MultipleStrings.Items[i].Name + '=' +
      LanguageDataModule.MultiStringHolderFileTypes.MultipleStrings.Items[i].Strings.Text);

  LFileTypes := TStringList.Create;
  with TBigIniFile.Create(GetIniFilename) do
  try
    ReadSectionValues('FileTypes', LFileTypes);
    for i := 0 to LFileTypes.Count - 1 do
    begin
      j := LFileTypes.IndexOfName(FFileTypes.Names[i]);
      if j <> -1 then
        FFileTypes.ValueFromIndex[i] := LFileTypes.ValueFromIndex[j];
    end;
  finally
    LFileTypes.Free;
    Free;
  end;
end;

function TEditBoneOptionsContainer.GetFilterCount: Cardinal;
begin
  Result := FFileTypes.Count;
end;

function TEditBoneOptionsContainer.GetFilterExt(FilterIndex: Cardinal): string;
begin
  { -2 because filter index is not 0-based and there's all files (in save dialog) first }
  Result := StringBetween(FFileTypes.Strings[FilterIndex - 2], '(', ')');
  Result := StringReplace(Result, '*', '', []);
  if Pos(';', Result) <> 0 then
    Result := Copy(Result, 1, Pos(';', Result) - 1);
end;

function TEditBoneOptionsContainer.GetFilterIndex(FileExt: string): Cardinal;
var
  i: Integer;
begin
  Result := 1;
  for i := 0 to FFileTypes.Count - 1 do
    if IsExtInFileType(FileExt, FFileTypes.Strings[i]) then
    begin
      Result := i + 2;
      Break;
    end;
end;

function TEditBoneOptionsContainer.GetFilters: string;
var
  i: Integer;
begin
  Result := Format('%s'#0'*.*'#0, [LanguageDataModule.GetConstant('AllFiles')]);
  i := 0;
  while i < FFileTypes.Count do
  begin
    Result := Format('%s%s'#0'%s', [Result, LanguageDataModule.MultiStringHolderFileTypes.MultipleStrings.Items[i].Strings.Text,
      StringBetween(FFileTypes.Strings[i], '(', ')')]);
    Inc(i);
    if i < FFileTypes.Count then
      Result := Format('%s'#0, [Result]);
  end;
  Result := Format('%s'#0#0, [Result]);
end;

function TEditBoneOptionsContainer.GetExtensions: string;
var
  i: Integer;
begin
  Result := '*.*|';
  for i := 0 to FFileTypes.Count - 1 do
    Result := Format('%s%s|', [Result, StringBetween(FFileTypes.Strings[i], '(', ')')]);
end;

function TEditBoneOptionsContainer.SupportedFileExtensions(Refresh: Boolean): string;
var
  i: Integer;
begin
  if (FSupportedFileExtensions = '') or Refresh then
    for i := 0 to FFileTypes.Count - 1 do
      FSupportedFileExtensions := Format('%s%s;', [FSupportedFileExtensions, StringBetween(FFileTypes.Strings[i], '(', ')')]);
  Result := FSupportedFileExtensions;
end;

{$endif}

end.
