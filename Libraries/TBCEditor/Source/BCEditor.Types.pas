unit BCEditor.Types;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Forms, Vcl.Graphics, Vcl.Controls, BCEditor.Highlighter.Attributes,
  BCEditor.Consts, System.SysUtils;

type
  TBCEditorArrayOfString = array of string;
  TBCEditorArrayOfSingle = array of Single;

  TBCEditorCharMethod = function(AChar: Char): Boolean of object;

  TBCEditorCaretStyle = (csVerticalLine, csThinVerticalLine, csHorizontalLine, csThinHorizontalLine, csHalfBlock, csBlock);

  TBCEditorCompletionProposalEvent = procedure(Sender: TObject; AItems: TStrings; const AInput: string) of object;

  TBCEditorDropFilesEvent = procedure(ASender: TObject; APos: TPoint; AFiles: TStrings) of object;

  TBCEditorPaintEvent = procedure(ASender: TObject; ACanvas: TCanvas) of object;

  TBCEditorReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  TBCEditorReplaceTextEvent = procedure(ASender: TObject; const ASearch, AReplace: string; ALine, AColumn: Integer;
    ADeleteLine: Boolean; var AAction: TBCEditorReplaceAction) of object;

  TBCEditorScrollEvent = procedure(ASender: TObject; AScrollBar: TScrollBarKind) of object;

  TBCEditorCaretChangedEvent = procedure(ASender: TObject; X, Y: Integer) of object;

  TBCEditorBookmarkPanelPaintEvent = procedure(ASender: TObject; ACanvas: TCanvas; ARect: TRect; AFirstLine: Integer; ALastLine: Integer) of object;
  TBCEditorBookmarkPanelLinePaintEvent = procedure(ASender: TObject; ACanvas: TCanvas; ARect: TRect; ALineNumber: Integer) of object;

  TBCEditorLinePaintEvent = procedure(ASender: TObject; ACanvas: TCanvas; ARect: TRect; ALineNumber: Integer; const AIsMinimapLine: Boolean) of object;

  TBCEditorCustomLineColorsEvent = procedure(ASender: TObject; ALine: Integer; var AUseColors: Boolean;
    var AForeground: TColor; var ABackground: TColor) of object;

  TBCEditorCustomTokenAttributeEvent = procedure(ASender: TObject; const AText: string; const ALine: Integer;
    const APosition: Integer; var AForegroundColor: TColor; var ABackgroundColor: TColor; var AStyles: TFontStyles) of object;

  TBCEditorCreateFileStreamEvent = procedure(ASender: TObject; const AFileName: string; var AStream: TStream) of object;

  TBCEditorStateFlag = (sfCaretChanged, sfScrollBarChanged, sfLinesChanging, sfIgnoreNextChar, sfCaretVisible, sfDblClicked,
    sfWaitForDragging, sfCodeFoldingInfoClicked, sfInSelection, sfDragging);
  TBCEditorStateFlags = set of TBCEditorStateFlag;

  TBCEditorOption = (
    eoAutoIndent, { Will indent the caret on new lines with the same amount of leading white space as the preceding line }
    eoDragDropEditing, { Allows you to select a block of text and drag it within the document to another location }
    eoDropFiles, { Allows the editor accept OLE file drops }
    eoTrimTrailingSpaces { Spaces at the end of lines will be trimmed and not saved }
  );
  TBCEditorOptions = set of TBCEditorOption;

  TBCEditorCaretOption = (
    coRightMouseClickMove { When clicking with the right mouse for a popup menu, move the cursor to that location }
  );
  TBCEditorCaretOptions = set of TBCEditorCaretOption;

  TBCEditorCaretMultiEditOption = (
    meoShowActiveLine,
    meoShowGhost { Ghost caret follows mouse cursor when moved }
  );
  TBCEditorCaretMultiEditOptions = set of TBCEditorCaretMultiEditOption;

  TBCEditorScrollOption = (
    soAutosizeMaxWidth, { Automatically resizes the MaxScrollWidth property when inserting text }
    soHalfPage, { When scrolling with page-up and page-down commands, only scroll a half page at a time }
    soHintFollows, { The scroll hint follows the mouse when scrolling vertically }
    soPastEndOfFileMarker, { Allows the cursor to go past the end of file marker }
    soPastEndOfLine, { Allows the cursor to go past the last character into the white space at the end of a line }
    soShowHint, { Shows a hint of the visible line numbers when scrolling vertically }
    soWheelClickMove { Scrolling by mouse move after wheel click. }
  );
  TBCEditorScrollOptions = set of TBCEditorScrollOption;

  TBCEditorTabOption = (
    toColumns,
    toPreviousLineIndent,
    toSelectedBlockIndent,
    toTabsToSpaces
    );
  TBCEditorTabOptions = set of TBCEditorTabOption;

  PBCEditorSelectionMode = ^TBCEditorSelectionMode;
  TBCEditorSelectionMode = (
    smColumn,
    smNormal
  );

  TBCEditorSelectionOption = (
    soALTSetsColumnMode,
    soExpandRealNumbers,
    soFromEndOfLine,
    soHighlightSimilarTerms,
    soTermsCaseSensitive,
    soToEndOfLastLine,
    soToEndOfLine,
    soTripleClickRowSelect
  );
  TBCEditorSelectionOptions = set of TBCEditorSelectionOption;

  TBCEditorSearchChanges = (
    scRefresh,
    scSearch,
    scEngineUpdate
  );
  TBCEditorSearchChangeEvent = procedure(Event: TBCEditorSearchChanges) of object;

  TBCEditorSearchOption = (
    soBackwards,
    soBeepIfStringNotFound,
    soCaseSensitive,
    soEntireScope,
    soHighlightResults,
    soSearchOnTyping,
    soSelectedOnly,
    soShowStringNotFound,
    soShowSearchMatchNotFound,
    soWholeWordsOnly,
    soWrapAround
  );
  TBCEditorSearchOptions = set of TBCEditorSearchOption;

  TBCEditorSyncEditOption = (
    seCaseSensitive
  );
  TBCEditorSyncEditOptions = set of TBCEditorSyncEditOption;

  TBCEditorReplaceOption = (
    roBackwards,
    roCaseSensitive,
    roEntireScope,
    roPrompt,
    roReplaceAll,
    roSelectedOnly,
    roWholeWordsOnly
  );
  TBCEditorReplaceOptions = set of TBCEditorReplaceOption;

  TBCEditorReplaceActionOption = (
    eraReplace,
    eraDeleteLine
  );

  TBCEditorSearchEngine = (
    seNormal,
    seRegularExpression,
    seWildCard
  );

  TBCEditorSearchMapOption = (
    moShowActiveLine
  );
  TBCEditorSearchMapOptions = set of TBCEditorSearchMapOption;

  TBCEditorCompletionProposalOption = (
    cpoAutoInvoke,
    cpoCaseSensitive,
    cpoFiltered,
    cpoParseItemsFromText
  );
  TBCEditorCompletionProposalOptions = set of TBCEditorCompletionProposalOption;

  TBCEditorLeftMarginBookMarkPanelOption = (
    bpoToggleBookmarkByClick
  );
  TBCEditorLeftMarginBookMarkPanelOptions = set of TBCEditorLeftMarginBookMarkPanelOption;

  TBCEditorRightMarginOption = (
    rmoMouseMove,
    rmoShowMovingHint
  );
  TBCEditorRightMarginOptions = set of TBCEditorRightMarginOption;

  TBCEditorTextPosition = record
    Char: Integer;
    Line: Integer;
  end;
  PBCEditorTextPosition = ^TBCEditorTextPosition;

  TBCEditorDisplayPosition = record
    Column: Integer;
    Row: Integer;
  end;
  PBCEditorDisplayPosition = ^TBCEditorDisplayPosition;

  TBCEditorMatchingPairTokenMatch = record
    Position: TBCEditorTextPosition;
    Token: string;
  end;

  TBCEditorBreakType = (
    btUnspecified,
    btAny,
    btTerm
  );
  TBCEditorRangeType = (
    ttUnspecified,
    ttAddress,
    ttAssemblerComment,
    ttAssemblerReservedWord,
    ttAttribute,
    ttBlockComment,
    ttCharacter,
    ttDirective,
    ttHexNumber,
    ttHighlightedBlock,
    ttHighlightedBlockSymbol,
    ttLineComment,
    ttMailtoLink,
    ttMethod,
    ttMethodName,
    ttNumber,
    ttReservedWord,
    ttString,
    ttSymbol,
    ttWebLink
  );

  TBCEditorMatchingPairToken = record
    OpenToken: string;
    CloseToken: string;
  end;
  PBCEditorMatchingPairToken = ^TBCEditorMatchingPairToken;

  TBCEditorMatchingTokenResult = (
    trCloseAndOpenTokenFound,
    trCloseTokenFound,
    trNotFound,
    trOpenTokenFound,
    trOpenAndCloseTokenFound
  );

  TBCEditorMatchingPairMatch = record
    OpenToken: string;
    CloseToken: string;
    OpenTokenPos: TBCEditorTextPosition;
    CloseTokenPos: TBCEditorTextPosition;
    TokenAttribute: TBCEditorHighlighterAttribute;
  end;

  TBCEditorKeyPressWEvent = procedure(ASender: TObject; var AKey: Char) of object;

  TBCEditorContextHelpEvent = procedure(ASender: TObject; AWord: string) of object;

  TBCEditorMouseCursorEvent = procedure(ASender: TObject; const ALineCharPos: TBCEditorTextPosition; var ACursor: TCursor) of object;

  TBCEditorEmptySpace = (
    esNone,
    esSpace,
    esTab
  );

  TBCEditorTokenHelper = record
    Background: TColor;
    CharsBefore: Integer;
    EmptySpace: TBCEditorEmptySpace;
    FontStyle: TFontStyles;
    Foreground: TColor;
    IsItalic: Boolean;
    Length: Integer;
    MatchingPairUnderline: Boolean;
    MaxLength: Integer;
    Text: string;
  end;

  TBCEditorSpecialCharsEndOfLineStyle = (
    eolArrow,
    eolEnter,
    eolPilcrow
  );

  TBCEditorSpecialCharsOption = (
    scoTextColor,
    scoMiddleColor
  );
  TBCEditorSpecialCharsOptions = set of TBCEditorSpecialCharsOption;
  TBCEditorSpecialCharsStyle = (scsDot, scsSolid);

  TBCEditorTabConvertProc = function(const ALine: string; ATabWidth: Integer; var AHasTabs: Boolean;
    const ATabChar: Char = BCEDITOR_SPACE_CHAR): string;

  TBCEditorLeftMarginLineNumberOption = (
    lnoIntens,
    lnoLeadingZeros,
    lnoAfterLastLine
  );
  TBCEditorLeftMarginLineNumberOptions = set of TBCEditorLeftMarginLineNumberOption;

  TBCEditorMatchingPairOption = (
    mpoHighlightAfterToken,
    mpoHighlightUnmatched,
    mpoUnderline,
    mpoUseMatchedColor
  );
  TBCEditorMatchingPairOptions = set of TBCEditorMatchingPairOption;

  TBCEditorMinimapOption = (
    moShowBookmarks,
    moShowIndentGuides,
    moShowSearchResults,
    moShowSpecialChars
  );
  TBCEditorMinimapOptions = set of TBCEditorMinimapOption;

  TBCEditorMinimapAlign = (maLeft, maRight);
  TBCEditorSearchMapAlign = (saLeft, saRight);

  TBCEditorUndoOption = (
    uoGroupUndo,
    uoUndoAfterSave
  );
  TBCEditorUndoOptions = set of TBCEditorUndoOption;

  TBCEditorCase = (cNone=-1, cUpper=0, cLower=1, cAlternating=2, cSentence=3, cTitle=4, cOriginal=5);

  TBCEditorKeyCharType = (ctFoldOpen, ctFoldClose, ctSkipOpen, ctSkipClose);

  TBCEditorSortOrder = (soToggle, soAsc, soDesc);

  TBCEditorChangeReason = (crInsert, crPaste, crDragDropInsert, crDelete, crLineBreak, crIndent, crUnindent,
    crCaret, crSelection, crNothing, crGroupBreak);

  TBCEditorWordWrapStyle = (wwsClientWidth, wwsRightMargin, wwsSpecified);

  TBCEditorCodeFoldingMarkStyle = (msSquare, msCircle);
  TBCEditorCodeFoldingChanges = (fcEnabled, fcRefresh, fcRescan);

  TBCEditorCodeFoldingChangeEvent = procedure(Event: TBCEditorCodeFoldingChanges) of object;

  TBCEditorCodeFoldingOption = (
    cfoFoldMultilineComments,
    cfoHighlightFoldingLine,
    cfoHighlightIndentGuides,
    cfoHighlightMatchingPair,
    cfoShowCollapsedCodeHint,
    cfoShowCollapsedLine,
    cfoShowIndentGuides,
    cfoUncollapseByHintClick
  );
  TBCEditorCodeFoldingOptions = set of TBCEditorCodeFoldingOption;

  TBCEditorLeftMarginBorderStyle = (mbsNone, mbsMiddle, mbsRight);

  TBCEditorScrollHintFormat = (shfTopLineOnly, shfTopToBottom);

  TBCEditorIndicatorOption = (ioInvertBlending, ioShowBorder, ioUseBlending);
  TBCEditorIndicatorOptions = set of TBCEditorIndicatorOption;

  TBCEditorQuadColor = packed record
  case Boolean of
    True: (Blue, Green, Red, Alpha: Byte);
    False: (Quad: Cardinal);
  end;
  PBCEditorQuadColor = ^TBCEditorQuadColor;

implementation

end.
