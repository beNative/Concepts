unit BCEditor.Consts;

interface

uses
  Vcl.Graphics;

type
  TBCEditorCharSet = set of AnsiChar;

const
  BCEDITOR_VERSION = '2.0.0 (dev)';
  BCEDITOR_BOOKMARK_IMAGE_COUNT = 10;
  BCEDITOR_CLIPBOARD_MAX_RETRIES = 5;
  BCEDITOR_CLIPBOARD_DELAY_STEP_MS = 200;
  BCEDITOR_WHEEL_DIVISOR = 120;
  BCEDITOR_TOKEN_MAX_LENGTH = 128;
  BCEDITOR_MAX_SCROLL_RANGE = High(Smallint);
  BCEDITOR_MAILTO = 'mailto:';
  BCEDITOR_HTTP = 'http://';
  { Max values }
  BCEDITOR_MAX_BOOKMARKS = 9;
  { Characters }
  BCEDITOR_UNDERSCORE = '_';
  BCEDITOR_CODE_FOLDING_VALID_CHARACTERS = ['\', '@', '_'];
  BCEDITOR_REAL_NUMBER_CHARS = ['e', 'E', '.'];
  BCEDITOR_NONE_CHAR = #0;
  BCEDITOR_BACKSPACE_CHAR = #8;
  BCEDITOR_TAB_CHAR = #9;
  BCEDITOR_LINEFEED = #10;
  BCEDITOR_CARRIAGE_RETURN = #13;
  BCEDITOR_CARRIAGE_RETURN_KEY = 13;
  BCEDITOR_SUBSTITUTE_CHAR = #26;
  BCEDITOR_ESCAPE = #27;
  BCEDITOR_ESCAPE_KEY = 27;
  BCEDITOR_SPACE_CHAR = #32;
  BCEDITOR_EXCLAMATION_MARK = #33;
  BCEDITOR_LOW_LINE = #95;
  BCEDITOR_CTRL_BACKSPACE = #127;
  BCEDITOR_PILCROW_CHAR = Char($00B6);
  BCEDITOR_LINE_SEPARATOR = Char($2028);
  BCEDITOR_WORD_BREAK_CHARACTERS = ['.', ',', ';', ':', '"', '''', '!', '?', '[', ']', '(', ')', '{', '}', '^',
    '=', '+', '-', '*', '/', '\', '|', ' '];
  BCEDITOR_EXTRA_WORD_BREAK_CHARACTERS = ['´', '`', '°', '&', '$', '@', '§', '%', '#', '~', '<', '>'];
  BCEDITOR_DEFAULT_DELIMITERS: TBCEditorCharSet = ['*', '/', '+', '-', '=', '\', '|', '&', '(', ')', '[', ']', '{', '}',
    '`', '~', '!', '@', ',', '$', '%', '^', '?', ':', ';', '''', '"', '.', '>', '<', '#'];
  BCEDITOR_ABSOLUTE_DELIMITERS: TBCEditorCharSet = [BCEDITOR_NONE_CHAR, BCEDITOR_TAB_CHAR, BCEDITOR_LINEFEED,
    BCEDITOR_CARRIAGE_RETURN, BCEDITOR_SPACE_CHAR, BCEDITOR_SUBSTITUTE_CHAR];
  { Encoding }
  BCEDITOR_UTF8BOM: array [0 .. 2] of Byte = ($EF, $BB, $BF);
  { Highlighter attribute elements }
  BCEDITOR_ATTRIBUTE_ELEMENT_COMMENT = 'Comment';
  BCEDITOR_ATTRIBUTE_ELEMENT_STRING = 'String';
  { Default colors }
  clSelectionColor = $00A56D53;
  clSearchHighlighter = $0078AAFF;
  clSearchInSelectionBackground = $00FCFDCD;
  clActiveLineBackground = $00E6FAFF;
  clLeftMarginBackground = $00FFFFFF;
  clLeftMarginFontForeground = $00CC9999;
  clSearchMapActiveLine = $00F4F4F4;
  clIndentHighlight = $00CC9999;
  clIndent = $00CC9999;
  clMatchingPairUnderline = $00CC9999;
  clMinimapVisibleLines = $00E6FAFF;
  clMinimapBookmark = clGreen;
  clWordWrapIndicatorArrow = clNavy;
  clWordWrapIndicatorLines = clBlack;
  clSyncEditBackground = $00FCFDCD;
  { Undo }
  BCEDITOR_UNDO_BLOCK_NUMBER_START = 10;
  { Resource file bitmaps }
  BCEDITOR_ACTIVE_LINE = 'BCEDITORACTIVELINE';
  BCEDITOR_BOOKMARK_IMAGES = 'BCEDITORBOOKMARKIMAGES';
  BCEDITOR_MOUSE_MOVE_SCROLL = 'BCEDITORMOUSEMOVESCROLL';
  BCEDITOR_NULL_IMAGE = 'BCEDITORNULLIMAGE';
  BCEDITOR_SYNCEDIT = 'BCEDITORSYNCEDIT';
  BCEDITOR_NULL_IMAGE_WIDTH = 16;
  { Mouse wheel scroll cursor indexes }
  scNone = -1;
  scNorth = 0;
  scNorthEast = 1;
  scEast = 2;
  scSouthEast = 3;
  scSouth = 4;
  scSouthWest = 5;
  scWest = 6;
  scNorthWest = 7;

implementation

end.
