unit BCEditor.Language;

interface

resourcestring
  { BCEditor.Editor.Base }
  SBCEditorVersion = 'Version';
  SBCEditorScrollInfoTopLine = 'Top line: %d';
  SBCEditorScrollInfo = '%d - %d';
  SBCEditorSearchStringNotFound = 'Search string ''%s'' not found';
  SBCEditorSearchMatchNotFound = 'Search match not found.%sRestart search from the beginning of the file?';
  SBCEditorRightMarginPosition = 'Position: %d';
  SBCEditorSearchEngineNotAssigned = 'Search engine has not been assigned';

  { BCEditor.Editor.KeyCommands }
  SBCEditorDuplicateShortcut = 'Shortcut already exists';

  { BCEditor.MacroRecorder }
  SBCEditorCannotRecord = 'Cannot record macro; already recording or playing';
  SBCEditorCannotPlay = 'Cannot playback macro; already playing or recording';
  SBCEditorCannotPause = 'Can only pause when recording';
  SBCEditorCannotResume = 'Can only resume when paused';
  SBCEditorShortcutAlreadyExists = 'Shortcut already exists';

  { BCEditor.Print.Preview }
  SBCEditorPreviewScrollHint = 'Page: %d';

  { BCEditor.TextBuffer }
  SBCEditorListIndexOutOfBounds = 'Invalid stringlist index %d';
  SBCEditorInvalidCapacity = 'Stringlist capacity cannot be smaller than count';

  { BCEditor.Highlighter.Import.JSON }
  SBCEditorErrorInHighlighterParse = 'JSON parse error on line %d column %d: %s';
  SBCEditorErrorInHighlighterImport = 'Error in highlighter import: %s';

  { BCEditor.Search }
  SBCEditorPatternIsEmpty = 'Pattern is empty';

  { BCEditor.PaintHelper }
  SBCEditorValueMustBeSpecified = 'SetBaseFont: ''Value'' must be specified.';

implementation

end.