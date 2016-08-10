unit BCEditor.Language;

interface

resourcestring

  { BCEditor.Editor.Base }
  SBCEditorScrollInfoTopLine = 'Верхняя строка: %d';
  SBCEditorScrollInfo = '%d - %d';
  SBCEditorSearchStringNotFound = 'Строка ''%s'' не найдена';
  SBCEditorSearchMatchNotFound = 'Выражение не найдено. %s Повторить поиск с начала текста?';
  SBCEditorRightMarginPosition = 'Позиция: %d';
  SBCEditorSearchEngineNotAssigned = 'Поисковая машина не может быть присвоена';

  { BCEditor.Editor.KeyCommands }
  SBCEditorDuplicateShortcut = 'Сочетание клавиш уже существует';

  { BCEditor.MacroRecorder }
  SBCEditorCannotRecord = 'Невозможно записать макрос - он уже записывается или проигрывается';
  SBCEditorCannotPlay = 'Невозможно воспроизвести макрос - он уже записывается или проигрывается';
  SBCEditorCannotPause = 'Можно приостановить запись';
  SBCEditorCannotResume = 'Можно продолжить после паузы';
  SBCEditorShortcutAlreadyExists = 'Сочетание клавиш уже существует';

  { BCEditor.Print.Preview }
  SBCEditorPreviewScrollHint = 'Страница: %d';

  { BCEditor.TextBuffer }
  SBCEditorListIndexOutOfBounds = 'Неверный индекст списка строк %d';
  SBCEditorInvalidCapacity = 'Ёмкость списка строк не может быть меньше количества строк в нём';

  { BCEditor.Highlighter.Import.JSON }
  SBCEditorErrorInHighlighterParse = 'Ошибка разбора JSON строка %d столбец %d: %s';
  SBCEditorErrorInHighlighterImport = 'Ошибка импорта схемы подсветки: %s';

  { BCEditor.Search }
  SBCEditorPatternIsEmpty = 'Паттерн занят';

  { BCEditor.TextDrawer }
  SBCEditorValueMustBeSpecified = 'SetBaseFont: ''Value'' должно быть указано.';

implementation

end.
