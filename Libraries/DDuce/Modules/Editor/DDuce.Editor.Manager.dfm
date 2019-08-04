object dmEditorManager: TdmEditorManager
  OldCreateOrder = False
  Height = 405
  Width = 410
  object aclActions: TActionList
    Images = imlMain
    OnExecute = aclActionsExecute
    Left = 352
    Top = 32
    object actSettings: TAction
      Category = 'Settings'
      Caption = 'Settings'
      Hint = 'Editor settings'
      ImageIndex = 7
      ShortCut = 24659
      OnExecute = actSettingsExecute
    end
    object actSearch: TAction
      Category = 'Find'
      Caption = '&Find...'
      Hint = 'Find'
      ImageIndex = 9
      ShortCut = 16454
      OnExecute = actSearchExecute
    end
    object actFindNextWord: TAction
      Category = 'Find'
      Caption = 'Find &next word'
      Hint = 'Find next word occurence'
      ImageIndex = 21
      ShortCut = 49192
      OnExecute = actFindNextWordExecute
    end
    object actFindPrevWord: TAction
      Category = 'Find'
      Caption = 'Find &previous word'
      Hint = 'Find previous word occurence'
      ImageIndex = 22
      ShortCut = 49190
      OnExecute = actFindPrevWordExecute
    end
    object actSearchReplace: TAction
      Category = 'Find'
      Caption = '&Replace...'
      Hint = 'Replace'
      ImageIndex = 10
      ShortCut = 16466
      OnExecute = actSearchReplaceExecute
    end
    object actSaveAs: TAction
      Category = 'File'
      Caption = 'Save as...'
      Hint = 'Save file as'
      ImageIndex = 55
      ShortCut = 24659
      OnExecute = actSaveAsExecute
    end
    object actOpen: TAction
      Category = 'File'
      Caption = 'Open'
      Hint = 'Open file'
      ImageIndex = 3
      ShortCut = 16463
      OnExecute = actOpenExecute
    end
    object actExportToHTML: TAction
      Category = 'Export'
      Caption = 'Save as HTML file'
      Hint = 'Save as HTML file'
      OnExecute = actExportToHTMLExecute
    end
    object actExportToWiki: TAction
      Category = 'Export'
      Caption = 'Save as Wiki file'
      Enabled = False
      Hint = 'Save as Wiki file'
      OnExecute = actExportToWikiExecute
    end
    object actExportToRTF: TAction
      Category = 'Export'
      Caption = 'Save as RTF file'
      Hint = 'Save as RTF file'
      OnExecute = actExportToRTFExecute
    end
    object actCopytHTMLToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy as HTML object'
      Hint = 'Copy to clipboard as HTML object'
      OnExecute = actCopytHTMLToClipboardExecute
    end
    object actCopyWikiToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy as Wiki object'
      Hint = 'Copy to clipboard as Wiki object'
      OnExecute = actCopyWikiToClipboardExecute
    end
    object actCopyRTFToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy as RTF object'
      Hint = 'Copy to clipboard as RTF object'
      OnExecute = actCopyRTFToClipboardExecute
    end
    object actCopyRTFTextToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy as RTF text'
      Hint = 'Copy to clipboard as RTF text'
      OnExecute = actCopyRTFTextToClipboardExecute
    end
    object actCopyWikiTextToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy as Wiki text'
      Hint = 'Copy to clipboard as TeX text'
      OnExecute = actCopyWikiTextToClipboardExecute
    end
    object actCopyHTMLTextToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy as HTML text'
      Hint = 'Copy to clipboard as HTML text'
      OnExecute = actCopyHTMLTextToClipboardExecute
    end
    object actPageSetup: TAction
      Category = 'Print'
      Caption = 'Page setup'
      Enabled = False
      Hint = 'Page setup'
      ImageIndex = 35
      Visible = False
      OnExecute = actPageSetupExecute
    end
    object actPrintPreview: TAction
      Category = 'Print'
      Caption = 'Print preview'
      Enabled = False
      Hint = 'Print preview'
      ImageIndex = 35
      Visible = False
      OnExecute = actPrintPreviewExecute
    end
    object actPrint: TAction
      Category = 'Print'
      Caption = 'Print'
      Enabled = False
      Hint = 'Print'
      ImageIndex = 34
      ShortCut = 16464
      Visible = False
      OnExecute = actPrintExecute
    end
    object actCopyToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy to clipboard'
      Hint = 'Copy to clipboard'
      ImageIndex = 1
      ShortCut = 16451
      OnExecute = actCopyToClipboardExecute
    end
    object actShowSpecialCharacters: TAction
      Category = 'Settings'
      AutoCheck = True
      Caption = 'Show special characters'
      Hint = 'Show special characters (like tabs and spaces).'
      ImageIndex = 84
      ShortCut = 24658
      OnExecute = actShowSpecialCharactersExecute
    end
    object actUpperCaseSelection: TAction
      Category = 'Selection'
      Caption = 'Uppercase'
      Hint = 'Upper case selection'
      ImageIndex = 20
      ShortCut = 16469
      OnExecute = actUpperCaseSelectionExecute
    end
    object actLowerCaseSelection: TAction
      Category = 'Selection'
      Caption = 'Lowercase'
      Hint = 'Lower case selection'
      ImageIndex = 25
      ShortCut = 16460
      OnExecute = actLowerCaseSelectionExecute
    end
    object actToggleComment: TAction
      Category = 'Selection'
      Caption = 'Toggle comment'
      Hint = 'Toggle comment for current line or each selected line.'
      ImageIndex = 17
      ShortCut = 16575
      OnExecute = actToggleCommentExecute
    end
    object actAlignSelection: TAction
      Category = 'Selection'
      Caption = 'Align'
      Hint = 'Align selected lines'
      ImageIndex = 29
      ShortCut = 49242
      OnExecute = actAlignSelectionExecute
    end
    object actSortSelectedLines: TAction
      Category = 'Selection'
      Caption = 'Toggle sort selected lines'
      Hint = 'Toggle sort order for selected lines'
      ImageIndex = 16
      ShortCut = 49235
      OnExecute = actSortSelectedLinesExecute
    end
    object actFindNext: TAction
      Category = 'Find'
      Caption = 'Find next'
      Hint = 'Find next'
      ImageIndex = 21
      ShortCut = 114
      OnExecute = actFindNextExecute
    end
    object actFindPrevious: TAction
      Category = 'Find'
      Caption = 'Find previous'
      Hint = 'Find previous'
      ImageIndex = 22
      ShortCut = 16498
      OnExecute = actFindPreviousExecute
    end
    object actQuoteLines: TAction
      Category = 'Selection'
      Caption = 'Quote lines'
      Hint = 'Quote each line in the selection.'
      ImageIndex = 82
      ShortCut = 16465
      OnExecute = actQuoteLinesExecute
    end
    object actQuoteLinesAndDelimit: TAction
      Category = 'Selection'
      Caption = 'Quote lines and delimit'
      Hint = 'Quote each line in the selection and delimit with comma'
      ImageIndex = 110
      ShortCut = 24657
      OnExecute = actQuoteLinesAndDelimitExecute
    end
    object actFormat: TAction
      Category = 'Commands'
      Caption = 'Format text'
      Hint = 'Formats text depending on the current highlighter.'
      ImageIndex = 74
      ShortCut = 49222
      OnExecute = actFormatExecute
    end
    object actIncFontSize: TAction
      Category = 'Settings'
      Caption = 'Increase font size'
      Hint = 'Increase editor font size'
      ImageIndex = 78
      SecondaryShortCuts.Strings = (
        'CTRL+'#39'+'#39)
      ShortCut = 16491
      OnExecute = actIncFontSizeExecute
    end
    object actDecFontSize: TAction
      Category = 'Settings'
      Caption = 'Decrease font size'
      Hint = 'Decrease editor font size'
      ImageIndex = 77
      ShortCut = 16493
      OnExecute = actDecFontSizeExecute
    end
    object actShowCodeShaper: TAction
      Category = 'ToolViews'
      Caption = 'Shape code'
      Hint = 'Apply custom code formatting to selection'
      ImageIndex = 79
      ShortCut = 49219
      OnExecute = actShowCodeShaperExecute
    end
    object actPascalStringOfSelection: TAction
      Category = 'Selection'
      Caption = 'Pascal string'
      Hint = 'Convert selection to pascal string declaration'
      ImageIndex = 114
      ShortCut = 57424
      OnExecute = actPascalStringOfSelectionExecute
    end
    object actSave: TAction
      Category = 'File'
      Caption = 'Save'
      Hint = 'Save'
      ImageIndex = 4
      ShortCut = 16467
      OnExecute = actSaveExecute
    end
    object actFoldLevel0: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 0'
      GroupIndex = 2
      Hint = 'Sets the fold level to 0'
      ImageIndex = 59
      OnExecute = actFoldLevel0Execute
    end
    object actFoldLevel1: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 1'
      GroupIndex = 2
      Hint = 'Sets the fold level to 1'
      ImageIndex = 60
      OnExecute = actFoldLevel1Execute
    end
    object actFoldLevel2: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 2'
      GroupIndex = 2
      Hint = 'Sets the fold level to 2'
      ImageIndex = 61
      OnExecute = actFoldLevel2Execute
    end
    object actFoldLevel3: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 3'
      GroupIndex = 2
      Hint = 'Sets the fold level to 3'
      ImageIndex = 62
      OnExecute = actFoldLevel3Execute
    end
    object actFoldLevel4: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 4'
      GroupIndex = 2
      Hint = 'Sets the fold level to 4'
      ImageIndex = 63
      OnExecute = actFoldLevel4Execute
    end
    object actFoldLevel5: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 5'
      GroupIndex = 2
      Hint = 'Sets the fold level to 5'
      ImageIndex = 64
      OnExecute = actFoldLevel5Execute
    end
    object actFoldLevel6: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 6'
      GroupIndex = 2
      Hint = 'Sets the fold level to 6'
      ImageIndex = 65
      OnExecute = actFoldLevel6Execute
    end
    object actFoldLevel7: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 7'
      GroupIndex = 2
      Hint = 'Sets the fold level to 7'
      ImageIndex = 66
      OnExecute = actFoldLevel7Execute
    end
    object actFoldLevel8: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 8'
      GroupIndex = 2
      Hint = 'Sets the fold level to 8'
      ImageIndex = 67
      OnExecute = actFoldLevel8Execute
    end
    object actFoldLevel9: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 9'
      GroupIndex = 2
      Hint = 'Sets the fold level to 9'
      ImageIndex = 68
      OnExecute = actFoldLevel9Execute
    end
    object actFoldLevel10: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level max'
      Checked = True
      GroupIndex = 2
      Hint = 'Sets the fold level to 10'
      ImageIndex = 69
      OnExecute = actFoldLevel10Execute
    end
    object actToggleFoldLevel: TAction
      Category = 'Fold'
      Caption = 'Toggle fold level'
      Hint = 'Toggle folding level.'
      ImageIndex = 69
      ShortCut = 118
      OnExecute = actToggleFoldLevelExecute
    end
    object actToggleHighlighter: TAction
      Category = 'Highlighter'
      Caption = 'Toggle highlighter'
      Hint = 'Toggle highlighter.'
      ImageIndex = 75
      ShortCut = 115
      OnExecute = actToggleHighlighterExecute
    end
    object actInspect: TAction
      Category = 'Debug'
      Caption = 'Inspect'
      Hint = 'Show inspector'
      ImageIndex = 52
      ShortCut = 16496
      OnExecute = actInspectExecute
    end
    object actShowCodeFilter: TAction
      Category = 'ToolViews'
      Caption = 'Filter code'
      Hint = 'Filter code'
      ImageIndex = 70
      ShortCut = 16455
      OnExecute = actShowCodeFilterExecute
    end
    object actInsertColorValue: TAction
      Category = 'Insert'
      Caption = 'Insert color value'
      Hint = 'Insert numeric color value'
      ImageIndex = 88
      ShortCut = 8304
      OnExecute = actInsertColorValueExecute
    end
    object actReload: TAction
      Category = 'File'
      Caption = 'Reload'
      Hint = 'Reload content.'
      ImageIndex = 14
      ShortCut = 116
      OnExecute = actReloadExecute
    end
    object actNew: TAction
      Category = 'File'
      Caption = 'New'
      Hint = 'Create a new editor view.'
      ImageIndex = 39
      ShortCut = 16462
      OnExecute = actNewExecute
    end
    object actShowPreview: TAction
      Category = 'ToolViews'
      Caption = 'Show preview'
      Hint = 'Show text preview.'
      ImageIndex = 71
      ShortCut = 49232
      OnExecute = actShowPreviewExecute
    end
    object actAutoFormatXML: TAction
      Category = 'Settings'
      Caption = 'Autoformat XML'
      ImageIndex = 50
      ShortCut = 49222
      OnExecute = actAutoFormatXMLExecute
    end
    object actHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      Hint = 'Show help dialog.'
      ImageIndex = 54
      OnExecute = actHelpExecute
    end
    object actOpenFileAtCursor: TAction
      Category = 'File'
      Caption = 'Open file at cursor'
      Hint = 'Open file at cursor'
      ImageIndex = 80
      ShortCut = 16397
      OnExecute = actOpenFileAtCursorExecute
    end
    object actDequoteLines: TAction
      Category = 'Selection'
      Caption = 'Dequote lines'
      Hint = 'Dequote each line in the selection.'
      ImageIndex = 86
      ShortCut = 24644
      OnExecute = actDequoteLinesExecute
    end
    object actCopy: TAction
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy selection'
      ImageIndex = 1
      ShortCut = 16451
      OnExecute = actCopyExecute
    end
    object actShowCharacterMap: TAction
      Category = 'ToolViews'
      Caption = 'Show character map'
      Hint = 'Show character map with Ansi and Unicode characters.'
      ImageIndex = 43
      OnExecute = actShowCharacterMapExecute
    end
    object actQuoteSelection: TAction
      Category = 'Selection'
      Caption = 'Quote'
      Hint = 'Quote selection'
      ImageIndex = 83
      OnExecute = actQuoteSelectionExecute
    end
    object actDequoteSelection: TAction
      Category = 'Selection'
      Caption = 'Dequote'
      Hint = 'Dequote selection'
      ImageIndex = 85
      ShortCut = 16452
      OnExecute = actDequoteSelectionExecute
    end
    object actAutoGuessHighlighter: TAction
      Category = 'Commands'
      Caption = 'Auto guess highlighter'
      Hint = 'Auto guess highlighter'
      ImageIndex = 87
      ShortCut = 16456
      OnExecute = actAutoGuessHighlighterExecute
    end
    object actSmartSelect: TAction
      Category = 'Select'
      Caption = 'Smartselect'
      Hint = 'Make a smart selection'
      ImageIndex = 89
      ShortCut = 112
      OnExecute = actSmartSelectExecute
    end
    object actClose: TAction
      Category = 'File'
      Caption = 'Close active editor view'
      Hint = 'Close the active editor instance.'
      ImageIndex = 31
      ShortCut = 16499
      OnExecute = actCloseExecute
    end
    object actStripFirstChar: TAction
      Category = 'Selection'
      Caption = 'Strip first character'
      Hint = 'Strip first character from every line in the selection.'
      ImageIndex = 95
      ShortCut = 57414
      OnExecute = actStripFirstCharExecute
    end
    object actStripLastChar: TAction
      Category = 'Selection'
      Caption = 'Strip last character'
      Hint = 'Strip last character from every line in the selection.'
      ImageIndex = 94
      ShortCut = 57420
      OnExecute = actStripLastCharExecute
    end
    object actRedo: TAction
      Category = 'Edit'
      Caption = 'Redo'
      Hint = 'Redo'
      ImageIndex = 37
      ShortCut = 24666
      OnExecute = actRedoExecute
    end
    object actCloseOthers: TAction
      Category = 'File'
      Caption = 'Close all other editors'
      Hint = 'Close all other editors'
      ShortCut = 24691
      OnExecute = actCloseOthersExecute
    end
    object actMonitorChanges: TAction
      Category = 'Settings'
      Caption = 'Monitor changes'
      Hint = 'Monitor for external file changes.'
      ImageIndex = 90
      ShortCut = 24653
      OnExecute = actMonitorChangesExecute
    end
    object actAbout: TAction
      Category = 'Help'
      Caption = 'About'
      Hint = 'Shows the about dialog.'
      ImageIndex = 73
      OnExecute = actAboutExecute
    end
    object actShowActions: TAction
      Category = 'Debug'
      Caption = 'Show editor actions'
      Hint = 'Show the list of all available actions.'
      OnExecute = actShowActionsExecute
    end
    object actCopyFullPath: TAction
      Category = 'Edit'
      Caption = 'Copy full path'
      Hint = 'Copy full filepath to the clipboard.'
      OnExecute = actCopyFullPathExecute
    end
    object actCopyFileName: TAction
      Category = 'Edit'
      Caption = 'Copy filename'
      Hint = 'Copy the filename to the clipboard.'
      OnExecute = actCopyFileNameExecute
    end
    object actCopyFilePath: TAction
      Category = 'Edit'
      Caption = 'Copy file path'
      Hint = 'Copy the file path to the clipboard.'
      OnExecute = actCopyFilePathExecute
    end
    object actEncodeBase64: TAction
      Category = 'Selection'
      Caption = 'Encode base64'
      Hint = 'Encode selected text to base64.'
      OnExecute = actEncodeBase64Execute
    end
    object actDecodeBase64: TAction
      Category = 'Selection'
      Caption = 'Decode base64'
      Hint = 'Decode selected text from base64.'
      OnExecute = actDecodeBase64Execute
    end
    object actShowViews: TAction
      Category = 'ToolViews'
      Caption = 'Show views'
      Hint = 'Show list of all editor views.'
      ImageIndex = 91
      ShortCut = 49238
      OnExecute = actShowViewsExecute
    end
    object actSyncEdit: TAction
      Category = 'Selection'
      Caption = 'Sync edit'
      Hint = 'Synchronized edit'
      ImageIndex = 5
      ShortCut = 24650
      OnExecute = actSyncEditExecute
    end
    object actClear: TAction
      Category = 'Select'
      Caption = 'Clear'
      Hint = 'Clear all text in the editor view.'
      ImageIndex = 72
      OnExecute = actClearExecute
    end
    object actCreateDesktopLink: TAction
      Category = 'Commands'
      Caption = 'Create desktop link'
      Hint = 'Create a desktop shortcut to the active file.'
      ImageIndex = 92
      ShortCut = 49228
      OnExecute = actCreateDesktopLinkExecute
    end
    object actExit: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Exit application'
      ImageIndex = 19
      OnExecute = actExitExecute
    end
    object actStripMarkup: TAction
      Category = 'Selection'
      Caption = 'Strip markup'
      Hint = 'Strip markup tags from XML/HTML.'
      ImageIndex = 93
      ShortCut = 57427
      OnExecute = actStripMarkupExecute
    end
    object actPaste: TAction
      Category = 'Edit'
      Caption = 'Paste'
      Hint = 'Paste'
      ImageIndex = 2
      ShortCut = 16470
      OnExecute = actPasteExecute
    end
    object actCut: TAction
      Category = 'Edit'
      Caption = 'Cut'
      Hint = 'Cut'
      ImageIndex = 0
      ShortCut = 16472
      OnExecute = actCutExecute
    end
    object actUndo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      Hint = 'Undo'
      ImageIndex = 38
      ShortCut = 16474
      OnExecute = actUndoExecute
    end
    object actSelectAll: TAction
      Category = 'Select'
      Caption = 'Select &All'
      Hint = 'Select all.'
      ImageIndex = 23
      ShortCut = 16449
      OnExecute = actSelectAllExecute
    end
    object actDelete: TAction
      Category = 'Edit'
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 31
      ShortCut = 46
    end
    object actShowTest: TAction
      Category = 'ToolViews'
      AutoCheck = True
      Caption = 'Show testform'
      ShortCut = 49220
      OnExecute = actShowTestExecute
    end
    object actToggleBlockCommentSelection: TAction
      Category = 'Selection'
      Caption = 'Toggle block comment selection'
      Hint = 
        'Comment/uncomment the selection with the highlighter'#39's block com' +
        'ment tags.'
      ImageIndex = 17
      ShortCut = 24767
      OnExecute = actToggleBlockCommentSelectionExecute
    end
    object actShowStructureViewer: TAction
      Category = 'ToolViews'
      Caption = 'XML treeview'
      ShortCut = 49240
      OnExecute = actShowStructureViewerExecute
    end
    object actSelectionInfo: TAction
      Category = 'Debug'
      Caption = 'SelectionInfo'
      ShortCut = 24651
      OnExecute = actSelectionInfoExecute
    end
    object actStayOnTop: TAction
      Category = 'Settings'
      AutoCheck = True
      Caption = 'Stay on top'
      Hint = 
        'Lets the application window stay on top of all other opened wind' +
        'ows.'
      ImageIndex = 96
      OnExecute = actStayOnTopExecute
    end
    object actToggleMaximized: TAction
      Category = 'Settings'
      AutoCheck = True
      Caption = 'Toggle maximized'
      Hint = 'Toggles between maximized and restored mode.'
      ImageIndex = 97
      ShortCut = 122
      OnExecute = actToggleMaximizedExecute
    end
    object actSingleInstance: TAction
      Category = 'Settings'
      AutoCheck = True
      Caption = 'Single instance'
      Hint = 'Allow only one active instance of the application.'
      ImageIndex = 98
      OnExecute = actSingleInstanceExecute
    end
    object actSelectionMenu: TAction
      Category = 'Selection'
      Caption = 'Selection'
      ImageIndex = 41
    end
    object actSelectionModeMenu: TAction
      Category = 'SelectionMode'
      Caption = 'Selectionmode'
    end
    object actHighlighter: TAction
      Category = 'Highlighter'
      Caption = 'Highlighter'
    end
    object actLineBreakStyleMenu: TAction
      Category = 'Linebreakstyle'
      Caption = 'Linebreakstyle'
    end
    object actEncodingMenu: TAction
      Category = 'Encoding'
      Caption = 'Encoding'
    end
    object actExportMenu: TAction
      Category = 'Export'
      Caption = 'Export'
      ImageIndex = 80
    end
    object actClipboardMenu: TAction
      Category = 'Clipboard'
      Caption = 'Clipboard'
      ImageIndex = 100
    end
    object actInsertGUID: TAction
      Category = 'Insert'
      Caption = 'Insert GUID'
      Hint = 'Inserts a new GUID value.'
      ImageIndex = 99
      ShortCut = 24647
      OnExecute = actInsertGUIDExecute
    end
    object actInsertMenu: TAction
      Category = 'Insert'
      Caption = 'Insert'
      ImageIndex = 102
    end
    object actFindAllOccurences: TAction
      Category = 'Find'
      Caption = 'Find all occurences'
      Hint = 
        'Finds all occurences of the selected text or the current word at' +
        ' the caret position.'
      ImageIndex = 27
      ShortCut = 24646
      OnExecute = actFindAllOccurencesExecute
    end
    object actSearchMenu: TAction
      Category = 'Find'
      Caption = 'Search'
      ImageIndex = 9
    end
    object actIndent: TAction
      Category = 'Selection'
      Caption = 'Indent'
      Hint = 'Indent selected lines.'
      ImageIndex = 30
      ShortCut = 24649
      OnExecute = actIndentExecute
    end
    object actUnindent: TAction
      Category = 'Selection'
      Caption = 'Unindent'
      Hint = 'Unindent selected lines.'
      ImageIndex = 12
      ShortCut = 24661
      OnExecute = actUnindentExecute
    end
    object actSelectMenu: TAction
      Category = 'Select'
      Caption = 'Select'
      ImageIndex = 101
    end
    object actFileMenu: TAction
      Category = 'File'
      Caption = 'File'
      ImageIndex = 103
    end
    object actShowHTMLViewer: TAction
      Category = 'ToolViews'
      Caption = 'Show HTML viewer'
      OnExecute = actShowHTMLViewerExecute
    end
    object actShowHexEditor: TAction
      Category = 'ToolViews'
      Caption = 'Show Hex editor'
      OnExecute = actShowHexEditorExecute
    end
    object actShowMinimap: TAction
      Category = 'Settings'
      AutoCheck = True
      Caption = 'Show minimap'
      Hint = 'Show minimap.'
      ImageIndex = 120
      ShortCut = 49229
      OnExecute = actShowMinimapExecute
    end
    object actConvertTabsToSpaces: TAction
      Category = 'Selection'
      Caption = 'Convert TABs to spaces in selection'
      Hint = 
        'Convert TAB character to the equivalent amount of spaces in the ' +
        'selection.'
      ImageIndex = 111
      OnExecute = actConvertTabsToSpacesExecute
    end
    object actExecuteScriptOnSelection: TAction
      Category = 'Selection'
      Caption = 'Execute script on selection'
      Hint = 'Execute script function on the selected lines.'
    end
    object actShowScriptEditor: TAction
      Category = 'ToolViews'
      Caption = 'Show script editor'
      OnExecute = actShowScriptEditorExecute
    end
    object actShowFilterTest: TAction
      Category = 'Debug'
      Caption = 'Show filter test dialog'
      ShortCut = 57460
      OnExecute = actShowFilterTestExecute
    end
    object actHighlighterMenu: TAction
      Category = 'Highlighter'
      Caption = 'Highlighter'
      ImageIndex = 75
    end
    object actFoldMenu: TAction
      Category = 'Fold'
      Caption = 'Fold'
      ImageIndex = 69
    end
    object actSettingsMenu: TAction
      Category = 'Settings'
      Caption = 'Settings'
      ImageIndex = 7
    end
    object actEncodeURL: TAction
      Category = 'Selection'
      Caption = 'Encode URL'
      Hint = 'Convert the selected text to an encoded URL string.'
      OnExecute = actEncodeURLExecute
    end
    object actDecodeURL: TAction
      Category = 'Selection'
      Caption = 'Decode URL'
      Hint = 'Decode the URL from the selected text.'
      OnExecute = actDecodeURLExecute
    end
    object actSelectionEncodeMenu: TAction
      Category = 'Selection'
      Caption = 'Encode'
      ImageIndex = 106
    end
    object actSelectionDecodeMenu: TAction
      Category = 'Selection'
      Caption = 'Decode'
      ImageIndex = 107
    end
    object actStripComments: TAction
      Category = 'Selection'
      Caption = 'Strip comments'
      Hint = 'Strip all comments from the selected code.'
      ImageIndex = 109
      OnExecute = actStripCommentsExecute
    end
    object actMergeBlankLines: TAction
      Category = 'Selection'
      Caption = 'Merge blank lines'
      Hint = 'Merge blank lines to one in the selected lines.'
      ImageIndex = 108
      OnExecute = actMergeBlankLinesExecute
    end
    object actCompressSpace: TAction
      Category = 'Selection'
      Caption = 'Compress spaces'
      Hint = 'Reduce consecutive spaces to one.'
      ImageIndex = 56
      OnExecute = actCompressSpaceExecute
    end
    object actCompressWhitespace: TAction
      Category = 'Selection'
      Caption = 'Compress whitespace'
      Hint = 
        'Replace any number of consecutive whitespace (including newlines' +
        ')'#13#10' with a single whitespace.'
      ImageIndex = 76
      OnExecute = actCompressWhitespaceExecute
    end
    object actSaveAll: TAction
      Category = 'File'
      Caption = 'Save all'
      Hint = 'Save all changes.'
      ImageIndex = 113
      ShortCut = 24659
      OnExecute = actSaveAllExecute
    end
    object actRecordMacro: TAction
      Category = 'Commands'
      Caption = 'Record macro'
      ImageIndex = 116
      ShortCut = 24658
      OnExecute = actRecordMacroExecute
    end
    object actPlaybackMacro: TAction
      Category = 'Commands'
      Caption = 'Playback macro'
      ImageIndex = 118
      ShortCut = 24656
      OnExecute = actPlaybackMacroExecute
    end
    object actEncodeXML: TAction
      Category = 'Selection'
      Caption = 'Encode XML'
      OnExecute = actEncodeXMLExecute
    end
    object actDecodeXML: TAction
      Category = 'Selection'
      Caption = 'Decode XML'
      OnExecute = actDecodeXMLExecute
    end
    object actToggleWordWrap: TAction
      Category = 'Settings'
      AutoCheck = True
      Caption = 'Toggle wordwrap'
      Hint = 'Toggle wordwrap mode.'
      ImageIndex = 119
      ShortCut = 49239
      OnExecute = actToggleWordWrapExecute
    end
    object actShowIndentGuides: TAction
      Category = 'Settings'
      AutoCheck = True
      Caption = 'Show indent guides'
      OnExecute = actShowIndentGuidesExecute
    end
    object actShowSearchmap: TAction
      Category = 'Settings'
      AutoCheck = True
      Caption = 'Show searchmap'
      Hint = 'Show searchmap bar.'
      OnExecute = actShowSearchmapExecute
    end
  end
  object ppmExport: TPopupMenu
    Images = imlMain
    Left = 40
    Top = 248
    object MenuItem1: TMenuItem
    end
  end
  object ppmHighLighters: TPopupMenu
    Images = imlMain
    Left = 40
    Top = 200
  end
  object ppmClipboard: TPopupMenu
    Images = imlMain
    Left = 40
    Top = 144
  end
  object dlgSave: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShowHelp, ofPathMustExist, ofCreatePrompt, ofShareAware, ofEnableSizing]
    Left = 285
    Top = 32
  end
  object dlgOpen: TOpenDialog
    Left = 285
    Top = 88
  end
  object ppmFold: TPopupMenu
    Images = imlMain
    Left = 40
    Top = 88
    object MenuItem43: TMenuItem
      Action = actFoldLevel0
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF191919FF151515FF151515FF151515FF151515FF1515
        15FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF191919FF565656FFD0D0D0FFFFFFFFFFE0E0E0FF8383
        83FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF2121
        21FF212121FF212121FF393939FFF1F1F1FFFFFFFFFFC2C2C2FFF1F1F1FFFFFF
        FFFF656565FF191919FF212121FF212121FF212121FF151515FF191919FF2121
        21FF212121FF212121FFA6A6A6FFFFFFFFFFA6A6A6FF212121FF7A7A7AFFFFFF
        FFFFE0E0E0FF212121FF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF2D2D2DFFD0D0D0FFFFFFFFFF6F6F6FFF2D2D2DFF353535FFFFFF
        FFFFFFFFFFFF353535FF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3535
        35FF353535FF2D2D2DFFFFFFFFFFFFFFFFFF656565FF353535FF2D2D2DFFFFFF
        FFFFFFFFFFFF656565FF353535FF353535FF353535FF212121FF2D2D2DFF3939
        39FF393939FF2D2D2DFFFFFFFFFFFFFFFFFF656565FF353535FF2D2D2DFFFFFF
        FFFFFFFFFFFF656565FF353535FF393939FF393939FF2D2D2DFF353535FF4040
        40FF404040FF353535FFD0D0D0FFFFFFFFFF777777FF393939FF404040FFFFFF
        FFFFFFFFFFFF4D4D4DFF393939FF404040FF404040FF353535FF393939FF4040
        40FF404040FF393939FFAFAFAFFFFFFFFFFFAFAFAFFF393939FF838383FFFFFF
        FFFFE0E0E0FF393939FF404040FF404040FF404040FF393939FF404040FF4D4D
        4DFF4D4D4DFF4D4D4DFF565656FFF1F1F1FFFFFFFFFFCACACAFFF1F1F1FFFFFF
        FFFF898989FF404040FF4D4D4DFF4D4D4DFF4D4D4DFF404040FF404040FF5656
        56FF565656FF565656FF404040FF656565FFD8D8D8FFFFFFFFFFE0E0E0FF9898
        98FF404040FF565656FF565656FF565656FF565656FF404040FF4D4D4DFF5656
        56FF565656FF565656FF565656FF4D4D4DFF404040FF393939FF404040FF4D4D
        4DFF565656FF565656FF565656FF565656FF565656FF4D4D4DFF4D4D4DFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF4D4D4DFF000000004D4D
        4DFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem44: TMenuItem
      Action = actFoldLevel1
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF191919FF191919FF151515FF151515FF151515FF1515
        15FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF191919FF191919FF151515FFFFFFFFFFFFFFFFFF5656
        56FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF2121
        21FF212121FF212121FF212121FF212121FF191919FFFFFFFFFFFFFFFFFF5959
        59FF191919FF212121FF212121FF212121FF212121FF151515FF191919FF2121
        21FF212121FF212121FF212121FF212121FF212121FFFFFFFFFFFFFFFFFF5959
        59FF212121FF212121FF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF212121FFFFFFFFFFFFFFFFFF6161
        61FF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3131
        31FF313131FF313131FF313131FF313131FF2D2D2DFFFFFFFFFFFFFFFFFF6161
        61FF313131FF313131FF313131FF313131FF313131FF212121FF2D2D2DFF3939
        39FF393939FF393939FF393939FF393939FF2D2D2DFFFFFFFFFFFFFFFFFF6161
        61FF313131FF393939FF393939FF393939FF393939FF2D2D2DFF313131FF4040
        40FF404040FF404040FF393939FF313131FF313131FFFFFFFFFFFFFFFFFF6969
        69FF393939FF404040FF404040FF404040FF404040FF313131FF393939FF4040
        40FF404040FF404040FF393939FFD7D7D7FF878787FFFFFFFFFFFFFFFFFF6969
        69FF404040FF404040FF404040FF404040FF404040FF393939FF404040FF4C4C
        4CFF4C4C4CFF4C4C4CFF565656FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6969
        69FF4C4C4CFF4C4C4CFF4C4C4CFF4C4C4CFF4C4C4CFF404040FF404040FF5656
        56FF565656FF565656FF404040FF565656FFB1B1B1FFFFFFFFFFFFFFFFFF7171
        71FF4C4C4CFF565656FF565656FF565656FF565656FF404040FF4C4C4CFF5656
        56FF565656FF565656FF565656FF565656FF4C4C4CFF393939FF393939FF4C4C
        4CFF565656FF565656FF565656FF565656FF565656FF4C4C4CFF4C4C4CFF5959
        59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
        59FF595959FF595959FF595959FF595959FF595959FF4C4C4CFF000000004C4C
        4CFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4C4C4CFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem45: TMenuItem
      Action = actFoldLevel2
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF151515FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF151515FF191919FF191919FF151515FF151515FF2121
        21FF212121FF212121FF191919FFC2C2C2FFFFFFFFFFFFFFFFFFD0D0D0FFC2C2
        C2FFC2C2C2FFC2C2C2FF191919FF212121FF212121FF151515FF191919FF2121
        21FF212121FF212121FF212121FF212121FFA6A6A6FFFFFFFFFFF1F1F1FF4C4C
        4CFF212121FF212121FF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFFA9A9A9FFFFFFFFFFF1F1
        F1FF464646FF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3131
        31FF313131FF313131FF313131FF313131FF313131FF2D2D2DFFA9A9A9FFFFFF
        FFFFD0D0D0FF2D2D2DFF313131FF313131FF313131FF212121FF2D2D2DFF3939
        39FF393939FF393939FF393939FF393939FF393939FF313131FF393939FFE3E3
        E3FFFFFFFFFF828282FF313131FF393939FF393939FF2D2D2DFF313131FF4646
        46FF464646FF464646FF464646FF393939FF393939FF464646FF393939FFA6A6
        A6FFFFFFFFFFBDBDBDFF313131FF464646FF464646FF313131FF393939FF4646
        46FF464646FF464646FF464646FF515151FF515151FF393939FF393939FFA9A9
        A9FFFFFFFFFFBDBDBDFF393939FF464646FF464646FF393939FF464646FF4C4C
        4CFF4C4C4CFF4C4C4CFF464646FFB0B0B0FFFFFFFFFFCACACAFFD8D8D8FFFFFF
        FFFFFFFFFFFF898989FF464646FF4C4C4CFF4C4C4CFF464646FF464646FF5151
        51FF515151FF515151FF4C4C4CFF717171FFBDBDBDFFF1F1F1FFFFFFFFFFE3E3
        E3FF989898FF464646FF515151FF515151FF515151FF464646FF4C4C4CFF5151
        51FF515151FF515151FF515151FF515151FF464646FF464646FF393939FF4646
        46FF4C4C4CFF515151FF515151FF515151FF515151FF4C4C4CFF4C4C4CFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF4C4C4CFF000000004C4C
        4CFF515151FF515151FF515151FF515151FF515151FF515151FF515151FF5151
        51FF515151FF515151FF515151FF515151FF4C4C4CFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem46: TMenuItem
      Action = actFoldLevel3
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF191919FF151515FF151515FF151515FF151515FF1515
        15FF151515FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF191919FF818181FFD0D0D0FFFFFFFFFFFFFFFFFFB1B1
        B1FF343434FF191919FF191919FF191919FF191919FF151515FF151515FF2121
        21FF212121FF212121FF191919FF959595FFE1E1E1FFC2C2C2FFE1E1E1FFFFFF
        FFFFE1E1E1FF191919FF212121FF212121FF212121FF151515FF191919FF2121
        21FF212121FF212121FF212121FF404040FF212121FF212121FF212121FFE1E1
        E1FFFFFFFFFF5C5C5CFF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF343434FFE1E1
        E1FFFFFFFFFF464646FF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3434
        34FF343434FF343434FF343434FF2D2D2DFF818181FFC2C2C2FFF1F1F1FFFFFF
        FFFF9C9C9CFF2D2D2DFF343434FF343434FF343434FF212121FF2D2D2DFF4040
        40FF404040FF404040FF404040FF343434FF959595FFFFFFFFFFFFFFFFFFD0D0
        D0FF404040FF404040FF404040FF404040FF404040FF2D2D2DFF343434FF4646
        46FF464646FF464646FF464646FF404040FF404040FF343434FFADADADFFFFFF
        FFFF959595FF404040FF464646FF464646FF464646FF343434FF404040FF4646
        46FF464646FF464646FF464646FF515151FF404040FF404040FF7A7A7AFFFFFF
        FFFFCACACAFF404040FF464646FF464646FF464646FF404040FF464646FF4D4D
        4DFF4D4D4DFF4D4D4DFF464646FFA3A3A3FFE1E1E1FFCACACAFFF1F1F1FFFFFF
        FFFFA3A3A3FF464646FF4D4D4DFF4D4D4DFF4D4D4DFF464646FF464646FF5151
        51FF515151FF515151FF4D4D4DFF9C9C9CFFD8D8D8FFFFFFFFFFFFFFFFFFB1B1
        B1FF4D4D4DFF4D4D4DFF515151FF515151FF515151FF464646FF4D4D4DFF5151
        51FF515151FF515151FF515151FF4D4D4DFF464646FF404040FF404040FF4D4D
        4DFF515151FF515151FF515151FF515151FF515151FF4D4D4DFF4D4D4DFF5C5C
        5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C
        5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF4D4D4DFF000000004D4D
        4DFF515151FF515151FF515151FF515151FF515151FF515151FF515151FF5151
        51FF515151FF515151FF515151FF515151FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem47: TMenuItem
      Action = actFoldLevel4
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF1515
        15FF151515FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF191919FF191919FF191919FF151515FFFFFFFFFFFFFF
        FFFF565656FF191919FF191919FF191919FF191919FF151515FF151515FF2121
        21FF212121FF191919FF191919FF191919FF191919FF191919FFFFFFFFFFFFFF
        FFFF595959FF191919FF212121FF212121FF212121FF151515FF191919FF2121
        21FF212121FF4C4C4CFFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFFFFFFFFFFFFF
        FFFFD5D5D5FF7A7A7AFF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF616161FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF8C8C8CFF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3232
        32FF323232FF2D2D2DFFD5D5D5FFFFFFFFFF464646FF2D2D2DFFFFFFFFFFFFFF
        FFFF616161FF2D2D2DFF323232FF323232FF323232FF212121FF2D2D2DFF3939
        39FF393939FF323232FF616161FFFFFFFFFFBEBEBEFF2D2D2DFFFFFFFFFFFFFF
        FFFF616161FF323232FF393939FF393939FF393939FF2D2D2DFF323232FF4646
        46FF464646FF464646FF323232FFBEBEBEFFFFFFFFFF696969FFFFFFFFFFFFFF
        FFFF696969FF393939FF464646FF464646FF464646FF323232FF393939FF4646
        46FF464646FF464646FF464646FF565656FFF2F2F2FFAFAFAFFFFFFFFFFFFFFF
        FFFF696969FF464646FF464646FF464646FF464646FF393939FF464646FF4C4C
        4CFF4C4C4CFF4C4C4CFF4C4C4CFF464646FF969696FFFFFFFFFFFFFFFFFFFFFF
        FFFF696969FF4C4C4CFF4C4C4CFF4C4C4CFF4C4C4CFF464646FF464646FF5656
        56FF565656FF565656FF565656FF565656FF464646FFE5E5E5FFFFFFFFFFFFFF
        FFFF717171FF4C4C4CFF565656FF565656FF565656FF464646FF4C4C4CFF5656
        56FF565656FF565656FF565656FF565656FF4C4C4CFF464646FF393939FF3939
        39FF4C4C4CFF565656FF565656FF565656FF565656FF4C4C4CFF4C4C4CFF5959
        59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
        59FF595959FF595959FF595959FF595959FF595959FF4C4C4CFF000000004C4C
        4CFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4C4C4CFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem48: TMenuItem
      Action = actFoldLevel5
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF151515FF151515FF151515FF151515FF151515FF1515
        15FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF818181FFE1E1E1FFFFFFFFFFFFFFFFFFC1C1C1FF6565
        65FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF2121
        21FF212121FF191919FF959595FFE1E1E1FFC1C1C1FFD0D0D0FFFFFFFFFFFFFF
        FFFF656565FF191919FF212121FF212121FF212121FF151515FF191919FF2121
        21FF212121FF212121FF313131FF212121FF212121FF212121FF898989FFFFFF
        FFFFE1E1E1FF212121FF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF6F6F6FFFFFFF
        FFFFFFFFFFFF212121FF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3131
        31FF313131FF313131FF393939FF656565FF656565FF818181FFE1E1E1FFFFFF
        FFFFC1C1C1FF2D2D2DFF313131FF313131FF313131FF212121FF2D2D2DFF3939
        39FF393939FF313131FF656565FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1F1
        F1FF595959FF313131FF393939FF393939FF393939FF2D2D2DFF313131FF4040
        40FF404040FF404040FF313131FFFFFFFFFFE1E1E1FF959595FF777777FF4040
        40FF393939FF404040FF404040FF404040FF404040FF313131FF393939FF4040
        40FF404040FF404040FF313131FFFFFFFFFFD0D0D0FF393939FF393939FF3939
        39FF404040FF404040FF404040FF404040FF404040FF393939FF404040FF4D4D
        4DFF4D4D4DFF4D4D4DFF393939FFD8D8D8FFFFFFFFFFCACACAFFCACACAFFCACA
        CAFF898989FF404040FF4D4D4DFF4D4D4DFF4D4D4DFF404040FF404040FF5151
        51FF515151FF515151FF404040FFCACACAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFF989898FF4D4D4DFF515151FF515151FF515151FF404040FF4D4D4DFF5151
        51FF515151FF515151FF4D4D4DFF404040FF393939FF393939FF393939FF3939
        39FF4D4D4DFF515151FF515151FF515151FF515151FF4D4D4DFF4D4D4DFF5959
        59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
        59FF595959FF595959FF595959FF595959FF595959FF4D4D4DFF000000004D4D
        4DFF515151FF515151FF515151FF515151FF515151FF515151FF515151FF5151
        51FF515151FF515151FF515151FF515151FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem49: TMenuItem
      Action = actFoldLevel6
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF191919FF151515FF151515FF151515FF151515FF1515
        15FF151515FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF191919FF565656FFD0D0D0FFFFFFFFFFEFEFEFFFA1A1
        A1FF262626FF191919FF191919FF191919FF191919FF151515FF151515FF2626
        26FF262626FF191919FF595959FFFFFFFFFFFFFFFFFFC2C2C2FFE1E1E1FFFFFF
        FFFFD0D0D0FF191919FF262626FF262626FF262626FF151515FF191919FF2626
        26FF262626FF262626FFB5B5B5FFFFFFFFFF898989FF262626FF262626FFE1E1
        E1FFFFFFFFFF7A7A7AFF262626FF262626FF262626FF191919FF262626FF2D2D
        2DFF2D2D2DFF262626FFFFFFFFFFFFFFFFFF616161FF2D2D2DFF2D2D2DFFC2C2
        C2FFFFFFFFFF898989FF2D2D2DFF2D2D2DFF2D2D2DFF262626FF262626FF3131
        31FF313131FF2D2D2DFFFFFFFFFFFFFFFFFFD0D0D0FF616161FF898989FFFFFF
        FFFFFFFFFFFF727272FF313131FF313131FF313131FF262626FF2D2D2DFF3939
        39FF393939FF313131FFD0D0D0FFFFFFFFFFF1F1F1FFFFFFFFFFFFFFFFFFFFFF
        FFFFACACACFF313131FF393939FF393939FF393939FF2D2D2DFF313131FF4040
        40FF404040FF393939FFA1A1A1FFFFFFFFFFACACACFF727272FF929292FF7272
        72FF393939FF393939FF404040FF404040FF404040FF313131FF393939FF4040
        40FF404040FF404040FF565656FFF1F1F1FFFFFFFFFF929292FF393939FF3939
        39FF404040FF404040FF404040FF404040FF404040FF393939FF404040FF4D4D
        4DFF4D4D4DFF4D4D4DFF404040FF898989FFFFFFFFFFFFFFFFFFF1F1F1FFCACA
        CAFF898989FF404040FF4D4D4DFF4D4D4DFF4D4D4DFF404040FF404040FF5656
        56FF565656FF565656FF565656FF404040FF565656FFB5B5B5FFE1E1E1FFFFFF
        FFFF989898FF4D4D4DFF565656FF565656FF565656FF404040FF4D4D4DFF5656
        56FF565656FF565656FF565656FF565656FF565656FF4D4D4DFF404040FF3939
        39FF4D4D4DFF565656FF565656FF565656FF565656FF4D4D4DFF4D4D4DFF5959
        59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
        59FF595959FF595959FF595959FF595959FF595959FF4D4D4DFF000000004D4D
        4DFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem50: TMenuItem
      Action = actFoldLevel7
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF151515FF151515FF151515FF151515FF191919FF1919
        19FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF262626FFEFEFEFFFFFFFFFFF727272FF191919FF1919
        19FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF2626
        26FF262626FF262626FF191919FF858585FFFFFFFFFFE1E1E1FF191919FF2626
        26FF262626FF262626FF262626FF262626FF262626FF151515FF191919FF2626
        26FF262626FF262626FF262626FF313131FFF0F0F0FFFFFFFFFF7A7A7AFF2626
        26FF262626FF262626FF262626FF262626FF262626FF191919FF262626FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF8C8C8CFFFFFFFFFFD4D4D4FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF262626FF262626FF3131
        31FF313131FF313131FF313131FF313131FF393939FFF0F0F0FFFFFFFFFF6464
        64FF313131FF313131FF313131FF313131FF313131FF262626FF2D2D2DFF3939
        39FF393939FF393939FF393939FF393939FF313131FF909090FFFFFFFFFFC9C9
        C9FF313131FF393939FF393939FF393939FF393939FF2D2D2DFF313131FF4040
        40FF404040FF404040FF404040FF404040FF393939FF404040FFF0F0F0FFFFFF
        FFFF696969FF393939FF404040FF404040FF404040FF313131FF393939FF4040
        40FF404040FF404040FF393939FF393939FF393939FF393939FF909090FFFFFF
        FFFFBDBDBDFF393939FF404040FF404040FF404040FF393939FF404040FF4D4D
        4DFF4D4D4DFF404040FFA3A3A3FFC9C9C9FFC9C9C9FFC9C9C9FFD8D8D8FFFFFF
        FFFFFFFFFFFF565656FF4D4D4DFF4D4D4DFF4D4D4DFF404040FF404040FF5656
        56FF565656FF404040FFC9C9C9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF727272FF4D4D4DFF565656FF565656FF404040FF4D4D4DFF5656
        56FF565656FF4D4D4DFF404040FF393939FF393939FF393939FF393939FF3939
        39FF393939FF4D4D4DFF565656FF565656FF565656FF4D4D4DFF4D4D4DFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF4D4D4DFF000000004D4D
        4DFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem51: TMenuItem
      Action = actFoldLevel8
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF191919FF151515FF151515FF151515FF151515FF1515
        15FF151515FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF262626FF838383FFD0D0D0FFFFFFFFFFE1E1E1FFA1A1
        A1FF262626FF191919FF191919FF191919FF191919FF151515FF151515FF2626
        26FF262626FF191919FFA1A1A1FFFFFFFFFFF1F1F1FF838383FFC2C2C2FFFFFF
        FFFFD0D0D0FF191919FF262626FF262626FF262626FF151515FF191919FF2626
        26FF262626FF262626FFFFFFFFFFFFFFFFFF6B6B6BFF262626FF313131FFFFFF
        FFFFFFFFFFFF4D4D4DFF262626FF262626FF262626FF191919FF262626FF2D2D
        2DFF2D2D2DFF2D2D2DFFE1E1E1FFFFFFFFFF8C8C8CFF2D2D2DFF616161FFFFFF
        FFFFFFFFFFFF464646FF2D2D2DFF2D2D2DFF2D2D2DFF262626FF262626FF3131
        31FF313131FF2D2D2DFF565656FFF1F1F1FFFFFFFFFFC2C2C2FFFFFFFFFFFFFF
        FFFFAAAAAAFF2D2D2DFF313131FF313131FF313131FF262626FF2D2D2DFF3939
        39FF393939FF393939FF393939FFBEBEBEFFFFFFFFFFFFFFFFFFFFFFFFFFE1E1
        E1FF393939FF313131FF393939FF393939FF393939FF2D2D2DFF313131FF4646
        46FF464646FF393939FFA1A1A1FFFFFFFFFFD0D0D0FF4D4D4DFFAAAAAAFFFFFF
        FFFFBEBEBEFF313131FF464646FF464646FF464646FF313131FF393939FF4646
        46FF464646FF393939FFCACACAFFFFFFFFFF959595FF393939FF6B6B6BFFFFFF
        FFFFFFFFFFFF313131FF464646FF464646FF464646FF393939FF464646FF4D4D
        4DFF4D4D4DFF464646FF7D7D7DFFFFFFFFFFF1F1F1FF959595FFD8D8D8FFFFFF
        FFFFBEBEBEFF464646FF4D4D4DFF4D4D4DFF4D4D4DFF464646FF464646FF5656
        56FF565656FF565656FF464646FF7D7D7DFFD8D8D8FFFFFFFFFFF1F1F1FFB1B1
        B1FF4D4D4DFF4D4D4DFF565656FF565656FF565656FF464646FF4D4D4DFF5656
        56FF565656FF565656FF565656FF4D4D4DFF464646FF393939FF464646FF4D4D
        4DFF565656FF565656FF565656FF565656FF565656FF4D4D4DFF4D4D4DFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF4D4D4DFF000000004D4D
        4DFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem52: TMenuItem
      Action = actFoldLevel9
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF151515FF151515FF151515FF151515FF151515FF1919
        19FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF151515FFFFFFFFFFFFFFFFFFC1C1C1FF727272FF1515
        15FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF2121
        21FF212121FF212121FF191919FFC1C1C1FFC1C1C1FFFFFFFFFFFFFFFFFFD1D1
        D1FF191919FF212121FF212121FF212121FF212121FF151515FF191919FF2121
        21FF212121FF212121FF212121FF212121FF212121FF404040FFD1D1D1FFFFFF
        FFFF898989FF212121FF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF6D6D6DFFB7B7B7FFB7B7B7FF898989FFFFFF
        FFFFF1F1F1FF212121FF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3131
        31FF313131FF2D2D2DFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF565656FF313131FF313131FF313131FF212121FF2D2D2DFF4040
        40FF404040FF313131FFE3E3E3FFFFFFFFFFACACACFF2D2D2DFF666666FFFFFF
        FFFFFFFFFFFF666666FF313131FF404040FF404040FF2D2D2DFF313131FF4040
        40FF404040FF313131FFFFFFFFFFFFFFFFFF6D6D6DFF404040FF313131FFFFFF
        FFFFFFFFFFFF5C5C5CFF404040FF404040FF404040FF313131FF404040FF4040
        40FF404040FF404040FFD1D1D1FFFFFFFFFF959595FF404040FF6D6D6DFFFFFF
        FFFFF1F1F1FF404040FF404040FF404040FF404040FF404040FF404040FF4D4D
        4DFF4D4D4DFF404040FF7D7D7DFFFFFFFFFFFFFFFFFFCACACAFFF1F1F1FFFFFF
        FFFF959595FF404040FF4D4D4DFF4D4D4DFF4D4D4DFF404040FF404040FF5656
        56FF565656FF565656FF404040FF7D7D7DFFD8D8D8FFFFFFFFFFE3E3E3FF9898
        98FF404040FF565656FF565656FF565656FF565656FF404040FF4D4D4DFF5656
        56FF565656FF565656FF565656FF4D4D4DFF404040FF404040FF404040FF4D4D
        4DFF565656FF565656FF565656FF565656FF565656FF4D4D4DFF4D4D4DFF5C5C
        5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C
        5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF4D4D4DFF000000004D4D
        4DFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
  end
  object dlgColor: TColorDialog
    CustomColors.Strings = (
      'ColorA=000000'
      'ColorB=000080'
      'ColorC=008000'
      'ColorD=008080'
      'ColorE=800000'
      'ColorF=800080'
      'ColorG=808000'
      'ColorH=808080'
      'ColorI=C0C0C0'
      'ColorJ=0000FF'
      'ColorK=00FF00'
      'ColorL=00FFFF'
      'ColorM=FF0000'
      'ColorN=FF00FF'
      'ColorO=FFFF00'
      'ColorP=FFFFFF'
      'ColorQ=C0DCC0'
      'ColorR=F0CAA6'
      'ColorS=F0FBFF'
      'ColorT=A4A0A0')
    Options = [cdFullOpen, cdAnyColor]
    Left = 285
    Top = 144
  end
  object ppmEncoding: TPopupMenu
    Images = imlMain
    Left = 40
    Top = 315
  end
  object ppmLineBreakStyle: TPopupMenu
    Images = imlMain
    Left = 117
    Top = 88
  end
  object ppmSelectionMode: TPopupMenu
    Images = imlMain
    Left = 117
    Top = 32
  end
  object ppmSelection: TPopupMenu
    Images = imlMain
    Left = 117
    Top = 144
  end
  object ppmEditor: TPopupMenu
    Images = imlMain
    Left = 40
    Top = 32
  end
  object ppmInsert: TPopupMenu
    Images = imlMain
    Left = 121
    Top = 215
  end
  object ppmSearch: TPopupMenu
    Images = imlMain
    Left = 117
    Top = 280
  end
  object ppmSettings: TPopupMenu
    Images = imlMain
    Left = 200
    Top = 280
  end
  object ppmSelect: TPopupMenu
    Images = imlMain
    Left = 200
    Top = 215
  end
  object ppmFile: TPopupMenu
    Images = imlMain
    Left = 280
    Top = 224
  end
  object ppmSelectionEncode: TPopupMenu
    Images = imlMain
    Left = 120
    Top = 336
  end
  object ppmSelectionDecode: TPopupMenu
    Images = imlMain
    Left = 208
    Top = 344
  end
  object imlMain: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Left = 320
    Top = 272
    Bitmap = {
      494C010179007D00080010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000040000000F0010000010020000000000000F0
      0100000000000000000000000000000000000000000000000000000000000303
      0334050505730303033400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000060606302F2F
      2F91ECECECFF2F2F2F9106060630000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000707072F3131318DECEC
      ECFFEBEBEBFFECECECFF3131318D0707072F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000A0A0A500D0D0D6B0D0D
      0D6BEAEAEAFF0D0D0D6B0D0D0D6B0A0A0A500000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000505
      0573E8E8E8FF0505057300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000060606250E0E0E64111111730B0B
      0B75E6E6E6FF0B0B0B7511111173111111731111117311111173111111731111
      117311111173111111730E0E0E64060606250000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001B1B1B60AFAFAFD5EBEBEBFFEAEA
      EAFFE6E6E6FFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEA
      EAFFEAEAEAFFEBEBEBFFAFAFAFD51B1B1B600000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002525256CF1F1F1FFECECECFFECEC
      ECFFECECECFFECECECFFECECECFFECECECFFECECECFFECECECFFECECECFFECEC
      ECFFECECECFFECECECFFF1F1F1FF2525256C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002B2B2B6AF9F9F9FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF9F9F9FF2B2B2B6A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002929295BC0C0C0D4FEFEFEFFFEFE
      FEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
      FEFFFEFEFEFFFEFEFEFFC0C0C0D42929295B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000111111222B2B2B5A323232672B2B
      2B66FFFFFFFF2B2B2B6632323267323232673232326732323267323232673232
      326732323267323232672B2B2B5A111111220000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001B1B
      1B66FFFFFFFF1B1B1B6600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000B0B0B447A7A
      7AB3FFFFFFFF7A7A7AB30B0B0B44000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000014141461FFFF
      FFFFFFFFFFFFFFFFFFFF14141461000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001C1C1C4E9191
      91B7FFFFFFFF919191B71C1C1C4E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000505050D2525
      254E2E2E2E612525254E0505050D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000600000012000000190000001E0000001900000012000000060000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000012B00510237016600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000361C015C5C2E019900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      001000000B2F010162A404048FDF0202A0F804048FDF010162A400000B2F0000
      0010000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000F700CBE2DA92BFF0B6008AC000000010000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000391D
      025C7E4204CC7E4204CC00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000100202
      295807079FF12424B2F93A3AC3FF4040C7FF3434C1FF1F1FB1F906069FF10202
      2958000000100000000000000000000000000000000000000000000000005C2F
      02997A3E02CC7A3E02CC5C2F0299000000005C2F02997A3E02CC7A3E02CC5C2F
      0299000000000000000000000000000000000000000000000000000000000000
      00000D6B0ABB0ABC0AFF16AE15FB117B0FD80009001500000000000000000000
      00000000000000000000000000000000000000000000000000003B1F035C8246
      08CCFFBF26FF824608CC00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000600000D310C0C
      A0F14646C9FF4F4FCFFF3F3FCBFF3C3CCAFF3F3FCAFF4646CDFF3838C5FF0B0B
      A0F100000D310000000600000000000000000000000000000000000000007F42
      06CCFFBD1EFFFFB810FF7F4206CC000000007F4206CCFFB80FFFFFB508FF7F42
      06CC000000000000000000000000000000000000000000000000000000000000
      00000A6608B808D508FF00C500FF0BAE0BFC148912F001190038000000000000
      000000000000000000000000000000000000000000003E22065C884B0CCCFFC5
      38FFFFBB19FF884B0CCC884B0CCC884B0CCC884B0CCC864A0CCA80470CC16738
      0A9A2C1903420100000100000000000000000000000000000013090969A63939
      BDFA5B5BD5FF4141CEFF4141CEFF4242CEFF4242CEFF4141CEFF5050D2FF2D2D
      B9F909096AA70000001300000000000000000000000000000000000000008548
      0ACCFCBA21FFFBAF05FF85480ACC0000000085480ACCFBB515FFFBAF05FF8548
      0ACC000000000000000000000000000000000000000000000000000000000000
      0000096007B507D007FF00C400FF00B000FF049A04FE017A00F9002F006B0000
      0000000000000000000000000000000000003F25075C8E5212CCFFCC4DFFFFB2
      00FFFFB710FFFFBC1DFFFFBB1AFFFFBA18FFFFB916FFFFB814FFF7B214FCDB99
      18F0A06313D54C2B096E0100000100000000000000000000001A13139AE16363
      D6FF5252D5FF4949D3FF4949D3FF4949D4FF4949D4FF4949D3FF4F4FD5FF5252
      D2FF13139AE10000001A00000000000000000000000000000000000000008C4F
      0FCCF2B636FFEDA717FF8C4F0FCC000000008C4F0FCCF0B12BFFEDA717FF8C4F
      0FCC000000000000000000000000000000000000000000000000000000000000
      0000085A06B105BB05FF00B400FF00A600FF009400FF007E00FF117710FB0546
      03A200000001000000000000000000000000945716CCF9D387FFF3B03AFFF6AC
      0DFFF6AC0BFFF6AC0BFFF6AC0BFFF6AC0BFFF6AC0BFFF6AC0BFFF6AC0BFFF6AD
      0DFFEEAB1EFBA5681AD52F1C074200000000000000000000001E1616B0FB7C7C
      E0FF5454D9FF5151D8FF5151D9FF5151D9FF5151D9FF5151D9FF5353D9FF6969
      DCFF1616B0FB0000001E00000000000000000000000000000000000000009457
      15CCE8B24DFFDD9C2CFF945715CC00000000945715CCE5AB43FFDD9C2CFF9457
      15CC000000000000000000000000000000000000000000000000000000000000
      0000045404AF03A603FF00A100FF009700FF008800FF027702FF006200FF095A
      09FB065106CC000000010000000000000000462A0B5C9B5E1BCCFAD68AFFEDAD
      49FFEDAF36FFF1B843FFF0B741FFF0B63FFFEFB43BFFECAC2FFFE9A420FFE8A3
      1EFFE9A420FFD39932F07547159A00000000000000000000001A1A1AA0E37878
      DFFF6464DFFF5858DDFF5959DEFF5959DEFF5959DEFF5959DEFF6060DFFF6868
      DCFF1A1AA0E30000001A00000000000000000000000000000000000000009B5E
      1BCCE3B360FFD0933EFF9B5E1BCC000000009B5E1BCCDCA855FFD0933EFF9B5E
      1BCC000000000000000000000000000000000000000000000000000000000000
      0000034D02AC29A529FF24A324FF29A029FF2D992DFF309030FF358835FF3178
      31FC0A4E09CB00000001000000000000000000000000482C0E5CA2661FCCFBD9
      8DFFF4C475FFA2661FCCA2661FCCA2661FCCA46921CEB57C31D9DEA94CF9DA9B
      33FFDA9932FFDEA647FC995F1EC1000000000000000000000013151572A95A5A
      CEFB8686E8FF6161E1FF6262E1FF6262E1FF6262E1FF6262E1FF7D7DE6FF5151
      CBFB151573AA000000130000000000000000000000000000000000000000A367
      20CCF3C578FFE1A558FFA36720CC00000000A36720CCDDAB5EFFCC9043FFA367
      20CC000000000000000000000000000000000000000000000000000000000000
      0000014701A93DA43DFF3BA23BFF3FA23FFF449E44FF489A48FF317C31FB0235
      02990000000100000000000000000000000000000000000000004B300F5CA86C
      24CCFDDD91FFA86C24CC0000000000000000060502094F331161B88137D9D8A2
      51FFD39746FFE2AF5FFFA66B23CA000000000000000000000006020210332525
      B0F18787E4FF9090EAFF7777E5FF7070E4FF7676E5FF8B8BE9FF7C7CE1FF2525
      B1F202021033000000060000000000000000000000000000000000000000AA6E
      26CCF7CB7EFFF1B669FFAA6E26CC00000000AA6E26CCECBE71FFE0A558FFAA6E
      26CC000000000000000000000000000000000000000000000000000000000000
      0000003F00A654A854FF54A954FF58A958FF5BA65BFE2B742BF9001E00600000
      0000000000000000000000000000000000000000000000000000000000004D33
      115CAE7228CCAE7228CC00000000000000000000000008050209B0742BCEF9CE
      82FFF6C275FFF9CD81FFAE7228CC000000000000000000000000000000100B0B
      325B2929B5F36969D3FB9494E7FFA4A4EDFF9292E7FF6464D2FB2929B5F30B0B
      325B00000010000000000000000000000000000000000000000000000000B075
      2BCCFBD589FFF7C477FFB0752BCC00000000B0752BCCFBD286FFF7C477FFB075
      2BCC000000000000000000000000000000000000000000000000000000000000
      0000003800A66CB36CFF6CB46CFF69AD69FD206220EB000D0031000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005035135C8559229900000000000000000000000000000000B3772CCCFDD6
      8AFFFBCB7EFFFCD589FFB3772CCC000000000000000000000000000000000000
      0010030310321E1E77A82B2BA9E22C2CBDFB2B2BA9E21E1E77A8030310320000
      001000000000000000000000000000000000000000000000000000000000B67A
      2FCCFFE094FFFEDA8EFFB67A2FCC00000000B67A2FCCFFDF93FFFEDA8EFFB67A
      2FCC000000000000000000000000000000000000000000000000000000000000
      0000003200A685C185FF6EAA6EFD144C14CD0004001200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B77B2FCCFFDF
      93FFFFDA8EFFFFDE92FFB77B2FCC000000000000000000000000000000000000
      000000000006000000130000001A0000001E0000001A00000013000000060000
      0000000000000000000000000000000000000000000000000000000000008B5F
      2599BA7E32CCBA7E32CC8B5F2599000000008B5F2599BA7E32CCBA7E32CC8B5F
      2599000000000000000000000000000000000000000000000000000000000000
      0000003200A67AAC7AFE0734079B000000010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008B5F2599BA7E
      33CCBA7E33CCBA7E33CC8B5F2599000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000016004A021D025C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001D0F052B9D6D4CC9A4785AC9A4795BC9A17152C9A17051C9A881
      66C9A47858C9A77C60C9A77756C9000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009E6C4BC9B8967FC9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9B39D
      8DC9C9C9C9C9BFA18AC9A06F4FC9000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001F10
      052FAE7954DECA936DF8CC9873F8CC9773F8CC9673F8D4A98BF8CF9E7CF8CD99
      76F8D39D79F8BC977CC99D6947C9000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002A87
      56DF298153FF298153FF277D50FF277B4FFF26784CFF26774CFF26774CFF2677
      4CFF247349DC0000000000000000000000000000000000000000000000000000
      00000000000000000098000000CC000000CC000000CC000000CC000000980000
      000000000000000000000000000000000000000000000000000000000000AF77
      53DECBA68CDEF1EAE6F8F4EFEBF8F8F8F8F8F8F8F7F8DFC7B6F8F8F8F8F8EDCC
      B3F8CB9571F8BC967FC99D6B49C9000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000031A0
      66FF40A36FFF40A36FFF41A36FFF38A069FF359E66FF319C63FF2D9A60FF2D9A
      60FF277D50FF0000000000000000000000000000000000000000000000000000
      000000000000000000BA0000002F00000000000000000000002F000000BA0000
      0000000000000000000000000000000000000000000024120636C88B61FFD199
      72FFD09A74FFCD9068FFCD8E67FFD6A482FFD09870FFD49E7AFFD4976DFFE6B8
      97F8C3855CF8B89178C99F6F4FC9000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000031A0
      66FF43B97EFF32A367FF319F65FF309C64FF309A62FF2F9861FF2F955EFF3AA1
      6CFF277D50FF0000000000000000000000000000000000000000000000000000
      000000000000000000830000002C00000000000000000000002C000000830000
      00000000000000000000000000000000000000000000C9895FFFEABFA1FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFE3C7B3FFFFFFFFFFF3CDB0FFCC8D64FFE7B7
      9AF8C5875DF8B58E73C99E6D4CC9000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000051000051860000868200008200000000000000000000000033A6
      69FF54BD89FF35A96BFF33A569FF32A367FF319F65FF309C64FF309A62FF46A6
      73FF298153FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C78A60FFE4AF8CFFFFF8
      F1FFFFFBF7FFFFFFFFFFFFFFFFFFCC936DFFFFFFFFFFEFC09EFFC7865AFFE3B9
      9CF8C8946FF8B88F77C99A6641C900000000280000287E00007E4D00004D0000
      000071000071320000323A00003A0000000052000052320000321E00001E1600
      00163A00003A490000497E00007EA10000A100000000000000000000000035AB
      6CFF5ABE8BFF38AF70FF36AD6DFF35A96BFF33A669FF33A367FF319F65FF48A7
      76FF2A8656FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C17E52FFDDA885FFFDFC
      FBFFFDFBFAFFFEFDFCFFFEFDFDFFFEFCFBFFFDFCFAFFEFBFA1FFC8885CFFD0B1
      8DF8C6926BF8BC8F79C999633FC9000000008300008306000006AD0000AD2B00
      002B9F00009F2E00002E7600007600000000A60000A6650000653E00003E2E00
      002E76000076AC0000AC0C00000C9A00009A0000000000000000000000003DB1
      73FF63BF91FF45B57AFF3EB275FF38AF70FF36AD6DFF35A96BFF33A569FF50AA
      7CFF2B8C59FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BF845CFFD89E77FFD89F
      77FFD9A078FFD9A079FFDBA37BFFDDA680FFDDA57DFFEAB899FFCA8D64FFD9B7
      98F8C18A62F8BCA292C99C6742C90000000086000086CD0000CD8B00008B1500
      00159C00009C2E00002E7800007800000000A60000A6650000653E00003E2E00
      002E76000076B10000B1000000006E00006E00000000000000000000000044B5
      79FF73C29BFF53BD86FF4BB980FF45B57AFF3EB275FF38AF70FF36AD6DFF5CB0
      86FF2E925DFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BB7D56FFD49A72FFD99E
      76FFDB9F78FFDCA17AFFDEA37CFFE1A781FFE2A781FFE6B592FFC98B60FFE7AD
      92F8BF7C50F8996845C9713E1E9B00000000870000877E00007EAA0000AA4D00
      004DCD0000CD42000042C90000C97B00007BA60000A665000065A50000A5A300
      00A342000042660000668B00008BBA0000BA00000000000000000000000053BC
      86FF71C39AFF60C290FF59BF8BFF53BD86FF4BB980FF45B57AFF3EB275FF5DB1
      87FF2F9962FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B06F43FFD59C78FFF3F5
      EDFFEDF1E5FFEFF0E6FFEFF1E6FFEDF1E6FFF3F3EAFFEAB697FFC48153FFD0B3
      A2DEAC7249DE0000000000000000000000000000000000000000000000001400
      0014740000740000000000000000000000005200005200000000000000000000
      00000000000000000000000000000000000000000000000000000000000053BC
      86FF7CC5A0FF7CC5A0FF83C8A6FF7AC59FFF75C59EFF74C49BFF69C194FF69C1
      94FF2F9962FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AA6839FFDD9C7AFFE4F4
      E9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEAF3E8FFEFB69AFFC37E50FFA973
      4CDE7D4522AC0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002800002800000000000000000000
      0000000000000000000000000000000000000000000000000000000000005AAC
      82DF5FC28FFF5FC28FFF5AC08CFF57BE89FF54BD87FF4EB982FF4BB880FF4BB8
      80FF369964DB0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AB693DFFDDA285FF0E4D
      EEFF1E4AFAFF0E4DEEFF0E4DEEFF1E4AFAFF0E4DEEFFEFCEBAFFC68354FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A65E2DFFAB6C43FFCD8B
      6BFFD78A6DFFDA8B6CFFDC8D6BFFE18F6FFFD38B67FFC38457FF8F5027C50000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009DC4B3FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009DC4B3FF0F5A3EFF9DC4
      B3FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000098000000CC0000
      00CC000000CC000000CC000000980000000000000000135F42FF1F7452FF135D
      41FA000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000D1947002345CC002344CC000C1748000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0072000000E00000001C00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000BA0000002F0000
      0000000000000000002F000000BA000000000000000000010105186447FF0000
      00000000000000000000737373FF737373FF737373FF737373FF737373FF7373
      73FF000000000000000000000000000000000000000000000000000000000000
      000000000000000C1748002243CC5394B7FF33669AFF002547CA000C18480000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000010101B80101018300000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000830000002C0000
      0000000000000000002C000000830000000000000000000201051E6A4CFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000131F41003658BB39709FFF376E9DFF5E9FC0FF4477ABFF002A4CC6000E
      1A46000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000066010101CC00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000020105247152FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000000C
      1748002243CC4E8DB3FF67ACC8FF4780ACFF4F87B3FF69AAC8FF5488BBFF0031
      52C100101C440000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000101
      0195020202C6020202C600000000000000000000000000000000000000000000
      00000000000000000000FF0000FF36160E400000000000000000000000000000
      00000000000000000000000000000000000000000000000201052A7757FF0000
      00000000000000000000838383FF838383FF838383FF838383FF838383FF8383
      83FF838383FF838383FF000000000000000000000000000000000015213F003C
      5FB65EA1C0FF3E78A3FF4177A7FF65A6C5FF609DC2FF5D95C1FF73B4D1FF6498
      CBFF000000AB0000003C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000202
      02C1020202C1020202C100000000000000000000000000000000000000002314
      0E27A05034CDFF0000FFFF0000FFFF0000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000020105307E5DFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000C1748002243CC3267
      98FF4F8CB3FF68ABC8FF4C91AFFF1A5E81FF000000FF6DA9CDFF6BA3CEFF6C6C
      6CFFAA9999FF000000A500131F42000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000101
      0160020202BE0101016000000000000000000000000000000000000000009F5A
      42AF29140D3500000000FF0000FF36160E400000000000000000000000000000
      0000000000000000000000000000000000000000000001020105368663FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000017233E004063B169AEC9FF65A8
      C5FF5592B8FF367CA6FF17849FFFD9F4FFFF3386ADFF7ABAD5FF7D7D7DFFCEC0
      C0FF787878FF5488BBFF003351A60000000000000000000000000000006D0101
      01DC0000006D000000000000006D010101DC0000006D00000000000000000000
      0000000000000000000000000000000000000000000000000000000000004326
      1C4A000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000010202053C8C68FF0000
      00000000000000000000959595FF959595FF959595FF959595FF959595FF9595
      95FF0000000000000000000000000000000000324B830C4E70B8185B7CC3296C
      8BD03F90AAF11D8CA2FE78E6F7FF1E99B7FF6FBCD2FF888888FFD3CACAFF8383
      83FF5FA4C6FF62A7C9FF003554A5000000000000000000000000010101CF0101
      01CF010101CF00000000010101CF010101CF010101CF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000102020542936EFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000001020500070B14000F17280035
      467F006880DC78E6F7FF1593ACF558AFCBFD919191FFD9D4D4FF8D8D8DFF67AC
      CEFF73B8D4FF003D5FB400152140000000000000000000000000010101C80101
      01C80101019600000000010101C8010101C80101019600000000000000000000
      00000000000000000000000000000000000000000000000000004C4C4CFF0000
      0000000000004C4C4CFF000000004C4C4CFF000000004C4C4CFF4C4C4CFF0000
      0000000000000000000000000000000000000000000001030205489973FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000002101341005D
      6FBE78E6F7FF006D7FBA00445A8E00000069DDDCDCFF949494FF6FB4D6FF80C4
      DBFF003F62B200144ECC00051848000000000000000000000000020202C20101
      01610000000000000000020202C2010101610000000000000000000000000000
      00000000000000000000000000000000000000000000000000004C4C4CFF0000
      0000000000004C4C4CFF000000004C4C4CFF000000004C4C4CFF000000004C4C
      4CFF0000000000000000000000000000000000000000010302054D9E78FF0000
      00000000000000000000A1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1
      A1FFA1A1A1FFA1A1A1FF0000000000000000000000000303032E1010107D78E6
      F7FF006B7DB500282E3F00000000000000240000006788CCDDFF87CBDDFF0041
      63AF002666CC3E71B6FF001F5CCC000A1F480000000000000000010101770101
      01A6000000000000000001010177010101A60000000000000000000000000000
      00000000000000000000000000000000000000000000000000004C4C4CFF0000
      0000000000004C4C4CFF4C4C4CFF4C4C4CFF000000004C4C4CFF4C4C4CFF0000
      0000000000000000000000000000000000000000000051A37CFF6BB699FF50A0
      7AFA000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000002B460E0E268EF7F7F7FF1F1F
      1F6C00282E3F000000000000000000000000000B1039003D5C9C003C5B9C0017
      243E00102748002B6DCC5185C9FF002464CC0000000000000000000000180202
      02BD0101015F0000000000000018020202BD0101015F00000000000000000000
      00000000000000000000000000000000000000000000000000004C4C4CFF0000
      0000000000004C4C4CFF000000004C4C4CFF000000004C4C4CFF000000004C4C
      4CFF00000000000000000000000000000000000000009DC4B3FF55A780FF9DC4
      B3FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000086C18080FFFF1B1B357C0B0B
      0B24000000000000000000000000000000000000000000000000000000000000
      00000000000000112948002F72CC000F27480000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004C4C4CFF4C4C4CFF4C4C
      4CFF00000000000000004C4C4CFF00000000000000004C4C4CFF4C4C4CFF0000
      00000000000000000000000000000000000000000000000000009DC4B3FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000003140000098AF090923390000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000033000000000000
      00000000000000000000000000000000000000000000000000000000001E0000
      003300000033000000330000001E00000000000000000000000000042B990004
      38CC000438CC000438CC000438CC000438CC000438CC000438CC000438CC0004
      38CC000438CC00042B9900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000716D6AFF0000000A0000
      0000000000000000001200000029000000330000002800000023006038AB009E
      5DFF009D5CFF009E5DFF005E39AC0000001E0000000000000000000C43CCF9F9
      E9FFF3F3E2FFF3F3E2FFF3F3E2FFF3F3E2FFF3F3E2FFF3F3E2FFF3F3E2FFF3F3
      E2FFF9F9E9FF000C43CC00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000605D5BFF12110F590000
      0002000000310018307B00376FD9004288FF002F6FD5006645C000A867FF00BA
      86FF76DFC4FF00BA86FF00A669FF005E39AC0000000000000000001752CCF4F4
      E5FFE8E8D9FFE8E8D9FFE8E8D9FFE8E8D9FFE8E8D9FFE8E8D9FFE8E8D9FFE8E8
      D9FFF4F4E5FF001752CC00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005C2E0199371C015C0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000371C015C5C2E0199000000000000000000000000000000000000
      00000000000000000000000000000000000000000000534F4DFF5B4F47FF0003
      063D004388F700428CFF067BCEFF0043A1FF0673D3FF00A454FF00C18AFF00BB
      82FFFFFFFFFF00BB82FF00C08CFF009E5DFF0000000000000000001B57CCF5F5
      E7FF767676FFEAEADCFFBDBDACFFCECEBDFFC5C5B4FFCECEBDFFCECEBDFFEAEA
      DCFFF5F5E7FF001B57CC00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007D4104CC7D4104CC391D
      025C000000000000000000000000000000000000000000000000000000000000
      0000391D025C7D4104CC7D4104CC000000000000000000000000000000000000
      000000000000000000000000000000000000000000001A18165A2A3E52F10041
      8DFF0E83D6FF0953ADFF359AE2FF0043A4FF3693E9FF009F4FFF75E6CAFFFFFF
      FFFFFFFFFFFFFFFFFFFF76E5CCFF009C5BFF0000000000000000001E5ACCF6F6
      EAFFECECE0FFECECE0FFECECE0FFECECE0FFECECE0FFECECE0FFECECE0FFECEC
      E0FFF6F6EAFF001E5ACC00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000824608CCFFB913FF8246
      08CC3B1F045C0000000000000000000000000000000000000000000000003B1F
      045C824608CCFFC027FF824608CC000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000003E86F20F5F
      B9FF339BE2FF0D58B3FF4BACECFF0045A9FF4DA6F4FF009F4DFF00CC94FF00C8
      8EFFFFFFFFFF00C88FFF00CC98FF009D5CFF000000000000000000205ECCF7F7
      EDFFFF9965FFEFEFE4FFC2C2B1FFD3D3C2FFD3D3C2FFD9D9CBFFEFEFE4FFEFEF
      E4FFF7F7EDFF00205ECC00000000000000000000000000000000884B0CCC0000
      000066390A99884B0CCC66390A99884B0CCC66390A99884B0CCCFFB80FFFFFBB
      1AFF884B0CCC3D22065C000000000000000000000000000000003D22065C884B
      0CCCFFC63AFFFFBB1AFF884B0CCC66390A99884B0CCC66390A99884B0CCC6639
      0A9900000000884B0CCC00000000000000000000000000000000003F89FF2777
      D1FF5DB4EEFF2069BFFF73C5F9FF0048B0FF75C2FFFF0A8682FF00AF67FF00D4
      9AFF75EED3FF00D49EFF00B071FF005E38960000000000000000002362CCF9F9
      F0FFF2F2E8FFF2F2E8FFF2F2E8FFF2F2E8FFF2F2E8FFF2F2E8FFF2F2E8FFF2F2
      E8FFF9F9F0FF002362CC00000000000000000000000000000000000000000000
      00008D5111CCF8BF3DFF8D5111CCF7BC35FFB77610E1F6B423FFF5AF17FFF5AB
      0DFFF7B92DFF8D5111CC4025085C00000000000000004025085C8D5111CCF9CD
      70FFF5AC14FFF6B11DFFF6B62AFFB77610E1F7BA31FF8D5111CCF7B72AFF8D51
      11CC000000000000000000000000000000000000003300000020003C87FF4A91
      DEFF8CCEFBFF4385CEFFA4E2FFFF004DB9FFA6E1FFFF4981D5FF3BB5A0FF009F
      4EFF00A154FF00A35AFF005833A0000000000000000000000000002665CCFAFA
      F3FF6D70C2FFF4F4EDFFD0D0BFFFD8D8C7FFD0D0BFFFD8D8C7FFD8D8C7FFE1E1
      D0FFFAFAF3FF002665CC0000000000000000945716CC000000006F4211999457
      16CC945716CC945716CC945716CC995C17CFB3741DE1D19025F5E19E28FFE19E
      28FFE19E28FFEAB348FF945716CC43270A5C43270A5C945716CCFAD68AFFEEB2
      65FFEDB05BFFE7A63BFFD29027F5B3741DE1995C17CF945716CC945716CC9457
      16CC945716CC6F42119900000000945716CC706D69FF49433DB2003B87FF6EAB
      EAFFA0DAFFFF2B79CEFF1C6FCAFF0053C1FF1D74D2FF2D79D0FFACE1FFFF729D
      E2FF00348CFF463838A57B666AFF000000000000000000000000002869CCFBFB
      F6FFF7F7F1FFF7F7F1FFF7F7F1FFF7F7F1FFF7F7F1FFF7F7F1FFF7F7F1FFF7F7
      F1FFFBFBF6FF002869CC000000000000000000000000000000009B5E1BCCF8D0
      84FF9B5E1BCCF7CD81FFA46722D2EDC070FFBD8035E8D39643FFD09340FFD093
      40FFD39643FFD89B48FFECBB6BFF9B5E1BCC9B5E1BCCFCDC90FFEEB366FFEEB3
      66FFEEB366FFEEB366FFEAAE5FFFC2873BE8E8BB69FFA1641FD2E3B361FF9B5E
      1BCCE2B05EFF9B5E1BCC0000000000000000272524636B6157FF003C88FF0064
      DCFF0F6FD4FF519AE2FF6DB0EBFF6DB4EFFF6DB2EFFF5199E1FF1770D0FF0054
      C5FF003B8AFF6C6058FF26242360000000000000000000000000002A6CCCFDFD
      F9FF21AA32FFFAFAF5FFCCCCBBFFDDDDCCFFE5E5D4FFE3E3D8FFFAFAF5FFFAFA
      F5FFFDFDF9FF002A6CCC0000000000000000794D1899A1651FCCA1651FCCA165
      1FCCA1651FCCA46822CEB2762ED7C3883EE3D69A50EFE6AA5EFAEEB265FFEEB2
      65FFEEB265FFF7CC80FFA1651FCC482D0E5C482D0E5CA1651FCCFEE094FFF1B8
      6BFFF1B86BFFF1B86BFFE9AF63FAD89D54EFC58B41E3B2772FD7A46822CEA165
      1FCCA1651FCCA1651FCCA1651FCC794D18990000000000000011003D8AFF5BB1
      F8FF6CBDF6FF53B1F2FF49ACF1FF49ACF1FF49ACF1FF53B1F2FF66BAF6FF61B1
      F2FF00428EFF0000001100000000000000000000000000000000002C6ECCFEFE
      FBFFFCFCF9FFFCFCF9FFFCFCF9FFFCFCF9FFFCFCF9FFFCFCF9FFFCFCF9FFFCFC
      F9FFFEFEFBFF002C6ECC0000000000000000A86C24CCFBD98DFFA86C24CC0000
      0000A86C24CCFBD68AFFB77C33D7FAD488FFEBBD70F7F6C97CFFF4C174FFF2B9
      6CFFFAD68AFFA86C24CC4B30105C00000000000000004B30105CA86C24CCFFE3
      97FFF6C275FFF8C97CFFF9D084FFECC175F7FBD78BFFB87E35D7FBD589FFA86C
      24CC00000000A86C24CCFAD185FFA86C24CC000000001A171678083F78FF1778
      C6FF2794E2FF2791DEFF2790DDFF2790DCFF2790DDFF2791DEFF2795E2FF1678
      C6FF073F78FF1A17167800000000000000000000000000000000002E71CCFFFF
      FEFFFEFEFCFFDCDCDBFFDCDCDBFFDCDCDBFFDCDCDBFFDCDCDBFFDCDCDBFFFEFE
      FCFFFFFFFEFF002E71CC000000000000000081541E99AD7128CC81541E990000
      000081541E99AD7128CC81541E99AD7128CCAD7128CCAD7128CCFBD488FFFEDF
      93FFAD7128CC4D33125C000000000000000000000000000000004D33125CAD71
      28CCFFE599FFFDD88CFFAD7128CCAD7128CCAD7128CC81541E99AD7128CC8154
      1E990000000081541E99AD7128CC81541E990000000056534FFF58514DFF0035
      74D700539DFF046FC1FF0378CEFF0378CEFF0378CFFF0470C2FF00539DFF0035
      74D758514DFF56534FFF00000000000000000000000000000000003073CCFFFF
      FFFFA3A2A2FF8B8989FF9A9898FFA7A5A5FFACAAAAFFA19F9EFF929090FFA7A6
      A6FFFFFFFFFF003073CC00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B3772CCCFFE498FFB377
      2CCC5035145C0000000000000000000000000000000000000000000000005035
      145CB3772CCCFFE599FFB3772CCC000000000000000000000000000000000000
      0000000000000000000000000000000000000000000065615FFF000000000000
      00000020437B003A7BE9003D89FF003C83FF003D7EFF003C7FE90021447B0000
      00000000000065615FFF00000000000000000000000000000000003275CC6195
      D9FF3467ACFFD2C7C7FFD8CFCFFFE4DEDEFFE4DEDEFFD8CFCFFFD2C7C7FF3467
      ACFF6195D9FF003275CC00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B77B2FCCB77B2FCC5237
      155C000000000000000000000000000000000000000000000000000000000000
      00005237155CB77B2FCCB77B2FCC000000000000000000000000000000000000
      0000000000000000000000000000000000000000000073706CFF000000000000
      0000000000005A4C40FFACA298FF92887EFF665C54FF352920FF000000000000
      00000000000073706CFF00000000000000000000000000000000002659990033
      77CC003377CC00276DD0626B92E6DCDADAFFDCDADAFF626B92E600276DD00033
      77CC003377CC0026599900000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008B5F26995439175C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000005439175C8B5F2699000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000393632B0454241FF3D3A39FF363433FF1E1B1A9B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000102061A1A1A5221212167212121671A1A1A52000102060000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000042B990004
      38CC000438CC000438CC000438CC000438CC000438CC000438CC000438CC0004
      38CC000438CC00042B9900000000000000000000000000000000000000000000
      000000000000000000000505054E1A1A1A950101013E00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000005C000000000000005C000000000000005C000000000000005C0000
      00000000005C000000000000000000000000000000000000001D000000340000
      0036000000360000003600000036000000360000003600000036000000360000
      003600000036000000330000001D000000000000000000000000000C43CC386B
      B0FF3568ADFF3467ACFF3366ABFF3366ABFF3366ABFF3265AAFF3265AAFF3164
      A9FF3164A9FF000C43CC00000000000000000000000000000000000000000000
      000000000000010101183131319BFFFFFFFF1D1D1D9300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      005C000000000000000000000000000000000000000000000000000000000000
      0000000000000000005C00000000000000000000000000000034EFEFEFF5FAFA
      FAFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFAFAFAFDEDEDEDF300000033000000000000000000000000001752CC3669
      AEFF3063A8FF3063A8FF3063A8FF3063A8FF3063A8FF3063A8FF3063A8FF3063
      A8FF3164A9FF001752CC000000000000000000000000070707630303032F0000
      00000000000005050553B2B2B2DEECECECF60C0C0C7100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000070000000FF000000FF000000FF000000FF000000FF0000
      0070000000000000000000000000000000000000000100000036FBFBFBFEFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFAFAFAFD00000036000000000000000000000000001B57CC3669
      AEFF2D60A5FF2D60A5FF2D60A5FF2D60A5FF2D60A5FF2D60A5FF2D60A5FF2D60
      A5FF2F62A7FF001B57CC0000000000000000000000000C0C0C81232323910404
      042D0202021736363698FFFFFFFF6E6E6EBD0404043100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      005C00000000000000FFFFFFFFFFFFFFFFFFBFBFBFFFFFFFFFFFFFFFFFFF0000
      00FF000000000000005C00000000000000000000000100000036FCFCFCFFFCFC
      FCFFDDDDDDFFDBDBDBFFD9D9D9FFD7D7D7FFD5D5D5FFD4D4D4FFD4D4D4FFD4D4
      D4FFFBFBFBFFFCFCFCFF00000036000000010000000000000000001E5ACC376A
      AEFF2B5EA2FF2B5EA2FF2B5EA2FF2B5EA2FF2B5EA2FF2B5EA2FF2B5EA2FF2B5E
      A2FF2F62A7FF001E5ACC0000000000000000000000000F0F0F7EDEDEDEEF2626
      268F0B0B0B5FB3B3B3DCEDEDEDF61616166F0000000700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000070000000FF000000FFFFFFFFFF000000FF000000FF0000
      0070000000000000000000000000000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFFFAFAFAFFFAFA
      FAFFFAFAFAFFFCFCFCFF0000003600000001000000000000000000205ECC376A
      AFFF285BA0FF285BA0FF285BA0FF285BA0FF285BA0FF285BA0FF285BA0FF285B
      A0FF2F62A7FF00205ECC0000000000000000000000001212127AFFFFFFFFDEDE
      DEEF5B5B5BA9FFFFFFFF727272B90606062F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      005C000000000000000000000000000000FFFFFFFFFF000000FF000000000000
      0000000000000000005C00000000000000000000000100000036FCFCFCFFFCFC
      FCFFD6D6D6FFD3D3D3FFCFCFCFFFCDCDCDFFCBCBCBFFC8C8C8FFC8C8C8FFC6C6
      C6FFF8F8F8FFFCFCFCFF00000036000000010000000000000000002362CC3E71
      B6FF275A9FFF25589DFF25589DFF25589DFF25589DFF25589DFF25589DFF2558
      9DFF2F62A7FF002362CC00000000000000000000000016161678FFFFFFFFFFFF
      FFFFFFFFFFFFEDEDEDF52020207F1616167816161678161616781010105A0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FFFFFFFFFF000000FF000000000000
      0000000000000000000000000000000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF9F9F9FFF9F9
      F9FFF8F8F8FFFCFCFCFF00000036000000010000000000000000002665CC477A
      BFFF3568ADFF2B5EA3FF24579CFF23569BFF23569BFF23569BFF23569BFF2356
      9BFF3063A8FF002665CC00000000000000000000000017171775FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2D2D2D860909092F0000
      0000000000000000000000000000000000000000000000000000000000000000
      005C000000000000000000000000000000FFFFFFFFFF000000FF000000000000
      0000000000000000005C00000000000000000000000100000036FCFCFCFFFCFC
      FCFFCECECEFFCACACAFFC6C6C6FFC3C3C3FFC0C0C0FFBDBDBDFFBCBCBCFFBABA
      BAFFF6F6F6FFFCFCFCFF00000036000000010000000000000000002869CC4C80
      C4FF3D70B5FF3D70B5FF396CB1FF3164A9FF2A5DA2FF25589DFF23569BFF2356
      9BFF3467ACFF002869CC00000000000000000000000019191973FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2E2E2E840A0A0A2E000000000000
      00CD000000D90000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FFFFFFFFFF000000FF000000000000
      0000000000000000000000000000000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF6F6F6FFF3F3
      F3FFF2F2F2FFFCFCFCFF00000036000000010000000000000000002A6CCC5084
      C8FF4275BAFF4275BAFF4275BAFF4275BAFF4275BAFF4275BAFF4275BAFF4275
      BAFF4C80C4FF002A6CCC0000000000000000000000001B1B1B71FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF313131830B0B0B2E00000000000000000000
      00C7000000CB0000000000000000000000000000000000000000000000000000
      005C000000000000000000000000000000FFFFFFFFFF000000FF000000000000
      0000000000000000005C00000000000000000000000100000036FCFCFCFFFCFC
      FCFFC7C7C7FFC2C2C2FFBEBEBEFFB8B8B8FFB4B4B4FFB1B1B1FFAEAEAEFFACAC
      ACFFEDEDEDFFFCFCFCFF00000036000000010000000000000000002C6ECC5589
      CDFF477ABFFF477ABFFF477ABFFF477ABFFF477ABFFF477ABFFF477ABFFF477A
      BFFF5185C9FF002C6ECC0000000000000000000000001D1D1D6FFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF323232810B0B0B2D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FFFFFFFFFF000000FF000000000000
      0000000000000000000000000000000000000000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF1F1F1FFECECECFFEAEA
      EAFFE6E6E6FFFCFCFCFF00000036000000010000000000000000002E71CC588C
      D0FF4C80C4FF3F72B7FF3F72B7FF3F72B7FF3F72B7FF3F72B7FF3F72B7FF4C80
      C4FF5589CDFF002E71CC0000000000000000000000001E1E1E6DFFFFFFFFFFFF
      FFFFFFFFFFFF3333337F0C0C0C2C000000000000000000000000000000000000
      00BC000000C00000000000000000000000000000000000000000000000000000
      005C0000000000000070000000FF000000FFFFFFFFFF000000FF000000FF0000
      0070000000000000005C00000000000000000000000100000036FCFCFCFFF9F9
      F9FFC0C0C0FFBABABAFFB4B4B4FFAFAFAFFFAAAAAAFFA5A5A5FFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF00000036000000010000000000000000003073CC5B8F
      D3FF2B5EA3FF23569BFF295CA1FF2F62A7FF3164A9FF2C5FA4FF26599EFF2D60
      A5FF598DD1FF003073CC0000000000000000000000001F1F1F6BFFFFFFFFFFFF
      FFFF3434347D0C0C0C2B00000000000000000000000000000000000000000000
      0038000000B80000008E00000000000000000000000000000000000000000000
      000000000000000000FFFFFFFFFFFFFFFFFFBFBFBFFFFFFFFFFFFFFFFFFF0000
      00FF000000000000000000000000000000000000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFFCFCFCFFF6F6
      F6FFF4F4F4FF5757579100000020000000000000000000000000003275CC6195
      D9FF396CB1FFD2C7C7FFD8CFCFFFE4DEDEFFE4DEDEFFD8CFCFFFD2C7C7FF396C
      B1FF5F93D7FF003275CC00000000000000000000000020202069FFFFFFFF3535
      357C0D0D0D2B0000000000000000000000000000000000000000000000000000
      000000000050000000B900000047000000000000000000000000000000000000
      005C0000000000000070000000FF000000FF000000FF000000FF000000FF0000
      0070000000000000005C00000000000000000000000000000036F9F9F9FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFE7E7
      E7FF545454910000002000000002000000000000000000000000002659990033
      77CC003377CC00276DD0626B92E6DCDADAFFDCDADAFF626B92E600276DD00033
      77CC003377CC00265999000000000000000000000000212121683636367B0D0D
      0D2A0000000000000000000000000000000000000000000000000000002C0000
      002C0000004C000000B50000005C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000033E9E9E9F0F9F9
      F9FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF5454
      5491000000200000000200000000000000000000000000000000000000000000
      000000000000000102061A1A1A5221212167212121671A1A1A52000102060000
      000000000000000000000000000000000000000000001919194D0D0D0D2A0000
      0000000000000000000000000000000000000000000000000000000000AC0000
      00AD000000AF000000B10000002D000000000000000000000000000000000000
      005C000000000000005C000000000000005C000000000000005C000000000000
      005C000000000000005C0000000000000000000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      0020000000020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000412310794B2A138C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00030B0B0B30404040DF5F5F5FF4676767F7686868F91F1F1F5E161616436565
      65F2676767F75F5F5FF4404040DF0B0B0B30000000000000000058A7DDFF58A7
      DDFF58A7DDFF58A7DDFF58A7DDFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000002020289000000310000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000150B0529854A21F5854B23F51E100739000000000000
      0000000000000000000000000000000000000000000000000000000000000606
      0629404040D7848484EF9E9E9EEDAEAEAEFF979797FF707070FF777777FF9797
      97FFAEAEAEFF9E9E9EED848484EF404040D70000000058A7DDFF58A7DDFF58A7
      DDFF4AB3E4FF3CC6EFFF4FB4E5FF58A7DDFF0000000000000000000000000000
      000000000000000000000000000000000000000000000000002D2E2E2E9D0101
      014C000000030000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000010000036C3A19CC86491FFD86491FFD75411CDE0502010A0000
      000000000000000000000000000000000000704A35A9C38E67FFC08B65FF8262
      4CFF6B6866FFD3D3CEFF6C5D54FF424140FF454545FF505050FF505050FF4545
      45FF424140FF695A51FFC4C4C4FF606060EF58A7DDFF58A7DDFF354E71FF2238
      59FF58A7DDFF41C7F0FF46C9F0FF53B6E5FF58A7DDFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000418585
      85CB01010166000000110000000000002C4800007ACC00005999000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000100000208040210080402100804021008040210020100040000
      000000000000000000000000000000000000C8926BFFFFFFFFFFFFFFFFFF9090
      90FF868686FFD3D3CEFFF8F8F8FF474747FFBDBDBDFFCECECEFFC2C2C2FFADAD
      ADFF474747FFF8F8F8FFC4C4C4FF7E7E7EF758A7DDFF58A7DDFF526F96FF4662
      87FF58A7DDFF46C9F0FF4BCBF1FF51CCF1FF58A7DDFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000030000
      0055D0D0D0F70101017B00002269000080CC5454EEFF000077CC000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484EA8D8D8DFF8D8D8DFF8D8D8DFF8D8D8DFF8D8D8DFF8D8D8DFF8484
      84EA00000000000000000000000000000000CA946DFFFFFFFFFFFFFFFFFFC0C0
      BFFF7A7A7AFFD3D3CEFF888887FF4C4C4CFF646464FF929292FF787878FF6464
      64FF4C4C4CFF888888FFC4C4C4FF6B6B6BF058A7DDFF4AB3E4FF58A7DDFF58A7
      DDFF51B5E5FF4BCBF1FF51CCF1FF55CEF2FF58A7DDFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000D00000067C4C4C4FF000086CC4A4ADCFF4E4EE8FF000078CC000000000000
      0000000000000000000000000000000000000000000000000000180D052F0000
      00008D8D8DFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFF8D8D
      8DFF000000001A0E06310100000200000000CC976EFFFFFFFFFFFFFFFCFFFAFA
      F8FF9E9E9DFF919191FFE8E8E8FFDDDDDDFFC1C1C1FF919191FF9A9A99FFDADA
      DAFFDDDDDDFFC4C4C4FF919191FF2828288A58A7DDFF3CC6EFFF41C7F0FF46C9
      F0FF4BCBF1FF51CCF1FF55CEF2FF5BCFF2FF58A7DDFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000226100008DCC7171EBFF3535CEFF4040D8FF00007BC9000000000000
      00000000000000000000000000000000000000000000301A0B5B7D441EEA0000
      00008D8D8DFFF3F3F3FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFF3F3F3FF8D8D
      8DFF000000004827118966391ABC0503010BD19C72FFFFFFFFFFFEFEFCFFFEFE
      FCFFF6F6F4FFBABAB8FF8D8D8DFF9E9E9EFF8B8B8BFFDCDCDBFFEEEDEAFF8D8D
      8CFF9E9E9EFF8D8D8DFF90725FFF0202020B58A7DDFF4FB4E5FF46C9F0FF4BCB
      F1FF51CCF1FF55CEF2FF5BCFF2FF5FD0F2FF58A7DDFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      3548000093CC9B9BFEFF5F5FEEFF4848DCFF4646D5FF000082C400002A480000
      0000000000000000000000000000000000003E220E76874A21FB844920F70000
      00008D8D8DFFF7F7F7FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF7F7F7FF8D8D
      8DFF000000004A29128C884C21FE713F1CD3D49E74FFFFFFFFFFFEFEFCFFFDFD
      FBFFFDFDFCFFFDFDFBFFFDFDF9FFFCFCF8FFFBF9F7FFFBF9F5FFFBF8F4FFFBF7
      F2FFFBF5F2FFFFFFFFFFB27B59FF000000000000000058A7DDFF53B6E5FF51CC
      F1FF55CEF2FF5BCFF2FF5FD0F2FF64D2F3FF5FB9E6FF58A7DDFF58A7DDFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      99CCA8A8FFFF8E8EFFFF8686FCFF7575F2FF5C5CECFF5959E4FF00007ACB0000
      2A4800000000000000000000000000000000140A04267C441FE583481FF60000
      00008D8D8DFFCDB5A4FF9E6D4BFF9E6D4BFF9E6D4BFF9E6D4BFFCCB3A1FF8D8D
      8DFF000000004A29128C86491FFE371E0D68D5A075FFFFFFFFFFFDFDFCFFFDFD
      FBFFFDFDFAFFFCFCF9FFFCFBF7FFFBF9F5FFFBF8F4FFFBF7F3FFFBF5F2FFFAF3
      EFFFF8F2ECFFFFFFFFFFB57D5BFF00000000000000000000000058A7DDFF58A7
      DDFF58A7DDFF58A7DDFF5DB9E6FF68D3F3FF6BD9FAFF6EDFFFFF58A7DDFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      7381000099AB000099AB00009AAD000097B09C9CFFFF5C5CECFF5959E4FF0000
      80C600007BC9000078CC00002A4800000000000000000A0502135E3316B20000
      00008D8D8DFFDCCABEFFDBC8BCFFDBC8BCFFDBC8BCFFDBC8BCFFDBC8BCFF8D8D
      8DFF0000000041230F7C2715094A00000000D8A278FFFFFFFFFFFDFDFAFFFCFC
      FAFFFCFBF9FFFBFAF6FFFBF8F5FFFBF7F4FFFBF6F1FFF8F4EEFFF7F2EBFFF7F0
      EAFFF6ECE8FFFFFFFFFFB7815DFF000000000000000000000000000000000000
      0000000000000000000058A7DDFF60BCE9FF6EDFFFFF71E5FFFF58A7DDFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000353C000099AB9C9CFFFF5C5CECFF4242
      D5FF4B4BE4FF5555EFFF000078CC000000000000000000000000010000030000
      0000848484EA8D8D8DFF8D8D8DFF8D8D8DFF8D8D8DFF8D8D8DFF8D8D8DFF8484
      84EA00000000020100050000000000000000D9A378FFFFFFFFFFFCFBF9FFFCFB
      F8FFFBF9F7FFFBF7F4FFFAF7F2FFF9F5F0FFF7F3EDFFF6EFEAFFF5EBE7FFF3EA
      E4FFF2E7DEFFFFFFFFFFBA855FFF000000000000000000000000000000000000
      000000000000000000000000000058A7DDFF62C1EBFF74ECFFFF65C6EBFF58A7
      DDFF58A7DDFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000353C000098AF8080F9FF4E4E
      E1FF5353DCFF000082C400002B47000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DBA479FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFBD8762FF000000000000000000000000000000000000
      00000000000000000000000000000000000058A7DDFF65C6EBFF7CFAFFFF81FF
      FFFF58A7DDFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000009AAD8D8DFFFF9393
      FCFF00008FB80000304300000000000000000000000000000000000000000000
      000000000000020100046D3B19CE87491FFF87491FFF7B431DE8090502120000
      000000000000000000000000000000000000DCA77AFFDCA77AFFDCA77AFFDCA7
      7AFFDCA77AFFDCA77AFFDCA77AFFDCA77AFFDCA77AFFDCA77AFFDCA77AFFDCA7
      7AFFDCA77AFFDCA77AFFC08B65FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000058A7DDFF68CBEBFF85FF
      FFFF58A7DDFF0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000099ABA8A8FFFF0000
      97B00000333F0000000000000000000000000000000000000000000000000000
      00000000000000000000160C052B854921F5874D25F52815094C000000000000
      000000000000000000000000000000000000DBAA84FDE8B992FFE8B992FFE8B9
      92FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B9
      92FFE8B992FFE8B992FFBF8F6DFD000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000058A7DDFF6BCB
      EBFF6DCBEBFF58A7DDFF58A7DDFF000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000353C000099AB0000
      363D000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000004224107C5A3216A700000000000000000000
      000000000000000000000000000000000000472F216BD3A987F4DCA77AFFDCA6
      79FFDAA479FFD8A278FFD5A075FFD49E74FFD29D72FFCF9A71FFCE996FFFCB96
      6EFFC9946BFFBB9374F4472F216B000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000058A7
      DDFF6FCBEBFF93FFFFFF58A7DDFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000010000030503010B00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000058A7DDFF58A7DDFF58A7DDFF000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000D381D881765
      33F2186A36FF176533F20D381D88000000000000000000000000000000000000
      00000000000000000000000D1947002345CC002344CC000C1748000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C371C84278C52FF63BA
      8DFF95D2B2FF63BA8DFF278C52FF0D3A1E8C0000000000000000000000000000
      000000000000000C1748002243CC5394B7FF33669AFF002547CA000C18480000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001A6836F761BA8BFF5FBA
      87FF000000005FB987FF66BC8FFF186735F70000000000000000000000000000
      000000131F41003658BB39709FFF376E9DFF5E9FC0FF4477ABFF002A4CC6000E
      1A46000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000307A4BFF9CD4B6FF0000
      0000000000000000000095D2B2FF186A36FF000000000000000000000000000C
      1748002243CC4E8DB3FF67ACC8FF4780ACFF4F87B3FF69AAC8FF5488BBFF0031
      52C100101C440000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000418359F790D3B1FF92D6
      B1FF0000000064BC8CFF66BC8FFF186735F700000000000000000015213F003C
      5FB65EA1C0FF3E78A3FF4177A7FF65A6C5FF609DC2FF5D95C1FF73B4D1FF6498
      CBFF000000AB0000003C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000030B0B0B304040
      40DF5F5F5FF4676767F7686868F91F1F1F5E161616435B7966F960AB81FF95D4
      B4FFBAE6D0FF69BB8FFF2C8F56FF0D3A1E8C00000000000C1748002243CC3267
      98FF4F8CB3FF68ABC8FF66A7C6FF4C80B3FF70B1CEFF6DA9CDFF6BA3CEFF6C6C
      6CFFAA9999FF000000A500131F42000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000A00000017000000190000
      0016000000120000000D0000000900000007000000080000000A0000000E0000
      00120000001600000019000000180000000F0000000006060629404040D78484
      84EF9E9E9EEDAEAEAEFF979797FF707070FF777777FF979797FF85A18FFF5B94
      70FE4E8E65FF408156FD193D2691000000000017233E004063B169AEC9FF65A8
      C5FF5592B8FF4A80AFFF5C97BFFF76B9D2FF659DC8FF7ABAD5FF7D7D7DFFCEC0
      C0FF787878FF5488BBFF003351A6000000000101011404040461050505770404
      0461010101140000000000000000000000000000000000000000000000000000
      0D21000042A6000052CC000042A600000D210000052F000042AB000052CC0000
      42AB0000063D0000001A000000120000000E0000000F000000140000001B0000
      00310404046705050577040404670000002B0000000012121269606060EF4444
      44743B3B3BB93F3F3FFA454545FF505050FF505050FF454545FF3F3F3FFA3B3B
      3BB944444474606060EF121212690000000000324B830C4E70B8185B7CC3296C
      8BD04F93AEE771B6CDFB6DACCCFF659DC8FF83C7DAFF888888FFD3CACAFF8383
      83FF5FA4C6FF62A7C9FF003554A5000000000D0D0D5E767676BDE4E4E4FF7676
      76BD0D0D0D5E000000000000000000000000000069CC00001B33000000000000
      56A71C1CADE83333EBFF1C1CADE8000056A7000056A71C1CADE83333EBFF1C1C
      ADE8000056A70000000000000D33000069CC0000000000000000000000000D0D
      0D5E767676BDE4E4E4FF767676BD0D0D0D5E000000001A1A1A8A7E7E7EF73B3B
      3B670303030A3B3B3BF3BDBDBDFFCECECEFFC2C2C2FFADADADFF3B3B3BF30303
      030A3B3B3B677E7E7EF71A1A1A8A000000000001020500070B14000F17280018
      2541002D44770A4867AB40859FDB77B7D2FC919191FFD9D4D4FF8D8D8DFF67AC
      CEFF73B8D4FF003D5FB400152140000000001D1D1D6FF2F2F2FFF2F2F2FFF2F2
      F2FF1D1D1D6F212121662121216600000000000066B3000075CC000000000000
      75CC5555F5FF5555F5FF5555F5FF000075CC000075CC5555F5FF5555F5FF5555
      F5FF000075CC00000000000075CC000066B30000000021212166212121661D1D
      1D6FF2F2F2FFF2F2F2FFF2F2F2FF1D1D1D6F000000000E0E0E4E6B6B6BF0C6C6
      C6EE3E3E3EB6494949FC646464FF929292FF787878FF646464FF494949FC3E3E
      3EB6949494E16B6B6BF00E0E0E4E000000000000000000000000000000000000
      0000000000000006091000293F6D00000069DDDCDCFF949494FF6FB4D6FF80C4
      DBFF003F62B200144ECC000518480000000022222257919191B8FDFDFDFF9191
      91B82222225700000000000000000000000000007FCC00001E33000000000000
      68A73939C1E87171FEFF3939C1E8000068A7000068A73939C1E87171FEFF3939
      C1E8000068A70000000000001E3300007FCC0000000000000000000000002222
      2257919191B8FDFDFDFF919191B82222225700000000010101062828288A9191
      91FFE8E8E8FFDDDDDDFFC1C1C1FF707070DE5D5D5DC2D3D3D3F8DDDDDDFFC4C4
      C4FF919191FF2828288A01010106000000000000000000000000000000000000
      0000000000000000000000000000000000240000006788CCDDFF87CBDDFF0041
      63AF002666CC3E71B6FF001F5CCC000A1F480808081128282854313131672828
      2854080808110000000000000000000000000000000000000000000000000000
      152100006DA6000086CC00006DA6000015210000152100006DA6000086CC0000
      6DA6000015210000000000000000000000000000000000000000000000000808
      08112828285431313167282828540808081100000000000000000202020B3030
      30757C7C7CEE9E9E9EFF6C6C6CE1171717370A0A0A186B6B6BDD9E9E9EFF7C7C
      7CEE303030750202020B00000000000000000000000000000030000000BC0000
      001800000018000000BC0000003000000000000B1039003D5C9C003C5B9C0017
      243E00102748002B6DCC5185C9FF002464CC0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000017000000B9000000460000
      00000000000000000046000000B9000000170000000000000000000000000000
      00000000000000112948002F72CC000F27480000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000005B00000072000000000000
      00000000000000000000000000720000005B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000017000000B3000000430000
      00000000000000000043000000B3000000170000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000002C000000B00000
      001600000016000000B00000002C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000003030316000000B90000
      00D30000001D0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000704A35A9C38E67FFC08B65FFBE8863FFBB8560FFB9835EFFB47D5BFFB17A
      57FFAE7856FFAD7555FFA97050FF704A35A90000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000011111166C8C9C9FFA59F
      9FFF010101980000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C8926BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA97150FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000300000008E0000
      00000000008E0000003000000000000000000000000011111166DCDDDDFFBAAE
      AEFF010101980000000000000000000000000000000000000000000000000304
      0507222D3D573E5371A85675A1E95A7CAFF5587CB2F65477AFF43D5884C22030
      49720B111A290000000000000000000000000000000000000000000000000000
      0000D8A278FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFEFFFFFEFEFFFFFFFFFFB7815DFF0000000000000000B07957FFB079
      57FFB07957FF00000000DD9BD9FFDD9BD9FFDD9BD9FF00000000B176FFFFB176
      FFFFB176FFFF00000000000000000000000000000017000000B9000000180000
      000000000018000000B900000017000000000000000011111166B4B4B4FF8F8F
      8FFF01010198000000000000000000000000000000000000000081868DB192A6
      C2FB85AADDFF8DB4E9FF9BBBE9FFA5C0E7FFA5BFE6FFA1BDE7FF90B1E3FF6B9A
      E3FF5F85C2FE677790CA01010204000000000000000000000000000000000000
      0000D9A378FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FEFFFFFEFEFFFEFEFEFFFFFFFFFFBA855FFF0000000000000000B07957FFB079
      57FFB07957FF00000000DD9BD9FFDD9BD9FFDD9BD9FF00000000B176FFFFB176
      FFFFB176FFFF000000000000000000000000000000720000005B000000000000
      0000000000000000005B00000072000000000004070F063A5BB19CDBDBE99BDB
      DBE907395AB7000407100000000000000000000000009AA0A8C69CBCE3FF9DC0
      ECFFB1C3DBEEBEC6D4E6C8CBCEE0CCCCCCDECCCCCCDECCCCCCDECBCBCBDEB5C0
      D2E691B3E7FF6596E1FF6680A8E600000001000000000000000062412E94AB7D
      5BE0DBA479FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFBD8762FF0000000000000000B07957FFB079
      57FFB07957FF00000000DD9BD9FFDD9BD9FFDD9BD9FF00000000B176FFFFB176
      FFFFB176FFFF00000000000000000000000000000017000000B3000000170000
      000000000017000000B300000017000000000014203D26556EA999D9D9E897D9
      D9E826546EAE00131F4000000000000000007F838799ADC8E8FFC0D3EAFFE0E6
      EDFFEFEFEFFFE9DAD1FFE1B9A1FFDA9C76FFD99870FFDFB095FFE8D4C9FFF0F0
      F0FFE6E9EEFFB5C9E9FF6596E2FF647899C60000000000000000AF805EE0E0E0
      E0E0DCA77AFFDCA77AFFDCA77AFFDCA77AFFDCA77AFFDCA77AFFDCA77AFFDCA7
      7AFFDCA77AFFDCA77AFFDCA77AFFC08B65FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000002C000000840000
      0000000000840000002C0000000000010204012D4A855F909FC38BD1D1E389D1
      D1E360919FC6012C4A8F0001020400000000B5C4D3EED7E0E9FFF0F0F0FFF4F4
      F4FFF2E4DCFFE7B79AFFE2A17AFFE19D72FFDF986BFFDE9363FFE2A27BFFEFDB
      CFFFF4F4F4FFF0F0F0FFCBD5E7FF759DD9F90000000000000000BD8E69E0E0E0
      E0E0DDAD86FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B9
      92FFE8B992FFE8B992FFE8B992FFBF8F6DFD0000000000000000B2EBD0FFB2EB
      D0FFB2EBD0FF000000006CCC4FFF6CCC4FFF6CCC4FFF00000000EBB05FFFEBB0
      5FFFEBB05FFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000001E3355215470A399D7D7E675C7C7DE7CCA
      CADFA0DADAE6235570AD011E325E00000000DADDE1FCEEEEEEFFF4F4F4FFF9F9
      F9FFF0D1BEFFE9B18EFFE7AB86FFE5A67DFFE4A076FFE39B6FFFE19667FFEBBB
      9EFFF9F9F9FFF4F4F4FFEEEEEEFF93B0DCFE0000000000000000BE8F69E0E0E0
      E0E0C8B1A4EDDDB390FEDCA77AFFDCA679FFDAA479FFD8A278FFD5A075FFD29D
      72FFCF9A71FFCE996FFFC49A78FF472F216B0000000000000000B2EBD0FFB2EB
      D0FFB2EBD0FF000000006CCC4FFF6CCC4FFF6CCC4FFF00000000EBB05FFFEBB0
      5FFFEBB05FFF0000000000000000000000000000000000000000000000000000
      00000000000000000000000F19290A3E5F9385C4C7DA51BBBBDD4FBABADC5EBF
      BFDD77C8C8DF97CACDDC0D3F60A0000F192EB0C9DEFEDCE2E8FFF7F7F7FFFBFB
      FBFFF0C4ABFFECB898FFECB491FF1A120EFF18110DFFE7A479FFE59D72FFE7A4
      7CFFFBFBFBFFF7F7F7FFD2DCECFF678FCCFF56392982966D4FC4D29D74F8F2EC
      E9F8F1ECE9F8F1EBE8F8F0EBE8F8F0EBE8F8F0EBE8F8F0EBE7F8F0EAE7F8EAE7
      E4F0E0E0E0E0A67756E000000000000000000000000000000000B2EBD0FFB2EB
      D0FFB2EBD0FF000000006CCC4FFF6CCC4FFF6CCC4FFF00000000EBB05FFFEBB0
      5FFFEBB05FFF0000000000000000000000000000000000000000000000000000
      000000000000000000000029456C4E8998B862C3C3E13DB2B2D839B1B1D940B4
      B4DB55BCBCDC80CECEE35E8F9EBE012A4679C0C8CFE8ADCBE5FFBDCAD6FFFBFB
      FBFFF1C9B2FFEFBFA2FFEEBA9BFF1C140FFF1A130EFFEAAA84FFE8A57BFFE9AA
      84FFFBFBFBFFC4D2E7FF7AA6E5FF7497CDFB997052C4C4C4C4C4D9AB84F8D9AB
      84F8D9AB84F8D9AB84F8D9AB84F8D9AB84F8D9AB84F8D9AB84F8D9AB84F8D19D
      73F8C1936BE0A87A59E000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000134578490CACBD96EBEBED56FBDBDD35BB7B7D33EB1
      B1D73FB5B5DB5EBFBFDD96CBCCD9013559937C7C7C8EC5D6E6FFB7D1ECFF97AB
      BFFFD6CAC5FFEFC4A9FFEFC0A4FFEEBB9DFFEDB695FFEBB18EFFE9AB85FFEFC7
      B1FFA4BAD9FF83ACE4FF86B1F0FF7D848EA0A67C5CC4C4C4C4C4D9AE8CF7E3BA
      99F8E3BA99F8E3BA99F8E3BA99F8E3BA99F8E3BA99F8E3BA99F8E3BA99F8DCAF
      89F8CBA280E0A87D60DE000000000000000000000000000000006A6EFEFF6A6E
      FEFF6A6EFEFF0000000072AAFFFF72AAFFFF72AAFFFF0000000066D5F0FF66D5
      F0FF66D5F0FF0000000000000000000000000000000000000000000000000000
      0000000000000000000001345781A4D3D4DC96CDCDD997CDCDD889C7C7D65BB7
      B7D339B1B1D950BABADC95CFCFDC0135598F00000000A4A4A4B9C0D3E6FFBBD3
      ECFFA1B9D0FF8FA0B3FF9BA1ABFFB3A8A6FFB4A5A1FFB2A19BFF9B9DA6FF819D
      C0FF94BAECFF91B8EEFF9DADC2D200000000A67D5CC4C4C4C4C4BAA69ADAD9B4
      96F6D9AB84F8D9AA83F8D8A883F8D6A682F8D3A47FF8D0A27DF8CE9F7CF8C693
      6AF8AC8769E03E291D5E000000000000000000000000000000006A6EFEFF6A6E
      FEFF6A6EFEFF0000000072AAFFFF72AAFFFF72AAFFFF0000000066D5F0FF66D5
      F0FF66D5F0FF0000000000000000000000000000000000000000000000000000
      00000000000000000000002C496A729FACBEA7D9D9E19BCECED997CDCDD86FBD
      BDD33DB2B2D857BEBEDF699EAAC2002D4B7500000000000000008F8F8FA1C8D5
      E0F9BAD3E8FFBED5EEFFB6CFE9FFA5BED9FF9FB8D5FF9FBBDCFFAAC8F1FFA3C3
      EEFFA1C0E9FE9399A2B20000000000000000A87E5DC4C4C4C4C4C4C4C4C4C4C4
      C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C49168
      4BC40000000000000000000000000000000000000000000000006A6EFEFF6A6E
      FEFF6A6EFEFF0000000072AAFFFF72AAFFFF72AAFFFF0000000066D5F0FF66D5
      F0FF66D5F0FF0000000000000000000000000000000000000000000000000000
      000000000000000000000015233316476485AFD8D9DFA3D5D5DE94CCCCD867BB
      BBD34CB8B8DC87CCCDDF1548658C001524370000000000000000000000003434
      343C999EA1B5BFCEDDF6BCD0E5FFB8CEE6FFB4CBE6FFB1C9E6FFAFC4E0F899A3
      B1C43535353C000000000000000000000000A9805EC4A9805EC4A9805EC4A980
      5EC4A9805EC4A9805EC4A9805EC4A9805EC4A9805EC4A9805EC4A9805EC4936B
      4EC4000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000001001F344A154663826896A6B88FC2C3CF7ABB
      BDCE5592A0BA124663880020354F000000010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A88365C2B28E70C4B28E70C4B28E
      70C4B28E70C4B28E70C4B28E70C4B28E70C4B28E70C4B28E70C4B28E70C4936D
      54C2000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000014212F00294460003151730031
      5274002944620014223100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000036241952A38268BCA9805EC4A97F
      5DC4A77E5DC4A37B5AC4A37959C4A17858C49F7657C49E7555C496765CC43624
      1952000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000E7000000520000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000505050F3535358F4646
      46BF5F5F5FFF5F5F5FFF5E5E5EFF474747BF1111112F000000005E5E5EFF5E5E
      5EFF5F5F5FFF0000000000000000000000000000000000000F0F00002F2F7575
      75CF8F8F8FFF8F8F8FFF909090FF8F8F8FFF909090FF909090FF909090FF9090
      90FF888897FF7474A4FF757575CF000000000000000000000F0F00002F2F0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000F0F00002F2F0000000000000000000000E0565656FF000000E70000
      0052000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000505050F585858EF7E7E
      7EFFB4B4B4FFBBBBBBFFB3B3B3FF919191FF585858EF000000005F5F5FFFBBBB
      BBFF5E5E5EFF000000000000000000000000000000000000AFAF0000FFFF5A5A
      B9FFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFB0B0
      BFFF2222F2FF0000FFFF7373A3FF00000000000000000000AFAF0000FFFF0000
      5F5F000000000000000000000000000000000000000000000000000000000000
      0F0F0000CFCF0000FFFF00002F2F000000000000004E000000E0595959FF0000
      00E7000000520000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001111112F5858
      58EF5E5E5EFF5F5F5FFF6E6E6EFFBBBBBBFF5E5E5EFF000000005F5F5FFFBBBB
      BBFF5F5F5FFF0000000000000000000000000000000000005F5F0000FFFF0000
      FFFF7474D4FFBBBBBBFFBABABAFFBBBBBBFFBBBBBBFFBBBBBBFFB0B0BFFF2222
      F2FF0000FFFF2222F2FF878796FF00000000000000000A0A697C0000FFFF0000
      FFFF3B3B9BFF525252DF1111112F00000000000000001111112F4E4E5DE01111
      E1FF0000FFFF0F0FDEF910101F3B00000000000000000000004E000000E05D5D
      5DFF000000E70000005200000002090D01111B1F03242D2E052F242004290F0A
      0119020100060000000000000000000000000000000000000000000000000000
      000000000000000000005F5F5FFFBBBBBBFF5E5E5EFF000000005E5E5EFFB9B9
      B9FF5E5E5EFF000000000000000000000000000000000000000000005F5F0000
      FFFF0000FFFF7575D4FFBBBBBBFFBBBBBBFFB9B9B9FFB0B0BFFF2222F2FF0000
      FFFF2222F2FFB0B0BFFF909090FF000000000B0B0B1F585858EF5A5ABAFF0000
      FFFF0000FFFF5D5DBDFF535353DF000000000B0B0B1F545462EF1A1AEAFF0000
      FFFF2222F2FF8C8C9BFF535353DF0000000000000000000000000000004E0000
      00E0616161FF000000E81D3207713F5B125B697C1E7C8789248B867421866E4F
      176E44270A460B04011500000001000000000000000000000000000000000000
      000000000000000000005F5F5FFFBABABAFF5D5D5DFF000000005F5F5FFFBABA
      BAFF5E5E5EFF0000000000000000000000000000000000000000000000005A5A
      B9FF0000FFFF0000FFFF7575D4FFBABABAFFB0B0BFFF2222F2FF0000FFFF2222
      F2FFAFAFBEFFBABABAFF909090FF000000003B3B3B9F848484FFABABABFF5A5A
      BAFF0000FFFF0000FFFF3B3B9AFF00000000373746A41818E8FF0000FFFF1818
      E8FF868695FFBBBBBBFF5F5F5FFF000000000000000000000000000000000000
      004E000000E1666666FF060B02F25A8029C7B2D352D3DBDE5DE1DCBE56DCC48D
      44C49553289556210F570B020115000000000000000000000000000000000000
      0000000000000505050F5E5E5EFFBBBBBBFF5F5F5FFF000000005F5F5FFFBABA
      BAFF5F5F5FFF0000000000000000000000000000000000000000000000008989
      89FF6767C6FF0000FFFF0000FFFF6E6ED7FF2121F1FF0000FFFF1E1EEEFF9C9C
      ABFFB4B4B4FFBBBBBBFF8F8F8FFF00000000595959EFAEAEAEFF9D9D9DFF9292
      92FF5B5BBBFF0000FFFF0000FFFF000068681010E0FC0000FFFF1B1BEBFF7C7C
      8CFF878787FFBABABAFF5E5E5EFF000000000000000000000000000000000000
      0000062A0764040C03ED6B6B6BFF0F160AFE95AD5AFFF9FC8FFFFFDD89FFFAB2
      76FAD87652D895362895440C0B46010000060000000000000000000000002F2F
      2F7F535353DF5E5E5EFF6D6D6DFFBBBBBBFF5F5F5FFF000000005E5E5EFFBABA
      BAFF5F5F5FFF000000000000000000000000000000002F2F2F7F595959EF5E5E
      5EFF666666FF5A5ABAFF0000FFFF0000FFFF0000FFFF1111E1FF60606FFF9191
      91FFA4A4A4FFB9B9B9FF8F8F8FFF000000005F5F5FFFBBBBBBFF8A8A8AFFA2A2
      A2FFBABABAFF5A5ABAFF0000FFFF0000FFFF0000FFFF2222F2FF80808FFFA2A2
      A2FFBBBBBBFF919191FF595959EF000000000000000000000000000000000005
      01060D42144226752DB60E1C0DFC6F6F6FFF131710FFABAA77FFFFDCACFFFFB2
      96FFFA8279FAC4464CC46D17226D1001041A00000000000000003535358F6D6D
      6DFFA2A2A2FFBBBBBBFFBABABAFFBABABAFF5F5F5FFF000000005E5E5EFFBBBB
      BBFF5E5E5EFF0000000000000000000000002323235F646464FFA2A2A2FFBBBB
      BBFF8A8A8AFF60606FFF0F0FF1FF0000FFFF0000FFFF6E6ED7FF8B8B8BFF6666
      66FFA5A5A5FFBBBBBBFF909090FF000000005F5F5FFF959595FF595959EF5858
      58EF5F5F5FFF484857D10303E3E90000FFFF0000FFFF5858C1FF585858EF5858
      58EF5F5F5FFF4D4D4DCF1D1D1D4F00000000000000000000000000000000010C
      050C1457295744B463B459AC6BF8141E15FF737373FF171714FFAE9287FFFFB1
      B4FFFF8BA1FFDD587CDD8622408624040F2900000000000000005F5F5FFFAEAE
      AEFFADADADFF959595FF929292FFBBBBBBFF5F5F5FFF000000005F5F5FFFBBBB
      BBFF5E5E5EFF000000000000000000000000535353DF9C9C9CFFA4A4A4FF9898
      98FFB0B0BFFF1111E1FF0000FFFF1111F3FF0000FFFF0000FFFF7373D3FF5F5F
      5FFFA8A8A8FFBBBBBBFF909090FF00000000535353DF7A7A7AFF474747BF0000
      000000000F0F0000CFCF0000FFFF0000E0E00000FFFF0000FFFF2D2D8BD60000
      000000000000000000000000000000000000000000000000000000000000010C
      080E0E6D629B4AB982B989F9C1F978B297FF191E1CFF757575FF171416FFD89B
      B8FFFF91C8FFE15EA0E18B26598B2E061A2F00000000000000005E5E5EFFBABA
      BAFF989898FF969696FF959595FFBBBBBBFF5E5E5EFF000000005F5F5FFFBBBB
      BBFF5F5F5FFF0000000000000000000000005F5F5FFFB3B3B3FFA1A1A1FFA7A7
      B6FF1D1DEEFF0000FFFF1111E1FFA9A9B8FF6262C2FF0000FFFF0000FFFF3F3F
      9EFF8F8F8FFF8F8F8FFF757575CF000000002E2E2E7F747474FF646464FF2C2C
      3B850000CFCF0000FFFF0000CFCF00000F0F1D1D7BAD0000FFFF0000FFFF1D1D
      7BAD000000000000000000000000000000000000000000000000000000000140
      4663068894CE29A59DC97EF1D9F1A8FDF6FF87A8B1FF1A1A1EFFF0F0F0FF946B
      93FFD776C3FED354B2D365DDEEFF2004172400000000000000005F5F5FFFBBBB
      BBFF9B9B9BFF989898FF959595FFBBBBBBFF5E5E5EFF000000005E5E5EFFBABA
      BAFF5F5F5FFF0000000000000000000000005E5E5EFF7E7E7EFF595968FF1111
      E1FF0000FFFF0606D5E0595968FF7D7D7DFF5D5D5DFF3B3B9BFF0000FFFF0000
      FFFF00005F5F0000000000000000000000000505050F595959EF7B7B8BFF1414
      E4FF0000FFFF0808D7E60A0A192C000000000505050F383897F40000FFFF0000
      FFFF343493EA2F2F2F7F0B0B0B1F000000000000000000000000000000000001
      0102075E62792C8580863AAFBBE491E2FCFCA9D1FFFFA5A8E5FF917CADFFF0F0
      F0FF894D8CF7973791BC5B13515B0D010B120000000000000000595959EFA3A3
      A3FFAEAEAEFF989898FF979797FFBBBBBBFF5F5F5FFF5F5F5FFF5F5F5FFFBBBB
      BBFF5F5F5FFF5F5F5FFF0000000000000000525252DF5D5D6CFF0B0BDAED0000
      FFFF0000CFCF00000F0F535353DF646464FF3B3B3B9F0000000000005F5F0000
      FFFF0000FFFF00005F5F00000000000000000000000010101F3B1010E0FC0000
      FFFF1919E9FF787887FF595959EF00000000000000001111112F383897F40000
      FFFF0000FFFF4F4FAFFF595959EF000000000000000000000000000000000000
      000002121213166169720E8DA4E755A6D2E180A5F1F18E91F9F9967ADCF77F5A
      9FF1F0F0F0FF49234EB92D072E2F0100010200000000000000002323235F6464
      64FF9C9C9CFFBBBBBBFFBABABAFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBB
      BBFFBBBBBBFF5E5E5EFF00000000000000002F2F2F7F1C1CCDFF0000FFFF0C0C
      DBF00A0A192C000000002F2F2F7F5F5F5FFF646464FF414141AF0B0B0B1F0000
      5F5F0000FFFF0000FFFF00002F2F00000000000000000000AFAF0000FFFF0A0A
      D9EA4D4D5CE05D5D5DFF525252DF0000000000000000000000000505050F2121
      80B80000FFFF0000FFFF434372E4000000000000000000000000000000000000
      0000003A425E02778AC165DDEEFF0D89A7DE2A74ADC94E50B9B95F47B4B45C37
      90A8482D59AD2F0E3B5304000508000000000000000000000000000000001D1D
      1D4F535353DF5F5F5FFF5E5E5EFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F
      5FFF5F5F5FFF5E5E5EFF000000000000000000000000282887CC2020D0FF6D6D
      7CFF595959EF0000000000000000414141AF696969FF747474FF595959EF0000
      000000005F5F0000AFAF00000F0F000000000000000000005F5F0000AFAF0000
      0F0F000000000000000000000000000000000000000000000000000000000000
      000000005F5F0000AFAF00000F0F000000000000000000000000000000000000
      000000000000001D212E017A8CB80D2D5156131F4E4E191A5B5B21155757200D
      4242130521220300050600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003535358F5F5F
      5FFF595959EF0000000000000000000000003535358F5E5E5EFF595959EF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000003C44570000010201020A0A02020E0E017589B70200
      050600000000000000000000000000000000000000000000001D000000340000
      003600000036000000360000003600000036000000360000003600771FF7027A
      1DFF000F0250000000330000001D000000000000000000000000000000000000
      000000000000151F2257555353C94F4E4EC34F4E4EC34F4E4EC34F4E4EC34F4E
      4EC3504F4FC4424242B804040429000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000034EFEFEFF5FAFA
      FAFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFF0A8832FF42A1
      5EFF1A8935FFC8DBCCF50000003300000000041022600A2A4BB70B2C4EBD0A2B
      4DBC07284AB84F626DD3FEF9F5FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFFBFBFAFFE6E6E5F61818187A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007575
      75CF8F8F8FFF8F8F8FFF909090FF8F8F8FFF909090FF909090FF909090FF9090
      90FF909090FF8F8F8FFF757575CF000000000000000100000036FBFBFBFEFCFC
      FCFFFCFCFCFFFCFCFCFF219751FF1B9149FF158F43FF0F8B3BFF399F5DFF80C1
      96FF45A361FF178933FF001304570000000008213FA286A1C3FF3F6CA1FF3C69
      9FFF2E5E98FF6D7C8FFFFAF8F7FFE7E5E5FFE7E5E5FFE7E5E5FFE7E5E5FFE7E5
      E5FFEBE9E9FFF4F3F3FB1818187A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009090
      90FFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBB
      BBFFBBBBBBFFBBBBBBFF8E8E8EFF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF289B5AFF90CAA9FF8DC8A5FF8AC6A1FF88C59EFF69B6
      85FF82C297FF47A565FF00721DEE00160431082140A48AA6C6FF4372A6FF3F70
      A3FF30649DFF6E7E91FFFBF9F8FFE9E8E9FFE9E8E7FFE9E8E7FFE9E8E7FFE9E8
      E7FFEDECEBFFF4F4F4FB1818187A00000000000000001111112F535353DF5E5E
      5EFF5F5F5FFF525252DF1111112F00000000000000001111112F535353DF5D5D
      5DFF5F5F5FFF535353DF1111112F000000000000000000000000000000009090
      90FFBABABAFFBBBBBBFFBABABAFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBABA
      BAFFBABABAFFBBBBBBFF8F8F8FFF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF309F62FF94CDADFF6EBA8EFF6AB889FF65B685FF60B3
      80FF66B582FF83C298FF3BA05BFF007D24FC08213FA38DA8C9FF497BB1FF497B
      B1FF30649DFF6E7E91FFEBEAEAFFEBEAEAFFEBEAEAFFEBEAEAFFEBEAEAFFEBEA
      EAFFEFEEEEFFF4F4F4FB1818187A000000000B0B0B1F585858EF919191FFBBBB
      BBFFBBBBBBFF969696FF535353DF000000000B0B0B1F595959EF909090FFBABA
      BAFFBBBBBBFF959595FF535353DF000000000000000000000000000000008F8F
      8FFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFB9B9B9FFBBBBBBFFBBBBBBFFBABA
      BAFFBBBBBBFFBBBBBBFF909090FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF36A36AFF96CEB0FF94CDADFF91CBAAFF90CBA8FF73BC
      90FF8AC7A1FF45A567FF068634FD0007021008213FA390ACCBFF497BB1FF497B
      B1FF30649DFF6E7E91FFEEEFF1FFEEEDEDFF0095FFFF0095FFFFEEEDEDFFEEED
      EDFFF2F1F1FFF4F4F4FB1818187A000000003B3B3B9F848484FFABABABFF9191
      91FF969696FFBBBBBBFF5E5E5EFF000000003B3B3B9F858585FFA5A5A5FF8484
      84FF8E8E8EFFBBBBBBFF5F5F5FFF000000000000000000000000000000009090
      90FFBBBBBBFFBBBBBBFFBBBBBBFFBABABAFFBBBBBBFFBABABAFFB9B9B9FFBBBB
      BBFFBABABAFFBABABAFF909090FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF3CA56EFF38A46DFF34A267FF309E61FF54AF7BFF91CB
      AAFF4EAB73FF189045FF0006023F0000000108213FA394AFCDFF5282B2FF487A
      B2FF30649DFF0095FFFF0095FFFFF1EFEFFF0095FFFF0095FFFFF1EFEFFF0597
      FFFF0095FFFFF5F4F4FB1818187A00000000595959EFAEAEAEFF9D9D9DFF9292
      92FF939393FFBABABAFF5F5F5FFF00000000585858EFAEAEAEFF939393FF8585
      85FF878787FFBABABAFF5E5E5EFF000000000000000000000000000000008989
      89FFA5A5A5FFA6A6A6FFB5B5B5FFBBBBBBFFB6B6B6FFA5A5A5FFA5A5A5FFA6A6
      A6FFB4B4B4FFBBBBBBFF8F8F8FFF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFAFAFAFF38A368FF59B3
      81FF279856FFF4F8F6FF0000003600000001082140A396B2D0FF5E8BB8FF5E8B
      B8FF0095FFFF19FFFFFF00C6FFFF0095FFFF19FFFFFF36FFFFFF0597FFFF05C7
      FFFF33FCFCFF0095FFFF1818187A000000005F5F5FFFBBBBBBFF8A8A8AFFA2A2
      A2FFBABABAFF919191FF595959EF000000005F5F5FFFBBBBBBFF888888FFA2A2
      A2FFBBBBBBFF919191FF595959EF00000000000000002F2F2F7F595959EF5E5E
      5EFF666666FF919191FFA6A6A6FF8B8B8BFF646464FF5E5E5EFF666666FF9191
      91FFA4A4A4FFB9B9B9FF8F8F8FFF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FF3DA66FFF309F
      64FFEBEFEDFFFCFCFCFF0000003600000001082140A399B4D3FF5E8BB8FF5E8B
      B8FF30649DFF0095FFFF5CFFFFFF8CFFFFFFB6FFFFFFB6FFFFFF8CFFFFFF5CFF
      FFFF0095FFFFF6F5F5FB1818187A000000005F5F5FFF959595FF595959EF5858
      58EF5F5F5FFF4D4D4DCF1D1D1D4F000000005F5F5FFF969696FF585858EF5858
      58EF5F5F5FFF4D4D4DCF1D1D1D4F000000002323235F646464FFA2A2A2FFBBBB
      BBFF8A8A8AFF666666FF848484FF646464FFA3A3A3FFBBBBBBFF8B8B8BFF6666
      66FFA5A5A5FFBBBBBBFF909090FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF2F2F2FFEFEF
      EFFFEDEDEDFFFCFCFCFF0000003600000001082140A39BB7D7FF5E8BB8FF5E8B
      B8FF30649DFF6E7E91FF0095FFFF00C6FFFFCCFFFFFFCCFFFFFF00C6FFFF0095
      FFFF00000000F7F6F6FB1919197A00000000535353DF7A7A7AFF474747BF0000
      000000000000000000000000000000000000535353DF797979FF474747BF0000
      000000000000000000000000000000000000535353DF9C9C9CFFA4A4A4FF9898
      98FFBBBBBBFF5E5E5EFF656565FF9C9C9CFF9D9D9DFF8E8E8EFFB9B9B9FF5F5F
      5FFFA8A8A8FFBBBBBBFF909090FF000000000000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF1F1F1FFECECECFFEAEA
      EAFFE6E6E6FFFCFCFCFF0000003600000001082140A3A0BBD7FF6794C1FF5E8B
      B8FF5887B7FF0095FFFF5CFFFFFF8CFFFFFFB6FFFFFFB6FFFFFF8CFFFFFF5CFF
      FFFF0095FFFF6E6E6ED408080734000000002E2E2E7F747474FF646464FF2F2F
      2F7F000000000000000000000000000000002F2F2F7F747474FF636363FF2F2F
      2F7F000000000000000000000000000000005F5F5FFFB3B3B3FFA1A1A1FFB1B1
      B1FFA2A2A2FF636363FF5F5F5FFFB4B4B4FF9E9E9EFFAEAEAEFFA3A3A3FF6464
      64FF8F8F8FFF8F8F8FFF757575CF000000000000000100000036FCFCFCFFF9F9
      F9FFF9F9F9FFF9F9F9FFF7F7F7FFF6F6F6FFF2F2F2FFEBEBEBFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF0000003600000001092240A3A4BEDAFF6996C2FF5E8B
      B8FF0095FFFF19FFFFFF00C6FFFF0095FFFF19FFFFFF2CF7F7FF0597FEFF04C6
      FDFF29F0F1F70095FFFF00000001000000000505050F595959EF848484FF6E6E
      6EFF535353DF2F2F2F7F0B0B0B1F000000000505050F595959EF858585FF6E6E
      6EFF535353DF2F2F2F7F0B0B0B1F000000005E5E5EFF7E7E7EFF5F5F5FFF5F5F
      5FFF5E5E5EFF2323235F5F5F5FFF7D7D7DFF5D5D5DFF5F5F5FFF5E5E5EFF2323
      235F000000000000000000000000000000000000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFFCFCFCFFF6F6
      F6FFF4F4F4FF575757910000002000000000092240A4A6C1DDFF88A6C4FF8E91
      93FF747679FF0095FFFF0095FFFF737678FF0095FFFF0393FAFF31618BD80393
      FAFF0091F8F9000000000000000000000000000000001111112F585858EF7979
      79FF8B8B8BFF808080FF595959EF00000000000000001111112F595959EF7979
      79FF8B8B8BFF7E7E7EFF595959EF00000000525252DF636363FF3A3A3A9F0000
      00000000000000000000535353DF646464FF3B3B3B9F00000000000000000000
      0000000000000000000000000000000000000000000000000036F9F9F9FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFE7E7
      E7FF5454549100000020000000020000000009213FA1B3CAE5FFB8CADEFF9D9E
      A1FFF0F1F3FFEFEEEEFFEAEBEBFFE6E7EAFF0095FFFF0798FFFF7594AFE20A1C
      41C00000000000000000000000000000000000000000000000000505050F3535
      358F525252DF5D5D5DFF525252DF0000000000000000000000000505050F3535
      358F535353DF5F5F5FFF525252DF000000002F2F2F7F5D5D5DFF646464FF4141
      41AF0B0B0B1F000000002F2F2F7F5F5F5FFF646464FF414141AF0B0B0B1F0000
      0000000000000000000000000000000000000000000000000033E9E9E9F0F9F9
      F9FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF5454
      549100000020000000020000000000000000030D1A42193759C018375AC21434
      56BF28486CD8FCF9F8FFD9E0E6F8133459C6123255C1133458C40C2848AF040D
      1E59000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000404040AF696969FF7474
      74FF595959EF0000000000000000414141AF696969FF747474FF595959EF0000
      000000000000000000000000000000000000000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      0020000000020000000000000000000000000000000000000000000000000000
      00003C484C65AEAAA9F79F9D9CEA050B0D160000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003535358F5F5F
      5FFF595959EF0000000000000000000000003535358F5E5E5EFF595959EF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000001B0000003F0000000000000000000000000000
      000000000000000000000000000000000000000000000404040F242424893737
      37CD3F3F3FEB363636CC282828980A0A0A281B1B1B65363636CB3D3D3DE41B1B
      1B6500000000000000000000000000000000000000000404040F242424893737
      37CD3F3F3FEB363636CC282828980A0A0A281B1B1B65363636CB3D3D3DE41B1B
      1B65000000000000000000000000000000000000000000000000000000003404
      08FF340408FF210E012400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000002F0000007D0000000000000000000000000000
      000000000000000000000000000000000000060606183E3E3EE3868686FFCCCC
      CCFFE5E5E5FFCFCFCFFFA2A2A2FF4A4A4AF95D5D5DFFC9C9C9FFDCDCDCFF4747
      47FB00000000000000000000000000000000060606183E3E3EE3868686FFCCCC
      CCFFE5E5E5FFCFCFCFFFA2A2A2FF4A4A4AF95D5D5DFFC9C9C9FFDCDCDCFF4747
      47FB000000000000000000000000000000000000000000000000000000003404
      08FF340408FFD15A07DFD25B07E02811012B0000000000000000000000000000
      0000000001010000000000000000000000000000000000000000000000000000
      0000000000000000002F000000E2000000970000000000000000000000000000
      0000000000000000000000000000000000002F2F2FA5949494FFECECECFFEAEA
      EAFFC5C5C5FFC0C0C0FFE8E8E8FFE2E2E2FFC6C6C6FFF3F3F3FFF0F0F0FF4D4D
      4DFF000000000000000000000000000000002F2F2FA5949494FFECECECFFEAEA
      EAFFC5C5C5FFC0C0C0FFE8E8E8FFE2E2E2FFC6C6C6FFF3F3F3FFF0F0F0FF4D4D
      4DFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000051230357D75D08E62D13013000000000000000000000
      B5BC0000C3CB0000010100000000000000000000000000000000000000000000
      000000000051000000FB000000CA000000C6000000BF000000C6000000BF0000
      000000000000000000000000000000000000494949EADBDBDBFFEAEAEAFF6969
      69FF3C3C3CC1363636AF515151F4B6B6B6FFF5F5F5FFF2F2F2FF676767FF3C3C
      3CC100000000000000000000000000000000494949EADBDBDBFFEAEAEAFF6969
      69FF3C3C3CC1363636AF515151F4B6B6B6FFF5F5F5FFF2F2F2FF676767FF3C3C
      3CC1000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000632B036AA34606AE000000000B0001160D00
      A7C70000B5BC0000000000000000000000000000FFFF00000000000000000000
      000000000000000000BE000000BE0000009F0000003F0000006E000000C60000
      00000000000000000000000000000000FFFF575757FCF4F4F4FFF2F2F2FF5454
      54F70404040E000000000C0C0C265B5B5BF9E9E9E9FFF0F0F0FF5A5A5AFF0202
      020800000000000000000000000000000000575757FCF4F4F4FFF2F2F2FF5454
      54F70404040E000000000C0C0C265B5B5BF9E9E9E9FFF0F0F0FF5A5A5AFF0202
      0208000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002911012CCA5707D80A000017480002930500
      000A000000000000000000000000000000000000FFFF0000FFFF000000000000
      000000000000000000000000009F0000009C000000000000003F000000BF0000
      000000000000000000000000FFFF0000FFFF484848C8E8E8E8FFFBFBFBFFB9B9
      B9FF5B5B5BF43C3C3CA7262626694B4B4BD0ECECECFFFAFAFAFF626262FF0202
      020800000000000000000000000000000000484848C8E8E8E8FFFBFBFBFFB9B9
      B9FF5B5B5BF43C3C3CA7262626694B4B4BD0ECECECFFFAFAFAFF626262FF0202
      0208000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000053240359A84406BE0A0000170500000A0000
      0000000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000
      000000000000000000000000001F0000008F000000000000001F0000008F0000
      0000000000000000FFFF0000FFFF0000FFFF2B2B2B70949494FFF3F3F3FFFAFA
      FAFFF6F6F6FFDBDBDBFFBABABAFF8E8E8EFFEAEAEAFFFDFDFDFF6B6B6BFF0303
      0308000000000000000000000000000000002B2B2B70949494FFF3F3F3FFFAFA
      FAFFF6F6F6FFDBDBDBFFBABABAFF8E8E8EFFEAEAEAFFFDFDFDFF6B6B6BFF0303
      0308000000000000000000000000000000000000000000000000000000000000
      000000000000000000000D05000ECB5307E3630B04AF0500000A000000000000
      0000000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000
      FFFF0000000000000000000000000000000F00000000000000370000005E0000
      00000000FFFF0000FFFF0000FFFF0000FFFF00000001303030767C7C7CFCBABA
      BAFFD5D5D5FFEBEBEBFFF8F8F8FFFBFBFBFFFBFBFBFFFBFBFBFF717171FF0303
      03080000000000000000000000000000000000000001303030767C7C7CFCBABA
      BAFFD5D5D5FFEBEBEBFFF8F8F8FFFBFBFBFFFBFBFBFFFBFBFBFF717171FF0303
      0308000000000000000000000000000000000000000000000000000000000000
      000000000000000000008F3B05A08B3505A40500000A00000000000000000000
      0000000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000
      FFFF0000000000000000000000000000000000000000000000000000001B0000
      00000000FFFF0000FFFF0000FFFF0000FFFF0000000000000000101010262D2D
      2D6642424297575757C7737373F8B4B4B4FF4848B2FF000087FF00007CFF0000
      7AFF000083FF000066B400000000000000000000000000000000101010262D2D
      2D6642424297575757C7737373F8B4B4B4FFE7EFE7FF63A564FF428744FF070F
      071F000000000000000000000000000000000000000000000000000000000000
      0000000000000100EFFA0000F6FF0F0500140000000000000000000000000000
      0000000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FFFF0000FFFF0000FFFF515151AD777777FF777777FF5959
      59BF0808081300000000131313297A7A7AFE00008DFF001ECDFF112DD1FF233C
      D6FF344BDBFF000087FF0000000000000000515151AD777777FF777777FF5959
      59BF0808081300000000131313297A7A7AFE5BA05DFF1E8E4BFF1B8947FF2563
      23D8030A03180000000000000000000000000000000000000000000000000000
      00000903000C0A04E9FD0100EFFA000000000000000000000000000000000000
      0000000000000000000000000000000000000000FFFF0000FFFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FFFF0000FFFF6F6F6FE2E4E4E4FFEFEFEFFFBDBD
      BDFF838383FF797979F7888888FFBFBFBFFF00008AFF112DD1FF233CD6FF1A25
      9EFF000071FF0000498C00000000000000006F6F6FE2E4E4E4FFEFEFEFFFBDBD
      BDFF838383FF797979F7888888FFBFBFBFFF61A363FF1C8A48FF069965FF1789
      4AFF1E5017D7184212BB215B18FF1A4515B40000000000000000000000000A00
      00177A1A0CC69F511CA900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FFFF00000000000000000000
      00000000000000000098000000CC000000CC000000CC000000CC000000980000
      00000000000000000000000000000000FFFF4D4D4D95B6B6B6FFECECECFFEDED
      EDFFEDEDEDFFECECECFFEFEFEFFFEFEFEFFF00008FFF233CD6FF1A25A7FF4659
      DFFF2C34AFFF000074D700000E18000000004D4D4D95B6B6B6FFECECECFFEDED
      EDFFEDEDEDFFECECECFFEFEFEFFFEFEFEFFFDEE6DFFF488A47FF188B4CFF08B5
      81FF138C50FF1A4B0EFF0BDEA9FF1F5716FF0000000000000000000000002200
      01479D5323AC522D135700000000000000000000000000000000000000000000
      00000000000000000000340604FF340604FF0000000000000000000000000000
      000000000000000000BA0000002F00000000000000000000002F000000BA0000
      0000000000000000000000000000000000000707070D707070CCA7A7A7FFD3D3
      D3FFE0E0E0FFE5E5E5FFDDDDDDFFC8C8C8FF000099FF344BDBFF000083FF2C34
      B3FF6977E9FF3D43C1FF00008AD7000010180707070D707070CCA7A7A7FFD3D3
      D3FFE0E0E0FFE5E5E5FFDDDDDDFFC8C8C8FFAEAEAEFF757C75E5245D20DB1590
      53FF0AD09BFF0F8F57FF0CECB6FF1D5212FF0000000009000015400003830000
      000090542A977A48248000000000000000000000000000000000000000000000
      0000000000000C07030D340604FF340604FF0000000000000000000000000000
      000000000000000000830000002C00000000000000000000002C000000830000
      00000000000000000000000000000000000000000000010101033434345B6D6D
      6DBF7C7C7CD8858585E9767676CF5252528F16166AA700009CFF00006EBB0000
      7DD73D43C4FF8E96F2FF4F52D4FF000091C300000000010101033434345B6D6D
      6DBF7C7C7CD8858585E9767676CF5252528F2C2C2C4E0606060B1D4F1ABA225E
      1AFF13975DFF0CECB6FF0DF1BBFF1E5414FF0000ABB20D00B3D50400000B0000
      000023160C25E08C4FEA3C25153F000000000000000000000000000000000000
      000004030105985F369F955D359C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0F18000090D74F52D5FF4F52DBFF000098CC0000000000000000000000000000
      00000000000000000000000000000000000000000000000000002B7228FF0BDE
      A9FF0CECB6FF0DF1BBFF0DF1BBFF225E1AFF00009CA20000A3A9000000000000
      000000000000412B1B44D78D58DF9D6740A3503421531E140C202A1C112C5538
      2359CE8855D6B27549B9100A0611000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000001118000098CC000091C3000011180000000000000000000000000000
      0000000000000000000000000000000000000000000000000000215720B42A70
      26FF266821FF24631EFF25641EFF1C4B18B40000000000000000000000000000
      00000000000000000000080604096D4B3371A9754FAFD69465DDC88B5ECFA774
      4FAD4B34234E0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004C311B656641
      2487664124876641248766412487664124876641248766412487664124876641
      2487664124874C311B6500000000000000000000000000000000000000000000
      00000000000005273D511778B4EA157FC1FE147EC1FE177EBCF51872AADD0E51
      7AA0010D151C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000140B07234828197D7D462CDB834B31F3824A31F376442DDB4327197D120A
      0723000000000000000000000000000000000000000000000000623F23820000
      00000000000000000000FEFEFEFFFEFEFEFFFEFDFCFFFDFCFBFFFDFBF9FFFCFA
      F8FFFDFBF9FF623F238200000000000000000000000000000000000000000000
      0000010E161E2A8BC7FECAE7F3FFE4F4F9FFE7F5F9FFE0F2F8FFD8EDF6FF9CCE
      E7FF2187C5FE0E547EA500030507000000000000000000000000000000000000
      00000000000000000000000D1947002345CC002344CC000C1748000000000000
      000000000000000000000000000000000000000000000000000000000000331B
      10538A4C2EE6B28056FFD5B793FFDBC3A6FFDAC3A6FFD2B490FFAB7951FF7946
      2EE62C19115300000000000000000000000000000000000000005D3C217B0000
      0000FEFEFEFFFEFEFEFFFEFDFCFFFDFBF9FFFCFAF8FFFCF9F6FFFBF7F3FFFAF6
      F2FFFBF8F5FF5D3C217B0000000000000000222222FF222222FF131313FB0909
      09EC113F5BAE9ECEE7FFAFE0CFFF008677FF48B99BFF97D8ECFFA4DDEDFFDBF1
      F7FFE4F3F9FF68B0D8FE0C4A6F93000000000000000000000000000000000000
      000000000000000C1748002243CC5394B7FF33669AFF002547CA000C18480000
      0000000000000000000000000000000000000000000000000000361F10539851
      30F4CBA77CFFD8BB9FFFC39C76FFB68A61FFB4865FFFBE9671FFD1B397FFC5A3
      76FF7E4B32F42C191153000000000000000061330599804306CC804306CCB376
      2AFFB3762AFFB3762AFFB3762AFFB3762AFFB3762AFFB3762AFFB3762AFFB376
      2AFFB3762AFF804306CC804306CC22120333000000000E0E0EB8666666FF2828
      28FF12171AFED7ECF5FF1CA764FF33E1CBFF008677FF8ED5EAFF4364DBFF0000
      CCFF8399E7FFE2F3F8FF2589C6FE031724300000000000000000000000000000
      000000131F41003658BB39709FFF376E9DFF5E9FC0FF4477ABFF002A4CC6000E
      1A460000000000000000000000000000000000000000170E0722995D2FE5CFAA
      81FFDABCA2FFBE9165FFBA8C61FFB7895EFFB3845DFFB1835CFFB0835BFFCDAA
      8DFFC6A578FF7B472EE5120A072200000000915413CCFFB508FFFFB60AFFFFB7
      0CFFFFB80FFFFFB913FFFFBB1AFFFFC029FFFFC639FFFFCB49FFFFD056FFFFD2
      5FFFFFD362FFFFD566FFE0A93BF3915413CC000000000505056D141414FF6666
      66FF141414FFA9B6BDFF4BBA9CFF12A35DFF48BA9EFF8AD2E9FF0E14D2FF0078
      F5FF0000CCFFD8F0F7FFAFD7EBFF0D4F789E000000000000000000000000000C
      1748002243CC4E8DB3FF67ACC8FF4780ACFF4F87B3FF69AAC8FF5488BBFF0031
      52C100101C44000000000000000000000000000000005B3B1A7EBF915DFFE0C2
      A8FFC5966BFFC29168FFE1CBB8FFFEFDFCFFFFFFFEFFEADCD0FFB4855DFFB385
      5DFFD4B599FFAE7A55FF46271A7E00000000794B1799A46821CCA46821CCD79B
      4EFFD79B4EFFD79B4EFFD79B4EFFD79B4EFFD79B4EFFD79B4EFFD79B4EFFD79B
      4EFFD79B4EFFE2AF62FFFFE18CFFA46821CC0000000000000000090909B21414
      14FFCFD7D3FF858A88FF659DB1FF81CCE6FF82CDE6FF83CEE7FF4165DBFF0E15
      D2FF476DDDFF9BD8EBFFD3EAF4FF156DA3D400000000000000000015213F003C
      5FB65EA1C0FF3E78A3FF4177A7FF65A6C5FF609DC2FF5D95C1FF73B4D1FF6498
      CBFF000000AB0000003C000000000000000000000000A16B2DDBDBBC9CFFD5AD
      89FFC7986BFFC39568FFC19366FFEDDFD3FFFAF7F4FFBB8B62FFB98A62FFB88A
      61FFC59D77FFD2B893FF7C452DDB00000000000000002415053367422489FEFE
      FBFFFBFBF9FFFAFAF7FFF9F9F5FFFAF6F2FFF7F7F1FFF8F1ECFFF8F0EAFFF7EE
      E7FFF9F2EDFFDEBA9CFFEDC97CF3B4782DCC000000000000000000000000083B
      5C7A929292FFECEEEEFF858A88FF5C98AFFF7AC8E4FF7BC9E4FF7CC9E5FF7ECA
      E5FF80CBE6FF8AD0E8FFDEF0F7FF167DBBF500000000000C1748002243CC3267
      98FF4F8CB3FF68ABC8FF66A7C6FF4C80B3FF70B1CEFF6DA9CDFF6BA3CEFF6C6C
      6CFFAA9999FF000000A500131F420000000000000000B87932F6E3C7AFFFD0A2
      75FFC5996AFFC49769FFC49668FFEEE0D4FFFBF7F4FFBF9065FFBE8F64FFBE8F
      63FFBE9268FFDFC6AAFF8D4C31F60000000021110233804306CC784D2A9FFDFD
      FAFFC5985DFFB27529FFD9BE99FFF9F3EFFFF8F1ECFFF8F0EAFFF7EEE7FFF4E8
      DEFFF6EDE6FF81532EABBB7F33CC2E1F0C3300000000031826331375B1E8469C
      CEFDD6EBF5FF858A88FF5F8CBCFF01588FFF5895ADFF74C4E2FF76C5E3FF8583
      76FF87491FFF868A80FFE3F3F8FF137EC1FE0017233E004063B169AEC9FF65A8
      C5FF5592B8FF4A80AFFF5C97BFFF76B9D2FF659DC8FF7ABAD5FF7D7D7DFFCEC0
      C0FF787878FF5488BBFF003351A60000000000000000BE8038F6E4C9B0FFD0A3
      79FFCC9D70FFC79A6BFFC5986AFF00000000FFFFFEFFC39668FFC19467FFC294
      67FFC3986CFFDFC5ABFF904F31F600000000894C0DCCDF9A11F3CA9061FFFDFD
      F9FFBC8033FFFFC435FFBC8033FFDCC19BFFF8F0EAFFF7EEE7FFF4E8DEFFF1E2
      D5FFF5EBE3FF654124862E1F0C3300000000042337492286C5FEB7DBEDFFDCEE
      F6FFDEEFF6FFAED9EBFF01588FFF5F8CBCFF01588FFF5392ACFF6FC0E0FF8B57
      32FFCF9F71FF87491FFFDCEEF6FF167CBBF500324B830C4E70B8185B7CC3296C
      8BD04F93AEE771B6CDFB6DACCCFF659DC8FF83C7DAFF888888FFD3CACAFF8383
      83FF5FA4C6FF62A7C9FF003554A50000000000000000AE7A37DBE0BC9FFFDBB3
      93FFCFA074FFCD9E71FFCB9C70FFDDBFA3FFDDBFA2FFC5996AFFC5996AFFC498
      6AFFD1AB85FFD8BA97FF88492DDB00000000945715CCFFC73CFFD4993CFFC78B
      3EFFC78B3EFFEAAD2AFFFFD566FFC78B3EFFDFC49EFFF4E8DEFFF1E2D5FFEFDD
      CFFFF4E9E0FF52351D6D00000000000000001372ACE1C2E0F0FFC3E1F0FF117D
      C1FF107CC1FF107CC1FF9FCEE2FF01588FFF5E8BB8FF01588FFF4F8FAAFF7C86
      81FF8A5834FFC8CAC7FFBFDFEFFF13699ECF0001020500070B14000F17280018
      2541002D44770A4867AB40859FDB77B7D2FC919191FFD9D4D4FF8D8D8DFF67AC
      CEFF73B8D4FF003D5FB40015214000000000000000006547227ECD9C67FFE7CB
      B4FFD4A579FFD0A076FFCF9E73FFFBF8F5FFFBF8F5FFCB9E70FFCB9D70FFCDA1
      76FFDFC0A5FFB98A5AFF512D197E000000009F621ECCE6B44BF3FFD465FFFFD5
      66FFFFD568FFFFD25FFFFFCF55FFFFE08AFFD29649FFF1E2D5FFEFDDCFFFEEDB
      CCFFF4E9E0FF51341D6C0000000000000000127DC0FEDDEEF6FF107CC1FF010B
      121800000102010E161E107CC1FFA6D5EAFF01588FFF5F8CBCFF01588FFF4B8C
      A9FFB6DDEDFFDBEEF6FF4299CEFE042337490000000000000000000000000000
      0000000000000006091000293F6D00000069DDDCDCFF949494FF6FB4D6FF80C4
      DBFF003F62B200144ECC0005184800000000000000001C130A22B97F3FE5D9B2
      8CFFE6CAB3FFD6A97CFFD1A578FFE2C4A8FFE1C3A8FFD0A275FFD1A476FFDDBD
      A2FFD0AC85FF995B2FE5160D072200000000291A0833A96D26CCA96D26CCDDA1
      54FFDDA154FFEAC065FFFFE496FFDDA154FFE3C89DFFEFDDCFFFEEDBCCFFEEDB
      CCFFF4E9E0FF51341C6B00000000000000001378B6EFCDE5F2FF1B83C3FF0213
      1E2800000001031B2A38107CC1FFA2D3E9FF58AFD9FF01588FFF5F8CBCFF0158
      8FFFA6B5BCFF53A2D2FE11669BCB000000000000000000000000000000000000
      0000000000000000000000000000000000240000006788CCDDFF87CBDDFF0041
      63AF002666CC3E71B6FF001F5CCC000A1F48000000000000000045301853C687
      43F4D9B28CFFE6CDB8FFE0BA9DFFD7AB85FFD6A982FFD9B391FFE1C2ABFFD4AE
      86FFA96532F439231153000000000000000000000000000000004F321C69FBF7
      F3FFE6AA5DFFFFE599FFE6AA5DFFE8CDA2FFEFDDCFFFEEDBCCFFCD976AFFCD97
      6AFFCD976AFF5D3C217C0000000000000000083B5B784D9ED0FED4E9F3FF1E85
      C4FF107CC1FF107CC1FF9FCEE7FF67B5DBFF81C2E1FFCCE6F2FF01588FFF5F8C
      BCFF01588FFF083653AA00000001000000000000000000000000000000000000
      000000000000000000000000000000000000000B1039003D5C9C003C5B9C0017
      243E00102748002B6DCC5185C9FF002464CC0000000000000000000000004531
      1953BC8341E6D0A069FFE0BFA0FFE3C5AEFFE3C5AEFFDFBC9FFFC89761FFAA70
      2FE63D28115300000000000000000000000000000000000000004E321C68FAF6
      F2FFEEC186FFEDB164FFEBD0A5FFEFDDCFFFEEDBCCFFEEDBCCFFD6AA86FF0000
      00004E321C6811110B2500000000000000000000000110689FD23892CAFDBADB
      EDFFD8EBF5FFD6EAF4FFD3E8F3FFCEE6F2FFBCDCEEFF78B6DBFE2487C5FE0158
      8FFF5F8CBCFF01588FFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000112948002F72CC000F27480000000000000000000000000000
      00001D140B236849257DB47D41DBC38541F3C1843DF3AC7736DB61441F7D1B12
      08230000000000000000000000000000000000000000000000004E311B67FBF8
      F5FFF9F2EDFFF6EDE6FFF5EAE1FFF4E9E0FFF4E9E0FFF4E9E0FFDCB698FF4E31
      1B6712120B2500000000000000000000000000000000000000010735516C1171
      ADE4127CC0FD127CBEFA1277B6F01272ACE30F6093C206324E67010D151C0000
      000001588FFF014D7DDF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003A25144D4D31
      1B664D311B664D311B664D311B664D311B664D311B664D311B664D311B661B11
      0924000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000090909FF090909FF0909
      09FF090909FF090909FF090909FF090909FF090909FF090909FF090909FF0909
      09FF090909FF090909FF090909FF0000000000000000090909FF090909FF0909
      09FF090909FF090909FF090909FF090909FF090909FF090909FF090909FF0909
      09FF090909FF090909FF090909FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000090909FF141414FF141414FF1414
      14FF141414FF141414FF141414FF141414FF141414FF141414FF141414FF1414
      14FF141414FF141414FF141414FF090909FF090909FF141414FF141414FF1414
      14FF141414FF141414FF141414FF141414FF141414FF141414FF141414FF1414
      14FF141414FF141414FF141414FF090909FF0000000000000000000000000000
      000000000000000000002B8EC8FF2B8EC8FF2B8EC8FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000141414FF181818FF181818FF1818
      18FF141414FF141414FF141414FF141414FF141414FF181818FF181818FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF181818FF141414FF141414FF141414FF141414FF141414FF181818FF1818
      18FF181818FF181818FF181818FF141414FF0000000000000000000000000000
      00000000000000000000399FCFFF05F1F8FF2B8EC8FF00000000000000000000
      000000000000000000000000000000000000A97050FFC38E67FFC08B65FFBE88
      63FFBB8560FFB9835EFFB47D5BFFB27B59FFB17A57FFAE7856FFAD7555FFAB74
      53FFA97252FFA97050FFA97050FF00000000141414FF181818FF181818FF1818
      18FF141414FFFFFFFFFFFFFFFFFFC1C1C1FF717171FF141414FF181818FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF181818FF181818FF181818FF181818FF181818FF181818FF181818FF1818
      18FF181818FF181818FF181818FF141414FF0000000000000000000000000000
      00000000000000000000399FCFFF05F1F8FF2B8EC8FF00000000000000000000
      000000000000000000000000000000000000C8926BFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A97150FF00000000141414FF202020FF202020FF2020
      20FF181818FFC1C1C1FFC1C1C1FFFFFFFFFFFFFFFFFFD1D1D1FF181818FF2020
      20FF202020FF202020FF202020FF141414FF141414FF202020FF202020FF2020
      20FF202020FF202020FF202020FF202020FF202020FF202020FF202020FF2020
      20FF202020FF202020FF202020FF141414FF0000000000000000000000000000
      00000000000000000000399FCFFF05F1F8FF2B8EC8FF00000000000000000000
      000000000000000000000000000000000000CA946DFF0000000000000000FFFF
      FEFFFFFFFDFFFEFEFDFFFEFEFCFFFEFEFCFFFEFEFCFFFEFEFCFFFEFEFAFFFEFE
      FAFFFCFCF9FF00000000AA7252FF00000000181818FF202020FF202020FF2020
      20FF202020FF202020FF202020FF3F3F3FFFD1D1D1FFFFFFFFFF898989FF2020
      20FF202020FF202020FF202020FF181818FF181818FF202020FF202020FF2020
      20FF202020FF202020FF202020FF202020FF202020FF202020FF202020FF2020
      20FF202020FF202020FF202020FF181818FF0000000000000000000000000000
      0000000000000000000048B0D6FF05F1F8FF2B8EC8FF00000000000000000000
      000000000000000000000000000000000000CC976EFF00000000FFFFFCFFFFFF
      FDFFFEFEFCFFFEFEFCFFFEFEFBFFFDFDFAFFFDFDFAFFFDFDFAFFFDFDFAFFFCFC
      F7FFFBFBF6FF00000000AC7453FF00000000202020FF2C2C2CFF2C2C2CFF2C2C
      2CFF2C2C2CFF6C6C6CFFB7B7B7FFB7B7B7FF898989FFFFFFFFFFF1F1F1FF2020
      20FF2C2C2CFF2C2C2CFF2C2C2CFF202020FF202020FF2C2C2CFF2C2C2CFF2C2C
      2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C
      2CFF2C2C2CFF2C2C2CFF2C2C2CFF202020FF0000000000000000000000000000
      00000000000048B0D6FF48B0D6FF05F1F8FF2C8FC8FF2C8FC8FF000000000000
      000000000000000000000000000000000000D19C72FF00000000FEFEFCFFFEFE
      FCFFFEFEFCFFFDFDFBFFFDFDFBFFFDFDFAFFFDFDF8FFFBFBF9FFFBFAF7FFFBFA
      F6FFFBF8F4FF00000000B07957FF00000000202020FF303030FF303030FF2C2C
      2CFF717171FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5555
      55FF303030FF303030FF303030FF202020FF202020FF343434FF343434FF3434
      34FF343434FF343434FF343434FF343434FF343434FF343434FF343434FF3434
      34FF343434FF343434FF343434FF202020FF0000000000000000000000000000
      000048B0D6FF48B0D6FF13F1F8FF05F1F8FF05E9F5FF2C8FC8FF2C8FC8FF0000
      000000000000000000000000000000000000D49E74FFA6A6A6FFE1E1E0FFA6A6
      A6FFE0E0E0FFA6A6A6FFE0E0DDFFA6A6A6FFDFDDDBFFA6A6A6FFDFDCD8FFA6A6
      A6FFDFD9D7FFA6A6A6FFB27B59FF000000002C2C2CFF3F3F3FFF3F3F3FFF3030
      30FFE3E3E3FFFFFFFFFFACACACFF2C2C2CFF656565FFFFFFFFFFFFFFFFFF6565
      65FF303030FF3F3F3FFF3F3F3FFF2C2C2CFF2C2C2CFF383838FF383838FF3838
      38FF383838FF383838FF383838FF383838FF383838FF383838FF383838FF3838
      38FF383838FF383838FF383838FF2C2C2CFF00000000000000000000000048B0
      D6FF48B0D6FF2BEEF8FF13F1F8FF00FFFFFF05E9F5FF0DDCF1FF2C8FC8FF2C8F
      C8FF00000000000000000000000000000000D5A075FF00000000FDFDFCFFFDFD
      FBFFFDFDFAFFFCFCF9FFFCFBF7FFFBF9F5FFFBF8F4FFFBF7F3FFFBF5F2FFFAF3
      EFFFF8F2ECFF00000000B57D5BFF00000000303030FF3F3F3FFF3F3F3FFF3030
      30FFFFFFFFFFFFFFFFFF6C6C6CFF3F3F3FFF303030FFFFFFFFFFFFFFFFFF5B5B
      5BFF3F3F3FFF3F3F3FFF3F3F3FFF303030FF343434FF3F3F3FFF3F3F3FFF3F3F
      3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F
      3FFF3F3F3FFF3F3F3FFF3F3F3FFF343434FF000000000000000048B0D6FF48B0
      D6FF49E9F8FF2BEEF8FF0FFFFFFF00FFFFFF00F7FCFF0DDCF1FF1ACCECFF2C8F
      C8FF2C8FC8FF000000000000000000000000D8A278FF00000000FDFDFAFFFCFC
      FAFFFCFBF9FFFBFAF6FFFBF8F5FFFBF7F4FFFBF6F1FFF8F4EEFFF7F2EBFFF7F0
      EAFFF6ECE8FF00000000B7815DFF000000003F3F3FFF3F3F3FFF3F3F3FFF3F3F
      3FFFD1D1D1FFFFFFFFFF959595FF3F3F3FFF6C6C6CFFFFFFFFFFF1F1F1FF3F3F
      3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF383838FF3F3F3FFF3F3F3FFF3F3F
      3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F
      3FFF3F3F3FFF3F3F3FFF3F3F3FFF383838FF0000000048B0D6FF48B0D6FF67E3
      F8FF49E9F8FF2BFDFFFF0FFFFFFF00FFFFFF00F7FCFF07E9F8FF1ACCECFF26BF
      E8FF2C8FC8FF2C8FC8FF0000000000000000D9A378FF00000000FCFBF9FFFCFB
      F8FFFBF9F7FFFBF7F4FFFAF7F2FFF9F5F0FFF7F3EDFFF6EFEAFFF5EBE7FFF3EA
      E4FFF2E7DEFF00000000BA855FFF000000003F3F3FFF4C4C4CFF4C4C4CFF3F3F
      3FFF7C7C7CFFFFFFFFFFFFFFFFFFCACACAFFF1F1F1FFFFFFFFFF959595FF3F3F
      3FFF4C4C4CFF4C4C4CFF4C4C4CFF3F3F3FFF3F3F3FFF4C4C4CFF4C4C4CFF4C4C
      4CFF4C4C4CFF4C4C4CFF4C4C4CFF4C4C4CFF4C4C4CFF4C4C4CFF4C4C4CFF4C4C
      4CFF4C4C4CFF4C4C4CFF4C4C4CFF3F3F3FFF48B0D6FF48B0D6FF84DDF8FF67E3
      F8FF4DF6FFFF2BFCFFFF0FFFFFFF00FFFFFF00F7FCFF07E9F8FF16D9F4FF26BF
      E8FF32B5E5FF2C8FC8FF2C8FC8FF00000000DBA479FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BD8762FF000000003F3F3FFF555555FF555555FF5555
      55FF3F3F3FFF7C7C7CFFD8D8D8FFFFFFFFFFE3E3E3FF989898FF3F3F3FFF5555
      55FF555555FF555555FF555555FF3F3F3FFF3F3F3FFF555555FF555555FF5555
      55FF555555FF555555FF555555FF555555FF555555FF555555FF555555FF5555
      55FF555555FF555555FF555555FF3F3F3FFF48B0D6FF95D9F8FF84DDF8FF67E3
      F8FF4AE9F8FF2BEFF8FF13F1F8FF05F1F8FF05E9F5FF0DDCF1FF1ACCECFF27BF
      E8FF32B4E5FF39ADE2FF2C8FC8FF00000000DCA77AFFDCA77AFFDCA77AFFDCA7
      7AFFDCA77AFFDCA77AFFDCA77AFFDCA77AFFDCA77AFFDCA77AFFDCA77AFFDCA7
      7AFFDCA77AFFDCA77AFFC08B65FF000000004C4C4CFF555555FF555555FF5555
      55FF555555FF4C4C4CFF3F3F3FFF3F3F3FFF3F3F3FFF4C4C4CFF555555FF5555
      55FF555555FF555555FF555555FF4C4C4CFF4C4C4CFF555555FF555555FF5555
      55FF555555FF555555FF555555FF555555FF555555FF555555FF555555FF5555
      55FF555555FF555555FF555555FF4C4C4CFF62BBDBFF7DC6E0FF7DC6E0FF7DC6
      E0FF7DC6E0FF7DC6E0FF7DC6E0FF7DC6E0FF7DC6E0FF7DC6E0FF7DC6E0FF7DC6
      E0FF7DC6E0FF7DC6E0FF52ADD4FF00000000DDAC85FFE8B992FFE8B992FFE8B9
      92FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B9
      92FFE8B992FFE8B992FFC1906EFF000000004C4C4CFF5B5B5BFF5B5B5BFF5B5B
      5BFF5B5B5BFF5B5B5BFF5B5B5BFF5B5B5BFF5B5B5BFF5B5B5BFF5B5B5BFF5B5B
      5BFF5B5B5BFF5B5B5BFF5B5B5BFF4C4C4CFF4C4C4CFF595959FF595959FF5959
      59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
      59FF595959FF595959FF595959FF4C4C4CFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A97050FFDDB18DFFDCA77AFFDCA6
      79FFDAA479FFD8A278FFD5A075FFD49E74FFD29D72FFCF9A71FFCE996FFFCB96
      6EFFC9946BFFC49A79FFA97050FF00000000000000004C4C4CFF555555FF5555
      55FF555555FF555555FF555555FF555555FF555555FF555555FF555555FF5555
      55FF555555FF555555FF4C4C4CFF00000000000000004C4C4CFF555555FF5555
      55FF555555FF555555FF555555FF555555FF555555FF555555FF555555FF5555
      55FF555555FF555555FF4C4C4CFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000090909FF090909FF0909
      09FF090909FF090909FF090909FF090909FF090909FF090909FF090909FF0909
      09FF090909FF090909FF090909FF0000000000000000090909FF090909FF0909
      09FF090909FF090909FF090909FF090909FF090909FF090909FF090909FF0909
      09FF090909FF090909FF090909FF0000000000000000090909FF090909FF0909
      09FF090909FF090909FF090909FF090909FF090909FF090909FF090909FF0909
      09FF090909FF090909FF090909FF0000000000000000090909FF090909FF0909
      09FF090909FF090909FF090909FF090909FF090909FF090909FF090909FF0909
      09FF090909FF090909FF090909FF00000000090909FF141414FF141414FF1414
      14FF141414FF141414FF141414FF141414FF141414FF141414FF141414FF1414
      14FF141414FF141414FF141414FF090909FF090909FF141414FF141414FF1414
      14FF141414FF141414FF141414FF141414FF141414FF141414FF141414FF1414
      14FF141414FF141414FF141414FF090909FF090909FF141414FF141414FF1414
      14FF141414FF141414FF141414FF141414FF141414FF141414FF141414FF1414
      14FF141414FF141414FF141414FF090909FF090909FF141414FF141414FF1414
      14FF141414FF141414FF141414FF141414FF141414FF141414FF141414FF1414
      14FF141414FF141414FF141414FF090909FF141414FF181818FF181818FF1818
      18FF141414FF141414FF141414FF141414FF141414FF141414FF181818FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF181818FF141414FF141414FF141414FF141414FF141414FF141414FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF141414FF141414FF141414FF141414FF181818FF181818FF181818FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF181818FF141414FF141414FF141414FF141414FF141414FF141414FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF818181FFE1E1E1FFFFFFFFFFFFFFFFFFC1C1C1FF646464FF181818FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF181818FF555555FFD0D0D0FFFFFFFFFFEFEFEFFFA1A1A1FF252525FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF252525FFEFEFEFFFFFFFFFFF717171FF181818FF181818FF181818FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF252525FF838383FFD0D0D0FFFFFFFFFFE1E1E1FFA1A1A1FF252525FF1818
      18FF181818FF181818FF181818FF141414FF141414FF202020FF202020FF1818
      18FF959595FFE1E1E1FFC1C1C1FFD0D0D0FFFFFFFFFFFFFFFFFF646464FF1818
      18FF202020FF202020FF202020FF141414FF141414FF252525FF252525FF1818
      18FF585858FFFFFFFFFFFFFFFFFFC2C2C2FFE1E1E1FFFFFFFFFFD0D0D0FF1818
      18FF252525FF252525FF252525FF141414FF141414FF252525FF252525FF2525
      25FF181818FF858585FFFFFFFFFFE1E1E1FF181818FF252525FF252525FF2525
      25FF252525FF252525FF252525FF141414FF141414FF252525FF252525FF1818
      18FFA1A1A1FFFFFFFFFFF1F1F1FF838383FFC2C2C2FFFFFFFFFFD0D0D0FF1818
      18FF252525FF252525FF252525FF141414FF181818FF202020FF202020FF2020
      20FF303030FF202020FF202020FF202020FF898989FFFFFFFFFFE1E1E1FF2020
      20FF202020FF202020FF202020FF181818FF181818FF252525FF252525FF2525
      25FFB5B5B5FFFFFFFFFF898989FF252525FF252525FFE1E1E1FFFFFFFFFF7979
      79FF252525FF252525FF252525FF181818FF181818FF252525FF252525FF2525
      25FF252525FF303030FFF0F0F0FFFFFFFFFF797979FF252525FF252525FF2525
      25FF252525FF252525FF252525FF181818FF181818FF252525FF252525FF2525
      25FFFFFFFFFFFFFFFFFF6A6A6AFF252525FF303030FFFFFFFFFFFFFFFFFF4C4C
      4CFF252525FF252525FF252525FF181818FF202020FF2C2C2CFF2C2C2CFF2C2C
      2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF6E6E6EFFFFFFFFFFFFFFFFFF2020
      20FF2C2C2CFF2C2C2CFF2C2C2CFF202020FF252525FF2C2C2CFF2C2C2CFF2525
      25FFFFFFFFFFFFFFFFFF606060FF2C2C2CFF2C2C2CFFC2C2C2FFFFFFFFFF8989
      89FF2C2C2CFF2C2C2CFF2C2C2CFF252525FF252525FF2C2C2CFF2C2C2CFF2C2C
      2CFF2C2C2CFF2C2C2CFF8C8C8CFFFFFFFFFFD4D4D4FF2C2C2CFF2C2C2CFF2C2C
      2CFF2C2C2CFF2C2C2CFF2C2C2CFF252525FF252525FF2C2C2CFF2C2C2CFF2C2C
      2CFFE1E1E1FFFFFFFFFF8C8C8CFF2C2C2CFF606060FFFFFFFFFFFFFFFFFF4545
      45FF2C2C2CFF2C2C2CFF2C2C2CFF252525FF202020FF303030FF303030FF3030
      30FF383838FF646464FF646464FF818181FFE1E1E1FFFFFFFFFFC1C1C1FF2C2C
      2CFF303030FF303030FF303030FF202020FF252525FF303030FF303030FF2C2C
      2CFFFFFFFFFFFFFFFFFFD0D0D0FF606060FF898989FFFFFFFFFFFFFFFFFF7171
      71FF303030FF303030FF303030FF252525FF252525FF303030FF303030FF3030
      30FF303030FF303030FF383838FFF0F0F0FFFFFFFFFF636363FF303030FF3030
      30FF303030FF303030FF303030FF252525FF252525FF303030FF303030FF2C2C
      2CFF555555FFF1F1F1FFFFFFFFFFC2C2C2FFFFFFFFFFFFFFFFFFAAAAAAFF2C2C
      2CFF303030FF303030FF303030FF252525FF2C2C2CFF383838FF383838FF3030
      30FF646464FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1F1F1FF585858FF3030
      30FF383838FF383838FF383838FF2C2C2CFF2C2C2CFF383838FF383838FF3030
      30FFD0D0D0FFFFFFFFFFF1F1F1FFFFFFFFFFFFFFFFFFFFFFFFFFACACACFF3030
      30FF383838FF383838FF383838FF2C2C2CFF2C2C2CFF383838FF383838FF3838
      38FF383838FF383838FF303030FF909090FFFFFFFFFFC9C9C9FF303030FF3838
      38FF383838FF383838FF383838FF2C2C2CFF2C2C2CFF383838FF383838FF3838
      38FF383838FFBEBEBEFFFFFFFFFFFFFFFFFFFFFFFFFFE1E1E1FF383838FF3030
      30FF383838FF383838FF383838FF2C2C2CFF303030FF3F3F3FFF3F3F3FFF3F3F
      3FFF303030FFFFFFFFFFE1E1E1FF959595FF767676FF3F3F3FFF383838FF3F3F
      3FFF3F3F3FFF3F3F3FFF3F3F3FFF303030FF303030FF3F3F3FFF3F3F3FFF3838
      38FFA1A1A1FFFFFFFFFFACACACFF717171FF929292FF717171FF383838FF3838
      38FF3F3F3FFF3F3F3FFF3F3F3FFF303030FF303030FF3F3F3FFF3F3F3FFF3F3F
      3FFF3F3F3FFF3F3F3FFF383838FF3F3F3FFFF0F0F0FFFFFFFFFF686868FF3838
      38FF3F3F3FFF3F3F3FFF3F3F3FFF303030FF303030FF454545FF454545FF3838
      38FFA1A1A1FFFFFFFFFFD0D0D0FF4C4C4CFFAAAAAAFFFFFFFFFFBEBEBEFF3030
      30FF454545FF454545FF454545FF303030FF383838FF3F3F3FFF3F3F3FFF3F3F
      3FFF303030FFFFFFFFFFD0D0D0FF383838FF383838FF383838FF3F3F3FFF3F3F
      3FFF3F3F3FFF3F3F3FFF3F3F3FFF383838FF383838FF3F3F3FFF3F3F3FFF3F3F
      3FFF555555FFF1F1F1FFFFFFFFFF929292FF383838FF383838FF3F3F3FFF3F3F
      3FFF3F3F3FFF3F3F3FFF3F3F3FFF383838FF383838FF3F3F3FFF3F3F3FFF3F3F
      3FFF383838FF383838FF383838FF383838FF909090FFFFFFFFFFBDBDBDFF3838
      38FF3F3F3FFF3F3F3FFF3F3F3FFF383838FF383838FF454545FF454545FF3838
      38FFCACACAFFFFFFFFFF959595FF383838FF6A6A6AFFFFFFFFFFFFFFFFFF3030
      30FF454545FF454545FF454545FF383838FF3F3F3FFF4C4C4CFF4C4C4CFF4C4C
      4CFF383838FFD8D8D8FFFFFFFFFFCACACAFFCACACAFFCACACAFF898989FF3F3F
      3FFF4C4C4CFF4C4C4CFF4C4C4CFF3F3F3FFF3F3F3FFF4C4C4CFF4C4C4CFF4C4C
      4CFF3F3F3FFF898989FFFFFFFFFFFFFFFFFFF1F1F1FFCACACAFF898989FF3F3F
      3FFF4C4C4CFF4C4C4CFF4C4C4CFF3F3F3FFF3F3F3FFF4C4C4CFF4C4C4CFF3F3F
      3FFFA3A3A3FFC9C9C9FFC9C9C9FFC9C9C9FFD8D8D8FFFFFFFFFFFFFFFFFF5555
      55FF4C4C4CFF4C4C4CFF4C4C4CFF3F3F3FFF454545FF4C4C4CFF4C4C4CFF4545
      45FF7C7C7CFFFFFFFFFFF1F1F1FF959595FFD8D8D8FFFFFFFFFFBEBEBEFF4545
      45FF4C4C4CFF4C4C4CFF4C4C4CFF454545FF3F3F3FFF505050FF505050FF5050
      50FF3F3F3FFFCACACAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF989898FF4C4C
      4CFF505050FF505050FF505050FF3F3F3FFF3F3F3FFF555555FF555555FF5555
      55FF555555FF3F3F3FFF555555FFB5B5B5FFE1E1E1FFFFFFFFFF989898FF4C4C
      4CFF555555FF555555FF555555FF3F3F3FFF3F3F3FFF555555FF555555FF3F3F
      3FFFC9C9C9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7171
      71FF4C4C4CFF555555FF555555FF3F3F3FFF454545FF555555FF555555FF5555
      55FF454545FF7C7C7CFFD8D8D8FFFFFFFFFFF1F1F1FFB1B1B1FF4C4C4CFF4C4C
      4CFF555555FF555555FF555555FF454545FF4C4C4CFF505050FF505050FF5050
      50FF4C4C4CFF3F3F3FFF383838FF383838FF383838FF383838FF4C4C4CFF5050
      50FF505050FF505050FF505050FF4C4C4CFF4C4C4CFF555555FF555555FF5555
      55FF555555FF555555FF555555FF4C4C4CFF3F3F3FFF383838FF4C4C4CFF5555
      55FF555555FF555555FF555555FF4C4C4CFF4C4C4CFF555555FF555555FF4C4C
      4CFF3F3F3FFF383838FF383838FF383838FF383838FF383838FF383838FF4C4C
      4CFF555555FF555555FF555555FF4C4C4CFF4C4C4CFF555555FF555555FF5555
      55FF555555FF4C4C4CFF454545FF383838FF454545FF4C4C4CFF555555FF5555
      55FF555555FF555555FF555555FF4C4C4CFF4C4C4CFF585858FF585858FF5858
      58FF585858FF585858FF585858FF585858FF585858FF585858FF585858FF5858
      58FF585858FF585858FF585858FF4C4C4CFF4C4C4CFF585858FF585858FF5858
      58FF585858FF585858FF585858FF585858FF585858FF585858FF585858FF5858
      58FF585858FF585858FF585858FF4C4C4CFF4C4C4CFF595959FF595959FF5959
      59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
      59FF595959FF595959FF595959FF4C4C4CFF4C4C4CFF595959FF595959FF5959
      59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
      59FF595959FF595959FF595959FF4C4C4CFF000000004C4C4CFF505050FF5050
      50FF505050FF505050FF505050FF505050FF505050FF505050FF505050FF5050
      50FF505050FF505050FF4C4C4CFF00000000000000004C4C4CFF555555FF5555
      55FF555555FF555555FF555555FF555555FF555555FF555555FF555555FF5555
      55FF555555FF555555FF4C4C4CFF00000000000000004C4C4CFF555555FF5555
      55FF555555FF555555FF555555FF555555FF555555FF555555FF555555FF5555
      55FF555555FF555555FF4C4C4CFF00000000000000004C4C4CFF555555FF5555
      55FF555555FF555555FF555555FF555555FF555555FF555555FF555555FF5555
      55FF555555FF555555FF4C4C4CFF0000000000000000090909FF090909FF0909
      09FF090909FF090909FF090909FF090909FF090909FF090909FF090909FF0909
      09FF090909FF090909FF090909FF0000000000000000090909FF090909FF0909
      09FF090909FF090909FF090909FF090909FF090909FF090909FF090909FF0909
      09FF090909FF090909FF090909FF0000000000000000090909FF090909FF0909
      09FF090909FF090909FF090909FF090909FF090909FF090909FF090909FF0909
      09FF090909FF090909FF090909FF0000000000000000090909FF090909FF0909
      09FF090909FF090909FF090909FF090909FF090909FF090909FF090909FF0909
      09FF090909FF090909FF090909FF00000000090909FF141414FF141414FF1414
      14FF141414FF141414FF141414FF141414FF141414FF141414FF141414FF1414
      14FF141414FF141414FF141414FF090909FF090909FF141414FF141414FF1414
      14FF141414FF141414FF141414FF141414FF141414FF141414FF141414FF1414
      14FF141414FF141414FF141414FF090909FF090909FF141414FF141414FF1414
      14FF141414FF141414FF141414FF141414FF141414FF141414FF141414FF1414
      14FF141414FF141414FF141414FF090909FF090909FF141414FF141414FF1414
      14FF141414FF141414FF141414FF141414FF141414FF141414FF141414FF1414
      14FF141414FF141414FF141414FF090909FF141414FF181818FF181818FF1818
      18FF181818FF181818FF141414FF141414FF141414FF141414FF181818FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF141414FF141414FF141414FF141414FF141414FF141414FF141414FF1414
      14FF141414FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF181818FF141414FF141414FF141414FF141414FF141414FF141414FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF181818FF181818FF181818FF141414FF141414FF141414FF141414FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF181818FF181818FF141414FFFFFFFFFFFFFFFFFF555555FF181818FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF141414FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF141414FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF181818FF818181FFD0D0D0FFFFFFFFFFFFFFFFFFB1B1B1FF333333FF1818
      18FF181818FF181818FF181818FF141414FF141414FF181818FF181818FF1818
      18FF181818FF181818FF181818FF141414FFFFFFFFFFFFFFFFFF555555FF1818
      18FF181818FF181818FF181818FF141414FF141414FF202020FF202020FF2020
      20FF202020FF202020FF181818FFFFFFFFFFFFFFFFFF585858FF181818FF2020
      20FF202020FF202020FF202020FF141414FF141414FF202020FF202020FF2020
      20FF181818FFC2C2C2FFFFFFFFFFFFFFFFFFD0D0D0FFC2C2C2FFC2C2C2FFC2C2
      C2FF181818FF202020FF202020FF141414FF141414FF202020FF202020FF2020
      20FF181818FF959595FFE1E1E1FFC2C2C2FFE1E1E1FFFFFFFFFFE1E1E1FF1818
      18FF202020FF202020FF202020FF141414FF141414FF202020FF202020FF1818
      18FF181818FF181818FF181818FF181818FFFFFFFFFFFFFFFFFF585858FF1818
      18FF202020FF202020FF202020FF141414FF181818FF202020FF202020FF2020
      20FF202020FF202020FF202020FFFFFFFFFFFFFFFFFF585858FF202020FF2020
      20FF202020FF202020FF202020FF181818FF181818FF202020FF202020FF2020
      20FF202020FF202020FFA6A6A6FFFFFFFFFFF1F1F1FF4B4B4BFF202020FF2020
      20FF202020FF202020FF202020FF181818FF181818FF202020FF202020FF2020
      20FF202020FF3F3F3FFF202020FF202020FF202020FFE1E1E1FFFFFFFFFF5B5B
      5BFF202020FF202020FF202020FF181818FF181818FF202020FF202020FF4B4B
      4BFFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFFFFFFFFFFFFFFFFFD5D5D5FF7979
      79FF202020FF202020FF202020FF181818FF202020FF2C2C2CFF2C2C2CFF2C2C
      2CFF2C2C2CFF2C2C2CFF202020FFFFFFFFFFFFFFFFFF606060FF2C2C2CFF2C2C
      2CFF2C2C2CFF2C2C2CFF2C2C2CFF202020FF202020FF2C2C2CFF2C2C2CFF2C2C
      2CFF2C2C2CFF2C2C2CFF2C2C2CFFA9A9A9FFFFFFFFFFF1F1F1FF454545FF2C2C
      2CFF2C2C2CFF2C2C2CFF2C2C2CFF202020FF202020FF2C2C2CFF2C2C2CFF2C2C
      2CFF2C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF333333FFE1E1E1FFFFFFFFFF4545
      45FF2C2C2CFF2C2C2CFF2C2C2CFF202020FF202020FF2C2C2CFF2C2C2CFF6060
      60FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8C8C
      8CFF2C2C2CFF2C2C2CFF2C2C2CFF202020FF202020FF303030FF303030FF3030
      30FF303030FF303030FF2C2C2CFFFFFFFFFFFFFFFFFF606060FF303030FF3030
      30FF303030FF303030FF303030FF202020FF202020FF303030FF303030FF3030
      30FF303030FF303030FF303030FF2C2C2CFFA9A9A9FFFFFFFFFFD0D0D0FF2C2C
      2CFF303030FF303030FF303030FF202020FF202020FF333333FF333333FF3333
      33FF333333FF2C2C2CFF818181FFC2C2C2FFF1F1F1FFFFFFFFFF9C9C9CFF2C2C
      2CFF333333FF333333FF333333FF202020FF202020FF313131FF313131FF2C2C
      2CFFD5D5D5FFFFFFFFFF454545FF2C2C2CFFFFFFFFFFFFFFFFFF606060FF2C2C
      2CFF313131FF313131FF313131FF202020FF2C2C2CFF383838FF383838FF3838
      38FF383838FF383838FF2C2C2CFFFFFFFFFFFFFFFFFF606060FF303030FF3838
      38FF383838FF383838FF383838FF2C2C2CFF2C2C2CFF383838FF383838FF3838
      38FF383838FF383838FF383838FF303030FF383838FFE3E3E3FFFFFFFFFF8282
      82FF303030FF383838FF383838FF2C2C2CFF2C2C2CFF3F3F3FFF3F3F3FFF3F3F
      3FFF3F3F3FFF333333FF959595FFFFFFFFFFFFFFFFFFD0D0D0FF3F3F3FFF3F3F
      3FFF3F3F3FFF3F3F3FFF3F3F3FFF2C2C2CFF2C2C2CFF383838FF383838FF3131
      31FF606060FFFFFFFFFFBEBEBEFF2C2C2CFFFFFFFFFFFFFFFFFF606060FF3131
      31FF383838FF383838FF383838FF2C2C2CFF303030FF3F3F3FFF3F3F3FFF3F3F
      3FFF383838FF303030FF303030FFFFFFFFFFFFFFFFFF686868FF383838FF3F3F
      3FFF3F3F3FFF3F3F3FFF3F3F3FFF303030FF303030FF454545FF454545FF4545
      45FF454545FF383838FF383838FF454545FF383838FFA6A6A6FFFFFFFFFFBDBD
      BDFF303030FF454545FF454545FF303030FF333333FF454545FF454545FF4545
      45FF454545FF3F3F3FFF3F3F3FFF333333FFADADADFFFFFFFFFF959595FF3F3F
      3FFF454545FF454545FF454545FF333333FF313131FF454545FF454545FF4545
      45FF313131FFBEBEBEFFFFFFFFFF686868FFFFFFFFFFFFFFFFFF686868FF3838
      38FF454545FF454545FF454545FF313131FF383838FF3F3F3FFF3F3F3FFF3F3F
      3FFF383838FFD7D7D7FF878787FFFFFFFFFFFFFFFFFF686868FF3F3F3FFF3F3F
      3FFF3F3F3FFF3F3F3FFF3F3F3FFF383838FF383838FF454545FF454545FF4545
      45FF454545FF505050FF505050FF383838FF383838FFA9A9A9FFFFFFFFFFBDBD
      BDFF383838FF454545FF454545FF383838FF3F3F3FFF454545FF454545FF4545
      45FF454545FF505050FF3F3F3FFF3F3F3FFF797979FFFFFFFFFFCACACAFF3F3F
      3FFF454545FF454545FF454545FF3F3F3FFF383838FF454545FF454545FF4545
      45FF454545FF555555FFF2F2F2FFAFAFAFFFFFFFFFFFFFFFFFFF686868FF4545
      45FF454545FF454545FF454545FF383838FF3F3F3FFF4B4B4BFF4B4B4BFF4B4B
      4BFF555555FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF686868FF4B4B4BFF4B4B
      4BFF4B4B4BFF4B4B4BFF4B4B4BFF3F3F3FFF454545FF4B4B4BFF4B4B4BFF4B4B
      4BFF454545FFB0B0B0FFFFFFFFFFCACACAFFD8D8D8FFFFFFFFFFFFFFFFFF8989
      89FF454545FF4B4B4BFF4B4B4BFF454545FF454545FF4C4C4CFF4C4C4CFF4C4C
      4CFF454545FFA3A3A3FFE1E1E1FFCACACAFFF1F1F1FFFFFFFFFFA3A3A3FF4545
      45FF4C4C4CFF4C4C4CFF4C4C4CFF454545FF454545FF4B4B4BFF4B4B4BFF4B4B
      4BFF4B4B4BFF454545FF969696FFFFFFFFFFFFFFFFFFFFFFFFFF686868FF4B4B
      4BFF4B4B4BFF4B4B4BFF4B4B4BFF454545FF3F3F3FFF555555FF555555FF5555
      55FF3F3F3FFF555555FFB1B1B1FFFFFFFFFFFFFFFFFF707070FF4B4B4BFF5555
      55FF555555FF555555FF555555FF3F3F3FFF454545FF505050FF505050FF5050
      50FF4B4B4BFF707070FFBDBDBDFFF1F1F1FFFFFFFFFFE3E3E3FF989898FF4545
      45FF505050FF505050FF505050FF454545FF454545FF505050FF505050FF5050
      50FF4C4C4CFF9C9C9CFFD8D8D8FFFFFFFFFFFFFFFFFFB1B1B1FF4C4C4CFF4C4C
      4CFF505050FF505050FF505050FF454545FF454545FF555555FF555555FF5555
      55FF555555FF555555FF454545FFE5E5E5FFFFFFFFFFFFFFFFFF707070FF4B4B
      4BFF555555FF555555FF555555FF454545FF4B4B4BFF555555FF555555FF5555
      55FF555555FF555555FF4B4B4BFF383838FF383838FF4B4B4BFF555555FF5555
      55FF555555FF555555FF555555FF4B4B4BFF4B4B4BFF505050FF505050FF5050
      50FF505050FF505050FF454545FF454545FF383838FF454545FF4B4B4BFF5050
      50FF505050FF505050FF505050FF4B4B4BFF4C4C4CFF505050FF505050FF5050
      50FF505050FF4C4C4CFF454545FF3F3F3FFF3F3F3FFF4C4C4CFF505050FF5050
      50FF505050FF505050FF505050FF4C4C4CFF4B4B4BFF555555FF555555FF5555
      55FF555555FF555555FF4B4B4BFF454545FF383838FF383838FF4B4B4BFF5555
      55FF555555FF555555FF555555FF4B4B4BFF4B4B4BFF585858FF585858FF5858
      58FF585858FF585858FF585858FF585858FF585858FF585858FF585858FF5858
      58FF585858FF585858FF585858FF4B4B4BFF4B4B4BFF595959FF595959FF5959
      59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
      59FF595959FF595959FF595959FF4B4B4BFF4C4C4CFF5B5B5BFF5B5B5BFF5B5B
      5BFF5B5B5BFF5B5B5BFF5B5B5BFF5B5B5BFF5B5B5BFF5B5B5BFF5B5B5BFF5B5B
      5BFF5B5B5BFF5B5B5BFF5B5B5BFF4C4C4CFF4B4B4BFF585858FF585858FF5858
      58FF585858FF585858FF585858FF585858FF585858FF585858FF585858FF5858
      58FF585858FF585858FF585858FF4B4B4BFF000000004B4B4BFF555555FF5555
      55FF555555FF555555FF555555FF555555FF555555FF555555FF555555FF5555
      55FF555555FF555555FF4B4B4BFF00000000000000004B4B4BFF505050FF5050
      50FF505050FF505050FF505050FF505050FF505050FF505050FF505050FF5050
      50FF505050FF505050FF4B4B4BFF00000000000000004C4C4CFF505050FF5050
      50FF505050FF505050FF505050FF505050FF505050FF505050FF505050FF5050
      50FF505050FF505050FF4C4C4CFF00000000000000004B4B4BFF555555FF5555
      55FF555555FF555555FF555555FF555555FF555555FF555555FF555555FF5555
      55FF555555FF555555FF4B4B4BFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000090909FF090909FF0909
      09FF090909FF090909FF090909FF090909FF090909FF090909FF090909FF0909
      09FF090909FF090909FF090909FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEFE
      FFFFA6A599FFB1AFACFFDADADAFF000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEFE
      FFFFA6A599FFB1AFACFFDADADAFF00000000090909FF141414FF141414FF1414
      14FF141414FF141414FF141414FF141414FF141414FF141414FF141414FF1414
      14FF141414FF141414FF141414FF090909FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFFFF6C96
      FFFF4292F6FFEEE9DFFFAFADABFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFFFF6C96
      FFFF4292F6FFEEE9DFFFAFADABFF00000000141414FF181818FF181818FF1818
      18FF181818FF141414FF141414FF141414FF141414FF141414FF181818FF1818
      18FF181818FF181818FF181818FF141414FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFDFFFF6A95FFFF429A
      FFFF69DAFFFF5CADF5FFA89F91FF000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFDFFFF6A95FFFF429A
      FFFF69DAFFFF5CADF5FFA89F91FF00000000141414FF181818FF181818FF1818
      18FF181818FF555555FFD0D0D0FFFFFFFFFFE0E0E0FF838383FF181818FF1818
      18FF181818FF181818FF181818FF141414FF0000FFFF00000000000000000000
      00000000000000000098000000CC000000CC000000CC000000CC000000980000
      00000000000000000000000000000000FFFF0000000000000000000000000000
      000000000000000000000000000000000000F6F7F6FF5F88FDFF459FFFFF6EDA
      FFFF4FACFFFF5285FFFFF5F6FFFF000000000000000000000000000000000000
      000000000000000000000000000000000000F6F7F6FF5F88FDFF459FFFFF6EDA
      FFFF4FACFFFF5285FFFFF5F6FFFF00000000141414FF202020FF202020FF2020
      20FF383838FFF1F1F1FFFFFFFFFFC2C2C2FFF1F1F1FFFFFFFFFF646464FF1818
      18FF202020FF202020FF202020FF141414FF0000FFFF0000FFFF000000000000
      000000000000000000BA0000002F00000000000000000000002F000000BA0000
      000000000000000000000000FFFF0000FFFF0000000000000000000000000000
      0000E2E3E3FFC7C8C9FFD2D3D4FFF9F9F9FFBFBBB7FF6F91B2FF60D3FFFF4DAA
      FFFF5988FFFFF8F8FFFF00000000000000000000000000000000000000000000
      0000E2E3E3FFC7C8C9FFD2D3D4FFF9F9F9FFBFBBB7FF6F91B2FF60D3FFFF4DAA
      FFFF5988FFFFF8F8FFFF0000000000000000181818FF202020FF202020FF2020
      20FFA6A6A6FFFFFFFFFFA6A6A6FF202020FF797979FFFFFFFFFFE0E0E0FF2020
      20FF202020FF202020FF202020FF181818FF0000FFFF0000FFFF0000FFFF0000
      000000000000000000830000002C00000000000000000000002C000000830000
      0000000000000000FFFF0000FFFF0000FFFF0000000000000000EBEBEBFF9799
      9BFFB3A081FFD2B588FFC3AA83FF8F8D87FF717377FFFFF7F0FF6A93BDFF5482
      FEFFF9F9FFFF0000000000000000000000000000000000000000EBEBEBFF9799
      9BFFB3A081FFD2B588FFC3AA83FF8F8D87FF717377FFFFF7F0FF6A93BDFF5482
      FEFFF9F9FFFF000000000000000000000000202020FF2C2C2CFF2C2C2CFF2C2C
      2CFFD0D0D0FFFFFFFFFF6E6E6EFF2C2C2CFF343434FFFFFFFFFFFFFFFFFF3434
      34FF2C2C2CFF2C2C2CFF2C2C2CFF202020FF0000FFFF0000FFFF0000FFFF0000
      FFFF000000000000000000000000000000000000000000000000000000000000
      00000000FFFF0000FFFF0000FFFF0000FFFF00000000F2F2F2FF98958EFFF5CB
      84FFF5CB84FFF1C885FFF5CE8EFFFCD08CFFAE9E85FF75767AFFBFBAAFFFF7F8
      FBFF0000000000000000000000000000000000000000F2F2F2FF98958EFFF5CB
      84FFF5CB84FFC1E0AAFFDAB77DFFFCD08CFFAE9E85FF75767AFFBFBAAFFFF7F8
      FBFF00000000000000000000000000000000202020FF343434FF343434FF2C2C
      2CFFFFFFFFFFFFFFFFFF646464FF343434FF2C2C2CFFFFFFFFFFFFFFFFFF6464
      64FF343434FF343434FF343434FF202020FF0000FFFF0000FFFF0000FFFF0000
      FFFF000000000000000000000000000000000000000000000000000000000000
      00000000FFFF0000FFFF0000FFFF0000FFFF00000000AEB1B4FFECCB8EFFF3D1
      92FFEECE92FFEDCC8EFFECC784FFEDC687FFFDD28FFF97948EFFFEFEFEFF0000
      00000000000000000000000000000000000000000000AEB1B4FFECCB8EFFF3D1
      92FFD3EDBBFF008100FFC8E2B1FFEDC687FFFDD28FFF97948EFFFEFEFEFF0000
      0000000000000000000000000000000000002C2C2CFF383838FF383838FF2C2C
      2CFFFFFFFFFFFFFFFFFF646464FF343434FF2C2C2CFFFFFFFFFFFFFFFFFF6464
      64FF343434FF383838FF383838FF2C2C2CFF0000FFFF0000FFFF0000FFFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FFFF0000FFFF0000FFFF00000000A4A098FFFEDFA1FFDACA
      AEFFB2A58FFFB1A189FFB09D83FFAD9776FFC8AA7BFFC1A987FFE2E2E3FF0000
      00000000000000000000000000000000000000000000A4A098FFFEDFA1FFCAE8
      BCFFCFE9CAFF059609FFCFE7C7FFC8E2B1FFDBBB87FFC1A987FFE2E2E3FF0000
      000000000000000000000000000000000000343434FF3F3F3FFF3F3F3FFF3434
      34FFD0D0D0FFFFFFFFFF767676FF383838FF3F3F3FFFFFFFFFFFFFFFFFFF4C4C
      4CFF383838FF3F3F3FFF3F3F3FFF343434FF0000FFFF0000FFFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FFFF0000FFFF00000000AAA595FFFFEBB9FF0014
      A4FF00119AFF000D8FFF000983FF000474FFDCBC85FFCFB68DFFD5D5D7FF0000
      00000000000000000000000000000000000000000000AAA595FFC5EBBCFF0091
      00FF059508FF049608FF049507FF008100FFC1E3AFFFCFB68DFFD5D5D7FF0000
      000000000000000000000000000000000000383838FF3F3F3FFF3F3F3FFF3838
      38FFAFAFAFFFFFFFFFFFAFAFAFFF383838FF838383FFFFFFFFFFE0E0E0FF3838
      38FF3F3F3FFF3F3F3FFF3F3F3FFF383838FF0000FFFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFF00000000ABA9A6FFFFF7CBFFE4E6
      E8FFE4E6E9FFE0DBCBFFDCCFB8FFD8C4A5FFFAD594FFAF9F85FFEEEFEFFF0000
      00000000000000000000000000000000000000000000ABA9A6FFFFF7CBFFD9FD
      DBFFC4F0CAFF059609FFD0EACBFFD3EFBFFFFAD594FFAF9F85FFEEEFEFFF0000
      0000000000000000000000000000000000003F3F3FFF4C4C4CFF4C4C4CFF4C4C
      4CFF555555FFF1F1F1FFFFFFFFFFCACACAFFF1F1F1FFFFFFFFFF898989FF3F3F
      3FFF4C4C4CFF4C4C4CFF4C4C4CFF3F3F3FFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D5D6D7FFD5CCB1FFFFFF
      F2FFFFFFF2FFFBF3D2FFF6E2B4FFF7D99EFFF6D393FFA1A1A4FF000000000000
      00000000000000000000000000000000000000000000D5D6D7FFD5CCB1FFFFFF
      F2FFD9FFDFFF009300FFD7E6C1FFF7D99EFFF6D393FFA1A1A4FF000000000000
      0000000000000000000000000000000000003F3F3FFF555555FF555555FF5555
      55FF3F3F3FFF646464FFD8D8D8FFFFFFFFFFE0E0E0FF989898FF3F3F3FFF5555
      55FF555555FF555555FF555555FF3F3F3FFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEFEFEFFBBBBBBFFD6CF
      B6FFFFFFDCFFFFF6CAFFFFEBB1FFEDD49CFF989590FFEDEDEEFF000000000000
      00000000000000000000000000000000000000000000FEFEFEFFBBBBBBFFD6CF
      B6FFFFFFDCFFC5EFC1FFFFEBB1FFEDD49CFF989590FFEDEDEEFF000000000000
      0000000000000000000000000000000000004C4C4CFF555555FF555555FF5555
      55FF555555FF4C4C4CFF3F3F3FFF383838FF3F3F3FFF4C4C4CFF555555FF5555
      55FF555555FF555555FF555555FF4C4C4CFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFEFFCFCF
      D1FFA3A4A0FFA8A397FF9D9A92FFAAABB0FFF1F1F1FF00000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFEFFCFCF
      D1FFA3A4A0FFA8A397FF9D9A92FFAAABB0FFF1F1F1FF00000000000000000000
      0000000000000000000000000000000000004C4C4CFF595959FF595959FF5959
      59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
      59FF595959FF595959FF595959FF4C4C4CFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004C4C4CFF555555FF5555
      55FF555555FF555555FF555555FF555555FF555555FF555555FF555555FF5555
      55FF555555FF555555FF4C4C4CFF000000000000000000000000000000000000
      0000A3672CFF0000000000000000000000000000000000000000000000000000
      000000000000A3672CFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000044251081D38044FF5256
      54FF525654FF525654FF525654FF525654FF525654FF525654FF525654FF5256
      54FFD38044FFD38044FFD38044FF442510810000000000000000000000000000
      000000000000A3672CFF00000000000000000000000000000000000000000000
      0000A3672CFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000170F092354371F7D925F35DBA06738F39F6537F38E592FDB5032197D160D
      06230000000000000000000000000000000044251081D38044FFCCA98EFF927A
      62FFE3E3E3FFD6D6D6FFBABABAFFBABABAFFBABABAFFB9B9B9FFB9B9B9FF947B
      62FFCE9F71FFDCB898FFEAD2BEFFD38044FF0000000000000000000000000000
      00000000000000000000A3672CFF00000000000000000000000000000000A367
      2CFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003B28
      1A53A16F46E6D7BBA3FFE9DACAFFECE0D1FFECE0D1FFE8D8C8FFD3B59CFF9661
      34E636221253000000000000000000000000D38044FFCCA98EFFDCB898FF927A
      61FFE1E1E1FFE6E6E6FF525654FF525654FFC5C5C5FFC2C2C2FFB2B2B2FF9379
      60FFCE9E70FFDBB694FFEAD2BEFFD38044FF000000000000000000000000A367
      2CFFA3672CFFA3672CFF00000000C19262FFC19262FFC19262FFC19262FF0000
      0000A3672CFFA3672CFFA3672CFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003D2C1E53B381
      56F4E7D5C4FFE5D2BFFFC9A685FFB88E66FFB68A64FFC5A180FFE0CCBAFFE3D0
      BEFFA36A3DF4372313530000000000000000D38044FFEAD2BEFFCF9F71FF927A
      61FFC5C5C5FFE1E1E1FF525654FF525654FFC5C5C5FFC5C5C5FFBFBFBFFF9379
      60FFCE9E70FFDBB694FFEAD2BEFFD38044FF0000000000000000000000000000
      00000000000000000000A3672CFF00000000000000000000000000000000A367
      2CFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001A130E22AF835EE5EAD8
      C9FFE3CDBAFFC0946AFFBA8C61FFCFB094FFCFB094FFB7895EFFB28760FFDAC0
      AAFFE4D1C0FF9C683EE5170F082200000000D38044FFEAD2BEFFCF9F71FF9279
      60FFC5C5C5FFC5C5C5FF525654FF525654FFE2E2E2FFC5C5C5FFB9B9B9FF9379
      60FFCE9E70FFDBB694FFEAD2BEFFD38044FF0000000000000000000000000000
      000000000000A3672CFF00000000000000000000000000000000000000000000
      0000A3672CFF0000000000000000000000000000000A00000000000000000000
      000000000000041B0F7C00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000644E3B7EE4CCB9FFEAD6
      C5FFC79970FFBF9065FFBF9065FFF7F1ECFFF6F0EAFFB7895EFFB7895EFFB589
      62FFE2CEBBFFD9BDA6FF583C257E00000000D38044FFEAD2BEFFCF9F71FF9C7D
      5FFFBABABAFFC5C5C5FFC5C5C5FFE1E1E1FFE7E7E7FFD7D7D7FFBABAB9FF997A
      5CFFCE9E6FFFDBB694FFEAD2BEFFD38044FF0000000000000000000000000000
      0000A3672CFF0000000000000000000000008C663FC08E6945BF000000000000
      000000000000A3672CFF00000000000000000000000600000000000000000000
      000000000000125D38F2094527CF000000000000000000000004000000000000
      00000000000000000000000000000000000000000000B58F71DBEFE1D3FFD9B5
      95FFC7986BFFC39568FFC19366FFBF9065FFBF9065FFBB8B62FFB98A62FFB88A
      61FFCBA786FFEADCCCFF9E704ADB00000000D38044FFEAD2BEFFCF9F71FFB08C
      68FF947B61FF947B61FF947B61FF947B61FF947B61FF947B61FF947B61FFB08C
      68FFDBB694FFCF9F71FFEAD2BEFFD38044FF0000000000000000000000000000
      00000000000000000000000000008A633BC1A96F36FFAA7037FF8E6944C00000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001D7F4EF844B47CFD08512DBD03100A1D00000000000000000000
      00000000000000000000000000000000000000000000D1AA8AF6F2E4D9FFD1A5
      79FFC5996AFFC49769FFC49668FFFAF6F2FFF3EAE1FFC2956CFFBE8F64FFBE8F
      63FFC0956CFFEFE3D5FFB8875DF600000000D38044FFEAD2BEFFCF9F71FFCF9F
      71FFCF9F71FFCF9F71FFCF9F71FFCF9F71FFCF9F71FFCF9F71FFCF9F71FFCF9F
      71FFCF9F71FFCF9F71FFEAD2BEFFD38044FF0000000000000000000000000000
      00000000000000000000895F36C3A96F36FFBE8D5CFFC09060FFAE753EFF8F6A
      44C200000000000000000000000000000000147746D10F7A45E2117A46E0127A
      46E0137947DF1B8852F55DDA9CFF47B680FD0B6539BF030F091B000000000000
      00000000000000000000000000000000000000000000D8B293F6F2E5DAFFD1A6
      7DFFCC9D70FFC79A6BFFC5986AFFE2CCB6FFF8F3EEFFF6EEE8FFD9BDA1FFC294
      67FFC59B70FFF0E2D6FFBE9067F600000000D38044FFEAD2BEFFCF9F71FFFEFE
      FEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
      FEFFFCFCFCFFCF9F71FFEAD2BEFFD38044FF0000000000000000000000000000
      0000000000008F6B46BF8F5C2BDAAA7037FFC09060FFC19262FFB07841FF9667
      38DA987859BF0000000000000000000000004AB480FD91E3BAFF82DDB0FF7BDC
      ACFF6ED9A4FF5ED49BFF46D08BFF47D18DFF45BB81FD116F40B1010B06120000
      00000000000000000000000000000000000000000000C5A58CDBF3E5D9FFDFBB
      9EFFCFA074FFCD9E71FFF5EBE3FFE4CBB4FFE7D3BFFFFBF8F6FFE5D3BFFFC498
      6AFFD6B491FFEEE0D2FFAF8866DB00000000D38044FFEAD2BEFFCF9F71FFFEFE
      FEFFF4F4F4FF000000FF01588FFFC3D5DFFFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFCF9F71FFEAD2BEFFD38044FF0000000000000000000000000000
      0000000000000000000000000000A3672CFFC39566FFC39566FFA96F36FF0000
      00000000000000000000000000000000000053C38DFD9CE6C1FF8AE1B6FF8BE0
      B6FF87E0B5FF86DFB4FF7CDEAEFF82DFB1FF61CB98FD117D47B1020D07120000
      000000000000000000000000000000000000000000007463557EF4E3D4FFEFDC
      CDFFD5A87DFFD0A076FFFBF8F5FFFCF8F5FFFCF8F5FFFBF8F5FFD1A881FFCFA4
      7AFFEAD5C3FFEAD4C2FF6752407E00000000D38044FFEAD2BEFFCF9F71FFFEFE
      FEFFD7D7D7FF01588FFF25414BFF35566AFF01588FFFACBDC8FFD7D7D7FFD7D7
      D7FFF3F3F3FFCF9F71FFEAD2BEFFD38044FF0000000000000000000000000000
      0000000000000000000000000000AE753EFFC39566FFC39566FFA36F3CEC0000
      0000000000000000000000000000000000001BA05ED118AA61E21AA962E01AA9
      62E01AA861DF27BC71F5ACEACBFF78D9AAFD0C904EBF04160D1B000000000000
      00000000000000000000000000000000000000000000201B1822D6B9A1E5F6E9
      DDFFECD8C6FFD7AC81FFDCBB9AFFF6ECE3FFF5ECE2FFE4C8AEFFD2A77AFFE6CE
      BAFFF1E2D5FFC4A182E51C17122200000000D38044FFEAD2BEFFCF9F71FFFEFE
      FEFFF4F4F4FFF4F4F4FF385A6FFF8AABC2FF5485A3FF01588FFFC3D5DFFFF4F4
      F4FFF4F4F4FFCE9E70FFEAD2BEFFD38044FF0000000000000000000000000000
      00000000000000000000986532E5BF8F5EFFC19262FFBD8C5AFF966F48CB0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000036CC81F88DE0B7FD0B9B52BF051B101F00000000000000000000
      00000000000000000000000000000000000000000000000000004F453C53E7C9
      B0F4F7EADFFFEEDED0FFE3C1A7FFD8AE89FFD7AC86FFDDBB9CFFEBD6C7FFF3E6
      D9FFD9B698F4493C32530000000000000000D38044FFEAD2BEFFCF9F71FFF9F9
      F9FFE2E2E2FFE2E2E2FF01588FFFC7EBEFFF69ACD2FF5687A4FF01588FFF91B0
      C4FFF9F9F9FFCE9E70FFEAD2BEFFD38044FF97581AFF98591CFF9A5C1FFF9C5E
      21FF9D5F23FFA3672CFFBE8D5CFFC39566FFC6996CFFB7834EFF9F8368C00000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003AC581EF21A161C2071A111F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005046
      3E53DDC1AAE6F9E9DCFFF6E8DDFFF3E5DAFFF3E5DAFFF5E7DCFFF5E4D6FFD4B4
      9BE64B403753000000000000000000000000D38044FFEAD2BEFFCF9F71FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FF01588FFFC7EBEFFF69ACD2FF5483A0FF0158
      8FFFC7D9E3FFCF9F71FFEAD2BEFFD38044FF98591CFFB7834EFFB88450FFBA87
      54FFBB8956FFC19262FFC09060FFC5986AFFBB8956FF96714BC8000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000154A3057091E1323000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000221E1A23796A5F7DD4B9A5DBEACDB5F3E8CBB3F3D0B59FDB7666597D201C
      182300000000000000000000000000000000D38044FFEAD2BEFFEED9C8FF3DAF
      FCFF3DAFFCFF3DAFFCFF3DAFFCFF3DAFFCFF01588FFFC6EAEEFF70ADCFFF0158
      8FFF3DAFFCFFEAD2BEFFEAD2BEFFD38044FF9B5D20FF9D5F23FF9E6024FFA164
      29FFA3672CFFAA7037FFB7834EFF9C7F63C00000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000583014A7D38044FFD38044FF005B
      CEFF005BCEFF005BCEFF005BCEFF005BCEFF005BCEFF01588FFF01588FFF0059
      B7FF005BCEFFD38044FFD38044FF583014A70000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000276D2CFF246828FF206324FF1D5F21FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001810092355391E7D936333DBA16A34F3A06832F38E5C2ADB5133177D160E
      0623000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002C7432FF73BD79FF71BD77FF216425FF000000000000
      00000000000000000000000000000000000000000000000000000000FFFF0000
      FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000
      FFFF0000FFFF0000FFFF00000000000000000000000000000000000000003B29
      1853A06F3EE6B47E49FFD0A983FFD9B898FFD9B898FFCDA47BFFAC733BFF955F
      2BE635210E530000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000327C38FF78C07DFF75BF7BFF256A2AFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003B2B1A53AF7D
      4BF4C6996CFFE1C6ACFFD5B18EFFCAA075FFC99E72FFD5B18EFFE0C4A9FFBF8F
      5EFF9E652DF435210E5300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001747
      19B8000200060000000000000000000000000000000000000000000000000000
      0000000000000000000038853EFF7CC282FF79C180FF2A712FFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000754516C4764514C900000000000000000000
      0000000000000000000000000000000000000000000019120C22A87B4DE5C99E
      72FFE1C6ACFFC99E72FFC79B6EFFC6996CFFC49668FFC49668FFC6996CFFE0C4
      A9FFBF8F5EFF945E2AE5150D0522000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000009180A382365
      27F71B531ED90000000000000000000000000000000000000000000000000000
      000000000000000000003E8D45FF81C587FF7DC385FF307935FF000000000000
      0000000000000000000000000000000000000000000000000000868688FF8686
      88FF868688FF868688FF81501FCFAF7740FFAD743CFF7C4816D1868688FF8686
      88FF868688FF868688FF0000000000000000000000005E462E7EBF8F5EFFE2C8
      AEFFCCA379FFC99E72FFC99E72FFFEFDFCFFFEFDFCFFC6996CFFC49668FFC699
      6CFFE0C4A9FFAE753EFF5134177E000000000000000060BE6CFF5CB867FF57B1
      61FF52A95BFF4CA155FF46994EFF409148FF3A8841FF34803AFF3E8844FF58A1
      5DFF438B48FF1A501DCF00000000000000000000000000000000000000000000
      0000000000000000000044954BFF85C78CFF82C689FF35823CFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000040201068F5D2CD9B5804AFFCCA379FFCCA379FFAE753EFF824C17DB0302
      00060000000000000000000000000000000000000000A67E56DBD5B18EFFD8B6
      95FFCDA47BFFCAA075FFBF8F5EFFFEFDFCFFFEFDFCFFC6996CFFC6996CFFC79B
      6EFFD5B18EFFCDA47BFF905E2DDB000000000000000064C370FFA0D7A9FF9CD5
      A5FF98D3A1FF94D09DFF90CE98FF8BCB93FF87C98EFF82C689FF7DC384FF79C1
      80FF75BE7BFF448C49FF194B1BC4000000000000000000000000000000000000
      00000000000000000000499E52FF8ACA91FF87C98EFF3B8A42FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007E562EB8A66F39F7C19262FFCEA67DFFCDA47BFFB17943FF95591EF76F41
      15BB0000000000000000000000000000000000000000BD9164F6E2C8AEFFD5B1
      8EFFD2AC87FFFEFDFCFFFEFDFCFFFEFDFCFFFEFDFCFFFEFDFCFFFEFDFCFFC699
      6CFFCAA075FFDCBD9FFFA36B34F6000000000000000067C773FFA5DAAEFFA2D8
      ABFF9ED6A7FF9AD4A3FF96D29FFF93CF9AFF8ECC95FF89CA90FF85C78BFF81C5
      87FF7CC282FF49914FFF1D5220C9000000000000000000000000000000000000
      000000000000000000004FA658FF8ECC95FF8BCB93FF419249FF000000000000
      0000000000000000000000000000000000000000000000000000868688FF8686
      88FF868688FF868688FFB5804AFFD0A983FFCFA881FFA2652AFF868688FF8686
      88FF868688FF868688FF000000000000000000000000BF9368F6E6CFB8FFDABA
      9AFFD1AB85FFFEFDFCFFFEFDFCFFFEFDFCFFFEFDFCFFFEFDFCFFFEFDFCFFC699
      6CFFCCA379FFDCBD9FFFA66F39F6000000000000000067C773FF67C773FF64C3
      70FF60BE6CFF5CB867FF57B161FF52A95BFF4CA155FF46994EFF409148FF59A3
      61FF549D5BFF26612BD100000000000000000000000000000000000000000000
      0000000000000000000055AD5EFF93CF9AFF90CE98FF479A4FFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B17943FFD2AC87FFD1AB85FFA66B31FF000000000000
      00000000000000000000000000000000000000000000AB855EDBE2C8AEFFE2C8
      AEFFD3AE8AFFD1AB85FFCAA075FFFEFDFCFFFEFDFCFFC09060FFC99E72FFC79B
      6EFFD8B695FFD2AC87FF956435DB000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003E8C
      46F7327538DB0000000000000000000000000000000000000000000000000000
      000000000000000000005AB464FF96D29FFF94D09CFF5CAC64FF0F2211380000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B7834EFFD3AE8AFFD3AE8AFFAB7139FF000000000000
      00000000000000000000000000000000000000000000634F3A7ED3AE8AFFF0E1
      D3FFDDBFA2FFD8B695FFD2AC87FFFEFDFCFFFEFDFCFFD3AE8AFFCDA47BFFCFA8
      81FFE3CAB1FFB7834EFF563B1F7E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003370
      39BB010301060000000000000000000000000000000000000000000000000000
      0000498F52BB5EB96AF778C683FF9AD4A3FF98D3A1FF7CC386FF4C9F54F73572
      3BB8000000000000000000000000000000000000000000000000868688FF8686
      88FF868688FF868688FFBE9368FFD2B597FFD1B394FFB68756FF868688FF8686
      88FF868688FF868688FF0000000000000000000000001B151022B59069E5E1C6
      ACFFF1E3D6FFDCBD9FFFD5B18EFFD4B08CFFD9B898FFD8B695FFD7B593FFE4CB
      B3FFC99E72FFA17142E517100922000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000204020657A861DB7BCA87FF9ED6A7FF9CD4A5FF72C07CFF48924FD90103
      0206000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C29A70FFD4B89CFFD3B699FFBA8D60FF000000000000
      000000000000000000000000000000000000000000000000000042352753C199
      70F4E2C8AEFFF3E7DCFFE7D1BBFFDFC3A7FFDDBFA2FFE2C8AEFFE9D4C0FFCFA8
      81FFB07E4DF43B2A185300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000053A15DD17DCA88FF7AC885FF4B9554CF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C49D74FFD5B99EFFD4B89CFFBD9266FF000000000000
      0000000000000000000000000000000000000000000000000000000000004235
      2753B6906AE6D8B695FFEAD6C3FFEEDECEFFEBD8C6FFE3CAB1FFC99E72FFAA7E
      51E63C2C1C530000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000509C5AC94D9555C400000000000000000000
      0000000000000000000000000000000000000000000000000000868688FF8686
      88FF868688FF868688FFC59E76FFC59E76FFC29A70FFC1986DFF868688FF8686
      88FF868688FF868688FF00000000000000000000000000000000000000000000
      00001C161023634E397DAC8862DBBD9469F3BB9165F3A78058DB5E47307D1A13
      0C23000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000001C501FC4194E1DC900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D7B597EBE7C09FFFE3BC9AFFE0B7
      95FFDDB28FFFD9AE8AFFD6A985FFD3A57EFFD0A07AFFCD9C75FFCA9971FFC896
      6DFFC6936AFFC49068FFC49067FFB4855FEB0000000000000000000000000000
      00000000000000000000806C5DFF7A5F4AFF7C614CFF806C5DFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000245E29CF478F4CFF448C49FF1B521FD1000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EECBABFFE8D5C8FFE8D4C5FFE7D2
      C3FFE5D0C1FFE5CEBEFFE3CCBCFFE3CAB9FFE2C9B7FFE1C7B5FFE0C5B3FFDFC4
      B1FFDFC4B0FFDEC2AFFFDEC2AEFFC49067FF0000000000000000000000000000
      000000000000000000007B604BFFECCEB5FFECCEB5FF7B604BFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000010301062F6F34D9509956FF78C07DFF75BF7BFF458D4AFF1D5721DB0002
      0006000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F2D0B1FFEAD8CCFFFCFCFCFFDABA
      A4FFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFCCA182FFFCFCFCFFDFC3B0FFC59269FF0000000000000000000000000000
      00000000000000000000856245FFEFCFB2FFCBAB8EFF856245FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002F6935B83A8842F764AD6BFF7CC282FF79C180FF4A924FFF25692AF71A4B
      1DBB000000000000000000000000000000000000000000000000000000000103
      0206336E38B80000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F5D4B6FFECDCD0FFDEC3AFFFDDBF
      ABFFDBBDA7FFD9BAA3FFD8B79FFFD6B49CFFD5B197FFD3AE94FFD1AB8FFFD0A8
      8CFFCFA688FFCDA385FFE0C5B3FFC7956DFF0000000000000000000000000000
      000000000000000000008E6241FFF6D0AEFFD2AC8AFF8E6241FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000E2010384D9A54FF81C587FF7DC385FF307935FF000000000000
      0000000000000000000000000000000000000000000000000000000000004690
      4DD94A9C52F70F21113800000000000000000000000000000000000000000000
      000000000000000000000000000000000000F8D8BBFFEDDED4FFFCFCFCFFFCFC
      FCFFDEC2AEFFFCFCFCFFDBBCA6FFFCFCFCFFD8B69EFFFCFCFCFFD5B097FFFCFC
      FCFFFCFCFCFFFCFCFCFFE1C8B6FFCA9971FF0000000000000000806C5DFF7B60
      4CFF836147FF8A6242FF8E6241FFD2AC8AFFD4AB8AFF906240FF8E6241FF8761
      45FF7D6149FF826C5DFF00000000000000000000000000000000000000000000
      0000000000000000000044954BFF85C78CFF82C689FF35823CFF000000000000
      00000000000000000000000000000000000000000000000000004B9554CF70BE
      7AFF79C183FF5AAA63FF46994EFF409148FF3A8841FF34803AFF2E7734FF296F
      2EFF246828FF206223FF1C5D1FFF00000000FADCBFFFEEE0D6FFE3CBBAFFE2C9
      B8FFE0C6B4FFDFC4B1FFDEC2ADFFDCBFAAFFDABCA6FFD9B9A2FFD7B69EFFD6B3
      9AFFE9D6CAFFFCFCFCFFE3CBBAFFCE9D76FF00000000000000007B604BFFE6CC
      B4FFD3B59CFFD4B497FFD5B395FFD4B294FFCFAC8BFFCFA989FFCCA688FFC6A4
      87FFDCBEA3FF7D604BFF00000000000000000000000000000000000000000000
      00000000000000000000499E52FF8ACA91FF87C98EFF3B8A42FF000000000000
      000000000000000000000000000000000000000000004D9656C47AC886FF9CD5
      A5FF98D3A1FF94D09DFF90CE98FF8BCB93FF87C98EFF82C689FF7DC384FF79C1
      80FF75BE7BFF71BD77FF206223FF00000000FCDEC1FFEFE2D8FFFCFCFCFFE4CC
      BCFFFCFCFCFFE1C9B7FFFCFCFCFFDFC3B0FFFCFCFCFFDCBEA9FFFCFCFCFFD9B8
      A1FFFCFCFCFFF7F3F0FFE5CEBEFFD1A27CFF00000000000000007B604BFFEED4
      BCFFEFD2B7FFF2D2B5FFF1CFB2FFEDCFB2FFEFCFB2FFF5CFB1FFF4CEB0FFEFCD
      B0FFEBCDB2FF7E624AFF00000000000000000000000000000000000000000000
      000000000000000000004FA658FF8ECC95FF8BCB93FF419249FF000000000000
      00000000000000000000000000000000000000000000519D5BC97ECC8AFFA2D8
      ABFF9ED6A7FF9AD4A3FF96D29FFF93CF9AFF8ECC95FF89CA90FF85C78BFF81C5
      87FF7CC282FF77C07DFF246828FF00000000FCDEC1FFEFE2D8FFEFE2D8FFEFE2
      D8FFEEE1D7FFEEE0D6FFEEDFD4FFEDDDD2FFECDCD1FFEBDACEFFEAD8CCFFEAD7
      CAFFE8D5C7FFE7D3C5FFE7D1C2FFD5A883FF0000000000000000806C5DFF7961
      4BFF836147FF8A6242FF8A6242FFEDCFB4FFEDCFB4FF8C6242FF8C6241FF8562
      45FF7E624AFF816E5DFF00000000000000000000000000000000000000000000
      0000000000000000000055AD5EFF93CF9AFF90CE98FF479A4FFF000000000000
      000000000000000000000000000000000000000000000000000054A35ED180CD
      8BFF7BC987FF5CB867FF57B161FF52A95BFF4CA155FF46994EFF409148FF3A88
      41FF34803AFF2E7734FF296F2EFF00000000E0C5ACE3FCDEC1FFBBB9B6FFB3B1
      AEFFFADBBEFFF8D8BBFFF6D5B7FFF3D2B3FFF0CEAEFFEDC9AAFFEAC5A4FFE7C0
      9FFFE3BC9AFFE0B795FFDDB28FFFC19B7BE30000000000000000000000000000
      000000000000000000008A6144FFECCEB5FFECCEB5FF8A6144FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005AB464FF96D29FFF94D09CFF4DA256FF000000000000
      00000000000000000000000000000000000000000000000000000000000058AB
      63DB61BD6DF70000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2C2C2FD8484
      84B40B0B0B100000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000836147FFEACEB6FFEACEB6FF836147FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005EBB69FF9AD4A3FF98D3A1FF52AA5CFF000000000000
      0000000000000000000000000000000000000000000000000000000000000204
      02064B9254BB0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CACACAF8C3C3
      C3F84E4E4E670000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B604BFFE8CDB8FFE8CDB8FF7B604BFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000062C06EFF9ED6A7FF9CD4A5FF58B262FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000057575767CCCC
      CCF6C2C2C2EFB3B3B3E5A2A2A2D88C8C8CC3646464935050507D4343436E2929
      2949020202040000000000000000000000000000000000000000000000000000
      00000000000000000000806C5DFF7B604BFF7B604BFF806C5DFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000065C571FFA2D8ABFFA0D7A9FF5CB867FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000067C773FF66C672FF64C26FFF61BE6CFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000797F7DE2858A88FC858A
      88FC858A88FC858A88FC858A88FC858A88FC858A88FC858A88FC858A88FC858A
      88FC858A88FC858A88FC7C817FE7292A2A4EC76E7DFFC76E7DFFC76E7DFFC76E
      7DFFC76E7DFFC76E7DFFC76E7DFFC76E7DFFC76E7DFFC76E7DFF000000000000
      00000000000000000000000000000000000000000000AE4A30C0E96240FF5F36
      2866E96240FFE96240FF5F362866E96240FFE96240FF5F362866E96240FFE962
      40FF5F362866E96240FFAE4A30C0000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000757A78D9C2C7C5FFE5EAE8FFE4E9
      E7FFE4E9E7FFE4E9E7FFE4E9E7FFE4E9E7FFE4E9E7FFE4E9E7FFE4E9E7FFE4E9
      E7FFE4E9E7FFE4E9E7FFC9CFCCFF7C817FE7C76E7DFFF1D6DAFFEECED3FFEDCB
      D1FFEDCBD1FFEDCBD1FFEDCBD1FFEDCBD1FFF0D3D8FFC76E7DFF000000000000
      00000000000000000000000000000000000000000000DF5D3BFF5F3628665F36
      28665F3628665F3628665F3628665F3628665F3628665F3628665F3628665F36
      28665F3628665F362866DF5D3BFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000848987FBE2E7E5FFF7F8F8FFE2E7
      E5FFD8DFDCFFD9E0DDFFD9E0DDFFD9E0DDFFD9E0DDFFD9E0DDFFD9E0DDFFD8DF
      DCFFE2E7E5FFF7F9F8FFE4E9E7FF858A88FCD08390FFF8EAECFFF5E1E4FFF5E1
      E4FFF4DEE2FFF3DCDFFFF3DCDFFFF3DCDFFFF5E1E4FFC76E7DFF000000000000
      000000000000000000000000000000000000000000005F3628665F3628665F36
      28665F3628665F3628665F3628665F3628665F3628665F3628665F3628665F36
      28665F3628665F3628665F36286600000000000000008C4626AED98E5CFEDB90
      5FFFC77A4AED6C2C178D0B0402101F0A052A7C301BA2BF673CEAD67D50FFD57B
      4CFF923E24BD230A05310000000000000000848987FBE2E7E5FFE0E6E4FFFAFB
      FBFFFBFBFBFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFBFB
      FBFFFBFBFBFFE2E7E5FFE4E9E7FF858A88FCD28794FFF8EAECFFEFD1D6FFECC8
      CEFFECC8CEFFECC8CEFFEBC6CCFFEDCBD1FFF5E1E4FFC76E7DFF000000000000
      00000000000000000000000000000000000000000000C54E2CFF5F3628665F36
      28665F3628665F3628665F3628665F3628665F3628665F3628665F3628665F36
      28665F3628665F362866C54E2CFF00000000000000000703010A36190C44E1A4
      74FFC98151EC2F130A3D00000000000000000E0502136123137FDC966AFFDB8F
      5FFF3E140A54010000020000000000000000848987FBE2E7E5FFD8DFDCFFFAFC
      FCFFF2F3F3FFECEEEEFFD3D5D5FFA3A4A4FFD5D7D7FFBABCBCFFECEEEEFFF2F3
      F3FFFAFCFCFFD8DFDCFFE4E9E7FF858A88FCD48C98FFF9EDEFFFF4DEE2FFF2D9
      DDFFF1D6DAFFF1D6DAFFF0D3D8FFF0D3D8FFF4DEE2FFC76E7DFF000000000000
      00000000000000000000000000000000000000000000B84826FF5F3628665F36
      28665F3628665F3628665F3628665F3628665F3628665F3628665F3628665F36
      28665F3628665F362866B84826FF000000000000000000000000000000018046
      269BE2A97BFF7A3C20990000000000000000000000008D4426B1DFA076FFDA89
      59FF18080422000000000000000000000000848987FBE3E7E6FFDAE0DEFFFFFF
      FFFFECEEEEFFCFD1D1FF292929FF959696FF545555FF101010FFECEEEEFFECEE
      EEFFFFFFFFFFD8DFDCFFE4E9E7FF858A88FCD7939EFFFBF3F4FFEFD1D6FFEBC6
      CCFFECC8CEFFECC8CEFFE9C0C7FFE6B8C0FFECC8CEFFC76E7DFF000000000000
      000000000000000000000000000000000000000000005F3628665F3628665F36
      28665F3628665F3628665F3628665F3628665F3628665F3628665F3628665F36
      28665F3628665F3628665F362866000000000000000000000000000000001008
      0415C98755EAE2A778FFB26C3CD3904928B2924928B5B4663AD8E3A881FFD483
      53F91406031B000000000000000000000000848987FBE4E8E7FFDDE3E0FFFFFF
      FFFFECEEEEFFBBBDBDFF555656FFECEEEEFFDEE0E0FF080808FFECEEEEFFECEE
      EEFFFFFFFFFFD8DFDCFFE4E9E7FF858A88FCD998A3FFFCF6F7FFF0D3D8FFE7BB
      C2FFEBC6CCFFEECED3FFE2B3BAFFD1929AFFD895A0FFC97282FF000000000000
      0000000000000D000D0D100010100B000B0B00000000AB4321FF5F3628665F36
      28665F3628665F3628665F3628665F3628665F3628665F3628665F3628665F36
      28665F3628665F362866AB4321FF000000000000000000000000000000000000
      00004A26135CD79763F6B27242D13E1C0E4E200E072A804022A0E1A679FFCD7F
      4EF20903010D000000000000000000000000848987FBE6EAE8FFDFE5E3FFFFFF
      FFFFECEEEEFFECEEEEFF8B8C8CFF565757FF565757FF171717FFECEEEEFFECEE
      EEFFFFFFFFFFD8DFDCFFE4E9E7FF858A88FCDA9AA5FFFCF9F9FFB6A2B0FFCB8C
      91FFD38A96FFCF9998FF587098FF5A7AA3FF946172FFBF5E6EFF000000000000
      0000000000005B5656FF454141FF524144FF00000000AC4725FF5F3628665F36
      28665F3628665F3628665F3628665F3628665F3628665F3628665F3628665F36
      28665F3628665F362866AC4725FF000000000000000000000000000000000000
      0000020100038F552BAAD89A66F7522814660703010A6C331B88E1A678FFAF64
      37D204010006000000000000000000000000848987FBE8EBEAFFE2E7E5FFFFFF
      FFFFECEEEEFFEBEDEDFFC1C3C3FFE9EBEBFFB0B1B1FF505050FFECEEEEFFECEE
      EEFFFFFFFFFFD8DFDCFFE4E9E7FF858A88FCDC9FA9FFFEFCFCFF547199FF5870
      98FFB19498FF567098FF7BC4E7FF547199FF425482FF8D4D61FF000000008F8F
      90FF535353FF5A5656FF3A393BFF625455FF000000005F3628665F3628665F36
      28665F3628665F3628665F3628665F3628665F3628665F3628665F3628665F36
      28665F3628665F3628665F362866000000000000000000000000000000000000
      0000000000001B0F0722C48957E2C08351DF4422115468341A80E2A97BFF8745
      25A500000000000000000000000000000000848987FBE9ECEBFFE4E9E7FFFFFF
      FFFFEBEDEDFFEAECECFF717272FF515151FF7E807EFFD0D2D2FFE9EBEAFFE9EB
      EAFFFFFFFFFFD8DFDCFFE4E9E7FF858A88FCDDA2ABFFFEFCFCFF547098FFCBEF
      F6FF547199FF7BC4E7FF547199FF7BC4E7FF547199FF3D588CFF364C84FF8F8F
      90FF91959BFF5A5656FF514C4DFF625455FF00000000BE5A38FF5F3628665F36
      28665F3628665F3628665F3628665F3628665F3628665F3628665F3628665F36
      28665F3628665F362866BE5A38FF000000000000000000000000000000000000
      0000000000000000000063381B77DBA16CF7935C2FAD864E29A0E4AE82FF7C41
      229900000000000000000000000000000000848987FBEAEDECFFE6EAE9FFFFFF
      FFFFEAEDEDFFE9EDECFFE8EBEAFF858887FFB6B9B8FFE2E7E5FFE0E5E3FFE0E5
      E3FFFFFFFFFFD8DFDCFFE4E9E7FF858A88FCE2AEB7FFFEFCFCFFFCFAFAFF5670
      98FFCBEFF6FF547199FF7BC4E7FF547199FF7BC4E7FF5DADEAFF4C99E4FF8F8F
      90FFF1F1F1FF5A5656FF676362FF6A5C5EFF00000000CC6644FF5F3628665F36
      28665F3628665F3628665F3628665F3628665F3628665F3628665F3628665F36
      28665F3628665F362866CC6644FF000000000000000000000000000000000000
      00000000000000000000170D061CA16D3EBAD7A06CF2D8A16EF5DA9F6DF77D46
      249806020108000000000000000000000000848987FBEAEEECFFE8EBEAFFF7F8
      F7FFF1F3F3FFEAEDECFFE7EAE9FFDEE3E1FF585A59FFCCD2D0FFD9DFDDFFE2E7
      E5FFF0F2F1FFD8DFDCFFE4E9E7FF858A88FCE2AEB7FFF9EDEFFFF9EDEFFFF8EB
      ECFF557098FFA5E9F4FF547199FF7BC4E7FF6DBEEFFF5DACE9FF4C99E4FF8F8F
      90FFF1F1F1FF5A5656FF747171FF757273FF000000005F3628665F3628665F36
      28665F3628665F3628665F3628665F3628665F3628665F3628665F3628665F36
      28665F3628665F3628665F362866000000000000000000000000000000000000
      00000000000000000000462A1453C4915DDEE9BC91FFE7BA8FFFE7B78BFFDEA1
      6EFB98592EB60703010A0000000000000000848987FBEAEEECFFE8EBEBFFEDF0
      F0FFF2F4F4FFF2F3F3FFF0F2F2FFEEF1EFFFECEFEEFFEAEDECFFE8ECEAFFE5E9
      E7FFEBEEEDFFDEE4E1FFE4E9E7FF858A88FCE2AEB7FFDFA7B0FFDFA7B0FFDFA7
      B0FFDDAAA3FF547199FF97C2D9FFC4EBF6FF6DC0F1FF5DAEEBFF3A5C9BFF8F8F
      90FFF1F1F1FF5A5656FF8C8C8CFF8C8C8CFF00000000E47E5CFF5F3628665F36
      28665F3628665F3628665F3628665F3628665F3628665F3628665F3628665F36
      28665F3628665F362866E47E5CFF000000000000000000000000000000000000
      0000000000000000000036221040704822857E522A94794D258F7A4A24908250
      279A723D1F8A0A04020D0000000100000000848987FBEAEDECFFE8ECEBFFEAED
      ECFFEBEDEDFFE9ECEBFFE6EAE9FFE3E7E6FFDFE5E3FFDCE2E0FFD8DFDCFFD8DF
      DCFFDCE2E0FFE2E8E6FFE4E9E7FF858A88FC0000000000000000000000000000
      00000000000000000000000000006996C0FF598DC7FF546CA4FF000000008F8F
      90FF8F8F90FF5A5656FF8C8C8CFF797878FF00000000B1654BC0ED8764FF5F36
      2866ED8764FFED8764FF5F362866ED8764FFED8764FF5F362866ED8764FFED87
      64FF5F362866ED8764FFB1654BC0000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000747876D6C6CAC8FFEDF0EFFFEFF1
      F1FFEFF1F1FFEFF1F0FFEDF0EFFFEBEEEDFFE9EDEBFFE7EBE9FFE5E9E7FFE5E9
      E7FFE5E9E7FFE5E9E7FFC8CECBFF7B807EE40000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000675658FF5A5656FF6C5659FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002021213E767B79DB858A88FF858A
      88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A
      88FF858A88FF858A88FF7A7E7CE1262727480000000000000000E3C1AAFFCD8E
      66FFC0703FFFBD6A36FFBD6A36FFBD6A36FFBD6936FFBD6935FFBC6934FFBC69
      34FFBC6834FFBE6D3BFFCA8B62FFE4C3AEFF0000000000000000C0606FFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0606FFF0000000000000000000000000000001D000000340000
      00360000003600000036000000360000003600000036000000360D381CA11765
      33F5186A36FF176533F50D381D96000000000000000000000000C57C4DFFF9F2
      ECFFF8EDE0FFF6EBDEFFF6EADEFFADF3FBFF009ABBFF009ABBFFADF3FBFFFAF2
      EAFFFCF7F3FFFDF9F5FFFFFFFEFFC37A4DFF00000000C0606FFFC0606FFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0606FFFC0606FFF000000000000000000000034EFEFEFF5FAFA
      FAFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFF86B196FF278C52FF63BA
      8DFF95D2B2FF63BA8DFF278C52FF0D3A1E8C0000000000000000C2763FFFF6EC
      E0FFFDBF67FFFCBD66FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFFCBE5FFFFCBC61FFFEFCF9FFBD6B36FF00000000C0606FFFC16271FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C16271FFC0606FFF000000000000000100000036FBFBFBFEFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFF21703EFF61BA8BFF5FBA
      87FFFFFFFFFF5FB987FF66BC8FFF186735F70000000000000000C37C42FFF7ED
      E3FFFDC26DFFFFD8A0FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFFFD493FFFBBE64FFFBF7F4FFBE6B37FF00000000C26473FFC56A79FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C56A79FFC26473FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFF307A4BFF9CD4B6FFFFFF
      FFFFFFFFFFFFFFFFFFFF95D2B2FF186A36FF0000000000000000C77E44FFE092
      5FFFE08C4BFFF7B455FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFF7B24EFFF7B24EFFFCF9F5FFC2733CFF00000000C36675FFCD7B8AFFC060
      6FFF0000000000000000000000000000000000000000C0606FFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C0606FFF000000000000000000000000000000000000
      0000C0606FFFCD7B8AFFC36675FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFAFAFAFF498B61FF90D3B1FF92D6
      B1FFFFFFFFFF64BC8CFF66BC8FFF186735F70000000000000000D8813FFFE5A3
      64FFE19157FFFDE5D3FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFFBE0C9FFFBE1C8FFFDFAF7FFC37A41FF00000000C36675FFD7939EFFC468
      77FF0000000000000000000000000000000000000000C0606FFFC0606FFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0606FFFC0606FFF000000000000000000000000000000000000
      0000C46877FFD7939EFFC36675FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFBFBFBFFA5C4B1FF60AB81FF95D4
      B4FFBAE6D0FF69BB8FFF2C8F56FF0D3A1E8CF2CFB5FFE6A35AFFE8AA69FFE39B
      5CFFF9D8C3FFFDE7D6FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFFBE1CBFFFBE1C9FFFBF7F2FFC78145FF00000000C16271FFD6919CFFD48C
      98FFBF5E6EFF00000000000000000000000000000000C0606FFFC87080FFC060
      6FFF000000000000000000000000000000000000000000000000000000000000
      0000C0606FFFC87080FFC0606FFF00000000000000000000000000000000BF5E
      6EFFD48C98FFD6919CFFC16271FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFABC8B6FF5F99
      74FF4E8E65FF498A60FF163A239800000001E9AC60FFECB878FFE5A457FFF2D8
      C4FFFEE8D6FFFEE8D7FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFFADFC7FFFADFC6FFFAF2EAFFC88447FF0000000000000000CA7584FFE4B3
      BBFFCF818EFFC0606FFFC0606FFFBF5E6EFFBF5E6EFFC0606FFFCC7987FFC76E
      7DFFC0606FFF000000000000000000000000000000000000000000000000C060
      6FFFC76E7DFFCC7987FFC0606FFFBF5E6EFFBF5E6EFFC0606FFFC0606FFFCF81
      8EFFE4B3BBFFCA7584FF00000000000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFAFAFAFFF9F9F9FFF6F6
      F6FFF6F6F6FFFCFCFCFF0000003600000001F4D5B7FFEAAE60FFEAB570FFE8A6
      62FFFADBC5FFFEE8D8FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFF9DDC3FFF8DCC2FFFAF4EDFFC8864AFF0000000000000000C0606FFFD58E
      9AFFE2AEB7FFD6919CFFCD7B8AFFCD7B8AFFCE7D8CFFCB7786FFCB7786FFCA75
      84FFC87080FFC0606FFF00000000000000000000000000000000C0606FFFC870
      80FFCA7584FFCB7786FFCB7786FFCE7D8CFFCD7B8AFFCD7B8AFFD6919CFFE2AE
      B7FFD58E9AFFC0606FFF00000000000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF6F6F6FFF3F3
      F3FFF2F2F2FFFCFCFCFF00000036000000010000000000000000DF9849FFEBB6
      71FFE8A760FFFDE7D6FFECB264FFADF3FBFF009ABBFF009ABBFFADF3FBFFF5D6
      BBFFF6DABDFFF6D8BBFFFAF4EFFFC8874BFF000000000000000000000000C060
      6FFFD38A96FFDEA4AEFFDC9FA9FFD7939EFFD38A96FFD08390FFCC7987FFCA75
      84FFCA7584FFC87080FFC0606FFF0000000000000000C0606FFFC87080FFCA75
      84FFCA7584FFCC7987FFD08390FFD38A96FFD7939EFFDC9FA9FFDEA4AEFFD38A
      96FFC0606FFF0000000000000000000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF2F2F2FFEFEF
      EFFFEDEDEDFFFCFCFCFF00000036000000010000000000000000CA8D4EFFE9B1
      6DFFE8AE5EFFFCE6D4FFECB664FFECB664FFADF3FBFFADF3FBFFF5D6BBFFF5D6
      BBFFF3D4B5FFF1D2B3FFF8F4F0FFC6864BFF0000000000000000000000000000
      0000C0606FFFC76E7DFFD28794FFD58E9AFFD48C98FFD18592FFCE7D8CFFCC79
      87FFC87080FFC0606FFF00000000000000000000000000000000C0606FFFC870
      80FFCC7987FFCE7D8CFFD18592FFD48C98FFD58E9AFFD28794FFC76E7DFFC060
      6FFF000000000000000000000000000000000000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF1F1F1FFECECECFFEAEA
      EAFFE6E6E6FFFCFCFCFF00000036000000010000000000000000C88D50FFF8EF
      E6FFFCE3CFFFFBE4D0FFFCE4CFFFADF3FBFF009ABBFF009ABBFFADF3FBFFF4E9
      DFFFF7F2ECFFFBF7F3FFF5EFE9FFC38147FF0000000000000000000000000000
      00000000000000000000C0606FFFC16271FFC16271FFC0606FFFD18592FFC66C
      7BFFC0606FFF000000000000000000000000000000000000000000000000C060
      6FFFC66C7BFFD18592FFC0606FFFC16271FFC16271FFC0606FFF000000000000
      0000000000000000000000000000000000000000000100000036FCFCFCFFF9F9
      F9FFF9F9F9FFF9F9F9FFF7F7F7FFF6F6F6FFF2F2F2FFEBEBEBFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF00000036000000010000000000000000C98E52FFF9F5
      F1FFFCE3CDFFFBE3CEFFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFFCE6CDFFFAE5C9FFE2B684FFD6A884FF0000000000000000000000000000
      00000000000000000000000000000000000000000000C0606FFFC66C7BFFC060
      6FFF000000000000000000000000000000000000000000000000000000000000
      0000C0606FFFC66C7BFFC0606FFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFFCFCFCFFF6F6
      F6FFF4F4F4FF5757579100000020000000000000000000000000CA9259FFFBF7
      F3FFFAE0C7FFFBE1C9FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFF6D8B4FFE1B07CFFDD9669FF000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0606FFFC0606FFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0606FFFC0606FFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000036F9F9F9FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFE7E7
      E7FF545454910000002000000002000000000000000000000000D2A273FFF8F3
      EEFFF9F5EFFFF8F4EDFFF8F3EDFFADF3FBFF009ABBFF009ABBFFADF3FBFFF2E6
      D7FFE2B27CFFDD996BFF00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0606FFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C0606FFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000033E9E9E9F0F9F9
      F9FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF5454
      5491000000200000000200000000000000000000000000000000E8CFB9FFD7AA
      7CFFCC945AFFCA9155FFCA9054FFCA9054FFCA9154FFCB9054FFC98F54FFCF9D
      69FFDEB290FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      002000000002000000000000000000000000000000000000001D000000340000
      0036000000360000003600000036000000360000003600000036000000360000
      003600000036000000330000001D000000000000000000000000000000000824
      2F37000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C695
      60FDCA9864FFCA9764FFCA9764FFCA9764FFCA9763FFC99763FFC99763FFCA98
      64FFC69460FD00000000000000000000000000000000858887FF858A88FF858A
      88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A
      88FF858A88FF858A88FF949695FF000000000000000000000034EFEFEFF5FAFA
      FAFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFAFAFAFDEDEDEDF30000003300000000000000000000000000000000228F
      B9D5239AC9E901070A0C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000007A7A7AC2676767DA575757FFC795
      60FFF9F7F6FFF9F1ECFFF9F1EBFFF8F0E9FFF7EDE6FFF4EAE1FFF2E8DEFFFAF8
      F6FFC79460FF232323FF3F3F3FD9707070BF898C8BFFBBBCBBFFE5E5E5FFE2E2
      E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2
      E2FFE2E2E2FFE1E2E2FFB6B7B7FF878A88FF0000000100000036FBFBFBFEFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFAFAFAFD00000036000000000000000000000000000000000208
      0A0C2CAEDFFF2AABDEFF135A768A000000000000000000000000000000000000
      000000000000000000000000000000000000696969FDA7A7A7FFB5B5B5FF8181
      81FFAFACAAFFC5C0BDFFC5C0BDFFC5C0BDFFC5C0BDFFC5C0BDFFC5C0BDFFADAA
      A8FF2B2B2BFFB5B5B5FF9B9B9BFF222222FF858A88FFEBEBEAFFD0CFCFFFCFCF
      CFFFCBCBCBFFCCCBCBFFCBCCCCFFCCCBCCFFCCCCCCFFD0CFD0FFD0CFD0FFD0CF
      D0FFD0D0D0FFD3D4D3FFEBEAEAFF858A88FF0000000100000036FCFCFCFFFCFC
      FCFF174259FF2A6089FF4B8ABEFF6EA8CBFFE0E9F1FFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFCFCFCFF00000036000000010000000000000000000000000000
      0000228EB5CF4EBCE7FF4BBAE6FF239BCCED030E131700000000000000000000
      0000000000000000000000000000000000006F6F6FFFB5B5B5FFB5B5B5FF9595
      95FF818181FF818181FF787878FF6D6D6DFF606060FF515151FF424242FF4141
      41FF6D6D6DFFB5B5B5FFB5B5B5FF242424FF858A88FFFFFFFFFFD6D6D6FF9696
      96FF959696FF969696FF969596FF969696FF959596FF969696FF969696FF9696
      95FF959696FFD6D5D6FFEFEEEEFF858A88FF0000000100000036FCFCFCFFFCFC
      FCFF2D6685FF94C7F9FF91C9F9FF4085C9FF256AAEFFD4E2EEFFFAFAFAFFFAFA
      FAFFFAFAFAFFFCFCFCFF00000036000000010000000000000000000000000000
      0000000000002AAFDFFF84D3F2FF54BDE7FF2DAADEFF145C7B90000000000000
      000000000000000000000000000000000000747474FFBBBBBBFFBBBBBBFF8D8D
      8DFFD4D4D4FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9B9FFD3D3
      D3FF838383FFBBBBBBFFBBBBBBFF292929FF858A88FFFFFFFFFFDDDDDCFFDDDD
      DDFFDDDCDDFFDDDDDDFFDDDDDCFFDDDCDDFFD8D8D8FFD7D7D8FFD8D8D8FFD8D8
      D8FFD8D8D8FFDDDDDDFFF3F3F3FF858A88FF0000000100000036FCFCFCFFFCFC
      FCFF4289AAFFE0F2FFFF539AD8FF1979BEFF4898C5FF478EC7FFD8E6F3FFF8F8
      F8FFF8F8F8FFFCFCFCFF00000036000000010000000000000000000000000000
      0000000000002186A8C06FCCEEFF83D2F2FF7DCEF1FF49B6E4FF219CCFF1030E
      131700000000000000000000000000000000797979FFD7D7D7FFD7D7D7FF9797
      97FFD8D8D8FFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFD7D7
      D7FF8E8E8EFFD7D7D7FFD7D7D7FF3E3E3EFF858A88FFFFFFFFFFE3E3E3FFA0A0
      A0FFA0A0A0FFA0A0A0FFA0A0A0FFE3E3E3FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1
      A1FFA1A1A1FFE3E3E3FFF9F9F9FF858A88FF0000000100000036FCFCFCFFFCFC
      FCFFA5C3D7FF79B6D5FF90B7D1FF54C9E4FF5ADFF5FF77D0EDFF509DDDFFDFEB
      F5FFF8F8F8FFFCFCFCFF000000360000000134B6D9F034BADFF833B8DEF831B6
      DDF82FB3DCF82DB1DBF886D7F3FF2EB6EBFF49BCECFF80CEF1FF50B9E6FF2BA8
      DDFF1563869D0000000000000000000000007D7D7DFFF9F9F9FFF9F9F9FFABAB
      ABFFDFDFDFFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFDFDF
      DFFFA3A3A3FFF9F9F9FFF9F9F9FF606060FF858A88FFFFFFFFFFE3E3E3FFA0A0
      A0FFBEBEBEFFBEBEBEFFA0A0A0FFE3E3E3FFE3E3E3FFE3E3E3FFE3E3E3FFE3E3
      E3FFE3E3E3FFE3E3E3FFFFFFFFFF858A88FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFB2D5E5FF75BAD7FFC2F6FDFF62DFF7FF5CE2F8FF78D3F0FF4898
      DCFFDEE9F2FFFCFCFCFF000000360000000134B3D4E974DAF2FF93E6F8FF91E3
      F7FF8DE0F6FF8ADCF5FF8ADBF5FF88D7F4FF84D3F2FF7ECFF1FF7BCCF0FF79C9
      EFFF47B4E3FF219CD1F504151D2300000000818181F9FCFCFCFFFCFCFCFFCBCB
      CBFFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2
      F2FFC6C6C6FFFCFCFCFFFCFCFCFF707070FE858A88FFFFFFFFFFE9E9E9FFA0A0
      A0FFBEBEBEFFBEBEBEFFA0A0A0FFE9E9E9FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0
      A0FFA0A0A0FFE9E9E9FFFFFFFFFF858A88FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFAFD4E5FF76CBE7FFC7F7FDFF5DDCF5FF59E1F7FF7AD4
      F1FF4A9ADDFFD4E5F5FF00000036000000010C2A32374ECDECFF98E9F9FF49D5
      F3FF44CFF1FF3FCAF0FF37C2EEFF89D9F4FF2DB3E0FE2AACD9F829A9D9F827A7
      D8F825A4D6F823A1D5F81F95C7E900000000818181DAD2D2D2FFE8E8E8FF7C7C
      7CFF7C7C7CFF7C7C7CFF7C7C7CFF7C7C7CFF7C7C7CFF7C7C7CFF7C7C7CFF7C7C
      7CFF7C7C7CFFE8E8E8FFC4C4C4FF5F5F5FE1858A88FFFFFFFFFFE9E9E9FFA0A0
      A0FFBEBEBEFFBEBEBEFFA0A0A0FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFFFFFFFFF858A88FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFBDE5F2FF78D3EEFFC7F7FDFF5EDCF5FF5AE2
      F7FF79D6F2FF50A2E2FF0C1A265C000000010000000038BEDFF480E1F5FF8EE6
      F8FF42D2F3FF3ECDF1FF38C7EFFF8CDCF5FF57C6EAFF14526674000000000000
      000000000000000000000000000000000000B1B1B1CD9A9A9AFFCCCCCCFFC78B
      4DFFF9F4EDFFFEE8D8FFFEE8D7FFFDE5D3FFFCE4D1FFFAE0C7FFF9DDC3FFFAF4
      EDFFC78549FFC3C3C3FF737373FFA4A4A4CD858A88FFFFFFFFFFE9E9E9FFA0A0
      A0FFA0A0A0FFA0A0A0FFA0A0A0FFE9E9E9FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0
      A0FFA0A0A0FFE9E9E9FFFFFFFFFF858A88FF0000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFBAE3F0FF7CD4EEFFC4F6FDFF6BDD
      F6FF6CCAEDFF62A3D7FF5690C3F00C151E270000000014424D545AD4EFFF99EA
      F9FF46D6F4FF41D0F2FF3CCBF0FF6DD5F3FF7ED7F3FF49C0E7FF0D34414A0000
      000000000000000000000000000000000000000000009C9C9CC2818181F4C589
      4BFFF9F4EFFFFEE7D7FFFDE7D5FFFCE6D2FFFBE1CCFFF8DCC2FFF6DABDFFFAF4
      EFFFC48347FF5C5C5CF48F8F8FC200000000858A88FFFFFFFFFFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFFFFFFFFF858A88FF0000000100000036FCFCFCFFF9F9
      F9FFF9F9F9FFF9F9F9FFF7F7F7FFF6F6F6FFF2F2F2FFA8D9E8FF81D6EEFFB2E3
      F9FF8BC0E7FFAED3F6FFC4E0FCFF629ACCF700000000000000003BC5E4F993E9
      F9FF71E1F7FF44D4F3FF40CEF2FF3BC9F0FF8ADCF5FF6FD0EFFF3CBBE4FF0618
      1E2300000000000000000000000000000000000000000000000000000000C189
      4CF9F9F4F0FFFCE6D3FFFDE7D3FFFBE3CDFFFAE0C8FFF5D6BBFFF3D4B5FFF8F4
      F0FFBF8247F9000000000000000000000000858A88FFFFFFFFFFE9E9E9FF6B6B
      6BFF6B6B6BFF6B6B6BFF6B6B6BFF6B6B6BFF6B6B6BFFE9E9E9FFE9E9E9FFA0A0
      A0FFA0A0A0FFE9E9E9FFFFFFFFFF858A88FF0000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFAFE4F3FF76BE
      E7FFB4D2F0FFE5F3FFFFACD2EFFF417FB5E800000000000000002068788363D9
      F1FF9AEBFAFF47D8F4FF43D3F3FF3ECEF1FF39C8F0FF8CDCF5FF61CBEDFF30B6
      E0FC00000000000000000000000000000000000000000000000000000000C189
      4EF7F9F5F1FFFCE3CFFFFCE4CFFFFAE1CAFFF9DDC4FFF4E9DFFFF7F2ECFFF5EF
      E9FFC07E46FB000000000000000000000000858A88FFFFFFFFFFE9E9E9FF6B6B
      6BFF6B6B6BFFA0A0A0FF6B6B6BFF6B6B6BFFA0A0A0FFE9E9E9FFE9E9E9FFA0A0
      A0FFA0A0A0FFE9E9E9FFFFFFFFFF858A88FF0000000000000036F9F9F9FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFACD5
      E4FF57A5D8FF85B1DBFF459DD0FF0F374D5E0000000000000000000000003ECA
      E9FD9AEDFAFF99EBF9FF97E8F9FF94E5F8FF91E2F7FF8EDFF6FF8BDBF5FF55C7
      EBFF2EAFD8F3000000000000000000000000000000000000000000000000C188
      4EF6F9F5F1FFFCE3CDFFFBE3CDFFF9E0C8FFF8DCC2FFFDFBF8FFFCE6CDFFE2B6
      84FFA48266C5000000000000000000000000858A88FFFFFFFFFFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFFFFFFFFF858A88FF0000000000000033E9E9E9F0F9F9
      F9FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF5454
      5491000000200000000200000000000000000000000000000000000000002882
      95A23ECCEBFF3DCBEAFF3BC9E9FF3AC7E9FF39C4E8FF37C2E7FF35C0E6FF34BD
      E5FF32BBE4FF2BA2C8E10000000000000000000000000000000000000000C185
      4BFAF7F2ECFFF8F4EEFFF8F3EDFFF8F3EDFFF8F2ECFFF2E6D7FFE2B27CFFD390
      64F600000000000000000000000000000000858A88FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF858A88FF000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      0020000000020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B19D
      8DC3AC8863CDC78B4FFEC88C4EFFC38C52F7C48B52F7C4884CFEA7846BC10000
      000000000000000000000000000000000000858A88FF858A88FF858A88FF858A
      88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A
      88FF858A88FF858A88FF858A88FF858A88FF0000000000000000000000003AA8
      DBFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003AA8DBFF3AA8
      DBFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000930000
      00C6000000C7000000C9000000CB000000CC000000D1000000A0000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003AA8DBFF3AA8DBFF3AA8
      DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8
      DBFF3AA8DBFF3AA8DBFF0000000000000000000000000D0D0DFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000777777FF0000000000000000000000000000000000000000000000007777
      77FF00000000000000000000000000000000000000003AA8DBFF3AA8DBFF3AA8
      DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8
      DBFF3AA8DBFF3AA8DBFF000000000000000000000000151515FF00000000E3AA
      83FFE1A57CFFDE9F76FFDC9B71FFDA966BFFD89066FFD68C61FFD5865CFFD485
      59FFD28357FFD28357FFD28357FF000000000000000000000000000000C00000
      00C1000000C3000000C4000000C6000000C7000000C9000000CB000000CC0000
      00D1000000D7000000DD00000000000000000000000000000000000000006A6A
      6AFF191919FF565656FF00000000000000000000000000000000565656FF1919
      19FF6A6A6AFF00000000000000000000000000000000000000003AA8DBFF3AA8
      DBFF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001E1E1EFF00000000E5B0
      89FFEAC0A2FFE9BD9EFFE8B99AFFE6B596FFE4B192FFE3AD8DFFE1A989FFDFA5
      86FFDEA383FFDDA080FFD28357FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000777777FF1919
      19FF000000FF060606FF565656FF0000000000000000575757FF060606FF0000
      00FF191919FF777777FF00000000000000000000000000000000000000003AA8
      DBFF000000003F3A34FF49433DFF48433DFF393530FF292522FF000000003633
      33FF272421FF23211EFF191715FF040303FF000000002A2A2AFF00000000E6B3
      8CFFECC4A6FFEBC2A3FFEABFA0FFE8BC9DFFE7B899FFE5B494FFE4AF91FFE2AC
      8CFFE1A888FFDFA585FFD48559FF00000000FF0000FF00000000000000000000
      000000000000A8462DC0E25E3CFFE25E3CFFE25E3CFFE25E3CFFE25E3CFFA846
      2DC0000000000000000000000000000000000000000000000000000000005656
      56FF060606FF000000FF030303FF636363FF646464FF040404FF000000FF0606
      06FF565656FF0000000000000000000000000000000000000000000000000000
      00000000000048423CFF90857BFFABA197FF7B7167FF48423DFF000000004743
      40FFA09488FF9F9489FF7C7268FF181614FF00000000353535FF00000000E6B3
      8CFFECC5A7FFECC5A7FFEBC4A5FFEBC1A3FFEABEA0FFE8BA9CFFE6B697FFE4B3
      93FFE3AF8FFFE1AA8BFFD68C61FF00000000FF0000FFFF0000FF000000000000
      000000000000C8502EFFF8CFC2FFF8CFC2FFF8CFC2FFF8CFC2FFF8CFC2FFC850
      2EFF000000000000000000000000000000000000000000000000000000000000
      0000585858FF030303FF000000FF070707FF070707FF000000FF040404FF5757
      57FF000000000000000000000000000000000000000000000000000000000000
      000000000000514B45FF90867CFFAEA59BFF766C63FF544F4BFF000000004441
      3EFF9A8F84FFA0978CFF797066FF1E1B19FF00000000424242FF00000000E6B3
      8CFFE6B38CFFE6B38CFFE6B38CFFE6B38CFFE5B089FFE3AC85FFE2A980FFE1A3
      7AFFDE9E74FFDC996FFFDA9469FF00000000FF0000FFFF0000FFFF0000FF0000
      00000000000087351BC0B14523FFB14523FFB14523FFB14523FFB14523FF8735
      1BC0000000000000000000000000000000000000000000000000000000000000
      000000000000656565FF070707FF000000FF000000FF070707FF646464FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000077716DFFA3978CFFC2B9AFFFA6998EFF827B75FF000000006E6B
      68FF9C9186FFBBB0A5FFA69A8EFF4E4B48FF000000004F4F4FFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF0000FFFF0000FFFF0000FFFF00
      00FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000636363FF070707FF000000FF000000FF070707FF636363FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000005F5852FF45403AFF4D4741FF37322EFF373431FFC6C6C5FF5957
      55FF1C1A18FF1F1C1AFF12100FFF101010FF000000005B5B5BFF00000000F5C5
      A7FFF5C3A3FFF5C09FFFF5BE9BFFF3BB99FFF3B895FFF3B792FFF3B690FFF3B6
      8EFF00000000000000000000000000000000FF0000FFFF0000FFFF0000FF0000
      00000000000090442AC0BC5735FFBC5735FFBC5735FFBC5735FFBC5735FFBC57
      35FFBC5735FF90442AC000000000000000000000000000000000000000000000
      0000565656FF030303FF000000FF070707FF070707FF000000FF030303FF5656
      56FF000000000000000000000000000000000000000000000000000000000000
      000000000000A3978BFFA99D90FF7E746AFF7E7368FF7C7166FF4D463EFF6A5F
      55FF84776CFF796D63FF766C62FF1B1917FF00000000676767FF00000000F5C8
      ABFFF7D3BCFFF7D2BAFFF7CFB6FFF7CDB4FFF6CBB1FFF6CAAEFFF6C8ACFFF3B6
      90FF00000000000000000000000000000000FF0000FFFF0000FF000000000000
      000000000000D6704EFFF8CFC2FFF8CFC2FFF8CFC2FFF8CFC2FFF8CFC2FFF8CF
      C2FFF8CFC2FFD6704EFF00000000000000000000000000000000000000005656
      56FF060606FF000000FF040404FF636363FF656565FF030303FF000000FF0606
      06FF565656FF0000000000000000000000000000000000000000000000000000
      000000000000C8BFB7FFA89B90FF897D73FF867769FF8E7E6FFFA19181FF9583
      71FF937E6CFF897565FF7C7065FF545250FF00000000737373FF00000000F5CB
      AEFFF7D5C0FFF7D3BDFFF7D2BAFFF7D0B8FFF7CEB4FFF6CCB2FFF6CBAFFFF3B8
      94FF00000000000000000000000000000000FF0000FF00000000000000000000
      000000000000AF6348C0EB8562FFEB8562FFEB8562FFEB8562FFEB8562FFEB85
      62FFEB8562FFAF6348C000000000000000000000000000000000777777FF1919
      19FF000000FF060606FF575757FF0000000000000000585858FF060606FF0000
      00FF191919FF777777FF00000000000000000000000000000000000000000000
      00000000000000000000897D72FF9B8E82FF887C71FF554E47FFC0BCB8FF5C55
      4DFF857567FF7B6E62FF6B645EFFBBBAB9FF000000007E7E7EFF00000000F5CB
      AFFFF7D7C2FFF7D5C0FFF7D4BEFFF7D2BBFFF7D1B8FFF7CFB5FFF6CDB3FFF3BB
      96FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006B6B
      6BFF1A1A1AFF565656FF00000000000000000000000000000000575757FF1919
      19FF6A6A6AFF0000000000000000000000000000000000000000000000000000
      00000000000000000000E5E3E1FFCFC9C4FFACA297FF766E65FF00000000766C
      63FFA09488FFBAB3ADFFE2E1E0FF0000000000000000898989FF00000000F5CB
      AFFFF5CBAFFFF5CBAFFFF5C9ACFFF5C8AAFFF5C5A7FFF5C3A3FFF5C09FFFF5BE
      9BFF000000000000000000000000000000000000000000000000000000AD0000
      00AE000000AF000000B0000000B1000000B2000000B4000000B5000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000787878FF0000000000000000000000000000000000000000000000007777
      77FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000DFDBD7FFB3A79CFFA5998DFF00000000AC9F
      92FF94897EFFB8B4B0FF000000000000000000000000919191FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      00AC000000AD000000AD000000AE000000AF000000B0000000B1000000B20000
      00B4000000B50000008900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6BFF8FFC6BFF8FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000E07021183451399D96F1EFFD96D1BFFAC5615CC64300C77642F
      0C77C75D18EEC75B18EE7F3A0E990E0601110000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003AA8DBFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6BFF8FF7464EEFF6E5DEDFF9387
      F2FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000058311066E18F4BFFEBB183FFE3914DFFDA7222FFDA7327FFE088
      47FFE7A26FFFE3935BFFDB7432FF391A06440000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003AA8DBFF3AA8DBFF0000000000000000000000003AA8DBFF3AA8DBFF0000
      00003AA8DBFF3AA8DBFF00000000000000003AA8DBFF00000000000000003AA8
      DBFF00000000000000003AA8DBFF00000000C6BFF8FF6E5DEDFF6E5DEDFF6E5D
      EDFF9B80C8FFE2C0AAFF00000000000000000000000000000000E2C0AAFFE2C0
      AAFF000000000000000000000000000000000000000000000000000000000000
      0000000000003B220C44E28D43FFECB588FFDC792BFF0E0702111D0F0422BC60
      19DDE6A470FFDC7933FF391B06440000000000000000000000003AA8DBFF3AA8
      DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8
      DBFF3AA8DBFF3AA8DBFF3AA8DBFF000000000000000000000000000000003AA8
      DBFF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009387F2FF6E5DEDFF6346
      CEFF6143C9FF8C5D9AFF00000000000000000000000000000000CE946DFFBE6F
      3DFFC17645FF0000000000000000000000000000000000000000000000000000
      00000000000000000000B26827CCE69958FFE49754FF66391277000000006635
      0F77E18C4BFFE18B48FF64320C770000000000000000000000003AA8DBFF3AA8
      DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8
      DBFF3AA8DBFF3AA8DBFF3AA8DBFF000000000000000000000000000000003AA8
      DBFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000ABA1F5FF6142
      C8FF6142C8FF6851DDFFABA1F5FF00000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF00000000000000000000000000000000000000000000
      000000000000000000000F090311A36126BBE59855FFE59552FFB06221CC924F
      19AAE69D62FFE8A46CFFAD5917CC000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003AA8DBFF3AA8DBFF00000000000000000000000000000000000000003AA8
      DBFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008854
      8EFF6142C8FF6851DDFF6E5DEDFFABA1F5FF000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF00000000000000000000000000000000000000000000
      0000000000003A8241E538853EFF307936FF7E7931FF93772DFCA35F23BBDF7D
      2BFFE3924FFFEBB386FFDC7627FF0E070211040303FF191715FF23211EFF2724
      21FF363333FF00000000292522FF393530FF48433DFF49433DFF3F3A34FF0000
      00003AA8DBFF0000000000000000000000000000000000000000000000003AA8
      DBFF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BD6D
      3AFF88548EFF6851DDFF6E5DEDFF6E5DEDFFABA1F5FF0000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF00000000000000000000000000000000000000000000
      000000000000000000003F8E47F9BCB271FFE8A05DFFBC8136FE0E0803110000
      0000854B1B99E69E60FFE1924DFF572F0E66181614FF7C7268FF9F9489FFA094
      88FF474340FF0000000048423DFF7B7167FFABA197FF90857BFF48423CFF0000
      0000000000000000000000000000000000000000000000000000000000003AA8
      DBFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6A586FFC278
      49FFCA8A60FFABA1F5FF6E5DEDFF6E5DEDFF6E5DEDFFABA1F5FF00000000D39E
      7BFFC27849FFCC9067FF00000000000000000000000000000000000000000000
      0000000000002B5A308A70B679FFB6C590FFE69B54FFE8A464FF78491E880000
      000077461C88E69F60FFE59857FF673913771E1B19FF797066FFA0978CFF9A8F
      84FF44413EFF00000000544F4BFF766C63FFAEA59BFF90867CFF514B45FF0000
      0000000000000000000000000000000000000000000000000000000000003AA8
      DBFF0000000000000000443F38FF5B544DFF4E4842FF302C28FF020202073B37
      35FF3D3934FF312D29FF080606FF000000000000000000000000C9885DFFC073
      42FFC27748FF00000000ABA1F5FF6E5DEDFF6E5DEDFF6E5DEDFFABA1F5FFC988
      5DFFC27849FFC9885DFF00000000000000000000000000000000000000000000
      000034663A8D80C588FFB0DCB6FF74B87BFF8B8E44FAE49649FFE8A360FFE28B
      3BFFE69E5BFFE9A96EFFE49955FF593413664E4B48FFA69A8EFFBBB0A5FF9C91
      86FF6E6B68FF00000000827B75FFA6998EFFC2B9AFFFA3978CFF77716DFF0000
      000000000000000000000000000000000000000000003AA8DBFF3AA8DBFF0000
      00003AA8DBFF3AA8DBFF564F48FFA89D94FF8F857AFF504A45FF010101075F59
      53FFADA195FF8C8276FF191715FF00000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF00000000ABA1F5FF6E5DEDFF6E5DEDFF6E5DEDFF8854
      8EFFBD6D3AFFE2C0AAFF000000000000000086593496E29254FFD58550FF140C
      06173E78459F88CB90FF83C68BFF819A50FF000100034F8240E569441F77A569
      2EBBA5672BBB965C26AA5A36166600000000101010FF12100FFF1F1C1AFF1C1A
      18FF595755FFC6C6C5FF373431FF37322EFF4D4741FF45403AFF5F5852FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000079726DFFB9AEA3FFB2A69BFF817A73FF000000007772
      6DFFB9AEA2FFB3A699FF494643FF00000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF0000000000000000ABA1F5FF6E5DEDFF6E5DEDFF6142
      C8FF88548EFFE2C0AAFF0000000000000000000000002A1C112FD89260FF3421
      1441000000003F7946A189A85DFFAD6734C80000000000000000000000000000
      0000000000000000000000000000000000001B1917FF766C62FF796D63FF8477
      6CFF6A5F55FF4D463EFF7C7166FF7E7368FF7E746AFFA99D90FFA3978BFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005B544EFF48433CFF3E3834FF312D2AFFC5C5C4FF3F3D
      3BFF1B1917FF141211FF101010FF00000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF000000000000000000000000ABA1F5FF6E5DEDFF6142
      C8FF6142C8FF9B80C8FF000000000000000000000000000000007F56348CD697
      67FF70482D8A311E133FE29458FF8F562DA30000000000000000000000000000
      000000000000000000000000000000000000545250FF7C7065FF897565FF937E
      6CFF958371FFA19181FF8E7E6FFF867769FF897D73FFA89B90FFC8BFB7FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B1A599FF9D9286FF83776BFF897B6EFF554B41FF8071
      65FF8A7A6EFF86796DFF252321FF00000000000000000000000000000000D4A1
      7EFFBD6E3BFFBE6F3CFF000000000000000000000000000000008D5E9BFF6143
      C9FF6448D0FF6E5DEDFFABA1F5FF00000000000000000000000000000000E29C
      61FFA36D46C2120C0614E59456FF663F23740000000000000000000000000000
      000000000000000000000000000000000000BBBAB9FF6B645EFF7B6E62FF8575
      67FF5C554DFFC0BCB8FF554E47FF887C71FF9B8E82FF897D72FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000756F699E908377FF8A7C6FFF6E6358FEB5ABA0FF7A6B
      5DFE877463FF6D6157FE8E8D8BFF000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABA1
      F5FF6E5DEDFF6E5DEDFF6E5DEDFF000000000000000000000000000000000000
      0000DA9964FF8D603CA2EBA266FF3421133A0000000000000000000000000000
      00000000000000000000000000000000000000000000E2E1E0FFBAB3ADFFA094
      88FF766C63FF00000000766E65FFACA297FFCFC9C4FFE5E3E1FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002323222ACFCAC5FFB0A69CFE6E665EFF29282739776D
      63FFA59A90FEC7C4C1FF28282834000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000ABA1F5FF6E5DEDFFABA1F5FF000000000000000000000000000000000000
      00009C7049ADE2B089FFE8AB77FF5C3E25680000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B8B4B0FF9489
      7EFFAC9F92FF00000000A5998DFFB3A79CFFDFDBD7FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000062615F6DC2B9B0FFA69A8EFF05040407A598
      8CFFA9A29CFF20201F2C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E0A772FFE2A66DFFE09F69FFC68B5CEA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005C3E
      2766E69A60FFE5975DFFE2945AFFE39359FF2D1D11330000000088563299E28D
      52FFE28C50FFE28A4FFFE28A4FFF87532F990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003AA8DBFF3AA8DBFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B58484FFB584
      84FFB58484FFB58484FFB58484FFB58484FFB58484FFB58484FFB58484FFB584
      84FFB58484FFB58484FFB58484FF000000000000000000000000000000000F0A
      06116B482E77E8A572FFDA905EFE915F3DA90E090510000000001E130B226943
      2777E59A67FFEAAD83FFE4945DFF3C2415440000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003AA8
      DBFF3AA8DBFF3AA8DBFF3AA8DBFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59CFFFFEF
      D6FFF7E7C6FFF7DEBDFFF7DEB5FFF7D6ADFFF7D6A5FFF7CE9CFFF7CE94FFF7CE
      94FFF7CE94FFF7D69CFFB58484FF000000000000000000000000000000000000
      0000000000005C3F2866E5A574FFDD9867FF2619102D00000000000000000000
      0000E39056FFE9A779FFB47142CC000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003AA8DBFF3AA8
      DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59CFFFFEF
      D6FF848484FF848484FF848484FF848484FF848484FF848484FF848484FF8484
      84FF848484FFF7CE9CFFB58484FF000000000000000000000000000000000000
      000000000000000000007B563788E1A578FFD38E5DF5A26B47BDA16B45BDA068
      43BDDF9A6BFFE9A778FFA6693EBB000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003AA8DBFF3AA8DBFF00000000000000003F3A34FF49433DFF48433DFF3935
      30FF292522FF00000000363333FF272421FF23211EFF191715FF040303FF0000
      00003AA8DBFF3AA8DBFF00000000000000000000000000000000C6ADA5FFFFF7
      E7FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF
      84FF84FF84FFF7CE9CFFB58484FF000000000000000000000000000000000000
      000000000000000000000F0A0711CC8F5DE3E3AA80FF956543AD020101030000
      0000E5975CFFE9A675FF88583599000000003F3A34FF49433DFF48433DFF3935
      30FF292522FF00000000363333FF272421FF23211EFF191715FF040303FF0000
      00003AA8DBFF3AA8DBFF000000000000000048423CFF90857BFFABA197FF7B71
      67FF48423DFF00000000474340FFA09488FF9F9489FF7C7268FF181614FF0000
      00003AA8DBFF3AA8DBFF00000000000000000000000000000000C6ADA5FFFFF7
      E7FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF
      84FF84FF84FFF7CE94FFB58484FF000000000000000000000000000000000000
      00000000000000000000000000002E211533E5A471FFDA9865FA4A3221536B48
      2E77E9A672FFE8A571FF6A452B770000000048423CFF90857BFFABA197FF7B71
      67FF48423DFF00000000474340FFA09488FF9F9489FF7C7268FF181614FF0000
      00003AA8DBFF3AA8DBFF0000000000000000514B45FF90867CFFAEA59BFF766C
      63FF544F4BFF0000000044413EFF9A8F84FFA0978CFF797066FF1E1B19FF0000
      00003AA8DBFF3AA8DBFF00000000000000000000000000000000C6ADA5FFFFFF
      F7FF848484FF848484FF848484FF848484FF848484FF848484FF848484FF8484
      84FF848484FFF7CE9CFFB58484FF000000000000000000000000000000000000
      00003A8241E538853EFF307936FF296F2EFF848448FBE4AA7BFFB68055D16D4C
      3179EAAA78FFE9A875FF6B482D7700000000514B45FF90867CFFAEA59BFF766C
      63FF544F4BFF0000000044413EFF9A8F84FFA0978CFF797066FF1E1B19FF0000
      00003AA8DBFF3AA8DBFF000000000000000077716DFFA3978CFFC2B9AFFFA699
      8EFF827B75FF000000006E6B68FF9C9186FFBBB0A5FFA69A8EFF4E4B48FF0000
      00003AA8DBFF3AA8DBFF00000000000000000000000000000000DEC6B5FFFFFF
      FFFF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF
      84FF84FF84FF84FF84FFB58484FF000000000000000000000000000000000000
      0000000000003F8E47F99BD2A2FF95D09DFF286C2DF8C68E5FDBDC9D6AF9A474
      4DBAEDB487FFE9A671FF3D291A440000000077716DFFA3978CFFC2B9AFFFA699
      8EFF827B75FF000000006E6B68FF9C9186FFBBB0A5FFA69A8EFF4E4B48FF0000
      00003AA8DBFF3AA8DBFF00000000000000005F5852FF45403AFF4D4741FF3732
      2EFF373431FFC6C6C5FF595755FF1C1A18FF1F1C1AFF12100FFF101010FF0000
      00003AA8DBFF3AA8DBFF00000000000000000000000000000000DEC6B5FFFFFF
      FFFF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF
      84FF84FF84FF84FF84FFB58484FF000000000000000000000000000000000000
      00002B5A308A70B679FFA6D8ADFF9ED3A5FF307836FC1F160F22E0A36DFAE0A0
      6DFCEEBC95FFEAA974FF3D2B1B44000000005F5852FF45403AFF4D4741FF3732
      2EFF373431FFC6C6C5FF595755FF1C1A18FF1F1C1AFF12100FFF101010FF0000
      00003AA8DBFF3AA8DBFF0000000000000000A3978BFFA99D90FF7E746AFF7E73
      68FF7C7166FF4D463EFF6A5F55FF84776CFF796D63FF766C62FF1B1917FF0000
      00003AA8DBFF3AA8DBFF00000000000000000000000000000000DEC6B5FFFFFF
      FFFF848484FF848484FF848484FF848484FF848484FF848484FF848484FF8484
      84FF848484FFF7DEB5FFB58484FF00000000BD641EDDD96F1DFFAC5515CC687F
      40CA80C588FFB0DCB6FF74B87BFF3E8B45F537833DFBCF996AE7DBA16EF4EABB
      92FFE9BA93FFE7B489FFCB9061E800000000A3978BFFA99D90FF7E746AFF7E73
      68FF7C7166FF4D463EFF6A5F55FF84776CFF796D63FF766C62FF1B1917FF0000
      00003AA8DBFF3AA8DBFF0000000000000000C8BFB7FFA89B90FF897D73FF8677
      69FF8E7E6FFFA19181FF958371FF937E6CFF897565FF7C7065FF545250FF0000
      00003AA8DBFF3AA8DBFF00000000000000000000000000000000DEBDB5FFFFFF
      FFFF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF
      84FF84FF84FFF7DEB5FFB58484FF00000000E49554FFCC6E22EE66340E77859B
      4EF288CB90FF83C68BFF48773DBF00010003398040E17C5D408A82604391C08C
      61D4CB9666E2CB9363E2835E409400000000C8BFB7FFA89B90FF897D73FF8677
      69FF8E7E6FFFA19181FF958371FF937E6CFF897565FF7C7065FF545250FF0000
      00003AA8DBFF3AA8DBFF000000000000000000000000897D72FF9B8E82FF887C
      71FF554E47FFC0BCB8FF5C554DFF857567FF7B6E62FF6B645EFFBBBAB9FF0000
      00003AA8DBFF3AA8DBFF00000000000000000000000000000000DEC6B5FFFFFF
      FFFF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF
      84FF84FF84FFC6BDADFFB58484FF00000000E18E41FFD0752AEE2B1807334926
      0B5592AA5CFF829D4EF901020103000000000000000000000000000000000000
      00000000000000000000000000000000000000000000897D72FF9B8E82FF887C
      71FF554E47FFC0BCB8FF5C554DFF857567FF7B6E62FF6B645EFFBBBAB9FF0000
      00003AA8DBFF3AA8DBFF00000000000000000000000000000000CFC9C4FFACA2
      97FF766E65FF00000000766C63FFA09488FFBAB3ADFF00000000000000000000
      00003AA8DBFF3AA8DBFF00000000000000000000000000000000E7C6B5FFFFFF
      FFFF848484FF848484FF848484FF848484FF848484FFFFF7EFFFF7E7D6FFC6A5
      94FFB5948CFFB58C84FFB58484FF000000004B2E1355C1732DDDE18C40FFB062
      21CCE7A36BFFDE8139FF391C0744000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E5E3E1FFCFC9C4FFACA2
      97FF766E65FF00000000766C63FFA09488FFBAB3ADFFE2E1E0FF000000000000
      0000000000000000000000000000000000000000000000000000C3BFBCDFB3A7
      9CFFA5998DFF00000000AC9F92FF94897EFFB8B4B0FF000000003AA8DBFF3AA8
      DBFF3AA8DBFF3AA8DBFF3AA8DBFF3AA8DBFF0000000000000000E7C6B5FFFFFF
      FFFF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FFE7CECEFFBD8C
      72FFEFB572FFEFA549FFC6846AFF000000001E130822A5672BBB693F1A77683D
      1677B26424CCE49858FF743D1288000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DFDBD7FFB3A7
      9CFFA5998DFF00000000AC9F92FF94897EFFB8B4B0FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003AA8
      DBFF3AA8DBFF3AA8DBFF3AA8DBFF000000000000000000000000EFCEBDFFFFFF
      FFFF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FFE7D6CEFFC694
      7AFFFFC672FFCE9472FF00000000000000001E130922E6984DFFC37B37DD0F09
      031177471C88E69F5FFFA2591EBB000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003AA8DBFF3AA8DBFF00000000000000000000000000000000E7C6B5FFFFF7
      F7FFFFF7EFFFFFF7EFFFFFF7EFFFFFF7EFFFFFF7EFFFFFF7EFFFE7CECEFFC694
      7AFFCE9C84FF000000000000000000000000000000004C321755C5803BDDE393
      43FFE69A51FFE28E42FF77461A88000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B5FFEFCE
      B5FFEFCEB5FFEFCEB5FFEFCEB5FFE7C6B5FFE7C6B5FFEFCEB5FFDEBDB5FFBD84
      7AFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DFCFC4FF87491FFF87491FFFDFCF
      C4FF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E
      8DFF8C8E8DFF8C8E8DFF8C8E8DFFACAEADFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001862
      ADFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000087491FFFB59579FFA48D77FF8749
      1FFFDFCFC4FFFDFDFDFFFDFDFDFFFCFCFCFFFDFDFDFFFDFDFDFFFCFCFCFFFDFD
      FDFFFCFCFCFFFBFBFBFDEFEFEFF38C8E8DFF000000001631BBFF102BB9FF0824
      B6FF021FB5FF001BB3FF0017B2FF0014B1FF0010B0FF000DAFFF000AAEFF0007
      ADFF0005ACFF0004ACFF0002ABFF0000000000000000000000001862ADFF3399
      FFFF1862ADFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2C0AAFFE2C0AAFF00000000000000000000000000000000E2C0AAFFE2C0
      AAFF0000000000000000000000000000000087491FFFCCBBADFFB59579FFA48D
      77FF87491FFFDFCFC4FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFCFCFCFFFDFD
      FDFFFDFDFDFFFDFDFDFFFBFBFBFD8C8E8DFF00000000233CBEFF4B72F2FF3A5E
      F0FF355AF0FF3056F0FF2D54EFFF2850EFFF244DEFFF1F48EEFF1C46EEFF1843
      EEFF1540EDFF2151EFFF0005ACFF00000000000000001862ADFF379CFFFF359A
      FFFF3399FFFF1862ADFF000000000000000014831FFFA5DBA9FF000000000000
      000000000000000000000000000000000000000000000000000000000000C886
      5AFFBE703EFFC58052FF00000000000000000000000000000000CE946DFFBE6F
      3DFFC17645FF000000000000000000000000DFCFC4FF87491FFFCCBBADFFB595
      79FFA48D77FF87491FFFE3E3E3FFE1E1E1FFE0E0E0FFDEDEDEFFDDDDDDFFDEDE
      DEFFDDDDDDFFFCFCFCFFFCFCFCFF8C8E8DFF000000003149C3FF4C6DF2FF3856
      EFFF1B3CEBFF0026E5FF102FDCFF2344EDFF1D3FECFF0920DBFF000BE4FF0518
      E8FF082EEBFF1742EEFF0009ADFF000000001862ADFF1862ADFF1862ADFF3FA2
      FFFF1862ADFF1862ADFF1862ADFF0000000014831FFF14831FFF000000000000
      000000000000000000000000000000000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF0000000000000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF000000000000000000000000DFCFC4FF87491FFFCCBB
      ADFF87491FFF858A88FF858A88FFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFCFCFCFF8C8E8DFF000000003E54C6FF5877F2FF2F4E
      EEFF1A3DEBFFD9DFFCFF4561F0FF1436DFFF0F2FDFFF3B51EEFFD6DCFBFF020F
      E8FF071CE9FF1C46EEFF000CAEFF0000000000000000000000001862ADFF4DAD
      FFFF1862ADFF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF0000000000000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF00000000000000000000000000000001BAA79BDE8749
      1FFF858A88FFE2E3E5FFB7B8B9FF858A88FF858A88FF858A88FFB4B4B4FFD4D4
      D4FFD2D2D2FFF9F9F9FFFDFDFDFF8C8E8DFF000000004B60CAFF6481F3FF2F4F
      EEFFF5F6FEFFFBFCFFFFE6E9FDFF4B66F0FF425EEFFFE2E7FDFFFBFBFFFFF3F4
      FEFF0013E8FF234BEEFF0013B1FF0000000000000000000000001862ADFF5EB9
      FFFF1862ADFF00000000000000000000000014831FFFAFE0B2FF9ED7A3FF8ECD
      93FF00000000000000000000000000000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF0000000000000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF000000000000000000000000000000018C8E8DFFFBFB
      FBFF858A88FFB7B8B9FFE2E3E5FFE2E3E5FFE2E3E5FFE2E3E5FF858A88FFFAFA
      FAFFFAFAFAFFF9F9F9FFFDFDFDFF8C8E8DFF000000005469CEFF6F8BF4FF4D68
      F0FF4F6BF1FFFAFBFFFFEEF1FEFFECEEFEFFEBEEFEFFECEFFDFFF8FAFFFF2040
      EDFF102EEBFF2C53EFFF001AB3FF0000000000000000000000001862ADFF6FC6
      FFFF1862ADFF00000000000000000000000014831FFF14831FFF14831FFF1483
      1FFF000000000000000000000000000000000000000000000000D6A586FFC278
      49FFCA8A60FF000000000000000000000000000000000000000000000000D39E
      7BFFC27849FFCC9067FF000000000000000000000000000000018C8E8DFFFCFC
      FCFFC8C8C8FF858A88FFE2E3E5FF858A88FF858A88FFE2E3E5FF858A88FFCACA
      CAFFC8C8C8FFF8F8F8FFFDFDFDFF8C8E8DFF000000005A71D2FF7693F5FF6782
      F3FF516EF1FF5C75F1FFF2F4FEFFF3F5FEFFF3F5FEFFF0F2FEFF3857EEFF1F41
      EDFF2E4EEEFF355AF0FF0623B6FF0000000000000000000000001862ADFF80D4
      FFFF1862ADFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C9885DFFC073
      42FFC27748FF000000000000000000000000000000000000000000000000C988
      5DFFC27849FFC9885DFF000000000000000000000000000000018C8E8DFFFCFC
      FCFFFCFCFCFF858A88FFE2E3E5FF858A88FFE2E3E5FF858A88FFFCFCFCFFF8F8
      F8FFF6F6F6FFF5F5F5FFFDFDFDFF8C8E8DFF000000006078D5FF93A7F7FF8099
      F5FF6883EAFF7891F4FFF6F7FEFFFEFEFFFFFEFEFFFFF5F6FEFF5C75F2FF3D5A
      E6FF4B67F0FF5473F2FF112CB9FF0000000000000000000000001862ADFF91E0
      FFFF1862ADFF00000000000000000000000014831FFFA7DDACFF94D099FF81C5
      86FF6CB872FF58AD60FF0000000000000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF0000000000000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF000000000000000000000000000000018C8E8DFFFDFD
      FDFFBCBCBCFF858A88FFE2E3E5FFE2E3E5FF858A88FFBCBCBCFFBCBCBCFFBCBC
      BCFFBCBCBCFFF1F1F1FFFDFDFDFF8C8E8DFF000000006680D9FF9EB0F7FF7D99
      F6FF88A0F6FFF7F8FFFFFEFEFFFFFDFDFFFFFDFDFFFFFEFEFFFFF5F7FEFF607A
      F3FF4563F0FF607DF3FF1C36BCFF0000000000000000000000001862ADFF9FEA
      FFFF1862ADFF00000000000000000000000014831FFF14831FFF14831FFF1483
      1FFF14831FFF14831FFF0000000000000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF0000000000000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF000000000000000000000000000000018C8E8DFFFCFC
      FCFFFCFCFCFFFCFCFCFF858A88FF858A88FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFF4F4F4FFFCFCFCFF8C8E8DFF000000006C87DDFFAAB9F8FF829C
      F5FFF6F8FEFFFFFFFFFFFAFBFFFF8BA1F6FF869CF5FFF9FAFFFFFFFFFFFFF3F5
      FEFF4563F0FF6D88F3FF2841C0FF0000000000000000000000001862ADFFA9F3
      FFFF1862ADFF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF0000000000000000000000000000000000000000BD6D
      3AFFBD6D3AFFE2C0AAFF000000000000000000000000000000018C8E8DFFFCFC
      FCFFACAEADFFACAEADFFACAEADFFACAEADFFACAEADFFACAEADFFACAEADFFACAE
      ADFFACAEADFFFCFCFCFFFDFDFDFF8C8E8DFF00000000718DE0FFB3C0F9FF96AE
      F7FF8BA4F6FFE4EAFDFF96ACF7FF859DF6FF7D97F5FF859BF5FFDCE2FCFF5D77
      F2FF5F79F3FF7790F4FF344BC3FF0000000000000000000000001862ADFFABF4
      FFFF1862ADFF00000000000000000000000014831FFFB0E1B4FFA2D8A6FF93D0
      99FF85C78BFF76BF7CFF68B66FFF5AAE62FF000000000000000000000000D4A1
      7EFFBD6E3BFFBE6F3CFF00000000000000000000000000000000C68154FFBF71
      40FFCC8D65FF00000000000000000000000000000000000000018C8E8DFFFDFD
      FDFFF9F9F9FFFAFAFAFFF9F9F9FFF9F9F9FFF6F6F6FFF4F4F4FFEFEFEFFFFDFD
      FDFFF8F8F8FFF7F7F7FF8C8E8DFFACAEADFF000000007593E2FFBBC7F9FFABBE
      F9FFA1B7F7FF95ADF6FF99AFF7FF9CB0F7FF96AAF7FF869DF5FF738CF3FF768E
      F4FF7990F4FF8298F5FF3E54C6FF0000000000000000000000001862ADFFABF4
      FFFF1862ADFF00000000000000000000000014831FFF14831FFF14831FFF1483
      1FFF14831FFF14831FFF14831FFF14831FFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008C8E8DFFFAFA
      FAFDF6F6F6FFF7F7F7FFF8F8F8FFF7F7F7FFF5F5F5FFF3F3F3FFEEEEEEFFFCFC
      FCFFECECECFF8C8E8DFFACAEADFF00000002000000007998E5FFDEE4FCFFDAE0
      FCFFD9DFFCFFD7DDFCFFD4DBFBFFD1D9FBFFCDD6FBFFCAD3FAFFC5CFFAFFC1CC
      FAFFBDC8F9FFC0CCFAFF495ECAFF0000000000000000000000001862ADFF1862
      ADFF1862ADFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008C8E8DFFEBEB
      EBF0FAFAFAFDFDFDFDFFFDFDFDFFFCFCFCFFFDFDFDFFFCFCFCFFFDFDFDFFFAFA
      FAFF8C8E8DFFACAEADFF0000000200000000000000007C9BE7FF7C9BE7FF7B9A
      E6FF7A99E5FF7896E4FF7593E2FF728FE0FF6E8ADEFF6984DBFF657DD8FF6077
      D5FF5B71D2FF566BCFFF5266CDFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000ACAEADFF8C8E
      8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E
      8DFFACAEADFF0000000200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000683F20A2000000010000
      00000000000003010105352010537C4E2BBE975F32E9A16333FA955E33E46F47
      27AA170E07240000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000944
      50690A445069000F121800000000000000000000000000000000000000930000
      00C6000000C7000000C9000000CB000000CC000000D1000000A0000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A46433FF5F3B1F930000
      0000362111549B5F33EEAC7A50F3CEA684FFD8B697FFDBB999FFD3AC8AFFBF92
      6BFCA06335F63A24125B00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000003A
      475D4698A3B20B7184AE00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A46636FEB18057F78955
      2CD4A9744AF4E3CAB4FFECDAC9FFE7D1BCFFE3C9B0FFDEBEA0FFD2AB88FFCEA5
      82FFD3AE8EFF9F6335F51B10082A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000015
      1A2241C1D8FC42C4D7FE11525F7F000000000000000000000000000000C00000
      00C1000000C3000000C4000000C6000000C7000000C9000000CB000000CC0000
      00D1000000D7000000DD00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A56637FDF1E4D8FFD3B1
      94FEF4E9E0FFF3E8DDFFEDDCCCFFD1AC8EFEA97248F5A26433FBA66838FFA568
      38FEA96C3CFFB0774BFF6E4526A8000000000000000000000000000000000000
      0000000000000000000000A0C4FF000000000000000000000000000000000000
      00000DA9CAFE5CD9E8FE209BB4EF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A46536FDF6EEE6FFF5EC
      E3FFF5EDE4FFE6D2C1FFA97349F583522CCA22150A36000000004429156A9D63
      37EDB67B4EFFA66939FEA26535FA000000000000000000000000000000000000
      00000000000000A0C4FF00A0C4FF000000000000000000000000000000000010
      131A01ACC8FF87E6F1FE10A2C2FF00000000000000000000000000000000FF00
      00FF00000000A8462DC0E25E3CFFE25E3CFFE25E3CFFE25E3CFFE25E3CFFA846
      2DC0000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A26434FCF6EEE6FFEBD7
      C4FFEAD9C9FFA36433FE4429156A0000000000000000000000000704020B975C
      30E96F523B8D8B603EC2A46433FF030101050000000000000000000000000008
      081100A0C4FF75EDFBFF00A0C4FF0000000000000000001E243000303A50007E
      9AC96CE6F5FF75E2EFFF18A3C1FF000000000000000000000000FF0000FFFF00
      00FF00000000C8502EFFF8CFC2FFF8CFC2FFF8CFC2FFF8CFC2FFF8CFC2FFC850
      2EFF000000000000000000000000000000000000000000000000806C5DFF7B60
      4BFF806148FF816148FF816148FF816148FF816148FF836147FF816247FF8161
      48FF7B604BFF806C5DFF000000000000000000000000A26333FCF5EDE5FFF6ED
      E5FFF5ECE4FFD5B59BFD925B30E00A0603100000000000000000000000000000
      000015100C1A2D221939A26333FC0704020C00000000000000000008081100A0
      C4FF75EDFBFF75EDFBFF00A0C4FF00A0C4FF00A0C4FF00A0C4FF00A9C4FF6DE1
      EEFF0EC9DFFF68E4F2FF199FBAF80000000000000000FF0000FFFF0000FFFF00
      00FF0000000087351BC0B14523FFB14523FFB14523FFB14523FFB14523FF8735
      1BC00000000000000000000000000000000000000000000000007B604BFFE6CC
      B4FFD1B69CFFD2B59AFFD1B398FFD2B295FFCBAB8EFFCBA98CFFC8A689FFC6A3
      89FFDCBDA4FF7D604BFF000000000000000000000000A06232F9A36433FEA364
      33FEA36333FDA26333FCA16332FB774825B9120B051D0F0904180F0904180F09
      04180F0904180F090418120B051C00000000000000000008081100A0C4FF75ED
      FBFF03C3DAFF75EDFBFF68EAF9FF68EAF9FF68EAF9FF68EAF9FF04DDF7FF09C8
      DFFF06C2D8FF6EDCEBFF199CB7F400000000FF0000FFFF0000FFFF0000FFFF00
      00FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B604BFFEED4
      BCFFEDD2B8FFEFD2B7FFEDCFB4FFEFCFB2FFEECEB1FFF1CFB2FFF0CEB1FFEFCC
      B2FFEBCCB3FF7D604BFF0000000000000000000000000805020D000000000000
      000000000000000000000000000000000000673F20A0A46433FFA87044F8A972
      49F7A97249F7A97349F7A46433FF050301080008081100A0C4FF78EDFBFF31E2
      F8FF2BDFF4FF03C0D6FF03C0D6FF03C0D6FF1CD2E8FF1CD2E8FF1CD2E8FF0AC8
      DFFF69E5F3FF1AA6BFF8107F95CB0000000000000000FF0000FFFF0000FFFF00
      00FF0000000090442AC0BC5735FFBC5735FFBC5735FFBC5735FFBC5735FFBC57
      35FFBC5735FF90442AC000000000000000000000000000000000806C5DFF7B60
      4BFF806148FF816148FF816148FF826348FF816247FF836147FF836147FF8461
      49FF7D604BFF806C5DFF000000000000000000000000A26333FC583B257E221A
      132B0D090613010000020000000000000000020100047F4F2AC4CCA88CFAF6EE
      E7FFF2E6DBFFF6EEE6FFA36738FB0503010900A0C4FFADF3FBFF2EE0F6FF31E2
      F8FF31E2F7FF31E2F7FF2EE0F5FF28DBF1FF1CD2E8FF1CD2E8FF1CD2E8FF35D9
      ECFF3FCDE1FF117F95CA0004040A000000000000000000000000FF0000FFFF00
      00FF00000000D6704EFFF8CFC2FFF8CFC2FFF8CFC2FFF8CFC2FFF8CFC2FFF8CF
      C2FFF8CFC2FFD6704EFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A36434FEA46738FB7A59
      3F9D955C31E6160D07230000000000000000000000003D251360A46534FFE9D7
      C7FFEBD8C6FFF5ECE3FFA36738FA0603020A0004040A00A0C4FFADF3FBFF2EE0
      F6FF31E2F7FF28DBF1FF2EE0F5FF28DBF1FF15CDE3FF35D9ECFF68E7F6FF40CD
      E2FE1092ACE4001718320000000000000000000000000000000000000000FF00
      00FF00000000AF6348C0EB8562FFEB8562FFEB8562FFEB8562FFEB8562FFEB85
      62FFEB8562FFAF6348C000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009E6235F3AB6F40FFA86B
      3BFEA06537F54B2E1775100905192C1B0E4585542CCDB2835CF5EBDBCDFFF5EB
      E2FFF6EEE6FFF6EEE6FFA46738FA0704020B000000000008081100A0C4FFADF3
      FBFF30E1F6FF1FE3FAFF72ECFAFF6EEBFAFF6DE8F7FF68E1F0F810869CD41088
      A1D5000D0D1C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000653F229BBE8F67FDC598
      71FFA86A3BFFA46534FFA56839FCAE7F58F3D8BAA0FEF1E4D8FFF2E6DBFFF3E8
      DDFFCCA687FDEAD8C8FFA36738F90805020D00000000000000000008081100A0
      C4FFADF3FBFF24E4FBFF00A0C4FF00A0C4FF1192ACE7108198CF116778A40005
      050B000000000000000000000000000000000000000000000000000000AD0000
      00AE000000AF000000B0000000B1000000B2000000B4000000B5000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001A1008299F6436F5D3AD
      8CFFDCBD9DFFDDBEA1FFE5CBB4FFE9D3BFFFEEDDCCFFF0E2D5FFE7D2BFFFA871
      47F57C4D28C0A56D40F7A26434FC0905020E0000000000000000000000000008
      081100A0C4FFADF3FBFF00A0C4FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000331F1050A063
      35F6BC8D65FAD3B08FFFDFC2A8FFDEC1A8FFD4B193FFB1815AF49B6033F03822
      11580000000041281566A46433FF0905030F0000000000000000000000000000
      0000000C0F1400A0C4FF00A0C4FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      00AC000000AD000000AD000000AE000000AF000000B0000000B1000000B20000
      00B4000000B50000008900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000120B
      051D6841249F915B32DE9F6233F6965D32E57B4D2ABC35201053030101050000
      000000000000000000004D2F18790A0603100000000000000000000000000000
      0000000000000000000000A0C4FF000000000000000000000000000000000000
      000000000000000000000000000000000000DFCFC4FF87491FFF87491FFFDFCF
      C4FF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E
      8DFF8C8E8DFF8C8E8DFF8C8E8DFFACAEADFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000103A52F7275D85FB4786BAFB316B8EC1050F172200000000000000000000
      000000000000000000000000000000000000000000004F525194858A88FF858A
      88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A
      88FF858A88FF858A88FF505352940000000087491FFFB59579FFA48D77FF8749
      1FFFDFCFC4FFFDFDFDFFFDFDFDFFFCFCFCFFFDFDFDFFFDFDFDFFFCFCFCFFFDFD
      FDFFFCFCFCFFFBFBFBFDEFEFEFF38C8E8DFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002A6381FB94C7F9FF91C9F9FF4085C9FF1A5EA2F30816222F000000000000
      00000000000000000000000000000000000000000000848987FEF8F8F8FFF8F8
      F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FF000000FFF8F8F8FF000000FFF8F8
      F8FFF8F8F8FFF8F8F8FF878C8AFE0000000087491FFFCCBBADFFB59579FFA48D
      77FF87491FFFDFCFC4FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFCFCFCFFFDFD
      FDFFFDFDFDFFFDFDFDFFFBFBFBFD8C8E8DFF000000003D3833FF38332FFF322E
      2AFF2B2824FF262320FF1F1C1AFF0202021A0E0C0BDB0A0908FF060605FF0303
      02FF000000FF000000FF00000000000000000000000000000000000000000000
      00004289AAFFE0F2FFFF539AD8FF1979BEFF4898C5FF3F81B6FF000000000000
      00000000000000000000000000000000000000000000858A88FFF9F9F9FFDBC3
      ADFFDCC4AEFFDDC5AFFFDEC6B0FFDFC7B1FFE0C8B2FF000000FFF7F7F7FFF9F9
      F9FFF8F8F8FFF7F7F7FF888D8BFF00000000DFCFC4FF87491FFFCCBBADFFB595
      79FFA48D77FF87491FFFE3E3E3FFE1E1E1FFE0E0E0FFDEDEDEFFDDDDDDFFDEDE
      DEFFDDDDDDFFFCFCFCFFFCFCFCFF8C8E8DFF0000000045403AFF85796FFFC3B8
      AEFF7B7167FF7E746AFF35312CFF0101010F211E1CD495897CFFBAAEA2FF7B71
      67FF7E746AFF000000FF00000000000000003D3833FF38332FFF322E2AFF2B28
      24FF97B5C9FF79B6D5FF90B7D1FF54C9E4FF5ADFF5FF77D0EDFF4E9BDBFF0000
      00FF000000FF00000000000000000000000000000000858A88FFF9F9F9FFDBC3
      ADFFB09882FFB09882FFB09882FFB09882FFE0C8B2FF000000FFF7F7F7FFF8F8
      F8FFF7F7F7FFF7F7F7FF888D8BFF0000000000000000DFCFC4FF87491FFFCCBB
      ADFF87491FFF858A88FF858A88FFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFCFCFCFF8C8E8DFF000000004C4640FF83776EFFCCC3
      BAFF776E64FF7A7066FF2D2926F9000000011A1916EE95897CFFC2B8ADFF776E
      64FF7B7167FF050404FF000000000000000045403AFF85796FFFC3B8AEFF7B71
      67FF7E746AFFA4C6D7FF74B8D6FFC2F6FDFF62DFF7FF5CE2F8FF78D3F0FF4999
      DCFF000000FF00000000000000000000000000000000858A88FFFAFAFAFFDBC3
      ADFFDCC4AEFFDDC5AFFFDEC6B0FFDFC7B1FFE0C8B2FF000000FFF5F5F5FFF6F6
      F6FFF6F6F6FFF7F7F7FF888D8BFF000000000000000000000001BAA79BDE8749
      1FFF858A88FFE2E3E5FFB7B8B9FF858A88FF858A88FF858A88FFB4B4B4FFD4D4
      D4FFD2D2D2FFF9F9F9FFFDFDFDFF8C8E8DFF00000000514A44FC83776EFFCCC3
      BAFF786F65FF70675EFF2D2A26D5000000001E1C1AD585796FFFC2B8ADFF776E
      64FF7A7066FF090807FC00000000000000004C4640FF83776EFFCCC3BAFF776E
      64FF7A7066FF2D2926F989AEBFFF76CBE7FFC7F7FDFF5DDCF5FF59E1F7FF7AD4
      F1FF4B9ADEFF00000000000000000000000000000000858A88FFFBFBFBFFDBC3
      ADFFDCC4AEFFDDC5AFFFDEC6B0FFDEC6B0FF000000FFE0C8B2FF000000FFE1C9
      B3FFF4F4F4FFF7F7F7FF888D8BFF0000000000000000000000018C8E8DFFFBFB
      FBFF858A88FFB7B8B9FFE2E3E5FFE2E3E5FFE2E3E5FFE2E3E5FF858A88FFFAFA
      FAFFFAFAFAFFF9F9F9FFFDFDFDFF8C8E8DFF00000000443E39C39F9286FFCCC3
      BAFFC0B4AAFFA6988BFF282522A8000000001C1A18A8908478FFC2B8ADFFC0B4
      AAFFA89B8EFF0C0B0AC30000000000000000514A44FC83776EFFCCC3BAFF786F
      65FF70675EFF2D2A26D50000000095BDCAFF78D3EEFFC7F7FDFF5EDCF5FF5AE2
      F7FF79D6F2FF4FA0E0FF000000000000000000000000858A88FFFBFBFBFFDAC2
      ACFFAF9781FFAF9781FFAF9781FFAF9781FFAF9781FFAF9781FFAF9781FFE0C8
      B2FFF2F2F2FFF7F7F7FF888D8BFF0000000000000000000000018C8E8DFFFCFC
      FCFFC8C8C8FF858A88FFE2E3E5FF858A88FF858A88FFE2E3E5FF858A88FFCACA
      CAFFC8C8C8FFF8F8F8FFFDFDFDFF8C8E8DFF0202010559524BF9413C37FF5750
      49FF3C3732FF322E2AFF1F1C19E504040330151311B5191715FF24211FFF1816
      14FF0E0D0CFF000000EE0000000200000000443E39C39F9286FFCCC3BAFFC0B4
      AAFFA6988BFF282522A8000000001C1A18A8B5DEEBFF7CD4EEFFC4F6FDFF6BDD
      F6FF6CCAEDFF62A3D7FF6299C9FF0C151E2600000000858A88FFFCFCFCFFDAC2
      ACFFDBC3ADFFDCC4AEFFDDC5AFFFDDC5AFFFDEC6B0FFDEC6B0FFDFC7B1FFDFC7
      B1FFF0F0F0FFF8F8F8FF888D8BFF0000000000000000000000018C8E8DFFFCFC
      FCFFFCFCFCFF858A88FFE2E3E5FF858A88FFE2E3E5FF858A88FFFCFCFCFFF8F8
      F8FFF6F6F6FFF5F5F5FFFDFDFDFF8C8E8DFF030202059D9185FFB1A396FF7E74
      6AFF7B7167FF766C63FF6B625AFF2D2925FF554E47FF80756BFF7B7167FF766C
      63FF6F665DFF000000FE000000050000000059524BF9413C37FF575049FF3C37
      32FF322E2AFF1F1C19E504040330151311B5191715FFB5E6F5FF81D6EEFFB2E3
      F9FF8BC0E7FFAED3F6FFC4E0FCFF629ACCF700000000858A88FFFCFCFCFFDAC2
      ACFFDAC2ACFFDBC3ADFFDCC4AEFFDDC5AFFFDDC5AFFFDEC6B0FFDEC6B0FFDEC6
      B0FFEEEEEEFFF8F8F8FF888D8BFF0000000000000000000000018C8E8DFFFDFD
      FDFFBCBCBCFF858A88FFE2E3E5FFE2E3E5FF858A88FFBCBCBCFFBCBCBCFFBCBC
      BCFFBCBCBCFFF1F1F1FFFDFDFDFF8C8E8DFF020202049A8E82E1BAAEA2FF8276
      6CFF82766CFFAA917AFFBAA794FFB3A18BFAB09781FF9F8D7CFF836C5AFF7062
      56FF95897CFF030302E000000003000000009D9185FFB1A396FF7E746AFF7B71
      67FF766C63FF6B625AFF2D2925FF554E47FF80756BFF7B7167FFB1E6F5FF76BE
      E7FFB4D2F0FFE5F3FFFFACD2EFFF417FB5E800000000858A88FFFCFCFCFFD9C1
      ABFFAF9781FFAF9781FFAF9781FFAF9781FFDCC4AEFFAF9781FFAF9781FFDDC5
      AFFFECECECFFF8F8F8FF888D8BFF0000000000000000000000018C8E8DFFFCFC
      FCFFFCFCFCFFFCFCFCFF858A88FF858A88FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFF4F4F4FFFCFCFCFF8C8E8DFF05050508262320489B8E82FF9D91
      85FF867A70FF554E47FF4F4943FF80756BFF6D655CFF826B57FFA6917CFF9484
      73FF554E47FF0505057A00000001000000009A8E82E1BAAEA2FF82766CFF8276
      6CFFAA917AFFBAA794FFB3A18BFAB09781FF9F8D7CFF836C5AFF706256FF94BD
      CCFF57A5D8FF85B1DBFF459DD0FF0F374D5E00000000858A88FFFDFDFDFFD8C0
      AAFFAF9781FFAF9781FFAF9781FFAF9781FFDBC3ADFFDCC4AEFFDCC4AEFFDCC4
      AEFFEBEBEBFFF8F8F8FF888D8BFF0000000000000000000000018C8E8DFFFCFC
      FCFFACAEADFFACAEADFFACAEADFFACAEADFFACAEADFFACAEADFFACAEADFFACAE
      ADFFACAEADFFFCFCFCFFFDFDFDFF8C8E8DFF0000000000000000736A61FFA497
      8AFF95897CFF9F9286FF3D3833FF000000004B453FFF7D7369FF85796FFF3D38
      33FF2D2925A70101010C0000000200000000262320489B8E82FF9D9185FF867A
      70FF554E47FF4F4943FF80756BFF6D655CFF826B57FFA6917CFF948473FF554E
      47FF0505057A00000001000000000000000000000000858A88FFFDFDFDFFD8C0
      AAFFAF9781FFAF9781FFAF9781FFAF9781FFDBC3ADFFDBC3ADFFDBC3ADFFDBC3
      ADFFDBC3ADFFF8F8F8FF888D8BFF0000000000000000000000018C8E8DFFFDFD
      FDFFF9F9F9FFFAFAFAFFF9F9F9FFF9F9F9FFF6F6F6FFF4F4F4FFEFEFEFFFFDFD
      FDFFF8F8F8FFF7F7F7FF8C8E8DFFACAEADFF0000000000000000000000000000
      00007E7469E2C3B8AEFF645C54FF000000007B7167FFA89B8EFF8B8075E40000
      00000000000000000000000000000000000000000000736A61FFA4978AFF9589
      7CFF9F9286FF3D3833FF000000004B453FFF7D7369FF85796FFF3D3833FF2D29
      25A70101010C00000002000000000000000000000000858A88FFFEFEFEFFD7BF
      A9FFAF9781FFAF9781FFAF9781FFAF9781FFDAC2ACFFAF9781FFAF9781FFAF97
      81FFDAC2ACFFF8F8F8FF888D8BFF0000000000000000000000008C8E8DFFFAFA
      FAFDF6F6F6FFF7F7F7FFF8F8F8FFF7F7F7FFF5F5F5FFF3F3F3FFEEEEEEFFFCFC
      FCFFECECECFF8C8E8DFFACAEADFF000000020000000000000000000000000000
      00008A7F74E2BCB0A4FF9D9185FF00000000AEA093FF9D9185FF564F48DA0000
      0000000000000000000000000000000000000000000000000000000000007E74
      69E2C3B8AEFF645C54FF000000007B7167FFA89B8EFF8B8075E4000000000000
      00000000000000000000000000000000000000000000858A88FFFDFDFDFFD6BE
      A8FFD7BFA9FFD7BFA9FFD8C0AAFFD8C0AAFFD9C1ABFFD9C1ABFFD9C1ABFFD9C1
      ABFFD9C1ABFFF8F8F8FF888D8BFF0000000000000000000000008C8E8DFFEBEB
      EBF0FAFAFAFDFDFDFDFFFDFDFDFFFCFCFCFFFDFDFDFFFCFCFCFFFDFDFDFFFAFA
      FAFF8C8E8DFFACAEADFF00000002000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008A7F
      74E2BCB0A4FF9D9185FF00000000AEA093FF9D9185FF564F48DA000000000000
      00000000000000000000000000000000000000000000848987FEFDFDFDFFFEFE
      FEFFFEFEFEFFFDFDFDFFFDFDFDFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFAFA
      FAFFFAFAFAFFF9F9F9FF878C8AFE000000000000000000000000ACAEADFF8C8E
      8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E
      8DFFACAEADFF0000000200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004F525094858A88FF858A
      88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A
      88FF858A88FF858A88FF4F525194000000000000000044251081D38044FF5256
      54FF525654FF525654FF525654FF525654FF525654FF525654FF525654FF5256
      54FFD38044FFD38044FFD38044FF44251081000000000000001D000000340000
      0036000000360000003600000036000000360000003600000036000000360000
      003600000036000000330000001D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002D180A5687491FFF87491FFF8749
      1FFF241308440000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000044251081D38044FFCCA98EFF927A
      62FFE3E3E3FFD6D6D6FFBABABAFFBABABAFFBABABAFFB9B9B9FFB9B9B9FF947B
      62FFCE9F71FFDCB898FFEAD2BEFFD38044FF0000000000000034EFEFEFF5FAFA
      FAFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFAFAFAFDEDEDEDF3000000330000000000000000123F57FF14415DFF2468
      9CFF2B75B4FF275E7EAD00000000000000000000000000000000000000000000
      00000000000000000000000000000000000087491FFFBEA592FFB89C86FFB89C
      86FF87491FFF2413084400000000000000000000000000000000000000004346
      45816E7270D3434645810000000000000000D38044FFCCA98EFFDCB898FF927A
      61FFE1E1E1FFE6E6E6FF525654FF525654FFC5C5C5FFC2C2C2FFB2B2B2FF9379
      60FFCE9E70FFDBB694FFEAD2BEFFD38044FF0000000100000036FBFBFBFEFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFAFAFAFD000000360000000000000000114158FF5C9CD4FFA6CF
      F5FFA9CFECFF478BC1FF2B75B4FF000000000000000000000000000000000000
      00000000000000000000000000000000000087491FFFCCBBADFFA7917CFFB595
      79FFAE8B71FF87491FFF241308440000000000000000000000002729284C858A
      88FF9EA1A0FF858A88FF0000000000000000D38044FFEAD2BEFFCF9F71FF927A
      61FFC5C5C5FFE1E1E1FF525654FF525654FFC5C5C5FFC5C5C5FFBFBFBFFF9379
      60FFCE9E70FFDBB694FFEAD2BEFFD38044FF0000000100000036FCFCFCFFFCFC
      FCFF174259FF2A6089FF4B8ABEFF6EA8CBFFE0E9F1FFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFCFCFCFF0000003600000001000000001D6C93FFCBE3F9FF60AA
      ECFF3F98E8FF1466C2FF155FAAFF2B75B4FF0000000000000000000000000000
      00000000000000000000000000000000000087491FFFCCBBADFFA48D77FFA28A
      73FFB49579FFB3937BFF87491FFF00000000000000002729284C858A88FFD2D3
      D4FFC2C3C4FF858A88FF0000000000000000D38044FFEAD2BEFFCF9F71FF9279
      60FFC5C5C5FFC5C5C5FF525654FF525654FFE2E2E2FFC5C5C5FFB9B9B9FF9379
      60FFCE9E70FFDBB694FFEAD2BEFFD38044FF0000000100000036FCFCFCFFFCFC
      FCFF2D6685FF94C7F9FF91C9F9FF4085C9FF256AAEFFD4E2EEFFFAFAFAFFFAFA
      FAFFFAFAFAFFFCFCFCFF0000003600000001000000001D6C93FFC8E1F2FFD1E7
      FAFF337CB5FF3099C3FF6CC4DCFF499CCFFF3383C7FF00000000000000000000
      000000000000000000000000000000000000190D063087491FFFD4C8BDFFA48D
      77FFA48D77FFBEA592FF87491FFF000000002729284C858A88FFE2E3E4FFC2C4
      C6FF858A88FF434645810000000000000000D38044FFEAD2BEFFCF9F71FF9C7D
      5FFFBABABAFFC5C5C5FFC5C5C5FFE1E1E1FFE7E7E7FFD7D7D7FFBABAB9FF997A
      5CFFCE9E6FFFDBB694FFEAD2BEFFD38044FF0000000100000036FCFCFCFFFCFC
      FCFF4289AAFFE0F2FFFF539AD8FF1979BEFF4898C5FF478EC7FFD8E6F3FFF8F8
      F8FFF8F8F8FFFCFCFCFF000000360000000100000000040C13202589B9FFB0CB
      E1FF66A9C8FF5FDCF5FF43D6F4FF8EEEFAFF5CB4E6FF3A8FD9FF000000000000
      000000000000000000000000000000000000000000000D07031987491FFFCCBB
      ADFFCCBBADFFB3937BFF87491FFF43464581858A88FFD3D3D4FFBDBEBFFF858A
      88FF2729284C000000020000000000000000D38044FFEAD2BEFFCF9F71FFB08C
      68FF947B61FF947B61FF947B61FF947B61FF947B61FF947B61FF947B61FFB08C
      68FFDBB694FFCF9F71FFEAD2BEFFD38044FF0000000100000036FCFCFCFFFCFC
      FCFFA5C3D7FF79B6D5FF90B7D1FF54C9E4FF5ADFF5FF77D0EDFF509DDDFFDFEB
      F5FFF8F8F8FFFCFCFCFF00000036000000010000000000000000000000002589
      B9FFBEE6F2FFB3F4FCFF5FDCF5FF43D6F4FF8EEEFAFF5CB4E6FF3A8FD9FF0000
      00000000000000000000000000000000000000000000000000000D0703198749
      1FFF87491FFF87491FFF85837CFFAAADADFFC8C9CAFFBDBEBFFF858A88FF2729
      284C00000000000000000000000000000000D38044FFEAD2BEFFCF9F71FFCF9F
      71FFCF9F71FFCF9F71FFCF9F71FFCF9F71FFCF9F71FFCF9F71FFCF9F71FFCF9F
      71FFCF9F71FFCF9F71FFEAD2BEFFD38044FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFB2D5E5FF75BAD7FFC2F6FDFF62DFF7FF5CE2F8FF78D3F0FF4898
      DCFFDEE9F2FFFCFCFCFF00000036000000010000000000000000000000000000
      00002690BFFFC3EDF8FFB3F4FCFF5FDCF5FF43D6F4FF8EEEFAFF5CB4E6FF3A8F
      D9FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B5B7B8FF858A88FFB7B8B9FF858A88FF2729284C0000
      000200000000000000000000000000000000D38044FFEAD2BEFFCF9F71FFFEFE
      FEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
      FEFFFEFEFEFFCF9F71FFEAD2BEFFD38044FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFAFD4E5FF76CBE7FFC7F7FDFF5DDCF5FF59E1F7FF7AD4
      F1FF4A9ADDFFD4E5F5FF00000036000000010000000000000000000000000000
      0000000000002EBAE4FFC3EDF8FFB3F4FCFF5FDCF5FF43D6F4FF8EEEFAFF5CB4
      E6FF3A8FD9FF000000000000000000000000000000002729284C858A88FF858A
      88FF858A88FF858A88FFD0D1D2FFA3A4A4FF858A88FF43464581000000000000
      000000000000000000000000000000000000D38044FFEAD2BEFFCF9F71FFFEFE
      FEFFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFCF9F71FFEAD2BEFFD38044FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFBDE5F2FF78D3EEFFC7F7FDFF5EDCF5FF5AE2
      F7FF79D6F2FF50A2E2FF0C1A265C000000010000000000000000000000000000
      000000000000000000002EBAE4FFC3EDF8FFB3F4FCFF5FDCF5FF43D6F4FF8EEE
      FAFF5CB4E6FF3A8FD9FF00000000000000002729284C858A88FFF2F2F2FEEFF0
      F0FFECEDEDFEE9EBEBFEB6B9BAFF858A88FF2729284C858A88FF2729284C0000
      000000000000000000000000000000000000D38044FFEAD2BEFFCF9F71FFFEFE
      FEFFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7
      D7FFF4F4F4FFCF9F71FFEAD2BEFFD38044FF0000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFBAE3F0FF7CD4EEFFC4F6FDFF6BDD
      F6FF6CCAEDFF62A3D7FF5690C3F00C151E270000000000000000000000000000
      00000000000000000000000000002EBAE4FFC3EDF8FFB3F4FCFF67D9F5FF6ECF
      F3FF589DD0FF72ABDDFF4E91C9FF00000000858A88FFF4F5F5FEA9ACABFF858A
      88FFF7F7F7FFE2E3E5FFAAADADFF77787881000000002729284C858A88FF2729
      284C00000000000000000000000000000000D38044FFEAD2BEFFCF9F71FFFEFE
      FEFFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFCF9F71FFEAD2BEFFD38044FF0000000100000036FCFCFCFFF9F9
      F9FFF9F9F9FFF9F9F9FFF7F7F7FFF6F6F6FFF2F2F2FFA8D9E8FF81D6EEFFB2E3
      F9FF8BC0E7FFAED3F6FFC4E0FCFF629ACCF70000000000000000000000000000
      0000000000000000000000000000000000002EBAE4FFC3EDF8FFA8E2F8FF6BAE
      DDFFA5CFF4FFA5CFF4FFBDDBF7FF508EC4F72729284C858A88FF000000002729
      284C858A88FFFAFAFAFF858A88FF0000000000000000000000002729284C858A
      88FF878C8AFF0000004C0000000000000000D38044FFEAD2BEFFCF9F71FFF9F9
      F9FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2
      E2FFF9F9F9FFCF9F71FFEAD2BEFFD38044FF0000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFAFE4F3FF76BE
      E7FFB4D2F0FFE5F3FFFFACD2EFFF417FB5E80000000000000000000000000000
      000000000000000000000000000000000000000000002EBAE4FFA7D4F4FFC5E1
      F8FFCCE3F9FFCCE3F9FFBDDBF7FF4E8FC7FD0000000000000000000000000000
      0000858A88FFEEF0F0FF858A88FF00000000000000000000000000000000858A
      88FFF0F0F0FF858A88FF0000004C00000000D38044FFEAD2BEFFCF9F71FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFCF9F71FFEAD2BEFFD38044FF0000000000000036F9F9F9FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFACD5
      E4FF57A5D8FF85B1DBFF459DD0FF0F374D5E0000000000000000000000000000
      00000000000000000000000000000000000000000000000000004FA8D9FF69A5
      D8FFC9E1F7FFCBE3F8FF4195CAFF215984AE000000000000000000000000858A
      88FFE9EBECFF858A88FF2729284C000000000000000000000000000000000000
      004C858A88FFEBECECFC858A88FF00000000D38044FFEAD2BEFFEED9C8FF3DAF
      FCFF3DAFFCFF3DAFFCFF3DAFFCFF3DAFFCFF3DAFFCFF3DAFFCFF3DAFFCFF3DAF
      FCFF3DAFFCFFEAD2BEFFEAD2BEFFD38044FF0000000000000033E9E9E9F0F9F9
      F9FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF5454
      5491000000200000000200000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000001060809489C
      C9EA4F92C8FD4D90C8FF2889B7DF051015190000000000000000000000002729
      284C858A88FF2729284C00000000000000000000000000000000000000000000
      00000000004C858A88FF2729284C00000000583014A7D38044FFD38044FF005B
      CEFF005BCEFF005BCEFF005BCEFF005BCEFF005BCEFF005BCEFF005BCEFF005B
      CEFF005BCEFFD38044FFD38044FF583014A7000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      0020000000020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000050A1118679F333CC2F718207FB90001
      0F1C000000000000000000000000000000000000000000000000000000000000
      0000000000004244437F858A88FF858A88FF858A88FF858A88FF858A88FF858A
      88FF858A88FF858A88FF4244437F00000000000000000000000000042B990004
      38CC000438CC000438CC000438CC000438CC000438CC000438CC000438CC0004
      38CC000438CC00042B9900000000000000000000000000000000000000000000
      00000000000000111A30002C447C00334D950000001A0000001A0000001A0000
      001A0000001A0000001A0000001A000000110000000000000000000000000001
      111F000215260000010300000000070C44703F4AD9FF252DA5DD3C46D1FF1218
      6CA4000000000000000000000000000000000000000000000000000000000000
      000000000000858A88FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF858A88FF000000000000000000000000000C43CC386B
      B0FF3568ADFF3467ACFF3366ABFF3366ABFF3366ABFF3265AAFF3265AAFF3164
      A9FF3164A9FF000C43CC0000000000000000000000000000000000000000001F
      3050003B5A99155B7FC04791B4E0064A69E21717068717170687171706871717
      06871717068717170687171706870E0E046A000000000000000005093A63303B
      C1F83640CFFF151C76B000000C18171E7BAF343FCAFD00000C1810166295323C
      BFF800010B140000000000000000000000000000000000000000000000000000
      000000000000858A88FFFFFFFFFFB6BDBAFFB6BDBAFFB6BDBAFFB6BDBAFFECEE
      EEFFECEEEEFFFFFFFFFF858A88FF000000000000000000000000001752CC3669
      AEFF3063A8FF3063A8FF3063A8FF31506CFF304857FF304857FF304857FF3048
      57FF304958FF19242FE71919088514140764000000000000000000344E8E3189
      B2F165B4D8FA81CBECFF84CEEEFF4086A8FFFFFFFFFFFFFFFEFFFEFEFDFFFEFE
      FCFFFDFDFAFFFCFCF9FFFEFEF9FF1F1F0E7F00000000000000002832A4DE2F38
      B4E51C2488C23E49DDFF171F83C21C268DCB2F38BCF200000103070C49773640
      C8FF010211200000000000000000000000000000000000000000000000000000
      000000000000858A88FFFFFFFFFFECEEEEFFECEEEEFFECEEEEFFECEEEEFFECEE
      EEFFECEEEEFFFFFFFFFF858A88FF000000000000000000000000001B57CC3669
      AEFF2D60A5FF2D60A5FF2D60A5FF3A5567FFFFFFFFFFFFFFFEFFFEFEFDFFFDFD
      FBFFFDFDF9FFFCFCF7FFFDFDF8FF2323127B000000000000000000466BB287D1
      EFFF7ECAE9FF7ECAE9FF87D0EFFF4389ACFFFEFEFEFFFDFDFCFFFCFCFBFFFBFB
      F8FFFAFAF6FFF8F8F4FFFBFBF6FF2525157800000000000000002E37B4EC171F
      76AF000000000F166599414FD2FFCAA274FE404ED6FF0D14649D262F9FD42830
      A6DF0000050A0000000000000000000000000000000000000000000000000000
      000000000000858A88FFFFFFFFFFB6BDBAFFB6BDBAFFB6BDBAFFB6BDBAFFB6BD
      BAFFB6BDBAFFFFFFFFFF858A88FF000000000000000000000000001E5ACC376A
      AEFF2B5EA2FF2B5EA2FF2B5EA2FF3F5A6DFFFFFFFEFFBDBDACFFBDBDACFFCECE
      BDFFC6C6B5FFDFDFCEFFFAFAF4FF28281776000000000000000000476BA48AD3
      F0FF82CDEBFF82CDEBFF8AD3F0FF458BAFFFCCCCBBFFCBCBBAFFDBDBCAFFD1D1
      C0FFF8F8F4FFF7F7F1FFFAFAF4FF2828177600000000000000001118679C3A45
      CCFF0104203A02052F533A47D4FFDBBD9CFFEECCA6FF3F4BDEFF3942D1FF060A
      3E69000000000000000000000000000000004244437F858A88FF858A88FF858A
      88FF858A88FF858A88FFFFFFFFFFECEEEEFFECEEEEFFECEEEEFFECEEEEFFECEE
      EEFFECEEEEFFFFFFFFFF858A88FF00000000000000000000000000205ECC376A
      AFFF285BA0FF285BA0FF285BA0FF405C70FFFEFEFDFFE2E2D1FFE2E2D1FFE2E2
      D1FFE2E2D1FFE2E2D1FFFAFAF2FF2A2A1973000000000000000000466A9E8ED6
      F2FF87D0EDFF87D0EDFF8ED6F2FF478FB2FFEDEDDCFFECECDBFFEBEBDAFFE9E9
      D8FFF7F7F1FFF5F5EFFFFAFAF2FF2A2A187400000000000000000001111F242C
      9CD73B45CFFF303ABFF23E4BD7FFD8BC9AFFF6EAE1FF8C6D43BF2216053B0000
      000100000000000000000000000000000000858A88FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF858A88FFFFFFFFFFB6BDBAFFB6BDBAFFB6BDBAFFB6BDBAFFB6BD
      BAFFB6BDBAFFFFFFFFFF858A88FF000000000000000000000000002362CC3E71
      B6FF275A9FFF25589DFF25589DFF415D72FFFDFDFBFFC5C5B4FFC5C5B4FFD6D6
      C5FFD6D6C5FFCDCDBCFFF8F8ECFF2C2C1B71000000000000000000476B9B92DA
      F4FF8BD4F0FF8BD4F0FF92DAF4FF4B92B6FFCACAB9FFDADAC9FFC7C7B6FFD6D6
      C5FFD4D4C3FFCACAB9FFF9F9EFFF2B2B1A720000000000000000000000000001
      111F10166499212995CC090E5489957953CCF0E0D0FF8D7049C50603000B0000
      000000000000000000000000000000000000858A88FFFFFFFFFFB6BDBAFFB6BD
      BAFFB6BDBAFF858A88FFFFFFFFFFECEEEEFFECEEEEFFECEEEEFFECEEEEFFECEE
      EEFFECEEEEFFFFFFFFFF858A88FF000000000000000000000000002665CC477A
      BFFF3568ADFF2B5EA3FF24579CFF415E74FFFDFDF9FFEBEBDAFFEBEBDAFFF5F5
      EEFFF3F3EBFFEEEEE3FFF5F5E5FF2D2D1D6E000000000000000000466A9797DE
      F6FF90D8F2FF90D8F2FF97DEF6FF4E96B8FFEBEBDAFFE9E9D8FFE7E7D6FFE5E5
      D4FFE4E4D3FFE2E2D1FFF6F6E8FF2D2D1C700000000000000000000000000000
      000000000000000001020000000065503290F6EADDFFE1CDB4FF846945BF0000
      000000000000000000000000000000000000858A88FFFFFFFFFFECEEEEFFECEE
      EEFFECEEEEFF858A88FFFFFFFFFFB6BDBAFFB6BDBAFFB6BDBAFFB6BDBAFFB6BD
      BAFFB6BDBAFFFFFFFFFF858A88FF000000000000000000000000002869CC4C80
      C4FF3D70B5FF3D70B5FF396CB1FF4C6981FFFCFCF7FFCCCCBBFFD4D4C3FFF3F3
      EBFFA4A493FFA4A493FFA4A493FF2323117C000000000000000000476A949BE1
      F7FF94DBF4FF94DBF4FF9BE1F7FF5199BCFFC7C7B6FFD6D6C5FFC3C3B2FFC2C2
      B1FFC0C0AFFFBEBEADFFF4F4E3FF2E2E1D6E0000000000000000000000000000
      0000000000000000000000000000604B2E89F1E2D4FFC5AA88F4F5EBE0FF664D
      2D9D00000000000000000000000000000000858A88FFFFFFFFFFB6BDBAFFB6BD
      BAFFB6BDBAFF858A88FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF858A88FF000000000000000000000000002A6CCC5084
      C8FF4275BAFF4275BAFF4275BAFF57758DFFFBFBF5FFF5F5EEFFF3F3EBFFEEEE
      E3FFB6B6A5FFFFFFFFFF30301F6A11110B260000000000000000004669909EE5
      F9FF98DFF6FF98DFF6FF9EE5F9FF559CBFFFE7E7D6FFE5E5D4FFE4E4D3FFF0F0
      E6FFEBEBDDFFE7E7D6FFF2F2E1FF2F2F1E6C0000000000000000000000000000
      00000000000000000000000000005B472A84EEDFCEFF3B280C65A68B69DAE7D6
      C3FD513E257B000000000000000000000000858A88FFFFFFFFFFECEEEEFFECEE
      EEFFECEEEEFFB8BCBBFF858A88FF858A88FF858A88FF858A88FF858A88FF858A
      88FF858A88FF858A88FF4244437F000000000000000000000000002C6ECC5589
      CDFF477ABFFF477ABFFF477ABFFF5C7A93FFFDFDF6FFFAFAF2FFF8F8ECFFF4F4
      E5FFC2C2B1FF324C62E111110B250000000000000000000000000045698DA3E8
      FBFF9DE3F9FF9DE3F9FFA3E8FBFF579FC2FFC3C3B2FFD3D3C2FFC8C8B7FFEBEB
      DDFFA4A493FFA4A493FFA4A493FF2323117C0000000000000000000000000000
      00000000000000000000000000005541267CE7D5C1FF1910052B0F09011BB297
      77E7BCA68AE1453826620000000000000000858A88FFFFFFFFFFB6BDBAFFB6BD
      BAFFB6BDBAFFB6BDBAFFB6BDBAFFB6BDBAFFFFFFFFFF858A88FF000000000000
      0000000000000000000000000000000000000000000000000000002E71CC588C
      D0FF4C80C4FF3F72B7FF3F72B7FF507498FF58768FFF58768FFF58768FFF607E
      97FF66859DFF11396ED4000000000000000000000000000000000046698AA6EB
      FCFFA1E6FBFFA1E6FBFFA6EBFCFF61A8C9FFF4F4ECFFF0F0E6FFEBEBDDFFE7E7
      D6FFB6B6A5FFFFFFFFFF3131206911110B250000000000000000000000000000
      00000000000000000000000000004F3C2275DEC9AFFF120C031F00000000150E
      0326B39771EB90724ECB0000000000000000858A88FFFFFFFFFFECEEEEFFECEE
      EEFFECEEEEFFECEEEEFFECEEEEFFECEEEEFFFFFFFFFF858A88FF000000000000
      0000000000000000000000000000000000000000000000000000003073CC5B8F
      D3FF2B5EA3FF23569BFF295CA1FF2F62A7FF3164A9FF2C5FA4FF26599EFF2D60
      A5FF598DD1FF003073CC0000000000000000000000000000000000466887A9EE
      FDFFA4E9FCFFA4E9FCFFAAEEFDFF6CAFCDFFF8F8EEFFF6F6E8FFF3F3E3FFF2F2
      E1FFC2C2B1FF325B60B811110B25000000000000000000000000000000000000
      00000000000000000000000000004A381E6FCFB79AF709060110000000000000
      00001911052C86663BCB0000000000000000858A88FFFFFFFFFFB6BDBAFFB6BD
      BAFFB6BDBAFFB6BDBAFFB6BDBAFFB6BDBAFFFFFFFFFF858A88FF000000000000
      0000000000000000000000000000000000000000000000000000003275CC6195
      D9FF396CB1FFD2C7C7FFD8CFCFFFE4DEDEFFE4DEDEFFD8CFCFFFD2C7C7FF396C
      B1FF5F93D7FF003275CC0000000000000000000000000000000000456885ACF1
      FFFFABEFFEFF94E2F8FF6DC8EDFF4297B9FF8EB7BAFF8EB7BAFF8EB7BAFF8EB7
      BAFF93BCBBFF0F4D679600000000000000000000000000000000000000000000
      000000000000000000000000000045331B69B39A7AE005030009000000000000
      000000000000030200060000000000000000858A88FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF858A88FF000000000000
      0000000000000000000000000000000000000000000000000000002659990033
      77CC003377CC00276DD0626B92E6DCDADAFFDCDADAFF626B92E600276DD00033
      77CC003377CC00265999000000000000000000000000000000000045678388DB
      F4FF5FC1E9FF5EBFEAFF80D3F4FF9CE3FDFFA2E6FFFFA2E6FFFFA2E6FFFFA2E6
      FFFFA6EAFFFF0045678300000000000000000000000000000000000000000000
      00000000000000000000000000001D1407316B4F2AA102010005000000000000
      0000000000000000000000000000000000004244437F858A88FF858A88FF858A
      88FF858A88FF858A88FF858A88FF858A88FF858A88FF4244437F000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000102061A1A1A5221212167212121671A1A1A52000102060000
      000000000000000000000000000000000000000000000000000000334D610045
      6781004567810045678100456781004567810045678100456781004567810045
      67810045678100334D610000000000000000424D3E000000000000003E000000
      2800000040000000F00100000100010000000000800F00000000000000000000
      000000000000000000000000FFFFFF00E3FF000000000000C1FF000000000000
      80FF00000000000080FF000000000000E3FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000E3FF000000000000C1FF000000000000C1FF000000000000
      C1FF000000000000C1FF000000000000FFFFFFFFFFFFFFFFF01FFFFFF3FFF3FF
      E00FFFFFF0FFE3FFC007E00FF07FC3FF8003E00FF03F80038003E00FF01F0001
      8003E00FF00700018003E00FF00300018003E00FF00380018003E00FF007C301
      8003E00FF01FE381C007E00FF03FF3C1E00FE00FF07FFFC1F01FE00FF0FFFFC1
      FFFFFFFFF3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF801FFFFFFFF
      FFFFF801FFFFFFFFFFFFE001FFFFE007F81FE001FFFFE007F81F8001FFFFE007
      F81F8001FFF8E007FFFF80011100E007FFFF80010100E007FFFF80010102E007
      FFFF80010000E007FFFF8007E77FE007FFFF8007FF7FE007FFFF801FFFFFFFFF
      FFFF801FFFFFFFFFFFFFFFFFFFFFFFFFDFFFFFFFFFFFFFFF8FFFFFFFFFFFFF81
      8FFF0000FFE3FF999C0F0000FFE3FF999FFF0000FFE3FFFF9FFF0000FFE3FCFF
      9C030000FFE3E0FF9FFF0000FFE3E4FF9FFF0000C47FEFFF9C0F0000C47FFFFF
      9FFF0000C47FDA9F9FFF0000CCFFDAAF9C030000CCFFD89F8FFF0000C47FDAAF
      8FFF0000FFFF8D9FDFFF0000FFFFFFFFBFC1C003FFFFFFFF0000C003FFFFFFFF
      0000C003000000000000C003000000000000C003000000000000C00300000000
      0000C003000000000000C003000000000000C003000000000000C00300000000
      0000C003000000000000C003000000000000C003000000000000C00300000000
      0000C003000000000000C003FFFFFFFFC0038001F5578001C0038001EFFB8001
      C0038001F80F0001C0038001E80B0000C0038001F80F0000C0038001EE3B0000
      C0038001FE3F0000C0038001EE3B0000C0038001FE3F0000C0038001EE3B0000
      C0038001FE3F0000C0038001E80B0000C0038001F80F0001C0038001E80B8001
      C0038001FFFF8003C0038001EAAB8007FFFFFE7FE000C1FF8001FC3FE00080FF
      8001F81F0000007F8001F81F0000007F8001F00F0000007F8001D0090000007F
      800190080000007F800110080001801F800110080001C01F800190090001FC1F
      8001D00B0001FE078001FFFF0001FF078001F81F0001FF878001FC3F0001FFC1
      8001FE7F0001FFE1FFFFFE7FFFFFFFF1FFC18000FFFFFFFFFF808000FFFFFFFF
      FF888000FFFFFFFFFF9C8000FFFFFFFFFF888000FFFFFFFF80008000FFFF0000
      8001000000000000800100000000000080010000000000008001000000000000
      8001000000000000C0030000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFF
      FFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFFFC00FFFFF000FFFFFE00FFFFF000
      FFFF9200E007F000C4471000C001F000C44738008000C000C44710000000C000
      FFFF90000000C000C447FC000000C000C447F00000000003C447E00000000003
      FFFFC00000000003C447000080010003C4470000C003000FC4470000E007000F
      FFFF0000FFFF000FFFFF0000FFFF000FFFFFFFFFFFFF9C00804780019FF30C00
      804780018FE10400C047800181818000FC47C0010101C000FC47E0010101E000
      F847E00100010000E047800100010000C047000100010000C0470001101F0000
      C0470001000F0000C047000701010000C003004381810000C003040181C10000
      E00386118FF10000FFFFC71FFFFFC0008001F801FFFFFFFF80010001FFFFE001
      00010001FFFFE001000000018181E001000000010101E001000000010101E001
      000000010101E00100000001010180010000000101010001000000091F1F0001
      000000010F0F0001000000010101000F0001000781811C7F8001000FC1C1041F
      8003000FFFFF861F8007F0FFFFFFC71FFCFF800F800FE3FFFCFF000F000FE0F7
      F8FF000F000FFC63F01F000F000FFE47781E040F040FFE0F3C9C000F000FFE1F
      1C98000F000FFC3F0E90000F000FFC7F0FD0C003C00FF8FF1FF804030407F1FF
      3FFC00030000E3FF781E00010000E3FCF99F0000000093F8F99F8000800011F1
      FFFFFFE0FFC03801FFFFFFF0FFC0FC07FFFFFFFF8000F807FFFFF00F9C00F001
      0000E007900000010000C003000080000000800100008000000080010000C000
      000080018000E000000080010000800000008101000100000000800100010000
      000080010001000000008001000100010000C003800100010000E00780110003
      0000F00F80038013FFFFFFFF8007FFFF80018001FFFFFFFF000000000001FFFF
      00000000000100010000000000017FFD00000000000160050000000000014005
      0000000000014005000000000001000100000000000140050000000000014005
      00000000000140050000000000017FFD00000000000100010000000000010001
      00000000FFFF000180018001FFFFFFFF80018001800180010000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000800180018001800180018001800180010000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000008001800180018001FFFFFFFFFFFF8001FFFFFFE1FFE10000
      FFFFFFC1FFC10000FFFFFF81FF810000781EFF01FF010000399CF003F0030000
      1998C007C00700000FF0800F800F00000FF0801F801F00001FF8801F801F0000
      3FFC801F801F00007FFE801F801F0000FFFF803F803F0000FFFF803F803F0000
      FFFFC07FC07F0000FFFFFFFFFFFF8001F1E3FFFFFFFF8000F807FFFFF00F0000
      0000FFFFE00700000000FFFFC00300000000FFFF800100000000797F80010000
      0000789F800100000000781F800100000000001F800100000000001F80010000
      0000001F800100000000001F800100000000001FC00300000000001FE0070000
      0000001FF00F00000000001FFFFF0000FFFFFFFFFFFFFFFFFFFFFC3FFFFFF00F
      FFFFFC3FC003E007FFFFFC3FFFFFC003FFE7FC3FFE7F8001FFC7FC3FC0038001
      8003FC3FF00F80018001FC3FF00F80018001FC3FC00380018003FC3FFC3F8001
      FFE7FC1FFC3F8001FFE7F00FC0038001FFFFF00FFC3FC003FFFFFC3FFC3FE007
      FFFFFE7FC003F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE7FFFFF
      0000F007FC3FFFFF0000F00FF00FFFFF0000C003F00FE7FF0000C003F83FE3FF
      0000C003FC3FC0010000C003FC3F80010000C003FC3F80010000C003FC3FC001
      0000C003FC3FE7FFC7FFC003FC3FE7FFC7FFF00BFC3FFFFFC007F00FFC3FFFFF
      FFFFFFFFFC3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000003F8001FFFF0000
      003F8001FFFF0000003F800180030000003F800183030000003F8001C3870000
      003F8001E007000000388001F007000000008001F007000000008001F80F0000
      00008001FC0F000000008001FC07000000008001FC03000000008001FC010000
      F8008001FFFF0000FE30FFFFFFFF0000C000DFFFFFFB8001C0009FFFFFF98000
      C0009FFFFFF90000C0009FFFFFF90000C0008FBFFDF10000C0008F9FF9F10000
      0000878FF1E100000000C007E00300000000C003C0030000C000E00180070000
      C000F003C00F0000C000FC07E03F0000C000FF8FF1FF0001C001FF9FF9FF8001
      C003FFBFFDFF8003C007FFFFFFFF80078001EFFFE00780018001E3FF00000000
      0001E1FF000000000000F07F000000000000F83F000000000000F80F00000000
      0000000700000000000000010000000000000001000000000000803F00000000
      0000801F800100000000C00FE00700000000C00FE00700008000E007E0070000
      8003E003E00F00008007FFFFE01F0000EFFFFFFFFFFFFFFFCFFFFFFFC03FFFFF
      8003BFFFFFFFF7EF8003A001C003E3C7CFFFA001FFFFC183E820A001780FE007
      F820A001380FF00FF820A001180FF81FF820BFFF0FFFF81FF800A00F1803F00F
      F800A00F3803E007F800A00F7803C183FC00A00FFFFFE3C7FC21A00FC03FF7EF
      FE23BFFFFFFFFFFFFFFFFFFFC003FFFF9FFFF800FFF7FFFF0FFFF800FFF3936D
      03CFF801C001EFFF83C7FC21C001EFFFC1E3FC01FFF3EFFFE0E3F8000417EFFF
      E063FC10041FEFFFC023F810041FEC01C403F000041F9001E2030001001FFC21
      E30388FF001FFC01E383C0FF001FFC01E3C1E0FF003FFC01FFE1F0FF843FFC01
      FFF1F0FFC47FFE03FFFFF0FFFFFFFFFFE040FFF3FFFFC001E040FFE1FFFFC001
      F871FFC0FFFFC001FC01FFF30013C001FC1100130013C001FE0100130013C001
      F00100130013C001F80100110011C001F00100110011C001000100110011C001
      000100130013C00101FF00118011C00101FF801FC040C00101FFC07FFFE1C003
      01FFFFFFFFF3C00781FFFFFFFFFFC00FFFFFFFFF0000FFFFEFFFFFFF00008001
      C7FFF3CF00008001833FE3C700008001013FE3E300008001C7FFE3E300008001
      C70FE3E300008001C70FC7E300008001C7FFC7E300008001C703E3E300008001
      C703E3E300008001C7FFE3E300008001C700E3C700008001C700FFFF00008001
      C7FFFFFF00008001FFFFFFFF0000FFFFFFFFFFFF9807FFE3C03FFFFF9003FFE3
      FFFFFFFF8001FFE1C003FFFF8001FDF1FFFFFFFF8041F9E1E80FFFFF81C0E181
      C80FC00380F0C001880FC003800180010FFFC003BF0000018803C00383000001
      C803FFFF83800003E803FFFF80008007FFFFFFFF8000C00FC03FFFFF8000E1FF
      FFFFFFFFC008F1FFC003FFFFE01CFDFF0000FFFFF07F80010000FFFFF03F8001
      00008003F03F8001000080030007800100008003000780010000810300078001
      0000810302038001000000010200800100000001000080010000000100008001
      00000001000080010000C101000380010000F11F820380010000F11FE23F8001
      0000FFFFE23F80010000FFFFFFFF800180008001FFFF00000000800183FF0000
      0000000181FF00000000000080FF000000000000807F000000000000803F0000
      00000000E01F000000000000F00F000000000000F807000000000000FC030000
      00000000FE01000000000000FF00000000000000FF80000000008000FFC00000
      00008003FFC0000000008007FFFF0000FE0FF801C000C000E20FF801C000C000
      C007F801C000C000C007F801C000C000C807F801C000C000C00F0001C000C000
      C00F0001C000C000E01F0001C000C000FA1F0001C000C000FE0F0001C000C000
      FE070001C000C000FE03003FC000C000FE23003FC000C000FE33003FC000C000
      FE3B003FC000C000FE3F003FC000C00000000000000000000000000000000000
      000000000000}
  end
end
