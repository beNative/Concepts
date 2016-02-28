inherited MainForm: TMainForm
  Caption = ''
  ClientHeight = 644
  ClientWidth = 1348
  Color = clWhite
  Position = poScreenCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TBCSplitter [0]
    Left = 1002
    Top = 0
    Height = 625
    Align = alRight
    SkinData.SkinSection = 'SPLITTER'
  end
  inherited StatusBar: TBCStatusBar
    Top = 625
    Width = 1348
    Panels = <
      item
        Alignment = taCenter
        Width = 86
      end
      item
        Width = 86
      end
      item
        Width = 86
      end
      item
        Width = 50
      end>
  end
  object PanelProperty: TBCPanel [2]
    Left = 1008
    Top = 0
    Width = 340
    Height = 625
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alRight
    BevelOuter = bvNone
    DoubleBuffered = False
    ParentColor = True
    ParentDoubleBuffered = False
    TabOrder = 1
    SkinData.SkinSection = 'TRANSPARENT'
    object ObjectInspectorEh: TObjectInspectorEh
      AlignWithMargins = True
      Left = 0
      Top = 5
      Width = 334
      Height = 615
      Margins.Left = 0
      Margins.Top = 5
      Margins.Right = 6
      Margins.Bottom = 5
      Align = alClient
      Options = [goVertLineEh, goEditingEh, goAlwaysShowEditorEh]
    end
  end
  object PanelLeft: TBCPanel [3]
    AlignWithMargins = True
    Left = 6
    Top = 0
    Width = 996
    Height = 625
    Margins.Left = 6
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 2
    SkinData.SkinSection = 'TRANSPARENT'
    object SplitterUndo: TBCSplitter
      Left = 593
      Top = 0
      Height = 598
      Align = alRight
      Visible = False
      SkinData.SkinSection = 'SPLITTER'
    end
    object Editor: TBCEditor
      AlignWithMargins = True
      Left = 0
      Top = 5
      Width = 593
      Height = 593
      Cursor = crIBeam
      Margins.Left = 0
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 0
      ActiveLine.Indicator.Visible = False
      Align = alClient
      Caret.NonBlinking.Enabled = False
      Caret.Options = []
      CodeFolding.Colors.Indent = clBlack
      CodeFolding.Hint.Font.Charset = DEFAULT_CHARSET
      CodeFolding.Hint.Font.Color = clWindowText
      CodeFolding.Hint.Font.Height = -11
      CodeFolding.Hint.Font.Name = 'Courier New'
      CodeFolding.Hint.Font.Style = []
      CodeFolding.Visible = True
      CompletionProposal.CloseChars = '()[]. '
      CompletionProposal.Columns = <
        item
        end>
      CompletionProposal.Font.Charset = DEFAULT_CHARSET
      CompletionProposal.Font.Color = clWindowText
      CompletionProposal.Font.Height = -11
      CompletionProposal.Font.Name = 'Courier New'
      CompletionProposal.Font.Style = []
      CompletionProposal.ShortCut = 16416
      CompletionProposal.Trigger.Chars = '.'
      CompletionProposal.Trigger.Enabled = False
      Ctl3D = True
      Directories.Colors = 'Colors'
      Directories.Highlighters = 'Highlighters'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      LeftMargin.Font.Charset = DEFAULT_CHARSET
      LeftMargin.Font.Color = 13408665
      LeftMargin.Font.Height = -11
      LeftMargin.Font.Name = 'Courier New'
      LeftMargin.Font.Style = []
      LeftMargin.Width = 55
      Lines.Strings = (
        '')
      MatchingPair.Enabled = True
      Minimap.Font.Charset = DEFAULT_CHARSET
      Minimap.Font.Color = clWindowText
      Minimap.Font.Height = -4
      Minimap.Font.Name = 'Courier New'
      Minimap.Font.Style = []
      Minimap.Width = 140
      OnCaretChanged = EditorCaretChanged
      ParentCtl3D = False
      RightMargin.Position = 80
      RightMargin.Visible = True
      Search.Enabled = False
      SpecialChars.Style = scsDot
      SyncEdit.ShortCut = 24650
      TabOrder = 0
      WordWrap.Enabled = False
      WordWrap.Indicator.Glyph.Data = {
        7E030000424D7E0300000000000036000000280000000F0000000E0000000100
        2000000000004803000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
        000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0080000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
        0000FF00FF00FF00FF00FF00FF00FF00FF008000000080000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF008000000080000000800000008000000080000000FF00
        FF00FF00FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF008000000080000000FF00FF00FF00FF0080000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF0080000000FF00FF00FF00FF0080000000FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF0080000000FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF0080000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00800000008000000080000000800000008000
        00008000000080000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00}
      WordWrap.Indicator.MaskColor = clFuchsia
      WordWrap.Position = 80
      WordWrap.Style = wwsClientWidth
    end
    object PanelSearch: TBCPanel
      AlignWithMargins = True
      Left = 0
      Top = 601
      Width = 996
      Height = 21
      Margins.Left = 0
      Margins.Right = 0
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      SkinData.SkinSection = 'CHECKBOX'
      object BCSplitter1: TBCSplitter
        Left = 200
        Top = 0
        Height = 21
        SkinData.SkinSection = 'SPLITTER'
      end
      object SpeedButtonFindPrevious: TBCSpeedButton
        Left = 206
        Top = 0
        Width = 21
        Height = 21
        Action = ActionFindPrevious
        Align = alLeft
        Flat = True
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageListSmall
        ImageIndex = 38
      end
      object SpeedButtonFindNext: TBCSpeedButton
        Left = 227
        Top = 0
        Width = 21
        Height = 21
        Action = ActionFindNext
        Align = alLeft
        Flat = True
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageListSmall
        ImageIndex = 37
      end
      object SpeedButtonDivider: TBCSpeedButton
        AlignWithMargins = True
        Left = 248
        Top = 1
        Width = 10
        Height = 19
        Margins.Left = 0
        Margins.Top = 1
        Margins.Right = 0
        Margins.Bottom = 1
        Align = alLeft
        Flat = True
        ButtonStyle = tbsDivider
        SkinData.SkinSection = 'TOOLBUTTON'
        ImageIndex = 3
      end
      object SpeedButtonOptions: TBCSpeedButton
        Left = 258
        Top = 0
        Width = 21
        Height = 21
        Action = ActionOptions
        Align = alLeft
        Flat = True
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageListSmall
        ImageIndex = 78
      end
      object SpeedButtonClose: TBCSpeedButton
        Left = 975
        Top = 0
        Width = 21
        Height = 21
        Action = ActionClose
        Align = alRight
        Flat = True
        Glyph.Data = {
          36060000424D3606000000000000360000002800000020000000100000000100
          18000000000000060000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FF5B5B5B5B5B5BFF00FFFF00FFFF00FF5B5B5B5B5B5BFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171717171FF00FFFF
          00FFFF00FF717171717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FF5B5B5B5B5B5B5B5B5B5B5B5BFF00FF5B5B5B5B5B5B5B5B5B5B5B5BFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFFFFFF717171FF
          00FF717171FFFFFFFFFFFF717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FF5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5BFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFFFFFFFFFFFF71
          7171FFFFFFFFFFFFFFFFFF717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FF5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5BFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF787878FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FF5B5B5B5B5B5B5B5B5B5B5B5B5B5B5BFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFF
          FFFFFFFFFF717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FF5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5BFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FF5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5BFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFFFFFFFFFFFF71
          7171FFFFFFFFFFFFFFFFFF717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FF5B5B5B5B5B5B5B5B5B5B5B5BFF00FF5B5B5B5B5B5B5B5B5B5B5B5BFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFFFFFF717171FF
          00FF717171FFFFFFFFFFFF717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FF5B5B5B5B5B5BFF00FFFF00FFFF00FF5B5B5B5B5B5BFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171717171FF00FFFF
          00FFFF00FF717171717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        Margin = 0
        NumGlyphs = 2
        SkinData.SkinSection = 'CHECKBOX'
        Images = ImagesDataModule.ImageListSmall
      end
      object ComboBoxSearchText: TBCComboBox
        Left = 0
        Top = 0
        Width = 200
        Height = 21
        Hint = 'Search text'
        Align = alLeft
        Alignment = taLeftJustify
        BoundLabel.Indent = 4
        BoundLabel.Layout = sclTopLeft
        SkinData.SkinSection = 'COMBOBOX'
        VerticalAlignment = taAlignTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemIndex = -1
        ParentFont = False
        TabOrder = 0
        OnChange = ComboBoxSearchTextChange
        OnKeyPress = ComboBoxSearchTextKeyPress
        UseMouseWheel = False
      end
      object PanelRight: TBCPanel
        Left = 279
        Top = 0
        Width = 696
        Height = 21
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        SkinData.SkinSection = 'CHECKBOX'
        object LabelSearchResultCount: TsLabel
          AlignWithMargins = True
          Left = 689
          Top = 0
          Width = 4
          Height = 21
          Margins.Left = 0
          Margins.Top = 0
          Margins.Bottom = 0
          Align = alRight
          ParentFont = False
          Layout = tlCenter
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
        end
      end
    end
    object PanelUndo: TBCPanel
      Left = 599
      Top = 0
      Width = 397
      Height = 598
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      Visible = False
      SkinData.SkinSection = 'CHECKBOX'
      object BCSplitter3: TBCSplitter
        Left = 0
        Top = 293
        Width = 397
        Height = 6
        Cursor = crVSplit
        Align = alTop
        Color = clBtnFace
        ParentColor = False
        SkinData.SkinSection = 'SPLITTER'
      end
      object MemoRedo: TsMemo
        AlignWithMargins = True
        Left = 0
        Top = 299
        Width = 397
        Height = 294
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 5
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
      object MemoUndo: TsMemo
        AlignWithMargins = True
        Left = 0
        Top = 5
        Width = 397
        Height = 288
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
    end
  end
  inherited SkinManager: TBCSkinManager
    Effects.AllowGlowing = False
    IsDefault = True
    MenuSupport.UseExtraLine = False
    ThirdParty.ThirdEdits = ' '#13#10'TBCEditorPrintPreview'#13#10
    ThirdParty.ThirdVirtualTrees = 'TVirtualDrawTree'#13#10
    Left = 166
    Top = 26
  end
  inherited TitleBar: TBCTitleBar
    Items = <
      item
        Caption = 'File'
        DropdownMenu = PopupMenuFile
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 22
        Index = 0
        Name = 'TitleBarItemFile'
        ShowHint = True
        Style = bsMenu
        Width = 48
      end
      item
        Align = tbaCenterInSpace
        Caption = 'TBCEditor Control Demo v1.1'
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 21
        Index = 1
        Name = 'TitleBarItemCaption'
        ShowHint = False
        Style = bsInfo
        Width = 161
      end
      item
        Align = tbaRight
        Caption = 'Object Pascal'
        DropdownMenu = PopupMenuDummy
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 22
        Index = 2
        Name = 'TitleBarItemHighlighter'
        ShowHint = False
        Style = bsMenu
        Width = 101
        OnClick = TitleBarItems2Click
      end
      item
        Align = tbaRight
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Index = 3
        Name = 'TitleBarItemSpacing2'
        ShowHint = False
        Style = bsSpacing
        Width = 6
      end
      item
        Align = tbaRight
        Caption = 'Default'
        DropdownMenu = PopupMenuDummy
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 22
        Index = 4
        Name = 'TitleBarItemColors'
        ShowHint = False
        Style = bsMenu
        Width = 68
        OnClick = TitleBarItems4Click
      end
      item
        Align = tbaRight
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Index = 5
        Name = 'TitleBarItemSpacing3'
        ShowHint = False
        Style = bsSpacing
        Width = 2
      end>
    Left = 88
    Top = 22
  end
  inherited SkinProvider: TBCSkinProvider
    Left = 250
    Top = 30
  end
  inherited ApplicationEvents: TApplicationEvents
    OnMessage = ApplicationEventsMessage
    Left = 88
    Top = 88
  end
  inherited ActionList: TActionList
    Images = ImagesDataModule.ImageListSmall
    Left = 178
    Top = 92
    object ActionSearch: TAction
      Caption = 'ActionSearch'
      ShortCut = 16454
      OnExecute = ActionSearchExecute
    end
    object ActionFileOpen: TAction
      Caption = 'Open...'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = ActionFileOpenExecute
    end
    object ActionPreview: TAction
      Caption = 'Print preview...'
      ImageIndex = 10
      OnExecute = ActionPreviewExecute
    end
    object ActionSkins: TAction
      Caption = 'Skins...'
      ImageIndex = 76
      OnExecute = ActionSkinsExecute
    end
    object ActionFindNext: TAction
      Hint = 'Find next'
      ImageIndex = 37
      ShortCut = 114
      OnExecute = ActionFindNextExecute
    end
    object ActionFindPrevious: TAction
      Hint = 'Find previous'
      ImageIndex = 38
      ShortCut = 8306
      OnExecute = ActionFindPreviousExecute
    end
    object ActionOptions: TAction
      Hint = 'Options'
      ImageIndex = 78
      OnExecute = ActionOptionsExecute
    end
    object ActionClose: TAction
      Hint = 'Close'
      OnExecute = ActionCloseExecute
    end
  end
  inherited MainMenu: TMainMenu
    Left = 81
    Top = 282
  end
  object PopupMenuFile: TPopupMenu
    Images = ImagesDataModule.ImageList
    Left = 84
    Top = 164
    object MenuItemFileOpen: TMenuItem
      Action = ActionFileOpen
      RadioItem = True
    end
    object MenuItemSeparator1: TMenuItem
      Caption = '-'
    end
    object MenuItemPrintPreview: TMenuItem
      Action = ActionPreview
      RadioItem = True
    end
    object MenuItemSkins: TMenuItem
      Action = ActionSkins
    end
    object MenuItemSeparator2: TMenuItem
      Caption = '-'
    end
    object MenuItemExit: TMenuItem
      Action = ActionFileExit
    end
  end
  object PopupMenuDummy: TPopupMenu
    Left = 84
    Top = 220
  end
  object MultiStringHolderFileTypes: TBCMultiStringHolder
    MultipleStrings = <
      item
        Name = 'Assembler (68HC11)'
        Strings.Strings = (
          '.asm')
      end
      item
        Name = 'AutoIt v3'
        Strings.Strings = (
          '.au3')
      end
      item
        Name = 'AWK'
        Strings.Strings = (
          '.awk')
      end
      item
        Name = 'C#'
        Strings.Strings = (
          '.cs')
      end
      item
        Name = 'C++'
        Strings.Strings = (
          '.c;.cpp;.h;.hpp')
      end
      item
        Name = 'CSS'
        Strings.Strings = (
          '.css')
      end
      item
        Name = 'Delphi Form Module'
        Strings.Strings = (
          '.dfm')
      end
      item
        Name = 'HTML with Scripts'
        Strings.Strings = (
          '.htm;.html')
      end
      item
        Name = 'Java'
        Strings.Strings = (
          '.java')
      end
      item
        Name = 'JavaScript'
        Strings.Strings = (
          '.js')
      end
      item
        Name = 'JSON'
        Strings.Strings = (
          '.json')
      end
      item
        Name = 'MS-DOS Batch'
        Strings.Strings = (
          '.bat')
      end
      item
        Name = 'Object Pascal'
        Strings.Strings = (
          '.pas;.dpr')
      end
      item
        Name = 'Perl'
        Strings.Strings = (
          '.pl')
      end
      item
        Name = 'PHP'
        Strings.Strings = (
          '.php')
      end
      item
        Name = 'Python'
        Strings.Strings = (
          '.py')
      end
      item
        Name = 'SQL (Standard)'
        Strings.Strings = (
          '.sql')
      end
      item
        Name = 'Visual Basic'
        Strings.Strings = (
          '.vb')
      end
      item
        Name = 'XML'
        Strings.Strings = (
          '.xml')
      end>
    Left = 324
    Top = 108
  end
  object OpenDialog: TsOpenDialog
    Left = 342
    Top = 52
  end
end
