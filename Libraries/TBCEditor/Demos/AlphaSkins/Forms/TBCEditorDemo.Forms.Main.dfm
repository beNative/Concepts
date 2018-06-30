inherited MainForm: TMainForm
  Caption = ''
  ClientHeight = 644
  ClientWidth = 1100
  Color = clWhite
  Position = poScreenCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TBCSplitter [0]
    Left = 754
    Top = 0
    Height = 625
    Align = alRight
    SkinData.SkinSection = 'SPLITTER'
  end
  inherited StatusBar: TBCStatusBar
    Top = 625
    Width = 1100
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
    ParentFont = True
    ParentShowHint = False
    ShowHint = True
    UseSystemFont = False
  end
  object PanelProperty: TBCPanel [2]
    Left = 760
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
    object ObjectInspector: TBCObjectInspector
      AlignWithMargins = True
      Left = 0
      Top = 5
      Width = 335
      Height = 615
      Margins.Left = 0
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      DefaultNodeHeight = 13
      DragOperations = []
      EditDelay = 0
      Header.AutoSizeIndex = 1
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoAutoResize, hoColumnResize, hoShowImages]
      IncrementalSearch = isAll
      Indent = 16
      Margin = 0
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoChangeScale]
      TreeOptions.MiscOptions = [toEditable, toFullRepaintOnResize, toGridExtensions, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowRoot, toShowVertGridLines, toThemeAware]
      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
      Columns = <
        item
          Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coFixed, coAllowFocus]
          Position = 0
          Width = 160
          WideText = 'Property'
        end
        item
          Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAllowFocus, coEditable]
          Position = 1
          Width = 171
          WideText = 'Value'
        end>
    end
  end
  object PanelLeft: TBCPanel [3]
    AlignWithMargins = True
    Left = 6
    Top = 0
    Width = 748
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
    object Editor: TBCEditor
      AlignWithMargins = True
      Left = 0
      Top = 5
      Width = 748
      Height = 593
      Cursor = crIBeam
      Margins.Left = 0
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 0
      ActiveLine.Indicator.Visible = False
      Align = alClient
      Caret.Options = []
      CodeFolding.Hint.Font.Charset = DEFAULT_CHARSET
      CodeFolding.Hint.Font.Color = clWindowText
      CodeFolding.Hint.Font.Height = -11
      CodeFolding.Hint.Font.Name = 'Courier New'
      CodeFolding.Hint.Font.Style = []
      CodeFolding.Hint.Indicator.Glyph.Visible = False
      CompletionProposal.CloseChars = '()[]. '
      CompletionProposal.Columns = <
        item
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          Items = <>
          Title.Font.Charset = DEFAULT_CHARSET
          Title.Font.Color = clWindowText
          Title.Font.Height = -12
          Title.Font.Name = 'Courier New'
          Title.Font.Style = []
        end>
      CompletionProposal.Options = [cpoAutoConstraints, cpoAddHighlighterKeywords, cpoFiltered, cpoParseItemsFromText, cpoResizeable]
      CompletionProposal.SecondaryShortCut = 0
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
      LineSpacing = 0
      MatchingPair.Enabled = True
      Minimap.Font.Charset = DEFAULT_CHARSET
      Minimap.Font.Color = clWindowText
      Minimap.Font.Height = -4
      Minimap.Font.Name = 'Courier New'
      Minimap.Font.Style = []
      Minimap.Options = [moShowBookmarks]
      OnCaretChanged = EditorCaretChanged
      ParentCtl3D = False
      Search.Enabled = False
      Search.InSelection.Background = clBlack
      SpecialChars.EndOfLine.Visible = True
      SpecialChars.Selection.Visible = True
      SpecialChars.Style = scsDot
      SyncEdit.ShortCut = 24650
      TabOrder = 0
      TokenInfo.Enabled = True
      TokenInfo.Font.Charset = DEFAULT_CHARSET
      TokenInfo.Font.Color = clWindowText
      TokenInfo.Font.Height = -11
      TokenInfo.Font.Name = 'Courier New'
      TokenInfo.Font.Style = []
      TokenInfo.Title.Font.Charset = DEFAULT_CHARSET
      TokenInfo.Title.Font.Color = clWindowText
      TokenInfo.Title.Font.Height = -11
      TokenInfo.Title.Font.Name = 'Courier New'
      TokenInfo.Title.Font.Style = []
      WordWrap.Indicator.Bitmap.Data = {
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
    end
    object PanelSearch: TBCPanel
      AlignWithMargins = True
      Left = 0
      Top = 601
      Width = 748
      Height = 21
      Margins.Left = 0
      Margins.Right = 0
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      SkinData.SkinSection = 'CHECKBOX'
      object SplitterSearch: TBCSplitter
        Left = 227
        Top = 0
        Height = 21
        SkinData.SkinSection = 'SPLITTER'
      end
      object SpeedButtonFindPrevious: TBCSpeedButton
        Left = 233
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
        Left = 254
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
      object SpeedButtonSearchDivider1: TBCSpeedButton
        AlignWithMargins = True
        Left = 296
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
        Left = 358
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
        Left = 727
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
      object SpeedButtonCaseSensitive: TBCSpeedButton
        Left = 306
        Top = 0
        Width = 21
        Height = 21
        Action = ActionCaseSensitive
        Align = alLeft
        AllowAllUp = True
        GroupIndex = 1
        Flat = True
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageListSmall
        ImageIndex = 144
      end
      object SpeedButtonInSelection: TBCSpeedButton
        Left = 327
        Top = 0
        Width = 21
        Height = 21
        Action = ActionInSelection
        Align = alLeft
        AllowAllUp = True
        GroupIndex = 2
        Flat = True
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageListSmall
        ImageIndex = 145
      end
      object SpeedButtonSearchDivider2: TBCSpeedButton
        AlignWithMargins = True
        Left = 348
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
      object SpeedButtonSearchEngine: TBCSpeedButton
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 21
        Height = 21
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 6
        Margins.Bottom = 0
        Action = ActionSearchEngine
        Align = alLeft
        Flat = True
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageListSmall
        ImageIndex = 143
      end
      object SpeedButtonFindAll: TBCSpeedButton
        Left = 275
        Top = 0
        Width = 21
        Height = 21
        Action = ActionFindAll
        Align = alLeft
        Flat = True
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageListSmall
        ImageIndex = 151
      end
      object ComboBoxSearchText: TBCComboBox
        Left = 27
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
        Left = 379
        Top = 0
        Width = 348
        Height = 21
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        SkinData.SkinSection = 'CHECKBOX'
        object LabelSearchResultCount: TsLabel
          AlignWithMargins = True
          Left = 342
          Top = 0
          Width = 3
          Height = 13
          Margins.Left = 0
          Margins.Top = 0
          Margins.Bottom = 0
          Align = alRight
          Layout = tlCenter
        end
      end
    end
  end
  inherited SkinManager: TBCSkinManager
    Effects.AllowAnimation = False
    Effects.AllowAeroBluring = False
    Effects.AllowGlowing = False
    IsDefault = True
    MenuSupport.UseExtraLine = False
    Options.ChangeSysColors = True
    SkinInfo = 'N/A'
    ThirdParty.ThirdEdits = ' '#13#10'TBCEditorPrintPreview'#13#10
    ThirdParty.ThirdVirtualTrees = 'TBCObjectInspector'#13#10'TVirtualDrawTree'#13#10
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
        Width = 48
        Index = 0
        Name = 'TitleBarItemFile'
        ShowHint = True
        Style = bsMenu
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
        Width = 161
        Index = 1
        Name = 'TitleBarItemCaption'
        ShowHint = False
        Style = bsInfo
      end
      item
        Align = tbaRight
        Caption = 'ANSI'
        DropdownMenu = PopupMenuDummy
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 22
        Width = 56
        Index = 2
        Name = 'TitleBarItemEncoding'
        ShowHint = False
        Style = bsMenu
        OnClick = TitleBarItems2Click
      end
      item
        Align = tbaRight
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Width = 6
        Index = 3
        Name = 'TitleBarItemSpacing1'
        ShowHint = False
        Style = bsSpacing
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
        Width = 101
        Index = 4
        Name = 'TitleBarItemHighlighter'
        ShowHint = False
        Style = bsMenu
        OnClick = TitleBarItems4Click
      end
      item
        Align = tbaRight
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Width = 6
        Index = 5
        Name = 'TitleBarItemSpacing2'
        ShowHint = False
        Style = bsSpacing
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
        Width = 68
        Index = 6
        Name = 'TitleBarItemColors'
        ShowHint = False
        Style = bsMenu
        OnClick = TitleBarItems6Click
      end
      item
        Align = tbaRight
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Width = 2
        Index = 7
        Name = 'TitleBarItemSpacing3'
        ShowHint = False
        Style = bsSpacing
      end>
    Left = 88
    Top = 22
  end
  inherited SkinProvider: TsSkinProvider
    AllowAnimation = False
    Left = 248
    Top = 26
  end
  inherited ApplicationEvents: TApplicationEvents
    OnMessage = ApplicationEventsMessage
    Left = 88
    Top = 88
  end
  inherited ActionList: TActionList
    Images = ImagesDataModule.ImageListSmall
    Left = 164
    Top = 88
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
    object ActionFindAll: TAction
      Hint = 'Find all'
      ImageIndex = 151
      OnExecute = ActionFindAllExecute
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
    object ActionSearchEngine: TAction
      Hint = 'Select search engine'
      ImageIndex = 143
      OnExecute = ActionSearchEngineExecute
    end
    object ActionCaseSensitive: TAction
      GroupIndex = 1
      Hint = 'Case sensitive'
      ImageIndex = 144
      OnExecute = ActionCaseSensitiveExecute
    end
    object ActionInSelection: TAction
      GroupIndex = 2
      Hint = 'In selection'
      ImageIndex = 145
      OnExecute = ActionInSelectionExecute
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
    Left = 344
    Top = 114
  end
  object OpenDialog: TsOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 336
    Top = 34
  end
end
