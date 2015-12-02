inherited MainForm: TMainForm
  Caption = 'TBCEditor Control Version 1.0b - Property Demo'
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
      RightMargin.Position = 80
      RightMargin.Visible = True
      Search.Enabled = False
      SpecialChars.Style = scsDot
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
    object PanelSearchFrame: TBCPanel
      Left = 0
      Top = 598
      Width = 748
      Height = 27
      Align = alBottom
      AutoSize = True
      BevelOuter = bvNone
      Padding.Top = 3
      Padding.Bottom = 3
      TabOrder = 1
      SkinData.SkinSection = 'CHECKBOX'
      inline SearchFrame: TBCSearchFrame
        Left = 0
        Top = 3
        Width = 748
        Height = 21
        Align = alBottom
        Color = clWindow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentBackground = False
        ParentColor = False
        ParentFont = False
        TabOrder = 0
        inherited SearchPanel: TBCPanel
          Width = 748
          inherited SpeedButtonSearchClose: TBCSpeedButton
            Left = 727
            Images = nil
          end
          inherited PanelRight: TBCPanel
            Width = 394
            inherited LabelSearchResultCount: TBCLabelFX
              Left = 382
              Height = 21
            end
          end
          inherited PanelToolbar: TBCPanel
            inherited SpeedButtonFindPrevious: TBCSpeedButton
              Images = nil
            end
            inherited SpeedButtonFindNext: TBCSpeedButton
              Images = nil
            end
            inherited SpeedButtonOptions: TBCSpeedButton
              Images = nil
            end
          end
        end
        inherited ActionList: TActionList
          Images = nil
        end
      end
    end
  end
  inherited SkinManager: TBCSkinManager
    ExtendedBorders = True
    ThirdParty.ThirdEdits = ' '#13#10'TBCEditorPrintPreview'#13#10
    OnGetMenuExtraLineData = SkinManagerGetMenuExtraLineData
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
        Height = 21
        Index = 0
        Name = 'TitleBarItemFile'
        ShowHint = True
        Style = bsMenu
        Width = 47
      end
      item
        Align = tbaCenterInSpace
        Caption = 'TBCEditor Control Demo v1.0b'
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 20
        Index = 1
        Name = 'TitleBarItemCaption'
        ShowHint = False
        Style = bsInfo
        Width = 174
      end
      item
        Align = tbaRight
        Caption = 'Object Pascal'
        DropdownMenu = PopupMenuHighlighters
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 21
        Index = 2
        Name = 'TitleBarItemHighlighter'
        ShowHint = False
        Style = bsMenu
        Width = 103
        OnMouseDown = TitleBarItems2MouseDown
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
        DropdownMenu = PopupMenuColors
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 21
        Index = 4
        Name = 'TitleBarItemColors'
        ShowHint = False
        Style = bsMenu
        Width = 69
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
    Top = 26
  end
  inherited ApplicationEvents: TApplicationEvents
    OnMessage = ApplicationEventsMessage
    Left = 88
    Top = 88
  end
  inherited ActionList: TActionList
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
  object PopupMenuHighlighters: TPopupMenu
    Left = 84
    Top = 220
  end
  object PopupMenuColors: TPopupMenu
    Left = 84
    Top = 276
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
