object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TBCEditor Demo'
  ClientHeight = 596
  ClientWidth = 914
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterVertical: TSplitter
    Left = 207
    Top = 0
    Height = 596
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 207
    Height = 596
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object SplitterHorizontal: TSplitter
      Left = 0
      Top = 466
      Width = 207
      Height = 3
      Cursor = crVSplit
      Align = alBottom
    end
    object ListBoxColors: TListBox
      AlignWithMargins = True
      Left = 3
      Top = 469
      Width = 204
      Height = 124
      Margins.Top = 0
      Margins.Right = 0
      Align = alBottom
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBoxColorsClick
    end
    object ListBoxHighlighters: TListBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 204
      Height = 463
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      ItemHeight = 13
      TabOrder = 1
      OnClick = ListBoxHighlightersClick
    end
  end
  object Editor: TBCEditor
    AlignWithMargins = True
    Left = 210
    Top = 3
    Width = 701
    Height = 590
    Cursor = crIBeam
    Margins.Left = 0
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
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Items = <>
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -11
        Title.Font.Name = 'Courier New'
        Title.Font.Style = []
      end>
    CompletionProposal.SecondaryShortCut = 0
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
    LineSpacing = 0
    MatchingPair.Enabled = True
    Minimap.Font.Charset = DEFAULT_CHARSET
    Minimap.Font.Color = clWindowText
    Minimap.Font.Height = -1
    Minimap.Font.Name = 'Courier New'
    Minimap.Font.Style = []
    SpecialChars.Style = scsDot
    SyncEdit.ShortCut = 24650
    TabOrder = 1
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
end
