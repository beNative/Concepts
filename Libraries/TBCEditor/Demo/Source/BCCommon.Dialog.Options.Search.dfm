inherited SearchOptionsDialog: TSearchOptionsDialog
  Caption = 'Search options'
  ClientHeight = 329
  ClientWidth = 253
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TBCPanel
    Left = 0
    Top = 0
    Width = 253
    Height = 297
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    SkinData.SkinSection = 'CHECKBOX'
    object StickyLabelBeepIfSearchStringNotFound: TsStickyLabel
      Left = 8
      Top = 12
      Width = 178
      Height = 13
      AutoSize = False
      Caption = 'Beep if search string not found'
      AttachTo = SliderBeepIfSearchStringNotFound
      Gap = 8
    end
    object StickyLabelCaseSensitive: TsStickyLabel
      Left = 8
      Top = 35
      Width = 178
      Height = 13
      AutoSize = False
      Caption = 'Case sensitive'
      AttachTo = SliderCaseSensitive
      Gap = 8
    end
    object StickyLabelWholeWordsOnly: TsStickyLabel
      Left = 8
      Top = 242
      Width = 178
      Height = 13
      AutoSize = False
      Caption = 'Whole words only'
      AttachTo = SliderWholeWordsOnly
      Gap = 8
    end
    object StickyLabelRegularExpression: TsStickyLabel
      Left = 8
      Top = 104
      Width = 178
      Height = 13
      AutoSize = False
      Caption = 'Regular expression'
      AttachTo = SliderRegularExpression
      Gap = 8
    end
    object StickyLabelWildCard: TsStickyLabel
      Left = 8
      Top = 219
      Width = 178
      Height = 13
      AutoSize = False
      Caption = 'Wildcard'
      AttachTo = SliderWildCard
      Gap = 8
    end
    object StickyLabelSearchOnTyping: TsStickyLabel
      Left = 8
      Top = 127
      Width = 178
      Height = 13
      AutoSize = False
      Caption = 'Search on typing'
      AttachTo = SliderSearchOnTyping
      Gap = 8
    end
    object StickyLabelEntireScope: TsStickyLabel
      Left = 8
      Top = 58
      Width = 178
      Height = 13
      AutoSize = False
      Caption = 'Entire scope'
      AttachTo = SliderEntireScope
      Gap = 8
    end
    object StickyLabelHighlightResult: TsStickyLabel
      Left = 8
      Top = 81
      Width = 178
      Height = 13
      AutoSize = False
      Caption = 'Highlight results'
      AttachTo = SliderHighlightResult
      Gap = 8
    end
    object StickyLabelSelectedOnly: TsStickyLabel
      Left = 8
      Top = 150
      Width = 178
      Height = 13
      AutoSize = False
      Caption = 'Selected only'
      AttachTo = SliderSelectedOnly
      Gap = 8
    end
    object StickyLabelShowSearchStringNotFound: TsStickyLabel
      Left = 8
      Top = 196
      Width = 178
      Height = 13
      AutoSize = False
      Caption = 'Show search string not found'
      AttachTo = SliderShowSearchStringNotFound
      Gap = 8
    end
    object StickyLabelShowSearchMatchNotFound: TsStickyLabel
      Left = 8
      Top = 173
      Width = 178
      Height = 13
      AutoSize = False
      Caption = 'Show search match not found'
      AttachTo = SliderShowSearchMatchNotFound
      Gap = 8
    end
    object StickyLabelWrapAround: TsStickyLabel
      Left = 8
      Top = 265
      Width = 178
      Height = 13
      AutoSize = False
      Caption = 'Wrap around'
      AttachTo = SliderWrapAround
      Gap = 8
    end
    object SliderBeepIfSearchStringNotFound: TsSlider
      Left = 194
      Top = 8
      Width = 50
      AutoSize = True
      TabOrder = 0
      BoundLabel.Indent = 6
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
    end
    object SliderCaseSensitive: TsSlider
      Left = 194
      Top = 31
      Width = 50
      AutoSize = True
      TabOrder = 1
      BoundLabel.Indent = 6
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
      SliderOn = False
    end
    object SliderEntireScope: TsSlider
      Left = 194
      Top = 54
      Width = 50
      AutoSize = True
      TabOrder = 2
      BoundLabel.Indent = 6
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
      SliderOn = False
    end
    object SliderHighlightResult: TsSlider
      Left = 194
      Top = 77
      Width = 50
      AutoSize = True
      TabOrder = 3
      BoundLabel.Indent = 6
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
      SliderOn = False
    end
    object SliderRegularExpression: TsSlider
      Left = 194
      Top = 100
      Width = 50
      AutoSize = True
      TabOrder = 4
      BoundLabel.Indent = 6
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
      SliderOn = False
    end
    object SliderSearchOnTyping: TsSlider
      Left = 194
      Top = 123
      Width = 50
      AutoSize = True
      TabOrder = 5
      BoundLabel.Indent = 6
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
      SliderOn = False
    end
    object SliderSelectedOnly: TsSlider
      Left = 194
      Top = 146
      Width = 50
      AutoSize = True
      TabOrder = 6
      BoundLabel.Indent = 6
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
      SliderOn = False
    end
    object SliderShowSearchStringNotFound: TsSlider
      Left = 194
      Top = 192
      Width = 50
      AutoSize = True
      TabOrder = 8
      BoundLabel.Indent = 6
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
      SliderOn = False
    end
    object SliderWildCard: TsSlider
      Left = 194
      Top = 215
      Width = 50
      AutoSize = True
      TabOrder = 9
      BoundLabel.Indent = 6
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
      SliderOn = False
    end
    object SliderWholeWordsOnly: TsSlider
      Left = 194
      Top = 238
      Width = 50
      AutoSize = True
      TabOrder = 10
      BoundLabel.Indent = 6
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
      SliderOn = False
    end
    object SliderShowSearchMatchNotFound: TsSlider
      Left = 194
      Top = 169
      Width = 50
      AutoSize = True
      TabOrder = 7
      BoundLabel.Indent = 6
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
      SliderOn = False
    end
    object SliderWrapAround: TsSlider
      Left = 194
      Top = 261
      Width = 50
      AutoSize = True
      TabOrder = 11
      BoundLabel.Indent = 6
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
      SliderOn = False
    end
  end
  object PanelButton: TBCPanel
    Left = 0
    Top = 297
    Width = 253
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 1
    SkinData.SkinSection = 'CHECKBOX'
    object ButtonOK: TBCButton
      Left = 90
      Top = 0
      Width = 75
      Height = 24
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      SkinData.SkinSection = 'BUTTON'
    end
    object ButtonCancel: TBCButton
      AlignWithMargins = True
      Left = 170
      Top = 0
      Width = 75
      Height = 24
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alRight
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      SkinData.SkinSection = 'BUTTON'
    end
  end
end
