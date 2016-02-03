inherited OptionsStatusBarFrame: TOptionsStatusBarFrame
  Width = 164
  Height = 208
  object Panel: TBCPanel [0]
    AlignWithMargins = True
    Left = 4
    Top = 0
    Width = 160
    Height = 208
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    SkinData.SkinSection = 'CHECKBOX'
    object StickyLabelUseSystemFont: TsStickyLabel
      Left = 0
      Top = 4
      Width = 98
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Use system font'
      ParentColor = False
      AttachTo = SliderUseSystemFont
      Gap = 8
    end
    object StickyLabelShowMacro: TsStickyLabel
      Left = 0
      Top = 28
      Width = 98
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Show macro'
      ParentColor = False
      AttachTo = SliderShowMacro
      Gap = 8
    end
    object StickyLabelShowCaretPosition: TsStickyLabel
      Left = 0
      Top = 52
      Width = 98
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Show caret position'
      ParentColor = False
      AttachTo = SliderShowCaretPosition
      Gap = 8
    end
    object StickyLabelShowKeyState: TsStickyLabel
      Left = 0
      Top = 76
      Width = 98
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Show key state'
      ParentColor = False
      AttachTo = SliderShowKeyState
      Gap = 8
    end
    object StickyLabelShowModified: TsStickyLabel
      Left = 0
      Top = 100
      Width = 98
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Show modified'
      ParentColor = False
      AttachTo = SliderShowModified
      Gap = 8
    end
    object FontComboBoxFont: TBCFontComboBox
      Left = 0
      Top = 141
      Width = 160
      Height = 22
      Alignment = taLeftJustify
      BoundLabel.Active = True
      BoundLabel.Caption = 'Font'
      BoundLabel.Indent = 4
      BoundLabel.Layout = sclTopLeft
      VerticalAlignment = taAlignTop
      TabOrder = 5
    end
    object EditFontSize: TBCEdit
      Left = 0
      Top = 187
      Width = 64
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      Text = '0'
      SkinData.SkinSection = 'EDIT'
      BoundLabel.Active = True
      BoundLabel.Caption = 'Font size'
      BoundLabel.Indent = 4
      BoundLabel.Layout = sclTopLeft
      EnterToTab = False
      OnlyNumbers = True
      NumbersWithDots = False
      NumbersWithSpots = False
      ErrorColor = 14803198
      NumbersAllowMinus = False
      NumbersAllowPlus = False
    end
    object SliderUseSystemFont: TsSlider
      Left = 106
      Top = 0
      Width = 50
      AutoSize = True
      TabOrder = 0
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
    object SliderShowMacro: TsSlider
      Left = 106
      Top = 24
      Width = 50
      AutoSize = True
      TabOrder = 1
      BoundLabel.Caption = 'Show'
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
    object SliderShowCaretPosition: TsSlider
      Left = 106
      Top = 48
      Width = 50
      AutoSize = True
      TabOrder = 2
      BoundLabel.Caption = 'Show'
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
    object SliderShowKeyState: TsSlider
      Left = 106
      Top = 72
      Width = 50
      AutoSize = True
      TabOrder = 3
      BoundLabel.Caption = 'Show'
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
    object SliderShowModified: TsSlider
      Left = 106
      Top = 96
      Width = 50
      AutoSize = True
      TabOrder = 4
      BoundLabel.Caption = 'Show'
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
  end
  inherited FrameAdapter: TsFrameAdapter
    Left = 116
    Top = 170
  end
end
