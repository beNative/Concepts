inherited OptionsMainMenuFrame: TOptionsMainMenuFrame
  Width = 164
  Height = 106
  Visible = False
  object Panel: TBCPanel [0]
    AlignWithMargins = True
    Left = 4
    Top = 0
    Width = 160
    Height = 106
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
      Width = 90
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
    object SliderUseSystemFont: TsSlider
      Left = 98
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
    object FontComboBoxFont: TBCFontComboBox
      Left = 0
      Top = 43
      Width = 160
      Height = 22
      Alignment = taLeftJustify
      BoundLabel.Active = True
      BoundLabel.Caption = 'Font'
      BoundLabel.Indent = 4
      BoundLabel.Layout = sclTopLeft
      VerticalAlignment = taAlignTop
      TabOrder = 1
    end
    object EditFontSize: TBCEdit
      Left = 0
      Top = 85
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
      TabOrder = 2
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
  end
  inherited FrameAdapter: TsFrameAdapter
    Left = 114
    Top = 60
  end
end
