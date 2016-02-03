inherited OptionsEditorCaretFrame: TOptionsEditorCaretFrame
  Width = 223
  Height = 310
  object Panel: TBCPanel [0]
    AlignWithMargins = True
    Left = 4
    Top = 0
    Width = 219
    Height = 310
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    BevelOuter = bvNone
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
    SkinData.SkinSection = 'CHECKBOX'
    object StickyLabelVisible: TsStickyLabel
      Left = 0
      Top = 4
      Width = 150
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Visible'
      ParentColor = False
      AttachTo = SliderVisible
      Gap = 8
    end
    object StickyLabelRightMouseClickMovesCaret: TsStickyLabel
      Left = 0
      Top = 27
      Width = 150
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Right mouse click moves caret'
      ParentColor = False
      AttachTo = SliderRightMouseClickMovesCaret
      Gap = 8
    end
    object GroupBoxNonBlinkingCaret: TBCGroupBox
      Left = 0
      Top = 47
      Width = 219
      Height = 142
      Caption = ' Non-blinking caret'
      TabOrder = 0
      SkinData.SkinSection = 'GROUPBOX'
      Checked = False
      object StickyLabelNonblinkingCaretEnabled: TsStickyLabel
        Left = 10
        Top = 20
        Width = 38
        Height = 13
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Enabled'
        ParentColor = False
        AttachTo = SliderNonblinkingCaretEnabled
        Gap = 8
      end
      object ColorBoxNonblinkingCaretBackground: TBCColorComboBox
        Left = 10
        Top = 60
        Width = 200
        Height = 22
        BoundLabel.Active = True
        BoundLabel.Caption = 'Background color'
        BoundLabel.Indent = 4
        BoundLabel.Layout = sclTopLeft
        SkinData.SkinSection = 'COMBOBOX'
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor]
        Selected = clWindow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        ColorText = 'clWindow'
      end
      object ColorBoxNonblinkingCaretForeground: TBCColorComboBox
        Left = 10
        Top = 105
        Width = 200
        Height = 22
        BoundLabel.Active = True
        BoundLabel.Caption = 'Foreground color'
        BoundLabel.Indent = 4
        BoundLabel.Layout = sclTopLeft
        SkinData.SkinSection = 'COMBOBOX'
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor]
        Selected = clWindow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        ColorText = 'clWindow'
      end
      object SliderNonblinkingCaretEnabled: TsSlider
        Left = 56
        Top = 16
        Width = 50
        AutoSize = True
        TabOrder = 2
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
    object GroupBoxStyles: TBCGroupBox
      Left = 0
      Top = 191
      Width = 219
      Height = 119
      Caption = ' Styles'
      TabOrder = 1
      SkinData.SkinSection = 'GROUPBOX'
      Checked = False
      object ComboBoxStylesInsertCaret: TBCComboBox
        Left = 10
        Top = 37
        Width = 133
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Alignment = taLeftJustify
        BoundLabel.Active = True
        BoundLabel.Caption = 'Insert'
        BoundLabel.Indent = 4
        BoundLabel.Layout = sclTopLeft
        DropDownCount = 9
        SkinData.SkinSection = 'COMBOBOX'
        VerticalAlignment = taAlignTop
        Style = csOwnerDrawFixed
        ItemIndex = -1
        TabOrder = 0
        UseMouseWheel = False
      end
      object ComboBoxStylesOverwriteCaret: TBCComboBox
        Left = 10
        Top = 83
        Width = 133
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Alignment = taLeftJustify
        BoundLabel.Active = True
        BoundLabel.Caption = 'Overwrite'
        BoundLabel.Indent = 4
        BoundLabel.Layout = sclTopLeft
        DropDownCount = 9
        SkinData.SkinSection = 'COMBOBOX'
        VerticalAlignment = taAlignTop
        Style = csOwnerDrawFixed
        ItemIndex = -1
        TabOrder = 1
        UseMouseWheel = False
      end
    end
    object SliderVisible: TsSlider
      Left = 158
      Top = 0
      Width = 50
      AutoSize = True
      TabOrder = 2
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
    object SliderRightMouseClickMovesCaret: TsSlider
      Left = 158
      Top = 23
      Width = 50
      AutoSize = True
      TabOrder = 3
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
end
