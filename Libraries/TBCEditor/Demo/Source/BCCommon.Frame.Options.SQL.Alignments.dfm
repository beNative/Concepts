inherited OptionsSQLAlignmentsFrame: TOptionsSQLAlignmentsFrame
  Width = 199
  Height = 65
  object Panel: TBCPanel [0]
    AlignWithMargins = True
    Left = 4
    Top = 0
    Width = 195
    Height = 65
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    SkinData.SkinSection = 'CHECKBOX'
    object StickyLabelKeywordAlignmentLeftJustify: TsStickyLabel
      Left = 0
      Top = 48
      Width = 137
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Keyword alignment strict left'
      ParentColor = False
      AttachTo = SliderKeywordAlignmentLeftJustify
      Gap = 8
    end
    object ComboBoxKeywordAlign: TBCComboBox
      Left = 0
      Top = 17
      Width = 186
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taLeftJustify
      BoundLabel.Active = True
      BoundLabel.Caption = 'Keyword align'
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
    object SliderKeywordAlignmentLeftJustify: TsSlider
      Left = 145
      Top = 44
      Width = 50
      AutoSize = True
      TabOrder = 1
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
    Left = 130
    Top = 65532
  end
end
