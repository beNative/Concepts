inherited OptionsEditorTabulatorFrame: TOptionsEditorTabulatorFrame
  Width = 182
  Height = 87
  object Panel: TBCPanel [0]
    AlignWithMargins = True
    Left = 4
    Top = 0
    Width = 178
    Height = 87
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
    object StickyLabelSelectedBlockIndent: TsStickyLabel
      Left = 0
      Top = 4
      Width = 120
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Selected block indent'
      ParentColor = False
      AttachTo = SliderSelectedBlockIndent
      Gap = 8
    end
    object StickyLabelTabsToSpaces: TsStickyLabel
      Left = 0
      Top = 27
      Width = 120
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Tabs to spaces'
      ParentColor = False
      AttachTo = SliderTabsToSpaces
      Gap = 8
    end
    object EditWidth: TBCEdit
      Left = 0
      Top = 66
      Width = 64
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      TabOrder = 0
      Text = '2'
      SkinData.SkinSection = 'EDIT'
      BoundLabel.Active = True
      BoundLabel.Caption = 'Width'
      BoundLabel.Indent = 4
      BoundLabel.Layout = sclTopLeft
      EnterToTab = False
      OnlyNumbers = True
      NumbersWithDots = False
      NumbersWithSpots = False
      ErrorColor = clBlack
      NumbersAllowMinus = False
      NumbersAllowPlus = False
    end
    object SliderSelectedBlockIndent: TsSlider
      Left = 128
      Top = 0
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
    object SliderTabsToSpaces: TsSlider
      Left = 128
      Top = 23
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
  inherited FrameAdapter: TsFrameAdapter
    Left = 48
    Top = 86
  end
end
