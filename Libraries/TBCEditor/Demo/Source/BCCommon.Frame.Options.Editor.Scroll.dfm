inherited OptionsEditorScrollFrame: TOptionsEditorScrollFrame
  Width = 212
  Height = 136
  object Panel: TBCPanel [0]
    AlignWithMargins = True
    Left = 4
    Top = 0
    Width = 208
    Height = 136
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
    object StickyLabelAutosizeMaxWidth: TsStickyLabel
      Left = 0
      Top = 4
      Width = 150
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Autosize max width'
      ParentColor = False
      AttachTo = SliderAutosizeMaxWidth
      Gap = 8
    end
    object StickyLabelHalfPage: TsStickyLabel
      Left = 0
      Top = 27
      Width = 150
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Half page'
      ParentColor = False
      AttachTo = SliderHalfPage
      Gap = 8
    end
    object StickyLabelHintFollows: TsStickyLabel
      Left = 0
      Top = 50
      Width = 150
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Hint follows'
      ParentColor = False
      AttachTo = SliderHintFollows
      Gap = 8
    end
    object StickyLabelPastEndOfFile: TsStickyLabel
      Left = 0
      Top = 73
      Width = 150
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Past end of file'
      ParentColor = False
      AttachTo = SliderPastEndOfFile
      Gap = 8
    end
    object StickyLabelPastEndOfLineMarker: TsStickyLabel
      Left = 0
      Top = 96
      Width = 150
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Past end of line marker'
      ParentColor = False
      AttachTo = SliderPastEndOfLineMarker
      Gap = 8
    end
    object StickyLabelShowHint: TsStickyLabel
      Left = 0
      Top = 119
      Width = 150
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Show hint'
      ParentColor = False
      AttachTo = SliderShowHint
      Gap = 8
    end
    object SliderAutosizeMaxWidth: TsSlider
      Left = 158
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
    object SliderHalfPage: TsSlider
      Left = 158
      Top = 23
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
    object SliderHintFollows: TsSlider
      Left = 158
      Top = 46
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
    object SliderPastEndOfFile: TsSlider
      Left = 158
      Top = 69
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
    object SliderPastEndOfLineMarker: TsSlider
      Left = 158
      Top = 92
      Width = 50
      AutoSize = True
      TabOrder = 4
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
    object SliderShowHint: TsSlider
      Left = 158
      Top = 115
      Width = 50
      AutoSize = True
      TabOrder = 5
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
