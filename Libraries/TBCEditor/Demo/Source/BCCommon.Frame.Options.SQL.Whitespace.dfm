inherited OptionsSQLWhitespaceFrame: TOptionsSQLWhitespaceFrame
  Width = 373
  Height = 136
  object Panel: TBCPanel [0]
    AlignWithMargins = True
    Left = 4
    Top = 0
    Width = 369
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
    object StickyLabelSpaceAroundOperator: TsStickyLabel
      Left = 0
      Top = 4
      Width = 311
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Space around operator of arithmetric expression'
      ParentColor = False
      AttachTo = SliderSpaceAroundOperator
      Gap = 8
    end
    object StickyLabelSpaceInsideCreate: TsStickyLabel
      Left = 0
      Top = 27
      Width = 311
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Space inside parenthesis in CREATE FUNCTION/PROCEDURE'
      ParentColor = False
      AttachTo = SliderSpaceInsideCreate
      Gap = 8
    end
    object StickyLabelSpaceInsideExpression: TsStickyLabel
      Left = 0
      Top = 50
      Width = 311
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Space inside parenthesis in expression'
      ParentColor = False
      AttachTo = SliderSpaceInsideExpression
      Gap = 8
    end
    object StickyLabelSpaceInsideSubquery: TsStickyLabel
      Left = 0
      Top = 73
      Width = 311
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Space inside parenthesis in subquery'
      ParentColor = False
      AttachTo = SliderSpaceInsideSubquery
      Gap = 8
    end
    object StickyLabelSpaceInsideFunction: TsStickyLabel
      Left = 0
      Top = 96
      Width = 311
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Space inside parenthesis in function call'
      ParentColor = False
      AttachTo = SliderSpaceInsideFunction
      Gap = 8
    end
    object StickyLabelSpaceInsideTypename: TsStickyLabel
      Left = 0
      Top = 119
      Width = 311
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Space inside parenthesis of typename in CREATE TABLE'
      ParentColor = False
      AttachTo = SliderSpaceInsideTypename
      Gap = 8
    end
    object SliderSpaceAroundOperator: TsSlider
      Left = 319
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
    object SliderSpaceInsideCreate: TsSlider
      Left = 319
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
    object SliderSpaceInsideExpression: TsSlider
      Left = 319
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
    object SliderSpaceInsideSubquery: TsSlider
      Left = 319
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
    object SliderSpaceInsideFunction: TsSlider
      Left = 319
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
    object SliderSpaceInsideTypename: TsSlider
      Left = 319
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
  inherited FrameAdapter: TsFrameAdapter
    Left = 160
    Top = 56
  end
end
