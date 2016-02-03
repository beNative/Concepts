inherited ReplaceDialog: TReplaceDialog
  Caption = 'Replace'
  ClientHeight = 413
  ClientWidth = 369
  Padding.Left = 6
  Padding.Top = 6
  Padding.Right = 6
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxOptions: TBCGroupBox
    AlignWithMargins = True
    Left = 6
    Top = 120
    Width = 357
    Height = 173
    Margins.Left = 0
    Margins.Right = 0
    Align = alTop
    Caption = ' Options'
    TabOrder = 4
    TabStop = True
    SkinData.SkinSection = 'GROUPBOX'
    Checked = False
    object StickyLabelCaseSensitive: TsStickyLabel
      Left = 12
      Top = 26
      Width = 102
      Height = 13
      AutoSize = False
      Caption = 'Case sensitive'
      AttachTo = SliderCaseSensitive
      Gap = 8
    end
    object StickyLabelPromptOnReplace: TsStickyLabel
      Left = 12
      Top = 49
      Width = 102
      Height = 13
      AutoSize = False
      Caption = 'Prompt on replace'
      AttachTo = SliderPromptOnReplace
      Gap = 8
    end
    object StickyLabelRegularExpression: TsStickyLabel
      Left = 12
      Top = 72
      Width = 102
      Height = 13
      AutoSize = False
      Caption = 'Regular expressions'
      AttachTo = SliderRegularExpression
      Gap = 8
    end
    object StickyLabelSelectedOnly: TsStickyLabel
      Left = 12
      Top = 95
      Width = 102
      Height = 13
      AutoSize = False
      Caption = 'Selected only'
      AttachTo = SliderSelectedOnly
      Gap = 8
    end
    object StickyLabelWholeWordsOnly: TsStickyLabel
      Left = 12
      Top = 118
      Width = 102
      Height = 13
      AutoSize = False
      Caption = 'Whole words only'
      AttachTo = SliderWholeWordsOnly
      Gap = 8
    end
    object StickyLabelWildCard: TsStickyLabel
      Left = 12
      Top = 141
      Width = 102
      Height = 13
      AutoSize = False
      Caption = 'Wild card'
      AttachTo = SliderWildCard
      Gap = 8
    end
    object SliderCaseSensitive: TsSlider
      Left = 122
      Top = 22
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
      SliderOn = False
    end
    object SliderPromptOnReplace: TsSlider
      Left = 122
      Top = 45
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
    end
    object SliderRegularExpression: TsSlider
      Left = 122
      Top = 68
      Width = 50
      AutoSize = True
      TabOrder = 2
      OnClick = SliderRegularExpressionClick
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
      Left = 122
      Top = 91
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
    object SliderWholeWordsOnly: TsSlider
      Left = 122
      Top = 114
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
    object SliderWildCard: TsSlider
      Left = 122
      Top = 137
      Width = 50
      AutoSize = True
      TabOrder = 5
      OnClick = SliderWildCardClick
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
  object PanelSearchForComboBox: TBCPanel
    Left = 6
    Top = 6
    Width = 357
    Height = 38
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 0
    SkinData.SkinSection = 'CHECKBOX'
    object ComboBoxSearchFor: TBCComboBox
      Left = 0
      Top = 17
      Width = 357
      Height = 21
      Align = alBottom
      Alignment = taLeftJustify
      BoundLabel.Active = True
      BoundLabel.Caption = 'Search for'
      BoundLabel.Indent = 4
      BoundLabel.Layout = sclTopLeft
      SkinData.SkinSection = 'COMBOBOX'
      VerticalAlignment = taAlignTop
      ItemIndex = -1
      TabOrder = 0
      OnChange = ComboBoxSearchForChange
      UseMouseWheel = False
    end
  end
  object PanelReplaceWith: TBCPanel
    Left = 6
    Top = 44
    Width = 357
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 1
    SkinData.SkinSection = 'CHECKBOX'
    object RadioButtonReplaceWith: TBCRadioButton
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 81
      Height = 26
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alLeft
      BiDiMode = bdLeftToRight
      Caption = 'Replace with'
      Checked = True
      ParentBiDiMode = False
      ParentColor = False
      TabOrder = 0
      TabStop = True
      OnClick = RadioButtonReplaceWithClick
      SkinData.SkinSection = 'CHECKBOX'
    end
  end
  object PanelReplaceWithComboBox: TBCPanel
    Left = 6
    Top = 70
    Width = 357
    Height = 21
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 2
    SkinData.SkinSection = 'PANEL'
    object ComboBoxReplaceWith: TBCComboBox
      Left = 0
      Top = 0
      Width = 357
      Height = 21
      Align = alClient
      Alignment = taLeftJustify
      SkinData.SkinSection = 'COMBOBOX'
      VerticalAlignment = taAlignTop
      ItemIndex = -1
      TabOrder = 0
      UseMouseWheel = False
    end
  end
  object PanelButtons: TBCPanel
    AlignWithMargins = True
    Left = 9
    Top = 369
    Width = 354
    Height = 41
    Margins.Right = 0
    Align = alBottom
    BevelOuter = bvNone
    Padding.Top = 8
    Padding.Bottom = 8
    TabOrder = 6
    SkinData.SkinSection = 'CHECKBOX'
    object ButtonOK: TBCButton
      AlignWithMargins = True
      Left = 119
      Top = 8
      Width = 75
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 0
      Align = alRight
      Caption = 'OK'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
      SkinData.SkinSection = 'BUTTON'
    end
    object ButtonReplaceAll: TBCButton
      AlignWithMargins = True
      Left = 199
      Top = 8
      Width = 75
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 0
      Align = alRight
      Caption = 'Replace all'
      Enabled = False
      ModalResult = 6
      TabOrder = 1
      SkinData.SkinSection = 'BUTTON'
    end
    object ButtonCancel: TBCButton
      Left = 279
      Top = 8
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
      SkinData.SkinSection = 'BUTTON'
    end
  end
  object GroupBoxReplaceIn: TBCGroupBox
    Left = 6
    Top = 296
    Width = 357
    Height = 70
    Align = alTop
    BiDiMode = bdLeftToRight
    Caption = ' Replace in'
    ParentBiDiMode = False
    TabOrder = 5
    TabStop = True
    SkinData.SkinSection = 'GROUPBOX'
    Checked = False
    object RadioButtonWholeFile: TBCRadioButton
      Left = 12
      Top = 20
      Width = 67
      Height = 20
      Caption = 'Whole file'
      Checked = True
      TabOrder = 0
      TabStop = True
      SkinData.SkinSection = 'CHECKBOX'
    end
    object RadioButtonAllOpenFiles: TBCRadioButton
      Left = 12
      Top = 41
      Width = 80
      Height = 20
      Caption = 'All open files'
      TabOrder = 1
      SkinData.SkinSection = 'CHECKBOX'
    end
  end
  object PanelDeleteLine: TBCPanel
    Left = 6
    Top = 91
    Width = 357
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 3
    SkinData.SkinSection = 'CHECKBOX'
    object RadioButtonDeleteLine: TBCRadioButton
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 70
      Height = 26
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alLeft
      BiDiMode = bdLeftToRight
      Caption = 'Delete line'
      ParentBiDiMode = False
      ParentColor = False
      TabOrder = 0
      TabStop = True
      OnClick = RadioButtonDeleteLineClick
      SkinData.SkinSection = 'CHECKBOX'
    end
  end
end
