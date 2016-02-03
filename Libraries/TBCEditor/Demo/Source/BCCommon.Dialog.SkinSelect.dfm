inherited SkinSelectDialog: TSkinSelectDialog
  Caption = 'Select skin'
  ClientHeight = 383
  ClientWidth = 748
  Padding.Left = 6
  Padding.Top = 6
  Padding.Right = 6
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterLeft: TBCSplitter
    Left = 146
    Top = 6
    Height = 330
    SkinData.SkinSection = 'SPLITTER'
  end
  object SplitterRight: TBCSplitter
    Left = 510
    Top = 6
    Height = 330
    Align = alRight
  end
  object ListBoxSkins: TsListBox
    Left = 6
    Top = 6
    Width = 140
    Height = 330
    Align = alLeft
    TabOrder = 0
    OnClick = ListBoxSkinsClick
    SkinData.SkinSection = 'EDIT'
  end
  object PanelButtons: TBCPanel
    AlignWithMargins = True
    Left = 9
    Top = 339
    Width = 733
    Height = 41
    Margins.Right = 0
    Align = alBottom
    BevelOuter = bvNone
    Padding.Top = 8
    Padding.Bottom = 8
    TabOrder = 1
    SkinData.SkinSection = 'CHECKBOX'
    object ButtonOK: TBCButton
      AlignWithMargins = True
      Left = 578
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
      ModalResult = 1
      TabOrder = 0
      SkinData.SkinSection = 'BUTTON'
    end
    object ButtonCancel: TBCButton
      Left = 658
      Top = 8
      Width = 75
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      SkinData.SkinSection = 'BUTTON'
    end
  end
  object PanelPreviewArea: TBCPanel
    Left = 152
    Top = 6
    Width = 358
    Height = 330
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Preview area'
    TabOrder = 2
    SkinData.SkinSection = 'GROUPBOX'
  end
  object PanelSkinColorization: TBCPanel
    Left = 516
    Top = 6
    Width = 226
    Height = 330
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 3
    SkinData.SkinSection = 'CHECKBOX'
    object StickyLabelExtendedBordersMode: TsStickyLabel
      Left = 10
      Top = 214
      Width = 150
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Extended borders mode'
      Color = clBtnFace
      ParentColor = False
      AttachTo = SliderExtendedBordersMode
      Gap = 8
    end
    object StickyLabelBlendOnMove: TsStickyLabel
      Left = 10
      Top = 238
      Width = 150
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Blend a form when moved'
      Color = clBtnFace
      ParentColor = False
      AttachTo = SliderBlendOnMove
      Gap = 8
    end
    object StickyLabelAllowGlowing: TsStickyLabel
      Left = 10
      Top = 262
      Width = 150
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Allow glowing'
      Color = clBtnFace
      ParentColor = False
      AttachTo = SliderAllowGlowing
      Gap = 8
    end
    object GroupBoxSkinColorization: TsGroupBox
      Left = 6
      Top = 0
      Width = 219
      Height = 197
      Caption = ' Skin colorization '
      TabOrder = 0
      CaptionLayout = clTopCenter
      CaptionYOffset = 6
      SkinData.SkinSection = 'PANEL_LOW'
      SkinData.OuterEffects.Visibility = ovAlways
      CaptionSkin = 'PROGRESSH'
      CaptionWidth = 100
      Checked = False
      object LabelSaturationValue: TsLabel
        Left = 192
        Top = 87
        Width = 6
        Height = 13
        Caption = '0'
        ParentFont = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
      end
      object LabelHueOffsetValue: TsLabel
        Left = 192
        Top = 27
        Width = 6
        Height = 13
        Caption = '0'
        ParentFont = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
      end
      object LabelBrightnessValue: TsLabel
        Left = 192
        Top = 143
        Width = 6
        Height = 13
        Caption = '0'
        ParentFont = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
      end
      object LabelHueOffsetMin: TsLabel
        Left = 10
        Top = 52
        Width = 6
        Height = 13
        Caption = '0'
        Enabled = False
      end
      object LabelHueOffsetMax: TsLabel
        Left = 164
        Top = 52
        Width = 18
        Height = 13
        Caption = '360'
        Enabled = False
      end
      object LabelSaturationMin: TsLabel
        Left = 10
        Top = 108
        Width = 22
        Height = 13
        Caption = '-100'
        Enabled = False
      end
      object LabelSaturationMax: TsLabel
        Left = 164
        Top = 108
        Width = 18
        Height = 13
        Caption = '100'
        Enabled = False
      end
      object LabelHueOffset: TsLabel
        Left = 70
        Top = 52
        Width = 54
        Height = 13
        Caption = 'HUE Offset'
      end
      object LabelSaturation: TsLabel
        Left = 73
        Top = 108
        Width = 50
        Height = 13
        Caption = 'Saturation'
      end
      object LabelBrightnessMin: TsLabel
        Left = 10
        Top = 164
        Width = 16
        Height = 13
        Alignment = taCenter
        Caption = '-50'
        Enabled = False
      end
      object LabelBrightnessMax: TsLabel
        Left = 170
        Top = 164
        Width = 12
        Height = 13
        Alignment = taCenter
        Caption = '50'
        Enabled = False
      end
      object LabelBrightness: TsLabel
        Left = 72
        Top = 164
        Width = 50
        Height = 13
        Caption = 'Brightness'
      end
      object TrackBarSaturation: TsTrackBar
        Tag = 5
        Left = 2
        Top = 82
        Width = 188
        Height = 24
        Max = 100
        Min = -100
        PageSize = 24
        Frequency = 20
        TabOrder = 1
        TickStyle = tsNone
        OnChange = TrackBarSaturationChange
        SkinData.SkinSection = 'TRACKBAR'
        ShowProgress = True
        BarOffsetV = 0
        BarOffsetH = 0
        ShowProgressFrom = -100
      end
      object TrackBarHueOffset: TsTrackBar
        Tag = 5
        Left = 2
        Top = 22
        Width = 188
        Height = 28
        Max = 360
        PageSize = 24
        Frequency = 36
        TabOrder = 0
        TickStyle = tsNone
        OnChange = TrackBarHueOffsetChange
        SkinData.SkinSection = 'TRACKBAR'
        ShowProgress = True
        OnSkinPaint = TrackBarHueOffsetSkinPaint
        BarOffsetV = 0
        BarOffsetH = 0
      end
      object TrackBarBrightness: TsTrackBar
        Tag = 5
        Left = 2
        Top = 138
        Width = 188
        Height = 24
        Max = 50
        Min = -50
        PageSize = 24
        Frequency = 20
        TabOrder = 2
        TickStyle = tsNone
        OnChange = TrackBarBrightnessChange
        SkinData.SkinSection = 'TRACKBAR'
        ShowProgress = True
        BarOffsetV = 0
        BarOffsetH = 0
        ShowProgressFrom = -50
      end
    end
    object SliderExtendedBordersMode: TsSlider
      Left = 168
      Top = 210
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
      OnSliderChange = SliderExtendedBordersModeSliderChange
    end
    object SliderBlendOnMove: TsSlider
      Left = 168
      Top = 234
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
      SliderOn = False
      OnSliderChange = SliderBlendOnMoveSliderChange
    end
    object SliderAllowGlowing: TsSlider
      Left = 168
      Top = 258
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
      SliderOn = False
      OnSliderChange = SliderBlendOnMoveSliderChange
    end
  end
end
