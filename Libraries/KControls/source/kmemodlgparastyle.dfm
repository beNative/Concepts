object KMemoParaStyleForm: TKMemoParaStyleForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Paragraph style'
  ClientHeight = 456
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object BUOk: TButton
    Left = 211
    Top = 423
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BUCancel: TButton
    Left = 292
    Top = 423
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GBCommon: TGroupBox
    Left = 8
    Top = 8
    Width = 359
    Height = 65
    Caption = 'Common'
    TabOrder = 2
    object LBalignment: TLabel
      Left = 18
      Top = 27
      Width = 51
      Height = 13
      Caption = 'Alignment:'
    end
    object CoBAlign: TComboBox
      Left = 92
      Top = 24
      Width = 125
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'left'
      Items.Strings = (
        'left'
        'center'
        'right'
        'justify')
    end
    object CBWordWrap: TCheckBox
      Left = 232
      Top = 26
      Width = 97
      Height = 17
      Caption = 'Wrap text'
      TabOrder = 1
    end
  end
  object GBIndent: TGroupBox
    Left = 8
    Top = 79
    Width = 359
    Height = 90
    Caption = 'Indentation'
    TabOrder = 3
    object LBFirstIndent: TLabel
      Left = 180
      Top = 21
      Width = 37
      Height = 13
      Caption = 'Special:'
    end
    object LBLeftIndent: TLabel
      Left = 18
      Top = 27
      Width = 56
      Height = 13
      Caption = 'Left indent:'
    end
    object LBRightIndent: TLabel
      Left = 18
      Top = 57
      Width = 62
      Height = 13
      Caption = 'Right indent:'
    end
    object CoBFIrstIndent: TComboBox
      Left = 180
      Top = 40
      Width = 71
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'none'
      Items.Strings = (
        'none'
        'first line'
        'hanging')
    end
    object EDFirstIndent: TKNumberEdit
      Left = 257
      Top = 40
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 250.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      Precision = 1
      UpDownStep = 5.000000000000000000
      TabOrder = 1
    end
    object EDLeftIndent: TKNumberEdit
      Left = 98
      Top = 24
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 250.000000000000000000
      Min = -250.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      Precision = 1
      UpDownStep = 5.000000000000000000
      TabOrder = 3
    end
    object EDRightIndent: TKNumberEdit
      Left = 98
      Top = 54
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 250.000000000000000000
      Min = -250.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      Precision = 1
      UpDownStep = 5.000000000000000000
      TabOrder = 5
    end
  end
  object GBSpacing: TGroupBox
    Left = 8
    Top = 175
    Width = 359
    Height = 93
    Caption = 'Spacing'
    TabOrder = 4
    object LBSpaceAbove: TLabel
      Left = 18
      Top = 27
      Width = 66
      Height = 13
      Caption = 'Space above:'
    end
    object LBSpaceBelow: TLabel
      Left = 18
      Top = 57
      Width = 64
      Height = 13
      Caption = 'Space below:'
    end
    object LBLineSpacing: TLabel
      Left = 180
      Top = 27
      Width = 62
      Height = 13
      Caption = 'Line spacing:'
    end
    object LBLineSpacingValue: TLabel
      Left = 180
      Top = 57
      Width = 65
      Height = 13
      Caption = 'Factor/value:'
    end
    object EDSpaceAbove: TKNumberEdit
      Left = 98
      Top = 24
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 250.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      Precision = 1
      UpDownStep = 3.000000000000000000
      TabOrder = 0
    end
    object EDSpaceBelow: TKNumberEdit
      Left = 98
      Top = 54
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 250.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      Precision = 1
      UpDownStep = 3.000000000000000000
      TabOrder = 2
    end
    object CoBLineSpacing: TComboBox
      Left = 248
      Top = 24
      Width = 95
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      OnClick = CoBLineSpacingClick
      Items.Strings = (
        'single'
        '1.5 lines'
        'double'
        'auto'
        'exact'
        'factor')
    end
    object EDLineSpacingValue: TKNumberEdit
      Left = 271
      Top = 54
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      DecimalSeparator = ','
      Max = 100.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      TabOrder = 5
    end
  end
  object GBShading: TGroupBox
    Left = 8
    Top = 274
    Width = 359
    Height = 143
    Caption = 'Borders and shading'
    TabOrder = 5
    object LBBorderLeft: TLabel
      Left = 18
      Top = 27
      Width = 58
      Height = 13
      Caption = 'Left border:'
    end
    object LBBorderRight: TLabel
      Left = 18
      Top = 57
      Width = 64
      Height = 13
      Caption = 'Right border:'
    end
    object LBBorderTop: TLabel
      Left = 183
      Top = 27
      Width = 57
      Height = 13
      Caption = 'Top border:'
    end
    object LBBorderBottom: TLabel
      Left = 183
      Top = 57
      Width = 73
      Height = 13
      Caption = 'Bottom border:'
    end
    object LBBorderColor: TLabel
      Left = 18
      Top = 87
      Width = 62
      Height = 13
      Caption = 'Border color:'
    end
    object LBShading: TLabel
      Left = 183
      Top = 87
      Width = 42
      Height = 13
      Caption = 'Shading:'
    end
    object LBBorderRadius: TLabel
      Left = 18
      Top = 116
      Width = 68
      Height = 13
      Caption = 'Border radius:'
    end
    object EDBorderLeft: TKNumberEdit
      Left = 98
      Top = 24
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 10.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      Precision = 1
      TabOrder = 0
    end
    object EDBorderRight: TKNumberEdit
      Left = 98
      Top = 54
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 10.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      Precision = 1
      TabOrder = 2
    end
    object EDBorderTop: TKNumberEdit
      Left = 271
      Top = 24
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 10.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      Precision = 1
      TabOrder = 4
    end
    object EDBorderBottom: TKNumberEdit
      Left = 271
      Top = 54
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 10.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      Precision = 1
      TabOrder = 6
    end
    object CLBBorder: TKColorButton
      Left = 98
      Top = 82
      Width = 70
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
      ColorDlgOptions = []
    end
    object CLBShading: TKColorButton
      Left = 271
      Top = 82
      Width = 70
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
      ColorDlgOptions = []
    end
    object EDBorderRadius: TKNumberEdit
      Left = 98
      Top = 113
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 10.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      Precision = 1
      TabOrder = 10
    end
  end
end
