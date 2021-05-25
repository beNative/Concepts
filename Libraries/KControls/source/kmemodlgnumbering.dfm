object KMemoNumberingForm: TKMemoNumberingForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Bullets and numbering'
  ClientHeight = 402
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BUOk: TButton
    Left = 211
    Top = 367
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BUCancel: TButton
    Left = 292
    Top = 367
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object RGNumbering: TRadioGroup
    Left = 8
    Top = 8
    Width = 359
    Height = 161
    Caption = 'Numbering style'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Bullets'
      'Arabic (1,2,3)'
      'Letter Lo (a,b,c)'
      'Letter Hi (A,B,C)'
      'Roman Lo (i,ii,iii)'
      'Roman Hi (I,II,III)')
    TabOrder = 2
    OnClick = RGNumberingClick
  end
  object GBOptions: TGroupBox
    Left = 8
    Top = 175
    Width = 359
    Height = 98
    Caption = 'Options'
    TabOrder = 3
    object LBFirstIndent: TLabel
      Left = 10
      Top = 27
      Width = 58
      Height = 13
      Caption = 'First indent:'
    end
    object LBLeftIndent: TLabel
      Left = 10
      Top = 57
      Width = 56
      Height = 13
      Caption = 'Left indent:'
    end
    object LBListLevel: TLabel
      Left = 186
      Top = 57
      Width = 29
      Height = 13
      Caption = 'Level:'
    end
    object EDFirstIndent: TKNumberEdit
      Left = 90
      Top = 24
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 250.000000000000000000
      Min = -250.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      UpDownStep = 5.000000000000000000
      TabOrder = 0
    end
    object EDLeftIndent: TKNumberEdit
      Left = 90
      Top = 54
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 250.000000000000000000
      Min = -250.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      UpDownStep = 5.000000000000000000
      TabOrder = 2
    end
    object CoBListLevel: TComboBox
      Left = 245
      Top = 54
      Width = 100
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      OnClick = CoBListLevelClick
      Items.Strings = (
        'first'
        'second'
        'third'
        'fourth'
        'fifth'
        'sixth'
        'seventh'
        'eighth'
        'ninth')
    end
  end
  object GBStartAt: TGroupBox
    Left = 8
    Top = 279
    Width = 359
    Height = 80
    Caption = 'Starting value'
    TabOrder = 4
    object RBContinuous: TRadioButton
      Left = 10
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Continuous'
      TabOrder = 0
      OnClick = RBContinuousClick
    end
    object RBStartFromOne: TRadioButton
      Left = 10
      Top = 47
      Width = 113
      Height = 17
      Caption = 'Again start from 1'
      TabOrder = 1
      OnClick = RBContinuousClick
    end
    object RBStartAt: TRadioButton
      Left = 185
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Set custom value:'
      TabOrder = 2
      OnClick = RBContinuousClick
    end
    object EDStartAt: TKNumberEdit
      Left = 184
      Top = 47
      Width = 55
      Height = 21
      DecimalSeparator = ','
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      Value = 1.000000000000000000
      TabOrder = 3
    end
  end
end
