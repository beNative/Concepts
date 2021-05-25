object KMemoTextStyleForm: TKMemoTextStyleForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Text style'
  ClientHeight = 473
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BUOk: TButton
    Left = 181
    Top = 436
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BUCancel: TButton
    Left = 262
    Top = 436
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GBFont: TGroupBox
    Left = 8
    Top = 8
    Width = 329
    Height = 218
    Caption = 'Font'
    TabOrder = 2
    object LBFontName: TLabel
      Left = 18
      Top = 19
      Width = 55
      Height = 13
      Caption = 'Font name:'
    end
    object LBFontSize: TLabel
      Left = 236
      Top = 19
      Width = 47
      Height = 13
      Caption = 'Font size:'
    end
    object EDFont: TEdit
      Left = 18
      Top = 38
      Width = 208
      Height = 21
      TabOrder = 0
      Text = 'EDFont'
      OnChange = EDFontChange
    end
    object LiBFont: TListBox
      Left = 18
      Top = 60
      Width = 208
      Height = 141
      Style = lbOwnerDrawFixed
      ItemHeight = 20
      TabOrder = 1
      OnClick = LiBFontClick
      OnDrawItem = LiBFontDrawItem
    end
    object LiBFontSize: TListBox
      Left = 236
      Top = 60
      Width = 75
      Height = 141
      ItemHeight = 13
      TabOrder = 2
      OnClick = LiBFontSizeClick
    end
    object EDFontSize: TKNumberEdit
      Left = 236
      Top = 38
      Width = 59
      Height = 21
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 200.000000000000000000
      Min = 1.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      Value = 1.000000000000000000
      TabOrder = 3
      OnChange = EDFontSizeChange
    end
  end
  object GBStyle: TGroupBox
    Left = 8
    Top = 303
    Width = 329
    Height = 127
    Caption = 'Style'
    TabOrder = 3
    object CBBold: TCheckBox
      Left = 18
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Bold'
      TabOrder = 0
    end
    object CBItalic: TCheckBox
      Left = 18
      Top = 47
      Width = 97
      Height = 17
      Caption = 'Italic'
      TabOrder = 1
    end
    object CBUnderline: TCheckBox
      Left = 18
      Top = 70
      Width = 97
      Height = 17
      Caption = 'Underlined'
      TabOrder = 2
    end
    object CBStrikeout: TCheckBox
      Left = 18
      Top = 93
      Width = 97
      Height = 17
      Caption = 'Strikeout'
      TabOrder = 3
    end
    object CBCaps: TCheckBox
      Left = 161
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Capitals'
      TabOrder = 4
      OnClick = CBCapsClick
    end
    object CBSmallCaps: TCheckBox
      Left = 161
      Top = 47
      Width = 97
      Height = 17
      Caption = 'Small capitals'
      TabOrder = 5
      OnClick = CBSmallCapsClick
    end
    object CBSubscript: TCheckBox
      Left = 161
      Top = 70
      Width = 97
      Height = 17
      Caption = 'Subscript'
      TabOrder = 6
      OnClick = CBSubscriptClick
    end
    object CBSuperscript: TCheckBox
      Left = 161
      Top = 93
      Width = 97
      Height = 17
      Caption = 'Superscript'
      TabOrder = 7
      OnClick = CBSuperscriptClick
    end
  end
  object GBColors: TGroupBox
    Left = 8
    Top = 229
    Width = 329
    Height = 68
    Caption = 'Colors'
    TabOrder = 4
    object LBTextShading: TLabel
      Left = 161
      Top = 31
      Width = 42
      Height = 13
      Caption = 'Shading:'
    end
    object LBFontColor: TLabel
      Left = 18
      Top = 31
      Width = 52
      Height = 13
      Caption = 'Font color:'
    end
    object CLBTextShading: TKColorButton
      Left = 241
      Top = 26
      Width = 70
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      ColorDlgOptions = []
    end
    object CLBFontColor: TKColorButton
      Left = 82
      Top = 26
      Width = 70
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      ColorDlgOptions = []
    end
  end
end
