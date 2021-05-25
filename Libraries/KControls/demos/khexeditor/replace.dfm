inherited ReplaceForm: TReplaceForm
  Left = 618
  Top = 243
  Caption = 'Replace Text'
  ClientHeight = 267
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object LBReplaceText: TLabel [1]
    Left = 8
    Top = 44
    Width = 65
    Height = 13
    Caption = 'Rep&lace with:'
    FocusControl = CBTextToReplace
  end
  inherited CBTextToFind: TComboBox
    Left = 80
    Width = 241
  end
  inherited GBOptions: TGroupBox
    Top = 72
    Height = 85
    TabOrder = 2
    object CBPromptOnReplace: TCheckBox
      Left = 8
      Top = 56
      Width = 153
      Height = 17
      Caption = '&Prompt on replace'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  inherited BUFind: TButton
    Left = 60
    Top = 236
    Caption = '&Replace'
    TabOrder = 6
    OnClick = CBTextToReplaceClick
  end
  inherited BUCancel: TButton
    Left = 244
    Top = 236
    TabOrder = 8
  end
  inherited GBDirection: TGroupBox
    Top = 72
    Height = 65
    TabOrder = 3
  end
  inherited GBScope: TGroupBox
    Top = 164
    TabOrder = 4
  end
  inherited GBOrigin: TGroupBox
    Top = 164
    TabOrder = 5
  end
  object CBTextToReplace: TComboBox
    Left = 80
    Top = 40
    Width = 241
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    OnSelect = CBTextToReplaceClick
  end
  object BUReplaceAll: TButton
    Left = 144
    Top = 236
    Width = 89
    Height = 25
    Caption = 'Replace &All'
    ModalResult = 14
    TabOrder = 7
  end
end
