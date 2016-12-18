object frmBindings: TfrmBindings
  Left = 0
  Top = 0
  Caption = 'Object binding'
  ClientHeight = 309
  ClientWidth = 326
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblProp: TLabel
    Left = 135
    Top = 24
    Width = 42
    Height = 13
    Caption = 'lblProp'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object lblContactObject: TLabel
    Left = 135
    Top = 8
    Width = 154
    Height = 13
    Caption = 'Contact object property values:'
  end
  object lblDoubleNumber: TLabel
    Left = 147
    Top = 227
    Width = 80
    Height = 13
    Caption = 'lblDoubleNumber'
  end
  object edtFirstname: TLabeledEdit
    Left = 8
    Top = 24
    Width = 121
    Height = 21
    BevelInner = bvNone
    BevelOuter = bvNone
    EditLabel.Width = 51
    EditLabel.Height = 13
    EditLabel.Caption = 'Firstname:'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
  end
  object edtLastname: TLabeledEdit
    Left = 8
    Top = 61
    Width = 121
    Height = 21
    EditLabel.Width = 50
    EditLabel.Height = 13
    EditLabel.Caption = 'Lastname:'
    TabOrder = 1
  end
  object edtAddress: TLabeledEdit
    Left = 8
    Top = 184
    Width = 233
    Height = 21
    EditLabel.Width = 43
    EditLabel.Height = 13
    EditLabel.Caption = 'Address:'
    TabOrder = 4
  end
  object edtCompanyName: TLabeledEdit
    Left = 8
    Top = 144
    Width = 121
    Height = 21
    EditLabel.Width = 76
    EditLabel.Height = 13
    EditLabel.Caption = 'CompanyName:'
    TabOrder = 3
  end
  object edtEmail: TLabeledEdit
    Left = 8
    Top = 96
    Width = 121
    Height = 21
    EditLabel.Width = 28
    EditLabel.Height = 13
    EditLabel.Caption = 'Email:'
    TabOrder = 2
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 290
    Width = 326
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object btnValidate: TButton
    Left = 8
    Top = 251
    Width = 115
    Height = 25
    Action = actValidate
    TabOrder = 6
  end
  object edtNumber: TLabeledEdit
    Left = 8
    Top = 224
    Width = 121
    Height = 21
    EditLabel.Width = 41
    EditLabel.Height = 13
    EditLabel.Caption = 'Number:'
    TabOrder = 7
  end
  object aclMain: TActionList
    Left = 168
    Top = 72
    object actValidate: TAction
      Caption = 'Validate input data'
      OnExecute = actValidateExecute
    end
  end
end
