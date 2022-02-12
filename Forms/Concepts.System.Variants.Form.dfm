object frmVariants: TfrmVariants
  Left = 0
  Top = 0
  Caption = 'Variants'
  ClientHeight = 127
  ClientWidth = 261
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object chkNullStrictConvert: TCheckBox
    Left = 136
    Top = 70
    Width = 112
    Height = 17
    Action = actToggleNullStrictConvert
    TabOrder = 0
  end
  object btnUnassigned: TButton
    Left = 8
    Top = 35
    Width = 113
    Height = 25
    Action = actAssignUnassigned
    TabOrder = 1
  end
  object btnShowAsString: TButton
    Left = 127
    Top = 35
    Width = 121
    Height = 25
    Action = actShowAsString
    TabOrder = 2
  end
  object btnNull: TButton
    Left = 8
    Top = 66
    Width = 113
    Height = 25
    Action = actAssignNull
    TabOrder = 3
  end
  object btnEmptyParam: TButton
    Left = 8
    Top = 97
    Width = 113
    Height = 25
    Action = actAssignEmptyParam
    TabOrder = 4
  end
  object pnlHeader: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 255
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 5
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 255
      Height = 29
      Align = alClient
      Alignment = taCenter
      Caption = 'This form demonstrates some features of the Variant type.'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 238
      ExplicitHeight = 26
    end
  end
  object aclMain: TActionList
    Left = 216
    Top = 56
    object actAssignUnassigned: TAction
      Caption = 'Assign Unassigned'
      OnExecute = actAssignUnassignedExecute
    end
    object actAssignNull: TAction
      Caption = 'Assign Null'
      OnExecute = actAssignNullExecute
    end
    object actAssignEmptyParam: TAction
      Caption = 'Assign EmptyParam'
      OnExecute = actAssignEmptyParamExecute
    end
    object actShowAsString: TAction
      Caption = 'Show as string'
      OnExecute = actShowAsStringExecute
    end
    object actToggleNullStrictConvert: TAction
      Caption = 'NullStrictConvert'
      OnExecute = actToggleNullStrictConvertExecute
    end
  end
end
