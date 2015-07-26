object frmVariants: TfrmVariants
  Left = 0
  Top = 0
  Caption = 'Variants'
  ClientHeight = 138
  ClientWidth = 269
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object chkNullStrictConvert: TCheckBox
    Left = 16
    Top = 16
    Width = 137
    Height = 17
    Action = actToggleNullStrictConvert
    TabOrder = 0
  end
  object btnUnassigned: TButton
    Left = 16
    Top = 39
    Width = 113
    Height = 25
    Action = actAssignUnassigned
    TabOrder = 1
  end
  object btnShowAsString: TButton
    Left = 135
    Top = 39
    Width = 121
    Height = 25
    Action = actShowAsString
    TabOrder = 2
  end
  object btnNull: TButton
    Left = 16
    Top = 70
    Width = 113
    Height = 25
    Action = actAssignNull
    TabOrder = 3
  end
  object btnEmptyParam: TButton
    Left = 16
    Top = 101
    Width = 113
    Height = 25
    Action = actAssignEmptyParam
    TabOrder = 4
  end
  object aclMain: TActionList
    Left = 16
    Top = 136
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
