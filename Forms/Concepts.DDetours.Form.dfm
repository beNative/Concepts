object frmDDetours: TfrmDDetours
  Left = 0
  Top = 0
  Caption = 'DDetours'
  ClientHeight = 54
  ClientWidth = 309
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object btnUnitName: TButton
    Left = 128
    Top = 8
    Width = 156
    Height = 25
    Action = actCallDetouredMethod
    TabOrder = 0
  end
  object chkDetour1Enabled: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Detour1 enabled'
    TabOrder = 1
    OnClick = chkDetour1EnabledClick
  end
  object chkDetour2Enabled: TCheckBox
    Left = 8
    Top = 31
    Width = 97
    Height = 17
    Caption = 'Detour2 enabled'
    TabOrder = 2
    OnClick = chkDetour2EnabledClick
  end
  object aclMain: TActionList
    Left = 304
    Top = 104
    object actCallDetouredMethod: TAction
      Caption = 'Call detoured method'
      OnExecute = actCallDetouredMethodExecute
    end
  end
end
