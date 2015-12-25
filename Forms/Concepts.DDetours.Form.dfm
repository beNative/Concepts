object frmDDetours: TfrmDDetours
  Left = 0
  Top = 0
  Caption = 'DDetours'
  ClientHeight = 282
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblHook: TLabel
    Left = 200
    Top = 152
    Width = 34
    Height = 13
    Caption = 'lblHook'
  end
  object btnHook: TButton
    Left = 40
    Top = 24
    Width = 75
    Height = 25
    Action = actHook
    TabOrder = 0
  end
  object btnUnhook: TButton
    Left = 121
    Top = 24
    Width = 75
    Height = 25
    Action = actUnhook
    TabOrder = 1
  end
  object aclMain: TActionList
    Left = 200
    Top = 144
    object actHook: TAction
      Caption = 'Hook'
      OnExecute = actHookExecute
    end
    object actUnhook: TAction
      Caption = 'Unhook'
      OnExecute = actUnhookExecute
    end
  end
end
