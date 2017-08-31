object frmRTTI: TfrmRTTI
  Left = 0
  Top = 0
  Caption = 'RTTI'
  ClientHeight = 85
  ClientWidth = 127
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object btnExecute: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Action = actExecute
    TabOrder = 0
  end
  object aclMain: TActionList
    Left = 16
    Top = 40
    object actExecute: TAction
      Caption = 'Execute'
      OnExecute = actExecuteExecute
    end
  end
end
