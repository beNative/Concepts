object frmSpringInterception: TfrmSpringInterception
  Left = 0
  Top = 0
  Caption = 'Spring Interception'
  ClientHeight = 94
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnStart: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Action = actStart
    TabOrder = 0
  end
  object btnStop: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Action = actStop
    TabOrder = 1
  end
  object btnMove: TButton
    Left = 170
    Top = 8
    Width = 75
    Height = 25
    Action = actMove
    TabOrder = 2
  end
  object aclMain: TActionList
    Left = 64
    Top = 48
    object actStart: TAction
      Caption = 'Start'
      OnExecute = actStartExecute
    end
    object actStop: TAction
      Caption = 'Stop'
      OnExecute = actStopExecute
    end
    object actMove: TAction
      Caption = 'Move'
      OnExecute = actMoveExecute
    end
  end
end
