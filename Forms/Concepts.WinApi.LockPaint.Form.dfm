object frmLockPaint: TfrmLockPaint
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'LockPaint'
  ClientHeight = 71
  ClientWidth = 323
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
  object btnLockPaint: TButton
    Left = 8
    Top = 8
    Width = 150
    Height = 25
    Action = actLockPaint
    TabOrder = 0
  end
  object btnUnlockPaint: TButton
    Left = 164
    Top = 8
    Width = 150
    Height = 25
    Action = actUnlockPaint
    TabOrder = 1
  end
  object btnLockWindowUpdate: TButton
    Left = 8
    Top = 39
    Width = 150
    Height = 25
    Action = actLockWindowUpdate
    TabOrder = 2
  end
  object btnUnlockWindowUpdate: TButton
    Left = 164
    Top = 39
    Width = 150
    Height = 25
    Action = actUnlockWindowUpdate
    TabOrder = 3
  end
  object aclMain: TActionList
    Left = 8
    object actLockPaint: TAction
      Caption = 'Lock paint (F1)'
      ShortCut = 112
      OnExecute = actLockPaintExecute
    end
    object actUnlockPaint: TAction
      Caption = 'Unlock Paint (F2)'
      ShortCut = 113
      OnExecute = actUnlockPaintExecute
    end
    object actLockWindowUpdate: TAction
      Caption = 'Lock Window Update (F3)'
      ShortCut = 114
      OnExecute = actLockWindowUpdateExecute
    end
    object actUnlockWindowUpdate: TAction
      Caption = 'Unlock Window Update (F4)'
      ShortCut = 115
      OnExecute = actUnlockWindowUpdateExecute
    end
    object actDraw: TAction
      Caption = 'actDraw'
      ShortCut = 116
      OnExecute = actDrawExecute
    end
  end
end
