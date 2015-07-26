object frmAnonymousMethods: TfrmAnonymousMethods
  Left = 0
  Top = 0
  Caption = 'Anonymous Methods'
  ClientHeight = 286
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnExec: TButton
    Left = 32
    Top = 32
    Width = 129
    Height = 25
    Action = actAssignProcVariable
    TabOrder = 0
  end
  object btnExecuteProc: TButton
    Left = 32
    Top = 63
    Width = 129
    Height = 25
    Action = actExecuteProc
    TabOrder = 1
  end
  object aclMain: TActionList
    Left = 328
    Top = 24
    object actAssignProcVariable: TAction
      Caption = 'Assign TProc variable'
      OnExecute = actAssignProcVariableExecute
    end
    object actExecuteProc: TAction
      Caption = 'Execute TProc variable'
      OnExecute = actExecuteProcExecute
    end
  end
end
