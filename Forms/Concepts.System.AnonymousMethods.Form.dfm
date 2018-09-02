object frmAnonymousMethods: TfrmAnonymousMethods
  Left = 0
  Top = 0
  Caption = 'Anonymous Methods'
  ClientHeight = 70
  ClientWidth = 280
  Color = clBtnFace
  Constraints.MaxHeight = 108
  Constraints.MaxWidth = 296
  Constraints.MinHeight = 108
  Constraints.MinWidth = 296
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  DesignSize = (
    280
    70)
  PixelsPerInch = 96
  TextHeight = 13
  object btnExec: TButton
    Left = 8
    Top = 36
    Width = 129
    Height = 25
    Action = actAssignProcVariable
    Anchors = [akBottom]
    TabOrder = 0
  end
  object btnExecuteProc: TButton
    Left = 143
    Top = 36
    Width = 129
    Height = 25
    Action = actExecuteProc
    Anchors = [akBottom]
    TabOrder = 1
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 280
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
    TabOrder = 2
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 280
      Height = 29
      Align = alClient
      Alignment = taCenter
      Caption = 'This form demonstrates anonymous methods.'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 261
      ExplicitHeight = 13
    end
  end
  object aclMain: TActionList
    Left = 32
    Top = 8
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
