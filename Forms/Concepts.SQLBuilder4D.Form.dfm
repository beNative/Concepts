object frmSQLBuilder4D: TfrmSQLBuilder4D
  Left = 0
  Top = 0
  Caption = 'SQLBuilder4D demo'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object mmoMain: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 41
    Width = 629
    Height = 293
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitLeft = 17
    ExplicitTop = 48
    ExplicitWidth = 616
  end
  object btnTest1: TButton
    Left = 8
    Top = 10
    Width = 110
    Height = 25
    Action = actTest1
    TabOrder = 1
  end
  object btnTest2: TButton
    Left = 122
    Top = 10
    Width = 110
    Height = 25
    Action = actTest2
    TabOrder = 2
  end
  object aclMain: TActionList
    Left = 312
    Top = 176
    object actTest1: TAction
      Caption = 'Test1'
      OnExecute = actTest1Execute
    end
    object actTest2: TAction
      Caption = 'Test2'
      OnExecute = actTest2Execute
    end
  end
end
