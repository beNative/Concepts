object frmSQLBuilder4D: TfrmSQLBuilder4D
  Left = 0
  Top = 0
  Caption = 'SQLBuilder4D demo'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object mmoMain: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 72
    Width = 629
    Height = 262
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object btnTest1: TButton
    Left = 8
    Top = 41
    Width = 110
    Height = 25
    Action = actTest1
    TabOrder = 1
  end
  object btnTest2: TButton
    Left = 122
    Top = 41
    Width = 110
    Height = 25
    Action = actTest2
    TabOrder = 2
  end
  object pnlHeader: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 629
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
    TabOrder = 3
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 635
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 629
      Height = 29
      Align = alClient
      Alignment = taCenter
      Caption = 
        'This form demonstrates how to use the SQLBuilder4D unit to build' +
        ' SQL statements in code.'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 509
      ExplicitHeight = 13
    end
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
