object frmSpringInterception: TfrmSpringInterception
  Left = 0
  Top = 0
  Caption = 'Spring Interception'
  ClientHeight = 94
  ClientWidth = 257
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
  object btnStart: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Action = actStart
    TabOrder = 0
  end
  object btnStop: TButton
    Left = 89
    Top = 40
    Width = 75
    Height = 25
    Action = actStop
    TabOrder = 1
  end
  object btnMove: TButton
    Left = 170
    Top = 40
    Width = 75
    Height = 25
    Action = actMove
    TabOrder = 2
  end
  object pnlHeader: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 251
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
    ExplicitWidth = 257
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 251
      Height = 29
      Align = alClient
      Alignment = taCenter
      Caption = 'This form demonstrates Spring interception.'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 250
      ExplicitHeight = 13
    end
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
