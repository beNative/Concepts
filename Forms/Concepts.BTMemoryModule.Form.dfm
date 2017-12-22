object frmBTMemoryModule: TfrmBTMemoryModule
  Left = 0
  Top = 0
  Caption = 'BTMemoryModule'
  ClientHeight = 82
  ClientWidth = 236
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnExecuteFromDLL: TButton
    Left = 8
    Top = 40
    Width = 129
    Height = 25
    Action = actExecuteFromDLL
    TabOrder = 0
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 236
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
    TabOrder = 1
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 236
      Height = 29
      Align = alClient
      Alignment = taCenter
      Caption = 'This form demonstrates how to use the BTMemoryModule unit.'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 225
      ExplicitHeight = 26
    end
  end
  object aclMain: TActionList
    Left = 176
    Top = 40
    object actExecuteFromDLL: TAction
      Caption = 'Execute DLL from file'
      OnExecute = actExecuteFromDLLExecute
    end
  end
end
