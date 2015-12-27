object frmClassProxy: TfrmClassProxy
  Left = 0
  Top = 0
  Caption = 'Class Proxy'
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
  object lblHooked: TLabel
    Left = 200
    Top = 152
    Width = 46
    Height = 13
    Caption = 'lblHooked'
  end
  object btnAddFormProxy: TButton
    Left = 8
    Top = 24
    Width = 153
    Height = 25
    Action = actAddFormProxy
    TabOrder = 0
  end
  object btnReleaseFormProxy: TButton
    Left = 167
    Top = 24
    Width = 146
    Height = 25
    Action = actReleaseFormProxy
    TabOrder = 1
  end
  object btnShowMessage: TButton
    Left = 8
    Top = 64
    Width = 153
    Height = 25
    Action = actShowMessage
    TabOrder = 2
  end
  object aclMain: TActionList
    Left = 200
    Top = 144
    object actAddFormProxy: TAction
      Caption = 'actAddFormProxy'
      OnExecute = actAddFormProxyExecute
    end
    object actReleaseFormProxy: TAction
      Caption = 'actReleaseFormProxy'
      OnExecute = actReleaseFormProxyExecute
    end
    object actShowMessage: TAction
      Caption = 'actShowMessage'
      OnExecute = actShowMessageExecute
    end
  end
end
