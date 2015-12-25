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
  end
end
