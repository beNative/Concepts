object frmClassProxy: TfrmClassProxy
  Left = 0
  Top = 0
  Caption = 'Class Proxy'
  ClientHeight = 38
  ClientWidth = 323
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnAddFormProxy: TButton
    Left = 8
    Top = 8
    Width = 153
    Height = 25
    Action = actCallClassProxyMethods
    TabOrder = 0
  end
  object btnReleaseFormProxy: TButton
    Left = 167
    Top = 8
    Width = 146
    Height = 25
    Action = actCallInterfaceProxyMethods
    TabOrder = 1
  end
  object aclMain: TActionList
    Left = 184
    Top = 8
    object actCallClassProxyMethods: TAction
      Caption = 'Call classproxy merthods'
      OnExecute = actCallClassProxyMethodsExecute
    end
    object actCallInterfaceProxyMethods: TAction
      Caption = 'Call interfaceproxy methods'
      OnExecute = actCallInterfaceProxyMethodsExecute
    end
  end
end
