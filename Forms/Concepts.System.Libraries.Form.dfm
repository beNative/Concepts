object frmLibraries: TfrmLibraries
  Left = 0
  Top = 0
  Caption = 'Libraries'
  ClientHeight = 200
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
  object chkEnabled: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Enabled'
    TabOrder = 0
    OnClick = chkEnabledClick
  end
  object mmoDevices: TMemo
    Left = 8
    Top = 93
    Width = 185
    Height = 89
    TabOrder = 1
  end
  object btnDiscoverDevices: TButton
    Left = 8
    Top = 31
    Width = 145
    Height = 25
    Action = actDiscoverDevices
    TabOrder = 2
  end
  object btnStartDiscoverable: TButton
    Left = 8
    Top = 62
    Width = 145
    Height = 25
    Action = actStartDiscoverable
    TabOrder = 3
  end
  object bltBluetooth: TBluetooth
    Enabled = False
    OnDiscoveryEnd = bltBluetoothDiscoveryEnd
    OnDiscoverableEnd = bltBluetoothDiscoverableEnd
    OnRemoteRequestPair = bltBluetoothRemoteRequestPair
    Left = 200
    Top = 16
  end
  object aclMain: TActionList
    Left = 312
    Top = 24
    object actStartDiscoverable: TAction
      Caption = 'Make device descoverable'
      OnExecute = actStartDiscoverableExecute
    end
    object actDiscoverDevices: TAction
      Caption = 'Discover devices'
      OnExecute = actDiscoverDevicesExecute
    end
  end
end
