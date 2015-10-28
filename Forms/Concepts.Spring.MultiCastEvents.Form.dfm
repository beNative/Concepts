object frmMulticastEvents: TfrmMulticastEvents
  Left = 0
  Top = 0
  Caption = 'Multicast events'
  ClientHeight = 246
  ClientWidth = 359
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    359
    246)
  PixelsPerInch = 96
  TextHeight = 13
  object lblImageIndex: TLabel
    Left = 16
    Top = 192
    Width = 62
    Height = 13
    Caption = 'ImageIndex:'
  end
  object btnTriggerChangeEvent: TButton
    Left = 8
    Top = 8
    Width = 347
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Trigger TestComponent  OnChange event'
    TabOrder = 0
  end
  object pnlImageIndex: TPanel
    Left = 82
    Top = 180
    Width = 30
    Height = 22
    TabOrder = 1
  end
  object pbrPosition: TProgressBar
    Left = 155
    Top = 180
    Width = 174
    Height = 21
    TabOrder = 2
  end
  object btnExecute: TButton
    Left = 155
    Top = 128
    Width = 166
    Height = 25
    Action = actExecute
    Images = dmResources.imlMain
    TabOrder = 3
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 176
    Top = 72
    object actExecute: TAction
      Caption = 'Execute'
      OnExecute = actExecuteExecute
    end
  end
end
