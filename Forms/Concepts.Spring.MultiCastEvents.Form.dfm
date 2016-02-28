object frmMulticastEvents: TfrmMulticastEvents
  Left = 0
  Top = 0
  Caption = 'Multicast events'
  ClientHeight = 88
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  DesignSize = (
    288
    88)
  PixelsPerInch = 96
  TextHeight = 13
  object lblImageIndex: TLabel
    Left = 8
    Top = 9
    Width = 62
    Height = 13
    Caption = 'ImageIndex:'
  end
  object pnlImageIndex: TPanel
    Left = 76
    Top = 4
    Width = 30
    Height = 22
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pbrPosition: TProgressBar
    Left = 8
    Top = 34
    Width = 273
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object btnExecute: TButton
    Left = 107
    Top = 3
    Width = 174
    Height = 25
    Action = actExecute
    Anchors = [akLeft, akTop, akRight]
    Default = True
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 2
  end
  object trbImageIndex: TTrackBar
    Left = 8
    Top = 52
    Width = 273
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Max = 500
    Frequency = 100
    ShowSelRange = False
    TabOrder = 3
    TickStyle = tsManual
    OnChange = trbImageIndexChange
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 248
    Top = 8
    object actExecute: TAction
      Caption = 'Execute childform'
      OnExecute = actExecuteExecute
    end
  end
end
