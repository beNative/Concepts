object frmMulticastEvents: TfrmMulticastEvents
  Left = 0
  Top = 0
  Caption = 'Multicast events'
  ClientHeight = 124
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  DesignSize = (
    288
    124)
  PixelsPerInch = 96
  TextHeight = 13
  object lblImageIndex: TLabel
    Left = 8
    Top = 43
    Width = 62
    Height = 13
    Caption = 'ImageIndex:'
  end
  object pnlImageIndex: TPanel
    Left = 71
    Top = 40
    Width = 30
    Height = 22
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pbrPosition: TProgressBar
    Left = 8
    Top = 68
    Width = 273
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object btnExecute: TButton
    Left = 107
    Top = 37
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
    Top = 86
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
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 288
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
    TabOrder = 4
    ExplicitLeft = -207
    ExplicitWidth = 495
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 288
      Height = 29
      Align = alClient
      Alignment = taCenter
      Caption = 'This form demonstrates how to use Spring multicast events.'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 242
      ExplicitHeight = 26
    end
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 248
    Top = 48
    object actExecute: TAction
      Caption = 'Execute childform'
      OnExecute = actExecuteExecute
    end
  end
end
