object frmMulticastEventsChild: TfrmMulticastEventsChild
  Left = 0
  Top = 0
  ClientHeight = 42
  ClientWidth = 585
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  DesignSize = (
    585
    42)
  PixelsPerInch = 96
  TextHeight = 13
  object trbImageIndex: TTrackBar
    Left = 8
    Top = 8
    Width = 569
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    PageSize = 1
    ShowSelRange = False
    TabOrder = 0
    OnChange = trbImageIndexChange
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 72
    object actTest: TAction
    end
  end
end
