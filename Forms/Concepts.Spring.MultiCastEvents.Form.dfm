object frmMulticastEvents: TfrmMulticastEvents
  Left = 0
  Top = 0
  Caption = 'Multicast events'
  ClientHeight = 41
  ClientWidth = 237
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    237
    41)
  PixelsPerInch = 96
  TextHeight = 13
  object btnTriggerChangeEvent: TButton
    Left = 8
    Top = 8
    Width = 225
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Trigger TestComponent  OnChange event'
    TabOrder = 0
    OnClick = btnTriggerChangeEventClick
  end
end
