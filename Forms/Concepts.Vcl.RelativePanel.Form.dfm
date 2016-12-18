object frmRelativePanel: TfrmRelativePanel
  Left = 0
  Top = 0
  Caption = 'RelativePanel'
  ClientHeight = 506
  ClientWidth = 731
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlRelativePanel: TRelativePanel
    Left = 48
    Top = 32
    Width = 497
    Height = 321
    ControlCollection = <
      item
        Control = btn1
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end>
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      497
      321)
    object btn1: TButton
      Left = 72
      Top = 32
      Width = 129
      Height = 25
      Anchors = []
      Caption = 'btn1'
      TabOrder = 0
    end
  end
end
