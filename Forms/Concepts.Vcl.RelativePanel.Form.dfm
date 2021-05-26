object frmRelativePanel: TfrmRelativePanel
  Left = 0
  Top = 0
  Caption = 'RelativePanel'
  ClientHeight = 343
  ClientWidth = 515
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object pnlRelativePanel: TRelativePanel
    Left = 8
    Top = 8
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
      Left = 0
      Top = 0
      Width = 185
      Height = 25
      Anchors = []
      Caption = 'Button'
      TabOrder = 0
    end
  end
end
