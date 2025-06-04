object frmStyledComponents: TfrmStyledComponents
  Left = 0
  Top = 0
  ClientHeight = 898
  ClientWidth = 1222
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  PopupMode = pmAuto
  Position = poMainFormCenter
  OnShow = FormShow
  TextHeight = 15
  object splVertical: TSplitter
    Left = 396
    Top = 35
    Width = 8
    Height = 844
    ExplicitLeft = 329
    ExplicitTop = 27
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 879
    Width = 1222
    Height = 19
    Panels = <>
    ExplicitTop = 862
    ExplicitWidth = 1216
  end
  object pnlLeft: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 38
    Width = 390
    Height = 838
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitHeight = 821
  end
  object pnlMain: TPanel
    Left = 404
    Top = 35
    Width = 818
    Height = 844
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 812
    ExplicitHeight = 827
    object btnStyledButton: TStyledButton
      Left = 56
      Top = 16
      Width = 121
      Height = 41
      Caption = 'btnStyledButton'
      TabOrder = 0
    end
  end
  object pnlHeader: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1216
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    Caption = 'This form demonstrates the ETHEA StyledComponents library.'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 3
    ExplicitWidth = 1210
  end
end
