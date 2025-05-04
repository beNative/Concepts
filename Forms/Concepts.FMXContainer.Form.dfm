object frmFMXContainer: TfrmFMXContainer
  Left = 0
  Top = 0
  Caption = 'FMXContainer'
  ClientHeight = 461
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  ShowHint = True
  TextHeight = 13
  object pnlFMXContainer: TPanel
    Left = 0
    Top = 32
    Width = 784
    Height = 429
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 778
    ExplicitHeight = 412
  end
  object pnlHeader: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 778
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    Caption = 
      'This VCL form hosts a FireMonkey form by using the FMXContainter' +
      ' control.'
    Color = clWhite
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 1
    ExplicitWidth = 772
  end
end
