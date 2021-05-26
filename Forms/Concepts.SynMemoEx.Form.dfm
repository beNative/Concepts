object frmSynMemoEx: TfrmSynMemoEx
  Left = 0
  Top = 0
  Caption = 'TMemoEx component demo'
  ClientHeight = 716
  ClientWidth = 1164
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 329
    Top = 35
    Width = 8
    Height = 662
    ExplicitTop = 0
    ExplicitHeight = 580
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 697
    Width = 1164
    Height = 19
    Panels = <>
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 35
    Width = 329
    Height = 662
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 29
    ExplicitHeight = 668
  end
  object pnlMain: TPanel
    Left = 337
    Top = 35
    Width = 827
    Height = 662
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 29
    ExplicitHeight = 668
  end
  object pnlHeader: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1158
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    Caption = 'This form demonstrates the TMemoEx control.'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 3
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 1164
  end
end
