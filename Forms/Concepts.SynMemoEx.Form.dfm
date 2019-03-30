object frmSynMemoEx: TfrmSynMemoEx
  Left = 0
  Top = 0
  Caption = 'TMemoEx component demo'
  ClientHeight = 716
  ClientWidth = 1164
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 329
    Top = 29
    Width = 8
    Height = 668
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
    Top = 29
    Width = 329
    Height = 668
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlMain: TPanel
    Left = 337
    Top = 29
    Width = 827
    Height = 668
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 1164
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
    ExplicitTop = -6
  end
end
