object frmVirtualTreeView: TfrmVirtualTreeView
  Left = 0
  Top = 0
  Caption = 'Virtual treeview'
  ClientHeight = 678
  ClientWidth = 1342
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
    Top = 28
    Width = 8
    Height = 631
    ExplicitTop = -19
    ExplicitHeight = 697
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 659
    Width = 1342
    Height = 19
    Panels = <>
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 28
    Width = 329
    Height = 631
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlMain: TPanel
    Left = 337
    Top = 28
    Width = 1005
    Height = 631
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 1342
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 3
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 1342
      Height = 28
      Align = alClient
      Alignment = taCenter
      AutoSize = False
      Caption = 'This form demonstrates the TVirtualStringTree control.'
      EllipsisPosition = epWordEllipsis
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 799
      ExplicitHeight = 26
    end
  end
end
