object frmChromeTabs: TfrmChromeTabs
  Left = 0
  Top = 0
  Caption = 'ChromeTabs demo'
  ClientHeight = 586
  ClientWidth = 947
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 35
    Width = 947
    Height = 551
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 29
    ExplicitHeight = 557
    object splSplitter: TSplitter
      Left = 313
      Top = 0
      Width = 8
      Height = 532
      ResizeStyle = rsUpdate
      ExplicitLeft = 273
      ExplicitTop = 1
      ExplicitHeight = 362
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 313
      Height = 532
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 538
    end
    object pnlRight: TPanel
      Left = 321
      Top = 0
      Width = 626
      Height = 532
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitHeight = 538
      object pnlDrag: TPanel
        Left = 0
        Top = 0
        Width = 626
        Height = 532
        Align = alClient
        BevelOuter = bvNone
        DockSite = True
        TabOrder = 0
        ExplicitHeight = 538
      end
    end
    object sbrStatusBar: TStatusBar
      Left = 0
      Top = 532
      Width = 947
      Height = 19
      Panels = <>
      ParentShowHint = False
      ShowHint = True
      ExplicitTop = 538
    end
  end
  object pnlHeader: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 941
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
    TabOrder = 1
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 947
    object lblHeader: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 935
      Height = 26
      Align = alClient
      Alignment = taCenter
      Caption = 'This form demonstrates the TChromeTabs control.'
      Layout = tlCenter
      WordWrap = True
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 285
      ExplicitHeight = 13
    end
  end
end
