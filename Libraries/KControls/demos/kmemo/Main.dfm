object MainForm: TMainForm
  Left = 566
  Top = 256
  Caption = 'KMemoDemo'
  ClientHeight = 556
  ClientWidth = 997
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    997
    556)
  PixelsPerInch = 96
  TextHeight = 13
  object PNMain: TPanel
    Left = 0
    Top = 0
    Width = 997
    Height = 519
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 500
      Top = 0
      Width = 4
      Height = 519
      ResizeStyle = rsUpdate
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 500
      Height = 519
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 504
      Top = 0
      Width = 493
      Height = 519
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 503
      ExplicitWidth = 494
    end
  end
  object BULoad: TButton
    Left = 24
    Top = 526
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Load files'
    TabOrder = 1
    OnClick = BULoadClick
  end
  object BUPreview: TButton
    Left = 105
    Top = 526
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Preview'
    TabOrder = 2
    OnClick = BUPreviewClick
  end
  object BUPrint: TButton
    Left = 186
    Top = 526
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Print'
    TabOrder = 3
    OnClick = BUPrintClick
  end
  object BUTest: TButton
    Left = 428
    Top = 526
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Run Test'
    TabOrder = 4
    OnClick = BUTestClick
  end
  object CoBTest: TComboBox
    Left = 267
    Top = 530
    Width = 145
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 5
  end
  object KPrintPreviewDialog1: TKPrintPreviewDialog
    Left = 408
    Top = 507
  end
  object KPrintSetupDialog1: TKPrintSetupDialog
    Left = 448
    Top = 507
  end
end
