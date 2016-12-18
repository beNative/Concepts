object frmPropertyInspector: TfrmPropertyInspector
  Left = 0
  Top = 0
  ClientHeight = 767
  ClientWidth = 1057
  Color = clBtnFace
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
    Top = 0
    Width = 1057
    Height = 767
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 838
    ExplicitHeight = 586
    object splSplitter: TSplitter
      Left = 313
      Top = 0
      Width = 8
      Height = 748
      ResizeStyle = rsUpdate
      ExplicitLeft = 273
      ExplicitTop = 1
      ExplicitHeight = 362
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 313
      Height = 748
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 567
      object cbxControls: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 307
        Height = 21
        Margins.Bottom = 0
        Align = alTop
        Style = csDropDownList
        DropDownCount = 20
        TabOrder = 0
        OnChange = cbxControlsChange
      end
    end
    object pnlRight: TPanel
      Left = 321
      Top = 0
      Width = 736
      Height = 748
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 517
      ExplicitHeight = 567
      object splHorizontal: TSplitter
        Left = 0
        Top = 417
        Width = 736
        Height = 8
        Cursor = crVSplit
        Align = alTop
      end
      object pnlRightTop: TPanel
        Left = 0
        Top = 0
        Width = 736
        Height = 417
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
      end
      object pnlRightBottom: TPanel
        Left = 0
        Top = 425
        Width = 736
        Height = 323
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitLeft = 240
        ExplicitTop = 560
        ExplicitWidth = 185
        ExplicitHeight = 41
      end
    end
    object sbrStatusBar: TStatusBar
      Left = 0
      Top = 748
      Width = 1057
      Height = 19
      Panels = <>
      ParentShowHint = False
      ShowHint = True
      ExplicitTop = 567
      ExplicitWidth = 838
    end
  end
  object aclMain: TActionList
    Left = 608
    Top = 456
  end
end
