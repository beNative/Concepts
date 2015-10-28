object frmPropertyInspector: TfrmPropertyInspector
  Left = 0
  Top = 0
  ClientHeight = 586
  ClientWidth = 838
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
    Width = 838
    Height = 586
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object splSplitter: TSplitter
      Left = 313
      Top = 0
      Width = 8
      Height = 567
      ResizeStyle = rsUpdate
      ExplicitLeft = 273
      ExplicitTop = 1
      ExplicitHeight = 362
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 313
      Height = 567
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
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
      Width = 517
      Height = 567
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        517
        567)
      object lblLabel: TLabel
        Left = 6
        Top = 3
        Width = 506
        Height = 62
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'This form demonstrates the TPropertyInspector control. '#13#10'You can' +
          ' adjust properties of any control shown on this form.'
        EllipsisPosition = epWordEllipsis
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 245
      end
      object chkCheckBox: TCheckBox
        Left = 6
        Top = 83
        Width = 506
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'CheckBox'
        TabOrder = 0
      end
    end
    object sbrStatusBar: TStatusBar
      Left = 0
      Top = 567
      Width = 838
      Height = 19
      Panels = <>
      ParentShowHint = False
      ShowHint = True
    end
  end
end
