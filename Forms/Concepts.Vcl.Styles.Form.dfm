object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 486
  ClientWidth = 1189
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    1189
    486)
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 0
    Top = 5
    Width = 50
    Height = 13
    Caption = 'VCL Styles'
  end
  object btnactCnPrefixWizard: TButton
    Left = 0
    Top = 458
    Width = 105
    Height = 25
    Action = actApplyStyle
    Anchors = [akLeft, akBottom]
    Caption = 'Apply Style'
    TabOrder = 0
  end
  object lv1: TListView
    Left = 0
    Top = 8
    Width = 641
    Height = 400
    Columns = <
      item
        Caption = 'Source'
        Width = 80
      end
      item
        Caption = 'Style File'
        Width = 150
      end
      item
        Caption = 'Name'
        Width = 100
      end
      item
        Caption = 'Author'
        Width = 100
      end
      item
        Caption = 'Author URL'
        Width = 100
      end
      item
        Caption = 'Version'
        Width = 55
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object pnl1: TPanel
    Left = 656
    Top = 8
    Width = 525
    Height = 475
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
  object actmgr1: TActionManager
    Left = 288
    Top = 341
    StyleName = 'Platform Default'
    object actApplyStyle: TAction
      Caption = 'ActionApplyStyle'
    end
  end
end
