object frmObjectDataSetTest: TfrmObjectDataSetTest
  Left = 0
  Top = 0
  Caption = 'TObjectDataSet Test'
  ClientHeight = 590
  ClientWidth = 1051
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    1051
    590)
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 565
    Top = 25
    Height = 525
    Align = alRight
    ExplicitLeft = 448
    ExplicitTop = 152
    ExplicitHeight = 100
  end
  object dbgList: TDBGrid
    Left = 0
    Top = 25
    Width = 565
    Height = 525
    Align = alClient
    BorderStyle = bsNone
    DataSource = dsList
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnTitleClick = dbgListTitleClick
  end
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 0
    Width = 1051
    Height = 25
    DataSource = dsList
    Align = alTop
    TabOrder = 1
  end
  object edFilter: TEdit
    Left = 0
    Top = 550
    Width = 1051
    Height = 21
    Align = alBottom
    TabOrder = 2
    OnKeyDown = edFilterKeyDown
  end
  object sbTotal: TStatusBar
    Left = 0
    Top = 571
    Width = 1051
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object dbgClone: TDBGrid
    Left = 568
    Top = 25
    Width = 483
    Height = 525
    Align = alRight
    DataSource = dsClone
    TabOrder = 5
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object cbFiltered: TCheckBox
    Left = 946
    Top = 527
    Width = 97
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Filtered'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = cbFilteredClick
  end
  object dsList: TDataSource
    Left = 488
    Top = 264
  end
  object dsClone: TDataSource
    Left = 664
    Top = 224
  end
end
