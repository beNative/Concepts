object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 335
  ClientWidth = 592
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    592
    335)
  PixelsPerInch = 96
  TextHeight = 13
  object dbg1: TDBGrid
    Left = 8
    Top = 39
    Width = 576
    Height = 288
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = ds1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object btn1: TButton
    Left = 8
    Top = 8
    Width = 81
    Height = 25
    Caption = 'Load From List'
    TabOrder = 1
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 95
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Save to DB'
    TabOrder = 2
    OnClick = btn2Click
  end
  object cbFilter: TCheckBox
    Left = 464
    Top = 16
    Width = 120
    Height = 17
    Caption = 'Default Status Filter'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = cbFilterClick
  end
  object btn3: TButton
    Left = 176
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 4
    OnClick = btn3Click
  end
  object btn4: TButton
    Left = 256
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Refresh Record'
    TabOrder = 5
    OnClick = btn4Click
  end
  object ds1: TDataSource
    Left = 264
    Top = 248
  end
end
