object frmPublishedFields: TfrmPublishedFields
  Left = 0
  Top = 0
  ClientHeight = 124
  ClientWidth = 259
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 11
    Top = 38
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object lbl2: TLabel
    Left = 11
    Top = 67
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object lbl3: TLabel
    Left = 11
    Top = 99
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object btn1: TButton
    Left = 49
    Top = 33
    Width = 75
    Height = 26
    Caption = 'Button1'
    TabOrder = 0
  end
  object btn2: TButton
    Left = 49
    Top = 62
    Width = 75
    Height = 26
    Caption = 'Button2'
    TabOrder = 1
  end
  object btn3: TButton
    Left = 49
    Top = 94
    Width = 75
    Height = 26
    Caption = 'Button3'
    TabOrder = 2
  end
  object edt1: TEdit
    Left = 130
    Top = 35
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'Edit1'
  end
  object edt2: TEdit
    Left = 130
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'Edit2'
  end
  object edt3: TEdit
    Left = 130
    Top = 96
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'Edit3'
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 259
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    Caption = 'These components have no published fields.'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 6
  end
end
