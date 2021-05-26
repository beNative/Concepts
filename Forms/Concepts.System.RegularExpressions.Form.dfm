object frmRegularExpressions: TfrmRegularExpressions
  Left = 0
  Top = 0
  Caption = 'Regular expressions demo'
  ClientHeight = 369
  ClientWidth = 472
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  DesignSize = (
    472
    369)
  PixelsPerInch = 96
  TextHeight = 13
  object lblError: TLabel
    Left = 8
    Top = 145
    Width = 363
    Height = 28
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
    WordWrap = True
    ExplicitWidth = 455
  end
  object lblWorking: TLabel
    Left = 383
    Top = 163
    Width = 81
    Height = 13
    Alignment = taCenter
    Anchors = [akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitLeft = 475
  end
  object mmoInput: TMemo
    Left = 8
    Top = 8
    Width = 456
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    OnChange = mmoInputChange
  end
  object mmoOutput: TMemo
    Left = 8
    Top = 208
    Width = 456
    Height = 155
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object edtRegExpression: TEdit
    Left = 8
    Top = 119
    Width = 456
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    TextHint = 'Regular expression'
    OnChange = edtRegExpressionChange
  end
  object edtReplace: TEdit
    Left = 8
    Top = 181
    Width = 456
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    TextHint = 'Replace with'
    OnChange = edtReplaceChange
  end
  object chkThreaded: TCheckBox
    Left = 384
    Top = 145
    Width = 80
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Threaded'
    TabOrder = 4
  end
end
