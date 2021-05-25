object AboutForm: TAboutForm
  Left = 465
  Top = 394
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 120
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LBProductName: TLabel
    Left = 8
    Top = 8
    Width = 139
    Height = 19
    Caption = 'Hex Editor (Demo)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    IsControl = True
  end
  object LBCopyright: TLabel
    Left = 8
    Top = 32
    Width = 152
    Height = 13
    Caption = 'Copyright (C) 2006 Tomas Krysl'
    IsControl = True
  end
  object LBEmail: TLabel
    Left = 8
    Top = 53
    Width = 196
    Height = 13
    Caption = 'https://github.com/kryslt/KControls'
  end
  object BUOk: TButton
    Left = 111
    Top = 84
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
end
