object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 289
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnContainer: TButton
    Left = 24
    Top = 24
    Width = 137
    Height = 25
    Caption = 'automock container '
    TabOrder = 0
    OnClick = btnContainerClick
  end
  object btnMock: TButton
    Left = 24
    Top = 72
    Width = 137
    Height = 25
    Caption = 'mock setup'
    TabOrder = 1
    OnClick = btnMockClick
  end
  object btnMockRegistration: TButton
    Left = 24
    Top = 120
    Width = 137
    Height = 25
    Caption = 'mock registration'
    TabOrder = 2
    OnClick = btnMockRegistrationClick
  end
end
