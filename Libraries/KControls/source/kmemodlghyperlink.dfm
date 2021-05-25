object KMemoHyperlinkForm: TKMemoHyperlinkForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Insert/edit hyperlink'
  ClientHeight = 135
  ClientWidth = 524
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    524
    135)
  PixelsPerInch = 96
  TextHeight = 13
  object LBText: TLabel
    Left = 24
    Top = 16
    Width = 73
    Height = 13
    Caption = 'Displayed text:'
  end
  object LBHyperlink: TLabel
    Left = 24
    Top = 56
    Width = 23
    Height = 13
    Caption = 'URL:'
  end
  object BUOk: TButton
    Left = 341
    Top = 95
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BUCancel: TButton
    Left = 422
    Top = 95
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object EDText: TEdit
    Left = 127
    Top = 13
    Width = 370
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object CoBURL: TComboBox
    Left = 127
    Top = 53
    Width = 370
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
end
