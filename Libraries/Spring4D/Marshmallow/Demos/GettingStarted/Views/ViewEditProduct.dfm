object ProductEditForm: TProductEditForm
  Left = 0
  Top = 0
  Caption = 'Enter Product Details'
  ClientHeight = 132
  ClientWidth = 389
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    389
    132)
  PixelsPerInch = 96
  TextHeight = 13
  object QuantityLabel: TLabel
    Left = 8
    Top = 65
    Width = 46
    Height = 13
    Caption = 'Quantity:'
  end
  object NameLabel: TLabel
    Left = 8
    Top = 11
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object PriceLabel: TLabel
    Left = 8
    Top = 38
    Width = 27
    Height = 13
    Caption = 'Price:'
  end
  object NameEdit: TEdit
    Left = 72
    Top = 8
    Width = 309
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'Name'
  end
  object PriceMaskEdit: TMaskEdit
    Left = 72
    Top = 35
    Width = 309
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = ''
    TextHint = 'Price'
  end
  object QuantitySpinEdit: TSpinEdit
    Left = 72
    Top = 62
    Width = 309
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 1
  end
  object OkButton: TButton
    Left = 216
    Top = 103
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 306
    Top = 103
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
