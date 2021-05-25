object KPictureEditForm: TKPictureEditForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'KControls picture editor'
  ClientHeight = 247
  ClientWidth = 376
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
  object BULoad: TButton
    Left = 248
    Top = 16
    Width = 97
    Height = 25
    Caption = 'Load...'
    TabOrder = 0
    OnClick = BULoadClick
  end
  object BUSave: TButton
    Left = 248
    Top = 47
    Width = 97
    Height = 25
    Caption = 'Save...'
    TabOrder = 1
    OnClick = BUSaveClick
  end
  object BUOK: TButton
    Left = 248
    Top = 169
    Width = 97
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object BUCancel: TButton
    Left = 248
    Top = 200
    Width = 97
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object PNMain: TPanel
    Left = 8
    Top = 16
    Width = 225
    Height = 209
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 4
    object IMMain: TImage
      Left = 0
      Top = 0
      Width = 221
      Height = 205
      Align = alClient
      Center = True
      Proportional = True
    end
  end
  object CBStretch: TCheckBox
    Left = 248
    Top = 136
    Width = 55
    Height = 19
    Caption = 'Stretch'
    TabOrder = 5
    OnClick = CBStretchClick
  end
  object BUClear: TButton
    Left = 248
    Top = 78
    Width = 97
    Height = 25
    Caption = 'Clear'
    TabOrder = 6
    OnClick = BUClearClick
  end
  object ODMain: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 256
    Top = 128
  end
  object SDMain: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 296
    Top = 128
  end
end
