object frmLazyInstantiation: TfrmLazyInstantiation
  Left = 0
  Top = 0
  Caption = 'Lazy instantiation'
  ClientHeight = 249
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    418
    249)
  PixelsPerInch = 96
  TextHeight = 13
  object lblDescription: TLabel
    Left = 8
    Top = 10
    Width = 402
    Height = 38
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This example demonstrates lazy instantiation through the Lazy<T>' +
      ' type in the Delphi Spring library. Objects of the wrapped type ' +
      'will be created by the specified delegate when they are first re' +
      'ferenced.'
    WordWrap = True
  end
  object btnAddTextToLazyMemo: TButton
    Left = 8
    Top = 57
    Width = 130
    Height = 25
    Action = actAddTextToLazyMemo
    TabOrder = 0
  end
  object pnlMemo: TPanel
    Left = 8
    Top = 88
    Width = 402
    Height = 110
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'pnlMemo'
    TabOrder = 1
  end
  object pnlCheckBox: TPanel
    Left = 8
    Top = 204
    Width = 402
    Height = 37
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'pnlCheckBox'
    TabOrder = 2
  end
  object btnToggleLazyCheckbox: TButton
    Left = 144
    Top = 57
    Width = 130
    Height = 25
    Action = actToggleLazyCheckbox
    TabOrder = 3
  end
  object btnShowClassName: TButton
    Left = 280
    Top = 57
    Width = 130
    Height = 25
    Action = actShowClassName
    TabOrder = 4
  end
  object aclMain: TActionList
    Left = 200
    Top = 144
    object actToggleLazyCheckbox: TAction
      Caption = 'Toggle lazy checkbox'
      OnExecute = actToggleLazyCheckboxExecute
    end
    object actAddTextToLazyMemo: TAction
      Caption = 'Add text to lazy memo'
      OnExecute = actAddTextToLazyMemoExecute
    end
    object actShowClassName: TAction
      Caption = 'Show classname'
      OnExecute = actShowClassNameExecute
    end
  end
end
