object frmLazyInstantiation: TfrmLazyInstantiation
  Left = 0
  Top = 0
  Caption = 'Lazy instantiation'
  ClientHeight = 366
  ClientWidth = 419
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  DesignSize = (
    419
    366)
  TextHeight = 13
  object lblDescription: TLabel
    Left = 0
    Top = 35
    Width = 419
    Height = 52
    Align = alTop
    AutoSize = False
    Caption = 
      'This example demonstrates lazy instantiation through the Lazy<T>' +
      ' type in the Delphi Spring library. Objects of the wrapped type ' +
      'will be created by the specified delegate when they are first re' +
      'ferenced.'
    Color = clCream
    ParentColor = False
    Transparent = False
    WordWrap = True
  end
  object btnAddTextToLazyMemo: TButton
    Left = 8
    Top = 97
    Width = 130
    Height = 25
    Action = actAddTextToLazyMemo
    TabOrder = 0
  end
  object pnlMemo: TPanel
    Left = 8
    Top = 136
    Width = 403
    Height = 169
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 397
    ExplicitHeight = 152
  end
  object pnlCheckBox: TPanel
    Left = 8
    Top = 313
    Width = 403
    Height = 47
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 296
    ExplicitWidth = 397
  end
  object btnToggleLazyCheckbox: TButton
    Left = 144
    Top = 97
    Width = 130
    Height = 25
    Action = actToggleLazyCheckbox
    TabOrder = 3
  end
  object btnShowClassName: TButton
    Left = 281
    Top = 97
    Width = 130
    Height = 25
    Action = actShowClassName
    TabOrder = 4
  end
  object pnlHeader: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 413
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 5
    ExplicitWidth = 407
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 413
      Height = 29
      Align = alClient
      Alignment = taCenter
      Caption = 'This form demonstrates the Spring4D ILazy type.'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 278
      ExplicitHeight = 13
    end
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
      Hint = 'Lazy creation of TMyType class.'
      OnExecute = actShowClassNameExecute
    end
  end
end
