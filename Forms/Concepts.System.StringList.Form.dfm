object frmStringList: TfrmStringList
  Left = 0
  Top = 0
  ClientHeight = 297
  ClientWidth = 781
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object chkSorted: TCheckBox
    Left = 111
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Sorted'
    TabOrder = 0
    OnClick = chkSortedClick
  end
  object chkStrictDelimiter: TCheckBox
    Left = 214
    Top = 8
    Width = 97
    Height = 15
    Caption = 'StrictDelimiter'
    TabOrder = 1
    OnClick = chkStrictDelimiterClick
  end
  object chkCaseSensitive: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = 'CaseSensitive'
    TabOrder = 2
    OnClick = chkCaseSensitiveClick
  end
  object btnClear: TButton
    Left = 317
    Top = 3
    Width = 150
    Height = 25
    Action = actClear
    TabOrder = 3
  end
  object pnlGrid: TGridPanel
    Left = 0
    Top = 31
    Width = 781
    Height = 266
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'pnlGrid'
    ColumnCollection = <
      item
        Value = 33.333333333333330000
      end
      item
        Value = 33.333333333333330000
      end
      item
        Value = 33.333333333333330000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = pnlText
        Row = 0
      end
      item
        Column = 1
        Control = pnlDelimitedText
        Row = 0
      end
      item
        Column = 2
        Control = pnlNameValues
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAuto
      end>
    TabOrder = 4
    object pnlText: TPanel
      Left = 0
      Top = 0
      Width = 260
      Height = 266
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object btnSetText: TButton
        AlignWithMargins = True
        Left = 3
        Top = 238
        Width = 254
        Height = 25
        Action = actSetText
        Align = alBottom
        TabOrder = 0
      end
      object mmoText: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 254
        Height = 229
        Align = alClient
        TabOrder = 1
      end
    end
    object pnlDelimitedText: TPanel
      Left = 260
      Top = 0
      Width = 260
      Height = 266
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object btnSetDelimitedText: TButton
        AlignWithMargins = True
        Left = 3
        Top = 238
        Width = 254
        Height = 25
        Action = actSetDelimitedText
        Align = alBottom
        TabOrder = 0
      end
      object mmoDelimitedText: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 254
        Height = 229
        Align = alClient
        TabOrder = 1
      end
    end
    object pnlNameValues: TPanel
      Left = 520
      Top = 0
      Width = 261
      Height = 266
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object btnSetNameValues: TButton
        AlignWithMargins = True
        Left = 3
        Top = 238
        Width = 255
        Height = 25
        Action = actSetNameValues
        Align = alBottom
        TabOrder = 0
      end
      object lstValueList: TValueListEditor
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 255
        Height = 229
        Align = alClient
        DisplayOptions = [doColumnTitles, doAutoColResize]
        KeyOptions = [keyEdit, keyAdd, keyDelete]
        TabOrder = 1
        ColWidths = (
          121
          128)
        RowHeights = (
          18
          18)
      end
    end
  end
  object aclMain: TActionList
    Left = 744
    object actClear: TAction
      Caption = 'Clear'
      OnExecute = actClearExecute
    end
    object actSetText: TAction
      Caption = 'SetText'
      OnExecute = actSetTextExecute
    end
    object actSetDelimitedText: TAction
      Caption = 'SetDelimitedText'
      OnExecute = actSetDelimitedTextExecute
    end
    object actSetNameValues: TAction
      Caption = 'SetNameValues'
      OnExecute = actSetNameValuesExecute
    end
  end
end
