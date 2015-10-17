object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Concepts'
  ClientHeight = 669
  ClientWidth = 683
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlVST: TPanel
    Left = 0
    Top = 0
    Width = 683
    Height = 616
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 788
    ExplicitHeight = 586
    object edtFilter: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 677
      Height = 21
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnChange = edtFilterChange
      OnKeyDown = edtFilterKeyDown
      OnKeyUp = edtFilterKeyUp
      ExplicitWidth = 782
    end
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 650
    Width = 683
    Height = 19
    Panels = <>
    SimplePanel = True
    ExplicitWidth = 788
  end
  object pnlButtons: TGridPanel
    Left = 0
    Top = 616
    Width = 683
    Height = 34
    Align = alBottom
    ColumnCollection = <
      item
        Value = 34.297279594373700000
      end
      item
        Value = 33.023156756930660000
      end
      item
        Value = 32.679563648695640000
      end>
    ControlCollection = <
      item
        Column = 1
        Control = btnExecute
        Row = 0
      end
      item
        Column = 2
        Control = btnClose
        Row = 0
      end
      item
        Column = 0
        Control = btnExecuteModal
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 2
    object btnExecute: TButton
      AlignWithMargins = True
      Left = 237
      Top = 4
      Width = 218
      Height = 26
      Action = actExecute
      Align = alClient
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 0
      ExplicitLeft = 308
      ExplicitWidth = 75
      ExplicitHeight = 0
    end
    object btnClose: TButton
      AlignWithMargins = True
      Left = 461
      Top = 4
      Width = 218
      Height = 26
      Action = actClose
      Align = alClient
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 1
      ExplicitLeft = 532
      ExplicitWidth = 75
      ExplicitHeight = 0
    end
    object btnExecuteModal: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 227
      Height = 26
      Action = actExecuteModal
      Align = alClient
      Default = True
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 2
      ExplicitLeft = 3
      ExplicitTop = 23
      ExplicitWidth = 218
      ExplicitHeight = 25
    end
  end
  object aclMain: TActionList
    Left = 192
    Top = 128
    object actExecute: TAction
      Caption = 'Execute'
      OnExecute = actExecuteExecute
    end
    object actClose: TAction
      Caption = 'Close'
      OnExecute = actCloseExecute
    end
    object actExecuteModal: TAction
      Caption = 'Execute modal'
      OnExecute = actExecuteModalExecute
    end
  end
end
