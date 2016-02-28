object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Concepts'
  ClientHeight = 562
  ClientWidth = 1007
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
    Width = 1007
    Height = 509
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object edtFilter: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 1001
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
    end
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 543
    Width = 1007
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object pnlButtons: TGridPanel
    Left = 0
    Top = 509
    Width = 1007
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
      Left = 348
      Top = 4
      Width = 325
      Height = 26
      Action = actExecute
      Align = alClient
      DoubleBuffered = True
      ImageMargins.Left = 4
      Images = dmResources.imlMain
      ParentDoubleBuffered = False
      TabOrder = 0
    end
    object btnClose: TButton
      AlignWithMargins = True
      Left = 679
      Top = 4
      Width = 324
      Height = 26
      Action = actClose
      Align = alClient
      DoubleBuffered = True
      ImageMargins.Left = 4
      Images = dmResources.imlMain
      ParentDoubleBuffered = False
      TabOrder = 1
    end
    object btnExecuteModal: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 338
      Height = 26
      Action = actExecuteModal
      Align = alClient
      Default = True
      DoubleBuffered = True
      ImageMargins.Left = 4
      Images = dmResources.imlMain
      ParentDoubleBuffered = False
      TabOrder = 2
    end
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 192
    Top = 128
    object actExecute: TAction
      Caption = 'Execute'
      ImageIndex = 519
      OnExecute = actExecuteExecute
    end
    object actClose: TAction
      Caption = 'Close'
      ImageIndex = 339
      OnExecute = actCloseExecute
    end
    object actExecuteModal: TAction
      Caption = 'Execute modal'
      ImageIndex = 518
      OnExecute = actExecuteModalExecute
    end
  end
end
