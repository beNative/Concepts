object frmVirtualTreeView: TfrmVirtualTreeView
  Left = 0
  Top = 0
  Caption = 'Virtual treeview'
  ClientHeight = 678
  ClientWidth = 1341
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 329
    Top = 34
    Width = 8
    Height = 625
    ExplicitTop = -19
    ExplicitHeight = 697
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 659
    Width = 1341
    Height = 19
    Panels = <>
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 34
    Width = 329
    Height = 625
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlMain: TPanel
    Left = 337
    Top = 34
    Width = 1004
    Height = 625
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object splHorizontal: TSplitter
      Left = 0
      Top = 288
      Width = 1004
      Height = 8
      Cursor = crVSplit
      Align = alTop
      ExplicitTop = 287
      ExplicitWidth = 1005
    end
    object pnlColumnSettings: TGridPanel
      Left = 0
      Top = 296
      Width = 1004
      Height = 329
      Align = alClient
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 14.285714285714290000
        end
        item
          Value = 14.285714285714290000
        end
        item
          Value = 14.285714285714290000
        end
        item
          Value = 14.285714285714290000
        end
        item
          Value = 14.285714285714290000
        end
        item
          Value = 14.285714285714290000
        end
        item
          Value = 14.285714285714260000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = pnlCol0
          Row = 0
        end
        item
          Column = 1
          Control = pnlCol1
          Row = 0
        end
        item
          Column = 2
          Control = pnlCol2
          Row = 0
        end
        item
          Column = 3
          Control = pnlCol3
          Row = 0
        end
        item
          Column = 4
          Control = pnlCol4
          Row = 0
        end
        item
          Column = 5
          Control = pnlCol5
          Row = 0
        end
        item
          Column = 6
          Control = pnlCol6
          Row = 0
        end>
      RowCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 20.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      object pnlCol0: TPanel
        Left = 0
        Top = 0
        Width = 143
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      object pnlCol1: TPanel
        Left = 143
        Top = 0
        Width = 143
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
      end
      object pnlCol2: TPanel
        Left = 286
        Top = 0
        Width = 143
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
      end
      object pnlCol3: TPanel
        Left = 429
        Top = 0
        Width = 143
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
      end
      object pnlCol4: TPanel
        Left = 572
        Top = 0
        Width = 143
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
      end
      object pnlCol5: TPanel
        Left = 715
        Top = 0
        Width = 143
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 5
      end
      object pnlCol6: TPanel
        Left = 858
        Top = 0
        Width = 146
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 6
      end
    end
    object pnlTreeView: TPanel
      Left = 0
      Top = 0
      Width = 1004
      Height = 288
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object pnlHeader: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1335
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 3
    DesignSize = (
      1335
      28)
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 361
      Height = 28
      Align = alLeft
      Alignment = taCenter
      AutoSize = False
      Caption = 'This form demonstrates the TVirtualStringTree control.'
      EllipsisPosition = epWordEllipsis
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      WordWrap = True
    end
    object lblFocusedNode: TLabel
      Left = 1250
      Top = 7
      Width = 75
      Height = 13
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'lblFocusedNode'
      Layout = tlCenter
      ExplicitLeft = 1257
    end
    object btnAutoSizeColumns: TButton
      Left = 608
      Top = 0
      Width = 120
      Height = 25
      Action = actAutoSizeColumns
      TabOrder = 0
    end
    object btnSelectFirstNode: TButton
      Left = 356
      Top = 0
      Width = 120
      Height = 25
      Action = actSelectFirstNode
      TabOrder = 1
    end
    object btnSelectLastNode: TButton
      Left = 482
      Top = 0
      Width = 120
      Height = 25
      Action = actSelectLastNode
      TabOrder = 2
    end
    object btnListHeights: TButton
      Left = 734
      Top = 0
      Width = 120
      Height = 25
      Action = actListHeights
      TabOrder = 3
    end
  end
  object aclMain: TActionList
    Left = 664
    Top = 344
    object actAutoSizeColumns: TAction
      Caption = 'AutoSizeColumns'
      OnExecute = actAutoSizeColumnsExecute
    end
    object actSelectFirstNode: TAction
      Caption = 'Select First Node'
      OnExecute = actSelectFirstNodeExecute
    end
    object actSelectLastNode: TAction
      Caption = 'Select Last Node'
      OnExecute = actSelectLastNodeExecute
    end
    object actListHeights: TAction
      Caption = 'List Heights'
      OnExecute = actListHeightsExecute
    end
  end
end
